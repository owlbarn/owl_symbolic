(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph
open Owl_graph
open Owl_symbolic_symbol

let topo_iter_expr f (n : symbolic_node) = iter_ancestors ~order:DFS ~traversal:PostOrder f [|n|]

(* `any` *)
let has_symbol expr e = 
  let flag = ref false in
  let typ  = op_type e in 
  topo_iter_expr (fun n ->
    let s = Owl_graph.attr n in 
    let f = match s with 
    | Variable _ -> true 
    | _ -> op_type s = typ
    in
    flag := !flag || f
  ) expr;
  !flag


let xreplace () = ()


let extract_multiplicatively _expr _c = None


(* return true if expr is not in a canonical form with respect to its sign. *)
let could_extract_mius_sign _expr = false 


(* Remove common factors from terms in all arguments without
  changing the underlying structure of the expr. No expansion or
  simplification (and no processing of non-commutatives) is performed. 
  Example: factor_terms(-x - y) -> - (x + y)
  *)
let factor_terms (expr : symbolic_node) = expr
