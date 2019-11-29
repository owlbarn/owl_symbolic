(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_graph
open Owl_symbolic_graph
open Owl_symbolic_symbol

let topo_iter_expr f (n : symbolic_node) = iter_ancestors ~order:DFS ~traversal:PostOrder f [|n|]

(* `any` -- a clumsy solution with early break *)
let has_symbol (expr : symbolic_node) e = 
  let typ  = op_type e in
  let nam  = name e in
  let flag = ref false in 
  let _ = try 
    topo_iter_expr (fun n ->
      let s = Owl_graph.attr n in 
      let f = match s with 
      | Variable _ -> (op_type s = typ) && (name s = nam)
      | _          -> op_type s = typ
      in
      if f then raise Owl_exception.FOUND
    ) expr
  with Owl_exception.FOUND -> flag := true 
  in
  !flag

(* Replace occurrences of objects within the expression.
 * (1 + x*y).xreplace({x: pi}) -> pi*y + 1
 * (x + y + 2).xreplace({x + y: 2}) -> x + y + 2
 *)
let xreplace expr _rule = 
  (* 1. if rule already in expr then return rule[expr] *)
  (* 2. Iterative apply xreplace on all children *)
  expr


(* ((x*y)**3).extract_multiplicatively(x**2 * y) -->  x*y**2 *)
let extract_multiplicatively _expr _c = 
  (* 1. if expr is NaN, return None *)
  (* 2. if c is "One", return expr *)
  (* 3. if c equals to self, return One *)

  (* 4. if c is add, get primitive a, b:
   * E.g. (2*x + 4*y).primitive() --> (2, x + 2*y)
   * if a != 1, then c = a * b 
   *)
  (* 5. if c is mul, make it as a, b terms;
   * let x = extract_multiplicatively(expr, a); 
   * if x is not None, then return  extract_multiplicatively(x, b);
   * Here the idea: we want y so that  expr = cy = a b y; if expr = a x, then y = x/b 
   *)
   (* 6. let quotien = expr / c; if expr is number, then
    * if expr and quotient are both int/float/rational, and (expr is not positive OR quotient is non-negative)
    * then return quotient
    *)
  None


(* return true if expr is not in a canonical form with respect to its sign. *)
let could_extract_mius_sign _expr = false 


(* Remove common factors from terms in all arguments without
  changing the underlying structure of the expr. No expansion or
  simplification (and no processing of non-commutatives) is performed. 
  Example: factor_terms(-x - y) -> - (x + y)
  *)
let factor_terms (expr : symbolic_node) = expr
