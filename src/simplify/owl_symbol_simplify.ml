(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph

(* TODO: 
 * - differentiate basic/expr/functions...?
 * - 'is_atom'/'get_autom'/etc.
 * - simp function works on node or graph? perhaps node....
 *)

(* Replace y - x with -(x - y) if -1 can be extracted from y - x. *)
let sub_pre expr = expr

(* Replace 1*-1*x with -x. *)
let sub_post expr = expr


(*
 * Make all Add sub-expressions canonical wrt sign.
 * If an Add subexpression, ``a``, can have a sign extracted,
 * as determined by could_extract_minus_sign, it is replaced
 * with Mul(-1, a, evaluate=False). This allows signs to be
 * extracted from powers and products.
 *)

let rec signsimp (expr : Owl_symbolic_graph.symbolic_node) =
  (* Step 1. if not instance of Expr, or is atom, return *)
  (* Step 2. *)
  let expr = expr |> sub_pre |> sub_post in
  (* Step 3. if not instance of Expr, or is atom, return *)
  (* Step 4. if expr is Add, apply signsimp to all children 
     ---> so, the simp function works on node or graph? *)
  (* Step 5 (optinonal). evaluate so that x + x -> 2 * x *)
  (* find all the Mul type atoms a's of expr, and xreplace a with -(-a)*)
  expr
