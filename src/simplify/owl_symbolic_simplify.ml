(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph

(* Principle: do not care about details, just make it work; 
 * check every step with example *)

(* TODO: 
 * - differentiate basic/expr/functions...?
 * - 'is_atom'/'get_autom'/etc.
 * - simp function works on node or graph? perhaps node....
 * - check simplify length 
 *)

(* Replace y - x with -(x - y) if -1 can be extracted from y - x. *)
let sub_pre expr =
  (* 1. get all the Add atoms if their minus sign could be extracted;
   * and then `xreplace` in expr
   *)
  (* 2.  y - x -> 1*-1*(x - y) in expr *)
  expr


(* Replace 1*-1*x with -x. *)
let sub_post expr =
  (* traverse all nodes in expr, for each Mul which has one and negone as children *)
  (* xreplace *)
  expr


(*
 * Make all Add sub-expressions canonical wrt sign.
 * If an Add subexpression, ``a``, can have a sign extracted,
 * as determined by could_extract_minus_sign, it is replaced
 * with Mul(-1, a, evaluate=False). This allows signs to be
 * extracted from powers and products.
 *)

let signsimp (expr : symbolic_node) =
  (* Step 1. if not instance of Expr, or is atom, return *)
  (* Step 2. *)
  let expr = expr |> sub_pre |> sub_post in
  (* Step 3. if not instance of Expr, or is atom, return *)
  (* Step 4. if expr is Add, apply signsimp to all children *)
  (* Step 5 (optinonal). evaluate so that x + x -> 2 * x *)
  (* find all the Mul type atoms a's of expr, and xreplace a with -(-a)*)
  expr


let inversecombine expr = expr

(* Simplify a non-commutative expression composed of multiplication
  and raising to a power by grouping repeated subterms into one power.
  Priority is given to simplifications that give the fewest number
  of arguments in the end (for example, in a*b*a*b*c*a*b*c simplifying
  to (a*b)**2*c*a*b*c gives 5 arguments while a*b*(a*b*c)**2 has 3).
  If `expr` is a sum of such terms, the sum of the simplified terms
  is returned. *)
let nc_simplify expr = expr

(* Find a simple representation for a number or, if there are free symbols or
    if rational=True, then replace Floats with their Rational equivalents. If
    no change is made and rational is not False then Floats will at least be
    converted to Rationals.
  E.g. nsimplify(pi, tolerance=0.01) --> 22/7
  *)
let nsimplify expr = expr

(* Sum simplification *)
let sum_simplify expr = expr

(* Main entrance *)
let simplify expr =
  (* 1. if expression is zero, return zero *)
  (* 2. put everything in expr and put under one single abs *)
  let expr = signsimp expr |> Owl_symbolic_radsimp.collect_abs in
  (* 3. return expr if does not have children, return expr *)
  (* 4. if contains function, simplify function and its reverse funcction to 1, e.g. sin an asin *)
  let expr = inversecombine expr in
  (* 5. Using `replace`, for all non-add/mul/pow/exp, apply simplify on all its children  *)
  (* 6. apply nc_simplify if expr is not commutative *)
  let expr = nc_simplify expr in
  (* 7. if rational_flag is true and expr contains float, rationalise float;
   * and return the rational number to float once done. *)
  let expr = nsimplify expr in
  (* 8. factor terms *)
  let expr = Owl_symbolic_expr.factor_terms expr in
  (* 9: a series of specific simplification: Piecewise, Besselbase, ... only use Sum as example *)
  (* if expr has Sum, apply simplify accordingly *)
  let expr = sum_simplify expr in
  (* 10. powsimp; radsimp; .... ignore here. *)
  expr
