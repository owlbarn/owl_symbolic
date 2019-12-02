(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Target: Int, Rational, Var, Add *)

open Owl_symbolic_symbol
open Owl_symbolic_graph

let rec _to_canonical node =
  let sym = Owl_graph.attr node in
  match sym with
  | Int _      -> canonical_int node
  | Float _    -> canonical_float node
  | Variable _ -> canonical_var node
  | Add _      -> canonical_add node
  | Rational _ -> canonical_rat node
  | _          -> failwith "error: _to_canonical"


and canonical_int node =
  let parents = Owl_graph.parents node in
  assert (parents = [||]);
  let value = Owl_symbolic_symbol.int_value (Owl_graph.attr node) in
  if value = 0
  then (
    let new_sym = Owl_symbolic_operator.zero () in
    set_sym node (Owl_graph.attr new_sym) (* TODO: redundant step *))
  else if value = 1
  then (
    let new_sym = Owl_symbolic_operator.one () in
    set_sym node (Owl_graph.attr new_sym))
  else if value = -1
  then (
    let new_sym = Owl_symbolic_operator.negone () in
    set_sym node (Owl_graph.attr new_sym))


and canonical_float node =
  let value = Owl_symbolic_symbol.float_value (Owl_graph.attr node) in
  if value = 0.
  then (
    let new_sym = Owl_symbolic_operator.zero () in
    set_sym node (Owl_graph.attr new_sym))


and canonical_rat node =
  let parents = Owl_graph.parents node in
  let p = parents.(0) in
  let q = parents.(1) in
  _to_canonical p;
  _to_canonical q;
  let get_rat p =
    match Owl_graph.attr p with
    | Zero _     -> 0, 1 (* TODO: are they really necessary? *)
    | One _      -> 1, 1
    | NegOne _   -> -1, 1
    | Int _      ->
      let v = Owl_symbolic_symbol.int_value (Owl_graph.attr p) in
      v, 1
    | Float _    ->
      let v = Owl_symbolic_symbol.float_value (Owl_graph.attr p) in
      Owl_symbolic_utils.float_as_ratio v
    | Rational x -> x.p, x.q
    | _          -> failwith "canonical_rat: unsupported input type"
  in
  let pn, pd = get_rat p in
  let qn, qd = get_rat q in
  let p = pn * qd in
  let q = qn * pd in
  (* now both p and q are integers *)
  let p, q =
    if q = 0
    then failwith "canonical_rat: Division_by_zero"
    else if q < 0
    then -p, -q
    else (
      let gcd = Owl_symbolic_utils.gcd (abs p) q in
      p / gcd, q / gcd)
  in
  if q == 1
  then (
    let s = Owl_symbolic_operator.int p |> Owl_graph.attr in
    (* TODO change to symbol *)
    set_sym node s;
    Owl_graph.remove_edge parents.(0) node;
    Owl_graph.remove_edge parents.(1) node;
    Owl_graph.remove_node parents.(0);
    Owl_graph.remove_node parents.(1))
  else (
    let s1 = Owl_symbolic_operator.int p |> Owl_graph.attr in
    set_sym parents.(0) s1;
    let s2 = Owl_symbolic_operator.int q |> Owl_graph.attr in
    set_sym parents.(1) s2
    (* !!!!! TODO: but you have to remove the parents ?!!!!! *)
    (* let x = Owl_graph.attr node in 
    x.p <- p;
    x.q <- q *))


and canonical_var _node = ()
and canonical_add _node = ()

let canonical_form sym_graph =
  let output = Owl_symbolic_graph.get_output_nodes sym_graph in
  Array.iter _to_canonical output
