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
  | Variable x -> canonical_var x
  | Add x      -> canonical_add x
  | _          -> failwith "error: _to_canonical"


(*| Rational x -> canonical_rat x *)
and canonical_int node =
  let parents = Owl_graph.parents node in
  assert (parents = [||]);
  let value = Owl_symbolic_symbol.int_value (Owl_graph.attr node) in
  if value = 0
  then (
    let new_sym = Owl_symbolic_operator.zero () in
    set_sym node (Owl_graph.attr new_sym) (* redundant step *))
  else if value = 1
  then (
    let new_sym = Owl_symbolic_operator.one () in
    set_sym node (Owl_graph.attr new_sym) (* redundant step *))
  else if value = -1
  then (
    let new_sym = Owl_symbolic_operator.negone () in
    set_sym node (Owl_graph.attr new_sym) (* redundant step *))


and canonical_rat _node = ()
and canonical_var _node = ()
and canonical_add _node = ()

let canonical_form sym_graph =
  let output = Owl_symbolic_graph.get_output_nodes sym_graph in
  Array.iter _to_canonical output
