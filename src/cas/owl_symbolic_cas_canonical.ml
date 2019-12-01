(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

 (** Target: Int, Rational, Var, Add *)

open Owl_symbolic_symbol

let rec _to_canonical expr =
  match expr with 
  | Int x -> canonical_int x
  | Variable x -> canonical_var x
  | Add x -> canonical_add x
  | _ -> failwith "error: _to_canonical"
  (*| Rational x -> canonical_rat x *)

and canonical_int x = ()

and canonical_rat x = ()

and canonical_var x = ()

and canonical_add x = ()


let canonical_form sym_graph =
  let output = Owl_symbolic_graph.get_output_nodes sym_graph in 
  let syms = Array.map Owl_graph.attr output in 
  Array.iter _to_canonical syms