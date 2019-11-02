(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_graph 

(** A series of graph operations. *)

(** NOTE: I have Tree structure in mind when coding all these... *)
let make_graph (attr : Owl_symbolic_symbol.t) (inputs : symbolic_graph array) =
  let child = node attr in
  connect_ancestors inputs [|child|];
  child

let null_graph =
  let attr = Owl_symbolic_symbol.NOOP in 
  node attr

(** The name of the graph is the name of root *)
let name = Owl_graph.name 

(* Return an array of nodes *)
let iterate f (g : symbolic_graph) = iter_ancestors f [|g|]

let length (g : symbolic_graph) = Owl_graph.length [|g|]

(** Targeted operations on the graph *)

(** Pretty print a symbolic tree (not graph) to terminal *)
let print _g = ()

(** Derivative of symbolic tree *)
let derive _g = ()

(** Replace certain part of tree with anoter subtree *)
let replace _g _m _r = ()
