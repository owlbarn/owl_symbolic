(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_graph

type symbolic_node = Owl_symbolic_symbol.t Owl_graph.node

type symbolic_graph =
  { sym_nodes : symbolic_node array
  ; graph_name : string
  }

(** A series of graph operations. *)

(** NOTE: I have Tree structure in mind when coding all these... *)
let make_node (sym : Owl_symbolic_symbol.t) (parents : symbolic_node array) =
  let child = node sym in
  connect_ancestors parents [| child |];
  let uniq_parents = Owl_utils_array.unique parents in
  Array.iter (fun parent -> connect_descendants [| parent |] [| child |]) uniq_parents;
  child


let null_graph =
  let attr = Owl_symbolic_symbol.NOOP in
  node attr


(** The name of the graph is the name of root *)
let name = Owl_graph.name

(* Return an array of nodes *)
let iter f (g : symbolic_node) = iter_ancestors f [| g |]
let length (g : symbolic_node) = Owl_graph.length [| g |]

(** Targeted operations on the graph *)

(** Pretty print a symbolic tree (not graph) to terminal *)
let print _g = ()

(** Derivative of symbolic tree *)
let derive _g = ()

(** Replace certain part of tree with anoter subtree *)
let replace _g _m _r = ()
