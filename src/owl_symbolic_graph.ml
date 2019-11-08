(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_graph

type symbolic_node = Owl_symbolic_symbol.t Owl_graph.node

type symbolic_graph =
  { mutable sym_nodes : symbolic_node array
  ; mutable name : string
  }

(** A series of graph operations. *)

let make_node (sym : Owl_symbolic_symbol.t) (parents : symbolic_node array) =
  let child = node sym in
  connect_ancestors parents [| child |];
  let uniq_parents = Owl_utils_array.unique parents in
  Array.iter (fun parent -> connect_descendants [| parent |] [| child |]) uniq_parents;
  child


let make_graph nodes name = { sym_nodes = nodes; name }
let null_graph = { sym_nodes = [||]; name = "" }

(* !!! notice the target is sym! *)


let get_input_nodes _sym_graph = [||]


let get_output_nodes _sym_graph = [||]



(** The name of node *)
let name sym_node =
  let sym = Owl_graph.attr sym_node in
  Owl_symbolic_symbol.name sym


let iter f (g : symbolic_graph) =
  iter_ancestors ~order:DFS ~traversal:PostOrder f g.sym_nodes


let length (g : symbolic_graph) = g.sym_nodes |> Owl_graph.length

(** Targeted operations on the graph *)

(** Print a symbolic tree (not graph) to terminal *)
let to_dot _g = ()

(** Derivative of symbolic tree *)
let derive _g = ()

(** Replace certain part of tree with anoter subtree *)
let replace _g _m _r = ()
