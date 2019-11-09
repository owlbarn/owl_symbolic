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

let iter f (g : symbolic_graph) =
  iter_ancestors ~order:DFS ~traversal:PostOrder f g.sym_nodes


let iter_print (g : symbolic_graph) =
  iter
    (fun sym_node ->
      let a = Owl_graph.attr sym_node in
      Printf.fprintf stderr "%s\n" (Owl_symbolic_symbol.name a))
    g


(* !!! notice the target is sym! *)

(* or `get_input_syms`? *)
let get_input_nodes sym_graph =
  (* get all the "symbol" nodes in sym_graph *)
  let inputs = ref [||] in
  iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let op_typ = Owl_symbolic_symbol.op_type sym in
      if op_typ = "Variable"
      then
        (* TODO: maybe need a copy of node instead of just node; 
         * it's about performance *)
        inputs := Array.append !inputs [| sym_node |])
    sym_graph;
  !inputs


let is_variable op_type = op_type = "Variable"

let get_output_nodes sym_graph =
  (* Assume only one output node in graph; note performance issue *)
  let root_node = sym_graph.sym_nodes.(0) in
  [| root_node |]


(** The name of node *)
let name sym_node =
  let sym = Owl_graph.attr sym_node in
  Owl_symbolic_symbol.name sym


let length (g : symbolic_graph) =
  let cnt = ref 0 in
  iter (fun _ -> cnt := !cnt + 1) g;
  !cnt


(** Targeted operations on the graph *)

(** Print a symbolic tree (not graph) to terminal *)
let to_dot _g = ()

(** Derivative of symbolic tree *)
let derive _g = ()

(** Replace certain part of tree with anoter subtree *)
let replace _g _m _r = ()
