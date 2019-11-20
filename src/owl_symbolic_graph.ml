(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_graph
open Owl_symbolic_types

type symbolic_node = Owl_symbolic_symbol.t Owl_graph.node

type t =
  { mutable sym_nodes : symbolic_node array
  ; mutable name : string
  ; mutable node_names : string array (* existing nodes in graph *)
  }

(** A series of graph operations. *)

let _debug_shape = true

let make_node (sym : Owl_symbolic_symbol.t) (parents : symbolic_node array) =
  let child = node sym in
  (* update the child's input and output shape *)
  if Array.length parents > 0
  then (
    let in_shapes =
      Array.map
        (fun sym_node -> Owl_graph.attr sym_node |> Owl_symbolic_symbol.out_shape)
        parents
    in
    let shape = Owl_symbolic_shape.infer_shape in_shapes sym in
    (* TODO: remove this part in product code *)
    if _debug_shape = true
    then (
      let foo =
        match shape.(0) with
        | Some s -> s
        | None   -> [||]
      in
      Owl_log.info
        "%s: %s\n"
        (Owl_symbolic_symbol.name sym)
        (Owl_utils_array.to_string string_of_int foo));
    Owl_symbolic_symbol.set_out_shape sym shape.(0)
    (* Currently only use the first shape *));
  connect_ancestors parents [| child |];
  let uniq_parents = Owl_utils_array.unique parents in
  Array.iter (fun parent -> connect_descendants [| parent |] [| child |]) uniq_parents;
  child


(* Create a symbolic graph; check duplicated names *)
let make_graph (nodes : symbolic_node array) name =
  let node_names = ref [||] in
  Owl_graph.iter_ancestors
    (fun n ->
      let x = Owl_graph.attr n |> Owl_symbolic_symbol.name in
      node_names := Array.append [| x |] !node_names)
    nodes;
  let node_names = !node_names in
  if Owl_symbolic_utils.check_uniq node_names = false
  then raise (INVALID_NAME "make_graph: the nodes contain duplicated names");
  { sym_nodes = nodes; name; node_names }


(* Topological sort *)
let iter f (g : t) = iter_ancestors ~order:DFS ~traversal:PostOrder f g.sym_nodes

(* get all the "variable" nodes in sym_graph *)
let get_input_nodes sym_graph =
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


(* Assume only one output node in graph; note performance issue *)
let get_output_nodes sym_graph =
  let root_node = sym_graph.sym_nodes.(0) in
  [| root_node |]


(** Utilities *)

let null_graph = make_graph [||] ""

let iter_print (g : t) =
  iter
    (fun sym_node ->
      let a = Owl_graph.attr sym_node in
      Printf.fprintf stderr "%s\n" (Owl_symbolic_symbol.name a))
    g


let is_variable op_type = op_type = "Variable"

let name sym_node =
  let sym = Owl_graph.attr sym_node in
  Owl_symbolic_symbol.name sym


let length (g : t) =
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
