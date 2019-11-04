(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module G = Owl_computation_cpu_engine.Make (Owl_dense_ndarray.S)
open G

(* open Owl_graph *)

type t = G.graph

(** Helper function *)

let get_const_value (attr : Symbol.Shape.Type.attr) =
  if Array.length attr.value > 0
  then (
    let v = attr.value.(0) in
    if Device.is_elt v
    then Device.value_to_float v
    else failwith "Non-float value const not supported yet")
  else failwith "Non-value const"


(** Main entry *)

(* let to_symbolic (cgraph : t) =
  let outputs = G.get_outputs cgraph in
  (* 0th iterations: name each node *)
  iter_ancestors
    (fun node ->
      let name = Owl_graph.name node in
      let name =
        if name <> ""
        then name
        else (
          let id = Owl_graph.id node in
          Printf.sprintf "owlnode%d" id)
      in
      Owl_graph.set_name node name)
    outputs;
  (* 1st iteration : on owl_cgraph *)
  let sym_graph = ref Owl_symbolic_graph.null_graph in
  iter_ancestors
    (fun node ->
      let cgraph_attr : Symbol.Shape.Type.attr = Owl_graph.attr node in
      let name = Owl_graph.name node in
      let cgraph_inputs = Owl_graph.children node in
      let sym =
        match cgraph_attr.op with
        | Const    ->
          let value = get_const_value cgraph_attr in
          Owl_symbolic_operator.flt value
        (* This is WRONG!!! *)
        | Sin      -> 
          let inputs = find_node_in_array [||] cgraph_inputs in 
          Owl_symbolic_operator.sin inputs.(0)
        | Pow      -> Owl_symbolic_operator.pow !sym_graph !sym_graph
        | Var      -> Owl_symbolic_operator.symbol name
        | Ones shp -> Owl_symbolic_operator.tensor shp
        | _        -> failwith "not supported"
      in
      sym_graph := sym)
    outputs;
  !sym_graph


let of_symbolic (_sym_graph : symbolic_graph) =
  let attr =
    { op = Noop
    ; freeze = true
    ; reuse = false
    ; state = Valid
    ; shape = [||]
    ; value = [||]
    ; block = None
    }
  in
  Owl_graph.node attr


let eval_arr (sym_graph : symbolic_graph) =
  let cgraph_arr = of_symbolic sym_graph |> G.node_to_arr in
  G.eval_arr [| cgraph_arr |];
  G.unpack_arr cgraph_arr


let eval_elt (sym_graph : symbolic_graph) =
  let cgraph_elt = of_symbolic sym_graph |> G.node_to_elt in
  G.eval_elt [| cgraph_elt |];
  G.unpack_elt cgraph_elt
*)
