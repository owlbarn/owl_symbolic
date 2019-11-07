(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
module G = Owl_symbolic_graph
module S = Owl_symbolic_symbol

type t = Onnx_types.graph_proto

let make_onnx_attr () = 
  PT.default_attribute_proto ()

let make_onnx_node op_type input_names output_names name attr =
  PT.default_node_proto 
    ~input:input_names ~output:output_names ~name
     ~op_type ~attribute:attr ()

let make_onnx_graph (nodes : PT.node_proto array) (_output_names : string) =
  let nodes = Array.to_list nodes in
  PT.default_graph_proto ~node:nodes ()


let make_onnx_model graph =
  let ir_version = Int64.of_int 1 in 
  let producer_name = "owl" in
  let producer_version = "0.6.0" in 
  let domain = "xyz.ocaml" in 
  let model_version = Int64.of_int 0 in 
  let doc_string = "" in 
  let graph = Some graph in
  let opset_import = [] in 
  let metadata_props = [] in 
  PT.default_model_proto ~ir_version 
    ~producer_name ~producer_version 
    ~domain ~model_version ~doc_string
    ~opset_import ~metadata_props 
    ~graph
    ()

(** Core function. Converts symbolic nodes to onnx nodes. *)
let sym_nodes_to_onnx (sym_nodes : G.symbolic_node array) =

  let nodes = Array.make (Array.length sym_nodes) (PT.default_node_proto ()) in

  (* Assume a one-to-one projection; might be changed later *)
  Array.iteri (fun i sym_node -> 
    let sym = Owl_graph.attr sym_node in
    let sym_attrs = [|""|] in (* S.get_attrs sym in *)
    let onnx_attrs = ref [] in 
    Array.iter (fun _a -> 
    (* match symbolic attribute and make onnx attribute;
     * target attr: dtype, T, shape, value *)
      let a = make_onnx_attr () in 
      onnx_attrs := List.append !onnx_attrs [a]
    ) sym_attrs;
    let onnx_attrs = !onnx_attrs in

    let name = S.name sym in 
    let op_type = S.op_type sym in
    let input_names = S.input sym in 
    let output_names = S.output sym in (* output should be node names *)

    let n = make_onnx_node op_type input_names 
      output_names name onnx_attrs in 
    nodes.(i) <- n
  ) sym_nodes;
  nodes 


(** Main entry of conversion *)
let of_symbolic (sym_graph : Owl_symbolic_graph.symbolic_graph) = 
    (* Step 1: convert symbolic nodes to  *)
    let symnodes = sym_graph.sym_nodes in 
    let nodes = sym_nodes_to_onnx symnodes in 
    (* The inpput/output names should be specifiled *)
    let output_names = "" in 
    (* Steps 2- N: more processing such as rewriting complex nodes *)
    (* Final Step: make graph *)
    let graph = make_onnx_graph nodes output_names in 

    (* Maybe some post-processing steps *)
    make_onnx_model graph


let to_symbolic (_onnx_graph : t) = G.null_graph


let serialise (onnx_model : Onnx_types.model_proto) filename =
  let encoder = Pbrt.Encoder.create () in
  PB.encode_model_proto onnx_model encoder;
  let oc = open_out filename in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc