(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
module G = Owl_symbolic_graph
module S = Owl_symbolic_symbol
module T = Owl_symbolic_types

type t = Onnx_types.graph_proto

let data_type_map =   
  let dict = Hashtbl.create 20 in
  Hashtbl.add dict (T.SDT_Float) 1;
  Hashtbl.add dict (T.SDT_Uint8) 2;
  Hashtbl.add dict (T.SDT_Int8)  3;
  Hashtbl.add dict (T.SDT_Uint16) 4;
  Hashtbl.add dict (T.SDT_Int16) 5;
  Hashtbl.add dict (T.SDT_Int32) 6;
  Hashtbl.add dict (T.SDT_Int64) 7;
  Hashtbl.add dict (T.SDT_String) 8;
  Hashtbl.add dict (T.SDT_Bool) 9;
  Hashtbl.add dict (T.SDT_Float16) 10;
  Hashtbl.add dict (T.SDT_Double) 11;
  Hashtbl.add dict (T.SDT_Uint32) 12;
  Hashtbl.add dict (T.SDT_Uint64) 13;
  Hashtbl.add dict (T.SDT_Complex32) 14;
  Hashtbl.add dict (T.SDT_Complex64) 15;  
  dict


let map_data_type_to_int32 typ = 
  try
    Hashtbl.find data_type_map typ |> Int32.of_int 
  with Not_found -> 
    Int32.of_int 0 (* DataType:  UNDEFINED = 0 *)


(* TODO: this still does not include all possible cases *)
let make_onnx_io name elem_type shape =
  let dim = Array.map (fun d ->
      let value = PT.Dim_value (Int64.of_int d) in 
      PT.default_tensor_shape_proto_dimension ~value ()
    ) shape |> Array.to_list
  in
  let shape = if (Array.length shape = 0) then None else 
    let shape = PT.default_tensor_shape_proto ~dim () in  
    Some shape 
  in 
  let type_proto_tensor = PT.default_type_proto_tensor ~shape ~elem_type () in 
  let value = PT.Tensor_type type_proto_tensor in 
  let type_ = Some (PT.default_type_proto ~value ()) in 
  PT.default_value_info_proto ~name ~type_ ()


let make_onnx_initializers_raw name data_type shape raw_data = 
  let dims = Array.map Int64.of_int shape |> Array.to_list in 
  let segment = None in
  let doc_string = "" in
  PT.default_tensor_proto ~dims ~data_type 
    ~segment ~name ~doc_string
    ~raw_data 
    ()

let make_onnx_initializers_float _name _data_type _shape _float_data = ()


let make_onnx_initializers_int32 _name _data_type _shape _int32_data = ()


let make_onnx_attr () = 
  PT.default_attribute_proto ()

let make_onnx_node op_type input_names output_names name attr =
  PT.default_node_proto 
    ~input:input_names ~output:output_names ~name
     ~op_type ~attribute:attr ()

let make_onnx_graph ?(name="owl_sym_graph") 
  (nodes : Onnx_types.node_proto array) 
  (initializer_ : Onnx_types.tensor_proto array)
  (inputs  : Onnx_types.value_info_proto array) 
  (outputs : Onnx_types.value_info_proto array)
  =
  let node = Array.to_list nodes in
  let input = Array.to_list inputs in 
  let output = Array.to_list outputs in 
  let initializer_ = Array.to_list initializer_ in 
  let doc_string = "" in
  let sparse_initializer = [] in
  let value_info = [] in 
  let quantization_annotation = [] in
  PT.default_graph_proto
    ~name
    ~node
    ~initializer_
    ~input
    ~output 
    ~doc_string 
    ~sparse_initializer
    ~value_info
    ~quantization_annotation 
    ()


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
    (* Steps 1.x : more processing such as rewriting complex nodes *)

    (* Step 2: inpput/output  *)
    
    (* required information: shape, element_type, node_name. *)
    let inputs = Array.map (fun _sym_node ->
      let nodename = "" in
      let elem_type = Int32.of_int 1 in
      let shape = [||] in
      make_onnx_io nodename elem_type shape 
    ) (G.get_input_nodes sym_graph)
    in


    let outputs = Array.map (fun _sym_node ->
      let nodename = "" in
      let elem_type = Int32.of_int 1 in
      let shape = [||] in
      make_onnx_io nodename elem_type shape
    ) (G.get_output_nodes sym_graph)
    in

    (* Step 3: initializers, corresponding to each input *)

    let initializer_ = Array.map (fun _sym_node ->
      let nodename = "" in
      let elem_type = Int32.of_int 1 in
      let shape = [||] in
      (* TODO: dummy data *)
      let raw_data = Bytes.of_string "" in 
      make_onnx_initializers_raw nodename elem_type shape raw_data
    ) (G.get_input_nodes sym_graph)
    in

    (* Maybe some post-processing steps *)

    (* Final Step: make graph and model *)
    let graph = make_onnx_graph nodes initializer_ inputs outputs in 
    make_onnx_model graph


let to_symbolic (_onnx_graph : t) = G.null_graph


let serialise (onnx_model : Onnx_types.model_proto) filename =
  let encoder = Pbrt.Encoder.create () in
  PB.encode_model_proto onnx_model encoder;
  let oc = open_out filename in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc