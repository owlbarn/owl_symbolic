(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
open Owl_symbolic_types

module S = Owl_symbolic_symbol

type t = Onnx_types.graph_proto

let map_data_type_to_int32 typ =
  match typ with
  | SDT_Float -> Int32.of_int 1
  | SDT_Uint8 -> Int32.of_int 2
  | SDT_Int8 -> Int32.of_int 3
  | SDT_Uint16 -> Int32.of_int 4
  | SDT_Int16 -> Int32.of_int 5
  | SDT_Int32 -> Int32.of_int 6
  | SDT_Int64 -> Int32.of_int 7
  | SDT_String -> Int32.of_int 8
  | SDT_Bool -> Int32.of_int 9
  | SDT_Float16 -> Int32.of_int 10
  | SDT_Double -> Int32.of_int 11
  | SDT_Uint32 -> Int32.of_int 12
  | SDT_Uint64 -> Int32.of_int 13
  | SDT_Complex32 -> Int32.of_int 14
  | SDT_Complex64 -> Int32.of_int 15


(* DataType:  UNDEFINED = 0 *)

(* TODO: this still does not include all possible cases *)
let make_onnx_io name elt_type shape =
  let dim =
    Array.map
      (fun d ->
        let value = PT.Dim_value (Int64.of_int d) in
        PT.default_tensor_shape_proto_dimension ~value ())
      shape
    |> Array.to_list
  in
  let shape = Some (PT.default_tensor_shape_proto ~dim ()) in
  let type_proto_tensor = PT.default_type_proto_tensor ~shape ~elem_type:elt_type () in
  let value = PT.Tensor_type type_proto_tensor in
  let type_ = Some (PT.default_type_proto ~value ()) in
  PT.default_value_info_proto ~name ~type_ ()


let make_onnx_tensor_float f =
  let float_data = [ f ] in
  let dims = [] in
  let data_type = map_data_type_to_int32 SDT_Float in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let make_onnx_tensor_int i =
  let int32_data = [ Int32.of_int i ] in
  let dims = [] in
  let data_type = map_data_type_to_int32 SDT_Int32 in
  PT.default_tensor_proto ~dims ~int32_data ~data_type ()


let make_onnx_initializers_raw name data_type shape raw_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let segment = None in
  let doc_string = "" in
  PT.default_tensor_proto ~dims ~data_type ~segment ~name ~doc_string ~raw_data ()


let make_onnx_initializers_float _name _data_type _shape _float_data = ()
let make_onnx_initializers_int32 _name _data_type _shape _int32_data = ()

(** Create onnx attribute from the symbolic attribute *)
let make_onnx_attr (sym_attr : string * attrvalue) =
  let sym_attr_name, sym_attr_value = sym_attr in
  (* TODO: filter out attributes that should be ignored. *)
  let _ignored_attrs = [| "_class"; "_output" |] in
  match sym_attr_name with
  | "shape" ->
    let name = "shape" in
    (* it's "kernel_shape" in onnx conv *)
    let type_ = PT.Ints in
    let shape =
      get_attrvalue_shape sym_attr_value |> Array.map Int64.of_int |> Array.to_list
    in
    PT.default_attribute_proto ~name ~type_ ~ints:shape ()
  | "axis" ->
    let name = "axis" in
    let type_ = PT.Int in
    let value = get_attrvalue_int sym_attr_value |> Int64.of_int in
    PT.default_attribute_proto ~name ~type_ ~i:value ()
  | "float_value" ->
    let name = "value" in
    let (type_ : PT.attribute_proto_attribute_type) = PT.Tensor in
    let fvalue = get_attrvalue_float sym_attr_value in
    let tensor = Some (make_onnx_tensor_float fvalue) in
    PT.default_attribute_proto ~name ~type_ ~t:tensor ()
  | _ -> failwith ("make_onnx_attr: unsupported attr type: " ^ sym_attr_name)


let make_onnx_node op_type input_names output_names name attr =
  let input_names = Array.to_list input_names in
  PT.default_node_proto
    ~input:input_names
    ~output:output_names
    ~name
    ~op_type
    ~attribute:attr
    ()


let make_onnx_graph
    ?(name = "owl_sym_graph")
    (nodes : Onnx_types.node_proto array)
    (initializer_ : Onnx_types.tensor_proto array)
    (inputs : Onnx_types.value_info_proto array)
    (outputs : Onnx_types.value_info_proto array)
  =
  let node = Array.to_list nodes in
  let input = Array.to_list inputs in
  let output = Array.to_list outputs in
  let initializer_ = Array.to_list initializer_ in
  let doc_string = "owl symbolic graph" in
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
  (* NOTE: IR version and opset matters. *)
  let ir_version = Int64.of_int 6 in
  let producer_name = "owl" in
  let producer_version = "0.6.0" in
  let domain = "xyz.ocaml" in
  let model_version = Int64.of_int 0 in
  let doc_string = "owl-symbolic" in
  let graph = Some graph in
  let opset = PT.default_operator_set_id_proto ~version:(Int64.of_int 11) () in
  let opset_import = [ opset ] in
  let metadata_props = [] in
  PT.default_model_proto
    ~ir_version
    ~producer_name
    ~producer_version
    ~domain
    ~model_version
    ~doc_string
    ~opset_import
    ~metadata_props
    ~graph
    ()


let map_sym_optyp_to_onnx sym_optyp =
  match sym_optyp with
  | "Float" -> "Constant"
  | "Tensor" -> "Constant"
  | _ -> sym_optyp


(** Attributes scheme: https://github.com/onnx/onnx/blob/master/docs/Operators.md *)
let build_onnx_attrs sym =
  let onnx_attrs =
    match sym with
    | S.Float _ ->
      (* create "value" attribute for constant *)
      let name = "value" in
      let (type_ : PT.attribute_proto_attribute_type) = PT.Tensor in
      let v = S.float_value sym in
      let tensor = Some (make_onnx_tensor_float v) in
      let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
      [ a_value ]
    | S.Int _ ->
      (* create "value" attribute for constant *)
      let name = "value" in
      let (type_ : PT.attribute_proto_attribute_type) = PT.Tensor in
      let v = S.int_value sym in
      let tensor = Some (make_onnx_tensor_int v) in
      let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
      [ a_value ]
    | _ -> []
  in
  onnx_attrs


(** Core function. Converts symbolic nodes to onnx nodes. *)
let build_onnx_nodes (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  (* Not one-to-one projection *)
  let nodes = ref [||] in
  Owl_symbolic_graph.iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let op_type = S.op_type sym in
      if not (Owl_symbolic_graph.is_variable op_type)
      then (
        let name = S.name sym in
        let input_names = S.input sym in
        let output_names = [ name ] in
        let op_type = map_sym_optyp_to_onnx op_type in
        (* Build onnx attributes  *)
        let onnx_attrs = build_onnx_attrs sym in
        let n = make_onnx_node op_type input_names output_names name onnx_attrs in
        nodes := Array.append [| n |] !nodes))
    sym_graph;
  !nodes


let _build_io_value_info sym_node =
  let sym = Owl_graph.attr sym_node in
  let nodename = S.name sym in
  let attrs = S.sym_attrs sym in
  let elt_type = ref Int32.one in
  let _shape = ref (S.shape sym) in
  let shape = ref [| 2; 2 |] in
  Array.iter
    (fun (k, v) ->
      if k = "dtype"
      then
        elt_type := get_attrvalue_type v |> map_data_type_to_int32
        (*if k = "shape" then shape := T.get_attrvalue_shape v *))
    attrs;
  make_onnx_io nodename !elt_type !shape


let build_onnx_inputs sym_graph =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let nodename = S.name sym in
      let elt_type = Int32.one in
      (* assume only float dtype *)
      let shape = S.shape sym in
      make_onnx_io nodename elt_type shape)
    (Owl_symbolic_graph.get_input_nodes sym_graph)


let build_onnx_outputs sym_graph =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let nodename = S.name sym in
      (* default output valueinfoproto name to be "result" *)
      let elt_type = Int32.one in
      (* assume only float dtype *)
      let shape = S.get_out_shape sym in
      let shape =
        match shape with
        | Some s -> s
        | None   -> failwith "build_onnx_outputs: non specified output shape."
      in
      make_onnx_io nodename elt_type shape)
    (Owl_symbolic_graph.get_output_nodes sym_graph)


(** Main entry of conversion *)
let of_symbolic (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  (* Step 1: convert symbolic nodes to  *)
  let nodes = build_onnx_nodes sym_graph in
  (* Steps 1.x : more processing such as rewriting complex nodes *)

  (* Step 2: inpput/output  *)
  let inputs = build_onnx_inputs sym_graph in
  let outputs = build_onnx_outputs sym_graph in
  (* Step 3: initializers, corresponding to each input *)
  (* let initializer_ =
    Array.map
      (fun _sym_node ->
        let nodename = "" in
        let elt_type = Int32.of_int 1 in
        let shape = [||] in
        (* TODO: dummy data *)
        let raw_data = Bytes.of_string "" in
        make_onnx_initializers_raw nodename elt_type shape raw_data)
      (Owl_symbolic_graph.get_input_nodes sym_graph)
  in *)
  let initializer_ = [||] in
  (* Maybe some post-processing steps *)

  (* Final Step: make graph and model *)
  let graph = make_onnx_graph nodes initializer_ inputs outputs in
  make_onnx_model graph


let to_symbolic (_onnx_graph : t) = Owl_symbolic_graph.null_graph

let serialise (onnx_model : Onnx_types.model_proto) filename =
  let encoder = Pbrt.Encoder.create () in
  PB.encode_model_proto onnx_model encoder;
  let oc = open_out filename in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
