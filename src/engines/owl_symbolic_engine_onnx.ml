(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
module G = Owl_symbolic_graph
module S = Owl_symbolic_symbol
module T = Owl_symbolic_types

type t = Onnx_types.graph_proto

let map_data_type_to_int32 typ =
  match typ with
  | T.SDT_Float     -> Int32.of_int 1
  | T.SDT_Uint8     -> Int32.of_int 2
  | T.SDT_Int8      -> Int32.of_int 3
  | T.SDT_Uint16    -> Int32.of_int 4
  | T.SDT_Int16     -> Int32.of_int 5
  | T.SDT_Int32     -> Int32.of_int 6
  | T.SDT_Int64     -> Int32.of_int 7
  | T.SDT_String    -> Int32.of_int 8
  | T.SDT_Bool      -> Int32.of_int 9
  | T.SDT_Float16   -> Int32.of_int 10
  | T.SDT_Double    -> Int32.of_int 11
  | T.SDT_Uint32    -> Int32.of_int 12
  | T.SDT_Uint64    -> Int32.of_int 13
  | T.SDT_Complex32 -> Int32.of_int 14
  | T.SDT_Complex64 -> Int32.of_int 15


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
  let shape =
    if Array.length shape = 0
    then None
    else (
      let shape = PT.default_tensor_shape_proto ~dim () in
      Some shape)
  in
  let type_proto_tensor = PT.default_type_proto_tensor ~shape ~elem_type:elt_type () in
  let value = PT.Tensor_type type_proto_tensor in
  let type_ = Some (PT.default_type_proto ~value ()) in
  PT.default_value_info_proto ~name ~type_ ()


let make_onnx_initializers_raw name data_type shape raw_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let segment = None in
  let doc_string = "" in
  PT.default_tensor_proto ~dims ~data_type ~segment ~name ~doc_string ~raw_data ()


let make_onnx_initializers_float _name _data_type _shape _float_data = ()
let make_onnx_initializers_int32 _name _data_type _shape _int32_data = ()

(** Create onnx attribute from the symbolic attribute *)
let make_onnx_attr (sym_attr : string * T.attrvalue) =
  (* target attr: dtype, T, value *)
  let sym_attr_name, sym_attr_value = sym_attr in
  (* TODO: filter out attributes that should be ignored. *)
  let _ignored_attrs = [| "_class"; "_output" |] in
  match sym_attr_name with
  | "shape" ->
    let name = "shape" in
    (* it's "kernel_shape" in onnx conv *)
    let type_ = PT.Ints in
    let shape =
      T.get_attrvalue_shape sym_attr_value |> Array.map Int64.of_int |> Array.to_list
    in
    PT.default_attribute_proto ~name ~type_ ~ints:shape ()
  | "axis" ->
    let name = "axis" in
    let type_ = PT.Int in
    let value = T.get_attrvalue_int sym_attr_value |> Int64.of_int in
    PT.default_attribute_proto ~name ~type_ ~i:value ()
  | _ -> failwith ("make_onnx_attr: unsupported attr type: " ^ sym_attr_name)


let make_onnx_node op_type input_names output_names name attr =
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


(** Core function. Converts symbolic nodes to onnx nodes. *)
let sym_nodes_to_onnx (sym_nodes : G.symbolic_node array) =
  let nodes = Array.make (Array.length sym_nodes) (PT.default_node_proto ()) in
  (* Assume a one-to-one projection; might be changed later *)
  Array.iteri
    (fun i sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let input_names = S.input sym in
      let output_names = [ name ] in
      (* Attributes might be later adjusted in specific nodes. 
       * For example, "kernel_shape" is specific to onnx-conv, while in a symbolic node 
       * we could only have "shape"; or in a symbolic node we also have "foobar" attr, 
       * but we don't want that to be translated to onnx attribute.
       *)
      let sym_attrs = S.sym_attrs sym in
      let onnx_attrs = ref [] in
      Array.iter
        (fun sym_attr_pair ->
          (* match symbolic attribute and make onnx attribute *)
          let onnx_attr = make_onnx_attr sym_attr_pair in
          onnx_attrs := List.append !onnx_attrs [ onnx_attr ])
        sym_attrs;
      let onnx_attrs = !onnx_attrs in
      let typ = S.op_type sym in
      let op_type =
        match sym with
        | Float _ -> "Constant"
        | Int _   -> "Constant"
        | _       -> typ
      in
      let n = make_onnx_node op_type input_names output_names name onnx_attrs in
      nodes.(i) <- n)
    sym_nodes;
  nodes


let build_io_from_sym sym_node = 
  let sym = Owl_graph.attr sym_node in 
  let nodename = S.name sym in
  let attrs = S.sym_attrs sym in
  let elt_type = ref Int32.one  in
  let shape = ref [||] in 
  Array.iter (fun (k, v) -> 
    if k = "dtype" then 
      elt_type := T.get_attrvalue_type v |> map_data_type_to_int32 ;
    if k = "shape" then 
       shape := T.get_attrvalue_shape v
  ) attrs;
  make_onnx_io nodename !elt_type !shape

(** Main entry of conversion *)
let of_symbolic (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  (* Step 1: convert symbolic nodes to  *)
  let symnodes = sym_graph.sym_nodes in
  let nodes = sym_nodes_to_onnx symnodes in
  (* Steps 1.x : more processing such as rewriting complex nodes *)

  (* Step 2: inpput/output  *)

  (* required information: shape, element_type, node_name. *)
  let inputs = Array.map build_io_from_sym 
    (G.get_input_nodes sym_graph) in
  let outputs = Array.map build_io_from_sym 
    (G.get_output_nodes sym_graph)
  in
  (* Step 3: initializers, corresponding to each input *)
  let initializer_ =
    Array.map
      (fun _sym_node ->
        let nodename = "" in
        let elt_type = Int32.of_int 1 in
        let shape = [||] in
        (* TODO: dummy data *)
        let raw_data = Bytes.of_string "" in
        make_onnx_initializers_raw nodename elt_type shape raw_data)
      (G.get_input_nodes sym_graph)
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
