(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
open Owl_symbolic_types
module S = Owl_symbolic_symbol

type t = Onnx_types.model_proto

(** Mapping functions *)

let map_elt_type_to_int32 typ =
  match typ with
  | SNT_Noop      -> Int32.of_int 0
  | SNT_Float     -> Int32.of_int 1
  | SNT_Uint8     -> Int32.of_int 2
  | SNT_Int8      -> Int32.of_int 3
  | SNT_Uint16    -> Int32.of_int 4
  | SNT_Int16     -> Int32.of_int 5
  | SNT_Int32     -> Int32.of_int 6
  | SNT_Int64     -> Int32.of_int 7
  | SNT_String    -> Int32.of_int 8
  | SNT_Bool      -> Int32.of_int 9
  | SNT_Float16   -> Int32.of_int 10
  | SNT_Double    -> Int32.of_int 11
  | SNT_Uint32    -> Int32.of_int 12
  | SNT_Uint64    -> Int32.of_int 13
  | SNT_Complex32 -> Int32.of_int 14
  | SNT_Complex64 -> Int32.of_int 15
  | SNT_SEQ _     -> failwith "map_elt_type_to_int32: type is sequence"


let map_sym_optyp_to_onnx sym_optyp =
  match sym_optyp with
  | "Int"     -> "Constant"
  | "Float"   -> "Constant"
  | "Complex" -> "Constant"
  | "Tensor"  -> "Constant"
  | _         -> sym_optyp


(** Wrapper for building onnx-proto's *)

let make_onnx_tensor_floats ?(shape = [||]) fs =
  let float_data = Array.to_list fs in
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 SNT_Float) in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let make_onnx_tensor_ints ?(shape = [||]) i =
  let int32_data = Array.map Int32.of_int i |> Array.to_list in
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 SNT_Int32) in
  PT.default_tensor_proto ~dims ~int32_data ~data_type ()


let make_onnx_tensor_int64s ?(shape = [||]) i =
  let int64_data = Array.map Int64.of_int i |> Array.to_list in
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 SNT_Int64) in
  PT.default_tensor_proto ~dims ~int64_data ~data_type ()


let make_onnx_tensor_complex c =
  let r, i = c in
  let float_data = [ r; i ] in
  let dims = [] in
  let data_type = Some (map_elt_type_to_int32 SNT_Complex32) in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let _make_onnx_initializers_raw name data_type shape raw_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 data_type) in
  let name = Some name in
  let raw_data = Some raw_data in
  PT.default_tensor_proto ~dims ~data_type ~name ~raw_data ()


let make_onnx_initializers_float name data_type shape float_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 data_type) in
  let name = Some name in
  let float_data = Array.to_list float_data in
  PT.default_tensor_proto ~dims ~data_type ~name ~float_data ()


let make_onnx_initializers_int32 name data_type shape int_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 data_type) in
  let name = Some name in
  let int32_data = Array.map Int32.of_int int_data |> Array.to_list in
  PT.default_tensor_proto ~dims ~data_type ~name ~int32_data ()


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
  let type_proto_tensor =
    PT.default_type_proto_tensor ~shape ~elem_type:(Some elt_type) ()
  in
  let value = PT.Tensor_type type_proto_tensor in
  let type_ = Some (PT.default_type_proto ~value ()) in
  PT.default_value_info_proto ~name:(Some name) ~type_ ()


let make_onnx_node op_type input_names output_names name attr =
  let input_names = Array.to_list input_names in
  let output_names = Array.to_list output_names in
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
  let sparse_initializer = [] in
  let value_info = [] in
  let quantization_annotation = [] in
  PT.default_graph_proto
    ~name:(Some name)
    ~node
    ~initializer_
    ~input
    ~output
    ~sparse_initializer
    ~value_info
    ~quantization_annotation
    ()


let make_onnx_model graph =
  let ir_version = Some (Int64.of_int 6) in
  let producer_name = Some "owl" in
  let producer_version = Some "0.6.0" in
  let domain = Some "xyz.ocaml" in
  let model_version = Some (Int64.of_int 0) in
  let doc_string = Some "owl-symbolic" in
  let graph = Some graph in
  let opset = PT.default_operator_set_id_proto ~version:(Some (Int64.of_int 11)) () in
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


(** Functions to build part of onnx graph *)

let check_same types name =
  let flag = ref true in
  if Array.length types = 0
  then failwith "build_onnx_type_check: empty parents for non-input ops";
  Array.iter (fun t -> if t <> types.(0) then flag := false) types;
  if !flag = false
  then (
    let msg = Printf.sprintf "%s: inputs are of differnt type." name in
    raise (TYPE_CHECK msg))


let check_constraint t constraints name =
  if Array.mem t constraints = false
  then (
    let msg = Printf.sprintf "%s: input type not in constraints." name in
    raise (TYPE_CHECK msg))


(* This step performs type checking of the symbolic graph to see if it fits the ONNX operator schemas. 
 * Some things to note:
 *   + Both input and output types of each operator are array of number_type.
 *   + Type checking does not consider if the output is optional or not
 *   + The inferred output type of an operator is always unique. 
 *   + Do not change the structure of symgraph itself. 
 *   + This function just perform type checking, and thus returns nothing. 
 *   + There are 9 ONNX ops Sequence* that involves "seq(tensor)" types instead of tensor. Ignore them for now.
 *   + In the main body, I still check based on Symbolic node, not onnx nodes, this could be logically wrong.
 *)

let _types_constraint00 = [| SNT_Float; SNT_Float16; SNT_Double |]

let _types_constraint01 =
  [| SNT_Int8; SNT_Int16; SNT_Int32; SNT_Int64; SNT_Float16; SNT_Float; SNT_Double |]


let _types_constraint02 =
  [| SNT_Uint32; SNT_Uint64; SNT_Int32; SNT_Int64; SNT_Float16; SNT_Float; SNT_Double |]


let _types_constraint03 =
  [| SNT_Uint8
   ; SNT_Uint16
   ; SNT_Uint32
   ; SNT_Uint64
   ; SNT_Int8
   ; SNT_Int16
   ; SNT_Int32
   ; SNT_Int64
   ; SNT_Float16
   ; SNT_Float
   ; SNT_Double
   ; SNT_String
   ; SNT_Bool
   ; SNT_Complex32
   ; SNT_Complex64
  |]


let _types_constraint04 =
  [| SNT_Uint8
   ; SNT_Uint16
   ; SNT_Uint32
   ; SNT_Uint64
   ; SNT_Int8
   ; SNT_Int16
   ; SNT_Int32
   ; SNT_Int64
   ; SNT_Float16
   ; SNT_Float
   ; SNT_Double
  |]


let _types_constraint05 =
  [| SNT_Uint8
   ; SNT_Uint16
   ; SNT_Uint32
   ; SNT_Uint64
   ; SNT_Int8
   ; SNT_Int16
   ; SNT_Int32
   ; SNT_Int64
   ; SNT_Float16
   ; SNT_Float
   ; SNT_Double
   ; SNT_String
   ; SNT_Bool
  |]


let type_check_pattern00 sym = [| Owl_symbolic_symbol.dtype sym |]

let type_check_pattern01 target_type type_constraint name =
  check_constraint target_type.(0) type_constraint name;
  [| target_type.(0) |]


let type_check_pattern02 target_types type_constraint name =
  check_same target_types name;
  check_constraint target_types.(0).(0) type_constraint name;
  [| target_types.(0).(0) |]


let type_check_pattern03 target_types type_constraint0 type_constraint1 name =
  check_constraint target_types.(0).(0) type_constraint0 name;
  check_constraint target_types.(1).(0) type_constraint1 name;
  [| target_types.(0).(0) |]


let build_onnx_type_check (sym_graph : Owl_symbolic_graph.t) =
  let len = Owl_symbolic_graph.length sym_graph in
  let dtypes = Hashtbl.create len in
  Owl_symbolic_graph.topo_iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      (* Get input types *)
      let parents = Owl_graph.parents sym_node in
      let ptypes =
        Array.map
          (fun sym_node ->
            let s = Owl_graph.attr sym_node in
            match s with
            | Owl_symbolic_symbol.Float _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Int _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Tensor _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Complex _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Variable _ -> [| Owl_symbolic_symbol.dtype s |]
            | _ -> Hashtbl.find dtypes (S.name s))
          parents
      in
      (* Type checking *)
      let out_type =
        match sym with
        | Float _              -> type_check_pattern00 sym
        | Int _                -> type_check_pattern00 sym
        | Pi _                 -> type_check_pattern00 sym
        | Tensor _             -> type_check_pattern00 sym
        | Complex _            -> type_check_pattern00 sym
        | Variable _           -> type_check_pattern00 sym
        | RandomUniform _      ->
          let dt = Owl_symbolic_symbol.dtype sym in
          type_check_pattern01 [| dt |] _types_constraint00 name
        | Sin _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Cos _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Sqrt _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Exp _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Log _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Neg _                -> type_check_pattern01 ptypes.(0) _types_constraint01 name
        | Relu _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Add _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Sub _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Mul _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Div _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Pow _                -> type_check_pattern02 ptypes _types_constraint00 name
        | MatMul _             -> type_check_pattern02 ptypes _types_constraint02 name
        | Gemm _               -> type_check_pattern02 ptypes _types_constraint02 name
        | Max _                -> type_check_pattern02 ptypes _types_constraint00 name
        | Min _                -> type_check_pattern02 ptypes _types_constraint00 name
        | Sum _                -> type_check_pattern02 ptypes _types_constraint00 name
        | ReduceSum _          -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceMax _          -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | Reshape _            ->
          type_check_pattern03 ptypes _types_constraint03 [| SNT_Int64 |] name
        | Identity s           ->
          let idx = s.idx in
          [| ptypes.(0).(idx) |]
        | Split s              ->
          let n = Array.length s.split in
          let t = type_check_pattern01 ptypes.(0) _types_constraint03 name in
          Array.make n t.(0)
        | Concat _             -> type_check_pattern02 ptypes _types_constraint03 name
        | Pad _                ->
          let t = type_check_pattern01 ptypes.(0) _types_constraint04 name in
          type_check_pattern01 ptypes.(1) [| SNT_Int64 |] name |> ignore;
          if Array.length ptypes = 3
          then type_check_pattern01 ptypes.(2) _types_constraint04 name |> ignore;
          t
        | Conv _               -> type_check_pattern02 ptypes _types_constraint00 name
        | Cast x               ->
          type_check_pattern01 ptypes.(0) _types_constraint05 name |> ignore;
          let t = x.target in
          type_check_pattern01 [| t |] _types_constraint05 name
        | Squeeze _            -> type_check_pattern01 ptypes.(0) _types_constraint03 name
        | Tile _               ->
          type_check_pattern01 ptypes.(1) [| SNT_Int64 |] name |> ignore;
          type_check_pattern01 ptypes.(0) _types_constraint03 name
        | MaxPool _            ->
          let t1 = type_check_pattern01 ptypes.(0) _types_constraint00 name in
          let t2 = SNT_Int64 in
          [| t1.(0); t2 |]
        | BatchNormalization _ ->
          let t = type_check_pattern02 ptypes _types_constraint00 name in
          Array.make 5 t.(0)
        | Dropout _            ->
          let t = type_check_pattern01 ptypes.(0) _types_constraint00 name in
          [| t.(0); SNT_Bool |]
        | SequenceEmpty s      -> [| SNT_SEQ s.dtype |]
        | _                    -> [| SNT_Noop |]
      in
      Hashtbl.add dtypes name out_type)
    sym_graph;
  dtypes


(* Build ONNX attributions for each node 
 * Attributes scheme: https://github.com/onnx/onnx/blob/master/docs/Operators.md
 *)

let build_onnx_attrs_float sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.float_value sym in
  let tensor = Some (make_onnx_tensor_floats [| v |]) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_int sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.int_value sym in
  let tensor = Some (make_onnx_tensor_ints [| v |]) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_complex sym =
  let name = Some "value" in
  (* create "value" attribute for Constant *)
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.complex_value sym in
  let tensor = Some (make_onnx_tensor_complex v) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_pi _sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = Owl_const.pi in
  let tensor = Some (make_onnx_tensor_floats [| v |]) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_tensor sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.tensor_value sym in
  let tensor =
    match v.dtype with
    | SNT_Float ->
      let flts =
        match v.flt_val with
        | Some f -> f
        | None   -> [||]
      in
      Some (make_onnx_tensor_floats ~shape:v.shape flts)
    | SNT_Int32 ->
      let ints =
        match v.int_val with
        | Some i -> i
        | None   -> [||]
      in
      Some (make_onnx_tensor_ints ~shape:v.shape ints)
    | SNT_Int64 ->
      let ints =
        match v.int_val with
        | Some i -> i
        | None   -> [||]
      in
      Some (make_onnx_tensor_int64s ~shape:v.shape ints)
    | _         ->
      let t = Owl_symbolic_types.number_type_to_string v.dtype in
      let err_msg = Printf.sprintf "build_onnx_attrs: unsupported type: %s\n" t in
      failwith err_msg
  in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_randomuniform (x : Owl_symbolic_ops_generator.RandomUniform.t) =
  (* create "dtype" attribute *)
  let name_dtype = Some "dtype" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = x.dtype |> map_elt_type_to_int32 |> Int64.of_int32 in
  let attr_dtype = PT.default_attribute_proto ~name:name_dtype ~type_ ~i:(Some i) () in
  (* create "high" attribute *)
  let name_high = Some "high" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  let f = Some x.high in
  let attr_high = PT.default_attribute_proto ~name:name_high ~type_ ~f () in
  (* create "low" attribute *)
  let name_low = Some "low" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  let f = Some x.low in
  let attr_low = PT.default_attribute_proto ~name:name_low ~type_ ~f () in
  (* TODO: create "seed" attribute -- currently leave to ONNX *)
  (* create "shape" attribute *)
  let name_shape = Some "shape" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.shape |> Array.to_list in
  let attr_shape = PT.default_attribute_proto ~name:name_shape ~type_ ~ints () in
  [ attr_dtype; attr_high; attr_low; attr_shape ]


let build_onnx_attrs_gemm (x : Owl_symbolic_ops_math.Gemm.t) =
  (* create "alpha" attribute *)
  let name_alpha = Some "alpha" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  let f = Some x.alpha in
  let attr_alpha = PT.default_attribute_proto ~name:name_alpha ~type_ ~f () in
  (* create "beta" attribute *)
  let name_beta = Some "beta" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  let f = Some x.beta in
  let attr_beta = PT.default_attribute_proto ~name:name_beta ~type_ ~f () in
  (* create "transA" attribute *)
  let name_transA = Some "transA" in
  let type_ = Some PT.Int in
  let i = if x.transA then Int64.one else Int64.zero in
  let attr_transA = PT.default_attribute_proto ~name:name_transA ~type_ ~i:(Some i) () in
  (* create "transB" attribute *)
  let name_transB = Some "transB" in
  let type_ = Some PT.Int in
  let i = if x.transB then Int64.one else Int64.zero in
  let attr_transB = PT.default_attribute_proto ~name:name_transB ~type_ ~i:(Some i) () in
  [ attr_alpha; attr_beta; attr_transA; attr_transB ]


let build_onnx_attrs_reducesum (x : Owl_symbolic_ops_reduction.ReduceSum.t) =
  let name_axes = Some "axes" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.axes |> Array.to_list in
  let attr_axes = PT.default_attribute_proto ~name:name_axes ~type_ ~ints () in
  let name_keepdims = Some "keepdims" in
  let type_ = Some PT.Int in
  let i = if x.keepdims = true then Int64.one else Int64.zero in
  let attr_keepdims =
    PT.default_attribute_proto ~name:name_keepdims ~type_ ~i:(Some i) ()
  in
  [ attr_axes; attr_keepdims ]


let build_onnx_attrs_reducemax (x : Owl_symbolic_ops_reduction.ReduceMax.t) =
  let name_axes = Some "axes" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.axes |> Array.to_list in
  let attr_axes = PT.default_attribute_proto ~name:name_axes ~type_ ~ints () in
  let name_keepdims = Some "keepdims" in
  let type_ = Some PT.Int in
  let i = if x.keepdims = true then Int64.one else Int64.zero in
  let attr_keepdims =
    PT.default_attribute_proto ~name:name_keepdims ~type_ ~i:(Some i) ()
  in
  [ attr_axes; attr_keepdims ]


let build_onnx_attrs_split (x : Owl_symbolic_ops_tensor.Split.t) =
  let name_axis = Some "axis" in
  let type_ = Some PT.Int in
  let i = Int64.of_int x.axis in
  let attr_axis = PT.default_attribute_proto ~name:name_axis ~type_ ~i:(Some i) () in
  let name_split = Some "split" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.split |> Array.to_list in
  let attr_split = PT.default_attribute_proto ~name:name_split ~type_ ~ints () in
  [ attr_axis; attr_split ]


let build_onnx_attrs_concat (x : Owl_symbolic_ops_tensor.Concat.t) =
  let name_axis = Some "axis" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Int64.of_int x.axis in
  let attr_axis = PT.default_attribute_proto ~name:name_axis ~type_ ~i:(Some i) () in
  [ attr_axis ]


let build_onnx_attrs_pad (x : Owl_symbolic_ops_tensor.Pad.t) =
  let name_mode = Some "mode" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.String in
  let s = Some (Bytes.of_string x.mode) in
  let attr_mode = PT.default_attribute_proto ~name:name_mode ~type_ ~s () in
  [ attr_mode ]


let build_onnx_attrs_cast (x : Owl_symbolic_ops_tensor.Cast.t) =
  let name_to = Some "to" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Some (x.target |> map_elt_type_to_int32 |> Int64.of_int32) in
  let attr_to = PT.default_attribute_proto ~name:name_to ~type_ ~i () in
  [ attr_to ]


let build_onnx_attrs_squeeze (x : Owl_symbolic_ops_tensor.Squeeze.t) =
  match x.axes with
  | Some axes ->
    let name_axes = Some "axes" in
    let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
    let ints = Array.map Int64.of_int axes |> Array.to_list in
    let attr_axes = PT.default_attribute_proto ~name:name_axes ~type_ ~ints () in
    [ attr_axes ]
  | None      -> []


let build_onnx_attrs_conv (x : Owl_symbolic_ops_nn.Conv.t) =
  (* create "auto_pad" attribute *)
  let name_pad = Some "auto_pad" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.String in
  let s = Some (x.auto_pad |> Bytes.of_string) in
  let attr_pad = PT.default_attribute_proto ~name:name_pad ~type_ ~s () in
  (* create "dilations" attribute *)
  let name_dil = Some "dilations" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.dilations |> Array.to_list in
  let attr_dil = PT.default_attribute_proto ~name:name_dil ~type_ ~ints () in
  (* create "group" attribute *)
  let name_group = Some "group" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Some (Int64.of_int x.group) in
  let attr_group = PT.default_attribute_proto ~name:name_group ~type_ ~i () in
  (* create "strides"  attribute *)
  let name_strides = Some "strides" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.strides |> Array.to_list in
  let attr_strides = PT.default_attribute_proto ~name:name_strides ~type_ ~ints () in
  (* TODO: pads *)
  [ attr_pad; attr_dil; attr_group; attr_strides ]


let build_onnx_attrs_maxpool (x : Owl_symbolic_ops_nn.MaxPool.t) =
  (* create "auto_pad" attribute *)
  let name_pad = Some "auto_pad" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.String in
  let s = Some (x.auto_pad |> Bytes.of_string) in
  let attr_pad = PT.default_attribute_proto ~name:name_pad ~type_ ~s () in
  (* create "ceil_mode" attribute *)
  let name_ceil = Some "ceil_mode" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Some (Int64.of_int x.ceil_mode) in
  let attr_ceil = PT.default_attribute_proto ~name:name_ceil ~type_ ~i () in
  (* create "dilations" attribute *)
  let name_dil = Some "dilations" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.dilations |> Array.to_list in
  let attr_dil = PT.default_attribute_proto ~name:name_dil ~type_ ~ints () in
  (* create "kernel_shape"  attribute *)
  let name_kernel = Some "kernel_shape" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.kernel_shp |> Array.to_list in
  let attr_kernel = PT.default_attribute_proto ~name:name_kernel ~type_ ~ints () in
  (* create "strides"  attribute *)
  let name_strides = Some "strides" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int x.strides |> Array.to_list in
  let attr_strides = PT.default_attribute_proto ~name:name_strides ~type_ ~ints () in
  (* TODO: pads *)
  (* create "storage_order" attribute *)
  let name_order = Some "storage_order" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Some (Int64.of_int x.storage_order) in
  let attr_order = PT.default_attribute_proto ~name:name_order ~type_ ~i () in
  [ attr_pad; attr_ceil; attr_dil; attr_kernel; attr_strides; attr_order ]


let build_onnx_attrs_dropout (x : Owl_symbolic_ops_nn.Dropout.t) =
  let name_axis = Some "ratio" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  let f = Some x.ratio in
  let attr_axis = PT.default_attribute_proto ~name:name_axis ~type_ ~f () in
  [ attr_axis ]


let build_onnx_attrs_seq_empty (x : Owl_symbolic_ops_sequence.SequenceEmpty.t) =
  let name = Some "dtype" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Some (map_elt_type_to_int32 x.dtype |> Int64.of_int32) in
  let a_dtype = PT.default_attribute_proto ~name ~type_ ~i () in
  [ a_dtype ]


let build_onnx_attrs sym =
  let onnx_attrs =
    match sym with
    | S.Float _         -> build_onnx_attrs_float sym
    | S.Int _           -> build_onnx_attrs_int sym
    | S.Complex _       -> build_onnx_attrs_complex sym
    | S.Pi _            -> build_onnx_attrs_pi sym
    | S.Tensor _        -> build_onnx_attrs_tensor sym
    | S.RandomUniform x -> build_onnx_attrs_randomuniform x
    | S.Gemm x          -> build_onnx_attrs_gemm x
    | S.ReduceSum x     -> build_onnx_attrs_reducesum x
    | S.ReduceMax x     -> build_onnx_attrs_reducemax x
    | S.Split x         -> build_onnx_attrs_split x
    | S.Concat x        -> build_onnx_attrs_concat x
    | S.Pad x           -> build_onnx_attrs_pad x
    | S.Cast x          -> build_onnx_attrs_cast x
    | S.Squeeze x       -> build_onnx_attrs_squeeze x
    | S.Conv x          -> build_onnx_attrs_conv x
    | S.MaxPool x       -> build_onnx_attrs_maxpool x
    | S.SequenceEmpty x -> build_onnx_attrs_seq_empty x
    | S.Dropout x       -> build_onnx_attrs_dropout x
    | _                 -> []
  in
  onnx_attrs


(** Core function. Converts symbolic nodes to onnx nodes. *)
let build_onnx_nodes (sym_graph : Owl_symbolic_graph.t) =
  let nodes = ref [||] in
  Owl_symbolic_graph.topo_iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let op_type = S.op_type sym in
      (* Input variable does not belong to graph *)
      if not (Owl_symbolic_graph.is_variable op_type)
      then (
        let name = S.name sym in
        let input_names = S.input sym in
        let output_names = S.output sym in
        let op_type = Some (map_sym_optyp_to_onnx op_type) in
        (* Build onnx attributes  *)
        let onnx_attrs = build_onnx_attrs sym in
        let name = Some name in
        let n = make_onnx_node op_type input_names output_names name onnx_attrs in
        nodes := Array.append !nodes [| n |]))
    sym_graph;
  !nodes


let build_onnx_inputs sym_graph _type_dict =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let elt_type = S.dtype sym |> map_elt_type_to_int32 in
      let shape = S.shape sym in
      make_onnx_io name elt_type shape)
    (Owl_symbolic_graph.get_input_nodes sym_graph)


let build_onnx_outputs sym_graph type_dict =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let elt_type = (Hashtbl.find type_dict name).(0) |> map_elt_type_to_int32 in
      let shape = S.out_shape sym in
      let shape =
        match shape.(0) with
        | Some s -> s
        | None   -> failwith "build_onnx_outputs: non-specified output shape."
      in
      make_onnx_io name elt_type shape)
    (Owl_symbolic_graph.get_output_nodes sym_graph)


let build_onnx_initializers sym_graph =
  let inits = ref [||] in
  Array.iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let (init : tensor option) = S.initializer_ sym in
      match init with
      | Some init ->
        let dtype = init.dtype in
        let shape = init.shape in
        let init_tensor =
          match dtype with
          | SNT_Float ->
            let flt_val = init.flt_val in
            let flt_val =
              match flt_val with
              | Some f -> f
              | None   -> [||]
            in
            make_onnx_initializers_float name dtype shape flt_val
          | SNT_Int32 ->
            let int_val = init.int_val in
            let int_val =
              match int_val with
              | Some i -> i
              | None   -> [||]
            in
            make_onnx_initializers_int32 name dtype shape int_val
          | _         -> failwith "build_onnx_initializers: unsupported type"
        in
        inits := Array.append [| init_tensor |] !inits
      | None      -> ())
    (Owl_symbolic_graph.get_input_nodes sym_graph);
  !inits


(** Main entry of conversion to ONNX graph *)
let of_symbolic (sym_graph : Owl_symbolic_graph.t) =
  (* Step 0: walk through the sym_graph and check shapes *)
  let type_dict = build_onnx_type_check sym_graph in
  (* Step 1: convert symbolic nodes to  *)
  let nodes = build_onnx_nodes sym_graph in
  (* Steps 1.x : more processing such as rewriting complex nodes *)

  (* Step 2: inpput/output  *)
  let inputs = build_onnx_inputs sym_graph type_dict in
  let outputs = build_onnx_outputs sym_graph type_dict in
  (* Step 3: initializers, corresponding to each input *)
  let initializer_ = build_onnx_initializers sym_graph in
  (* Step N: Maybe some post-processing steps *)

  (* Final Step: make graph and model *)
  let graph = make_onnx_graph nodes initializer_ inputs outputs in
  make_onnx_model graph


(** Main entry of conversion from ONNX graph (dummy) *)
let to_symbolic (_onnx_graph : Onnx_types.model_proto) = Owl_symbolic_graph.null_graph

(** save an onnx model to Protobuf file *)
let save (onnx_model : Onnx_types.model_proto) filename =
  let encoder = Pbrt.Encoder.create () in
  PB.encode_model_proto onnx_model encoder;
  let oc = open_out filename in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc


(** load an onnx model from Protobuf file *)
let load _filename = Obj.magic None
