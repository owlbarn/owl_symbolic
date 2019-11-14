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
  | SDT_Noop      -> Int32.of_int 0 (* Undefined *)
  | SDT_Float     -> Int32.of_int 1
  | SDT_Uint8     -> Int32.of_int 2
  | SDT_Int8      -> Int32.of_int 3
  | SDT_Uint16    -> Int32.of_int 4
  | SDT_Int16     -> Int32.of_int 5
  | SDT_Int32     -> Int32.of_int 6
  | SDT_Int64     -> Int32.of_int 7
  | SDT_String    -> Int32.of_int 8
  | SDT_Bool      -> Int32.of_int 9
  | SDT_Float16   -> Int32.of_int 10
  | SDT_Double    -> Int32.of_int 11
  | SDT_Uint32    -> Int32.of_int 12
  | SDT_Uint64    -> Int32.of_int 13
  | SDT_Complex32 -> Int32.of_int 14
  | SDT_Complex64 -> Int32.of_int 15


let map_sym_optyp_to_onnx sym_optyp =
  match sym_optyp with
  | "Int"     -> "Constant"
  | "Float"   -> "Constant"
  | "Complex" -> "Constant"
  | "Tensor"  -> "Constant"
  | _         -> sym_optyp


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
  let data_type = Some (map_data_type_to_int32 SDT_Float) in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let make_onnx_tensor_int i =
  let int32_data = [ Int32.of_int i ] in
  let dims = [] in
  let data_type = Some (map_data_type_to_int32 SDT_Int32) in
  PT.default_tensor_proto ~dims ~int32_data ~data_type ()


(* For float and complex64 values
  Complex32 tensors are encoded as a single array of floats,
  with the real components appearing in odd numbered positions,
  and the corresponding imaginary component apparing in the
  subsequent even numbered position. (e.g., [1.0 + 2.0i, 3.0 + 4.0i]
  is encoded as [1.0, 2.0 ,3.0 ,4.0]
  When this field is present, the data_type field MUST be FLOAT or COMPLEX32.
*)
let make_onnx_tensor_complex c =
  let r, i = c in
  let float_data = [ r; i ] in
  let dims = [] in
  let data_type = Some (map_data_type_to_int32 SDT_Complex32) in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let make_onnx_initializers_raw name data_type shape raw_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_data_type_to_int32 data_type) in
  let name = Some name in
  let raw_data = Some raw_data in
  PT.default_tensor_proto ~dims ~data_type ~name ~raw_data ()


let make_onnx_initializers_float name data_type shape float_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_data_type_to_int32 data_type) in
  let name = Some name in
  let float_data = Array.to_list float_data in
  PT.default_tensor_proto ~dims ~data_type ~name ~float_data ()


let make_onnx_initializers_int32 name data_type shape int_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_data_type_to_int32 data_type) in
  let name = Some name in
  let int32_data = Array.map Int32.of_int int_data |> Array.to_list in
  PT.default_tensor_proto ~dims ~data_type ~name ~int32_data ()


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
  (* NOTE: IR version and opset do matter; check the doc *)
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


(* TODO: more useful information? *)
let _check_same types name =
  let flag = ref true in
  if Array.length types = 0
  then failwith "build_onnx_type_check: empty parents for non-input ops";
  Array.iter (fun t -> if t <> types.(0) then flag := false) types;
  if !flag = false
  then (
    let msg = Printf.sprintf "%s: inputs are of differnt type." name in
    raise (TYPE_CHECK msg))


let _check_constraint t constraints name =
  if Array.mem t constraints = false
  then (
    let msg = Printf.sprintf "%s: input type not in constraints." name in
    raise (TYPE_CHECK msg))


(* This step performs type checking of the symbolic graph to see if it fits the ONNX operator schemas. 
 * Some things to note:
 *   + Both input and output types of each operator are array of sym_data_type.
 *   + Type checking does not consider if the output is optional or not
 *   + The inferred output type of an operator is always unique. 
 *   + Do not change the structure of symgraph itself. 
 *   + This function just perform type checking, and thus returns nothing. 
 *   + There are 9 ONNX ops Sequence* that involves "seq(tensor)" types instead of tensor. Ignore them for now.
 *   + In the main body, I still check based on Symbolic node, not onnx nodes, this could be logically wrong.
 *)

let build_onnx_type_check (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  let len = Owl_symbolic_graph.length sym_graph in
  let dtypes = Hashtbl.create len in
  (* Assume this iter is topologically correct *)
  Owl_symbolic_graph.iter
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
            | Owl_symbolic_symbol.Float _ -> Owl_symbolic_symbol.dtype s
            | Owl_symbolic_symbol.Int _ -> Owl_symbolic_symbol.dtype s
            | Owl_symbolic_symbol.Tensor _ -> Owl_symbolic_symbol.dtype s
            | Owl_symbolic_symbol.Complex _ -> Owl_symbolic_symbol.dtype s
            | Owl_symbolic_symbol.Variable _ -> Owl_symbolic_symbol.dtype s
            | _ -> Hashtbl.find dtypes (S.name s))
          parents
      in
      (* Type checking *)
      let out_type =
        match sym with
        | Float _    -> Owl_symbolic_symbol.dtype sym
        | Int _      -> Owl_symbolic_symbol.dtype sym
        | Pi _       -> Owl_symbolic_symbol.dtype sym
        | Tensor _   -> Owl_symbolic_symbol.dtype sym
        | Complex _  -> Owl_symbolic_symbol.dtype sym
        | Variable _ -> Owl_symbolic_symbol.dtype sym
        | Sin _      ->
          _check_constraint ptypes.(0) [| SDT_Float; SDT_Float16; SDT_Double |] name;
          ptypes.(0)
        | Cos _      ->
          _check_constraint ptypes.(0) [| SDT_Float; SDT_Float16; SDT_Double |] name;
          ptypes.(0)
        | Exp _      ->
          _check_constraint ptypes.(0) [| SDT_Float; SDT_Float16; SDT_Double |] name;
          ptypes.(0)
        | Add _      ->
          _check_same ptypes name;
          let c =
            [| SDT_Uint32
             ; SDT_Uint64
             ; SDT_Int32
             ; SDT_Int64
             ; SDT_Float16
             ; SDT_Float
             ; SDT_Double
            |]
          in
          _check_constraint ptypes.(0) c name;
          ptypes.(0)
        | Sub _      ->
          _check_same ptypes name;
          let c =
            [| SDT_Uint32
             ; SDT_Uint64
             ; SDT_Int32
             ; SDT_Int64
             ; SDT_Float16
             ; SDT_Float
             ; SDT_Double
            |]
          in
          _check_constraint ptypes.(0) c name;
          ptypes.(0)
        | Mul _      ->
          _check_same ptypes name;
          let c =
            [| SDT_Uint32
             ; SDT_Uint64
             ; SDT_Int32
             ; SDT_Int64
             ; SDT_Float16
             ; SDT_Float
             ; SDT_Double
            |]
          in
          _check_constraint ptypes.(0) c name;
          ptypes.(0)
        | Div _      ->
          _check_same ptypes name;
          let c =
            [| SDT_Uint32
             ; SDT_Uint64
             ; SDT_Int32
             ; SDT_Int64
             ; SDT_Float16
             ; SDT_Float
             ; SDT_Double
            |]
          in
          _check_constraint ptypes.(0) c name;
          ptypes.(0)
        | Pow _      ->
          _check_same ptypes name;
          _check_constraint ptypes.(0) [| SDT_Float; SDT_Float16; SDT_Double |] name;
          ptypes.(0)
        | _          -> SDT_Noop
      in
      Hashtbl.add dtypes name out_type)
    sym_graph


(** Attributes scheme: https://github.com/onnx/onnx/blob/master/docs/Operators.md *)
let build_onnx_attrs sym =
  let onnx_attrs =
    match sym with
    | S.Float _   ->
      (* create "value" attribute for constant *)
      let name = Some "value" in
      let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
      let v = S.float_value sym in
      let tensor = Some (make_onnx_tensor_float v) in
      let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
      [ a_value ]
    | S.Int _     ->
      let name = Some "value" in
      let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
      let v = S.int_value sym in
      let tensor = Some (make_onnx_tensor_int v) in
      let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
      [ a_value ]
    | S.Complex _ ->
      let name = Some "value" in
      let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
      let v = S.complex_value sym in
      let tensor = Some (make_onnx_tensor_complex v) in
      let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
      [ a_value ]
    | S.Pi _      ->
      let name = Some "value" in
      let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
      let v = Owl_const.pi in
      let tensor = Some (make_onnx_tensor_float v) in
      let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
      [ a_value ]
    | _           -> []
  in
  onnx_attrs


(** Core function. Converts symbolic nodes to onnx nodes. *)
let build_onnx_nodes (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  (* NOTE: Nodes in a graph must be topologically sorted. *)
  let nodes = ref [||] in
  Owl_symbolic_graph.iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let op_type = S.op_type sym in
      (* input variable does not belong to graph *)
      if not (Owl_symbolic_graph.is_variable op_type)
      then (
        let name = S.name sym in
        let input_names = S.input sym in
        let output_names = [ name ] in
        let op_type = Some (map_sym_optyp_to_onnx op_type) in
        (* Build onnx attributes  *)
        let onnx_attrs = build_onnx_attrs sym in
        let name = Some name in
        let n = make_onnx_node op_type input_names output_names name onnx_attrs in
        nodes := Array.append !nodes [| n |]))
    sym_graph;
  !nodes


let build_onnx_inputs sym_graph =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let nodename = S.name sym in
      let elt_type = Int32.one in
      (* assume only float dtype *)
      let shape = S.shape sym in
      make_onnx_io (Some nodename) (Some elt_type) shape)
    (Owl_symbolic_graph.get_input_nodes sym_graph)


let build_onnx_outputs sym_graph =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let nodename = S.name sym in
      let elt_type = Int32.one in
      (* assume only float dtype *)
      let shape = S.get_out_shape sym in
      let shape =
        match shape with
        | Some s -> s
        | None   -> failwith "build_onnx_outputs: non specified output shape."
      in
      make_onnx_io (Some nodename) (Some elt_type) shape)
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
          | SDT_Float ->
            let flt_val = init.flt_val in
            let flt_val =
              match flt_val with
              | Some f -> f
              | None   -> [||]
            in
            make_onnx_initializers_float name dtype shape flt_val
          | SDT_Int32 ->
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


(** Main entry of conversion *)
let of_symbolic (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  (* Step 0: walk through the sym_graph and check shapes *)
  build_onnx_type_check sym_graph;
  (* Step 1: convert symbolic nodes to  *)
  let nodes = build_onnx_nodes sym_graph in
  (* Steps 1.x : more processing such as rewriting complex nodes *)

  (* Step 2: inpput/output  *)
  let inputs = build_onnx_inputs sym_graph in
  let outputs = build_onnx_outputs sym_graph in
  (* Step 3: initializers, corresponding to each input *)
  let initializer_ = build_onnx_initializers sym_graph in
  (* Step N: Maybe some post-processing steps *)

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
