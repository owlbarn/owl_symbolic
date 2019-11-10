[@@@ocaml.warning "-27-30-39"]

type tensor_proto_segment_mutable = {
  mutable begin_ : int64;
  mutable end_ : int64;
}

let default_tensor_proto_segment_mutable () : tensor_proto_segment_mutable = {
  begin_ = 0L;
  end_ = 0L;
}

type string_string_entry_proto_mutable = {
  mutable key : string;
  mutable value : string;
}

let default_string_string_entry_proto_mutable () : string_string_entry_proto_mutable = {
  key = "";
  value = "";
}

type tensor_proto_mutable = {
  mutable dims : int64 list;
  mutable data_type : int32;
  mutable segment : Onnx_types.tensor_proto_segment option;
  mutable float_data : float list;
  mutable int32_data : int32 list;
  mutable string_data : bytes list;
  mutable int64_data : int64 list;
  mutable name : string;
  mutable doc_string : string;
  mutable raw_data : bytes;
  mutable external_data : Onnx_types.string_string_entry_proto list;
  mutable data_location : Onnx_types.tensor_proto_data_location;
  mutable double_data : float list;
  mutable uint64_data : int64 list;
}

let default_tensor_proto_mutable () : tensor_proto_mutable = {
  dims = [];
  data_type = 0l;
  segment = None;
  float_data = [];
  int32_data = [];
  string_data = [];
  int64_data = [];
  name = "";
  doc_string = "";
  raw_data = Bytes.create 0;
  external_data = [];
  data_location = Onnx_types.default_tensor_proto_data_location ();
  double_data = [];
  uint64_data = [];
}

type sparse_tensor_proto_mutable = {
  mutable values : Onnx_types.tensor_proto option;
  mutable indices : Onnx_types.tensor_proto option;
  mutable dims : int64 list;
}

let default_sparse_tensor_proto_mutable () : sparse_tensor_proto_mutable = {
  values = None;
  indices = None;
  dims = [];
}

type tensor_shape_proto_dimension_mutable = {
  mutable value : Onnx_types.tensor_shape_proto_dimension_value;
  mutable denotation : string;
}

let default_tensor_shape_proto_dimension_mutable () : tensor_shape_proto_dimension_mutable = {
  value = Onnx_types.Dim_value (0L);
  denotation = "";
}

type tensor_shape_proto_mutable = {
  mutable dim : Onnx_types.tensor_shape_proto_dimension list;
}

let default_tensor_shape_proto_mutable () : tensor_shape_proto_mutable = {
  dim = [];
}

type type_proto_tensor_mutable = {
  mutable elem_type : int32;
  mutable shape : Onnx_types.tensor_shape_proto option;
}

let default_type_proto_tensor_mutable () : type_proto_tensor_mutable = {
  elem_type = 0l;
  shape = None;
}

type type_proto_mutable = {
  mutable value : Onnx_types.type_proto_value;
  mutable denotation : string;
}

let default_type_proto_mutable () : type_proto_mutable = {
  value = Onnx_types.Tensor_type (Onnx_types.default_type_proto_tensor ());
  denotation = "";
}

type type_proto_sequence_mutable = {
  mutable elem_type : Onnx_types.type_proto option;
}

let default_type_proto_sequence_mutable () : type_proto_sequence_mutable = {
  elem_type = None;
}

type type_proto_map_mutable = {
  mutable key_type : int32;
  mutable value_type : Onnx_types.type_proto option;
}

let default_type_proto_map_mutable () : type_proto_map_mutable = {
  key_type = 0l;
  value_type = None;
}

type value_info_proto_mutable = {
  mutable name : string;
  mutable type_ : Onnx_types.type_proto option;
  mutable doc_string : string;
}

let default_value_info_proto_mutable () : value_info_proto_mutable = {
  name = "";
  type_ = None;
  doc_string = "";
}

type tensor_annotation_mutable = {
  mutable tensor_name : string;
  mutable quant_parameter_tensor_names : Onnx_types.string_string_entry_proto list;
}

let default_tensor_annotation_mutable () : tensor_annotation_mutable = {
  tensor_name = "";
  quant_parameter_tensor_names = [];
}

type attribute_proto_mutable = {
  mutable name : string;
  mutable ref_attr_name : string;
  mutable doc_string : string;
  mutable type_ : Onnx_types.attribute_proto_attribute_type;
  mutable f : float;
  mutable i : int64;
  mutable s : bytes;
  mutable t : Onnx_types.tensor_proto option;
  mutable g : Onnx_types.graph_proto option;
  mutable sparse_tensor : Onnx_types.sparse_tensor_proto option;
  mutable floats : float list;
  mutable ints : int64 list;
  mutable strings : bytes list;
  mutable tensors : Onnx_types.tensor_proto list;
  mutable graphs : Onnx_types.graph_proto list;
  mutable sparse_tensors : Onnx_types.sparse_tensor_proto list;
}

let default_attribute_proto_mutable () : attribute_proto_mutable = {
  name = "";
  ref_attr_name = "";
  doc_string = "";
  type_ = Onnx_types.default_attribute_proto_attribute_type ();
  f = 0.;
  i = 0L;
  s = Bytes.create 0;
  t = None;
  g = None;
  sparse_tensor = None;
  floats = [];
  ints = [];
  strings = [];
  tensors = [];
  graphs = [];
  sparse_tensors = [];
}

type graph_proto_mutable = {
  mutable node : Onnx_types.node_proto list;
  mutable name : string;
  mutable initializer_ : Onnx_types.tensor_proto list;
  mutable sparse_initializer : Onnx_types.sparse_tensor_proto list;
  mutable doc_string : string;
  mutable input : Onnx_types.value_info_proto list;
  mutable output : Onnx_types.value_info_proto list;
  mutable value_info : Onnx_types.value_info_proto list;
  mutable quantization_annotation : Onnx_types.tensor_annotation list;
}

let default_graph_proto_mutable () : graph_proto_mutable = {
  node = [];
  name = "";
  initializer_ = [];
  sparse_initializer = [];
  doc_string = "";
  input = [];
  output = [];
  value_info = [];
  quantization_annotation = [];
}

type node_proto_mutable = {
  mutable input : string list;
  mutable output : string list;
  mutable name : string;
  mutable op_type : string;
  mutable domain : string;
  mutable attribute : Onnx_types.attribute_proto list;
  mutable doc_string : string;
}

let default_node_proto_mutable () : node_proto_mutable = {
  input = [];
  output = [];
  name = "";
  op_type = "";
  domain = "";
  attribute = [];
  doc_string = "";
}

type operator_set_id_proto_mutable = {
  mutable domain : string;
  mutable version : int64;
}

let default_operator_set_id_proto_mutable () : operator_set_id_proto_mutable = {
  domain = "";
  version = 0L;
}

type model_proto_mutable = {
  mutable ir_version : int64;
  mutable opset_import : Onnx_types.operator_set_id_proto list;
  mutable producer_name : string;
  mutable producer_version : string;
  mutable domain : string;
  mutable model_version : int64;
  mutable doc_string : string;
  mutable graph : Onnx_types.graph_proto option;
  mutable metadata_props : Onnx_types.string_string_entry_proto list;
}

let default_model_proto_mutable () : model_proto_mutable = {
  ir_version = 0L;
  opset_import = [];
  producer_name = "";
  producer_version = "";
  domain = "";
  model_version = 0L;
  doc_string = "";
  graph = None;
  metadata_props = [];
}


let rec decode_version d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Onnx_types.Start_version:Onnx_types.version)
  | 1 -> (Onnx_types.Ir_version_2017_10_10:Onnx_types.version)
  | 2 -> (Onnx_types.Ir_version_2017_10_30:Onnx_types.version)
  | 3 -> (Onnx_types.Ir_version_2017_11_3:Onnx_types.version)
  | 4 -> (Onnx_types.Ir_version_2019_1_22:Onnx_types.version)
  | 5 -> (Onnx_types.Ir_version_2019_3_18:Onnx_types.version)
  | 6 -> (Onnx_types.Ir_version:Onnx_types.version)
  | _ -> Pbrt.Decoder.malformed_variant "version"

let rec decode_attribute_proto_attribute_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Onnx_types.Undefined:Onnx_types.attribute_proto_attribute_type)
  | 1 -> (Onnx_types.Float:Onnx_types.attribute_proto_attribute_type)
  | 2 -> (Onnx_types.Int:Onnx_types.attribute_proto_attribute_type)
  | 3 -> (Onnx_types.String:Onnx_types.attribute_proto_attribute_type)
  | 4 -> (Onnx_types.Tensor:Onnx_types.attribute_proto_attribute_type)
  | 5 -> (Onnx_types.Graph:Onnx_types.attribute_proto_attribute_type)
  | 11 -> (Onnx_types.Sparse_tensor:Onnx_types.attribute_proto_attribute_type)
  | 6 -> (Onnx_types.Floats:Onnx_types.attribute_proto_attribute_type)
  | 7 -> (Onnx_types.Ints:Onnx_types.attribute_proto_attribute_type)
  | 8 -> (Onnx_types.Strings:Onnx_types.attribute_proto_attribute_type)
  | 9 -> (Onnx_types.Tensors:Onnx_types.attribute_proto_attribute_type)
  | 10 -> (Onnx_types.Graphs:Onnx_types.attribute_proto_attribute_type)
  | 12 -> (Onnx_types.Sparse_tensors:Onnx_types.attribute_proto_attribute_type)
  | _ -> Pbrt.Decoder.malformed_variant "attribute_proto_attribute_type"

let rec decode_tensor_proto_segment d =
  let v = default_tensor_proto_segment_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.begin_ <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto_segment), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.end_ <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto_segment), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.begin_ = v.begin_;
    Onnx_types.end_ = v.end_;
  } : Onnx_types.tensor_proto_segment)

let rec decode_string_string_entry_proto d =
  let v = default_string_string_entry_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_string_entry_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(string_string_entry_proto), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.key = v.key;
    Onnx_types.value = v.value;
  } : Onnx_types.string_string_entry_proto)

let rec decode_tensor_proto_data_location d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Onnx_types.Default:Onnx_types.tensor_proto_data_location)
  | 1 -> (Onnx_types.External:Onnx_types.tensor_proto_data_location)
  | _ -> Pbrt.Decoder.malformed_variant "tensor_proto_data_location"

let rec decode_tensor_proto d =
  let v = default_tensor_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.uint64_data <- List.rev v.uint64_data;
      v.double_data <- List.rev v.double_data;
      v.external_data <- List.rev v.external_data;
      v.int64_data <- List.rev v.int64_data;
      v.string_data <- List.rev v.string_data;
      v.int32_data <- List.rev v.int32_data;
      v.float_data <- List.rev v.float_data;
      v.dims <- List.rev v.dims;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.dims <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.data_type <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.segment <- Some (decode_tensor_proto_segment (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.float_data <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits32 d)::l) [] d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.int32_data <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.string_data <- (Pbrt.Decoder.bytes d) :: v.string_data;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.int64_data <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(8)" pk
    | Some (12, Pbrt.Bytes) -> begin
      v.doc_string <- Pbrt.Decoder.string d;
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(12)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.raw_data <- Pbrt.Decoder.bytes d;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(9)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.external_data <- (decode_string_string_entry_proto (Pbrt.Decoder.nested d)) :: v.external_data;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(13)" pk
    | Some (14, Pbrt.Varint) -> begin
      v.data_location <- decode_tensor_proto_data_location d;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(14)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.double_data <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.uint64_data <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(11)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.dims = v.dims;
    Onnx_types.data_type = v.data_type;
    Onnx_types.segment = v.segment;
    Onnx_types.float_data = v.float_data;
    Onnx_types.int32_data = v.int32_data;
    Onnx_types.string_data = v.string_data;
    Onnx_types.int64_data = v.int64_data;
    Onnx_types.name = v.name;
    Onnx_types.doc_string = v.doc_string;
    Onnx_types.raw_data = v.raw_data;
    Onnx_types.external_data = v.external_data;
    Onnx_types.data_location = v.data_location;
    Onnx_types.double_data = v.double_data;
    Onnx_types.uint64_data = v.uint64_data;
  } : Onnx_types.tensor_proto)

let rec decode_sparse_tensor_proto d =
  let v = default_sparse_tensor_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.dims <- List.rev v.dims;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.values <- Some (decode_tensor_proto (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sparse_tensor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.indices <- Some (decode_tensor_proto (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sparse_tensor_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.dims <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sparse_tensor_proto), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.values = v.values;
    Onnx_types.indices = v.indices;
    Onnx_types.dims = v.dims;
  } : Onnx_types.sparse_tensor_proto)

let rec decode_tensor_shape_proto_dimension_value d = 
  let rec loop () = 
    let ret:Onnx_types.tensor_shape_proto_dimension_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "tensor_shape_proto_dimension_value"
      | Some (1, _) -> (Onnx_types.Dim_value (Pbrt.Decoder.int64_as_varint d) : Onnx_types.tensor_shape_proto_dimension_value) 
      | Some (2, _) -> (Onnx_types.Dim_param (Pbrt.Decoder.string d) : Onnx_types.tensor_shape_proto_dimension_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_tensor_shape_proto_dimension d =
  let v = default_tensor_shape_proto_dimension_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.value <- Onnx_types.Dim_value (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto_dimension), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- Onnx_types.Dim_param (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto_dimension), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.denotation <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto_dimension), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.value = v.value;
    Onnx_types.denotation = v.denotation;
  } : Onnx_types.tensor_shape_proto_dimension)

let rec decode_tensor_shape_proto d =
  let v = default_tensor_shape_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.dim <- List.rev v.dim;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.dim <- (decode_tensor_shape_proto_dimension (Pbrt.Decoder.nested d)) :: v.dim;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.dim = v.dim;
  } : Onnx_types.tensor_shape_proto)

let rec decode_type_proto_tensor d =
  let v = default_type_proto_tensor_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.elem_type <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto_tensor), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.shape <- Some (decode_tensor_shape_proto (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto_tensor), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.elem_type = v.elem_type;
    Onnx_types.shape = v.shape;
  } : Onnx_types.type_proto_tensor)

let rec decode_type_proto_value d = 
  let rec loop () = 
    let ret:Onnx_types.type_proto_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "type_proto_value"
      | Some (1, _) -> (Onnx_types.Tensor_type (decode_type_proto_tensor (Pbrt.Decoder.nested d)) : Onnx_types.type_proto_value) 
      | Some (4, _) -> (Onnx_types.Sequence_type (decode_type_proto_sequence (Pbrt.Decoder.nested d)) : Onnx_types.type_proto_value) 
      | Some (5, _) -> (Onnx_types.Map_type (decode_type_proto_map (Pbrt.Decoder.nested d)) : Onnx_types.type_proto_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_type_proto d =
  let v = default_type_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- Onnx_types.Tensor_type (decode_type_proto_tensor (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto), field(1)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.value <- Onnx_types.Sequence_type (decode_type_proto_sequence (Pbrt.Decoder.nested d));
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.value <- Onnx_types.Map_type (decode_type_proto_map (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.denotation <- Pbrt.Decoder.string d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.value = v.value;
    Onnx_types.denotation = v.denotation;
  } : Onnx_types.type_proto)

and decode_type_proto_sequence d =
  let v = default_type_proto_sequence_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.elem_type <- Some (decode_type_proto (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto_sequence), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.elem_type = v.elem_type;
  } : Onnx_types.type_proto_sequence)

and decode_type_proto_map d =
  let v = default_type_proto_map_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.key_type <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto_map), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value_type <- Some (decode_type_proto (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(type_proto_map), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.key_type = v.key_type;
    Onnx_types.value_type = v.value_type;
  } : Onnx_types.type_proto_map)

let rec decode_value_info_proto d =
  let v = default_value_info_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(value_info_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.type_ <- Some (decode_type_proto (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(value_info_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.doc_string <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(value_info_proto), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.name = v.name;
    Onnx_types.type_ = v.type_;
    Onnx_types.doc_string = v.doc_string;
  } : Onnx_types.value_info_proto)

let rec decode_tensor_annotation d =
  let v = default_tensor_annotation_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.quant_parameter_tensor_names <- List.rev v.quant_parameter_tensor_names;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.tensor_name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_annotation), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.quant_parameter_tensor_names <- (decode_string_string_entry_proto (Pbrt.Decoder.nested d)) :: v.quant_parameter_tensor_names;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_annotation), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.tensor_name = v.tensor_name;
    Onnx_types.quant_parameter_tensor_names = v.quant_parameter_tensor_names;
  } : Onnx_types.tensor_annotation)

let rec decode_attribute_proto d =
  let v = default_attribute_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.sparse_tensors <- List.rev v.sparse_tensors;
      v.graphs <- List.rev v.graphs;
      v.tensors <- List.rev v.tensors;
      v.strings <- List.rev v.strings;
      v.ints <- List.rev v.ints;
      v.floats <- List.rev v.floats;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(1)" pk
    | Some (21, Pbrt.Bytes) -> begin
      v.ref_attr_name <- Pbrt.Decoder.string d;
    end
    | Some (21, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(21)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.doc_string <- Pbrt.Decoder.string d;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(13)" pk
    | Some (20, Pbrt.Varint) -> begin
      v.type_ <- decode_attribute_proto_attribute_type d;
    end
    | Some (20, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(20)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.f <- Pbrt.Decoder.float_as_bits32 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.i <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.s <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.t <- Some (decode_tensor_proto (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.g <- Some (decode_graph_proto (Pbrt.Decoder.nested d));
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(6)" pk
    | Some (22, Pbrt.Bytes) -> begin
      v.sparse_tensor <- Some (decode_sparse_tensor_proto (Pbrt.Decoder.nested d));
    end
    | Some (22, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(22)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.floats <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits32 d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.ints <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.strings <- (Pbrt.Decoder.bytes d) :: v.strings;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.tensors <- (decode_tensor_proto (Pbrt.Decoder.nested d)) :: v.tensors;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.graphs <- (decode_graph_proto (Pbrt.Decoder.nested d)) :: v.graphs;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(11)" pk
    | Some (23, Pbrt.Bytes) -> begin
      v.sparse_tensors <- (decode_sparse_tensor_proto (Pbrt.Decoder.nested d)) :: v.sparse_tensors;
    end
    | Some (23, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(attribute_proto), field(23)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.name = v.name;
    Onnx_types.ref_attr_name = v.ref_attr_name;
    Onnx_types.doc_string = v.doc_string;
    Onnx_types.type_ = v.type_;
    Onnx_types.f = v.f;
    Onnx_types.i = v.i;
    Onnx_types.s = v.s;
    Onnx_types.t = v.t;
    Onnx_types.g = v.g;
    Onnx_types.sparse_tensor = v.sparse_tensor;
    Onnx_types.floats = v.floats;
    Onnx_types.ints = v.ints;
    Onnx_types.strings = v.strings;
    Onnx_types.tensors = v.tensors;
    Onnx_types.graphs = v.graphs;
    Onnx_types.sparse_tensors = v.sparse_tensors;
  } : Onnx_types.attribute_proto)

and decode_graph_proto d =
  let v = default_graph_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.quantization_annotation <- List.rev v.quantization_annotation;
      v.value_info <- List.rev v.value_info;
      v.output <- List.rev v.output;
      v.input <- List.rev v.input;
      v.sparse_initializer <- List.rev v.sparse_initializer;
      v.initializer_ <- List.rev v.initializer_;
      v.node <- List.rev v.node;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.node <- (decode_node_proto (Pbrt.Decoder.nested d)) :: v.node;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(2)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.initializer_ <- (decode_tensor_proto (Pbrt.Decoder.nested d)) :: v.initializer_;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(5)" pk
    | Some (15, Pbrt.Bytes) -> begin
      v.sparse_initializer <- (decode_sparse_tensor_proto (Pbrt.Decoder.nested d)) :: v.sparse_initializer;
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(15)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.doc_string <- Pbrt.Decoder.string d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.input <- (decode_value_info_proto (Pbrt.Decoder.nested d)) :: v.input;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(11)" pk
    | Some (12, Pbrt.Bytes) -> begin
      v.output <- (decode_value_info_proto (Pbrt.Decoder.nested d)) :: v.output;
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.value_info <- (decode_value_info_proto (Pbrt.Decoder.nested d)) :: v.value_info;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(13)" pk
    | Some (14, Pbrt.Bytes) -> begin
      v.quantization_annotation <- (decode_tensor_annotation (Pbrt.Decoder.nested d)) :: v.quantization_annotation;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(graph_proto), field(14)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.node = v.node;
    Onnx_types.name = v.name;
    Onnx_types.initializer_ = v.initializer_;
    Onnx_types.sparse_initializer = v.sparse_initializer;
    Onnx_types.doc_string = v.doc_string;
    Onnx_types.input = v.input;
    Onnx_types.output = v.output;
    Onnx_types.value_info = v.value_info;
    Onnx_types.quantization_annotation = v.quantization_annotation;
  } : Onnx_types.graph_proto)

and decode_node_proto d =
  let v = default_node_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attribute <- List.rev v.attribute;
      v.output <- List.rev v.output;
      v.input <- List.rev v.input;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.input <- (Pbrt.Decoder.string d) :: v.input;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.output <- (Pbrt.Decoder.string d) :: v.output;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.op_type <- Pbrt.Decoder.string d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(4)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.domain <- Pbrt.Decoder.string d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(7)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.attribute <- (decode_attribute_proto (Pbrt.Decoder.nested d)) :: v.attribute;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.doc_string <- Pbrt.Decoder.string d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(node_proto), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.input = v.input;
    Onnx_types.output = v.output;
    Onnx_types.name = v.name;
    Onnx_types.op_type = v.op_type;
    Onnx_types.domain = v.domain;
    Onnx_types.attribute = v.attribute;
    Onnx_types.doc_string = v.doc_string;
  } : Onnx_types.node_proto)

let rec decode_operator_set_id_proto d =
  let v = default_operator_set_id_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.domain <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(operator_set_id_proto), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.version <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(operator_set_id_proto), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.domain = v.domain;
    Onnx_types.version = v.version;
  } : Onnx_types.operator_set_id_proto)

let rec decode_model_proto d =
  let v = default_model_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.metadata_props <- List.rev v.metadata_props;
      v.opset_import <- List.rev v.opset_import;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.ir_version <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(1)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.opset_import <- (decode_operator_set_id_proto (Pbrt.Decoder.nested d)) :: v.opset_import;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(8)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.producer_name <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.producer_version <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.domain <- Pbrt.Decoder.string d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.model_version <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.doc_string <- Pbrt.Decoder.string d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.graph <- Some (decode_graph_proto (Pbrt.Decoder.nested d));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(7)" pk
    | Some (14, Pbrt.Bytes) -> begin
      v.metadata_props <- (decode_string_string_entry_proto (Pbrt.Decoder.nested d)) :: v.metadata_props;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(model_proto), field(14)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Onnx_types.ir_version = v.ir_version;
    Onnx_types.opset_import = v.opset_import;
    Onnx_types.producer_name = v.producer_name;
    Onnx_types.producer_version = v.producer_version;
    Onnx_types.domain = v.domain;
    Onnx_types.model_version = v.model_version;
    Onnx_types.doc_string = v.doc_string;
    Onnx_types.graph = v.graph;
    Onnx_types.metadata_props = v.metadata_props;
  } : Onnx_types.model_proto)

let rec decode_tensor_proto_data_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Onnx_types.Undefined:Onnx_types.tensor_proto_data_type)
  | 1 -> (Onnx_types.Float:Onnx_types.tensor_proto_data_type)
  | 2 -> (Onnx_types.Uint8:Onnx_types.tensor_proto_data_type)
  | 3 -> (Onnx_types.Int8:Onnx_types.tensor_proto_data_type)
  | 4 -> (Onnx_types.Uint16:Onnx_types.tensor_proto_data_type)
  | 5 -> (Onnx_types.Int16:Onnx_types.tensor_proto_data_type)
  | 6 -> (Onnx_types.Int32:Onnx_types.tensor_proto_data_type)
  | 7 -> (Onnx_types.Int64:Onnx_types.tensor_proto_data_type)
  | 8 -> (Onnx_types.String:Onnx_types.tensor_proto_data_type)
  | 9 -> (Onnx_types.Bool:Onnx_types.tensor_proto_data_type)
  | 10 -> (Onnx_types.Float16:Onnx_types.tensor_proto_data_type)
  | 11 -> (Onnx_types.Double:Onnx_types.tensor_proto_data_type)
  | 12 -> (Onnx_types.Uint32:Onnx_types.tensor_proto_data_type)
  | 13 -> (Onnx_types.Uint64:Onnx_types.tensor_proto_data_type)
  | 14 -> (Onnx_types.Complex64:Onnx_types.tensor_proto_data_type)
  | 15 -> (Onnx_types.Complex128:Onnx_types.tensor_proto_data_type)
  | 16 -> (Onnx_types.Bfloat16:Onnx_types.tensor_proto_data_type)
  | _ -> Pbrt.Decoder.malformed_variant "tensor_proto_data_type"

let rec encode_version (v:Onnx_types.version) encoder =
  match v with
  | Onnx_types.Start_version -> Pbrt.Encoder.int_as_varint (0) encoder
  | Onnx_types.Ir_version_2017_10_10 -> Pbrt.Encoder.int_as_varint 1 encoder
  | Onnx_types.Ir_version_2017_10_30 -> Pbrt.Encoder.int_as_varint 2 encoder
  | Onnx_types.Ir_version_2017_11_3 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Onnx_types.Ir_version_2019_1_22 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Onnx_types.Ir_version_2019_3_18 -> Pbrt.Encoder.int_as_varint 5 encoder
  | Onnx_types.Ir_version -> Pbrt.Encoder.int_as_varint 6 encoder

let rec encode_attribute_proto_attribute_type (v:Onnx_types.attribute_proto_attribute_type) encoder =
  match v with
  | Onnx_types.Undefined -> Pbrt.Encoder.int_as_varint (0) encoder
  | Onnx_types.Float -> Pbrt.Encoder.int_as_varint 1 encoder
  | Onnx_types.Int -> Pbrt.Encoder.int_as_varint 2 encoder
  | Onnx_types.String -> Pbrt.Encoder.int_as_varint 3 encoder
  | Onnx_types.Tensor -> Pbrt.Encoder.int_as_varint 4 encoder
  | Onnx_types.Graph -> Pbrt.Encoder.int_as_varint 5 encoder
  | Onnx_types.Sparse_tensor -> Pbrt.Encoder.int_as_varint 11 encoder
  | Onnx_types.Floats -> Pbrt.Encoder.int_as_varint 6 encoder
  | Onnx_types.Ints -> Pbrt.Encoder.int_as_varint 7 encoder
  | Onnx_types.Strings -> Pbrt.Encoder.int_as_varint 8 encoder
  | Onnx_types.Tensors -> Pbrt.Encoder.int_as_varint 9 encoder
  | Onnx_types.Graphs -> Pbrt.Encoder.int_as_varint 10 encoder
  | Onnx_types.Sparse_tensors -> Pbrt.Encoder.int_as_varint 12 encoder

let rec encode_tensor_proto_segment (v:Onnx_types.tensor_proto_segment) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Onnx_types.begin_ encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Onnx_types.end_ encoder;
  ()

let rec encode_string_string_entry_proto (v:Onnx_types.string_string_entry_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.key encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.value encoder;
  ()

let rec encode_tensor_proto_data_location (v:Onnx_types.tensor_proto_data_location) encoder =
  match v with
  | Onnx_types.Default -> Pbrt.Encoder.int_as_varint (0) encoder
  | Onnx_types.External -> Pbrt.Encoder.int_as_varint 1 encoder

let rec encode_tensor_proto (v:Onnx_types.tensor_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Onnx_types.dims;
  ) encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Onnx_types.data_type encoder;
  begin match v.Onnx_types.segment with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto_segment x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits32 x encoder;
    ) v.Onnx_types.float_data;
  ) encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int32_as_varint x encoder;
    ) v.Onnx_types.int32_data;
  ) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  ) v.Onnx_types.string_data;
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Onnx_types.int64_data;
  ) encoder;
  Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.name encoder;
  Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.doc_string encoder;
  Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Onnx_types.raw_data encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_string_string_entry_proto x) encoder;
  ) v.Onnx_types.external_data;
  Pbrt.Encoder.key (14, Pbrt.Varint) encoder; 
  encode_tensor_proto_data_location v.Onnx_types.data_location encoder;
  Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) v.Onnx_types.double_data;
  ) encoder;
  Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Onnx_types.uint64_data;
  ) encoder;
  ()

let rec encode_sparse_tensor_proto (v:Onnx_types.sparse_tensor_proto) encoder = 
  begin match v.Onnx_types.values with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto x) encoder;
  | None -> ();
  end;
  begin match v.Onnx_types.indices with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Onnx_types.dims;
  ) encoder;
  ()

let rec encode_tensor_shape_proto_dimension_value (v:Onnx_types.tensor_shape_proto_dimension_value) encoder = 
  begin match v with
  | Onnx_types.Dim_value x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int64_as_varint x encoder;
  | Onnx_types.Dim_param x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  end

and encode_tensor_shape_proto_dimension (v:Onnx_types.tensor_shape_proto_dimension) encoder = 
  begin match v.Onnx_types.value with
  | Onnx_types.Dim_value x ->
    Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int64_as_varint x encoder;
  | Onnx_types.Dim_param x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  end;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.denotation encoder;
  ()

let rec encode_tensor_shape_proto (v:Onnx_types.tensor_shape_proto) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_shape_proto_dimension x) encoder;
  ) v.Onnx_types.dim;
  ()

let rec encode_type_proto_tensor (v:Onnx_types.type_proto_tensor) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Onnx_types.elem_type encoder;
  begin match v.Onnx_types.shape with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_shape_proto x) encoder;
  | None -> ();
  end;
  ()

let rec encode_type_proto_value (v:Onnx_types.type_proto_value) encoder = 
  begin match v with
  | Onnx_types.Tensor_type x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto_tensor x) encoder;
  | Onnx_types.Sequence_type x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto_sequence x) encoder;
  | Onnx_types.Map_type x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto_map x) encoder;
  end

and encode_type_proto (v:Onnx_types.type_proto) encoder = 
  begin match v.Onnx_types.value with
  | Onnx_types.Tensor_type x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto_tensor x) encoder;
  | Onnx_types.Sequence_type x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto_sequence x) encoder;
  | Onnx_types.Map_type x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto_map x) encoder;
  end;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.denotation encoder;
  ()

and encode_type_proto_sequence (v:Onnx_types.type_proto_sequence) encoder = 
  begin match v.Onnx_types.elem_type with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto x) encoder;
  | None -> ();
  end;
  ()

and encode_type_proto_map (v:Onnx_types.type_proto_map) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Onnx_types.key_type encoder;
  begin match v.Onnx_types.value_type with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto x) encoder;
  | None -> ();
  end;
  ()

let rec encode_value_info_proto (v:Onnx_types.value_info_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.name encoder;
  begin match v.Onnx_types.type_ with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_type_proto x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.doc_string encoder;
  ()

let rec encode_tensor_annotation (v:Onnx_types.tensor_annotation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.tensor_name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_string_string_entry_proto x) encoder;
  ) v.Onnx_types.quant_parameter_tensor_names;
  ()

let rec encode_attribute_proto (v:Onnx_types.attribute_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.name encoder;
  Pbrt.Encoder.key (21, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.ref_attr_name encoder;
  Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.doc_string encoder;
  Pbrt.Encoder.key (20, Pbrt.Varint) encoder; 
  encode_attribute_proto_attribute_type v.Onnx_types.type_ encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Onnx_types.f encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Onnx_types.i encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Onnx_types.s encoder;
  begin match v.Onnx_types.t with
  | Some x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto x) encoder;
  | None -> ();
  end;
  begin match v.Onnx_types.g with
  | Some x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_graph_proto x) encoder;
  | None -> ();
  end;
  begin match v.Onnx_types.sparse_tensor with
  | Some x -> 
    Pbrt.Encoder.key (22, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_sparse_tensor_proto x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits32 x encoder;
    ) v.Onnx_types.floats;
  ) encoder;
  Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Onnx_types.ints;
  ) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  ) v.Onnx_types.strings;
  List.iter (fun x -> 
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto x) encoder;
  ) v.Onnx_types.tensors;
  List.iter (fun x -> 
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_graph_proto x) encoder;
  ) v.Onnx_types.graphs;
  List.iter (fun x -> 
    Pbrt.Encoder.key (23, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_sparse_tensor_proto x) encoder;
  ) v.Onnx_types.sparse_tensors;
  ()

and encode_graph_proto (v:Onnx_types.graph_proto) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_node_proto x) encoder;
  ) v.Onnx_types.node;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto x) encoder;
  ) v.Onnx_types.initializer_;
  List.iter (fun x -> 
    Pbrt.Encoder.key (15, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_sparse_tensor_proto x) encoder;
  ) v.Onnx_types.sparse_initializer;
  Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.doc_string encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_value_info_proto x) encoder;
  ) v.Onnx_types.input;
  List.iter (fun x -> 
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_value_info_proto x) encoder;
  ) v.Onnx_types.output;
  List.iter (fun x -> 
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_value_info_proto x) encoder;
  ) v.Onnx_types.value_info;
  List.iter (fun x -> 
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_annotation x) encoder;
  ) v.Onnx_types.quantization_annotation;
  ()

and encode_node_proto (v:Onnx_types.node_proto) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Onnx_types.input;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Onnx_types.output;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.name encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.op_type encoder;
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.domain encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_attribute_proto x) encoder;
  ) v.Onnx_types.attribute;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.doc_string encoder;
  ()

let rec encode_operator_set_id_proto (v:Onnx_types.operator_set_id_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.domain encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Onnx_types.version encoder;
  ()

let rec encode_model_proto (v:Onnx_types.model_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Onnx_types.ir_version encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_operator_set_id_proto x) encoder;
  ) v.Onnx_types.opset_import;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.producer_name encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.producer_version encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.domain encoder;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Onnx_types.model_version encoder;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Onnx_types.doc_string encoder;
  begin match v.Onnx_types.graph with
  | Some x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_graph_proto x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_string_string_entry_proto x) encoder;
  ) v.Onnx_types.metadata_props;
  ()

let rec encode_tensor_proto_data_type (v:Onnx_types.tensor_proto_data_type) encoder =
  match v with
  | Onnx_types.Undefined -> Pbrt.Encoder.int_as_varint (0) encoder
  | Onnx_types.Float -> Pbrt.Encoder.int_as_varint 1 encoder
  | Onnx_types.Uint8 -> Pbrt.Encoder.int_as_varint 2 encoder
  | Onnx_types.Int8 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Onnx_types.Uint16 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Onnx_types.Int16 -> Pbrt.Encoder.int_as_varint 5 encoder
  | Onnx_types.Int32 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Onnx_types.Int64 -> Pbrt.Encoder.int_as_varint 7 encoder
  | Onnx_types.String -> Pbrt.Encoder.int_as_varint 8 encoder
  | Onnx_types.Bool -> Pbrt.Encoder.int_as_varint 9 encoder
  | Onnx_types.Float16 -> Pbrt.Encoder.int_as_varint 10 encoder
  | Onnx_types.Double -> Pbrt.Encoder.int_as_varint 11 encoder
  | Onnx_types.Uint32 -> Pbrt.Encoder.int_as_varint 12 encoder
  | Onnx_types.Uint64 -> Pbrt.Encoder.int_as_varint 13 encoder
  | Onnx_types.Complex64 -> Pbrt.Encoder.int_as_varint 14 encoder
  | Onnx_types.Complex128 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Onnx_types.Bfloat16 -> Pbrt.Encoder.int_as_varint 16 encoder
