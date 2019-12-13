(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

let infer_shape_00 _input_shapes = [| Some [||] |]

let infer_shape_01 input_shapes =
  match input_shapes.(0).(0) with
  | Some s -> [| Some Array.(copy s) |]
  | None   -> [| None |]


let infer_shape_03 input_shapes =
  let s0 = input_shapes.(0).(0) in
  let s1 = input_shapes.(1).(0) in
  match s0, s1 with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(broadcast1 s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_07 input_shapes axis =
  let s0 = Array.map (fun s -> s.(0)) input_shapes in
  if Array.exists
       (function
         | Some _ -> false
         | None   -> true)
       s0
  then [| None |]
  else (
    let s1 =
      Array.map
        (function
          | Some a -> a
          | None   -> failwith "infer_shape_07")
        s0
    in
    [| Some Owl_utils_infer_shape.(concatenate s1 axis) |])


let infer_shape_08 input_shapes axis splits =
  match input_shapes.(0).(0) with
  | Some s ->
    let s0 = Owl_utils_infer_shape.(split s axis splits) in
    Array.map (fun s -> Some s) s0
  | None   -> Array.(make (length splits) None)


let infer_shape_10 input_shapes axis keepdims =
  match input_shapes.(0).(0) with
  | Some s -> [| Some Owl_symbolic_utils.(reduce s axis keepdims) |]
  | None   -> [| None |]


let infer_shape_11 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_12 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_symbolic_utils.(conv2d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_13 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_15 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_17 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_19 input_shapes =
  let x_shape = input_shapes.(0).(0) in
  let y_shape = input_shapes.(1).(0) in
  match x_shape, y_shape with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(dot s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_21 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_symbolic_utils.(pool2d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_conv (x : Owl_symbolic_ops_nn.Conv.t) input_shapes =
  let l = x.dim in
  let padding = if x.auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
  if l = 1
  then infer_shape_11 input_shapes padding x.strides
  else if l = 2
  then infer_shape_12 input_shapes padding x.strides
  else if l = 3
  then infer_shape_13 input_shapes padding x.strides
  else failwith "Owl_symbolic_shape: illegal conv dimensions."


let infer_shape_maxpool (x : Owl_symbolic_ops_nn.MaxPool.t) input_shapes =
  let l = Array.length x.kernel_shp in
  let ndim =
    match input_shapes.(0).(0) with
    | Some i -> Array.length i - 2
    | None   -> failwith "infer_shape_maxpool: input shape is none"
  in
  assert (ndim = l);
  let padding = if x.auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
  let dim =
    if ndim = 1
    then infer_shape_15 input_shapes padding x.kernel_shp x.strides
    else if ndim = 2
    then infer_shape_21 input_shapes padding x.kernel_shp x.strides
    else if ndim = 3
    then infer_shape_17 input_shapes padding x.kernel_shp x.strides
    else failwith "Owl_symbolic_shape: illegal maxpool dimensions."
  in
  [| dim.(0); dim.(0) |]


let infer_shape_batch_normalization input_shapes =
  let msg = "Owl_symbolic_shape: error unpacking BatchNormalisation input shapes." in
  let unpack = Owl_symbolic_utils.get_option_value msg in
  let shp_x = unpack input_shapes.(0).(0) in
  let shp_scale = unpack input_shapes.(1).(0) in
  let shp_b = unpack input_shapes.(2).(0) in
  let shp_mean = unpack input_shapes.(3).(0) in
  let shp_var = unpack input_shapes.(4).(0) in
  let c = shp_x.(0) in
  assert (Array.length shp_scale = 1);
  assert (Array.length shp_b = 1);
  assert (Array.length shp_mean = 1);
  assert (Array.length shp_var = 1);
  assert (c = shp_scale.(0));
  assert (c = shp_b.(0));
  assert (c = shp_mean.(0));
  assert (c = shp_var.(0));
  [| Some shp_x; Some [||]; Some [||]; Some [||]; Some [||] |]


(** Main entry *)

let infer_shape input_shapes sym =
  match sym with
  | Int _                -> [| Some [||] |]
  | Float _              -> [| Some [||] |]
  | Complex _            -> [| Some [||] |]
  | Pi _                 -> [| Some [||] |]
  | Tensor _             ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Variable _           ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | RandomUniform _      ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Sin _                -> infer_shape_01 input_shapes
  | Cos _                -> infer_shape_01 input_shapes
  | Tan _                -> infer_shape_01 input_shapes
  | Asin _               -> infer_shape_01 input_shapes
  | Acos _               -> infer_shape_01 input_shapes
  | Atan _               -> infer_shape_01 input_shapes
  | Sinh _               -> infer_shape_01 input_shapes
  | Cosh _               -> infer_shape_01 input_shapes
  | Tanh _               -> infer_shape_01 input_shapes
  | Asinh _              -> infer_shape_01 input_shapes
  | Acosh _              -> infer_shape_01 input_shapes
  | Atanh _              -> infer_shape_01 input_shapes
  | Sqrt _               -> infer_shape_01 input_shapes
  | Exp _                -> infer_shape_01 input_shapes
  | Log _                -> infer_shape_01 input_shapes
  | Abs _                -> infer_shape_01 input_shapes
  | Neg _                -> infer_shape_01 input_shapes
  | Floor _              -> infer_shape_01 input_shapes
  | Ceil _               -> infer_shape_01 input_shapes
  | Round _              -> infer_shape_01 input_shapes
  | Relu _               -> infer_shape_01 input_shapes
  | Add _                -> infer_shape_03 input_shapes
  | Sub _                -> infer_shape_03 input_shapes
  | Mul _                -> infer_shape_03 input_shapes
  | Div _                -> infer_shape_03 input_shapes
  | Pow _                -> infer_shape_01 input_shapes
  | MatMul _             -> infer_shape_19 input_shapes
  | ReduceSum x          -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMax x          -> infer_shape_10 input_shapes x.axes x.keepdims
  | Reshape x            -> [| Some x.shape |]
  | Identity x           ->
    let idx = x.idx in
    [| input_shapes.(0).(idx) |]
  | Split x              -> infer_shape_08 input_shapes x.axis x.split
  | Concat x             -> infer_shape_07 input_shapes x.axis
  | Conv x               -> infer_shape_conv x input_shapes
  | MaxPool x            -> infer_shape_maxpool x input_shapes
  | BatchNormalization _ -> infer_shape_batch_normalization input_shapes
  | SequenceEmpty _      -> [||]
  | _                    -> [| None |]
