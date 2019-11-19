(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

let _infer_shape_00 _input_shapes = [| Some [||] |]

let _infer_shape_01 input_shapes =
  match input_shapes.(0) with
  | Some s -> [| Some Array.(copy s) |]
  | None   -> [| None |]


let _infer_shape_03 input_shapes =
  let s0 = input_shapes.(0) in
  let s1 = input_shapes.(1) in
  match s0, s1 with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(broadcast1 s0 s1) |]
  | _, _             -> [| None |]


let _infer_shape_10 input_shapes axis keepdims =
  match input_shapes.(0) with
  | Some s -> [| Some Owl_symbolic_utils.(reduce s axis keepdims) |]
  | None   -> [| None |]


let _infer_shape_11 input_shapes padding stride =
  let input_shape = input_shapes.(0) in
  let kernel_shape = input_shapes.(1) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _, _                    -> [| None |]


let _infer_shape_12 input_shapes padding stride =
  let input_shape = input_shapes.(0) in
  let kernel_shape = input_shapes.(1) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv2d input padding kernel stride) |]
  | _, _                    -> [| None |]


let _infer_shape_13 input_shapes padding stride =
  let input_shape = input_shapes.(0) in
  let kernel_shape = input_shapes.(1) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _, _                    -> [| None |]


let _infer_shape_15 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _          -> [| None |]


let _infer_shape_17 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _          -> [| None |]


let _infer_shape_19 input_shapes =
  let x_shape = input_shapes.(0) in
  let y_shape = input_shapes.(1) in
  match x_shape, y_shape with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(dot s0 s1) |]
  | _, _             -> [| None |]


let _infer_shape_21 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(pool2d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape input_shapes sym =
  match sym with
  | Int _           -> [| Some [||] |]
  | Float _         -> [| Some [||] |]
  | Complex _       -> [| Some [||] |]
  | Pi _            -> [| Some [||] |]
  | Tensor _        ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Variable _      ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | RandomUniform _ ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Sin _           -> _infer_shape_01 input_shapes
  | Cos _           -> _infer_shape_01 input_shapes
  | Sqrt _          -> _infer_shape_01 input_shapes
  | Exp _           -> _infer_shape_01 input_shapes
  | Log _           -> _infer_shape_01 input_shapes
  | Neg _           -> _infer_shape_01 input_shapes
  | Relu _          -> _infer_shape_01 input_shapes
  | Add _           -> _infer_shape_03 input_shapes
  | Sub _           -> _infer_shape_03 input_shapes
  | Mul _           -> _infer_shape_03 input_shapes
  | Div _           -> _infer_shape_03 input_shapes
  | Pow _           -> _infer_shape_01 input_shapes
  | MatMul _        -> _infer_shape_19 input_shapes
  | ReduceSum x     -> _infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMax x     -> _infer_shape_10 input_shapes x.axes x.keepdims
  | Reshape x       -> [| Some x.shape |]
  | Conv x          ->
    let l = Array.length x.strides in
    let padding = if x.auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
    if l = 3
    then _infer_shape_11 input_shapes padding x.strides
    else if l = 4
    then _infer_shape_12 input_shapes padding x.strides
    else if l = 5
    then _infer_shape_13 input_shapes padding x.strides
    else failwith "Owl_symbolic_shape: illegal conv dimensions."
  | MaxPool x       ->
    let l = Array.length x.strides in
    let padding = if x.auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
    if l = 4
    then _infer_shape_15 input_shapes padding x.kernel_shp x.strides
    else if l = 4
    then _infer_shape_21 input_shapes padding x.kernel_shp x.strides
    else if l = 5
    then _infer_shape_17 input_shapes padding x.kernel_shp x.strides
    else failwith "Owl_symbolic_shape: illegal maxpool dimensions."
  | _               -> [| None |]

(* It has been shown that current _infer_shape_03 
  works for add, addscalar and scalaradd; so no need to 
  rush to add new symbols perhaps *)
