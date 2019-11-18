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


let infer_shape input_shapes sym =
  match sym with
  | Int _       -> [| Some [||] |]
  | Float _     -> [| Some [||] |]
  | Complex _   -> [| Some [||] |]
  | Pi _        -> [| Some [||] |]
  | Tensor _    ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Variable _  ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Sin _       -> _infer_shape_01 input_shapes
  | Cos _       -> _infer_shape_01 input_shapes
  | Sqrt _      -> _infer_shape_01 input_shapes
  | Exp _       -> _infer_shape_01 input_shapes
  | Log _       -> _infer_shape_01 input_shapes
  | Neg _       -> _infer_shape_01 input_shapes
  | Add _       -> _infer_shape_03 input_shapes
  | Sub _       -> _infer_shape_03 input_shapes
  | Mul _       -> _infer_shape_03 input_shapes
  | Div _       -> _infer_shape_03 input_shapes
  | Pow _       -> _infer_shape_01 input_shapes
  | ReduceSum x -> _infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMax x -> _infer_shape_10 input_shapes x.axes x.keepdims
  | _           -> [| None |]

(* It has been shown that current _infer_shape_03 
  works for add, addscalar and scalaradd; so no need to 
  rush to add new symbols perhaps *)
