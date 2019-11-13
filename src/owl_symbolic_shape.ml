(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

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


let infer_shape input_shapes sym =
  match sym with
  | Int _      -> [| Some [||] |]
  | Float _    -> [| Some [||] |]
  | Complex _  -> [| Some [||] |]
  | Tensor _   ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Variable _ ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Sin _      -> _infer_shape_01 input_shapes
  | Cos _      -> _infer_shape_01 input_shapes
  | Exp _      -> _infer_shape_01 input_shapes
  | Add _      -> _infer_shape_03 input_shapes
  | Sub _      -> _infer_shape_03 input_shapes
  | Mul _      -> _infer_shape_03 input_shapes
  | Div _      -> _infer_shape_03 input_shapes
  | Pow _      -> _infer_shape_01 input_shapes
  | _          -> [| None |]
