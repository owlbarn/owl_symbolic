(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t =
  | Add of Owl_onnx_ops_math.Add.t
  | Sub of Owl_onnx_ops_math.Sub.t
  | Mul of Owl_onnx_ops_math.Mul.t
  | Div of Owl_onnx_ops_math.Div.t

let op_type = function
  | Add _ -> Owl_onnx_ops_math.Add.op_type
  | Sub _ -> Owl_onnx_ops_math.Sub.op_type
  | Mul _ -> Owl_onnx_ops_math.Mul.op_type
  | Div _ -> Owl_onnx_ops_math.Div.op_type
