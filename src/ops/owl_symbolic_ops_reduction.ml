(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** ReduceMax, ReduceMin, ReduceSum, ReduceSumSquare, ReduceMean, ReduceProd, 
ReduceLogSum, ReduceLogSumExp, ReduceL1, ReduceL2 *)

open Owl_symbolic_types

module ReduceSum = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    ; mutable axes : int array
    ; mutable keepdims : bool (* NOTE: ONNX requires an int parameter *)
    }

  let op_type = "ReduceSum"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = None; axes; keepdims }
end

module ReduceMax = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    ; mutable axes : int array
    ; mutable keepdims : bool
    }

  let op_type = "ReduceMax"

  let create ?(keepdims = true) ?name x axes =
    let attrs = [||] in
    let input = [| x |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = None; axes; keepdims }
end
