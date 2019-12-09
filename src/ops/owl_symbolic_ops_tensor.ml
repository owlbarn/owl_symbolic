(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Cast, Reshape, Shape, Size, Concat, Split, Slice, Transpose, Scatter,
 ScatterND, ScatterElements, Gather, GatherElements, Squeeze, UnSqueeze, 
 SpaceToDepth, DepthToSpace, Tile, Upsample, Resize, Identity, Compress, 
 OneHot, IsNaN, IsInf, Where, NonZero, ReverseSequence, Unique, GatherND, Pad 
 *)

open Owl_symbolic_types

module Reshape = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable shape : int array
    ; mutable out_shape : int array option
    }

  let op_type = "Reshape"

  let create ?(out_shape = None) name input shape attrs =
    { name; input; attrs; shape; out_shape }
end
