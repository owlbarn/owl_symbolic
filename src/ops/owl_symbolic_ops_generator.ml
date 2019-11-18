(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types

module RandomUniform = struct
  type t =
    { mutable name : string
    ; mutable attrs : (string * attrvalue) array
    ; mutable dtype : number_type
    ; mutable shape : int array
    ; mutable out_shape : int array option
    ; mutable high : float
    ; mutable low : float
    ; mutable seed : float option
    }

  let op_type = "RandomUniform"

  let create ?(low = 0.) ?(high = 1.) ?(seed = None) name attrs dtype shape =
    { name; attrs; dtype; shape; out_shape = Some shape; high; low; seed }
end
