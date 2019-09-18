(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module type Sig = sig
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  val op_type : string
  val type_constraints : string list
  val doc_string : string
end

module Make (M : Sig) = struct
  type t = M.t

  let name (x : t) = x.name
  let op_type () = M.op_type
  let doc_string () = M.doc_string
  let of_onnx (x : t) =
    Owl_onnx_spcs.PB.
  let to_onnx (x : t) = ()
end
