(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** And, Or, Xor, Greater, Less, Equal, Not, BitShift  *)

open Owl_symbolic_types

module Equal = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "Equal"

  let create ?name lhs_name rhs_name =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| lhs_name; rhs_name |] in
    { name; input; attrs; out_shape = [| None |] }
end
