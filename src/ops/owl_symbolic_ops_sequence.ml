(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** Implemented: SequenceEmpty, SequenceAt, SequenceInsert, SequenceLength, SequenceConstruct, SequenceErease *)

(** SplitToSequence, ConcatFromSequence *)

open Owl_symbolic_types

module SequenceEmpty = struct
  type t =
    { mutable name : string
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable dtype : number_type
    }

  let op_type = "SequenceEmpty"

  let create ?name ?(dtype = SNT_Float) () =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; attrs; out_shape = [| None |]; dtype }
end

module SequenceInsert = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable pos : int
    }

  let op_type = "SequenceInsert"

  let create ?name pos seq_name tensor_name pos_name =
    let attrs = [||] in
    let input = [| seq_name; tensor_name; pos_name |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; pos }
end

module SequenceAt = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable pos : int
    }

  let op_type = "SequenceAt"

  let create ?name pos input_name pos_name =
    let attrs = [||] in
    let input = [| input_name; pos_name |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; pos }
end

module SequenceLength = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "SequenceLength"

  let create ?name seq_name =
    let attrs = [||] in
    let input = [| seq_name |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module SequenceConstruct = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    }

  let op_type = "SequenceConstruct"

  let create ?name xn =
    let attrs = [||] in
    let input = xn in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |] }
end

module SequenceErase = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option array
    ; mutable pos : int
    }

  let op_type = "SequenceErase"

  let create ?name pos xn posn =
    let attrs = [||] in
    let input = [| xn; posn |] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    { name; input; attrs; out_shape = [| None |]; pos }
end
