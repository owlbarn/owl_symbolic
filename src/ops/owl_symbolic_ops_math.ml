(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(* TODO: type restrain must be reflected here *)

open Owl_symbolic_types

module Sin = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Sin"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Cos = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Cos"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Exp = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Exp"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Add = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
          (* ; mutable pos_scalar : int --> differentiate add (0), scalaradd(-1), and addscalar(1) *)
    }

  let op_type = "Add"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
  let type_constraints = []
  let doc_string = "Addition"
end

module Sub = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Sub"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Mul = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Mul"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Div = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Div"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Pow = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Pow"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

(** Is `output` necessary? *)
