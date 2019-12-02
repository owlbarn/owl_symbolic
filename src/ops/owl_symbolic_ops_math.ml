(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types

(* p and q are not specified by user; but rather later calculated in canonical part *)
module Rational = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    ; mutable p : int
    ; mutable q : int
    }

  let op_type = "Rational"

  let create ?(out_shape = None) name input attrs =
    { name; input; attrs; out_shape; p = 0; q = 0 }
end

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

module Sqrt = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Sqrt"
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

module Log = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Log"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Neg = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Neg"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end

module Relu = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "Relu"
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

(* Matrix product that behaves like numpy.matmul:
 * https://docs.scipy.org/doc/numpy-1.13.0/reference/generated/numpy.matmul.html *)
module MatMul = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    }

  let op_type = "MatMul"
  let create ?(out_shape = None) name input attrs = { name; input; attrs; out_shape }
end
