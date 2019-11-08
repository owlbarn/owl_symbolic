(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(* TODO: type restrain must be reflected here *)

open Owl_symbolic_types

module Add = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Add"
  let create name input attrs = { name; input; attrs }
  let type_constraints = []
  let doc_string = "Addition"
end

module Sub = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Sub"
  let create name input attrs = { name; input; attrs }
end

module Mul = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Mul"
  let create name input attrs = { name; input; attrs }
end

module Div = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Div"
  let create name input attrs = { name; input; attrs }
end

module Sin = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Sin"
  let create name input attrs = { name; input; attrs }
end

module Cos = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Cos"
  let create name input attrs = { name; input; attrs }
end

module Exp = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Exp"
  let create name input attrs = { name; input; attrs }
end

module Pow = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Pow"
  let create name input attrs = { name; input; attrs }
end

module ExpConst = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "ExpConst"
  let create name input attrs = { name; input; attrs }
end

module Float = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : float
    }

  let op_type = "Float"
  let create name input attrs value = { name; input; attrs; value }
end

module Tensor = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable typ : sym_data_type
    ; mutable shape : int array
    }

  let op_type = "Tensor"

  let create ?(shape = [||]) ?(typ = SDT_Float) name input attrs =
    { name; input; attrs; typ; shape }
end

module Variable = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable typ : sym_data_type
    ; mutable shape : int array
    }

  let op_type = "Variable"
  let create name input attrs typ shape = { name; input; attrs; typ; shape }
end

module Int = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : int
    }

  let op_type = "Int"
  let create name input attrs value = { name; input; attrs; value }
end

module Complex = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable real : float
    ; mutable img : float
    }

  let op_type = "Complex"
  let create name input attrs real img = { name; input; attrs; real; img }
end

(** Is `output` necessary? *)
