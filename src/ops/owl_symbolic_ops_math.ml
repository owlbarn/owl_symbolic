(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types

module Add = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Add"
  let create name input output attrs = { name; input; output; attrs }
  let type_constraints = []
  let doc_string = "Addition"
end

module Sub = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Sub"
  let create name input output attrs = { name; input; output; attrs }
end

module Mul = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Mul"
  let create name input output attrs = { name; input; output; attrs }
end

module Div = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Div"
  let create name input output attrs = { name; input; output; attrs }
end

module Sin = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Sin"
  let create name input output attrs = { name; input; output; attrs }
end

module Cos = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Cos"
  let create name input output attrs = { name; input; output; attrs }
end

module Exp = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Exp"
  let create name input output attrs = { name; input; output; attrs }
end

module Pow = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "Pow"
  let create name input output attrs = { name; input; output; attrs }
end

module ExpConst = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    }

  let op_type = "ExpConst"
  let create name input output attrs = { name; input; output; attrs }
end

module Float = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : float
    }

  let op_type = "Float"
  let create name input output attrs value = { name; input; output; attrs; value }
end

module Tensor = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    ; mutable typ : sym_data_type
    ; mutable shape : int array
    ; mutable id : string (* Not name, but like "x" or "y" *)
    }

  let op_type = "Tensor"

  let create ?(shape = [||]) ?(typ = SDT_Float) name input output attrs id =
    { name; input; output; attrs; typ; shape; id }
end

module Placeholder = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    ; mutable typ : sym_data_type
    ; mutable shape : int array
    ; mutable id : string (* Not name, but like "x" or "y" *)
    }

  let op_type = "Symbol"
  let create name input output attrs typ shape id = { name; input; output; attrs; typ; shape; id }
end

module Int = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : int
    }

  let op_type = "Int"
  let create name input output attrs value = { name; input; output; attrs; value }
end

module Complex = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable attrs : (string * attrvalue) array
    ; mutable real : float
    ; mutable img : float
    }

  let op_type = "Complex"
  let create name input output attrs real img = { name; input; output; attrs; real; img }
end

(** Is `output` necessary? *)
