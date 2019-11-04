(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Add = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Add"
  let create name input output = { name; input; output }
  let type_constraints = []
  let doc_string = "Addition"
end

module Sub = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Sub"
  let create name input output = { name; input; output }
end

module Mul = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Mul"
  let create name input output = { name; input; output }
end

module Div = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Div"
  let create name input output = { name; input; output }
end

module Sin = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Sin"
  let create name input output = { name; input; output }
end

module Cos = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Cos"
  let create name input output = { name; input; output }
end

module Exp = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Exp"
  let create name input output = { name; input; output }
end

module Pow = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "Pow"
  let create name input output = { name; input; output }
end

module ExpConst = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    }

  let op_type = "ExpConst"
  let create name input output = { name; input; output }
end

module Float = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable value : float
    }

  let op_type = "Float"
  let create name input output value = { name; input; output; value }
end

module Tensor = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable shape : int array
    ; mutable id : string (* Not name, but like "x" or "y" *)
    }

  let op_type = "Tensor"
  let create ?(shape = [||]) name input output id = { name; input; output; shape; id }
end

module Symbol = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable id : string (* Not name, but like "x" or "y" *)
    }

  let op_type = "Symbol"
  let create name input output id = { name; input; output; id }
end

module Int = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable value : int
    }

  let op_type = "Int"
  let create name input output value = { name; input; output; value }
end

module Complex = struct
  type t =
    { mutable name : string
    ; mutable input : string list
    ; mutable output : string list
    ; mutable real : float
    ; mutable img : float
    }

  let op_type = "Complex"
  let create name input output real img = { name; input; output; real; img }
end

(** Is `output` necessary? *)
