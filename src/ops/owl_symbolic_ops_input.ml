(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types

 module Int = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : int
    ; mutable out_shape : int array option
    }

  let op_type = "Int"
  let create name input attrs value = { name; input; attrs; value; out_shape = Some [||] }
end

module Float = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : float
    ; mutable out_shape : int array option
    }

  let op_type = "Float"
  let create name input attrs value = { name; input; attrs; value; out_shape = Some [||] }
end

module Complex = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable real : float
    ; mutable img : float
    ; mutable out_shape : int array option
    }

  let op_type = "Complex"

  let create name input attrs real img =
    { name; input; attrs; real; img; out_shape = Some [||] }
end

module Tensor = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable value : tensor
    ; mutable out_shape : int array option
    }

  let op_type = "Tensor"
  let create name input attrs value = { name; input; attrs; value; out_shape = Some [||] }
end

module Variable = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable typ : number_type
    ; mutable shape : int array
    ; mutable out_shape : int array option
    ; mutable init : tensor option
    }

  let op_type = "Variable"

  let create name input attrs typ shape init =
    { name; input; attrs; typ; shape; out_shape = Some shape; init }
end

module Pi = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    ; mutable dtype : number_type
    }

  let op_type = "Pi"

  let create ?(dtype = SDT_Float) name =
    { name; input = [||]; attrs = [||]; out_shape = Some [||]; dtype }
end