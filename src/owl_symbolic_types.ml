(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type sym_data_type =
  | SDT_Float
  | SDT_Double
  | SDT_Complex32
  | SDT_Complex64
  | SDT_Bool
  | SDT_String
  | SDT_Int8
  | SDT_Int16
  | SDT_Int32
  | SDT_Int64
  | SDT_Uint8
  | SDT_Uint16
  | SDT_Uint32
  | SDT_Uint64
  | SDT_Float16

type tensor =
  { dtype : sym_data_type
  ; shape : int array
  ; str_val : string array option
  ; flt_val : float array option
  ; int_val : int array option
  ; raw_val : bytes option
  }

type attrvalue =
  | ATTR_Nil
  | ATTR_Int of int
  | ATTR_Bool of bool
  | ATTR_Type of sym_data_type
  | ATTR_Float of float
  | ATTR_Shape of int array
  | ATTR_String of string
  | ATTR_Tensor of tensor
  | ATTR_Array of attrvalue array
  | ATTR_NameArray of
      { name : string
      ; attr : (string * attrvalue) array
      }

let get_attrvalue_int v =
  match v with
  | ATTR_Int i -> i
  | _          -> failwith "get_attrvalue_int: incorrect attr type"


let get_attrvalue_float v =
  match v with
  | ATTR_Float f -> f
  | _            -> failwith "get_attrvalue_float: incorrect attr type"


let get_attrvalue_type v =
  match v with
  | ATTR_Type typ -> typ
  | _             -> failwith "get_attrvalue_type: incorrect attr type"


let get_attrvalue_shape v =
  match v with
  | ATTR_Shape s -> s
  | _            -> failwith "get_attrvalue_shape: incorrect attr type"


let make_tensor
    ?(flt_val = None)
    ?(int_val = None)
    ?(str_val = None)
    ?(raw_val = None)
    dtype
    shape
  =
  { dtype; shape; flt_val; int_val; str_val; raw_val }


(** flt, int, ... ? *)
