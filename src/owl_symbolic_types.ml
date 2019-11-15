(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(* number_type *)
type number_type =
  | SNT_Noop
  | SNT_Float
  | SNT_Double
  | SNT_Complex32
  | SNT_Complex64
  | SNT_Bool
  | SNT_String
  | SNT_Int8
  | SNT_Int16
  | SNT_Int32
  | SNT_Int64
  | SNT_Uint8
  | SNT_Uint16
  | SNT_Uint32
  | SNT_Uint64
  | SNT_Float16

let number_type_to_string = function
  | SNT_Noop      -> "SNT_Noop"
  | SNT_Float     -> "SNT_Float"
  | SNT_Uint8     -> "SNT_Uint8"
  | SNT_Int8      -> "SNT_Int8"
  | SNT_Uint16    -> "SNT_Uint16"
  | SNT_Int16     -> "SNT_Int16"
  | SNT_Int32     -> "SNT_Int32"
  | SNT_Int64     -> "SNT_Int64"
  | SNT_String    -> "SNT_String"
  | SNT_Bool      -> "SNT_Bool"
  | SNT_Float16   -> "SNT_Float16"
  | SNT_Double    -> "SNT_Double"
  | SNT_Uint32    -> "SNT_Uint32"
  | SNT_Uint64    -> "SNT_Uint64"
  | SNT_Complex32 -> "SNT_Complex32"
  | SNT_Complex64 -> "SNT_Complex64"


(* type sci_const = Pi | and more ... *)

type tensor =
  { dtype : number_type
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
  | ATTR_Type of number_type
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


let get_symtensor_dtype (t : tensor) = t.dtype

(** flt, int, ... ? *)

exception TYPE_CHECK of string
(** Exception definition *)

exception INVALID_NAME of string

(* TODO: At some poin we may need to provide uses with the ability to build new operations. *)
