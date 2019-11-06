(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type tensor_typ =
  | S
  | D
  | C
  | Z

(** NOTE: no real data should be included in symbolic computation *)
type tensor = {
  dtype          : string;
  tensor_shape   : int array;
  string_val     : string array option;
  float_val      : float array option;
  int_val        : int array option;
  tensor_content : bytes option;
}

type attrvalue =
  | ATTR_Nil
  | ATTR_Int      of int
  | ATTR_Bool     of bool
  | ATTR_Type     of string
  | ATTR_Float    of float
  | ATTR_Shape    of int array
  | ATTR_String   of string
  | ATTR_Tensor   of tensor
  | ATTR_List     of attrvalue array
  | ATTR_Namelist of {name : string; attr: (string * attrvalue) array}

(** flt, int, ... ? *)
