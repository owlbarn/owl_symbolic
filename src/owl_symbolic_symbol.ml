(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*
 * The operations included: 
 * (1) Numbers: Integers, Complex, Float. E.g. in "exp = x + y - 0.1" the 0.1 is a float;
 * note that its not related to the tensor type (float or double etc.)
 * Tensor is also what we need.
 * (2) Special constant: ExpConst
 * (3) Symbol. It's not specified when defined. You can just define "Symbol 'x'" in the symbolic grpah.
 * Only before evaluation (conversion) should the symbol be replaced with float, int, tensor, etc. 
 * (4) Unary Op: Sin, Cos, Exp
 * (5) Binary Op: +, -, *, /, pow
 *)

open Owl_symbolic_ops_math

type t =
  | NOOP
  | Int of Int.t
  | Complex of Complex.t
  | Float of Float.t
  | Tensor of Tensor.t
  | Variable of Variable.t
  | Sin of Sin.t
  | Cos of Cos.t
  | Exp of Exp.t
  | Add of Add.t
  | Sub of Sub.t
  | Mul of Mul.t
  | Div of Div.t
  | Pow of Pow.t

(* TODO: PowScalar; ScalarPow *)

let name = function
  | Int x      -> Int.(x.name)
  | Float x    -> Float.(x.name)
  | Complex x  -> Complex.(x.name)
  | Tensor x   -> Tensor.(x.name)
  | Variable x -> Variable.(x.name)
  | Sin x      -> Sin.(x.name)
  | Cos x      -> Cos.(x.name)
  | Exp x      -> Exp.(x.name)
  | Add x      -> Add.(x.name)
  | Sub x      -> Sub.(x.name)
  | Mul x      -> Mul.(x.name)
  | Div x      -> Div.(x.name)
  | Pow x      -> Pow.(x.name)
  | _          -> failwith "owl_symbolic_symbol.name"


let input = function
  | Int x      -> Int.(x.input)
  | Float x    -> Float.(x.input)
  | Complex x  -> Complex.(x.input)
  | Tensor x   -> Tensor.(x.input)
  | Variable x -> Variable.(x.input)
  | Sin x      -> Sin.(x.input)
  | Cos x      -> Cos.(x.input)
  | Exp x      -> Exp.(x.input)
  | Add x      -> Add.(x.input)
  | Sub x      -> Sub.(x.input)
  | Mul x      -> Mul.(x.input)
  | Div x      -> Div.(x.input)
  | Pow x      -> Pow.(x.input)
  | _          -> failwith "owl_symbolic_symbol.input"


let op_type = function
  | Int _      -> Int.op_type
  | Float _    -> Float.op_type
  | Complex _  -> Complex.op_type
  | Tensor _   -> Tensor.op_type
  | Variable _ -> Variable.op_type
  | Sin _      -> Sin.op_type
  | Cos _      -> Cos.op_type
  | Exp _      -> Exp.op_type
  | Add _      -> Add.op_type
  | Sub _      -> Sub.op_type
  | Mul _      -> Mul.op_type
  | Div _      -> Div.op_type
  | Pow _      -> Pow.op_type
  | _          -> failwith "owl_symbolic_symbol.op_type"


let sym_attrs = function
  | Int x      -> Int.(x.attrs)
  | Float x    -> Float.(x.attrs)
  | Complex x  -> Complex.(x.attrs)
  | Tensor x   -> Tensor.(x.attrs)
  | Variable x -> Variable.(x.attrs)
  | Sin x      -> Sin.(x.attrs)
  | Cos x      -> Cos.(x.attrs)
  | Exp x      -> Exp.(x.attrs)
  | Add x      -> Add.(x.attrs)
  | Sub x      -> Sub.(x.attrs)
  | Mul x      -> Mul.(x.attrs)
  | Div x      -> Div.(x.attrs)
  | Pow x      -> Pow.(x.attrs)
  | _          -> failwith "owl_symbolic_symbol.sym_attrs: unsupported symbol."


let get_out_shape = function
  | Int x      -> Int.(x.out_shape)
  | Float x    -> Float.(x.out_shape)
  | Complex x  -> Complex.(x.out_shape)
  | Tensor x   -> Tensor.(x.out_shape)
  | Variable x -> Variable.(x.out_shape)
  | Sin x      -> Sin.(x.out_shape)
  | Cos x      -> Cos.(x.out_shape)
  | Exp x      -> Exp.(x.out_shape)
  | Add x      -> Add.(x.out_shape)
  | Sub x      -> Sub.(x.out_shape)
  | Mul x      -> Mul.(x.out_shape)
  | Div x      -> Div.(x.out_shape)
  | Pow x      -> Pow.(x.out_shape)
  | _          -> failwith "get_out_shape: unsupported op."


let set_out_shape sym shape =
  match sym with
  | Int x      -> x.out_shape <- shape
  | Float x    -> x.out_shape <- shape
  | Complex x  -> x.out_shape <- shape
  | Tensor x   -> x.out_shape <- shape
  | Variable x -> x.out_shape <- shape
  | Sin x      -> x.out_shape <- shape
  | Cos x      -> x.out_shape <- shape
  | Exp x      -> x.out_shape <- shape
  | Add x      -> x.out_shape <- shape
  | Sub x      -> x.out_shape <- shape
  | Mul x      -> x.out_shape <- shape
  | Div x      -> x.out_shape <- shape
  | Pow x      -> x.out_shape <- shape
  | _          -> failwith "set_out_shape: unsupported op."


(* Not `out_shape` *)
let shape = function
  | Variable x -> Variable.(x.shape)
  | Tensor x   -> Tensor.(x.shape)
  | _          -> [||]


let float_value = function
  | Float x -> Float.(x.value)
  | _       -> failwith "owl_symbolic_symbol.float_value"


let int_value = function
  | Int x -> Int.(x.value)
  | _     -> failwith "owl_symbolic_symbol.int_value"


let initializer_ = function
  | Variable x -> Variable.(x.init)
  | _          -> failwith "owl_symbolic_symbol.initializer_"

(*

let tensor_value_flt = function
  | Tensor x -> Tensor.(x.flt_val)
  | _        -> failwith "owl_symbolic_symbol.tensor_value_flt"


let tensor_value_int = function
  | Tensor x -> Tensor.(x.int_val)
  | _        -> failwith "owl_symbolic_symbol.tensor_value_int"


let tensor_value_complex = function
  | Tensor x -> Tensor.(x.complex_val)
  | _        -> failwith "owl_symbolic_symbol.tensor_value_complex"


let tensor_value_str = function
  | Tensor x -> Tensor.(x.str_val)
  | _        -> failwith "owl_symbolic_symbol.tensor_value_str"

*)
