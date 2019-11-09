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
  | ExpConst of ExpConst.t
  | Variable of Variable.t
  | Sin of Sin.t
  | Cos of Cos.t
  | Exp of Exp.t
  | Add of Add.t
  | Sub of Sub.t
  | Mul of Mul.t
  | Div of Div.t
  | Pow of Pow.t

let name = function
  | Float x    -> Float.(x.name)
  | Tensor x   -> Tensor.(x.name)
  | Add x      -> Add.(x.name)
  | Sub x      -> Sub.(x.name)
  | Mul x      -> Mul.(x.name)
  | Div x      -> Div.(x.name)
  | Sin x      -> Sin.(x.name)
  | Cos x      -> Cos.(x.name)
  | Pow x      -> Pow.(x.name)
  | Variable x -> Variable.(x.name)
  | _          -> failwith "owl_symbolic_symbol.name"


let input = function
  | Float x    -> Float.(x.input)
  | Tensor x   -> Tensor.(x.input)
  | Add x      -> Add.(x.input)
  | Sub x      -> Sub.(x.input)
  | Mul x      -> Mul.(x.input)
  | Div x      -> Div.(x.input)
  | Sin x      -> Sin.(x.input)
  | Cos x      -> Cos.(x.input)
  | Pow x      -> Pow.(x.input)
  | Variable x -> Variable.(x.input)
  | _          -> failwith "owl_symbolic_symbol.input"


let shape = function
  | Variable x -> Variable.(x.shape)
  | Tensor x   -> Tensor.(x.shape)
  | _          -> [||]


let float_value = function
  | Float x -> Float.(x.value)
  | _       -> failwith "owl_symbolic_symbol.float_value"


let int_value = function
  | Int x -> Int.(x.value)
  | _       -> failwith "owl_symbolic_symbol.int_value"


let op_type = function
  | Float _    -> Float.op_type
  | Tensor _   -> Tensor.op_type
  | Add _      -> Add.op_type
  | Sub _      -> Sub.op_type
  | Mul _      -> Mul.op_type
  | Div _      -> Div.op_type
  | Sin _      -> Sin.op_type
  | Cos _      -> Cos.op_type
  | Pow _      -> Pow.op_type
  | Variable _ -> Variable.op_type
  | _          -> failwith "owl_symbolic_symbol.op_type"


let sym_attrs = function
  | Float x    -> Float.(x.attrs)
  | Tensor x   -> Tensor.(x.attrs)
  | Add x      -> Add.(x.attrs)
  | Sub x      -> Sub.(x.attrs)
  | Mul x      -> Mul.(x.attrs)
  | Div x      -> Div.(x.attrs)
  | Sin x      -> Sin.(x.attrs)
  | Cos x      -> Cos.(x.attrs)
  | Pow x      -> Pow.(x.attrs)
  | Variable x -> Variable.(x.attrs)
  | _          -> failwith "owl_symbolic_symbol.sym_attrs: unsupported symbol."
