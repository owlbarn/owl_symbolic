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

type t =
  | NOOP
  | Int of Owl_symbolic_ops_math.Int.t
  | Complex of Owl_symbolic_ops_math.Complex.t
  | Float of Owl_symbolic_ops_math.Float.t
  | Tensor of Owl_symbolic_ops_math.Tensor.t
  | ExpConst of Owl_symbolic_ops_math.ExpConst.t
  | Symbol of Owl_symbolic_ops_math.Symbol.t
  | Sin of Owl_symbolic_ops_math.Sin.t
  | Cos of Owl_symbolic_ops_math.Cos.t
  | Exp of Owl_symbolic_ops_math.Exp.t
  | Add of Owl_symbolic_ops_math.Add.t
  | Sub of Owl_symbolic_ops_math.Sub.t
  | Mul of Owl_symbolic_ops_math.Mul.t
  | Div of Owl_symbolic_ops_math.Div.t
  | Pow of Owl_symbolic_ops_math.Pow.t

let name = function
  | Float x  -> Owl_symbolic_ops_math.Float.(x.name)
  | Tensor x -> Owl_symbolic_ops_math.Tensor.(x.name)
  | Add x    -> Owl_symbolic_ops_math.Add.(x.name)
  | Sub x    -> Owl_symbolic_ops_math.Sub.(x.name)
  | Mul x    -> Owl_symbolic_ops_math.Mul.(x.name)
  | Div x    -> Owl_symbolic_ops_math.Div.(x.name)
  | Sin x    -> Owl_symbolic_ops_math.Sin.(x.name)
  | Cos x    -> Owl_symbolic_ops_math.Cos.(x.name)
  | Pow x    -> Owl_symbolic_ops_math.Pow.(x.name)
  | _        -> failwith "owl_symbolic_symbol.name"


let input = function
  | Float x  -> Owl_symbolic_ops_math.Float.(x.input)
  | Tensor x -> Owl_symbolic_ops_math.Tensor.(x.input)
  | Add x    -> Owl_symbolic_ops_math.Add.(x.input)
  | Sub x    -> Owl_symbolic_ops_math.Sub.(x.input)
  | Mul x    -> Owl_symbolic_ops_math.Mul.(x.input)
  | Div x    -> Owl_symbolic_ops_math.Div.(x.input)
  | Sin x    -> Owl_symbolic_ops_math.Sin.(x.input)
  | Cos x    -> Owl_symbolic_ops_math.Cos.(x.input)
  | Pow x    -> Owl_symbolic_ops_math.Pow.(x.input)
  | _        -> failwith "owl_symbolic_symbol.input"


let output = function
  | Float x  -> Owl_symbolic_ops_math.Float.(x.output)
  | Tensor x -> Owl_symbolic_ops_math.Tensor.(x.output)
  | Add x    -> Owl_symbolic_ops_math.Add.(x.output)
  | Sub x    -> Owl_symbolic_ops_math.Sub.(x.output)
  | Mul x    -> Owl_symbolic_ops_math.Mul.(x.output)
  | Div x    -> Owl_symbolic_ops_math.Div.(x.output)
  | Sin x    -> Owl_symbolic_ops_math.Sin.(x.output)
  | Cos x    -> Owl_symbolic_ops_math.Cos.(x.output)
  | Pow x    -> Owl_symbolic_ops_math.Pow.(x.output)
  | _        -> failwith "owl_symbolic_symbol.output"


let shape = function
  | _ -> failwith "owl_symbolic_symbol.shape"


let value = function
  | Float x -> Owl_symbolic_ops_math.Float.(x.value)
  | _       -> failwith "owl_symbolic_symbol.float"


let op_type = function
  | Add _ -> Owl_symbolic_ops_math.Add.op_type
  | Sub _ -> Owl_symbolic_ops_math.Sub.op_type
  | Mul _ -> Owl_symbolic_ops_math.Mul.op_type
  | Div _ -> Owl_symbolic_ops_math.Div.op_type
  | _     -> failwith "owl_symbolic_symbol.op_type"


(** Question: what if I want to move one math symbol to another group such as , say, nn? *)
