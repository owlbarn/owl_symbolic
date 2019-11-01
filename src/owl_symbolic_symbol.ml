(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t =
  | NOOP
  | Add   of Owl_symbolic_ops_math.Add.t
  | Sub   of Owl_symbolic_ops_math.Sub.t
  | Mul   of Owl_symbolic_ops_math.Mul.t
  | Div   of Owl_symbolic_ops_math.Div.t
  | Sin   of Owl_symbolic_ops_math.Sin.t
  | Cos   of Owl_symbolic_ops_math.Cos.t
  | Pow   of Owl_symbolic_ops_math.Pow.t
  | One   of Owl_symbolic_ops_math.One.t
  | Var   of Owl_symbolic_ops_math.Var.t
  | Ones  of Owl_symbolic_ops_math.Ones.t
  | Float of Owl_symbolic_ops_math.Float.t

let name = function
  | Add x   -> Owl_symbolic_ops_math.Add.(x.name)
  | Sub x   -> Owl_symbolic_ops_math.Sub.(x.name)
  | Mul x   -> Owl_symbolic_ops_math.Mul.(x.name)
  | Div x   -> Owl_symbolic_ops_math.Div.(x.name)
  | Sin x   -> Owl_symbolic_ops_math.Sin.(x.name)
  | Cos x   -> Owl_symbolic_ops_math.Cos.(x.name)
  | Pow x   -> Owl_symbolic_ops_math.Pow.(x.name)
  | One x   -> Owl_symbolic_ops_math.One.(x.name)
  | Var x   -> Owl_symbolic_ops_math.Var.(x.name)
  | Ones x  -> Owl_symbolic_ops_math.Ones.(x.name)
  | Float x -> Owl_symbolic_ops_math.Float.(x.name)
  | _       -> failwith "owl_symbolic_symbol.name"


let input = function
  | Add x   -> Owl_symbolic_ops_math.Add.(x.input)
  | Sub x   -> Owl_symbolic_ops_math.Sub.(x.input)
  | Mul x   -> Owl_symbolic_ops_math.Mul.(x.input)
  | Div x   -> Owl_symbolic_ops_math.Div.(x.input)
  | Sin x   -> Owl_symbolic_ops_math.Sin.(x.input)
  | Cos x   -> Owl_symbolic_ops_math.Cos.(x.input)
  | Pow x   -> Owl_symbolic_ops_math.Pow.(x.input)
  | One x   -> Owl_symbolic_ops_math.One.(x.input)
  | Var x   -> Owl_symbolic_ops_math.Var.(x.input)
  | Ones x  -> Owl_symbolic_ops_math.Ones.(x.input)
  | Float x -> Owl_symbolic_ops_math.Float.(x.input)
  | _       -> failwith "owl_symbolic_symbol.input"


let output = function
  | Add x   -> Owl_symbolic_ops_math.Add.(x.output)
  | Sub x   -> Owl_symbolic_ops_math.Sub.(x.output)
  | Mul x   -> Owl_symbolic_ops_math.Mul.(x.output)
  | Div x   -> Owl_symbolic_ops_math.Div.(x.output)
  | Sin x   -> Owl_symbolic_ops_math.Sin.(x.output)
  | Cos x   -> Owl_symbolic_ops_math.Cos.(x.output)
  | Pow x   -> Owl_symbolic_ops_math.Pow.(x.output)
  | One x   -> Owl_symbolic_ops_math.One.(x.output)
  | Var x   -> Owl_symbolic_ops_math.Var.(x.output)
  | Ones x  -> Owl_symbolic_ops_math.Ones.(x.output)
  | Float x -> Owl_symbolic_ops_math.Float.(x.output)
  | _       -> failwith "owl_symbolic_symbol.output"


let shape = function
  | Ones x -> Owl_symbolic_ops_math.Ones.(x.shape)
  | Var x  -> Owl_symbolic_ops_math.Var.(x.shape)
  | _      -> failwith "owl_symbolic_symbol.shape"


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
