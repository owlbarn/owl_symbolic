(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t =
  | NOOP
  | Add of Owl_symbolic_ops_math.Add.t
  | Sub of Owl_symbolic_ops_math.Sub.t
  | Mul of Owl_symbolic_ops_math.Mul.t
  | Div of Owl_symbolic_ops_math.Div.t
  | Sin of Owl_symbolic_ops_math.Sin.t
  | Cos of Owl_symbolic_ops_math.Cos.t
  | Pow of Owl_symbolic_ops_math.Pow.t
  | One of Owl_symbolic_ops_math.One.t
  | Ones of Owl_symbolic_ops_math.Ones.t 
  | Float of Owl_symbolic_ops_math.Float.t


let name = function
  | Add x -> Owl_symbolic_ops_math.Add.(x.name)
  | Sub x -> Owl_symbolic_ops_math.Sub.(x.name)
  | Mul x -> Owl_symbolic_ops_math.Mul.(x.name)
  | Div x -> Owl_symbolic_ops_math.Div.(x.name)
  | Sin x -> Owl_symbolic_ops_math.Sin.(x.name)
  | Cos x -> Owl_symbolic_ops_math.Cos.(x.name)
  | Pow x -> Owl_symbolic_ops_math.Pow.(x.name)
  | One x -> Owl_symbolic_ops_math.One.(x.name)
  | Ones x -> Owl_symbolic_ops_math.Ones.(x.name)
  | Float x -> Owl_symbolic_ops_math.Float.(x.name)
  | _     -> failwith "owl_symbolic_symbol.name"


let op_type = function
  | Add _ -> Owl_symbolic_ops_math.Add.op_type
  | Sub _ -> Owl_symbolic_ops_math.Sub.op_type
  | Mul _ -> Owl_symbolic_ops_math.Mul.op_type
  | Div _ -> Owl_symbolic_ops_math.Div.op_type
  | _     -> failwith "owl_symbolic_symbol.op_type"
