(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t =
  | NOOP
  | Int of Owl_symbolic_ops_generator.Int.t
  | Complex of Owl_symbolic_ops_generator.Complex.t
  | Float of Owl_symbolic_ops_generator.Float.t
  | Tensor of Owl_symbolic_ops_generator.Tensor.t
  | Variable of Owl_symbolic_ops_generator.Variable.t
  | RandomUniform of Owl_symbolic_ops_generator.RandomUniform.t
  | RandomNormal of Owl_symbolic_ops_generator.RandomNormal.t
  | Zero of Owl_symbolic_ops_generator.Zero.t
  | One of Owl_symbolic_ops_generator.One.t
  | NegOne of Owl_symbolic_ops_generator.NegOne.t
  | Pi of Owl_symbolic_ops_generator.Pi.t
  | Sin of Owl_symbolic_ops_math.Sin.t
  | Cos of Owl_symbolic_ops_math.Cos.t
  | Tan of Owl_symbolic_ops_math.Tan.t
  | Asin of Owl_symbolic_ops_math.Asin.t
  | Acos of Owl_symbolic_ops_math.Acos.t
  | Atan of Owl_symbolic_ops_math.Atan.t
  | Sinh of Owl_symbolic_ops_math.Sinh.t
  | Cosh of Owl_symbolic_ops_math.Cosh.t
  | Tanh of Owl_symbolic_ops_math.Tanh.t
  | Asinh of Owl_symbolic_ops_math.Asinh.t
  | Acosh of Owl_symbolic_ops_math.Acosh.t
  | Atanh of Owl_symbolic_ops_math.Atanh.t
  | Sqrt of Owl_symbolic_ops_math.Sqrt.t
  | Exp of Owl_symbolic_ops_math.Exp.t
  | Log of Owl_symbolic_ops_math.Log.t
  | Sigmoid of Owl_symbolic_ops_math.Sigmoid.t
  | Relu of Owl_symbolic_ops_math.Relu.t
  | Abs of Owl_symbolic_ops_math.Abs.t
  | Neg of Owl_symbolic_ops_math.Neg.t
  | Floor of Owl_symbolic_ops_math.Floor.t
  | Ceil of Owl_symbolic_ops_math.Ceil.t
  | Round of Owl_symbolic_ops_math.Round.t
  | Rational of Owl_symbolic_ops_math.Rational.t
  | Add of Owl_symbolic_ops_math.Add.t
  | Sub of Owl_symbolic_ops_math.Sub.t
  | Mul of Owl_symbolic_ops_math.Mul.t
  | Div of Owl_symbolic_ops_math.Div.t
  | Pow of Owl_symbolic_ops_math.Pow.t
  | Mod of Owl_symbolic_ops_math.Mod.t
  | MatMul of Owl_symbolic_ops_math.MatMul.t
  | Gemm of Owl_symbolic_ops_math.Gemm.t
  | Max of Owl_symbolic_ops_math.Max.t
  | Min of Owl_symbolic_ops_math.Min.t
  | Sum of Owl_symbolic_ops_math.Sum.t
  | And of Owl_symbolic_ops_logical.And.t
  | Or of Owl_symbolic_ops_logical.Or.t
  | Not of Owl_symbolic_ops_logical.Not.t
  | Xor of Owl_symbolic_ops_logical.Xor.t
  | Equal of Owl_symbolic_ops_logical.Equal.t
  | ReduceSum of Owl_symbolic_ops_reduction.ReduceSum.t
  | ReduceMax of Owl_symbolic_ops_reduction.ReduceMax.t
  | ReduceMin of Owl_symbolic_ops_reduction.ReduceMin.t
  | ReduceMean of Owl_symbolic_ops_reduction.ReduceMean.t
  | ReduceSumSquare of Owl_symbolic_ops_reduction.ReduceSumSquare.t
  | ReduceProd of Owl_symbolic_ops_reduction.ReduceProd.t
  | ReduceLogSum of Owl_symbolic_ops_reduction.ReduceLogSum.t
  | ReduceLogSumExp of Owl_symbolic_ops_reduction.ReduceLogSumExp.t
  | ReduceL1 of Owl_symbolic_ops_reduction.ReduceL1.t
  | ReduceL2 of Owl_symbolic_ops_reduction.ReduceL2.t
  | Reshape of Owl_symbolic_ops_tensor.Reshape.t
  | Identity of Owl_symbolic_ops_tensor.Identity.t
  | Split of Owl_symbolic_ops_tensor.Split.t
  | Concat of Owl_symbolic_ops_tensor.Concat.t
  | Pad of Owl_symbolic_ops_tensor.Pad.t
  | Cast of Owl_symbolic_ops_tensor.Cast.t
  | Squeeze of Owl_symbolic_ops_tensor.Squeeze.t
  | Tile of Owl_symbolic_ops_tensor.Tile.t
  | Shape of Owl_symbolic_ops_tensor.Shape.t
  | Size of Owl_symbolic_ops_tensor.Size.t
  | Transpose of Owl_symbolic_ops_tensor.Transpose.t
  | Slice of Owl_symbolic_ops_tensor.Slice.t
  | SpaceToDepth of Owl_symbolic_ops_tensor.SpaceToDepth.t
  | IsNaN of Owl_symbolic_ops_tensor.IsNaN.t
  | NonZero of Owl_symbolic_ops_tensor.NonZero.t
  | Where of Owl_symbolic_ops_tensor.Where.t
  | Conv of Owl_symbolic_ops_nn.Conv.t
  | MaxPool of Owl_symbolic_ops_nn.MaxPool.t
  | BatchNormalization of Owl_symbolic_ops_nn.BatchNormalization.t
  | Dropout of Owl_symbolic_ops_nn.Dropout.t
  | SequenceEmpty of Owl_symbolic_ops_sequence.SequenceEmpty.t

val name : t -> string

val op_type : t -> string

val input : t -> string array

val set_input : t -> string array -> unit

val out_shape : t -> int array option array

val set_out_shape : t -> int array option array -> unit

val attrs : t -> (string * Owl_symbolic_types.attrvalue) array

val set_attrs : t -> (string * Owl_symbolic_types.attrvalue) array -> unit

val output : t -> string array

val dtype : t -> Owl_symbolic_types.number_type

val shape : t -> int array

val axes : t -> int array

val float_value : t -> float

val int_value : t -> int

val complex_value : t -> float * float

val tensor_value : t -> Owl_symbolic_types.tensor

val initializer_ : t -> Owl_symbolic_types.tensor option

val compare : t -> t -> int
