(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph

val noop : symbolic_node

val int : ?name:string -> int -> symbolic_node

val float : ?name:string -> float -> symbolic_node

val complex : ?name:string -> float -> float -> symbolic_node

val pi : ?name:string -> unit -> symbolic_node

val tensor : ?name:string -> Owl_symbolic_types.tensor -> symbolic_node

val variable
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?shape:int array
  -> ?init:Owl_symbolic_types.tensor
  -> string
  -> symbolic_node

val random_uniform
  :  ?dtype:Owl_symbolic_types.number_type
  -> ?seed:float option
  -> ?low:float
  -> ?high:float
  -> ?name:string
  -> int array
  -> symbolic_node

val equal : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val sin : ?name:string -> symbolic_node -> symbolic_node

val cos : ?name:string -> symbolic_node -> symbolic_node

val tan : ?name:string -> symbolic_node -> symbolic_node

val asin : ?name:string -> symbolic_node -> symbolic_node

val acos : ?name:string -> symbolic_node -> symbolic_node

val atan : ?name:string -> symbolic_node -> symbolic_node

val sinh : ?name:string -> symbolic_node -> symbolic_node

val cosh : ?name:string -> symbolic_node -> symbolic_node

val tanh : ?name:string -> symbolic_node -> symbolic_node

val asinh : ?name:string -> symbolic_node -> symbolic_node

val acosh : ?name:string -> symbolic_node -> symbolic_node

val atanh : ?name:string -> symbolic_node -> symbolic_node

val sqrt : ?name:string -> symbolic_node -> symbolic_node

val exp : ?name:string -> symbolic_node -> symbolic_node

val log : ?name:string -> symbolic_node -> symbolic_node

val sigmoid : ?name:string -> symbolic_node -> symbolic_node

val abs : ?name:string -> symbolic_node -> symbolic_node

val neg : ?name:string -> symbolic_node -> symbolic_node

val floor : ?name:string -> symbolic_node -> symbolic_node

val ceil : ?name:string -> symbolic_node -> symbolic_node

val round : ?name:string -> symbolic_node -> symbolic_node

val relu : ?name:string -> symbolic_node -> symbolic_node

val add : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val sub : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val mul : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val div : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val pow : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val modular : ?name:string -> ?fmod:int -> symbolic_node -> symbolic_node -> symbolic_node

val matmul : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val gemm
  :  ?name:string
  -> ?alpha:float
  -> ?beta:float
  -> ?transA:bool
  -> ?transB:bool
  -> ?c:symbolic_node
  -> symbolic_node
  -> symbolic_node
  -> symbolic_node

val max : ?name:string -> symbolic_node array -> symbolic_node

val min : ?name:string -> symbolic_node array -> symbolic_node

val sum : ?name:string -> symbolic_node array -> symbolic_node

val reduce_sum
  :  ?keepdims:bool
  -> ?name:string
  -> symbolic_node
  -> int array
  -> symbolic_node

val reduce_max
  :  ?keepdims:bool
  -> ?name:string
  -> symbolic_node
  -> int array
  -> symbolic_node

val reduce_min
  :  ?keepdims:bool
  -> ?name:string
  -> symbolic_node
  -> int array
  -> symbolic_node

val reduce_mean
  :  ?keepdims:bool
  -> ?name:string
  -> symbolic_node
  -> int array
  -> symbolic_node

val reduce_sum_square
  :  ?keepdims:bool
  -> ?name:string
  -> symbolic_node
  -> int array
  -> symbolic_node

val reduce_prod
  :  ?keepdims:bool
  -> ?name:string
  -> symbolic_node
  -> int array
  -> symbolic_node

val reshape : ?name:string -> symbolic_node -> symbolic_node -> symbolic_node

val split : ?name:string -> ?axis:int -> symbolic_node -> int array -> symbolic_node array

val concat : ?name:string -> ?axis:int -> symbolic_node array -> symbolic_node

val cast
  :  ?name:string
  -> symbolic_node
  -> Owl_symbolic_types.number_type
  -> symbolic_node

val pad
  :  ?name:string
  -> ?mode:string
  -> ?v:symbolic_node
  -> symbolic_node
  -> int array
  -> symbolic_node

val squeeze : ?name:string -> ?axes:int array -> symbolic_node -> symbolic_node

val tile : ?name:string -> symbolic_node -> int array -> symbolic_node

val conv
  :  ?name:string
  -> ?dim:int
  -> ?padding:Owl_symbolic_types.pad
  -> ?strides:int array
  -> ?dilations:int array
  -> ?bias:symbolic_node
  -> symbolic_node
  -> symbolic_node
  -> symbolic_node

val maxpool
  :  ?name:string
  -> ?strides:int array
  -> ?dilations:int array
  -> ?padding:Owl_symbolic_types.pad
  -> symbolic_node
  -> int array
  -> symbolic_node * symbolic_node

val batch_norm
  :  ?name:string
  -> ?eps:float
  -> ?momentum:float
  -> symbolic_node
  -> symbolic_node
  -> symbolic_node
  -> symbolic_node
  -> symbolic_node
  -> symbolic_node * symbolic_node * symbolic_node * symbolic_node * symbolic_node

val dropout
  :  ?name:string
  -> ?ratio:float
  -> symbolic_node
  -> symbolic_node * symbolic_node

val seq_empty
  :  ?name:string
  -> ?dtype:Owl_symbolic_types.number_type
  -> unit
  -> symbolic_node
