(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

val init : Owl_symbolic_types.nn_init -> int array -> Owl_symbolic_graph.symbol

val activation
  :  ?name:string
  -> Owl_symbolic_types.activation
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val input
  :  ?name:string
  -> ?dtype:Owl_symbolic_types.number_type
  -> int array
  -> Owl_symbolic_graph.symbol

val max_pool2d
  :  ?name:string
  -> ?padding:Owl_symbolic_types.pad
  -> int array
  -> int array
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val avg_pool2d
  :  ?name:string
  -> ?padding:Owl_symbolic_types.pad
  -> int array
  -> int array
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val global_max_pool2d
  :  ?name:string
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val global_avg_pool2d
  :  ?name:string
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val dropout
  :  ?name:string
  -> float
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val lambda
  :  (Owl_symbolic_graph.symbol -> Owl_symbolic_graph.symbol)
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val fully_connected
  :  ?init_typ:Owl_symbolic_types.nn_init
  -> int
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val conv2d
  :  ?name:string
  -> ?padding:Owl_symbolic_types.pad
  -> ?init_typ:Owl_symbolic_types.nn_init
  -> int array
  -> int array
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val transpose_conv2d
  :  ?name:string
  -> ?padding:Owl_symbolic_types.pad
  -> ?init_typ:Owl_symbolic_types.nn_init
  -> int array
  -> int array
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val linear
  :  ?init_typ:Owl_symbolic_types.nn_init
  -> int
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val normalisation
  :  ?name:string
  -> ?_axis:'a
  -> ?eps:float
  -> ?momentum:float
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val concat
  :  ?name:string
  -> ?axis:int
  -> Owl_symbolic_graph.symbol array
  -> Owl_symbolic_graph.symbol

val add
  :  ?name:string
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol
  -> Owl_symbolic_graph.symbol

val flt : ?name:string -> float -> Owl_symbolic_graph.symbol

val tanh : ?name:string -> Owl_symbolic_graph.symbol -> Owl_symbolic_graph.symbol

val get_network : ?name:string -> Owl_symbolic_graph.symbol -> Owl_symbolic_graph.t
