(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t

val of_symbolic : Owl_symbolic_graph.symbolic_graph -> Onnx_types.model_proto
val to_symbolic : Onnx_types.model_proto -> Owl_symbolic_graph.symbolic_graph
val save : Onnx_types.model_proto -> string -> unit
val load : string -> Onnx_types.model_proto
