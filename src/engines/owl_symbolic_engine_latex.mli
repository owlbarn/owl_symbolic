(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t = string

val of_symbolic : Owl_symbolic_graph.symbolic_graph -> t
val to_symbolic : t -> Owl_symbolic_graph.symbolic_graph
val save : t -> string -> unit
val load : string -> t
