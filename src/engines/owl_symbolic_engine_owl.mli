(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make (G : Owl_computation_engine_sig.Flatten_Sig) : sig
  type t = G.graph

  val to_symbolic : t -> Owl_symbolic_graph.t
  val of_symbolic : Owl_symbolic_graph.t -> t
  val save : 'a -> 'b -> unit
  val load : 'a -> 'b
end
