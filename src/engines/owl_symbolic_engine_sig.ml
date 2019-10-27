open Owl_symbolic_types

module type Sig = sig

  type t

  val of_symbolic : symbolic_graph -> t

  val to_symbolic : t -> symbolic_graph

end
