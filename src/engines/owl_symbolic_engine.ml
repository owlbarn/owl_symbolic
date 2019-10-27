module Make (E : Owl_symbolic_engine_sig.Sig) = struct
  type t

  include Specs

  val to_symbolic : Owl_engine.t -> Owl_symbolic_symbol.t
  val of_symbolic : Owl_symbolic_symbol.t -> Owl_engine.t
end
