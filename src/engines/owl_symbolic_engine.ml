module Make (S : Owl_symbolic_engine_sig.Sig) = struct

  module Symbolic = S

  let to_symbolic = S.to_symbolic

  let of_symbolic = S.of_symbolic

end
