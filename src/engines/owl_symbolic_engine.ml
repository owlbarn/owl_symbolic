(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make (S : Owl_symbolic_engine_sig.Sig) = struct

  module Symbolic = S

  let to_symbolic = S.to_symbolic

  let of_symbolic = S.of_symbolic

end
