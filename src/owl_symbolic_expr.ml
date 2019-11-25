(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph

let has _expr _e = true
let xreplace () = ()

(* Remove common factors from terms in all arguments without
  changing the underlying structure of the expr. No expansion or
  simplification (and no processing of non-commutatives) is performed. 
  Example: factor_terms(-x - y) -> - (x + y)
  *)
let factor_terms (expr : symbolic_node) = expr
