(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t = string (* or other type? json? *)

let of_symbolic (_sym_graph : Owl_symbolic_graph.symbolic_graph) = "dummy_math:\\sum_i^j"
let to_symbolic (_latex_str : string) = Owl_symbolic_graph.null_graph

(** save latex expression to file *)
let save _expr _filename = ()

(** load latex expression from file *)
let load _filename = Obj.magic None
