(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
module G = Owl_computation_cpu_engine.Make (Owl_dense_ndarray.S)
open G

type t = G.Type.t

let to_symbolic (_cgraph : t) = { symbols = [||] }

let eval_arr (_sym_graph : symbolic_graph) = ()

let eval_flt (_sym_graph : symbolic_graph) = ()

let of_symbolic (_sym_graph : symbolic_graph) =
  let attr =
    { op = Noop
    ; freeze = true
    ; reuse = false
    ; state = Valid
    ; shape = [||]
    ; value = [||]
    ; block = None
    }
  in
  Owl_graph.node attr


