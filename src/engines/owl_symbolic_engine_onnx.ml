(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_specs

type t = Onnx_types.graph_proto

(* Dummy impl *)

let of_symbolic sym_graph =
  let syms = sym_graph.symbols in
  let _sym = syms.(0) in
  let default_graph = PT.default_graph_proto () in
  default_graph


let to_symbolic (_onnx_graph : t) = { symbols = [||] }
