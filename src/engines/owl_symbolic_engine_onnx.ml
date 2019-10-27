open Owl_symbolic_types

type t = Onnx_types.graph_proto

let of_symbolic sym_graph =
  let _syms = sym_graph.symbols in
  ()
