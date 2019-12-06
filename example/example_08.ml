open Owl_symbolic
open Op
open Infix

(* construct *)
let y = rational (int 6) (int 4) + int 1 + variable "x_i"
let g = SymGraph.make_graph [| y |] "sym_graph"
(* initial simplification *)
let _ = Owl_symbolic_cas_canonical.canonical_form g
(* print to LaTeX string *)
let s = LaTeX_Engine.of_symbolic g
let _ = Printf.printf "%s\n" s
(* print graph to dot/pdf file *)
let _ = Owl_symbolic_graph.to_dot g "foo.dot"
