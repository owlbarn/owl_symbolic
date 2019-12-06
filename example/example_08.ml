open Owl_symbolic
open Op
open Infix

(* construct *)
let y = rational (int 6) (int 4) + int 1 + variable "x_i"
let g = SymGraph.make_graph [| y |] "sym_graph"
(* initial simplification *)
(* let _ = Owl_symbolic_cas_canonical.canonical_form g *)
(* print to LaTeX string *)
let s = LaTeX_Engine.of_symbolic g
(* option 1: print to string *)
let _ = Printf.printf "%s\n" s
(* option 2: print to html with KaTeX *)
let _ = LaTeX_Engine.html "example_08.html" s
(* option 3: print graph to dot/pdf file for debugging *)
(* let _ = Owl_symbolic_graph.to_dot g "example_08.dot"
let _ = Sys.command "dot -Tpdf example_08.dot -o example_08.pdf" *)
