open Owl_symbolic
open Op

open Infix
(* open Type *)

(* 
let x = variable "x" 
let y = (int 6 / int 4) + int 1 + x *)

let y = rational (int 6) (int 4) + (int 1)
let g = SymGraph.make_graph [| y |] "sym_graph"

(* Target *)
let _ = Owl_symbolic_cas_canonical.canonical_form g 
 
let s = LaTeX_Engine.of_symbolic g

let _ = Owl_symbolic_graph.to_dot g "foo.dot"

let _ = Printf.printf "%s\n" s

(* let _ = Owl_symbolic_cas.pprint s *)
