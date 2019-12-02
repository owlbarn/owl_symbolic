open Owl_symbolic
open Op
open Infix
open Type

let x = variable "x"
let y = (int 6 / int 4) + int 1 + x
let g = SymGraph.make_graph [| y |] "sym_graph"

(* Target
let s = Owl_symbolic_cas.to_canonical g
let _ = Owl_symbolic_cas.pprint s
*)
