open Owl_symbolic
open Op
open Infix

let test_print () =
  (* construct *)
  let x = variable "x_0" in
  let y =
    exp ((sin x ** float 2.) + (cos x ** float 2.))
    + (float 10. * (x ** float 2.))
    + exp (pi () * complex 0. 1.)
  in
  let g = SymGraph.make_graph [| y |] "sym_graph" in
  (* print to LaTeX string *)
  let s = LaTeX_Engine.of_symbolic g in
  (* option 2: print to html with KaTeX *)
  LaTeX_Engine.html "example_08.html" s


let test_canonical () =
  (* construct *)
  let y = rational (int 6) (int 4) + int 1 + variable "x_i" in
  let g = SymGraph.make_graph [| y |] "sym_graph" in
  (* initial simplification *)
  let _ = Owl_symbolic_cas_canonical.canonical_form g in
  (* print to LaTeX string *)
  let s = LaTeX_Engine.of_symbolic g in
  (* option 2: print to html with KaTeX *)
  LaTeX_Engine.html "example_08.html" s


(* option 3: print graph to dot/pdf file for debugging *)
(* let _ = Owl_symbolic_graph.to_dot g "example_08.dot"
  let _ = Sys.command "dot -Tpdf example_08.dot -o example_08.pdf" *)

let _ = test_print ()
