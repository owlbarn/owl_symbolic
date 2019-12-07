open Owl_symbolic
open Op
open Infix

let make_expr0 () =
  (* construct *)
  let x = variable "x_0" in
  let y =
    exp ((sin x ** float 2.) + (cos x ** float 2.))
    + (float 10. * (x ** float 2.))
    + exp (pi () * complex 0. 1.)
  in
  let expr = SymGraph.make_graph [| y |] "sym_graph" in
  (* to LaTeX string *)
  LaTeX_Engine.of_symbolic expr |> print_endline;
  expr


let make_expr1 () =
  (* construct *)
  let x = variable "x_i" in 
  let y = rational (int 6) (int 4) * x + (int 2) * x in
  let expr = SymGraph.make_graph [| y |] "sym_graph" in
  (* initial simplification *)
  let _ = Owl_symbolic_cas_canonical.canonical_form expr in
  (* print to html for debugging *)
  (* let s = Owl_symbolic_graph.to_dot expr in 
  let _ = Owl_io.write_file "example_08.dot" s in
  Sys.command "dot -Tpdf example_08.dot -o example_08.pdf" |> ignore; *)
  LaTeX_Engine.of_symbolic expr |> print_endline;
  expr


let make_expr2 () =
  (* construct *)
  let y = int 6 + variable "x_0" + variable "x_1" in
  let expr = SymGraph.make_graph [| y |] "sym_graph" in
  LaTeX_Engine.of_symbolic expr |> print_endline;
  expr


let _ =
  let exprs = [ make_expr1 (); ] in
  LaTeX_Engine.html ~dot:true ~exprs "example_08.html"
