(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

type t = string (* or other type? json? *)

let rec to_latex sym_node =
  let sym = Owl_graph.attr sym_node in
  match sym with
  | One _      -> "1"
  | Int _      -> Owl_symbolic_symbol.int_value sym |> string_of_int
  | Float _    -> Owl_symbolic_symbol.float_value sym |> string_of_float
  | Rational _ -> to_latex_rational sym_node
  | Variable _ -> name sym
  | Add _      -> to_latex_add sym_node
  | Mul _      -> to_latex_mul sym_node
  | _          -> failwith (Printf.sprintf "Not implemented: %s" (op_type sym))


and to_latex_rational node =
  let parents = Owl_graph.parents node in
  let p = to_latex parents.(0) in
  let q = to_latex parents.(1) in
  Printf.sprintf "\\frac{%s}{%s}" p q


and to_latex_add node =
  let parents = Owl_graph.parents node in
  let tex =
    Array.fold_left
      (fun s p ->
        (* TODO: if p contains negone use -; if we need brackets *)
        let ptex = to_latex p in
        s ^ "+" ^ ptex)
      ""
      parents
  in
  String.sub tex 1 (String.length tex - 1)


(* TODO: a lot more special cases... *)
and to_latex_mul node =
  let parents = Owl_graph.parents node in
  let tex =
    Array.fold_left
      (fun s p ->
        let ptex = to_latex p in
        s ^ " \\times " ^ ptex)
      ""
      parents
  in
  String.sub tex 1 (String.length tex - 1)


let of_symbolic (sym_graph : Owl_symbolic_graph.t) =
  let output_node = Owl_symbolic_graph.get_output_nodes sym_graph in
  to_latex output_node.(0)


(* Do we truly need this direction? Or perhaps a `printer` class suits better? *)
let to_symbolic (_latex_str : string) = Owl_symbolic_graph.null_graph

(** save latex expression to file *)
let save _expr _filename = ()

(** load latex expression from file *)
let load _filename = Obj.magic None


(** Helper functions *)

let html filename tex = 
  let html_str = Printf.sprintf {|
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>Owl - OCaml Scientic and Engineering Computing</title>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.css" integrity="sha384-zB1R0rpPzHqg7Kpt0Aljp8JPLqbXI3bhnPWROx27a9N0Ll6ZP/+DiW/UqRcLbRjq" crossorigin="anonymous">
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/katex.min.js" integrity="sha384-y23I5Q6l+B6vatafAwxRu/0oK/79VlbSz7Q9aiSZUvyWYIYsd+qj+o24G5ZU2zJz" crossorigin="anonymous"></script>
    <script defer src="https://cdn.jsdelivr.net/npm/katex@0.11.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"
      onload="renderMathInElement(document.body);"></script>
    <script>
      document.addEventListener("DOMContentLoaded", function() {
        renderMathInElement(document.body,{
          delimiters: [
            {left: "<math>", right: "</math>", display: false},
            {left: "$$", right: "$$", display: true},
            {left: "$", right: "$", display: false},
            {left: "\\[", right: "\\]", display: true},
            {left: "\\(", right: "\\)", display: false},
          ]}
      );
    });
    </script>
  </head>
  	
  <body>
  	<div class="jumbotron text-center">
  		<h1> Owl-Symbolic in $\LaTeX$ </h1>
	  </div>
  	<div class="container jumbotron">
  		$$%s$$
	  </div>
  </body>
</html>
  |} tex in
  Owl_io.write_file filename html_str