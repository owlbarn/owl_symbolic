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
  | Complex _  -> to_latex_complex sym_node
  | Pi _       -> "\\pi"
  | Rational _ -> to_latex_rational sym_node
  | Variable _ -> name sym
  | Exp _      -> to_latex_exp sym_node
  | Sin _      -> to_latex_sin sym_node
  | Cos _      -> to_latex_cos sym_node
  | Add _      -> to_latex_add sym_node
  | Mul _      -> to_latex_mul sym_node
  | Pow _      -> to_latex_pow sym_node
  | _          -> failwith (Printf.sprintf "Not implemented: %s" (op_type sym))


and to_latex_complex node =
  let sym = Owl_graph.attr node in
  let real, img =
    match sym with
    | Complex s -> s.real, s.img
    | _         -> failwith "to_latex_complex: unexpected symbol"
  in
  if real = 0.
  then Printf.sprintf "%fi" img
  else if img = 0.
  then Printf.sprintf "%f" real
  else if img = 1.
  then Printf.sprintf "%f+i" real
  else Printf.sprintf "%f+%fi" real img


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
  assert (Array.length parents = 2);
  let ps = Array.map to_latex parents in
  Printf.sprintf "%s\\times %s" ps.(0) ps.(1)


and to_latex_pow node =
  let parents = Owl_graph.parents node in
  assert (Array.length parents = 2);
  let ps = Array.map to_latex parents in
  ps.(0) ^ "^" ^ ps.(1)


and to_latex_exp node =
  let parents = Owl_graph.parents node in
  assert (Array.length parents = 1);
  let p = to_latex parents.(0) in
  Printf.sprintf "\\exp(%s)" p


and to_latex_sin node =
  let parents = Owl_graph.parents node in
  assert (Array.length parents = 1);
  let p = to_latex parents.(0) in
  Printf.sprintf "\\sin(%s)" p


and to_latex_cos node =
  let parents = Owl_graph.parents node in
  assert (Array.length parents = 1);
  let p = to_latex parents.(0) in
  Printf.sprintf "\\sin(%s)" p


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

let one_section section_id embed_dot expr =
  let expr_tex = of_symbolic expr in
  let dot_tex =
    if embed_dot
    then
      Printf.sprintf
        {|
        <div class="container" id="viz-graph-%i"></div>
        <script>
          d3.select("#viz-graph-%i").graphviz()
            .fade(false)
            .renderDot(`%s`);
        </script>
      |}
        section_id
        section_id
        (Owl_symbolic_graph.to_dot expr)
    else ""
  in
  Printf.sprintf
    {|
      <div class="container jumbotron">
        <h2>Expression #%i</h2>
        $$%s$$
      </div>
      %s
    |}
    section_id
    expr_tex
    dot_tex


let html ?(dot = false) ~exprs filename =
  let section_id = ref 0 in
  let tex =
    List.fold_left
      (fun acc expr ->
        section_id := !section_id + 1;
        acc ^ "\n" ^ one_section !section_id dot expr)
      ""
      exprs
  in
  let html_str =
    Printf.sprintf
      {|
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
    <script src="https://cdnjs.cloudflare.com/ajax/libs/d3/5.12.0/d3.min.js"></script>
    <script src="https://unpkg.com/viz.js@1.8.1/viz.js" type="javascript/worker"></script>
    <script src="https://unpkg.com/d3-graphviz@2.6.1/build/d3-graphviz.js"></script>
  </head>
    
  <body>
    <div class="jumbotron text-center">
      <h1> Owl-Symbolic $\LaTeX$ Engine </h1>
    </div>
    %s
  </body>
</html>
  |}
      tex
  in
  Owl_io.write_file filename html_str
