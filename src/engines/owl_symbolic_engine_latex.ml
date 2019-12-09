(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

type t = string (* or other type? json? *)

let rec to_latex sym_node =
  let sym = Owl_graph.attr sym_node in
  match sym with
  | One _      -> to_latex_one sym_node
  | Int _      -> to_latex_int sym_node
  | Float _    -> to_latex_float sym_node
  | Complex _  -> to_latex_complex sym_node
  | Pi _       -> to_latex_pi sym_node
  | Variable _ -> to_latex_variable sym_node
  | Exp _      -> to_latex_exp sym_node
  | Sin _      -> to_latex_sin sym_node
  | Cos _      -> to_latex_cos sym_node
  | Add _      -> to_latex_add sym_node
  | Mul _      -> to_latex_mul sym_node
  | Div _      -> to_latex_div sym_node
  | Pow _      -> to_latex_pow sym_node
  | Equal _    -> to_latex_equal sym_node
  | _          -> failwith (Printf.sprintf "Not implemented: %s" (op_type sym))


and to_latex_one _ = "1"

and to_latex_pi _ = "\\pi"

and to_latex_variable node =
  let sym = Owl_graph.attr node in
  name sym


and to_latex_int node =
  let sym = Owl_graph.attr node in
  let v = Owl_symbolic_symbol.int_value sym in
  string_of_int v


and to_latex_float node =
  let sym = Owl_graph.attr node in
  let v = Owl_symbolic_symbol.float_value sym in
  if Owl_symbolic_utils.flt_is_int v
  then int_of_float v |> string_of_int
  else string_of_float v


and to_latex_complex node =
  let sym = Owl_graph.attr node in
  let real, img =
    match sym with
    | Complex s -> s.real, s.img
    | _         -> failwith "to_latex_complex: unexpected symbol"
  in
  if real = 0.
  then Printf.sprintf "%.2fi" img
  else if img = 0.
  then Printf.sprintf "%f" real
  else if img = 1.
  then Printf.sprintf "%f+i" real
  else Printf.sprintf "%.2f+%.2fi" real img


and to_latex_div node =
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
  Printf.sprintf "\\cos(%s)" p


(* TODO: but do we really need a class for each symbol, e.g \doteq, \approx... ? 
 * Perhaps set them as parameters.
 *)
and to_latex_equal node =
  let parents = Owl_graph.parents node in
  let lhs = to_latex parents.(0) in
  let rhs = to_latex parents.(1) in
  Printf.sprintf "%s = %s" lhs rhs


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

let html_section section_id embed_dot expr =
  let expr_tex = of_symbolic expr in
  let dot_tex =
    if embed_dot
    then
      Printf.sprintf
        {|
          <div style="text-align:center; padding:20px">
            <button class="btn btn-outline-primary" type="button" data-toggle="collapse" data-target="#viz-graph-%i" aria-expanded="false" aria-controls="viz-graph-%i">
              <i class="fas fa-plus"></i>
              computation graph
            </button>
            <button class="btn btn-outline-primary" type="button" onClick="copyToClipboard('expr-latex-%i')">
              <i class="far fa-copy"></i>
              copy LaTeX
            </button>
          </div>
          <div class="collapse" style="text-align:center" id="viz-graph-%i"></div>
          <script>
            d3.select("#viz-graph-%i").graphviz()
              .fade(false)
              .renderDot(`%s`);
          </script>
        |}
        section_id
        section_id
        section_id
        section_id
        section_id
        (Owl_symbolic_graph.to_dot expr)
    else ""
  in
  Printf.sprintf
    {|
      <div class="container jumbotron" style="padding-top:30px; padding-bottom:20px">
        <h2>
          <span class="badge badge-secondary">
            <i class="fa fa-square-root-alt"></i>
            Expression #%i
          </span>
        </h2>
        <div id="expr-latex-%i" style="visibility:hidden; height:0px">%s</div>
        $$%s$$
        %s
      </div>
    |}
    section_id
    section_id
    expr_tex
    expr_tex
    dot_tex


let html_footer () =
  Printf.sprintf
    {|
      <div class="container" style="width:100%%; text-align:center">
        OCaml Scientific and Engineering Computing <br />
        Copyright (c) 2016-2019 <a href="http://ocaml.xyz">ocaml.xyz</a>
      </div>
    |}


let html ?(dot = false) ~exprs filename =
  let section_id = ref 0 in
  let body =
    List.fold_left
      (fun acc expr ->
        section_id := !section_id + 1;
        acc ^ "\n" ^ html_section !section_id dot expr)
      ""
      exprs
  in
  let footer = html_footer () in
  let html_str =
    Printf.sprintf
      {|
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>Owl - OCaml Scientic and Engineering Computing</title>

    <script src="https://code.jquery.com/jquery-3.4.1.slim.min.js" integrity="sha384-J6qa4849blE2+poT4WnyKhv5vZF5SrPo0iEjwBvKU7imGFAV0wwj1yYfoRSJoZ+n" crossorigin="anonymous"></script>
    <script src="https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js" integrity="sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo" crossorigin="anonymous"></script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js" integrity="sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6" crossorigin="anonymous"></script>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.css" integrity="sha256-46qynGAkLSFpVbEBog43gvNhfrOj+BmwXdxFgVK/Kvc=" crossorigin="anonymous" />

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

    <script>
      function copyToClipboard(latexID) {
        var latexItem = document.getElementById(latexID);
        navigator.clipboard.writeText(latexItem.innerHTML).then(function() {
          console.log("owl: copy to clipboard.");
        }, function(err) {
          console.error('owl: fail to copy latex due to ', err);
        });
      }
    </script>

    <script src="https://cdnjs.cloudflare.com/ajax/libs/d3/5.12.0/d3.min.js"></script>
    <script src="https://unpkg.com/viz.js@1.8.1/viz.js" type="javascript/worker"></script>
    <script src="https://unpkg.com/d3-graphviz@2.6.1/build/d3-graphviz.js"></script>
  </head>
    
  <body>
    <div class="text-center" style="padding:20px">
      <h1> Owl-Symbolic $\LaTeX$ Engine </h1>
    </div>
    %s
    %s
  </body>
</html>
    |}
      body
      footer
  in
  Owl_io.write_file filename html_str
