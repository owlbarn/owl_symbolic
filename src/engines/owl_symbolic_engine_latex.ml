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

let html ?(dot = false) ~exprs filename =
  let html_str = Owl_symbolic_engine_html.make of_symbolic dot exprs in
  Owl_io.write_file filename html_str
