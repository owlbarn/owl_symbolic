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
