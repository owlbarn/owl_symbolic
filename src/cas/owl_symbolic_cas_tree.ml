(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol
open Owl_symbolic_operator
open Owl_symbolic_infix

(** Convert symbol_graph into functional-rich cse_graph (or tree actually) *)

(* convert sym_graph to cas_graph *)
let build _sym_graph = ()

(* print expressoin on stdout *)
let pprint _cas_graph = ()

(* Operations *)

(* Assume the nodes are already canonically orderred *)
let extract_mul_coeff node =
  let ap = Owl_graph.attr node in
  match ap with
  | Mul _ ->
    let ps = Owl_graph.parents node in
    (match Owl_graph.attr ps.(0) with
    | Rational _ -> ps.(0), ps.(1)
    | NegOne _   -> ps.(0), negone () * ps.(1)
    | _          -> one (), node)
  | _     -> failwith "extract_mul_coeff: not mul op"


let is_rational = function
  | Int _      -> true
  | Rational _ -> true
  | _          -> false


let is_zero = function
  | Int s     -> s.value = 0
  | Float s   -> s.value = 0.
  | Complex s -> s.real = 0. && s.img = 0.
  | _         -> false
