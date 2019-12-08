(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol
open Owl_symbolic_operator
open Owl_graph

let rec cas_add (x : t node) (y : t node) =
  let ax = attr x in
  match ax with
  | Div _ -> _add_div x y 
  | Int _ -> _add_int x y
  | _     -> add x y


and _add_int x y = 
  let ax = attr x in
  let ay = attr y in
  match ay with
    | Div _   -> cas_add y x
    | Int _   ->
      let v = int_value ax + int_value ay in
      int v
    | Float _ -> cas_add y x
    | _       -> add x y


and _add_div x y = 
  (* TODO: we need the inference rules *)
  let ay = attr y in
  let ps = parents x in
  let p = attr ps.(0) |> int_value in
  let q = attr ps.(1) |> int_value in
  match ay with
  | Int ys  ->
    let pn = int (p + (q * ys.value)) in
    let qn = int q in
    (* TODO: remove old nodes? or use that in the canonical only? *)
    div pn qn
  | Div _   ->
    let ps2 = parents x in
    let p2 = attr ps2.(0) |> int_value in
    let q2 = attr ps2.(1) |> int_value in
    let pn = int ((p * q2) + (q * p2)) in
    let qn = int (q * q2) in
    div pn qn
  | Float _ -> cas_add y x
  | _       -> add x y