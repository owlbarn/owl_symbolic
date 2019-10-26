(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_utils

let add x y =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "add_%i" suffix in
  let x_name = Owl_symbolic_symbol.name x in
  let y_name = Owl_symbolic_symbol.name y in
  let o_name = "z" in
  let input = [ x_name; y_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Add.create name input output in
  Owl_symbolic_symbol.Add o
