(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_utils
open Owl_symbolic_graph

(** Signatures: unit/graph -> graph ... -> graph *)

(** TODO: The design of `make_graph` feels like wrong... 
  * check owl.to_symbolic *)
let one () =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "one_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.One.create name input output in
  let node = Owl_symbolic_symbol.One o in
  make_graph node [| null_graph |]


let ones shape =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "ones_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Ones.create ~shape name input output in
  let node = Owl_symbolic_symbol.Ones o in
  make_graph node [| null_graph |]


let flt x =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "float_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let value = x in
  let o = Owl_symbolic_ops_math.Float.create name input output value in
  let node = Owl_symbolic_symbol.Float o in
  make_graph node [| null_graph |]


let add x y =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "add_%i" suffix in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let o_name = "z" in
  let input = [ x_name; y_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Add.create name input output in
  let node = Owl_symbolic_symbol.Add o in
  make_graph node [| x; y |]


let pow x y =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "pow_%i" suffix in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let o_name = "z" in
  let input = [ x_name; y_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Pow.create name input output in
  let node = Owl_symbolic_symbol.Pow o in
  make_graph node [| x; y |]


let sin x =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "sin_%i" suffix in
  let x_name = Owl_symbolic_graph.name x in
  let o_name = "y" in
  let input = [ x_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Sin.create name input output in
  let node = Owl_symbolic_symbol.Sin o in
  make_graph node [| x |]


let var id =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "var_%i" suffix in
  let o_name = "x" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Var.create name input output id in
  let node = Owl_symbolic_symbol.Var o in
  make_graph node [| null_graph |]
