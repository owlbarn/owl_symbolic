(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_utils
open Owl_symbolic_graph

(** Signatures: unit/graph -> graph ... -> graph *)

(** TODO: The design of `make_graph` feels like wrong... 
  * check owl.to_symbolic *)
let integer value =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "integer_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Int.create name input output value in
  let node = Owl_symbolic_symbol.Int o in
  make_graph node [||]


let flt x =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "float_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let value = x in
  let o = Owl_symbolic_ops_math.Float.create name input output value in
  let node = Owl_symbolic_symbol.Float o in
  make_graph node [||]


let complex r i =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "complex_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Complex.create name input output r i in
  let node = Owl_symbolic_symbol.Complex o in
  make_graph node [||]


let tensor ?(id = "") shape =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "tensor_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Tensor.create ~shape name input output id in
  let node = Owl_symbolic_symbol.Tensor o in
  make_graph node [||]


let expconst () =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "expconst_%i" suffix in
  let o_name = "a" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.ExpConst.create name input output in
  let node = Owl_symbolic_symbol.ExpConst o in
  make_graph node [||]


let symbol id =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "var_%i" suffix in
  let o_name = "x" in
  let input = [] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Symbol.create name input output id in
  let node = Owl_symbolic_symbol.Symbol o in
  make_graph node [||]


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
