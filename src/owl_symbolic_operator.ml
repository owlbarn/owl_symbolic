(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_namespace
open Owl_symbolic_graph
open Owl_symbolic_types

(* TODO: check user-defined name is uniq in the whole graph *)

let noop =
  let sym = Owl_symbolic_symbol.NOOP in
  make_node sym [||]


let integer ?name value =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "integer_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Int.create name input attrs value in
  let sym = Owl_symbolic_symbol.Int o in
  make_node sym [||]


let flt ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "float_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let value = x in
  let o = Owl_symbolic_ops_math.Float.create name input attrs value in
  let sym = Owl_symbolic_symbol.Float o in
  make_node sym [||]


let complex ?name r i =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "complex_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Complex.create name input attrs r i in
  let sym = Owl_symbolic_symbol.Complex o in
  make_node sym [||]


let tensor ?name shape =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "tensor_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Tensor.create ~shape name input attrs in
  let sym = Owl_symbolic_symbol.Tensor o in
  make_node sym [||]


let expconst () =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "expconst_%i" suffix in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.ExpConst.create name input attrs in
  let sym = Owl_symbolic_symbol.ExpConst o in
  make_node sym [||]


let variable ?(shape = [||]) ?(typ = SDT_Float) ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "variable_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Variable.create name input attrs typ shape in
  let sym = Owl_symbolic_symbol.Variable o in
  make_node sym [||]


let add ?name x y =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "add_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Add.create name input attrs in
  let sym = Owl_symbolic_symbol.Add o in
  make_node sym [| x; y |]


let pow ?name x y =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "pow_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Pow.create name input attrs in
  let sym = Owl_symbolic_symbol.Pow o in
  make_node sym [| x; y |]


let sin ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "sin_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Sin.create name input attrs in
  let sym = Owl_symbolic_symbol.Sin o in
  make_node sym [| x |]
