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


(* Do we really a "Tensor" node ?
let tensor ?name ?flt_val ?int_val ?str_val ?complex_val shape =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "tensor_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o =
    Owl_symbolic_ops_math.Tensor.create
      ~shape
      ~flt_val
      ~int_val
      ~str_val
      ~complex_val
      name
      input
      attrs
  in
  let sym = Owl_symbolic_symbol.Tensor o in
  make_node sym [||]
*)

let variable ?(shape = [||]) ?(typ = SDT_Float) ?name ?init () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "variable_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Variable.create name input attrs typ shape init in
  let sym = Owl_symbolic_symbol.Variable o in
  make_node sym [||]


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


let cos ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "cos_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Cos.create name input attrs in
  let sym = Owl_symbolic_symbol.Cos o in
  make_node sym [| x |]


let exp ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "exp_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Exp.create name input attrs in
  let sym = Owl_symbolic_symbol.Exp o in
  make_node sym [| x |]


(* Syntax sugar is exp is used frequently *)
let expconst () = exp (flt 1.)

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


let sub ?name x y =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "sub_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Sub.create name input attrs in
  let sym = Owl_symbolic_symbol.Sub o in
  make_node sym [| x; y |]


let mul ?name x y =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "mul_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Mul.create name input attrs in
  let sym = Owl_symbolic_symbol.Mul o in
  make_node sym [| x; y |]


let div ?name x y =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "div_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Div.create name input attrs in
  let sym = Owl_symbolic_symbol.Div o in
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
