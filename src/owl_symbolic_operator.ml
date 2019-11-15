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
  let o = Owl_symbolic_ops_input.Int.create name input attrs value in
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
  let o = Owl_symbolic_ops_input.Float.create name input attrs value in
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
  let o = Owl_symbolic_ops_input.Complex.create name input attrs r i in
  let sym = Owl_symbolic_symbol.Complex o in
  make_node sym [||]


let pi ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "pi_%i" suffix
  in
  let o = Owl_symbolic_ops_input.Pi.create name in
  let sym = Owl_symbolic_symbol.Pi o in
  make_node sym [||]


(* Do we really a "Tensor" node ? *)
let tensor ?name t =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "tensor_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let value = t in
  let o = Owl_symbolic_ops_input.Tensor.create name input attrs value in
  let sym = Owl_symbolic_symbol.Tensor o in
  make_node sym [||]


(* The shape and type are decided by initial value; 
 * if initial value not given, user need to specify them. 
 * Shape value default to scalar [||], and type default to SDT_Float. *)
let variable ?shape ?typ ?init name =
  let true_shape = match init with 
    | Some (t : tensor) ->  
      if (shape <> None) then (
        Owl_log.warn "Variable %s: shape overridden by initializers" name 
      );
      t.shape
    | None              -> 
      match shape with 
      | Some s -> s
      | None   -> [||]
  in 

  let true_typ = match init with 
    | Some (t : tensor) ->  
      if (shape <> None) then (
        Owl_log.warn "Variable %s: type overridden by initializers" name;
      );
      t.dtype
    | None              -> 
      match typ with 
      | Some s -> s
      | None   -> SDT_Float
  in
  
  let attrs = [||] in
  let o = Owl_symbolic_ops_input.Variable.create name attrs true_typ true_shape init in
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


(** The frequently used constants *)

let expconst () = exp (flt 1.)
