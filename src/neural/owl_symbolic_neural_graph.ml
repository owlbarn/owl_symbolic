(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_graph
open Owl_symbolic_types

let init typ shape =
  match typ with
  | Uniform (a, b)       -> Owl_symbolic_operator.random_uniform ~low:a ~high:b shape
  | Gaussian (mu, sigma) ->
    Owl_symbolic_operator.random_normal ~mean:mu ~stddev:sigma shape
  | Standard             -> Owl_symbolic_operator.random_uniform shape


let activation ?name act_typ input_node =
  match act_typ with
  | Tanh -> Owl_symbolic_operator.tanh ?name input_node
  | _    -> failwith "unimplemented yet"


let input ?name shape =
  let name = Owl_symbolic_utils.node_name ?name "Variable" in
  Owl_symbolic_operator.variable ~shape name


let max_pool2d ?name ?(padding = VALID) kernel strides input_node =
  let a, _ = Owl_symbolic_operator.maxpool ?name ~strides ~padding kernel input_node in
  a


let dropout ?name ratio input_node =
  let a, _ = Owl_symbolic_operator.dropout ?name ~ratio input_node in
  a


let lambda (f : symbol -> symbol) (input_node : symbol) = f input_node

let fully_connected ?(init_typ = Owl_symbolic_types.Standard) outputs input_node =
  let in_shape = input_node |> Owl_graph.attr |> Owl_symbolic_symbol.out_shape in
  let shp =
    match in_shape.(0) with
    | Some s -> s
    | None   -> failwith "fully_connected: unspecified input shape"
  in
  let m = Array.fold_left (fun a b -> a * b) 1 shp in
  let n = outputs in
  let x = Owl_symbolic_operator.reshape [| n; m |] input_node in
  let w = init init_typ [| m; n |] in
  let b = Owl_symbolic_operator.zeros [| 1; n |] in
  Owl_symbolic_operator.(add (matmul x w) b)

(*


let conv2d ?name ?(padding=SAME_UPPER) ?(init_typ=Init.Tanh) 
  kernel stride input_node = ()

let linear ?name ?(init_typ=Init.Standard)
  outputs input_node = 
  let m = 0 in 
  let n = 0 in
  let x = Owl_symbolic_operator.reshape [|n; m|] input_node in 
  let w = Owl_symbolic_operator.random_uniform [|m; n|] in 
  let b = Owl_symbolic_operator.zeros [|1; n|] in
  let y = Owl_symbolic_operator.(add (matmul x w ) b) in 
  y


let normalisation ?name ?(axis=(-1)) ?decay ?mu ?var input_node = ()
*)
