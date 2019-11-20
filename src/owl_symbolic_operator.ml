(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_namespace
open Owl_symbolic_graph
open Owl_symbolic_types

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
  let attrs = [||] in
  let o = Owl_symbolic_ops_input.Int.create name attrs value in
  let sym = Owl_symbolic_symbol.Int o in
  make_node sym [||]


let flt ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "float_%i" suffix
  in
  let attrs = [||] in
  let value = x in
  let o = Owl_symbolic_ops_input.Float.create name attrs value in
  let sym = Owl_symbolic_symbol.Float o in
  make_node sym [||]


let complex ?name r i =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "complex_%i" suffix
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_input.Complex.create name attrs r i in
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


let tensor ?name t =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "tensor_%i" suffix
  in
  let attrs = [||] in
  let value = t in
  let o = Owl_symbolic_ops_input.Tensor.create name attrs value in
  let sym = Owl_symbolic_symbol.Tensor o in
  make_node sym [||]


(* The shape and type are decided by initial value; 
 * if initial value not given, user need to specify them. 
 * Shape value default to scalar [||], and type default to SNT_Float. *)
let variable ?shape ?dtype ?init name =
  let true_shape =
    match init with
    | Some (t : tensor) ->
      if shape <> None
      then Owl_log.warn "Variable %s: shape overridden by initializers" name;
      t.shape
    | None              ->
      (match shape with
      | Some s -> s
      | None   -> [||])
  in
  let true_dtype =
    match init with
    | Some (t : tensor) ->
      if shape <> None
      then Owl_log.warn "Variable %s: type overridden by initializers" name;
      t.dtype
    | None              ->
      (match dtype with
      | Some s -> s
      | None   -> SNT_Float)
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_input.Variable.create name attrs true_dtype true_shape init in
  let sym = Owl_symbolic_symbol.Variable o in
  make_node sym [||]


let random_uniform ?(dtype = SNT_Float) ?(low = 0.) ?(high = 1.) ?name shape =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "random_uniform_%i" suffix
  in
  let attrs = [||] in
  let o =
    Owl_symbolic_ops_generator.RandomUniform.create ~low ~high name attrs dtype shape
  in
  let sym = Owl_symbolic_symbol.RandomUniform o in
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


let sqrt ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "sqrt_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Sqrt.create name input attrs in
  let sym = Owl_symbolic_symbol.Sqrt o in
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


let log ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "log_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Log.create name input attrs in
  let sym = Owl_symbolic_symbol.Log o in
  make_node sym [| x |]


let neg ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "neg_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Neg.create name input attrs in
  let sym = Owl_symbolic_symbol.Neg o in
  make_node sym [| x |]


let relu ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "relu_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Relu.create name input attrs in
  let sym = Owl_symbolic_symbol.Relu o in
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


let matmul ?name x y =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "matmul_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.MatMul.create name input attrs in
  let sym = Owl_symbolic_symbol.MatMul o in
  make_node sym [| x; y |]


let reduce_sum ?(keepdims = true) ?name x axes =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "reduce_sum_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_reduction.ReduceSum.create ~keepdims name input attrs axes in
  let sym = Owl_symbolic_symbol.ReduceSum o in
  make_node sym [| x |]


let reduce_max ?(keepdims = true) ?name x axes =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "reduce_max_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name x in
  let input = [| x_name |] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_reduction.ReduceMax.create ~keepdims name input attrs axes in
  let sym = Owl_symbolic_symbol.ReduceMax o in
  make_node sym [| x |]


(* NOTEICE that reshape accept a shape tensor as input node; not as attributes *)
let reshape ?name data shape =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "reduce_max_%i" suffix
  in
  let x_name = Owl_symbolic_graph.name data in
  let y_name = Owl_symbolic_graph.name shape in
  let input = [| x_name; y_name |] in
  let attrs = [||] in
  let shp = (Owl_symbolic_symbol.tensor_value (Owl_graph.attr shape)).int_val in
  let shp =
    match shp with
    | Some s -> s
    | None   -> failwith "Owl_symbolic_operator.reshape: empty shape input."
  in
  let o = Owl_symbolic_ops_tensor.Reshape.create name input shp attrs in
  let sym = Owl_symbolic_symbol.Reshape o in
  make_node sym [| data; shape |]


(* This interface needs to be updated; make dilations and strides default value *)
let conv ?name ?bias input kernel padding dilations strides =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "conv_%i" suffix
  in
  (* TODO: or new padding type? Should we use same_lower or same_upper?  *)

  (* let ndims = *)
  let auto_pad = if padding = "SAME" then "SAME_LOWER" else "VALID" in
  let attrs = [||] in
  let kernel_shp = Owl_symbolic_symbol.shape (Owl_graph.attr kernel) in
  let i_name = Owl_symbolic_graph.name input in
  let k_name = Owl_symbolic_graph.name kernel in
  let inputs =
    match bias with
    | Some n ->
      let b_name = Owl_symbolic_graph.name n in
      [| i_name; k_name; b_name |]
    | None   -> [| i_name; k_name |]
  in
  let o =
    Owl_symbolic_ops_nn.Conv.create
      ~auto_pad
      name
      inputs
      attrs
      kernel_shp
      strides
      dilations
  in
  let sym = Owl_symbolic_symbol.Conv o in
  match bias with
  | Some b -> make_node sym [| input; kernel; b |]
  | None   -> make_node sym [| input; kernel |]


(* TODO: every problem in conv applies here *)
(* !!!! Currently ignore its second optional output -- that may require some structural change *)
let maxpool ?name input kernel strides padding dilations =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "maxpool_%i" suffix
  in
  let auto_pad = if padding = "SAME" then "SAME_LOWER" else "VALID" in
  let attrs = [||] in
  let i_name = Owl_symbolic_graph.name input in
  let inputs = [| i_name |] in
  let o =
    Owl_symbolic_ops_nn.MaxPool.create
      ~auto_pad
      name
      inputs
      attrs
      kernel
      strides
      dilations
  in
  let sym = Owl_symbolic_symbol.MaxPool o in
  make_node sym [| input |]


(** The frequently used constants *)

let expconst () = exp (flt 1.)
