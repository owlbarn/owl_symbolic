(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_namespace
open Owl_symbolic_graph
open Owl_symbolic_types

let noop_sym = Owl_symbolic_symbol.NOOP
let noop = make_node noop_sym [||]

let int_sym ?name value =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "integer_%i" suffix
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_generator.Int.create name attrs value in
  Owl_symbolic_symbol.Int o


let int ?name value =
  let sym = int_sym ?name value in
  make_node sym [||]


let zero_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "zero_%i" suffix
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_generator.Zero.create name attrs in
  Owl_symbolic_symbol.Zero o


let zero ?name () =
  let sym = zero_sym ?name () in
  make_node sym [||]


let one_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "one_%i" suffix
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_generator.One.create name attrs in
  Owl_symbolic_symbol.One o


let one ?name () =
  let sym = one_sym ?name () in
  make_node sym [||]


let negone_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "negone_%i" suffix
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_generator.NegOne.create name attrs in
  Owl_symbolic_symbol.NegOne o


let negone ?name () =
  let sym = negone_sym ?name () in
  make_node sym [||]


let float_sym ?name x =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "float_%i" suffix
  in
  let attrs = [||] in
  let value = x in
  let o = Owl_symbolic_ops_generator.Float.create name attrs value in
  Owl_symbolic_symbol.Float o


let float ?name x =
  let sym = float_sym ?name x in
  make_node sym [||]


let complex_sym ?name r i =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "complex_%i" suffix
  in
  let attrs = [||] in
  let o = Owl_symbolic_ops_generator.Complex.create name attrs r i in
  Owl_symbolic_symbol.Complex o


let complex ?name r i =
  let sym = complex_sym ?name r i in
  make_node sym [||]


let pi_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "pi_%i" suffix
  in
  let o = Owl_symbolic_ops_generator.Pi.create name in
  Owl_symbolic_symbol.Pi o


let pi ?name () =
  let sym = pi_sym ?name () in
  make_node sym [||]


let tensor_sym ?name t =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "tensor_%i" suffix
  in
  let attrs = [||] in
  let value = t in
  let o = Owl_symbolic_ops_generator.Tensor.create name attrs value in
  Owl_symbolic_symbol.Tensor o


let tensor ?name t =
  let sym = tensor_sym ?name t in
  make_node sym [||]


(* The shape and type are decided by initial value; 
 * if initial value not given, user need to specify them. 
 * Shape value default to scalar [||], and type default to SNT_Float. *)
let variable_sym ?shape ?dtype ?init name =
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
  let o =
    Owl_symbolic_ops_generator.Variable.create name attrs true_dtype true_shape init
  in
  Owl_symbolic_symbol.Variable o


let variable ?shape ?dtype ?init name =
  let sym = variable_sym ?shape ?dtype ?init name in
  make_node sym [||]


let random_uniform_sym ?(dtype = SNT_Float) ?(low = 0.) ?(high = 1.) ?name shape =
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
  Owl_symbolic_symbol.RandomUniform o


let random_uniform ?dtype ?low ?high ?name shape =
  let sym = random_uniform_sym ?dtype ?low ?high ?name shape in
  make_node sym [||]


let sin_sym ?name x =
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
  Owl_symbolic_symbol.Sin o


let sin ?name x =
  let sym = sin_sym ?name x in
  make_node sym [| x |]


let cos_sym ?name x =
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
  Owl_symbolic_symbol.Cos o


let cos ?name x =
  let sym = cos_sym ?name x in
  make_node sym [| x |]


(* TODO: are these `input` truely necessary for an symbol? ... *)

let sqrt_sym ?name x =
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
  Owl_symbolic_symbol.Sqrt o


let sqrt ?name x =
  let sym = sqrt_sym ?name x in
  make_node sym [| x |]


let exp_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "exp_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Exp.create name input attrs in
  Owl_symbolic_symbol.Exp o


let exp ?name x =
  let sym = exp_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  Owl_symbolic_symbol.set_input sym [| x_name |];
  make_node sym [| x |]


let log_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "log_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Log.create name input attrs in
  Owl_symbolic_symbol.Log o


let log ?name x =
  let sym = log_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  Owl_symbolic_symbol.set_input sym [| x_name |];
  make_node sym [| x |]


let neg_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "neg_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Neg.create name input attrs in
  Owl_symbolic_symbol.Neg o


let neg ?name x =
  let sym = neg_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  Owl_symbolic_symbol.set_input sym [| x_name |];
  make_node sym [| x |]


let relu_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "relu_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Relu.create name input attrs in
  Owl_symbolic_symbol.Relu o


let relu ?name x =
  let sym = relu_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  Owl_symbolic_symbol.set_input sym [| x_name |];
  make_node sym [| x |]


(* allows float, int, and rational; both nodes must be there *)
let rational_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "rat_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Rational.create name input attrs in
  Owl_symbolic_symbol.Rational o


let rational ?name p q =
  let p_type = Owl_symbolic_symbol.op_type (Owl_graph.attr p) in
  let q_type = Owl_symbolic_symbol.op_type (Owl_graph.attr q) in
  if not ((p_type = "Int" || p_type = "Float") && (q_type = "Int" || q_type = "Float"))
  then failwith "rational: input nodes hould only be int or float";
  let sym = rational_sym ?name () in
  let p_name = Owl_symbolic_graph.name p in
  let q_name = Owl_symbolic_graph.name q in
  Owl_symbolic_symbol.set_input sym [| p_name; q_name |];
  make_node sym [| p; q |]


let add_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "add_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Add.create name input attrs in
  Owl_symbolic_symbol.Add o


let add ?name x y =
  let sym = add_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  Owl_symbolic_symbol.set_input sym input;
  make_node sym [| x; y |]


let sub_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "sub_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Sub.create name input attrs in
  Owl_symbolic_symbol.Sub o


let sub ?name x y =
  let sym = sub_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  Owl_symbolic_symbol.set_input sym input;
  make_node sym [| x; y |]


let mul_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "mul_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Mul.create name input attrs in
  Owl_symbolic_symbol.Mul o


let mul ?name x y =
  let sym = mul_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  Owl_symbolic_symbol.set_input sym input;
  make_node sym [| x; y |]


let div_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "div_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Div.create name input attrs in
  Owl_symbolic_symbol.Div o


let div ?name x y =
  let sym = div_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  Owl_symbolic_symbol.set_input sym input;
  make_node sym [| x; y |]


let pow_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "pow_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.Pow.create name input attrs in
  Owl_symbolic_symbol.Pow o


let pow ?name x y =
  let sym = pow_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  Owl_symbolic_symbol.set_input sym input;
  make_node sym [| x; y |]


let matmul_sym ?name () =
  let suffix = generate_suffix () in
  let name =
    match name with
    | Some n -> n
    | None   -> Printf.sprintf "matmul_%i" suffix
  in
  let input = [||] in
  let attrs = [||] in
  let o = Owl_symbolic_ops_math.MatMul.create name input attrs in
  Owl_symbolic_symbol.MatMul o


let matmul ?name x y =
  let sym = mul_sym ?name () in
  let x_name = Owl_symbolic_graph.name x in
  let y_name = Owl_symbolic_graph.name y in
  let input = [| x_name; y_name |] in
  Owl_symbolic_symbol.set_input sym input;
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
  let auto_pad = if padding = "SAME" then "SAME_LOWER" else "VALID" in
  let attrs = [||] in
  let kernel_shp = Owl_symbolic_symbol.shape (Owl_graph.attr kernel) in
  (* NOTE: kernel_shp only takes the h and w part, not channels *)
  let ndims = Array.length strides + 2 in
  let kernel_shp = Array.sub kernel_shp 2 (ndims - 2) in
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

let expconst () = exp (float 1.)
