(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

(* If an input is None, it means the static shape checking is not possible, and returns None. *)

let infer_shape_00 _input_shapes = [| Some [||] |]

let infer_shape_01 input_shapes =
  match input_shapes.(0).(0) with
  | Some s -> [| Some Array.(copy s) |]
  | None   -> [| None |]


let infer_shape_03 input_shapes =
  let s0 = input_shapes.(0).(0) in
  let s1 = input_shapes.(1).(0) in
  match s0, s1 with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(broadcast1 s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_05 input_shapes repeats =
  match input_shapes.(0).(0) with
  | Some s -> [| Some Owl_utils_infer_shape.(tile s repeats) |]
  | None   -> [| None |]


let infer_shape_07 input_shapes axis =
  let s0 = Array.map (fun s -> s.(0)) input_shapes in
  if Array.exists
       (function
         | Some _ -> false
         | None   -> true)
       s0
  then [| None |]
  else (
    let s1 =
      Array.map
        (function
          | Some a -> a
          | None   -> failwith "infer_shape_07")
        s0
    in
    [| Some Owl_utils_infer_shape.(concatenate s1 axis) |])


let infer_shape_08 input_shapes axis splits =
  match input_shapes.(0).(0) with
  | Some s ->
    let s0 = Owl_utils_infer_shape.(split s axis splits) in
    Array.map (fun s -> Some s) s0
  | None   -> Array.(make (length splits) None)


let infer_shape_10 input_shapes axis keepdims =
  match input_shapes.(0).(0) with
  | Some s ->
    let l = Array.length s in
    assert (Array.for_all (fun a -> a >= ~-l && a <= l - 1) axis);
    [| Some Owl_symbolic_utils.(reduce s axis keepdims) |]
  | None   -> [| None |]


let infer_shape_11 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_12 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_symbolic_utils.(conv2d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_13 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_15 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_17 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_19 input_shapes =
  let x_shape = input_shapes.(0).(0) in
  let y_shape = input_shapes.(1).(0) in
  match x_shape, y_shape with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(dot s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_20 input_shapes axis =
  match input_shapes.(0).(0) with
  | Some s ->
    let axis = List.map (fun i -> Owl_types.R_ (Array.of_list i)) axis |> Array.of_list in
    let axis = Owl_base_slicing.check_slice_definition axis s in
    [| Some Owl_base_slicing.(calc_slice_shape axis) |]
  | None   -> [| None |]


let infer_shape_21 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_symbolic_utils.(pool2d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_31 input_shapes =
  let msg = "Owl_symbolic_shape: infer_shape_31." in
  let unpack = Owl_symbolic_utils.get_option_value msg in
  let flag = Array.exists (fun x -> x.(0) = None) input_shapes in
  if flag
  then [| None |]
  else (
    let broadcast_shp =
      Array.fold_left
        (fun accu shps ->
          let shp = shps.(0) |> unpack in
          Owl_utils_infer_shape.broadcast1 shp accu)
        [||]
        input_shapes
    in
    [| Some broadcast_shp |])


let infer_shape_32 input_shapes =
  match input_shapes.(0).(0) with
  | Some s -> [| Some [| Array.length s |] |]
  | None   -> [| None |]


let infer_shape_33 input_shapes =
  match input_shapes.(0).(0) with
  | Some _ -> [| Some [||] |]
  | None   -> [| None |]


let infer_shape_gemm (x : Owl_symbolic_ops_math.Gemm.t) input_shapes =
  let l = Array.length input_shapes in
  assert (l = 2 || l = 3);
  match input_shapes.(0).(0), input_shapes.(1).(0) with
  | Some a_shp, Some b_shp ->
    assert (Array.length a_shp = 2);
    assert (Array.length b_shp = 2);
    let a_shp = if x.transA then [| a_shp.(1); a_shp.(0) |] else a_shp in
    let b_shp = if x.transB then [| b_shp.(1); b_shp.(0) |] else b_shp in
    assert (a_shp.(1) = b_shp.(0));
    [| Some [| a_shp.(0); b_shp.(1) |] |]
  | _, _                   -> [| None |]


(* TODO: check the impl of this function *)
let infer_shape_pad (x : Owl_symbolic_ops_tensor.Pad.t) input_shapes =
  assert (Array.length input_shapes >= 2);
  let return_shp =
    match input_shapes.(0).(0), input_shapes.(1).(0) with
    | Some data_shp, Some pads_shp ->
      assert (Array.length pads_shp = 1);
      let l = Array.length data_shp in
      assert (pads_shp.(0) = 2 * l);
      (* we cannot do shape checking of a graph containing Pad ndoe
       if the pads value is dynamically got from nodes, not as arguments in x.p *)
      let s = Array.mapi (fun i d -> x.p.(i) + d + x.p.(i + l)) data_shp in
      [| Some s |]
    | _, _                         -> [| None |]
  in
  if Array.length input_shapes = 3
  then (
    match input_shapes.(2).(0) with
    | Some s ->
      assert (Array.length s = 0);
      return_shp
    | _      -> [| None |])
  else return_shp


(* NOTE: this function set default parameters of symbol, 
 * something other than shape checking *)
let infer_shape_transpose input_shapes (x : Owl_symbolic_ops_tensor.Transpose.t) =
  let perm = x.perm in
  match input_shapes.(0).(0) with
  | Some s ->
    let l = Array.length s in
    (match perm with
    | Some p ->
      assert (Array.length p = l);
      let p' = Array.copy p in
      Array.sort Stdlib.compare p';
      assert (p' = Owl_utils_array.range 0 (l - 1));
      [| Some Owl_utils_infer_shape.(transpose s p) |]
    | None   ->
      let p = Owl_utils_array.range 0 (l - 1) in
      Owl_utils_array.reverse p;
      x.perm <- Some p;
      [| Some Owl_utils_infer_shape.(transpose s p) |])
  | None   -> [| None |]


(* TODO: test *)
let infer_shape_slice input_shapes (x : Owl_symbolic_ops_tensor.Slice.t) =
  match input_shapes.(0).(0) with
  | Some s ->
    let dim = Array.length s in
    let axes =
      match x.axes with
      | Some a ->
        assert (Array.for_all (fun x -> x >= ~-dim && x <= dim - 1) a);
        a
      | None   -> Owl_utils_array.range 0 (dim - 1)
    in
    let steps =
      match x.steps with
      | Some x -> x
      | None   -> Array.make dim 1
    in
    let l = Array.length axes in
    assert (Array.length steps = l);
    assert (Array.length x.starts = l);
    assert (Array.length x.ends = l);
    (* full index of [start; end; step] list *)
    let index = Array.make dim [ 0 ] in
    let c = ref 0 in
    for i = 0 to dim - 1 do
      let idx =
        if Array.mem i axes
        then (
          let ret = [ x.starts.(!c); x.ends.(!c); steps.(!c) ] in
          c := !c + 1;
          ret)
        else [ 0; s.(i) - 1; 1 ]
      in
      index.(i) <- idx
    done;
    infer_shape_20 input_shapes (Array.to_list index)
  | None   -> [| None |]


let infer_shape_space_to_depth input_shapes block_size =
  match input_shapes.(0).(0) with
  | Some s ->
    assert (Array.length s = 4);
    let shp =
      [| s.(0); s.(1) * block_size * block_size; s.(2) / block_size; s.(3) / block_size |]
    in
    [| Some shp |]
  | None   -> [| None |]


let infer_shape_conv input_shapes (x : Owl_symbolic_ops_nn.Conv.t) =
  let l = x.dim in
  let padding = if x.auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
  if l = 1
  then infer_shape_11 input_shapes padding x.strides
  else if l = 2
  then infer_shape_12 input_shapes padding x.strides
  else if l = 3
  then infer_shape_13 input_shapes padding x.strides
  else failwith "Owl_symbolic_shape: illegal conv dimensions."


let infer_shape_maxpool input_shapes (x : Owl_symbolic_ops_nn.MaxPool.t) =
  let l = Array.length x.kernel_shp in
  match input_shapes.(0).(0) with
  | Some i ->
    let ndim = Array.length i - 2 in
    assert (ndim = l);
    let padding = if x.auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
    let dim =
      if ndim = 1
      then infer_shape_15 input_shapes padding x.kernel_shp x.strides
      else if ndim = 2
      then infer_shape_21 input_shapes padding x.kernel_shp x.strides
      else if ndim = 3
      then infer_shape_17 input_shapes padding x.kernel_shp x.strides
      else failwith "Owl_symbolic_shape: illegal maxpool dimensions."
    in
    [| dim.(0); dim.(0) |]
  | None   -> [| None; None |]


let infer_shape_batch_normalization input_shapes =
  let shp_x = input_shapes.(0).(0) in
  let shp_scale = input_shapes.(1).(0) in
  let shp_b = input_shapes.(2).(0) in
  let shp_mean = input_shapes.(3).(0) in
  let shp_var = input_shapes.(4).(0) in
  match shp_x, shp_scale, shp_b, shp_mean, shp_var with
  | Some x, Some scale, Some b, Some mean, Some var ->
    let c = x.(0) in
    assert (Array.length scale = 1);
    assert (Array.length b = 1);
    assert (Array.length mean = 1);
    assert (Array.length var = 1);
    assert (c = scale.(0));
    assert (c = b.(0));
    assert (c = mean.(0));
    assert (c = var.(0));
    [| Some x; Some [||]; Some [||]; Some [||]; Some [||] |]
  | _, _, _, _, _ -> [| None; None; None; None; None |]


let infer_shape_squeeze (input_shapes : int array option array array) axes =
  let input_shape = input_shapes.(0).(0) in
  match input_shape, axes with
  | Some shp, Some ax ->
    let l = Array.length shp in
    Array.iter
      (fun a ->
        assert (a < l);
        if shp.(a) <> 1
        then failwith "infer_shape_squeeze: specified axis is not single-dim")
      ax;
    let new_shp =
      Owl_utils_array.filteri (fun i _ -> not (Array.exists (fun a -> a = i) ax)) shp
    in
    [| Some new_shp |]
  | Some shp, None    ->
    let new_shp = Owl_utils_array.filter (fun s -> s <> 1) shp in
    [| Some new_shp |]
  | _, _              -> [| None |]


(** Main entry *)

let infer_shape input_shapes sym =
  match sym with
  | Int _                -> [| Some [||] |]
  | Float _              -> [| Some [||] |]
  | Complex _            -> [| Some [||] |]
  | Pi _                 -> [| Some [||] |]
  | Tensor _             ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Variable _           ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | RandomUniform _      ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | RandomNormal _       ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Sin _                -> infer_shape_01 input_shapes
  | Cos _                -> infer_shape_01 input_shapes
  | Tan _                -> infer_shape_01 input_shapes
  | Asin _               -> infer_shape_01 input_shapes
  | Acos _               -> infer_shape_01 input_shapes
  | Atan _               -> infer_shape_01 input_shapes
  | Sinh _               -> infer_shape_01 input_shapes
  | Cosh _               -> infer_shape_01 input_shapes
  | Tanh _               -> infer_shape_01 input_shapes
  | Asinh _              -> infer_shape_01 input_shapes
  | Acosh _              -> infer_shape_01 input_shapes
  | Atanh _              -> infer_shape_01 input_shapes
  | Sqrt _               -> infer_shape_01 input_shapes
  | Exp _                -> infer_shape_01 input_shapes
  | Log _                -> infer_shape_01 input_shapes
  | Sigmoid _            -> infer_shape_01 input_shapes
  | Abs _                -> infer_shape_01 input_shapes
  | Neg _                -> infer_shape_01 input_shapes
  | Floor _              -> infer_shape_01 input_shapes
  | Ceil _               -> infer_shape_01 input_shapes
  | Round _              -> infer_shape_01 input_shapes
  | Relu _               -> infer_shape_01 input_shapes
  | Add _                -> infer_shape_03 input_shapes
  | Sub _                -> infer_shape_03 input_shapes
  | Mul _                -> infer_shape_03 input_shapes
  | Div _                -> infer_shape_03 input_shapes
  | Pow _                -> infer_shape_01 input_shapes
  | Mod _                -> infer_shape_01 input_shapes
  | MatMul _             -> infer_shape_19 input_shapes
  | Gemm x               -> infer_shape_gemm x input_shapes
  | Max _                -> infer_shape_31 input_shapes
  | Min _                -> infer_shape_31 input_shapes
  | Sum _                -> infer_shape_31 input_shapes
  | ReduceSum x          -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMax x          -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMin x          -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMean x         -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceSumSquare x    -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceProd x         -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceLogSum x       -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceLogSumExp x    -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceL1 x           -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceL2 x           -> infer_shape_10 input_shapes x.axes x.keepdims
  | Reshape x            -> [| Some x.shape |]
  | Identity x           ->
    let idx = x.idx in
    [| input_shapes.(0).(idx) |]
  | Split x              -> infer_shape_08 input_shapes x.axis x.split
  | Concat x             -> infer_shape_07 input_shapes x.axis
  | Pad x                -> infer_shape_pad x input_shapes
  | Cast _               -> infer_shape_01 input_shapes
  | Squeeze x            -> infer_shape_squeeze input_shapes x.axes
  | Tile x               -> infer_shape_05 input_shapes x.repeats
  | Shape _              -> infer_shape_32 input_shapes
  | Size _               -> infer_shape_33 input_shapes
  | Transpose x          -> infer_shape_transpose input_shapes x
  | Slice x              -> infer_shape_slice input_shapes x
  | SpaceToDepth x       -> infer_shape_space_to_depth input_shapes x.blocksize
  | Conv x               -> infer_shape_conv input_shapes x
  | MaxPool x            -> infer_shape_maxpool input_shapes x
  | BatchNormalization _ -> infer_shape_batch_normalization input_shapes
  | Dropout _            ->
    let t = infer_shape_01 input_shapes in
    [| t.(0); t.(0) |]
  | SequenceEmpty _      -> [||]
  | _                    -> [| None |]
