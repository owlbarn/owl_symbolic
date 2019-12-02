(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

let nelt shp = Array.fold_left ( * ) 1 shp

(* check if all the elements in an array are uniq *)
let check_uniq arr =
  let uniq_arr = Owl_utils_array.unique arr in
  Array.length arr = Array.length uniq_arr


(* input order *)
let to_nchw_order a =
  let n = Array.length a in
  if n < 3
  then a
  else (
    let b = Array.make n 0 in
    b.(0) <- a.(0);
    b.(1) <- a.(n - 1);
    for i = 1 to n - 2 do
      b.(i + 1) <- a.(i)
    done;
    b)


(* weight order *)
let hwio_to_oihw_order a =
  let n = Array.length a in
  if n < 3
  then a
  else (
    let b = Array.make n 0 in
    b.(0) <- a.(n - 1);
    b.(1) <- a.(n - 2);
    for i = 0 to n - 3 do
      b.(i + 2) <- a.(i)
    done;
    b)


(** Function for shape inference *)

(* add keepdims *)
let reduce shape axis keepdims =
  let d = Array.length shape in
  let a = Array.map (fun i -> Owl_utils_ndarray.adjust_index i d) axis in
  let s = Array.copy shape in
  Array.iter
    (fun i ->
      assert (i < d);
      s.(i) <- 1)
    a;
  if keepdims = false
  then Array.to_list s |> List.filter (fun x -> x <> 1) |> Array.of_list
  else s


(* NCHW order *)
let pool2d input_shape padding kernel_shape stride_shape =
  let batches = input_shape.(0) in
  let input_cols = input_shape.(2) in
  let input_rows = input_shape.(3) in
  let in_channel = input_shape.(1) in
  let kernel_cols = kernel_shape.(0) in
  let kernel_rows = kernel_shape.(1) in
  let col_stride = stride_shape.(0) in
  let row_stride = stride_shape.(1) in
  let output_cols, output_rows =
    Owl_utils_infer_shape.calc_conv2d_output_shape
      padding
      input_cols
      input_rows
      kernel_cols
      kernel_rows
      row_stride
      col_stride
  in
  [| batches; in_channel; output_cols; output_rows |]


(* NCHW order *)
let conv2d input_shape padding kernel_shape stride_shape =
  let batches = input_shape.(0) in
  let input_cols = input_shape.(2) in
  let input_rows = input_shape.(3) in
  let in_channel = input_shape.(1) in
  let kernel_cols = kernel_shape.(2) in
  let kernel_rows = kernel_shape.(3) in
  let out_channel = kernel_shape.(0) in
  assert (in_channel = kernel_shape.(1));
  let col_stride = stride_shape.(0) in
  let row_stride = stride_shape.(1) in
  let output_cols, output_rows =
    Owl_utils_infer_shape.calc_conv2d_output_shape
      padding
      input_cols
      input_rows
      kernel_cols
      kernel_rows
      row_stride
      col_stride
  in
  [| batches; output_cols; output_rows; out_channel |]


(* Represent float as rational format: (numerator, denominator) *)

let float_as_ratio _v = 666, 777

let rec gcd a b =
  match a mod b with
  | 0 -> b
  | r -> gcd b r
