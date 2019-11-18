(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

let nelt shp = Array.fold_left ( * ) 1 shp

(* check if all the elements in an array are uniq *)
let check_uniq arr =
  let uniq_arr = Owl_utils_array.unique arr in
  Array.length arr = Array.length uniq_arr


(* helperf function of reduction op shape inference *)

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
