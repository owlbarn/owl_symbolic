(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

let nelt shp = Array.fold_left ( * ) 1 shp

(* check if all the elements in an array are uniq *)
let check_uniq arr =
  let uniq_arr = Owl_utils_array.unique arr in
  Array.length arr = Array.length uniq_arr
