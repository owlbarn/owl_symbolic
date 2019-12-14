open Owl_symbolic
open Op

(*
let test_gemm () =
  let a = variable ~shape:[| 3; 4 |] "a" in
  let b = variable ~shape:[| 4; 5 |] "b" in
  (* the user need to know that the correct shape of c *)
  let c = variable ~shape:[| 3; 5 |] "c" in
  let x = gemm ~c a b in
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"
*)

let test_split () =
  let a = variable ~shape:[| 3; 5 |] "a" in
  let x = split ~axis:1 a [| 2; 3 |] in
  let g = SymGraph.make_graph x "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"


let _ = test_split ()
