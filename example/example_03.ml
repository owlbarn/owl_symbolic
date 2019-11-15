open Owl_symbolic
open Op
open Type

(* open Owl_symbolic_infix *)

let _ =
  let x = variable "X" in
  let y = variable "Y" in
  (* exp(sin(x)^2 + cos(x)^2) + 10*y^2 *)
  (* exp(pi * i ) = 0) *)
  let z_wrong =
    add
      (exp (add (pow (sin x) (integer 2)) (pow (cos x) (integer 2))))
      (mul (flt 10.) (pow y (flt 2.)))
  in
  let g_wrong = SymGraph.make_graph [| z_wrong |] "sym_graph_wrong" in
  try ONNX_Engine.of_symbolic g_wrong |> ignore with
  | TYPE_CHECK _ ->
    Printf.printf "Type checking works well on wrong symbolic graph.\n";
    let z =
      add
        (exp (add (pow (sin x) (flt 2.)) (pow (cos x) (flt 2.))))
        (mul (flt 10.) (pow y (flt 2.)))
    in
    let g = SymGraph.make_graph [| z |] "sym_graph" in
    let y = ONNX_Engine.of_symbolic g in
    ONNX_Engine.save y "test.onnx"

(* TOOD: Pity we cannot run the beautiful Euler's formula 
 * (exp (mul (complex 0. 1.) (pi ()))) on ONNX now , because type checking makes sure
 * that exp(x) always give the same type as x, so we have to turn pi into complex number,
 * get exp value returned as -1 + 0*i and then convert it back to float -1. 
 * At some point we may need data type broadcast, but not now. 
 *)
