open Owl_symbolic
open Op

(* open Owl_symbolic_infix *)

let _ =
  let x = variable ~shape:[||] ~name:"X" () in
  let y = variable ~shape:[||] ~name:"Y" () in
  (* exp(sin(x)^2 + cos(x)^2) + 10*y^2 *)
  (* exp(-1 * i ) = 0) *)
  let z =
    add
      (* (exp (add (pow (sin x) (integer 2)) (pow (cos x) (integer 2)))) 
       * --> this shall leads to error in "of_symbolic"
       *)
      (exp (add (pow (sin x) (flt 2.)) (pow (cos x) (flt 2.))))
      (mul (flt 10.) (pow y (flt 2.)))
  in
  let g = SymGraph.make_graph [| z |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.serialise y "test.onnx"
