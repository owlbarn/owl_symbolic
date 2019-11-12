open Owl_symbolic
open Op
(* open Owl_symbolic_infix *)

let _ =
  let x = variable ~shape:[||] ~name:"X" () in
  let y = variable ~shape:[||] ~name:"Y" () in
  (* exp(sin(x)^2 + cos(x)^2) + 7*y *)
  let z = 
    (add
      (pow (expconst ())
        (add (pow (sin x) (integer 2)) (pow (cos x) (integer 2))))
      (mul (flt 7.) y))
  in
  let g = SymGraph.make_graph [| z |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.serialise y "test.onnx"
