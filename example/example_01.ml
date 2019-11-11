open Owl_symbolic

let _ =
  (* let x = Op.(add (flt 3.) (sin (tensor [| 3; 3 |]))) in *)
  (* let x = Op.(flt 666.) in *)
  let x = Op.(add (flt 666.) (flt 1.)) in
  (* let x = Op.(tensor [|3;3|]) in *)
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.serialise y "test.onnx"