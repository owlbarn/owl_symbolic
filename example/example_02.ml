open Owl_symbolic

let _ =
  let x = Op.(add (flt 666.)
    (variable ~shape:[||] ~name:"X" ())) 
  in
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.serialise y "test.onnx"