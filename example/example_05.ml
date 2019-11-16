open Owl_symbolic

module G = Owl_computation_cpu_engine.Make (Owl_dense_ndarray.S)
include Owl_algodiff_generic.Make (G)

let make_graph () =
  (* let x = G.ones [|2; 3|] |> pack_arr in *)
  let x = G.var_arr ~shape:[|2;3|] "x" |> pack_arr in
  let y = G.var_elt "y" |> pack_elt in
  let z = Maths.(sin x + y) in
  let input  = [| unpack_arr x |> G.arr_to_node; unpack_elt y |> G.elt_to_node |] in
  let output = [| unpack_arr z |> G.arr_to_node |] in
  G.make_graph ~input ~output "graph"

let _ =
  let k = 
   make_graph ()
   |> OWL_Engine.to_symbolic
   |> ONNX_Engine.of_symbolic
  in 
  ONNX_Engine.save k "test.onnx"
