open Owl_symbolic

module G = Owl_computation_cpu_engine.Make (Owl_dense_ndarray.S)
include Owl_algodiff_generic.Make (G)

let make_graph () =
  let x = G.ones [|2; 3|] |> pack_arr in
  let y = G.var_elt "y"   |> pack_elt in
  let z = Maths.(sin x + y) in
  let input  = [| unpack_elt y |> G.elt_to_node |] in
  let output = [| unpack_arr z |> G.arr_to_node |] in
  G.make_graph ~input ~output "graph"

let g = make_graph ()

let sym_graph = OWL_Engine.to_symbolic g