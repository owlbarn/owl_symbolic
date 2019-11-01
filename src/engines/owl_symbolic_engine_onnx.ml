(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_specs
module G = Owl_symbolic_graph
module S = Owl_symbolic_symbol

type t = Onnx_types.graph_proto

(** Routines for building const tensor proto *)

let make_tensorproto_one sym =
  let name = S.name sym in
  PT.default_tensor_proto ~name ~int32_data:[ Int32.one ] ()


let make_tensorproto_ones sym =
  let name = S.name sym in
  let shp = S.shape sym in
  let dims = Array.map Int64.of_int shp |> Array.to_list in
  let int32_data =
    Array.make (Owl_symbolic_utils.nelem shp) Int32.one |> Array.to_list
  in
  PT.default_tensor_proto ~name ~dims ~int32_data ()


let make_tensorproto_float sym =
  let name = S.name sym in
  let float_data = [ S.value sym ] in
  PT.default_tensor_proto ~name ~float_data ()


(** Main entry *)

let of_symbolic (sym_graph : symbolic_graph) =
  let syms = G.iterate sym_graph in
  let len = Array.length syms in
  let default_node = PT.default_node_proto () in
  let node = Array.make len default_node in
  let initialiser = ref [] in
  let input = ref [] in
  let output = ref [] in
  for i = 0 to len - 1 do
    (* build nodeprotos *)
    let n = syms.(i) in
    let name = S.name n in
    let ninput = S.input n in
    let noutput = S.output n in
    let nproto = PT.default_node_proto ~name ~input:ninput ~output:noutput () in
    node.(i) <- nproto;
    (* build constant inputs : TensorProto *)
    let constant =
      match n with
      | One _   -> [ make_tensorproto_one n ]
      | Ones _  -> [ make_tensorproto_ones n ]
      | Float _ -> [ make_tensorproto_float n ]
      | _       -> []
    in
    initialiser := List.append !initialiser constant;
    (* inputs and outputs : ValueInforProto *)
    let input_valinfo =
      match n with
      | Var _ -> [ PT.default_value_info_proto ~name () ]
      | _     -> []
    in
    input := List.append !input input_valinfo;
    output := [ PT.default_value_info_proto ~name:(G.name sym_graph) () ]
  done;
  (* result *)
  let node = Array.to_list node in
  let initialiser = !initialiser in
  let input = !input in
  let output = !output in
  let default_graph =
    PT.default_graph_proto ~node ~initializer_:initialiser ~input ~output ()
  in
  default_graph


let to_symbolic (_onnx_graph : t) = { symbols = [||] }


let serialise (onnx_graph : t ) filename = 
  let encoder = Pbrt.Encoder.create () in
  PB.encode_graph_proto onnx_graph encoder;
  let oc = open_out filename in 
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc