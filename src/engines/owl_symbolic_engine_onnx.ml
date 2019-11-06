(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*open Owl_symbolic_types *)
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

let of_symbolic (sym_graph : Owl_symbolic_graph.symbolic_graph) =
  let len = G.length sym_graph in
  let default_node = PT.default_node_proto () in
  let node = Array.make len default_node in
  let initialiser = ref [] in
  let input = ref [] in
  let output = ref [] in
  let i = ref 0 in
  G.iter
    (fun n ->
      (* build nodeprotos *)
      let sym = Owl_graph.attr n in
      let name = S.name sym in
      let ninput = S.input sym in
      let noutput = S.output sym in
      let op_type = S.op_type sym in
      (* build node attributes *)
      let attr_name = name ^ "_attr" in
      let attr =
        match sym with
        | Float _  ->
          let _v = S.value sym in
          PT.default_attribute_proto ~name:attr_name ~type_:PT.Float ~f:1.0 ()
        | Int _    ->
          PT.default_attribute_proto ~name:attr_name ~type_:PT.Int ~i:(Int64.of_int 1) ()
        | Tensor _ ->
          let t = PT.default_tensor_proto () in
          PT.default_attribute_proto ~name:attr_name ~type_:PT.Tensor ~t:(Some t) ()
        | _        -> PT.default_attribute_proto ~name:attr_name ()
      in
      let nproto =
        PT.default_node_proto
          ~name
          ~input:ninput
          ~output:noutput
          ~op_type
          ~attribute:[ attr ]
          ()
      in
      node.(!i) <- nproto;
      i := !i + 1;
      (* build constant inputs : TensorProto *)
      let constant =
        match sym with
        | Float _ -> [ make_tensorproto_float sym ]
        | _       -> []
      in
      initialiser := List.append !initialiser constant;
      (* inputs and outputs : ValueInforProto *)
      let input_valinfo =
        match sym with
        | Symbol _ -> [ PT.default_value_info_proto ~name () ]
        | _        -> []
      in
      input := List.append !input input_valinfo;
      (* TOOD: find the correct output *)
      output := [ PT.default_value_info_proto ~name:"" () ])
    sym_graph;
  (* result *)
  let node = Array.to_list node in
  let initialiser = !initialiser in
  let input = !input in
  let output = !output in
  let default_graph =
    PT.default_graph_proto ~node ~initializer_:initialiser ~input ~output ()
  in
  default_graph


let to_symbolic (_onnx_graph : t) = G.null_graph

let serialise (onnx_graph : t) filename =
  let encoder = Pbrt.Encoder.create () in
  PB.encode_graph_proto onnx_graph encoder;
  let oc = open_out filename in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc
