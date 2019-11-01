(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_specs

module G = Owl_symbolic_graph
module S = Owl_symbolic_symbol

type t = Onnx_types.graph_proto


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
    let nproto = PT.default_node_proto ~name 
      ~input:ninput ~output:noutput () in
    node.(i) <- nproto;

    (* build constant inputs : TensorProto *)
    let op_typ = Owl_symbolic_symbol.op_type n in
    let constant = match op_typ with
      | "One"    -> [PT.default_tensor_proto 
        ~name ~int32_data:[Int32.one] ()]
      | "Ones"   -> [PT.default_tensor_proto 
        ~name ~int32_data:[Int32.one] ()]
      | "Float"  -> [PT.default_tensor_proto 
        ~name ~float_data:[1.] ()]
      | "Tensor" -> []
      | _ -> []
    in
    initialiser := List.append !initialiser constant;
    ()
    (* inputs and outputs : ValueInforProto *)

  done;
  (* result *)
  let node = Array.to_list node in
  let initialiser = !initialiser in 
  let input = !input in 
  let output = !output in 
  let default_graph = PT.default_graph_proto 
    ~node  ~initializer_:initialiser ~input ~output () in
  default_graph


let to_symbolic (_onnx_graph : t) = { symbols = [||] }
