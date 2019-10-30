(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_specs
(* open Owl_symbolic_symbol *)

type t = Onnx_types.graph_proto


let of_symbolic (sym_graph : symbolic_graph) =
  let syms = sym_graph.symbols in
  let len = Array.length syms in 

  let default_node = PT.default_node_proto () in
  let node = Array.make len default_node in
  let initialiser = ref [] in 
  let input = ref [] in 
  let output = ref [] in 

  (* Change this to a graph.iterate function *)
  for i = 0 to len - 1 do 
    (* Build nodeprotos *)
    let n = syms.(i) in 
    let name = Owl_symbolic_symbol.name n in
    let ninput = [] in 
    let noutput = [] in 
    let nproto = PT.default_node_proto ~name 
      ~input:ninput ~output:noutput () in 
    node.(i) <- nproto;

    (* constant inputs : TensorProto *)
    let op_typ = Owl_symbolic_symbol.op_type n in
    let constant = match op_typ with
      | "One"   -> PT.default_tensor_proto 
        ~name ~int32_data:[Int32.one] ()
      | "Ones"  -> PT.default_tensor_proto 
        ~name ~int32_data:[Int32.one] ()
      | "Float" -> PT.default_tensor_proto 
        ~name ~float_data:[1.] ()
      | _ -> failwith "fail"
    in
    initialiser := List.append !initialiser [constant];
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
