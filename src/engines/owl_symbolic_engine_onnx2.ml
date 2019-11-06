(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
module G = Owl_symbolic_graph
module S = Owl_symbolic_symbol

type t = Onnx_types.graph_proto

let make_onnx_node op_type input_names output_names name attr =
    let input_names = Array.to_list input_names in 
    let output_names = Array.to_list output_names in 
    PT.default_node_proto 
        ~input:input_names ~output:output_names ~name
        ~op_type ~attribute:attr ()

let make_onnx_graph (nodes : PT.node_proto array) (_output_names : string) =
    let nodes = Array.to_list nodes in
    PT.default_graph_proto ~node:nodes ()
    

(** Core function. Converts symbolic nodes to onnx nodes. *)
let sym_nodes_to_onnx (sym_nodes : G.symbolic_node array) =
    let nodes = [||] in
    (* Assume a one-to-one projection; might be changed later *)
    Array.iteri (fun i sym -> 
        let sym_attrs = S.get_attrs sym in 
        let onnx_attrs = [] in 
        Array.iter (fun a -> 
            (** match symbolic attribute and make onnx attribute *)
            ()
        ) sym_attrs;

        let name = "" in 
        let op_type = "" in
        let input_names = [|""|] in 
        let output_names = [|""|] in

        let n = make_onnx_node op_type input_names 
            output_names name onnx_attrs in 
        nodes.(i) <- n
    ) sym_nodes;
    nodes 


(** Main entry of conversion *)
let of_symbolic (sym_graph : Owl_symbolic_graph.symbolic_graph) = 
    (** Step 1: convert symbolic nodes to  *)
    let symnodes = sym_graph.sym_nodes in 
    let nodes = sym_nodes_to_onnx symnodes in 
    (* The inpput/output names should be specifiled *)
    let output_names = "" in 
    (** Steps 2- N: more processing such as rewriting complex nodes *)
    (** Final Step: make graph *)
    make_onnx_graph nodes output_names
    (** Maybe some post-processing steps *)
