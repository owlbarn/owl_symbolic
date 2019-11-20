(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*
open Owl_types

module Make (A : Ndarray_Mutable) = struct

  module G = Owl_computation_cpu_engine.Make (A)
*)

module Make (G : Owl_computation_engine_sig.Flatten_Sig) = struct
  open G.Optimiser.Operator.Symbol.Shape.Type

  type t = G.graph

  (* Target op (Owl -- Symbolic):
  HOWEVER, beware that: Uniform node  requires two const nodes as parameter;
  so we are not one-to-one map.
  Wait, no actually, we need to have "RandomUniform/Normal" operators in Symbolic as in ONNX. 
  That means we need to ditch the two const node, but use then as attributes of the RandomUniform node.
  No need to shape inference since that' already done in Owl

  Special ops:

  - Uniform+Const ---> multiple owl nodes to one sym node
  - Reshape --> onne owl nodes to multiple sym node
  - Conv --> optional input
  - maxpool  -> multiple output (2, one of them is optional)

  *)

  (** Helper function *)

  (*
  let _get_const_value (attr : Symbol.Shape.Type.attr) =
    if Array.length attr.value > 0
    then (
      let v = attr.value.(0) in
      if Device.is_elt v
      then Device.value_to_float v
      else failwith "Non-float value const not supported yet")
    else failwith "Non-value const"
  *)

  (** Main entry *)

  (* TODO: create subroutines to do those long match *)
  let to_symbolic (cgraph : t) =
    let outputs = G.get_outputs cgraph in
    (* name each node properly *)
    Owl_graph.iter_ancestors
      ~order:DFS
      ~traversal:PostOrder
      (fun node ->
        let name = Owl_graph.name node in
        let name =
          if name <> ""
          then name
          else (
            let id = Owl_graph.id node in
            Printf.sprintf "owlnode%d" id)
        in
        Owl_graph.set_name node name)
      outputs;
    (* TODO: change the length *)
    let syms = Hashtbl.create 100 in
    (* iterate Owl CGraph in topology order *)
    Owl_graph.iter_ancestors
      ~order:DFS
      ~traversal:PostOrder
      (fun node ->
        let cnode_attr = Owl_graph.attr node in
        let name = Owl_graph.name node in
        (* find in dict the input sym nodes of current sym *)
        (* TODO: there has to be the performance issue *)
        let (sym_inputs : Owl_symbolic_graph.symbolic_node array) =
          Array.map
            (fun n ->
              let n = Owl_graph.name n in
              try Hashtbl.find syms n with
              | Not_found -> failwith "owl_to_symbolic: input node not found.")
            (Owl_graph.parents node)
        in
        (* build the current symbol *)
        let sym =
          (* Damn you ocamlformat; sometimes I just want to punch you in that perfect teeth *)
          match cnode_attr.op with
          | Var ->
            let shape = cnode_attr.shape in
            let s =
              match shape.(0) with
              | Some s -> s
              | None   -> failwith "unspecified owl shape"
            in
            Owl_symbolic_operator.variable ~shape:s ~dtype:SNT_Float name
          | Zeros shp ->
            let ele_num = Owl_symbolic_utils.nelt shp in
            let flt_val = Array.make ele_num 0. in
            let tensor = Owl_symbolic_types.make_tensor ~flt_val shp in
            Owl_symbolic_operator.tensor ~name tensor
          | Ones shp ->
            let ele_num = Owl_symbolic_utils.nelt shp in
            let flt_val = Array.make ele_num 1. in
            let tensor = Owl_symbolic_types.make_tensor ~flt_val shp in
            Owl_symbolic_operator.tensor ~name tensor
          | Uniform shp ->
            (* !!! we need to get its input from CGraph node; while they are 
             * both just attributes in symbolic;
             * Also, node the order of high/low; should be checked later *)
            let inodes = Owl_graph.parents node in
            let high =
              G.Optimiser.Operator.Symbol.node_to_elt inodes.(0)
              |> G.Optimiser.Operator.Symbol.elt_to_float
            in
            let low =
              G.Optimiser.Operator.Symbol.node_to_elt inodes.(1)
              |> G.Optimiser.Operator.Symbol.elt_to_float
            in
            Owl_symbolic_operator.random_uniform ~name ~high ~low shp
          | Const ->
            (* NOTE: Uniform's constant parents are converted but later ignored. *)
            let shape =
              match cnode_attr.shape.(0) with
              | Some s -> s
              | None   -> failwith "Const: unspecified owl shape"
            in
            let flt_val =
              if shape = [||]
              then
                [| G.Optimiser.Operator.Symbol.node_to_elt node
                   |> G.Optimiser.Operator.Symbol.elt_to_float
                |]
              else
                (* TODO: G.node_to_arr node |> G.unpack_arr |> A.to_array *)
                failwith "Convert constant Ndarray value is not supported yet."
            in
            (* TODO: change the dtype to float/double accoding to specific Owl ndarray type *)
            let t = Owl_symbolic_types.make_tensor ~flt_val shape in
            Owl_symbolic_operator.tensor ~name t
          | Sin -> Owl_symbolic_operator.sin ~name sym_inputs.(0)
          | Cos -> Owl_symbolic_operator.cos ~name sym_inputs.(0)
          | Sqrt -> Owl_symbolic_operator.sqrt ~name sym_inputs.(0)
          | Exp -> Owl_symbolic_operator.exp ~name sym_inputs.(0)
          | Log -> Owl_symbolic_operator.log ~name sym_inputs.(0)
          | Neg -> Owl_symbolic_operator.neg ~name sym_inputs.(0)
          | Scalar_Neg -> Owl_symbolic_operator.neg ~name sym_inputs.(0) (* ? *)
          | Relu -> Owl_symbolic_operator.relu ~name sym_inputs.(0)
          | Add -> Owl_symbolic_operator.add ~name sym_inputs.(0) sym_inputs.(1)
          | AddScalar -> Owl_symbolic_operator.add ~name sym_inputs.(0) sym_inputs.(1)
          | ScalarAdd -> Owl_symbolic_operator.add ~name sym_inputs.(0) sym_inputs.(1)
          | Sub -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1)
          | SubScalar -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1)
          | ScalarSub -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1)
          | Scalar_Sub ->
            Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1) (* ? *)
          | Mul -> Owl_symbolic_operator.mul ~name sym_inputs.(0) sym_inputs.(1)
          | MulScalar -> Owl_symbolic_operator.mul ~name sym_inputs.(0) sym_inputs.(1)
          | ScalarMul -> Owl_symbolic_operator.mul ~name sym_inputs.(0) sym_inputs.(1)
          | Div -> Owl_symbolic_operator.div ~name sym_inputs.(0) sym_inputs.(1)
          | DivScalar -> Owl_symbolic_operator.div ~name sym_inputs.(0) sym_inputs.(1)
          | ScalarDiv -> Owl_symbolic_operator.div ~name sym_inputs.(0) sym_inputs.(1)
          | Pow -> Owl_symbolic_operator.pow ~name sym_inputs.(0) sym_inputs.(1)
          | PowScalar -> Owl_symbolic_operator.pow ~name sym_inputs.(0) sym_inputs.(1)
          | ScalarPow -> Owl_symbolic_operator.pow ~name sym_inputs.(0) sym_inputs.(1)
          (* A more proper implementation could be GEMM instead of MatMul *)
          | Dot (_, _, _, _) ->
            Owl_symbolic_operator.matmul ~name sym_inputs.(0) sym_inputs.(1)
          | SumReduce a -> Owl_symbolic_operator.reduce_sum ~name sym_inputs.(0) a
          | Sum a -> Owl_symbolic_operator.reduce_sum ~name sym_inputs.(0) [| a |]
          | Sum' ->
            (* !!! *)
            let shape = cnode_attr.shape in
            let len =
              match shape.(0) with
              | Some s -> Array.length s
              | None   -> failwith "Owl_engine/sum':unspecified owl shape."
            in
            let axes = Owl_utils_array.range 0 (len - 1) in
            Owl_symbolic_operator.reduce_sum ~name ~keepdims:false sym_inputs.(0) axes
          | Max a -> Owl_symbolic_operator.reduce_max ~name sym_inputs.(0) [| a |]
          | Reshape shp ->
            let t =
              Owl_symbolic_types.make_tensor
                ~dtype:SNT_Int64
                ~int_val:shp
                [| Array.length shp |]
            in
            let shp_node = Owl_symbolic_operator.tensor t in
            (* !!!NOTE: we create a node shp_node, but it is not added to the dict
             * since it is only used by reshape node; also note the order of two inputs. *)
            Owl_symbolic_operator.reshape ~name sym_inputs.(0) shp_node
          | Conv2d (padding, stride) ->
            let pad = if padding = SAME then "SAME" else "VALID" in
            Owl_symbolic_operator.conv
              ~name
              sym_inputs.(0)
              sym_inputs.(1)
              pad
              stride
              [| 1; 1 |]
          | MaxPool2d (padding, kernel, stride) ->
            let pad = if padding = SAME then "SAME" else "VALID" in
            Owl_symbolic_operator.maxpool
              ~name
              sym_inputs.(0)
              kernel
              stride
              pad
              [| 1; 1 |]
          | _ ->
            failwith
              (Printf.sprintf
                 "Node type not supported: %s"
                 (G.Optimiser.Operator.Symbol.op_to_str cnode_attr.op))
        in
        Hashtbl.add syms name sym)
      outputs;
    (* choose only the output symbols to be in the graph *)
    let output_sym_nodes =
      Array.map
        (fun n ->
          let name = Owl_graph.name n in
          Hashtbl.find syms name)
        outputs
    in
    (* TODO: why the fxxk can't I use cgraph.name? *)
    Owl_symbolic_graph.make_graph output_sym_nodes ""


  (* Dummy *)
  let of_symbolic (_sym_graph : Owl_symbolic_graph.t) =
    G.make_graph ~input:[||] ~output:[||] "dummy-graph"


  (*
  let eval_arr (sym_graph : Owl_symbolic_graph.t) =
    let cgraph_arr = of_symbolic sym_graph |> G.node_to_arr in
    G.eval_arr [| cgraph_arr |];
    G.unpack_arr cgraph_arr


  let eval_elt (sym_graph : Owl_symbolic_graph.t) =
    let cgraph_elt = of_symbolic sym_graph |> G.node_to_elt in
    G.eval_elt [| cgraph_elt |];
    G.unpack_elt cgraph_elt
  *)

  (** save an cgraph model to file *)
  let save _cgraph _filename = ()

  (** load an cgraph model from file *)
  let load _filename = Obj.magic None
end
