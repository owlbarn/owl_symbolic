(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module G = Owl_computation_cpu_engine.Make (Owl_dense_ndarray.S)
open G
open Owl_graph
open Owl_symbolic_operator

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

let get_const_value (attr : Symbol.Shape.Type.attr) =
  if Array.length attr.value > 0
  then (
    let v = attr.value.(0) in
    if Device.is_elt v
    then Device.value_to_float v
    else failwith "Non-float value const not supported yet")
  else failwith "Non-value const"


(** Main entry *)

let to_symbolic (cgraph : G.graph) =
  let outputs = G.get_outputs cgraph in
  (* name each node properly *)
  iter_ancestors
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
  iter_ancestors
    ~order:DFS
    ~traversal:PostOrder
    (fun node ->
      let cnode_attr : Symbol.Shape.Type.attr = Owl_graph.attr node in
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
          variable ~shape:s ~dtype:SNT_Float name
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
          let high = G.node_to_elt inodes.(0) |> elt_to_float in
          let low = G.node_to_elt inodes.(1) |> elt_to_float in
          random_uniform ~name ~high ~low shp
        | Sin -> sin ~name sym_inputs.(0)
        | Cos -> cos ~name sym_inputs.(0)
        | Sqrt -> sqrt ~name sym_inputs.(0)
        | Exp -> exp ~name sym_inputs.(0)
        | Log -> log ~name sym_inputs.(0)
        | Neg -> neg ~name sym_inputs.(0)
        | Scalar_Neg -> neg ~name sym_inputs.(0) (* ? *)
        | Relu -> relu ~name sym_inputs.(0)
        | Add -> add ~name sym_inputs.(0) sym_inputs.(1)
        | AddScalar -> add ~name sym_inputs.(0) sym_inputs.(1)
        | ScalarAdd -> add ~name sym_inputs.(0) sym_inputs.(1)
        | Sub -> sub ~name sym_inputs.(0) sym_inputs.(1)
        | SubScalar -> sub ~name sym_inputs.(0) sym_inputs.(1)
        | ScalarSub -> sub ~name sym_inputs.(0) sym_inputs.(1)
        | Mul -> mul ~name sym_inputs.(0) sym_inputs.(1)
        | MulScalar -> mul ~name sym_inputs.(0) sym_inputs.(1)
        | ScalarMul -> mul ~name sym_inputs.(0) sym_inputs.(1)
        | Div -> div ~name sym_inputs.(0) sym_inputs.(1)
        | DivScalar -> div ~name sym_inputs.(0) sym_inputs.(1)
        | ScalarDiv -> div ~name sym_inputs.(0) sym_inputs.(1)
        | Pow -> pow ~name sym_inputs.(0) sym_inputs.(1)
        | PowScalar -> pow ~name sym_inputs.(0) sym_inputs.(1)
        | ScalarPow -> pow ~name sym_inputs.(0) sym_inputs.(1)
        (* A more proper implementation could be GEMM instead of MatMul *)
        | Dot (_, _, _, _) -> matmul ~name sym_inputs.(0) sym_inputs.(1)
        | SumReduce a -> reduce_sum ~name sym_inputs.(0) a
        | Sum a -> reduce_sum ~name sym_inputs.(0) [| a |]
        | Sum' ->
          (* !!! *)
          let shape = cnode_attr.shape in
          let len =
            match shape.(0) with
            | Some s -> Array.length s
            | None   -> failwith "Owl_engine/sum':unspecified owl shape."
          in
          let axes = Owl_utils_array.range 0 (len - 1) in
          reduce_sum ~name ~keepdims:false sym_inputs.(0) axes
        | Max a -> reduce_max ~name sym_inputs.(0) [| a |]
        | Reshape shp ->
          let t =
            Owl_symbolic_types.make_tensor
              ~dtype:SNT_Int64
              ~int_val:shp
              [| Array.length shp |]
          in
          let shp_node = tensor t in
          (* !!!NOTE: we create a node shp_node, but it is not added to the dict
           * since it is only used by reshape node; also note the order of two inputs. *)
          reshape ~name sym_inputs.(0) shp_node
        | _ ->
          failwith
            (Printf.sprintf "Node type not supported: %s" (G.op_to_str cnode_attr.op))
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
  Owl_symbolic_graph.make_graph output_sym_nodes cgraph.name


(* Dummy *)
let of_symbolic (_sym_graph : Owl_symbolic_graph.symbolic_graph) =
  let attr =
    { op = Noop
    ; freeze = true
    ; reuse = false
    ; state = Valid
    ; shape = [||]
    ; value = [||]
    ; block = None
    }
  in
  let n = Owl_graph.node attr in
  G.make_graph ~input:[||] ~output:[| n |] "dummy-graph"


(*
let eval_arr (sym_graph : symbolic_graph) =
  let cgraph_arr = of_symbolic sym_graph |> G.node_to_arr in
  G.eval_arr [| cgraph_arr |];
  G.unpack_arr cgraph_arr


let eval_elt (sym_graph : symbolic_graph) =
  let cgraph_elt = of_symbolic sym_graph |> G.node_to_elt in
  G.eval_elt [| cgraph_elt |];
  G.unpack_elt cgraph_elt
*)

(** save an cgraph model to file *)
let save _cgraph _filename = ()

(** load an cgraph model from file *)
let load _filename = None

(* Add mli files at some point *)
