(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types

(** A series of graph operations. The implementation may differ.
  * For example, Owl_graph could be used. *)

let null_graph = { symbols = [||] }

(** NOTE: I have Tree structure in mind when coding all these... *)
let make_graph (n : Owl_symbolic_symbol.t) (inputs : symbolic_graph array) =
  let nodes =
    Array.fold_left
      (fun a x ->
        let ns = x.symbols in
        Array.append a ns)
      [||]
      inputs
  in
  { symbols = Array.append [| n |] nodes }


(** The name of the graph is the name of root *)
let name (g : symbolic_graph) =
  let header = g.symbols.(0) in
  Owl_symbolic_symbol.name header


(* Return an array of nodes *)
let iterate (g : symbolic_graph) = g.symbols

(** Targeted operations on the graph *)

(** Pretty print a symbolic tree (not graph) to terminal *)
let print _g = ()

(** Derivative of symbolic tree *)
let derive _g = ()

(** Replace certain part of tree with anoter subtree *)
let replace _g _m _r = ()
