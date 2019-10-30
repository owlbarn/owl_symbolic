(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types

(** A series of graph operations. The implementation may differ. *)

let null_graph = { symbols = [||]}

(** NOTE: I have Tree structure in mind when coding all these... *)
let make_graph (n : Owl_symbolic_symbol.t) (inputs : symbolic_graph array) =
    let nodes = Array.fold_left (fun a x -> 
        let ns = x.symbols in
        Array.append a ns
    ) [||]  inputs 
    in
    { symbols = Array.append [|n|] nodes }

let name (g : symbolic_graph) = 
    let header = g.symbols.(0) in 
    Owl_symbolic_symbol.name header

(** Targeted operations on the graph *)

let print _x = ()

let derive _x = ()

let replace _x _y = ()