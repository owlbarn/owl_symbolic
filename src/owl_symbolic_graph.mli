(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type symbol = Owl_symbolic_symbol.t Owl_graph.node

type t =
  { mutable sym_nodes : symbol array
  ; mutable name : string
  ; mutable node_names : string array
  }

val make_node
  :  Owl_symbolic_symbol.t
  -> symbol array
  -> Owl_symbolic_symbol.t Owl_graph.node

val make_graph : symbol array -> string -> t

(* TODO: The function in graph module is messy *)

val topo_iter : (Owl_symbolic_symbol.t Owl_graph.node -> unit) -> t -> unit

val get_input_nodes : t -> Owl_symbolic_symbol.t Owl_graph.node array

val get_output_nodes : t -> symbol array

val null_graph : t

val iter_print : t -> unit

val is_variable : string -> bool

val name : Owl_symbolic_symbol.t Owl_graph.node -> string

val length : t -> int

val shape_or_value : Owl_symbolic_symbol.t Owl_graph.node -> string

val refnum : 'a Owl_graph.node -> int

val to_dot : t -> string

val set_sym : symbol -> Owl_symbolic_symbol.t -> unit

val tensor_node_from_int_array : ?name:string -> int array -> symbol
