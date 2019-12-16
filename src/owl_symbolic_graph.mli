(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type symbolic_node = Owl_symbolic_symbol.t Owl_graph.node

type t =
  { mutable sym_nodes : symbolic_node array
  ; mutable name : string
  ; mutable node_names : string array
  }

val make_node
  :  Owl_symbolic_symbol.t
  -> symbolic_node array
  -> Owl_symbolic_symbol.t Owl_graph.node

val make_graph : symbolic_node array -> string -> t

val topo_iter : (Owl_symbolic_symbol.t Owl_graph.node -> unit) -> t -> unit

val get_input_nodes : t -> Owl_symbolic_symbol.t Owl_graph.node array

val get_output_nodes : t -> symbolic_node array

val null_graph : t

val iter_print : t -> unit

val is_variable : string -> bool

val name : Owl_symbolic_symbol.t Owl_graph.node -> string

val length : t -> int

val shape_or_value : Owl_symbolic_symbol.t Owl_graph.node -> string

val refnum : 'a Owl_graph.node -> int

val to_dot : t -> string

val set_sym : symbolic_node -> Owl_symbolic_symbol.t -> unit
