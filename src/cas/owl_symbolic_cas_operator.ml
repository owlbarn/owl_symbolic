(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

 (** Target: Int, Rational, Var, Add *)

(*
 module Int = struct

  type t = {
    v : int 
  }

  let create v = 
    if v = 1 then one () else 
    if v = 0 then zero () else 
    if v = -1 then negzero () else 
    int v
  
 end 
 *)