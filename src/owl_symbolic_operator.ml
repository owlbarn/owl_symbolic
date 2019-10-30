(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_utils

let one () = 
  let suffix = generate_suffix () in 
  let name = Printf.sprintf "one_%i" suffix in
  let o_name = "a" in 
  let input = [ ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.One.create name input output in
  Owl_symbolic_symbol.One o


let ones () = 
  let suffix = generate_suffix () in 
  let name = Printf.sprintf "ones_%i" suffix in
  let o_name = "a" in 
  let input = [ ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Ones.create name input output in
  Owl_symbolic_symbol.Ones o

let flt x = 
  let suffix = generate_suffix () in 
  let name = Printf.sprintf "float_%i" suffix in
  let o_name = "a" in 
  let input = [ ] in
  let output = [ o_name ] in
  let value = x in 
  let o = Owl_symbolic_ops_math.Float.create name input output value in
  Owl_symbolic_symbol.Float o

let add x y =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "add_%i" suffix in
  let x_name = Owl_symbolic_symbol.name x in
  let y_name = Owl_symbolic_symbol.name y in
  let o_name = "z" in
  let input = [ x_name; y_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Add.create name input output in
  Owl_symbolic_symbol.Add o

let pow x y =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "pow_%i" suffix in
  let x_name = Owl_symbolic_symbol.name x in
  let y_name = Owl_symbolic_symbol.name y in
  let o_name = "z" in
  let input = [ x_name; y_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Pow.create name input output in
  Owl_symbolic_symbol.Pow o


let sin x =
  let suffix = generate_suffix () in
  let name = Printf.sprintf "sin_%i" suffix in
  let x_name = Owl_symbolic_symbol.name x in
  let o_name = "y" in
  let input = [ x_name ] in
  let output = [ o_name ] in
  let o = Owl_symbolic_ops_math.Sin.create name input output in
  Owl_symbolic_symbol.Sin o

let construct _x = ()

let print _x = ()

let derive _x = ()

let replace _x _y = ()