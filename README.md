# Owl Symbolic Library

Working in progress ...

## Example 


```ocaml
#require "owl-symbolic";;

open Owl_symbolic;; 

let x = Op.(add (flt 3.) (sin (tensor [|3;3|])))
let g = SymGraph.make_graph [|x|] "sym_graph"

let y = ONNX_Engine.of_symbolic g
let _ = ONNX_Engine.serialise y "test.onnx"
```
