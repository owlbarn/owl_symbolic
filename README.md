# Owl Symbolic Library

Working in progress ...

## Example 


```ocaml
#require "owl-symbolic";;

open Owl_symbolic;; 

let x = Op.(add (flt 3.) (sin (tensor [|3;3|])))
let y = ONNX_Engine.of_symbolic x
let _ = ONNX_Engine.serialise y "test_onnx.proto"
```
