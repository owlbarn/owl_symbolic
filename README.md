# Owl Symbolic Library

Example 
-------

```ocaml
#require "owl-symbolic";;

open Owl_symbolic;; 

let x = Op.(add (flt 3.) (sin (ones [|3;3|])));;
let y = ONNX_Engine.of_symbolic x
```
