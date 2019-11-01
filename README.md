# Owl Symbolic Library

## Example 

```ocaml
#require "owl-onnx";;
open Owl_onnx;; 

let x = Owl_symbolic_operator.(add (flt 3.) (sin (ones [|3;3|])));;
let y = Owl_symbolic.ONNX_Engine.of_symbolic x;;
```