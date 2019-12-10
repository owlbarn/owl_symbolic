(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2019 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(** AveragePool, MaxPool, MaxUnpool, LpPool, MaxRoiPool,
Conv, QLinearConv,ConvInteger, ConvTranspose, GlobalAveragePool, 
GlobalMaxPool, GlobalLpPool, BatchNormalization, 
InstanceNormalization, LpNormalization, Dropout, Shrink, Flatten, 
LRN, TfIdfVectorizer, StringNormalizer, MeanVarianceNormalization
*)

open Owl_symbolic_types

module Conv = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    ; mutable auto_pad : string
          (* one of NOTSET (default), SAME_UPPER, SAME_LOWER and VALID *)
    ; mutable dilations : int array
    ; mutable kernel_shp : int array
    ; mutable pads : int array option
          (* This attribute cannot be used simultaneously with auto_pad attribute 
           * TODO: Currently set to None; only use auto_pad
           *)
    ; mutable strides : int array
    ; mutable group : int (* TODO: currently use default value *)
    }

  let op_type = "Conv"

  let create
      ?(out_shape = None)
      ?(auto_pad = "NOTSET")
      ?(pads = None)
      name
      input
      attrs
      kernel_shp
      strides
      dilations
    =
    { name
    ; input
    ; attrs
    ; out_shape
    ; auto_pad
    ; dilations
    ; kernel_shp
    ; pads
    ; strides
    ; group = 1
    }
end

module MaxPool = struct
  type t =
    { mutable name : string
    ; mutable input : string array
    ; mutable attrs : (string * attrvalue) array
    ; mutable out_shape : int array option
    ; mutable auto_pad : string
    ; mutable ceil_mode : int
    ; mutable dilations : int array
    ; mutable kernel_shp : int array
    ; mutable pads : int array option
    ; mutable storage_order : int
    ; mutable strides : int array
    }

  let op_type = "MaxPool"

  let create ?(padding = VALID) ?strides ?dilations ?name input_name kernel_shp =
    let attrs = [||] in
    let name = Owl_symbolic_utils.node_name ?name op_type in
    let input = [| input_name |] in
    let dim = Array.length kernel_shp in
    let dilations =
      match dilations with
      | Some d ->
        assert (Array.length d = dim);
        d
      | None   -> Array.make dim 1
    in
    let strides =
      match strides with
      | Some s ->
        assert (Array.length s = dim);
        s
      | None   -> Array.make dim 1
    in
    let auto_pad, pads =
      match padding with
      | SAME_UPPER -> "SAME_UPPER", None
      | SAME_LOWER -> "SAME_LOWRE", None
      | VALID      -> "VALID", None
      | PAD p      ->
        assert (Array.length p = dim);
        "NOTSET", Some p
    in
    { name
    ; input
    ; attrs
    ; out_shape = None
    ; auto_pad
    ; pads
    ; ceil_mode = 0 (* TODO: should we use floor or ceil? *)
    ; dilations
    ; kernel_shp
    ; storage_order = 0 (* We stick with Row-major *)
    ; strides
    }
end
