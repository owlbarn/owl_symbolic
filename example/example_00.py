#!/usr/bin/env python3

import onnx

if __name__ == "__main__":
    model = onnx.load("test.onnx")
    print(model)