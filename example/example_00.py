#!/usr/bin/env python3

import onnx
import torch

if __name__ == "__main__":
    input = torch.randn(10, 3, 224, 224)
    model = onnx.load("test.onnx")
    onnx.checker.check_model(model)
    print(model)