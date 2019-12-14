#!/usr/bin/env python3

import numpy as np 
import onnxruntime as rt

""" 1. GEMM

sess = rt.InferenceSession("test.onnx")
a = sess.get_inputs()[0].name
b = sess.get_inputs()[1].name
c = sess.get_inputs()[2].name
a_input = np.ones((3, 4)).astype(np.float32)
b_input = np.ones((4, 5)).astype(np.float32)
c_input = np.ones((3, 5)).astype(np.float32)
pred_onnx = sess.run(None, {a: a_input, b : b_input, c: c_input})[0]

flag01 = np.array_equal(pred_onnx, 
    np.asarray([
    [5., 5., 5., 5., 5.],
    [5., 5., 5., 5., 5.],
    [5., 5., 5., 5., 5.]]))
"""

""" 2. Split  
sess = rt.InferenceSession("test.onnx")
a = sess.get_inputs()[0].name
a_input = np.ones(sess.get_inputs()[0].shape).astype(np.float32)
pred_onnx = sess.run(None, {a: a_input})
print(pred_onnx[0])
print(pred_onnx[1])
"""

sess = rt.InferenceSession("test.onnx")
a = sess.get_inputs()[0].name
a_input = np.ones(sess.get_inputs()[0].shape).astype(np.float32)
pred_onnx = sess.run(None, {a: a_input})
print(pred_onnx[0])
print(pred_onnx[1])