#!/usr/bin/env python3
import numpy as np
import h5py

h5filename = "fast.h5"
with h5py.File(h5filename, "r") as h5file:
    id_pic = h5file["/ID_PIC"][:]

print(
    len(np.unique(id_pic)) == len(id_pic)
)
print(
    np.min(id_pic) == 1
)
print(
    np.max(id_pic) == len(id_pic)
)