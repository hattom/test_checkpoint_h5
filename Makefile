FC = mpifort

ifdef HDF5_HOME
	HDF5_FFLAGS = -I$(HDF5_HOME)/include
	HDF5_LDFLAGS = -Wl,-rpath,$(HDF5_HOME)/lib -L$(HDF5_HOME)/lib -lhdf5_fortran -lhdf5 -lz
else
	HDF5_FFLAGS = -I/usr/include/hdf5/openmpi/
	HDF5_LDFLAGS = -L/usr/lib/x86_64-linux-gnu/hdf5/openmpi/ -lhdf5_fortran -lhdf5 -lz
endif

FFLAGS = $(HDF5_FFLAGS) $(FFLAGS_F90)
LDFLAGS = $(HDF5_LDFLAGS)

.PHONY: run

test_h5: test_h5.o
	$(FC) -o $@ $^ $(LDFLAGS) $(FFLAGS)

%.o: %.F90
	$(FC) $(FFLAGS) -c $^

run:
	mpirun -n 1 ./test_h5
run2:
	mpirun -n 2 ./test_h5
