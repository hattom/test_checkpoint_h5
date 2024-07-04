FC = mpifort
FFLAGS = -I/usr/include/hdf5/openmpi/
LDFLAGS = -L/usr/lib/x86_64-linux-gnu/hdf5/openmpi/ -lhdf5_fortran -lhdf5 -lz

.PHONY: run

test_h5: test_h5.o
	$(FC) -o $@ $^ $(LDFLAGS)

%.o: %.F90
	$(FC) $(FFLAGS) -c $^

run:
	mpirun -n 1 ./test_h5
run2:
	mpirun -n 2 ./test_h5