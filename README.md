# test_h5: a miniapp for benchmarking particle checkpointing based on the gyrokinetic PIC code ORB5

## Compile time options
* `-DNO_MPI_F08`: use `use mpi` instead of `use mpi_f08`
* `-DDO_F90`: include f90-style file per process as well as HDF5
* `-DH5_INDEPENDENT`: perform "independent" parallel-HDF5 IO instead of "collective"
