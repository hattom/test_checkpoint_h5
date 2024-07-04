program test_h5
  use iso_fortran_env, only: REAL64, INT64
  use mpi_f08
  use hdf5
  implicit none

  real(kind=REAL64), allocatable, dimension(:) :: s_vals
  integer(kind=int64), allocatable, dimension(:) :: id_vals

  integer :: ip, npart
  ! MPI
  integer :: ierr, me_world, nranks
  ! HDF5
  integer :: error
  integer(hid_t) :: plist_id, wlist_id
  integer(hid_t) :: file_id
  !integer :: comm, info
  !integer :: rank
  integer(HSIZE_T) :: dimsf(1),dimsm(1)
  integer(HID_T) :: filespace,memspace
  integer(HID_T), allocatable, dimension(:) :: dset_id
  integer(HID_T) :: ddd
  integer :: nfields
  integer :: ifile
  integer(HSIZE_T) :: start(1)
  integer(HSIZE_T) :: count(1)
  integer(HSIZE_T) :: stride(1)
  integer(HSIZE_T) :: block_(1)
  integer :: total_part_num
  integer, dimension(:), allocatable :: part_per_proc
  integer :: myrec
  real :: dum(1000)

  character(160) :: fname

  npart = 100000

  allocate(s_vals(npart))
  allocate(id_vals(npart))

  call MPI_Init(ierr)
  call Mpi_Comm_rank(MPI_COMM_WORLD, me_world, ierr)
  call Mpi_Comm_size(MPI_COMM_WORLD, nranks, ierr)


  allocate(part_per_proc(nranks))
  call MPI_Allgather(npart, 1, MPI_INTEGER, part_per_proc, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
  total_part_num = sum(part_per_proc)

  myrec = 0
  do ifile = 1, me_world
    myrec = myrec + part_per_proc(ifile)
  enddo

  do ip = 1, npart
    s_vals(ip) = 1.0e-7_real64 * (ip + myrec)
    id_vals(ip) = ip + myrec
  end do

  print *, me_world, '/', nranks, id_vals(50000), s_vals(50000), myrec, total_part_num

  fname = 'fast.h5'


  ! Initialize fortran predefined datatypes
  call h5open_f(error)
  ! Setup file access property list for MPI-IO access
  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
  call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD%MPI_VAL, MPI_INFO_NULL%MPI_VAL, error)

  ! Create the file collectively
  call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, error, access_prp=plist_id)


  count(1) = npart
  start(1) = myrec
  stride(1) = 1
  block_(1) = 1

  dimsf(1) = total_part_num
  dimsm(1) = npart
  nfields = 2

  allocate(dset_id(nfields))
  call h5screate_simple_f(1, count, memspace, error)
  call h5screate_simple_f(1, dimsf, filespace, error)
  call h5dcreate_f(file_id, "S_PIC", H5T_NATIVE_DOUBLE, filespace, dset_id(1), error)
  call h5dcreate_f(file_id, "ID_PIC", H5T_STD_I64LE, filespace, dset_id(2), error)

  call h5pcreate_f(H5P_DATASET_XFER_F, wlist_id, error)
  call h5pset_dxpl_mpio_f(wlist_id, H5FD_MPIO_COLLECTIVE_F, error)
  call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, start, count, error, stride, block_)

  call h5dwrite_f(dset_id(1), H5T_NATIVE_DOUBLE, s_vals(:), dimsf, &
                  error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
  call h5dwrite_f(dset_id(2), H5T_STD_I64LE, id_vals(:), dimsf, &
                  error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
  !call h5dwrite_f(dset_id(1), H5T_NATIVE_DOUBLE, s_vals(:), dimsf, &
  !  error, file_space_id=filespace, xfer_prp=wlist_id)

  call MPI_Barrier(MPI_COMM_WORLD, error)

  do ifile = 1, nfields
    call h5dclose_f(dset_id(ifile), error)
  end do

  call h5sclose_f(filespace, error)
  call h5sclose_f(memspace, error)
  call h5pclose_f(plist_id, error)
  call h5pclose_f(wlist_id, error)

  ! close file
  call h5fclose_f(file_id, error)
  ! close fortran interface
  call h5close_f(error)



  call MPI_Finalize(ierr)
end program test_h5