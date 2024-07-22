module test_h5_mod
  use iso_fortran_env, only: REAL64, INT64
  use mpi_f08
  use hdf5
  implicit none
  character(len=*), parameter :: fname = 'fast.h5'

  contains

  subroutine test_write
    real(kind=REAL64), allocatable, dimension(:) :: s_vals
    integer(kind=int64), allocatable, dimension(:) :: id_vals
    integer, allocatable, dimension(:) :: rank_vals

    integer :: ip, npart
    ! MPI
    integer :: ierr, me_world, nranks
    ! HDF5
    integer :: error
    integer(hid_t) :: plist_id, wlist_id
    integer(hid_t) :: file_id
    !integer :: comm, info
    !integer :: rank
    integer(HID_T) :: filespace, memspace, filespace_p, memspace_p, filespace_s, memspace_s
    integer(HID_T), allocatable, dimension(:) :: dset_id
    integer :: nfields, ifile
    integer(HSIZE_T), dimension(1) :: start_, count_, stride_, block_, dimsf
    integer(HSIZE_T), dimension(1) :: start_p, count_p, stride_p, block_p, dimsf_p
    integer(HSIZE_T), dimension(1) :: start_s, count_s, stride_s, block_s, dimsf_s
    integer :: total_part_num
    integer, dimension(:), allocatable :: part_per_proc
    integer :: myrec
    real(kind=REAL64), dimension(:), allocatable :: serial_arr

    call Mpi_Comm_rank(MPI_COMM_WORLD, me_world, ierr)
    call Mpi_Comm_size(MPI_COMM_WORLD, nranks, ierr)

    npart = 100 + me_world - nranks/2
    print *, me_world, "npart:", npart

    allocate(s_vals(npart))
    allocate(id_vals(npart))
    allocate(rank_vals(npart))

    allocate(part_per_proc(nranks))
    call MPI_Allgather(npart, 1, MPI_INTEGER, part_per_proc, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
    total_part_num = sum(part_per_proc)
    myrec = sum(part_per_proc(1:me_world))

    do ip = 1, npart
      s_vals(ip) = 1.0e-7_real64 * (ip + myrec)
      id_vals(ip) = ip + myrec
    end do
    rank_vals(:) = me_world

    print *, me_world, '/', nranks, id_vals(5), s_vals(5), myrec, total_part_num

    ! Setup file access property list for MPI-IO access
    call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD%MPI_VAL, MPI_INFO_NULL%MPI_VAL, error)

    ! Create the file collectively
    call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, error, access_prp=plist_id)

    start_s(1) = merge(1, 0, me_world>0)
    stride_s(1) = 1
    block_s(1) = merge(1, 0, me_world==0)
    dimsf_s(1) = 1
    count_s(1) = merge(1, 0, me_world==0)
    allocate(serial_arr(count_s(1)))
    serial_arr(:) = 3.0_REAL64

    count_p(1) = 1
    start_p(1) = me_world
    stride_p(1) = 1
    block_p(1) = 1
    dimsf_p(1) = nranks

    count_(1) = npart
    start_(1) = myrec
    stride_(1) = 1
    block_(1) = 1
    dimsf(1) = total_part_num

    nfields = 3
    nfields = nfields + 1
    nfields = nfields + 1
    allocate(dset_id(nfields))

    call h5screate_simple_f(1, count_p, memspace_p, error)
    call h5screate_simple_f(1, dimsf_p, filespace_p, error)
    call h5pcreate_f(H5P_DATASET_XFER_F, wlist_id, error)
    call h5pset_dxpl_mpio_f(wlist_id, H5FD_MPIO_COLLECTIVE_F, error)
    call h5dcreate_f(file_id, "npart", H5T_NATIVE_INTEGER, filespace_p, dset_id(nfields), error)
    call h5sselect_hyperslab_f(filespace_p, H5S_SELECT_SET_F, start_p, count_p, error, stride_p, block_p)
    call h5dwrite_f(dset_id(nfields), H5T_NATIVE_INTEGER, (/ npart /), dimsf_p, &
                    error, mem_space_id=memspace_p, file_space_id=filespace_p, xfer_prp=wlist_id)
    call h5sclose_f(filespace_p, error)
    call h5sclose_f(memspace_p, error)


    call h5screate_simple_f(1, count_s, memspace_s, error)
    call h5screate_simple_f(1, dimsf_s, filespace_s, error)
    call h5dcreate_f(file_id, "mumax", H5T_NATIVE_DOUBLE, filespace_s, dset_id(nfields-1), error)
    call h5sselect_hyperslab_f(filespace_s, H5S_SELECT_SET_F, start_s, count_s, error, stride_s, block_s)
    call h5dwrite_f(dset_id(nfields-1), H5T_NATIVE_DOUBLE, serial_arr, dimsf_s, &
                    error, mem_space_id=memspace_s, file_space_id=filespace_s, xfer_prp=wlist_id)
    call h5sclose_f(filespace_s, error)
    call h5sclose_f(memspace_s, error)

    call h5screate_simple_f(1, count_, memspace, error)
    call h5screate_simple_f(1, dimsf, filespace, error)
    call h5dcreate_f(file_id, "S_PIC", H5T_NATIVE_DOUBLE, filespace, dset_id(1), error)
    call h5dcreate_f(file_id, "ID_PIC", H5T_STD_I64LE, filespace, dset_id(2), error)
    call h5dcreate_f(file_id, "PROC_PIC", H5T_NATIVE_INTEGER, filespace, dset_id(3), error)

    call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, start_, count_, error, stride_, block_)

    call h5dwrite_f(dset_id(1), H5T_NATIVE_DOUBLE, s_vals(:), dimsf, &
                    error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
    call h5dwrite_f(dset_id(2), H5T_STD_I64LE, id_vals(:), dimsf, &
                    error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
    call h5dwrite_f(dset_id(3), H5T_NATIVE_INTEGER, rank_vals(:), dimsf, &
                    error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)

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

    deallocate(s_vals, id_vals, rank_vals, part_per_proc, dset_id, serial_arr)
  end subroutine test_write

  subroutine test_read_p
    real(kind=REAL64), allocatable, dimension(:) :: s_vals
    integer(kind=int64), allocatable, dimension(:) :: id_vals
    integer, allocatable, dimension(:) :: rank_vals

    integer :: npart
    ! MPI
    integer :: ierr, me_world, nranks
    ! HDF5
    integer :: error
    integer(hid_t) :: plist_id, wlist_id
    integer(hid_t) :: file_id
    !integer :: comm, info
    !integer :: rank
    integer(HID_T) :: filespace, memspace
    integer(HID_T) :: filespace_p, memspace_p
    integer(HID_T) :: filespace_s, memspace_s
    integer(HID_T), allocatable, dimension(:) :: dset_id
    integer :: nfields
    integer :: ifile
    integer(HSIZE_T), dimension(1) :: start_, count_, stride_, block_, dimsf
    integer(HSIZE_T), dimension(1) :: start_p, count_p, stride_p, block_p, dimsf_p
    integer(HSIZE_T), dimension(1) :: start_s, count_s, stride_s, block_s, dimsf_s
    integer :: total_part_num
    integer, dimension(:), allocatable :: part_per_proc
    integer :: myrec
    real(kind=REAL64) :: mumax


    nfields = 3
    nfields = nfields + 1
    nfields = nfields + 1
    allocate(dset_id(nfields))

    call Mpi_Comm_rank(MPI_COMM_WORLD, me_world, ierr)
    call Mpi_Comm_size(MPI_COMM_WORLD, nranks, ierr)

    count_p(1) = 1
    start_p(1) = me_world
    stride_p(1) = 1
    block_p(1) = 1
    dimsf_p(1) = nranks

    count_s(1) = 1
    start_s(1) = 0
    stride_s(1) = 1
    block_s(1) = 1
    dimsf_s(1) = 1
    call MPI_Barrier(MPI_COMM_WORLD, error)

    ! Setup file access property list for MPI-IO access
    call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD%MPI_VAL, MPI_INFO_NULL%MPI_VAL, error)
    ! Create the file collectively
    call h5fopen_f(fname, H5F_ACC_RDONLY_F, file_id, error, access_prp=plist_id)


    !!! number of particles per processor: 1 element per Rank
    call h5screate_simple_f(1, count_p, memspace_p, error)
    call h5screate_simple_f(1, dimsf_p, filespace_p, error)
    call h5pcreate_f(H5P_DATASET_XFER_F, wlist_id, error)
    call h5pset_dxpl_mpio_f(wlist_id, H5FD_MPIO_COLLECTIVE_F, error)
    call h5dopen_f(file_id, "npart", dset_id(nfields), error)
    call h5sselect_hyperslab_f(filespace_p, H5S_SELECT_SET_F, start_p, count_p, error, stride_p, block_p)
    call h5dread_f(dset_id(nfields), H5T_NATIVE_INTEGER, npart, dimsf_p, &
                   error, mem_space_id=memspace_p, file_space_id=filespace_p, xfer_prp=wlist_id)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    call h5sclose_f(filespace_p, error)
    call h5sclose_f(memspace_p, error)
    print *, me_world, npart

    !!! global scalars: Every rank reads the same scalar
    call h5screate_simple_f(1, count_s, memspace_s, error)
    call h5screate_simple_f(1, dimsf_s, filespace_s, error)
    call h5pcreate_f(H5P_DATASET_XFER_F, wlist_id, error)
    call h5pset_dxpl_mpio_f(wlist_id, H5FD_MPIO_COLLECTIVE_F, error)
    call h5dopen_f(file_id, "mumax", dset_id(nfields-1), error)
    call h5sselect_hyperslab_f(filespace_s, H5S_SELECT_SET_F, start_s, count_s, error, stride_s, block_s)
    call h5dread_f(dset_id(nfields-1), H5T_NATIVE_DOUBLE, mumax, dimsf_s, &
                   error, mem_space_id=memspace_s, file_space_id=filespace_s, xfer_prp=wlist_id)
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    call h5sclose_f(filespace_s, error)
    call h5sclose_f(memspace_s, error)

    print *, me_world, mumax


    allocate(s_vals(npart))
    allocate(id_vals(npart))
    allocate(rank_vals(npart))

    allocate(part_per_proc(nranks))
    call MPI_Allgather(npart, 1, MPI_INTEGER, part_per_proc, 1, MPI_INTEGER, MPI_COMM_WORLD, ierr)
    total_part_num = sum(part_per_proc)
    myrec = sum(part_per_proc(1:me_world))

    count_(1) = npart
    start_(1) = myrec
    stride_(1) = 1
    block_(1) = 1
    dimsf(1) = total_part_num

    s_vals(:) = -1.0_real64
    id_vals(:) = -1_int64
    rank_vals(:) = -3

    !!! Large arrays, each rank reads a unique subset (based on npart from above)
    call h5screate_simple_f(1, count_, memspace, error)
    call h5screate_simple_f(1, dimsf, filespace, error)

    call h5dopen_f(file_id, "S_PIC", dset_id(1), error)
    call h5dopen_f(file_id, "ID_PIC", dset_id(2), error)
    call h5dopen_f(file_id, "PROC_PIC", dset_id(3), error)

    call h5pcreate_f(H5P_DATASET_XFER_F, wlist_id, error)
    call h5pset_dxpl_mpio_f(wlist_id, H5FD_MPIO_COLLECTIVE_F, error)
    call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, start_, count_, error, stride_, block_)

    call h5dread_f(dset_id(1), H5T_NATIVE_DOUBLE, s_vals(:), dimsf, &
                    error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
    call h5dread_f(dset_id(2), H5T_STD_I64LE, id_vals(:), dimsf, &
                    error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
    call h5dread_f(dset_id(3), H5T_NATIVE_INTEGER, rank_vals(:), dimsf, &
                    error, mem_space_id=memspace, file_space_id=filespace, xfer_prp=wlist_id)
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

    print *, "XXY", count(rank_vals /= me_world)
  end subroutine test_read_p
end module test_h5_mod

program test_h5
  use iso_fortran_env, only: REAL64, INT64
  use mpi_f08
  use hdf5
  use test_h5_mod
  implicit none
  integer :: ierr, error

  call MPI_Init(ierr)
  ! Initialize fortran predefined datatypes
  call h5open_f(error)

  call test_write()
  call test_read_p()

  ! close fortran interface
  call h5close_f(error)
  call MPI_Finalize(ierr)

end program
