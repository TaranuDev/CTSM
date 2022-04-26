module writeNETCDF
contains

subroutine writenetcdffile(array)
    use netcdf
    implicit none
    real, intent(IN), dimension(:) :: array

    integer :: file_id, xdim_id
    integer :: array_id
    integer, dimension(1) :: arrdims
    character(len=*), parameter :: arrunit = 'ergs'

    integer :: i
    integer :: ierr

    i = size(array,1)

    ! create the file
    ierr = nf90_create(path='/glade/u/home/staranu/CTSM_input_test/tools/mksurfdata_map/test_array1.nc', cmode=NF90_CLOBBER, ncid=file_id)

    ! define the dimensions
    ierr = nf90_def_dim(file_id, 'X', i, xdim_id)

    ! now that the dimensions are defined, we can define variables on them,...
    arrdims = (/ xdim_id /)
    ierr = nf90_def_var(file_id, 'Array',  NF90_REAL, arrdims, array_id)

    ! ...and assign units to them as an attribute 
    ierr = nf90_put_att(file_id, array_id, "units", arrunit)

    ! done defining
    ierr = nf90_enddef(file_id)

    ! Write out the values
    ierr = nf90_put_var(file_id, array_id, array)

    ! close; done
    ierr = nf90_close(file_id)
return
end subroutine writenetcdffile
end module writeNETCDFs