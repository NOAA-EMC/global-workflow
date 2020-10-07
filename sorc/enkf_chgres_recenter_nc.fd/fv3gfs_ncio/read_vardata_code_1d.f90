    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(in), optional :: nslice
    integer, intent(in), optional :: slicedim
    integer, intent(in), optional :: ncstart(1)
    integer, intent(in), optional :: nccount(1)
    integer, intent(out), optional :: errcode
    integer ncerr, nvar, n, nd, dimlen, ncount
    integer, allocatable, dimension(:) :: start, count
    logical return_errcode
    if(present(errcode)) then
       return_errcode=.true.
       errcode = 0
    else
       return_errcode=.false.
    endif
    if (present(nslice)) then
       ncount = nslice
    else
       ncount = 1
    endif
    nvar = get_nvar(dset,varname)
    allocate(start(dset%variables(nvar)%ndims),count(dset%variables(nvar)%ndims))    
    start(:) = 1
    count(:) = 1
    if (present(slicedim)) then
       nd = slicedim
    else
       nd = dset%variables(nvar)%ndims
    end if
    do n=1,dset%variables(nvar)%ndims
       if (n == nd) then
          start(n) = ncount
          count(n) = 1
       else
          start(n) = 1
          count(n) = dset%variables(nvar)%dimlens(n)
          dimlen = dset%variables(nvar)%dimlens(n)
       end if
    end do
    if (dset%variables(nvar)%ndims /= 1 .and. dset%variables(nvar)%ndims /= 2) then
       if (return_errcode) then
          call nccheck(ncerr,halt=.false.)
          errcode=nf90_ebaddim
          return
       else
          print *,'rank of data array != variable ndims (or ndims-1)'
          stop 99
       endif
    endif
    if (allocated(values)) deallocate(values)
    if (present(ncstart) .and. present(nccount)) then
       allocate(values(nccount(1)))
       start(1)=ncstart(1); count(1)=nccount(1)
       if (dset%variables(nvar)%ndims == 2) then
         start(2)=1; count(2)=1
       end if 
       ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values,&
                  start=start, count=count)
    else
       if (dset%variables(nvar)%ndims == 2) then
          allocate(values(dimlen))
          ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values,&
                  start=start, count=count)
       else
          allocate(values(dset%variables(nvar)%dimlens(1)))
          ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
       end if
    end if
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
    else
       call nccheck(ncerr)
    endif
