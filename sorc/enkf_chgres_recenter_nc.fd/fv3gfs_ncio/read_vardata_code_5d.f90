    type(Dataset), intent(in) :: dset
    character(len=*), intent(in) :: varname
    integer, intent(out), optional :: errcode
    integer ncerr, nvar, n1,n2,n3,n4,n5
    logical return_errcode
    if(present(errcode)) then
       return_errcode=.true.
       errcode = 0
    else
       return_errcode=.false.
    endif
    nvar = get_nvar(dset,varname)
    if (dset%variables(nvar)%ndims /= 5) then
       if (return_errcode) then
          errcode=nf90_ebaddim
          return
       else
          print *,'rank of data array != variable ndims (or ndims-1)'
          stop 99
       endif
    endif
    n1 = dset%variables(nvar)%dimlens(1)
    n2 = dset%variables(nvar)%dimlens(2)
    n3 = dset%variables(nvar)%dimlens(3)
    n4 = dset%variables(nvar)%dimlens(4)
    n5 = dset%variables(nvar)%dimlens(5)
    if (allocated(values)) deallocate(values)
    allocate(values(n1,n2,n3,n4,n5))
    ncerr = nf90_get_var(dset%ncid, dset%variables(nvar)%varid, values)
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
    else
       call nccheck(ncerr)
    endif
