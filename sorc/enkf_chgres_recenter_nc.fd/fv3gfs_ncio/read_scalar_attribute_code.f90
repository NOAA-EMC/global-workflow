    ! read attribute 'attname' return in 'values'.  If optional
    ! argument 'varname' is given, an variable attribute is returned.
    ! if the attribute is an 1d array, values should be an allocatable 1d
    ! array of the correct type. if values is allocated, it be deallocated
    ! and reallocated.
    type(Dataset), intent(in) :: dset
    character(len=*), intent(in), optional :: varname
    integer, intent(out), optional :: errcode
    character(len=*), intent(in) :: attname
    integer ncerr, varid, nvar
    logical return_errcode
    if(present(errcode)) then
        return_errcode=.true.
        errcode = 0
    else
        return_errcode=.false.
    endif
    if(present(varname))then
        nvar = get_nvar(dset,varname)
        varid = dset%variables(nvar)%varid
    else
        varid = NF90_GLOBAL
    endif 
    ncerr = nf90_get_att(dset%ncid, varid, trim(attname), values)
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
       return
    else
       call nccheck(ncerr)
    endif
