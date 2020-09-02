    type(Dataset), intent(in) :: dset
    character(len=*), intent(in), optional :: varname
    character(len=*), intent(in) :: attname
    integer, intent(out), optional :: errcode
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
    ncerr = nf90_redef(dset%ncid)
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
       if (ncerr /= 0) return
    else
       call nccheck(ncerr)
    endif
    ncerr = nf90_put_att(dset%ncid, varid, trim(attname), values)
    if (return_errcode) then
       call nccheck(ncerr,halt=.false.)
       errcode=ncerr
       ncerr = nf90_enddef(dset%ncid)
       return
    else
       call nccheck(ncerr)
       ncerr = nf90_enddef(dset%ncid)
       call nccheck(ncerr)
    endif
