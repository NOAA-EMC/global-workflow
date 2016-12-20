subroutine read_locinfo()
   ! read localization scales from text file (hybens_locinfo)
   use kinds, only : r_kind,i_kind,r_single
   use params, only : nlevs,corrlengthnh,corrlengthtr,corrlengthsh,letkf_flag
   use enkf_obsmod, only: obloc, oblnp, corrlengthsq, lnsigl, nobstot, &
   obpress, obtype, nobs_conv, nobs_oz, oberrvar
   use kdtree2_module, only: kdtree2, kdtree2_create, kdtree2_destroy, &
                             kdtree2_result, kdtree2_n_nearest
   use constants, only: zero, rearth
   use gridinfo, only: gridloc, logp
   use mpisetup
   logical lexist
   character(len=40)  :: fname = 'hybens_locinfo'
   real(r_kind) oblnp_indx(1)
   real(r_single), allocatable, dimension(:) :: &
   hlength,vlength,lnsigl1,corrlengthsq1
   real(r_kind) logp_tmp(nlevs)
   type(kdtree2),pointer :: kdtree_grid
   type(kdtree2_result),dimension(:),allocatable :: sresults
   integer(i_kind) k, msig, iunit, n1, n2 ,ideln, nob, ierr
   iunit = 91
   ! read in vertical profile of horizontal and vertical localization length
   ! scales, set values for each ob.
   ! First, check the status of input file
   inquire(file=trim(fname),exist=lexist)
   if ( lexist ) then
      allocate(hlength(nlevs),vlength(nlevs))
      allocate(corrlengthsq1(nobstot),lnsigl1(nobstot))
      open(iunit,file=trim(fname),form='formatted')
      rewind(iunit)
      read(iunit,100) msig
      if ( msig /= nlevs ) then
         write(6,*) 'READ_LOCINFO:  ***ERROR*** error in ',trim(fname)
         write(6,*) 'READ_LOCINFO:  levels do not match,msig[read in],nsig[defined] = ',msig,nlevs
         close(iunit)
         call stop2(123)
      endif
      do k=1,nlevs
        read(iunit,101) hlength(k),vlength(k)
        hlength(k) = hlength(k)/0.388
        vlength(k) = abs(vlength(k))/0.388
        ! factor of 0.388 to convert from e-folding scale
        ! to distance Gaspari-Cohn function goes to zero.
        if (nproc .eq. 0) print *,'level=',k,'localization scales (horiz,vert)=',hlength(k),vlength(k)
      end do
      close(iunit)
   else 
     write(6,*) 'READ_LOCINFO:  ***ERROR*** INPUT FILE MISSING -- ',trim(fname)
     call stop2(124)
   end if 
   100 format(I4)
   101 format(F8.1,3x,F6.2)
    kdtree_grid => kdtree2_create(gridloc,sort=.false.,rearrange=.true.)
    allocate(sresults(1))
    if (nobstot > numproc) then
       ideln = int(real(nobstot)/real(numproc))
       n1 = 1 + nproc*ideln
       n2 = (nproc+1)*ideln
       if (nproc == numproc-1) n2 = nobstot
    else
       if(nproc < nobstot)then
         n1 = nproc+1
         n2 = n1
       else
         n1=1
         n2=0
       end if
    end if
    lnsigl1=zero
    corrlengthsq1=zero
    do nob=n1,n2
       if (oberrvar(nob) .lt. 1.e20) then
          ! find horizontal grid point closest to this ob
          call kdtree2_n_nearest(tp=kdtree_grid,qv=obloc(:,nob),nn=1,results=sresults)
          ! find vertical level closest to ob pressure at that grid point.
          oblnp_indx(1) = oblnp(nob)
          if (oblnp_indx(1) .le. logp(sresults(1)%idx,1)) then
             oblnp_indx(1) = 1
          else if (oblnp_indx(1) .ge. logp(sresults(1)%idx,nlevs)) then
             oblnp_indx(1) = nlevs
          else
             logp_tmp = logp(sresults(1)%idx,1:nlevs)
             call grdcrd(oblnp_indx,1,logp_tmp,nlevs,1)
          end if
          corrlengthsq1(nob) = (hlength(nint(oblnp_indx(1)))*1.e3_r_single/rearth)**2
          lnsigl1(nob) = vlength(nint(oblnp_indx(1)))
          ! don't use computed value for ps vertical localization.
          !if (obtype(nob)(1:3) .eq. ' ps')  lnsigl1(nob) = lnsigl(nob)
          ! for radiance obs, double vertical localization used.
          !if (nob > nobs_conv+nobs_oz) lnsigl(nob) = 2.*lnsigl(nob)
          !if (nproc .eq. 0 .and. obtype(nob)(1:3) .eq. '  t') then
          !   write(6,102) nob,trim(obtype(nob)),obpress(nob),oblnp_indx(1),&
          !   sqrt(corrlengthsq1(nob))*rearth/1000.,lnsigl1(nob)
          !endif
          !102 format(i7,1x,a20,1x,f6.1,1x,f5.2,1x,f6.1,1x,f4.2)
       else
          corrlengthsq1(nob) = corrlengthsq(nob)
          lnsigl1(nob) = lnsigl(nob)
       end if
    enddo
    if (nproc .eq. 0) close(iunit)
    ! distribute the results to all processors.
    call mpi_allreduce(lnsigl1,lnsigl,nobstot,mpi_real4,mpi_sum,mpi_comm_world,ierr)
    call mpi_allreduce(corrlengthsq1,corrlengthsq,nobstot,mpi_real4,mpi_sum,mpi_comm_world,ierr)
    call kdtree2_destroy(kdtree_grid)
    ! For LETKF, modify values of corrlengthnh,tr,sh for use in observation box
    ! calculation to be equal to maximum value for any level.
    if (letkf_flag) then
      corrlengthnh=maxval(hlength(1:nlevs))*1.e3_r_single/rearth
      corrlengthtr = corrlengthnh
      corrlengthsh = corrlengthnh
    endif
    deallocate(sresults,hlength,vlength,corrlengthsq1,lnsigl1)
end subroutine read_locinfo
