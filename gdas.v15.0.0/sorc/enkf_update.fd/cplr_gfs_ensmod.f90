subroutine get_user_ens_(grd,member,ntindex,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_user_ens_    pretend atmos bkg is the ensemble
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Read in GFS ensemble members in to GSI ensemble.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!
!   input argument list:
!     grd      - grd info for ensemble
!     member   - index for ensemble member
!     ntindex  - time index for ensemble
!
!   output argument list:
!     atm_bundle - atm bundle w/ fields for ensemble member
!     iret       - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use mpimod, only: mype
    use kinds, only: i_kind,r_kind,r_single
    use gridmod, only: regional,use_gfs_nemsio
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_4dvar, only: ens_fhrlevs
    use hybrid_ensemble_parameters, only: ensemble_path,enspreproc
    use hybrid_ensemble_parameters, only: uv_hyb_ens
    use hybrid_ensemble_parameters, only: sp_ens
    use hybrid_ensemble_parameters, only: en_perts
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer,gsi_bundleputvar

    implicit none

    ! Declare passed variables
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname='get_user_ens_'
    character(len=70) :: filename
    logical :: zflag
    logical,save :: inithead = .true.

    integer(i_kind) :: ierr
    integer(i_kind) :: im,jm,km
    integer(i_kind) :: lunit = 10
    real(r_kind),pointer,dimension(:,:)   :: ps
    real(r_kind),pointer,dimension(:,:,:) :: u
    real(r_kind),pointer,dimension(:,:,:) :: v
    real(r_kind),pointer,dimension(:,:,:) :: tv
    real(r_kind),pointer,dimension(:,:,:) :: q
    real(r_kind),pointer,dimension(:,:,:) :: oz
    real(r_kind),pointer,dimension(:,:,:) :: cwmr
    real(r_single),allocatable,dimension(:,:)   :: scr2
    real(r_single),allocatable,dimension(:,:,:) :: scr3

    if ( enspreproc ) then

       if ( member == 0 ) then
          write(filename,11) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),mype
       else
          write(filename,21) trim(adjustl(ensemble_path)),member,ens_fhrlevs(ntindex),mype
       endif
11     format(a,'ensmean',     '_f',i2.2,'.pe',i4.4)
21     format(a,'ens_mem',i3.3,'_f',i2.2,'.pe',i4.4)

       ! This is all that needs to be done.
       !call preproc_read_gfsatm(grd,filename,iret)

       ! Keeping this for now
       im = en_perts(1,1)%grid%im
       jm = en_perts(1,1)%grid%jm
       km = en_perts(1,1)%grid%km

       allocate(scr2(im,jm))
       allocate(scr3(im,jm,km))

       call gsi_bundlegetpointer(atm_bundle,'ps',ps,  ierr); iret = ierr
       call gsi_bundlegetpointer(atm_bundle,'sf',u ,  ierr); iret = ierr + iret
       call gsi_bundlegetpointer(atm_bundle,'vp',v ,  ierr); iret = ierr + iret
       call gsi_bundlegetpointer(atm_bundle,'t' ,tv,  ierr); iret = ierr + iret
       call gsi_bundlegetpointer(atm_bundle,'q' ,q ,  ierr); iret = ierr + iret
       call gsi_bundlegetpointer(atm_bundle,'oz',oz,  ierr); iret = ierr + iret
       call gsi_bundlegetpointer(atm_bundle,'cw',cwmr,ierr); iret = ierr + iret
       if ( iret /= 0 ) then
          if ( mype == 0 ) then
             write(6,'(A)') 'get_user_ens_: ERROR!'
             write(6,'(A)') 'For now, GFS requires all MetFields: ps,u,v,(sf,vp)tv,q,oz,cw'
             write(6,'(A)') 'but some have not been found. Aborting ... '
          endif
          goto 100
       endif

       open(lunit,file=trim(adjustl(filename)),form='unformatted',iostat=iret)
       if ( iret /= 0 ) goto 100
       read(lunit,iostat=ierr) scr2; ps   = scr2; iret = ierr
       read(lunit,iostat=ierr) scr3; u    = scr3; iret = ierr + iret
       read(lunit,iostat=ierr) scr3; v    = scr3; iret = ierr + iret
       read(lunit,iostat=ierr) scr3; tv   = scr3; iret = ierr + iret
       read(lunit,iostat=ierr) scr3; q    = scr3; iret = ierr + iret
       read(lunit,iostat=ierr) scr3; oz   = scr3; iret = ierr + iret
       read(lunit,iostat=ierr) scr3; cwmr = scr3; iret = ierr + iret
       close(lunit)
       if ( iret /= 0 ) goto 100

       call gsi_bundleputvar(atm_bundle,'ps',ps,  ierr); iret = ierr
       call gsi_bundleputvar(atm_bundle,'sf',u ,  ierr); iret = ierr + iret
       call gsi_bundleputvar(atm_bundle,'vp',v ,  ierr); iret = ierr + iret
       call gsi_bundleputvar(atm_bundle,'t' ,tv,  ierr); iret = ierr + iret
       call gsi_bundleputvar(atm_bundle,'q' ,q ,  ierr); iret = ierr + iret
       call gsi_bundleputvar(atm_bundle,'oz',oz,  ierr); iret = ierr + iret
       call gsi_bundleputvar(atm_bundle,'cw',cwmr,ierr); iret = ierr + iret

       if ( iret /= 0 ) then
          if ( mype == 0 ) then
             write(6,'(A)') 'get_user_ens_: ERROR!'
             write(6,'(A)') 'For now, GFS needs to put all MetFields: ps,u,v,(sf,vp)tv,q,oz,cw'
             write(6,'(A)') 'but some have not been found. Aborting ... '
          endif
          goto 100
       endif

       if ( allocated(scr2) ) deallocate(scr2)
       if ( allocated(scr3) ) deallocate(scr3)

    else ! if ( enspreproc )

       if ( regional ) then
          zflag = .true.
       else
          zflag = .false.
       endif

       ! if member == 0, read ensemble mean
       if ( member == 0 ) then
          write(filename,12) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex)
       else
          write(filename,22) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),member
       endif
12     format(a,'sigf',i2.2,'_ensmean'     )
22     format(a,'sigf',i2.2,'_ens_mem',i3.3)

       if ( use_gfs_nemsio ) then
          call general_read_gfsatm_nems(grd,sp_ens,filename,uv_hyb_ens,.false., &
               zflag,atm_bundle,.true.,iret)
       else
          call general_read_gfsatm(grd,sp_ens,sp_ens,filename,uv_hyb_ens,.false., &
               zflag,atm_bundle,inithead,iret)
       endif

       inithead = .false.

    endif ! if ( enspreproc )

100 continue

    if ( iret /= 0 ) then
       if ( mype == 0 ) then
          write(6,'(A)') 'get_user_ens_: WARNING!'
          write(6,'(3A,I5)') 'Trouble reading ensemble file : ', trim(filename), ', IRET = ', iret
       endif
    endif

    return

end subroutine get_user_ens_

subroutine put_gsi_ens_(grd,member,ntindex,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    put_gsi_ens_    write out an internally gen ens to file
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Write out GSI ensemble to file.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!
!   input argument list:
!     grd      - grd info for ensemble
!     member   - index for ensemble member
!     ntindex  - time index for ensemble
!   atm_bundle - bundle of ensemble perturbations
!
!   output argument list:
!     iret      - return code, 0 for successful write
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: i_kind
    use mpimod, only: mype
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_bundlemod, only: gsi_bundle
    use gsi_4dvar, only: ens_fhrlevs
    use hybrid_ensemble_parameters, only: ensemble_path
    use hybrid_ensemble_parameters, only: sp_ens
    use gridmod, only: use_gfs_nemsio

    implicit none

    ! Declare passed variables
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname='put_gsi_ens_'
    character(len=70) :: filename
    integer(i_kind) :: mype_atm
    logical,save :: inithead = .true.

    mype_atm = member

    write(filename,13) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),member
13  format(a,'sigf',i2.2,'_ens_pert',i3.3)

    if ( use_gfs_nemsio ) then
       if ( mype == 0 ) then
          write(6,*) 'write_nemsatm is not adapted to write out perturbations yet'
          iret = 999
       endif
       !call write_nemsatm(grd,...)
    else
       call general_write_gfsatm(grd,sp_ens,sp_ens,filename,mype_atm, &
            atm_bundle,ntindex,inithead,iret)
    endif

    inithead = .false.

    if ( iret /= 0 ) then
       if ( mype == mype_atm ) then
          write(6,'(A)') 'put_gsi_ens_: WARNING!'
          write(6,'(3A,I5)') 'Trouble writing ensemble perturbation to file : ', trim(filename), ', IRET = ', iret
       endif
    endif

    return

end subroutine put_gsi_ens_

subroutine non_gaussian_ens_grid_(elats,elons)

    use kinds, only: r_kind
    use hybrid_ensemble_parameters, only: sp_ens

    implicit none

    ! Declare passed variables
    real(r_kind), intent(out) :: elats(size(sp_ens%rlats)),elons(size(sp_ens%rlons))

    elats=sp_ens%rlats
    elons=sp_ens%rlons

    return

end subroutine non_gaussian_ens_grid_

