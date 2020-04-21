module get_gfs_ensmod_mod

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gfs_ensmod_mod    handles gfs ensemble 
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Handle GFS ensemble (full fields and perturbations)
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2019-07-09  todling  - revised abstract layer
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use mpeu_util, only: die
    use mpimod, only: mype,npe
    use abstract_ensmod, only: this_ens_class => abstractEnsemble

    implicit none
    private
    public :: ensemble
    public :: ensemble_typemold

    type, extends(this_ens_class) :: ensemble
      private
      contains
      procedure :: get_user_ens => get_gfs_ens
      procedure :: get_user_Nens => get_gfs_Nens
      procedure :: put_user_ens => put_gfs_ens
      procedure :: non_gaussian_ens_grid => non_gaussian_ens_grid_gfs
      procedure, nopass:: mytype => typename
      procedure, nopass:: create_sub2grid_info
      procedure, nopass:: destroy_sub2grid_info
    end type ensemble

    character(len=*),parameter:: myname="gfs_ensmod"

    type(ensemble),target:: mold_

contains

function ensemble_typemold()
  implicit none
  type(ensemble),pointer:: ensemble_typemold
  ensemble_typemold => mold_
end function ensemble_typemold

function typename()
  implicit none
  character(len=:),allocatable:: typename
  typename='['//myname//'::ensemble]'
end function typename

subroutine get_gfs_Nens(this,grd,members,ntindex,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gfs_Nens    pretend atmos bkg is the ensemble
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Read in GFS ensemble members in to GSI ensemble.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2016-07-20  mpotts   - refactored into class/module
!   2019-07-09  todling  - revised in light of truly abstract layer
!   2019-09-24  martin   - added in support for gfs netCDF IO
!
!   input argument list:
!     grd      - grd info for ensemble
!     members  - number of ensemble members (size of bundle)
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

    use kinds, only: i_kind,r_kind,r_single
    use gridmod, only: use_gfs_nemsio, use_gfs_ncio
    use general_sub2grid_mod, only: sub2grid_info
    use hybrid_ensemble_parameters, only: ens_fast_read
    use hybrid_ensemble_parameters, only: grd_ens
    use gsi_bundlemod, only: gsi_bundle
    use control_vectors, only: nc2d,nc3d

    implicit none

    ! Declare passed variables
    class(ensemble),     intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: members
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle(:)
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname_='get_user_ens_gfs'
    real(r_single),allocatable,dimension(:,:,:,:) :: en_loc3
    integer(i_kind) :: m_cvars2d(nc2d),m_cvars3d(nc3d)

    integer(i_kind) :: n
    real(r_kind),allocatable,dimension(:) :: clons,slons

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate

    if ( (use_gfs_nemsio .or. use_gfs_ncio) .and. ens_fast_read ) then
       allocate(en_loc3(grd_ens%lat2,grd_ens%lon2,nc2d+nc3d*grd_ens%nsig,members))
       allocate(clons(grd_ens%nlon),slons(grd_ens%nlon))
       call get_user_ens_gfs_fastread_(ntindex,en_loc3,m_cvars2d,m_cvars3d, &
                         grd_ens%lat2,grd_ens%lon2,grd_ens%nsig, &
                         nc2d,nc3d,members,iret,clons,slons)
       do n=1,members
          call move2bundle_(grd,en_loc3(:,:,:,n),atm_bundle(n), &
                            m_cvars2d,m_cvars3d,iret,clons,slons)
       end do
       deallocate(en_loc3,clons,slons)
    else
       do n = 1,members
          call get_gfs_ens(this,grd,n,ntindex,atm_bundle(n),iret)
       end do
    endif

    return

end subroutine get_gfs_Nens

subroutine get_user_ens_gfs_fastread_(ntindex,en_loc3,m_cvars2d,m_cvars3d, &
                                lat2in,lon2in,nsigin,nc2din,nc3din,n_ensin,iret,clons,slons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_user_ens_gfs_fastread_
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Read in GFS ensemble members in to GSI ensemble.  This is the
!           version which reads all ensemble members simultaneously in
!           parallel to n_ens processors.  This is followed by a scatter
!           to subdomains on all processors.  This version will only work
!           if n_ens <= npe, where npe is the total number of processors
!           available.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      NOTE:  In this version, just copy pole row values to halo rows beyond
!      pole.  Verify that that is what is done in current GSI.  If so, then
!      postpone proper values for halo points beyond poles.  Main goal here is
!      to get bit-wise identical results between fast ensemble read and current
!      ensemble read.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2016-10-11  parrish  - create fast parallel code
!   2019-07-10  zhu      - read convective clouds
!   2019-09-24  martin   - add in support for use_gfs_ncio
!
!   input argument list:
!     ntindex  - time index for ensemble
!     ens_atm_bundle - atm bundle w/ fields for ensemble
!
!   output argument list:
!     ens_atm_bundle - atm bundle w/ fields for ensemble
!     iret           - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use mpimod, only: mpi_comm_world,ierror,mpi_real8,mpi_integer4,mpi_max
    use kinds, only: i_kind,r_single,r_kind
    use constants, only: zero
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_4dvar, only: ens_fhrlevs
    use hybrid_ensemble_parameters, only: n_ens,grd_ens
    use hybrid_ensemble_parameters, only: ensemble_path
    use control_vectors, only: nc2d,nc3d
    !use control_vectors, only: cvars2d,cvars3d
    use genex_mod, only: genex_info,genex_create_info,genex,genex_destroy_info
    use gridmod, only: use_gfs_nemsio, use_gfs_ncio
    use jfunc, only: cnvw_option

    implicit none

    ! Declare passed variables
    integer(i_kind),     intent(in   ) :: ntindex
    real(r_single),      intent(inout) :: en_loc3(lat2in,lon2in,nc2din+nc3din*nsigin,n_ensin)
    integer(i_kind),     intent(inout) :: m_cvars2d(nc2din),m_cvars3d(nc3din)
    integer(i_kind),     intent(in   ) :: lat2in,lon2in,nsigin,nc2din,nc3din,n_ensin
    integer(i_kind),     intent(  out) :: iret
    real(r_kind),        intent(inout) :: clons(grd_ens%nlon),slons(grd_ens%nlon)


    ! Declare internal variables
    character(len=*),parameter :: myname_='get_user_ens_gfs_fastread_'
    character(len=70) :: filename
    character(len=70) :: filenamesfc
    integer(i_kind) :: i,ii,j,jj,k,n
    integer(i_kind) :: io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,i_ens
    integer(i_kind) :: ip,ips,ipe,jps,jpe
    integer(i_kind) :: ias,iae,iasm,iaem,iaemz,jas,jae,jasm,jaem,jaemz
    integer(i_kind) :: kas,kae,kasm,kaem,kaemz,mas,mae,masm,maem,maemz
    integer(i_kind) :: ibs,ibe,ibsm,ibem,ibemz,jbs,jbe,jbsm,jbem,jbemz
    integer(i_kind) :: kbs,kbe,kbsm,kbem,kbemz,mbs,mbe,mbsm,mbem,mbemz
    integer(i_kind) :: n2d
    integer(i_kind) :: nlon,nlat,nsig
    type(genex_info) :: s_a2b
    real(r_single),allocatable,dimension(:,:,:,:) :: en_full,en_loc
    integer(i_kind),allocatable,dimension(:) :: m_cvars2dw,m_cvars3dw
    integer(i_kind) base_pe,base_pe0

    iret = 0

    nlat=grd_ens%nlat
    nlon=grd_ens%nlon
    nsig=grd_ens%nsig

    ! write out contents of cvars2d, cvars3d

    !if (mype == 0 ) then
    !    write(6,*) ' in get_user_ens_fastread_,cvars2d=',(trim(cvars2d(i)),i=1,2)
    !    write(6,*) ' in get_user_ens_fastread_,cvars3d=',(trim(cvars3d(i)),i=1,6)
    !endif

    !  set up partition of available processors for parallel read
    if ( n_ens > npe ) &
        call die(myname_, ': ***ERROR*** CANNOT READ ENSEMBLE  n_ens > npe, increase npe >= n_ens', 99)

    call ens_io_partition_(n_ens,io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,i_ens)

    ! setup communicator for scatter to subdomains:

    ! first, define gsi subdomain boundaries in global units:

    ip=1   !  halo width is hardwired at 1
    ips=grd_ens%istart(mype+1)
    ipe=ips+grd_ens%lat1-1
    jps=grd_ens%jstart(mype+1)
    jpe=jps+grd_ens%lon1-1


!!!!!!!!!!!!NOTE--FOLLOWING HAS MANY VARS TO BE DEFINED--NLAT,NLON ARE ENSEMBLE DOMAIN DIMS
!!!!!!!!for example,  n2d = nc3d*nsig + nc2d

    n2d=nc3d*grd_ens%nsig+nc2d
    ias=1 ; iae=0 ; jas=1 ; jae=0 ; kas=1 ; kae=0 ; mas=1 ; mae=0
    if(mype==io_pe) then
       ias=1 ; iae=nlat
       jas=1 ; jae=nlon
       kas=1 ; kae=n2d
       mas=n_io_pe_s ; mae=n_io_pe_em
    endif
    iasm=ias ; iaem=iae ; jasm=jas ; jaem=jae ; kasm=kas ; kaem=kae ; masm=mas ; maem=mae

    ibs =ips    ; ibe =ipe    ; jbs =jps    ; jbe =jpe
    ibsm=ibs-ip ; ibem=ibe+ip ; jbsm=jbs-ip ; jbem=jbe+ip
    kbs =1   ; kbe =n2d ; mbs =1   ; mbe =n_ens
    kbsm=kbs ; kbem=kbe ; mbsm=mbs ; mbem=mbe
    iaemz=max(iasm,iaem) ; jaemz=max(jasm,jaem)
    kaemz=max(kasm,kaem) ; maemz=max(masm,maem)
    ibemz=max(ibsm,ibem) ; jbemz=max(jbsm,jbem)
    kbemz=max(kbsm,kbem) ; mbemz=max(mbsm,mbem)
    call genex_create_info(s_a2b,ias ,iae ,jas ,jae ,kas ,kae ,mas ,mae , &
                                 ibs ,ibe ,jbs ,jbe ,kbs ,kbe ,mbs ,mbe , &
                                 iasm,iaem,jasm,jaem,kasm,kaem,masm,maem, &
                                 ibsm,ibem,jbsm,jbem,kbsm,kbem,mbsm,mbem)

!!  read ensembles

    allocate(en_full(iasm:iaemz,jasm:jaemz,kasm:kaemz,masm:maemz))

    write(filename,22) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),mas
22  format(a,'sigf',i2.2,'_ens_mem',i3.3)

    if (cnvw_option) then
       write(filenamesfc,23) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),mas
23     format(a,'sfcf',i2.2,'_ens_mem',i3.3)
    end if

    allocate(m_cvars2dw(nc2din),m_cvars3dw(nc3din))
    m_cvars2dw=-999
    m_cvars3dw=-999

    if ( mas == mae ) then
       if ( use_gfs_nemsio ) then
          if (cnvw_option) then
             call parallel_read_nemsio_state_(en_full,m_cvars2dw,m_cvars3dw,nlon,nlat,nsig, &
                                            ias,jas,mas, &
                                            iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                            filename,.true.,clons,slons,filenamesfc)
          else
             call parallel_read_nemsio_state_(en_full,m_cvars2dw,m_cvars3dw,nlon,nlat,nsig, &
                                            ias,jas,mas, &
                                            iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                            filename,.true.,clons,slons)
          end if
       else
           call parallel_read_gfsnc_state_(en_full,m_cvars2dw,m_cvars3dw,nlon,nlat,nsig, &
                                         ias,jas,mas, &
                                         iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                         filename,.true.,clons,slons)
       end if
    end if
    base_pe0=-999
    if ( mas == 1 .and. mae == 1 ) base_pe0=mype

    call mpi_allreduce(base_pe0,base_pe,1,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    call mpi_bcast(clons,grd_ens%nlon,mpi_real8,base_pe,mpi_comm_world,ierror)
    call mpi_bcast(slons,grd_ens%nlon,mpi_real8,base_pe,mpi_comm_world,ierror)

    call mpi_allreduce(m_cvars2dw,m_cvars2d,nc2d,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    call mpi_allreduce(m_cvars3dw,m_cvars3d,nc3d,mpi_integer4,mpi_max,mpi_comm_world,ierror)

! scatter to subdomains:

    allocate(en_loc(ibsm:ibemz,jbsm:jbemz,kbsm:kbemz,mbsm:mbemz))

    en_loc=zero
    call genex(s_a2b,en_full,en_loc)

    deallocate(en_full)
    call genex_destroy_info(s_a2b)  ! check on actual routine name

! transfer en_loc to en_loc3

! Look to thread here OMP
    do n=1,n_ens
       do k=1,nc2d+nc3d*nsig
          jj=0
          do j=jbsm,jbem
             jj=jj+1
             ii=0
             do i=ibsm,ibem
                ii=ii+1
                en_loc3(ii,jj,k,n)=en_loc(i,j,k,n)
             enddo
          enddo
       enddo
    enddo

end subroutine get_user_ens_gfs_fastread_

subroutine move2bundle_(grd,en_loc3,atm_bundle,m_cvars2d,m_cvars3d,iret,clons,slons)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    move2bundle  transfer 1 ensemble member to bundle
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: transfer one ensemble member to bundle
!
! program history log:
!   2016-06-30  parrish -- copy and adapt get_user_ens_member_ to transfer 1
!                            ensemble member
!   2019-03-13  eliu    -- add precipitation components 
!
!   input argument list:
!     grd        - grd info for ensemble
!     en_loc3    - ensemble member
!     atm_bundle - empty atm bundle
!     m_cvars2d  - maps 3rd index in en_loc3 for start of each 2d variable
!     m_cvars3d  - maps 3rd index in en_loc3 for start of each 3d variable
!
!   output argument list:
!     atm_bundle - atm bundle w/ fields for ensemble member
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: i_kind,r_kind,r_single
    use constants, only: zero,one,two,fv
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_destroy_info
    use hybrid_ensemble_parameters, only: en_perts
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer,gsi_bundleputvar
    use gsi_bundlemod, only : assignment(=)
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
    use mpeu_util, only: getindex  

    implicit none

    ! Declare passed variables
    type(sub2grid_info), intent(in   ) :: grd
    real(r_single),      intent(in   ) :: en_loc3(grd%lat2,grd%lon2,nc2d+nc3d*grd%nsig)
    type(gsi_bundle),    intent(inout) :: atm_bundle
    integer(i_kind),     intent(in   ) :: m_cvars2d(nc2d),m_cvars3d(nc3d)
    integer(i_kind),     intent(  out) :: iret
    real(r_kind),        intent(in   ) :: clons(grd%nlon),slons(grd%nlon)

    ! Declare internal variables
    character(len=*),parameter :: myname_='move2bundle_'
    character(len=70) :: filename

    integer(i_kind) :: ierr
    integer(i_kind) :: im,jm,km,m,k
    integer(i_kind) :: icw,iql,iqi,iqr,iqs,iqg  
    real(r_kind),pointer,dimension(:,:) :: ps
    !real(r_kind),pointer,dimension(:,:) :: sst
    real(r_kind),pointer,dimension(:,:,:) :: u,v,tv,q,oz,cwmr
    real(r_kind),pointer,dimension(:,:,:) :: qlmr,qimr,qrmr,qsmr,qgmr   
    real(r_single),allocatable,dimension(:,:)  :: scr2
    real(r_single),allocatable,dimension(:,:,:) :: scr3
    type(sub2grid_info) :: grd2d,grd3d
    real(r_kind),parameter :: r0_001 = 0.001_r_kind

    im = en_perts(1,1)%grid%im
    jm = en_perts(1,1)%grid%jm
    km = en_perts(1,1)%grid%km

    allocate(scr2(im,jm))
    allocate(scr3(im,jm,km))

    ! Check hydrometeors in control variables 
    icw=getindex(cvars3d,'cw')
    iql=getindex(cvars3d,'ql')
    iqi=getindex(cvars3d,'qi')
    iqr=getindex(cvars3d,'qr')
    iqs=getindex(cvars3d,'qs')
    iqg=getindex(cvars3d,'qg')

!   initialize atm_bundle to zero

    atm_bundle=zero

    call gsi_bundlegetpointer(atm_bundle,'ps',ps,  ierr); iret = ierr
    !call gsi_bundlegetpointer(atm_bundle,'sst',sst, ierr); iret = ierr
    call gsi_bundlegetpointer(atm_bundle,'sf',u ,  ierr); iret = ierr + iret
    call gsi_bundlegetpointer(atm_bundle,'vp',v ,  ierr); iret = ierr + iret
    call gsi_bundlegetpointer(atm_bundle,'t' ,tv,  ierr); iret = ierr + iret
    call gsi_bundlegetpointer(atm_bundle,'q' ,q ,  ierr); iret = ierr + iret
    call gsi_bundlegetpointer(atm_bundle,'oz',oz,  ierr); iret = ierr + iret
    if (icw>0) call gsi_bundlegetpointer(atm_bundle,'cw',cwmr,ierr); iret = ierr + iret
    if (iql>0) call gsi_bundlegetpointer(atm_bundle,'ql',qlmr,ierr); iret = ierr + iret
    if (iqi>0) call gsi_bundlegetpointer(atm_bundle,'qi',qimr,ierr); iret = ierr + iret
    if (iqr>0) call gsi_bundlegetpointer(atm_bundle,'qr',qrmr,ierr); iret = ierr + iret
    if (iqs>0) call gsi_bundlegetpointer(atm_bundle,'qs',qsmr,ierr); iret = ierr + iret
    if (iqg>0) call gsi_bundlegetpointer(atm_bundle,'qg',qgmr,ierr); iret = ierr + iret
    if ( iret /= 0 ) then
       if ( mype == 0 ) then
          write(6,'(A)') trim(myname_) // ': ERROR!'
          write(6,'(A)') trim(myname_) // ': For now, GFS requires all MetFields: ps,u,v,(sf,vp)tv,q,oz,cw'
          write(6,'(A)') trim(myname_) // ': but some have not been found. Aborting ... '
          write(6,'(A)') trim(myname_) // ': WARNING!'
          write(6,'(3A,I5)') trim(myname_) // ': Trouble reading ensemble file : ', trim(filename), ', IRET = ', iret
       endif
       return
    endif

    do m=1,nc2d
       scr2(:,:)=en_loc3(:,:,m_cvars2d(m))
       if(trim(cvars2d(m))=='ps') ps=scr2
    !  if(trim(cvars2d(m))=='sst') sst=scr2    !  no sst for now
    enddo
    do m=1,nc3d
       do k=1,km
          scr3(:,:,k)=en_loc3(:,:,m_cvars3d(m)+k-1)
       enddo
       if(trim(cvars3d(m))=='sf')  u    = scr3
       if(trim(cvars3d(m))=='vp')  v    = scr3
       if(trim(cvars3d(m))=='t')   tv   = scr3
       if(trim(cvars3d(m))=='q')   q    = scr3
       if(trim(cvars3d(m))=='oz')  oz   = scr3
       if(trim(cvars3d(m))=='cw')  cwmr = scr3
       if(trim(cvars3d(m))=='ql')  qlmr = scr3
       if(trim(cvars3d(m))=='qi')  qimr = scr3
       if(trim(cvars3d(m))=='qr')  qrmr = scr3
       if(trim(cvars3d(m))=='qs')  qsmr = scr3
       if(trim(cvars3d(m))=='qg')  qgmr = scr3
    enddo

!   convert ps from Pa to cb
    ps=r0_001*ps
!   convert t to virtual temperature
    tv=tv*(one+fv*q)

!--- now update pole values of atm_bundle using general_sub2grid (so halos also
!       automatically updated.

    call create_grd23d_(grd2d,1)
    call create_grd23d_(grd3d,grd%nsig)

    call update_scalar_poles_(grd2d,ps)
    call update_vector_poles_(grd3d,u,v,clons,slons)
    call update_scalar_poles_(grd3d,tv)
    call update_scalar_poles_(grd3d,q)
    call update_scalar_poles_(grd3d,oz)
    if (icw>0) call update_scalar_poles_(grd3d,cwmr)
    if (iql>0) call update_scalar_poles_(grd3d,qlmr)
    if (iqi>0) call update_scalar_poles_(grd3d,qimr)
    if (iqr>0) call update_scalar_poles_(grd3d,qrmr)
    if (iqs>0) call update_scalar_poles_(grd3d,qsmr)
    if (iqg>0) call update_scalar_poles_(grd3d,qgmr)

    call gsi_bundleputvar(atm_bundle,'ps',ps,  ierr); iret = ierr
    !call gsi_bundleputvar(atm_bundle,'sst',sst,ierr); iret = ierr + iret  ! no sst for now
    call gsi_bundleputvar(atm_bundle,'sf',u ,  ierr); iret = ierr + iret
    call gsi_bundleputvar(atm_bundle,'vp',v ,  ierr); iret = ierr + iret
    call gsi_bundleputvar(atm_bundle,'t' ,tv,  ierr); iret = ierr + iret
    call gsi_bundleputvar(atm_bundle,'q' ,q ,  ierr); iret = ierr + iret
    call gsi_bundleputvar(atm_bundle,'oz',oz,  ierr); iret = ierr + iret
    if (icw>0) call gsi_bundleputvar(atm_bundle,'cw',cwmr,ierr); iret = ierr + iret
    if (iql>0) call gsi_bundleputvar(atm_bundle,'ql',qlmr,ierr); iret = ierr + iret
    if (iqi>0) call gsi_bundleputvar(atm_bundle,'qi',qimr,ierr); iret = ierr + iret
    if (iqr>0) call gsi_bundleputvar(atm_bundle,'qr',qrmr,ierr); iret = ierr + iret
    if (iqs>0) call gsi_bundleputvar(atm_bundle,'qs',qsmr,ierr); iret = ierr + iret
    if (iqg>0) call gsi_bundleputvar(atm_bundle,'qg',qgmr,ierr); iret = ierr + iret
    if ( iret /= 0 ) then
       if ( mype == 0 ) then
          write(6,'(A)') trim(myname_) // ': ERROR!'
          write(6,'(A)') trim(myname_) // ': For now, GFS needs to put all MetFields: ps,u,v,(sf,vp)tv,q,oz,cw'
          write(6,'(A)') trim(myname_) // ': but some have not been found. Aborting ... '
          write(6,'(A)') trim(myname_) // ': WARNING!'
          write(6,'(3A,I5)') trim(myname_) // ': Trouble reading ensemble file : ', trim(filename), ', IRET = ', iret
       endif
       return
    endif

    call general_sub2grid_destroy_info(grd2d,grd)
    call general_sub2grid_destroy_info(grd3d,grd)

    if ( allocated(scr2) ) deallocate(scr2)
    if ( allocated(scr3) ) deallocate(scr3)

    return

end subroutine move2bundle_

subroutine create_grd23d_(grd23d,nvert)

    use kinds, only: i_kind
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
    use hybrid_ensemble_parameters, only: grd_ens

    implicit none

    ! Declare local parameters

    ! Declare passed variables
    type(sub2grid_info), intent(inout) :: grd23d
    integer(i_kind),     intent(in   ) :: nvert

    ! Declare local variables
    integer(i_kind) :: inner_vars = 1
    logical :: regional = .false.

    call general_sub2grid_create_info(grd23d,inner_vars,grd_ens%nlat,grd_ens%nlon, &
                                      nvert,nvert,regional,s_ref=grd_ens)

end subroutine create_grd23d_

subroutine update_scalar_poles_(grd,s)

    use kinds, only: i_kind,r_kind
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid,general_grid2sub

    implicit none

    ! Declare passed variables
    type(sub2grid_info), intent(in   ) :: grd
    real(r_kind),        intent(inout) :: s(grd%lat2,grd%lon2,grd%num_fields)

    ! Declare local variables
    integer(i_kind) inner_vars,lat2,lon2,nlat,nlon,nvert,kbegin_loc,kend_loc,kend_alloc
    integer(i_kind) ii,i,j,k
    real(r_kind),allocatable,dimension(:) :: sloc
    real(r_kind),allocatable,dimension(:,:,:,:) :: work

    lat2=grd%lat2
    lon2=grd%lon2
    nlat=grd%nlat
    nlon=grd%nlon
    nvert=grd%num_fields
    inner_vars=grd%inner_vars
    kbegin_loc=grd%kbegin_loc
    kend_loc=grd%kend_loc
    kend_alloc=grd%kend_alloc
    allocate(sloc(lat2*lon2*nvert))
    allocate(work(inner_vars,nlat,nlon,kbegin_loc:kend_alloc))
    ii=0
    do k=1,nvert
       do j=1,lon2
          do i=1,lat2
             ii=ii+1
             sloc(ii)=s(i,j,k)
          enddo
       enddo
    enddo
    call general_sub2grid(grd,sloc,work)

    do k=kbegin_loc,kend_loc
       call fillpoles_s_(work(1,:,:,k),nlon,nlat)
    enddo
    call general_grid2sub(grd,work,sloc)
    ii=0
    do k=1,nvert
       do j=1,lon2
          do i=1,lat2
             ii=ii+1
             s(i,j,k)=sloc(ii)
          enddo
       enddo
    enddo

    deallocate(sloc,work)

end subroutine update_scalar_poles_

subroutine update_vector_poles_(grd,u,v,clons,slons)

   use kinds, only: i_kind,r_kind
   use constants, only: zero
   use general_sub2grid_mod, only: sub2grid_info,general_sub2grid,general_grid2sub

   implicit none

   ! Declare local parameters

   ! Declare passed variables
   type(sub2grid_info)               ,intent(in   ) :: grd
   real(r_kind)                      ,intent(inout) :: u(grd%lat2,grd%lon2,grd%num_fields)
   real(r_kind)                      ,intent(inout) :: v(grd%lat2,grd%lon2,grd%num_fields)
   real(r_kind)                      ,intent(in   ) :: clons(grd%nlon),slons(grd%nlon)

   ! Declare local variables
   integer(i_kind) inner_vars,lat2,lon2,nlat,nlon,nvert,kbegin_loc,kend_loc,kend_alloc
   integer(i_kind) ii,i,j,k
   real(r_kind),allocatable,dimension(:) :: uloc,vloc
   real(r_kind),allocatable,dimension(:,:,:,:) :: uwork,vwork
   real(r_kind),allocatable,dimension(:,:) :: tempu,tempv

   lat2=grd%lat2
   lon2=grd%lon2
   nlat=grd%nlat
   nlon=grd%nlon
   nvert=grd%num_fields
   inner_vars=grd%inner_vars
   kbegin_loc=grd%kbegin_loc
   kend_loc=grd%kend_loc
   kend_alloc=grd%kend_alloc
   allocate(uloc(lat2*lon2*nvert))
   allocate(vloc(lat2*lon2*nvert))
   allocate(uwork(inner_vars,nlat,nlon,kbegin_loc:kend_alloc))
   allocate(vwork(inner_vars,nlat,nlon,kbegin_loc:kend_alloc))
   allocate(tempu(nlat,nlon),tempv(nlat,nlon))
   uwork=zero ; vwork=zero ; uloc=zero ; vloc=zero
   ii=0
   do k=1,nvert
      do j=1,lon2
         do i=1,lat2
            ii=ii+1
            uloc(ii)=u(i,j,k)
            vloc(ii)=v(i,j,k)
         enddo
      enddo
   enddo
   call general_sub2grid(grd,uloc,uwork)
   call general_sub2grid(grd,vloc,vwork)

   do k=kbegin_loc,kend_loc
      do j=1,nlon
         do i=1,nlat
            tempu(i,j)=uwork(1,i,j,k)
            tempv(i,j)=vwork(1,i,j,k)
         enddo
      enddo
      call fillpoles_v_(tempu,tempv,nlon,nlat,clons,slons)
      do j=1,nlon
         do i=1,nlat
            uwork(1,i,j,k)=tempu(i,j)
            vwork(1,i,j,k)=tempv(i,j)
         enddo
      enddo
   enddo
   call general_grid2sub(grd,uwork,uloc)
   call general_grid2sub(grd,vwork,vloc)
   ii=0
   do k=1,nvert
      do j=1,lon2
         do i=1,lat2
            ii=ii+1
            u(i,j,k)=uloc(ii)
            v(i,j,k)=vloc(ii)
         enddo
      enddo
   enddo

   deallocate(uloc,uwork,tempu)
   deallocate(vloc,vwork,tempv)

end subroutine update_vector_poles_

subroutine ens_io_partition_(n_ens,io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,i_ens)

!     do computation on all processors, then assign final local processor
!     values.

      use kinds, only: r_kind,i_kind
      use constants, only: half

      implicit none

!     Declare passed variables
      integer(i_kind),intent(in   ) :: n_ens
      integer(i_kind),intent(  out) :: io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,i_ens

!     Declare local variables
      integer(i_kind) :: io_pe0(n_ens)
      integer(i_kind) :: iskip,jskip,nextra,ipe,n
      integer(i_kind) :: nsig

      i_ens=-1
      nsig=1
      iskip=npe/n_ens
      nextra=npe-iskip*n_ens
      jskip=iskip
      io_pe=-1
      io_pe0=-1
      n_io_pe_s=1
      n_io_pe_e=0

      ipe=0
      do n=1,n_ens
         io_pe0(n)=ipe
         if(n <= nextra) then
            jskip=iskip+1
         else
            jskip=iskip
         endif
         ipe=ipe+jskip
      enddo
      do n=1,n_ens
         if(mype==0) write(6,'(2(a,1x,i5,1x))') 'reading ensemble member', n,  'on pe', io_pe0(n)
      enddo

      do n=1,n_ens
         if(mype==io_pe0(n)) then
            i_ens=n
            io_pe=mype
            n_io_pe_s=(n-1)*nsig+1
            n_io_pe_e=n*nsig
         endif
      enddo
      n_io_pe_em=max(n_io_pe_s,n_io_pe_e)

end subroutine ens_io_partition_

subroutine parallel_read_nemsio_state_(en_full,m_cvars2d,m_cvars3d,nlon,nlat,nsig, &
                                        ias,jas,mas, &
                                        iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                        filename,init_head,clons,slons,filenamesfc)

   use kinds, only: i_kind,r_kind,r_single
   use constants, only: r60,r3600,zero,one,half,pi,deg2rad
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
   use ncepnems_io, only: error_msg,imp_physics
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
   use nemsio_module, only: nemsio_getrechead
   use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
   use general_sub2grid_mod, only: sub2grid_info
   use jfunc, only: cnvw_option

   implicit none

   ! Declare local parameters

   ! Declare passed variables
   integer(i_kind),  intent(in   ) :: nlon,nlat,nsig
   integer(i_kind),  intent(in   ) :: ias,jas,mas
   integer(i_kind),  intent(in   ) :: iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz
   integer(i_kind),  intent(inout) :: m_cvars2d(nc2d),m_cvars3d(nc3d)
   real(r_single),   intent(inout) :: en_full(iasm:iaemz,jasm:jaemz,kasm:kaemz,masm:maemz)
   character(len=*), intent(in   ) :: filename
   character(len=*), optional, intent(in) :: filenamesfc
   logical,          intent(in   ) :: init_head
   real(r_kind),     intent(inout) :: clons(nlon),slons(nlon)

   ! Declare local variables
   integer(i_kind) i,ii,j,jj,k,lonb,latb,levs,latb2,lonb2
   integer(i_kind) k2,k3,k3u,k3v,k3t,k3q,k3cw,k3oz,kf
   integer(i_kind) k3ql,k3qi,k3qr,k3qs,k3qg       
   integer(i_kind) iret
   integer(i_kind) :: istop = 101
   integer(i_kind),dimension(7):: idate
   integer(i_kind),dimension(4):: odate
   integer(i_kind) nframe,nfhour,nfminute,nfsecondn,nfsecondd
   integer(i_kind) nrec
   character(len=120) :: myname_ = 'parallel_read_nemsio_state_'
   character(len=1)   :: null = ' '
   real(r_single),allocatable,dimension(:) :: work,work2
! NOTE:  inportant to keep 8 byte precision for work array, even though what is
! on ensemble NEMS file is 4 byte precision.  The NEMSIO automatically (through
! interfaces presumably) must be able to read 4 byte and 8 byte records and pass
! them on to 4 or 8 byte, whatever is the users choice.  However, since some
! initial arithmetic is done before storing the ensembles as 4 byte, in order to
! preserve bit wise reproducibility between current and new ensemble read
   real(r_single),allocatable,dimension(:,:,:) ::  temp2
   real(r_single),allocatable,dimension(:,:,:,:) ::  temp3
   real(r_kind) :: fhour
   type(nemsio_gfile) :: gfile
   type(nemsio_gfile) :: gfilesfc
   real(r_kind),allocatable,dimension(:) :: rlats,rlons
   real(r_single),allocatable,dimension(:) :: r4lats,r4lons

   if ( init_head)call nemsio_init(iret=iret)
   if (iret /= 0) call error_msg(trim(myname_),trim(filename),null,'init',istop,iret,.true.)

   call nemsio_open(gfile,filename,'READ',iret=iret)
   if (iret /= 0) call error_msg(trim(myname_),trim(filename),null,'open',istop+1,iret,.true.)

   call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
        nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
        idate=idate, dimx=lonb, dimy=latb,dimz=levs,nrec=nrec)

   if (  nframe /= 0 ) &
      call die(myname_, ': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe)

!  check nlat, nlon against latb, lonb

   if ( nlat /= latb+2 .or. nlon /= lonb ) then
      if ( mype == 0 ) &
         write(6,*)trim(myname_),': ***ERROR*** incorrect resolution, nlat,nlon=',nlat,nlon, &
                               ', latb+2,lonb=',latb+2,lonb
      call die(myname_, ': ***ERROR*** incorrect resolution',101)
   endif

   if (cnvw_option) then
      call nemsio_open(gfilesfc,filenamesfc,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(myname_),trim(filenamesfc),null,'open',istop+2,iret,.true.)

      call nemsio_getfilehead(gfilesfc,iret=iret,dimx=lonb2, dimy=latb2)
      if (iret == 0) then
         if ( latb2+2 /= nlat .or. lonb2 /=nlon) then
            if ( mype == 0 ) &
               write(6,*)trim(myname_),': ***ERROR*** incorrect resolution, nlat,nlon=',nlat,nlon, &
                               ', latb2+2,lonb2=',latb2+2,lonb2 
            call die(myname_, ': ***ERROR*** incorrect resolution',101)
         endif
      endif
   endif

!  obtain r4lats,r4lons,rlats,rlons,clons,slons exactly as computed in general_read_gfsatm_nems:

   allocate(rlats(latb+2),rlons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
   call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
   call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
   do j=1,latb
      rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
   enddo
   do j=1,lonb
      rlons(j)=deg2rad*r4lons(j)
   enddo
   deallocate(r4lats,r4lons)
   rlats(1)=-half*pi
   rlats(latb+2)=half*pi
   do j=1,lonb
      clons(j)=cos(rlons(j))
      slons(j)=sin(rlons(j))
   enddo

   fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
   odate(1) = idate(4)  !hour
   odate(2) = idate(2)  !month
   odate(3) = idate(3)  !day
   odate(4) = idate(1)  !year

   allocate(work(nlon*(nlat-2)))
   if (imp_physics == 11 .or. imp_physics == 8) allocate(work2(nlon*(nlat-2)))
   allocate(temp3(nlat,nlon,nsig,nc3d))
   allocate(temp2(nlat,nlon,nc2d))
   k3u=0 ; k3v=0 ; k3t=0 ; k3q=0 ; k3cw=0 ; k3oz=0
   k3ql=0; k3qi=0; k3qr=0; k3qs=0; k3qg=0 
   do k3=1,nc3d
      if(cvars3d(k3)=='sf') k3u=k3
      if(cvars3d(k3)=='vp') k3v=k3
      if(cvars3d(k3)=='t') k3t=k3
      if(cvars3d(k3)=='q') k3q=k3
      if(cvars3d(k3)=='cw') k3cw=k3
      if(cvars3d(k3)=='oz') k3oz=k3
      if(cvars3d(k3)=='ql') k3ql=k3
      if(cvars3d(k3)=='qi') k3qi=k3
      if(cvars3d(k3)=='qr') k3qr=k3
      if(cvars3d(k3)=='qs') k3qs=k3
      if(cvars3d(k3)=='qg') k3qg=k3
      do k=1,nsig
         if(trim(cvars3d(k3))=='cw') then
            call nemsio_readrecv(gfile,'clwmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'clwmr','read',istop+6,iret,.true.)
            if (imp_physics == 11 .or. imp_physics == 8) then
               call nemsio_readrecv(gfile,'icmr','mid layer',k,work2,iret=iret)
               if (iret /= 0) then
                  call error_msg(trim(myname_),trim(filename),'icmr','read',istop+7,iret,.true.)
               else
                  work = work + work2
               endif
            endif
            if (cnvw_option) then
               call nemsio_readrecv(gfilesfc,'cnvcldwat','mid layer',k,work2,iret=iret)
               if (iret /= 0) then
                  call error_msg(trim(myname_),trim(filenamesfc),'cnvcldwat','read',istop+11,iret,.true.)
               else
                  work = work + work2
               end if
            end if
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='ql') then
            call nemsio_readrecv(gfile,'clwmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'clwmr','read',istop+8,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qi') then
            call nemsio_readrecv(gfile,'icmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'icmr','read',istop+9,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qr') then
            call nemsio_readrecv(gfile,'rwmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'rwmr','read',istop+10,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qs') then
            call nemsio_readrecv(gfile,'snmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'snmr','read',istop+11,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qg') then
            call nemsio_readrecv(gfile,'grle','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'grle','read',istop+12,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='oz') then
            call nemsio_readrecv(gfile,'o3mr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'o3mr','read',istop+5,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='q') then
            call nemsio_readrecv(gfile,'spfh','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),trim(cvars3d(k3)),'read',istop+4,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='t') then
            call nemsio_readrecv(gfile,'tmp','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'tmp','read',istop+3,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='sf') then
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'ugrd','read',istop+1,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='vp') then
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'vgrd','read',istop+2,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         endif
      enddo
   enddo
!  if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3cw==0.or.k3oz==0) & 
   if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3oz==0) &  
      write(6,'(" WARNING, problem with one of k3-")')

!   convert T to Tv:    postpone this calculation
!  temp3(:,:,:,k3t)=temp3(:,:,:,k3t)*(one+fv*temp3(:,:,:,k3q))

   temp2=zero
   do k2=1,nc2d
     !if(trim(cvars2d(k2))=='sst') then
     !   call nemsio_readrecv(gfile,'hgt','sfc',1,work,iret=iret)
     !   if (iret /= 0) call error_msg(trim(myname_),trim(filename),'pres','read',istop+7,iret,.true.)
     !   call move1_(work,temp2(:,:,k2),nlon,nlat)
     !elseif(trim(cvars2d(k2))=='ps') then
      if(trim(cvars2d(k2))=='ps') then
         call nemsio_readrecv(gfile,'pres','sfc',1,work,iret=iret)
         if (iret /= 0) call error_msg(trim(myname_),trim(filename),'hgt','read',istop+8,iret,.true.)
         !work=r0_001*work  ! convert Pa to cb   !  postpone this calculation
         call move1_(work,temp2(:,:,k2),nlon,nlat)
      endif
   enddo
   deallocate(work)
   if (imp_physics == 11 .or. imp_physics == 8) deallocate(work2)

!  move temp2,temp3 to en_full
   kf=0
   do k3=1,nc3d
      m_cvars3d(k3)=kf+1
      do k=1,nsig
         kf=kf+1
         jj=jas-1
         do j=1,nlon
            jj=jj+1
            ii=ias-1
            do i=1,nlat
               ii=ii+1
               en_full(ii,jj,kf,mas)=temp3(i,j,k,k3)
            enddo
         enddo
      enddo
   enddo
   do k2=1,nc2d
      m_cvars2d(k2)=kf+1
      kf=kf+1
      jj=jas-1
      do j=1,nlon
         jj=jj+1
         ii=ias-1
         do i=1,nlat
            ii=ii+1
            en_full(ii,jj,kf,mas)=temp2(i,j,k2)
         enddo
      enddo
   enddo

   deallocate(temp3)
   deallocate(temp2)

end subroutine parallel_read_nemsio_state_

subroutine parallel_read_gfsnc_state_(en_full,m_cvars2d,m_cvars3d,nlon,nlat,nsig, &
                                        ias,jas,mas, &
                                        iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                        filename,init_head,clons,slons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    parallel_read_gfsnc_state_    read GFS netCDF ensemble member 
!   prgmmr: Martin            org: NCEP/EMC                date: 2019-09-24
!
! program history log:
!   2019-09-24 Martin    Initial version.  Based on sub parallel_read_nemsio_state_ 
!
!$$$

   use kinds, only: i_kind,r_kind,r_single
   use constants, only: r60,r3600,zero,one,half,pi,deg2rad
   use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
   use general_sub2grid_mod, only: sub2grid_info
   use module_fv3gfs_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                           close_dataset, get_dim, read_vardata 

   implicit none

   ! Declare local parameters

   ! Declare passed variables
   integer(i_kind),  intent(in   ) :: nlon,nlat,nsig
   integer(i_kind),  intent(in   ) :: ias,jas,mas
   integer(i_kind),  intent(in   ) :: iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz
   integer(i_kind),  intent(inout) :: m_cvars2d(nc2d),m_cvars3d(nc3d)
   real(r_single),   intent(inout) :: en_full(iasm:iaemz,jasm:jaemz,kasm:kaemz,masm:maemz)
   character(len=*), intent(in   ) :: filename
   logical,          intent(in   ) :: init_head
   real(r_kind),     intent(inout) :: clons(nlon),slons(nlon)

   ! Declare local variables
   integer(i_kind) i,ii,j,jj,k,lonb,latb,levs,kr
   integer(i_kind) k2,k3,k3u,k3v,k3t,k3q,k3cw,k3oz,kf
   integer(i_kind) iret
   integer(i_kind) :: istop = 101
   character(len=120) :: myname_ = 'parallel_read_gfsnc_state_'
   character(len=1)   :: null = ' '
   real(r_single),allocatable,dimension(:,:,:) ::  temp2, rwork3d1, rwork3d2
   real(r_single),allocatable,dimension(:,:) ::  rwork2d
   real(r_single),allocatable,dimension(:,:,:,:) ::  temp3
   real(r_kind),allocatable,dimension(:) :: rlats,rlons
   real(r_kind),allocatable,dimension(:) :: rlats_tmp,rlons_tmp
   type(Dataset) :: atmges
   type(Dimension) :: ncdim


   atmges = open_dataset(filename)
   ! get dimension sizes
   ncdim = get_dim(atmges, 'grid_xt'); lonb = ncdim%len
   ncdim = get_dim(atmges, 'grid_yt'); latb = ncdim%len
   ncdim = get_dim(atmges, 'pfull'); levs = ncdim%len

!  check nlat, nlon against latb, lonb

   if ( nlat /= latb+2 .or. nlon /= lonb ) then
      if ( mype == 0 ) &
         write(6,*)trim(myname_),': ***ERROR*** incorrect resolution, nlat,nlon=',nlat,nlon, &
                               ', latb+2,lonb=',latb+2,lonb
      call die(myname_, ': ***ERROR*** incorrect resolution',101)
   endif

!  obtain rlats_tmp,rlons_tnp,rlats,rlons,clons,slons exactly as computed in general_read_gfsatm_nems:

   allocate(rlats(latb+2),rlons(lonb))
   call read_vardata(atmges, 'grid_xt', rlons_tmp)
   call read_vardata(atmges, 'grid_yt', rlats_tmp)
   do j=1,latb
      rlats(latb+2-j)=deg2rad*rlats_tmp(j)
   enddo
   do j=1,lonb
      rlons(j)=deg2rad*rlons_tmp(j)
   enddo
   deallocate(rlats_tmp,rlons_tmp)
   rlats(1)=-half*pi
   rlats(latb+2)=half*pi
   do j=1,lonb
      clons(j)=cos(rlons(j))
      slons(j)=sin(rlons(j))
   enddo

   allocate(rwork3d2(nlon,(nlat-2),nsig))
   allocate(temp3(nlat,nlon,nsig,nc3d))
   allocate(temp2(nlat,nlon,nc2d))
   k3u=0 ; k3v=0 ; k3t=0 ; k3q=0 ; k3cw=0 ; k3oz=0
   do k3=1,nc3d
      if(cvars3d(k3)=='sf') k3u=k3
      if(cvars3d(k3)=='vp') k3v=k3
      if(cvars3d(k3)=='t') k3t=k3
      if(cvars3d(k3)=='q') k3q=k3
      if(cvars3d(k3)=='cw') k3cw=k3
      if(cvars3d(k3)=='oz') k3oz=k3
      if (trim(cvars3d(k3))=='cw') then
         call read_vardata(atmges, 'clwmr', rwork3d1)
         rwork3d2 = 0
         call read_vardata(atmges, 'icmr', rwork3d2) 
         rwork3d1 = rwork3d1 + rwork3d2
         do k=1,nsig
            kr = levs+1-k
            call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
         end do
      else if(trim(cvars3d(k3))=='oz') then
         call read_vardata(atmges, 'o3mr', rwork3d1)
         do k=1,nsig
            kr = levs+1-k
            call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
         end do
      else if(trim(cvars3d(k3))=='q') then
         call read_vardata(atmges, 'spfh', rwork3d1)
         do k=1,nsig
            kr = levs+1-k
            call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
         end do
      else if(trim(cvars3d(k3))=='t') then
         call read_vardata(atmges, 'tmp', rwork3d1)
         do k=1,nsig
            kr = levs+1-k
            call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
         end do
      else if(trim(cvars3d(k3))=='sf') then
         call read_vardata(atmges, 'ugrd', rwork3d1)
         do k=1,nsig
            kr = levs+1-k
            call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
         end do
      else if(trim(cvars3d(k3))=='vp') then
         call read_vardata(atmges, 'vgrd', rwork3d1)
         do k=1,nsig
            kr = levs+1-k
            call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
         end do
      end if
   enddo
   if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3cw==0.or.k3oz==0) &
      write(6,'(" WARNING, problem with one of k3-")')

   temp2=zero
   do k2=1,nc2d
      if(trim(cvars2d(k2))=='ps') then
         call read_vardata(atmges, 'pressfc', rwork2d)
         call move1_(rwork2d,temp2(:,:,k2),nlon,nlat)
      endif
   enddo
   deallocate(rwork2d, rwork3d1)
   deallocate(rwork3d2)

!  move temp2,temp3 to en_full
   kf=0
   do k3=1,nc3d
      m_cvars3d(k3)=kf+1
      do k=1,nsig
         kf=kf+1
         jj=jas-1
         do j=1,nlon
            jj=jj+1
            ii=ias-1
            do i=1,nlat
               ii=ii+1
               en_full(ii,jj,kf,mas)=temp3(i,j,k,k3)
            enddo
         enddo
      enddo
   enddo
   do k2=1,nc2d
      m_cvars2d(k2)=kf+1
      kf=kf+1
      jj=jas-1
      do j=1,nlon
         jj=jj+1
         ii=ias-1
         do i=1,nlat
            ii=ii+1
            en_full(ii,jj,kf,mas)=temp2(i,j,k2)
         enddo
      enddo
   enddo

   deallocate(temp3)
   deallocate(temp2)

end subroutine parallel_read_gfsnc_state_

subroutine fillpoles_s_(temp,nlon,nlat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fillpoles_s_  make pole points average of nearest pole row
!   prgmmr: parrish          org: emc/ncep            date: 2016-10-14
!
! abstract:  make pole points average of nearest pole row.
!
! program history log:
!   2016-10-14  parrish  - initial code
!
!   input argument list:
!     temp     - 2-d input array containing gsi global horizontal field
!     nlon     - number of gsi/gfs longitudes
!     nlat     - number of gsi latitudes (nlat-2 is gfs--no pole points)
!
!   output argument list:
!     temp     - 2-d output array containing gsi global horizontal field with
!                    pole values set equal to average of adjacent pole rows
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   use kinds, only: i_kind,r_kind
   use constants, only: zero,one

   implicit none

   integer(i_kind),intent(in   ) :: nlon,nlat
   real(r_kind), intent(inout) :: temp(nlat,nlon)

   integer(i_kind) nlatm1,i
   real(r_kind) sumn,sums,rnlon

!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm1=nlat-1
   do i=1,nlon
      sumn=sumn+temp(nlatm1,i)
      sums=sums+temp(2,i)
   end do
   rnlon=one/float(nlon)
   sumn=sumn*rnlon
   sums=sums*rnlon

!  Load means into local work array
   do i=1,nlon
      temp(1,i)   =sums
      temp(nlat,i)=sumn
   end do

end subroutine fillpoles_s_

subroutine fillpoles_v_(tempu,tempv,nlon,nlat,clons,slons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fillpoles_v_  create vector values at pole from nearest pole row
!   prgmmr: parrish          org: emc/ncep            date: 2016-10-14
!
! abstract:  create vector values at pole from nearest pole row.
!
! program history log:
!   2016-10-14  parrish  - initial code
!
!   input argument list:
!     tempu    - 2-d input array containing gsi global horizontal westerly vector component
!     tempv    - 2-d input array containing gsi global horizontal easterly vector component
!     nlon     - number of gsi/gfs longitudes
!     nlat     - number of gsi latitudes (nlat-2 is gfs--no pole points)
!
!   output argument list:
!     temp     - 2-d output array containing gsi global horizontal field with
!                    pole values set equal to average of adjacent pole rows
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

   use kinds, only: i_kind,r_kind
   use constants, only: zero

   implicit none

   integer(i_kind),intent(in   ) :: nlon,nlat
   real(r_kind),   intent(inout) :: tempu(nlat,nlon),tempv(nlat,nlon)
   real(r_kind),   intent(in   ) :: clons(nlon),slons(nlon)

   integer(i_kind) i
   real(r_kind) polnu,polnv,polsu,polsv

!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   do i=1,nlon
      polnu=polnu+tempu(nlat-1,i)*clons(i)-tempv(nlat-1,i)*slons(i)
      polnv=polnv+tempu(nlat-1,i)*slons(i)+tempv(nlat-1,i)*clons(i)
      polsu=polsu+tempu(2,i     )*clons(i)+tempv(2,i     )*slons(i)
      polsv=polsv+tempu(2,i     )*slons(i)-tempv(2,i     )*clons(i)
   end do
   polnu=polnu/float(nlon)
   polnv=polnv/float(nlon)
   polsu=polsu/float(nlon)
   polsv=polsv/float(nlon)
   do i=1,nlon
      tempu(nlat,i)= polnu*clons(i)+polnv*slons(i)
      tempv(nlat,i)=-polnu*slons(i)+polnv*clons(i)
      tempu(1,i   )= polsu*clons(i)+polsv*slons(i)
      tempv(1,i   )= polsu*slons(i)-polsv*clons(i)
   end do

end subroutine fillpoles_v_

subroutine move1_(work,temp,nlon,nlat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    move1_   move gfs lon lat array to gsi lat lon array
!   prgmmr: parrish          org: emc/ncep            date: 2016-10-14
!
! abstract: move gfs lon lat array to gsi lat lon array.
!
! program history log:
!   2016-10-14  parrish  - initial code
!
!   input argument list:
!     work     - 1-d input array containing gfs horizontal field
!     nlon     - number of gsi/gfs longitudes
!     nlat     - number of gsi latitudes (nlat-2 is gfs--no pole points)
!
!   output argument list:
!     temp     - 2-d output array containing gsi global horizontal field
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use kinds, only: i_kind,r_kind,r_single
    use constants, only: zero

    implicit none

    integer(i_kind),intent(in   ) :: nlon,nlat
    real(r_single), intent(in   ) :: work(nlon*(nlat-2))
    real(r_single), intent(  out) :: temp(nlat,nlon)

    integer(i_kind) ii,i,j

    ii=0
    temp(1,:)=zero
    temp(nlat,:)=zero
    do i=nlat-1,2,-1
       do j=1,nlon
          ii=ii+1
          temp(i,j)=work(ii)
       enddo
    enddo

end subroutine move1_

 subroutine get_gfs_ens(this,grd,member,ntindex,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gfs_ens
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Read in GFS ensemble members in to GSI ensemble.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2019-03-13  eliu     - add precipitation component 
!   2019-09-24  martin   - added option for use_gfs_ncio
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

    use kinds, only: i_kind,r_kind
    use gridmod, only: use_gfs_nemsio, use_gfs_ncio
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_4dvar, only: ens_fhrlevs
    use hybrid_ensemble_parameters, only: ensemble_path
    use hybrid_ensemble_parameters, only: uv_hyb_ens
    use hybrid_ensemble_parameters, only: sp_ens
    use gsi_bundlemod, only: gsi_bundle
    use gridmod, only: fv3_full_hydro   

    implicit none

    ! Declare passed variables
    class(ensemble),     intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: atm_bundle
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname_='get_user_ens_gfs_member_'
    character(len=70) :: filename
    logical :: zflag = .false.
    logical,save :: inithead = .true.

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate

    ! if member == 0, read ensemble mean
    if ( member == 0 ) then
       write(filename,12) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex)
    else
       write(filename,22) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),member
    endif
12  format(a,'sigf',i2.2,'_ensmean'     )
22  format(a,'sigf',i2.2,'_ens_mem',i3.3)

    if ( use_gfs_nemsio ) then
       if (fv3_full_hydro) then

          call general_read_fv3atm_nems(grd,sp_ens,filename,uv_hyb_ens,.false., &
               zflag,atm_bundle,.true.,iret)

       else

          call general_read_gfsatm_nems(grd,sp_ens,filename,uv_hyb_ens,.false., &
               zflag,atm_bundle,.true.,iret)

       endif
    else if ( use_gfs_ncio ) then
       call general_read_gfsatm_nc(grd,sp_ens,filename,uv_hyb_ens,.false., &
            zflag,atm_bundle,.true.,iret)
    else
       call general_read_gfsatm(grd,sp_ens,sp_ens,filename,uv_hyb_ens,.false., &
            zflag,atm_bundle,inithead,iret)
    endif

    inithead = .false.

    if ( iret /= 0 ) then
        if ( mype == 0 ) then
            write(6,'(A)') 'get_user_ens_: WARNING!'
          write(6,'(3A,I5)') 'Trouble reading ensemble file : ', trim(filename), ', IRET = ', iret
       endif
    endif

    return

end subroutine get_gfs_ens

subroutine put_gfs_ens(this,grd,member,ntindex,pert,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    put_gfs_ens    write out an internally gen ens to file
!   prgmmr: mahajan          org: emc/ncep            date: 2016-06-30
!
! abstract: Write out GSI ensemble to file.
!
! program history log:
!   2016-06-30  mahajan  - initial code
!   2016-07-20  mpotts   - refactored into class/module
!   2019-09-24  martin   - stub for use_gfs_ncio
!
!   input argument list:
!     grd      - grd info for ensemble
!     member   - index for ensemble member
!     ntindex  - time index for ensemble
!        pert  - bundle of ensemble perturbations
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
    use general_sub2grid_mod, only: sub2grid_info
    use gsi_bundlemod, only: gsi_bundle
    use gsi_4dvar, only: ens_fhrlevs
    use hybrid_ensemble_parameters, only: ensemble_path
    use hybrid_ensemble_parameters, only: sp_ens
    use gridmod, only: use_gfs_nemsio, use_gfs_ncio

    implicit none

    ! Declare passed variables
    class(ensemble),     intent(inout) :: this
    type(sub2grid_info), intent(in   ) :: grd
    integer(i_kind),     intent(in   ) :: member
    integer(i_kind),     intent(in   ) :: ntindex
    type(gsi_bundle),    intent(inout) :: pert
    integer(i_kind),     intent(  out) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname_='put_gfs_ens'
    character(len=70) :: filename
    integer(i_kind) :: mype_atm
    logical,save :: inithead = .true.

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    mype_atm = member

    write(filename,13) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),member
13  format(a,'sigf',i2.2,'_ens_pert',i3.3)

    if ( use_gfs_nemsio ) then
       if ( mype == 0 ) then
          write(6,*) 'write_nemsatm is not adapted to write out perturbations yet'
          iret = 999
       endif
       !call write_nemsatm(grd,...)
    else if ( use_gfs_ncio ) then
       if ( mype == 0 ) then
          write(6,*) 'write_gfsncatm is not adapted to write out perturbations yet'
          iret = 999
       endif
       !call write_gfsncatm(grd,...)
    else
       call general_write_gfsatm(grd,sp_ens,sp_ens,filename,mype_atm, &
            pert,ntindex,inithead,iret)
    endif

    inithead = .false.

    if ( iret /= 0 ) then
       if ( mype == mype_atm ) then
          write(6,'(A)') 'put_gsi_ens_: WARNING!'
          write(6,'(3A,I5)') 'Trouble writing ensemble perturbation to file : ', trim(filename), ', IRET = ', iret
       endif
    endif

    return

end subroutine put_gfs_ens

subroutine non_gaussian_ens_grid_gfs(this,elats,elons)

    use kinds, only: r_kind
    use hybrid_ensemble_parameters, only: sp_ens

    implicit none

    ! Declare passed variables
    class(ensemble), intent(inout) :: this
    real(r_kind), intent(out) :: elats(:),elons(:)

    character(len=*),parameter :: myname_=myname//'non_gaussian_ens_grid'

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate

    if (size(elats)/=size(sp_ens%rlats).or.size(elons)/=size(sp_ens%rlons)) then
       if(mype==0) then
         write(6,*) myname_,': inconsistent ens nlat/nlon'
         write(6,*) myname_,':  actual(vec) ', size(elats),size(elons)
         write(6,*) myname_,': defined(vec) ', size(sp_ens%rlats),size(sp_ens%rlons)
      endif
      call stop2(999)
   endif

   elats=sp_ens%rlats
   elons=sp_ens%rlons

   return

end subroutine non_gaussian_ens_grid_gfs

subroutine create_sub2grid_info(s2gi,nsig,npe,s2gi_ref)
!> Create temporary communication information object for read ensemble routines
   use kinds, only: i_kind
   use gridmod, only: regional
   use general_sub2grid_mod, only: sub2grid_info
   use general_sub2grid_mod, only: general_sub2grid_create_info
   implicit none
 
   ! Declare passed variables
   type(sub2grid_info), intent(out  ) :: s2gi
   integer(i_kind),     intent(in   ) :: nsig
   integer(i_kind),     intent(in   ) :: npe
   type(sub2grid_info), intent(in   ) :: s2gi_ref

   call general_sub2grid_create_info(s2gi, inner_vars=1, &
        nlat=s2gi_ref%nlat,nlon=s2gi_ref%nlon,nsig=nsig, &
        num_fields=min(6*nsig+1,npe),regional=regional)
return
end subroutine create_sub2grid_info

subroutine destroy_sub2grid_info(s2gi)
!> Destroy the object
   use general_sub2grid_mod, only: sub2grid_info
   use general_sub2grid_mod, only: general_sub2grid_destroy_info
   implicit none
 
   ! Declare passed variables
   type(sub2grid_info), intent(inout) :: s2gi

   call general_sub2grid_destroy_info(s2gi)
return
end subroutine destroy_sub2grid_info

end module get_gfs_ensmod_mod
