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
    use kinds, only: i_kind,r_kind,r_single
    use mpeu_util, only: die
    use mpimod, only: mype,npe
    use abstract_ensmod, only: this_ens_class => abstractEnsemble
    use genex_mod, only: genex_info

    implicit none
    private

    integer(i_kind) :: ias,iae,iasm,iaem,iaemz,jas,jae,jasm,jaem,jaemz
    integer(i_kind) :: kas,kae,kasm,kaem,kaemz,mas,mae,masm,maem,maemz
    integer(i_kind) :: ibs,ibe,ibsm,ibem,ibemz,jbs,jbe,jbsm,jbem,jbemz
    integer(i_kind) :: kbs,kbe,kbsm,kbem,kbemz,mbs,mbe,mbsm,mbem,mbemz
    integer(i_kind) :: icw,iql,iqi,iqr,iqs,iqg  
    integer(i_kind) :: n2d
    type(genex_info) :: s_a2b

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

    integer(i_kind) :: n

    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate

    if ( (use_gfs_nemsio .or. use_gfs_ncio) .and. ens_fast_read ) then
       call get_user_ens_gfs_fastread_(ntindex,atm_bundle, &
                         grd_ens%lat2,grd_ens%lon2, &
                         nc2d,nc3d,iret,grd)
    else
       do n = 1,members
          call get_gfs_ens(this,grd,n,ntindex,atm_bundle(n),iret)
       end do
    endif

    return

end subroutine get_gfs_Nens

subroutine get_user_ens_gfs_fastread_(ntindex,atm_bundle, &
                           lat2in,lon2in,nc2din,nc3din,iret,grd)
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
!     atm_bundle - atm bundle w/ fields for ensemble
!
!   output argument list:
!     atm_bundle - atm bundle w/ fields for ensemble
!     iret           - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

    use mpimod, only: mpi_comm_world,ierror,mpi_real8,mpi_integer4,mpi_max
    use constants, only: zero
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_destroy_info
    use gsi_4dvar, only: ens_fhrlevs
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only : assignment(=)
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,ntlevs_ens
    use hybrid_ensemble_parameters, only: ensemble_path
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
    use genex_mod, only: genex_create_info,genex,genex_destroy_info
    use gridmod, only: use_gfs_nemsio
    use jfunc, only: cnvw_option
    use mpeu_util, only: getindex  

    implicit none

    ! Declare passed variables
    integer(i_kind),     intent(in   ) :: ntindex
    integer(i_kind),     intent(in   ) :: lat2in,lon2in,nc2din,nc3din
    integer(i_kind),     intent(  out) :: iret
    type(sub2grid_info), intent(in   ) :: grd
    type(gsi_bundle),    intent(inout) :: atm_bundle(:)


    ! Declare internal variables
    character(len=*),parameter :: myname_='get_user_ens_gfs_fastread_'
    character(len=70) :: filename
    character(len=70) :: filenamesfc
    integer(i_kind) :: i,ii,j,k,n
    integer(i_kind) :: io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,i_ens
    integer(i_kind) :: ip
    integer(i_kind) :: nlon,nlat,nsig
    integer(i_kind),dimension(n_ens) :: io_pe0,iretx
    real(r_single),allocatable,dimension(:,:,:,:) :: en_full,en_loc
    real(r_single),allocatable,dimension(:,:,:) :: sloc
    integer(i_kind),allocatable,dimension(:) :: m_cvars2dw,m_cvars3dw
    integer(i_kind) :: m_cvars2d(nc2d),m_cvars3d(nc3d)
    type(sub2grid_info) :: grd3d


    nlat=grd_ens%nlat
    nlon=grd_ens%nlon
    nsig=grd_ens%nsig

    if(ntindex == 1)then
    !  set up partition of available processors for parallel read
      if ( n_ens > npe ) &
          call die(myname_, ': ***ERROR*** CANNOT READ ENSEMBLE  n_ens > npe, increase npe >= n_ens', 99)


      call ens_io_partition_(n_ens,io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,io_pe0,i_ens)


    ! setup communicator for scatter to subdomains:
    ! first, define gsi subdomain boundaries in global units:

!!!!!!!!!!!!NOTE--FOLLOWING HAS MANY VARS TO BE DEFINED--NLAT,NLON ARE ENSEMBLE DOMAIN DIMS
!!!!!!!!for example,  n2d = nc3d*nsig + nc2d

      n2d=nc3d*grd_ens%nsig+nc2d
      ias=0 ; iae=0 ; jas=0 ; jae=0 ; kas=1 ; kae=0 ; mas=1 ; mae=0
      if(mype==io_pe) then
         iae=nlat+1
         jae=nlon+1
         kae=n2d
         mas=n_io_pe_s ; mae=n_io_pe_em
      endif
      iasm=ias ; iaem=iae ; jasm=jas ; jaem=jae ; kasm=kas ; kaem=kae ; masm=mas ; maem=mae

      ip=1   !  halo width is hardwired at 1
      ibs=grd_ens%istart(mype+1)
      ibe=ibs+grd_ens%lat1-1
      jbs=grd_ens%jstart(mype+1)
      jbe=jbs+grd_ens%lon1-1
      jbs=jbs-1
      jbe=jbe+1
      ibs=ibs-1
      ibe=ibe+1

      ibsm=ibs ; ibem=ibe ; jbsm=jbs ; jbem=jbe

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
  
      if(mype==0)then
        do n=1,n_ens
          write(6,'(3(a,1x,i5,1x))') 'reading ensemble member', n,'on pe', io_pe0(n)
        enddo
      end if

    end if
    if(mype==0) write(6,*) ' reading time level ',ntindex 
    allocate(m_cvars2dw(nc2din),m_cvars3dw(nc3din))
    m_cvars2dw=-999
    m_cvars3dw=-999

!!  read ensembles

    if ( mas == mae ) then
       allocate(en_full(iasm:iaemz,jasm:jaemz,kasm:kaemz,masm:maemz))
       write(filename,22) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),mas
22     format(a,'sigf',i2.2,'_ens_mem',i3.3)
       if ( use_gfs_nemsio ) then
          if (cnvw_option) then
             write(filenamesfc,23) trim(adjustl(ensemble_path)),ens_fhrlevs(ntindex),mas
23           format(a,'sfcf',i2.2,'_ens_mem',i3.3)
             call parallel_read_nemsio_state_(en_full,m_cvars2dw,m_cvars3dw,nlon,nlat,nsig, &
                                            ias,jas,mas, &
                                            iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                            filename,.true.,filenamesfc)
          else
             call parallel_read_nemsio_state_(en_full,m_cvars2dw,m_cvars3dw,nlon,nlat,nsig, &
                                            ias,jas,mas, &
                                            iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                            filename,.true.)
          end if
       else
          call parallel_read_gfsnc_state_(en_full,m_cvars2dw,m_cvars3dw,nlon,nlat,nsig, &
                                         ias,jas,mas, &
                                         iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                         filename)
       end if
    else
       allocate(en_full(1,1,1,1))
    end if

! scatter to subdomains:

    call mpi_allreduce(m_cvars2dw,m_cvars2d,nc2d,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    call mpi_allreduce(m_cvars3dw,m_cvars3d,nc3d,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    deallocate(m_cvars2dw,m_cvars3dw)

    ! Check hydrometeors in control variables 
    icw=getindex(cvars3d,'cw')
    iql=getindex(cvars3d,'ql')
    iqi=getindex(cvars3d,'qi')
    iqr=getindex(cvars3d,'qr')
    iqs=getindex(cvars3d,'qs')
    iqg=getindex(cvars3d,'qg')

!   en_loc=zero

    allocate(en_loc(ibsm:ibemz,jbsm:jbemz,kbsm:kbemz,mbsm:mbemz))
    call genex(s_a2b,en_full,en_loc)

    deallocate(en_full)

    if(ntindex == ntlevs_ens)call genex_destroy_info(s_a2b)  


    call create_grd23d_(grd3d,nc2d+nc3d*grd%nsig)


    allocate(sloc(grd3d%lat2,grd3d%lon2,grd3d%num_fields))
    iretx=0
!$omp parallel do  schedule(dynamic,1) private(n,k,j,i,sloc) 
    do n=1,n_ens
       do k=1,grd3d%num_fields
          do j=1,grd3d%lon2
             do i=1,grd3d%lat2
                sloc(i,j,k)=en_loc(i+ibsm-1,j+jbsm-1,k,n)
             enddo
          enddo
       enddo
       call move2bundle_(grd3d,sloc,atm_bundle(n),m_cvars2d,m_cvars3d,iretx(n))
    enddo
    iret=iretx(1)
    do n=2,n_ens
       iret=iret+iretx(n)
    end do
    deallocate(en_loc,sloc)
    call general_sub2grid_destroy_info(grd3d,grd)

end subroutine get_user_ens_gfs_fastread_

subroutine move2bundle_(grd3d,en_loc3,atm_bundle,m_cvars2d,m_cvars3d,iret)

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
!     sloc    - ensemble member
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

    use general_sub2grid_mod, only: sub2grid_info
    use hybrid_ensemble_parameters, only: en_perts
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d

    implicit none

    ! Declare passed variables
    type(sub2grid_info), intent(in   ) :: grd3d
    type(gsi_bundle),    intent(inout) :: atm_bundle
    real(r_single),      intent(inout) :: en_loc3(grd3d%lat2,grd3d%lon2,grd3d%num_fields)
    integer(i_kind),     intent(in   ) :: m_cvars2d(nc2d),m_cvars3d(nc3d)
    integer(i_kind),     intent(inout) :: iret

    ! Declare internal variables
    character(len=*),parameter :: myname_='move2bundle_'

    integer(i_kind) :: ierr,i,j
    integer(i_kind) :: km1,m
    real(r_single),pointer,dimension(:,:) :: ps
    !real(r_kind),pointer,dimension(:,:) :: sst
    real(r_single),pointer,dimension(:,:,:) :: u,v,tv,q,oz,cwmr
    real(r_single),pointer,dimension(:,:,:) :: qlmr,qimr,qrmr,qsmr,qgmr   

!   atm_bundle to zero done earlier

    call gsi_bundlegetpointer(atm_bundle,'ps',ps,  ierr); iret = ierr
    !call gsi_bundlegetpointer(atm_bundle,'sst',sst, ierr); iret = iret+ierr

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
          write(6,'(A)') trim(myname_) // ': For now, GFS requires all MetFields: ps,u,v,(sf,vp)tv,q,oz'
          write(6,'(A)') trim(myname_) // ': but some have not been found. Aborting ... '
          write(6,'(A)') trim(myname_) // ': WARNING!'
       endif
       return
    endif

    do m=1,nc2d
!      convert ps from Pa to cb
       if(trim(cvars2d(m))=='ps') ps=en_loc3(:,:,m_cvars2d(m))
!      if(trim(cvars2d(m))=='sst') sst=en_loc3(:,:,m_cvars2d(m)) !no sst for now
    enddo

    km1 = en_perts(1,1,1)%grid%km - 1
    do m=1,nc3d
       if(trim(cvars3d(m))=='sf')then
          u    = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='vp') then
          v    = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='t')  then
!  Note tv here is sensible temperature.  Converted to virtual temperature
!  later.
          tv   = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='q')  then
          q    = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='oz') then
          oz   = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='cw') then
          cwmr = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='ql') then
          qlmr = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='qi') then
          qimr = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='qr') then
          qrmr = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='qs') then
          qsmr = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       else if(trim(cvars3d(m))=='qg') then
          qgmr = en_loc3(:,:,m_cvars3d(m):m_cvars3d(m)+km1)
       end if
    enddo

    return

end subroutine move2bundle_

subroutine create_grd23d_(grd23d,nvert)

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

subroutine ens_io_partition_(n_ens,io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,io_pe0,i_ens)

!     do computation on all processors, then assign final local processor
!     values.

      use constants, only: half

      implicit none

!     Declare passed variables
      integer(i_kind),intent(in   ) :: n_ens
      integer(i_kind),intent(  out) :: io_pe,n_io_pe_s,n_io_pe_e,n_io_pe_em,i_ens
      integer(i_kind),intent(  out) :: io_pe0(n_ens)

!     Declare local variables
      integer(i_kind) :: iskip,jskip,nextra,ipe,n
      integer(i_kind) :: nsig

      i_ens=-1
      nsig=1
      iskip=npe/n_ens
      nextra=npe-iskip*(n_ens-1)-1
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
         if(ipe > npe) then
            write(6,*)' ens_io_partition_:  ***ERROR*** ',ipe,jskip,' processor error: PROGRAM STOPS'
            call stop2(999)
         end if
         ipe=ipe+jskip
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

subroutine parallel_read_nemsio_state_(en_full,m_cvars2dw,m_cvars3d,nlon,nlat,nsig, &
                                        ias,jas,mas, &
                                        iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                        filename,init_head,filenamesfc)

   use constants, only: r60,r3600,zero,one,half,deg2rad
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
   integer(i_kind),  intent(inout) :: m_cvars2dw(nc2d),m_cvars3d(nc3d)
   real(r_single),   intent(inout) :: en_full(iasm:iaemz,jasm:jaemz,kasm:kaemz,masm:maemz)
   character(len=*), intent(in   ) :: filename
   character(len=*), optional, intent(in) :: filenamesfc
   logical,          intent(in   ) :: init_head

   ! Declare local variables
   integer(i_kind) i,ii,j,jj,k,lonb,latb,levs,latb2,lonb2
   integer(i_kind) k2,k3,k3u,k3v,k3t,k3q,k3cw,k3oz,kf
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
   real(r_kind),allocatable,dimension(:) :: rlons
   real(r_kind) :: clons(nlon),slons(nlon)
   real(r_single),allocatable,dimension(:) :: r4lons

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

!  obtain r4lons,rlons,clons,slons exactly as computed in general_read_gfsatm_nems:

   allocate(rlons(lonb),r4lons(lonb*latb))
   call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
   do j=1,lonb
      rlons(j)=deg2rad*r4lons(j)
   enddo
   deallocate(r4lons)
   do j=1,lonb
      clons(j)=cos(rlons(j))
      slons(j)=sin(rlons(j))
   enddo
   deallocate(rlons)

   fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
           real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
   odate(1) = idate(4)  !hour
   odate(2) = idate(2)  !month
   odate(3) = idate(3)  !day
   odate(4) = idate(1)  !year

   allocate(work(nlon*(nlat-2)))
   if (imp_physics == 11) allocate(work2(nlon*(nlat-2)))
   allocate(temp3(nlat,nlon,nsig,nc3d))
   temp3=zero
   k3u=0 ; k3v=0 ; k3t=0 ; k3q=0 ; k3cw=0 ; k3oz=0
   do k3=1,nc3d
      do k=1,nsig
         if(trim(cvars3d(k3))=='t') then
            k3t=k3
            call nemsio_readrecv(gfile,'tmp','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'tmp','read',istop+3,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='sf') then
            k3u=k3
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'ugrd','read',istop+1,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='vp') then
            k3v=k3
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'vgrd','read',istop+2,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='q') then
            k3q=k3
            call nemsio_readrecv(gfile,'spfh','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),trim(cvars3d(k3)),'read',istop+4,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='oz') then
            k3oz=k3
            call nemsio_readrecv(gfile,'o3mr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'o3mr','read',istop+5,iret,.true.)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='cw') then
            k3cw=k3
            call nemsio_readrecv(gfile,'clwmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'clwmr','read',istop+6,iret,.true.)
            if (imp_physics == 11) then
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
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='ql') then
            call nemsio_readrecv(gfile,'clwmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'clwmr','read',istop+8,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qi') then
            call nemsio_readrecv(gfile,'icmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'icmr','read',istop+9,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qr') then
            call nemsio_readrecv(gfile,'rwmr','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'rwmr','read',istop+10,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qs') then
            call nemsio_readrecv(gfile,'snmr','mid layer',k,work,iret=iret)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'snmr','read',istop+11,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         elseif(trim(cvars3d(k3))=='qg') then
            call nemsio_readrecv(gfile,'grle','mid layer',k,work,iret=iret)
            if (iret /= 0) call error_msg(trim(myname_),trim(filename),'grle','read',istop+12,iret)
            call move1_(work,temp3(:,:,k,k3),nlon,nlat)
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         end if
      enddo
   enddo
   do k=1,nsig
      call fillpoles_sv_(temp3(:,:,k,k3u),temp3(:,:,k,k3v),nlon,nlat,clons,slons)
   end do
!  if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3cw==0.or.k3oz==0) & 
   if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3oz==0) &  
      write(6,'(" WARNING, problem with one of k3-")')

!  move temp3 to en_full
   kf=0
   do k3=1,nc3d
      m_cvars3d(k3)=kf+1
      do k=1,nsig
         kf=kf+1
         jj=jas
         do j=1,nlon
            jj=jj+1
            ii=ias
            do i=1,nlat
               ii=ii+1
               en_full(ii,jj,kf,mas)=temp3(i,j,k,k3)
            enddo
         enddo
         ii=ias
         do i=1,nlat
            ii=ii+1
            en_full(ii,jasm,kf,mas)=en_full(ii,jaem-1,kf,mas)
            en_full(ii,jaem,kf,mas)=en_full(ii,jasm+1,kf,mas)
         enddo
         jj=jas-1
         do j=jasm,jaem
           jj=jj+1
           en_full(iasm,jj,kf,mas)=en_full(iasm+1,jj,kf,mas)
           en_full(iaem,jj,kf,mas)=en_full(iaem-1,jj,kf,mas)
         end do
      enddo
   enddo
   deallocate(temp3)

!   convert T to Tv:    postpone this calculation
!  temp3(:,:,:,k3t)=temp3(:,:,:,k3t)*(one+fv*temp3(:,:,:,k3q))

   allocate(temp2(nlat,nlon,nc2d))
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
         call fillpoles_ss_(temp2(:,:,k2),nlon,nlat)
      endif
   enddo
   deallocate(work)
   if (imp_physics == 11) deallocate(work2)

!  move temp2 to en_full
   do k2=1,nc2d
      m_cvars2dw(k2)=kf+1
      kf=kf+1
      jj=jas
      do j=1,nlon
         jj=jj+1
         ii=ias
         do i=1,nlat
            ii=ii+1
            en_full(ii,jj,kf,mas)=temp2(i,j,k2)
         enddo
      enddo
      ii=ias
      do i=1,nlat
         ii=ii+1
         en_full(ii,jasm,kf,mas)=en_full(ii,jaem-1,kf,mas)
         en_full(ii,jaem,kf,mas)=en_full(ii,jasm+1,kf,mas)
      enddo
      jj=jas-1
      do j=jasm,jaem
        jj=jj+1
        en_full(iasm,jj,kf,mas)=en_full(iasm+1,jj,kf,mas)
        en_full(iaem,jj,kf,mas)=en_full(iaem-1,jj,kf,mas)
      end do
   enddo

   deallocate(temp2)

end subroutine parallel_read_nemsio_state_

subroutine parallel_read_gfsnc_state_(en_full,m_cvars2dw,m_cvars3d,nlon,nlat,nsig, &
                                        ias,jas,mas, &
                                        iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz, &
                                        filename)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    parallel_read_gfsnc_state_    read GFS netCDF ensemble member 
!   prgmmr: Martin            org: NCEP/EMC                date: 2019-09-24
!
! program history log:
!   2019-09-24 Martin    Initial version.  Based on sub parallel_read_nemsio_state_ 
!
!$$$

   use constants, only: r60,r3600,zero,one,half,deg2rad,zero_single
   use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
   use general_sub2grid_mod, only: sub2grid_info
   use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                           close_dataset, get_dim, read_vardata 

   implicit none

   ! Declare local parameters

   ! Declare passed variables
   integer(i_kind),  intent(in   ) :: nlon,nlat,nsig
   integer(i_kind),  intent(in   ) :: ias,jas,mas
   integer(i_kind),  intent(in   ) :: iasm,iaemz,jasm,jaemz,kasm,kaemz,masm,maemz
   integer(i_kind),  intent(inout) :: m_cvars2dw(nc2d),m_cvars3d(nc3d)
   real(r_single),   intent(inout) :: en_full(iasm:iaemz,jasm:jaemz,kasm:kaemz,masm:maemz)
   character(len=*), intent(in   ) :: filename

   ! Declare local variables
   logical :: file_exist
   integer(i_kind) i,ii,j,jj,k,lonb,latb,levs,kr,ierror
   integer(i_kind) k2,k3,k3u,k3v,k3t,k3q,k3cw,k3oz,kf
   character(len=120) :: myname_ = 'parallel_read_gfsnc_state_'
   real(r_single),allocatable,dimension(:,:,:) :: rwork3d1, rwork3d2
   real(r_single),allocatable,dimension(:,:) ::  temp2,rwork2d
   real(r_single),allocatable,dimension(:,:,:,:) ::  temp3
   real(r_kind),allocatable,dimension(:) :: rlons_tmp
   real(r_kind) :: clons(nlon),slons(nlon)
   real(r_kind) :: rlons
   type(Dataset) :: atmges
   type(Dimension) :: ncdim

!  Check to see if requested file exists
   inquire(file=filename,exist=file_exist)
   if (.not.file_exist) then
      write(6,*)' PARALLEL_READ_GFSNC_STATE:  ***FATAL ERROR*** ',trim(filename),' NOT AVAILABLE: PROGRAM STOPS'
      call die(myname_, ': ***FATAL ERROR*** insufficient ens fcst for hybrid',999)
   endif

   ierror=0
!  If file exists, open and process
   atmges = open_dataset(filename,errcode=ierror)
   if (ierror /=0) then
      write(6,*)' PARALLEL_READ_GFSNC_STATE:  ***FATAL ERROR*** ',trim(filename),' NOT AVAILABLE: PROGRAM STOPS'
      call stop2(999)
   endif
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

!  obtain rlons_tnp,rlons,clons,slons exactly as computed in general_read_gfsatm_nems:

   call read_vardata(atmges, 'grid_xt', rlons_tmp)
   do j=1,lonb
      rlons=deg2rad*rlons_tmp(j)
      clons(j)=cos(rlons)
      slons(j)=sin(rlons)
   enddo
   deallocate(rlons_tmp)

   allocate(rwork3d1(nlon,(nlat-2),nsig))
   allocate(temp3(nlat,nlon,nsig,nc3d))
   k3u=0 ; k3v=0 ; k3t=0 ; k3q=0 ; k3cw=0 ; k3oz=0
   do k3=1,nc3d
      if(trim(cvars3d(k3))=='t') then
         k3t=k3
         call read_vardata(atmges, 'tmp', rwork3d1)
      else if(trim(cvars3d(k3))=='sf') then
         k3u=k3
         call read_vardata(atmges, 'ugrd', rwork3d1)
      else if(trim(cvars3d(k3))=='vp') then
         k3v=k3
         call read_vardata(atmges, 'vgrd', rwork3d1)
      else if(trim(cvars3d(k3))=='q') then
         k3q=k3
         call read_vardata(atmges, 'spfh', rwork3d1)
      else if (trim(cvars3d(k3))=='cw') then
         k3cw=k3
         call read_vardata(atmges, 'clwmr', rwork3d1)
         allocate(rwork3d2(nlon,(nlat-2),nsig))
         rwork3d2 = 0._r_single
         call read_vardata(atmges, 'icmr', rwork3d2) 
         rwork3d1 = rwork3d1 + rwork3d2
         deallocate(rwork3d2)
      else if(trim(cvars3d(k3))=='oz') then
         k3oz=k3
         call read_vardata(atmges, 'o3mr', rwork3d1)
      else if(trim(cvars3d(k3))=='ql') then
         call read_vardata(atmges, 'clwmr', rwork3d1)
      else if(trim(cvars3d(k3))=='qi') then
         call read_vardata(atmges, 'icmr', rwork3d1)
      else if(trim(cvars3d(k3))=='qr') then
         call read_vardata(atmges, 'rwmr', rwork3d1)
      else if(trim(cvars3d(k3))=='qs') then
         call read_vardata(atmges, 'snmr', rwork3d1)
      else if(trim(cvars3d(k3))=='qg') then
         call read_vardata(atmges, 'grle', rwork3d1)
      end if
!$omp parallel do  schedule(dynamic,1) private(k,kr)
      do k=1,nsig
         kr = levs+1-k
         call move1_(rwork3d1(:,:,kr),temp3(:,:,k,k3),nlon,nlat)
      end do
   enddo
   deallocate(rwork3d1)

!  if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3cw==0.or.k3oz==0) &
   if (k3u==0.or.k3v==0.or.k3t==0.or.k3q==0.or.k3oz==0) &
      write(6,'(" WARNING, problem with one of k3-")')

!$omp parallel do  schedule(dynamic,1) private(k,k3) 
   do k=1,nsig
      call fillpoles_sv_(temp3(:,:,k,k3u),temp3(:,:,k,k3v),nlon,nlat,clons,slons)
   end do

!  move temp3 to en_full
!$omp parallel do  schedule(dynamic,1) private(k3,k,kf,j,jj,i,ii) 
   do k3=1,nc3d
      if(k3 /= k3u .and. k3 /= k3v)then
         do k=1,nsig
            call fillpoles_ss_(temp3(:,:,k,k3),nlon,nlat)
         end do
      end if
      kf=(k3-1)*nsig
      m_cvars3d(k3)=kf+1
      do k=1,nsig
         kf=kf+1
         jj=jas
         do j=1,nlon
            jj=jj+1
            ii=ias
            do i=1,nlat
               ii=ii+1
               en_full(ii,jj,kf,mas)=temp3(i,j,k,k3)
            enddo
         enddo
         ii=ias
         do i=1,nlat
            ii=ii+1
            en_full(ii,jasm,kf,mas)=en_full(ii,jaem-1,kf,mas)
            en_full(ii,jaem,kf,mas)=en_full(ii,jasm+1,kf,mas)
         enddo
         jj=jas-1
         do j=jasm,jaem
           jj=jj+1
           en_full(iasm,jj,kf,mas)=en_full(iasm+1,jj,kf,mas)
           en_full(iaem,jj,kf,mas)=en_full(iaem-1,jj,kf,mas)
         end do
      enddo
   enddo

   deallocate(temp3)
   allocate(temp2(nlat,nlon))
   allocate(rwork2d(nlon,(nlat-2)))
   kf=nc3d*nsig
   do k2=1,nc2d
      if(trim(cvars2d(k2))=='ps') then
         call read_vardata(atmges, 'pressfc', rwork2d)
         call move1_(rwork2d,temp2,nlon,nlat)
         call fillpoles_ss_(temp2,nlon,nlat)

!  move temp2 to en_full
         kf=kf+1
         m_cvars2dw(k2)=kf
         jj=jas
         do j=1,nlon
            jj=jj+1
            ii=ias
            do i=1,nlat
               ii=ii+1
               en_full(ii,jj,kf,mas)=temp2(i,j)
            enddo
         enddo
         ii=ias
         do i=1,nlat
            ii=ii+1
            en_full(ii,jasm,kf,mas)=en_full(ii,jaem-1,kf,mas)
            en_full(ii,jaem,kf,mas)=en_full(ii,jasm+1,kf,mas)
         enddo
         jj=jas-1
         do j=jasm,jaem
           jj=jj+1
           en_full(iasm,jj,kf,mas)=en_full(iasm+1,jj,kf,mas)
           en_full(iaem,jj,kf,mas)=en_full(iaem-1,jj,kf,mas)
         end do
      end if
   enddo
   call close_dataset(atmges)

   deallocate(rwork2d)
   deallocate(temp2)

end subroutine parallel_read_gfsnc_state_

subroutine fillpoles_ss_(temp,nlon,nlat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fillpoles_ss_  make pole points average of nearest pole row
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

   use constants, only: zero,one

   implicit none

   integer(i_kind),intent(in   ) :: nlon,nlat
   real(r_single), intent(inout) :: temp(nlat,nlon)

   integer(i_kind) nlatm1,i
   real(r_kind) sumn,sums,rnlon
   real(r_single) sumn_sing,sums_sing

!  Compute mean along southern and northern latitudes
   sumn=zero
   sums=zero
   nlatm1=nlat-1
   do i=1,nlon
      sumn=sumn+temp(nlatm1,i)
      sums=sums+temp(2,i)
   end do
   rnlon=one/real(nlon,r_kind)
   sumn_sing=sumn*rnlon
   sums_sing=sums*rnlon

!  Load means into local work array
   do i=1,nlon
      temp(1,i)   =sums_sing
      temp(nlat,i)=sumn_sing
   end do

end subroutine fillpoles_ss_

subroutine fillpoles_sv_(tempu,tempv,nlon,nlat,clons,slons)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fillpoles_sv_  create vector values at pole from nearest pole row
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

   use constants, only: zero

   implicit none

   integer(i_kind),intent(in   ) :: nlon,nlat
   real(r_single), intent(inout) :: tempu(nlat,nlon),tempv(nlat,nlon)
   real(r_kind),   intent(in   ) :: clons(nlon),slons(nlon)

   integer(i_kind) i,nlatm
   real(r_kind) polnu,polnv,polsu,polsv

!  Compute mean along southern and northern latitudes
   polnu=zero
   polnv=zero
   polsu=zero
   polsv=zero
   nlatm=nlat-1
   do i=1,nlon
      polnu=polnu+tempu(nlatm,i)*clons(i)-tempv(nlatm,i)*slons(i)
      polnv=polnv+tempu(nlatm,i)*slons(i)+tempv(nlatm,i)*clons(i)
      polsu=polsu+tempu(2,i    )*clons(i)+tempv(2,i    )*slons(i)
      polsv=polsv+tempu(2,i    )*slons(i)-tempv(2,i    )*clons(i)
   end do
   polnu=polnu/real(nlon,r_kind)
   polnv=polnv/real(nlon,r_kind)
   polsu=polsu/real(nlon,r_kind)
   polsv=polsv/real(nlon,r_kind)
   do i=1,nlon
      tempu(nlat,i)= polnu*clons(i)+polnv*slons(i)
      tempv(nlat,i)=-polnu*slons(i)+polnv*clons(i)
      tempu(1,i   )= polsu*clons(i)+polsv*slons(i)
      tempv(1,i   )= polsu*slons(i)-polsv*clons(i)
   end do

end subroutine fillpoles_sv_

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

    use constants, only: zero

    implicit none

    integer(i_kind),intent(in   ) :: nlon,nlat
    real(r_single), intent(in   ) :: work(nlon*(nlat-2))
    real(r_single), intent(  out) :: temp(nlat,nlon)

    integer(i_kind) ii,i,j

    ii=0
!  Polar points will be filled in later
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
               zflag,atm_bundle,.true.,iret,ntindex)

       endif
    else if ( use_gfs_ncio ) then
       if (fv3_full_hydro) then
          call general_read_gfsatm_allhydro_nc(grd,sp_ens,filename,uv_hyb_ens,.false., &
               zflag,atm_bundle,iret)
       else
          call general_read_gfsatm_nc(grd,sp_ens,filename,uv_hyb_ens,.false., &
               zflag,atm_bundle,.true.,iret)
       endif
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
