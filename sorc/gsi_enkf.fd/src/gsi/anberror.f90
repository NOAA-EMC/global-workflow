module anberror
!$$$   module documentation block
!                .      .    .                                       .
! module:    anberror   holds info related to anisotropic background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: contains information pertaining to the background error
!
! program history log:
!   2005-02-08  parrish
!   2005-05-24  pondeca - accommodate 2dvar only surface analysis option
!                         in create_anberror_vars_reg
!   2005-11-29  derber - remove anset_ozone_var (included in anprewgt_reg)
!   2007-08-21  pondeca - add qvar3d allocate (bug fix)
!   2008-11-03  sato - update for global mode and sub-domain mode
!   2008-12-10  zhu  - use nvars from jfunc,add changes for generalized control variables
!   2010-06-05  todling - an_amp no longer has wired-in order of variables in CV
!   2015-05-02  parrish - add logical parameter rtma_bkerr_sub2slab to allow
!                          anisotropic recursive filter to be used in slab mode,
!                          while rest of code is run in subdomain mode
!
! subroutines included:
!   sub init_anberror             - initialize extra anisotropic background error
!                                       related variables
!   sub create_anberror_vars      - allocate global anisotropic background error
!                                       related variables
!   sub destroy_anberror_vars     - deallocate global anisotropic background error
!                                       related variables
!   sub create_anberror_vars_reg  - allocate regional anisotropic background error
!                                       related variables
!   sub anberror_vert_partition
!   sub destroy_anberror_vars_reg - deallocate regional anisotropic background error
!                                       related variables
!   sub anberror_vert_partition_subdomain_option
!   sub halo_update_reg0
!   sub halo_update_reg
!
! Variable Definitions:
!   def anisotropic - if true, then use anisotropic background error\
!   def ancovmdl    - covariance model settings - 0: pt-based, 1: ensemble-based
!   def indices     - rf-indices for zonal patch
!   def indices_p   - rf-indices for polar patches
!   def idvar       - used by anisotropic filter code
!   def jdvar       -  to apply filter simultaneously
!   def kvar_start  -  to all variables, when stored
!   def kvar_end    -  in horizontal slab mode.
!   def var_names   -  descriptive names of variables
!   def clenmax     - max allowable correlation length in grid units
!   def clenmaxi    - 1/clenmax
!   def smooth_len  - length scale for horizontal and vertical smoothing
!                     of background, in analysis grid units.  The smoothed
!                     background is used in a variety of ways to define
!                     anisotropic error correlations.
!   def triad4      - for 2d variables, if true, use blended triad algorithm
!   def ifilt_ord   - filter order for anisotropic filters
!   def npass       - 2*npass = number of factors in background error
!   def normal      - number of random vectors to use for filter normalization
!                       ( if < 0 then slightly slower, but results independent of
!                         number of processors)
!   def binom       - if true, weight correlation lengths of factors using binomial
!                      distribution, with shortest scales on outside, longest scales
!                      on inside.  This can help to produce smoother correlations in the
!                      presence of strong anisotrophy
!   def ngauss      - number of gaussians to add together in each factor
!   def rgauss      - multipliers on reference aspect tensor for each gaussian factor
!   def an_amp      - multiplying factors on reference background error variances
!   def an_vs       - scale factor for background error vertical scales (temporary carry over from
!                      isotropic inhomogeneous option)
!   def grid_ratio  - ratio of coarse to fine grid, in fine grid units (coarse grid
!                      used when applying recursive filter)
!   def an_flen_u   - coupling parameter for connecting horizontal wind to background error
!   def an_flen_t   - coupling parameter for connecting potential temperature gradient to background error
!   def an_flen_z   - coupling parameter for connecting terrain gradient to background error
!   def rtma_subdomain_option - if true, then apply recursive filters in subdomain space
!                                (currently constructed to work only when running 2d analysis
!                                 and analysis and filter grid are the same)
!   def rtma_bkerr_sub2slab - for rtma, if rtma_subdomain_option=T, and rtma_bkerr_sub2slab=T,
!                               then in anbkerror, use following sequence of
!                               operations:
!                                   sub2grid
!                                   tfgrid2agrid
!                                   raf4,
!                                   raf4_ad
!                                   fgrid2agrid
!                                   gridsub
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,r_single,i_kind,i_long,r_double
  use constants, only:  zero,half,one,two,three,four
  use raflib, only: filter_cons, filter_indices
  use berror, only: qvar3d
  use gridmod, only: lat2,lon2,nsig
  use fgrid2agrid_mod, only: fgrid2agrid_parm
  use control_vectors, only: nvars
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! set default to private
  private
! set modules to public
  public :: init_anberror
  public :: create_anberror_vars
  public :: destroy_anberror_vars
  public :: create_anberror_vars_reg
  public :: anberror_vert_partition
  public :: destroy_anberror_vars_reg
  public :: anberror_vert_partition_subdomain_option
  public :: halo_update_reg0
  public :: halo_update_reg
! set passed variables to public
  public :: pf2aP1,pf2aP2,pf2aP3,nx,ny,mr,nr,nf,rtma_subdomain_option
  public :: s2g_rff
  public :: rtma_bkerr_sub2slab
  public :: nsmooth,nsmooth_shapiro,indices,indices_p,ngauss,filter_all,filter_p2,filter_p3
  public :: kvar_start,kvar_end,var_names,levs_jdvar,anisotropic
  public :: idvar,triad4,ifilt_ord,npass,normal,binom,rgauss,anhswgt,an_amp,an_vs
  public :: ancovmdl,covmap,lreadnorm,afact0,smooth_len,jdvar
  public :: an_flen_t,an_flen_z,an_flen_u,grid_ratio,grid_ratio_p

  integer(i_kind),parameter:: max_ngauss=20

  logical anisotropic
  integer(i_kind):: ancovmdl

  integer(i_kind) nx,ny,mr,nr,nf

  type(filter_indices):: indices
  type(filter_indices):: indices_p

  integer(i_kind),allocatable::idvar(:),jdvar(:),kvar_start(:),kvar_end(:),levs_jdvar(:)
  character(80),allocatable::var_names(:)
  real(r_kind) clenmax,clenmaxi,smooth_len
  type(filter_cons),save:: filter_all(7), filter_p2(7), filter_p3(7)
!                          for full/zonal_patch, and polar_patches
  logical triad4,binom
  integer(i_long) ifilt_ord,npass,ngauss,normal,nsmooth,nsmooth_shapiro
  real(r_kind):: anhswgt(max_ngauss)
  real(r_double) rgauss(max_ngauss)
  real(r_double),allocatable,dimension(:,:) :: an_amp
  real(r_double) an_vs
  real(r_kind) grid_ratio, grid_ratio_p
  real(r_double) an_flen_u,an_flen_t,an_flen_z
  type(fgrid2agrid_parm):: pf2aP1,pf2aP2,pf2aP3
  type(sub2grid_info):: s2g_rff
!                          for full/zonal_patch, and polar_patches
  real(r_kind),allocatable:: afact0(:)
  logical:: covmap         ! flag for covariance map
  logical:: lreadnorm      ! flag for normalization operation

!--- for subdomain option
  logical rtma_subdomain_option
  logical rtma_bkerr_sub2slab
  integer(i_kind),allocatable:: nrecv_halo(:),ndrecv_halo(:),nsend_halo(:),ndsend_halo(:)
  integer(i_kind) nrecv_halo_loc,nsend_halo_loc
  integer(i_kind),allocatable:: info_send_halo(:,:),info_recv_halo(:,:)

contains

  subroutine init_anberror
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_anberror    set constants for anisotropic background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: intializes extra constants needed for the anisotropic
!               global mode background error
!
! program history log:
!   2005-02-08  parrish
!   2008-11-03  sato - update for global mode
!   2010-06-05  todling - an_amp0 moved to control_vectors
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

    use raflib, only: set_indices
    implicit none

    integer(i_kind) k

    anisotropic=.false.
    ancovmdl=0
    clenmax=120.0_r_kind
    clenmaxi=one/clenmax
    smooth_len=four

    call set_indices(indices,   0,0,0,0,0,0,0,0,0,0,0,0)
    call set_indices(indices_p, 0,0,0,0,0,0,0,0,0,0,0,0)

!   allocate filter_all:

    do k=1,7

       allocate(filter_all(k)%istart(2),filter_all(k)%ib(2))
       allocate(filter_all(k)%nrecv(2),filter_all(k)%ndrecv(2))
       allocate(filter_all(k)%nsend(2),filter_all(k)%ndsend(2))
       allocate(filter_all(k)%lnf(2,2,2,2),filter_all(k)%bnf(2,2,2))
       allocate(filter_all(k)%amp(2,2,2,2),filter_all(k)%ia(2))
       allocate(filter_all(k)%ja(2),filter_all(k)%ka(2))

       allocate(filter_p2(k)%istart(2),filter_p2(k)%ib(2))
       allocate(filter_p2(k)%nrecv(2),filter_p2(k)%ndrecv(2))
       allocate(filter_p2(k)%nsend(2),filter_p2(k)%ndsend(2))
       allocate(filter_p2(k)%lnf(2,2,2,2),filter_p2(k)%bnf(2,2,2))
       allocate(filter_p2(k)%amp(2,2,2,2),filter_p2(k)%ia(2))
       allocate(filter_p2(k)%ja(2),filter_p2(k)%ka(2))

       allocate(filter_p3(k)%istart(2),filter_p3(k)%ib(2))
       allocate(filter_p3(k)%nrecv(2),filter_p3(k)%ndrecv(2))
       allocate(filter_p3(k)%nsend(2),filter_p3(k)%ndsend(2))
       allocate(filter_p3(k)%lnf(2,2,2,2),filter_p3(k)%bnf(2,2,2))
       allocate(filter_p3(k)%amp(2,2,2,2),filter_p3(k)%ia(2))
       allocate(filter_p3(k)%ja(2),filter_p3(k)%ka(2))

    end do

!   set other parameters to default values

    npass=1
    ifilt_ord=4_i_long
    triad4=.true.
    binom=.true.
    normal=-200_i_long
    nsmooth=0
    nsmooth_shapiro=0
    ngauss=3_i_long
    rgauss=zero
    an_vs=one
    grid_ratio=two
    grid_ratio_p=zero
    an_flen_u=-one      ! this turns off anisotropic coupling to horizontal wind
    an_flen_t=-one      ! this turns off anisotropic coupling to grad(pot temp)
    an_flen_z=-one      ! this turns off anisotropic coupling to grad(terrain)
    rgauss(1)=half
    rgauss(2)=one
    rgauss(3)=two
    anhswgt(:)=one

    rtma_subdomain_option=.false.
    rtma_bkerr_sub2slab=.false.

    allocate(afact0(nvars))
    afact0=zero
    covmap=.false.
    lreadnorm=.false.

  end subroutine init_anberror


  subroutine create_anberror_vars(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_anberror_vars  create arrays for anisotropic global background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: creates arrays for anisotropic global background error
!
! program history log:
!   2004-01-01  parrish
!   2008-11-03  sato - add actual procedures
!
!   input argument list:
!    mype     - mpi task id!
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

    use fgrid2agrid_mod, only: create_fgrid2agrid
    use jfunc, only: nrclen,nclen
    use berror, only: varprd,vprecond,bnf=>nf,bnr=>nr
    use gridmod, only: nlat,nlon
    implicit none

    integer(i_kind),intent(in   ) :: mype

    allocate(varprd(max(1,nrclen)))
    allocate(vprecond(nclen))
    allocate(an_amp(max_ngauss,nvars))
    an_amp=one/three

!  compute vertical partition variables used by anisotropic filter code

    call anberror_vert_partition(mype)

! Grid constant to transform to 3 pieces

    nx=nlon*3/2
    nx=nx/2*2
    ny=nlat*8/9
    ny=ny/2*2
    if(mod(nlat,2)/=0) ny=ny+1
    mr=0
    nr=nlat/4
    nf=nr
    bnr=nr
    bnf=nf

!   initialize fgrid2agrid interpolation constants

    pf2aP1%grid_ratio=grid_ratio
    pf2aP1%nlona     =nx
    pf2aP1%nlata     =ny
    call create_fgrid2agrid(pf2aP1)
    if(mype==0) then
       write(6,*) 'set up pf2aP1', &
         pf2aP1%nlona,pf2aP1%nlata, &
         pf2aP1%nlonf,pf2aP1%nlatf
    end if

    indices%ids=1 ; indices%ide=pf2aP1%nlatf
    indices%jds=1 ; indices%jde=pf2aP1%nlonf
    indices%ips=indices%ids ; indices%ipe=indices%ide
    indices%jps=indices%jds ; indices%jpe=indices%jde

    if( grid_ratio_p > zero ) then
       pf2aP2%grid_ratio=grid_ratio_p
    else
       pf2aP2%grid_ratio=grid_ratio
    end if
    pf2aP2%nlona     = nf*2+1
    pf2aP2%nlata     = nf*2+1
    call create_fgrid2agrid(pf2aP2)
    if(mype==0) then
       write(6,*) 'set up pf2aP2', &
         pf2aP2%nlona,pf2aP2%nlata, &
         pf2aP2%nlonf,pf2aP2%nlatf
    end if

    pf2aP3%grid_ratio=pf2aP2%grid_ratio
    pf2aP3%nlona     =pf2aP2%nlona
    pf2aP3%nlata     =pf2aP2%nlata
    call create_fgrid2agrid(pf2aP3)
    if(mype==0) then
       write(6,*) 'set up pf2aP3', &
         pf2aP3%nlona,pf2aP3%nlata, &
         pf2aP3%nlonf,pf2aP3%nlatf
    end if

    indices_p%ids=1 ; indices_p%ide=pf2aP3%nlatf
    indices_p%jds=1 ; indices_p%jde=pf2aP3%nlonf
    indices_p%ips=indices_p%ids ; indices_p%ipe=indices_p%ide
    indices_p%jps=indices_p%jds ; indices_p%jpe=indices_p%jde

    allocate(qvar3d(lat2,lon2,nsig))

  end subroutine create_anberror_vars


  subroutine destroy_anberror_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_anberror_vars  deallocates anisotropic global background error arrays
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: deallocates global anisotropic background error arrays
!
! program history log:
!   2005-02-08  parrish
!   2007-09-04  sato - add actual procedures
!   2010-06-01  zhu  - add vprecond deallocate
!   2010-06-05  todling - add an_amp
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use fgrid2agrid_mod, only: destroy_fgrid2agrid
    use berror, only: vprecond
    implicit none

    deallocate(an_amp)
    deallocate(afact0)
    deallocate(qvar3d)
    deallocate(vprecond)

    call destroy_fgrid2agrid(pf2aP1)
    call destroy_fgrid2agrid(pf2aP2)
    call destroy_fgrid2agrid(pf2aP3)

  end subroutine destroy_anberror_vars


  subroutine create_anberror_vars_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_anberror_vars_reg  create arrays for anisotropic reg background error
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: creates arrays for regional background error
!
! program history log:
!   2005-02-08  parrish
!   2005-05-24  pondeca - take into consideration that nrclen=0 for 2dvar only
!                         surface analysis option!
!   2015-05-02  parrish - add general_sub2grid type(sub2grid_info) s2g_rff-- 
!                           similar to s2g_raf, but for filter grid which 
!                           will be coarser than analysis grid to reduce run time.
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use fgrid2agrid_mod, only: create_fgrid2agrid
    use jfunc, only: nrclen,nclen
    use berror, only: varprd,vprecond
    use gridmod, only: nlat,nlon,istart,jstart
    use general_commvars_mod, only: s2g_raf
    use general_sub2grid_mod, only: general_sub2grid_create_info
    implicit none

    integer(i_kind),intent(in   ) :: mype
    logical regional

    allocate(varprd(max(1,nrclen)))
    allocate(vprecond(nclen))
    allocate(an_amp(max_ngauss,nvars))
    an_amp=one/three

!   initialize fgrid2agrid interpolation constants
  ! if(rtma_subdomain_option) grid_ratio=one   ! commented out to allow dual resolution

    pf2aP1%grid_ratio=grid_ratio
    pf2aP1%nlata=nlat
    pf2aP1%nlona=nlon
    call create_fgrid2agrid(pf2aP1)

!    as alternative to indices, for use with rtma_bkerr_sub2slab, set up
!     s2g_rff, which is equivalent to s2g_raf, but for filter grid.

    regional=.true.
    call general_sub2grid_create_info(s2g_rff,s2g_raf%inner_vars, &
                   pf2aP1%nlatf,pf2aP1%nlonf,s2g_raf%nsig,s2g_raf%num_fields,regional)

!   compute vertical partition variables used by anisotropic filter code
    if(rtma_subdomain_option) then
       call halo_update_reg0(mype)
       call anberror_vert_partition_subdomain_option(mype)

       indices%ids=1 ; indices%ide=pf2aP1%nlatf
       indices%jds=1 ; indices%jde=pf2aP1%nlonf

!   following without halo

       indices%ips=max(indices%ids,min(istart(mype+1)              ,indices%ide))
       indices%ipe=max(indices%ids,min(lat2+istart(mype+1)-3,indices%ide))
       indices%jps=max(indices%jds,min(jstart(mype+1)              ,indices%jde))
       indices%jpe=max(indices%jds,min(lon2+jstart(mype+1)-3,indices%jde))

    else

       call anberror_vert_partition(mype)

       indices%ids=1        ; indices%ide=pf2aP1%nlatf
       indices%jds=1        ; indices%jde=pf2aP1%nlonf
       indices%ips=indices%ids ; indices%ipe=indices%ide
       indices%jps=indices%jds ; indices%jpe=indices%jde

    end if

    allocate(qvar3d(lat2,lon2,nsig))

  end subroutine create_anberror_vars_reg


  subroutine anberror_vert_partition(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    anberror_vert_partition
!
!   prgrmmr:
!
! abstract:      using existing vertical ordering of variables, create
!                modified indexing compatable with anisotropic filter code.
!
! program history log:
!   2008-06-05 safford - add subprogram doc block
!   2008-12-10 zhu     - add changes with nrf* for generalized control variables
!                      - use vlevs from gridmod
!   2010-05-28 todling - obtain variable id's on the fly (add getindex)
!   2015-06-09  pondeca - remove reference to stl and sti, which are now
!   handled more generally as motley control variales
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use gridmod, only: nsig1o,vlevs
    use mpimod, only: levs_id,nvar_id,npe,ierror,mpi_comm_world, &
           mpi_max,mpi_integer4
    use control_vectors, only: nrf_var
    use mpeu_util, only: getindex
    use gsi_io, only: verbose
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) idvar_last,k,kk
    integer(i_kind) nlevs0(0:npe-1),nlevs1(0:npe-1),nvar_id0(nsig1o*npe),nvar_id1(nsig1o*npe)
    logical print_verbose

    print_verbose = .false.
    if(verbose)print_verbose=.true.
    indices%kds=  1 ; indices%kde=vlevs
    indices_p%kds=1 ; indices_p%kde=vlevs

!  initialize idvar,kvar_start,kvar_end
! Determine how many vertical levels each mpi task will
! handle in the horizontal smoothing
    allocate(idvar(indices%kds:indices%kde),jdvar(indices%kds:indices%kde),kvar_start(nvars),kvar_end(nvars))
    allocate(var_names(nvars))
    do k=1,nvars
       var_names(k)=nrf_var(k)
    end do

!                     idvar  jdvar
!                       1      1       stream function
!                       2      2       velocity potential
!                       3      3       surface pressure
!                       4      4       virtual temperature
!                       5      5       specific humidity
!                       6      6       ozone
!                       7      7       sst
!                      10      8       cloud water
!                       8      9       surface temp (land)
!                       9     10       surface temp (ice)

    nlevs0=0
    do k=1,nsig1o
       if(levs_id(k)/=0) nlevs0(mype)=nlevs0(mype)+1
       if(k==1.or.k>=nsig1o-2_i_long .and. print_verbose) write(6,*)' k,levs_id(k)=',k,levs_id(k)
    end do

    call mpi_allreduce(nlevs0,nlevs1,npe,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    nvar_id0=0
    do k=1,nsig1o
       nvar_id0(mype*nsig1o+k)=nvar_id(k)
    end do
    call mpi_allreduce(nvar_id0,nvar_id1,npe*nsig1o,mpi_integer4,mpi_max,mpi_comm_world,ierror)

    kk=0
    do k=1,npe*nsig1o
       if(nvar_id1(k)>0) then
          kk=kk+1
          jdvar(kk)=nvar_id1(k)
       end if
    end do
    idvar_last=0
    kk=0
    do k=indices%kds,indices%kde
       if(jdvar(k)/=idvar_last) then
          idvar_last=jdvar(k)
          kk=kk+1
       end if
       idvar(k)=kk
    end do
    idvar_last=0
    do k=indices%kds,indices%kde
       if(idvar(k)/=idvar_last) then
          idvar_last=idvar(k)
          kvar_start(idvar_last)=k
       end if
    end do
    idvar_last=0
    do k=indices%kde,indices%kds,-1
       if(idvar(k)/=idvar_last) then
          idvar_last=idvar(k)
          kvar_end(idvar_last)=k
       end if
    end do

    if(mype==0 .and. print_verbose) then
       do k=indices%kds,indices%kde
          write(6,*)' in anberror_vert_partition, k,idvar(k),jdvar(k)=',k,idvar(k),jdvar(k)
       end do
       do k=1,nvars
          write(6,*)' k,kvar_start,end(k)=',k,kvar_start(k),kvar_end(k)
       end do
    end if
    indices%kpe=0
    indices_p%kpe=0
    do k=0,mype
       indices%kpe  =indices%kpe  +nlevs1(k)
       indices_p%kpe=indices_p%kpe+nlevs1(k)
    end do
    indices%kps=indices%kpe-nlevs1(mype)+1
    indices_p%kps=indices_p%kpe-nlevs1(mype)+1
    if(print_verbose)write(6,*)' in anberror_vert_partition, kps,kpe=',indices%kps,indices%kpe

  end subroutine anberror_vert_partition


  subroutine destroy_anberror_vars_reg
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_anberror_vars_reg  deallocate reg anisotropic
!                                           background error arrays
!   prgmmr: parrish          org: np23                date: 2005-02-08
!
! abstract: deallocates regional anisotropic background error arrays
!
! program history log:
!   2005-02-08  parrish
!   2007-08-21  pondeca - add qvar3d deallocate
!   2008-06-05  safford - rm unused var
!   2010-06-01  zhu  - add vprecond deallocate
!   2010-06-05  todling - add an_amp
!   2015-05-02  parrish - add general_sub2grid_destroy_info to remove s2g_rff
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

    use fgrid2agrid_mod, only: destroy_fgrid2agrid
    use berror, only: vprecond
    use general_sub2grid_mod, only: general_sub2grid_destroy_info
    implicit none

    deallocate(an_amp)
    deallocate(afact0)
    deallocate(qvar3d)
    deallocate(vprecond)
    call destroy_fgrid2agrid(pf2aP1)
    call general_sub2grid_destroy_info(s2g_rff)
!write(6,'(" FOR TEST ONLY--REMOVE THIS MESSAGE BEFORE FINAL COMMIT--SUCCESSFUL CALL TO general_sub2grid_destroy_info to remove s2g_rff in destroy_anberror_vars_reg")')

  end subroutine destroy_anberror_vars_reg


  subroutine anberror_vert_partition_subdomain_option(mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    anberror_vert_partition_subdomain_option
!
!   prgrmmr:
!
! abstract:      using existing vertical ordering of variables, create
!                modified indexing compatable with anisotropic filter code.
!
! program history log:
!   2008-06-05  safford -- add subprogram doc block
!   2008-12-10  zhu - add changes with nrf_var,nrf* for generalized control variables
!                   - use vlevs from gridmod
!   2010-05-25  todling - move levb, leve from jfunc here (only used here)
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-12-05  pondeca - add initialization of kvar_start, kvar_end, and levs_jdvar
!                         for nrf2_sst>0 
!   2013-04-04  pondeca - remove reference to stl and sti, which are now
!   handled more generally as motley control variales
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use gridmod, only: nsig,vlevs
    use control_vectors, only: nrf_var,nrf,nrf_3d,mvars
    use mpeu_util, only: getindex
    use gsi_io, only: verbose
    implicit none

    integer(i_kind),intent(in   ) :: mype

    integer(i_kind) n,k,kk,klevb,kleve

    integer(i_kind),allocatable,dimension(:):: nrf_levb,nrf_leve
    logical print_verbose

    print_verbose=.false.
    if(verbose)print_verbose=.true.
    indices%kds=1        ; indices%kde=vlevs
    indices%kps=indices%kds ; indices%kpe=indices%kde

!  initialize nvars,idvar,kvar_start,kvar_end
! Determine how many vertical levels each mpi task will
! handle in the horizontal smoothing
    allocate(idvar(indices%kds:indices%kde),jdvar(indices%kds:indices%kde),kvar_start(nvars),kvar_end(nvars))
    allocate(var_names(nvars),levs_jdvar(indices%kds:indices%kde))
    allocate(nrf_levb(nrf),nrf_leve(nrf))
    do k=1,nvars
       var_names(k)=nrf_var(k)
    end do

! initialize level pointer to each control variable
    klevb=1
    if (nrf_3d(1)) then
       kleve=klevb+nsig-1
    else
       kleve=klevb
    end if
    nrf_levb(1)=klevb
    nrf_leve(1)=kleve
    do n=2,nrf
       klevb=nrf_leve(n-1)+1
       if (nrf_3d(n)) then
          kleve=klevb+nsig-1
       else
          kleve=klevb
       end if
       nrf_levb(n)=klevb
       nrf_leve(n)=kleve
    end do

    do n=1,nrf
       kvar_start(n)=nrf_levb(n) 
       kvar_end(n)  =nrf_leve(n)
       idvar(kvar_start(n):kvar_end(n))=n
    end do
    
    do n=nrf+1,nrf+mvars
       kvar_start(n)=kvar_end(n-1)+1
       kvar_end(n)=kvar_start(n)           !motley variables are all 2d, at least for now
       idvar(kvar_start(n):kvar_end(n))=n
    end do
    jdvar=idvar

    kk=0
    do n=1,nrf
       if (nrf_3d(n)) then
          do k=1,nsig
             kk=kk+1
             levs_jdvar(kk)=k
          end do
       else
          kk=kk+1
          levs_jdvar(kk)=1
       end if
    end do

    do n=nrf+1,nrf+mvars
       kk=kk+1
       levs_jdvar(kk)=1
    end do

    deallocate(nrf_levb,nrf_leve)

    if(mype==0 .and. print_verbose) then
       do k=indices%kds,indices%kde
          write(6,*)' in anberror_vert_partition_subdomain_option, k,idvar,jdvar,levs_jdvar=', &
                      k,idvar(k),jdvar(k),levs_jdvar(k)
       end do
       do k=1,nvars
          write(6,*)' k,kvar_start,end(k)=',k,kvar_start(k),kvar_end(k)
       end do
       write(6,*)' in anberror_vert_partition_subdomain_option, kps,kpe=',indices%kps,indices%kpe
    end if

  end subroutine anberror_vert_partition_subdomain_option


subroutine halo_update_reg0(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    halo_update_reg0
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-15  lueken - added subprogram doc block
!
!   input argument list:
!    mype   - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: lat2,lon2,istart,jstart,nlat,nlon
  use mpimod, only: npe,mpi_integer4,mpi_sum,mpi_comm_world,ierror
  use raflib, only: indexxi4
  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,ii,j,mm1,mpe,iglob,jglob,mpi_string1
  integer(i_kind) ijglob_pe(nlat,nlon),ijglob_pe0(nlat,nlon)
  integer(i_kind) iorigin(3*(lat2+lon2)),indx(3*(lat2+lon2)),iwork(3*(lat2+lon2))

  allocate(nrecv_halo(0:npe-1),ndrecv_halo(0:npe),nsend_halo(0:npe-1),ndsend_halo(0:npe))
  allocate(info_send_halo(2,3*(lat2+lon2)),info_recv_halo(2,3*(lat2+lon2)))


  if(npe==1) return
  mm1=mype+1

  ijglob_pe0=0
  do j=2,lon2-1
     jglob=j+jstart(mm1)-2_i_long
     do i=2,lat2-1
        iglob=i+istart(mm1)-2_i_long
        ijglob_pe0(iglob,jglob)=mype
     end do
  end do
  call mpi_allreduce(ijglob_pe0,ijglob_pe,nlat*nlon,mpi_integer4,mpi_sum,mpi_comm_world,ierror)

!  create list of all points to be received with global i,j coordinates
  ii=0
  nrecv_halo=0
                      !ierror=0
  do j=1,lon2,lon2-1
     jglob=j+jstart(mm1)-2_i_long
     if(jglob<1.or.jglob>nlon) cycle
     do i=1,lat2
        iglob=i+istart(mm1)-2_i_long
        if(iglob<1.or.iglob>nlat) cycle
        ii=ii+1
        info_recv_halo(1,ii)=iglob ; info_recv_halo(2,ii)=jglob
        iorigin(ii)=ijglob_pe(iglob,jglob)
        nrecv_halo(ijglob_pe(iglob,jglob))=nrecv_halo(ijglob_pe(iglob,jglob))+1
                        ! if(iorigin(ii)==mype) ierror=ierror+1
     end do
  end do
  do i=1,lat2,lat2-1
     iglob=i+istart(mm1)-2_i_long
     if(iglob<1.or.iglob>nlat) cycle
     do j=2,lon2-1                                ! already have corner points
        jglob=j+jstart(mm1)-2_i_long
        if(jglob<1.or.jglob>nlon) cycle
        ii=ii+1
        info_recv_halo(1,ii)=iglob ; info_recv_halo(2,ii)=jglob
        iorigin(ii)=ijglob_pe(iglob,jglob)
        nrecv_halo(ijglob_pe(iglob,jglob))=nrecv_halo(ijglob_pe(iglob,jglob))+1
                        ! if(iorigin(ii)==mype) ierror=ierror+1
     end do
  end do

  ndrecv_halo(0)=0
  do mpe=1,npe
     ndrecv_halo(mpe)=ndrecv_halo(mpe-1)+nrecv_halo(mpe-1)
  end do

  call mpi_alltoall(nrecv_halo,1,mpi_integer4,nsend_halo,1,mpi_integer4,mpi_comm_world,ierror)
  ndsend_halo(0)=0
  do mpe=1,npe
     ndsend_halo(mpe)=ndsend_halo(mpe-1)+nsend_halo(mpe-1)
  end do
  nsend_halo_loc=ndsend_halo(npe)
  nrecv_halo_loc=ndrecv_halo(npe)

!   sort origin pe numbers from smallest to largest
  if(ii>0) then
     call indexxi4(ii,iorigin,indx)

!     use sort index to reorder
     do j=1,2
        do i=1,ii
           iwork(i)=info_recv_halo(j,indx(i))
        end do
        do i=1,ii
           info_recv_halo(j,i)=iwork(i)
        end do
     end do
  end if

  call mpi_type_contiguous(2,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_recv_halo,nrecv_halo,ndrecv_halo,mpi_string1, &
                     info_send_halo,nsend_halo,ndsend_halo,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

!   convert info arrays back to local coordinate units

  do i=1,nsend_halo_loc
     iglob=info_send_halo(1,i)
     info_send_halo(1,i)=iglob-istart(mm1)+2_i_long
     jglob=info_send_halo(2,i)
     info_send_halo(2,i)=jglob-jstart(mm1)+2_i_long
  end do

  do i=1,nrecv_halo_loc
     iglob=info_recv_halo(1,i)
     info_recv_halo(1,i)=iglob-istart(mm1)+2_i_long
     jglob=info_recv_halo(2,i)
     info_recv_halo(2,i)=jglob-jstart(mm1)+2_i_long
  end do

end subroutine halo_update_reg0


subroutine halo_update_reg(f,nvert)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    halo_update_reg
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-15  lueken - added subprogram doc block
!
!   input argument list:
!    nvert
!    f
!
!   output argument list:
!    f
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: lat2,lon2
  use mpimod, only: npe,mpi_rtype,mpi_comm_world,ierror
  implicit none

  integer(i_kind),intent(in   ) :: nvert
  real(r_kind)   ,intent(inout) :: f(lat2,lon2,nvert)

  integer(i_kind) i,k,mpi_string2
  real(r_kind) bufsend(nvert,nsend_halo_loc),bufrecv(nvert,nrecv_halo_loc)

  if(npe==1) return

!   now gather up points to send
  do i=1,nsend_halo_loc
     do k=1,nvert
        bufsend(k,i)=f(info_send_halo(1,i),info_send_halo(2,i),k)
     end do
  end do
  call mpi_type_contiguous(nvert,mpi_rtype,mpi_string2,ierror)
  call mpi_type_commit(mpi_string2,ierror)
  call mpi_alltoallv(bufsend,nsend_halo,ndsend_halo,mpi_string2, &
                     bufrecv,nrecv_halo,ndrecv_halo,mpi_string2,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string2,ierror)
!   finally distribute points back
  do i=1,nrecv_halo_loc
     do k=1,nvert
        f(info_recv_halo(1,i),info_recv_halo(2,i),k)=bufrecv(k,i)
     end do
  end do

end subroutine halo_update_reg

end module anberror
