!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
module anisofilter_glb
!$$$   module documentation block
!                .      .    .                                       .
! module:    anisofilter_glb
! prgmmr: sato             org: np23                date: 2007-10-30
!
! abstract:  computes the anisotropic aspect tensor of the background
!            error auto-correlation model for global mode.
!            Three filter spaces (Zonal, North & South Polar patches)
!            are prepared here.
!            This module is developed as a sub-module of anisofilter.
!
!            Caution : Not considering very long correlation length.
!                      There must be discontinuity problem between
!                      each patches at the stratospehre.
!
! program history log:
!   2007-10-30  sato
!   2007-11-06  sato : add anbkgvar_rewgt (interface to bkgvar_rewgt)
!   2007-11-16  sato : move theta based aspect part to a subroutine.
!   2007-12-14  sato : make some procedures to subroutine
!   2007-12-20  sato : add ensemble based aspect subroutines.
!   2008-02-27  sato - change iso-aniso composition in get_aspect_reg_ens.
!                      enable to use iensamp. delete some test code.
!   2010-03-31  treadon - replace specmod components with sp_a structure
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-06-05  todling - an_amp0 coming from control_vectors
!   2013-10-19  todling - metguess now holds background
!   2013-10-24  todling - general interface to strip
!   2013-10-25  todling - reposition ltosi and others to commvars
!
! subroutines included:
!   anprewgt                    - compute the anisotropic aspect tensor for the
!                                 3dvar case of gsi-regional
!   get_stat_factk              -
!
!   create_iso_array_glb        - allocate array for isotropic aspect tensors
!
!   destroy_iso_array_glb       - deallocate the array
!
!   init_anisofilter            - initialize anisotropic background error
!                                 related variables
!   read_bckgstats_glb          - read in background error statistics
!
!   get_background_glb          - compute smoothed versions of the background fields
!                                 and respective vertical derivatives on filter grid
!   raf_sm_glb                  -
!
!   get_aspect_pt               - compute aspect tensors based on grad(pt)
!
!   get_theta_corrl_lenghts_glb - compute function correlation length-scales of
!                                 Riishojgaard-type anisotropic auto-correlation
!                                 model based on the background potential temperature
!   isotropic_scales_glb        - compute isotropic length-scales of
!                                 auto-correlation model
!   anbkgvar_rewgt              - interface to bkgvar_rewgt
!
!   get_aspect_ens              - compute aspect tensors based on ensemble info
!
!   add_aniso_effect            - utility for get_aspect_ens
!   add_aniso_effect_jim        - utility for get_aspect_ens
!   add_aniso_effect_sty        - utility for get_aspect_ens
!   add_ensinfo_aniasp          - utility for get_aspect_ens
!   mk_ens_aniasp               - utility for get_aspect_ens
!
!   get_avgensv                 -
!
!   get_ensmber_glb             - get one ensemble information
!   ens_intpglb_coeff           - prepare interpolation coefficients from GRIB grid
!                                 to analysis grid
!   ens_intpglb                 - perform interpolation
!   ens_uv2psichi               - convert (u,v) into (psi,chi)
!   reload_single               - reshape the array data
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single,r_double,i_long
  use anberror, only: indices,indices_p,&
                      idvar,kvar_start,kvar_end, &
                      var_names, &
                      filter_all,pf2aP1, &
                      filter_p2, pf2aP2, &
                      filter_p3, pf2aP3, &
                      triad4,ifilt_ord,npass,normal,binom, &
                      ngauss,rgauss,anhswgt,an_amp,an_vs, &
                      ancovmdl, covmap, &
                      nsmooth,nsmooth_shapiro

  use gridmod, only: nsig,nsig1o,nlon,nlat, &
                     lat2,lon2, &
                     itotsub,lon1,lat1,&
                     displs_s,displs_g,ijn_s,ijn,strip

  use general_commvars_mod, only: ltosi_s,ltosj_s

  use constants, only: zero_single, tiny_single,            & ! for real(4)
                       zero,        tiny_r_kind, half, one, two, & ! for real(8)
                       rd_over_cp, rearth_equator, r100

  use raflib,only: init_raf4_wrap,raf_sm4_wrap,raf_sm4_ad_wrap

  use jfunc, only: varq,qoption,varcw,cwoption

  use control_vectors, only: an_amp0
  use control_vectors, only: cvars2d,cvars3d,cvarsmd
  use control_vectors, only: nrf_var,nrf_3d,nrf,nvars,nrf2_loc,nrf3_loc
  use control_vectors, only: nrf3 => nc3d
  use control_vectors, only: nrf2 => nc2d

  use guess_grids, only: ges_prsl,ntguessig,ges_tsen

  use mpimod, only: npe,levs_id,nvar_id,ierror,&
                    mpi_real8,mpi_real4,mpi_integer4,&
                    mpi_sum,mpi_comm_world

  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  use anisofilter, only: lreadnorm, &
                         r015, &
                         qlth_temp0, qltv_temp0, qlth_wind0, qltv_wind0, &
                         scalex1, scalex2, scalex3, &
                         stpcode_alloc,   stpcode_namelist, &
                         stpcode_ensdata, stpcode_statdata, &
                         fact_qopt2,mk_gradpt_slab,smther_one,hanning_smther, &
                         invert_aspect_tensor, &
                         mlat, ks, rfact0h, rfact0v, corz, corp, hwll, hwllp, vz, &
                         aspect, &
                         tx1_slab,tx2_slab,tx3_slab, &
                         asp1_max,asp2_max,asp3_max, &
                         theta0f,theta0zf,u0f,u0zf,v0f,v0zf,z0f,rh0f, &
                         asp10f,asp20f,asp30f,psg, &
                         qlth_temp, qltv_temp, &
                         qlth_wind, qltv_wind, &
                         EAMPMAX, EAMPMIN, &
                         pgesmin,pgesmax, &
                         mode_val
  use aniso_ens_util, only: pges_minmax


  use berror, only: bkgv_flowdep
  use mpeu_util, only: getindex
  use mpeu_util, only: die

  implicit none

! set default to private
  private
! set subroutines to public
  public :: anprewgt
  public :: get_stat_factk
  public :: create_iso_array_glb
  public :: destroy_iso_array_glb
  public :: init_anisofilter
  public :: read_bckgstats_glb
  public :: get_background_glb
  public :: get_aspect_pt
  public :: get_theta_corrl_lenghts_glb
  public :: isotropic_scales_glb
  public :: anbkgvar_rewgt
  public :: get_aspect_ens
  public :: add_aniso_effect
  public :: add_aniso_effect_jim
  public :: add_aniso_effect_sty
  public :: add_ensinfo_aniasp
  public :: mk_ens_aniasp
  public :: get_avgensv
  public :: get_ensmber_glb
  public :: ens_intpglb_coeff
  public :: ens_intpglb
  public :: ens_uv2psichi
  public :: reload_single
! set passed variables to public
  public :: p2ilatf,p0ilatf,p3ilatf,p3ilatfm,p2ilatfm,rh3f,rh2f,ensamp0f,ensamp3f,ensamp2f

  integer,parameter:: kthres=38           ! level threshold to apply anisotropy
  integer,parameter:: opt_sclclb_glb=1    ! iso scale calibration option
                                          ! 0: isoscale=isoscale *rfact0(ikind)
                                          ! 1: isoscale=isoscale**rfact0(1)+rfact0(2)
  real(r_kind),parameter:: rmis  =-1.e+20_r_kind

  real(r_single),allocatable,dimension(:,:,:,:)::aspect_p2,aspect_p3

  real(r_single),allocatable,dimension(:,:,:)::tx1p2_slab,tx2p2_slab,tx3p2_slab
  real(r_single),allocatable,dimension(:,:,:)::tx1p3_slab,tx2p3_slab,tx3p3_slab

  real(r_kind),allocatable::asp1p2_max(:,:),asp2p2_max(:,:),asp3p2_max(:,:)
  real(r_kind),allocatable::asp1p3_max(:,:),asp2p3_max(:,:),asp3p3_max(:,:)

  real(r_single),allocatable,dimension(:,:,:)::theta2f,theta2zf,u2f,u2zf,v2f,v2zf,z2f,rh2f ! north polar patch
  real(r_single),allocatable,dimension(:,:,:)::theta3f,theta3zf,u3f,u3zf,v3f,v3zf,z3f,rh3f ! souch polar patch

  real(r_kind),allocatable,dimension(:,:):: asp12f,asp22f,asp32f ! aspect tensor for north polar patch
  real(r_kind),allocatable,dimension(:,:):: asp13f,asp23f,asp33f ! aspect tensor for south polar patch

  real(r_kind),allocatable,dimension(:,:):: corsst  ,hwllsst   ! original grid
  real(r_kind),allocatable,dimension(:,:):: corsst0f,hwllsst0f ! for zonal patch
  real(r_kind),allocatable,dimension(:,:):: corsst2f,hwllsst2f ! for north polar patch
  real(r_kind),allocatable,dimension(:,:):: corsst3f,hwllsst3f ! for souch polar patch

  real(r_kind),allocatable,dimension(:)  :: dxzf,dyzf,p0ilatf               ! distance info for zonal patch
  real(r_kind),allocatable,dimension(:,:):: dxpf,dypf,rlatf,p2ilatf,p3ilatf ! distance info for polar patch

  real(r_single),allocatable,dimension(:,:,:):: sfvar0f,vpvar0f,tvar0f ! for bkgv_flowdep
  real(r_single),allocatable,dimension(:,:,:):: sfvar2f,vpvar2f,tvar2f
  real(r_single),allocatable,dimension(:,:,:):: sfvar3f,vpvar3f,tvar3f
  real(r_single),allocatable,dimension(:,:)  :: psvar0f
  real(r_single),allocatable,dimension(:,:)  :: psvar2f
  real(r_single),allocatable,dimension(:,:)  :: psvar3f

  real(r_single),allocatable,dimension(:,:,:):: ensamp0f,ensamp2f,ensamp3f

  real(r_kind):: hwllsstmin, p2ilatfm, p3ilatfm

  integer(i_kind),parameter::nmax_ensfld=20

  real(r_kind),allocatable :: field_st(:,:,:)
  integer(i_kind) :: kens_p

  integer(i_kind) :: nrf3_cw,nrf3_q,nrf3_sf,nrf3_vp,nrf3_t,nrf3_oz
  integer(i_kind) :: nrf2_ps,nrf2_sst,nrf2_stl,nrf2_sti

  character(len=*),parameter::myname='anisofilter_glb'
!-------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!=======================================================================
subroutine anprewgt(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    anprewgt
! prgmmr: sato             org: np23                date: 2007-09-19
!
! abstract: setup everything for anisotropic global background
!           error (3dvar case)
!
! program history log:
!   2007-09-19  sato
!   2010-03-11  zhu - make changes for generalized control variable,
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
!$$$ end documentation block

  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind):: i,j,k,k1,kvar,ivar,idiagflg,igauss,istat
  real(r_kind):: factk,factor,anhswgtsum
  logical flowvar

  character(len= 4):: clun

  data idiagflg /0/

! Get indexes to required CV variables
  nrf3_oz   = getindex(cvars3d,'oz')
  nrf3_t    = getindex(cvars3d,'t')
  nrf3_sf   = getindex(cvars3d,'sf')
  nrf3_vp   = getindex(cvars3d,'vp')
  nrf3_q    = getindex(cvars3d,'q')
  nrf3_cw   = getindex(cvars3d,'cw')
  nrf2_ps   = getindex(cvars2d,'ps')
  nrf2_sst  = getindex(cvars2d,'sst')
  nrf2_stl  = getindex(cvarsmd,'stl')
  nrf2_sti  = getindex(cvarsmd,'sti')

  call init_anisofilter
  call read_bckgstats_glb(mype)
  call get_background_glb(mype)

  if(bkgv_flowdep) call anbkgvar_rewgt(mype)

!----------------------------------------------
! Makes anisotropic aspect array
!----------------------------------------------
  aspect   =zero_single
  aspect_p2=zero_single
  aspect_p3=zero_single

  if(ancovmdl==1) then
     if(mype==0) write(6,*) 'ens-based auto correlation model'
     call get_aspect_ens(mype)
  else
     if(mype==0) write(6,*) 'pt-based auto correlation model'
     call get_aspect_pt(mype)
  end if

!----------------------------------------------
! Invert aspect array to get true aspect tensor
!----------------------------------------------
  call invert_aspect_tensor(aspect   ,pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)
  call invert_aspect_tensor(aspect_p2,pf2aP2%nlatf,pf2aP2%nlonf,nsig1o)
  call invert_aspect_tensor(aspect_p3,pf2aP3%nlatf,pf2aP3%nlonf,nsig1o)

!----------------------------------------------
! Initialize anisotropic recursive filter
!----------------------------------------------
  if(mype==0) write(6,*)' in get3berr_reg, nlat,nlon,(nlatf,nlonf)*3=', &
                                               nlat,nlon, &
                                               pf2aP1%nlatf,pf2aP1%nlonf, &
                                               pf2aP2%nlatf ,pf2aP2%nlonf, &
                                               pf2aP3%nlatf ,pf2aP3%nlonf

  if(mype==0) write(6,*)' ids,ide=',indices%ids,indices%ide,indices_p%ids,indices_p%ide
  if(mype==0) write(6,*)' jds,jde=',indices%jds,indices%jde,indices_p%ids,indices_p%ide

  if(lreadnorm) normal=0

  call init_raf4_wrap(aspect,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_all, &
                     nsmooth,nsmooth_shapiro, &
                     nvars,idvar,kvar_start,kvar_end,var_names, &
                     indices,mype, npe)

  call init_raf4_wrap(aspect_p2,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_p2, &
                     nsmooth,nsmooth_shapiro, &
                     nvars,idvar,kvar_start,kvar_end,var_names, &
                     indices_p,mype, npe)

  call init_raf4_wrap(aspect_p3,triad4,ngauss,rgauss,npass,normal,binom,ifilt_ord,filter_p3, &
                     nsmooth,nsmooth_shapiro, &
                     nvars,idvar,kvar_start,kvar_end,var_names, &
                     indices_p,mype, npe)

!----------------------------------------------
! Outputs filter values
!----------------------------------------------
  if(idiagflg==1) then
     write(clun(1:4),'(i4.4)') mype
     open (94,file='fltnorm.dat_'//clun,form='unformatted')
     if(lreadnorm) then; read(94)  filter_all(1)%amp
     else;               write(94) filter_all(1)%amp
     endif
     close(94)

     open (94,file='fltnorm_p2.dat_'//clun,form='unformatted')
     if(lreadnorm) then; read(94)  filter_p2(1)%amp
     else;               write(94) filter_p2(1)%amp
     endif
     close(94)

     open (94,file='fltnorm_p3.dat_'//clun,form='unformatted')
     if(lreadnorm) then; read(94)  filter_p3(1)%amp
     else;               write(94) filter_p3(1)%amp
     endif
     close(94)
  end if

!----------------------------------------------
! keep original amplitude factor for qoption2 (used in compute_derived)
!----------------------------------------------
  if(qoption==2) then
     deallocate(filter_all(2)%amp,stat=istat)
     allocate(filter_all(2)%amp(ngauss,indices%ips:indices%ipe, &
                               &       indices%jps:indices%jpe, &
                               &       indices%kps:indices%kpe))
     filter_all(2)%amp=filter_all(1)%amp
     deallocate(filter_p2(2)%amp,stat=istat)
     allocate(filter_p2(2)%amp(ngauss,indices_p%ips:indices_p%ipe,&
                              &       indices_p%jps:indices_p%jpe,&
                                      indices_p%kps:indices_p%kpe))
     filter_p2(2)%amp=filter_p2(1)%amp
     deallocate(filter_p3(2)%amp,stat=istat)
     allocate(filter_p3(2)%amp(ngauss,indices_p%ips:indices_p%ipe,&
                              &       indices_p%jps:indices_p%jpe,&
                                      indices_p%kps:indices_p%kpe))
     filter_p3(2)%amp=filter_p3(1)%amp
  end if

!-----------------------------------------------
! Output sample auto correlations
!-----------------------------------------------
  if(covmap) call antest_maps0_glb(mype,theta0f,z0f,theta2f,z2f,theta3f,z3f)

!----------------------------------------------
!  filter normalized to unit amplitude.
!  Now adjust amplitude to correspond to desired error amplitude.
! (an_amp are input tuneable error amplitude parameters)
! (corz, corp contain sqrt(error variance) from background error file)
!----------------------------------------------
  anhswgtsum=sum(anhswgt(1:ngauss))
  !-----------------------
  ! Zonal patch
  !-----------------------
  do k=indices%kps,indices%kpe
     ivar=idvar(k)
     kvar=k-kvar_start(ivar)+1
     do k1=1,nsig1o
        if(levs_id(k1)==kvar) exit
     end do

     an_amp(:,ivar)=an_amp0(ivar)

     flowvar=nrf_var(ivar)=='sf' .or. nrf_var(ivar)=='SF' .or. &
             nrf_var(ivar)=='vp' .or. nrf_var(ivar)=='VP' .or. &
             nrf_var(ivar)=='ps' .or. nrf_var(ivar)=='PS' .or. &
             nrf_var(ivar)=='t' .or. nrf_var(ivar)=='T'

     if(idiagflg==1) then
        if(bkgv_flowdep .and. flowvar) then
           select case (nrf_var(ivar))
              case('sf','SF'); write(6,*) 'ivar,k1,kvar,sfvar',ivar,k1,kvar,&
                       maxval(sfvar0f(:,:,k1)),minval(sfvar0f(:,:,k1)),&
                       maxval(corz(:,kvar,nrf3_sf)) ,minval(corz(:,kvar,nrf3_sf))
              case('vp','VP'); write(6,*) 'ivar,k1,kvar,vpvar',ivar,k1,kvar,&
                       maxval(vpvar0f(:,:,k1)),minval(vpvar0f(:,:,k1)),&
                       maxval(corz(:,kvar,nrf3_vp)) ,minval(corz(:,kvar,nrf3_vp))
              case('ps','PS'); write(6,*) 'ivar,k1,kvar,psvar',ivar,k1,kvar,&
                       maxval(psvar0f(:,:))   ,minval(psvar0f(:,:))   ,&
                       maxval(corp(:,nrf2_ps))           ,minval(corp(:,nrf2_ps))
              case('t','T'); write(6,*) 'ivar,k1,kvar,tvar' ,ivar,k1,kvar,&
                       maxval(tvar0f (:,:,k1)),minval(tvar0f (:,:,k1)),&
                       maxval(corz(:,kvar,nrf3_t)) ,minval(corz(:,kvar,nrf3_t))
           end select
        end if
     end if

     do j=indices%jps,indices%jpe
        do i=indices%ips,indices%ipe
           if(bkgv_flowdep .and. flowvar) then
              select case (nrf_var(ivar))
                 case('sf','SF'); factk=real(sfvar0f(i,j,k1),r_kind);
                 case('vp','VP'); factk=real(vpvar0f(i,j,k1),r_kind);
                 case('ps','PS'); factk=real(psvar0f(i,j)   ,r_kind);
                 case('t','T'); factk=real(tvar0f (i,j,k1),r_kind);
              end select
           else
              call get_stat_factk(p0ilatf(i),ivar,kvar,factk, &
                                  rh0f(i,j,k1),corsst0f(i,j))
           end if
           do igauss=1,ngauss
              factor=anhswgt(igauss)*factk*an_amp(igauss,ivar)/sqrt(anhswgtsum)
              filter_all(1)%amp(igauss,i,j,k)=factor*filter_all(1)%amp(igauss,i,j,k)
              if(allocated(ensamp0f)) then
                 filter_all(1)%amp(igauss,i,j,k)=filter_all(1)%amp(igauss,i,j,k)*sqrt(ensamp0f(i,j,k))
              end if
           end do
        end do
     end do
  end do

  !-----------------------
  ! Polar patches
  !-----------------------
  do k=indices_p%kps,indices_p%kpe
     ivar=idvar(k)
     kvar=k-kvar_start(ivar)+1
     do k1=1,nsig1o
        if(levs_id(k1)==kvar) exit
     end do

     an_amp(:,ivar)=an_amp0(ivar)

     flowvar=nrf_var(ivar)=='sf' .or. nrf_var(ivar)=='SF' .or. &
             nrf_var(ivar)=='vp' .or. nrf_var(ivar)=='VP' .or. &
             nrf_var(ivar)=='ps' .or. nrf_var(ivar)=='PS' .or. &
             nrf_var(ivar)=='t' .or. nrf_var(ivar)=='T'

     do j=indices_p%jps,indices_p%jpe
        do i=indices_p%ips,indices_p%ipe
 
      !-----------------------
      ! North polar
      !-----------------------
           if(bkgv_flowdep .and. flowvar) then
              select case (nrf_var(ivar))
                 case('sf','SF'); factk=real(sfvar2f(i,j,k1),r_kind)
                 case('vp','VP'); factk=real(vpvar2f(i,j,k1),r_kind)
                 case('ps','PS'); factk=real(psvar2f(i,j)   ,r_kind)
                 case('t','T'); factk=real(tvar2f (i,j,k1),r_kind)
              end select
           else
              if(p2ilatf(i,j)/=zero) then
                 call get_stat_factk(p2ilatf(i,j),ivar,kvar,factk, &
                                     rh2f(i,j,k1),corsst2f(i,j))
              else
                 call get_stat_factk(p2ilatfm    ,ivar,kvar,factk, &
                                     rh2f(i,j,k1),corsst2f(i,j))
              end if
           end if
           do igauss=1,ngauss
              factor=anhswgt(igauss)*factk*an_amp(igauss,ivar)/sqrt(anhswgtsum)
              filter_p2(1)%amp(igauss,i,j,k)=factor*filter_p2(1)%amp(igauss,i,j,k)
              if(allocated(ensamp2f)) then
                 filter_p2(1)%amp(igauss,i,j,k)=filter_p2(1)%amp(igauss,i,j,k)*sqrt(ensamp2f(i,j,k))
              end if
           end do

      !-----------------------
      ! South polar
      !-----------------------
           if(bkgv_flowdep .and. flowvar) then
              select case (nrf_var(ivar))
                 case('sf','SF'); factk=real(sfvar3f(i,j,k1),r_kind)
                 case('vp','VP'); factk=real(vpvar3f(i,j,k1),r_kind)
                 case('ps','PS'); factk=real(psvar3f(i,j)   ,r_kind)
                 case('t','T'); factk=real(tvar3f (i,j,k1),r_kind)
              end select
           else
              if(p3ilatf(i,j)/=zero) then
                 call get_stat_factk(p3ilatf(i,j),ivar,kvar,factk, &
                                     rh3f(i,j,k1),corsst3f(i,j))
              else
                 call get_stat_factk(p3ilatfm    ,ivar,kvar,factk, &
                                     rh3f(i,j,k1),corsst3f(i,j))
              end if
           end if
           do igauss=1,ngauss
              factor=anhswgt(igauss)*factk*an_amp(igauss,ivar)/sqrt(anhswgtsum)
              filter_p3(1)%amp(igauss,i,j,k)=factor*filter_p3(1)%amp(igauss,i,j,k)
              if(allocated(ensamp3f)) then
                 filter_p3(1)%amp(igauss,i,j,k)=filter_p3(1)%amp(igauss,i,j,k)*sqrt(ensamp3f(i,j,k))
              end if
           end do
        end do
     end do
  end do

!-----------------------------------------------
! Post process
! Note: corz, p?ilatf, ensamp?f are re-used when qoption==2
!-----------------------------------------------
! deallocate(corz,corp,hwll,hwllp,vz,aspect,aspect_p2,aspect_p3)
  deallocate(corp,hwll,hwllp,vz,aspect,aspect_p2,aspect_p3)
  deallocate(corsst0f ,corsst2f ,corsst3f )
  deallocate(hwllsst0f,hwllsst2f,hwllsst3f)
  deallocate(dxpf,dypf,dxzf,dyzf,rlatf)
  deallocate(theta0f,theta0zf,u0f,u0zf,v0f,v0zf,z0f,rh0f)
  deallocate(theta2f,theta2zf,u2f,u2zf,v2f,v2zf,z2f,rh2f)
  deallocate(theta3f,theta3zf,u3f,u3zf,v3f,v3zf,z3f,rh3f)
  deallocate(psg)
! deallocate(p0ilatf,p2ilatf,p3ilatf)

  deallocate(rfact0h,rfact0v)

  if(bkgv_flowdep) then
     deallocate(sfvar0f,vpvar0f,tvar0f,psvar0f)
     deallocate(sfvar2f,vpvar2f,tvar2f,psvar2f)
     deallocate(sfvar3f,vpvar3f,tvar3f,psvar3f)
  end if

! if(allocated(ensamp0f)) then
!    deallocate(ensamp0f,ensamp2f,ensamp3f)
! end if

  if(mype==0) write(6,*) '--- filter out step has finished ---'

end subroutine anprewgt
!=======================================================================
!=======================================================================
subroutine get_stat_factk(platf,ivar,kvar,factk,rh,dvsst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_stat_factk
! prgmmr: sato             org: np22                date: 2007-12-19
!
! abstract: interpolate corz/corp to get variance factor "factk".
!          (this routine is not used for flow dependent variance)
!
! program history log:
!   2007-12-18  sato
!   2010-03-11  zhu  - make changes for generalized control variable
!   2010-06-05  todling  - atsfc_sdv now comes from control vectors
!
!   input argument list:
!     platf - latitude grid info (global)
!     ivar  - element index
!     kvar  - level index
!     rh    - guess rh field used when qoption=2
!     dvsst - sst variance
!   output argument list:
!     factk - variance factor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use control_vectors,only: atsfc_sdv
  implicit none

  real(r_kind),   intent(in   ) :: platf
  integer(i_kind),intent(in   ) :: ivar,kvar
  real(r_kind),   intent(  out) :: factk
  real(r_single), intent(in   ) :: rh
  real(r_kind),   intent(in   ) :: dvsst

  integer(i_kind):: l,lp,n
  real(r_kind)   :: dl1,dl2

  l =int(platf)
  lp=l+1
  dl2=platf-float(l)
  dl1=one-dl2
  l = min(max(1,l ),mlat)
  lp= min(max(1,lp),mlat)

  if (ivar==nrf+1) then
     factk=atsfc_sdv(1)    ! land surface temperature
  else if (ivar==nrf+2) then
     factk=atsfc_sdv(2)    ! ice surface temperature
  else if (nrf_3d(ivar))then
     do n=1,nrf3
        if (nrf3_loc(n)==ivar) then
           factk=dl1*corz(l,kvar,n)+dl2*corz(lp,kvar,n)
           if (n==nrf3_q .and. qoption==2) &
                call fact_qopt2(factk,rh,kvar)! correction for qoption=2
        end if
     end do
  else
     do n=1,nrf2
        if (n==nrf2_sst) then
           factk=dvsst
        else if (nrf2_loc(n)==ivar) then
           factk=dl1*corp(l,n)+dl2*corp(lp,n)
        end if
     end do
  end if

  return
end subroutine get_stat_factk
!=======================================================================
!=======================================================================
subroutine create_iso_array_glb
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   create_iso_array_glb
! prgmmr: sato             org: np22                date: 2007-12-14
!
! abstract: initialize the array to save isotropic aspect tensor
!
! program history log:
!   2007-12-14  sato
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
  implicit none

  allocate(asp10f(pf2aP1%nlatf,pf2aP1%nlonf))
  allocate(asp20f(pf2aP1%nlatf,pf2aP1%nlonf))
  allocate(asp30f(pf2aP1%nlatf,pf2aP1%nlonf))

  allocate(asp12f(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(asp22f(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(asp32f(pf2aP2%nlatf,pf2aP2%nlonf))

  allocate(asp13f(pf2aP3%nlatf,pf2aP3%nlonf))
  allocate(asp23f(pf2aP3%nlatf,pf2aP3%nlonf))
  allocate(asp33f(pf2aP3%nlatf,pf2aP3%nlonf))
end subroutine create_iso_array_glb
!=======================================================================
!=======================================================================
subroutine destroy_iso_array_glb
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   destroy_iso_array_glb
! prgmmr: sato             org: np22                date: 2007-12-14
!
! abstract: discard isotropic aspect tensor array
!
! program history log:
!   2007-12-14  sato
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
  implicit none

  deallocate(asp10f,asp20f,asp30f)
  deallocate(asp12f,asp22f,asp32f)
  deallocate(asp13f,asp23f,asp33f)
  deallocate(ks)
end subroutine destroy_iso_array_glb
!=======================================================================
!=======================================================================
subroutine init_anisofilter
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   init_anisofilter
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: initialize anisotropic background error related variables
!
! program history log:
!   2006-08-01  pondeca
!   2007-09-04  sato
!   2010-03-11  zhu - make changes using nvars,nrf* for generlized control variable
!
!   input argument list:
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use smooth_polcarf, only: setup_smooth_polcas
  use patch2grid_mod, only: setup_patch2grid

  implicit none

  integer(i_kind) :: n 

  scalex1=1.2_r_kind
  scalex2=1.2_r_kind
  scalex3=1.2_r_kind

  allocate (rfact0h(nvars))
  allocate (rfact0v(nvars))
!--- original
! rfact0h(1)=one            ;  rfact0v(1)=1.50_r_kind
! rfact0h(2)=one            ;  rfact0v(2)=1.50_r_kind
! rfact0h(3)=1.20_r_kind    ;  rfact0v(3)=1.50_r_kind
! rfact0h(4)=1.50_r_kind    ;  rfact0v(4)=1.50_r_kind
! rfact0h(5)=1.50_r_kind    ;  rfact0v(5)=1.25_r_kind
!-- fine tuning with rgauss tuning
  if(opt_sclclb_glb==0) then
        do n=1,nrf
           select case(nrf_var(n))
              case('sf','SF'); rfact0h(n)=1.20_r_kind ;  rfact0v(n)=1.40_r_kind
              case('vp','VP'); rfact0h(n)=1.20_r_kind ;  rfact0v(n)=2.20_r_kind
              case('t','T')  ; rfact0h(n)=1.20_r_kind ;  rfact0v(n)=1.20_r_kind
              case('q','Q')  ; rfact0h(n)=1.50_r_kind ;  rfact0v(n)=1.50_r_kind
              case('oz','OZ'); rfact0h(n)=one         ;  rfact0v(n)=1.25_r_kind
              case('cw','CW'); rfact0h(n)=one         ;  rfact0v(n)=1.25_r_kind
              case('ps','PS'); rfact0h(n)=1.20_r_kind ;  rfact0v(n)=1.20_r_kind
              case('sst','SST'); rfact0h(n)=one       ;  rfact0v(n)=1.25_r_kind;
                                 rfact0h(nrf+1)=one   ;  rfact0v(nrf+1)=1.25_r_kind;
                                 rfact0h(nrf+2)=one   ;  rfact0v(nrf+2)=1.25_r_kind
           end select
        end do

!--  for exponent component =scale**rfact(1)+rfact(2)
  else if(opt_sclclb_glb==1) then
     rfact0h(1)=1.20_r_kind ;  rfact0v(1)=1.20_r_kind
     rfact0h(2)=0.20_r_kind ;  rfact0v(2)=0.20_r_kind
  end if
!---

  call setup_smooth_polcas
  call setup_patch2grid

end subroutine init_anisofilter
!=======================================================================
!=======================================================================
subroutine read_bckgstats_glb(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   read_bckgstats_glb
! prgmmr: sato             org: np22                date: 2007-12-14
!
! abstract: initialize the background statistics
!
! program history log:
!   2007-12-14  sato
!   2010-03-03  zhu - change the structure of error covariance file
!                   - read in the error info for used control variables only 
!                   - use nrf* for generalized control variables
!                   - mv varq to berror_read_wgt
!   2014-02-01  todling - update interface to berror_read_wgt
!
!   input argument list:
!    mype    - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: r1000, three
  use patch2grid_mod, only: grid2patch
  use m_berror_stats, only: berror_get_dims,berror_read_wgt
  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind):: mcount0,mcount,ierror
  real(r_kind) :: pbar4a,pbar4(nsig)

  integer(i_kind):: inerr,n,i,j,k,l
  real(r_single),dimension(nlat,nsig,nrf3):: corzin
  real(r_single),dimension(nlat,nrf2):: corpin
  real(r_single),dimension(nlat,nsig,nrf3):: hwllin
  real(r_single),dimension(nlat,nrf2):: hwllpin
  real(r_single),dimension(nsig,nlat,nrf3):: vscalesin
  real(r_single),dimension(nlat,nlon) :: corsstin
  real(r_single),dimension(nlat,nlon) :: hsst

  integer(i_kind):: nsigstat
  real(r_kind):: psfc015

  data inerr / 22 /

! Open background error statistics file
! Read header.  Ensure that vertical resolution is consistent
! with that specified via the user namelist
  call berror_get_dims(nsigstat,mlat,inerr)
  if(mype==0) then
     write(6,*)'read_bckgstats_glb(): read error amplitudes.  mype,nsigstat,mlat =',&
     mype,nsigstat,mlat
  end if

  if (nsig/=nsigstat .or. nlat/=mlat) then
     write(6,*)'PREWGT:  ***ERROR*** resolution of berror_stats ',&
               'incompatiable with guess'
     write(6,*)'PREWGT:  berror nsigstat,mlat=',nsigstat,mlat,&
               ' -vs- guess nsig,nlat=',nsig,nlat
     call stop2(stpcode_statdata)
  end if

  allocate ( corz(mlat,nsig,nrf3), corp(mlat,nrf2),  corsst(nlat,nlon) )
  allocate ( hwll(mlat,nsig,nrf3), hwllp(mlat,nrf2), hwllsst(nlat,nlon) )
  allocate ( vz(nsig,mlat,nrf3) )

! Read amplitudes
  call berror_read_wgt(corzin,corpin,hwllin,hwllpin,vscalesin,corsstin,hsst,varq,qoption,varcw,cwoption,mype,inerr)

  if(mype==0) then
     write(6,*) '--- start read_bckgstats_glb ---'
     write(6,*) 'maxmin-corzin:',  maxval(corzin(:,:,nrf3_sf)),minval(corzin(:,:,nrf3_sf))
     write(6,*) 'maxmin-cordin:',  maxval(corzin(:,:,nrf3_vp)),minval(corzin(:,:,nrf3_vp))
     write(6,*) 'maxmin-corhin:',  maxval(corzin(:,:,nrf3_t)),minval(corzin(:,:,nrf3_t))
     write(6,*) 'maxmin-corqin:',  maxval(corzin(:,:,nrf3_q)),minval(corzin(:,:,nrf3_q))
     write(6,*) 'maxmin-corozin:', maxval(corzin(:,:,nrf3_oz)),minval(corzin(:,:,nrf3_oz))
     write(6,*) 'maxmin-corsstin:', maxval(corsstin), minval(corsstin)
     write(6,*) '---'
     write(6,*) 'maxmin-hwllzin:',  maxval(hwllin(:,:,nrf3_sf)),minval(hwllin(:,:,nrf3_sf))
     write(6,*) 'maxmin-hwlldin:',  maxval(hwllin(:,:,nrf3_vp)),minval(hwllin(:,:,nrf3_vp))
     write(6,*) 'maxmin-hwllhin:',  maxval(hwllin(:,:,nrf3_t)),minval(hwllin(:,:,nrf3_t))
     write(6,*) 'maxmin-hwllqin:',  maxval(hwllin(:,:,nrf3_q)),minval(hwllin(:,:,nrf3_q))
     write(6,*) 'maxmin-hwllozin:',  maxval(hwllin(:,:,nrf3_oz)),minval(hwllin(:,:,nrf3_oz))
     write(6,*) 'maxmin-hsstin:',   maxval(hsst),     minval(hsst)
     write(6,*) '--- end read_bckgstats_glb ---'
  end if

! load the horizontal length scales
  do n=1,nrf3
     do k=1,nsig
        do i=1,nlat
           hwll(i,k,n)=real(hwllin(i,k,n),r_kind)
        end do
     end do
  end do

! surface pressure
  do n=1,nrf2
     do i=1,nlat
        hwllp(i,n)=real(hwllpin(i,n),r_kind)
     end do
  end do


! sea surface temperature, convert from km to m
! also calculate a minimum horizontal length scale for
! sst to be used for land skin temp and ice temp
  hwllsstmin=1.e10_r_kind
  do j=1,nlat
     do i=1,nlon
        hwllsst(j,i)=r1000*real(hsst(j,i),r_kind)
        if(hwllsstmin>hwllsst(j,i)) hwllsstmin=hwllsst(j,i)
     end do
  end do

! perturb background error
! Things to perturb: as(1-8), hzscl(1-3) and vs(1)
!  if (pert_berr) then
!     allocate(randfct(12))
!
!     call get_randoms(12,randfct,mype)
!     do i=1,8
!        as(i)=as(i)+as(i)*randfct(i)
!     end do
!     do i=1,3
!        hzscl(i)=hzscl(i)+hzscl(i)*randfct(8+i)
!     end do
!     vs=vs+vs*randfct(12)
!     if (mype==0) then
!        write(6,*) 'PREWGT: REDEFINE AS = ',as
!        write(6,*) 'PREWGT: REDEFINE HZSCL = ',hzscl
!        write(6,*) 'PREWGT: REDEFINE VS = ',vs
!     end if
!     deallocate(randfct)
!  end if

! As used in the code, the horizontal length scale
! parameters are used in an inverted form.  Invert
! the parameter values here.
!  do i=1,3
!     hzscl(i)=one/hzscl(i)
!  end do
!
! apply scaling (deflate/inflate) to vertical length scales
! note: parameter vs needs to be inverted
  an_vs=one/an_vs

! load vertical length scales
  do n=1,nrf3
     do k=1,nsig
        do i=1,nlat
           vz(k,i,n)=real(vscalesin(k,i,n),r_kind)*an_vs
        end do
     end do
  end do
  vz(:,:,nrf3_cw)=vz(:,:,nrf3_q)   ! for now use q error for cwm

  corz  =real(corzin,r_kind)
  corp  =real(corpin,r_kind)
  corsst=real(corsstin,r_kind)

  allocate(corsst0f(pf2aP1%nlatf,pf2aP1%nlonf))
  allocate(corsst2f(pf2aP2%nlatf ,pf2aP2%nlonf ))
  allocate(corsst3f(pf2aP3%nlatf ,pf2aP3%nlonf ))
  call grid2patch(corsst,corsst0f,corsst2f,corsst3f)
  deallocate(corsst)

  allocate(hwllsst0f(pf2aP1%nlatf,pf2aP1%nlonf))
  allocate(hwllsst2f(pf2aP2%nlatf ,pf2aP2%nlonf ))
  allocate(hwllsst3f(pf2aP3%nlatf ,pf2aP3%nlonf ))
  call grid2patch(hwllsst,hwllsst0f,hwllsst2f,hwllsst3f)
  deallocate(hwllsst)


! hybrid sigma level structure calculated in rdgstat
! ks used to load constant horizontal scales for SF/VP
! above sigma level 0.15
! loop l for diff variable of each PE.

  do k=1,nsig
     pbar4a=zero
     do j=1,lon2
        do i=1,lat2
           pbar4a=pbar4a+ges_prsl(i,j,k,ntguessig)*10._r_kind
        end do
     end do
     mcount0=lon2*lat2! It's OK to count buffer points
     call mpi_allreduce(pbar4a,pbar4(k),1,mpi_real8,mpi_sum,mpi_comm_world,ierror)
     call mpi_allreduce(mcount0,mcount,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
     pbar4(k)=pbar4(k)/float(mcount)
  end do

  psfc015=r015*pbar4(1)
  allocate (ks(nsig1o))
  do l=1,nsig1o
     ks(l)=nsig+1
     if(nrf_var(nvar_id(l))=='sf' .or. nrf_var(nvar_id(l))=='SF' & 
        .or. nrf_var(nvar_id(l))=='vp' .or. nrf_var(nvar_id(l))=='VP')then
        k_loop: do k=1,nsig
           if (pbar4(k)< psfc015) then
              ks(l)=k
              exit k_loop
           end if
        enddo k_loop
     endif
  end do
  if(mype==0) write(6,*) 'ks,pbar4=',ks,pbar4

end subroutine read_bckgstats_glb
!=======================================================================
!=======================================================================
subroutine get_background_glb(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_background_glb
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: compute background fields and their spatial derivatives
!           on filter grid for use in anisotropic covariance model.
!           built from parrish's old anprewgt_reg
!
!
! program history log:
!   2006-08-01  pondeca
!   2007-09-05  sato : for global
!   2010-03-03  zhu  - use nrf3_t
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use gridmod, only: rlats,rlons
  use patch2grid_mod, only: grid2patch
  use sub2fslab_mod, only: setup_sub2fslab, destroy_sub2fslab, &
                           sub2fslab_glb, sub2fslabdz_glb, sub2fslab2d_glb, sub2slab2d

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  character(len=*),parameter::myname_=myname//'*get_background_glb'
  integer(i_kind) i,j,k,mm1,k1,ivar,nlonfc,ier,it,loc,iderivative
  integer(i_kind) iflm,ilat,ilon,ilatp,ilatm,ilonp,ilonm,istatus

  real(r_kind) hwll_loc,rnf2,rnf212
  real(r_kind) asp1,asp2,asp3

  real(r_kind)  ,allocatable,dimension(:,:,:)::field
  logical:: ice

  real(r_kind),allocatable :: hflt_all(:,:)
  real(r_kind),allocatable :: hflt_p2 (:,:)
  real(r_kind),allocatable :: hflt_p3 (:,:)

  real(r_kind) :: dxymin
  real(r_kind),allocatable :: grid_wrk(:,:)
  real(r_kind),allocatable :: rlatsinf(:,:)
  real(r_kind),allocatable :: rlatcosf(:,:)
  real(r_kind),allocatable :: rlonf   (:,:)
  real(r_kind),allocatable :: rlonsinf(:,:)
  real(r_kind),allocatable :: rloncosf(:,:)

  real(r_kind),dimension(:,:  ),pointer:: ges_ps_it=>NULL()
  real(r_kind),dimension(:,:  ),pointer:: ges_z_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_u_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_v_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_tv_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q_it=>NULL()

  integer(i_long):: ngauss_smooth,npass_smooth,normal_smooth,ifilt_ord_smooth
  integer(i_long):: nsmooth_smooth,nsmooth_shapiro_smooth

  real(r_double) :: rgauss_smooth(1)

  real(r_kind):: dyi

!-----------------------------------------------
! get dx and dy for patches
!   Note:
!     On the zonal patch, dx & dy depends latitude only.
!     For polar patches, calculate dx & dy for north polar patch,
!     and use them for the both patches.
!-----------------------------------------------
  allocate(hflt_all(pf2aP1%nlatf,pf2aP1%nlonf))
  allocate(hflt_p2 (pf2aP2%nlatf ,pf2aP2%nlonf ))
  allocate(hflt_p3 (pf2aP3%nlatf ,pf2aP3%nlonf ))
  allocate(rlatsinf(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(rlatcosf(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(rlonf   (pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(rlonsinf(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(rloncosf(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(grid_wrk(nlat,nlon),stat=ier)

  allocate(p0ilatf(pf2aP1%nlatf))
  allocate(dxzf   (pf2aP1%nlatf))
  allocate(dyzf   (pf2aP1%nlatf))
  allocate(rlatf  (pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(p2ilatf(pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(p3ilatf(pf2aP3%nlatf,pf2aP3%nlonf))
  allocate(dxpf   (pf2aP2%nlatf,pf2aP2%nlonf))
  allocate(dypf   (pf2aP2%nlatf,pf2aP2%nlonf),stat=ier)

  if( ier /= 0 ) then
     write(6,*) 'get_background_glb: could not allocate latlon field'
     call stop2(stpcode_alloc)
  end if

  nlonfc = pf2aP1%nlonf / 2

  !-----------------------
  ! patch latitudes (rad)
  !-----------------------
  do ilat=1,nlat
     grid_wrk(ilat,:)=rlats(ilat)
  end do
  call grid2patch(grid_wrk,hflt_all,rlatf,hflt_p3)

  rlatcosf=cos(rlatf) ! for polar patches
  rlatsinf=sin(rlatf) ! for polar patches

  !-----------------------
  ! zonal patch
  !-----------------------
  do ilat=1,pf2aP1%nlatf
     ilatm=ilat-1
     ilatp=ilat+1
     if(ilatm<1) then
        dyi=one; ilatm=1
     else if(ilatp>pf2aP1%nlatf) then
        dyi=one; ilatp=pf2aP1%nlatf
     else
        dyi=half
     end if
     dyzf(ilat) = rearth_equator * ( hflt_all(ilatp,nlonfc) - hflt_all(ilatm,nlonfc) ) * dyi
  end do

  do ilat=1,pf2aP1%nlatf
     dxzf(ilat)= rearth_equator * ( rlons(2) - rlons(1) ) * pf2aP1%grid_ratio &
               * cos ( hflt_all(ilat,nlonfc) )
  end do

  if( mype==0 ) then
     do ilat=1,pf2aP1%nlatf
        write(6,*) 'dyzf,dxzf,ilat,rlatsf=',dyzf(ilat),dxzf(ilat),ilat,hflt_all(ilat,nlonfc)
     end do
  end if

  !-----------------------
  ! zonal patch end
  !-----------------------

  !-----------------------
  ! patch latitudes (original grid_number)
  !-----------------------
  do ilat=1,nlat
     grid_wrk(ilat,:)=real(ilat)
  end do
  call grid2patch(grid_wrk,hflt_all,p2ilatf,p3ilatf)

  p0ilatf(:)=hflt_all(:,nlonfc)

  rnf2  = pf2aP2%nlatf * half
  rnf212= rnf2**two

  p2ilatfm=p2ilatf(pf2aP2%nlatf/2,pf2aP2%nlonf/2)
  p3ilatfm=p3ilatf(pf2aP2%nlatf/2,pf2aP2%nlonf/2)

  do ilat=1,pf2aP2%nlatf
     do ilon=1,pf2aP2%nlonf
        if(((float(ilat)-rnf2)**2+(float(ilon)-rnf2)**2)>=rnf212) then
           p2ilatf(ilat,ilon)=zero
           p3ilatf(ilat,ilon)=zero
        else
           p2ilatfm=min(p2ilatfm,p2ilatf(ilat,ilon))
           p3ilatfm=max(p3ilatfm,p3ilatf(ilat,ilon))
        end if
     end do
  end do

  !-----------------------
  ! patch cos(longitude)
  !-----------------------
  do ilon=1,nlon
     grid_wrk(:,ilon)=cos(rlons(ilon))
  end do
  call grid2patch(grid_wrk,hflt_all,rloncosf,hflt_p3)

  !-----------------------
  ! patch sin(longitude)
  !-----------------------
  do ilon=1,nlon
     grid_wrk(:,ilon)=sin(rlons(ilon))
  end do
  call grid2patch(grid_wrk,hflt_all,rlonsinf,hflt_p3)

  rlonf=atan2(rlonsinf,rloncosf)

  deallocate(rlonsinf)
  deallocate(rloncosf)
  deallocate(grid_wrk)

  !-----------------------
  ! dxpf part
  !-----------------------
  dxpf=zero
  dxymin=zero
  do ilat=1,pf2aP2%nlatf
     do ilon=1,pf2aP2%nlonf
        ilonm=ilon-1
        ilonp=ilon+1
        dyi=half
        if(ilonm<1) then
           dyi=one; ilonm=1
        end if
        if(ilonp>pf2aP2%nlonf) then
           dyi=one; ilonp=pf2aP2%nlonf
        end if
        if( p2ilatf(ilat,ilon)/=zero ) then
           dxpf(ilat,ilon)=rearth_equator * &
             acos( rlatsinf(ilat,ilonm)*rlatsinf(ilat,ilonp) &
                & +rlatcosf(ilat,ilonm)*rlatcosf(ilat,ilonp) &
                & *cos(rlonf(ilat,ilonm)-rlonf(ilat,ilonp))) * dyi
           if( dxymin == zero .or. dxymin > dxpf(ilat,ilon) ) dxymin=dxpf(ilat,ilon)
        end if
     end do
  end do
  where(dxpf==zero) dxpf=dxymin

  !-----------------------
  ! dypf part
  !-----------------------
  dypf=zero
  dxymin=zero
  do ilon=1,pf2aP2%nlonf
     do ilat=1,pf2aP2%nlatf
        ilatm=ilat-1
        ilatp=ilat+1
        dyi=half
        if(ilatm<1) then
           dyi=one; ilatm=1
        end if
        if(ilatp>pf2aP2%nlatf) then
           dyi=one; ilatp=pf2aP2%nlatf
        end if
        if( p2ilatf(ilat,ilon)/=zero ) then
           dypf(ilat,ilon)=rearth_equator * &
             acos( rlatsinf(ilatm,ilon)*rlatsinf(ilatp,ilon) &
                & +rlatcosf(ilatm,ilon)*rlatcosf(ilatp,ilon) &
                & *cos(rlonf(ilatm,ilon)-rlonf(ilatp,ilon))) * dyi
           if( dxymin == zero .or. dxymin > dypf(ilat,ilon) ) dxymin=dypf(ilat,ilon)
        end if
     end do
  end do
  where(dypf==zero) dypf=dxymin

  deallocate(rlatsinf,rlatcosf,rlonf)
  deallocate(hflt_all,hflt_p2,hflt_p3 )

  !-----------------------
  ! Check dx & dy
  !-----------------------
  if( mype == 0 ) then
     write(6,*) 'Patch dxy has been estimated, dim=',pf2aP2%nlatf,pf2aP2%nlonf
     write(6,*) 'maxmin-dxpf'   ,maxval(dxpf)    ,minval(dxpf)
     write(6,*) 'maxmin-dypf'   ,maxval(dypf)    ,minval(dypf)

     write(6,*) '--- dxpf ---'
     do ilat=1,pf2aP2%nlatf
        write(6,'(1X,49F4.1)') dxpf(ilat,:)/100000.0_r_kind
     end do

     write(6,*) '--- dypf ---'
     do ilon=1,pf2aP2%nlonf
        write(6,'(1X,49F4.1)') dypf(:,ilon)/100000.0_r_kind
     end do

!    open(94,form='unformatted',file='dxypf.dat')
!    write(94) rloncosf(:,:),rlonsinf(:,:),rlatf(:,:)
!    write(94) dxpf(:,:),dypf(:,:)
!    close(94)

  end if
!---

  mm1=mype+1

!-----------------------------------------------
!  convert all basic variables from subdomain to slab mode and interpolate to filter grid
!  then repeat this process for all vertical derivatives of subdomain variables
!-----------------------------------------------

  allocate(field(lat2,lon2,nsig))

  allocate(theta0f (pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(theta0zf(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(theta2f (pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(theta2zf(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(theta3f (pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))
  allocate(theta3zf(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))

  allocate(u0f (pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(u0zf(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(u2f (pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(u2zf(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(u3f (pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))
  allocate(u3zf(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))

  allocate(v0f (pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(v0zf(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(v2f (pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(v2zf(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(v3f (pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))
  allocate(v3zf(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))

  allocate(z0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(z2f(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(z3f(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))

  allocate(rh0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(rh2f(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(rh3f(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))

  it=ntguessig

  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,  istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,  istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q', ges_q_it,   istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname_,'missing fields, ier= ', ier)

  call setup_sub2fslab
  ! T
  field(:,:,:)=ges_tv_it(:,:,:)/(ges_prsl(:,:,:,it)/r100)**rd_over_cp
  call sub2fslab_glb  (field,theta0f ,theta2f ,theta3f)
  call sub2fslabdz_glb(field,theta0zf,theta2zf,theta3zf)

  ! U
  call sub2fslab_glb  (ges_u_it,u0f ,u2f ,u3f )
  call sub2fslabdz_glb(ges_u_it,u0zf,u2zf,u3zf)

  ! V
  call sub2fslab_glb  (ges_v_it,v0f ,v2f ,v3f )
  call sub2fslabdz_glb(ges_v_it,v0zf,v2zf,v3zf)

  ! Z
  call sub2fslab2d_glb(ges_z_it,z0f ,z2f ,z3f )
  do j=1,pf2aP1%nlonf
     do i=1,pf2aP1%nlatf
        z0f(i,j,2:nsig1o)=z0f(i,j,1)
     end do
  end do
  do j=1,pf2aP2%nlonf
     do i=1,pf2aP2%nlatf
        z2f(i,j,2:nsig1o)=z2f(i,j,1)
     end do
  end do
  do j=1,pf2aP3%nlonf
     do i=1,pf2aP3%nlatf
        z3f(i,j,2:nsig1o)=z3f(i,j,1)
     end do
  end do

  ! RH
  ice=.true.
  field(:,:,:)=ges_q_it(:,:,:)
  iderivative=0
  call genqsat(field,ges_tsen(1,1,1,it),ges_prsl(1,1,1,it),lat2,lon2,nsig,ice,iderivative)
  field(:,:,:)=ges_q_it(:,:,:)/field(:,:,:)

  call sub2fslab_glb(field,rh0f,rh2f,rh3f)

  !---
  ! PS (2d full grid)
  ! note: psg will not be used in the filter space
  !---
  allocate(psg(nlat,nlon,nsig1o))
  field(:,:,1)=1000.0_r_single*ges_ps_it
  call sub2slab2d(field(1,1,1),psg)

  call destroy_sub2fslab
  deallocate(field)

!-----------------------------------------------------------
!-------end of getting background variables on filter grid--
!-----------------------------------------------------------

! ------------------------------------------------------------
! ------------ in this section, set up isotropic filter for
! ------------ generating smoothed guess
! ------------------------------------------------------------

  allocate(aspect   (7,pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(aspect_p2(7,pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
  allocate(aspect_p3(7,pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o))

  do k=1,nsig1o

     k1=levs_id(k)
     if (k1==0) cycle

     ivar=nrf3_t
     loc=nrf3_loc(ivar)
     iflm = aint(p0ilatf(pf2aP1%nlatf/2))
     hwll_loc=hwll(iflm,k1,ivar)
 
     do i=1,pf2aP1%nlatf

        asp1=hwll_loc/dyzf(i)*rfact0h(loc)
        asp2=hwll_loc/dxzf(i)*rfact0h(loc)
        asp3=one/vz(k1,iflm,ivar)*rfact0v(loc)
        do j=1,pf2aP1%nlonf
           aspect(1,i,j,k)=real(asp1**2,r_single)
           aspect(2,i,j,k)=real(asp2**2,r_single)
           aspect(3,i,j,k)=real(asp3**2,r_single)
           aspect(4:7,i,j,k)=zero_single
        end do
     end do

     asp3=one/vz(k1,iflm,ivar)*rfact0v(loc)
     do i=1,pf2aP2%nlatf
        do j=1,pf2aP2%nlonf
           asp1=hwll_loc/dxpf(i,j)*rfact0h(loc)
           asp2=hwll_loc/dypf(i,j)*rfact0h(loc)
           aspect_p2(1,i,j,k)=real(asp1**2,r_single)
           aspect_p2(2,i,j,k)=real(asp2**2,r_single)
           aspect_p2(3,i,j,k)=real(asp3**2,r_single)
           aspect_p2(4:7,i,j,k)=zero_single
        end do
     end do

  end do

  ngauss_smooth=1
  rgauss_smooth=one
  npass_smooth=1
  normal_smooth=0
  ifilt_ord_smooth=4
  nsmooth_smooth=0
  nsmooth_shapiro_smooth=0

  call init_raf4_wrap(aspect,triad4,ngauss_smooth,rgauss_smooth, &
                     npass_smooth,normal_smooth,binom, &
                     ifilt_ord_smooth,filter_all, &
                     nsmooth_smooth,nsmooth_shapiro_smooth, &
                     nvars,idvar,kvar_start,kvar_end,var_names, &
                     indices,mype, npe)

  call init_raf4_wrap(aspect_p2,triad4,ngauss_smooth,rgauss_smooth, &
                     npass_smooth,normal_smooth,binom, &
                     ifilt_ord_smooth,filter_p2, &
                     nsmooth_smooth,nsmooth_shapiro_smooth, &
                     nvars,idvar,kvar_start,kvar_end,var_names, &
                     indices_p,mype, npe)

  call raf_sm_glb(theta0f ,theta2f ,theta3f ,ngauss_smooth)
  call raf_sm_glb(theta0zf,theta2zf,theta3zf,ngauss_smooth)
  call raf_sm_glb(u0f ,u2f ,u3f ,ngauss_smooth)
  call raf_sm_glb(u0zf,u2zf,u3zf,ngauss_smooth)
  call raf_sm_glb(v0f ,v2f ,v3f ,ngauss_smooth)
  call raf_sm_glb(v0zf,v2zf,v3zf,ngauss_smooth)
  call raf_sm_glb(z0f ,z2f ,z3f ,ngauss_smooth)
  call raf_sm_glb(rh0f,rh2f,rh3f,ngauss_smooth)

  if(mype==0) then
     write(6,*) 'bkg-calc has been finished'
     write(6,*) 'dim for zonal patch', pf2aP1%nlatf,pf2aP1%nlonf
     write(6,*) 'dim for polar patch', pf2aP2%nlatf, pf2aP2%nlonf
!    open(94,form='unformatted',file='theta_f.dat')
!    write(94) theta0f, theta2f, theta3f
!    close(94)
  end if

end subroutine get_background_glb
!=======================================================================
!=======================================================================
subroutine raf_sm_glb(fslb0,fslb2,fslb3,ngauss_smooth)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    raf_sm_glb
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-17  lueken - added subprogram doc block
!
!   input argument list:
!    ngauss_smooth
!    fslb0
!    fslb2
!    fslb3
!
!   output argument list:
!    fslb0
!    fslb2
!    fslb3
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_long),intent(in   ) :: ngauss_smooth

  real(r_single) ,intent(inout) :: fslb0(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_single) ,intent(inout) :: fslb2(pf2aP2%nlatf,pf2aP2%nlonf)
  real(r_single) ,intent(inout) :: fslb3(pf2aP3%nlatf,pf2aP3%nlonf)

  !--- Zonal Patch
  call raf_sm4_wrap   (fslb0,filter_all,ngauss_smooth,indices  ,npe)
  call raf_sm4_ad_wrap(fslb0,filter_all,ngauss_smooth,indices  ,npe)
  !--- North Polar Patch
  call raf_sm4_wrap   (fslb2,filter_p2 ,ngauss_smooth,indices_p,npe)
  call raf_sm4_ad_wrap(fslb2,filter_p2 ,ngauss_smooth,indices_p,npe)
  !--- South Polar Patch,
  !    which use filter_p2 since the coordination shape must be same.
  call raf_sm4_wrap   (fslb3,filter_p2 ,ngauss_smooth,indices_p,npe)
  call raf_sm4_ad_wrap(fslb3,filter_p2 ,ngauss_smooth,indices_p,npe)

  return
end subroutine raf_sm_glb
!=======================================================================
!=======================================================================
subroutine get_aspect_pt(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_aspect_pt
! prgmmr: sato             org: np22                date: 2007-11-16
!
! abstract: define the anisotropic aspect tensor based on pt
!
! program history log:
!   2007-11-16  sato : for global
!   2010-03-11  zhu  - make changes using nrf*
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use anberror, only: afact0
  implicit none

  integer(i_kind),intent(in   ) :: mype
  character(len= 5):: cvar
  logical flowvar

  integer(i_kind):: i,j,k,k1
  real(r_kind):: fx1,fx2,fx3,asp1,asp2,asp3
  real(r_kind):: rk1,fblend
  real(r_single):: afact,qltv,qlth

!-----------------------------------------------------------
!-----------------------
!-----------------------------------------------------------
! Set up scales

! This first loop for nsig1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone

  call create_iso_array_glb

!-- allocate memories to be used in the get_theta_corrl_lenghts_glb
  allocate(qlth_temp(nsig))
  allocate(qltv_temp(nsig))
  allocate(qlth_wind(nsig))
  allocate(qltv_wind(nsig))

  allocate(asp1_max(10,nsig),asp1p2_max(10,nsig),asp1p3_max(10,nsig))
  allocate(asp2_max(10,nsig),asp2p2_max(10,nsig),asp3p2_max(10,nsig))
  allocate(asp3_max(10,nsig),asp2p3_max(10,nsig),asp3p3_max(10,nsig))

  allocate(tx1_slab(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(tx2_slab(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(tx3_slab(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(tx1p2_slab(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
  allocate(tx2p2_slab(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
  allocate(tx3p2_slab(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
  allocate(tx1p3_slab(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))
  allocate(tx2p3_slab(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))
  allocate(tx3p3_slab(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))

  call get_theta_corrl_lenghts_glb(mype)

  do k=1,nsig1o

     k1=levs_id(k)
     if (k1==0) cycle

     call isotropic_scales_glb(asp10f,asp20f,asp30f, &
                               asp12f,asp22f,asp32f, &
                               asp13f,asp23f,asp33f,k)
     qlth=one
     qltv=one
     cvar=trim(nrf_var(nvar_id(k)))
     if (cvar=='sf' .or. cvar=='SF') then ; qlth=qlth_wind(k1) ; qltv=qltv_wind(k1) ; endif
     if (cvar=='vp' .or. cvar=='VP') then ; qlth=qlth_wind(k1) ; qltv=qltv_wind(k1) ; endif
     if (cvar=='t' .or. cvar=='T') then ; qlth=qlth_temp(k1) ; qltv=qltv_temp(k1) ; endif
 
     flowvar=cvar=='sf' .or. cvar=='SF' .or. &
             cvar=='vp' .or. cvar=='VP' .or. &
             cvar=='t' .or. cvar=='T'

     rk1=float(k1-kthres)
     fblend=half*(one-tanh(rk1))
 
     !--- zonal patch
     do j=1,pf2aP1%nlonf
        do i=1,pf2aP1%nlatf
           asp1=asp10f(i,j); asp2=asp20f(i,j); asp3=asp30f(i,j)

           afact=zero_single; fx1=one; fx2=one; fx3=one
           if (flowvar .and. afact0(nvar_id(k))>zero ) then
              afact=real(afact0(nvar_id(k)),r_single)
              asp1=scalex1*asp1; fx1=real(tx1_slab(i,j,k),r_kind)
              asp2=scalex2*asp2; fx2=real(tx2_slab(i,j,k),r_kind)
              asp3=scalex3*asp3; fx3=real(tx3_slab(i,j,k),r_kind)
           endif

           aspect(1,i,j,k) = real(one/asp1**2+afact*fblend*fx1*fx1/(qlth**2)  ,r_single) ! 1st (y) direction    x1*x1
           aspect(2,i,j,k) = real(one/asp2**2+afact*fblend*fx2*fx2/(qlth**2)  ,r_single) ! 2nd (x) direction    x2*x2
           aspect(3,i,j,k) = real(one/asp3**2+afact*fblend*fx3*fx3/(qltv**2)  ,r_single) ! 3rd (z) direction    x3*x3
           aspect(4,i,j,k) = real(            afact*fblend*fx3*fx2/(qlth*qltv),r_single) !  x3*x2
           aspect(5,i,j,k) = real(            afact*fblend*fx3*fx1/(qlth*qltv),r_single) !  x3*x1
           aspect(6,i,j,k) = real(            afact*fblend*fx2*fx1/(qlth**2)  ,r_single) !  x2*x1
           aspect(7,i,j,k)=  zero_single

        end do
     end do

     !--- north polar patch
     do j=1,pf2aP2%nlonf
        do i=1,pf2aP2%nlatf
           asp1=asp12f(i,j); asp2=asp22f(i,j); asp3=asp32f(i,j)
 
           afact=zero; fx1=one; fx2=one; fx3=one
           if ( flowvar .and. afact0(nvar_id(k))>zero ) then
              afact=afact0(nvar_id(k))
              asp1=scalex1*asp1; fx1=real(tx1p2_slab(i,j,k),r_kind)
              asp2=scalex2*asp2; fx2=real(tx2p2_slab(i,j,k),r_kind)
              asp3=scalex3*asp3; fx3=real(tx3p2_slab(i,j,k),r_kind)
           endif

           aspect_p2(1,i,j,k) = real(one/asp1**2+afact*fblend*fx1*fx1/(qlth**2)  ,r_single) ! 1st (y) direction    x1*x1
           aspect_p2(2,i,j,k) = real(one/asp2**2+afact*fblend*fx2*fx2/(qlth**2)  ,r_single) ! 2nd (x) direction    x2*x2
           aspect_p2(3,i,j,k) = real(one/asp3**2+afact*fblend*fx3*fx3/(qltv**2)  ,r_single) ! 3rd (z) direction    x3*x3
           aspect_p2(4,i,j,k) = real(            afact*fblend*fx3*fx2/(qlth*qltv),r_single) !  x3*x2
           aspect_p2(5,i,j,k) = real(            afact*fblend*fx3*fx1/(qlth*qltv),r_single) !  x3*x1
           aspect_p2(6,i,j,k) = real(            afact*fblend*fx2*fx1/(qlth**2)  ,r_single) !  x2*x1
           aspect_p2(7,i,j,k)=  zero_single

        end do
     end do

     !--- south polar patch
     do j=1,pf2aP3%nlonf
        do i=1,pf2aP3%nlatf
           asp1=asp13f(i,j); asp2=asp23f(i,j); asp3=asp33f(i,j)

           afact=zero; fx1=one; fx2=one; fx3=one
           if ( flowvar .and. afact0(nvar_id(k))>zero ) then
              afact=afact0(nvar_id(k))
              asp1=scalex1*asp1; fx1=real(tx1p3_slab(i,j,k),r_kind)
              asp2=scalex2*asp2; fx2=real(tx2p3_slab(i,j,k),r_kind)
              asp3=scalex3*asp3; fx3=real(tx3p3_slab(i,j,k),r_kind)
           endif

           aspect_p3(1,i,j,k) = real(one/asp1**2+afact*fblend*fx1*fx1/(qlth**2)  ,r_single) ! 1st (y) direction    x1*x1
           aspect_p3(2,i,j,k) = real(one/asp2**2+afact*fblend*fx2*fx2/(qlth**2)  ,r_single) ! 2nd (x) direction    x2*x2
           aspect_p3(3,i,j,k) = real(one/asp3**2+afact*fblend*fx3*fx3/(qltv**2)  ,r_single) ! 3rd (z) direction    x3*x3
           aspect_p3(4,i,j,k) = real(            afact*fblend*fx3*fx2/(qlth*qltv),r_single) !  x3*x2
           aspect_p3(5,i,j,k) = real(            afact*fblend*fx3*fx1/(qlth*qltv),r_single) !  x3*x1
           aspect_p3(6,i,j,k) = real(            afact*fblend*fx2*fx1/(qlth**2)  ,r_single) !  x2*x1
           aspect_p3(7,i,j,k)=  zero_single

        end do
     end do

  end do

  call destroy_iso_array_glb

  deallocate(qlth_temp,qltv_temp)
  deallocate(qlth_wind,qltv_wind)

  deallocate(asp1_max,asp1p2_max,asp1p3_max )
  deallocate(asp2_max,asp2p2_max,asp2p3_max)
  deallocate(asp3_max,asp3p2_max,asp3p3_max)

  deallocate(tx1_slab,tx1p2_slab,tx1p3_slab)
  deallocate(tx2_slab,tx2p2_slab,tx2p3_slab)
  deallocate(tx3_slab,tx3p2_slab,tx3p3_slab)

end subroutine get_aspect_pt
!=======================================================================
!=======================================================================
subroutine get_theta_corrl_lenghts_glb(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_theta_corrl_lenghts_glb
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
! abstract: compute function correlations lengths of Riishojgaard-type
!           anisotropic auto-correlation model
!
! program history log:
!   2006-08-01  pondeca
!   2007-09-19  sato : for global
!   2010-03-11  zhu  - use nrf* for generalized control variable
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: four
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) i,j,k,kp,km,k1,it
  integer(i_kind) mcount0,mcount
  character(len= 5):: cvar

  real(r_kind) dzi

  real(r_kind) pbar4a,pbar4(nsig),hgt4(nsig),tbar4(nsig), &
               thetabar4(nsig),dthetabarz(nsig),dthetabarzmax, &
               qlth,qltv

!compute scaling factors for the function correlation length
  it=ntguessig
  do k=1,nsig
     pbar4a=zero
     do j=1,lon2
        do i=1,lat2
           pbar4a=pbar4a+ges_prsl(i,j,k ,it)*10._r_kind
        end do
     end do
     mcount0=lon2*lat2! It's OK to count buffer points
     call mpi_allreduce(pbar4a,pbar4(k),1,mpi_real8,mpi_sum,mpi_comm_world,ierror)
     call mpi_allreduce(mcount0,mcount,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
     pbar4(k)=pbar4(k)/float(mcount)
     call w3fa03(pbar4(k),hgt4(k),tbar4(k),thetabar4(k))
  end do

  dthetabarzmax=zero
  do k=1,nsig
     kp=min(nsig,k+1)
     km=max(1,k-1)
     dzi=one/(kp-km)
     dthetabarz(k)=dzi*(thetabar4(kp)-thetabar4(km))
     dthetabarzmax=max(dthetabarz(k),dthetabarzmax)
     if(mype==0) then
        write(6,'("in get_theta_corrl_lenghts_glb,k,pbar4,hgt4,tbar4=",i4,3f11.3)') k,pbar4(k),hgt4(k),tbar4(k)
        write(6,'("in get_theta_corrl_lenghts_glb,k,thetabar4,dthetabarz=",i4,2f11.3)') k,thetabar4(k),dthetabarz(k)
     endif
  end do
  if(mype==0) write(6,*)'in get_theta_corrl_lenghts_glb,dthetabarzmax=',dthetabarzmax

  do k=1,nsig
     dthetabarz(k)=dthetabarz(k)/dthetabarzmax
     if(mype==0) then
        write(6,*)'in get_theta_corrl_lenghts_glb,k,normalized dthetabarz=',k,dthetabarz(k)
     endif
  end do

  do k=1,nsig
     qlth_temp(k)=qlth_temp0
     qlth_wind(k)=qlth_wind0
     if (k<=kthres) then
        qltv_temp(k)=qltv_temp0
        qltv_wind(k)=qltv_wind0
     else
        qltv_temp(k)=qltv_temp0*dthetabarz(k)/dthetabarz(kthres)*four
        qltv_wind(k)=qltv_wind0*dthetabarz(k)/dthetabarz(kthres)*four
     endif
  end do

  call hanning_smther(qltv_temp, nsig, 5)
  call hanning_smther(qltv_wind, nsig, 5)

  if (mype==0) then
     do k=1,nsig
        write(6,*)'in get_theta_corrl_lenghts_glb,k,qltv_temp,qltv_wind=',k,qltv_temp(k),qltv_wind(k)
     enddo
  endif

! if (mype==0) then
!    open (94,file='std_atm.dat',form='unformatted')
!    write(94) pbar4
!    write(94) hgt4
!    write(94) tbar4
!    write(94) thetabar4
!    write(94) dthetabarz
!    close(94)
! endif

!-----------------------------------------------------------
!-----define the anisotropic aspect tensor------------------
!-----------------------------------------------------------
! Set up scales

  asp1_max(:,:)=zero
  asp2_max(:,:)=zero
  asp3_max(:,:)=zero

  do k=1,nsig1o
     k1=levs_id(k)
     if (k1==0)  cycle      !  skip to next k value
     call isotropic_scales_glb(asp10f,asp20f,asp30f, &
                               asp12f,asp22f,asp32f, &
                               asp13f,asp23f,asp33f,k)

     asp1_max(nvar_id(k),k1)=maxval(asp10f)
     asp2_max(nvar_id(k),k1)=maxval(asp20f)
     asp3_max(nvar_id(k),k1)=maxval(asp30f)

     asp1p2_max(nvar_id(k),k1)=maxval(asp12f)
     asp2p2_max(nvar_id(k),k1)=maxval(asp22f)
     asp3p2_max(nvar_id(k),k1)=maxval(asp32f)

     asp1p3_max(nvar_id(k),k1)=maxval(asp13f)
     asp2p3_max(nvar_id(k),k1)=maxval(asp23f)
     asp3p3_max(nvar_id(k),k1)=maxval(asp33f)

  end do

!-----use smoothed guess to compute fields of bounded horizontal
!     and vertical derivatives. Then smooth the resulting fields

  tx1_slab(:,:,:)=zero_single
  tx2_slab(:,:,:)=zero_single
  tx3_slab(:,:,:)=zero_single

  tx1p2_slab(:,:,:)=zero_single
  tx2p2_slab(:,:,:)=zero_single
  tx3p2_slab(:,:,:)=zero_single

  tx1p3_slab(:,:,:)=zero_single
  tx2p3_slab(:,:,:)=zero_single
  tx3p3_slab(:,:,:)=zero_single

  do k=1,nsig1o
     k1=levs_id(k)
     if (k1==0)  cycle      !  skip to next k value

     qlth=one
     qltv=one
     cvar=trim(nrf_var(nvar_id(k)))
     select case(cvar)
        case('sf','SF'); qlth=qlth_wind(k1) ; qltv=qltv_wind(k1)
        case('vp','VP'); qlth=qlth_wind(k1) ; qltv=qltv_wind(k1)
        case('t','T'); qlth=qlth_temp(k1) ; qltv=qltv_temp(k1)
     end select

     call mk_gradpt_slab( &
                 pf2aP1%nlatf,pf2aP1%nlonf, &
                 tx1_slab(1,1,k), &
                 tx2_slab(1,1,k), &
                 tx3_slab(1,1,k), &
                 theta0f (1,1,k), &
                 theta0zf(1,1,k), &
                 asp1_max(nvar_id(k),k1), &
                 asp2_max(nvar_id(k),k1), &
                 asp3_max(nvar_id(k),k1), &
                 qlth,qltv,nvar_id(k))

     call mk_gradpt_slab( &
                 pf2aP2%nlatf,pf2aP2%nlonf, &
                 tx1p2_slab(1,1,k), &
                 tx2p2_slab(1,1,k), &
                 tx3p2_slab(1,1,k), &
                 theta2f (1,1,k), &
                 theta2zf(1,1,k), &
                 asp1p2_max(nvar_id(k),k1), &
                 asp2p2_max(nvar_id(k),k1), &
                 asp3p2_max(nvar_id(k),k1), &
                 qlth,qltv,nvar_id(k), &
                 ilatf=p2ilatf)

     call mk_gradpt_slab( &
                 pf2aP3%nlatf,pf2aP3%nlonf, &
                 tx1p3_slab(1,1,k), &
                 tx2p3_slab(1,1,k), &
                 tx3p3_slab(1,1,k), &
                 theta3f (1,1,k), &
                 theta3zf(1,1,k), &
                 asp1p3_max(nvar_id(k),k1), &
                 asp2p3_max(nvar_id(k),k1), &
                 asp3p3_max(nvar_id(k),k1), &
                 qlth,qltv,nvar_id(k), &
                 ilatf=p3ilatf)

  end do


end subroutine get_theta_corrl_lenghts_glb
!=======================================================================
!=======================================================================
subroutine isotropic_scales_glb( &
             scale1   ,scale2   ,scale3,    &
             scale_np1,scale_np2,scale_np3, &
             scale_sp1,scale_sp2,scale_sp3,k)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   isotropic_scales_glb
! prgmmr: sato               org: np23                date: 2007-09-19
!
! abstract: compute isotropic length scales of auto-correlation model
!           for global mode.
!           built from pondeca's anprewgt_reg
!
! program history log:
!   2007-09-19  sato
!   2010-03-11  zhu - use nrf* for generalized control variables
!
!   input argument list:
!    k        - level number of field in slab mode
!
!   output argument list:
!    scale1     - 2d field of correlations lengths in the x-direction
!    scale2     - 2d field of correlations lengths in the y-direction
!    scale3     - 2d field of correlations lengths in the z-direction
!
!    scale_np1  - 2d field of correlations lengths in the x-direction for north polar patch
!    scale_np2  - 2d field of correlations lengths in the y-direction for north polar patch
!    scale_np3  - 2d field of correlations lengths in the z-direction for north polar patch
!
!    scale_sp1  - 2d field of correlations lengths in the x-direction for south polar patch
!    scale_sp2  - 2d field of correlations lengths in the y-direction for south polar patch
!    scale_sp3  - 2d field of correlations lengths in the z-direction for south polar patch
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

!Declare passed variables
  integer(i_kind),intent(in   ) :: k

  real(r_kind)   ,intent(  out) :: scale1(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)   ,intent(  out) :: scale2(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)   ,intent(  out) :: scale3(pf2aP1%nlatf,pf2aP1%nlonf)

  real(r_kind)   ,intent(  out) :: scale_np1(pf2aP2%nlatf,pf2aP2%nlonf)
  real(r_kind)   ,intent(  out) :: scale_np2(pf2aP2%nlatf,pf2aP2%nlonf)
  real(r_kind)   ,intent(  out) :: scale_np3(pf2aP2%nlatf,pf2aP2%nlonf)

  real(r_kind)   ,intent(  out) :: scale_sp1(pf2aP3%nlatf,pf2aP3%nlonf)
  real(r_kind)   ,intent(  out) :: scale_sp2(pf2aP3%nlatf,pf2aP3%nlonf)
  real(r_kind)   ,intent(  out) :: scale_sp3(pf2aP3%nlatf,pf2aP3%nlonf)

!Declare local variables
  integer(i_kind) i,j,k1,ivar,ifl,iflm,smx,n

  real(r_kind) hwll_loc

  k1=levs_id(k)
  ivar=nvar_id(k)

  !--- zonal patch
  iflm = aint(p0ilatf(pf2aP1%nlatf/2))
  iflm = min(max(1,iflm),nlat)
  do j=1,pf2aP1%nlonf
     do i=1,pf2aP1%nlatf
        ifl  = aint(p0ilatf(i))
        ifl  = min(max(1,ifl),nlat)
        scale3(i,j)=one
 
        if (nrf2_sst>0 .and. ivar>nrf) then
           hwll_loc=hwllsstmin     ! surface temp (land & ice)
        else if (nrf_3d(ivar)) then
           do n=1,nrf3
              if (nrf3_loc(n)==ivar) then 
!                if(k1 >= ks(k))then
!                   hwll_loc=hwll(iflm,k1,n)
!                   scale3(i,j)=one/vz(k1,iflm,n)
!                else
                    hwll_loc=hwll(ifl,k1,n)
                    scale3(i,j)=one/vz(k1,ifl,n)
!                end if
                 exit
              end if
           end do
        else
           do n=1,nrf2
              if (nrf2_loc(n)==ivar) then
                 if (n==nrf2_sst) then 
                    hwll_loc=hwllsst0f(i,j) ! SST
                 else
                    hwll_loc=hwllp(ifl,n)
                 end if
                 exit
              end if
           end do   
        end if

        scale1(i,j)=hwll_loc/dyzf(i)
        scale2(i,j)=hwll_loc/dxzf(i)
 
        ! rescaling to roughly match original analysis from purely isotropic
        ! option, ie.. when anisotropic=.false. in namelist "anbkgerr".
 
        if(opt_sclclb_glb==0) then
           scale1(i,j)=rfact0h(ivar)*scale1(i,j)
           scale2(i,j)=rfact0h(ivar)*scale2(i,j)
        else if(opt_sclclb_glb==1) then
           scale1(i,j)=scale1(i,j)**rfact0h(1) + rfact0h(2)
           scale2(i,j)=scale2(i,j)**rfact0h(1) + rfact0h(2)
        end if
        if (nrf_3d(ivar)) then 
           if(opt_sclclb_glb==0) then
              scale3(i,j)=rfact0v(ivar)*scale3(i,j)
           else if(opt_sclclb_glb==1) then
              scale3(i,j)=scale3(i,j)**rfact0v(1) + rfact0v(2)
           end if
        end if
      
     end do
  end do

  smx=maxval(scale1)
  where( scale1 <= zero ) scale1=smx
  smx=maxval(scale2)
  where( scale2 <= zero ) scale2=smx

  !--- polar patches
  !--- north polar patch
  iflm = floor(p2ilatfm)
  iflm = min(max(iflm,1),nlat)
  do j=1,pf2aP2%nlonf
     do i=1,pf2aP2%nlatf
        if( p2ilatf(i,j)/=zero ) then
           ifl=aint(p2ilatf(i,j))
           ifl=min(max(ifl,1),nlat)
        else
           ifl=iflm
        end if
        scale_np3(i,j)=one

        if (nrf2_sst>0 .and. ivar>nrf) then
           hwll_loc=hwllsstmin     ! surface temp (land & ice)
        else if (nrf_3d(ivar)) then
           do n=1,nrf3
              if (nrf3_loc(n)==ivar) then
!                if(k1 >= ks(k))then
!                   hwll_loc=hwll(iflm,k1,n)
!                   scale_np3(i,j)=one/vz(k1,iflm,n)
!                else
                 hwll_loc=hwll(ifl,k1,n)
                 scale_np3(i,j)=one/vz(k1,ifl,n)
!                end if
                 exit
              end if
           end do
        else
           do n=1,nrf2
              if (nrf2_loc(n)==ivar) then
                 if (n==nrf2_sst) then
                    hwll_loc=hwllsst2f(i,j)  ! SST
                 else
                    hwll_loc=hwllp(ifl,n) 
                 end if
                 exit
              end if
           end do
        end if

        scale_np1(i,j)=hwll_loc/dypf(i,j)
        scale_np2(i,j)=hwll_loc/dxpf(i,j)

        if(opt_sclclb_glb==0) then
           scale_np1(i,j)=rfact0h(ivar)*scale_np1(i,j)
           scale_np2(i,j)=rfact0h(ivar)*scale_np2(i,j)
        else if(opt_sclclb_glb==1) then
           scale_np1(i,j)=scale_np1(i,j)**rfact0h(1) + rfact0h(2)
           scale_np2(i,j)=scale_np2(i,j)**rfact0h(1) + rfact0h(2)
        end if
 
        if (nrf_3d(ivar)) then
           if(opt_sclclb_glb==0) then
              scale_np3(i,j)=rfact0v(ivar)*scale_np3(i,j)
           else if(opt_sclclb_glb==1) then
              scale_np3(i,j)=scale_np3(i,j)**rfact0v(1) + rfact0v(2)
           end if
        end if

     end do
  end do

  smx=maxval(scale_np1)
  where( scale_np1 <= zero ) scale_np1=smx
  smx=maxval(scale_np2)
  where( scale_np2 <= zero ) scale_np2=smx

  !--- south polar patch
  iflm = ceiling(p3ilatfm)
  iflm = min(max(iflm,1),nlat)
  do j=1,pf2aP3%nlonf
     do i=1,pf2aP3%nlatf
        if( p3ilatf(i,j)/=zero ) then
           ifl=nint(p3ilatf(i,j))
           ifl=min(max(ifl,1),nlat)
        else
           ifl=iflm
        end if
        scale_sp3(i,j)=one
 
        if (nrf2_sst>0 .and. ivar>nrf) then
           hwll_loc=hwllsstmin     ! surface temp (land & ice)
        else if (nrf_3d(ivar)) then
           do n=1,nrf3
              if (nrf3_loc(n)==ivar) then
!                if(k1 >= ks(k))then
!                   hwll_loc=hwll(iflm,k1,n)
!                   scale_sp3(i,j)=one/vz(k1,iflm,n)
!                else
                 hwll_loc=hwll(ifl,k1,n)
                 scale_sp3(i,j)=one/vz(k1,ifl,n)
!                end if
                 exit
              end if
           end do
        else
           do n=1,nrf2
              if (nrf2_loc(n)==ivar) then
                 if (n==nrf2_sst) then
                    hwll_loc=hwllsst3f(i,j) ! SST
                 else
                    hwll_loc=hwllp(ifl,n)
                 end if
                 exit
              end if
           end do
        end if

        scale_sp1(i,j)=hwll_loc/dypf(i,j)
        scale_sp2(i,j)=hwll_loc/dxpf(i,j)
 
        if(opt_sclclb_glb==0) then
           scale_sp1(i,j)=rfact0h(ivar)*scale_sp1(i,j)
           scale_sp2(i,j)=rfact0h(ivar)*scale_sp2(i,j)
        else if(opt_sclclb_glb==1) then
           scale_sp1(i,j)=scale_sp1(i,j)**rfact0h(1) + rfact0h(2)
           scale_sp2(i,j)=scale_sp2(i,j)**rfact0h(1) + rfact0h(2)
        end if

        if (nrf_3d(ivar)) then
           if(opt_sclclb_glb==0) then
              scale_sp3(i,j)=rfact0v(ivar)*scale_sp3(i,j)
           else if(opt_sclclb_glb==1) then
              scale_sp3(i,j)=scale_sp3(i,j)**rfact0v(1) + rfact0v(2)
           end if
        end if

     end do
  end do

  smx=maxval(scale_sp1)
  where( scale_sp1 <= zero ) scale_sp1=smx
  smx=maxval(scale_sp2)
  where( scale_sp2 <= zero ) scale_sp2=smx

  return
end subroutine isotropic_scales_glb
!=======================================================================
subroutine anbkgvar_rewgt(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   anbkgvar_rewgt
! prgmmr: sato               org: np23                date: 2007-11-07
!
! abstract: interface to bkgvar_rewgt()
!
! program history log:
!   2007-11-07  sato
!   2010-03-03  zhu  - use nrf*
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use gridmod, only: istart
  use sub2fslab_mod, only: setup_sub2fslab, destroy_sub2fslab, &
                           sub2fslab_glb, sub2fslab2d_glb
  implicit none

  integer(i_kind),intent(in   ) :: mype

  integer(i_kind):: i,j,k,ix,ier,mm1

  real(r_kind),dimension(lat2,lon2,nsig):: sfvar,vpvar,tvar
  real(r_kind),dimension(lat2,lon2):: psvar

  real(r_kind),allocatable,dimension(:,:,:)::field

  mm1=mype+1

  do k=1,nsig
     do i=1,lat2
        ix=istart(mm1)+i-2
        ix=max(ix,2)
        ix=min(nlat-1,ix)
        do j=1,lon2
           sfvar(i,j,k)=corz(ix,k,nrf3_sf)
           vpvar(i,j,k)=corz(ix,k,nrf3_vp)
           tvar (i,j,k)=corz(ix,k,nrf3_t)
        end do
     end do
  end do

  do i=1,lat2
     ix=istart(mm1)+i-2
     ix=max(ix,2)
     ix=min(nlat-1,ix)
     do j=1,lon2
        psvar(i,j)=corp(ix,nrf2_ps)
     end do
  end do

  call bkgvar_rewgt(sfvar,vpvar,tvar,psvar,mype)

! convert variance data into filtered grid

  allocate(sfvar0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(vpvar0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(tvar0f (pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
  allocate(psvar0f(pf2aP1%nlatf,pf2aP1%nlonf))

  allocate(sfvar2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
  allocate(vpvar2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
  allocate(tvar2f (pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
  allocate(psvar2f(pf2aP2%nlatf,pf2aP2%nlonf))

  allocate(sfvar3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))
  allocate(vpvar3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))
  allocate(tvar3f (pf2aP3%nlatf,pf2aP3%nlonf,nsig1o))
  allocate(psvar3f(pf2aP3%nlatf,pf2aP3%nlonf))

  allocate(field (lat2,lon2,nsig))

  if(ier/=0) then
     write(6,*) 'anbkgvar_rewgt(): could not allocate memories'
     call stop2(stpcode_alloc)
  end if

  call setup_sub2fslab
  call sub2fslab_glb  (sfvar,sfvar0f,sfvar2f,sfvar3f)
  call sub2fslab_glb  (vpvar,vpvar0f,vpvar2f,vpvar3f)
  call sub2fslab_glb  (tvar ,tvar0f ,tvar2f ,tvar3f )
  call sub2fslab2d_glb(psvar,psvar0f,psvar2f,psvar3f)
  call destroy_sub2fslab
  if(mype==0) write(6,*) 'psvar_chk:',maxval(psvar),minval(psvar),maxval(psvar0f),maxval(psvar2f),maxval(psvar3f)

  deallocate(field)

end subroutine anbkgvar_rewgt
!=======================================================================
!=======================================================================
subroutine get_aspect_ens(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_aspect_ens
! prgmmr: sato             org: np22                date: 2007-11-16
!
! abstract: define the anisotropic aspect tensor based on ensemble info
!
! program history log:
!   2007-11-16  sato - for global
!   2008-01-15  sato - add more accurate blending of iso & aniso tensor
!   2010-03-11  zhu  - use nvars from control_vectors
!   2013-10-24  todling - pges_minmax now gets time slot as input
!
!   input argument list:
!    mype     - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: three
  use anberror, only: afact0
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  real(r_single),allocatable,dimension(:,:,:,:):: aniasp_p0, aniasp_p2, aniasp_p3
  real(r_single),allocatable,dimension(:,:,:)  :: ensv_p0,   ensv_p2,   ensv_p3
  real(r_single),allocatable,dimension(:,:,:)  :: ens0f_p0,  ens0f_p2,  ens0f_p3
  real(r_single),allocatable,dimension(:,:,:)  :: ens0zf_p0, ens0zf_p2, ens0zf_p3
  real(r_kind)  ,allocatable,dimension(:,:,:)  :: enscoeff
  integer(i_kind),allocatable,dimension(:,:)   :: iref,jref

  integer(i_kind) nflds                      !# of distinct physical flds in each ens member
  integer(i_kind) ifldlevs(nmax_ensfld)      !# of vert levels in each physical fld of each ens member
  integer(i_kind) kens,ntens

  integer(i_kind) nflag(nvars),nkflag(nsig1o) !dimension is # of anl. variables
  integer(i_kind) kflag(nsig)

  real(r_single) qlxmin(nvars,nsig) !10 is # of analysis variables
  real(r_single) qlymin(nvars,nsig) !10 is # of analysis variables
  real(r_single) qlzmin(nvars,nsig) !10 is # of analysis variables

  real(r_single):: afact
  real(r_single):: rescvar(10)
  real(r_single):: rescvarzadj(10)
  real(r_single),parameter:: rperc=0.01_r_single
  real(r_single),parameter:: one_single=1.0_r_single

  logical:: lres1,lres2,lres3
  real(r_single) s1,s2,s3,smax,ensamp_mod
  real(r_kind):: aensv(6,nsig1o)
  real(r_kind):: aensv_p0(3,nsig1o),aensv_p2(3,nsig1o),aensv_p3(3,nsig1o)
  integer(i_kind):: nlatlonf,nsmp

  integer(i_kind):: idiagens ! if = 1 then print out diganostic info
  integer(i_kind):: icovmap  ! flag for output covariance map
  integer(i_kind):: icorlim  ! if = 1 then vertical correlation length is restricted
  integer(i_kind):: ibldani  ! 0: use simple formulation to blend iso-aniso aspect
                             ! 1: use Jim's formulation
                             ! 2: use Sato's formulation
  logical:: unbalens         ! if true, covariance model is based on unbalanced part for chi,t,psfc
                             ! if false,covariance model is based on full values for chi,t,psfc
  integer(i_kind):: iensamp  ! if = 1 then uses ensemble spread to multiply for an_amp

  integer(i_kind):: i,j,k,k1,nt1,m,nens(nsig1o),igrid,ifld,ivar,ier
  real(r_kind):: scalex1ens,scalex2ens,scalex3ens,asp1,asp2,asp3

  namelist/ensparam/ntens,nflds,ifldlevs, &
                    rescvar,rescvarzadj,idiagens,icovmap,icorlim, &
                    unbalens,ibldani,iensamp,scalex1ens, scalex2ens, scalex3ens

  data idiagens  / 0 /
  data icovmap   / 0 /
  data icorlim   / 0 /
  data ibldani   / 0 /
  data iensamp   / 0 /
  data unbalens / .false. /
  data rescvar / one,one,one,one,one, one,one,one,one,one /
  data rescvarzadj / one,one,one,one,one, one,one,one,one,one /

!-----------------------------------------------------------
!-----define the aspect tensor------------------------------
!-----------------------------------------------------------

!==> ensemble parameter:

  ntens   =0
  nflds   =0
  ifldlevs=0

!==> isotropic contribution: -> will be changed by namelist parameters
  scalex1ens   = 1.2_r_kind
  scalex2ens   = 1.2_r_kind
  scalex3ens   = 1.2_r_kind
!---

  open (55,file='ensparam_input',form='formatted')
  read (55,ensparam)
  close(55)

  if( nflds > nmax_ensfld ) then
     write(6,*) 'nflds (',nflds,') must be smaller than / equal to nmax_ensfld (',nmax_ensfld,')'
     call stop2(stpcode_namelist)
  end if

  if(icovmap==1) covmap=.true.

!-----------------------------------------------------------

  if (mype==0) then
     write(6,ensparam)
     do m=1,nflds
        print*,'in get_aspect_ens:, m,ifldlevs(m)=',m,ifldlevs(m)
     end do
  end if

  call create_iso_array_glb
  do k=1,nsig1o

     ivar=nvar_id(k)
     k1=levs_id(k)
     if (k1==0) cycle

     call isotropic_scales_glb(asp10f,asp20f,asp30f, &
                               asp12f,asp22f,asp32f, &
                               asp13f,asp23f,asp33f,k)

     !--- zonal patch
     do j=1,pf2aP1%nlonf
        do i=1,pf2aP1%nlatf
           if( afact0(ivar) < zero ) then
              asp1=asp10f(i,j)
              asp2=asp20f(i,j)
              asp3=asp30f(i,j)
           else
              asp1=scalex1ens*asp10f(i,j)
              asp2=scalex2ens*asp20f(i,j)
              asp3=scalex3ens*asp30f(i,j)
           endif
           aspect(1,i,j,k)  = real(one/asp1**2,r_single)  ! 1st (y) direction    x1*x1
           aspect(2,i,j,k)  = real(one/asp2**2,r_single)  ! 2nd (x) direction    x2*x2
           aspect(3,i,j,k)  = real(one/asp3**2,r_single)  ! 3rd (z) direction    x3*x3
           aspect(4:7,i,j,k)= zero_single                 ! x3*x2, x3*x1, x2*x1
        end do
     end do

     !--- north polar patch
     do j=1,pf2aP2%nlonf
        do i=1,pf2aP2%nlatf
           if( afact0(ivar) < zero ) then
              asp1=asp12f(i,j)
              asp2=asp22f(i,j)
              asp3=asp32f(i,j)
           else
              asp1=scalex1ens*asp12f(i,j)
              asp2=scalex2ens*asp22f(i,j)
              asp3=scalex3ens*asp32f(i,j)
           endif
           aspect_p2(1,i,j,k)  = real(one/asp1**2,r_single) ! 1st (y) direction    x1*x1
           aspect_p2(2,i,j,k)  = real(one/asp2**2,r_single) ! 2nd (x) direction    x2*x2
           aspect_p2(3,i,j,k)  = real(one/asp3**2,r_single) ! 3rd (z) direction    x3*x3
           aspect_p2(4:7,i,j,k)= zero_single                ! x3*x2, x3*x1, x2*x1
        end do
     end do

     !--- south polar patch
     do j=1,pf2aP3%nlonf
        do i=1,pf2aP3%nlatf
           if( afact0(ivar) >= zero ) then
              asp1=asp13f(i,j)
              asp2=asp23f(i,j)
              asp3=asp33f(i,j)
           else
              asp1=scalex1ens*asp13f(i,j)
              asp2=scalex2ens*asp23f(i,j)
              asp3=scalex3ens*asp33f(i,j)
           endif
           aspect_p3(1,i,j,k)  = real(one/asp1**2,r_single) ! 1st (y) direction    x1*x1
           aspect_p3(2,i,j,k)  = real(one/asp2**2,r_single) ! 2nd (x) direction    x2*x2
           aspect_p3(3,i,j,k)  = real(one/asp3**2,r_single) ! 3rd (z) direction    x3*x3
           aspect_p3(4:7,i,j,k)= zero_single                ! x3*x2, x3*x1, x2*x1
        end do
     end do

  end do
  call destroy_iso_array_glb

!==> ensemble contribution:

  if (ntens>0) then !see mark-0

     allocate(pgesmin(nsig))           !vert. profile of bckg layer minimum pressure
     allocate(pgesmax(nsig))           !vert. profile of bckg layer maximum pressure
     call pges_minmax(mype,ntguessig,pgesmin,pgesmax)

     allocate(enscoeff(4,nlat,nlon))
     allocate(iref(nlat,nlon))
     allocate(jref(nlat,nlon))
     call ens_intpglb_coeff(iref,jref,enscoeff,mype)

     allocate(aniasp_p0(6,pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
     allocate(aniasp_p2(6,pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
     allocate(aniasp_p3(6,pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o),stat=ier)
     if(ier/=0) then
        write(6,*) 'get_aspect_ens(): could not allocate aniasp)'
        call stop2(stpcode_alloc)
     end if

     allocate(ensv_p0(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
     allocate(ensv_p2(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
     allocate(ensv_p3(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o),stat=ier)
     if(ier/=0) then
        write(6,*) 'get_aspect_ens(): could not allocate ensv)'
        call stop2(stpcode_alloc)
     end if

     allocate(ens0f_p0(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
     allocate(ens0f_p2(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
     allocate(ens0f_p3(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o),stat=ier)
     if(ier/=0) then
        write(6,*) 'get_aspect_ens(): could not allocate ens0f'
        call stop2(stpcode_alloc)
     end if

     allocate(ens0zf_p0(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
     allocate(ens0zf_p2(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o))
     allocate(ens0zf_p3(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o),stat=ier)
     if(ier/=0) then
        write(6,*) 'get_aspect_ens(): could not allocate ens0zf'
        call stop2(stpcode_alloc)
     end if

     nens  =0
     nkflag=0

     aniasp_p0=zero_single
     aniasp_p2=zero_single
     aniasp_p3=zero_single
 
     ensv_p0=zero_single
     ensv_p2=zero_single
     ensv_p3=zero_single

     do kens=1,ntens
        do ifld=1,nflds
           if (ifldlevs(ifld)==0) cycle

           igrid=3
           nflag(:)=0
           kflag(:)=0

           call get_ensmber_glb(kens,ifld,igrid,ifldlevs, &
                                ens0f_p0 (1,1,1), ens0f_p2 (1,1,1), ens0f_p3 (1,1,1), &
                                ens0zf_p0(1,1,1), ens0zf_p2(1,1,1), ens0zf_p3(1,1,1), &
                                pgesmax(1),pgesmin(1), &
                                iref,jref, enscoeff(1,1,1), &
                                nflag(1),kflag(1),idiagens,unbalens,mype)
 
           lres1=any(nflag(:)==1)
           lres2=any(kflag(:)==1)
           if ( .not.lres1 .or. .not.lres2 ) cycle
 
           do k=1,nsig1o

              ivar=nvar_id(k)
              k1  =levs_id(k)

              if ( ivar==0 .or. k1==0 .or. &
                   nflag(ivar) /=1 .or. kflag(k1) /=1 ) cycle

              nkflag(k)=1
              nens(k)=nens(k)+1

              call add_ensinfo_aniasp(pf2aP1%nlatf,pf2aP1%nlonf, &
                                      ens0f_p0(1,1,k),ens0zf_p0(1,1,k), &
                                      ensv_p0 (1,1,k),aniasp_p0(1,1,1,k))

              call add_ensinfo_aniasp(pf2aP2%nlatf,pf2aP2%nlonf, &
                                      ens0f_p2(1,1,k),ens0zf_p2(1,1,k), &
                                      ensv_p2 (1,1,k),aniasp_p2(1,1,1,k))

              call add_ensinfo_aniasp(pf2aP3%nlatf,pf2aP3%nlonf, &
                                      ens0f_p3(1,1,k),ens0zf_p3(1,1,k), &
                                      ensv_p3 (1,1,k),aniasp_p3(1,1,1,k))

           end do

        end do !end of ifld do-loop
     end do   !end of kens do-loop

     deallocate(ens0f_p0, ens0f_p2, ens0f_p3 )
     deallocate(ens0zf_p0,ens0zf_p2,ens0zf_p3)
     deallocate(enscoeff,iref,jref)
     deallocate(pgesmin,pgesmax)

!==> rescale variances
     do k=1,nsig1o
        ivar=nvar_id(k)
        k1  =levs_id(k)
        if (ivar==0 .or. k1==0) cycle
        ensv_p0(:,:,k)=rescvar(ivar)*sqrt(ensv_p0(:,:,k))
        ensv_p2(:,:,k)=rescvar(ivar)*sqrt(ensv_p2(:,:,k))
        ensv_p3(:,:,k)=rescvar(ivar)*sqrt(ensv_p3(:,:,k))
     end do

!==> compute reasonable lower bounds for variances
     do k=1,nsig1o

        ivar=nvar_id(k)
        k1  =levs_id(k)
        if (ivar==0 .or. k1==0) cycle

        nt1=max(1,(nens(k)-1))
 
        s1=maxval(ensv_p0(:,:,k))/float(nt1)
        s2=maxval(ensv_p2(:,:,k))/float(nt1)
        s3=maxval(ensv_p3(:,:,k))/float(nt1)
        smax=max(s1,s2,s3)

        if ( nkflag(k)==1 ) then
           qlxmin(ivar,k1)=rperc*smax
           qlymin(ivar,k1)=rperc*smax
           qlzmin(ivar,k1)=rperc*smax
        else
           qlxmin(ivar,k1)=one_single
           qlymin(ivar,k1)=one_single
           qlzmin(ivar,k1)=one_single
        endif

     end do

!==> compute averages over each type of ens grid:
     do k=1,nsig1o

        ivar=nvar_id(k)
        k1  =levs_id(k)
        if (ivar==0 .or. k1==0) cycle

        nt1=max(1,(nens(k)-1))
        ensv_p0  (:,:,k)  =ensv_p0  (:,:,k)  /real(nt1,r_single)
        ensv_p2  (:,:,k)  =ensv_p2  (:,:,k)  /real(nt1,r_single)
        ensv_p3  (:,:,k)  =ensv_p3  (:,:,k)  /real(nt1,r_single)
        aniasp_p0(:,:,:,k)=aniasp_p0(:,:,:,k)/real(nt1,r_single)
        aniasp_p2(:,:,:,k)=aniasp_p2(:,:,:,k)/real(nt1,r_single)
        aniasp_p3(:,:,:,k)=aniasp_p3(:,:,:,k)/real(nt1,r_single)
 
        if(ibldani/=1) then
           call mk_ens_aniasp(pf2aP1%nlatf,pf2aP1%nlonf, &
                              qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                              ensv_p0(1,1,k),aniasp_p0(1,1,1,k))
           call mk_ens_aniasp(pf2aP2%nlatf,pf2aP2%nlonf, &
                              qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                              ensv_p2(1,1,k),aniasp_p2(1,1,1,k))
           call mk_ens_aniasp(pf2aP3%nlatf,pf2aP3%nlonf, &
                              qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                              ensv_p3(1,1,k),aniasp_p3(1,1,1,k))
        else
           call get_avgensv(pf2aP1%nlatf,pf2aP1%nlonf, &
                            qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                            ensv_p0(1,1,k),aensv_p0(1,k))
           call get_avgensv(pf2aP1%nlatf,pf2aP1%nlonf, &
                            qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                            ensv_p2(1,1,k),aensv_p2(1,k))
           call get_avgensv(pf2aP1%nlatf,pf2aP1%nlonf, &
                            qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                            ensv_p3(1,1,k),aensv_p3(1,k))
           do i=1,3
              aensv(i,k)=(aensv_p0(i,k)+aensv_p2(i,k)+aensv_p3(i,k))/three
           end do
           aensv(4,k)=aensv(3,k)*aensv(1,k)
           aensv(5,k)=aensv(3,k)*aensv(2,k)
           aensv(6,k)=aensv(2,k)*aensv(1,k)
           aensv(1,k)=aensv(1,k)*aensv(1,k)
           aensv(2,k)=aensv(2,k)*aensv(2,k)
           aensv(3,k)=aensv(3,k)*aensv(3,k)
        end if
     end do

!==> check the values...
     i=pf2aP1%nlatf/2
     j=pf2aP1%nlonf/2

     do k=1,nsig1o

        ivar=nvar_id(k)
        k1=levs_id(k)
        if (ivar==0 .or. k1==0) cycle
 
        write(6,*) 'get_aspect_ens: ivar,k1,k,nens(k),qlxyzmin=', ivar,k1,k,nens(k),'|', &
                    qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1)
        write(6,*) 'aspect_at_(nlatf/2,nlonf/2): ivar,k1,aspect,ensv=', ivar,'|', k1, &
                    aspect(1:3,i,j,k),'|',aniasp_p0(1:6,i,j,k),'|',ensv_p0(i,j,k)
        write(6,*) 'aspmax_min: ivar,k1,aspmax,aniaspmax,aspmin,aniaspmin=', ivar, k1,'|', &
                    (maxval(aspect(m,:,:,k)),m=1,3),'|', (maxval(aniasp_p0(m,:,:,k)),m=1,6),'|', &
                    (minval(aspect(m,:,:,k)),m=1,3),'|', (minval(aniasp_p0(m,:,:,k)),m=1,6)
     end do

!==> perform blending of various ens grids and
!    finally add isotropic + ensemble contribution together:
     if(iensamp==1) then
        allocate(ensamp0f(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o))
        allocate(ensamp2f(pf2aP2%nlatf,pf2aP2%nlonf,nsig1o))
        allocate(ensamp3f(pf2aP3%nlatf,pf2aP3%nlonf,nsig1o),stat=ier)
        ensamp0f=one_single
        ensamp2f=one_single
        ensamp3f=one_single
     end if

     do k=1,nsig1o

        ivar=nvar_id(k)
        k1=levs_id(k)
        if (ivar==0 .or. k1==0 ) cycle
 
        afact=afact0(ivar)
 
        lres1=any(abs(aniasp_p0(:,:,:,k))>tiny_single)
        lres2=any(abs(aniasp_p2(:,:,:,k))>tiny_single)
        lres3=any(abs(aniasp_p3(:,:,:,k))>tiny_single)
        lres1=(lres1.and.lres2.and.lres3)
 
        if (.not.lres1) then
           afact=zero_single
           cycle
        end if
 
        if(ibldani==0) then
           call add_aniso_effect(pf2aP1%nlatf,pf2aP1%nlonf,&
                                 aspect   (1,1,1,k),aniasp_p0(1,1,1,k),afact)
           call add_aniso_effect(pf2aP2%nlatf,pf2aP2%nlonf,&
                                 aspect_p2(1,1,1,k),aniasp_p2(1,1,1,k),afact)
           call add_aniso_effect(pf2aP3%nlatf,pf2aP3%nlonf,&
                                 aspect_p3(1,1,1,k),aniasp_p3(1,1,1,k),afact)
        else if(ibldani==1) then
           call add_aniso_effect_jim(pf2aP1%nlatf,pf2aP1%nlonf,&
                                 aspect   (1,1,1,k),aniasp_p0(1,1,1,k),afact,&
                                 qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                                 ensv_p0, aensv(1,k))
           call add_aniso_effect_jim(pf2aP2%nlatf,pf2aP2%nlonf,&
                                 aspect_p2(1,1,1,k),aniasp_p2(1,1,1,k),afact,&
                                 qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                                 ensv_p2, aensv(1,k))
           call add_aniso_effect_jim(pf2aP3%nlatf,pf2aP3%nlonf,&
                                 aspect_p3(1,1,1,k),aniasp_p3(1,1,1,k),afact,&
                                 qlxmin(ivar,k1),qlymin(ivar,k1),qlzmin(ivar,k1), &
                                 ensv_p3, aensv(1,k))
        else if(ibldani==2) then
           call add_aniso_effect_sty(pf2aP1%nlatf,pf2aP1%nlonf,&
                                 aspect   (1,1,1,k),aniasp_p0(1,1,1,k),k1)
           call add_aniso_effect_sty(pf2aP2%nlatf,pf2aP2%nlonf,&
                                 aspect_p2(1,1,1,k),aniasp_p2(1,1,1,k),k1)
           call add_aniso_effect_sty(pf2aP3%nlatf,pf2aP3%nlonf,&
                                 aspect_p3(1,1,1,k),aniasp_p3(1,1,1,k),k1)
        end if

        if(iensamp==1.and.lres1) then
        !  NOTE:
        !  Because the polar patches may include error data outside the patch circle,
        !  Only zonal patch data are used to estimate ensemble spread.
           nsmp=100
           nlatlonf=pf2aP1%nlatf*pf2aP1%nlonf
           call mode_val(ensamp0f(1,1,k),nlatlonf,nsmp,ensamp_mod)
           ensamp0f(:,:,k)=ensv_p0(:,:,k)/ensamp_mod
           ensamp2f(:,:,k)=ensv_p2(:,:,k)/ensamp_mod
           ensamp3f(:,:,k)=ensv_p3(:,:,k)/ensamp_mod
        end if
     end do

     if(iensamp==1) then
        where(ensamp0f>EAMPMAX) ensamp0f=EAMPMAX
        where(ensamp0f<EAMPMIN) ensamp0f=EAMPMIN
        where(ensamp2f>EAMPMAX) ensamp2f=EAMPMAX
        where(ensamp2f<EAMPMIN) ensamp2f=EAMPMIN
        where(ensamp3f>EAMPMAX) ensamp3f=EAMPMAX
        where(ensamp3f<EAMPMIN) ensamp3f=EAMPMIN
     end if

  end if !mark-0

  deallocate(ensv_p0  ,ensv_p2  ,ensv_p3)
  deallocate(aniasp_p0,aniasp_p2,aniasp_p3)
  if(allocated(field_st)) deallocate(field_st)

end subroutine get_aspect_ens
!=======================================================================
!=======================================================================
subroutine add_aniso_effect(nlatf,nlonf,asp,aniall,afact)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   add_aniso_effect
! prgmmr: sato             org: np22                date: 2007-11-16
!
! abstract: utility routine for get_aspect_ens.
!           composit isotropic aspect and anisotropic aspect with
!           the coefficient afact
!
! program history log:
!   2007-11-16  sato : for global
!
!   input argument list:
!    nlatf  - # of lat
!    nlonf  - # of lon
!    asp    - isotropic aspect
!    aniall - anisotropic aspect
!    afact  - comppsition coefficient
!
!   output argument list:
!    asp    - composit aspect
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlatf,nlonf
  real(r_single) ,intent(inout) :: asp   (7,nlatf,nlonf)
  real(r_single) ,intent(in   ) :: aniall(6,nlatf,nlonf)
  real(r_single) ,intent(in   ) :: afact

  integer(i_kind):: i,j

  do j=1,nlonf
     do i=1,nlatf
        asp(1,i,j) = asp(1,i,j)+afact*aniall(1,i,j)
        asp(2,i,j) = asp(2,i,j)+afact*aniall(2,i,j)
        asp(3,i,j) = asp(3,i,j)+afact*aniall(3,i,j)
        asp(4,i,j) =            afact*aniall(4,i,j)
        asp(5,i,j) =            afact*aniall(5,i,j)
        asp(6,i,j) =            afact*aniall(6,i,j)
        asp(7,i,j) = zero_single
     end do
  end do

end subroutine add_aniso_effect
!=======================================================================
!=======================================================================
subroutine add_aniso_effect_jim(nlatf,nlonf,asp,aniall,afact,&
                                     qlxmink,qlymink,qlzmink,ensv,aensvk)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   add_aniso_effect_jim
! prgmmr: sato             org: np22                date: 2008-01-15
!
! abstract: utility routine for get_aspect_ens.
!           composit isotropic aspect and anisotropic aspect with
!           the coefficient afact, using Jim's formulation.
!
! program history log:
!   2008-01-15  sato : for global
!
!   input argument list:
!    nlatf  - # of lat
!    nlonf  - # of lon
!    asp    - isotropic aspect
!    aniall - anisotropic aspect
!    afact  - comppsition coefficient
!    qlxmink -
!    qlymink -
!    qlzmink -
!    ensv    - 2D variance stats
!    aensvk  - global averaged variance
!
!   output argument list:
!    asp    - composit aspect
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlatf,nlonf
  real(r_single) ,intent(inout) :: asp   (7,nlatf,nlonf)
  real(r_single) ,intent(in   ) :: aniall(6,nlatf,nlonf)
  real(r_single) ,intent(in   ) :: afact
  real(r_single) ,intent(in   ) :: qlxmink,qlymink,qlzmink
  real(r_single) ,intent(in   ) :: ensv(nlatf,nlonf)
  real(r_kind)   ,intent(in   ) :: aensvk(6)

  real(r_kind):: qlx,qly,qlz
  integer(i_kind):: i,j

  do j=1,nlonf
     do i=1,nlatf
        qlx=max(qlxmink,ensv(i,j))
        qly=max(qlymink,ensv(i,j))
        qlz=max(qlzmink,ensv(i,j))
        asp(1,i,j) =(asp(1,i,j)*aensvk(2)+afact*aniall(1,i,j)) &
                   /(           aensvk(2)+afact*qly**2)
        asp(2,i,j) =(asp(2,i,j)*aensvk(1)+afact*aniall(2,i,j)) &
                   /(           aensvk(1)+afact*qlx**2)
        asp(3,i,j) =(asp(3,i,j)*aensvk(3)+afact*aniall(3,i,j)) &
                   /(           aensvk(3)+afact*qlz**2)
        asp(4,i,j) =                      afact*aniall(4,i,j) &
                   /(           aensvk(4)+afact*qlz*qlx)
        asp(5,i,j) =                      afact*aniall(5,i,j) &
                   /(           aensvk(5)+afact*qlz*qly)
        asp(6,i,j) =                      afact*aniall(6,i,j) &
                   /(           aensvk(6)+afact*qly*qlx)
        asp(7,i,j) = zero_single
     end do
  end do

end subroutine add_aniso_effect_jim
!=======================================================================
!=======================================================================
subroutine add_aniso_effect_sty(nlatf,nlonf,asp,aniall,k1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   add_aniso_effect_sty
! prgmmr: sato             org: np22                date: 2008-03-07
!
! abstract: utility routine for get_aspect_ens.
!           composit isotropic aspect and anisotropic aspect with
!           the coefficient alpha, by Yoshi's formulation
!          (blend shape and magnitude, seperately).
!
! program history log:
!   2007-11-16  sato : for global
!
!   input argument list:
!    nlatf  - # of lat
!    nlonf  - # of lon
!    asp    - isotropic aspect
!    aniall - anisotropic aspect
!    k1     - model level
!
!   output argument list:
!    asp    - composit aspect
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlatf,nlonf,k1
  real(r_single) ,intent(inout) :: asp   (7,nlatf,nlonf)
  real(r_single) ,intent(inout) :: aniall(6,nlatf,nlonf)

  real(r_single),parameter :: alpha=0.8_r_single
  real(r_single),parameter :: mindet=1.0_r_single, maxdet=10.0_r_single

  real(r_single):: deta0,deta1,mag,coeff_asplim
  real(r_single):: fblend,salpha,dalpha,rk1
  integer(i_kind):: i,j

!--
! Note:
! Since current ensemble data set contains the data up to 10mb (LV55),
! We need to reduce the ensemble data effect gradially below LV55.
!--
  rk1=real(k1-53)
  fblend=half*(one-tanh(rk1))

  salpha=alpha*fblend
  dalpha=(one-salpha)

  do j=1,nlonf
     do i=1,nlatf
        deta0=asp(1,i,j)   *asp(2,i,j)
        deta1=aniall(1,i,j)*aniall(2,i,j)-aniall(6,i,j)*aniall(6,i,j)
 
!       blend magnitude
        mag=min(max(dalpha+salpha*sqrt(deta1/deta0),mindet),maxdet)

!       normalize aspect shape only for horizontal direction
        coeff_asplim=sqrt(deta0/deta1)
        aniall(1,i,j)=aniall(1,i,j)*coeff_asplim
        aniall(2,i,j)=aniall(2,i,j)*coeff_asplim
        aniall(6,i,j)=aniall(6,i,j)*coeff_asplim

        aniall(4,i,j)=aniall(4,i,j)*sqrt(coeff_asplim)
        aniall(5,i,j)=aniall(5,i,j)*sqrt(coeff_asplim)

!       blend aspect shape and multiply blended magnitude
        asp(1,i,j) = (dalpha*asp(1,i,j)+salpha*aniall(1,i,j))*mag
        asp(2,i,j) = (dalpha*asp(2,i,j)+salpha*aniall(2,i,j))*mag
        asp(6,i,j) = (                  salpha*aniall(6,i,j))*mag
        asp(3,i,j) = (dalpha*asp(3,i,j)+salpha*aniall(3,i,j))
        asp(4,i,j) = (                  salpha*aniall(4,i,j))*sqrt(mag)
        asp(5,i,j) = (                  salpha*aniall(5,i,j))*sqrt(mag)
        asp(7,i,j) = zero_single
     end do
  end do

end subroutine add_aniso_effect_sty
!=======================================================================
!=======================================================================
subroutine add_ensinfo_aniasp(nlatf,nlonf,ens_f,ens_zf,ensv,aniasp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   add_ensinfo_aniasp
! prgmmr: sato             org: np22                date: 2007-11-16
!
! abstract: utility routine for get_aspect_ens.
!           add one emsemble element info to the stat variables
!
! program history log:
!   2007-11-16  sato : for global
!
!   input argument list:
!    nlatf  - # of lat
!    nlonf  - # of lon
!    ens_f  - ensemble info (2D fields)
!    ens_zf - ensemble info (Vertical derivative 2D fields)
!    ensv   - 2D variance
!    aniasp - aspect tensor
!
!   output argument list:
!    ensv   - 2D variance
!    aniasp - aspect tensor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlatf,nlonf
  real(r_single) ,intent(in   ) :: ens_f(nlatf,nlonf)
  real(r_single) ,intent(in   ) :: ens_zf(nlatf,nlonf)
  real(r_single) ,intent(inout) :: ensv(nlatf,nlonf)
  real(r_single) ,intent(inout) :: aniasp(6,nlatf,nlonf)

  integer(i_kind):: i,ip,im,j,jp,jm
  real(r_single):: dxi,dyi,fx1,fx2,fx3

  do j=1,nlonf
     do i=1,nlatf
        ensv(i,j)=ensv(i,j)+ens_f(i,j)*ens_f(i,j)

        jp=min(nlonf,j+1) ; jm=max(1,j-1); dxi=1.0_r_single/real(jp-jm,r_single)
        ip=min(nlatf,i+1) ; im=max(1,i-1); dyi=1.0_r_single/real(ip-im,r_single)
 
        fx1 = (ens_f(ip,j)-ens_f(im,j))*dyi
        fx2 = (ens_f(i,jp)-ens_f(i,jm))*dxi
        fx3 =  ens_zf(i,j)

        aniasp(1,i,j) = aniasp(1,i,j) + fx1*fx1 ! 1st (y) direction x1*x1
        aniasp(2,i,j) = aniasp(2,i,j) + fx2*fx2 ! 2nd (x) direction x2*x2
        aniasp(3,i,j) = aniasp(3,i,j) + fx3*fx3 ! 3rd (z) direction x3*x3
        aniasp(4,i,j) = aniasp(4,i,j) + fx3*fx2 !                   x3*x2
        aniasp(5,i,j) = aniasp(5,i,j) + fx3*fx1 !                   x3*x1
        aniasp(6,i,j) = aniasp(6,i,j) + fx2*fx1 !                   x2*x1

     end do
  end do

end subroutine add_ensinfo_aniasp
!=======================================================================
!=======================================================================
subroutine mk_ens_aniasp(nlatf,nlonf,qlxmink,qlymink,qlzmink, &
                         ensv,aniasp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   mk_ens_aniasp
! prgmmr: sato             org: np22                date: 2007-11-16
!
! abstract: utility routine for get_aspect_ens.
!           add one emsemble element info to the aspect tensor
!
! program history log:
!   2007-11-16  sato : for global
!
!   input argument list:
!    nlatf   - # of lat
!    nlonf   - # of lon
!    qlxmink -
!    qlymink -
!    qlzmink -
!    ensv    - 2D variance stats
!    aniasp  - aspect tensor stats
!
!   output argument list:
!    ensv   - 2D variance
!    aniasp - aspect tensor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlatf,nlonf
  real(r_single) ,intent(in   ) :: qlxmink,qlymink,qlzmink
  real(r_single) ,intent(inout) :: ensv(nlatf,nlonf)
  real(r_single) ,intent(inout) :: aniasp(6,nlatf,nlonf)

  integer(i_kind):: i,j
  real(r_single):: qlx,qly,qlz
  real(r_single):: c(6)

  do j=1,nlonf
     do i=1,nlatf
        c(:)=aniasp(:,i,j)
 
        qlx=real(max(qlxmink,ensv(i,j)),r_single)
        qly=real(max(qlymink,ensv(i,j)),r_single)
        qlz=real(max(qlzmink,ensv(i,j)),r_single)

        if ( qlx>tiny_r_kind .and. qly>tiny_r_kind .and. qlz>tiny_r_kind ) then
           aniasp(1,i,j)=c(1)/(qly*qly)
           aniasp(2,i,j)=c(2)/(qlx*qlx)
           aniasp(3,i,j)=c(3)/(qlz*qlz)
           aniasp(4,i,j)=c(4)/(qlz*qlx)
           aniasp(5,i,j)=c(5)/(qlz*qly)
           aniasp(6,i,j)=c(6)/(qly*qlx)
        end if
     end do
  end do
end subroutine mk_ens_aniasp
!=======================================================================
!=======================================================================
subroutine get_avgensv(nlatf,nlonf,qlxmink,qlymink,qlzmink,ensv,aensv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_avgensv
! prgmmr: sato             org: np22                date: 2008-01-15
!
! abstract: utility routine for get_aspect_ens.
!           add one emsemble element info to the aspect tensor
!
! program history log:
!   2007-11-16  sato : for global
!
!   input argument list:
!    nlatf   - # of lat
!    nlonf   - # of lon
!    qlxmink -
!    qlymink -
!    qlzmink -
!    ensv    - 2D variance stats
!
!   output argument list:
!    aensv(3)- averaged ensv for x,y,z
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlatf,nlonf
  real(r_single) ,intent(in   ) :: qlxmink,qlymink,qlzmink
  real(r_single) ,intent(in   ) :: ensv(nlatf,nlonf)
  real(r_kind)   ,intent(  out) :: aensv(3)

  integer(i_kind):: i,j,nc

  nc=nlonf*nlatf
  aensv(1:3)=zero
  do j=1,nlonf
     do i=1,nlatf
        aensv(1)=aensv(1)+max(ensv(i,j),qlxmink)/real(nc,r_kind)
        aensv(2)=aensv(2)+max(ensv(i,j),qlymink)/real(nc,r_kind)
        aensv(3)=aensv(3)+max(ensv(i,j),qlzmink)/real(nc,r_kind)
     end do
  end do
end subroutine get_avgensv
!=======================================================================
!=======================================================================
subroutine get_ensmber_glb(kens,ifld,igrid,ifldlevs, &
                           ens0f_p0,  ens0f_p2,  ens0f_p3, &
                           ens0zf_p0, ens0zf_p2, ens0zf_p3, &
                           pgesmax,pgesmin, &
                           iref,jref,enscoeff, &
                           nflag,kflag,idiagens,unbalens,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   get_ensmber_glb
! prgmmr: sato             org: np23                date: 2007-11-29
!
! abstract: obtain specific physical field of specific ens member on the
!           filter grid. also obtain the vertical derivative of that field.
!           modified get_ensmber() for global mode.
!
! program history log:
!   2007-11-29  sato
!
!   input argument list:
!    mype           - mpi task id
!    kens           - order # of this e-member
!    igrid          - grid number for this e-member
!    ifld           - order # of this physical field of the kens e-member
!    enscoeff(4,i,j)- bilinear interpolation coeffs from e-grid to anl grid
!    ifldlevs(ifld) - # of p-levels in the ifld field of the kens e-member
!    idiagens
!    unbalens
!    pgesmin,pgesmax
!    iref
!    jref
!
!   output argument list:
!    nflag(i) - 1 if field will be used to construct covariance of
!               ith anl variable, 0 otherwise
!    kflag(k) - 1 if field will be used to construct covariance at
!               the kth model level, 0 otherwise
!    ens0f_p0,ens0zf_p0
!    ens0f_p2,ens0zf_p2
!    ens0f_p3,ens0zf_p3
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use balmod,    only : bvz,agvz,wgvz
  use gridmod,   only: irc_s,ird_s
  use sub2fslab_mod, only: setup_sub2fslab, destroy_sub2fslab, &
                           sub2fslab_glb, sub2fslabdz_glb

  implicit none

! Declare passed variables
  integer(i_kind)                                             ,intent(in   ) :: kens,igrid,ifld,mype
  integer(i_kind)                                             ,intent(in   ) :: ifldlevs(nmax_ensfld)
  integer(i_kind)                                             ,intent(in   ) :: idiagens  ! if = 1 then print out diganostic info
  logical                                                     ,intent(in   ) :: unbalens
  real(r_kind)                                                ,intent(in   ) :: pgesmin(nsig),pgesmax(nsig)
  integer(i_kind)                                             ,intent(in   ) :: iref(nlat,nlon)
  integer(i_kind)                                             ,intent(in   ) :: jref(nlat,nlon)
  real(r_kind)                                                ,intent(in   ) :: enscoeff(4,nlat,nlon)

  integer(i_kind)                                             ,intent(  out) :: nflag(10) ! dim is # of anl variables
  integer(i_kind)                                             ,intent(  out) :: kflag(nsig)
  real(r_single),dimension(pf2aP1%nlatf,pf2aP1%nlonf,nsig1o)  ,intent(  out) :: ens0f_p0,ens0zf_p0
  real(r_single),dimension(pf2aP2%nlatf ,pf2aP2%nlonf ,nsig1o),intent(  out) :: ens0f_p2,ens0zf_p2
  real(r_single),dimension(pf2aP3%nlatf ,pf2aP3%nlonf ,nsig1o),intent(  out) :: ens0f_p3,ens0zf_p3

! Declare local variables
  integer(i_kind):: i,j,k,l,ivar,ier
  integer(i_kind):: n,kup
  integer(i_kind):: inttype !read in from each e-member. tells about
!                            desired vertical interp type for specific
!                            physical field. 0 for linear in p and
!                            1 in ln(p).

  real(r_kind),allocatable,dimension(:,:,:)::field,fieldz

  integer(i_kind):: lun,nx,ny,k1
  integer(i_kind):: kk,num_pad,kslab,kstart,kend,kslab_prev

  real(r_single),allocatable,dimension(:)    :: pres
  real(r_single),allocatable,dimension(:)    :: slab
  real(r_single),allocatable,dimension(:,:)  :: slab2,aslab2,sub
  real(r_single),allocatable,dimension(:,:,:):: h_loc
  real(r_single),allocatable,dimension(:)    :: tempa
  real(r_single) strp(lat1*lon1)
  real(r_single) auxa(lat2,lon2),auxb(nlon,nlat)
  real(r_single):: p0,gamma
  real(r_kind)  :: p1,p2
  logical:: one21
  character(3) clun,clun2

  integer(i_kind):: idiagflg,it

  integer(i_long):: ngauss_smooth
  integer(i_long):: nsmooth_smooth, nsmooth_shapiro_smooth

  data idiagflg /0/

!==========================================================================
!==>determine dimensions of input ensemble grid and allocate
!   slab, which is used to read in the ens fields:
!==========================================================================
  if (igrid == 3) then
     nx=360
     ny=181
  else
     if (mype == 0 ) then
        print*,'in get_ensmber: igrid=',igrid
        print*,'in get_ensmber: unsupported grid, aborting ...'
     endif
     call stop2(stpcode_ensdata)
  end if

  allocate(slab(nx*ny),slab2(nlat,nlon),aslab2(nlat,nlon),stat=ier)
  if(ier/=0) then
     write(6,*) 'get_ensmber_glb(): could not allocate slab'
     call stop2(stpcode_alloc)
  end if

!==========================================================================
!==>ens input fields are written as direct access files. determine the
!   address of desired initial record. also retrieve pressure values
!   of the field's vertical levels:
!==========================================================================
  write (clun(1:3),'(i3.3)') kens

  lun=55
  open (lun,file='ens.dat_'//clun,form='unformatted', &
        access='direct',recl=4*nx*ny)

  kstart=0
  do k=1,ifld-1
     kstart=kstart+(1+ifldlevs(k))
  end do
  kstart=kstart+1

  read(lun,rec=kstart) slab

  do i=1,nvars
     nflag(i)=nint(slab(i))
  end do

  ivar=nint(slab(20))

  n=nint(slab(25))
  if (igrid  /= n) then
     if (mype == 0) then
        print*,'in get_ensmber: igrid,n=',igrid,n
        print*,'in get_ensmber: inconsistency in grid type for this field. &
                                &igrid and n must be equal. aborting ...'
     end if
     call stop2(stpcode_ensdata)
  end if

  inttype=nint(slab(26))

  n=nint(slab(27))
  if (ifldlevs(ifld) /= n) then
     if (mype == 0) then
        print*,'in get_ensmber: ifldlevs(ifld),n=',ifldlevs(ifld),n
        print*,'in get_ensmber: inconsistency in number of levels for this field. &
                                &ifldlevs and n must be equal. aborting ...'
     end if
     call stop2(stpcode_ensdata)
  end if

  allocate(pres(ifldlevs(ifld)))
  do k=1,ifldlevs(ifld)
     pres(k)=slab(k+39)
  end do

  if (mype==0) &
     print*,'in get_ensmber: kens,ifld,kstart,igrid,nx,ny=', &
                             kens,ifld,kstart,igrid,nx,ny

!==========================================================================
!==>prepare for alltoallv comunications:
!==========================================================================
  n=ifldlevs(ifld)
  if (mod(n,npe) == 0) then
     num_pad=n
  else
     num_pad=(n/npe+1)*npe
  endif

!==========================================================================
!==>read in ensemble field and distribute over subdomains
!==========================================================================
  kslab_prev=1
  kstart=kstart+1
  kend=kstart+ifldlevs(ifld)-1

  allocate(tempa(itotsub))
  allocate(sub(lat2*lon2,max(num_pad,nsig)),stat=ier)
  if(ier/=0) then
     write(6,*) 'get_ensmber_glb(): could not allocate h_loc/tempa/sub'
     call stop2(stpcode_alloc)
  end if

  tempa(:)=zero_single

  do kslab=kstart,kend

     if (mod(kslab-kstart,npe) == mype) then
        read(lun,rec=kslab) slab

        slab2(:,:)=zero_single
        call ens_intpglb(slab(1),nx,ny,slab2(1,1),iref(1,1),jref(1,1),enscoeff(1,1,1))

        if (ifld==1 .or. ifld==2) then
           if (ifld==1)     read(lun,rec=(kslab+(ifldlevs(ifld)+1))) slab ! v-comp
           if (ifld==2) read(lun,rec=(kslab-(ifldlevs(ifld)+1))) slab ! u-comp
 
           aslab2(:,:)=zero_single
           call ens_intpglb(slab(1),nx,ny,aslab2(1,1),iref(1,1),jref(1,1),enscoeff(1,1,1))

           if (ifld==1) then
           !  slab2 =u -> vor, aslab2=v -> div
              if(idiagflg==1) &
                 write(6,*) 'k,max(u,v),min(u,v)',kslab-kstart+1, &
                             maxval(slab2),maxval(aslab2), &
                             minval(slab2),minval(aslab2)
              call ens_uv2psichi(slab2(1,1),aslab2(1,1))
              if(idiagflg==1) &
                 write(6,*) 'k,max(phi,chi),min(phi,chi)',kslab-kstart+1, &
                             maxval(slab2),maxval(aslab2), &
                             minval(slab2),minval(aslab2)
           else
           !  aslab2=u -> st,  slab2 =v -> vp
              if(idiagflg==1) &
                 write(6,*) 'k,max(u,v),min(u,v)',kslab-kstart+1, &
                             maxval(aslab2),maxval(slab2), &
                             minval(aslab2),minval(slab2)
              call ens_uv2psichi(aslab2(1,1),slab2(1,1))
              if(idiagflg==1) &
                 write(6,*) 'k,max(phi,chi),min(phi,chi)',kslab-kstart+1, &
                             maxval(aslab2),maxval(slab2), &
                             minval(aslab2),minval(slab2)
           end if

        end if

        do i=1,itotsub
           tempa(i)=slab2(ltosi_s(i),ltosj_s(i))
        end do

     endif

     kk=kslab-kstart+1

     if ( mod(kk,npe)==0 .or. kk==ifldlevs(ifld) ) then
        call mpi_alltoallv(tempa(1)         ,ijn_s,displs_s,mpi_real4, &
                           sub(1,kslab_prev),irc_s,ird_s,   mpi_real4, &
                           mpi_comm_world,ierror)

        kslab_prev=kk+1
     end if

  end do

  close(lun)
  deallocate(slab,slab2,aslab2)

  allocate(h_loc(lat2,lon2,nsig))
  h_loc(:,:,:)=zero_single
  call reload_single(sub,h_loc)
  deallocate(sub)

!==========================================================================
!==>interpolate vertically and populate field(:,:)
!==========================================================================
  it=ntguessig

  allocate(field (lat2,lon2,nsig))
  allocate(fieldz(lat2,lon2,nsig),stat=ier)

  if(ier/=0) then
     write(6,*) 'get_ensmber_glb(): could not allocate field'
     call stop2(stpcode_alloc)
  end if

  field (:,:,:)=zero
  fieldz(:,:,:)=zero

  n=ifldlevs(ifld)

  if (n == 1) then
     do k=1,nsig
        field(:,:,k)=h_loc(:,:,1)
     end do
  else
     do k=1,nsig
        do i=1,lat2
           do j=1,lon2
              p0=ges_prsl(i,j,k,it)*10._r_single
              if (p0<pres(n)) then
                 field(i,j,k)=h_loc(i,j,n)
              else if (p0>=pres(1)) then
                 field(i,j,k)=h_loc(i,j,1)
              else
                 do kk=1,n-1
                    if (p0<=pres(kk) .and. p0>=pres(kk+1)) then
                       if (inttype == 0) then
                          gamma=(h_loc(i,j,kk+1)-h_loc(i,j,kk))/(pres(kk+1)-pres(kk))
                          field(i,j,k)=h_loc(i,j,kk)+gamma*(p0-pres(kk))
                       else
                          gamma=(h_loc(i,j,kk+1)-h_loc(i,j,kk))/alog(pres(kk+1)/pres(kk))
                          field(i,j,k)=h_loc(i,j,kk)+gamma*alog(p0/pres(kk))
                       end if
                    end if
                 end do
              end if
           end do
        end do
     end do
  end if

! save the ST data to estimate unbalanced part
  if( unbalens .and. ivar==1 ) then
     if(.not.allocated(field_st)) allocate(field_st(lat2,lon2,nsig))
     field_st = field
     kens_p = kens
  end if

!==========================================================================
!==>Estimate unbalanced part
!==========================================================================
  if( unbalens ) then
     if( kens/=kens_p ) then
        write(6,*) 'get_ensmber_glb(): kens musb be equal to kens_p,',kens,kens_p
        call stop2(stpcode_ensdata)
     end if
!-------------------------------
! Subtract ST part
!-------------------------------
     select case(ivar)
!-------------------------------
! Chi
!-------------------------------
        case(2)
           do k=1,nsig
              do j=1,lon2
                 do i=1,lat2
                    field(i,j,k)=field(i,j,k)-bvz(i,k)*field_st(i,j,k)
                 end do
              end do
           end do
!-------------------------------
! T
!-------------------------------
        case(3)
           do k=1,nsig
              do l=1,nsig
                 do j=1,lon2
                    do i=1,lat2
                       field(i,j,l)=field(i,j,l)-agvz(i,l,k)*field_st(i,j,k)
                    end do
                 end do
              end do
           end do
!-------------------------------
! Psfc
!-------------------------------
        case(7)
           do j=1,lon2
              do i=1,lat2
                 do k=1,nsig
                    field(i,j,1)=field(i,j,1)-wgvz(i,k)*field_st(i,j,k)
                 end do
                 field(i,j,2:nsig)=field(i,j,1)
              end do
           end do
     end select
  end if

  deallocate(h_loc)

!-------------------------------
! Vertical Smoother
!-------------------------------
  if (n /= 1) then
     nsmooth_smooth=10
     nsmooth_shapiro_smooth=0
     call vert_smther(field,nsmooth_smooth,nsmooth_shapiro_smooth)
  end if

!==========================================================================
!==>Output to check the field
!==========================================================================
  if (idiagens==1) then
     write (clun(1:3),'(i3.3)') kens
     write (clun2(1:3),'(i3.3)') ifld
     open (54,file='field.dat_'//clun//'_'//clun2,form='unformatted')
     do k=1,nsig
        auxa(:,:)=field(:,:,k)
        call strip(auxa,strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
             tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
        if (mype==0) then
           auxb(:,:)=zero_single
           call unfill_mass_grid2t(tempa,nlon,nlat,auxb)
           write(54) auxb
        end if
     end do
     close(54)
  end if

  deallocate(tempa)

!==========================================================================
!==>populate kflag
!  code foresees possible use of ensemble grids that might only be given
!  on a limitted number of pressure levels whose range does not fully
!  contain that of the background pressure field. In that case, the
!  particular ensemble field can only be used to define the covariances
!  on a portion of the model's vertical levels. kflag determines what
!  those vertical levels are. kflag(k)=1(0) means that the current ensemble
!  field will(not) be used for the covariance on the the kth model level.
!==========================================================================
  kflag(1:nsig)=1

  n=ifldlevs(ifld)

  if (n == 1) kflag(2:nsig)=-1 !in the current code, n==1 is assumed to occur
                                       !only when ensemble field is a surface field

  kup=0
  if (n /= 1) then
     p1=pres(1)*one
     p2=pres(n)*one
     do k=nsig,1,-1
        if (pgesmax(k)>p1 .or. pgesmin(k)<p2 ) then
           kflag(k)=-1
           if (kup==0 .and. pgesmax(k)>p1) kup=k
        endif
     enddo
     if (pres(1)>=999.5_r_single) then
        do k=1,kup
           kflag(k)=1
        enddo
     endif
  endif

!==========================================================================


!==========================================================================
!==>convert field(:,:,:) from subdomain to slab mode and interpolate to   |
!   filter grid. repeat for the vertical derivative of field(:,:,:)       !
!==========================================================================
  ens0f_p0 (:,:,:)=zero_single
  ens0f_p2 (:,:,:)=zero_single
  ens0f_p3 (:,:,:)=zero_single
  ens0zf_p0(:,:,:)=zero_single
  ens0zf_p2(:,:,:)=zero_single
  ens0zf_p3(:,:,:)=zero_single

  call setup_sub2fslab
  call sub2fslab_glb  (field,ens0f_p0 ,ens0f_p2 ,ens0f_p3)
  call sub2fslabdz_glb(field,ens0zf_p0,ens0zf_p2,ens0zf_p3)
  call destroy_sub2fslab

!-----------------------------------------------------------
!-------end of getting background variables on filter grid--
!-----------------------------------------------------------

  one21=.true.
  if (one21) then
     do k=1,nsig1o
     !--- Zonal Patch
        call smther_one(ens0f_p0 (1,1,k),1,pf2aP1%nlatf,1,pf2aP1%nlonf,nsmooth_smooth)
        call smther_one(ens0zf_p0(1,1,k),1,pf2aP1%nlatf,1,pf2aP1%nlonf,nsmooth_smooth)
     !--- North Polar Patch
        call smther_one(ens0f_p2 (1,1,k),1,pf2aP2%nlatf,1,pf2aP2%nlonf,nsmooth_smooth)
        call smther_one(ens0zf_p2(1,1,k),1,pf2aP2%nlatf,1,pf2aP2%nlonf,nsmooth_smooth)
     !--- South Polar Patch,
        call smther_one(ens0f_p3 (1,1,k),1,pf2aP3%nlatf,1,pf2aP3%nlonf,nsmooth_smooth)
        call smther_one(ens0zf_p3(1,1,k),1,pf2aP3%nlatf,1,pf2aP3%nlonf,nsmooth_smooth)
     end do
  else

! Note:
! Use the smoothing filter prepared in get_background_glb...
     ngauss_smooth=1
     call raf_sm_glb(ens0f_p0 ,ens0f_p2 ,ens0f_p3 ,ngauss_smooth)
     call raf_sm_glb(ens0zf_p0,ens0zf_p2,ens0zf_p3,ngauss_smooth)
  endif

  do k=1,nsig1o
     k1=levs_id(k)
     if (k1==0) cycle
     if (kflag(k1) == 0) then
        ens0f_p0 (:,:,k)=zero_single; ens0zf_p0(:,:,k)=zero_single
        ens0f_p2 (:,:,k)=zero_single; ens0zf_p2(:,:,k)=zero_single
        ens0f_p3 (:,:,k)=zero_single; ens0zf_p3(:,:,k)=zero_single
     endif
  end do

  deallocate(pres)
  deallocate(field,fieldz)

end subroutine get_ensmber_glb
!=======================================================================
!=======================================================================
subroutine ens_intpglb_coeff(iref,jref,enscoeff,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_intpglb_coeff
!   prgmmr: sato             org: np22                date: 2007-11-16
!
! abstract: compute coefficients of horizontal bilinear
!           interpolation of ensemble field to analysis grid.
!
! program history log:
!   2007-11-16  sato
!
!   input argument list:
!    mype
!
!   output argument list:
!    iref
!    jref
!    enscoeff
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: rad2deg
  use gridmod, only: rlats,rlons
  implicit none

  integer(i_kind),intent(in   ) :: mype
  integer(i_kind),intent(  out) :: iref(nlat,nlon)
  integer(i_kind),intent(  out) :: jref(nlat,nlon)
  real   (r_kind),intent(  out) :: enscoeff(4,nlat,nlon)

  integer(i_kind):: i,j,iy,jx,jxp,kg
  real(r_kind):: rlat,rlon,xg,yg
  real(r_kind):: dxg,dxg1,dyg,dyg1
  real(r_kind):: r360

  r360  =360._r_kind

  enscoeff=rmis
  iref=0
  jref=0

  iy =181
  jx =360
  jxp=jx+1
  kg =1

  do j=1,nlon
     do i=1,nlat

     !  latlon for the analysing grid
        rlat=rlats(i)*rad2deg
        rlon=rlons(j)*rad2deg
        if (rlon < zero) rlon=rlon+r360
        if (rlon >=r360) rlon=rlon-r360

    !   correspoinding ensemble grid
        xg=rlon+one
        yg=rlat+90._r_kind+one

        dxg =xg-float(floor(xg))
        dyg =yg-float(floor(yg))
        dxg1=one-dxg
        dyg1=one-dyg

        if (xg >= one .and. xg <= float(jxp) .and.  &
            yg >= one .and. yg <= float(iy) ) then
           enscoeff(1,i,j)=dxg1*dyg1
           enscoeff(2,i,j)=dxg1*dyg
           enscoeff(3,i,j)=dxg *dyg1
           enscoeff(4,i,j)=dxg *dyg
           iref(i,j)=floor(yg)
           jref(i,j)=floor(xg)
        else
           print*, 'ens_intpglb_coeff(): out of range, xg,yg,rlat,rlon=',xg,yg,rlat,rlon
           call stop2(stpcode_ensdata)
        end if

     end do
  end do

  if(mype==0) &
     print*,'ens_intpglb_coeff(): maxmin-iref/jref:', &
       maxval(iref),minval(iref),maxval(jref),minval(jref)

end subroutine  ens_intpglb_coeff
!=======================================================================
!=======================================================================
subroutine ens_intpglb(workin,nx,ny,workout,iref,jref,enscoeff)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_intpglb
!   prgmmr: sato             org: np23                date: 2007-11-30
!
! abstract: perform horizontal interpolation of ensemble field to analysis
!           grid for global mode. modified fillanlgrd().
!
! program history log:
!   2007-11-30  sato
!
!   input argument list:
!     workin(nx,ny)  - ensemble field
!     nx,ny          - horizontal dimensions of ensemble field
!     enscoeff       - coefficients of bilienar interpolation from
!                      ensemble grid to analysis grid
!     iref
!     jref
!
!   output argument list:
!    workout(nlat,nlon) - ensemble field on analysis grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: nx,ny
  integer(i_kind),intent(in   ) :: iref(nlat,nlon)
  integer(i_kind),intent(in   ) :: jref(nlat,nlon)
  real(r_kind)   ,intent(in   ) :: enscoeff(4,nlat,nlon)
  real(r_single) ,intent(in   ) :: workin(nx,ny)
  real(r_single) ,intent(  out) :: workout(nlat,nlon)

! Declare local variables
  integer(i_kind) i,j,ii,jj,iip,jjp

!* *****************************************************************************

  do j=1,nlon
     do i=1,nlat
        jj=jref(i,j)
        if(jj==nx) then; jjp=1    ! Cyclic Domain
        else;            jjp=jj+1
        end if

        ii =iref(i,j)
        iip=min(ii+1,ny)          ! Need not to consider the special treatment for pole
        if( ii<1 .or. iip>ny .or. jj<1 .or. jjp > nx ) then
           print*, 'ens_intpglb: out of range, i,j,ii,iip,jj,jjp,nx,ny,nlat,nlon =',&
                                               i,j,ii,iip,jj,jjp,nx,ny,nlat,nlon
           call stop2(stpcode_ensdata)
        end if
        workout(i,j)=real(enscoeff(1,i,j)*real(workin(jj ,ii ),r_kind)+ &
                          enscoeff(2,i,j)*real(workin(jj ,iip),r_kind)+ &
                          enscoeff(3,i,j)*real(workin(jjp,ii ),r_kind)+ &
                          enscoeff(4,i,j)*real(workin(jjp,iip),r_kind),r_single)
     end do
  end do

end subroutine ens_intpglb
!=======================================================================
!=======================================================================
subroutine ens_uv2psichi(work1,work2)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ens_uv2psichi compute psi and chi on full domains
!   prgmmr: sato             org: np22                date: 2007-11-30
!
! abstract: get stream function and velocity potential from u and v
!           u  ,v   -> vor,div : from uv2vordiv(compatc_diffs.f90)
!           vor,div -> psi,chi : from getpsichi(bkgvar_rewgt.f90)
!
! program history log:
!   2007-11-30  sato
!
!   input argument list:
!     work1     - u on horizontal full domains
!     work2     - v on horizontal full domains
!
!   output argument list:
!     work1     - stream function on horizontal full domains
!     work2     - velocity potential on horizontal full domains
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use gridmod, only: sp_a,grd_a
  use compact_diffs,only: noq, coef, xdcirdp, ydsphdp,lcy,lacox1,lbcox1
  use compact_diffs,only: lacox2,lbcox2,lacoy1,lbcoy1,lacoy2,lbcoy2

  implicit none

! Declare passed variables
  real(r_single),dimension(nlat,nlon),intent(inout) :: work1,work2

! Declare local variables
  real(r_kind),dimension(nlat,nlon):: work3,work4
  integer(i_kind) ny
  integer(i_kind) iy,ix
  real(r_kind) rnlon,div_n,div_s,vor_n,vor_s
  real(r_kind),dimension(nlat-2,nlon):: grdu,grdv,&
       grid_div,grid_vor,du_dlat,du_dlon,dv_dlat,dv_dlon
  real(r_kind),dimension(sp_a%nc):: spc1,spc2

  ny =nlat-2

  du_dlat=zero
  du_dlon=zero
  dv_dlat=zero
  dv_dlon=zero
  grdu   =zero
  grdv   =zero
  grid_div=zero
  grid_vor=zero

!-----------------------------------------------
! step1: (u,v)->(vor,div)
! this part is from uv2vordiv(compatc_diffs.f90)
!-----------------------------------------------

  grdu(:,:)=real(work1(2:nlat-1,:),r_kind)
  grdv(:,:)=real(work2(2:nlat-1,:),r_kind)

! Compute x derivative of u:  du_dlon = du/dlon
  call xdcirdp(grdu,du_dlon,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2),&
       nlon,ny,noq)

! Compute x derivative of v:  dv_dlon = dv/dlon
  call xdcirdp(grdv,dv_dlon,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2),&
       nlon,ny,noq)

! Multiply u and v by cos(lat).  Note:  coef(lcy+iy) contains 1/cos(lat)
  do ix=1,nlon
     do iy=1,ny
        grdu(iy,ix)=grdu(iy,ix)/coef(lcy+iy)
        grdv(iy,ix)=grdv(iy,ix)/coef(lcy+iy)
     end do
  end do

! Compute y derivative of u:  du_dlat = du/dlat
  call ydsphdp(grdu,du_dlat,coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),&
       nlon,ny,noq)

! Compute y derivative of v:  dv_dlat = dv/dlat
  call ydsphdp(grdv,dv_dlat,coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),&
       nlon,ny,noq)

! Make corrections for convergence of meridians:
  do ix=1,nlon
     do iy=1,ny
        du_dlon(iy,ix)=du_dlon(iy,ix)*coef(lcy+iy)
        dv_dlon(iy,ix)=dv_dlon(iy,ix)*coef(lcy+iy)
        du_dlat(iy,ix)=du_dlat(iy,ix)*coef(lcy+iy)
        dv_dlat(iy,ix)=dv_dlat(iy,ix)*coef(lcy+iy)
     enddo
  enddo

! Combine derivatives to obtain vorticity and divergence
  do ix=1,nlon
     do iy=1,ny
        grid_div(iy,ix) = du_dlon(iy,ix) + dv_dlat(iy,ix)
        grid_vor(iy,ix) = dv_dlon(iy,ix) - du_dlat(iy,ix)
     end do
  end do

! Compute mean values to put in first and last row
  div_s=zero; div_n=zero
  vor_s=zero; vor_n=zero
  do ix=1,nlon
     div_s = div_s + grid_div( 1,ix)
     div_n = div_n + grid_div(ny,ix)
     vor_s = vor_s + grid_vor( 1,ix)
     vor_n = vor_n + grid_vor(ny,ix)
  end do
  rnlon = one/float(nlon)
  div_s = div_s*rnlon
  div_n = div_n*rnlon
  vor_s = vor_s*rnlon
  vor_n = vor_n*rnlon

  work3(2:nlat-1,:)=grid_vor(:,:); work3(1,:)=vor_s; work3(nlat,:)=vor_n
  work4(2:nlat-1,:)=grid_div(:,:); work4(1,:)=div_s; work4(nlat,:)=div_n

!-----------------------------------------------
! step2: (vor,div)->(psi,chi)
! this part is from getpsichi(bkgvar_rewgt.f90)
!-----------------------------------------------

  call general_g2s0(grd_a,sp_a,spc1,work3)
  call general_g2s0(grd_a,sp_a,spc2,work4)

! Inverse laplacian
  do ix=2,sp_a%ncd2
     spc1(2*ix-1)=spc1(2*ix-1)/(-sp_a%enn1(ix))
     spc1(2*ix  )=spc1(2*ix  )/(-sp_a%enn1(ix))
     spc2(2*ix-1)=spc2(2*ix-1)/(-sp_a%enn1(ix))
     spc2(2*ix  )=spc2(2*ix  )/(-sp_a%enn1(ix))
  end do


  work3=zero ; work4=zero
  call general_s2g0(grd_a,sp_a,spc1,work3)
  call general_s2g0(grd_a,sp_a,spc2,work4)

  work1(:,:)=real(work3(:,:),r_single)
  work2(:,:)=real(work4(:,:),r_single)

  return

end subroutine ens_uv2psichi
!=======================================================================
!=======================================================================
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  reload_single --- Transfer contents of 2-d array to 3-d array
!
! !INTERFACE:
!
subroutine reload_single(work_in,work_out)

! !USES:

  implicit none

! !INPUT PARAMETERS:

  real(r_single),dimension(lat2*lon2,nsig),intent(in   ) :: work_in   ! 2-d array

! !OUTPUT PARAMETERS:

  real(r_single),dimension(lat2,lon2,nsig),intent(  out) :: work_out  ! 3-d array

! !DESCRIPTION: Transfer contents of 2-d array to 3-d array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!   2007-12-10  sato, single precision version
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) i,j,k,ij

  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           work_out(i,j,k)=work_in(ij,k)
        end do
     end do
  end do
  return
end subroutine reload_single
!=======================================================================
!=======================================================================
end module anisofilter_glb
