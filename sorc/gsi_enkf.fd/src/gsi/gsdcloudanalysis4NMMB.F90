#ifdef NMMB_CLOUDANALYSIS
SUBROUTINE  gsdcloudanalysis4NMMB(mype)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine serves as a driver for generalized cloud/hydrometeor analysis
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  Add NCO document block
!    2010-04-30  Hu  Clean the code to meet GSI standard
!    2011-05-29  Todling - extra cloud-guess from MetGuess-Bundle (see Remark 1)
!                          some fields now from wrf_mass_guess_mod
!    2013-10-20  s.liu   - use cloud analysis for NMMB
!    2014-11-24  s.liu   - use radar-derived qr and qs in cloud analysis
!    2015-01-24  s.liu   - move cloud analysis related parameters used for NMMB to this subroutine
!    2015-03-20  s.liu   - tune the method of using radar-derived qr and qs
!    2016-02-26  s.liu   - use namelist to control cloud analysis parameters
!    2016-02-29  s.liu   - define l_use_hydroretrieval_all as public variable
!    2016-05-05  s.liu   - consider possible bias of derived reflecitvity from
!                          lightnint density based on region mask

!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!  1. Notice that now the fields point to the instance of the GUESS at ntguessig
!     no longer wired to 1 (as originally) - however, make sure when running RUC
!     ntguessig is set correctly (see questions around definition of itsig)
!  2. Notice that some WRF-variable and grid specific is now defined in wrf_guess_mod
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET) at NOAA/ESRL - Boulder, CO
!
!$$$
!
!_____________________________________________________________________
!
! 
  use constants, only: zero,one,rad2deg,fv,rd_over_cp,zero_single
  use constants, only: rd_over_cp, h1000
  use kinds,   only: r_single,i_kind, r_kind
  use mpimod, only: ierror,mpi_real4,mpi_real8,mpi_sum,mpi_comm_world
  use gridmod, only: pt_ll,eta1_ll,aeta1_ll
  use gridmod, only: regional,wrf_mass_regional,regional_time,region_lon,region_lat 
  use gridmod, only: nsig,lat2,lon2,istart,jstart,nlon,nlat
  use gridmod, only: itotsub,lon1,lat1,nlon_regional,nlat_regional,ijn,displs_g
  use guess_grids, only: pbl_height, load_gsdpbl_hgt
  use obsmod,  only: obs_setup,nsat1,ndat,dtype
  use guess_grids, only: ntguessig,ntguessfc
! use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat
  use guess_grids, only: soil_temp,isli2
  use guess_grids, only: ges_tsen,geop_hgtl
  use guess_grids, only: ges_prsl
  use rapidrefresh_cldsurf_mod, only: dfi_radar_latent_heat_time_period,   &
                                      metar_impact_radius,                 &
                                      metar_impact_radius_lowCloud,        &
                                      l_cleanSnow_WarmTs,l_conserve_thetaV,&
                                      r_cleanSnow_WarmTs_threshold,        &
                                      i_conserve_thetaV_iternum,           &
                                      l_cld_bld, cld_bld_hgt,              &
                                      build_cloud_frac_p, clear_cloud_frac_p, &
                                      nesdis_npts_rad, &
                                      l_use_hydroretrieval_all, &
                                      iclean_hydro_withRef,     &
                                      iclean_hydro_withRef_allcol, &
                                      ioption
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use gsi_metguess_mod, only: GSI_MetGuess_get
  use gsi_bundlemod, only: gsi_bundlegetpointer

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
!
! background
!
  real(r_single),allocatable:: t_bk(:,:,:)
  real(r_single),allocatable:: h_bk(:,:,:)
  real(r_single),allocatable:: p_bk(:,:,:)
  real(r_single),allocatable:: ps_bk(:,:)
  real(r_single),allocatable:: zh(:,:)
  real(r_single),allocatable:: q_bk(:,:,:)

  real(r_single),allocatable:: xlon(:,:)        ! 2D longitude in each grid
  real(r_single),allocatable:: xlat(:,:)        ! 2D latitude in each grid
! real(r_single),allocatable:: gsfc(:,:,:)
  real(r_single),  allocatable:: xland(:,:)
  real(r_single),allocatable:: soiltbk(:,:)
!  real(r_single),allocatable:: z_lcl(:,:)       ! lifting condensation level
  real(r_single),allocatable:: pblh(:,:)         ! PBL height (grid coordinate)

  real(r_kind),pointer,dimension(:,:)   :: ges_z =>NULL()
  real(r_kind),pointer,dimension(:,:)   :: ges_ps=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_q =>NULL()
! real(r_kind),pointer,dimension(:,:,:) :: ges_tv=>NULL()
!
!  surface observation
!
  INTEGER(i_kind) :: NVARCLD_P
  PARAMETER (NVARCLD_P=13)

  INTEGER(i_kind)              :: numsao
  real(r_single), allocatable  :: OI(:)
  real(r_single), allocatable  :: OJ(:)
  INTEGER(i_kind),allocatable  :: OCLD(:,:)
  CHARACTER*10,   allocatable  :: OWX(:)
  real(r_single), allocatable  :: Oelvtn(:)
  real(r_single), allocatable  :: Odist(:)
  character(8),   allocatable  :: cstation(:)
  real(r_single), allocatable  :: OIstation(:)
  real(r_single), allocatable  :: OJstation(:)
  real(r_single), allocatable  :: wimaxstation(:)
!
!  lightning observation: 2D field in RR grid
!
  real(r_kind),allocatable  :: lightning(:,:),lghtn_region_mask(:,:)
  real(r_kind),allocatable  :: lghtn_ref_bias(:,:)
!
!  GOES - NASA LaRc cloud products: several 2D fields in RR grid
!
  real(r_single),allocatable  :: nasalarc_cld(:,:,:)

!
!  radar observation : 3D reflectvity in RR grid
!
  real(r_kind),allocatable :: ref_mos_3d(:,:,:)
  real(r_kind),allocatable :: ref_mos_3d_tten(:,:,:)
! real(r_kind),allocatable :: ref_mosaic31(:,:,:)
  integer(i_kind)          :: nmsclvl_radar 
!
!  GOES - NESDIS cloud products : 2d fields
!
  real(r_single), allocatable :: sat_ctp(:,:)
  real(r_single), allocatable :: sat_tem(:,:)
  real(r_single), allocatable :: w_frac(:,:)
  integer(i_kind),allocatable :: nlev_cld(:,:)

  real(r_single), allocatable :: sat_ctp_nesdis(:,:)
  real(r_single), allocatable :: sat_tem_nesdis(:,:)
  real(r_single), allocatable :: w_frac_nesdis(:,:)
!
! cloud/hydrometeor analysis variables
!
!=========================================================================
!  cld_cover_3d in the Generalized Cloud/Hydrometeor Analysis code
!   Definition:  3-d gridded observation-based information
!      including 0-1 cloud-fraction (with zero value for clear)
!      and negative values indicating "unknown" status.
!   cld_cover_3d is initialized with negative values.
!   cld_type_3d, pcp_type_3d, wthr_type_2d - similar to cld_cover_3d
!=========================================================================

  REAL(r_single), allocatable :: cld_cover_3d(:,:,:)  ! cloud cover
  INTEGER(i_kind),allocatable :: cld_type_3d(:,:,:)   ! cloud type
  INTEGER(i_kind),allocatable :: pcp_type_3d(:,:,:)   ! precipitation type
  INTEGER(i_kind),allocatable :: wthr_type_2d(:,:)    ! weather type type
  integer(i_kind),allocatable :: cloudlayers_i(:,:,:) ! 5 different layers 
                                                      ! 1= the number of layers
                                                      ! 2,4,... bottom
                                                      ! 3,5,... top
!
  REAL(r_single),allocatable :: cldwater_3d(:,:,:)    ! cloud water
  REAL(r_single),allocatable :: cldice_3d(:,:,:)      ! cloud ice
  REAL(r_single),allocatable :: rain_3d(:,:,:)        ! rain
  REAL(r_single),allocatable :: nrain_3d(:,:,:)       ! rain number concentration
  REAL(r_single),allocatable :: snow_3d(:,:,:)        ! snow
  REAL(r_single),allocatable :: graupel_3d(:,:,:)     ! graupel
  REAL(r_single),allocatable :: cldtmp_3d(:,:,:)      ! cloud temperature
  real(r_single),allocatable :: max_obs_qrqs_save(:,:)
  real(r_single),allocatable :: max_bk_qrqs_save(:,:)
  real(r_single),allocatable :: ratio_qrqs_save(:,:)

  REAL(r_kind)    ::  thunderRadius=2.5_r_kind
  REAL(r_single)  ::  r_radius          ! influence radius of cloud based on METAR obs
  real(r_single)  ::  r_radius_lowCloud ! influence radius of low cloud to cloud top pressure
  INTEGER(i_kind) :: miss_obs_int
  REAL(r_kind)    :: miss_obs_real
  REAL(r_single)    :: miss_obs_single
  PARAMETER ( miss_obs_int = -99999  )
  PARAMETER ( miss_obs_real = -99999.0_r_kind )
  PARAMETER ( miss_obs_single = -99999.0_r_single )
  REAL(r_single)  ::  krad_bot          ! radar bottom level

!
! collect cloud
! REAL(r_kind)    :: cloud_def_p
! data  Cloud_def_p       / 0.000001_r_kind/
  REAL(r_kind),allocatable :: sumqci(:,:,:)  ! total liquid water
! REAL(r_kind),allocatable :: watericemax(:,:)  ! max of total liquid water
  INTEGER(i_kind),allocatable :: kwatericemax(:,:)  ! lowest level of total liquid water
! real(r_single),allocatable::temp1(:,:),tempa(:)
! real(r_single),allocatable::all_loc(:,:)
! real(r_single),allocatable::strp(:)
! INTEGER(i_kind) :: im,jm

   REAL(r_single),allocatable :: sumq(:,:,:)  ! total liquid water
!
! option in namelist
!
  INTEGER(i_kind) :: opt_cloudwaterice_retri  ! method for cloud water retrieval
  INTEGER(i_kind) :: opt_hydrometeor_retri    ! method for precipitation retrieval
  INTEGER(i_kind) :: opt_cloudtemperature     ! if open temperature adjustment scheme
  INTEGER(i_kind) :: istat_Surface,istat_NESDIS,istat_radar    ! 1 has observation
  INTEGER(i_kind) :: istat_NASALaRC,istat_lightning            ! 0 no observation
! INTEGER(i_kind) :: imerge_NESDIS_NASALaRC  !  =1 merge NASA LaRC with NESDIS
                                             !  =2 use NASA LaRC only
                                             !  = other, use NESDIS only
!
  REAL(r_kind), pointer :: ges_ql(:,:,:)=>NULL()    ! cloud water
  REAL(r_kind), pointer :: ges_qi(:,:,:)=>NULL()    ! could ice
  REAL(r_kind), pointer :: ges_qr(:,:,:)=>NULL()    ! rain
  REAL(r_kind), pointer :: ges_qs(:,:,:)=>NULL()    ! snow
  REAL(r_kind), pointer :: ges_qg(:,:,:)=>NULL()    ! graupel
  REAL(r_kind), pointer :: ges_ref(:,:,:)=>NULL()   ! ref
  REAL(r_kind), pointer :: ges_tten(:,:,:)=>NULL()  ! t-sensible
  REAL(r_kind), pointer :: dfi_tten(:,:,:)=>NULL()  ! tten
!
!  misc.
!
! logical :: l_use_hydroretrieval_all
  INTEGER(i_kind) :: i,j,k,itsig,itsfc
! INTEGER(i_kind) :: iglobal,jglobal,ilocal,jlocal
! LOGICAL :: ifindomain
  INTEGER(i_kind) :: imaxlvl_ref,imaxlvl_modref,i1,j1,i2,j2
  real(r_kind)    :: max_retrieved_qrqs,max_bk_qrqs,ratio_hyd_bk2obs
  REAL(r_kind)    :: qrlimit
  character(10)   :: obstype
  integer(i_kind) :: lunin, is, ier, istatus, ivar
  integer(i_kind) :: nreal,nchanl  !,ilat1s,ilon1s
  character(20)   :: isis

  real(r_kind)    :: refmax,snowtemp,raintemp  !,nraintemp,graupeltemp
! real(r_kind)    :: snowadd,ratio2
! integer(i_kind) :: imax, jmax, ista, iob, job
  real(r_kind)    :: dfi_lhtp  !, qmixr, tsfc
  real(r_kind)    :: t, qfi, qfs, qfr, qfw, t_inc
  real(r_kind)    :: iss, jss

!* output grads
  real(r_single)::outputgrads(lat2,lon2,nsig)
  real(r_single)::cref(lon2,lat2)
  real(r_single),parameter:: ze_qr_const=3.6308*1.0e9_r_single
  real(r_single),parameter:: ze_qs_const=3.268*1.0e9_r_single

!
!
  itsig=1 ! _RT shouldn't this be ntguessig?
  itsfc=1 ! _RT shouldn't this be ntguessig?
!
  call gsi_metguess_get('var::qi',ivar,ier)
  ier=0
! call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'tv',ges_tv,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'q' ,ges_q, istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'z' ,ges_z, istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'ps',ges_ps,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qi',ges_qi,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'ql',ges_ql,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qr',ges_qr,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qs',ges_qs,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qg',ges_qg,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'ref',ges_ref,istatus);ier=ier+istatus
  call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'tten',dfi_tten,istatus);ier=ier+istatus
  allocate(ges_tten(lat2,lon2,nsig))

  if(mype==0)then
     write(6,*)'gsdcloudanalysis4NMMB start ::'
! write(6,*)'sliu output bundle gsd::',GSI_MetGuess_Bundle(itsig)%r3(8)%shortname
  end if
  if(ier/=0) write(6,*)'gsdcloudanalysis :: no cloud guess, nothing to do RETURN'
  if(ier/=0) return ! no guess, nothing to do

  if(mype==0) then
     write(6,*) '========================================'
     write(6,*) 'gsdcloudanalysis: Start generalized cloud analysis '
     write(6,*) '========================================'
  endif
!
!
!
  krad_bot=7.0_r_single
  r_radius=metar_impact_radius
  r_radius_lowCloud=metar_impact_radius_lowCloud
  dfi_lhtp=dfi_radar_latent_heat_time_period

  if(mype==0) then
     write(6,*)'metar_impact_radius::',metar_impact_radius
     write(6,*)'metar_impact_radius_lowCloud::',metar_impact_radius_lowCloud
     write(6,*)'l_cld_bld::,',l_cld_bld
     write(6,*)'cld_bld_hgt::,',cld_bld_hgt
     write(6,*)'build_cloud_frac_p::,',build_cloud_frac_p
     write(6,*)'clear_cloud_frac_p::,',clear_cloud_frac_p
     write(6,*)'l_use_hydroretrieval_all::',l_use_hydroretrieval_all
     write(6,*)'iclean_hydro_withRef::',iclean_hydro_withRef
     write(6,*)'iclean_hydro_withRef_allcol::',iclean_hydro_withRef_allcol
     write(6,*)'dfi_radar_latent_heat_time_period::',dfi_lhtp
     write(6,*)'l_conserve_thetaV::',l_conserve_thetaV
     write(6,*)'i_conserve_thetaV_iternum::',i_conserve_thetaV_iternum
  end if

!* change option to Ferrier
  opt_hydrometeor_retri=2       ! 1=Kessler 2=Lin 3=Thompson
  opt_cloudtemperature=3        ! 3=latent heat, 4,5,6 = adiabat profile
  opt_cloudwaterice_retri=1     ! 1 = RUC layer saturation and autoconvert
                                ! 2 = convective 
!
! initialize the observation flag  
!
  istat_Surface=0
  istat_NESDIS=0
  istat_radar=1
  istat_lightning=0
  istat_NASALaRC=0
  call load_gsdpbl_hgt(mype)
!
!  check consistency of the options
!

! Now either stratiform or cumulus cloud is considered in the cloud
!  water calculation. This leads to a limitation for the temperature
!  adjustment when stratiform cloud is chosen because adiabat profile
!  scheme based on the convection. This limitation may change when 
!  stratiform and cumulus cloud are both considered at the same time in the future.

  if(opt_cloudwaterice_retri == 1 .and. opt_cloudtemperature >= 4) then
     write(6,*) 'gsdcloudanalysis: ',&
       'inconsistent option for opt_cloudwaterice_retri and opt_cloudtemperature'
     write(6,*) 'gsdcloudanalysis: ',&
       'opt_cloudtemperature must be set to 3 when opt_cloudwaterice_retri =1'
     call stop2(113)
  endif
!
!----------------------------------------------
! 2. read observations                  
!----------------------------------------------
!
! 1.1   allocate observation fields
!

  allocate(ref_mos_3d(lon2,lat2,nsig))
  allocate(ref_mos_3d_tten(lon2,lat2,nsig))
  ref_mos_3d=miss_obs_real
  ref_mos_3d_tten=miss_obs_real

  allocate(lightning(lon2,lat2))
  allocate(lghtn_region_mask(lon2,lat2))
  allocate(lghtn_ref_bias(lon2,lat2))
  lightning=-9999.0_r_kind
  lghtn_region_mask=0.0_r_kind
  lghtn_ref_bias=0.0_r_kind

  allocate(sat_ctp_nesdis(lon2,lat2))
  allocate(sat_tem_nesdis(lon2,lat2))
  allocate(w_frac_nesdis(lon2,lat2))
  sat_ctp_nesdis=miss_obs_real
  sat_tem_nesdis=miss_obs_real
  w_frac_nesdis=miss_obs_real

  allocate(sat_ctp(lon2,lat2))
  allocate(sat_tem(lon2,lat2))
  allocate(w_frac(lon2,lat2))
  allocate(nlev_cld(lon2,lat2))
  sat_ctp=miss_obs_real
  sat_tem=miss_obs_real
  w_frac=miss_obs_real
  nlev_cld=miss_obs_int
!
! 1.2 start to read observations                 
!
  Nmsclvl_radar = -999
  lunin=55
  open(lunin,file=obs_setup,form='unformatted')
  rewind lunin

  numsao=0
  do is=1,ndat

     if(dtype(is) /= ' ' .and. nsat1(is) > 0)then
!
!  1.2.2 read in surface observations
!
        if( dtype(is) == 'mta_cld' ) then
           numsao=nsat1(is) 
           allocate(OI(numsao))
           allocate(OJ(numsao))
           allocate(OCLD(NVARCLD_P,numsao))
           allocate(OWX(numsao))
           allocate(Oelvtn(numsao))
           allocate(Odist(numsao))
           allocate(cstation(numsao))
           allocate(OIstation(numsao))
           allocate(OJstation(numsao))
           allocate(wimaxstation(numsao))
           allocate(kwatericemax(lon2,lat2))
           call read_Surface(mype,lunin,regional_time,istart(mype+1),jstart(mype+1),lon2,lat2, &
                             numsao,NVARCLD_P,OI,OJ,OCLD,OWX,Oelvtn,Odist,cstation,OIstation,OJstation)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                                  &
                        'Surface cloud observations are read in successfully'
           istat_Surface=1

        elseif( dtype(is) == 'gos_ctp' ) then 
!
!  1.2.4 read in NESDIS cloud products
!
           write(6,*) 'gsdcloudanalysis: ',                             &
                         'start to read NESDIS cloud products'

           call read_NESDIS(mype,lunin,nsat1(is),regional_time,istart(mype+1),            &
                            jstart(mype+1),lon2,lat2,sat_ctp,sat_tem,w_frac,ioption)
           sat_ctp_nesdis=sat_ctp
           sat_tem_nesdis=sat_tem
           w_frac_nesdis=w_frac

           istat_NESDIS = 1 

        elseif( dtype(is) == 'rad_ref' ) then
!
!  1.2.6 read in reflectivity mosaic
!
!          allocate( ref_mosaic31(lon2,lat2,31) )
!          ref_mosaic31=-9999.0_r_kind

!          call read_radar_ref(mype,lunin,regional_time,istart(mype+1),jstart(mype+1), &
!                             lon2,lat2,Nmsclvl_radar,nsat1(is),ref_mosaic31)
!          if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                         &
!                        ' radar reflectivity is read in successfully'
!          istat_radar=1

        elseif( dtype(is)=='lghtn' ) then
!
!  1.2.8 read in lightning
!
           call read_Lightningbufr2cld(mype,lunin,regional_time,istart(mype+1),jstart(mype+1), &
                                   lon2,lat2,nsat1(is),lightning)
           istat_lightning = 1 

        elseif( dtype(is) =='larccld' ) then
!
!  1.2.9 read in NASA LaRC cloud products
!
           allocate(nasalarc_cld(lon2,lat2,5))
           nasalarc_cld=miss_obs_real

           call read_NASALaRC(mype,lunin,nsat1(is),regional_time,istart(mype+1),   &
                              jstart(mype+1),lon2,lat2,nasalarc_cld)
           if(mype == 0) write(6,*) 'gsdcloudanalysis:',                       &
                         'NASA LaRC cloud products are read in successfully'
           istat_NASALaRC = 1

        else
!
!  1.2.12  all other observations 
!
           read(lunin)  obstype,isis,nreal,nchanl
           read(lunin)
        endif   ! dtype
     endif
  enddo   ! is
  close(lunin)

88 format('gsdcloud Lightning::',7i6)
!
!  1.4  if there are NASA LaRC cloud products, use them to replace NESDIS ones.
!       So we use NASA LaRC data in the same way as NESDIS ones
!
! for NMMB, just merge NESDIS and NASA cloud product

     if(istat_NASALaRC == 1 ) then
        DO j=2,lat2-1
           DO i=2,lon2-1
             if(sat_ctp(i,j) < -99990.0_r_single) then   ! missing value is -999999.0
                sat_ctp(i,j) = nasalarc_cld(i,j,1)
                sat_tem(i,j) = nasalarc_cld(i,j,2)
                w_frac(i,j)  = nasalarc_cld(i,j,3)
                nlev_cld(i,j)= int(nasalarc_cld(i,j,5))
                istat_NESDIS =istat_NASALaRC
!            else
!               write(6,*)'merge sat data::',i,j,sat_tem(i,j)
             endif
           ENDDO
        ENDDO
     endif

  do k=1,nsig
    do j=1,lat2
      do i=1,lon2
        ref_mos_3d(i,j,k)=ges_ref(j,i,k)                       ! 
        if(abs(ref_mos_3d(i,j,k))<100.0_r_single) istat_radar=1
      end do
    end do
  end do

!
!  1.6 collect the cloud information from whole domain
!       and assign the background cloud to each METAR obs
!
  allocate(sumqci(lon2,lat2,nsig))
  do k=1,nsig
     do j=1,lat2
        do i=1,lon2
           sumqci(i,j,k)= ges_ql(j,i,k) + ges_qi(j,i,k)
        enddo
     enddo
  enddo

! allocate(watericemax(lon2,lat2))
! allocate(kwatericemax(lon2,lat2))
! watericemax=0._r_kind
! wimaxstation=0.0_r_single
! kwatericemax=-1
! do j=1,lat2
!    do i=1,lon2
!      do k = 1,nsig
!         watericemax(i,j) = max(watericemax(i,j),sumqci(i,j,k))
!      end do
!      do k=1,nsig
!         if (sumqci(i,j,k) > Cloud_def_p .and. kwatericemax(i,j) == -1) then
!            kwatericemax(i,j) = k
!         end if
!      end do
!    ENDDO
! ENDDO
!
! im=nlon_regional
! jm=nlat_regional
! allocate(all_loc(lat2,lon2))
! allocate(strp(lat1*lon1))
! allocate(tempa(itotsub))
! allocate(temp1(im,jm))
! do j=1,lat2
!    do i=1,lon2
!       all_loc(j,i) = watericemax(i,j)
!    enddo
! enddo
! call strip_single(all_loc(1,1),strp,1)
! call mpi_allgatherv(strp,ijn(mype+1),mpi_real4, &
!           tempa,ijn,displs_g,mpi_real4,mpi_comm_world,ierror)
! ierror=0
! if(ierror /= 0 ) then
!    write(*,*) 'MPI error: cloud analysis=',mype
! endif
! temp1=0.0_r_single
! call unfill_mass_grid2t(tempa,im,jm,temp1)

! if(istat_Surface==1) then
!    DO ista=1,numsao
!       iob = min(max(int(oistation(ista)+0.5),1),im)
!       job = min(max(int(ojstation(ista)+0.5),1),jm)
!       wimaxstation(ista)=temp1(iob,job)
!       if(wimaxstation(ista) > 0._r_single) then
!           i=int(oi(ista))
!           j=int(oj(ista))
!       endif
!    enddo
! endif
! deallocate(all_loc,strp,tempa,temp1)


!
!  1.6 check if data available: if no data in this subdomain, return. 
!
  if( (istat_radar + istat_Surface + istat_NESDIS + istat_lightning ) == 0 ) then
     write(6,*) ' No cloud observations available, return', mype
     deallocate(ref_mos_3d,ref_mos_3d_tten,lightning,sat_ctp,sat_tem,w_frac,nlev_cld)
     return
  endif
!
!----------------------------------------------
! 2. allocated background arrays and read background  
!    further observation data process before cloud analysis
!----------------------------------------------
!
! 2.2   allocate background and analysis fields
!
  allocate(t_bk(lon2,lat2,nsig))
  allocate(h_bk(lon2,lat2,nsig))
  allocate(p_bk(lon2,lat2,nsig))
  allocate(ps_bk(lon2,lat2))
  allocate(zh(lon2,lat2))
  allocate(q_bk(lon2,lat2,nsig))
  allocate(sumq(lon2,lat2,nsig))
  
  allocate(xlon(lon2,lat2))
  allocate(xlat(lon2,lat2))
  allocate(xland(lon2,lat2))
  allocate(soiltbk(lon2,lat2))
!  allocate(z_lcl(lon2,lat2))    
  allocate(pblh(lon2,lat2))
  
  allocate(cldwater_3d(lon2,lat2,nsig))
  allocate(cldice_3d(lon2,lat2,nsig))
  allocate(rain_3d(lon2,lat2,nsig))
  allocate(nrain_3d(lon2,lat2,nsig))
  allocate(snow_3d(lon2,lat2,nsig))
  allocate(graupel_3d(lon2,lat2,nsig))
  allocate(cldtmp_3d(lon2,lat2,nsig))


  cldwater_3d=miss_obs_real
  cldice_3d=miss_obs_real
  rain_3d=miss_obs_real
  nrain_3d=miss_obs_real
  snow_3d=miss_obs_real
  graupel_3d=miss_obs_real
  cldtmp_3d=miss_obs_real
!          
! 2.4 read in background fields
!          
  do j=1,lat2                
     do i=1,lon2
        iss=jstart(mype+1)+i-1
        jss=istart(mype+1)+j-1
        zh(i,j)     =ges_z(j,i)                     !  terrain in meter
        ps_bk(i,j)  =ges_ps(j,i)*10.0_r_single      !  surace pressure in mb
        xland(i,j)  =isli2(j,i)
        soiltbk(i,j)=soil_temp(j,i,itsfc)           !  soil temperature
        xlon(i,j)   =region_lon(jss,iss)*rad2deg    !  longitude back to degree
        xlat(i,j)   =region_lat(jss,iss)*rad2deg    !  latitude  back to degree

!       xland(i,j)  =isli_cld(j,i,itsfc)            !  0=water, 1=land, 2=ice
!       xlon(i,j)   =ges_xlon(j,i,itsfc)*rad2deg    !  longitude back to degree
!       xlat(i,j)   =ges_xlat(j,i,itsfc)*rad2deg    !  latitude  back to degree
!       soiltbk(i,j)=0.0
!       xlon(i,j)   =0.0
!       xlat(i,j)   =0.0
!       write(6,*)'xlon, xlat::',iss,jss,region_lon(jss,iss)*rad2deg,region_lat(jss,iss)*rad2deg,xland(i,j)
     ENDDO
  ENDDO

  do k=1,nsig
     do j=1,lat2
        do i=1,lon2
           q_bk(i,j,k)=ges_q(j,i,k)/(1.0_r_kind-ges_q(j,i,k))   ! specific humidity to mixing ratio, ges_q is spfh

           h_bk(i,j,k)=geop_hgtl(j,i,k,1)                           ! 
!          t_bk(i,j,k)=ges_tv(j,i,k,itsig)/                                  &
!                    (1.0_r_single+0.61_r_single*q_bk(i,j,k))   ! virtual temp to temp
           t_bk(i,j,k)=ges_tsen(j,i,k,1)*(100.0_r_kind/ges_prsl(j,i,k,1))**rd_over_cp  !to potential temperature
           p_bk(i,j,k)=ges_prsl(j,i,k,1)*10.0_r_kind
        ENDDO
     ENDDO
  ENDDO

! call BackgroundCld(mype,lon2,lat2,nsig,t_bk,p_bk,ps_bk,q_bk,h_bk,    &
!            zh,pt_ll,eta1_ll,aeta1_ll,regional,wrf_mass_regional)

! 
! 2.5 calculate PBL height
! 
  call calc_pbl_height(mype,lat2,lon2,nsig,q_bk,t_bk,p_bk,pblh)

!
!  2.6 vertical interpolation of radar reflectivity
!
  if(istat_radar ==  1 ) then
!    call vinterp_radar_ref(mype,lon2,lat2,nsig,Nmsclvl_radar, &
!                         ref_mos_3d,ref_mosaic31,h_bk,zh)
!    deallocate( ref_mosaic31 )
     ref_mos_3d_tten=ref_mos_3d
     call build_missing_REFcone(mype,lon2,lat2,nsig,krad_bot,ref_mos_3d_tten,h_bk,pblh)
  end if

!
!  2.8 convert lightning to reflectivity 
!
  do j=1,lat2
    do i=1,lon2
      cref(i,j)=-999.0_r_single
      do k=1,nsig
        if(abs(ref_mos_3d(i,j,k))<100.0_r_single)then
          cref(i,j) = max(cref(i,j),ref_mos_3d(i,j,k))
        end if
      end do
    end do
  end do

  do j=1,lat2
    do i=1,lon2
     do i1=-1,1
       do j1=-1,1
        i2=i+i1; j2=j+j1
        if(i2>0.and.j2>0) then
          if(abs(cref(i2,j2))<100.0_r_single) then
!           lightning(i,j)=-999.0_r_kind
            lghtn_region_mask(i,j)=1.0_r_kind !* with radar coverage
          end if
        end if
       end do
     end do
    end do
  end do

  if(istat_lightning ==  1 ) then
     call convert_lghtn2ref(mype,lon2,lat2,nsig,ref_mos_3d_tten,       &
                      lightning,lghtn_region_mask,lghtn_ref_bias,h_bk)
  endif

!
!
!----------------------------------------------
! 3.  Calculate 3-d cloud cover obs-information field (cld_cover_3d), 
!               cloud type, precipitation type 
!----------------------------------------------
!
  allocate(cld_cover_3d(lon2,lat2,nsig))
  allocate(cld_type_3d(lon2,lat2,nsig))
  allocate(wthr_type_2d(lon2,lat2))
  allocate(pcp_type_3d(lon2,lat2,nsig))
  allocate(cloudlayers_i(lon2,lat2,21))
  cld_cover_3d=miss_obs_real
  cld_type_3d =miss_obs_int
  wthr_type_2d=miss_obs_int
  pcp_type_3d =miss_obs_int
!
!
  if(istat_Surface ==  1) then
     wimaxstation=0.0_r_kind; kwatericemax=0
     call cloudCover_surface(mype,lat2,lon2,nsig,r_radius,thunderRadius,  &
              t_bk,p_bk,q_bk,h_bk,zh,                                     &
              numsao,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,Odist,        &
              cld_cover_3d,cld_type_3d,wthr_type_2d,pcp_type_3d,          &
              wimaxstation, kwatericemax)
     if(mype == 0) write(6,*) 'gsdcloudanalysis:',                        &  
                   'success in cloud cover analysis using surface data'
  endif

  if(istat_NESDIS == 1 ) then
     call cloudCover_NESDIS(mype,regional_time,lat2,lon2,nsig,            &
                         xlon,xlat,t_bk,p_bk,h_bk,zh,xland,               &
                         soiltbk,sat_ctp,sat_tem,w_frac,                  &
                         l_cld_bld,cld_bld_hgt,                           &
                         build_cloud_frac_p,clear_cloud_frac_p,nlev_cld,  &
                         cld_cover_3d,cld_type_3d,wthr_type_2d)

     if(mype == 0) write(6,*) 'gsdcloudanalysis:',                        & 
                   ' success in cloud cover analysis using NESDIS data'
  endif

! for Rapid Refresh application, turn off the radar reflectivity impact 
! if(istat_radar == 1 .or. istat_lightning == 1 ) then
!    write(6,*)'gsdcloudanalysis sliu max reflectivity::', maxval(ref_mos_3d)
!    call cloudCover_radar(mype,lat2,lon2,nsig,h_bk,zh,ref_mos_3d,  &
!                          cld_cover_3d,cld_type_3d,wthr_type_2d)
!    if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                 & 
!                  ' success in cloud cover analysis using radar data'
! endif
! for NMMB, turn on the radar reflectivity impact 
  call cloudCover_radar(mype,lat2,lon2,nsig,h_bk,zh,ref_mos_3d_tten,  &
                           cld_cover_3d,cld_type_3d,wthr_type_2d)

!
!----------------------------------------------
! 4.  Calculate 3-d cloud water and ice  
!     Calculate 3-d hydrometeors 
!     Calculate radar temperature tendency
!     Calculate in cloud temperature
!     moisture adjustment (to do)
!----------------------------------------------
!
! 4.2 find the cloud layers
!

  call cloudLayers(lat2,lon2,nsig,h_bk,zh,cld_cover_3d,               &
                   cld_type_3d,cloudlayers_i)
  if(mype==0) write(6,*) 'gsdcloudanalysis: success in finding cloud layers'
!
! 4.4 decide the cloud type
!
  call cloudType(lat2,lon2,nsig,h_bk,t_bk,p_bk,ref_mos_3d,            &
                 cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i)
  if(mype==0)  write(6,*) 'gsdcloudanalysis: success in deciding cloud types'
!
! 4.6 calculate liquid water content
!
  if(opt_cloudwaterice_retri == 1 ) then
     call cloudLWC_stratiform(mype,lat2,lon2,nsig,q_bk,t_bk,p_bk,      &
                  cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                  cldwater_3d,cldice_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 'success in modifying hydrometeors for stratiform clouds '

  elseif (opt_cloudwaterice_retri == 2) then
     call cloudLWC_Cumulus(lat2,lon2,nsig,h_bk,t_bk,p_bk,              &
                  cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                  cldwater_3d,cldice_3d,cldtmp_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 ' success in modifying hydrometeors from radar reflectivity'
  else
     write(6,*)'gsdcloudanalysis: ',                                   &
      ' Invalid cloud water calculation option, check opt_cloudwaterice_retri'
     call stop2(113)
  endif
!
! 4.8 calculate hydrometeors
!

  istat_radar=1
  where(rain_3d<0.0_r_single) rain_3d=0.0_r_single
  where(snow_3d<0.0_r_single) snow_3d=0.0_r_single
  if(istat_radar == 1 .or. istat_lightning == 1) then
     call PrecipType(lat2,lon2,nsig,t_bk,p_bk,q_bk,ref_mos_3d,         &
                    wthr_type_2d,pcp_type_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 ' success in deciding precipitation type'

     call PrecipMxR_radar(mype,lat2,lon2,nsig,      &
                  t_bk,p_bk,ref_mos_3d, q_bk, &
                  pcp_type_3d,rain_3d,nrain_3d,snow_3d,graupel_3d,opt_hydrometeor_retri) 
     !* note: for NMMB get qr, qs in kg/kg
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                               &
                 ' success in determining hydrometeor types from radar refl'
  endif

!
! 4.10 radar temperature tendency for DFI
!

! dfi_lhtp=20    ! 20 min forward intergration
   
  if (istat_radar==1) then
  if (istat_NESDIS==1) then
     call radar_ref2tten(mype,istat_radar,istat_lightning,lon2,lat2,nsig,ref_mos_3d_tten, &
                       cld_cover_3d,p_bk,t_bk,ges_tten,dfi_lhtp,krad_bot,pblh,sat_ctp)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                               &
                 ' success in getting dfi_tten from radar refl, istat_NESDIS==1'
  else
     call radar_ref2tten_nosat(mype,istat_radar,istat_lightning,lon2,lat2,nsig,      &
                       ref_mos_3d_tten,cld_cover_3d,p_bk,t_bk,ges_tten,dfi_lhtp,krad_bot,pblh)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                               &
                 ' success in getting dfi_tten from radar refl, istat_NESDIS==0'
  endif
  endif

!
! 4.12  temperature adjustment
!
!  call TempAdjust(mype,lat2,lon2,nsig,opt_cloudtemperature, t_bk, p_bk, w_bk, q_bk, &
!                   cldwater_3d,cldice_3d,cldtmp_3d)
!
!----------------------------------------------
! 5.  the final analysis or update background
!----------------------------------------------
!
!  the final analysis of cloud 

  DO k=1,nsig
     DO j=2,lat2-1
        DO i=2,lon2-1
           sumq(i,j,k)= ges_ql(j,i,k) + ges_qi(j,i,k)
           if( cld_cover_3d(i,j,k) > -0.001_r_single ) then 
              if( cld_cover_3d(i,j,k) > 0.6_r_single ) then 
                 cldwater_3d(i,j,k) = max(0.001_r_single*cldwater_3d(i,j,k),ges_ql(j,i,k))
                 cldice_3d(i,j,k)   = max(0.001_r_single*cldice_3d(i,j,k),ges_qi(j,i,k))
              else if(cld_cover_3d(i,j,k)<0.1_r_single) then   ! clean  cloud
                 cldwater_3d(i,j,k) = zero_single
                 cldice_3d(i,j,k) = zero_single
              else
                 cldwater_3d(i,j,k) = ges_ql(j,i,k)
                 cldice_3d(i,j,k) = ges_qi(j,i,k)
              endif
           else   ! unknown, using background values
              cldwater_3d(i,j,k) = ges_ql(j,i,k)
              cldice_3d(i,j,k) = ges_qi(j,i,k)
           endif
        END DO
     END DO
  END DO

!
!  the final analysis of precipitation
!
 allocate(max_bk_qrqs_save(lon2,lat2))
 allocate(max_obs_qrqs_save(lon2,lat2))
 allocate(ratio_qrqs_save(lon2,lat2))
 max_bk_qrqs_save=-999.0_r_single
 max_obs_qrqs_save=-999.0_r_single
 ratio_qrqs_save=-999.0_r_single
 ref_mos_3d=ref_mos_3d_tten

 if(l_use_hydroretrieval_all) then     !* use retr_qr, retr_qs block
     qrlimit=15.0_r_kind*0.001_r_kind
     DO k=1,nsig-1
       DO j=2,lat2-1
         DO i=2,lon2-1
           snowtemp=snow_3d(i,j,k)
           raintemp=rain_3d(i,j,k)
!          nraintemp=nrain_3d(i,j,k)
           rain_3d(i,j,k) = ges_qr(j,i,k)
!          nrain_3d(i,j,k)= ges_qnr(j,i,k)
           snow_3d(i,j,k) = ges_qs(j,i,k)
!          graupel_3d(i,j,k) = ges_qg(j,i,k)
           if(ref_mos_3d(i,j,k) > zero ) then
!             snow_3d(i,j,k) = MIN(max(max(snowtemp,zero)*0.001_r_kind,ges_qs(j,i,k)),qrlimit)
              snow_3d(i,j,k) = MIN(max(max(snowtemp,zero),ges_qs(j,i,k)),qrlimit)
!              rain_3d(i,j,k) = MIN(max(max(raintemp,zero)*0.001_r_kind,ges_qr(j,i,k)),qrlimit)  
!             raintemp = max(raintemp,zero)*0.001_r_kind  
              raintemp = max(raintemp,zero)
              if(raintemp > ges_qr(j,i,k) ) then
                  if(raintemp <= qrlimit) then
                     rain_3d(i,j,k) = raintemp
!                    nrain_3d(i,j,k)= nraintemp
                  else
                     rain_3d(i,j,k) = qrlimit
!                    nrain_3d(i,j,k)= nraintemp*(qrlimit/raintemp)
                  endif
              else
                 rain_3d(i,j,k) = MIN(ges_qr(j,i,k),qrlimit)
              endif
           elseif( ref_mos_3d(i,j,k) <= zero .and. & 
                   ref_mos_3d(i,j,k) > -100.0_r_kind ) then
              rain_3d(i,j,k) = zero
!             nrain_3d(i,j,k) = zero
              snow_3d(i,j,k) = zero
!             graupel_3d(i,j,k) = zero
           else
              rain_3d(i,j,k) = ges_qr(j,i,k)
!             nrain_3d(i,j,k)= ges_qnr(j,i,k)
              snow_3d(i,j,k) = ges_qs(j,i,k)
!             graupel_3d(i,j,k) = ges_qg(j,i,k)
           endif
         END DO
       END DO
     END DO
 else  ! hydrometeor anlysis for NMM forecast

     qrlimit=3.0_r_kind*0.001_r_kind
     DO j=2,lat2-1
       DO i=2,lon2-1
         refmax=-999.0_r_kind
         imaxlvl_ref=0
         DO k=1,nsig
           if(ref_mos_3d(i,j,k) > refmax) then
              imaxlvl_ref=k
              refmax=ref_mos_3d(i,j,k)
           endif
!!          rain_3d(i,j,k)=max(rain_3d(i,j,k)*0.001_r_kind,zero)
!!          snow_3d(i,j,k)=max(snow_3d(i,j,k)*0.001_r_kind,zero)
           rain_3d(i,j,k)=max(rain_3d(i,j,k),zero)
           snow_3d(i,j,k)=max(snow_3d(i,j,k),zero)
!!           ges_qnr(i,j,k)=max(ges_qnr(i,j,k),zero)
         ENDDO

         !* if refmax > 0 and refmax above the model bottom and below model top
         if( refmax > 0 .and. (imaxlvl_ref > 0 .and. imaxlvl_ref < nsig ) ) then   ! use retrieval hybrometeors
!          tsfc=t_bk(i,j,1)*(p_bk(i,j,1)/h1000)**rd_over_cp - 273.15_r_kind
!          tsfc=ges_tsen(i,j,1,1)- 273.15_r_kind
           !* compare ges snow and retr snow, using the larger one
           !* this will add snow based on retr snow. 
           !* the net add snow will be removed from rain at the same place
           !* the may not work for NMMB
!          r_cleanSnow_WarmTs_threshold = 4.0    !to delete
!          if(tsfc  < r_cleanSnow_WarmTs_threshold) then    ! add snow on cold sfc   
!             DO k=1,nsig
!                snowtemp=snow_3d(i,j,k) 
!                rain_3d(i,j,k) = ges_qr(j,i,k)
!                snow_3d(i,j,k) = ges_qs(j,i,k)
!                if(ref_mos_3d(i,j,k) > zero ) then
!                   snowtemp = MIN(max(snowtemp,ges_qs(j,i,k)),qrlimit)
!                   snowadd = max(snowtemp - snow_3d(i,j,k),zero)
!                   snow_3d(i,j,k) = snowtemp
!                   raintemp=rain_3d(i,j,k)   !+ graupel_3d(i,j,k)
!                   if(raintemp > snowadd ) then
!                      if(raintemp > 1.0e-6_r_kind) then
!                         ratio2=1.0_r_kind - snowadd/raintemp
!                         rain_3d(i,j,k) = rain_3d(i,j,k) * ratio2
!!                         graupel_3d(i,j,k) = graupel_3d(i,j,k) * ratio2
!                      endif
!                   else
!                      rain_3d(i,j,k) = 0.0_r_kind
!!                      graupel_3d(i,j,k) = 0.0_r_kind
!                   endif
!                endif
!              END DO
!          else    !  adjust hydrometeors based on maximum reflectivity level
              !* get retr hydrometeors on the maximum ref level
              !* get bk maximum hydrometeors in the column
              max_retrieved_qrqs=snow_3d(i,j,imaxlvl_ref)+rain_3d(i,j,imaxlvl_ref)
              max_bk_qrqs=-999.0_r_kind
              imaxlvl_modref=-999
              max_obs_qrqs_save(i,j)=max_retrieved_qrqs
              DO k=1,nsig
                 if(ges_qr(j,i,k)+ges_qs(j,i,k) > max_bk_qrqs) then
                     max_bk_qrqs = ges_qr(j,i,k)+ges_qs(j,i,k)
                     imaxlvl_modref=k
                 endif
              ENDDO
              max_bk_qrqs_save(i,j)=max_bk_qrqs
              !* if bk max_hydro is larger, get ratio, based on ratio reduce bk qr and qs
              !* if no retr max_hydro, what to do??? 
              if( max_bk_qrqs > max_retrieved_qrqs) then ! tune background hyhro
                 ratio_hyd_bk2obs=max(min(max_retrieved_qrqs/max_bk_qrqs,1.0_r_kind),0.80_r_kind)
                 ratio_qrqs_save(i,j)=ratio_hyd_bk2obs
                 do k=1,nsig
!!                   graupel_3d(i,j,k) = ges_qg(j,i,k)
!                   rain_3d(i,j,k) = ges_qr(j,i,k)
!!                   nrain_3d(i,j,k)= ges_qnr(j,i,k)
!                   snow_3d(i,j,k) = ges_qs(j,i,k)
                    if(ges_qr(j,i,k) > qrlimit/2.5_r_kind) then   !* based on qr retr curve
!                      rain_3d(i,j,k) = ges_qr(j,i,k)*ratio_hyd_bk2obs
!                      rain_3d(i,j,k) = MAX(rain_3d(i,j,k),ges_qr(j,i,k))
!!                      nrain_3d(i,j,k)= ges_qnr(j,i,k)*ratio_hyd_bk2obs
                       rain_3d(i,j,k) = MAX(MIN(rain_3d(i,j,k),qrlimit),ges_qr(j,i,k)*ratio_hyd_bk2obs)  ! do we need qrlimit?              
                    else
                       rain_3d(i,j,k) = ges_qr(j,i,k)
                    endif
                    if(ges_qs(j,i,k) > qrlimit/3.0_r_kind) then   !* based on qs retr curve
!                      snow_3d(i,j,k) = ges_qs(j,i,k)*ratio_hyd_bk2obs
!                      snow_3d(i,j,k) = MAX(snow_3d(i,j,k),ges_qs(j,i,k))
                       snow_3d(i,j,k) = MAX(MIN(snow_3d(i,j,k),qrlimit),ges_qs(j,i,k)*ratio_hyd_bk2obs)
                    else
                       snow_3d(i,j,k) = ges_qs(j,i,k)
                    end if
                 enddo
              else      !  use hydro in max refl level
              !* if bk max_hydro is smaller, just use retr hydr at max ref levl 
                 DO k=1,nsig
!                   graupel_3d(i,j,k) = ges_qg(j,i,k)
                    if(k==imaxlvl_ref) then
                       snow_3d(i,j,k) = MAX(MIN(snow_3d(i,j,k),qrlimit),ges_qs(j,i,k))
                       rain_3d(i,j,k) = MAX(MIN(rain_3d(i,j,k),qrlimit),ges_qr(j,i,k))  ! do we need qrlimit?              
!                      snow_3d(i,j,k) = MIN(snow_3d(i,j,k),qrlimit)
!                      rain_3d(i,j,k) = MIN(rain_3d(i,j,k),qrlimit)  ! do we need qrlimit?              
!                      nrain_3d(i,j,k) = nrain_3d(i,j,k)
                    else
                       snow_3d(i,j,k) = MAX(MIN(snow_3d(i,j,k),qrlimit)*1.0_r_kind,ges_qs(j,i,k))
                       rain_3d(i,j,k) = MAX(MIN(rain_3d(i,j,k),qrlimit)*1.0_r_kind,ges_qr(j,i,k))  ! do we need qrlimit?              
!                      rain_3d(i,j,k) = ges_qr(j,i,k)
!                      snow_3d(i,j,k) = ges_qs(j,i,k)
!                      nrain_3d(i,j,k) = ges_qnr(j,i,k)
                    endif

!                 else
!                      !use retr qr and qs
!                      snow_3d(i,j,k) = MIN(snow_3d(i,j,k),qrlimit)
!                      rain_3d(i,j,k) = MIN(rain_3d(i,j,k),qrlimit)  ! do we need qrlimit?              
!                 endif
                 END DO
              end if
        else        ! clean if ref=0 or use background hydrometeors
           !* !if( refmax > 0 .and. (imaxlvl_ref > 0 .and. imaxlvl_ref < nsig ) ) then   ! use retrieval hybrometeors
           DO k=1,nsig
              rain_3d(i,j,k) = ges_qr(j,i,k)
!!             nrain_3d(i,j,k)= ges_qnr(j,i,k)
              snow_3d(i,j,k) = ges_qs(j,i,k)
!!             graupel_3d(i,j,k) = ges_qg(j,i,k)
              !* using obsref -99 value 
              if((iclean_hydro_withRef==1)) then
                 if( iclean_hydro_withRef_allcol==1 .and. &
                    (refmax <= zero .and. refmax >= -100_r_kind) .and. &
                    (sat_ctp(i,j) >=1010.0_r_kind .and. sat_ctp(i,j) <1050._r_kind)) then     
                    rain_3d(i,j,k) = rain_3d(i,j,k)*0.80_r_single
!                   nrain_3d(i,j,k)= zero 
                    snow_3d(i,j,k) = snow_3d(i,j,k)*0.80_r_single
!                   graupel_3d(i,j,k) = zero
                 else
                    if((ref_mos_3d(i,j,k) <= zero .and.       &
                        ref_mos_3d(i,j,k) > -100.0_r_kind)) then
                       rain_3d(i,j,k) = rain_3d(i,j,k)*0.80_r_single
!                      nrain_3d(i,j,k)= zero
                       snow_3d(i,j,k) = snow_3d(i,j,k)*0.80_r_single
!                      graupel_3d(i,j,k) = zero
                    endif
                 endif
              endif
           END DO
         endif
       END DO
     END DO
 endif  !* use retr_qr retr_qs block

!
!  remove any negative hydrometeor mixing ratio values
!
  where(rain_3d<0.0_r_single) rain_3d=0.0_r_single
  where(snow_3d<0.0_r_single) snow_3d=0.0_r_single
  DO k=1,nsig
     DO j=2,lat2-1
        DO i=2,lon2-1
           cldwater_3d(i,j,k)= max(0.0_r_single,cldwater_3d(i,j,k))
           cldice_3d(i,j,k)  = max(0.0_r_single,cldice_3d(i,j,k))
           rain_3d(i,j,k)    = max(0.0_r_single,rain_3d(i,j,k))
           snow_3d(i,j,k)    = max(0.0_r_single,snow_3d(i,j,k))
           graupel_3d(i,j,k) = max(0.0_r_single,graupel_3d(i,j,k))
        END DO
     END DO
  END DO

! l_conserve_thetaV  = .true.
! i_conserve_thetaV_iternum = 3

  call cloud_saturation(mype,l_conserve_thetaV,i_conserve_thetaV_iternum,  &
                 lat2,lon2,nsig,q_bk,t_bk,p_bk,      &
                 cld_cover_3d,wthr_type_2d,cldwater_3d,cldice_3d,sumqci)
 
989 format("sliu debug1::",4e15.4)
988 format("ratio_hyd::",4i8,3e15.4)

!  call check_cloud(mype,lat2,lon2,nsig,q_bk,rain_3d,snow_3d,graupel_3d, &
!             cldwater_3d,cldice_3d,t_bk,p_bk,h_bk,                      &
!             numsao,NVARCLD_P,numsao,OI,OJ,OCLD,OWX,Oelvtn,cstation,    &
!             sat_ctp,cld_cover_3d)
!----------------------------------------------
! 6.  save the analysis results
!----------------------------------------------
  do k=1,nsig
     do j=1,lat2
        do i=1,lon2

           ges_q(j,i,k)=q_bk(i,j,k)/(1_r_kind+q_bk(i,j,k))   ! Here q is mixing ratio kg/kg, 
                                                        ! need to convert to specific humidity
!          t_inc=min(t_bk(i,j,k)/((100.0/ges_prsl(j,i,k,1))**rd_over_cp)-ges_tsen(j,i,k,1),0.3)
!          ges_tsen(j,i,k,1)=ges_tsen(j,i,k,1)+t_inc

           ges_tsen(j,i,k,1)= t_bk(i,j,k)/((100.0_r_kind/ges_prsl(j,i,k,1))**rd_over_cp)

           dfi_tten(j,i,k)=0.0_r_kind
!*         total heat=ges_tten*20min*60second, for each step:  total_heat/(3600 filter window)*(23 time step)
!*         for this case:  ges_tten*2400/3600*23=ges_tten*(10 to 15)

           if(ges_tten(j,i,k)<1.0_r_kind.and. ges_tten(j,i,k)>-0.0000001_r_kind) then
             dfi_tten(j,i,k)=ges_tten(j,i,k) !*20.0_r_kind
             if(dfi_tten(j,i,k)>0.0025_r_kind) dfi_tten(j,i,k)=0.0025_r_kind
           end if

           if(xland(i,j)==0.0_r_single)dfi_tten(j,i,k)=dfi_tten(j,i,k)*0.2_r_kind
           if(dfi_tten(j,i,k)<0.0_r_kind)dfi_tten(j,i,k)=0.0_r_kind

           ges_qr(j,i,k)=rain_3d(i,j,k)
           ges_qs(j,i,k)=snow_3d(i,j,k)
   
           ges_ql(j,i,k)=cldwater_3d(i,j,k)
           ges_qi(j,i,k)=cldice_3d(i,j,k)

!          t=ges_tsen(j,i,k,1)
           qfi=ges_qi(j,i,k)
           qfs=ges_qs(j,i,k)
           qfr=ges_qr(j,i,k)
           qfw=ges_ql(j,i,k)
           ges_qi(j,i,k)=qfi+qfs
           ges_qg(j,i,k)=qfi+qfs+qfr+qfw

        ENDDO
     ENDDO
  ENDDO

!
!----------------------------------------------
! 7.  release space
!----------------------------------------------
!
  deallocate(cld_cover_3d,cld_type_3d,wthr_type_2d, &
             pcp_type_3d,cloudlayers_i)
  deallocate(t_bk,h_bk,p_bk,ps_bk,zh,q_bk,sumq,sumqci,pblh)
  deallocate(xlon,xlat,xland,soiltbk)
  deallocate(cldwater_3d,cldice_3d,rain_3d,snow_3d,graupel_3d,cldtmp_3d)

  if(istat_Surface ==  1 ) then
     deallocate(OI,OJ,OCLD,OWX,Oelvtn,Odist,cstation)
  endif
  if(istat_NASALaRC == 1 ) then
     deallocate(nasalarc_cld)
  endif

  deallocate(sat_ctp,sat_tem,w_frac,nlev_cld)
  deallocate(ref_mos_3d,ref_mos_3d_tten,lightning)

  if(mype==0) then
     write(6,*) '========================================'
     write(6,*) 'gsdcloudanalysis: generalized cloud analysis finished:',mype
     write(6,*) '========================================'
  endif

END SUBROUTINE gsdcloudanalysis4NMMB


#else /* Start no NNMB cloud analysis library block */
SUBROUTINE  gsdcloudanalysis4NMMB(mype)
use kinds, only: i_kind
implicit none
integer(i_kind),intent(in) :: mype

if(mype==0) then
   write(*,*) 'dummy subroutine gsdcloudanalysis4NMMB'
endif

END SUBROUTINE  gsdcloudanalysis4NMMB
#endif /* End no NNMB cloud analysis library block */
