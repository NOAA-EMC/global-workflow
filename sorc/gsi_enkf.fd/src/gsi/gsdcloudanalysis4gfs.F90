subroutine  gsdcloudanalysis4gfs(mype)
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
!    2013-10-19  todling - metguess now holds background 
!    2013-10-24  todling - revisit strip interface
!    2014-10-22  Hu      - Add analysis for rain number concentation 
!                          reflectivity between 15-28dBZ
!    2014-12-22  Hu      - Add light rain in precipiation analysis using radar
!                          reflectivity between 15-28dBZ
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
  use constants, only: zero,one,rad2deg,fv
  use constants, only: rd_over_cp, h1000
  use kinds,   only: r_single,i_kind, r_kind
  use gridmod, only: pt_ll,eta1_ll,aeta1_ll
  use gridmod, only: regional,wrf_mass_regional,regional_time
  use gridmod, only: nsig,lat2,lon2,istart,jstart,twodvar_regional
  use gridmod, only: itotsub,lon1,lat1,nlon,nlat,ijn,displs_g,strip
  use gridmod, only: rlats,rlons
  use guess_grids, only: pbl_height, load_gsdpbl_hgt
  use obsmod,  only: obs_setup,nsat1,ndat,dtype
  use guess_grids, only: ntguessig,ntguessfc
  use wrf_mass_guess_mod, only: soil_temp_cld,isli_cld,ges_xlon,ges_xlat,ges_tten
  use guess_grids, only: isli,soil_temp,isli2
  use guess_grids, only: ges_tsen,ges_prsl
  use jfunc, only: tsensible
  use mpimod, only: mpi_comm_world,ierror,mpi_real4
  use rapidrefresh_cldsurf_mod, only: dfi_radar_latent_heat_time_period,   &
                                      metar_impact_radius,                 &
                                      metar_impact_radius_lowCloud,        &
                                      l_cleanSnow_WarmTs,l_conserve_thetaV,&
                                      r_cleanSnow_WarmTs_threshold,        &
                                      i_conserve_thetaV_iternum,           &
                                      l_cld_bld, cld_bld_hgt,              &
                                      build_cloud_frac_p, clear_cloud_frac_p, &
                                      nesdis_npts_rad, &
                                      iclean_hydro_withRef, iclean_hydro_withRef_allcol, &
                                      i_lightpcp,i_gsdcldanal_type,ioption

  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

#ifdef RR_CLOUDANALYSIS
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
  real(r_single),allocatable:: gsfc(:,:,:)
  real(r_single),  allocatable:: xland(:,:)
  real(r_single),allocatable:: soiltbk(:,:)
!  real(r_single),allocatable:: z_lcl(:,:)       ! lifting condensation level
!  real(r_single),allocatable:: pblh(:,:)         ! PBL height (grid coordinate)
!
!  surface observation
!
  integer(i_kind) :: nvarcld_p
  parameter (nvarcld_p=13)

  integer(i_kind)              :: numsao
  real(r_single), allocatable  :: oi(:)
  real(r_single), allocatable  :: oj(:)
  integer(i_kind),allocatable  :: ocld(:,:)
  character*10,   allocatable  :: owx(:)
  real(r_single), allocatable  :: oelvtn(:)
  real(r_single), allocatable  :: odist(:)
  character(8),   allocatable  :: cstation(:)
  real(r_single), allocatable  :: oistation(:)
  real(r_single), allocatable  :: ojstation(:)
  real(r_single), allocatable  :: wimaxstation(:)
!
  integer(i_kind),allocatable  :: osfc_station_map(:,:)
!
!  lightning observation: 2D field in RR grid
!
  real(r_single),allocatable  :: lightning(:,:)
!
!  GOES - NASA LaRc cloud products: several 2D fields in RR grid
!
  real(r_single),allocatable  :: nasalarc_cld(:,:,:)

!
!  radar observation : 3D reflectvity in RR grid
!
  real(r_kind),allocatable :: ref_mos_3d(:,:,:)
  real(r_kind),allocatable :: ref_mos_3d_tten(:,:,:)
  real(r_kind),allocatable :: ref_mosaic31(:,:,:)
  integer(i_kind)          :: nmsclvl_radar 
!
!  GOES - NESDIS cloud products : 2d fields
!
  real(r_single), allocatable :: sat_ctp(:,:)
  real(r_single), allocatable :: sat_tem(:,:)
  real(r_single), allocatable :: w_frac(:,:)
  integer(i_kind),allocatable :: nlev_cld(:,:)
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

  real(r_single), allocatable :: cld_cover_3d(:,:,:)  ! cloud cover
  integer(i_kind),allocatable :: cld_type_3d(:,:,:)   ! cloud type
  integer(i_kind),allocatable :: pcp_type_3d(:,:,:)   ! precipitation type
  integer(i_kind),allocatable :: wthr_type_2d(:,:)    ! weather type type
  integer(i_kind),allocatable :: cloudlayers_i(:,:,:) ! 5 different layers 
                                                      ! 1= the number of layers
                                                      ! 2,4,... bottom
                                                      ! 3,5,... top
!
  real(r_single),allocatable :: cldwater_3d(:,:,:)    ! cloud water
  real(r_single),allocatable :: cldice_3d(:,:,:)      ! cloud ice
  real(r_single),allocatable :: rain_3d(:,:,:)        ! rain
  real(r_single),allocatable :: nrain_3d(:,:,:)       ! rain number concentration
  real(r_single),allocatable :: snow_3d(:,:,:)        ! snow
  real(r_single),allocatable :: graupel_3d(:,:,:)     ! graupel
  real(r_single),allocatable :: cldtmp_3d(:,:,:)      ! cloud temperature

  real(r_single),allocatable :: rain_1d_save(:)       ! rain
  real(r_single),allocatable :: nrain_1d_save(:)      ! rain number concentration    
  real(r_single),allocatable :: snow_1d_save(:)       ! snow
  real(r_single),allocatable :: vis2qc(:,:)           ! fog

  real(r_kind)    ::  thunderRadius=2.5_r_kind
  real(r_single)  ::  r_radius          ! influence radius of cloud based on METAR obs
  real(r_single)  ::  r_radius_lowCloud ! influence radius of low cloud to cloud top pressure
  integer(i_kind) :: miss_obs_int
  real(r_kind)    :: miss_obs_real
  parameter ( miss_obs_int = -99999  )
  parameter ( miss_obs_real = -99999.0_r_kind )
  real(r_single)  ::  krad_bot          ! radar bottom level

!
! collect cloud
  real(r_kind)    :: cloud_def_p
  data  cloud_def_p       / 0.000001_r_kind/
  real(r_kind),allocatable :: sumqci(:,:,:)  ! total liquid water
  real(r_kind),allocatable :: watericemax(:,:)  ! max of total liquid water
  integer(i_kind),allocatable :: kwatericemax(:,:)  ! lowest level of total liquid water
  real(r_single),allocatable::temp1(:,:),tempa(:)
  real(r_single),allocatable::all_loc(:,:)
  real(r_single),allocatable::strp(:)
  integer(i_kind) :: im,jm
!
! option in namelist
!
  integer(i_kind) :: opt_cloudwaterice_retri  ! method for cloud water retrieval
  integer(i_kind) :: opt_hydrometeor_retri    ! method for precipitation retrieval
  integer(i_kind) :: opt_cloudtemperature     ! if open temperature adjustment scheme
  integer(i_kind) :: istat_surface,istat_nesdis,istat_radar    ! 1 has observation
  integer(i_kind) :: istat_nasalarc,istat_lightning            ! 0 no observation
  integer(i_kind) :: imerge_nesdis_nasalarc  !  =1 merge NASA LaRC with NESDIS
                                             !  =2 use NASA LaRC only
                                             !  = other, use NESDIS only
!
!
  real(r_kind), pointer :: ges_z (:,:  )=>NULL()  ! geopotential height
  real(r_kind), pointer :: ges_ps(:,:  )=>NULL()  ! surface pressure
  real(r_kind), pointer :: ges_tv(:,:,:)=>NULL()  ! virtual temperature
  real(r_kind), pointer :: ges_q (:,:,:)=>NULL()  ! specifici humidity

  real(r_kind), pointer :: ges_ql(:,:,:)=>NULL()  ! cloud water
  real(r_kind), pointer :: ges_qi(:,:,:)=>NULL()  ! could ice
  real(r_kind), pointer :: ges_qr(:,:,:)=>NULL()  ! rain
  real(r_kind), pointer :: ges_qs(:,:,:)=>NULL()  ! snow
  real(r_kind), pointer :: ges_qg(:,:,:)=>NULL()  ! graupel
  real(r_kind), pointer :: ges_qnr(:,:,:)=>NULL() ! rain number concentration
  real(r_kind), pointer,dimension(:,:,:)::ges_cwmr=>NULL()
!
!  misc.
!
  logical :: l_use_hydroretrieval_all
  integer(i_kind) :: i,j,k,itsig,itsfc
  integer(i_kind) :: iglobal,jglobal,ilocal,jlocal
  logical :: ifindomain
  integer(i_kind) :: imaxlvl_ref
  real(r_kind)    :: max_retrieved_qrqs,max_bk_qrqs,ratio_hyd_bk2obs
  real(r_kind)    :: qrlimit,qrlimit_lightpcp
  character(10)   :: obstype
  integer(i_kind) :: lunin, is, ier, istatus
  integer(i_kind) :: nreal,nchanl,ilat1s,ilon1s
  character(20)   :: isis

  real(r_kind)    :: refmax,snowtemp,raintemp,nraintemp,graupeltemp
  real(r_kind)    :: snowadd,ratio2
  integer(i_kind) :: imax, jmax, ista, iob, job
  real(r_kind)    :: dfi_lhtp, qmixr, tsfc
  integer(i_kind) :: ntsig,ntsfc
  integer(i_kind) :: ii,jj

!
!
  if(mype==0) then
     write(6,*) '========================================'
     write(6,*) 'gsdcloudanalysis4gfs: Start generalized cloud analysis:'
     write(6,*) '========================================'
  endif
!
  if(i_gsdcldanal_type==30) then
     ntsig = ntguessig
     ntsfc = ntguessfc

     ier=0
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntsig),'ps',ges_ps,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntsig),'z' ,ges_z ,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntsig),'tv',ges_tv,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntsig),'q', ges_q ,istatus);ier=ier+istatus

     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(ntsig),'cw', ges_cwmr,istatus);ier=ier+istatus
!     sfct(:,:,ntsig)=ges_tsen(:,:,1,ntsig)
     soil_temp(:,:,ntsig)=ges_tsen(:,:,1,ntsig)
     ges_ql=>ges_cwmr
  else
     itsig=1 ! _RT shouldn't this be ntguessig?
     itsfc=1 ! _RT shouldn't this be ntguessig?

     ier=0
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsfc),'ps',ges_ps,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsfc),'z' ,ges_z ,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'tv',ges_tv,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'q', ges_q ,istatus);ier=ier+istatus

     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'ql',ges_ql,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qi',ges_qi,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qr',ges_qr,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qs',ges_qs,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qg',ges_qg,istatus);ier=ier+istatus
     call gsi_bundlegetpointer (GSI_MetGuess_Bundle(itsig),'qnr',ges_qnr,istatus);ier=ier+istatus
  endif
  if(ier/=0) return ! no guess, nothing to do

!mhu  if(mype<10) ges_cwmr=ges_cwmr+0.001
!
!  if(mype==10) then
!     write(*,*) lat2,lon2,nsig
!     write(*,*) 'ps=',maxval(ges_ps(:,:)),minval(ges_ps(:,:))
!     do k=1,nsig
!        write(*,*) 't=',k,maxval(ges_tv(:,:,k)),minval(ges_tv(:,:,k))
!     enddo
!     do k=1,nsig
!        write(*,*) 'q=',k,maxval(ges_q(:,:,k)),minval(ges_q(:,:,k))
!     enddo
!     do k=1,nsig
!        write(*,*) 'cw=',k,maxval(ges_cwmr(:,:,k)),minval(ges_cwmr(:,:,k))
!     enddo
!     write(*,*) 'z=',maxval(ges_z(:,:)),minval(ges_z(:,:))
!     do k=1,nsig
!        write(*,*) 'tsen=',k,maxval(ges_tsen(:,:,k,ntsig)),minval(ges_tsen(:,:,k,ntsig))
!     enddo
!     do k=1,nsig
!        write(*,*) 'prsl=',k,maxval(ges_prsl(:,:,k,ntsig)),minval(ges_prsl(:,:,k,ntsig))
!     enddo
!     write(*,*) 'sfct=',k,maxval(sfct(:,:,ntsig)),minval(sfct(:,:,ntsig))
!     write(*,*) 'isli=',mype,maxval(isli2(:,:)),minval(isli2(:,:))
!     write(*,*) 'soilT=',k,maxval(soil_temp(:,:,ntsig)),minval(soil_temp(:,:,ntsig))
!     write(*,*) rlats
!     write(*,*) rlons
!  endif
!
!
  l_use_hydroretrieval_all=.false.
  krad_bot=7.0_r_single
  r_radius=metar_impact_radius
  r_radius_lowCloud=metar_impact_radius_lowCloud

  opt_hydrometeor_retri=3       ! 1=Kessler 2=Lin 3=Thompson
  opt_cloudtemperature=3        ! 3=latent heat, 4,5,6 = adiabat profile
  opt_cloudwaterice_retri=1     ! 1 = RUC layer saturation and autoconvert
                                ! 2 = convective 
  imerge_nesdis_nasalarc=2      !  =1 merge NASA LaRC with NESDIS
                                !  =2 use NASA LaRC only
                                !  =3 No Satellite cloud top used
                                !  = other, use NESDIS only

!
! initialize the observation flag  
!
  istat_surface=0
  istat_nesdis=0
  istat_radar=0
  istat_lightning=0
  istat_nasalarc=0

!mhu need debug more:  call load_gsdpbl_hgt(mype)
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
  lightning=-9999.0_r_kind

  allocate(sat_ctp(lon2,lat2))
  allocate(sat_tem(lon2,lat2))
  allocate(w_frac(lon2,lat2))
  allocate(nlev_cld(lon2,lat2))
  sat_ctp=miss_obs_real
  sat_tem=miss_obs_real
  w_frac=miss_obs_real
  nlev_cld=miss_obs_int

  allocate(osfc_station_map(lon2,lat2))
  osfc_station_map=miss_obs_int
!
! 1.2 start to read observations                 
!
  nmsclvl_radar = -999
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
           allocate(oi(numsao))
           allocate(oj(numsao))
           allocate(ocld(nvarcld_p,numsao))
           allocate(owx(numsao))
           allocate(oelvtn(numsao))
           allocate(odist(numsao))
           allocate(cstation(numsao))
           allocate(oistation(numsao))
           allocate(ojstation(numsao))
           allocate(wimaxstation(numsao))
           call read_Surface(mype,lunin,regional_time,istart(mype+1),jstart(mype+1),lon2,lat2, &
                             numsao,nvarcld_p,oi,oj,ocld,owx,oelvtn,odist,cstation,oistation,ojstation)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                                  &
                        'Surface cloud observations are read in successfully'
           istat_surface=1

!
!  1.2.4 read in NESDIS cloud products
!
        elseif( dtype(is) == 'gos_ctp' ) then 

           call read_NESDIS(mype,lunin,nsat1(is),regional_time,istart(mype+1),            &
                            jstart(mype+1),lon2,lat2,sat_ctp,sat_tem,w_frac,nesdis_npts_rad,ioption)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                             &
                         'NESDIS cloud products are read in successfully'
           istat_nesdis = 1 

!
!  1.2.6 read in reflectivity mosaic
!
        elseif( dtype(is) == 'rad_ref' ) then

           allocate( ref_mosaic31(lon2,lat2,31) )
           ref_mosaic31=-99999.0_r_kind

           call read_radar_ref(mype,lunin,regional_time,istart(mype+1),jstart(mype+1), &
                              lon2,lat2,nmsclvl_radar,nsat1(is),ref_mosaic31)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: ',                         &
                         ' radar reflectivity is read in successfully'
           istat_radar=1

!
!  1.2.8 read in lightning
!
        elseif( dtype(is)=='lghtn' ) then

           call read_Lightning2cld(mype,lunin,regional_time,istart(mype+1),jstart(mype+1), &
                                   lon2,lat2,nsat1(is),lightning)
           if(mype == 0) write(6,*) 'gsdcloudanalysis: Lightning is read in successfully'
           istat_lightning = 1 

!
!  1.2.9 read in NASA LaRC cloud products
!
        elseif( dtype(is) =='larcglb' ) then

           allocate(nasalarc_cld(lon2,lat2,5))
           nasalarc_cld=miss_obs_real

           call read_map_nasalarc(mype,lunin,nsat1(is),regional_time,istart(mype+1),   &
                              jstart(mype+1),lon2,lat2,nasalarc_cld,ioption)
           if(mype == 0) write(6,*) 'gsdcloudanalysis:',                       &
                         'NASA LaRC cloud products are read in successfully', &
                         mype
           istat_nasalarc = 1

!
!  1.2.12  all other observations 
!
        else
           read(lunin)  obstype,isis,nreal,nchanl
           read(lunin)
        endif   ! dtype
     endif
  enddo   ! is
  close(lunin)
!
!  1.4  if there are NASA LaRC cloud products, use them to replace NESDIS ones.
!       So we use NASA LaRC data in the same way as NESDIS ones
!
  if(imerge_nesdis_nasalarc == 1 ) then
     if(istat_nasalarc == 1 ) then
        do j=2,lat2-1
           do i=2,lon2-1
             if(sat_ctp(i,j) < -99990.0_r_kind) then   ! missing value is -999999.0
                sat_ctp(i,j) = nasalarc_cld(i,j,1)
                sat_tem(i,j) = nasalarc_cld(i,j,2)
                w_frac(i,j)  = nasalarc_cld(i,j,3)
                nlev_cld(i,j)= int(nasalarc_cld(i,j,5))
                istat_nesdis =istat_nasalarc
             endif
           enddo
        enddo
     endif
  elseif ( imerge_nesdis_nasalarc == 2) then
     if(istat_nasalarc == 1 ) then
       sat_ctp(:,:) = nasalarc_cld(:,:,1)
       sat_tem(:,:) = nasalarc_cld(:,:,2)
       w_frac(:,:)  = nasalarc_cld(:,:,3)
       nlev_cld(:,:)= int(nasalarc_cld(:,:,5))
       istat_nesdis =istat_nasalarc
     endif
  elseif ( imerge_nesdis_nasalarc == 3) then
       istat_nesdis = 0
  endif
 
!
!
!  1.6 collect the cloud information from whole domain
!       and assign the background cloud to each METAR obs
!
  allocate(sumqci(lon2,lat2,nsig))
  if(i_gsdcldanal_type==30) then
     do k=1,nsig
        do j=1,lat2
           do i=1,lon2
              sumqci(i,j,k)= ges_ql(j,i,k)
           enddo
        enddo
     enddo
  else
     do k=1,nsig
        do j=1,lat2
           do i=1,lon2
              sumqci(i,j,k)= ges_ql(j,i,k) + ges_qi(j,i,k)
           enddo
        enddo
     enddo
  endif

  allocate(watericemax(lon2,lat2))
  allocate(kwatericemax(lon2,lat2))
  watericemax=0._r_kind
  wimaxstation=0.0_r_single
  kwatericemax=-1
  do j=1,lat2
     do i=1,lon2
       do k = 1,nsig
          watericemax(i,j) = max(watericemax(i,j),sumqci(i,j,k))
       end do
       do k=1,nsig
          if (sumqci(i,j,k) > cloud_def_p .and. kwatericemax(i,j) == -1) then
             kwatericemax(i,j) = k
          end if
       end do
     enddo
  enddo
!
  im=nlon
  jm=nlat
  allocate(all_loc(lat2,lon2))
  allocate(strp(lat1*lon1))
  allocate(tempa(itotsub))
  allocate(temp1(im,jm))
  do j=1,lat2
     do i=1,lon2
        all_loc(j,i) = watericemax(i,j)
     enddo
  enddo
  call strip(all_loc,strp)
  call mpi_allgatherv(strp,ijn(mype+1),mpi_real4, &
            tempa,ijn,displs_g,mpi_real4,mpi_comm_world,ierror)
  ierror=0
  if(ierror /= 0 ) then
     write(*,*) 'MPI error: cloud analysis=',mype
  endif
  temp1=0.0_r_single
  call unfill_mass_grid2t(tempa,im,jm,temp1)

  if(istat_surface==1) then
     do ista=1,numsao
        iob = min(max(int(oistation(ista)+0.5_r_kind),1),im)
        job = min(max(int(ojstation(ista)+0.5_r_kind),1),jm)
        wimaxstation(ista)=temp1(iob,job)
        if(wimaxstation(ista) > 0._r_single) then
            i=int(oi(ista))
            j=int(oj(ista))
        endif
     enddo
  endif
  deallocate(all_loc,strp,tempa,temp1)

! make a surface station map in grid coordinate
  if(istat_surface==1) then
     do ista=1,numsao
        iob = int(oistation(ista)-jstart(mype+1)+2)
        job = int(ojstation(ista)-istart(mype+1)+2)
        if(iob >=1 .and. iob<=lon2-1 .and. job >=1 .and. job<=lat2-1) then
           osfc_station_map(iob,job)=1
           osfc_station_map(iob+1,job)=1
           osfc_station_map(iob,job+1)=1
           osfc_station_map(iob+1,job+1)=1
        endif
     enddo
  endif

!
!  1.8 check if data available: if no data in this subdomain, return. 
!
!mhu  if( (istat_radar + istat_surface + istat_nesdis + istat_lightning ) == 0 ) then
  if( (istat_nesdis + istat_surface) == 0 ) then
!mhu     ges_cwmr(:,:,nsig)=1003.0
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
  
  allocate(xlon(lon2,lat2))
  allocate(xlat(lon2,lat2))
  allocate(xland(lon2,lat2))
  allocate(soiltbk(lon2,lat2))
!  allocate(z_lcl(lon2,lat2))    
!  allocate(pblh(lon2,lat2))
  t_bk=miss_obs_real
  h_bk=miss_obs_real
  p_bk=miss_obs_real
  ps_bk=miss_obs_real
  zh=miss_obs_real
  q_bk=miss_obs_real
  xlon=miss_obs_real
  xlat=miss_obs_real
  xland=miss_obs_real
  soiltbk=miss_obs_real
!  pblh=miss_obs_real
  
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
  allocate(rain_1d_save(nsig))
  allocate(nrain_1d_save(nsig))
  allocate(snow_1d_save(nsig))
  rain_1d_save=miss_obs_real
  nrain_1d_save=miss_obs_real
  snow_1d_save=miss_obs_real
!          
! 2.4 read in background fields
!          
  if(i_gsdcldanal_type==30) then
!
     do j=1,lat2
        do i=1,lon2
           ii = i + jstart(mype+1) - 2  ! covert it to the global grid   
           jj = j + istart(mype+1) - 2  ! covert it to the global grid
           ii=max(1,min(ii,nlon))
           jj=max(1,min(jj,nlat))
           xlon(i,j) = rlons(ii)*rad2deg    !  longitude back to degree
           xlat(i,j) = rlats(jj)*rad2deg    !  latitude  back to degree
        enddo
     enddo
     do j=1,lat2
        do i=1,lon2
           zh(i,j)     =ges_z(j,i)                 !  terrain in meter
           ps_bk(i,j)  =ges_ps(j,i)*10.0_r_single  !  surace pressure in mb
           xland(i,j)  =isli2(j,i)                  !  0=water, 1=land, 2=ice
           soiltbk(i,j)=soil_temp(j,i,ntsig)       !  soil temperature
        enddo
     enddo
     do k=1,nsig
        do j=1,lat2
           do i=1,lon2
              p_bk(i,j,k)=ges_prsl(j,i,k,ntsig)*10.0_r_single
           enddo
        enddo
     enddo
  else
     do j=1,lat2                
        do i=1,lon2
           zh(i,j)     =ges_z(j,i)                     !  terrain in meter
           ps_bk(i,j)  =ges_ps(j,i)*10.0_r_single      !  surace pressure in mb
           xland(i,j)  =isli_cld(j,i,itsfc)            !  0=water, 1=land, 2=ice
           soiltbk(i,j)=soil_temp_cld(j,i,itsfc)       !  soil temperature
           xlon(i,j)   =ges_xlon(j,i,itsfc)*rad2deg    !  longitude back to degree
           xlat(i,j)   =ges_xlat(j,i,itsfc)*rad2deg    !  latitude  back to degree
        enddo
     enddo
  endif

  do k=1,nsig
     do j=1,lat2
        do i=1,lon2
           q_bk(i,j,k)=ges_q(j,i,k)                    ! specific humidity
           qmixr = q_bk(i,j,k)/(one - q_bk(i,j,k))     ! covert from specific
                                                       ! humidity to mixing ratio
           t_bk(i,j,k)=ges_tsen(j,i,k,ntsig)
        enddo
     enddo
  enddo

  if(i_gsdcldanal_type==30) then
     call BackgroundCldgfs(mype,lon2,lat2,nsig,t_bk,p_bk,ps_bk,q_bk,h_bk)
  else
     call BackgroundCld(mype,lon2,lat2,nsig,t_bk,p_bk,ps_bk,q_bk,h_bk,    &
             zh,pt_ll,eta1_ll,aeta1_ll,regional,wrf_mass_regional)
  endif


  !if(mype==25) then
  !   write(*,*) lat2,lon2,nsig
  !   write(*,*) 'ps=',maxval(ps_bk),minval(ps_bk)
  !   write(*,*) 'zh=',maxval(zh),minval(zh)
  !   write(*,*) 'xland=',maxval(xland),minval(xland)
  !   write(*,*) 'soiltbk=',maxval(soiltbk),minval(soiltbk)
  !   write(*,*) 'xlon=',maxval(xlon),minval(xlon)
  !   write(*,*) 'xlat=',maxval(xlat),minval(xlat)
  !   do k=1,nsig
  !      write(*,*) 't=',k,maxval(t_bk(:,:,k)),minval(t_bk(:,:,k))
  !   enddo
  !   do k=1,nsig
  !      write(*,*) 'q=',k,maxval(q_bk(:,:,k)),minval(q_bk(:,:,k))
  !   enddo
  !   do k=1,nsig
  !      write(*,*) 'cw=',k,maxval(ges_cwmr(:,:,k)),minval(ges_cwmr(:,:,k))
  !   enddo
  !   do k=1,nsig
  !      write(*,*) 'p_bk=',k,maxval(p_bk(:,:,k)),minval(p_bk(:,:,k))
  !   enddo
  !   do k=1,nsig
  !      write(*,*) 'h_bk=',k,maxval(h_bk(:,:,k)),minval(h_bk(:,:,k))
  !   enddo
  !endif

! 
! 2.5 calculate PBL height
! 
!mhu  call calc_pbl_height(mype,lat2,lon2,nsig,q_bk,t_bk,p_bk,pblh)

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
  allocate(vis2qc(lon2,lat2))
  vis2qc=miss_obs_real
!
!
  if(istat_surface ==  1) then
     call cloudCover_surface(mype,lat2,lon2,nsig,r_radius,thunderRadius,  &
              t_bk,p_bk,q_bk,h_bk,zh,                                     &
              numsao,nvarcld_p,numsao,oi,oj,ocld,owx,oelvtn,odist,        &
              cld_cover_3d,cld_type_3d,wthr_type_2d,pcp_type_3d,          &
              wimaxstation, kwatericemax,vis2qc)
     if(mype == 0) write(6,*) 'gsdcloudanalysis:',                        &  
                   'success in cloud cover analysis using surface data'
  endif

  if(istat_nesdis == 1 ) then
     call cloudCover_NESDIS(mype,regional_time,lat2,lon2,nsig,            &
                         xlon,xlat,t_bk,p_bk,h_bk,xland,                  &
                         soiltbk,sat_ctp,sat_tem,w_frac,                  &
                         l_cld_bld,cld_bld_hgt,                           &
                         build_cloud_frac_p,clear_cloud_frac_p,nlev_cld,  &
                         cld_cover_3d,cld_type_3d,wthr_type_2d,osfc_station_map)
     if(mype == 0) write(6,*) 'gsdcloudanalysis:',                        & 
                   ' success in cloud cover analysis using NESDIS data'
  endif

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

if(istat_nesdis == 1 ) then
  call cloudLayers(lat2,lon2,nsig,h_bk,zh,cld_cover_3d,               &
                   cld_type_3d,cloudlayers_i)
  if(mype==0) write(6,*) 'gsdcloudanalysis: success in finding cloud layers'
endif
!
! 4.4 decide the cloud type
!
!
! 4.6 calculate liquid water content
!
if(istat_nesdis == 1 ) then
     call cloudLWC_stratiform(mype,lat2,lon2,nsig,q_bk,t_bk,p_bk,      &
                  cld_cover_3d,cld_type_3d,wthr_type_2d,cloudlayers_i, &
                  cldwater_3d,cldice_3d)
     if(mype==0) write(6,*) 'gsdcloudanalysis: ',                      &
                 'success in modifying hydrometeors for stratiform clouds '

endif
!
!----------------------------------------------
! 5.  the final analysis or update background
!----------------------------------------------
!
!  the final analysis of cloud 
!

  if(i_gsdcldanal_type==30 ) then
    !if(istat_nasalarc == 1) then
    if((istat_nasalarc + istat_nesdis + istat_surface) > 1) then
     do k=1,nsig
        do j=2,lat2-1
           do i=2,lon2-1
              cldwater_3d(i,j,k)=cldwater_3d(i,j,k)+cldice_3d(i,j,k)
              if(cldwater_3d(i,j,k) < 0.0_r_kind) cldwater_3d(i,j,k)=zero
              if( cld_cover_3d(i,j,k) > -0.001_r_kind ) then
                 if( cld_cover_3d(i,j,k) > 0.6_r_kind ) then
                    cldwater_3d(i,j,k) = max(0.001_r_kind*cldwater_3d(i,j,k), &
                                             ges_cwmr(j,i,k))
                 else   ! clean  cloud
                    cldwater_3d(i,j,k) = zero
                 endif
              else   ! unknown, using background values
                 cldwater_3d(i,j,k) = ges_cwmr(j,i,k)
              endif
              cldwater_3d(i,j,k)= max(0.0_r_single,cldwater_3d(i,j,k))
           end do
        end do
     end do


!  call cloud_saturation(mype,l_conserve_thetaV,i_conserve_thetaV_iternum,  &
!                 lat2,lon2,nsig,q_bk,t_bk,p_bk,      &
!                 cld_cover_3d,wthr_type_2d,cldwater_3d,cldice_3d,sumqci)

!----------------------------------------------
! 6.  save the analysis results
!----------------------------------------------
!
!

!        do j=2,lat2-1
!           do i=2,lon2-1
!           write(*,*) mype,i,j,sat_ctp(i,j)
!       enddo
!       enddo

  !do k=1,nsig
  !   write(*,'(a,2I5,2e15.5)') 'update the ges_cwmr for',mype,k, &
  !          maxval(ges_cwmr(:,:,k)),minval(ges_cwmr(:,:,k))
  !enddo
  do k=1,nsig-1
     do j=2,lat2-1
        do i=2,lon2-1
           ges_cwmr(j,i,k)=cldwater_3d(i,j,k) 
            if(ges_cwmr(j,i,k)<0.0_r_kind) ges_cwmr(j,i,k)=0.0_r_kind
        enddo
     enddo
  enddo
!
!     do j=1,lat2
!        write(400+mype,'(2I10,40f8.1)') mype,j,(sat_ctp(i,j),i=1,lon2)
!     enddo
!!
!     ges_cwmr(:,:,nsig)=1003.0
!     do j=2,lat2-1
!!        do i=2,lon2-1
!             if(sat_ctp(i,j) < 0.0 .or. sat_ctp(i,j) > 1015.0 ) sat_ctp(i,j)=1015.0
!            ges_cwmr(j,i,k)=sat_ctp(i,j)    
!        enddo
!     enddo
!
     !do k=1,nsig
     !   write(*,'(a,2I5,2e15.5)') 'after update the ges_cwmr for',mype,k,&
     !       maxval(ges_cwmr(:,:,k)),minval(ges_cwmr(:,:,k))
     !enddo

    endif
  else  !regional case 

  endif !   regional or global 
!
!----------------------------------------------
! 7.  release space
!----------------------------------------------
!
  deallocate(cld_cover_3d,cld_type_3d,wthr_type_2d, &
             pcp_type_3d,cloudlayers_i)
  deallocate(t_bk,h_bk,p_bk,ps_bk,zh,q_bk,sumqci)
  deallocate(xlon,xlat,xland,soiltbk)
  deallocate(cldwater_3d,cldice_3d,rain_3d,nrain_3d,snow_3d,graupel_3d,cldtmp_3d)
  deallocate(vis2qc)

  if(istat_surface ==  1 ) then
     deallocate(oi,oj,ocld,owx,oelvtn,odist,cstation,oistation,ojstation,wimaxstation)
     deallocate(watericemax,kwatericemax) 
  endif
  if(istat_nasalarc == 1 ) then
     deallocate(nasalarc_cld)
  endif

  deallocate(sat_ctp,sat_tem,w_frac,nlev_cld)
  deallocate(ref_mos_3d,ref_mos_3d_tten,lightning)

  if(mype==3) then
     write(6,*) '========================================'
     write(6,*) 'gsdcloudanalysis: generalized cloud analysis finished:',mype
     write(6,*) '========================================'
  endif

#else /* Start no RR cloud analysis library block */
  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: mype
!

  if( mype == 0) write(6,*)'gsdcloudanalysis:  dummy routine, does nothing!'

#endif /* End no RR cloud analysis library block */
end subroutine gsdcloudanalysis4gfs
