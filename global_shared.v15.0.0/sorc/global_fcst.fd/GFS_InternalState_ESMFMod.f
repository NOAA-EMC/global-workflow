!
! !MODULE: GFS_InternalState_ESMFMod --- Internal state definition of the
!                                        ESMF gridded component of the GFS system.
!
! !DESCRIPTION: GFS_InternalState_ESMFMod --- Define the GFS internal state used to
!                                             create the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  November 2004      Weiyu Yang Initial code.
!  May 2005           Weiyu Yang for the updated GFS version.
!  February 2006      Shrinivas Moorthi updated for the new version of GFS
!  September2006      Weiyu Yang For the ensemble run couple version.
!  April    2009      Shrinivas Moorthi merge GEFS and generalized GFS versions
!
! !INTERFACE:
!
 MODULE GFS_InternalState_ESMFMod

!!USES:
!------
 USE ESMF_Mod
!USE nam_mrf_NAMSFC_NameList_ESMFMod
 USE NameList_ESMFMod

 USE MACHINE, ONLY: kind_rad, kind_phys, kind_io4, kind_evod
!USE resol_def     ! Wei yu's version did not have this why?
 USE layout1
 USE gg_def
 USE vert_def
 USE sig_io
 USE date_def
 USE namelist_def
 USE namelist_soilveg
 USE mpi_def
!!!!!! USE semi_lag_def
 USE coordinate_def                                      ! hmhj
 USE tracer_const                                        ! hmhj
 use module_ras , only : nrcmax
 use ozne_def
 use d3d_def
 use Sfc_Flx_ESMFMod
 use Nst_Var_ESMFMod

 IMPLICIT none

 TYPE GFS_InternalState

!     TYPE(nam_gfs_Namelist)    :: nam_gfs
!     TYPE(SOIL_VEG_NameList)   :: SOIL_VEG
!     TYPE(NAMSFC_NameList)     :: NAMSFC
      TYPE(nam_gfs_NameList)    :: nam_gfs
      TYPE(ESMF_State_Namelist) :: ESMF_Sta_List

      TYPE(Sfc_Var_Data)        :: sfc_fld
      TYPE(Flx_Var_Data)        :: flx_fld

      TYPE(Nst_Var_Data)        :: nst_fld

      INTEGER                   :: me, mm1, nodes
      INTEGER                   :: lonr_s, latr_s, lnt2_s
      INTEGER                   :: lnt2, grid4_i1(2)
      integer                   :: grib_inp
      INTEGER                   :: npe_single_member

      REAL(KIND = kind_io4), DIMENSION(:, :), POINTER ::            &
        z_im, ps_im, vor_im, div_im, temp_im, q_im, oz_im, scld_im, &
        z_ex, ps_ex, vor_ex, div_ex, temp_ex, q_ex, oz_ex, scld_ex

      REAL(KIND = kind_evod), DIMENSION(:, :), POINTER :: trieo_ls_im
      REAL(KIND = kind_evod), DIMENSION(:, :), POINTER :: write_work8
      REAL(KIND = kind_evod), DIMENSION(:, :), POINTER :: write_work8_ini

!
! idate1_im and idate1_ex:  (1) --- bfhour (integer), (2) - (5) --- idate.
!-------------------------------------------------------------------------
!
      INTEGER,               DIMENSION(:, :),    POINTER :: idate1_im, idate1_ex

      REAL(KIND = kind_io4), DIMENSION(:, :),    POINTER ::                               &
        orography_im,             t_skin_im,                  snow_depth_im,              &
        deep_soil_t_im,           roughness_im,               conv_cloud_cover_im,        &
        conv_cloud_base_im,       conv_cloud_top_im,          albedo_visible_scattered_im,&
        albedo_visible_beam_im,   albedo_nearIR_scattered_im, albedo_nearIR_beam_im,      &
        sea_level_ice_mask_im,    vegetation_cover_im,        canopy_water_im,            &
        m10_wind_fraction_im,     vegetation_type_im,         soil_type_im,               &
        zeneith_angle_facsf_im,   zeneith_angle_facwf_im,     uustar_im,                  &
        ffmm_im,                  ffhh_im,                    sea_ice_thickness_im,       &
        sea_ice_concentration_im, tprcp_im,                   srflag_im,                  &
        actual_snow_depth_im,     vegetation_cover_min_im,    vegetation_cover_max_im,    &
        slope_type_im,            snow_albedo_max_im,                                     &  
 
! MOdified by Weiyu (DHOU, 04/04/2008).
!-------------------
        soil_t_im1, soil_t_im2, soil_t_im3, soil_mois_im1, soil_mois_im2, soil_mois_im3,  &
        liquid_soil_moisture_im1, liquid_soil_moisture_im2, liquid_soil_moisture_im3,     &

        orography_ex,             t_skin_ex,                  snow_depth_ex,              &
        deep_soil_t_ex,           roughness_ex,               conv_cloud_cover_ex,        &
        conv_cloud_base_ex,       conv_cloud_top_ex,          albedo_visible_scattered_ex,&
        albedo_visible_beam_ex,   albedo_nearIR_scattered_ex, albedo_nearIR_beam_ex,      &
        sea_level_ice_mask_ex,    vegetation_cover_ex,        canopy_water_ex,            &
        m10_wind_fraction_ex,     vegetation_type_ex,         soil_type_ex,               &
        zeneith_angle_facsf_ex,   zeneith_angle_facwf_ex,     uustar_ex,                  &
        ffmm_ex,                  ffhh_ex,                    sea_ice_thickness_ex,       &
        sea_ice_concentration_ex, tprcp_ex,                   srflag_ex,                  &
        actual_snow_depth_ex,     vegetation_cover_min_ex,    vegetation_cover_max_ex,    &
        slope_type_ex,            snow_albedo_max_ex

      REAL(KIND = kind_io4), DIMENSION(:, :, :), POINTER ::                               &
        soil_mois_im,            soil_t_im,      soil_mois_ex, soil_t_ex,                 &
        liquid_soil_moisture_im, liquid_soil_moisture_ex

! To represent the trie_ls and trio_ls in the ESMF states, add levh. Weiyu.
!--------------------------------------------------------------------------

!     INTEGER ntrac,nxpt,nypt,jintmx,jcap,levs,levh,lonf,lonr,latg,latr
      INTEGER ntrac,jcap,levs,levh,lonf,lonr,latg,latr
      INTEGER ntoz, ntcw, ncld, lsoil, nmtvr,levr
      INTEGER num_p3d,num_p2d, num_a3d,num_a2d                         

      CHARACTER(16)                     ::  CFHOUR1

      INTEGER                           ::  KDT
      REAL                              ::  DELTIM

! For creating the ESMF interface state with the GFS
! internal parallel structure.   Weiyu.
!---------------------------------------------------
      INTEGER                               :: TRIEO_TOTAL_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: TRIE_LS_SIZE,    TRIO_LS_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: TRIEO_LS_SIZE
      INTEGER, ALLOCATABLE, DIMENSION(:)    :: LS_MAX_NODE_GLOBAL
      INTEGER, ALLOCATABLE, DIMENSION(:, :) :: LS_NODE_GLOBAL

! For flexible choose the time interval of the tendency time difference.
!-----------------------------------------------------------------------
      INTEGER                               :: advanceCount_SetUp

      CHARACTER(ESMF_MAXSTR) :: TRIEO_STATE_NAME
      CHARACTER(ESMF_MAXSTR) :: TRIEO_STINI_NAME

      INTEGER              ,ALLOCATABLE ::      LONSPERLAT (:)
      INTEGER              ,ALLOCATABLE ::      lonsperlar (:)
      INTEGER              ,ALLOCATABLE ::      LS_NODE    (:)
      INTEGER              ,ALLOCATABLE ::      LS_NODES   (:, :)
      INTEGER              ,ALLOCATABLE ::  MAX_LS_NODES   (:)

      INTEGER              ,ALLOCATABLE ::  LATS_NODES_A   (:)
      INTEGER              ,ALLOCATABLE ::  GLOBAL_LATS_A  (:)
!     INTEGER              ,ALLOCATABLE ::  LATS_NODES_EXT (:)
!     INTEGER              ,ALLOCATABLE ::  GLOBAL_LATS_EXT(:)

      INTEGER              ,ALLOCATABLE ::  LATS_NODES_R   (:)
      INTEGER              ,ALLOCATABLE ::  GLOBAL_LATS_R  (:)

      real(kind=kind_phys) ,allocatable ::  fscav(:)

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::        EPSE  (:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::        EPSO  (:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::        EPSEDN(:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::        EPSODN(:)

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       SNNP1EV(:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       SNNP1OD(:)

      INTEGER              ,ALLOCATABLE ::        NDEXEV(:)
      INTEGER              ,ALLOCATABLE ::        NDEXOD(:)

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNEV_A(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNOD_A(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PDDEV_A(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PDDOD_A(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNEW_A(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNOW_A(:,:)

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNEV_R(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNOD_R(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PDDEV_R(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PDDOD_R(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNEW_R(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       PLNOW_R(:,:)

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       TRIE_LS(:,:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::       TRIO_LS(:,:,:)

! For Ensemble forecast requirement, add two more arrays to save the
! initial conditions.   Weiyu.
!------------------------------------------------------------------
      REAL(KIND=KIND_EVOD) ,    POINTER ::       TRIE_LS_INI(:,:,:)
      REAL(KIND=KIND_EVOD) ,    POINTER ::       TRIO_LS_INI(:,:,:)


      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::    SYN_GR_A_1(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::    SYN_GR_A_2(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::    DYN_GR_A_1(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::    DYN_GR_A_2(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::    ANL_GR_A_1(:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE ::    ANL_GR_A_2(:,:)

      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: XLON(:,:),XLAT(:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: COSZDG(:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: sfalb(:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: HPRIME(:,:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: SWH(:,:,:),HLW(:,:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: FLUXR(:,:,:)
!!

!     REAL(KIND=KIND_RAD) ,ALLOCATABLE :: phy_f3d(:,:,:,:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: phy_f3d(:,:,:,:)
      REAL(KIND=KIND_RAD) ,ALLOCATABLE :: phy_f2d(:,:,:)

      REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: dyn_f3d(:,:,:,:)
      REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: dyn_f2d(:,:,:)

!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: LBASIY(:,:,:)
!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: PHI(:)
!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: DPHI(:)
!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: DLAM(:),LAMEXT(:,:),LAM(:,:)
!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: LAMMP(:,:,:)
!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: PHIMP(:,:,:)
!JFE  REAL(KIND=KIND_EVOD) ,ALLOCATABLE :: SIGMP(:,:,:)
!!
! FOR  OZONE PRODUCTION AND DISTRUCTION RATES:(INPUT THROU FIXIO_R)
      INTEGER LEV,LEVMAX
!!$$$      PARAMETER (LATS18=18, LEV46=46)
!!$$$      REAL POZ(LEV46),PHOUR
!!$$$      REAL OZPRDIN(LATS18,LEV46,36) !OZON PRODUCTION RATE
!!$$$      REAL OZDISIN(LATS18,LEV46,36) !OZON DISTUCTION RATE
      real phour
      INTEGER :: KFHOUR
      real, allocatable  :: poz(:),ozplin(:,:,:,:)
!     FOR OZONE INTERPOLATION:
      INTEGER,ALLOCATABLE:: JINDX1(:),JINDX2(:)
!
      REAL (KIND=KIND_PHYS) PDRYINI
      REAL,ALLOCATABLE:: DDY(:)
      REAL(KIND=KIND_EVOD) SLAG,SDEC,CDEC
!!
!JFE  INTEGER,ALLOCATABLE :: LATSINPE(:)
!JFE  INTEGER,ALLOCATABLE :: LATLOCAL(:,:)

      INTEGER              INIT,JCOUNT,JPT,NODE
      INTEGER              IBMSIGN
      INTEGER              LON_DIM,ILAT

      real(kind=kind_evod) colat1
!!
      REAL(KIND=KIND_EVOD) RONE
      REAL(KIND=KIND_EVOD) RLONS_LAT
      REAL(KIND=KIND_EVOD) SCALE_IBM

      INTEGER   P_GZ,P_ZEM,P_DIM,P_TEM,P_RM,P_QM
      INTEGER   P_ZE,P_DI,P_TE,P_RQ,P_Q,P_DLAM,P_DPHI,P_ULN,P_VLN
      INTEGER   P_W,P_X,P_Y,P_RT,P_ZQ
!C                                            OLD COMMON /COMFSPEC/
!!$$$      PARAMETER(P_GZ  = 0*LEVS+0*LEVH+1,  !      GZE/O(LNTE/OD,2),
!!$$$     X          P_ZEM = 0*LEVS+0*LEVH+2,  !     ZEME/O(LNTE/OD,2,LEVS),
!!$$$     X          P_DIM = 1*LEVS+0*LEVH+2,  !     DIME/O(LNTE/OD,2,LEVS),
!!$$$     X          P_TEM = 2*LEVS+0*LEVH+2,  !     TEME/O(LNTE/OD,2,LEVS),
!!$$$     X          P_RM  = 3*LEVS+0*LEVH+2,  !      RME/O(LNTE/OD,2,LEVH),
!!$$$     X          P_QM  = 3*LEVS+1*LEVH+2,  !      QME/O(LNTE/OD,2),
!!$$$     X          P_ZE  = 3*LEVS+1*LEVH+3,  !      ZEE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_DI  = 4*LEVS+1*LEVH+3,  !      DIE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_TE  = 5*LEVS+1*LEVH+3,  !      TEE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_RQ  = 6*LEVS+1*LEVH+3,  !      RQE/O(LNTE/OD,2,LEVH),
!!$$$     X          P_Q   = 6*LEVS+2*LEVH+3,  !       QE/O(LNTE/OD,2),
!!$$$     X          P_DLAM= 6*LEVS+2*LEVH+4,  !  DPDLAME/O(LNTE/OD,2),
!!$$$     X          P_DPHI= 6*LEVS+2*LEVH+5,  !  DPDPHIE/O(LNTE/OD,2),
!!$$$     X          P_ULN = 6*LEVS+2*LEVH+6,  !     ULNE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_VLN = 7*LEVS+2*LEVH+6,  !     VLNE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_W   = 8*LEVS+2*LEVH+6,  !       WE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_X   = 9*LEVS+2*LEVH+6,  !       XE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_Y   =10*LEVS+2*LEVH+6,  !       YE/O(LNTE/OD,2,LEVS),
!!$$$     X          P_RT  =11*LEVS+2*LEVH+6,  !      RTE/O(LNTE/OD,2,LEVH),
!!$$$     X          P_ZQ  =11*LEVS+3*LEVH+6)  !      ZQE/O(LNTE/OD,2)

      INTEGER                LOTS,LOTD,LOTA

!!$$$      PARAMETER            ( LOTS = 5*LEVS+1*LEVH+3 )
!!$$$      PARAMETER            ( LOTD = 6*LEVS+2*LEVH+0 )
!!$$$      PARAMETER            ( LOTA = 3*LEVS+1*LEVH+1 )

      INTEGER              IBRAD,IFGES,IHOUR,INI,J,JDT,KSOUT,MAXSTP
      INTEGER              mdt,idt,timetot,timer,time0
      INTEGER              MODS,N1,N2,N3,N4,NDGF,NDGI,NFILES,NFLPS
      INTEGER              n1hyb, n2hyb
      INTEGER              NGES,NGPKEN,NITER,NNMOD,NRADF,NRADR
      INTEGER              NSFCF,NSFCI,NSFCS,NSIGI,NSIGS,NSTEP
!     INTEGER              NZNLF,NZNLI,NZNLS,ID,IRET,NSOUT
      INTEGER              NZNLF,NZNLI,NZNLS,ID,IRET,NSOUT,kdt_switch

      INTEGER              IERR,IPRINT,K,L,LOCL,N
      INTEGER              LAN,LAT

      REAL(KIND=KIND_EVOD) CHOUR
      REAL(KIND=KIND_EVOD) zhour
      LOGICAL LSOUT
      LOGICAL SPS
!DHOU 05/28/2008, add SPS for applying stochastic or not
      INTEGER HOUTASPS
!DHOU 09/08/2008, add HOUTASPS for time (iintegration hour) of output after SPS

      REAL(KIND=KIND_EVOD),ALLOCATABLE :: TEE1(:)

!JFE  REAL(KIND=KIND_EVOD) PHIBS,DPHIBR

!!$$$      INTEGER              INDLSEV,JBASEV
!!$$$      INTEGER              INDLSOD,JBASOD


      INTEGER ikey,nrank_all,kcolor

      REAL(KIND=KIND_EVOD) CONS0P5,CONS1200,CONS3600    !CONSTANT
      REAL(KIND=KIND_EVOD) CONS0                        !CONSTANT

      LOGICAL LSLAG

 END TYPE GFS_InternalState

! This state is supported by C pointer not F90 pointer, thus
! need this wrap.
!-----------------------------------------------------------
 TYPE GFS_wrap
     TYPE (GFS_InternalState), POINTER :: Int_State
 END TYPE GFS_wrap

 END MODULE GFS_InternalState_ESMFMod
