!------------------------------------------------------------------------------
!
! MODULE: Kinds
!
! DESCRIPTION:
!> Define kinds and global parameters/constants/types of GCIP
!
! REVISION HISTORY:
! September 2011
! January 2014 - modified
!
!------------------------------------------------------------------------------

module Kinds
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! KIND Parameters
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  integer, parameter :: BYTE16=16, BYTE8=8, BYTE4=4, BYTE2=2, BYTE1=1
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! Coding constants
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  real, parameter :: MISSING=-9999.9, MISSING_INT = -99
  real, parameter :: BUFR_MISSING = 999999.0 ! BUFR missing data is 10.E10
  integer, parameter :: BUFR_REPORT_LENGTH = 512
  ! Physics constants
  real, parameter :: GAS_CONSTANT = 287.05
  real(kind=BYTE8), parameter :: PI=3.14159265358979
  real(kind=BYTE8), parameter :: R2D=180./PI, D2R=PI/180.
  ! space, tab, carriage return, line feed
  character, parameter :: SP=char(32), TB=char(9), CR=char(13), LF=char(10)
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! types
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
  ! model original input 3D fields
  type :: model_fields3D_t
     ! all models
     real, dimension(:,:,:), allocatable :: p   ! pressure
     real, dimension(:,:,:), allocatable :: h   ! height
     real, dimension(:,:,:), allocatable :: pvv ! pressureVerticalVelocity
     real, dimension(:,:,:), allocatable :: cwm ! cloudWaterMixingRatio
     real, dimension(:,:,:), allocatable :: t
     real, dimension(:,:,:), allocatable :: wvm ! waterVaporMixingRatio, derived from specificHumidity
     ! RAP & NAM
     real, dimension(:,:,:), allocatable :: rwm ! rainWaterMixingRatio
     real, dimension(:,:,:), allocatable :: snm ! snowMixingRatio
     ! RAP
     real, dimension(:,:,:), allocatable :: gpm ! graupelMixingRatio
     ! RAP - CICE (the same as ICMR, just conventional difference)
     real, dimension(:,:,:), allocatable :: icm ! iceMixingRatio
     ! GFS & NAM: model field; RAP: derived
     real, dimension(:,:,:), allocatable :: rh  ! relativeHumidity
  end type model_fields3D_t

  ! model variables for final icing algorithm
  type :: model_inputs_t
     ! original fields
     real, dimension(:,:,:), allocatable :: h  ! geopotentialHeight
     real, dimension(:,:,:), allocatable :: pvv! pressureVerticalVelocity
     real, dimension(:,:,:), allocatable :: p  ! pressure
     real, dimension(:,:,:), allocatable :: t  ! temperature
     real, dimension(:,:,:), allocatable :: rh ! relativeHumidity
     ! Derives
     real, dimension(:,:,:), allocatable :: ept ! equivPotentialTemp
     real, dimension(:,:,:), allocatable :: icc ! iceCondensate
     real, dimension(:,:,:), allocatable :: lqc ! liquidCondensate
     real, dimension(:,:,:), allocatable :: slw ! superCooledLiquidWater
     real, dimension(:,:,:), allocatable :: twp ! totalWaterPath
     ! 2D
     real, dimension(:,:), allocatable :: top ! topography
  end type model_inputs_t

  type :: metar_data_t
     logical :: affected  ! true -- affected by observation, false -- no observation
     real :: cloudBaseHeight
     real :: dist2CloudBaseHeight
     integer :: cloudCoverage
     real :: dist2CloudCoverage
     real :: dist2FreezingDrizzle
     real :: dist2FreezingRain
     real :: dist2IcePellets
     real :: dist2Rain
     real :: dist2Snow
     real :: dist2Drizzle
     real :: dist2Thunder
  end type metar_data_t

  type :: satellite_data_t
     real, allocatable :: vis(:, :)     ! NORM_ALBEDO: normalized albedo (visable)
     real, allocatable :: ch2(:, :)     ! SW_IR: shortwave infrared (channel 2 or 3.9)
     real, allocatable :: ch4(:, :)     ! IR: infrared (channel 4)
     real, allocatable :: ch2mch4(:, :) ! IR2-IR4: fog mask
     real, allocatable :: sunz(:, :)    ! SUN_ZENITH:  sun's zenith angle
     real, allocatable :: ch2_ref(:, :) ! SW_REFL: shortwave reflectance
     real, allocatable :: satice(:, :)  ! SAT_ICE_IDX: satellite icing index
  end type satellite_data_t

  type :: lightning_data_t
     real :: dist ! distance
     real :: rate
  end type lightning_data_t

  type :: pirep_data_t
     real :: interest
     real :: weight
  end type pirep_data_t

  type :: radar_data_t
     ! VIP percentile
     real, allocatable :: vipPct(:, :, :) ! (nx, ny, 7)
     ! Radar reflectivity is devided to 7 VIP fields
     ! VIP>=1
     ! VIP==1
     ! VIP==2
     ! VIP==3
     ! VIP==4
     ! VIP==5
     ! VIP==6

     ! DBZ percentile
     real, allocatable :: dbzPct(:, :, :) ! (nx, ny, 2)
     ! 25 percentage
     ! 75 percentage
  end type radar_data_t

  type :: input_t
     !------------------------------------------------
     ! model
     type(model_inputs_t) :: model ! mix of 2D and 3D

     !------------------------------------------------
     !satellite
     type(satellite_data_t) :: sat

     !------------------------------------------------
     ! METAR
     type(metar_data_t), allocatable :: metar(:,:)

     !------------------------------------------------
     ! radar
     type(radar_data_t) :: radar

     !------------------------------------------------
     ! pirep
     type(pirep_data_t), allocatable :: pirep(:,:,:)

     !------------------------------------------------
     ! lightning
     type(lightning_data_t), allocatable :: lightning(:, :)

  end type input_t

  type :: icing_output_t

     character(len=3) :: ctype ! FLT(flight level)/PRS(pressure level)
     real, allocatable :: levels(:)

     real, allocatable :: probability(:,:,:)
     real, allocatable :: sld(:,:,:)
     real, allocatable :: severity(:,:,:)
  end type icing_output_t

end module Kinds
