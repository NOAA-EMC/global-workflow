module cads
!$$$ module documentation block
!
! module:  cads
!   prgmmr: Jung
!
! abstract:  module containing subroutines for the cloud and aerosol detection software
!
! program history log:
!
!
!
! subroutines included:
!
!
!  remarks:  variable definitions
!
!
!$$$ end documentation block


  use kinds, only: i_kind, r_kind
  implicit none
  save

! set default to private
  private
! set routines to public
  public :: cloud_aerosol_detection
  public :: cads_setup_cloud
  public :: Cloud_Detect_Type
  public :: cads_imager_calc

  public :: M__Sensor,N__Num_Bands,N__GradChkInterval,N__Band_Size,N__Bands,N__Window_Width, &
            N__Window_Bounds,R__BT_Threshold,R__Grad_Threshold,R__Window_Grad_Threshold, L__Do_Quick_Exit, &
            L__Do_CrossBand, N__BandToUse,L__Do_Imager_Cloud_Detection, N__Num_Imager_Chans, &
            N__Num_Imager_Clusters,N__Imager_Chans,R__Stddev_Threshold,R__Coverage_Threshold, &
            R__FG_Departure_Threshold

    INTEGER(i_kind)          :: M__Sensor                ! Unique ID for sensor
    INTEGER(i_kind)          :: N__Num_Bands             ! Number of channel bands
    INTEGER(i_kind), POINTER :: N__GradChkInterval(:)    ! Window width used in gradient calculation
    INTEGER(i_kind), POINTER :: N__Band_Size(:)          ! Number of channels in each band
    INTEGER(i_kind), POINTER :: N__Bands(:,:)            ! Channel lists
    INTEGER(i_kind), POINTER :: N__Window_Width(:)       ! Smoothing filter window widths per band
    INTEGER(i_kind), POINTER :: N__Window_Bounds(:,:)    ! Channels in the spectral window gradient check
    INTEGER(i_kind), POINTER :: N__BandToUse(:)          ! Band number assignment for each channel
    LOGICAL  :: L__Do_Quick_Exit                         ! On/off switch for the Quick Exit scenario
    LOGICAL  :: L__Do_CrossBand                          ! On/off switch for the cross-band method
    REAL(r_kind), POINTER :: R__BT_Threshold(:)          ! BT threshold for cloud contamination
    REAL(r_kind), POINTER :: R__Grad_Threshold(:)        ! Gradient threshold for cloud contamination
    REAL(r_kind), POINTER :: R__Window_Grad_Threshold(:) ! Threshold for window gradient check in QE

    LOGICAL  :: L__Do_Imager_Cloud_Detection             ! On/off switch for the imager cloud detection
    INTEGER(i_kind)         :: N__Num_Imager_Chans       ! No. of imager channels
    INTEGER(i_kind)         :: N__Num_Imager_Clusters    ! No. of clusters to be expected
    INTEGER(i_kind),POINTER :: N__Imager_Chans(:)        ! List of imager channels
    REAL(r_kind),POINTER    :: R__Stddev_Threshold(:)    ! St. Dev. threshold, one for each imager channel
    REAL(r_kind)            :: R__Coverage_Threshold     ! Threshold for fractional coverage of a cluster
    REAL(r_kind)            :: R__FG_Departure_Threshold ! Threshold for imager FG departure


! set passed variables to public

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   * CADS_Module *
!   A. Collard  ECMWF 01/02/06

!   * PURPOSE *
!   -----------
!   Sets up structures to be used in processing of advanced IR sounders.

!   * MODIFICATIONS *
!   -----------------
!   01/02/06   A.Collard   1.0   Original export version.
!   17/11/09   R.Eresmaa   1.1   Include parameters of the Quick Exit /
!                                long-wave window gradient check.
!   11/11/11   R.Eresmaa   1.2   Add processing capability for CrIS.
!   03/12/13   R.Eresmaa   2.0   Add imager-assisted cloud detection.
!   10/11/15   R.Eresmaa   2.2   Changed instrument ID naming convention.
!                                Changed aerosol detection parameters.
!   20/12/16   R.Eresmaa   2.3   Remove aerosol detection parameters.
!   05/02/19   R.Eresmaa   2.4   Explicit KIND specifications.
!   16/04/20   R.Eresmaa   3.0   Combine cloud and aerosol detection, rename.
!                                Include aerosol type recognition.
!                                Include land sensitivity parameters.
!                                Include trace gas detection. Rename.


  INTEGER(i_kind), PARAMETER :: INST_ID_AIRS = 11
  INTEGER(i_kind), PARAMETER :: INST_ID_IASI = 16
  INTEGER(i_kind), PARAMETER :: INST_ID_CRIS = 27
  INTEGER(i_kind), PARAMETER :: INST_ID_IRS = 57
  INTEGER(i_kind), PARAMETER :: INST_ID_IASING = 59
  INTEGER(i_kind), PARAMETER :: INST_ID_IKFS2 = 94
  INTEGER(i_kind), PARAMETER :: INST_ID_HIRAS = 97
  INTEGER(i_kind), PARAMETER :: INST_ID_GIIRS = 98

  INTEGER(i_kind), PARAMETER :: JP__MIN_SENSOR_INDEX = INST_ID_AIRS
  INTEGER(i_kind), PARAMETER :: JP__MAX_SENSOR_INDEX = INST_ID_GIIRS

  TYPE Aerosol_Detect_Type
    INTEGER(i_kind) :: M__Sensor                         ! Unique ID for sensor
    INTEGER(i_kind) :: N__Num_Aerosol_Tests              ! Number of aerosol detection tests
    INTEGER(i_kind), POINTER :: N__Num_Regression(:)     ! Number of conversion coefficients for AOD
    INTEGER(i_kind), POINTER :: N__Num_Aerosol_Chans(:)  ! Number of aerosol detection channels
    INTEGER(i_kind), POINTER :: N__Aerosol_Chans(:,:)    ! List of aerosol detection channels
    INTEGER(i_kind)          :: N__Mean_Aerosol_Chans    ! Boxcar averaging window width
    REAL(r_kind), POINTER    :: R__Aerosol_TBD(:,:)      ! Aerosol detection thresholds
    REAL(r_kind), POINTER    :: R__coef_AOD(:,:)         ! Coefficients for conversion to AOD
    REAL(r_kind)             :: R__Rank_Thres_Coeff(3)   ! Coefficients to restrict rejections to affected channels
    REAL(r_kind)             :: R__Unclassified_Thres    ! Rejection threshold for unclassified aerosol
    REAL(r_kind)             :: R__Land_Fraction_Thres   ! Threshold for land fraction in FOV
  END TYPE Aerosol_Detect_Type

  TYPE Cloud_Detect_Type
    INTEGER(i_kind)          :: M__Sensor                ! Unique ID for sensor
    INTEGER(i_kind)          :: N__Num_Bands             ! Number of channel bands
    INTEGER(i_kind), POINTER :: N__GradChkInterval(:)    ! Window width used in gradient calculation
    INTEGER(i_kind), POINTER :: N__Band_Size(:)          ! Number of channels in each band
    INTEGER(i_kind), POINTER :: N__Bands(:,:)            ! Channel lists
    INTEGER(i_kind), POINTER :: N__Window_Width(:)       ! Smoothing filter window widths per band
    INTEGER(i_kind), POINTER :: N__Window_Bounds(:,:)    ! Channels in the spectral window gradient check
    INTEGER(i_kind), POINTER :: N__BandToUse(:)          ! Band number assignment for each channel
    LOGICAL  :: L__Do_Quick_Exit                         ! On/off switch for the Quick Exit scenario
    LOGICAL  :: L__Do_CrossBand                          ! On/off switch for the cross-band method
    REAL(r_kind), POINTER :: R__BT_Threshold(:)          ! BT threshold for cloud contamination
    REAL(r_kind), POINTER :: R__Grad_Threshold(:)        ! Gradient threshold for cloud contamination
    REAL(r_kind), POINTER :: R__Window_Grad_Threshold(:) ! Threshold for window gradient check in QE

    LOGICAL  :: L__Do_Imager_Cloud_Detection             ! On/off switch for the imager cloud detection
    INTEGER(i_kind)         :: N__Num_Imager_Chans       ! No. of imager channels
    INTEGER(i_kind)         :: N__Num_Imager_Clusters    ! No. of clusters to be expected
    INTEGER(i_kind),POINTER :: N__Imager_Chans(:)        ! List of imager channels
    REAL(r_kind),POINTER    :: R__Stddev_Threshold(:)    ! St. Dev. threshold, one for each imager channel
    REAL(r_kind)            :: R__Coverage_Threshold     ! Threshold for fractional coverage of a cluster
    REAL(r_kind)            :: R__FG_Departure_Threshold ! Threshold for imager FG departure
  END TYPE Cloud_Detect_Type

  TYPE Land_Sensitivity_Type
    INTEGER(r_kind)         :: M__Sensor                 ! Unique ID for sensor
    REAL(r_kind)            :: R__Land_Fraction_Thres    ! Threshold on land fraction
    REAl(r_kind)            :: R__Level_Thres            ! Threshold on normalized channel height assignment
  END TYPE Land_Sensitivity_Type

  TYPE Trace_Gas_Detect_Type
    INTEGER(i_kind)         :: M__Sensor                  ! Unique ID for sensor
    INTEGER(i_kind)         :: N__Num_Trace_Gas_Checks    ! Number of trace gases to be checked
    INTEGER(i_kind),POINTER :: N__Num_Tracer_Channels(:)  ! Number of gas-sensitive channels
    INTEGER(i_kind),POINTER :: N__Tracer_Channels(:,:)    ! Gas-sensitive channels
    INTEGER(i_kind),POINTER :: N__Num_Control_Channels(:) ! Number of control channels
    INTEGER(i_kind),POINTER :: N__Control_Channels(:,:)   ! Control channels
    INTEGER(i_kind),POINTER :: N__Num_Flagged_Channels(:) ! Number of affected channels
    INTEGER(i_kind),POINTER :: N__Flagged_Channels(:,:)   ! Affected channels
    REAL(r_kind),POINTER    :: R__D_Obs_Threshold(:)      ! Observed Tb difference threshold
    REAL(r_kind),POINTER    :: R__D_Dep_Threshold(:)      ! Departure difference threshold
  END TYPE Trace_Gas_Detect_Type


  TYPE(Aerosol_Detect_Type) :: &
     S__CADS_Setup_Aerosol(JP__Min_Sensor_Index:JP__Max_Sensor_Index)

  TYPE(Cloud_Detect_Type) :: &
     S__CADS_Setup_Cloud(JP__Min_Sensor_Index:JP__Max_Sensor_Index)

  TYPE(Land_Sensitivity_Type) :: &
     S__CADS_Setup_Land(JP__Min_Sensor_Index:JP__Max_Sensor_Index)

  TYPE(Trace_Gas_Detect_Type) :: &
     S__CADS_Setup_Trace_Gas(JP__Min_Sensor_Index:JP__Max_Sensor_Index)


contains

SUBROUTINE CADS_Abort(String)

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   *CADS_Abort*
!   R. Eresmaa   ECMWF   16/04/20

!   * PURPOSE *
!   -----------
!   Controlled abortion of running CADS when facing exceptions such as
!   necessary input files missing or they are corrupt.

!   * INTERFACE *
!   -------------
!   *CALL* * CADS_Abort()* from
!      CADS_Main, CADS_Setup_Aerosol, CADS_Setup_Cloud,
!      CADS_Setup_Land_Sensitivity, or CADS_Setup_Trace_Gas.

  IMPLICIT NONE
  CHARACTER(LEN=*) :: String

  WRITE(*,*) String
  STOP

END SUBROUTINE CADS_Abort

subroutine cloud_aerosol_detection( I__Sensor_ID, I__Num_Chans, I__Chan_ID, &
                                    I__Min_Level, I__Max_Level, Z__BT_Obser, Z__BT_Model, Z__Chan_Height, K__Chan_ID_Imager, &
                                    Z__Cluster_Fraction, Z__BT_in_Cluster, Z__BT_Overall_SDev, Z__BT_Model_Imager, &
                                    I__Flag_Cloud, Z__Cloud_Level )

!$$$ subprogram documentation block
!               .      .    .
! subprogram:  cloud_aerosol_detection    determine clear/cloudy profiles from hyperspectral IR instruments
!
!   prgmmr: jung           org: cimss            date: 2022-10-17
!
! abstract: determine if a profile is clear/cloudy.  If cloudy, determine which channels are affected
!           This subroutine is designed for infrared hyperspectral sounders. Current code supports AIRS, IASI and CrIS.a
!           This subroutine is based on the Cloud and Aerosol Detection Software Version 3 developed within the context
!           of the EUMETSAT and Met Office, UK, by one or more partners within the Numerical Weather Predicion's
!           Satellite Application Facilities.  A version of this code is operational at ECMWF.
!           COPYRIGHT 2020, EUMETSAT, ALL RIGHTS RESERVED.
!
! program history log:
!     2022-10-17  jung   Initial coding
!
! input argument list:
!     I_Sensor_ID  - internal sensor identification.
!     I__Num_Chans - number of channels per obs
!     I__Chan_ID   - array of actual channel numbers
!     Z__Longitude - FOV longitude
!     Z__Latitude  - FOV latitude
!     Z__Land_Fraction    - FOV land fraction
!     I__Min_Level - model tropopause height (start of cloud detection)
!     I__Max_Level - model top of boundary layer ( stop of cloud detection)
!     Z__BT_Obser  - observaton brightness temperature
!     Z__BT_Model  - model derived brightness temperature
!     Z__Chan_Height - model derived height where an opaque cloud influences the radiance.
!                    also used to re-organize channels
!     Z__Cloud_Level - Cloud height assignment
!
! output argument list:
!     icloud_layer  - model layer where cloud is detected
!
! attributes:
!     language: f90
!     machine:
!
!$$$ end documentation block

  use kinds, only: i_kind, r_kind
  implicit none

  integer(i_kind),                        intent(in   ) :: I__Sensor_ID
  integer(i_kind),                        intent(in   ) :: I__Num_Chans
  integer(i_kind),dimension(I__Num_Chans),intent(in   ) :: I__Chan_ID 
  integer(i_kind),                        intent(in   ) :: I__Min_Level !tropopause pressure
  integer(i_kind),                        intent(in   ) :: I__Max_Level !boundary layer pressure
  real(r_kind),                           intent(in   ) :: Z__BT_Obser(:)  !Observation BT
  real(r_kind),                           intent(in   ) :: Z__BT_Model(:) !Model derived BT
  real(r_kind),                           intent(in   ) :: Z__Chan_Height(:) !Channel height assignmenta
  integer(i_kind),                        intent(in   ) :: K__Chan_ID_Imager(:) ! imager channel numbers
  real(r_kind),                           intent(in   ) :: Z__Cluster_Fraction(:)
  real(r_kind),                           intent(in   ) :: Z__BT_in_Cluster(:,:)
  real(r_kind),                           intent(in   ) :: Z__BT_Overall_SDev(:)
  real(r_kind),                           intent(in   ) :: Z__BT_Model_Imager(:)
  real(r_kind),                           intent(  out) :: Z__Cloud_Level  ! cloud height assignment
  integer(i_kind),dimension(I__Num_Chans),intent(  out) :: I__Flag_Cloud      ! cloud use flag

! Interim prodcts

! Diagnostics: percentages of positive detections
! Input/Output file management

  N__Num_Imager_Chans = S__CADS_Setup_Cloud(I__Sensor_ID) % N__Num_Imager_Chans
  N__Num_Imager_Clusters = S__CADS_Setup_Cloud(I__Sensor_ID) % N__Num_Imager_Clusters

  CALL CADS_Detect_Cloud( I__Sensor_ID, I__Num_Chans, I__Chan_ID,I__Min_Level, I__Max_Level, N__Num_Imager_Chans, &
                 K__Chan_ID_Imager, N__Num_Imager_Clusters, I__Flag_Cloud, Z__BT_Obser, Z__BT_Model, Z__Chan_Height, &
                 Z__Cluster_Fraction, Z__BT_in_Cluster, Z__BT_Overall_SDev, Z__BT_Model_Imager, Z__Cloud_Level )
  
end subroutine cloud_aerosol_detection

SUBROUTINE CADS_Setup_Cloud

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and Meteo France.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.


!   * Cloud detection setup *
!   A. Collard  ECMWF 01/02/06

!   * PURPOSE *
!   -----------
!   Initialise cloud detection parameters for advanced infrared sounders.

!   * INTERFACE *
!   -------------
!   CADS_Setup_Cloud is called from CADS_Main.

!   * METHOD *
!   ----------
!   Default values are assigned to the cloud detections setup structure.

!   MODIFICATIONS
!   -------------
!   01/02/06   A.Collard   1.0   Original code.
!   19/10/06   A.Collard   1.1   Use IASI 300 Subset Channels.
!   17/11/09   R.Eresmaa   1.2   Use IASI 366 Subset Channels.
!                                Include parameters of the Quick Exit /
!                                long-wave window gradient check parameters.
!   11/11/11   R.Eresmaa   1.3   Default channel list for AIRS bands 3-5
!                                modified.
!                                Processing capability for CrIS added
!                                assuming a selection of 320 channels.
!   03/12/13   R,Eresmaa   2.0   Imager-assisted cloud detection added for
!                                IASI.
!                                Updated setup for CrIS.
!   19/01/15   R.Eresmaa   2.1   Remove unused variable specifications and
!                                switch aerosol detection on by default for
!                                AIRS and IASI.
!   10/11/15   R.Eresmaa   2.2   Changed instrument ID naming convention.
!                                Changed parameters of aerosol detection.
!   20/12/16   R.Eresmaa   2.3   Remove settings for aerosol detection.
!   05/02/19   R.Eresmaa   2.4   Explicit KIND specifications.
!                                Add HIRAS, GIIRS (IASING + IRS added earlier)
!   16/04/20   R.Eresmaa   3.0   Rename, tidy up.

  use kinds, only: i_kind, r_kind
  use gsi_io, only: verbose
  IMPLICIT NONE

! Local variables

  CHARACTER(LEN=6)   :: CL__InstrumentName
  CHARACTER(LEN=20)  :: CL__Cloud_Detection_File

  INTEGER(i_kind) :: J, J__Sensor      ! Loop variables
  INTEGER(i_kind) :: INIU1, IOS

!-----------------------
! Namelist variables
!-----------------------

! N.B. Max_Bands must be greater than 5
  INTEGER(i_kind), PARAMETER :: JP__Max_Bands    =    8
  INTEGER(i_kind), PARAMETER :: JP__Max_Channels = 8461

  INTEGER(i_kind) :: M__Sensor
  INTEGER(i_kind) :: N__Num_Bands
  INTEGER(i_kind) :: N__GradChkInterval(JP__Max_Bands)
  INTEGER(i_kind) :: N__Band_Size(JP__Max_Bands)
  INTEGER(i_kind) :: N__Bands(JP__Max_Channels,JP__Max_Bands)
  INTEGER(i_kind) :: N__Window_Width(JP__Max_Bands)
  INTEGER(i_kind) :: N__Window_Bounds(JP__Max_Bands,2)
  REAL(r_kind)    :: R__BT_Threshold(JP__Max_Bands)
  REAL(r_kind)    :: R__Grad_Threshold(JP__Max_Bands)
  REAL(r_kind)    :: R__Window_Grad_Threshold(JP__Max_Bands)
  LOGICAL         :: L__Do_Quick_Exit
  LOGICAL         :: L__Do_CrossBand
  INTEGER(i_kind) :: N__BandToUse(JP__Max_Bands)

! Imager-based cloud detection
  LOGICAL         :: L__Do_Imager_Cloud_Detection
  INTEGER(i_kind) :: N__Num_Imager_Chans
  INTEGER(i_kind) :: N__Num_Imager_Clusters
  INTEGER(i_kind) :: N__Imager_Chans(JP__Max_Bands)
  REAL(r_kind)    :: R__Stddev_Threshold(JP__Max_Bands)
  REAL(r_kind)    :: R__Coverage_Threshold
  REAL(r_kind)    :: R__FG_Departure_Threshold

! Namelist
  NAMELIST / Cloud_Detect_Coeffs / M__Sensor, N__Num_Bands,             &
           N__Band_Size, N__Bands, N__Window_Width, N__Window_Bounds,   &
           N__GradChkInterval, R__BT_Threshold, R__Grad_Threshold,      &
           R__Window_Grad_Threshold, L__Do_Quick_Exit,                  &
           L__Do_CrossBand, N__BandToUse,                               &
           L__Do_Imager_Cloud_Detection, N__Num_Imager_Chans,           &
           N__Num_Imager_Clusters, N__Imager_Chans,                     &
           R__Stddev_Threshold, R__Coverage_Threshold,                  &
           R__FG_Departure_Threshold

!============================================================================
!   Loop through sensors setting up cloud detection
!============================================================================

  SensorLoop : DO J__Sensor = JP__Min_Sensor_Index, JP__Max_Sensor_Index

!    SELECT CASE (I__Sensor_ID)
    SELECT CASE (J__Sensor)

    CASE(INST_ID_AIRS)
    !====================
    ! Set up AIRS
    !====================

      CL__InstrumentName='AIRS'
      CL__Cloud_Detection_File = 'AIRS_CLDDET.NL'

      N__Num_Bands = 5

      N__Band_Size(:) = 0
      N__Band_Size(1:N__Num_Bands) =(/138, 36, 54, 23, 65 /)

      N__Bands(:,:)= 0

      N__Bands(1:N__Band_Size(1),1) = &
    (/    1,    6,    7,   10,   11,   15,   16,   17,   20,   21, &
         22,   24,   27,   28,   30,   36,   39,   40,   42,   51, &
         52,   54,   55,   56,   59,   62,   63,   68,   69,   71, &
         72,   73,   74,   75,   76,   77,   78,   79,   80,   82, &
         83,   84,   86,   92,   93,   98,   99,  101,  104,  105, &
        108,  110,  111,  113,  116,  117,  123,  124,  128,  129, &
        138,  139,  144,  145,  150,  151,  156,  157,  159,  162, &
        165,  168,  169,  170,  172,  173,  174,  175,  177,  179, &
        180,  182,  185,  186,  190,  192,  193,  198,  201,  204, &
        207,  210,  213,  215,  216,  218,  221,  224,  226,  227, &
        232,  239,  248,  250,  251,  252,  253,  256,  257,  261, &
        262,  267,  272,  295,  299,  305,  308,  309,  310, &
        318,  321,  333,  338,  355,  362,  375,  475, &
        484,  497,  528,  587,  672,  787,  791,  843,  870,  914, &
        950 /)

      N__Bands(1:N__Band_Size(2),2) = &
     (/ 1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082, &
        1083, 1088, 1090, 1092, 1095, 1104, 1111, 1115, 1116, 1119, &
        1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, 1221, 1237, &
        1252, 1260, 1263, 1266, 1278, 1285 /)

      N__Bands(1:N__Band_Size(3),3) = &
     (/ 1290, 1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &
        1466, 1471, 1477, 1479, 1488, 1500, 1519, 1520, 1538, 1545, &
        1565, 1574, 1583, 1593, 1614, 1627, 1636, 1644, 1652, 1669, &
        1674, 1681, 1694, 1708, 1717, 1723, 1740, 1748, 1751, 1756, &
        1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, 1803, 1806, &
        1812, 1826, 1843, 1852 /)

      N__Bands(1:N__Band_Size(4),4) = &
     (/ 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 1877, &
        1881, 1882, 1883, 1884, 1897, 1901, 1911, 1917, 1918, 1921, &
        1923, 1924, 1928 /)

      N__Bands(1:N__Band_Size(5),5) = &
     (/ 1937, 1938, 1939, 1941, 1946, 1947, 1948, 1958, 1971, 1973, &
        1988, 1995, 2084, 2085, 2097, 2098, 2099, 2100, 2101, 2103, &
        2104, 2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, &
        2115, 2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, &
        2134, 2141, 2145, 2149, 2153, 2164, 2189, 2197, 2209, 2226, &
        2234, 2280, 2318, 2321, 2325, 2328, 2333, 2339, 2348, 2353, &
        2355, 2363, 2370, 2371, 2377 /)

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 5,5,5,5,5 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 14,6,8,5,8 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 475
      N__Window_Bounds(1,2) = 950

      R__BT_Threshold(:) = 0.0_r_kind
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.43_r_kind, 0.5_r_kind, 0.5_r_kind, 0.5_r_kind, 0.5_r_kind/)

      R__Grad_Threshold(:) = 0.0_r_kind
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02_r_kind, 0.02_r_kind, 0.02_r_kind, 0.02_r_kind, 0.02_r_kind /)

      R__Window_Grad_Threshold(:) = 0.0_r_kind
      R__Window_Grad_Threshold(1) = 0.4_r_kind

      L__Do_Quick_Exit = .TRUE.


    ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,4,1 /)


    ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0_r_kind
      R__Coverage_Threshold = 0.0_r_kind
      R__FG_Departure_Threshold = 0.0_r_kind


    CASE(INST_ID_IASI)
    !====================
    ! Set up IASI
    !====================

      CL__InstrumentName='IASI'
      CL__Cloud_Detection_File = 'IASI_CLDDET.NL'

      N__Num_Bands = 5

      N__Band_Size(:) = 0
      N__Band_Size(1:N__Num_Bands) =(/ 184, 15, 116, 4, 15 /)

      N__Bands(:,:)= 0

    ! Use the "IASI 366" Subset
      N__Bands(1:N__Band_Size(1),1) = &
     (/   16,   38,   49,   51,   55,   57,   59,   61,   63,   66, &
          70,   72,   74,   79,   81,   83,   85,   87,   89,   92, &
          95,   97,   99,  101,  104,  106,  109,  111,  113,  116, &
         119,  122,  125,  128,  131,  133,  135,  138,  141,  144, &
         146,  148,  151,  154,  157,  159,  161,  163,  165,  167, &
         170,  173,  176,  178,  179,  180,  183,  185,  187,  189, &
         191,  193,  195,  197,  199,  201,  203,  205,  207,  210, &
         212,  214,  217,  219,  222,  224,  226,  228,  230,  232, &
         234,  236,  239,  241,  242,  243,  246,  249,  252,  254, &
         256,  258,  260,  262,  265,  267,  269,  271,  272,  273, &
         275,  278,  280,  282,  284,  286,  288,  290,  292,  294, &
         296,  299,  301,  303,  306,  308,  310,  312,  314,  316, &
         318,  320,  323,  325,  327,  329,  331,  333,  335,  341, &
         347,  350,  352,  354,  356,  358,  360,  362,  364,  366, &
         369,  371,  373,  375,  377,  379,  381,  386,  389,  404, &
         407,  410,  414,  416,  426,  428,  432,  434,  445,  457, &
         515,  546,  552,  566,  571,  573,  646,  662,  668,  756, &
         867,  921, 1027, 1090, 1133, 1191, 1194, 1271, 1805, 1884, &
        1946, 1991, 2094, 2239 /)

      N__Bands(1:N__Band_Size(2),2) = &
     (/ 1479, 1509, 1513, 1521, 1536, 1574, 1579, 1585, 1587, 1626, &
        1639, 1643, 1652, 1658, 1671 /)

      N__Bands(1:N__Band_Size(3),3) = &
     (/ 2119, 2213, 2271, 2321, 2398, 2701, 2741, 2819, 2889, 2907, &
        2910, 2919, 2939, 2944, 2948, 2951, 2958, 2977, 2985, 2988, &
        2991, 2993, 3002, 3008, 3014, 3027, 3029, 3036, 3047, 3049, &
        3053, 3058, 3064, 3069, 3087, 3093, 3098, 3105, 3107, 3110, &
        3127, 3136, 3151, 3160, 3165, 3168, 3175, 3178, 3207, 3228, &
        3244, 3248, 3252, 3256, 3263, 3281, 3303, 3309, 3312, 3322, &
        3375, 3378, 3411, 3438, 3440, 3442, 3444, 3446, 3448, 3450, &
        3452, 3454, 3458, 3467, 3476, 3484, 3491, 3497, 3499, 3504, &
        3506, 3509, 3518, 3527, 3555, 3575, 3577, 3580, 3582, 3586, &
        3589, 3599, 3653, 3658, 3661, 4032, 5368, 5371, 5379, 5381, &
        5383, 5397, 5399, 5401, 5403, 5405, 5455, 5480, 5483, 5485, &
        5492, 5502, 5507, 5509, 5517, 5558 /)

      N__Bands(1:N__Band_Size(4),4) = &
     (/ 5988, 5992, 5994, 6003 /)

      N__Bands(1:N__Band_Size(5),5) = &
     (/ 6982, 6985, 6987, 6989, 6991, 6993, 6995, 6997, 7267, 7269, &
        7424, 7426, 7428, 7885, 8007 /)

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/12,5,5,5,5 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 10,6,8,5,8 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 573
      N__Window_Bounds(1,2) = 2239

      R__BT_Threshold(:) = 0.0_r_kind
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.5_r_kind, 0.5_r_kind, 0.5_r_kind, 0.5_r_kind, 0.5_r_kind /)

      R__Grad_Threshold(:) = 0.0_r_kind
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02_r_kind, 0.02_r_kind, 0.02_r_kind, 0.02_r_kind, 0.02_r_kind /)

      R__Window_Grad_Threshold(:) = 0.0_r_kind
      R__Window_Grad_Threshold(1) = 0.4_r_kind

      L__Do_Quick_Exit = .TRUE.


    ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,1,1 /)


    ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .TRUE.

      N__Num_Imager_Chans = 2
      N__Num_Imager_Clusters = 7

      N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

      R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75_r_kind, 0.80_r_kind /)

      R__Coverage_Threshold = 0.03_r_kind
      R__FG_Departure_Threshold = 1.0_r_kind


    CASE(INST_ID_CRIS)
    !====================
    ! Set up CRIS
    !====================

      CL__InstrumentName='CRIS'
      CL__Cloud_Detection_File = 'CRIS_CLDDET.NL'

      N__Num_Bands = 5
 
      N__Band_Size(:) = 0

      N__Band_Size(1:N__Num_Bands) =(/ 137, 123, 76, 12, 6 /)

      N__Bands(:,:)= 0

    ! Use the "CRIS 300" Subset
      N__Bands(1:N__Band_Size(1),1) = &
     (/    1,    5,    9,   13,   17,   18,   19,   20,   21,   22, &
          23,   24,   25,   26,   27,   28,   29,   30,   31,   32, &
          33,   34,   35,   36,   37,   38,   39,   40,   41,   42, &
          43,   44,   45,   46,   47,   48,   49,   50,   51,   52, &
          53,   54,   55,   56,   57,   58,   59,   60,   61,   62, &
          63,   64,   65,   66,   67,   68,   69,   70,   71,   72, &
          73,   74,   75,   76,   77,   78,   79,   80,   81,   82, &
          83,   84,   85,   86,   87,   88,   91,   92,   93,   94, &
          95,   96,   97,   99,  101,  105,  107,  109,  111,  113, &
         115,  116,  117,  118,  119,  120,  121,  122,  123,  124, &
         125,  133,  135,  137,  139,  141,  144,  147,  161,  173, &
         177,  181,  185,  195,  210,  221,  225,  229,  249,  257, &
         269,  273,  293,  301,  317,  333,  349,  369,  409,  433, &
         457,  481,  501,  549,  701,  705,  709 /)

      N__Bands(1:N__Band_Size(2),2) = &
     (/    3,    6,    7,    8,   10,   12,   14,   15,   16,   89, &
          90,  102,  103,  104,  106,  108,  110,  114,  126,  127, &
         129,  132,  134,  138,  140,  143,  145,  146,  148,  149, &
         150,  151,  153,  155,  156,  157,  158,  159,  162,  163, &
         164,  165,  166,  169,  170,  171,  172,  175,  180,  189, &
         200,  201,  205,  206,  214,  217,  218,  226,  228,  230, &
         231,  233,  236,  237,  240,  241,  245,  248,  252,  264, &
         265,  281,  285,  297,  324,  327,  361,  378,  389,  392, &
         400,  473,  493,  500,  503,  511,  527,  528,  529,  530, &
         531,  534,  538,  542,  544,  545,  547,  550,  553,  555, &
         590,  594,  598,  602,  606,  610,  614,  618,  622,  626, &
         645,  649,  653,  657,  661,  665,  685,  702,  703,  704, &
         706,  707,  713 /)

      N__Bands(1:N__Band_Size(3),3) = &
     (/  717,  725,  728,  729,  730,  731,  732,  733,  734,  735, &
         736,  741,  749,  757,  765,  773,  781,  789,  794,  797, &
         805,  806,  815,  822,  829,  839,  845,  853,  861,  868, &
         869,  872,  877,  885,  887,  893,  898,  900,  909,  912, &
         915,  917,  921,  929,  933,  941,  949,  957,  963,  965, &
         973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014, &
        1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093, &
        1101, 1109, 1117, 1125, 1133, 1141 /)
 
      N__Bands(1:N__Band_Size(4),4) = &
     (/ 1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, &
        1221, 1251 /)

      N__Bands(1:N__Band_Size(5),5) = &
     (/ 1189, 1197, 1205, 1213, 1221, 1251  /)


      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 5,5,5,3,3 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 6,6,8,3,3 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 229
      N__Window_Bounds(1,2) = 549

      R__BT_Threshold(:) = 0.0_r_kind
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.5_r_kind, 0.5_r_kind, 0.5_r_kind, 0.5_r_kind, 0.5_r_kind /)

      R__Grad_Threshold(:) = 0.0_r_kind
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02_r_kind, 0.02_r_kind, 0.02_r_kind, 0.02_r_kind, 0.02_r_kind /)

      R__Window_Grad_Threshold(:) = 0.0_r_kind
      R__Window_Grad_Threshold(1) = 0.4_r_kind

      L__Do_Quick_Exit = .TRUE.


    ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1,1,1,1,1 /)


    ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0_r_kind
      R__Coverage_Threshold = 0.0_r_kind
      R__FG_Departure_Threshold = 0.0_r_kind


    CASE(INST_ID_IRS)
    !====================
    ! Set up IRS
    !====================

      CL__InstrumentName='IRS'
      CL__Cloud_Detection_File = 'IRS_CLDDET.NL'

      N__Num_Bands = 1

      N__Band_Size(:) = 0

      N__Band_Size(1:N__Num_Bands) =(/ 138 /)

      N__Bands(:,:)= 0

      N__Bands(1:N__Band_Size(1),1) = &
     (/    1,    2,    3,    4,    5,    6,    7,    8,    9,   10, &
          11,   12,   13,   14,   15,   16,   17,   18,   19,   20, &
          21,   22,   23,   24,   25,   26,   27,   28,   29,   30, &
          31,   32,   33,   34,   35,   36,   37,   38,   39,   40, &
          41,   42,   43,   44,   45,   46,   48,   53,   54,   55, &
          56,   57,   58,   60,   61,   62,   63,   65,   70,   74, &
          75,   76,   77,   78,   79,   80,   81,   82,   83,   84, &
          85,   86,   87,   89,   90,   91,   92,   93,   94,   95, &
          96,   97,   98,   99,  100,  101,  102,  103,  104,  105, &
         106,  107,  108,  109,  118,  119,  131,  145,  163,  169, &
         177,  180,  190,  195,  199,  209,  215,  221,  231,  237, &
         252,  262,  268,  281,  289,  298,  312,  322,  328,  341, &
         347,  359,  375,  384,  390,  404,  412,  421,  648,  656, &
         667,  678,  686,  692,  709,  750,  792,  808 /)

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 12 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 10 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 131
      N__Window_Bounds(1,2) = 808

      R__BT_Threshold(:) = 0.0_r_kind
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.4_r_kind /)

      R__Grad_Threshold(:) = 0.0_r_kind
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02_r_kind /)

      R__Window_Grad_Threshold(:) = 0.0_r_kind
      R__Window_Grad_Threshold(1) = 0.4_r_kind

      L__Do_Quick_Exit = .TRUE.


    ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1 /)


    ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0_r_kind
      R__Coverage_Threshold = 0.0_r_kind
      R__FG_Departure_Threshold = 0.0_r_kind


    CASE(INST_ID_IASING)
    !====================
    ! Set up IASING
    !====================

      CL__InstrumentName='IASING'
      CL__Cloud_Detection_File = 'IASING_CLDDET.NL'

      N__Num_Bands = 1

      N__Band_Size(:) = 0

      N__Band_Size(1:N__Num_Bands) =(/ 254 /)

      N__Bands(:,:)= 0

      N__Bands(1:N__Band_Size(1),1) = &
     (/   31,   75,   97,  101,  109,  113,  117,  121,  125,  131, &
         139,  143,  147,  157,  161,  165,  169,  173,  177,  183, &
         189,  193,  197,  201,  207,  211,  217,  221,  225,  231, &
         237,  243,  249,  255,  261,  265,  269,  275,  281,  287, &
         291,  295,  301,  307,  313,  317,  321,  325,  329,  333, &
         339,  345,  351,  355,  357,  359,  365,  369,  373,  377, &
         381,  385,  389,  393,  397,  401,  403,  405,  407,  409, &
         411,  413,  415,  417,  419,  421,  423,  425,  427,  429, &
         431,  433,  435,  437,  439,  441,  443,  445,  447,  449, &
         451,  453,  455,  457,  459,  461,  463,  465,  467,  469, &
         471,  473,  475,  477,  479,  481,  483,  485,  487,  489, &
         491,  493,  495,  497,  499,  501,  503,  505,  507,  509, &
         511,  513,  515,  517,  519,  521,  523,  525,  527,  529, &
         531,  533,  535,  537,  539,  541,  543,  545,  547,  549, &
         551,  553,  555,  557,  559,  561,  563,  565,  567,  569, &
         571,  573,  575,  577,  579,  581,  583,  585,  587,  589, &
         591,  593,  595,  597,  601,  603,  605,  607,  609,  611, &
         613,  615,  617,  619,  621,  623,  625,  627,  629,  631, &
         633,  635,  637,  639,  641,  643,  645,  647,  649,  651, &
         653,  655,  657,  659,  661,  663,  665,  667,  669,  681, &
         693,  699,  703,  707,  711,  715,  719,  723,  727,  731, &
         737,  741,  745,  749,  753,  757,  761,  771,  777,  807, &
         813,  819,  827,  831,  851,  855,  863,  867,  889,  913, &
        1029, 1091, 1103, 1131, 1141, 1145, 1291, 1323, 1335, 1511, &
        1733, 1841, 2053, 2179, 2265, 2381, 2387, 2541, 3609, 3767, &
        3891, 3981, 4187, 4477 /)

      N__GradChkInterval(:) = 0
      N__GradChkInterval(1:N__Num_Bands) = (/ 25 /)

      N__Window_Width(:) = 0
      N__Window_Width(1:N__Num_Bands) = (/ 20 /)

      N__Window_Bounds(:,:) = 0
      N__Window_Bounds(1,1) = 1145
      N__Window_Bounds(1,2) = 4477

      R__BT_Threshold(:) = 0.0_r_kind
      R__BT_Threshold(1:N__Num_Bands) = (/ 0.27_r_kind /)

      R__Grad_Threshold(:) = 0.0_r_kind
      R__Grad_Threshold(1:N__Num_Bands) = (/ 0.02_r_kind /)

      R__Window_Grad_Threshold(:) = 0.0_r_kind
      R__Window_Grad_Threshold(1) = 0.4_r_kind

      L__Do_Quick_Exit = .TRUE.


    ! This is cross-band:

      L__Do_CrossBand = .TRUE.

      N__BandToUse(:) = 0
      N__BandToUse(1:N__Num_Bands) = (/ 1 /)

    ! This is the setup for imager cloud detection

      L__Do_Imager_Cloud_Detection = .FALSE.

      N__Num_Imager_Chans = 0
      N__Num_Imager_Clusters = 0
      N__Imager_Chans(:) = 0

      R__Stddev_Threshold(:) = 0.0_r_kind
      R__Coverage_Threshold = 0.0_r_kind
      R__FG_Departure_Threshold = 0.0_r_kind


    END SELECT

  !------------------------------------------------------------------
  ! Open and read file containing cloud detection setup for the
  ! current instrument
  !------------------------------------------------------------------

  INIU1=107
  OPEN(INIU1,STATUS='OLD',FORM='FORMATTED', &
       FILE=TRIM(CL__Cloud_Detection_File), IOSTAT=IOS)
  IF (IOS == 0) THEN
    READ(INIU1,nml=Cloud_Detect_Coeffs,IOSTAT=IOS)
    IF (IOS == 0) THEN
      if ( verbose ) WRITE(*,'(3X,A)') TRIM(CL__InstrumentName) // &
           ' CLOUD DETECTION FILE READ OK'
    ELSE
      CALL CADS_Abort('PROBLEM READING '//TRIM(CL__InstrumentName)//&
                     'CLOUD DETECTION FILE')
    ENDIF
    CLOSE(INIU1)
  ELSE
    if ( verbose ) WRITE(*,'(3X,A)') 'NO '//TRIM(CL__InstrumentName) // &
             ' CLOUD DETECTION FILE : Using Default Values'
  ENDIF

  IF (MAXVAL(N__Band_Size(:)) > JP__Max_Channels) &
             CALL CADS_Abort('Too many channels specified in cloud '//&
                             'detection - increase JP__Max_Channels')


    M__Sensor = J__SENSOR

  !------------------------------------------------------------------
  ! Set up the S__CADS_Setup_Cloud structure for current sensor
  !------------------------------------------------------------------

    S__CADS_Setup_Cloud(J__SENSOR) % M__SENSOR = M__Sensor

    S__CADS_Setup_Cloud(J__SENSOR) % N__Num_Bands = N__Num_Bands

    ALLOCATE( S__CADS_Setup_Cloud(J__SENSOR) % N__Band_Size(N__Num_Bands) )

    S__CADS_Setup_Cloud(J__SENSOR) % N__Band_Size(:) = &
          N__Band_Size(1:N__Num_Bands)

    ALLOCATE(S__CADS_Setup_Cloud(J__SENSOR) % N__Bands &
          (MAXVAL(N__Band_Size(:)), N__Num_Bands))

    S__CADS_Setup_Cloud(J__SENSOR) % N__Bands(:,:) = 0

    DO J = 1, N__Num_Bands
      S__CADS_Setup_Cloud(J__SENSOR) % N__Bands(1:N__Band_Size(J),J) = &
            N__Bands(1:N__Band_Size(J),J)
    ENDDO

    ALLOCATE( S__CADS_Setup_Cloud(J__SENSOR) % N__Window_Width(N__Num_Bands) )

    S__CADS_Setup_Cloud(J__SENSOR) % N__Window_Width(:) = &
          N__Window_Width(1:N__Num_Bands)

    ALLOCATE( S__CADS_Setup_Cloud(J__SENSOR) % R__BT_Threshold(N__Num_Bands) )
    S__CADS_Setup_Cloud(J__SENSOR) % R__BT_Threshold(:) = &
          R__BT_Threshold(1:N__Num_Bands)

    ALLOCATE(S__CADS_Setup_Cloud(J__SENSOR) % R__Grad_Threshold(N__Num_Bands))
    S__CADS_Setup_Cloud(J__SENSOR) % R__Grad_Threshold(:) = &
          R__Grad_Threshold(1:N__Num_Bands)

    ALLOCATE(S__CADS_Setup_Cloud(J__SENSOR) % &
          R__Window_Grad_Threshold(N__Num_Bands))

    S__CADS_Setup_Cloud(J__SENSOR) % R__Window_Grad_Threshold(:) = &
          R__Window_Grad_Threshold(1:N__Num_Bands)

    ALLOCATE(S__CADS_Setup_Cloud(J__SENSOR) % N__GradChkInterval(N__Num_Bands))
    S__CADS_Setup_Cloud(J__SENSOR) % N__GradChkInterval(:) = &
          N__GradChkInterval(1:N__Num_Bands)

    ALLOCATE(S__CADS_Setup_Cloud(J__SENSOR) % N__Window_Bounds(N__Num_Bands,2))
    S__CADS_Setup_Cloud(J__SENSOR) % N__Window_Bounds(:,:) = &
          N__Window_Bounds(1:N__Num_Bands,:)

    S__CADS_Setup_Cloud(J__SENSOR) % L__Do_Quick_Exit = L__Do_Quick_Exit


  !-------------
  ! Cross Band
  !-------------

    S__CADS_Setup_Cloud(J__SENSOR) % L__Do_CrossBand = L__Do_CrossBand

    ALLOCATE( S__CADS_Setup_Cloud(J__SENSOR) % N__BandToUse(N__Num_Bands) )
    S__CADS_Setup_Cloud(J__SENSOR) % N__BandToUse(:) = &
          N__BandToUse(1:N__Num_Bands)


  !-------------
  ! Imager cloud detection
  !-------------

    S__CADS_Setup_Cloud(J__SENSOR) % L__Do_Imager_Cloud_Detection = &
         L__Do_Imager_Cloud_Detection

    S__CADS_Setup_Cloud(J__SENSOR) % N__Num_Imager_Chans = &
          N__Num_Imager_Chans

    S__CADS_Setup_Cloud(J__SENSOR) % N__Num_Imager_Clusters = &
          N__Num_Imager_Clusters

    ALLOCATE( S__CADS_Setup_Cloud(J__SENSOR) % &
          N__Imager_Chans(N__Num_Imager_Chans))
    S__CADS_Setup_Cloud(J__SENSOR) % N__Imager_Chans(:) = &
          N__Imager_Chans(1:N__Num_Imager_Chans)

    ALLOCATE( S__CADS_Setup_Cloud(J__SENSOR) % &
          R__Stddev_Threshold(N__Num_Imager_Chans))
    S__CADS_Setup_Cloud(J__SENSOR) % R__Stddev_Threshold(:) = &
          R__Stddev_Threshold(1:N__Num_Imager_Chans)

    S__CADS_Setup_Cloud(J__SENSOR) % R__Coverage_Threshold = &
          R__Coverage_Threshold

    S__CADS_Setup_Cloud(J__SENSOR) % R__FG_Departure_Threshold = &
          R__FG_Departure_Threshold

  ENDDO SensorLoop

END SUBROUTINE CADS_SETUP_CLOUD

SUBROUTINE CADS_Detect_Cloud(  K__Sensor,  K__NChans,  K__ChanID, K__Minlev, K__Maxlev, &
                             K__Num_Imager_Chans, K__Chan_ID_Imager, K__Num_Imager_Clusters, &
                             K__Cloud_Flag, P__ObsBTs, P__ModelBTs, P__Chan_Level, P__Cluster_Fraction,&
                             P__BT_in_Cluster, P__BT_Overall_SDev, P__BT_Model_Imager, Z__Cloud_Level )

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.

!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   * CADS_Detect_Cloud *
!   Phil Watts   ECMWF   21/01/02

!   * PURPOSE *
!   -----------
!   Flag the presence or otherwise of cloud contamination in AIRS/IASI
!   channels using a rank-sorted/model difference method. Currently
!   only a digital filter is supported.


!   * INTERFACE *
!   -------------
!   *CALL* * CADS_Detect_Cloud( )* (from CADS_Main)
!   WHERE K__Sensor              : Satellite sensor (AIRS/IASI/CrIS)
!         K__NChans              : Number of channels
!         K__ChanID              : Channel indices of input channels
!         K__Minlev              : Highest allowed starting point for the cloud search
!         K__Maxlev              : Lowest allowed starting point in the initial cloud search
!         K__Num_Imager_Chans    : Number of collocated imager channels
!         K__Chan_ID_Imager      : Collocated imager channel indices
!         K__Num_Imager_Clusters : Number of collocated clusters
!         K__Cloud_Flag          : Cloud flag by channel; 0=clear, 1=cloudy
!         P__ObsBTs              : Potentially cloud-affected observed BTs
!         P__ModelBTs            : Clear background brightness temperatures (BTs)
!         P__Chan_Level          : Channel height assignments
!         P__Cluster_Fraction    : Fractional coverage of each cluster within FOV
!         P__BT_in_Cluster       : Cluster-mean brightness temperature (BT) on each channel
!         P__BT_Overall_SDev     : Overall BT standard deviation on each channel
!         P__BT_Model_Imager     : Forward-modelled BT on each channel
!         Z__Cloud_Level         : Cloud height assignment

!   * EXTERNALS *
!   -------------
!   CADS_Detect_Cloud_Imager, CADS_Detect_Cloud_Heapsort,
!   CADS_Detect_Cloud_Smooth, CADS_Detect_Cloud_Scenario,
!   CADS_Detect_Cloud_Separator

!   * MODIFICATIONS *
!   -----------------
!   A.Collard   1.0   01/02/06   Original export version
!   A.Collard   1.0.1 03/05/06   Allow for missing channels
!   A.Collard   1.0.2 04/05/06   Allow cross-band cloud detection
!   A.Collard   1.0.3 15/01/07   Initialise with automatic cross-band for
!                                all channels from band 1 for IASI
!   R.Eresmaa   1.1   17/11/09   Include parameters of the Quick Exit /
!                                long-wave window gradient check.
!                                Pass K__Chan_Low to CF_DIGITAL to allow
!                                detecting cirrus in case of compensating
!                                humidity bg error in PBL.
!   R.Eresmaa   1.2   11/11/11   Modify the cross-band option to be based
!                                on the lowest clear channel rather than
!                                on the highest cloud-contaminated one
!   R.Eresmaa   2.0   27/11/13   Add input cloud flag based on collocated
!                                imager data
!   R.Eresmaa   2.1   13/01/15   Make array size specifications implicit.
!   R.Eresmaa   2.2   10/11/15   Instrument ID naming convention made
!                                consistent with RTTOV.
!                                Changed setting of the aerosol flag.
!   R.Eresmaa   2.2.1 13/11/15   Don't allow flagging missing channels clear
!                                through the cross-band option.
!   R.Eresmaa   2.3   20/12/16   Remove the call to aerosol detection.
!   R.Eresmaa   2.4   05/02/19   Explicit KIND specifications.
!   R.Eresmaa   3.0   16/04/20   Move the call to imager-based detection here.

  use kinds, only: i_kind, r_kind
  use gsi_io, only: verbose
  IMPLICIT NONE

!* 0.1 Global arrays
  INTEGER(i_kind), INTENT(IN)  :: K__Sensor              ! Sensor
  INTEGER(i_kind), INTENT(IN)  :: K__NChans              ! No. of channels
  INTEGER(i_kind), INTENT(IN)  :: K__ChanID(:)           ! Channel IDs
  INTEGER(i_kind), INTENT(IN)  :: K__Minlev              ! Highest starting point for cloud search
  INTEGER(i_kind), INTENT(IN)  :: K__Maxlev              ! Lowest starting point in the initial search
  INTEGER(i_kind), INTENT(IN)  :: K__Num_Imager_Chans    ! No. of imager channels
  INTEGER(i_kind), INTENT(IN)  :: K__Chan_ID_Imager(:)   ! Imager channel IDs
  INTEGER(i_kind), INTENT(IN)  :: K__Num_Imager_Clusters ! No. of imager clusters
  INTEGER(i_kind), INTENT(OUT) :: K__Cloud_Flag(:)       ! Output cloud flags
  REAL(r_kind),    INTENT(IN)  :: P__ObsBTs(:)           ! Observed BTs
  REAL(r_kind),    INTENT(IN)  :: P__ModelBTs(:)         ! Model clear BTs
  REAL(r_kind),    INTENT(IN)  :: P__Chan_Level(:)       ! Channel height assignments
  REAL(r_kind),    INTENT(IN)  :: P__Cluster_Fraction(:) ! Cluster coverages
  REAL(r_kind),    INTENT(IN)  :: P__BT_in_Cluster(:,:)  ! Mean BT in cluster / channel
  REAL(r_kind),    INTENT(IN)  :: P__BT_Overall_Sdev(:)  ! St.Dev of imager BT in FOV
  REAL(r_kind),    INTENT(IN)  :: P__BT_Model_Imager(:)  ! Model-based estimate of imager BT
  REAL(r_kind),    INTENT(OUT) :: Z__Cloud_Level         ! Cloud hight assignment

!* 0.2 local variables
  INTEGER(i_kind)              :: IST,ICOUNT,J,I_K,JBAND,JBAND2
  INTEGER(i_kind)              :: I__Imager_Flag   ! Preliminary cloud flag from collocated imager data

!* 0.3 Local variables - band splitting details
  INTEGER(i_kind), POINTER     :: I__Bands(:,:)            ! Channel bands
  INTEGER(i_kind), POINTER     :: I__Band_Size(:)          ! Number of channels per band
  INTEGER(i_kind), POINTER     :: I__BandToUse(:)          ! Cross-band definitions
  INTEGER(i_kind)              :: I__Num_Bands             ! Number of bands
  INTEGER(i_kind)              :: I__NumFoundChans         ! Number of usable channels
  INTEGER(i_kind)              :: I__BandNumber(K__NChans) ! Channel band indicator
  INTEGER(i_kind)              :: I__WindowBounds(2)       ! Boundary of window
  INTEGER(i_kind)              :: I__Window_Chans(2)       ! Boundary of long-wave window
  INTEGER(i_kind), ALLOCATABLE :: I__INDEX(:)              ! Channel ranking within a band
  INTEGER(i_kind), ALLOCATABLE :: IDCHAN(:)                ! Overall channel ranking
  INTEGER(i_kind), ALLOCATABLE :: I__Cloud_Flag(:)         ! Rank-sorted output cloud flags
  INTEGER(i_kind)              :: I__Scenario_Index        ! 1--Quick Exit, 2--Warm Start, 3--Cold Start
  INTEGER(i_kind)              :: I__Start_Channel         ! Final starting channel in the cloud search

  LOGICAL  :: LL__Do_CrossBand

! Input array projections (handling one detection band at a time)
  REAL(r_kind), ALLOCATABLE    :: Z__DBT(:)        ! Original departures
  REAL(r_kind), ALLOCATABLE    :: Z__Smooth_DBT(:) ! Smoothed departures
  REAL(r_kind), ALLOCATABLE    :: Z__LEVEL(:)      ! Channel height assignments

!* 0.4 Local variables - digital filter parameters
  INTEGER(i_kind)              :: I__CHAN_HIGH          ! Channel at K__Minlev
  INTEGER(i_kind)              :: I__CHAN_LOW           ! Channel at K__Maxlev
  INTEGER(i_kind)              :: I__FirstCloudyChannel ! Highest cloud-affected channel
  INTEGER(i_kind)              :: I__LastClearChannel   ! Lowest clear channel
  INTEGER(i_kind),POINTER      :: I__Window_Width(:)    ! Box-car filter width
  INTEGER(i_kind),POINTER      :: I__GradChkInterval(:) ! Gradient-check interval

!======================================================================


! Get correct processing parameters for this sensor:
  I__Num_Bands       =  S__CADS_Setup_Cloud(K__Sensor) % N__Num_Bands
  I__Band_Size       => S__CADS_Setup_Cloud(K__Sensor) % N__Band_Size
  I__Bands           => S__CADS_Setup_Cloud(K__Sensor) % N__Bands
  I__Window_Width    => S__CADS_Setup_Cloud(K__Sensor) % N__Window_Width
  I__BandToUse       => S__CADS_Setup_Cloud(K__Sensor) % N__BandToUse
  LL__Do_CrossBand   =  S__CADS_Setup_Cloud(K__Sensor) % L__Do_CrossBand
  I__GradChkInterval => S__CADS_Setup_Cloud(K__Sensor) % N__GradChkInterval


! Initialise
  K__Cloud_Flag(:)=1       ! intialise ALL channels to cloudy


! Imager-based cloud detection
  I__Imager_Flag=0 ! Default assumption: no cloud affecting collocated imager data
  CALL CADS_Detect_Cloud_Imager(  K__Sensor, K__Num_Imager_Chans, K__Chan_ID_Imager, K__Num_Imager_Clusters, &
               I__Imager_Flag, P__Cluster_Fraction, P__BT_in_Cluster, P__BT_Overall_SDev, P__BT_Model_Imager )

! If using cross-band, set up an array indicating which channels correspond
! to which bands in K__ChanID
  IF (LL__Do_CrossBand) THEN
     I__BandNumber(:)=-1  ! Initialise
     DO JBAND = 1, I__Num_Bands
        DO I_K=1,K__NChans
           IF (ANY(I__BANDS(:,JBAND) == K__ChanID(I_K))) &
                 I__BandNumber(I_K)=JBand
        ENDDO
     ENDDO
  ENDIF


!1 Loop over bands
  Band_Loop: DO JBAND = 1, I__Num_Bands

  ! Don't bother doing the cloud detection if we're just going to use
  ! the results from another band anyway:
    IF (LL__Do_CrossBand) THEN
       IF (.NOT.(ANY(I__BandToUse(:) == JBAND))) CYCLE
    ENDIF

    ALLOCATE (Z__DBT(I__Band_Size(JBAND)))
    Z__DBT(:) = 0.0_r_kind

    ALLOCATE (Z__LEVEL(I__Band_Size(JBAND)))
    Z__LEVEL(:) = REAL(K__Maxlev)

    ALLOCATE (I__Cloud_Flag(I__Band_Size(JBAND)))
    ALLOCATE (I__INDEX(I__Band_Size(JBAND)))

    ALLOCATE (IDCHAN(I__Band_Size(JBAND)))
    IDCHAN(:) = 1


    I__WindowBounds(:)   = &
       S__CADS_Setup_Cloud(K__Sensor) % N__Window_Bounds(JBand,:)

!1.1 find channels within current band --------------------------------------
    I__NumFoundChans = 0
    I__Window_Chans(:) = -1

    DO J=1,I__Band_Size(JBAND)
      DO I_K=1,K__NChans
        IF (K__ChanID(I_K) == I__BANDS(J,JBAND)) THEN
!        IF (P__ObsBTs(I_K) < 0. .OR. P__ModelBTs(I_K) < 0.) CYCLE
          IF (P__ObsBTs(I_K) < 60.0_r_kind .OR. P__ModelBTs(I_K) < 60.0_r_kind) CYCLE    ! Missing channels are set to 50.0K
          I__NumFoundChans = I__NumFoundChans + 1
          Z__DBT(I__NumFoundChans)=P__ObsBTs(I_K)-P__ModelBTs(I_K)
          Z__LEVEL(I__NumFoundChans)=P__Chan_Level(I_K)
          I__INDEX(I__NumFoundChans)=I__NumFoundChans
          IDCHAN(I__NumFoundChans)=I_K
          IF (K__ChanID(I_K) == I__WindowBounds(1)) &
               I__Window_Chans(1) = I__NumFoundChans
          IF (K__ChanID(I_K) == I__WindowBounds(2)) &
               I__Window_Chans(2) = I__NumFoundChans
        ENDIF
      ENDDO
    ENDDO
    IF ( I__NumFoundChans == 0 ) THEN
      if (verbose) WRITE(*,*) &
          '**CADS_Detect_Cloud - WARNING: ' // &
          'CHANNELS NOT FOUND CYCLING BAND: **', JBAND
      IF (ALLOCATED(Z__DBT))        DEALLOCATE (Z__DBT)
      IF (ALLOCATED(Z__LEVEL))      DEALLOCATE (Z__LEVEL)
      IF (ALLOCATED(I__Cloud_Flag)) DEALLOCATE (I__Cloud_Flag)
      IF (ALLOCATED(I__INDEX))      DEALLOCATE (I__INDEX)
      IF (ALLOCATED(IDCHAN))        DEALLOCATE (IDCHAN)
      CYCLE Band_Loop
    ENDIF

!----------------------------------------------------------------------------
    IST=0
    ICOUNT=I__NumFoundChans
    I__Cloud_Flag(:)=1

!2. Sort according to channel height assignments
    CALL CADS_Detect_Cloud_Heapsort(I__NumFoundChans,Z__Level,I__Index)

!2.1 Find I__CHAN_LOW - lowest channel considered in the initial cloud search
    J=1
    DO WHILE (J < I__NumFoundChans .AND. Z__Level(I__Index(J)) < REAL(K__Maxlev))
      J=J+1
    ENDDO

    IF (J == I__NumFoundChans) THEN
      I__CHAN_LOW = I__NumFoundChans-1
    ELSE
      I__CHAN_LOW = J
    ENDIF
    IF(I__CHAN_LOW <= 1)I__CHAN_LOW=1

!2.1a Find I__CHAN_HIGH - highest allowed channel for starting the cloud search
    J=1
    DO WHILE (J < I__NumFoundChans .AND. Z__Level(I__Index(J)) < REAL(K__Minlev))
      J=J+1
    ENDDO
    I__CHAN_HIGH=J


! Smoothing
    ALLOCATE (Z__Smooth_DBT(I__NumFoundChans))
    Z__Smooth_DBT(:) = 0.0_r_kind

    CALL CADS_Detect_Cloud_Smooth( I__NumFoundChans, I__Window_Width(JBAND), Z__DBT(I__INDEX(1:I__NumFoundChans)), & 
                                 Z__Smooth_DBT(1:I__NumFoundChans) )


!3. Choice of cloud detection scenario

    CALL CADS_Detect_Cloud_Scenario( K__Sensor, JBAND, I__NumFoundChans, I__GradChkInterval(JBAND), I__Index(1:I__NumFoundChans), &
        I__CHAN_HIGH, I__CHAN_LOW, I__Window_Chans, I__Imager_Flag, I__Scenario_Index, I__Start_Channel, Z__Smooth_DBT(1:I__NumFoundChans))


!4. Identify the separation between clear/cloudy channels

    CALL CADS_Detect_Cloud_Separator( K__Sensor, JBAND, I__NumFoundChans, I__GradChkInterval(JBAND), I__Index(1:I__NumFoundChans), &
        I__Cloud_Flag, I__FirstCloudyChannel, I__LastClearChannel, I__Scenario_Index, I__Start_Channel, Z__Smooth_DBT(1:I__NumFoundChans))

    K__Cloud_Flag(IDCHAN(1:I__NumFoundChans)) = &
                        I__Cloud_Flag(1:I__NumFoundChans)

  ! Set cloud level for cross-band:
    IF (I__FirstCloudyChannel == 0) THEN   ! FOV is completely clear
      Z__Cloud_Level = 1.e20_r_kind   ! Large value
    ELSE
      Z__Cloud_Level = P__Chan_Level(IDCHAN(I__LastClearChannel))
    ENDIF

  ! Automatically do cross band cloud detection for all
  ! interferometer channels (whether assigned a band or not) if
  ! JBand == 1. This can be over-ridden for the other bands.

    IF (K__Sensor /= INST_ID_AIRS .AND. JBand == 1) &
        WHERE(P__Chan_Level(:) < Z__Cloud_Level) K__Cloud_Flag(:) = 0

    CrossBand : IF (LL__Do_CrossBand) THEN
    ! Cross Band:
    ! Loop through bands applying cloud detection to those that take their
    ! cloud detection information from the current band JBAND.
      DO JBand2 = 1, I__Num_Bands
        IF (I__BandToUse(JBand2) == JBand) THEN
          WHERE(P__Chan_Level(:) < Z__Cloud_Level .AND. &
               I__BandNumber == JBand2 .AND. &
               P__OBSBTs(:)>0.0_r_kind ) K__Cloud_Flag(:) = 0
        ENDIF
      ENDDO
    ENDIF CrossBand

! Deallocate arrays
    IF (ALLOCATED(Z__DBT))        DEALLOCATE (Z__DBT)
    IF (ALLOCATED(Z__Smooth_DBT)) DEALLOCATE (Z__Smooth_DBT)
    IF (ALLOCATED(Z__LEVEL))      DEALLOCATE (Z__LEVEL)
    IF (ALLOCATED(I__Cloud_Flag)) DEALLOCATE (I__Cloud_Flag)
    IF (ALLOCATED(I__INDEX))      DEALLOCATE (I__INDEX)
    IF (ALLOCATED(IDCHAN))        DEALLOCATE (IDCHAN)

  ENDDO Band_Loop

! Nullify pointers
  NULLIFY(I__Band_Size, I__Bands, I__Window_Width, I__BandToUse)

END SUBROUTINE CADS_Detect_Cloud

SUBROUTINE CADS_Detect_Cloud_Imager( K__Sensor,  K__Nchans,  K__Chanid, K__Nclust, K__Cloud_Flag,  P__Cl_Fraction, &
                                     P__Cl_Mean, P__Ov_Stddev,  P__FG_BT )

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   *CADS_Detect_Cloud_Imager*
!   R.Eresmaa   ECMWF   12/02/13

!   * PURPOSE *
!   -----------
!   Provide additional information for the cloud detection by making use
!   of collocated imager data, such as AVHRR collocated with IASI.

!   * INTERFACE *
!   -------------
!   *CALL* * CADS_Detect_Cloud_Imager( )* (from CADS_Detect_Cloud)
!   WHERE K__Sensor      : Satellite sensor id
!         K__Nchans      : Number of channels received as input
!         K__Chanid      : Provided channel IDs
!         K__Nclust      : Highest possible number of clusters
!         K__Cloud_Flag  : Output cloud flag (0-7, 0=clear)
!         P__Cl_Fraction : Fractional coverage of each cluster within FOV
!         P__Cl_Mean     : Cluster-mean brightness temperature (BT) on each
!                          channel
!         P__Ov_Stddev   : Overall BT standard deviation on each channel
!         P__FG_BT       : Forward-modelled BT on each channel

!   * METHOD *
!   ----------
!   A preliminary indicator of presence of clouds in the sounder
!   field-of-view (FOV) is derived using statistical radiance information
!   within collocated clusters of imager pixels.

!   * MODIFICATIONS *
!   -----------------
!   03/12/13   R.Eresmaa   2.0   Original export version.
!   19/01/15   R.Eresmaa   2.1   Make array size specifications implicit.
!                                Verify that channels intended to be used
!                                are received as input.
!   05/02/19   R.Eresmaa   2.4   Explicit kind specifications.
!   16/04/20   R.Eresmaa   3.0   Rename and tidy up.

  use kinds, only: i_kind, r_kind
  IMPLICIT NONE

!* Global arrays
  INTEGER(i_kind), INTENT(IN)  :: K__Sensor         ! Sensor id
  INTEGER(i_kind), INTENT(IN)  :: K__Nchans         ! No. of channels
  INTEGER(i_kind), INTENT(IN)  :: K__Chanid(:)      ! Channel IDs
  INTEGER(i_kind), INTENT(IN)  :: K__Nclust         ! No. of clusters
  INTEGER(i_kind), INTENT(OUT) :: K__Cloud_Flag     ! Output cloud flag
  REAL(r_kind),    INTENT(IN)  :: P__Cl_Fraction(:) ! Cluster fractions
  REAL(r_kind),    INTENT(IN)  :: P__Cl_Mean(:,:)   ! Cluster-mean BTs
  REAL(r_kind),    INTENT(IN)  :: P__Ov_Stddev(:)   ! Overall BT st.devs.
  REAL(r_kind),    INTENT(IN)  :: P__FG_BT(:)       ! First guess BT

!* Local variables - Setup of the imager cloud detection
  INTEGER(i_kind)          :: I__Num_Imager_Chans       ! No. of used channels
  INTEGER(i_kind), POINTER :: I__Imager_Chans(:)        ! List of used channels
  REAL(r_kind), POINTER    :: Z__Stddev_Threshold(:)    ! Homogeneity thresholds
  REAL(r_kind)             :: Z__Coverage_Threshold     ! Coverage threshold
  REAL(r_kind)             :: Z__FG_Departure_Threshold ! FG departure threshold

!* Additional local variables
  INTEGER(i_kind) :: I, J, IK, I_Temp_Flag, ICOUNT
  INTEGER(i_kind) :: I__Chan_Index(K__Nchans)
  REAL(r_kind) :: Z__Wsqdev, Z__Intercluster
  REAL(r_kind),dimension(K__Nclust) :: Z__Sqdev



!* 1.0 Initialize cloud flags as clear

  K__Cloud_Flag=0

  IF (S__CADS_Setup_Cloud(K__Sensor) % L__Do_Imager_Cloud_Detection) THEN


!* 1.1 Setup

    I__Num_Imager_Chans       = &
       S__CADS_Setup_Cloud(K__Sensor) % N__Num_Imager_Chans
    I__Imager_Chans           => &
       S__CADS_Setup_Cloud(K__Sensor) % N__Imager_Chans
    Z__Stddev_Threshold       => &
       S__CADS_Setup_Cloud(K__Sensor) % R__Stddev_Threshold
    Z__Coverage_Threshold     = &
       S__CADS_Setup_Cloud(K__Sensor) % R__Coverage_Threshold
    Z__FG_Departure_Threshold = &
       S__CADS_Setup_Cloud(K__Sensor) % R__FG_Departure_Threshold



!* 1.2 Channel indexing
    I__Chan_Index(:) = 0
    ICOUNT=0
    DO I=1,K__Nchans
      IK=0
      DO J=1,I__Num_Imager_Chans
        IF (K__Chanid(I)==I__Imager_Chans(J)) THEN
          ICOUNT=ICOUNT+1
          IK=ICOUNT
          EXIT
        ENDIF
      ENDDO
      I__Chan_Index(I)=IK
    ENDDO


!* 2.0 Compute squared first guess departures for each cluster

    DO J=1,K__Nclust
      Z__Sqdev(J) = 0.0_r_kind
      DO I=1,K__Nchans
        IF (I__Chan_Index(I)==0) CYCLE
        Z__Sqdev(J) = Z__Sqdev(J) + (P__Cl_Mean(I,J)-P__FG_BT(I))**2
      ENDDO
    ENDDO

!* 2.1 Homogeneity check: Do not diagnose presence of cloud if BT
!      standard deviation falls below given threshold on at least one
!      channel.

    I_Temp_Flag=1
    DO I=1,K__Nchans
      IF (I__Chan_Index(I)==0) CYCLE
      IF (P__Ov_Stddev(I)<Z__Stddev_Threshold(I__Chan_Index(I))) I_Temp_Flag=0
    ENDDO

    IF (I_Temp_Flag==1) K__Cloud_Flag=K__Cloud_Flag+4


!* 2.2 Consistency check: Do not diagnose presence of cloud if all
!      major clusters are consistent with each other, i.e., they are
!      closer to each other than to first guess.

    Consistency_Check : DO J=2,K__Nclust
      IF (P__Cl_Fraction(J)<Z__Coverage_Threshold) CYCLE Consistency_Check
      DO IK=1,J-1
        IF (P__Cl_Fraction(IK)<Z__Coverage_Threshold) CYCLE
        Z__Intercluster = 0.0_r_kind
        DO I=1,K__Nchans
          IF (I__Chan_Index(I)==0) CYCLE
          Z__Intercluster = Z__Intercluster + &
                        (P__Cl_Mean(I,J)-P__Cl_Mean(I,IK))**2
        ENDDO
        IF (Z__Intercluster>Z__Sqdev(J) .OR. Z__Intercluster>Z__Sqdev(IK)) THEN
          K__Cloud_Flag=K__Cloud_Flag+2
          Exit Consistency_Check
        ENDIF
      ENDDO
    ENDDO Consistency_Check


!* 2.3 First guess departure check: Do not diagnose presence of cloud
!      if fraction-weighted first guess departure falls below given
!      threshold.

    Z__Wsqdev = SUM(P__Cl_Fraction(:)*Z__Sqdev(:))
    IF (Z__Wsqdev>=Z__FG_Departure_Threshold) K__Cloud_Flag=K__Cloud_Flag+1

  ENDIF   ! L__Do_Imager_Cloud_Detection

END SUBROUTINE CADS_Detect_Cloud_Imager


SUBROUTINE CADS_Detect_Cloud_Heapsort(N, A, K_Index)

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   * CADS_Detect_Cloud_Heapsort *
!   A.Collard   ECMWF   01/02/06

!   * PURPOSE *
!   -----------
!   Basic heapsort algorithm.

!   * INTERFACE *
!   -------------
!   *CALL* * CADS_Detect_Cloud_Heapsort( )* (from CADS_Detect_Cloud)
!   WHERE N       : Length of input array
!         A       : Real input array
!         K_Index : Output ranked array

!   * MODIFICATIONS *
!   -----------------
!   16/05/06   A.Collard   1.0   Original version.
!   05/02/19   R.Eresmaa   2.4   Explicit KIND specifications
!   16/04/20   R.Eresmaa   3.0   Rename as part of the big clean for CADS V3


  use kinds, only: i_kind, r_kind
  IMPLICIT NONE

! Subroutine arguments
  INTEGER(i_kind), INTENT(IN)    :: N
  REAL(r_kind),    INTENT(IN)    :: A(:)
  INTEGER(i_kind), INTENT(INOUT) :: K_Index(:)

  INTEGER(i_kind) :: I,J,RIGHT,LEFT,IDX
  REAL(r_kind) :: TMP

!------------------------------------------

  IF (N <= 1) RETURN
  LEFT  = N/2+1
  RIGHT = N

  DO
    IF (LEFT > 1) THEN
      LEFT = LEFT - 1
      IDX  = K_Index(LEFT)
    ELSE
      IDX = K_Index(RIGHT)
      K_Index(RIGHT) = K_Index(1)
      RIGHT = RIGHT - 1
      IF (RIGHT == 1) THEN
        K_Index(1) = IDX
        EXIT
      ENDIF
    ENDIF
    TMP = A(IDX)
    I = LEFT
    J = 2*LEFT
    DO WHILE (J <= RIGHT)
      IF (J < RIGHT) THEN
        IF (A(K_Index(J)) < A(K_Index(J+1))) J = J + 1
      ENDIF
      IF (TMP < A(K_Index(J))) THEN
        K_Index(I) = K_Index(J)
        I = J
        J = 2*J
      ELSE
        J = RIGHT + 1
      ENDIF
    ENDDO
    K_Index(I) = IDX
  ENDDO

END SUBROUTINE CADS_Detect_Cloud_Heapsort

SUBROUTINE CADS_Detect_Cloud_Smooth(KV,KW,PV,PVA)

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   * CADS_Detect_Cloud_Smooth * - Boxcar-averaging in a REAL array
!   * Phil Watts  ECMWF 24/01/02

!   * PURPOSE *
!   -----------
!   Calculate the moving average (smoothing filter) of array
!   No error checking supplied.

!   * INTERFACE *
!   -------------
!   *CALL* * CADS_Detect_Cloud_Smooth( )* (from CADS_Detect_Cloud)
!   WHERE KV    : Number of elements in V
!         KW    : Window width for filter
!         PV    : Input array to be averaged
!         PVA   : Averaged array

!   * MODIFICATIONS *
!   -----------------
!   01/02/06   A.Collard   1.0   Original export version.
!   13/01/15   R.Eresmaa   2.1   Make array size specifications implicit.
!   05/02/19   R.Eresmaa   2.4   Explicit KIND specifications.
!   16/04/20   R.Eresmaa   3.0   Rename and tidy up.

  use kinds, only: i_kind, r_kind
  IMPLICIT NONE

!* 0.1 global variables
  INTEGER(i_kind), INTENT(IN)    :: KV      ! length of V
  INTEGER(i_kind), INTENT(IN)    :: KW      ! length of averaging window
  REAL(r_kind),    INTENT(IN)    :: PV(:)   ! original array
  REAL(r_kind),    INTENT(INOUT) :: PVA(:)  ! averaged array

!* 0.2 local variables
  INTEGER(i_kind) :: INJ,J,I

  PVA(:)=0.0_r_kind

  DO I = 1,KV  ! loop over array elements
    INJ=0
    DO J=I-KW/2,I+KW/2,1  ! loop over window
      IF (J > 0 .AND. J < (KV+1)) THEN ! if window element exists in
                                     ! original array
        INJ=INJ+1
        PVA(I)=PVA(I)+PV(J)            ! add value
      ENDIF
    ENDDO
    PVA(I)=PVA(I)/REAL(INJ)            ! mean value
  ENDDO

END SUBROUTINE CADS_Detect_Cloud_Smooth

SUBROUTINE CADS_Detect_Cloud_Scenario( K__Sensor, K__Band, K__NumChans, K__GradChkInterval, K__Index, K__Chan_High, &
                        K__Chan_Low, K__Chan_Windows, K__Imager_Flag, K__Scen_Index, K__Start_Channel, P__DBT)

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   * CADS_Detect_Cloud_Scenario *
!   PHIL WATTS   ECMWF   21/01/02

!   * PURPOSE *
!   -----------
!   Determine which of the three possible scenarios best describes
!   the input data.
!   Quick Exit - no cloud in the FOV
!   Warm Start - warm cloud above relatively colder surface
!   Cold Start - cold cloud above relatively warmer surface (most common)

!   * INTERFACE *
!   -------------
!   * CALL* * CADS_Detect_Cloud_Scenario( )* (from CADS_Detect_Cloud)
!   WHERE K__Sensor          : Satellite sensor (AIRS/IASI/CrIS)
!         K__Band            : Band number
!         K__NumChans        : Number of channels in this band
!         K__GradChkInterval : Gradient-checking interval
!         K__Index           : Ranking index for the input dBT signal
!         K__Chan_High       : High channel considered in initial minimum search
!         K__Chan_Low        : Low channel considered in initial minimum search
!         K__Chan_Windows    : Two channels defining longwave window
!         K__Imager_Flag     : Input flag from collocated imager data
!         K__Scen_Index      : Choice of cloud detection scenario (1, 2, or 3)
!         K__Start_Channel   : Channel index for the start of final search
!         P__DBT             : Input dBT signal

!    * MODIFICATIONS *
!    -----------------
!    03/02/06   A.Collard   1.0   Tidy up in preparation for IASI
!    03/05/06   A.Collard   1.0.1 Band size is now passed in (allows for
!                                 missing channels).
!    04/05/06   A.Collard   1.0.2 The index of the first cloudy channel is now
!                                 returned to allow cross-band cloud detection
!    16/02/07   A.Collard   1.0.3 Change to the padding to allow the bottom
!                                 channel to be flagged as clear in a
!                                 non-quickstart situation.
!    16/01/09   A.Collard   1.1   Gradient check on quick exit
!                                 Start channel for cold start moved to highest
!                                 channel where BT threshold exceeded
!    11/11/11   R.Eresmaa   1,2   Index of the lowest clear channel added to
!                                 the output parameters.
!                                 Change of the starting channel is no longer
!                                 allowed in cases where gradient > -threshold.
!    04/12/13   R.Eresmaa   2.0   Allow quick exit only if collocated imager
!                                 data supports hypothesis of a clear FOV
!    13/01/15   R.Eresmaa   2.1   Remove the need to create temporary array in
!                                 the call to MOVINGA.
!                                 the call to MOVINGA.
!    04/02/19   R.Eresmaa   2.4   Explicit KIND specifications.
!    16/04/20   R.Eresmaa   3.0   Divide the previous CF_Digital in two:
!                                 Cloud_Scenario (here) and Cloud_Separator.


  use kinds, only: i_kind, r_kind
  IMPLICIT NONE

!* 0.1 Global arrays
  INTEGER(i_kind), INTENT(IN) :: K__SENSOR           ! Sensor
  INTEGER(i_kind), INTENT(IN) :: K__Band             ! Band number
  INTEGER(i_kind), INTENT(IN) :: K__NumChans         ! Number of usable channels in band
  INTEGER(i_kind), INTENT(IN) :: K__GradChkInterval  ! Gradient-check interval
  INTEGER(i_kind), INTENT(IN) :: K__INDEX(:)         ! Ranking index for dBT
  INTEGER(i_kind), INTENT(IN) :: K__Chan_High        ! First channel clear of high stratospheric model errors
  INTEGER(i_kind), INTENT(IN) :: K__Chan_Low         ! Last channel clear of PBL humidity errors
  INTEGER(i_kind), INTENT(IN) :: K__Chan_Windows(2)  ! Two channels defining long-wave window bounds
  INTEGER(i_kind), INTENT(IN)    :: K__Imager_Flag   ! Input imager cloud flag
  INTEGER(i_kind), INTENT(OUT)   :: K__Scen_Index    ! Choice of scenario
  INTEGER(i_kind), INTENT(OUT)   :: K__Start_Channel ! Final starting channel
  REAL(r_kind), INTENT(IN)       :: P__DBT(:)        ! Input ranked-smoothed dBT signal

! Local variables
  REAL(r_kind), ALLOCATABLE :: Z__DBT_w_Buffer(:) ! Smoothed-ranked DBT
  INTEGER(i_kind) :: I__Buffer                    ! No. of buffer channels
  INTEGER(i_kind) :: I__Start_Channel             ! Primary starting channel for cloud search
  INTEGER(i_kind) :: I__Start_Channel_Surf        ! Secondary starting channel for cloud search
  INTEGER(i_kind) :: I__Max_Channel               ! Channel corresponding to maximum of the smoothed dBT
  INTEGER(i_kind) :: JCH,JMIN(1),JMAX(1),I

  LOGICAL :: LLCOLD, LL__WINDOW_GRAD_CHECK, LL__StartChannelChanged
  LOGICAL :: LL__Search_for_Cloud_Top

! These carry the values in S__CADS_Setup_Cloud
  REAL(r_kind)  :: Z__BT_Threshold          ! Solution contaminated threshold
  REAL(r_kind)  :: Z__Grad_Threshold        ! Gradient threshold at which to stop filter procession
  REAL(r_kind)  :: Z__Window_Grad_Threshold ! Gradient threshold for window check


!=============================================================================


  Z__BT_Threshold    = &
      S__CADS_Setup_Cloud(K__SENSOR) % R__BT_Threshold(K__Band)
  Z__Grad_Threshold =  &
      S__CADS_Setup_Cloud(K__SENSOR) % R__Grad_Threshold(K__Band)
  Z__Window_Grad_Threshold = &
      S__CADS_Setup_Cloud(K__SENSOR) % R__Window_Grad_Threshold(K__Band)


!1. Include buffer channels at the start and end of the input smoothed
!   departure array

  I__BUFFER = K__GradChkInterval
  ALLOCATE(Z__DBT_w_Buffer(-I__Buffer+1:K__NumChans+1))

  Z__DBT_w_Buffer(1:K__NumChans) = P__DBT(:)
  Z__DBT_w_Buffer(-I__BUFFER+1:0) = Z__DBT_w_Buffer(1)
  Z__DBT_w_Buffer(K__NumChans+1) = Z__DBT_w_Buffer(K__NumChans)


!2.  Prepare for the cloud search

! First define a set of key channels

  JMIN=MINLOC(Z__DBT_w_Buffer(K__Chan_High:K__NumChans))
  I__Start_Channel_Surf = K__Chan_High+JMIN(1)-1

  JMIN=MINLOC(Z__DBT_w_Buffer(K__Chan_High:K__Chan_Low))
  I__Start_Channel = K__Chan_High+JMIN(1)-1

! Look for highest channel with DBT<-BT_Threshold and move I__Start_Channel
! there if higher than current I__Start_Channel:
  JCH = I__Start_Channel
  StartChanLoop : DO I=K__Chan_High,K__NumChans
     IF (Z__DBT_w_Buffer(I) < -Z__BT_Threshold .OR. I == I__Start_Channel) THEN
        JCH = I
        Exit StartChanLoop
     ENDIF
  ENDDO StartChanLoop
  I__Start_Channel = JCH

! Do the same with I__Start_Channel_Surf
  JCH = I__Start_Channel_Surf
  StartChanLoop_Surf : DO I=K__Chan_High,K__NumChans
     IF (Z__DBT_w_Buffer(I) < -Z__BT_Threshold .OR. I == I__Start_Channel_Surf) THEN
        JCH = I
        Exit StartChanLoop_Surf
     ENDIF
  ENDDO StartChanLoop_Surf
  I__Start_Channel_Surf = JCH

! Find the position of the equivalent maximum departure (for quick exit test)
  JMAX=MAXLOC(Z__DBT_w_Buffer(K__Chan_High:K__NumChans))
  I__Max_Channel = K__Chan_High+JMAX(1)-1

! Long-wave window gradient check
  LL__WINDOW_GRAD_CHECK=.TRUE.
  IF (ALL(K__Chan_Windows > 0)) LL__WINDOW_GRAD_CHECK = &
      (ABS(Z__DBT_w_Buffer(K__INDEX(K__Chan_Windows(1))) - &
      Z__DBT_w_Buffer(K__INDEX(K__Chan_Windows(2)))) &
      < Z__Window_Grad_Threshold)

! Choose scenario to be followed
  LL__Search_for_Cloud_Top=.TRUE.
  IF (ABS(Z__DBT_w_Buffer(I__Start_Channel_Surf)) < Z__BT_Threshold .AND. &
      ABS(Z__DBT_w_Buffer(I__Start_Channel)) < Z__BT_Threshold .AND. &
      ABS(Z__DBT_w_Buffer(I__Max_Channel)) < Z__BT_Threshold .AND. &
      ABS(Z__DBT_w_Buffer(K__NumChans)) < Z__BT_Threshold .AND. &
      LL__WINDOW_GRAD_CHECK .AND. &
      K__Imager_Flag==0 .AND. &
      S__CADS_Setup_Cloud(K__SENSOR) % L__Do_Quick_Exit) THEN
   !Quick exit
      LL__Search_for_Cloud_Top=.FALSE.
  ELSEIF (ABS(Z__DBT_w_Buffer(I__Start_Channel)) < Z__BT_Threshold .AND. &
      Z__DBT_w_Buffer(K__NumChans) > Z__BT_Threshold ) THEN
   !Warm cloud start at next-to-bottom channel (allowing one channel for
   !gradient calculations).
     LLCOLD = .FALSE.
     I__Start_Channel = K__NumChans-1
  ELSEIF (Z__DBT_w_Buffer(I__Start_Channel) < -Z__BT_Threshold ) THEN
     LLCOLD = .TRUE.
  ELSEIF (Z__DBT_w_Buffer(I__Start_Channel) > Z__BT_Threshold ) THEN
     LLCOLD = .FALSE.
  ELSE
     LLCOLD = .TRUE.
  ENDIF

  IF (LL__Search_for_Cloud_Top) THEN  ! Either cold or warm start
                                      ! (but not quick exit)

    JCH=I__Start_Channel

! Re-evaluate the choice of scenario:
! If the primary starting channel appears clear, and the secondary
! starting channel is lower, start from the latter. In that case
! re-evaluate whether cold or warm start is more appropriate.
    IF (I__Start_Channel /= I__Start_Channel_Surf) THEN

      LL__StartChannelChanged  = .FALSE.
      IF (LLCOLD .AND. ( (Z__DBT_w_Buffer(JCH-1)-Z__DBT_w_Buffer(JCH+1)) < &
         Z__Grad_Threshold .AND. &
         Z__DBT_w_Buffer(JCH-K__GradChkInterval)-Z__DBT_w_Buffer(JCH+1) < &
         Z__Grad_Threshold .AND. &
         ABS(Z__DBT_w_Buffer(JCH)) < Z__BT_Threshold)) THEN
        I__Start_Channel = I__Start_Channel_Surf
        LL__StartChannelChanged  = .TRUE.
      ENDIF

      IF (LL__StartChannelChanged) THEN

        IF (ABS(Z__DBT_w_Buffer(I__Start_Channel)) < Z__BT_Threshold .AND. &
               Z__DBT_w_Buffer(K__NumChans) > Z__BT_Threshold ) THEN
        !Warm cloud start at next-to-bottom channel (allowing one channel for
        !gradient calculations).
          LLCOLD = .FALSE.
          I__Start_Channel = K__NumChans-1
        ELSEIF (Z__DBT_w_Buffer(I__Start_Channel) < -Z__BT_Threshold ) THEN
          LLCOLD = .TRUE.
        ELSEIF (Z__DBT_w_Buffer(I__Start_Channel) > Z__BT_Threshold ) THEN
          LLCOLD = .FALSE.
        ELSE
          LLCOLD = .TRUE.
        ENDIF
        JCH = I__Start_Channel

      ENDIF
    ENDIF

    IF (LLCOLD) THEN
      K__Scen_Index=3
    ELSE
      K__Scen_Index=2
    ENDIF
    K__Start_Channel = JCH

  ELSE

    K__Scen_Index=1
    K__Start_Channel=0

  ENDIF    ! Search for cloud top

  IF (ALLOCATED(Z__DBT_w_Buffer)) DEALLOCATE(Z__DBT_w_Buffer)

END SUBROUTINE CADS_Detect_Cloud_Scenario

SUBROUTINE CADS_Detect_Cloud_Separator( K__Sensor, K__Band, K__NumChans, K__GradChkInterval, K__Index, K__Cloud_Flag, &
           K__Cloud_Level, K__Clear_Level, K__Scen_Index, K__Start_Channel, P__DBT)

!   This software was developed within the context of the EUMETSAT
!   Satellite Application Facility on Numerical Weather Prediction
!   (NWP SAF), under the Cooperation Agreement dated 7 December 2016,
!   between EUMETSAT and the Met Office, UK, by one or more partners
!   within the NWP SAF. The partners in the NWP SAF are the Met
!   Office, ECMWF, DWD and MeteoFrance.
!
!   Copyright 2020, EUMETSAT, All Rights Reserved.

!   * CADS_Detect_Cloud_Separator *
!   PHIL WATTS   ECMWF   21/01/02

!   * PURPOSE *
!   -----------
!   Along the vertically-ranked and smoothed array of departures, find
!   the separating point at which all cloud-affected channels are on
!   one side and all clear channels are on the other side.

!   * INTERFACE *
!    ------------
!   * CALL* * CADS_Detect_Cloud_Separator( )* (from CADS_Detect_Cloud)
!   WHERE K__Sensor          : Satellite sensor (AIRS/IASI/CrIS)
!         K__Band            : Band number
!         K__NumChans        : Number of channels in this band
!         K__GradChkInterval : Gradient-checking interval
!         K__Index           : Ranking index for the input dBT signal
!         K__Cloud_Flag      : Cloud flag by channel; 0=clear, 1=cloudy
!         K__Cloud_Level     : Index of the highest cloud-contaminated channel
!         K__Clear_Level     : Index of the lowest clear channel
!         K__Scen_Index      : Choice of cloud detection scenario (1, 2, or 3)
!         K__Start_Channel   : Starting channel for the cloud search
!         P__DBT             : Input dBT signal

!    MODIFICATIONS
!    03/02/06   A.Collard   1.0   Tidy up in preparation for IASI
!    03/05/06   A.Collard   1.0.1 Band size is now passed in (allows for
!                                 missing channels).
!    04/05/06   A.Collard   1.0.2 The index of the first cloudy channel is now
!                                 returned to allow cross-band cloud detection
!    16/02/07   A.Collard   1.0.3 Change to the padding to allow the bottom
!                                 channel to be flagged as clear in a
!                                 non-quickstart situation.
!    16/01/09   A.Collard   1.1   Gradient check on quick exit
!                                 Start channel for cold start moved to highest
!                                 channel where BT threshold exceeded
!    11/11/11   R.Eresmaa   1,2   Index of the lowest clear channel added to
!                                 the output parameters.
!                                 Change of the starting channel is no longer
!                                 allowed in cases where gradient > -threshold.
!    04/12/13   R.Eresmaa   2.0   Allow quick exit only if collocated imager
!                                 data supports hypothesis of a clear FOV
!    13/01/15   R.Eresmaa   2.1   Remove the need to create temporary array in
!                                 the call to MOVINGA.
!    04/02/19   R.Eresmaa   2.4   Explicit KIND specifications.
!    16/04/20   R.Eresmaa   3.0   Divide the previous CF_Digital in two:
!                                 Cloud_Scenario and Cloud_Separator (here).

  use kinds, only: i_kind, r_kind
  IMPLICIT NONE

!* 0.1 Global arrays
  INTEGER(i_kind), INTENT(IN   ) :: K__SENSOR           ! Sensor
  INTEGER(i_kind), INTENT(IN   ) :: K__Band             ! Band number
  INTEGER(i_kind), INTENT(IN   ) :: K__NumChans         ! Number of usable channels in band
  INTEGER(i_kind), INTENT(IN   ) :: K__GradChkInterval  ! Gradient-check interval
  INTEGER(i_kind), INTENT(IN   ) :: K__INDEX(:)         ! Ranking index for dBT
  INTEGER(i_kind), INTENT(INOUT) :: K__Cloud_Flag(:) ! Cloud flags
  INTEGER(i_kind), INTENT(  OUT) :: K__Cloud_Level   ! Index of highest cloudy channel
  INTEGER(i_kind), INTENT(  OUT) :: K__Clear_Level   ! Index of lowest clear channel
  INTEGER(i_kind), INTENT(IN   ) :: K__Scen_Index    ! Choice of scenario
  INTEGER(i_kind), INTENT(IN   ) :: K__Start_Channel ! Choice of scenario
  REAL(r_kind),    INTENT(IN   ) :: P__DBT(:)        ! Input ranked dBT signal


! Local variables
  REAL(r_kind), ALLOCATABLE :: Z__DBT_w_Buffer(:) ! Smoothed-ranked DBT
  INTEGER(i_kind) :: I__Buffer                    ! No. of buffer channels
  INTEGER(i_kind) :: JCH

! These carry the values in S__CADS_Setup_Cloud
  REAL(r_kind)  :: Z__BT_Threshold          ! Solution contaminated threshold
  REAL(r_kind)  :: Z__Grad_Threshold        ! Gradient threshold at which to stop
                                          ! filter procession

!=============================================================================


  Z__BT_Threshold    = &
      S__CADS_Setup_Cloud(K__SENSOR) % R__BT_Threshold(K__Band)
  Z__Grad_Threshold =  &
      S__CADS_Setup_Cloud(K__SENSOR) % R__Grad_Threshold(K__Band)

  K__Cloud_Flag(:)=1

!1. Include buffer channels at the start and end of the input smoothed
!   departure array

  I__BUFFER = K__GradChkInterval
  ALLOCATE(Z__DBT_w_Buffer(-I__Buffer+1:K__NumChans+1))

  Z__DBT_w_Buffer(1:K__NumChans) = P__DBT(:)
  Z__DBT_w_Buffer(-I__BUFFER+1:0) = Z__DBT_w_Buffer(1)
  Z__DBT_w_Buffer(K__NumChans+1) = Z__DBT_w_Buffer(K__NumChans)


!2. Search for the lowest non-contaminated channel

  JCH = K__Start_Channel

  SELECT CASE (K__Scen_Index)

  CASE (1) ! Quick Exit
    K__Cloud_Level = 0

  CASE (2) ! Warm Start
! In the case of Warm Start, progress towards higher channels whilst
! -ve difference is decreasing
    DO WHILE ( ((Z__DBT_w_Buffer(JCH-1)-Z__DBT_w_Buffer(JCH+1)) < &
               -1.0_r_kind * Z__Grad_Threshold .OR. &
      (Z__DBT_w_Buffer(JCH-K__GradChkInterval)-Z__DBT_w_Buffer(JCH+1)) <  &
               -1.0_r_kind * Z__Grad_Threshold .OR. &
        ABS(Z__DBT_w_Buffer(JCH)) > Z__BT_Threshold) .AND. JCH > 1 )
      JCH = JCH-1
    ENDDO
    K__Cloud_Level = JCH

  CASE (3) ! Cold Start
! In the case of Cold Start, progress towards higher channels whilst
! -ve difference is decreasing
    DO WHILE (( (Z__DBT_w_Buffer(JCH-1)-Z__DBT_w_Buffer(JCH+1)) > &
               Z__Grad_Threshold .OR. &
       (Z__DBT_w_Buffer(JCH-K__GradChkInterval)-Z__DBT_w_Buffer(JCH+1)) > &
              Z__Grad_Threshold .OR. &
       ABS(Z__DBT_w_Buffer(JCH)) > Z__BT_Threshold) .AND. JCH > 1 )
      JCH = JCH-1
    ENDDO
    K__Cloud_Level = JCH

  CASE DEFAULT
  RETURN

  END SELECT

!3. Output channel indices for the highest cloud and lowest clear levels
  IF (K__Cloud_Level > 1) THEN
    K__Cloud_Flag(K__INDEX(1:K__Cloud_Level-1))=0
    K__Clear_Level=K__INDEX(K__Cloud_Level-1)
    K__Cloud_Level=K__INDEX(K__Cloud_Level)
  ELSEIF (K__Cloud_Level>0) THEN
    K__Clear_Level=K__INDEX(K__Cloud_Level)
    K__Cloud_Level=K__INDEX(K__Cloud_Level)
  ELSE
    K__Cloud_Flag(:)=0
  ENDIF

  IF (ALLOCATED(Z__DBT_w_Buffer)) DEALLOCATE(Z__DBT_w_Buffer)

END SUBROUTINE CADS_Detect_Cloud_Separator

subroutine cads_imager_calc(obstype,isis,nobs,nreal,nchanl,nsig,data_s,init_pass,mype, &
                             imager_cluster_fraction,imager_cluster_bt,imager_chan_stdev, imager_model_bt)

!$$$ subprogram documentation block
!
! subprogram: cads_imager_calc    compute model equivalent to the imager channels used by CADS
!   prgmmr: Jung
!
! abstract:  accumulate the data necessary to derive the model equivalent brightness temperatures 
!            used by the cloud and aerosol detection software for the imager cloud tests.
!
! program history log:
!
!
!
! subroutines included:
!
!
! input argument list: 
!
!    obstype - type of tb observation
!    isis - sensor/instrument/satellite id
!    nobs - number of observations
!    nreal - number of pieces of info (location, time, etc) per obs
!    nchanl - number of channels per obs
!    nsig - number of model layers
!    data_s - array containing input data information for a specific sensor
!    init_pass  - state of "setup" processing
!    mype - mpi task id
!
!  output argument list:

!    imager_cluster_fraction - CADS cluster fraction ( dimension 7)
!    imager_cluster_bt - avreage brightness temperature of a cluster
!    imager_chan stdev - brightness temperature standard deviation of the cluster
!    imager_model_bt - model derived brightness temperature 
!
!
!$$$ end documentation block

   use kinds, only: i_kind, r_kind
   use constants, only: zero
   use radiance_mod, only: rad_obs_type
   use radinfo, only: jpch_rad, nusis, crtm_coeffs_path, nsigradjac
   use crtm_interface, only: init_crtm, call_crtm, destroy_crtm, itime
   use obsmod, only: dval_use
   use gsi_nstcouplermod, only: nstinfo

   implicit none

   logical,                             intent(in)  :: init_pass
   character(len=10),                   intent(in)  :: obstype
   character(len=20),                   intent(in)  :: isis
   integer(i_kind),                     intent(in)  :: nobs, nreal, nchanl, nsig
   integer(i_kind),                     intent(in)  :: mype
   real(r_kind),dimension(nreal+nchanl,nobs),intent(in)  :: data_s
   real(r_kind),dimension(7,nobs),      intent(out) :: imager_cluster_fraction
   real(r_kind),dimension(2,7,nobs),    intent(out) :: imager_cluster_bt
   real(r_kind),dimension(2,nobs),      intent(out) :: imager_chan_stdev, imager_model_bt

! local variables
   integer(i_kind) :: jc, i, n
   integer(i_kind) :: itmp1_cads, itmp2_cads, nchanl_cads, maxinfo, dval_info, cads_info, error_status
   integer(i_kind),allocatable,dimension(:) :: ich_cads
   logical :: imager_spccoeff, imager_taucoeff
   real(r_kind) :: dtime, clw_guess, ciw_guess, rain_guess, snow_guess
   real(r_kind) :: trop5, tzbgr, dtsavg, sfc_speed 
   real(r_kind),dimension(nsig) :: qvp, tvp, qs, prsltmp
   real(r_kind),dimension(nsig+1) :: prsitmp
   real(r_kind),allocatable,dimension(:)   :: tsim_cads, emissivity_cads, chan_level_cads
   real(r_kind),allocatable,dimension(:)   :: ts_cads, emissivity_k_cads,data_s_cads
   real(r_kind),allocatable,dimension(:,:) :: ptau5_cads, temp_cads, wmix_cads, jacobian_cads
   character(len=80) :: spc_filename, tau_filename
   character(len=20) :: isis_cads
   character(len=10) :: obstype_cads

   type(rad_obs_type) :: radmod
   
   cads_info = 23
   dval_info = 0
   if (dval_use) dval_info = 2

   itmp1_cads = len(trim(obstype))
   itmp2_cads = len(trim(isis))

   if ( obstype == 'iasi' ) then
     isis_cads = 'avhrr3'//isis(itmp1_cads+1:itmp2_cads)
     obstype_cads = 'avhrr'
!    nchanl_cads = 3    !channels 3 - 5
   elseif ( obstype == 'cris' .or. obstype == 'cris-fsr' ) then
!    isis_cads = 'viirs-m'//isis(itmp1+1:itmp2) When naming convention becomes standarized with CrIS
     if ( isis == 'cris-fsr_npp' .or. isis == 'cris_npp' ) then
       isis_cads = 'viirs-m_npp'
     elseif ( isis == 'cris-fsr_n20' ) then
       isis_cads = 'viirs-m_n20'
       spc_filename = trim(crtm_coeffs_path)//trim(isis_cads)//'.SpcCoeff.bin'
       inquire(file=trim(spc_filename), exist=imager_spccoeff)
       if ( .not. imager_spccoeff ) isis_cads = 'viirs-m_j1'
     elseif ( isis == 'cris-fsr_n21' ) then
       isis_cads = 'viirs-m_n21'
       spc_filename = trim(crtm_coeffs_path)//trim(isis_cads)//'.SpcCoeff.bin'
       inquire(file=trim(spc_filename), exist=imager_spccoeff)
       if ( .not. imager_spccoeff ) isis_cads = 'viirs-m_j2'
     endif
     obstype_cads = 'viirs-m'
!    nchanl_cads = 5   ! channels 12 - 16
   endif

   spc_filename = trim(crtm_coeffs_path)//trim(isis_cads)//'.SpcCoeff.bin'
   inquire(file=trim(spc_filename), exist=imager_spccoeff)
   tau_filename = trim(crtm_coeffs_path)//trim(isis_cads)//'.TauCoeff.bin'
   inquire(file=trim(tau_filename), exist=imager_taucoeff)

! IF the RTM files exist allocate and setup various arrays for the RTM
   if ( imager_spccoeff .and.  imager_taucoeff) then
     nchanl_cads = 0
     do i=1,jpch_rad
       if (trim(isis_cads) == nusis(i)) then
         nchanl_cads = nchanl_cads +1
       endif
     end do

     allocate( ich_cads(nchanl_cads) )
     jc = 0
     do i=1,jpch_rad
       if (trim(isis_cads) == nusis(i)) then
         jc = jc +1
         ich_cads(jc) = i
       endif
     end do

     call init_crtm(init_pass,-99,mype,nchanl_cads,nreal,isis_cads,obstype_cads,radmod)

! Initialize variables needed for the infrared cloud and aerosol detection software
     allocate(data_s_cads(nreal+nchanl_cads),tsim_cads(nchanl_cads),emissivity_cads(nchanl_cads), &
            chan_level_cads(nchanl_cads),ptau5_cads(nsig,nchanl_cads),ts_cads(nchanl_cads),emissivity_k_cads(nchanl_cads), &
            temp_cads(nsig,nchanl_cads),wmix_cads(nsig,nchanl_cads), jacobian_cads(nsigradjac,nchanl_cads))

     do n = 1,nobs      ! loop to derive imager BTs for CADS
!    Extract analysis relative observation time.
       dtime = data_s(itime,n)
       maxinfo = nreal - cads_info - dval_info - nstinfo
       if ( sum(data_s(maxinfo+1:maxinfo+7,n)) > 0.90_r_kind ) then  ! imager cluster information exists for this profile
         data_s_cads = data_s(1:nreal+nchanl_cads,n)
         call call_crtm(obstype_cads,dtime,data_s_cads,nchanl_cads,nreal,ich_cads, &
              tvp,qvp,qs,clw_guess,ciw_guess,rain_guess,snow_guess,prsltmp,prsitmp, &
              trop5,tzbgr,dtsavg,sfc_speed,tsim_cads,emissivity_cads,chan_level_cads, &
              ptau5_cads,ts_cads,emissivity_k_cads,temp_cads,wmix_cads,jacobian_cads,error_status) 

!  Transfer imager data to arrays for qc_irsnd
         imager_cluster_fraction(1:7,n) = data_s(maxinfo+1:maxinfo+7,n)
         imager_cluster_bt(1,1:7,n) = data_s(maxinfo+8:maxinfo+14,n)
         imager_cluster_bt(2,1:7,n) = data_s(maxinfo+15:maxinfo+21,n)
         imager_chan_stdev(1:2,n) = data_s(maxinfo+22:maxinfo+23,n)
         imager_model_bt(1:2,n) = tsim_cads(nchanl_cads-1:nchanl_cads)
       endif ! imager information exists 
     end do   ! End loop to derive imager BTs

     call destroy_crtm
     deallocate(data_s_cads,tsim_cads,emissivity_cads, ich_cads,chan_level_cads,ptau5_cads,&
            ts_cads,emissivity_k_cads, temp_cads,wmix_cads, jacobian_cads)
   endif  ! RTM files exist

 end subroutine cads_imager_calc

end module cads
