!***********************************************************************
! File co2hc.f
!***********************************************************************
! File history
! Feb 28, 2008: changes by RAA in subroutine co2cc in O interpolation
!     to parameterization grid
! Jan 2007: Made by Rashid Akmaev from three predecessor files:
!     co2pro.f
!     qnir1.f
!     co2c.f
! Apr 06 2012   Henry Juang, initial implement for NEMS
! Oct 12 2012   Jun Wang     change reading files by 1 pe reading and 
!                            broardcasting to all pes
!
! Contains modules and subroutines
!     1) To create a global mean vertical CO2 profile either according
!       to Fomichev et al. (1998) model or (loosely) according to 
!       CRISTA-1/2 satellite data of Grossmann (2005) and Kostsov 
!       (2005), both personal communications.
!     2) To calculate IR CO2 cooling in 15-mu band by an updated version
!       of Fomichev et al. (1998) code
!     3) To calculate CO2 heating rates in the near infrared (Ogibalov
!       and Fomichev, 2003).
!
! Contains
!     module co2pro_mod - data for global CO2 profiles
!     module co2c_mod - data for IR cooling
!     module qnir_mod - data for NIR heating
!     subroutine co2pro_pre - prepare Fomichev profile
!     subroutine co2cri_pre - prepare CRISTA profile
!     subroutine co2cc - calculate IR cooling
!     subroutine co2cin - initialize IR cooling calculations
!     subroutine qnirc - calculate NIR heating
!
! Calls spline interpolation routines from file splin.f
!     splin1
!     splin2
!
! Reads reference IR matrices from 4 files
!      coeff_lte.150
!      coeff_lte.360
!      coeff_lte.540
!      coeff_lte.720
!     
!***********************************************************************

      module co2pro_mod

! Module to keep data and procedures for preparation of CO2 vertical
!     profile based on CRISTA data (or F1998 model)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! May 2005: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Profile on model grid
!hmhj real,dimension(:),allocatable,save:: co2my
      real,dimension(:),allocatable::      co2my

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Profile parameters
!
! - Surface VMR
      integer,parameter:: ico2am = 361
! This is a cut from radparm for reference as of Dec 7, 2006
! CO2 calculation parameters introduced into mtparm Aug 7, 2002, moved
! here Sep 13, 2002
!
!      parameter (ico2am = 313)
!      parameter (ico2am = 720)
!      parameter (ico2am = 360)
! 1980(1975)
!      parameter (ico2am = 331)
! 2000(1995)
!      parameter (ico2am = 361)
! Test
!      parameter (ico2am = 400)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! - Start diffusive equilibrium at
!      real, parameter:: xdiff=17.75
      real, parameter:: xdiff=16.5
      integer, parameter:: idiff=int(4.*xdiff+1.5)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Initial parameters
!
! Victor's initial x-grid 0-20 and CO2 and pressure profiles on it after
! Fomichev et al. (1998). Pressure calculated in co2cin
      integer,parameter:: ivic0=81
!hmhj real,dimension(ivic0),save:: co2vic,pvic
      real,dimension(ivic0)::      co2vic,pvic
      real,parameter,dimension(ivic0):: xvic0=(/                        &
     &    .00,    .25,    .50,    .75,   1.00,   1.25,   1.50,   1.75,  &
     &   2.00,   2.25,   2.50,   2.75,   3.00,   3.25,   3.50,   3.75,  &
     &   4.00,   4.25,   4.50,   4.75,   5.00,   5.25,   5.50,   5.75,  &
     &   6.00,   6.25,   6.50,   6.75,   7.00,   7.25,   7.50,   7.75,  &
     &   8.00,   8.25,   8.50,   8.75,   9.00,   9.25,   9.50,   9.75,  &
     &  10.00,  10.25,  10.50,  10.75,  11.00,  11.25,  11.50,  11.75,  &
     &  12.00,  12.25,  12.50,  12.75,  13.00,  13.25,  13.50,  13.75,  &
     &  14.00,  14.25,  14.50,  14.75,  15.00,  15.25,  15.50,  15.75,  &
     &  16.00,  16.25,  16.50,  16.75,  17.00,  17.25,  17.50,  17.75,  &
     &  18.00,  18.25,  18.50,  18.75,  19.00,  19.25,  19.50,  19.75,  &
     &  20.00 /)
! Victor's initial profile
      real,parameter,dimension(ivic0):: co2vi0=(/                       &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,  3.600E-04,      &
     &  3.580E-04,  3.540E-04,  3.500E-04,  3.410E-04,  3.280E-04,      &
     &  3.110E-04,  2.930E-04,  2.750E-04,  2.560E-04,  2.370E-04,      &
     &  2.180E-04,  1.990E-04,  1.800E-04,  1.610E-04,  1.420E-04,      &
     &  1.240E-04,  1.060E-04,  9.000E-05,  7.800E-05,  6.800E-05,      &
     &  5.900E-05,  5.100E-05,  4.400E-05,  3.700E-05,  3.000E-05,      &
     &  2.400E-05,  1.900E-05,  1.400E-05,  1.000E-05,  7.000E-06,      &
     &  5.000E-06 /)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     interface
!        subroutine splin1(x1,y1,x2,y2,n1,n2)
!        implicit none
!        integer,intent(in):: n1,n2
!        real,intent(in):: x1(n1),y1(n1),x2(n2)
!        real,intent(out):: y2(n2)
!        end subroutine splin1
!     end interface

      end module co2pro_mod

!***********************************************************************

      module co2c_mod

! Module to keep data related to CO2 cooling calculations

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Dec 2006: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Fixed parameters
!
! -Transition log-pressure level from recurrence relation to 
!       cooling-to-space (should be greater or equal to 16.5, Victor
!       recommends 16.5) 
! -Corresponding array index for full parameterization grid
!
      real,parameter:: xinter=16.5
      integer,parameter:: ivict=int(4.*xinter+1.1),itm50=ivict-50

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Variables prepared in co2cin
! -Number of first model layer (counted upwards) above xinter
!
!hmhj integer,save:: lraint
      integer        lraint
!
! -Cooling matrices on fixed Victor grid
!
!hmhj real,dimension(43,57),save:: vamat,vbmat
      real,dimension(43,57)::      vamat,vbmat
!
! -Escape functions for recurrence formula on Victor grid needed for 
!       x > 12.5 (index=1-itm50)
!
!hmhj real,save:: alvic(itm50)
      real        alvic(itm50)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      interface
         subroutine splin1(x1,y1,x2,y2,n1,n2)
         implicit none
         integer,intent(in):: n1,n2
         real,intent(in):: x1(n1),y1(n1),x2(n2)
         real,intent(out):: y2(n2)
         end subroutine splin1
!
         subroutine splin2(x1,y1,x2,y2,n1,n2,jm,km)
         implicit none
         integer,intent(in):: jm,km,n1,n2
         real,intent(in):: x1(n1),y1(jm,n1),x2(n2)
         real,intent(out):: y2(jm,n2)
         end subroutine splin2
      end interface

      end module co2c_mod

!***********************************************************************

      module qnir_mod

! Module to keep data and a procedure necessary for calculation of
!       CO2 heating in NIR after Ogibalov and Fomichev (2003)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! May 16, 2003: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! CO2 vertical column amounts (calculated in co2cin) on param.
! log-pressure grid x=2-14, dx=.25, index=1-49
!
      integer,parameter:: imnir=49
!hmhj real,save:: co2nir(imnir)
      real        co2nir(imnir)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Parameterization data
! Log-pressure grid for heating calculation
!
      real,parameter,dimension(imnir):: xnir=(/                         &
     &  2.00,  2.25,  2.50,  2.75,  3.00,  3.25,  3.50,                 &
     &  3.75,  4.00,  4.25,  4.50,  4.75,  5.00,  5.25,                 &
     &  5.50,  5.75,  6.00,  6.25,  6.50,  6.75,  7.00,                 &
     &  7.25,  7.50,  7.75,  8.00,  8.25,  8.50,  8.75,                 &
     &  9.00,  9.25,  9.50,  9.75, 10.00, 10.25, 10.50,                 &
     & 10.75, 11.00, 11.25, 11.50, 11.75, 12.00, 12.25,                 &
     & 12.50, 12.75, 13.00, 13.25, 13.50, 13.75, 14.00/)
!
! Above x=14 the heating rate is extrapolated to 0 at xtopn
!
      real,parameter:: xtopn=15.5,rdxnir=1./(xtopn-xnir(imnir))
!
! Tables of log(CO2 column amounts) and normalized heating rates
! taken and rearranged from Victor's pco2nir
!
      real,parameter,dimension(10,imnir):: lnco2n=reshape((/            &
     &  47.5235,  48.0588,  48.5941,  49.1295,  49.6653,                &
     &  50.2009,  50.7366,  51.2721,  51.8074,  52.3427,                &
     &  47.2737,  47.8092,  48.3446,  48.8802,  49.4155,                &
     &  49.9510,  50.4867,  51.0215,  51.5578,  52.0931,                &
     &  47.0244,  47.5598,  48.0954,  48.6304,  49.1661,                &
     &  49.7016,  50.2373,  50.7729,  51.3082,  51.8435,                &
     &  46.7749,  47.3102,  47.8456,  48.3809,  48.9168,                &
     &  49.4522,  49.9878,  50.5233,  51.0590,  51.5941,                &
     &  46.5254,  47.0611,  47.5963,  48.1318,  48.6676,                &
     &  49.2027,  49.7383,  50.2737,  50.8096,  51.3445,                &
     &  46.2756,  46.8111,  47.3467,  47.8820,  48.4173,                &
     &  48.9531,  49.4886,  50.0241,  50.5596,  51.0951,                &
     &  46.0263,  46.5615,  47.0973,  47.6327,  48.1682,                &
     &  48.7035,  49.2393,  49.7747,  50.3101,  50.8458,                &
     &  45.7769,  46.3125,  46.8479,  47.3833,  47.9189,                &
     &  48.4541,  48.9898,  49.5255,  50.0607,  50.5964,                &
     &  45.5275,  46.0626,  46.5987,  47.1342,  47.6693,                &
     &  48.2050,  48.7409,  49.2762,  49.8115,  50.3468,                &
     &  45.2780,  45.8133,  46.3488,  46.8846,  47.4198,                &
     &  47.9553,  48.4906,  49.0262,  49.5621,  50.0974,                &
     &  45.0284,  45.5639,  46.0995,  46.6350,  47.1704,                &
     &  47.7061,  48.2415,  48.7776,  49.3125,  49.8481,                &
     &  44.7791,  45.3146,  45.8502,  46.3853,  46.9209,                &
     &  47.4563,  47.9922,  48.5274,  49.0633,  49.5987,                &
     &  44.5294,  45.0650,  45.6006,  46.1360,  46.6717,                &
     &  47.2070,  47.7425,  48.2781,  48.8136,  49.3490,                &
     &  44.2797,  44.8156,  45.3511,  45.8866,  46.4226,                &
     &  46.9575,  47.4930,  48.0287,  48.5637,  49.0996,                &
     &  44.0305,  44.5667,  45.1019,  45.6371,  46.1730,                &
     &  46.7082,  47.2435,  47.7791,  48.3145,  48.8502,                &
     &  43.7816,  44.3173,  44.8521,  45.3877,  45.9233,                &
     &  46.4585,  46.9944,  47.5297,  48.0653,  48.6004,                &
     &  43.5320,  44.0676,  44.6027,  45.1384,  45.6740,                &
     &  46.2096,  46.7449,  47.2802,  47.8159,  48.3513,                &
     &  43.2825,  43.8177,  44.3540,  44.8889,  45.4245,                &
     &  45.9598,  46.4951,  47.0312,  47.5664,  48.1020,                &
     &  43.0329,  43.5684,  44.1047,  44.6395,  45.1749,                &
     &  45.7103,  46.2456,  46.7812,  47.3172,  47.8524,                &
     &  42.7836,  43.3188,  43.8544,  44.3899,  44.9253,                &
     &  45.4609,  45.9964,  46.5323,  47.0677,  47.6029,                &
     &  42.5341,  43.0693,  43.6049,  44.1405,  44.6758,                &
     &  45.2112,  45.7469,  46.2820,  46.8176,  47.3533,                &
     &  42.2842,  42.8200,  43.3555,  43.8906,  44.4266,                &
     &  44.9619,  45.4974,  46.0329,  46.5681,  47.1039,                &
     &  42.0343,  42.5702,  43.1057,  43.6412,  44.1763,                &
     &  44.7123,  45.2478,  45.7833,  46.3187,  46.8546,                &
     &  41.7851,  42.3208,  42.8563,  43.3917,  43.9273,                &
     &  44.4626,  44.9982,  45.5337,  46.0695,  46.6050,                &
     &  41.5355,  42.0709,  42.6062,  43.1418,  43.6773,                &
     &  44.2129,  44.7483,  45.2838,  45.8193,  46.3548,                &
     &  41.2854,  41.8209,  42.3564,  42.8920,  43.4274,                &
     &  43.9626,  44.4982,  45.0339,  45.5693,  46.1052,                &
     &  41.0354,  41.5714,  42.1066,  42.6420,  43.1775,                &
     &  43.7130,  44.2481,  44.7841,  45.3194,  45.8548,                &
     &  40.7855,  41.3210,  41.8560,  42.3920,  42.9272,                &
     &  43.4629,  43.9983,  44.5339,  45.0695,  45.6048,                &
     &  40.5352,  41.0705,  41.6061,  42.1417,  42.6770,                &
     &  43.2125,  43.7480,  44.2833,  44.8190,  45.3543,                &
     &  40.2843,  40.8201,  41.3556,  41.8906,  42.4264,                &
     &  42.9621,  43.4974,  44.0328,  44.5685,  45.1039,                &
     &  40.0335,  40.5690,  41.1046,  41.6405,  42.1755,                &
     &  42.7112,  43.2466,  43.7826,  44.3178,  44.8530,                &
     &  39.7821,  40.3180,  40.8534,  41.3889,  41.9246,                &
     &  42.4599,  42.9956,  43.5308,  44.0661,  44.6023,                &
     &  39.5306,  40.0662,  40.6014,  41.1370,  41.6729,                &
     &  42.2078,  42.7434,  43.2791,  43.8149,  44.3501,                &
     &  39.2785,  39.8138,  40.3493,  40.8848,  41.4203,                &
     &  41.9552,  42.4910,  43.0267,  43.5622,  44.0977,                &
     &  39.0251,  39.5607,  40.0964,  40.6314,  41.1671,                &
     &  41.7027,  42.2382,  42.7733,  43.3091,  43.8444,                &
     &  38.7707,  39.3061,  39.8421,  40.3771,  40.9128,                &
     &  41.4485,  41.9836,  42.5191,  43.0550,  43.5902,                &
     &  38.5153,  39.0507,  39.5867,  40.1219,  40.6571,                &
     &  41.1928,  41.7279,  42.2641,  42.7993,  43.3347,                &
     &  38.2577,  38.7934,  39.3288,  39.8642,  40.3996,                &
     &  40.9352,  41.4712,  42.0061,  42.5418,  43.0771,                &
     &  37.9979,  38.5335,  39.0689,  39.6045,  40.1402,                &
     &  40.6754,  41.2109,  41.7466,  42.2820,  42.8175,                &
     &  37.7354,  38.2710,  38.8062,  39.3420,  39.8773,                &
     &  40.4129,  40.9481,  41.4838,  42.0192,  42.5544,                &
     &  37.4684,  38.0036,  38.5390,  39.0747,  39.6102,                &
     &  40.1457,  40.6812,  41.2169,  41.7525,  42.2877,                &
     &  37.1969,  37.7321,  38.2674,  38.8029,  39.3387,                &
     &  39.8744,  40.4095,  40.9450,  41.4800,  42.0164,                &
     &  36.9174,  37.4533,  37.9888,  38.5242,  39.0597,                &
     &  39.5950,  40.1305,  40.6661,  41.2018,  41.7370,                &
     &  36.6321,  37.1678,  37.7030,  38.2386,  38.7742,                &
     &  39.3095,  39.8456,  40.3809,  40.9160,  41.4515,                &
     &  36.3383,  36.8738,  37.4095,  37.9450,  38.4802,                &
     &  39.0158,  39.5514,  40.0867,  40.6221,  41.1578,                &
     &  36.0350,  36.5703,  37.1060,  37.6411,  38.1769,                &
     &  38.7122,  39.2474,  39.7832,  40.3189,  40.8543,                &
     &  35.7230,  36.2585,  36.7940,  37.3293,  37.8649,                &
     &  38.4006,  38.9361,  39.4711,  40.0068,  40.5427,                &
     &  35.4046,  35.9397,  36.4754,  37.0111,  37.5464,                &
     &  38.0818,  38.6173,  39.1529,  39.6886,  40.2241,                &
     &  35.0799,  35.6155,  36.1510,  36.6865,  37.2225,                &
     &  37.7577,  38.2930,  38.8286,  39.3639,  39.8996/),              &
     &  (/10,imnir/))
      real,parameter,dimension(10,imnir):: qco2n=reshape((/             &
     & 1.966E-02, 1.493E-02, 1.127E-02, 8.224E-03, 5.478E-03,           &
     & 3.549E-03, 2.319E-03, 1.585E-03, 1.095E-03, 7.710E-04,           &
     & 1.947E-02, 1.495E-02, 1.147E-02, 8.681E-03, 6.035E-03,           &
     & 3.990E-03, 2.542E-03, 1.660E-03, 1.098E-03, 7.520E-04,           &
     & 1.977E-02, 1.510E-02, 1.157E-02, 8.899E-03, 6.456E-03,           &
     & 4.475E-03, 2.888E-03, 1.823E-03, 1.159E-03, 7.658E-04,           &
     & 2.065E-02, 1.551E-02, 1.173E-02, 9.002E-03, 6.710E-03,           &
     & 4.861E-03, 3.257E-03, 2.036E-03, 1.271E-03, 8.150E-04,           &
     & 2.240E-02, 1.633E-02, 1.204E-02, 9.105E-03, 6.851E-03,           &
     & 5.096E-03, 3.553E-03, 2.266E-03, 1.431E-03, 9.079E-04,           &
     & 2.532E-02, 1.771E-02, 1.258E-02, 9.279E-03, 6.948E-03,           &
     & 5.215E-03, 3.745E-03, 2.486E-03, 1.628E-03, 1.054E-03,           &
     & 2.867E-02, 1.961E-02, 1.347E-02, 9.619E-03, 7.083E-03,           &
     & 5.289E-03, 3.858E-03, 2.668E-03, 1.827E-03, 1.235E-03,           &
     & 3.108E-02, 2.166E-02, 1.491E-02, 1.032E-02, 7.343E-03,           &
     & 5.385E-03, 3.922E-03, 2.794E-03, 1.983E-03, 1.401E-03,           &
     & 3.215E-02, 2.336E-02, 1.670E-02, 1.154E-02, 7.864E-03,           &
     & 5.559E-03, 3.980E-03, 2.869E-03, 2.074E-03, 1.515E-03,           &
     & 3.247E-02, 2.443E-02, 1.820E-02, 1.302E-02, 8.719E-03,           &
     & 5.893E-03, 4.073E-03, 2.912E-03, 2.102E-03, 1.557E-03,           &
     & 3.260E-02, 2.492E-02, 1.898E-02, 1.413E-02, 9.668E-03,           &
     & 6.411E-03, 4.244E-03, 2.939E-03, 2.070E-03, 1.521E-03,           &
     & 3.283E-02, 2.509E-02, 1.918E-02, 1.456E-02, 1.030E-02,           &
     & 6.954E-03, 4.462E-03, 2.926E-03, 1.955E-03, 1.389E-03,           &
     & 3.311E-02, 2.517E-02, 1.912E-02, 1.454E-02, 1.048E-02,           &
     & 7.279E-03, 4.633E-03, 2.832E-03, 1.754E-03, 1.167E-03,           &
     & 3.347E-02, 2.527E-02, 1.904E-02, 1.436E-02, 1.038E-02,           &
     & 7.301E-03, 4.652E-03, 2.652E-03, 1.510E-03, 9.121E-04,           &
     & 3.385E-02, 2.543E-02, 1.900E-02, 1.415E-02, 1.011E-02,           &
     & 7.051E-03, 4.437E-03, 2.357E-03, 1.228E-03, 6.481E-04,           &
     & 3.427E-02, 2.560E-02, 1.898E-02, 1.394E-02, 9.687E-03,           &
     & 6.539E-03, 3.919E-03, 1.854E-03, 8.449E-04, 3.623E-04,           &
     & 3.482E-02, 2.588E-02, 1.905E-02, 1.376E-02, 9.105E-03,           &
     & 5.806E-03, 3.041E-03, 7.391E-04, 1.560E-04, 5.546E-06,           &
     & 3.565E-02, 2.642E-02, 1.933E-02, 1.374E-02, 9.426E-03,           &
     & 5.977E-03, 3.180E-03, 1.608E-03, 2.737E-04,-4.588E-04,           &
     & 3.720E-02, 2.751E-02, 2.010E-02, 1.414E-02, 9.465E-03,           &
     & 5.748E-03, 2.723E-03, 1.085E-03,-2.727E-04,-9.037E-04,           &
     & 3.977E-02, 2.947E-02, 2.160E-02, 1.523E-02, 1.012E-02,           &
     & 6.076E-03, 2.777E-03, 1.015E-03,-4.369E-04,-1.083E-03,           &
     & 4.356E-02, 3.241E-02, 2.394E-02, 1.711E-02, 1.154E-02,           &
     & 7.115E-03, 3.501E-03, 1.533E-03,-1.011E-04,-8.841E-04,           &
     & 4.848E-02, 3.623E-02, 2.703E-02, 1.974E-02, 1.235E-02,           &
     & 7.411E-03, 3.807E-03, 2.160E-03, 7.999E-04,-2.293E-04,           &
     & 5.433E-02, 4.077E-02, 3.059E-02, 2.279E-02, 1.544E-02,           &
     & 1.010E-02, 5.858E-03, 2.882E-03, 1.407E-03, 7.218E-04,           &
     & 6.076E-02, 4.559E-02, 3.430E-02, 2.589E-02, 1.840E-02,           &
     & 1.272E-02, 8.205E-03, 4.929E-03, 2.952E-03, 1.811E-03,           &
     & 6.726E-02, 5.041E-02, 3.787E-02, 2.875E-02, 2.106E-02,           &
     & 1.513E-02, 1.035E-02, 6.732E-03, 4.363E-03, 2.845E-03,           &
     & 7.346E-02, 5.507E-02, 4.135E-02, 3.130E-02, 2.328E-02,           &
     & 1.715E-02, 1.215E-02, 8.214E-03, 5.523E-03, 3.710E-03,           &
     & 7.948E-02, 5.945E-02, 4.459E-02, 3.360E-02, 2.507E-02,           &
     & 1.872E-02, 1.354E-02, 9.343E-03, 6.411E-03, 4.385E-03,           &
     & 8.539E-02, 6.343E-02, 4.739E-02, 3.562E-02, 2.646E-02,           &
     & 1.978E-02, 1.445E-02, 1.007E-02, 6.980E-03, 4.829E-03,           &
     & 9.095E-02, 6.684E-02, 4.951E-02, 3.710E-02, 2.745E-02,           &
     & 2.038E-02, 1.488E-02, 1.040E-02, 7.240E-03, 5.048E-03,           &
     & 9.545E-02, 6.941E-02, 5.086E-02, 3.797E-02, 2.798E-02,           &
     & 2.061E-02, 1.491E-02, 1.041E-02, 7.243E-03, 5.073E-03,           &
     & 9.806E-02, 7.108E-02, 5.163E-02, 3.819E-02, 2.805E-02,           &
     & 2.052E-02, 1.467E-02, 1.019E-02, 7.067E-03, 4.970E-03,           &
     & 9.848E-02, 7.186E-02, 5.208E-02, 3.810E-02, 2.771E-02,           &
     & 2.014E-02, 1.420E-02, 9.780E-03, 6.758E-03, 4.784E-03,           &
     & 9.690E-02, 7.176E-02, 5.240E-02, 3.791E-02, 2.712E-02,           &
     & 1.953E-02, 1.357E-02, 9.243E-03, 6.346E-03, 4.533E-03,           &
     & 9.375E-02, 7.099E-02, 5.269E-02, 3.781E-02, 2.646E-02,           &
     & 1.878E-02, 1.282E-02, 8.591E-03, 5.842E-03, 4.228E-03,           &
     & 8.980E-02, 7.195E-02, 5.433E-02, 3.832E-02, 2.699E-02,           &
     & 1.908E-02, 1.296E-02, 9.182E-03, 5.925E-03, 3.868E-03,           &
     & 8.497E-02, 6.957E-02, 5.414E-02, 3.881E-02, 2.687E-02,           &
     & 1.849E-02, 1.219E-02, 8.462E-03, 5.292E-03, 3.443E-03,           &
     & 8.002E-02, 6.581E-02, 5.308E-02, 3.926E-02, 2.557E-02,           &
     & 1.657E-02, 1.024E-02, 6.176E-03, 3.913E-03, 2.962E-03,           &
     & 7.459E-02, 6.401E-02, 5.311E-02, 4.028E-02, 2.777E-02,           &
     & 1.799E-02, 1.086E-02, 7.060E-03, 3.951E-03, 2.430E-03,           &
     & 6.906E-02, 6.070E-02, 5.202E-02, 4.080E-02, 2.846E-02,           &
     & 1.806E-02, 1.028E-02, 6.375E-03, 3.257E-03, 1.881E-03,           &
     & 6.237E-02, 5.626E-02, 4.974E-02, 4.035E-02, 2.864E-02,           &
     & 1.795E-02, 9.533E-03, 5.546E-03, 2.451E-03, 1.293E-03,           &
     & 5.748E-02, 5.295E-02, 4.800E-02, 4.009E-02, 2.897E-02,           &
     & 1.810E-02, 8.951E-03, 4.800E-03, 1.684E-03, 7.912E-04,           &
     & 5.032E-02, 4.733E-02, 4.389E-02, 3.758E-02, 2.754E-02,           &
     & 1.710E-02, 7.578E-03, 3.405E-03, 3.974E-04,-2.644E-05,           &
     & 5.011E-02, 4.733E-02, 4.419E-02, 3.838E-02, 2.856E-02,           &
     & 1.797E-02, 7.578E-03, 2.883E-03,-4.478E-04,-5.690E-04,           &
     & 4.976E-02, 4.688E-02, 4.375E-02, 3.825E-02, 2.886E-02,           &
     & 1.845E-02, 7.419E-03, 2.025E-03,-1.811E-03,-1.643E-03,           &
     & 6.052E-02, 5.599E-02, 5.142E-02, 4.474E-02, 3.456E-02,           &
     & 2.330E-02, 1.072E-02, 3.492E-03,-1.946E-03,-2.301E-03,           &
     & 7.280E-02, 6.608E-02, 5.953E-02, 5.132E-02, 4.042E-02,           &
     & 2.853E-02, 1.467E-02, 5.302E-03,-2.188E-03,-3.693E-03,           &
     & 8.215E-02, 7.354E-02, 6.532E-02, 5.586E-02, 4.450E-02,           &
     & 3.234E-02, 1.778E-02, 6.528E-03,-2.953E-03,-6.105E-03,           &
     & 8.764E-02, 7.782E-02, 6.851E-02, 5.813E-02, 4.656E-02,           &
     & 3.428E-02, 1.948E-02, 7.008E-03,-3.956E-03,-8.995E-03,           &
     & 9.172E-02, 8.098E-02, 7.095E-02, 5.969E-02, 4.784E-02,           &
     & 3.539E-02, 2.052E-02, 7.584E-03,-4.132E-03,-1.079E-02/),         &
     &  (/10,imnir/))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      contains
      subroutine lint1(x1,y1,n1,yleft,yright,x,y)
!
! A very simple linear interpolation of y1(x1) into y(x)
! ***x1 is assumed in ascending order***

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Apr 16, 2003: Rashid Akmaev
!       Made from lint for a scalar argument

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
! Array dimension
      integer,intent(in):: n1
      real,intent(in):: x1(n1),y1(n1),yleft,yright,x
      real,intent(out):: y
! Work memory
      integer:: i
      real:: dx

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      if(x.lt.x1(1)) then
         y=yleft
      elseif(x.gt.x1(n1)) then
         y=yright
      else
         do i=2,n1
            dx=x-x1(i)
            if(dx.le.0.) then
               y=y1(i)+(y1(i)-y1(i-1))*dx/(x1(i)-x1(i-1))
               return
            endif
         enddo
      endif
      end subroutine lint1

      end module qnir_mod

!***********************************************************************

      subroutine co2pro_pre(xmodel,lmodel,mumod)

! Routine to prepare CO2 global mean profile optionally based on 
!     Fomichev et al. (1998) or CRISTA data.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Jan 2006: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use co2pro_mod
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Input parameters
! xmodel(lmodel) - model grid (going up from LB)
! mumod(lmodel) - model molecular mass (g/mol) on the grid 
!
      integer,intent(in):: lmodel
      real,intent(in):: xmodel(lmodel),mumod(lmodel)
!
! Output profiles (in co2pro_mod)
! co2vic(ivic0) - CO2 on Victor initial grid
! co2my(lmodel) - CO2 on model grid

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Work variables and arrays
!
      integer:: l,lradif
      real:: vicmu(ivic0)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Check parameter consistency
!
      if(xvic0(ivic0)<12.5) then
         write(6,*)                                                     &
     &        '***Stop in co2pro_pre: Full Victor grid too low?'
         write(6,'(i8)') xvic0(ivic0)
         stop
      endif

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Allocate model profile kept in co2pro_mod
!
      allocate(co2my(lmodel))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Correct Victor's CO2 model for diffusive equilibrium above
!     xdiff (corresponding index of .25 grid idiff) set in co2pro_mod
! Find model layer of xmodel grid just above xdiff
!
      lradif=lmodel+1
      do l=1,lmodel
         if(xmodel(l).ge.xdiff) then
            lradif=l
            exit
         endif
      enddo
!
! Prepare auxiliary array of mu on Vick's grid above interface
!     level of x=12.5 (index=50)
!
      call splin1(xmodel,mumod,xvic0,vicmu,lmodel,ivic0)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Initialize CO2 on Vick's grid and normalize it by surface VMR
!
      co2vic(:)=(real(ico2am)/360.)*co2vi0(:)
!
! Correct Victor's model for diffusive equilibrium if necessary (if
!       ifdiff>ivic0, the loop is not executed)
!
      do l=idiff,ivic0
         co2vic(l)=exp(alog(co2vic(l-1))+(xvic0(l)-xvic0(l-1))*(1.-     &
     &        23.*(1./vicmu(l)+1./vicmu(l-1))))
      enddo
!
! Construct CO2 mixing ratio profile on model grid, calculate CO2
!     column amount.  Note: Victor's CO2 model (for x=0-20) is already
!     normalized for CO2 amount
!
      do l=1,lmodel
         co2my(l)=0.
      enddo
!
! Below xdiff interpolate Vick's CO2 model on model grid
!
      call splin1(xvic0,co2vic,xmodel,co2my,ivic0,lradif-1)
!
! Above xdiff use diffusive equilibrium if necessary (if lradif>lmodel,
!       the loop is not executed)
!
      do l=lradif,lmodel
         co2my(l)=exp(alog(co2my(l-1))+(xmodel(l)-xmodel(l-1))*(1.-     &
     &        23.*(1./mumod(l)+1./mumod(l-1))))
      enddo

      end subroutine co2pro_pre

!***********************************************************************

      subroutine crico2_pre(xmodel,lmodel,mumod)

! Routine to prepare CO2 global mean profile based on CRISTA data.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Jan 2006: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use co2pro_mod
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Input parameters
! xmodel(lmodel) - model grid (going up from LB)
! mumod(lmodel) - model molecular mass (g/mol) on the grid 
!
      integer,intent(in):: lmodel
      real,intent(in):: xmodel(lmodel),mumod(lmodel)
!
!
! Output profiles (in co2pro_mod)
! co2vic(ivic0) - CO2 on Victor initial grid
! co2my(lmodel) - CO2 on model grid

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Table loosely based on CRISTA data
!
      integer,parameter:: icrist=5
      real,parameter,dimension(icrist)::                                &
     &     xcrist=(/ 9.0, 10.0, 13., 15.5, 17.0 /),                     &
     &     cvcris=(/ 360e-6, 355e-6, 260e-6, 100e-6, 40e-6 /)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Work space
!
      integer:: l,lradif,lvic1,lvic2
      real:: vicmu(ivic0)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Check parameter consistency
!
      if(xvic0(ivic0)<12.5) then
         write(6,*)                                                     &
     &        '***Stop in co2pro_pre: Full Victor grid too low?'
         write(6,'(i8)') xvic0(ivic0)
         stop
      endif

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Allocate model profile kept in co2pro_mod
!
      allocate(co2my(lmodel))

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Correct Victor's CO2 model for diffusive equilibrium above
!     xdiff (corresponding index of .25 grid idiff) set in co2pro_mod
! Find model layer of xmodel grid just above xdiff
!
      lradif=lmodel+1
      do l=1,lmodel
         if(xmodel(l).ge.xdiff) then
            lradif=l
            exit
         endif
      enddo
!
! Prepare auxiliary array of mu on Vick's grid [only used below
!       between x=12.5 (l=50) and xdiff (l=idiff)]
!
      call splin1(xmodel,mumod,xvic0,vicmu,lmodel,ivic0)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Initialize CO2 on Victor's grid by his standard profile (normalized
!     to surface VMR 360 ppm)
!
      co2vic(:)=co2vi0(:)
!
! Correct Victor's model for CRISTA data and diffusive equilibrium
!     if necessary
! First find the levels within CRISTA range
!
      lvic1=1
      do l=1,ivic0
         if(xvic0(l).ge.xcrist(1)) then
            lvic1=l
            exit
         endif
      enddo
      lvic2=ivic0
      do l=1,ivic0
         if(xvic0(l).gt.xcrist(icrist)) then
            lvic2=l-1
            exit
         endif
      enddo
!
! Do spline interpolation for these levels
!
      call splin1(xcrist,cvcris,xvic0(lvic1:lvic2),                     &
     &     co2vic(lvic1:lvic2),icrist,lvic2-lvic1+1)
!
! Do diffusion if applicable
!
      if(idiff.gt.lvic2) then
         write(6,*)                                                     &
     &        '***Stop in co2pro_pre: Diffusion above CRISTA?'
         write(6,'(2i8)') idiff,lvic2
         stop
      endif
! (if ifdiff>ivic0, the loop is not executed)
      do l=idiff,ivic0
         co2vic(l)=exp(alog(co2vic(l-1))+(xvic0(l)-xvic0(l-1))*(1.-     &
     &        23.*(1./vicmu(l)+1./vicmu(l-1))))
      enddo
!
! Now normalize the profile to the surface mixing ratio
!
      co2vic(:)=(real(ico2am)/360.)*co2vic(:)
!
! Construct CO2 mixing ratio profile on model grid using the profile
!     just calculated on Victor's grid.  Note: that profile is already
!     normalized for CO2 surface VMR
!
      do l=1,lmodel
         co2my(l)=0.
      enddo
!
! Below xdiff interpolate from Victor's grid to model grid
!
      call splin1(xvic0,co2vic,xmodel,co2my,ivic0,lradif-1)
!
! Above xdiff use diffusive equilibrium (if lradif>lmodel, the loop
!       is not executed)
!
      do l=lradif,lmodel
         co2my(l)=exp(alog(co2my(l-1))+(xmodel(l)-xmodel(l-1))*(1.-     &
     &        23.*(1./mumod(l-1)+1./mumod(l))))
      enddo

      end subroutine crico2_pre

!***********************************************************************

      subroutine co2cc(im,jm,xtemp,temp,ltemp,xhr,hrate,lhr,mu,ro1,     &
     &     ro2,rn2)

! Routine to calculate CO2 cooling in 3-d on model vertical grid using 
! Victor's new parameterization (Fomichev et al., 1998) updated in 1999.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! History
! Feb 28, 2008: changes by RAA in O interpolation to parameterization
!     grid
! Dec 2006: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use co2c_mod
      use co2pro_mod, only:xvic0,pvic,co2vic,co2my

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine arguments
! INPUT
! -array dimensions: first array index, "longitude" index, 
!       vertical T-grid, vertical heating-rate grid (generally 
!       lhr < ltemp)
!
      integer,intent(in):: im,jm,ltemp,lhr
!
! - input temperature and its log-pressure grid [x=ln(1e5/p)] (xtemp
!       should normally extend down to the surface x=0)
! - output heating-rate log-pressure grid (should normally start above
!       x=2)
! - molecular mass (g/mol)
! - VMR of O, O2, N2 on heating-rate grid
!
      real,intent(in):: temp(im,ltemp),xtemp(ltemp),                    &
     &     xhr(lhr),mu(im,lhr),ro1(im,lhr),ro2(im,lhr),rn2(im,lhr)
!
! OUTPUT
! - output heating rate (J/kg)
!
      real,intent(out):: hrate(im,lhr)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Internal workspace
! Fixed parameters
! -collisional deactivation rate by O [1/(cm**3 s)]
!
!     real,parameter:: zco2o=1.5e-12
      real,parameter:: zco2o=3.e-12
!
! -composition on Victor grid needed for x above 12.5 (index=1-itm50),
!       stored here in case it can be reused
!
      real,dimension(im,itm50):: vicn2,vico2,vico1,vicmu
!
! Temp workspace
!
      integer:: i,iwork,j,l
      real:: d1,d2,work1,work2,zn2,zo2,ztotal
      real:: vict(im,ivict),vich(im,ivict-8),vics(im,ivict),            &
     &     vlamb(im,itm50),htilda(jm),flux(jm)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Boltzmann constant multiplied by 1e6 to convert pressure in Pa to
!     number density in 1/cm**3
!
      real,parameter:: bol106=1.3807e-17

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Victor's data (with his original comments):
! ak = h*c/k*V - to determine the Planck function, k - Boltzmann's 
!     constant
      real,parameter:: ak=960.217
!
! a10   - Einstine's (***Victor's classic spelling!!!***) coefficient 
!     for the fundamental band for the main isotope (1/s)
      real,parameter:: eina10=1.5988
!
! const - constant using to determine the heating rate:
!         const = Na*h*c*V*Gv'/Gv*Avv*1.03, where
!         Na - Avogadro number, h- Planck's constant, c- light speed, 
!         V - frequency of the fundamental vibrational transition in 
!     the main isotope, Gv'/Gv = 2 - ratio of the statistical weights 
!     for the fundamental transition, Av'v - Einstine coefficient for 
!     the fundamental band of the main isotope, 1.03 - correction to 
!     account for others than funndamental bands in the reccurence 
!     formula 
!
!      data const/2.63187e11/
!
! Victor's const multiplied by 1e-4 to convert from erg/(g*s) to W/kg
!
!      data conmy/2.63187e7/
!
! In updated parameterization Victor changed const apparently having
!     removed the 1.03 factor (see the paragraph above):
! const - constant using to determine the heating rate:
!         const = Na*h*c*V*Gv'/Gv*Avv, where
!         Na - Avogadro number, h- Planck's constant, c- light speed, 
!         V - frequency of the fundamental vibrational transition in 
!         the main isotope, Gv'/Gv = 2 - ratio of the statistical 
!         weights for the fundamental transition, Av'v - Einstine 
!         coefficient for the fundamental band of the main isotope.
!      data const/2.55521e11/, constb/8.82325e9/
!
      real,parameter:: conmy=2.55521e7

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate composition on Victor grid above x=12.5 (index 50) for 
!       recurrent formula
!
      call splin2(xhr,mu,xvic0(51),vicmu,lhr,itm50,im,jm)
      call splin2(xhr,rn2,xvic0(51),vicn2,lhr,itm50,im,jm)
      call splin2(xhr,ro2,xvic0(51),vico2,lhr,itm50,im,jm)
!
! Feb 28, 2008
! idea change: the following portion of the code commented out and 
!     replaced by RAA
!c For O interpolation find first model layer with [O]>.0
!c
!      iwork=0
!      do j=1,jm
!         do l=1,lhr
!c idea change
!c           if(ro1(j,l).gt.0.) then
!            if(ro1(j,l).gt.1.e-7) then
!               exit
!            endif
!         enddo
!         iwork=max(iwork,l)
!      enddo
!c     print*,'www1',iwork,xhr(iwork),lhr
!      if(iwork==0 .or. xhr(iwork)>12.5) then
!         write(6,*) '***Stop, co2cc: [O] is zero or negative***'
!c        print*,'www',iwork,ro1(1:jm,iwork-1)
!         write(6,*) iwork,xhr(iwork)
!         stop
!      endif
!      call splin2(xhr(iwork),ro1(1:im,iwork:),xvic0(51),vico1,         &
!     &     lhr-iwork+1,itm50,im,jm)
!
      call splin2(xhr,ro1,xvic0(51),vico1,lhr,itm50,im,jm)
!
! idea add: make sure O is non-negative
!
          vico1(:,:)=max(vico1(:,:),0.)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate heating rate for LTR/recurrence part. Convert Tin to 
! parameterization grid at x=0-xinter and calculate "source function" 
! using spline for T interpolation
!
      call splin2(xtemp,temp,xvic0,vict,ltemp,ivict,im,jm)
      do i=1,ivict
         do j=1,jm
            vics(j,i)=exp(-ak/vict(j,i))
         enddo
      enddo
      iwork=ivict-8
      do i=1,iwork
         do j=1,jm
            vich(j,i)=0.
         enddo
      enddo
!
! LTR heating (x=2-12.5, i=1-43) in W/kg
!
      do i=1,43
         do l=1,57
            do j=1,jm
               vich(j,i)=(vamat(i,l)+vbmat(i,l)*vics(j,i+8))*vics(j,l)+ &
     &              vich(j,i)
            enddo
         enddo
      enddo
!
! Calculate lambda for the recurrence-formula part (x=12.5-xinter, 
!     i=1-itm50, full index 51-ivict)
!
      do i=1,itm50
!
! No temperature dependence of collisional rate CO2-O
!
         do j=1,jm
            work1=vict(j,i+50)**(-1./3.)
            zn2=5.5e-17*sqrt(vict(j,i+50))+6.7e-10*exp(-83.8*work1)
            zo2=1.e-15*exp((564.*work1-230.9)*work1+23.37)
!
! Total collisional deactivation rate (factor in second line converts 
!     pressure to total number density)
!
            ztotal=pvic(i+50)*(vico1(j,i)*zco2o+vicn2(j,i)*zn2+         &
     &           vico2(j,i)*zo2)/(bol106*vict(j,i+50))
!
! Probability of photon survival
!
            vlamb(j,i)=eina10/(eina10+ztotal)
         enddo
      enddo
!
! Boundary condition at x=12.5 (recurrence index 1, heating index 43,
!     total index 51)
!
      do j=1,jm
         htilda(j)=vich(j,43)*vicmu(j,1)/                               &
     &        (conmy*co2vic(51)*(1.-vlamb(j,1)))
      enddo
!
! Recurrent formula
!
      do i=2,itm50
         d1=.25*alvic(i)+.75*alvic(i-1)
         d2=.75*alvic(i)+.25*alvic(i-1)
         work1=1.-d1
         work2=1.-d2
         do j=1,jm
            htilda(j)=((((1.-vlamb(j,i-1)*work1)*htilda(j))+            &
     &           d1*vics(j,i+49))-d2*vics(j,i+50))/(1.-vlamb(j,i)*work2)
            vich(j,i+42)=conmy*co2vic(i+50)*htilda(j)*(1.-vlamb(j,i))/  &
     &           vicmu(j,i)
         enddo
      enddo
!
! Transfer heating back from parameterization grid at x=2-xinter to
! model heating-rate grid by spline interpolation.
!
      call splin2(xvic0(9:),vich,xhr,hrate,iwork,lraint-1,im,jm)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate heating on model grid above xinter.  First find lower 
!     boundary condition at xinter.
!
      do j=1,jm
         flux(j)=htilda(j)+vics(j,ivict)
      enddo
!
! Assume that the upper portion of Tin is specified at model layers,
! for which hrate is calculated, that is Tin(ltemp) corresponds to
! hrate(lhr)
!
      iwork=ltemp-lhr
      do l=lraint,lhr
!
! No temperature dependence of collisional rate CO2-O
!
         do j=1,jm
            work1=temp(j,l+iwork)**(-1./3.)
            zn2=5.5e-17*sqrt(temp(j,l+iwork))+6.7e-10*exp(-83.8*work1)
            zo2=1.e-15*exp((564.*work1-230.9)*work1+23.37)
!
! Total collisional deactivation rate (factor in last line converts 
! pressure to total number density).  Pressure is recalculated here
! from input log-pressure (xhr).
!
            ztotal=1e5*exp(-xhr(l))*(zco2o*ro1(j,l)+rn2(j,l)*zn2+       &
     &           ro2(j,l)*zo2)/(bol106*temp(j,l+iwork))
!
! Heating rate
!
            hrate(j,l)=conmy*co2my(l)*(1.-eina10/(eina10+               &
     &           ztotal))*(flux(j)-exp(-ak/temp(j,l+iwork)))/mu(j,l)
         enddo
      enddo

      end subroutine co2cc

!***********************************************************************

      subroutine co2cin(xmod,pmod,mu,gr,lmod,me,mpi_ior,mpi_comm)

! Routine to prepare matrices and other parameters for implementation
! of full CO2 cooling scheme by Fomichev et al. (1998) modified later 
! (Victor Fomichev, personal communication, 1999) to account for updated
! non-LTE calculations by Ogibalov. This version made to accomodate CO2
! profiles different from the one suggested by Fomichev et al. (1998).
! Since CO2 profiles are assumed to be global means, the 
! parameterization parameters are considered global means as well and
! may be precalculated once. If variable CO2 profiles are available,
! some of these calculations should be done "on line".

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Dec 2006: Rashid Akmaev
! Oct 2012: Jun Wang: change reading for MPI environment

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use co2c_mod
      use co2pro_mod, only:xvic0,pvic,co2vic,co2my
      use qnir_mod, only:imnir,co2nir

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

      include 'mpif.h'
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine arguments
! INPUT
! -log-pressure grid for heating rate calculation going up
! -other variables on this grid (these are meant here as global means
! to prepare LTR matrices, escape functions, and CO2 column amounts for
! NIR heating calculations):
!       pressure (Pa)
!       global mean VMR of N2,O2, and O
!       global mean molecular weight (g/mol) 
!       gravity (m/sec**2)
!
      integer,intent(in)::lmod
      real,intent(in),dimension(lmod)::xmod,pmod,mu,gr
      integer, intent(in) :: me     ! my pe
      integer, intent(in) :: mpi_ior      ! mpi real for io
      integer, intent(in) :: mpi_comm     ! mpi communicator
!
! -directory where matrix files are located
!
!hmhj character(len=*),intent(in):: dir
!
! Subroutine OUTPUT is placed in modules
!       co2c_mod for use by co2cc and
!       qnir_mod for use by qnirc

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Internal work space
! Fixed parameters
! -Avogadro number (1/mol) made parameter Mar 11, 1999
!
      real,parameter:: ana=6.022e23
!
! Temp work space
!
      integer:: i,j,l,info
      real:: w1,wcolmy(lmod),war1(100),war2(100)
      real:: vmu(ivict),vgrav(ivict)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Cooling scheme parameters
! This section adopted from Victor's parcof:
! matrix coefficients for basic CO2 vmr of 150,360,540 and 720 ppm
! (basically these arrays give log(coeficient/vmr)
!
      real a150(43,57),b150(43,57), a360(43,57),b360(43,57),            &
     &     a540(43,57),b540(43,57), a720(43,57),b720(43,57)
!
! CO2 column amount and corresponding escape functions (eventually, 
! their log)
!
      real uco2ro(51), alo(51)
!
! CO2 column amount at X(67) grid, calculated for basic CO2 profile of 
! 360ppm:
! (note, this profile exibits CO2 vmr=const up to x=12.25 and decreases
! with height above this level)
!
! ***Another note (RAA): it only goes up to x=14***
!
      real uco2o(57)
!
! corrections to escape functions to calculate coefficients for the
! reccurence formula between x=12.5 and 13.75
!
      real cor150(6), cor360(6), cor540(6), cor720(6)
!
! auxiliary arrays (dimension of uco2 changed to 81 from 67, that is
!     x changed from 16.5 to 20)
!       Nov 2, 2006: dimension changed again to ivict
!
      real uref(4), co2int(4), uco2(ivict)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! This section also adopted from Victor's parcof (new version  - arrays
! cor* certainly changed)
! uco2ro(51),alo(51),uco2o(57), and cor150(6),cor360(6),cor540(6),
! cor720(6) are given below:
!
      data uco2ro/2.699726E+11,5.810773E+11,1.106722E+12,1.952319E+12,  &
     &            3.306797E+12,5.480155E+12,8.858565E+12,1.390142E+13,  &
     &            2.129301E+13,3.209300E+13,4.784654E+13,7.091442E+13,  &
     &            1.052353E+14,1.565317E+14,2.320320E+14,3.415852E+14,  &
     &            4.986668E+14,7.212717E+14,1.033831E+15,1.469497E+15,  &
     &            2.073209E+15,2.905406E+15,4.044901E+15,5.596946E+15,  &
     &            7.700499E+15,1.052205E+16,1.425730E+16,1.913609E+16,  &
     &            2.546953E+16,3.366464E+16,4.421144E+16,5.775381E+16,  &
     &            7.514254E+16,9.747013E+16,1.261393E+17,1.629513E+17,  &
     &            2.102188E+17,2.709114E+17,3.488423E+17,4.489076E+17,  &
     &            5.773939E+17,7.423736E+17,9.542118E+17,1.226217E+18,  &
     &            1.575480E+18,2.023941E+18,2.599777E+18,3.339164E+18,  &
     &            4.288557E+18,5.507602E+18,7.072886E+18/
      data alo /-2.410106E-04,-5.471415E-04,-1.061586E-03,-1.879789E-03,&
     &          -3.166020E-03,-5.185436E-03,-8.216667E-03,-1.250894E-02,&
     &          -1.838597E-02,-2.631114E-02,-3.688185E-02,-5.096491E-02,&
     &          -7.004056E-02,-9.603746E-02,-1.307683E-01,-1.762946E-01,&
     &          -2.350226E-01,-3.095215E-01,-4.027339E-01,-5.178570E-01,&
     &          -6.581256E-01,-8.265003E-01,-1.024684E+00,-1.252904E+00,&
     &          -1.509470E+00,-1.788571E+00,-2.081700E+00,-2.379480E+00,&
     &          -2.675720E+00,-2.967325E+00,-3.252122E+00,-3.530485E+00,&
     &          -3.803720E+00,-4.072755E+00,-4.338308E+00,-4.601048E+00,&
     &          -4.861585E+00,-5.120370E+00,-5.377789E+00,-5.634115E+00,&
     &          -5.889388E+00,-6.143488E+00,-6.396436E+00,-6.648774E+00,&
     &          -6.901465E+00,-7.155207E+00,-7.409651E+00,-7.663536E+00,&
     &          -7.915682E+00,-8.165871E+00,-8.415016E+00/
      data uco2o /7.760162E+21,6.043619E+21,4.706775E+21,3.665639E+21,  &
     &            2.854802E+21,2.223321E+21,1.731524E+21,1.348511E+21,  &
     &            1.050221E+21,8.179120E+20,6.369897E+20,4.960873E+20,  &
     &            3.863524E+20,3.008908E+20,2.343332E+20,1.824981E+20,  &
     &            1.421289E+20,1.106894E+20,8.620418E+19,6.713512E+19,  &
     &            5.228412E+19,4.071814E+19,3.171055E+19,2.469544E+19,  &
     &            1.923206E+19,1.497717E+19,1.166347E+19,9.082750E+18,  &
     &            7.072886E+18,5.507602E+18,4.288557E+18,3.339164E+18,  &
     &            2.599777E+18,2.023941E+18,1.575480E+18,1.226217E+18,  &
     &            9.542118E+17,7.423736E+17,5.773939E+17,4.489076E+17,  &
     &            3.488423E+17,2.709114E+17,2.102188E+17,1.629513E+17,  &
     &            1.261393E+17,9.747013E+16,7.514254E+16,5.775381E+16,  &
     &            4.421144E+16,3.366464E+16,2.546953E+16,1.913609E+16,  &
     &            1.425730E+16,1.052205E+16,7.700499E+15,5.596946E+15,  &
     &            4.044901E+15/
      data (cor150(i),i=1,6) /2.158307E-01,1.423648E-01,1.204398E-01,   &
     &                        7.631008E-02,3.199162E-02,6.634180E-04/
      data (cor360(i),i=1,6) /5.661261E-01,4.570891E-01,3.339225E-01,   &
     &                        1.890883E-01,8.223990E-02,3.202579E-02/
      data (cor540(i),i=1,6) /6.959335E-01,6.015297E-01,4.714436E-01,   &
     &                        3.011133E-01,1.535200E-01,5.512478E-02/
      data (cor720(i),i=1,6) /8.005666E-01,6.939198E-01,5.522208E-01,   &
     &                        3.629121E-01,1.910115E-01,7.053397E-02/

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! T must be known from x=0 to x=xinter(face) (16.5-20). 
! Calculate Victor's pressure (kept in co2pro_mod) from surface x=0 to
! xinter 
      do l=1,ivict
         pvic(l)=1e5*exp(-xvic0(l))
      enddo
!     call mymaxmin(pvic,ivict,ivict,1,' co2cin: pvic ')
!
! Find first model layer just above xinter
!
      lraint=1
      do l=1,lmod
         if(xmod(l).ge.xinter) then
            lraint=l
            exit
         endif
      enddo
!
! Determine if model configuration is consistent (T must be known up
!     to xinter).
!
      if(xmod(lmod).lt.xinter) then
         write(6,*) '***Stop in co2cin: model UB below Xinterface***'
         write(6,*) ' lmod= ',lmod 
         write(6,*) ' xmod(lmod)= ',xmod(lmod)
         write(6,*) ' xinter= ',xinter
         stop
      endif

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Prepare CO2 profile
!
! F98 model profile
      call co2pro_pre(xmod,lmod,mu)
      write(6,*) 'F98 CO2 model'
!     call mymaxmin(xmod,ivict,ivict,1,' co2cin: xmod ')
! CRISTA based model profile
!      call crico2_pre(xmod,lmod,mu)
!      write(6,*) 'CRISTA CO2 model'

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate auxiliary arrays of global mean molecular weight and
! gravity on full Victor grid for CO2 column amount calculation.
! Find first level of Victor grid above lowest model layer and 
!       interpolate
      i=1
      do l=1,ivict
         if(xvic0(l).ge.xmod(1)) then
            i=l
            exit
         endif
      enddo
      j=i-1
      call splin1(xmod,mu,xvic0(i),vmu(i),lmod,ivict-j)
      call splin1(xmod,gr,xvic0(i),vgrav(i),lmod,ivict-j)
!
! Below x(i) assume constant mu and g
!
      do l=1,j
         vmu(l)=mu(1)
         vgrav(l)=gr(1)
      enddo
!     call mymaxmin(vmu,j,j,1,' co2cin: vmu ')
!     call mymaxmin(vgrav,j,j,1,' co2cin: vgrav ')
!
! Calculate CO2 column amounts for matrix interpolation and for
! CO2 heating rates in the near IR.
!
! CO2 column amount above model upper layer (coefficient .1 transfers
! it to cm**-2 if g and p are in SI units and CO2 molecular weight 
! is in g/mol)
!
      wcolmy(lmod)=.1*ana*co2my(lmod)*pmod(lmod)/(gr(lmod)*46.)
!
! CO2 column amount for model layers above x=xinter
!
      w1=.1*.5*ana
      do l=lmod-1,lraint,-1
         wcolmy(l)=wcolmy(l+1)+w1*(pmod(l)-pmod(l+1))*                  &
     &        (co2my(l)/(mu(l)*gr(l))+co2my(l+1)/(mu(l+1)*gr(l+1)))
      enddo
!
! CO2 column amount at and below xinter on Victor's parameterization
! grid.  Use column amount on model grid to calculate upper
! boundary condition.
!
      uco2(ivict)=wcolmy(lraint)+w1*(pvic(ivict)-pmod(lraint))*         &
     &     (co2vic(ivict)/(vmu(ivict)*vgrav(ivict))+                    &
     &     co2my(lraint)/(mu(lraint)*gr(lraint)))
      do l=ivict-1,1,-1
         uco2(l)=uco2(l+1)+w1*(pvic(l)-pvic(l+1))*                      &
     &        (co2vic(l)/(vmu(l)*vgrav(l))+                             &
     &        co2vic(l+1)/(vmu(l+1)*vgrav(l+1)))
      enddo
!
! Save column amounts for output and use in NIR calculations at
! x=2-14
!
      if(xinter.ge.14.) then
         do l=1,imnir
            co2nir(l)=uco2(l+8)
         enddo
      else
         write(6,*) 'Stop in co2cin: xinter < 14'
         stop
      endif

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Read in Victor's reference matrices (this part is adopted from parcof)
!
  100 format((/))
  101 format((1x,5E15.6))
!hmhj open(10,file=dir//'/coeff_lte.150',status = 'OLD')
!hmhj open(11,file=dir//'/coeff_lte.360',status = 'OLD')
!hmhj open(12,file=dir//'/coeff_lte.540',status = 'OLD')
!hmhj open(13,file=dir//'/coeff_lte.720',status = 'OLD')
!
!jw: only pe0 will read the data
      if(me==0) then

        open(30,file='global_idea_coeff_lte.150',status = 'OLD')
        open(31,file='global_idea_coeff_lte.360',status = 'OLD')
        open(32,file='global_idea_coeff_lte.540',status = 'OLD')
        open(33,file='global_idea_coeff_lte.720',status = 'OLD')
        rewind(30)
        rewind(31)
        rewind(32)
        rewind(33)
        read(30,100)
        read(31,100)
        read(32,100)
        read(33,100)
        do i=1,43
          read(30,101) (a150(i,j), j=1,57)
          read(31,101) (a360(i,j), j=1,57)
          read(32,101) (a540(i,j), j=1,57)
          read(33,101) (a720(i,j), j=1,57)
        enddo
!     call mymaxmin(a150,43*57,43*57,1,' co2cin a150 ')
!     call mymaxmin(a360,43*57,43*57,1,' co2cin a360 ')
!     call mymaxmin(a540,43*57,43*57,1,' co2cin a540 ')
!     call mymaxmin(a720,43*57,43*57,1,' co2cin a720 ')
        read(30,100)
        read(31,100)
        read(32,100)
        read(33,100)
        do i=1,43
          read(30,101) (b150(i,j), j=1,57)
          read(31,101) (b360(i,j), j=1,57)
          read(32,101) (b540(i,j), j=1,57)
          read(33,101) (b720(i,j), j=1,57)
        enddo
!     call mymaxmin(b150,43*57,43*57,1,' co2cin b150 ')
!     call mymaxmin(b360,43*57,43*57,1,' co2cin b360 ')
!     call mymaxmin(b540,43*57,43*57,1,' co2cin b540 ')
!     call mymaxmin(b720,43*57,43*57,1,' co2cin b720 ')
        close(30)
        close(31)
        close(32)
        close(33)
!
!jw pe 0 finish reading
      endif

      call mpi_bcast(a150,size(a150),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(a360,size(a360),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(a540,size(a540),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(a720,size(a720),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(b150,size(b150),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(b360,size(b360),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(b540,size(b540),mpi_ior,0,mpi_comm,info)
      call mpi_bcast(b720,size(b720),mpi_ior,0,mpi_comm,info)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! This is again from parcof with modifications: Linear interpolation
! is replaced by spline interpolation.
! Interplolate coefficients for the matrix paramerization on Victor 
! fixed grid:
! to calculate vamat(i,j) and vbmat(i,j) stored in co2c_mod,
! a spline interpolation of basic coefficients over the CO2 amount
! between x(i) and x(j) (or between x(i-1) and x(i+1) for vamat(i,i)
! and vbmat(i,i)), is used
!
      do i = 1, 43
         do j = 1, 57
            if(j.eq.(i+8)) then
               w1 = uco2o(i+7)-uco2o(i+9)
               war1(1)  = uco2(i+7)-uco2(i+9)
            else
               w1 = abs(uco2o(i+8)-uco2o(j))
               war1(1)  = abs(uco2(i+8)-uco2(j))
            end if
            uref(1) = w1*150./360.
            uref(2) = w1
            uref(3) = w1*540./360.
            uref(4) = w1*720./360.
            co2int(1)=a150(i,j)
            co2int(2)=a360(i,j)
            co2int(3)=a540(i,j)
            co2int(4)=a720(i,j)
            call splin1(uref,co2int,war1,war2,4,1)
!
! Coefficient 1e-4 transforms erg/(g*s) to W/kg
!
!            AMAT(i,j)=co2(i+8)*exp(a)
            vamat(i,j)=1e-4*co2vic(i+8)*exp(war2(1))
            co2int(1)=b150(i,j)
            co2int(2)=b360(i,j)
            co2int(3)=b540(i,j)
            co2int(4)=b720(i,j)
            call splin1(uref,co2int,war1,war2,4,1)
!
! Coefficient 1e-4 transforms erg/(g*s) to W/kg
!
!            BMAT(i,j)=co2(i+8)*exp(a)
            vbmat(i,j)=1e-4*co2vic(i+8)*exp(war2(1))
            if(j.eq.(i+7).or.j.eq.(i+8).or.j.eq.(i+9)) then
               vamat(i,j)=-vamat(i,j)
               vbmat(i,j)=-vbmat(i,j)
            end if
         enddo
      enddo

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate parameters for recurrent formula.
! This part adopted from parcof with some modifications: Spline
! interpolation instead of linear and escape functions set to 1 at 
! very small CO2 amounts.
! Calculate coeeficients for the reccurence formula:
! between x=12.5 and 13.75 these coefficients (al) are calculated using
! correction to escape function. Starting up from x=14.00 
! parameterization coeficients are equal escape function.
      do i=1,itm50
         alvic(i)=0.
      enddo
      do i=1,6
         if(uco2(i+50).lt.uco2ro(1)) then
            war1(1)=0.
         else
            call splin1(uco2ro,alo,uco2(i+50),war1,51,1)
         endif
         co2int(1)=cor150(i)
         co2int(2)=cor360(i)
         co2int(3)=cor540(i)
         co2int(4)=cor720(i)
         uref(1) =uco2o(i+50)*150./360.
         uref(2) =uco2o(i+50)
         uref(3) =uco2o(i+50)*540./360.
         uref(4) =uco2o(i+50)*720./360.
         call splin1(uref,co2int,uco2(i+50),war2,4,1)
         alvic(i)=exp(war1(1)+war2(1))
      enddo
!
! Above x=14 there is no correction
!
      do i=7,itm50
         if(uco2(i+50).lt.uco2ro(1)) then
            war1(1)=0.
         else
            call splin1(uco2ro,alo,uco2(i+50),war1,51,1)
         endif
         alvic(i)=exp(war1(1))
      enddo

      end subroutine co2cin

!***********************************************************************

      subroutine qnirc(ct,x,co2mr,qnirh,lx)

! Subroutine to calcuate NIR heating rate in a vertical column after
!       Ogibalov and Fomichev (2003)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Dec 2003: Updated
! May 23, 2003: Written by Rashid Akmaev after extensive modifications
! of Victor's co2nir

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use qnir_mod
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine arguments
! INPUT
!- model grid dimension
!
      integer,intent(in):: lx
!
!-cosine of solar zenith angle
!-model log-pressure x-grid GOING UP
!-CO2 VMR on model grid (note that since VMR does not change, the
!       column CO2 amounts on the fixed parameterization grid are kept
!       in qnir_mod). CO2 VMR is also kept in another module but is
!       called as an argument to ensure it is on the same grid as
!       the heating rate
!
      real,intent(in):: ct,x(lx),co2mr(lx)
!
! OUTPUT
!-heating rate in K/s (does not have to be divided by Cp)
!
      real,intent(out):: qnirh(lx)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Work space
!
      integer:: l
      integer,save:: lmnir
      logical:: first=.true.
      real:: g,dx,hr(imnir)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Check how high the model grid goes compared to param grid, assume
! model pressure near the top of param grid (x=14) does change with
! time (fixed pressure grid near x=14)
!
      if(first) then
         lmnir=lx
         do l=1,lx
            if(x(l).gt.xnir(imnir)) then
               lmnir=l-1
               exit
            endif
         enddo
!        print *,' qnirc: lmnir=',lmnir
         first=.false.
      endif
!
! Initialize heating rate
!
      do l=1,lx
         qnirh(l)=0.
      enddo
      if(ct.ge.0.) then
!
! Calculate heating, otherwise do nothing.
! First calculate normalized heating on parameterization grid using
! linear interpolation (it is assumed following original code that if
! the CO2 column amount exceeds the largest in the table, qco2n(10,l),
! the normalized heating rate is 0)
!
         do l=1,imnir
            g=log(co2nir(l)*35./SQRT(1224.*ct**2+1.))
            call lint1(lnco2n(1,l),qco2n(1,l),10,qco2n(1,l),            &
     &           0.,g,hr(l))
         enddo
!
! Now calculate heating on specified model grid.
! Do spline interpolation (replaced by linear interpolation June 18,
! 2004) below up to x=xnir(imnir), model grid index lmnir
!
         do l=1,lmnir
            call lint1(xnir,hr,imnir,xnir(1),xnir(imnir),x(l),          &
     &           qnirh(l))
            qnirh(l)=co2mr(l)*qnirh(l)
         enddo
!
! Do linear interpolation above, if applicable, to zero at xtopn
! (currently set to 15.5) and above
!
         if(lmnir.lt.lx) then
            g=hr(imnir)*rdxnir
            do l=lmnir+1,lx
               dx=xtopn-x(l)
               if(dx.ge.0.) then
                  qnirh(l)=co2mr(l)*dx*g
               else
                  qnirh(l)=0.
               endif
            enddo
         endif
      endif

      end subroutine qnirc

!***********************************************************************
! End file co2hc.f
!***********************************************************************
!***********************************************************************
! File splin.f
!***********************************************************************
! December 2006: created by Rashid Akmaev

! Simple spline interpolation subroutines optimized for various numbers
!     of input/output arrays.

! Contains
!      subroutine splin1(x1,y1,x2,y2,n1,n2)
! for y1[x1(n1)] -> y2[x2(n2)]
!      subroutine splin2(x1,y1,x2,y2,n1,n2,jm,km)
! for km transforms y1[km,x1(n1)] -> y2[km,x2(n2)] on the same grids
! x1 and x2, and jm - first dimension of y1 and y2

!***********************************************************************

      subroutine splin1(x1,y1,x2,y2,n1,n2)

! A simple routine to interpolate y1[x1(n1)] to y2[x2(n2)] using cubic
! spline.
! Both x1 and x2 are assumed to be ordered in THE SAME, ascending or
! descending, order.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Oct 2006: Rashid Akmaev

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine arguments
! INPUT
      integer,intent(in):: n1,n2
      real,intent(in):: x1(n1),y1(n1),x2(n2)
! OUTPUT
      real,intent(out):: y2(n2)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Internal parameters, work space
!
      real,parameter:: one_third=1./3.
      integer:: i,k,l,nvs
      real:: a(n1),dx,dxmh,dy(n1),e(n1),f(n1),g,h(n1),wx1(n1),wx2(n2)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Initialize output
!
      y2(:)=0.
!
! Simple check of argument order
!
      if(x1(1).lt.x1(n1)) then
         wx1(:)=x1(:)
         wx2(:)=x2(:)
      else
!
! Reverse x1 and x2 (changing sign seems easier than changing order)
!
         wx1(:)=-x1(:)
         wx2(:)=-x2(:)
      endif
!
! Prepare spline coefficients (Note: they depend on y and so have to
! be recalculated every time)

      nvs=n1-1
      do k=1,nvs
         h(k)=wx1(k+1)-wx1(k)
         dy(k)=(y1(k+1)-y1(k))/h(k)
      enddo
      a(1)=0.
      a(n1)=0.
      e(n1)=0.
      f(n1)=0.
      do k=nvs,2,-1
         g=1./(h(k)*e(k+1)+2.*(h(k-1)+h(k)))
         e(k)=-g*h(k-1)
         f(k)=g*(3.*(dy(k)-dy(k-1))-h(k)*f(k+1))
      enddo
      do k=2,nvs
         a(k)=e(k)*a(k-1)+f(k)
      enddo
!
! Calculate spline values
!
      l=1
      do i=1,n2
         do k=l,nvs
            dx=wx2(i)-wx1(k)
            dxmh=dx-h(k)
            l=k
            if(dxmh.le.0.) exit
         enddo
         g=dx/h(l)
         y2(i)=y1(l)+dx*(dy(l)+one_third*dxmh*(a(l)*(2.-g)+             &
     &        a(l+1)*(1.+g)))
      enddo

      end subroutine splin1

!***********************************************************************

      subroutine splin2(x1,y1,x2,y2,n1,n2,jm,km)

! A simple routine to interpolate km arrays y1[x1(n1)], specified on
! the same grid x1(n1), to km arrays y2[x2(n2)] on the same grid x2(n2)
! using cubic spline, where km<=jm and jm is the first dimension of 
! arrays y1 and y2.
! Both grids x1 and x2 are assumed to be ordered in THE SAME, ascending
! or descending, order.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Dec 2006: Rashid Akmaev
! Made from splin1

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine arguments
! INPUT
      integer,intent(in):: jm,km,n1,n2
      real,intent(in):: x1(n1),y1(jm,n1),x2(n2)
! OUTPUT
      real,intent(out):: y2(jm,n2)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Internal parameters, work space
!
      real,parameter:: one_third=1./3.
      integer:: i,j,k,l,nvs
      real:: a(km,n1),dx,dxmh,dy(km,n1),e(n1),f(km,n1),g,g2,h(n1),      &
     &     wx1(n1),wx2(n2)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Initialize output
!
      y2(:,:)=0.
!
! Simple check of argument order
!
      if(x1(1).lt.x1(n1)) then
         wx1(:)=x1(:)
         wx2(:)=x2(:)
      else
!
! Reverse x1 and x2 (changing sign seems easier than changing order)
!
         wx1(:)=-x1(:)
         wx2(:)=-x2(:)
      endif
!
! Prepare spline coefficients
!
      nvs=n1-1
      do k=1,nvs
         h(k)=wx1(k+1)-wx1(k)
         do j=1,km
            dy(j,k)=(y1(j,k+1)-y1(j,k))/h(k)
         enddo
      enddo
      e(n1)=0.
      do j=1,km
         a(j,1)=0.
         a(j,n1)=0.
         f(j,n1)=0.
      enddo
! 
! Calculate e and f coefficients
!
      do k=nvs,2,-1
         g=1./(h(k)*e(k+1)+2.*(h(k-1)+h(k)))
         e(k)=-g*h(k-1)
         do j=1,km
            f(j,k)=g*(3.*(dy(j,k)-dy(j,k-1))-h(k)*f(j,k+1))
         enddo
      enddo
! 
! Calculate a coefficients
!
      do k=2,nvs
         do j=1,km
            a(j,k)=e(k)*a(j,k-1)+f(j,k)
         enddo
      enddo
!
! Calculate spline values
!
      l=1
      do i=1,n2
         do k=l,nvs
            dx=wx2(i)-wx1(k)
            dxmh=dx-h(k)
            l=k
            if(dxmh.le.0.) exit
         enddo
         dxmh=one_third*dxmh
         g=1.+dx/h(l)
         g2=3.-g
         do j=1,km
            y2(j,i)=y1(j,l)+dx*(dy(j,l)+dxmh*(a(j,l)*g2+a(j,l+1)*g))
         enddo
      enddo

      end subroutine splin2

!***********************************************************************
! End file splin.f
!***********************************************************************
