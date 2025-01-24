 module clw_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   clw_mod
!   prgmmr: derber      org: np2                 date: 2010-08-19
!
! abstract: This module contains routines to calculate cloud liquid water
!
! program history log:
!   2010-08-19 derber combine retrieval_mi,ret_ssmis,retrieval_amsre and part of setuprad
!   2011-07-02 todling use general interface for intrisic FORTRAN math functions
!
! subroutines included:
!   sub calc_clw        - calculates cloud liquid water (clw) for microwave channels over ocean (public)
!   sub retrieval_mi    - calculates clw for ssmi
!   sub ret_ssmis       - calculates clw for ssmis
!   sub ret_amsua       - calculates clw for amsua 
!   sub retrieval_amsre - calculates clw for amsre
!   sub retrieval_gmi   - calculates clw and gwp for gmi
!   sub retrieval_amsr2 - calculates clw for amsr2
!   sub retrieval_saphir- calculates gwp for saphir
!   sub rcwps_alg       - makes retrieval for AMSR-E observation
!   sub tbe_from_tbo    - perform corrections for scattering effect in amsr-e obs
!   sub tba_from_tbe    - adjust amsr-e obs to algorithm based brightness temperature
!   sub emis_water      - compute oceanic emissivity
!   sub epsp            - calculate the real part of the dielectric constant for saline water
!   sub epspp           - calculate the imaginary part of the dialectric constant for saline water
!
! Variable Definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

implicit none
! set default to private
  private
! set routines used externally to public
  public :: calc_clw, ret_amsua, gmi_37pol_diff

contains


 subroutine calc_clw(nadir,tb_obs,tsim,ich,nchanl,no85GHz,amsua,ssmi,ssmis,amsre,atms, &   
          amsr2,gmi,saphir,tsavg5,sfc_speed,zasat,clw,tpwc,gwp,kraintype,ierrret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   calc_clw    estimates cloud liquid water for micro. QC
!   prgmmr: derber           org: np23                date: 1995-07-06
!
! abstract: estimates cloud liquid water for microwave quality control and
!        bias correction.
!
! program history log:
!   2010-08-19  derber
!   2011-05-20  mccarty - added placeholder for ATMS
!   2013-01-22  zhu - add adp_anglebc option
!   2013-07-19  zhu - include negative clw values for amsua or atms when adp_anglebc=.true.
!   2013-11-25  kim - revisit logic of frozen points
!   2014-12-03  ejones- added GMI and AMSR2, variable for gwp, and call to
!               retrieval_gmi subroutine.
!   2015-03-11  ejones- added call to retrieval_amsr2 subroutine
!   2015-03-23  ejones- added call to retrieval_saphir subroutine
!   2015-08-20  zhu  - set negative clw to be zero 
!   2016-11-07  sienkiewicz - Additional constraint on AMSUA/ATMS ch 1,2 
!                        for calculating CLW sensitivyt term to exclude 
!                        invalid BT values  Leave CLW sensitivity term as 0. 
!                        if retrieval failed
!
!  input argument list:
!     nadir     - scan position
!     tb_obs    - observed brightness temperatures
!     tsim      - simulated brightness temperatures             
!     ich       - channel number array
!     nchanl    - number of channels    
!     no85ghz   - flag for instrument with no 85ghz channel   
!     amsua     - flag for amsua data
!     ssmi      - flag for ssmi  data
!     ssmis     - flag for ssmis data
!     amsre     - flag for amsre data
!     atms      - flag for atms data
!     amsr2     - flag for amsr2 data
!     gmi       - flag for gmi data
!     saphir    - flag for saphir data
!     tsavg5    - Surface temperature value
!     sfc_speed - surface wind speed (10m)
!     zasat     - satellite zenith angle
!
!   output argument list:
!     clw       - cloud liquid water
!     gwp       - graupel water path                                                   
!     tpwc      - total column water vapor                                           
!     kraintype - rain type
!     ierrret   - return flag
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use radinfo, only: ang_rad,cbias,air_rad,predx,adp_anglebc
  use constants, only: zero,one,amsua_clw_d1,amsua_clw_d2,t0c,r1000

  integer(i_kind)                   ,intent(in   ) :: nadir,nchanl
  real(r_kind),dimension(nchanl)    ,intent(in   ) :: tb_obs,tsim
  integer(i_kind),dimension(nchanl) ,intent(in   ) :: ich
  logical                           ,intent(in   ) :: no85GHz,amsre,ssmi,ssmis,amsua,atms,amsr2,gmi,saphir
  real(r_kind)                      ,intent(in   ) :: tsavg5,sfc_speed,zasat
  real(r_kind)                      ,intent(  out) :: clw,tpwc,gwp
  integer(i_kind)                   ,intent(  out) :: kraintype,ierrret


! Declare local parameters
  real(r_kind),parameter:: r284=284.0_r_kind
  real(r_kind),parameter:: r285=285.0_r_kind
  real(r_kind),parameter:: tbmax=550.0_r_kind

! Declare local variables
  real(r_kind) tbcx1,tbcx2


  if (amsua .or. atms) then

     clw = zero
    ! We want to reject sea ice points that may be frozen.  The sea freezes
    ! around -1.9C but we set the threshold at 1C to be safe.
     if(tsavg5>t0c-one .and. tb_obs(1) > zero  .and. tb_obs(2) > zero .and.  &
                             tb_obs(1) < tbmax .and. tb_obs(2) < tbmax ) then 
        if (adp_anglebc) then
           tbcx1=tsim(1)+cbias(nadir,ich(1))*ang_rad(ich(1))+predx(1,ich(1))*air_rad(ich(1))
           tbcx2=tsim(2)+cbias(nadir,ich(2))*ang_rad(ich(2))+predx(1,ich(2))*air_rad(ich(2))
        else
           tbcx1=tsim(1)+cbias(nadir,ich(1))*ang_rad(ich(1))
           tbcx2=tsim(2)+cbias(nadir,ich(2))*ang_rad(ich(2))
        end if
        if (tbcx1 <=r284 .and. tbcx2<=r284 .and. tb_obs(1) > zero &
            .and. tb_obs(2) > zero) then 
             clw=amsua_clw_d1*(tbcx1-tb_obs(1))/(r285-tbcx1)+ &
                 amsua_clw_d2*(tbcx2-tb_obs(2))/(r285-tbcx2)
             ierrret = 0
        else
             ierrret = 1
        endif
     else
!       clw = r1000
        ierrret = 1
     end if
     
     if (.not. adp_anglebc) clw = max(zero,clw)

  else if(ssmi) then

     call retrieval_mi(tb_obs(1),nchanl,no85GHz, &
          tpwc,clw,kraintype,ierrret ) 
     clw = max(zero,clw)

  else if (ssmis) then

     call ret_ssmis( tb_obs(1),nchanl,tpwc, clw, ierrret)
     clw = max(zero,clw)

  else if (amsre) then

     call retrieval_amsre(tb_obs(1),zasat,           &
          sfc_speed,tsavg5,tpwc,clw,kraintype,ierrret ) 
     clw = max(zero,clw)

  else if (gmi) then           ! call retrieval_gmi

       call retrieval_gmi(tb_obs,nchanl,clw,gwp,kraintype,ierrret)
       clw=max(zero,clw)
       gwp=max(zero,gwp)

  else if (amsr2) then        ! call retrieval_amsr2

     call retrieval_amsr2(tb_obs,nchanl,clw,kraintype,ierrret)
     clw=max(zero,clw)

  else if (saphir) then       ! call retrieval_saphir
    
     call retrieval_saphir(tb_obs,zasat,nchanl,gwp,kraintype,ierrret)
     gwp=max(zero,gwp)

  endif

  return
 end subroutine calc_clw

  subroutine retrieval_mi(tb,nchanl,no85GHz,   &
                          tpwc,clw,kraintype,ierr)   
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    retrieval_mi    retrieve clw/tpw and identify rain for SSM/I
!   prgmmr: okamoto          org: np23                date: 2003-12-27
!
! abstract: rain identification based on scattering index 
!           retrieve cloud liquid water (clw) and total precipitable water (tpw) 
!   These algorithm are based on  
!    "SSM/I Algorithm Specification Document, 2000, Raytheon"
!   However, to speed-up, rain rate algorithm is reduced to just 
!   rain identification over sea using scattering index 
!
!   tpwc:: column water vapor over ocean  [kg/m2]
!           range 0-80kg/m2, precision:0.5kg/m2
!          Petty,G.,1993,Proceedings shared processing network 
!           DMSP SSM/I Alogorithm symposium Monterey
!   clw :: column cloud water ove ocean  [kg/m2]
!           range 0-6.0kg/m2, precision:0.5kg/m2
!          Weng et.al.,1997,J.Climate vol.10
!   NOTE!  Assume that this code is run only over sea
!   NOTE!  NOTE! if retrieved clw/tpwc not available over ocean, set -9.99e+11
!
!   NOTE!  This code is applicable to SSM/I only (20Jan2000)
!
! program history log:
!   2003-12-27  okamoto
!   2004-07-12  okamoto - simplify rain identification
!   2005-09-20  sienkiewicz - move tb22v test to avoid negative log evaluation
!   2005-10-20  kazumori - delete amsre
!   2006-04-27  derber - clean up
!   2006-12-20  sienkiewicz - add no85GHz workaround for f08 DMSP
!   2008-04-16  safford - rm unused vars
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!     no85GHz - SSM/I 85GHz channels not used
!
!   output argument list:
!     tpwc    - column water vapor over ocean  [kg/m2]
!     clw     - column water vapor over ocean  [kg/m2]
!     kraintype-rain type
!        [0]no rain or undefine  
!        [1]retrieve by emission -> not available now  
!        [2]by scattering
!     ierr    - error flag
!        [0]pass or escape this subroutine without doing anything
!        [1]tbb gross error  [2]polarization gross erro 
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: two,zero,r10,r100

  implicit none
    
! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl),intent(in   ) :: tb
  logical                       ,intent(in   ) :: no85GHz

  integer(i_kind)               ,intent(  out) :: kraintype,ierr
  real(r_kind)                  ,intent(  out) :: tpwc,clw

! Declare local variables
  real(r_kind)::tbpol(3),tb19v,tb19h,tb22v,tb37v,tb37h,tb85v,tb85h
  real(r_kind)::tpw,clw19,clw37,clw85
  real(r_kind)::clw2term
  real(r_kind)::rmis=-9.99e11_r_kind
!     si85    - scattering index over ocean
  real(r_kind):: si85
 
  real(r_kind),parameter:: r290=290.0_r_kind
  real(r_kind),parameter:: r285=285.0_r_kind
 
!  ======  Initialize products to missing

  tpwc   = rmis;  clw = rmis ;  si85 = rmis
  clw19  = zero;  clw37  = zero
  ierr = 0
  kraintype  = 0

  tb19v = tb(1); tb19h = tb(2); tb22v = tb(3)
  tb37v = tb(4); tb37h = tb(5); tb85v = tb(6); tb85h = tb(7)
 
 
!  =======  initial QC : gross check ===============

! Gross error check on all channels.  If there are any
! bad channels, skip this obs. 
  if ( no85GHz ) then                ! if no 85GHz, only check ch 1-5
     if ( any(tb(1:5)  < 70.0_r_kind) .or. any(tb(1:5) > 320.0_r_kind ) ) then
        ierr = 1
        return
     endif
  else
     if ( any(tb < 70.0_r_kind) .or. any(tb > 320.0_r_kind ) ) then
        ierr = 1
        return
     end if
  endif
 
! Polarization check based on SSMI/EDR Algorithm Specification Document
  tbpol(1) = tb19v-tb19h     !tb19V-tb19h
  tbpol(2) = tb37v-tb37h     !tb37V-tb37h
  tbpol(3) = tb85v-tb85h     !tb85V-tb85h


  if ( no85GHz ) then                ! if no 85GHz, just check 22,37 GHz
     if (tbpol(1) < -two .or. tbpol(2) < -two) then
        ierr = 2
     endif
     
  else if ( any(tbpol < -two ) ) then 
     ierr = 2
     return
  end if
 
!
! Use scattering index for rain rate, only if 85V channel can be used.
!    note: this expression ~ Ferraro, et al 1988 JAS
!
  if ( .not. no85GHz ) then

!  =======   Rain Rate  ==============

! Generate rain rates over ocean using Sec 3.6 algorithm
!

!    Compute scattering index
     si85 = -174.4_r_kind + 0.715_r_kind*tb19v +  &
          2.439_r_kind*tb22v - 0.00504_r_kind*tb22v*tb22v - tb85v

     if (si85 >= r10) then
        kraintype=2
     end if

  endif

!   Skip emission-based rain rate retrieve process kraintype=1 to speedup 
!   Observations contaminated by emission-based rain are tossed by 
!    later clw-QC

  if(kraintype==0) then 

!  =======   TPW over ocean (when no rain)  ====================
!   Generate total precipitable water (tpw) using Sec 3.1 algorithm.
!   If rain is present, can not retrieve tpw since squared tb22v
!   term greatly increases error in tpw.  

     tpw = 232.89393_r_kind - 0.148596_r_kind*tb19v - 0.36954_r_kind*tb37v - &
           (1.829125_r_kind - 0.006193_r_kind*tb22v)*tb22v

!   Apply cubic correction for high and low values.
     tpwc = -3.75_r_kind + 1.507_r_kind*tpw - 0.01933_r_kind*(tpw**2) &
           + 0.0002191_r_kind*(tpw**3)
     tpwc = max(zero,tpwc)

!  =======   CLW over ocean (when no rain)  ====================
!   Generate cloud liquid water (clw) using algorithm in Sec 3.2.
!   If rain or sea ice is present, operational algorithm does not
!   generate a product.  

     if (tb22v<r285) then
        clw2term=log(r290-tb22v)
!
!      CLW using 37v as primary channel
        if (tb37v<r285) then 
           clw37 = -1.66_r_kind*( log(r290-tb37v ) -  &
                    2.99_r_kind - 0.32_r_kind*clw2term )
           clw = clw37           ! default if none of the other critera satisfied
        end if

!
!      CLW using 19v as primary channel
        if (tb19v<r285) then 
           clw19  = -3.20_r_kind*( log(r290-tb19v) -  &
                     2.84_r_kind - 0.4_r_kind*clw2term  ) 
           if (clw19 > 0.7_r_kind) then 
              clw = clw19
           else if ( .not. no85GHz .and. &
                clw37 <= 0.28_r_kind .and. tb85h<r285) then 
!
!          CLW using 85h as primary channel
              if(tpwc < 30.0_r_kind)then
                 clw85 = -0.44_r_kind*( log(r290-tb85h) + & 
                          1.11_r_kind - 1.26_r_kind*clw2term   )
                 if (clw85>zero ) clw = clw85
              end if
           end if
        end if

     end if

!    upper limit of 6.0 kg/m2.  
     clw=min(clw,6.0_r_kind)

  end if                    
 
  return
end subroutine retrieval_mi
subroutine ret_ssmis(tb,nchanl,tpwc,clw,ierr)
!$$$ subprogram documentation block
!
! subprogram:    ret_ssmis    retrieve various parameters for SSMIS
!
! prgmmr: weng, okamoto     org: np23                date: 2005-03-22
!
! abstract: retrieve clw from sounding ch using only 50.3GHz channel 
!     retrieve clw from SSM/I-like channels after mimicing frequency
!
!     These algorithm are based on
!
!     Weng, F., R. R. Ferraro, and N. C. Grody,2000: "Effects of AMSU cross-scan Symmetry of 
!          brightness temperatures on  retrieval of atmospheric and surface parameters", 
!          Ed. P. Pampaloni and S. Paloscia, VSP, Netherlands, 255-262, 2000 
!
!     Yan B. and F. Weng, 'Intercalibration between Special Sensor Microwave Imager and Sounder (SSMIS)
!         and Special Sensor Microwave Imager (SSM/I)', TGARS Special Issue on the DMSP SSMIS, 46, 984-995.
!
!
!     tpw:: total precipitable water 
!     clw :: column cloud water ove ocean  [kg/m2]
!         range 0-6.0kg/m2, precision:0.5kg/m2
!         Weng et.al.,1997,J.Climate vol.10   
!
! program history log:
!     2003-12-27  okamoto
!     2004-07-12  okamoto - simplify rain identification
!     2005-10-07  Xu & Pawlak - add documentation. Also, per Fuzhong Weng, deleted
!                 code for retrieval based on ssmis_las instrument since
!                 algorithm is intented only for ssmis_img, fixed indentation 
!     2006-04-27  Derber - modify for single profile
!     2007-01-24  kazumori - modify for UKMO preprocess SSMIS data, add retrieved tpw
!                            and bias correction of retrieved clw.
!     2008-04-16  safford  - rm unused uses and vars
!
!     2009-12-07  b.yan    - rewrite the ssmis TA level CLW agorithm and 
!                            remove the sea ice identification algorithm since this code frequently 
!                                   mis-identify ocean-pixels as sea ice-pixels
!
!     Reasons: in the previous version,there are the following bugs
!              (a) the ssmis data used in the gsi is tb level insetad of ta level
!              (b) the ssmis remapping coefficients are derived at ta level instead of tb level
!              (c) the polarization of the ssmis data is mis-used which will result in a totally wrong
!                  cloud liquid water path calculation
!              (d) the comments for the coefficients are not correct
!     In the new version of the code:
!              (a) TB2TA is needed to use the ssmis remapping coefficients derived at the ta level
!              (b) the remapping coefficients are updated , which are derived using the ssmis/ssmi SCO
!                  observations
!              (c) add the correct comments
!              (d) remove sea ice index and subroutine
!              (e) remove ssmis_las check if it is not necessary
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!     
!   Internal variable list:
!     tbx    - brightness temperatures at seven window channels
!     tby    - brightness temperatures at seven window channels which are consistent to f15 ssmi tb results
!     tax    - ssmis antenna temperatures at seven window channels
!     tay    - remapped ssmis antenna temperatures at seven window channels
!   output argument list:
!     clw     - column cloud water vapor over ocean  [kg/m2]
!     ierr    - error flag
!               [0]pass or escape this subroutine without doing anything
!               [1]tbb gross error
!               [2]polarization gross erro
!
! attributes:
!     language: f90
!     machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: zero,one
  
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl),intent(in   ) :: tb
  integer(i_kind)               ,intent(  out) :: ierr
  real(r_kind)                  ,intent(  out) :: tpwc
  real(r_kind)                  ,intent(  out) :: clw

! Declare local variables
  real(r_kind)::rmis=-9.99e11_r_kind
  integer(i_kind) :: i
  real(r_kind),parameter:: r285=285.0_r_kind
  real(r_kind),parameter:: r290=290.0_r_kind
  real(r_kind),dimension(7):: ap,bp,cp,dp,cp0,dp0,tbx,tby,tax,tay
  real(r_kind):: alg1,alg2,alg3

  ierr = 0
  clw = rmis

! Coefficients to remap SSMIS ssmi-like channels to SSMI TB
! Simultaneous Conical Overpass (SCO) data set-derived coefficients which are more accurate
!
  ap = (/0.00424_r_kind, -2.03627_r_kind, -2.52875_r_kind, 0.80170_r_kind, &
       -3.86053_r_kind, -7.43913_r_kind,1.53650_r_kind/)
  bp = (/1.00027_r_kind, 1.00623_r_kind, 0.99642_r_kind, 0.99139_r_kind, &
       1.00550_r_kind, 1.03121_r_kind, 0.99317_r_kind/)
     
! cp0 and dp0 for TA2TB
  cp0 = (/.969_r_kind, .969_r_kind, .974_r_kind, .986_r_kind,  &
          .986_r_kind, .988_r_kind, .988_r_kind/)
  dp0 = (/.00415_r_kind,.00473_r_kind,.0107_r_kind,.02612_r_kind,  &
           .0217_r_kind, .01383_r_kind, .01947_r_kind/)

  do i=1,7
! save brightness temperatures at seven window channels
     tbx(i) = tb(11+i)
! get ta2tb coefficients
     cp(i) = one/( cp0(i)*(one-dp0(i)) )
     dp(i) = cp(i) * dp0(i)
  end do

!   get ta from tb
  tax(1) = (tbx(1)*cp(2) + tbx(2)*dp(1))/(cp(1)*cp(2) - dp(1)*dp(2))
  tax(2) = (tbx(1)*dp(2) + tbx(2)*cp(1))/(cp(1)*cp(2) - dp(1)*dp(2))
  tax(3) = one/cp(3)*(tbx(3) + dp(3)*(.653_r_kind*tax(2)+ 96.6_r_kind))
  tax(4) = (tbx(4)*cp(5) + tbx(5)*dp(4))/(cp(4)*cp(5) - dp(4)*dp(5))
  tax(5) = (tbx(4)*dp(5) + tbx(5)*cp(4))/(cp(4)*cp(5) - dp(4)*dp(5))
  tax(6) = (tbx(6)*cp(7) + tbx(7)*dp(6))/(cp(6)*cp(7) - dp(6)*dp(7))
  tax(7) = (tbx(6)*dp(7) + tbx(7)*cp(6))/(cp(6)*cp(7) - dp(6)*dp(7))

! TAREMAPPING: Mimic ssmis imager channels to corresponding ssmis channels at ta level
! (So, tay is antenna temperature consistent to f15 ssmi ta)
  do i=1,7
     tay(i) =  ap(i) + bp(i)*tax(i)
  end do

! TA level CLW algorithm
  alg1 = rmis
  if( tay(2)<r285 .and. tay(3)<r285 ) then
     alg1 = -3.20_r_kind*(  &
          log( r290-tay(2) ) - 2.80_r_kind - 0.42_r_kind*log( r290-tay(3) )  )
  end if


! Determine clw
  if( alg1 > 0.70_r_kind ) then 
     clw = alg1
  else
     alg2=rmis
     if( tay(5)<r285 .and. tay(3)<r285 ) then
        alg2 = -1.66_r_kind*(  &
          log( r290-tay(5) ) - 2.90_r_kind - 0.349_r_kind*log( r290-tay(3) )  );
     end if
     if( alg2 > 0.28_r_kind ) then 
        clw = alg2
     else
!       Get ssmis tb consisitent to f15 ssmi tb
!       TA2TB(F15): Coefficients for tay to tby conversion

        tby(1)  =  cp(1)*tay(1) - dp(1)*tay(2);
!       tby(2)  =  cp(2)*tay(2) - dp(2)*tay(1);
        tby(3)  =  cp(3)*tay(3) - dp(3)*(.653_r_kind*tay(2) + 96.6_r_kind);
        tby(4)  =  cp(4)*tay(4) - dp(4)*tay(5);
!       tby(5)  =  cp(5)*tay(5) - dp(5)*tay(4);
!       tby(6)  =  cp(6)*tay(6) - dp(6)*tay(7);
!       tby(7)  =  cp(7)*tay(7) - dp(7)*tay(6);
     
!       Calculate parameter for clw
        tpwc = 232.89_r_kind - 0.1486_r_kind*tby(1) - 0.3695_r_kind*tby(4)  & 
            - ( 1.8291_r_kind - 0.006193_r_kind*tby(3) )*tby(3)
        if( tpwc < 30.0_r_kind ) then 
           alg3=rmis
           if( tay(7)<r285 .and. tay(3)<r285 ) then
              alg3 = -0.44_r_kind*( &
                   log( r290-tay(7) ) + 1.60_r_kind - 1.354_r_kind*log( r290-tay(3) )  )
           end if
           clw = alg3
        else
           clw = alg2
        end if
     end if
  end if
! clw qc
  clw = max(0.0_r_kind,min(clw,6.0_r_kind))

  return
end subroutine ret_ssmis

subroutine retrieval_amsre(tb,degre,  &
                        sfc_speed, sst, &
                        tpwc,clw,kraintype,ierr )

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  retrieval_amsre         make retrieval from AMSR-E observation
!
!   prgmmr: kazumori          org: np23                date: 2005-10-20
!
! abstract: This subroutine create retrievals from AMSR-E observation
!
! program history log:
!   2005-10-20  kazumori - create retrieval subroutine for AMSR-E
!   2005-10-21  kazumori - reformated for GSI
!   2006-04-26  kazumori - removed extra qc and comment changed
!   2006-07-27  kazumori - modified bias correction of retrieval
!                          and clean up the code
!   2008-04-16  safford  - rm unused uses and vars
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     amsre_low   - logical true if amsre_low is processed
!     amsre_mid   - logical true if amsre_mid is processed
!     amsre_hig   - logical true if amsre_hig is processed
!     sfc_speed   - guess wind speed at 10m
!     sst   - sea surface temperature[K]
!
!   output argument list:
!     tpwc    - column water vapor over ocean  [kg/m2]
!     clw     - column water vapor over ocean  [kg/m2]
!     kraintype - kraintype flag
!     ierr    - error flag
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: zero

  implicit none
  
! Input variable
  real(r_kind),dimension(12),intent(in   ) :: tb
  real(r_kind)              ,intent(in   ) :: sfc_speed
  real(r_kind)              ,intent(in   ) :: sst,degre

! Output variable
  integer(i_kind)           ,intent(  out) :: kraintype,ierr
  real(r_kind)              ,intent(  out) :: tpwc,clw

! Internal variable
  integer(i_kind) :: nchanl1
  real(r_kind) :: rwp,cwp,vr,vc
!     si85    - scattering index over ocean
  real(r_kind) :: si85

! Initialize variable
  nchanl1 = 12   ! Total AMSR-E channel number=12
  ierr = 0; kraintype=0
  rwp =zero;cwp=zero;vr=zero;vc=zero

! Gross error check on all channels.  If there are any
! bad channels, skip this obs.

  if ( any(tb < 50.0_r_kind) .or. any(tb > 400.0_r_kind ) ) then
     ierr = 1
     return
  end if

! Currently rwp and vc computations commented out since not used
  call RCWPS_Alg(degre,tb,sst,sfc_speed,rwp,cwp,vr,vc)

  tpwc=vr  ! 18.7GHz
!  tpwc=vc ! 36.5GHz
  clw=cwp
  clw = clw - 0.03_r_kind   ! remove bias
  si85=rwp

!  =======   TPW over ocean (when no rain)  ====================

  if(kraintype==0) then
     tpwc = max(zero,tpwc)

!  =======   CLW over ocean (when no rain)  ====================


!    Ensure clw is non-negative.
     clw    = max(zero,clw)
 
!     upper limit of 6.0 kg/m2.
!     if(clw>6.0_r_kind) clw=zero

  end if  !no rain

  return
end subroutine retrieval_amsre

subroutine retrieval_gmi(tb,nchanl,clw,gwp,kraintype,ierr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: retrieval_gmi   make retrieval from GMI observation
!   prgmmr: ejones            org: jcsda                date: 2014-11-15
!
! abstract: algorithm developped by kgarrett to retrieve clw and gwp for GPM GMI
!   observations. Retrieved clw and gwp used in qc_gmi to filter out obs
!   contaminated by cloud and precip.
!
!   NOTE: regressions assume ocean-only surface
!   NOTE: regressions are valid only for points where all channels are present
!   (i.e. not at the edges of swath1, where no observations from ch10-13 can be
!   co-registered with observations from ch1-9)
!
! program history log:
!   2014-11-15  ejones
!   2015-02-13  ejones - set clw high over swath1 edges so these points can be
!                        reliably filtered out in QC
!   2015-07-15  ejones - add systematic "bias correction" to GMI TBs prior to
!                        retrievals
!   2016-06-06  ejones - remove unused pred_var_gwp value
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!
!   output argument list:
!     clw     - column water vapor over ocean  [kg/m2]
!     gwp     - column ice over ocean          [kg/m2]
!     kraintype-rain type
!        [0]no rain or undefine
!        [1]retrieve by emission -> not available now
!        [2]by scattering
!     ierr    - error flag
!        [0]pass or escape this subroutine without doing anything
!        [1]tbb gross error
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind

  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl),intent(in   ) :: tb

  integer(i_kind)               ,intent(  out) :: kraintype,ierr
  real(r_kind)                  ,intent(  out) :: clw,gwp

! Declare local variables
  integer(i_kind)::tb_index(9)
  integer(i_kind)::nchan_reg,nvar_clw,nvar_gwp
  real(r_kind)::regr_coeff_clw(11),pred_var_clw(2)
  real(r_kind)::regr_coeff_gwp(10),pred_var_gwp
  real(r_kind)::sys_bias(13),tb_use(13)
  real(r_kind)::a0_clw,a0_gwp
!  real(r_kind)::tb10v,tb10h,tb18v,tb18h,tb23v,tb37v,tb37h,tb89v,tb89h,tb166v,tb166h,tb183v,tb183h

  integer(i_kind)::i,idx,diff_var
! ---------- Initialize some variables ---------------------

  kraintype = 0
  ierr = 0

  ! Brightness temperatures used for training CLW and GWP retrievals were
  ! simulated from ECMWF fields collocated with GMI observations. The retrievals
  ! here use actual GMI brightness temperatures, so for best results, a
  ! "systematic bias" (i.e. an average difference between GMI brightness
  ! temperatures and those simulated from ECMWF fields) is removed from GMI
  ! brightness temperatures prior to performing retrievals

  ! systematic bias
  sys_bias= (/ 1.7942_r_kind, 1.7793_r_kind, 3.7619_r_kind, 2.9459_r_kind,&
               1.8344_r_kind, 0.5227_r_kind, 0.1192_r_kind, 1.2375_r_kind,&
               1.3450_r_kind, -3.7479_r_kind, -3.5269_r_kind, -1.0282_r_kind,&
               -2.0826_r_kind /)

  ! brightness temperatures to use
  tb_use(1)=(tb(1)-sys_bias(1)); tb_use(2)=(tb(2)-sys_bias(2)); tb_use(3)=(tb(3)-sys_bias(3)) 
  tb_use(4)=(tb(4)-sys_bias(4)); tb_use(5)=(tb(5)-sys_bias(5)); tb_use(6)=(tb(6)-sys_bias(6)) 
  tb_use(7)=(tb(7)-sys_bias(7)); tb_use(8)=(tb(8)-sys_bias(8)); tb_use(9)=(tb(9)-sys_bias(9))
  tb_use(10)=(tb(10)-sys_bias(10)); tb_use(11)=(tb(11)-sys_bias(11)); tb_use(12)=(tb(12)-sys_bias(12)) 
  tb_use(13)=(tb(13)-sys_bias(13))

  ! intercepts
  a0_clw = -0.61127_r_kind
  a0_gwp = -3541.46329_r_kind

  nchan_reg = 9       ! number of channels used in regression
  nvar_clw = 11       ! number of independent variables in clw regression
  nvar_gwp = 10       ! number of independent variables in gwp regression

  ! channels used in regression
  tb_index = (/ 1, 2, 3, 4, 5, 6, 7, 12, 13 /)

  ! regression coefficients
  regr_coeff_clw = (/ 0.00378_r_kind, -0.00149_r_kind, -0.03438_r_kind, 0.01670_r_kind,&
                      0.00228_r_kind, 0.03884_r_kind, -0.02345_r_kind, -0.00036_r_kind,&
                      0.00044_r_kind, 1.95559_r_kind, -2.15143_r_kind /)

  regr_coeff_gwp = (/ 0.00393_r_kind, 0.00088_r_kind, -0.00063_r_kind, -0.00683_r_kind,&
                      0.00333_r_kind, -0.00382_r_kind, 0.00452_r_kind, 0.04765_r_kind,&
                     -0.00491_r_kind, 11.98897_r_kind /)

! ---------- Calculate predictors ---------------------------

  if(tb_use(4) > tb_use(3)) then
     return
  endif

  pred_var_clw(1) = log(tb_use(3)-tb_use(4))  !(tb18v - tb18h)
  pred_var_clw(2) = log(tb_use(6)-tb_use(7))   !(tb37v - tb37h)

  pred_var_gwp = 300.0_r_kind-log(tb_use(12))   !(tb183v)

! ---------- Gross check ------------------------------------
! Gross error check on all channels.  If there are any
! bad channels, skip this obs.

  if ( any(tb(1:9) < 50.0_r_kind) .or. any(tb(10:13) < 70.0_r_kind ) ) then
     ierr = 1
     return
  end if

! ---------- Apply regression to calculate clw and gwp ------

  ! calculate clw first
  clw = a0_clw
  diff_var = nvar_clw - nchan_reg  ! difference in number of channels used
                                   ! and independent variables

  ! loop over variables
  ! start with spectral independent variables
  if ( nchan_reg > 0 )then
    do i=1,nchan_reg
      idx = tb_index(i)
      clw = clw + ( tb_use(idx) * regr_coeff_clw(i) )
    enddo
  endif
  ! weight by non-spectral independent variables
  if ( nvar_clw > nchan_reg ) then
    do i=1,diff_var
      clw = clw + ( pred_var_clw(i) * regr_coeff_clw(i+nchan_reg) )
    enddo
  endif

  ! set maximum for clw at 6.0 kg/m2
  clw = min(clw,6.0_r_kind)

  ! if the observation is on a swath edge (where ch10-13 don't have
  ! values in the data, but are assigned high TBs (500K) in the reader), set clw
  ! to 999.0
  ! so these observations can be flagged for swath edge in qc_gmi

  if((tb(10) > 490.0_r_kind) .and. (tb(11) > 490.0_r_kind) .and. (tb(12) > 490.0_r_kind) &
     .and. (tb(13) > 490.0_r_kind)) then
     clw=999.0_r_kind
  end if

  ! calculate gwp
  gwp = a0_gwp
  diff_var = nvar_gwp - nchan_reg

  ! loop over variables
  ! spectral independent variables
  if ( nchan_reg > 0 )then
    do i=1,nchan_reg
      idx = tb_index(i)
      gwp = gwp + ( tb_use(idx) * regr_coeff_gwp(i) )
    enddo
  endif
  ! weight by non-spectral independent variables
  if ( nvar_gwp > nchan_reg ) then
    do i=1,diff_var
      gwp = gwp + ( pred_var_gwp * regr_coeff_gwp(i+nchan_reg) )
    enddo
  endif

  ! flag convective precip
  if ( gwp > 0.05_r_kind) then
    kraintype = 2
  endif

  return
end subroutine retrieval_gmi


subroutine retrieval_amsr2(tb,nchanl,clw,kraintype,ierr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: retrieval_amsr2   make retrieval from AMSR2 observation
!   prgmmr: ejones            org: jcsda                date: 2015-03-06
!
! abstract: algorithm developed by kgarrett to retrieve clw and gwp for
!   GCOMW1 AMSR2 observations. Retrieved clw and gwp used in qc_amsr2 to
!   filter out obs contaminated by cloud and precip.
!
!   NOTE: regressions assume ocean-only surface
!   NOTE: regressions are valid only for AMSR2 channels 1-14 (15-16 contain
!   redundant info)
!
! program history log:
!   2014-11-15  ejones
!   2015-10-02  ejones     - update for better retrieval of AMSR2 cloud
!   2015-10-07  ejones     - add bias correction to TBs prior to CLW retrieval
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!
!   output argument list:
!     clw     - column water vapor over ocean  [kg/m2]
!     kraintype-rain type
!        [0]no rain or undefine
!        [1]retrieve by emission -> not available now
!        [2]by scattering
!     ierr    - error flag
!        [0]pass or escape this subroutine without doing anything
!        [1]tbb gross error
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind

  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl),intent(in   ) :: tb

  integer(i_kind)               ,intent(  out) :: kraintype,ierr
  real(r_kind)                  ,intent(  out) :: clw  !gwp

! Declare local variables
  integer(i_kind)::tb_index(1)
  integer(i_kind)::nchan_reg,nvar_clw   !nvar_gwp
  real(r_kind)::regr_coeff_clw(3),pred_var_clw(2),sys_bias(14),tb_use(14)
  real(r_kind)::a0_clw
  real(r_kind)::tb6v,tb6h,tb7v,tb7h,tb10v,tb10h,tb18v,tb18h,tb23v,tb23h,tb36v,tb36h,tb89v,tb89h

  integer(i_kind)::i,idx,diff_var
! ---------- Initialize some variables ---------------------

  kraintype = 0
  ierr = 0

  tb6v=tb(1); tb6h=tb(2); tb7v=tb(3); tb7h=tb(4); tb10v=tb(5); tb10h=tb(6)
  tb18v=tb(7); tb18h=tb(8); tb23v=tb(9); tb23h=tb(10); tb36v=tb(11)
  tb36h=tb(12); tb89v=tb(13); tb89h=tb(14)

  ! Brightness temperatures used for training CLW retrievals were
  ! simulated from ECMWF fields collocated with AMSR2 observations. The retrievals
  ! here use actual AMSR2 brightness temperatures, so for best results, a
  ! "systematic bias" (i.e. an average difference between AMSR2 brightness
  ! temperatures and those simulated from ECMWF fields) is removed from AMSR2
  ! brightness temperatures prior to performing retrievals

  ! systematic bias
  sys_bias= (/ 0.4800_r_kind, 3.0737_r_kind, 0.7433_r_kind, 3.6430_r_kind,&
               3.5304_r_kind, 4.4270_r_kind, 5.1448_r_kind, 5.0785_r_kind,&
               4.9763_r_kind, 9.3215_r_kind, 2.5789_r_kind, 5.5274_r_kind,&
               0.6641_r_kind, 1.3674_r_kind /)

  ! brightness temperatures to use
  tb_use(1)=(tb(1)-sys_bias(1)); tb_use(2)=(tb(2)-sys_bias(2)); tb_use(3)=(tb(3)-sys_bias(3))
  tb_use(4)=(tb(4)-sys_bias(4)); tb_use(5)=(tb(5)-sys_bias(5)); tb_use(6)=(tb(6)-sys_bias(6))
  tb_use(7)=(tb(7)-sys_bias(7)); tb_use(8)=(tb(8)-sys_bias(8)); tb_use(9)=(tb(9)-sys_bias(9))
  tb_use(10)=(tb(10)-sys_bias(10)); tb_use(11)=(tb(11)-sys_bias(11)); tb_use(12)=(tb(12)-sys_bias(12))
  tb_use(13)=(tb(13)-sys_bias(13)); tb_use(14)=(tb(14)-sys_bias(14))

!   tb_index = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 /)
  tb_index = (/12/)                  ! use only Ch 12 in CLW retrieval

  ! intercepts
  a0_clw = -0.65929_r_kind

  nchan_reg = 1
  nvar_clw = 3

  ! regression coefficients
  regr_coeff_clw = (/-0.00013_r_kind, 1.64692_r_kind, -1.51916_r_kind /)


! ---------- Calculate predictors ---------------------------

  pred_var_clw(1) = log(tb_use(7) - tb_use(8))      !(tb18v - tb18h)
  pred_var_clw(2) = log(tb_use(11) - tb_use(12))    !(tb36v - tb36h)

! ---------- Gross check ------------------------------------
! Gross error check on all channels.  If there are any
! bad channels, skip this obs.

  if ( any(tb(1:14) < 2.7_r_kind) .or. any(tb(1:14) > 340.0_r_kind ) ) then
     ierr = 1
     return
  end if

! ---------- Apply regression to calculate clw --------------

  ! calculate clw first
  clw = a0_clw
  diff_var = nvar_clw - nchan_reg  ! difference in number of channels used
                                   ! and independent variables

  ! loop over variables
  ! start with spectral independent variables
  if ( nchan_reg > 0 )then
    do i=1,nchan_reg
      idx = tb_index(i)
      clw = clw + ( tb_use(idx) * regr_coeff_clw(i) )
    enddo
  endif
  ! weight by non-spectral independent variables
  if ( nvar_clw > nchan_reg ) then
    do i=1,diff_var
      clw = clw + ( pred_var_clw(i) * regr_coeff_clw(i+nchan_reg) )
    enddo
  endif

  ! set maximum for clw at 6.0 kg/m2
  clw = min(clw,6.0_r_kind)

! ---------- Apply regression to calculate gwp ------------
! Still need to work on this

  return
end subroutine retrieval_amsr2

subroutine retrieval_saphir(tb,iang,nchanl,gwp,kraintype,ierr)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: retrieval_saphir   make retrieval from SAPHIR observation
!   prgmmr: ejones            org: jcsda                date: 2015-03-23
!
! abstract: algorithm developped by kgarrett to retrieve clw and gwp for
!   Megha-Tropiques SAPHIR observations, by angle bin. Retrieved gwp is
!   used for screening observations for precipitation.
!
!   NOTE: regressions assume ocean-only surface
!
! program history log:
!   2015-03-23  ejones
!
!   input argument list:
!     tb      - Observed brightness temperature [K]
!     nchanl  - number of channels per obs
!     iang    - incidence angle
!
!   output argument list:
!     gwp     - column ice over ocean          [kg/m2]
!     kraintype-rain type
!        [0]no rain or undefine
!        [1]retrieve by emission -> not available now
!        [2]by scattering
!     ierr    - error flag
!        [0]pass or escape this subroutine without doing anything
!        [1]tbb gross error
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind

  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: nchanl
  real(r_kind)                  ,intent(in   ) :: iang
  real(r_kind),dimension(nchanl),intent(in   ) :: tb

  integer(i_kind)               ,intent(  out) :: kraintype,ierr
  real(r_kind)                  ,intent(  out) :: gwp

! Declare local variables
  real(r_kind)         ::abs_iang
  integer(i_kind)      ::tb_index(6)
  integer(i_kind)      ::nchan_reg,nvar_gwp,bin_no
  real(r_kind)         ::a0_gwp(5)
  real(r_kind)         ::angle_bin(6)
  real(r_kind)         ::regr_coeff_gwp(5,6)

  integer(i_kind)      ::i,idx
! ---------- Initialize some variables ---------------------

  kraintype = 0
  ierr = 0

  abs_iang = abs(iang)    ! make sure the incidence angle is positive
  bin_no = 0

  ! intercepts
  a0_gwp = (/ 4.04166_r_kind, 3.86160_r_kind, 3.37189_r_kind, &
              2.33067_r_kind, 1.95102_r_kind /)

  nchan_reg = 6       ! number of channels used in regression
  nvar_gwp = 6        ! number of independent variables in gwp regression

  ! channels used in regression
  tb_index = (/ 1, 2, 3, 4, 5, 6 /)

  ! angle bins used in regression
  angle_bin = (/ 0.0_r_kind, 10.0_r_kind, 20.0_r_kind, 30.0_r_kind, &
                 40.0_r_kind, 50.0_r_kind /)

  ! regression coefficients, one set for each angle bin
  regr_coeff_gwp(1,:) = (/ -0.00085_r_kind, -0.00938_r_kind, 0.04594_r_kind, &
                           -0.05311_r_kind, 0.00345_r_kind, -0.00110_r_kind /)
  regr_coeff_gwp(2,:) = (/ -0.00478_r_kind, -0.00388_r_kind, 0.06688_r_kind, &
                           -0.11137_r_kind, 0.06239_r_kind, -0.02363_r_kind /)
  regr_coeff_gwp(3,:) = (/ 0.00041_r_kind, -0.01416_r_kind, 0.08750_r_kind, &
                           -0.13836_r_kind, 0.08119_r_kind, -0.02905_r_kind /)
  regr_coeff_gwp(4,:) = (/ 0.00344_r_kind, -0.01420_r_kind, 0.08930_r_kind, &
                           -0.14820_r_kind, 0.09556_r_kind, -0.03418_r_kind /)
  regr_coeff_gwp(5,:) = (/ 0.00913_r_kind, -0.02569_r_kind, 0.11245_r_kind, &
                           -0.17690_r_kind, 0.11369_r_kind, -0.03943_r_kind /)

! ----------- Loop over bins and apply regression to calculate gwp -----------

  ! get bin number
  if ( (abs_iang >= angle_bin(1)) .and. (abs_iang < angle_bin(2)) ) then
    bin_no = 1
  else if ( (abs_iang >= angle_bin(2)) .and. (abs_iang < angle_bin(3)) ) then
    bin_no = 2
  else if ( (abs_iang >= angle_bin(3)) .and. (abs_iang < angle_bin(4)) ) then
    bin_no = 3
  else if ( (abs_iang >= angle_bin(4)) .and. (abs_iang < angle_bin(5)) ) then
    bin_no = 4
  else if ( (abs_iang >= angle_bin(5)) .and. (abs_iang < angle_bin(6)) ) then
    bin_no = 5
  else if ( (abs_iang < 0.0_r_kind) .or. (abs_iang > 50.0_r_kind) ) then
    write(6,*)'retrieval_saphir: could not determine angle bin.'
  endif

  ! calculate gwp
  gwp = a0_gwp(bin_no)

  ! loop over variables
  ! spectral independent variables
  if ( (nchan_reg > 0) .and. (bin_no /= 0) )then
    do i=1,nchan_reg
      idx = tb_index(i)
      gwp = gwp + ( tb(idx) * regr_coeff_gwp(bin_no,i) )
    enddo
  endif

  ! flag convective precip
  if ( gwp > 0.05_r_kind) then
    kraintype = 2
  endif

  return
end subroutine retrieval_saphir

subroutine RCWPS_Alg(theta,tbo,sst,wind,rwp,cwp,vr,vc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: RCWPS_Alg   make retrieval from AMSR-E observation
!
!   prgrmmr: Banghua Yan and Fuzhong Weng   org: NESDIS              date: 2004-09-20
!
! abstract:
!     Retrieve rain water path, cloud water path, and total precipitable water over oceans
!              from AMSR-E measurements
!     Please refer to the following paper for details
!
!  (1) Weng. F., L. Zhao, R. R. Ferraro, G. Poe, X. Li., and N. Grody, 2003:
!      Advanced microwave sounding unit cloud and precipitation algorithms, Radio Science, 38,
!      Mar 33, 1-12.
!  (2) Yan, B. and F. Weng,2004: "Rain and cloud water paths derived from Aqua AMSR-E measurements',
!     the 13th conference on satellite meteorolgy and oceanography, Norfolk in VA, Sept. 20-23.
!  (3) Yan, B. and F. Weng: New applications of AMSR-E measurements under tropical cyclones,
!      Part II: retrieval of liquid water path
!      submitted to J. Geophys. Res., 2005.
!
! program history log:
!      Algorithms created                                    : 03/02/04
!      beta version is released in ORA
!                                                            :  10/22/04
!      betat version is release to EMC                       :  06/14/05
!
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses and vars
!
!   input argument list:
!     tbo(1): Vertically polarized AMSR-E brighness temperature at 6.925 GHz
!     tbo(3):                                                      10.65 GHz
!     tbo(5):                                                      18.7  GHz
!     tbo(7):                                                      23.8  GHz
!     tbo(9):                                                      36.5  GHz
!     tbo(11):                                                     89    GHz  (not used here)
!     tbo(2): Horizontally polarized AMSR-E brighness temperature at 6.925 GHz
!     tbo(4):                                                        10.65 GHz
!     tbo(6):                                                        18.7  GHz
!     tbo(8):                                                        23.8  GHz
!     tbo(10):                                                       36.5  GHz
!     tbo(12):                                                       89    GHz  (not used here)
!     theta  : local zenith angle in degree
!     sst: sea surface temperature (K)
!     ssw: sea surface wind        (m/s)
!
!   output argument list:
!     rwp: rain water path         (mm)
!     cwp: cloud water path        (mm)
!     vr : total precipitable water retrieved at 18.7 GHz FOV
!     vc : total precipitable water retrieved at 36.5 GHz FOV
!
!   important internal variable:
!     tl        : cloud layer temperature
!     angle     : local zenith angle in radian
!     umu       : cosine of angle
!     freq(*)   : six frequencies (6.9, 10.7, 18.7, 23.8, 36.5, 89.0)
!     kw(*)     : vapor water mass absorption  coefficient at six frequencies
!     ko2_coe(*): fitting coefficients to calculate oxygen optical thickness at six frequencies
!     kl_coe(*) : fitting coefficients to calculate liquid water mass absorption coefficient at six frequencies
!
!   comments:
!
!     Called Subroutines:
!     TBE_FROM_TBO(): scattering correction for AMSR-E measurements
!     TBA_FROM_TBE(): RTM bias correction
!     emis_water()  : sea surface emissivity
!
!     Remarks:
!      Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: zero,half,one,two,five

  implicit none

  integer(i_kind) nch
  parameter(nch=6)

  real(r_kind),intent(in   ) :: tbo(nch*2)
  real(r_kind),intent(in   ) :: wind,theta,sst

  real(r_kind),intent(  out) :: rwp,vr,vc,cwp

  integer(i_kind) ich,i,polar_status
  real(r_kind)  angle,frequency,emissivity
  real(r_kind)  ev(nch)
  real(r_kind)  tauo(nch),kl(nch),tv(nch),tvmin(nch)
! real(r_kind)  thmin(nch),eh(nch)
  real(r_kind),save :: freq(nch)
  real(r_kind),save :: kw(nch)
  real(r_kind),save :: ko2_coe(nch,3),kl_coe(nch,3)
  real(r_kind)  umu,tl
  real(r_kind)  a0,a1,a2,b0,b1,b2
  data freq/6.925_r_kind,10.65_r_kind,18.7_r_kind,23.8_r_kind,37._r_kind,89.0_r_kind/
  data  kw/ 7.253225e-5_r_kind,1.8841e-4_r_kind,1.6962e-3_r_kind,5.268469e-3_r_kind,1.96235e-3_r_kind,9.399432e-3_r_kind/
  data  (ko2_coe(1,i),i=1,3)/1.045148e-002_r_kind,  2.909099e-005_r_kind, -1.258838e-007_r_kind/
  data  (ko2_coe(2,i),i=1,3)/1.144385e-002_r_kind,  3.133540e-005_r_kind, -1.367926e-007_r_kind/
  data  (ko2_coe(3,i),i=1,3)/1.545055e-002_r_kind,  4.119837e-005_r_kind, -1.825797e-007_r_kind/
  data  (ko2_coe(4,i),i=1,3)/2.016777e-002_r_kind,  5.265939e-005_r_kind, -2.361456e-007_r_kind/
  data  (ko2_coe(5,i),i=1,3)/5.452020e-002_r_kind,  1.377254e-004_r_kind, -6.308589e-007_r_kind/
  data  (ko2_coe(6,i),i=1,3)/6.360679e-002_r_kind,  1.084979e-004_r_kind, -6.453394e-007_r_kind/

  data (kl_coe(1,i),i=1,3)/ 1.045782e-002_r_kind, -3.353543e-004_r_kind,  5.306014e-006_r_kind/
  data (kl_coe(2,i),i=1,3)/ 2.459446e-002_r_kind, -7.827213e-004_r_kind,  1.225936e-005_r_kind/
  data (kl_coe(3,i),i=1,3)/ 7.431756e-002_r_kind, -2.276136e-003_r_kind,  3.413946e-005_r_kind/
  data (kl_coe(4,i),i=1,3)/ 1.182024e-001_r_kind, -3.487585e-003_r_kind,  5.012487e-005_r_kind/
  data (kl_coe(5,i),i=1,3)/ 2.677390e-001_r_kind, -6.890666e-003_r_kind,  8.319393e-005_r_kind/
  data (kl_coe(6,i),i=1,3)/ 1.034859e+000_r_kind, -9.715101e-003_r_kind, -6.591484e-005_r_kind/

  angle = theta 
  umu = cos(angle)
  vc  = 0.0_r_kind ! Since this variable is not used, calculation commented out!
  rwp = 0.0_r_kind ! Since this variable is not used, calculation commented out!

! A temporal assumption about cloud layer temperature (to be updated when it is available)
  tl = sst-20.0_r_kind-273.15_r_kind
  if (tl > 10.0_r_kind) tl = 10.0_r_kind

  call TBA_FROM_TBO(tbo,tv)


! Calculate KW and KL and tau_o2(taut) and emissivity
  do ich = 1, nch
     tauo(ich) = ko2_coe(ich,1) +  ko2_coe(ich,2)*sst + ko2_coe(ich,3)*sst*sst
     kl(ich)   = kl_coe(ich,1)  +  kl_coe(ich,2)*tl   + kl_coe(ich,3)*tl*tl
     frequency = freq(ich)
!    polar_status = 0
!    call emis_water(angle,frequency,sst,wind,polar_status, emissivity)
!    eh(ich) = emissivity
     polar_status = 1
     call emis_water(angle,frequency,sst,wind,polar_status, emissivity)
     ev(ich) = emissivity
     tvmin(ich) = sst*( one - exp(-tauo(ich)/umu)*exp(-tauo(ich)/umu)*(one-ev(ich)) )
!    thmin(ich) = sst*( one - exp(-tauo(ich)/umu)*exp(-tauo(ich)/umu)*(one-eh(ich)) )

! Quality control
     if (tv(ich) < tvmin(ich)) tv(ich) = tvmin(ich)
!    if (th(ich) < thmin(ich)) th(ich) = thmin(ich)
  enddo

  if ( ( sst-tv(3) > 0.01_r_kind ) .and. ( sst-tv(4) > 0.01_r_kind ) ) then
!    Calculate a0, a1, a2 and b0, b1 and b2 at 18.7 GHz over 23.8 GHz

!    18.7 over 23.8 GHz: rain water path
!    a0 = -half*kw(4)/(kw(4)*kl(3)-kw(3)*kl(4))
     b0 =  half*kl(4)/(kw(4)*kl(3)-kw(3)*kl(4))
!    a1 =  kw(3)/kw(4)
     b1 =  kl(3)/kl(4)
!    a2 = -two*(tauo(3) - a1*tauo(4))/umu +(one-a1)*log(sst) + log(one-ev(3)) - a1*log(one-ev(4))
     b2 = -two*(tauo(3) - b1*tauo(4))/umu +(one-b1)*log(sst) + log(one-ev(3)) - b1*log(one-ev(4))

!    rwp = a0*umu*( log(sst-tv(3)) - a1*log(sst-tv(4))-a2)
     vr  = b0*umu*( log(sst-tv(3)) - b1*log(sst-tv(4))-b2)

! Clear conditions
!    if (rwp < zero) rwp = zero
     if (vr < zero) vr = zero
  else

! Invalid retrieval
!    rwp = -999.0_r_kind
     vr  = -999.0_r_kind
  endif

  if ( ( sst-tv(4) > 0.01_r_kind ) .and. ( sst-tv(5) > 0.01_r_kind ) ) then
!    36.5 over 23.8 GHz: cloud water path
     a0 = -half*kw(4)/(kw(4)*kl(5)-kw(5)*kl(4))
!    b0 =  half*kl(4)/(kw(4)*kl(5)-kw(5)*kl(4))
     a1 =  kw(5)/kw(4)
!    b1 =  kl(5)/kl(4)
     a2 = -two*(tauo(5) - a1*tauo(4))/umu  +(one-a1)*log(sst) + &
                           log(one-ev(5)) - a1*log(one-ev(4))
!    b2 = -two*(tauo(5)  - b1*tauo(4))/umu +(one-b1)*log(sst) + &
!                          log(one-ev(5)) - b1*log(one-ev(4))
     cwp = a0*umu*( log(sst-tv(5) ) - a1*log(sst-tv(4))-a2)
!    vc = b0*umu*( log(sst-tv(5)) - b1*log(sst-tv(4))-b2 )
     if(cwp < zero) cwp = zero
!    if(vc < zero) vc = zero
  else
     cwp = -999.0_r_kind
!    vc  = -999.0_r_kind
  endif

! Quality control: remove residual effect of sea roughness on 18.7 GHz
! if ( cwp <= 0.3_r_kind) rwp = cwp
! if (cwp <= 0.2_r_kind .and. wind >= five) rwp = zero

end subroutine RCWPS_Alg

subroutine TBA_FROM_TBO(tbo,tvs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: TBE_FROM_TBO
!
!   prgrmmr: Banghua Yan      org: NESDIS        date:  2004-04-20
!
! abstract:
!     Perform corrections for scattering effect for AMSR-E measurements (6.9 ~ 36.5 GHz)
!     Note: Scattering effects on the tbo at 89 GHz are to be corrected
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!     tbo(*) : AMSRE measurements
!
!   output argument list:
!     tb(*)  : brightness temperatures with scattering correction
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: three
  implicit none

  integer(i_kind) nch
  parameter (nch = 6)

  real(r_kind),intent(in   ) :: tbo(nch*2)
  real(r_kind),intent(  out) :: tvs(nch)
! Remove intent(out) for ths since currently not used
! real(r_kind)               :: ths(nch)

  integer(i_kind) i,j,k
  real(r_kind) tbe(nch),tv18,tb(nch)
  real(r_kind),save :: coe_tbs(5,11)
  data (coe_tbs(1,k),k=1,11)/  &
    3.700653e+000_r_kind, 1.139987e-002_r_kind, 9.760773e-001_r_kind,-3.792810e-002_r_kind, 6.930733e-002_r_kind,&
    9.327399e-002_r_kind,-1.626923e-001_r_kind,-1.000955e-001_r_kind, 1.667856e-001_r_kind, 3.836404e-002_r_kind,&
   -6.907219e-002_r_kind/
  data (coe_tbs(2,k),k=1,11)/ &
    -1.341827e+000_r_kind, -2.947993e-002_r_kind, -1.031828e-003_r_kind,  3.924831e-002_r_kind,  9.954801e-001_r_kind, &
    -2.360948e-002_r_kind,  2.565339e-002_r_kind,  1.999745e-002_r_kind, -2.145227e-002_r_kind, -1.131899e-002_r_kind, &
    1.201244e-002_r_kind/
  data (coe_tbs(3,k),k=1,11)/  &
    2.223694e+000_r_kind,-1.482995e-002_r_kind,-5.777131e-003_r_kind, 2.200880e-003_r_kind, 2.359764e-002_r_kind, &
    5.473155e-002_r_kind, 9.022181e-001_r_kind,-6.822398e-002_r_kind, 1.177735e-001_r_kind, 2.131365e-002_r_kind, &
   -4.201306e-002_r_kind/
  data (coe_tbs(4,k),k=1,11)/  &
    7.324219e-003_r_kind,-3.075898e-002_r_kind,-2.037739e-003_r_kind, 2.657354e-002_r_kind, 1.731113e-002_r_kind, &
    3.657620e-002_r_kind,-1.028747e-001_r_kind,-5.361976e-002_r_kind, 1.126930e+000_r_kind, 1.459956e-002_r_kind, &
   -3.240352e-002_r_kind/
  data (coe_tbs(5,k),k=1,11)/ &
    7.333450e+000_r_kind, -1.293110e-001_r_kind,  1.874678e-002_r_kind,  1.842369e-001_r_kind, -2.854594e-002_r_kind,  &
    8.010966e-002_r_kind, -2.214617e-001_r_kind, -1.830799e-001_r_kind,  3.916801e-001_r_kind,  1.001790e-001_r_kind,  &
    7.973670e-001_r_kind/

! scattering correction
  tb(1) = tbo(1)
  tb(2) = tbo(3)
  tb(3) = tbo(5)
  tb(4) = tbo(7)
  tb(5) = tbo(9)
  tbe(6) = tbo(11)

! from tbscat to tbemiss
  do i = 1, 5
     tbe(i) = coe_tbs(i,1)
     do j=1,10
        tbe(i) = tbe(i) + coe_tbs(i,j+1)*tb(j)
     enddo
     tbe(5) = tbe(5) + 0.4_r_kind*(tbe(5)-tb(5))
  enddo

! correction of sea surface roughness

  tv18 = 0.0054_r_kind*tbo(7)*tbo(7) -1.9554_r_kind*tbo(7) +364.71_r_kind
  if ((tbo(5)-tv18 >= three)  .and. (tbo(2) >= 90.0_r_kind)) &
     tbe(3) = tbe(3) - 5.5_r_kind*(tbo(5)-tv18)/(10.0_r_kind-three)


! Adjust TBE to TBA required in the algorithms
  tvs(1) = 6.504761e+000_r_kind + 9.540653e-001_r_kind*tbe(1)
  tvs(2) = 5.405548e+000_r_kind + 9.632518e-001_r_kind*tbe(2)
  tvs(3) = 2.251144e+000_r_kind + 9.880477e-001_r_kind*tbe(3)
  tvs(4) = -3.358444e+000_r_kind + 1.028767e+000_r_kind*tbe(4)
  tvs(5) = 3.148166e+001_r_kind + 8.714348e-001_r_kind*tbe(5)
  tvs(6) = 2.861269e+001_r_kind + 9.155271e-001_r_kind*tbe(6)

end subroutine TBA_FROM_TBO
subroutine TBE_FROM_TBO(tbo,tb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: TBE_FROM_TBO
!
!   prgrmmr: Banghua Yan      org: NESDIS        date:  2004-04-20
!
! abstract:
!     Perform corrections for scattering effect for AMSR-E measurements (6.9 ~ 36.5 GHz)
!     Note: Scattering effects on the tbo at 89 GHz are to be corrected
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!     tbo(*) : AMSRE measurements
!
!   output argument list:
!     tb(*)  : brightness temperatures with scattering correction
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: three
  implicit none

  integer(i_kind) nch
  parameter (nch = 6)

  real(r_kind),intent(in   ) :: tbo(nch*2)
  real(r_kind),intent(  out) :: tb(nch*2)

  integer(i_kind) i,j,k
  real(r_kind) tbe(nch*2),tv18
  real(r_kind),save :: coe_tbs(10,11)
  data (coe_tbs(1,k),k=1,11)/  &
    2.836349e+000_r_kind, 1.001083e+000_r_kind,-1.245813e-002_r_kind,-1.431959e-002_r_kind, 3.735422e-002_r_kind, &
    4.765791e-002_r_kind,-9.139793e-002_r_kind,-5.571145e-002_r_kind, 9.269717e-002_r_kind, 2.102336e-002_r_kind, &
   -3.769030e-002_r_kind/
  data (coe_tbs(2,k),k=1,11)/  &
    3.700653e+000_r_kind, 1.139987e-002_r_kind, 9.760773e-001_r_kind,-3.792810e-002_r_kind, 6.930733e-002_r_kind,&
    9.327399e-002_r_kind,-1.626923e-001_r_kind,-1.000955e-001_r_kind, 1.667856e-001_r_kind, 3.836404e-002_r_kind,&
   -6.907219e-002_r_kind/
  data (coe_tbs(3,k),k=1,11)/ &
    -1.238724e+000_r_kind, -4.068373e-002_r_kind,  1.732873e-003_r_kind,  1.050165e+000_r_kind, -6.747865e-003_r_kind, &
    -8.419795e-003_r_kind,  4.294594e-003_r_kind, -5.081398e-003_r_kind,  2.105786e-002_r_kind, -1.441340e-003_r_kind, &
    -9.873924e-003_r_kind/
  data (coe_tbs(4,k),k=1,11)/ &
    -1.341827e+000_r_kind, -2.947993e-002_r_kind, -1.031828e-003_r_kind,  3.924831e-002_r_kind,  9.954801e-001_r_kind, &
    -2.360948e-002_r_kind,  2.565339e-002_r_kind,  1.999745e-002_r_kind, -2.145227e-002_r_kind, -1.131899e-002_r_kind, &
    1.201244e-002_r_kind/
  data (coe_tbs(5,k),k=1,11)/  &
    4.851257e+000_r_kind,-1.001152e-002_r_kind,-1.666120e-002_r_kind,-2.234872e-002_r_kind, 5.869221e-002_r_kind, &
    1.113674e+000_r_kind,-1.946697e-001_r_kind,-1.325814e-001_r_kind, 2.210909e-001_r_kind, 4.219167e-002_r_kind, &
   -7.981353e-002_r_kind/
  data (coe_tbs(6,k),k=1,11)/  &
    2.223694e+000_r_kind,-1.482995e-002_r_kind,-5.777131e-003_r_kind, 2.200880e-003_r_kind, 2.359764e-002_r_kind, &
    5.473155e-002_r_kind, 9.022181e-001_r_kind,-6.822398e-002_r_kind, 1.177735e-001_r_kind, 2.131365e-002_r_kind, &
   -4.201306e-002_r_kind/
  data (coe_tbs(7,k),k=1,11)/  &
    2.246658e+000_r_kind,-2.905825e-002_r_kind,-1.400843e-002_r_kind, 7.278482e-003_r_kind, 5.651486e-002_r_kind, &
    9.341484e-002_r_kind,-2.202809e-001_r_kind, 8.776495e-001_r_kind, 2.594665e-001_r_kind, 3.959633e-002_r_kind, &
   -8.018037e-002_r_kind/
  data (coe_tbs(8,k),k=1,11)/  &
    7.324219e-003_r_kind,-3.075898e-002_r_kind,-2.037739e-003_r_kind, 2.657354e-002_r_kind, 1.731113e-002_r_kind, &
    3.657620e-002_r_kind,-1.028747e-001_r_kind,-5.361976e-002_r_kind, 1.126930e+000_r_kind, 1.459956e-002_r_kind, &
   -3.240352e-002_r_kind/
  data (coe_tbs(9,k),k=1,11)/ &
    4.557663e+000_r_kind, -1.428426e-001_r_kind,  1.443825e-002_r_kind,  1.916111e-001_r_kind, -7.515940e-003_r_kind,   &
    1.687996e-001_r_kind, -3.647460e-001_r_kind, -3.194418e-001_r_kind,  5.943681e-001_r_kind,  1.140872e+000_r_kind,   &
   -2.494820e-001_r_kind/
  data (coe_tbs(10,k),k=1,11)/ &
    7.333450e+000_r_kind, -1.293110e-001_r_kind,  1.874678e-002_r_kind,  1.842369e-001_r_kind, -2.854594e-002_r_kind,  &
    8.010966e-002_r_kind, -2.214617e-001_r_kind, -1.830799e-001_r_kind,  3.916801e-001_r_kind,  1.001790e-001_r_kind,  &
    7.973670e-001_r_kind/

!v,h-->h,v
  tb(1) = tbo(2)
  tb(2) = tbo(1)
  tb(3) = tbo(4)
  tb(4) = tbo(3)
  tb(5) = tbo(6)
  tb(6) = tbo(5)
  tb(7) = tbo(8)
  tb(8) = tbo(7)
  tb(9) = tbo(10)
  tb(10) = tbo(9)
  tb(11) = tbo(12)
  tb(12) = tbo(11)

! from tbscat to tbemiss
  do i = 1, 10
     tbe(i) = coe_tbs(i,1)
     do j=1,10
        tbe(i) = tbe(i) + coe_tbs(i,j+1)*tb(j)
     enddo
     tbe(10) = tbe(10) + 0.4_r_kind*(tbe(10)-tb(10))
  enddo

! tbo at 89 GHz are to be corrected
!
  tbe(11) = tb(11)
  tbe(12) = tb(12)

!h,v --> v,h
  tb(1) = tbe(2)
  tb(2) = tbe(1)
  tb(3) = tbe(4)
  tb(4) = tbe(3)
  tb(5) = tbe(6)
  tb(6) = tbe(5)
  tb(7) = tbe(8)
  tb(8) = tbe(7)
  tb(9) = tbe(10)
  tb(10) = tbe(9)
  tb(11) = tbe(12)
  tb(12) = tbe(11)

! correction of sea surface roughness

  tv18 = 0.0054_r_kind*tbo(7)*tbo(7) -1.9554_r_kind*tbo(7) +364.71_r_kind
  if ((tbo(5)-tv18 >= three)  .and. (tbo(2) >= 90.0_r_kind)) &
     tb(5) = tb(5) - 5.5_r_kind*(tbo(5)-tv18)/(10.0_r_kind-three)
  return
  stop

end subroutine TBE_FROM_TBO

subroutine TBA_FROM_TBE(tbo,tvs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  TBA_FROM_TBO
!
!   prgmmr: Banghua Yan        org: NESDIS              date: 2004-04-20
!
! abstract:
!     Adjust AMSR-E measurements with scattering correction to algorithm-based brightness temperature
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!     tbo(*) : AMSRE measurements
!
!   output argument list:
!     tvs(*)  : algorithm-based brightness temperatures at a vertical polarization
!     ths(*)  : algorithm-based brightness temperatures at a horizontal polarization (removed)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind, i_kind
  implicit none

  integer(i_kind) nch
  parameter (nch = 6)

  real(r_kind),intent(in   ) :: tbo(nch*2)
  real(r_kind),intent(  out) :: tvs(nch)
! real(r_kind),intent(  out) :: tvs(nch),ths(nch)
! Remove intent(out) for ths since currently not used
! real(r_kind)               :: ths(nch)

  real(r_kind) tb(nch*2)

!v,h-->h,v
  tb(1) = tbo(2)
  tb(2) = tbo(1)
  tb(3) = tbo(4)
  tb(4) = tbo(3)
  tb(5) = tbo(6)
  tb(6) = tbo(5)
  tb(7) = tbo(8)
  tb(8) = tbo(7)
  tb(9) = tbo(10)
  tb(10) = tbo(9)
  tb(11) = tbo(12)
  tb(12) = tbo(11)

!  Currently ths commented out since it is not used.  

! ths(1) = 2.617433e+001_r_kind + 6.600980e-001_r_kind*tb(1)
  tvs(1) = 6.504761e+000_r_kind + 9.540653e-001_r_kind*tb(2)
! ths(2) = 1.402604e+001_r_kind + 8.144087e-001_r_kind*tb(3)
  tvs(2) = 5.405548e+000_r_kind + 9.632518e-001_r_kind*tb(4)
! ths(3) = 4.261467e+000_r_kind + 9.567850e-001_r_kind*tb(5)
  tvs(3) = 2.251144e+000_r_kind + 9.880477e-001_r_kind*tb(6)
! ths(4) = 3.366165e+000_r_kind + 9.979538e-001_r_kind*tb(7)
  tvs(4) = -3.358444e+000_r_kind + 1.028767e+000_r_kind*tb(8)
! ths(5) = 2.409077e+001_r_kind + 8.443854e-001_r_kind*tb(9)
  tvs(5) = 3.148166e+001_r_kind + 8.714348e-001_r_kind*tb(10)
! ths(6) = 1.919507e+001_r_kind + 9.322882e-001_r_kind*tb(11)
  tvs(6) = 2.861269e+001_r_kind + 9.155271e-001_r_kind*tb(12)

end subroutine TBA_FROM_TBE

subroutine emis_water(angle,frequency,sst,wind,polar_status,emissivity)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  emis_water  compute oceanic emissivity
!
!   prgmmr: Fuzhong Weng and Banghua Yan         org: NESDIS
!
! abstract: compute oceanic emissivity at two polarizations
!       References:
!       (1) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!           sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!       (2) Stogryn, A., 1972: The emissivity of sea foam at microwave frequencies, J. Geophys. Res.,
!            77, 1658-1666.
!       (3) Hollinger, J. P., 1971: Passive microwave measurements of sea surface roughness.
!            IEEE Transactions on Geoscience Electronics, GE-9(3), 165-169.
!
! program history
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses and vars
!
!   input argument list:
!         angle    :  incident angle in radian
!         sst      : temperature (K)
!         s        : sea water salinity (1/thousand)
!         frequency: (GHz)
!         wind     : wind speed (m/s)
!   output argument list:
!         rh       :  surface reflectance in horizontally polarized state
!         rv       :
!         eh       :  surface emitance in ....
!         ev       :
!   internal argument list:
!         foam     : foam fraction
!         g,tr     : emperical functions for wind induced
!                    changes in reflection coefficient
!         f        : frequency in Hz
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind, i_kind
  use constants, only: zero,one,four
  implicit none

  real(r_kind)   ,intent(in   ) :: angle, wind
  real(r_kind)   ,intent(in   ) :: sst,frequency
  integer(i_kind),intent(in   ) :: polar_status

  real(r_kind)   ,intent(  out) :: emissivity

  real(r_kind) s
  parameter (s = 35.5_r_kind)

  real(r_kind) t,degre
  real(r_kind) f
  real(r_kind) ref,rfoam,tr,g,rclear,foam,ev,eh,pi
  real(r_kind) ep,real_ep,imag_ep
  complex mu, eps, aid1,aid2,aid3,cang,refwat,rh,rv

  mu = cmplx (one,zero,r_kind)
  f = frequency*1.0e9_r_kind
  pi = four*atan(one)
  degre = angle*180.0_r_kind/pi
  cang = cmplx(angle,r_kind)
  t = sst ! - 273.15_r_kind

! complex dielectric properties of saline water
  call epsp(sst,s,f,ep)
  real_ep=ep
  call epspp(sst,s,f,ep)
  imag_ep=-ep
  eps=cmplx (real_ep,imag_ep,r_kind)

! complex refractive index of saline water (not used)
  refwat = csqrt(eps)
  aid1 = csqrt(mu*eps-csin(cang)**2)
  aid2 = mu*ccos(cang)-aid1
  aid3 = mu*ccos(cang)+aid1
  rh = aid2/aid3
  aid2 = eps*ccos(cang)-aid1
  aid3 = eps*ccos(cang)+aid1
  rv = aid2/aid3
  if(wind<7.0_r_kind) then
     foam=zero
  else
     foam=0.006_r_kind*(one-exp(-f*1.0e-9_r_kind/7.5_r_kind))*(wind-7.0_r_kind)
  endif

! correction for wind induced foam free sea surface
  if(foam<zero) foam=zero
  if(foam>one) foam=one

! emperical functions for wind induced reflection changes for hp
  g = one - 1.748e-3_r_kind*degre-7.336e-5_r_kind*degre**2+ 1.044e-7_r_kind*degre**3
  tr = wind*(1.15e-1_r_kind+3.8e-5_r_kind*degre**2)*sqrt(f*1.0e-9_r_kind)
  rfoam = one-(208.0_r_kind+1.29e-9_r_kind*f)/t*g
  ref = (cabs(rh))**2
  rclear = ref - tr/t
  eh =one- (one-foam)*rclear-foam*rfoam

! emperical functions for wind induced reflection changes for vp
  g  = one - 9.946e-4_r_kind*degre+3.218e-5_r_kind*degre**2 -1.187e-6_r_kind*degre**3+7.e-20_r_kind*degre**10
  tr = wind*(1.17e-1_r_kind-2.09e-3_r_kind*exp(7.32e-2_r_kind*degre))*sqrt(f*1.0e-9_r_kind)
  rfoam = one-(208.0_r_kind+1.29e-9_r_kind*f)/t*g
  ref = ( cabs(rv) )**2
  rclear = ref - tr/t
  ev = one-(one-foam)*rclear-foam*rfoam
  if(eh>one) eh=one
  if(eh<zero) eh=zero
  if(ev>one) ev=one
  if(ev<zero) ev=zero
  if (polar_status == 1) then
     emissivity = ev
  else
     emissivity = eh
  endif

  return

end subroutine emis_water

subroutine  epsp(t1,s,f,ep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  epsp
!
!  prgmmr: Fuzhong Weng        org: NESDIS
!
! abstract: calculates the real part of the dielectric constant for saline water
!       References:
!    (1) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!        sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!    (2) Ulaby, F.T., Moore, R.K., and Fung, A.K.,1986: Microwave remote sensing, active and passive, III,
!        From theory to applications, p.2024-2025.
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!         t1       : temperature (K)
!         s        : sea water salinity (1/thousand)
!         f        : (Hz)
!
!   output argument list:
!         ep
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_kind
  use constants, only: one
  implicit none

  real(r_kind),intent(in   ) :: f,t1,s

  real(r_kind),intent(  out) :: ep

  real(r_kind) t,t2,eswi,eswo,a,b,esw,tswo,tsw

  t=t1-273.0_r_kind
  t2=(t-25.0_r_kind)
  eswi = 4.9_r_kind
  eswo = 87.134_r_kind-1.949e-1_r_kind*t-1.276e-2_r_kind*t*t+2.491e-4_r_kind*t**3
  a = one+1.613e-5_r_kind*t*s-3.656e-3_r_kind*s+3.210e-5_r_kind*s*s-4.232e-7_r_kind*s**3
  esw = eswo*a
  tswo = 1.1109e-10_r_kind-3.824e-12_r_kind*t+6.938e-14_r_kind*t**2-5.096e-16_r_kind*t**3
  b = one+2.282e-5_r_kind*t*s-7.638e-4_r_kind*s-7.760e-6_r_kind*s**2+1.105e-8_r_kind*s**3
  tsw = tswo*b
  ep = eswi +(esw-eswi)/(one+(f*tsw)**2)

  return

end subroutine epsp

subroutine epspp (t1,s,f,ep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  epspp
!
!   prgmmr: Fuzhong Weng       org: NESDIS
!
! abstract: compute calculates the imaginair part  of the dielectric constant for saline water
!       References:
!    (1) Klein, L.A., and C.T. Swift, 1977: An improved model for the dielectric constant of
!        sea water at microwave frequencies, IEEE J. Oceanic Eng., OE-2, 104-111.
!    (2) Ulaby, F.T., Moore, R.K., and Fung, A.K.,1986: Microwave remote sensing, active and passive, III,
!        From theory to applications, p.2024-2025.
!
! program history log:
!      2005-10-21  kazumori - modified for GSI
!      2008-04-16  safford  - rm unused uses
!
!   input argument list:
!         t1       : temperature (K)
!         s        : sea water salinity (1/thousand)
!         f        : (Hz)
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind
  use constants, only: two,one,four
  implicit none

  real(r_kind),intent(in   ) :: f,t1,s
  real(r_kind),intent(  out) :: ep

  real(r_kind) t,t2,eswi,eswo,a,b,d,esw,tswo,tsw,sswo,fi,ssw
  real(r_kind) pi,eo

  t=t1-273.0_r_kind
  t2=t-25.0_r_kind
  eswi = 4.9_r_kind
  eo = 8.854e-12_r_kind
  pi = four * atan(one)
  eswo = 87.134_r_kind-1.949e-1_r_kind*t-1.276e-2_r_kind*t*t+2.491e-4_r_kind*t**3
  a = one+1.613e-5_r_kind*t*s-3.656e-3_r_kind*s+3.210e-5_r_kind*s*s-4.232e-7_r_kind*s**3
  esw = eswo*a
  tswo = 1.1109e-10_r_kind-3.824e-12_r_kind*t+6.938e-14_r_kind*t**2-5.096e-16_r_kind*t**3
  b = one+2.282e-5_r_kind*t*s-7.638e-4_r_kind*s-7.760e-6_r_kind*s**2+1.105e-8_r_kind*s**3
  tsw = tswo*b
  sswo = s*(0.18252_r_kind-1.4619e-3_r_kind*s+2.093e-5_r_kind*s**2-1.282e-7_r_kind*s**3)
  d = 25.0_r_kind-t
  fi = d*(2.033e-2_r_kind+1.266e-4_r_kind*d+2.464e-6_r_kind*d**2- s*(1.849e-5_r_kind-2.551e-7_r_kind*d+2.551e-8_r_kind*d*d))
  ssw = sswo*exp(-fi)
  ep = tsw*f*(esw-eswi)/(one+(tsw*f)**2)
  ep = ep + ssw/(two*pi*eo*f)

  return

end subroutine epspp

subroutine ret_amsua(tb_obs,nchanl,tsavg5,zasat,clwp_amsua,ierrret,scat)  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ret_amsua 
!
!  prgmmr: Min-Jeong Kim        org: JCSDA
!
! abstract: calculates cloud liquid water path
!       References:
!       Grody et al. (2001) : Determination of precipitable water and cloud liquid water
!                       over oceans from the NOAA 15 advanced microwave sounding unit.
!
! program history log:
!      2010-10-23  kim : Cloud liquid water path retrieval to compare with first guess
!                        in all sky condition  (using observed tbs instead of o-g tbs)  
!      2011-05-03  todling - add r_kind to constants
!      2013-12-10  eliu    - bug fix for applying the retrieval to sub-freezing
!                            surface temperature
!      2014-01-17  zhu     - add scattering index scat 
!      2014-01-31  mkim - add ierrret return flag for cloud qc near seaice edge 
!
!  input argument list:
!     tb_obs    - observed brightness temperatures
!     ich       - channel number array
!     nchanl    - number of channels    
!     tsavg5    - Surface temperature value
!     zasat     - satellite zenith angle
!
!   output argument list:
!     clwp_amsua       - amsu-a retrieved cloud liquid water path
!     ierrret          - return flag     
!     scat             - amsu-a scattering index
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_kind, i_kind
  use constants, only: one, t0c, zero
  implicit none


  integer(i_kind)                   ,intent(in   ) :: nchanl
  real(r_kind),dimension(nchanl)    ,intent(in   ) :: tb_obs
  real(r_kind)                      ,intent(in   ) :: tsavg5,zasat
  real(r_kind)                      ,intent(  out) :: clwp_amsua
  integer(i_kind)                   ,intent(  out) :: ierrret 
  real(r_kind),optional             ,intent(  out) :: scat

  real(r_kind),parameter:: r285=285.0_r_kind
  real(r_kind),parameter:: r284=284.0_r_kind
  real(r_kind),parameter:: r1000=1000.0_r_kind

! Declare local variables 
  real(r_kind) :: d0, d1, d2, coszat
! real(r_kind) :: c0, c1, c2
  real(r_kind) :: tb890 = zero

  
  coszat=cos(zasat)
  d0 = 8.240_r_kind - (2.622_r_kind - 1.846_r_kind*coszat)*coszat
  d1 = 0.754_r_kind
  d2 = -2.265_r_kind

  if (tsavg5>t0c-one .and. tb_obs(1)<=r284 .and. tb_obs(2)<=r284  .and. &
      tb_obs(1)>zero .and. tb_obs(2)>zero) then
     clwp_amsua=cos(zasat)*(d0 + d1*log(r285-tb_obs(1)) + d2*log(r285-tb_obs(2))) 
     ierrret = 0
     clwp_amsua=max(zero,clwp_amsua)
  else
     clwp_amsua = r1000  
     ierrret = 1
  endif

  if (present(scat)) then
     if (nchanl == 15) then
!       AMSU-A
        tb890 = tb_obs(15)
     else if (nchanl == 22) then
!       ATMS
        tb890 = tb_obs(16)
     endif
     if (tb890 > zero) then
        scat=-113.2_r_kind+(2.41_r_kind-0.0049_r_kind*tb_obs(1))*tb_obs(1)  &
             +0.454_r_kind*tb_obs(2)-tb890
     endif
     scat=max(zero,scat)
  end if

end subroutine ret_amsua

subroutine gmi_37pol_diff(tb37v,tb37h,tsim37v,tsim37h,clw,ierrret)
!$$$  subprogram documentation block
!                .      .    .                                       .    
! subprogram: gmi_37pol_diff 
!
!  prgmmr: Min-Jeong Kim
!
! abstract: calculates cloud amount index over ocean using normalized 37GHz polization difference
!
!   output argument list:
!     clw  
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!
  use kinds, only: r_kind, i_kind
  use constants, only: r1000, zero, one
  implicit none

  real(r_kind)                      ,intent(in   ) :: tb37v,tb37h
  real(r_kind)                      ,intent(in   ) :: tsim37v,tsim37h
  real(r_kind)                      ,intent(  out) :: clw
  integer(i_kind)                   ,intent(  out) :: ierrret

! Declare local variables

     ierrret = 0

     clw = one - (tb37v-tb37h)/(tsim37v-tsim37h)
     clw=max(zero,clw)
     if ((tb37h > tb37v) .or. (tb37h > 500_r_kind )) then
        ierrret = 1
        clw= r1000
     endif

end subroutine gmi_37pol_diff

end module clw_mod
