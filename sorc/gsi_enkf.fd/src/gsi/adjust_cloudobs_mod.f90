module adjust_cloudobs_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    adjust_cloudobs_mod
!   prgmmr: carley          org: np22                date: 2014-06-16
!
! abstract: Module contains routines which obtain the cloud amount, ceiling, etc. for
!           cloud observations associated with conventional obs as well as GOES obs.
!
! program history log:
!   2014-06-16  carley
!   2019-06-17  mmorris - toss obs over water, at night, that suggest clear
!                         conditions owing to erroneous clearing of low stratus
!   2019-12-05  mmorris - also toss obs over land, at night, that suggest clear
!                         conditions owing to erroneous clearing of low stratus
!
! subroutines included:
!   sub adjust_convcldobs    -     obtain cloud amount info from conventional prepbufr
!   sub adjust_goescldobs    -     obtain cloud amount info from goes cloud obs
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  implicit none

! set default to private
  private
! set subroutines to public
  public :: adjust_convcldobs
  public :: adjust_goescldobs

contains

subroutine adjust_convcldobs(cld2seq,cld2seqlevs,input_cldseq,cldseqlevs,wthstr,wthstrlevs, &
                        low_cldamt,low_cldamt_qc,mid_cldamt,mid_cldamt_qc, &
                        hig_cldamt,hig_cldamt_qc,tcamt,lcbas,tcamt_qc,lcbas_qc,ceiling,stnelev)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  adjust_convcldobs         obtain cloud amount info from conventional prepbufr
!   prgmmr: Yanqiu Zhu          org: np22                date: 2011-12-29
!
! abstract:  This routine obtain cloud amount info from conventional prepbufr
!
! program history log:
!   2011-12-29  zhu
!   2014-07-23  carley - add station elevation to lcbas/ceiling so output
!                        is MSL.
!   2014-09-02  carley - Adjust tcamt_qc assignment.    
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: tiny_r_kind,zero,one,two,three,four
  implicit none

! input variables
  integer(i_kind),intent(in) :: cldseqlevs,wthstrlevs,cld2seqlevs
  real(r_kind),dimension(3,10),intent(in):: input_cldseq
  real(r_kind),dimension(2,1),intent(in):: cld2seq
  real(r_kind),dimension(1),intent(in):: wthstr
  real(r_kind),intent(in) :: stnelev

! output variables
  integer(i_kind),intent (inout) :: low_cldamt_qc,mid_cldamt_qc,hig_cldamt_qc
  integer(i_kind),intent (inout) :: tcamt_qc,lcbas_qc
  real(r_kind),intent (inout) :: low_cldamt,mid_cldamt,hig_cldamt
  real(r_kind),intent (inout) :: tcamt,lcbas
  real(r_kind),intent (inout) :: ceiling

! declare local variables
  real(r_kind),parameter :: bmiss= 10.e10_r_kind
  real(r_kind),parameter :: lowcld1=zero 
  real(r_kind),parameter :: lowcld2=1500.0_r_kind
  real(r_kind),parameter :: midcld1=1500.0_r_kind
  real(r_kind),parameter :: midcld2=4000.0_r_kind

  integer(i_kind) :: k,kk
  integer(i_kind),dimension(10) :: cldamt_qc
  integer(i_kind),dimension(10) :: cldbas_qc
  real(r_kind) :: cld_prod,basmin
  real(r_kind) :: prod_low,prod_mid,prod_hig
  real(r_kind),dimension(10) :: cldamt
  real(r_kind),dimension(10) :: cldbas
  real(r_kind),dimension(10) :: fact
  real(r_kind),dimension(3,10):: cldseq
  logical no_cloud
  logical fog

! initialize output variables
  low_cldamt=bmiss
  mid_cldamt=bmiss
  hig_cldamt=bmiss
  tcamt=bmiss
  lcbas=bmiss
  ceiling=bmiss

  low_cldamt_qc=15
  mid_cldamt_qc=15
  hig_cldamt_qc=15
  tcamt_qc=15
  lcbas_qc=15

! initialize local variables
  cldamt=bmiss
  cldbas=bmiss
  cldamt_qc=15
  cldbas_qc=15
  fact=one

  !make local copy of cldseq
  cldseq=input_cldseq

! cloud amount and base height
! C 020011
!     0 0 oktas (0/10)
!     1 1 okta or less, but not zero (1/10 or less, but not zero)
!     2 2 oktas (2/10 - 3/10)
!     3 3 oktas (4/10)
!     4 4 oktas (5/10)
!     5 5 oktas (6/10)
!     6 6 oktas (7/10 - 8/10)
!     7 7 oktas or more, but not 8 oktas (9/10 or more, but not 10/10)
!     8 8 oktas (10/10)
!     9 Sky obscured by fog and/or other meteorological phenomena
!    10 Sky partially obscured by fog and/or other meteorological phenomena
!    11 Scattered
!    12 Broken
!    13 Few
!    14 Reserved
!    15 Cloud cover is indiscernible for reasons other than
!                 fog or other meteorological phenomena, or observation is not made

! vsso 0 08002 (surface observations) - vertical significance
!	5  Ceiling
!   	6  Clouds not detected below the following height(s)
!  	7  Low cloud
! 	8  Middle cloud
!	9  High cloud
!	10 Cloud layer with base below the station level and top above the station level
! 	11 Cloud layer with base and top below the station level
     
  do k=1,cldseqlevs

     ! Investigate cloud amounts
     if (cldseq(2,k) < 14.0_r_kind) then
        if (abs(cldseq(2,k)-13._r_kind) < tiny_r_kind) cldseq(2,k)=one
        if (abs(cldseq(2,k)-12._r_kind) < tiny_r_kind) cldseq(2,k)=three
        if (abs(cldseq(2,k)-11._r_kind) < tiny_r_kind) cldseq(2,k)=two
        if (abs(cldseq(2,k)-10._r_kind) < tiny_r_kind) cldseq(2,k)=four
        if (abs(cldseq(2,k)- 9._r_kind) < tiny_r_kind) cldseq(2,k)=7.0_r_kind
        cldamt(k)=cldseq(2,k)*12.5_r_kind   ! convert to percentage
        cldamt_qc(k)=0
     else
        cldamt(k)=bmiss
        cldamt_qc(k)=15
     end if

     ! Investigate cloud base heights and obtain ceiling if present
     if (cldseq(3,k) < bmiss) then
        cldbas(k)=cldseq(3,k)
        cldbas_qc(k)=0
        if (abs(cldseq(1,k)-5._r_kind) < tiny_r_kind) ceiling=cldbas(k)  !Obtaining ceiling here if present
     else
        cldbas(k)=bmiss
        cldbas_qc(k)=15
     end if
  end do

! total cloud amount (%) - TOCC
  if (cld2seqlevs>=1) then
     do k=1,cld2seqlevs
        if (cld2seq(1,k) <= 100.0_r_kind) then
           tcamt=cld2seq(1,k)
           tcamt_qc=0
           exit
        end if
     end do
  end if

! calculate quasi-tcamt 
  if (abs(tcamt-bmiss) < tiny_r_kind) then 
     if (cldseqlevs>0 .and. any(cldamt/=bmiss)) then 
!       check repeated data
        do k=1,cldseqlevs
           if (abs(cldamt(k)-bmiss) < tiny_r_kind) cycle  !avoid missing data
           do kk=k+1,cldseqlevs
              if (abs(cldamt(k)-bmiss) < tiny_r_kind) cycle !is this line needed?
              if (abs(cldamt(k)-cldamt(kk)) < tiny_r_kind) fact(kk)=zero
           end do
        end do

!       calculate quasi-tcamt
        cld_prod=one
        do k=1,cldseqlevs
           if (abs(cldamt(k)-bmiss) < tiny_r_kind) cycle
           cld_prod=cld_prod*(one-fact(k)*cldamt(k)/100.0_r_kind)
        end do
        tcamt=(one-cld_prod)*100.0_r_kind
        tcamt_qc=1
     else
        tcamt_qc=15
     end if
  end if

! HBLCS is defined in the WMO Manual 306 as "Height above surface 
! of the base of the lowest cloud seen".
!!!!!!!!!! CHECK the definition for first guess: AGL or above sea level
! cloud base height of the lowest cloud seen
! 0 20 201 HBLCS
!	0       0 to 50 meters
!       1       50 to 100 meters
!       2       100 to 200 meters
!       3       200 to 300 meters
!       4       300 to 600 meters
!       5       600 to 1000 meters
!       6       1000 to 1500 meters
!       7       1500 to 2000 meters
!       8       2000 to 2500 meters
!       9       2500 meters or more, no clouds
!    10-13      Reserved
!      14       Height of base of cloud not known or base of clouds at a level lower and tops at a level higher than that
!               of the station 
!      15       Missing value
  do k=1,cld2seqlevs
     if (cld2seq(2,k)>=10.0_r_kind) cycle
     lcbas_qc=0
     if (abs(cld2seq(2,k)-0.0_r_kind) <tiny_r_kind) then
        lcbas=25.0_r_kind
     else if (abs(cld2seq(2,k)-1.0_r_kind) <tiny_r_kind) then 
        lcbas=75.0_r_kind
     else if (abs(cld2seq(2,k)-2.0_r_kind) <tiny_r_kind) then 
        lcbas=150.0_r_kind
     else if (abs(cld2seq(2,k)-3.0_r_kind) <tiny_r_kind) then 
        lcbas=250.0_r_kind
     else if (abs(cld2seq(2,k)-4.0_r_kind) <tiny_r_kind) then 
        lcbas=450.0_r_kind
     else if (abs(cld2seq(2,k)-5.0_r_kind) <tiny_r_kind) then 
        lcbas=800.0_r_kind
     else if (abs(cld2seq(2,k)-6.0_r_kind) <tiny_r_kind) then 
        lcbas=1250.0_r_kind
     else if (abs(cld2seq(2,k)-7.0_r_kind) <tiny_r_kind) then 
        lcbas=1750.0_r_kind
     else if (abs(cld2seq(2,k)-8.0_r_kind) <tiny_r_kind) then 
        lcbas=2250.0_r_kind
     else if (abs(cld2seq(2,k)-9.0_r_kind) <tiny_r_kind) then 
        lcbas=3500.0_r_kind
        lcbas_qc=3
     end if
     if (abs(ceiling-bmiss)>tiny_r_kind .and. lcbas>ceiling) lcbas=ceiling
     exit
  end do

! enforce no cloud when lcbas>=3500 & tcamt=zero
  no_cloud=(tcamt==zero .and. tcamt_qc==0)
  if (no_cloud) then 
     lcbas=20000.0_r_kind
     lcbas_qc=0
     do k=1,cldseqlevs
        cldamt(k)=zero
        cldamt_qc(k)=0
        cldbas(k)=20000.0_r_kind
        cldbas_qc(k)=0
     end do
  end if

! handle where lcbas is missing
  if (abs(lcbas-bmiss) < tiny_r_kind) then 
     if (ceiling/=bmiss) then
        lcbas=ceiling 
        lcbas_qc=0
     else if (any(cldbas/=bmiss)) then
        basmin=bmiss
        do k=1,cldseqlevs
           if (abs(cldbas(k)-bmiss) < tiny_r_kind) cycle
           if (cldbas(k)<basmin) basmin=cldbas(k)
        end do
        lcbas=basmin
        if (abs(tcamt-bmiss)>tiny_r_kind .and. tcamt>60.0_r_kind) then
           lcbas_qc=3
        else
           lcbas_qc=4
        end if
     else
        lcbas_qc=15
     end if
  end if


! decide low,mid and high cloud amounts from cldamt and cldbas
  prod_low=one
  prod_mid=one
  prod_hig=one
  do k=1,cldseqlevs
     if (abs(cldbas(k)-bmiss)<tiny_r_kind) cycle
     if (cldbas(k)>=lowcld1 .and. cldbas(k)<lowcld2) then 
        if (abs(cldamt(k)-bmiss)>tiny_r_kind) prod_low=prod_low*(one-fact(k)*cldamt(k)/100.0_r_kind)
     elseif (cldbas(k)>=midcld1 .and. cldbas(k)<midcld2) then
        if (abs(cldamt(k)-bmiss)>tiny_r_kind) prod_mid=prod_mid*(one-fact(k)*cldamt(k)/100.0_r_kind)
     else 
        if (abs(cldamt(k)-bmiss)>tiny_r_kind) prod_hig=prod_hig*(one-fact(k)*cldamt(k)/100.0_r_kind)
     end if
  end do
  if (prod_low/=one) then 
     low_cldamt=(one-prod_low)*100.0_r_kind 
     low_cldamt_qc=3
  end if
  if (prod_mid/=one) then 
     mid_cldamt=(one-prod_mid)*100.0_r_kind
     mid_cldamt_qc=3
  end if
  if (prod_hig/=one) then 
     hig_cldamt=(one-prod_hig)*100.0_r_kind
     hig_cldamt_qc=3
  end if


! weather
  do k=1,wthstrlevs 
     fog=(wthstr(k)>= 40.0_r_kind .and. wthstr(k)<= 49.0_r_kind) .or. &
         (wthstr(k)>=130.0_r_kind .and. wthstr(k)<=135.0_r_kind) .or. &
         (wthstr(k)>=241.0_r_kind .and. wthstr(k)<=249.0_r_kind) 
     if (fog) then
        tcamt=100.0_r_kind
        lcbas=zero 
        tcamt_qc=0
        lcbas_qc=0

!       low cloud amount and base
        low_cldamt=100.0_r_kind
        low_cldamt_qc=1
        exit
     end if
  end do


  ! - Now refine QC marks for tcamt - !

  ! - Place somewhat less trust on obs of clear(er) sky (J. Gerth)
  if (tcamt <= 25.0_r_kind .and. tcamt_qc <= 3) then
     if (tcamt_qc==1) then
        tcamt_qc=2
     else
        tcamt_qc=1
     end if
   else if (tcamt >= 75.0_r_kind .and.  tcamt_qc <= 3 ) then ! sfc based sky cover obs are usually very good when indicating cloudy (J. Gerth)
     tcamt_qc=0
   end if

  !  Background field is MSL, so add station elevation to lcbas and ceiling here
  if (abs(lcbas-bmiss) > tiny_r_kind) lcbas=lcbas+stnelev
  if (abs(ceiling-bmiss) > tiny_r_kind) ceiling=ceiling+stnelev

end subroutine adjust_convcldobs


subroutine adjust_goescldobs(goescld,timeobs,dlat_earth,dlon_earth, &
                        low_cldamt,low_cldamt_qc,mid_cldamt,mid_cldamt_qc, &
                        hig_cldamt,hig_cldamt_qc,tcamt,tcamt_qc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  adjust_goescldobs         obtain cloud amount info from NESDIS goescld
!   prgmmr: Yanqiu Zhu          org: np22                date: 2012-01-03
!
! abstract:  This routine obtain cloud amount info from NESDIS goescld file
!
! program history log:
!   2012-01-03  zhu
!   2014-09-02  carley - Remove references to lcbas.  Remove use of cloud top
!                        pressure obs as pseudo clear-sky cover obs. Inflate qc flag
!                        for night obs and obs showing coverage <= 25%.    
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: iadate
  use constants, only: zero,one,deg2rad,rad2deg,pi
  implicit none

! input variables
  real(r_kind),intent(in) :: timeobs,dlat_earth,dlon_earth
  real(r_kind),intent(in):: goescld
! output variables
  integer(i_kind),intent(inout) :: low_cldamt_qc,mid_cldamt_qc,hig_cldamt_qc
  integer(i_kind),intent(inout) :: tcamt_qc
  real(r_kind),intent(inout) :: low_cldamt,mid_cldamt,hig_cldamt
  real(r_kind),intent(inout) :: tcamt

! declare local variables
  integer(i_kind),parameter,dimension(12):: mday=(/0,31,59,90,&
       120,151,181,212,243,273,304,334/)
  real(r_kind),parameter :: bmiss= 10.e10_r_kind
  real(r_kind),parameter :: lowcld1=zero 
  real(r_kind),parameter :: lowcld2=1500.0_r_kind
  real(r_kind),parameter :: midcld1=1500.0_r_kind
  real(r_kind),parameter :: midcld2=4000.0_r_kind
  real(r_kind),parameter :: zen_limit=80.0_r_kind

  integer(i_kind) :: leap_day,day_of_year
  integer(i_kind),dimension(8):: obs_time,anal_time
  real(r_kind) :: csza,sza,hrang,xlon,declin,gmt
  real(r_kind),dimension(5):: tmp_time

! initialize output variables
  low_cldamt=bmiss
  mid_cldamt=bmiss
  hig_cldamt=bmiss
  tcamt=bmiss

  low_cldamt_qc=15
  mid_cldamt_qc=15
  hig_cldamt_qc=15
  tcamt_qc=15

! cloud amount (%)
  if (goescld<=100.0_r_kind) then
     tcamt=goescld
     tcamt_qc=1


  ! calculate observation time to obtain zenith angle
     anal_time=0
     obs_time=0
     tmp_time=zero
     tmp_time(2)=timeobs
     anal_time(1)=iadate(1)
     anal_time(2)=iadate(2)
     anal_time(3)=iadate(3)
     anal_time(4)=0
     anal_time(5)=iadate(4)
     call w3movdat(tmp_time,anal_time,obs_time) ! observation time

     leap_day = 0
     if( mod(obs_time(1),4)==0 ) then
        if( (mod(obs_time(1),100)/=0).or.(mod(obs_time(1),400)==0) ) leap_day = 1
     endif
     day_of_year = mday(obs_time(2)) + obs_time(3)
     if(obs_time(2) > 2) day_of_year = day_of_year + leap_day

   ! calculation solar declination
     declin=deg2rad*23.45_r_kind*sin(2.0_r_kind*pi*(284+day_of_year)/365.0_r_kind)
 
   ! csza = fraction of solar constant (cos of zenith angle)
     xlon=dlon_earth*rad2deg
     gmt = obs_time(5)   ! UTC
     hrang= (15._r_kind*gmt + xlon - 180._r_kind )*deg2rad
     csza=sin(dlat_earth)*sin(declin)                &
           +cos(dlat_earth)*cos(declin)*cos(hrang)
     csza=max(-one,min(csza,one))
     sza=rad2deg*acos(csza)

     ! - Place somewhat less trust on obs of clear(er) sky (J. Gerth)
     if (tcamt <= 25.0_r_kind) tcamt_qc=2   
     ! - Increase tcamt_qc at night, and increase it moreso
     !     if at night and detecting sky cover <= 25% (J. Gerth recommendation)
     if (sza > zen_limit)  then
        if (tcamt <= 30._r_kind) then
           tcamt_qc=8 !toss obs at night that suggest clear conditions owing to erroneous clearing of low stratus
        else
           if (tcamt_qc==2) then
              tcamt_qc=3
           else
              tcamt_qc=2
           end if
        end if
     end if

  end if    
end subroutine adjust_goescldobs

end module adjust_cloudobs_mod

