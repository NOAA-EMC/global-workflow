subroutine compute_fact10(u,v,t,q,ps,prsi1,prsi2,skint,z0rl,islimsk,f10m)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    compute_fact10                compute 10m wind factor
!   prgmmr: treadon          org: np23                date: 2006-09-28
!
! abstract: Use GFS surface physics routines to compute 10m wind factor
!
! program history log:
!   2006-09-28 treadon - initial routine
!   2008-06-05 safford - rm unused vars and uses, comment out unused params
!
!   input argument list:
!      u       - u wind component (2d field, 1st model layer)
!      v       - v wind component
!      t       - sensible temperature
!      q       - specific humidity
!      ps      - surface pressure
!      prsi1   - surface pressure pressure
!      prsi2   - interface pressure at top of first layer
!      skint   - skin temperature
!      z0rl    - surface roughness
!      islimsk - land/sea/ice mask
!
!   output argument list:
!     f10m     - 10m wind factor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: grav,zero,quarter,half,one,two,four,five,&
       fv,rd,rd_over_cp,r1000
  implicit none

! Passed Variables
  real(r_kind)   ,intent(in   ) :: u,v,t,q,ps,skint,z0rl
  integer(i_kind),intent(in   ) :: islimsk
  real(r_kind)   ,intent(  out) :: f10m
  real(r_kind)   ,intent(in   ) :: prsi1,prsi2

! Local Variables
  real(r_kind):: prsl,prkl
  real(r_kind):: prki1,prki2
  real(r_kind):: q0,tem,del,rkap,rkapi,rkapp1
  real(r_kind)::ustar,wind,z0,z0max,ztmax,tv1,z1, &
       rat,thv1,theta1,tvs,dtv,rb,fm,fh,hlinf, &
       hl1,pm,ph,pm10,hl12,ph2,fm10
  real(r_kind):: psurf,ps1
  real(r_kind) restar,aa0,bb0,fhs,fms,hl0,hlt,adtv,bb,aa,hl0inf,hltinf,&
       hl110,olinf

! Local Parameters
  real(r_kind),parameter::  charnok=0.014_r_kind
! real(r_kind),parameter::  ca=0.4_r_kind
  real(r_kind),parameter::  alpha=five
  real(r_kind),parameter::  a0=-3.975_r_kind
  real(r_kind),parameter::  a1=12.32_r_kind
  real(r_kind),parameter::  b1=-7.755_r_kind
  real(r_kind),parameter::  b2=6.041_r_kind
  real(r_kind),parameter::  a0p=-7.941_r_kind
  real(r_kind),parameter::  a1p=24.75_r_kind
  real(r_kind),parameter::  b1p=-8.705_r_kind
  real(r_kind),parameter::  b2p=7.899_r_kind 
  real(r_kind),parameter::  vis=1.4e-5_r_kind
! real(r_kind),parameter::  aa1=-1.076_r_kind
! real(r_kind),parameter::  bb1=0.7045_r_kind
! real(r_kind),parameter::  cc1=-0.05808_r_kind
! real(r_kind),parameter::  bb2=-0.1954_r_kind
! real(r_kind),parameter::  cc2=0.009999_r_kind
! real(r_kind),parameter::  rnu=1.51e-5_r_kind
! real(r_kind),parameter::  arnu=0.135_r_kind*rnu
  real(r_kind),parameter::  ten=10.0_r_kind


!  INITIALIZE VARIABLES. ALL UNITS ARE SUPPOSEDLY M.K.S. UNLESS SPECIFIED
!  PSURF IS IN PASCALS
!  WIND IS WIND SPEED, THETA1 IS ADIABATIC SURFACE TEMP FROM LEVEL 1
!  SURFACE ROUGHNESS LENGTH IS CONVERTED TO M FROM CM


   rkap = rd_over_cp
   RKAPI  = one / RKAP
   RKAPP1 = one + RKAP

   rat=zero ; restar=zero ; ustar=zero
   del = prsi1-prsi2
   tem   = rkapp1*del
   prki1 = (prsi1*0.01_r_kind)**rkap
   prki2 = (prsi2*0.01_r_kind)**rkap
   prkl  = (prki1*prsi1-prki2*prsi2)/tem
   prsl  = 100.0_r_kind*prkl**rkapi

   psurf=ps*r1000
   ps1=prsl*r1000
   wind= sqrt( u*u + v*v )
   wind=max(wind,one)
   q0=max(q,1.e-8_r_kind)
   theta1=t*(prki1/prkl)
   tv1 =t*(one+fv*q0)
   thv1=theta1 *(one+fv*q0)
   tvs =skint*(one+fv*q0)
   z0=0.01_r_kind*z0rl
   z1=-rd*tv1*log(ps1/psurf)/grav


!  COMPUTE STABILITY DEPENDENT EXCHANGE COEFFICIENTS
   if (islimsk == 0) then
      ustar = sqrt(grav * z0 / charnok)
   end if
!  COMPUTE STABILITY INDICES (RB AND HLINF)
   z0max = min(z0,one*z1)
   ztmax = z0max
   if (islimsk == 0) then
      restar=ustar*z0max/vis
      restar=max(restar,1.e-6_r_kind)

!     Rat taken from Zeng, Zhao and Dickinson 1997
      rat = 2.67_r_kind * restar**quarter - 2.57_r_kind
      rat = min(rat,7.0_r_kind)
      ztmax = z0max * exp(-rat)
   end if

   dtv = thv1 - tvs
   adtv = abs(dtv)
   adtv = max(adtv,1.e-3_r_kind)
   dtv = sign(one,dtv)*adtv
   rb = grav*dtv*z1 / (half*(thv1+tvs)*wind*wind)
   rb=max(rb,-5.e3_r_kind)
   fm=log((z0max+z1) / z0max)
   fh=log((ztmax+z1) / ztmax)
   hlinf=rb*fm*fm / fh
   fm10=log((z0max+ten) / z0max)


!  STABLE CASE
   if (dtv >= zero) then
      hl1=hlinf
   end if        
   if ((dtv >= zero) .AND. (hlinf > quarter)) then
      hl0inf=z0max*hlinf/z1
      hltinf=ztmax*hlinf/z1
      aa=sqrt(one + four*alpha*hlinf)
      aa0=sqrt(one + four*alpha*hl0inf)
      bb=aa
      bb0=sqrt(one + four*alpha*hltinf)
      pm=aa0 - aa + log((aa+one)/(aa0+one))
      ph=bb0 - bb + log((bb+one)/(bb0+one))
      fms=fm-pm
      fhs=fh-ph
      hl1=fms*fms*rb/fhs
   end if

!  SECOND ITERATION
   if (dtv >= zero) then
      hl0=z0max*hl1/z1
      hlt=ztmax*hl1/z1
      aa=sqrt(one + four*alpha*hl1)
      aa0=sqrt(one + four*alpha*hl0)
      bb=aa
      bb0=sqrt(one + four*alpha*hlt)
      pm=aa0 - aa + log((aa+one)/(aa0+one))
      ph=bb0 - bb + log((bb+one)/(bb0+one))
      hl110=hl1*ten/z1
      aa=sqrt(one + four*alpha*hl110)
      pm10=aa0 - aa + log((aa+one)/(aa0+one))
      hl12=hl1*two/z1
      bb=sqrt(one + four*alpha*hl12)
      ph2=bb0 - bb + log((bb+one)/(bb0+one))
   end if


!  UNSTABLE CASE
!  CHECK FOR UNPHYSICAL OBUKHOV LENGTH
   if (dtv < zero) then
      olinf = z1/hlinf
      if ( abs(olinf) <= z0max*50.0_r_kind ) then
         hlinf = -z1/(50.0_r_kind*z0max)
      end if
   end if

!  GET PM AND PH
   if (dtv < zero .AND. hlinf >= (-half)) then
      hl1=hlinf
      pm=(a0 + a1*hl1)*hl1/(one + b1*hl1 + b2*hl1*hl1)
      ph=(a0p + a1p*hl1)*hl1/(one + b1p*hl1 + b2*hl1*hl1)
      hl110=hl1*ten/z1
      pm10=(a0 + a1*hl110)*hl110/(one + b1*hl110 + b2*hl110*hl110)
      hl12=hl1*two/z1
      ph2=(a0p + a1p*hl12)*hl12/(one + b1p*hl12 + b2p*hl12*hl12)
   end if
   if (dtv < zero .AND. hlinf < (-half)) then
      hl1=-hlinf
      pm=log(hl1) + two*hl1**(-quarter) - 0.8776_r_kind
      ph=log(hl1) + half*hl1**(-half) + 1.386_r_kind
      hl110=hl1*ten/z1
      pm10=log(hl110) + two*hl110**(-quarter) - 0.8776_r_kind
      hl12=hl1*two/z1
      ph2=log(hl12) + half*hl12**(-half) + 1.386_r_kind
   end if


!  FINISH THE EXCHANGE COEFFICIENT COMPUTATION TO PROVIDE FM AND FH
   fm=fm-pm
   fh=fh-ph
   fm10=fm10-pm10
   f10m=fm10/fm


   return
  end subroutine compute_fact10

