!$$$  Subprogram documentation block
!
! Subprogram: gslp       Compute sea level pressure as in the GFS
!   Prgmmr: Iredell      Org: np23        Date: 1999-10-18
!
! Abstract: This subprogram computes sea level pressure from profile data
!   using the Shuell method in the GFS.
!
! Program history log:
!   1999-10-18  Mark Iredell
!
! Usage:  call gslp(km,hs,ps,p,t,sh,prmsl,h,ho)
!   Input argument list:
!     km       integer number of levels
!     hs       real surface height (m)
!     ps       real surface pressure (Pa)
!     p        real (km) profile pressures (Pa)
!     t        real (km) profile temperatures (K)
!     sh       real (km) profile specific humidities (kg/kg)
!   Output argument list:
!     prmsl    real sea level pressure (Pa)
!     h        real integer-layer height (m)
!     ho       real integer-layer height at 1000hPa and 500hPa  (m)
!
! Modules used:
!   physcons       physical constants
!
! Attributes:
!   Language: Fortran 90
!
!$$$
subroutine gslp(km,hs,ps,p,t,sh,prmsl,h,ho)
  use physcons
  implicit none
  integer,intent(in):: km
  real,intent(in):: hs,ps
  real,intent(in),dimension(km):: p,t,sh
  real,intent(out):: prmsl
  real,intent(out),dimension(km):: h
  real,intent(out),dimension(2):: ho
  real,parameter:: gammam=-6.5e-3,zshul=75.,tvshul=290.66
  real,parameter:: pm1=1.e5,tm1=287.45,hm1=113.,hm2=5572.,&
                   fslp=con_g*(hm2-hm1)/(con_rd*tm1)
  integer k,i
  real aps,ap(km),tv(km)
  real apo(2)
  real tvu,tvd,gammas,part
  real hfac
!  compute model heights
  aps=log(ps)
  ap(1)=log(p(1))
  tv(1)=t(1)*(1+con_fvirt*sh(1))
  h(1)=hs-con_rog*tv(1)*(ap(1)-aps)
  do k=2,km
    ap(k)=log(p(k))
    tv(k)=t(k)*(1+con_fvirt*sh(k))
    h(k)=h(k-1)-con_rog*0.5*(tv(k-1)+tv(k))*(ap(k)-ap(k-1))
  enddo
!  compute 1000 and 500 mb heights
  apo(1)=log(1000.e2)
  apo(2)=log(500.e2)
  do i=1,2
    if(aps.lt.apo(i)) then
      tvu=tv(1)
      if(h(1).gt.zshul) then
        tvd=tvu-gammam*h(1)
        if(tvd.gt.tvshul) then
          if(tvu.gt.tvshul) then
            tvd=tvshul-5.e-3*(tvu-tvshul)**2
          else
            tvd=tvshul
          endif
        endif
        gammas=(tvu-tvd)/h(1)
      else
        gammas=0.
      endif
      part=con_rog*(apo(i)-ap(1))
      ho(i)=h(1)-tvu*part/(1.+0.5*gammas*part)
    else
      do k=1,km
        if(ap(k).lt.apo(i)) then
          ho(i)=h(k)-con_rog*tv(k)*(apo(i)-ap(k))
          exit
        endif
      enddo
    endif
  enddo
!  compute sea level pressure
  hfac=ho(1)/(ho(2)-ho(1))
  prmsl=pm1*exp(fslp*hfac)
end subroutine
