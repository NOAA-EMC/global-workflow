subroutine tpause_t(km,p,t,h,ptp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tpause_t    locate tropopause using temperature
!   prgmmr: iredell          org: np23                date: 1999-10-18
!
! abstract: This subprogram finds the tropopause level.  The tropopause 
!   is defined as the lowest level above 500 mb which has a temperature 
!   lapse rate of less than 2 K/km.  The lapse rate must average less 
!   than 2 K/km over a 2 km depth.  If no such level is found below 
!   50 mb, the tropopause is set to 50 mb.
!
!   The tropopause pressure is found hydrostatically.
!
! Program history log:
!   1999-10-18  Mark Iredell - original code
!   2004-05-15  Russ Treadon - add fix to handle low model top case
!   2004-06-15  Russ Treadon - update documentation
!   2013-01-26  parrish - WCOSS debug compile type mismatch error -- fixed by
!                          changing variable kd to kd(1), and modifying calls to
!                          subroutine rsearch.
!
!   Input argument list:
!     km       integer number of levels
!     p        real (km) pressure (Pa)
!     t        real (km) temperature (K)
!     h        real (km) height (m)
!
!   Output argument list:
!     ptp      real tropopause pressure (Pa)
!
! Attributes:
!   Language: Fortran 90
!   machine:
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: rd_over_g,zero,half,one
  implicit none

  integer(i_kind)           ,intent(in   ) :: km
  real(r_kind),dimension(km),intent(in   ) :: p,t,h
  real(r_kind)              ,intent(  out) :: ptp

  real(r_kind),parameter:: ptplim(2)=(/500.0e2_r_kind, 50.0e2_r_kind/)
  real(r_kind),parameter:: gamtp=2.0e-3_r_kind
  real(r_kind),parameter:: hd=2.0e3_r_kind
  real(r_kind) gamu,gamd,td,gami,wtp,ttrop,htp
  real(r_kind),dimension(km):: hphd
  integer(i_kind) klim(2),k,kd(1),ktp,kmm2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Find tropopause level
  kmm2=km-2
#ifdef ibm_sp
  call rsearch(kmm2,p(2:),2,ptplim,klim)
#else
  call rsearch(1,kmm2,1,1,p(2:),2,1,1,ptplim,1,1,klim)
#endif
  klim(1)=klim(1)+2

! The value for klim(2) below is the original value.  This caused problems in
! the regional gsi model because the model top is so close to 50 hPa.  The
! solution is to require that this limit be no more than the number of levels
! minus two.  Otherwise the call to rsearch can fail because k=klim(2)=km-1,
! so km-k-1 = km-(km-1)-1 = km-km+1-1 = 0

! original limit
! klim(2)=klim(2)+1

! new limit
  klim(2)=min(kmm2,klim(2))

  gamd=1.e+9_r_kind
  ktp=klim(2)
  wtp=zero
  do k=1,km
    hphd(k)=h(k)+hd
  end do
  do k=klim(1),klim(2)
     gamu=(t(k-1)-t(k+1))/(h(k+1)-h(k-1))
     if(gamu<=gamtp) then
#ifdef ibm_sp
        call rsearch(km-k-1,h(k+1:),1,hphd(k:),kd)
#else
        call rsearch(1,km-k-1,1,1,h(k+1:),1,1,1,hphd(k:),1,1,kd)
#endif
        td=t(k+kd(1))+(hphd(k)-h(k+kd(1)))/(h(k+kd(1)+1)-h(k+kd(1)))*(t(k+kd(1)+1)-t(k+kd(1)))
        gami=(t(k)-td)/hd
        if(gami<=gamtp) then
           ktp=k
           wtp=(gamtp-gamu)/(max(gamd,gamtp+0.1e-3_r_kind)-gamu)
           exit
        endif
     endif
     gamd=gamu
  enddo

! Compute tropopause level fields
  ttrop=t(ktp)-wtp*(t(ktp)-t(ktp-1))
  htp=h(ktp)-wtp*(h(ktp)-h(ktp-1))
  ptp=p(ktp)*exp((h(ktp)-htp)*(one-half*(ttrop/t(ktp)-one))/(rd_over_g*t(ktp)))
  return
end subroutine tpause_t
