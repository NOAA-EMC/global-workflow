subroutine init_jcdfi
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_jcdfi
!   prgmmr: tremolet
!
! abstract: Setup weights for Dolph-Chebyshev window digital filter
!
! program history log:
!   2007-10-18  tremolet - initial code
!   2009-08-17  lueken   - update documentation
!   2011-08-01  lueken   - changed F90 to f90 (no machine logic)
!
!   input argument list:
!
!   output argument list:
!
! attrbiutes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!----------------------------------------------------------------------

use kinds, only: r_kind,i_kind
use gsi_4dvar, only: nobs_bins, hr_obsbin
use constants, only: zero, one, two, pi,r3600
use mpimod, only: mype
use jcmod, only: wgtdfi

implicit none

real(r_kind) :: tauc,rtdfi
real(r_kind) :: zx(0:nobs_bins),zp(0:nobs_bins)
real(r_kind) :: zd,zh,zl,zn,zr,zs,zt
integer(i_kind) :: nstdfi,jj,jn

!----------------------------------------------------------------------

tauc   = 6.0_r_kind*r3600
rtdfi  = hr_obsbin *r3600
nstdfi = (nobs_bins-1)/2

if (mype==0) then
   write(6,*)'Setup weights for Dolph-Chebyshev window digital filter'
   write(6,*)'Number of DFI timesteps: ',2*nstdfi
   write(6,*)'DFI timestep: ',rtdfi
   write(6,*)'Cut-off period: ',tauc/r3600
   write(6,*)'Time-span: ',(two*nstdfi*rtdfi)/r3600
   write(6,*)'Filtered fields valid at initial time + ',INT(nstdfi*rtdfi),' s.'
   write(6,*)'init_jcdfi: tauc,rtdfi,nstdfi=',tauc,rtdfi,nstdfi
endif

allocate(wgtdfi(nobs_bins))
wgtdfi=zero
zx=zero
zp=zero

! Discretization of [0,pi/2]; computation of Chebyshev polynomials
zl=real(nobs_bins,r_kind)
zd=pi/zl
zt=pi/real(nstdfi,r_kind)
zs=one/cos(zt)
DO jj=0,nstdfi
   zx(jj)=real(jj,r_kind)*zd
   zp(jj)=fcheby(zs*cos(zx(jj)),2*nstdfi)
ENDDO
zr=two/zp(0)

! Computing weights : forward then backward time-steps
DO jn=0,nstdfi
   zh=one
   DO jj=1,nstdfi
      zh = zh + zr*zp(jj)*cos(real(2*jj,r_kind)*zx(jn))
   ENDDO
   wgtdfi(nstdfi+1+jn) = zh/zl
ENDDO
DO jn=1,nstdfi
   wgtdfi(nstdfi+1-jn) = wgtdfi(nstdfi+1+jn)
ENDDO

! Combining with simple filter
zt=two*pi*rtdfi/tauc
zs=zero
DO jn=-nstdfi,nstdfi
   zn = real(jn,r_kind)
   if (jn==0) then
      zh = zt/pi
   else
      zh = sin(zn*zt)/(zn*pi)
   endif
   wgtdfi(nstdfi+1+jn) = wgtdfi(nstdfi+1+jn)*zh
   zs = zs + wgtdfi(nstdfi+1+jn)
ENDDO
DO jn=-nstdfi,nstdfi
   wgtdfi(nstdfi+1+jn) = wgtdfi(nstdfi+1+jn)/zs
ENDDO

! Checking...
if (mype==0) write(6,*)'init_jcdfi: wgtdfi=',wgtdfi

zs=zero
DO jj=1,nobs_bins
   zs=zs+wgtdfi(jj)
ENDDO
if (ABS(zs-one)>EPSILON(zs)) then
   write(6,*)'init_jcdfi: Sum of weights is not 1',zs
   call stop2(318)
end if

!----------------------------------------------------------------------
contains
!----------------------------------------------------------------------
real(r_kind) function fcheby(px,kn)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fcheby
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2010-09-05  todling - bug fix: replace loop index jj to ii
!
!   input argument list:
!    kn
!    px
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
integer(i_kind), intent(in   ) :: kn
real(r_kind)   , intent(in   ) :: px

integer(i_kind) :: ii
real(r_kind) :: z0,z1,z2

jn=ABS(kn)
z0=one
z1=px

if (jn==0) then
   fcheby=z0
elseif (jn==1) then
   fcheby=z1
else
   do ii=2,jn
      z2=two*px*z1-z0
      z0=z1
      z1=z2
   enddo
   fcheby=z2
endif

return
end function fcheby
!----------------------------------------------------------------------
end subroutine init_jcdfi
