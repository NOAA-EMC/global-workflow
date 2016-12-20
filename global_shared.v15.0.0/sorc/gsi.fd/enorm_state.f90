subroutine enorm_state(xst,enorm,yst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    enorm_state
!   prgmmr: tremolet
!
! abstract: compute energy norm on GSI grid
!
! program history log:
!   2007-10-19  tremolet - initial code
!   2009-01-18  todling  - carry summation in quad precision
!   2009-08-14  lueken   - update documentation
!   2010-05-13  todling  - update to use gsi_bundle
!   2010-09-06  todling  - revisit pointers; add Q-term to norm
!   2011-08-01  lueken   - replaced F90 with f90 (no machine logic)
!   2013-10-19  todling  - metguess now holds background
!
!   input argument list:
!    xst
!    yst
!
!   output argument list:
!    enorm
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero,one,cp,rd,pi,two,r1000,hvap
use jcmod, only: eps_eer
use gridmod, only: nsig,nlat,nlon,lon2,lat2,istart,rlats,ak5,bk5
use mpimod, only: mype
use guess_grids, only: ntguessig
use state_vectors, only: dot_product
use gsi_bundlemod, only: assignment(=)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle

implicit none

type(gsi_bundle), intent(inout) :: xst
real(r_quad)    , intent(  out) :: enorm
type(gsi_bundle), intent(inout) :: yst

! Declare local variables
real(r_kind) :: qfact, tfact, pfact, pref, tref, gridfac, zps
real(r_kind) :: coslat(lat2), dsig(lat2,lon2,nsig), akk(nsig)
integer(i_kind) :: ii,jj,kk,ilat,ier
real(r_kind), parameter :: Pa_per_kPa = r1000
real(r_kind),pointer,dimension(:,:,:) :: xst_u,xst_v,xst_t,xst_q
real(r_kind),pointer,dimension(:,:,:) :: yst_u,yst_v,yst_t,yst_q
real(r_kind),pointer,dimension(:,:)   :: xst_p
real(r_kind),pointer,dimension(:,:)   :: yst_p
real(r_kind),pointer,dimension(:,:)   :: ges_ps

! ----------------------------------------------------------------------
tref=280.0_r_kind
pref=1.0e5_r_kind

tfact=cp/tref
pfact=rd*tref/(pref*pref)
qfact=abs(eps_eer)*hvap*hvap/(cp*tref)
gridfac=one/(nlat*nlon)

do kk=1,nsig
   akk(kk) = Pa_per_kPa * (ak5(kk)-ak5(kk+1))
enddo

call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'ps', ges_ps,ier)

dsig=HUGE(dsig)
do jj=2,lon2-1
   do ii=2,lat2-1
      zps = Pa_per_kPa * ges_ps(ii,jj)
      do kk=1,nsig
         dsig(ii,jj,kk) = akk(kk) + (bk5(kk)-bk5(kk+1)) * zps
      enddo
      if (ANY(dsig(ii,jj,:)<=zero)) then
         do kk=1,nsig
            write(6,'(A,I3,4(2X,F18.8))')'enorm ak,bk,pk,dsig=',&
               kk,r1000*ak5(kk),bk5(kk),r1000*ak5(kk)+bk5(kk)*pref,dsig(2,2,kk)
         enddo
         write(6,'(A,I3,4(2X,F18.8))')'enorm ak,bk,pk     =',&
            nsig+1,r1000*ak5(nsig+1),bk5(nsig+1),r1000*ak5(nsig+1)+bk5(nsig+1)*pref
         write(6,*)'enorm_state: negative dsig'
         call stop2(123)
      endif
   enddo
enddo

coslat=HUGE(coslat)
do ii=2,lat2-1
   ilat=istart(mype+1)+ii-2
   if (ilat<1.or.ilat>nlat) then
      write(6,*)'enorm_state: error ilat',ilat
      call stop2(124)
   end if
   coslat(ii)=cos(rlats(ilat))
   if (coslat(ii)<zero) then
      write(6,*)'enorm_state error coslat:',ii,ilat,rlats(ilat),pi/two,coslat(ii)
      coslat(ii)=-coslat(ii)
   endif
enddo
! ----------------------------------------------------------------------
yst=zero
! ----------------------------------------------------------------------

call gsi_bundlegetpointer(xst,'u', xst_u,ier)
call gsi_bundlegetpointer(xst,'v', xst_v,ier)
call gsi_bundlegetpointer(xst,'tv',xst_t,ier)
call gsi_bundlegetpointer(xst,'q', xst_q,ier)
call gsi_bundlegetpointer(xst,'ps',xst_p,ier)

call gsi_bundlegetpointer(yst,'u', yst_u,ier)
call gsi_bundlegetpointer(yst,'v', yst_v,ier)
call gsi_bundlegetpointer(yst,'tv',yst_t,ier)
call gsi_bundlegetpointer(yst,'q', yst_q,ier)
call gsi_bundlegetpointer(yst,'ps',yst_p,ier)

! U
do kk=1,nsig
   do jj=2,lon2-1
      do ii=2,lat2-1
         yst_u(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*xst_u(ii,jj,kk)
      enddo
   enddo
enddo

! V
do kk=1,nsig
   do jj=2,lon2-1
      do ii=2,lat2-1
         yst_v(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*xst_v(ii,jj,kk)
      enddo
   enddo
enddo

! T
do kk=1,nsig
   do jj=2,lon2-1
      do ii=2,lat2-1
         yst_t(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*tfact*xst_t(ii,jj,kk)
      enddo
   enddo
enddo

! Q
if(eps_eer>zero) then
   do kk=1,nsig
      do jj=2,lon2-1
         do ii=2,lat2-1
            yst_q(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*qfact*xst_q(ii,jj,kk)
         enddo
      enddo
   enddo
endif

! P
do jj=2,lon2-1
   do ii=2,lat2-1
      yst_p(ii,jj)=gridfac*coslat(ii)*pfact*xst_p(ii,jj)
   enddo
enddo

! ----------------------------------------------------------------------

enorm=DOT_PRODUCT(yst,xst)

! ----------------------------------------------------------------------

return
end subroutine enorm_state
subroutine enorm_state_red(xst,enorm,yst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    enorm_state
!   prgmmr: tremolet
!
! abstract: compute energy norm on GSI grid
!
! program history log:
!   2007-10-19  tremolet - initial code
!   2009-01-18  todling  - carry summation in quad precision
!   2009-08-14  lueken   - update documentation
!   2010-05-13  todling  - update to use gsi_bundle
!   2010-09-06  todling  - revisit pointers; add Q-term to norm
!   2011-08-01  lueken   - replaced F90 with f90 (no machine logic)
!   2013-10-19  todling  - metguess now holds background
!
!   input argument list:
!    xst
!    yst
!
!   output argument list:
!    enorm
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero,one,cp,rd,pi,two,r1000,hvap
use jcmod, only: eps_eer
use gridmod, only: nsig,nlat,nlon,lon2,lat2,istart,rlats,ak5,bk5
use mpimod, only: mype
use guess_grids, only: ntguessig
use state_vectors, only: dot_product_red
use gsi_bundlemod, only: assignment(=)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle

implicit none

type(gsi_bundle), intent(inout) :: xst
real(r_quad)    , intent(  out) :: enorm
type(gsi_bundle), intent(inout) :: yst

! Declare local variables
real(r_kind) :: qfact, tfact, pfact, pref, tref, gridfac, zps
real(r_kind) :: coslat(lat2), dsig(lat2,lon2,nsig), akk(nsig)
integer(i_kind) :: ii,jj,kk,ilat,ier
real(r_kind), parameter :: Pa_per_kPa = r1000
real(r_kind),pointer,dimension(:,:,:) :: xst_u,xst_v,xst_t,xst_q
real(r_kind),pointer,dimension(:,:,:) :: yst_u,yst_v,yst_t,yst_q
real(r_kind),pointer,dimension(:,:)   :: xst_p
real(r_kind),pointer,dimension(:,:)   :: yst_p
real(r_kind),pointer,dimension(:,:)   :: ges_ps

! ----------------------------------------------------------------------
tref=280.0_r_kind
pref=1.0e5_r_kind

tfact=cp/tref
pfact=rd*tref/(pref*pref)
qfact=abs(eps_eer)*hvap*hvap/(cp*tref)
gridfac=one/(nlat*nlon)

do kk=1,nsig
   akk(kk) = Pa_per_kPa * (ak5(kk)-ak5(kk+1))
enddo

call gsi_bundlegetpointer(gsi_metguess_bundle(ntguessig),'ps', ges_ps,ier)

dsig=HUGE(dsig)
do jj=2,lon2-1
   do ii=2,lat2-1
      zps = Pa_per_kPa * ges_ps(ii,jj)
      do kk=1,nsig
         dsig(ii,jj,kk) = akk(kk) + (bk5(kk)-bk5(kk+1)) * zps
      enddo
      if (ANY(dsig(ii,jj,:)<=zero)) then
         do kk=1,nsig
            write(6,'(A,I3,4(2X,F18.8))')'enorm ak,bk,pk,dsig=',&
               kk,r1000*ak5(kk),bk5(kk),r1000*ak5(kk)+bk5(kk)*pref,dsig(2,2,kk)
         enddo
         write(6,'(A,I3,4(2X,F18.8))')'enorm ak,bk,pk     =',&
            nsig+1,r1000*ak5(nsig+1),bk5(nsig+1),r1000*ak5(nsig+1)+bk5(nsig+1)*pref
         write(6,*)'enorm_state: negative dsig'
         call stop2(123)
      endif
   enddo
enddo

coslat=HUGE(coslat)
do ii=2,lat2-1
   ilat=istart(mype+1)+ii-2
   if (ilat<1.or.ilat>nlat) then
      write(6,*)'enorm_state: error ilat',ilat
      call stop2(124)
   end if
   coslat(ii)=cos(rlats(ilat))
   if (coslat(ii)<zero) then
      write(6,*)'enorm_state error coslat:',ii,ilat,rlats(ilat),pi/two,coslat(ii)
      coslat(ii)=-coslat(ii)
   endif
enddo
! ----------------------------------------------------------------------
yst=zero
! ----------------------------------------------------------------------

call gsi_bundlegetpointer(xst,'u', xst_u,ier)
call gsi_bundlegetpointer(xst,'v', xst_v,ier)
call gsi_bundlegetpointer(xst,'tv',xst_t,ier)
call gsi_bundlegetpointer(xst,'q', xst_q,ier)
call gsi_bundlegetpointer(xst,'ps',xst_p,ier)

call gsi_bundlegetpointer(yst,'u', yst_u,ier)
call gsi_bundlegetpointer(yst,'v', yst_v,ier)
call gsi_bundlegetpointer(yst,'tv',yst_t,ier)
call gsi_bundlegetpointer(yst,'q', yst_q,ier)
call gsi_bundlegetpointer(yst,'ps',yst_p,ier)

! U
do kk=1,nsig
   do jj=2,lon2-1
      do ii=2,lat2-1
         yst_u(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*xst_u(ii,jj,kk)
      enddo
   enddo
enddo

! V
do kk=1,nsig
   do jj=2,lon2-1
      do ii=2,lat2-1
         yst_v(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*xst_v(ii,jj,kk)
      enddo
   enddo
enddo

! T
do kk=1,nsig
   do jj=2,lon2-1
      do ii=2,lat2-1
         yst_t(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*tfact*xst_t(ii,jj,kk)
      enddo
   enddo
enddo

! Q
if(eps_eer>zero) then
   do kk=1,nsig
      do jj=2,lon2-1
         do ii=2,lat2-1
            yst_q(ii,jj,kk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*qfact*xst_q(ii,jj,kk)
         enddo
      enddo
   enddo
endif

! P
do jj=2,lon2-1
   do ii=2,lat2-1
      yst_p(ii,jj)=gridfac*coslat(ii)*pfact*xst_p(ii,jj)
   enddo
enddo

! ----------------------------------------------------------------------

enorm=DOT_PRODUCT_red(yst,xst,0)

! ----------------------------------------------------------------------

return
end subroutine enorm_state_red

