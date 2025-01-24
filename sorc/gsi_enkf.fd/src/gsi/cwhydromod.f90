module cwhydromod

!$$$ module documentation block
!           .      .    .                                       .
! module:   cwhydromod     module for cw2hydro and its adjoint cw2hydro_ad
!   prgmmr: yanqiu zhu
!
! abstract: module for cw2hydro and its adjoint cw2hydro_ad for cloudy radiance assimilation
!
! program history log:
!   2011-07-12  zhu - initial code
!
!
! subroutines included:
!   sub init_cw2hydro
!   sub destroy_cw2hydro
!   sub cw2hydro
!   sub cw2hydro_ad
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use constants, only: zero,one,r0_05,t0c,fv,max_varname_length
use gridmod, only: lat2,lon2,nsig
use guess_grids, only: ges_tsen,ntguessig
use derivsmod, only: cwgues
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle
implicit none

PRIVATE
PUBLIC cw2hydro_tl
PUBLIC cw2hydro_ad
PUBLIC cw2hydro_tl_hwrf
PUBLIC cw2hydro_ad_hwrf
real(r_kind),parameter :: t1=t0c-30.0_r_kind
real(r_kind),parameter :: t2=t0c-40.0_r_kind
real(r_kind),parameter :: coef1=0.05_r_kind
real(r_kind),parameter :: coef2=0.10_r_kind
real(r_kind),pointer,dimension(:,:,:):: fice=>NULL()
real(r_kind),pointer,dimension(:,:,:):: frain=>NULL()
real(r_kind),pointer,dimension(:,:,:):: frimef=>NULL()
integer(i_kind) :: istatus


contains

subroutine cw2hydro(sval,clouds,nclouds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cw2hydro
!   prgmmr: yanqiu zhu
!
! abstract:  Converts control variable cw to hydrometers
!
! program history log:
!   2011-07-12  zhu - initial code
!
!   input argument list:
!     sval - State variable
!     wbundle - bundle for control variable
!     clouds - cloud names
!
!   output argument list:
!     sval - State variable
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(gsi_bundle),intent(inout):: sval
integer(i_kind),intent(in) :: nclouds
character(len=max_varname_length),intent(in):: clouds(nclouds)

! Declare local variables
integer(i_kind) i,j,k,ic,istatus
real(r_kind),dimension(lat2,lon2,nsig) :: work
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         work(i,j,k)=-r0_05*(ges_tsen(i,j,k,ntguessig)-t0c)
         work(i,j,k)=max(zero,work(i,j,k))
         work(i,j,k)=min(one,work(i,j,k))
      end do
   end do
end do

! Split cw into cloud_lqw and cloud_ice, very simple for now
do ic=1,nclouds
   call gsi_bundlegetpointer (sval,clouds(ic),sv_rank3,istatus)
   if (istatus/=0) cycle
   sv_rank3=zero
   if (clouds(ic)=='ql') then
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               sv_rank3(i,j,k)=cwgues(i,j,k)*(one-work(i,j,k))
            end do
         end do
      end do
   else if (clouds(ic)=='qi') then
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               sv_rank3(i,j,k)=cwgues(i,j,k)*work(i,j,k)
            end do
         end do
      end do
   end if
end do

return
end subroutine cw2hydro


subroutine cw2hydro_tl(sval,wbundle,clouds,nclouds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cw2hydro_tl
!   prgmmr: yanqiu zhu
!
! abstract:  Tangent linear of converting control variable cw to hydrometers
!
! program history log:
!   2011-07-12  zhu - initial code
!   2014-04-24  zhu - comment out temperature increment impact on cloud for now
!
!   input argument list:
!     sval - State variable
!     wbundle - bundle for control variable
!     clouds - cloud names
!
!   output argument list:
!     sval - State variable
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(gsi_bundle),intent(inout):: sval
type(gsi_bundle),intent(in):: wbundle
integer(i_kind),intent(in) :: nclouds
!real(r_kind),intent(in) :: sv_tsen(lat2,lon2,nsig)
character(len=max_varname_length),intent(in):: clouds(nclouds)

! Declare local variables
integer(i_kind) i,j,k,ic,istatus
real(r_kind),dimension(lat2,lon2,nsig) :: work0
! real(r_kind),dimension(lat2,lon2,nsig) :: work
real(r_kind),pointer,dimension(:,:,:) :: cv_cw
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

! Get pointer to required control variable
call gsi_bundlegetpointer (wbundle,'cw',cv_cw,istatus)

do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         work0(i,j,k)=-r0_05*(ges_tsen(i,j,k,ntguessig)-t0c)
         work0(i,j,k)=max(zero,work0(i,j,k))
         work0(i,j,k)=min(one,work0(i,j,k))

!         work(i,j,k)=-r0_05*sv_tsen(i,j,k)
!         if (work0(i,j,k)<=zero) work(i,j,k)=zero
!         if (work0(i,j,k)>=one)  work(i,j,k)=zero
      end do
   end do
end do

! Split cv_cw into cloud_lqw and cloud_ice, very simple for now
do ic=1,nclouds
   call gsi_bundlegetpointer (sval,clouds(ic),sv_rank3,istatus)
   if (istatus/=0) cycle
   sv_rank3=zero
   if (clouds(ic)=='ql') then
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
!              sv_rank3(i,j,k)=cv_cw(i,j,k)*(one-work0(i,j,k))-cwgues(i,j,k)*work(i,j,k)
               sv_rank3(i,j,k)=cv_cw(i,j,k)*(one-work0(i,j,k))
            end do
         end do
      end do
   else if (clouds(ic)=='qi') then
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
!              sv_rank3(i,j,k)=cv_cw(i,j,k)*work0(i,j,k)+cwgues(i,j,k)*work(i,j,k)
               sv_rank3(i,j,k)=cv_cw(i,j,k)*work0(i,j,k)
            end do
         end do
      end do
   end if
end do

return
end subroutine cw2hydro_tl

subroutine cw2hydro_ad(rval,wbundle,clouds,nclouds)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cw2hydro_ad
!   prgmmr: yanqiu zhu
!
! abstract:  adjoint of cw2hydro
!
! program history log:
!   2011-07-12  zhu - initial code
!   2014-04-24  zhu - comment out temperature increment impact on cloud for now
!
!   input argument list:
!     rval - State variable
!     wbundle - work bundle 
!     clouds - cloud names
!
!   output argument list:
!     wbundle
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(gsi_bundle),intent(in):: rval
type(gsi_bundle),intent(inout):: wbundle
integer(i_kind),intent(in) :: nclouds
character(len=max_varname_length),intent(in):: clouds(nclouds)

! Declare local variables
integer(i_kind) i,j,k,ic,istatus
real(r_kind),dimension(lat2,lon2,nsig) :: work0
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3
real(r_kind),pointer,dimension(:,:,:) :: cv_cw

! Get pointer to required control variable

do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         work0(i,j,k)=-r0_05*(ges_tsen(i,j,k,ntguessig)-t0c)
         work0(i,j,k)=max(zero,work0(i,j,k))
         work0(i,j,k)=min(one,work0(i,j,k))
      end do
   end do
end do

call gsi_bundlegetpointer (wbundle,'cw',cv_cw,istatus)
do ic=1,nclouds
   call gsi_bundlegetpointer (rval,clouds(ic),rv_rank3,istatus)
   if (istatus/=0) cycle
   if (clouds(ic)=='ql') then
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               cv_cw(i,j,k)=cv_cw(i,j,k)+rv_rank3(i,j,k)*(one-work0(i,j,k))
               rv_rank3(i,j,k)=zero
            end do
         end do
      end do
   else if (clouds(ic)=='qi') then
      do k=1,nsig
         do j=1,lon2
            do i=1,lat2
               cv_cw(i,j,k)=cv_cw(i,j,k)+rv_rank3(i,j,k)*work0(i,j,k)
               rv_rank3(i,j,k)=zero
            end do
         end do
      end do
   end if

end do

return
end subroutine cw2hydro_ad

subroutine cw2hydro_tl_hwrf(sval,wbundle,sv_tsen)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cw2hydro_tl_hwrf
!   prgmmr: Ting-Chi Wu
!
! abstract:  Tangent linear of converting control variable cw to hydrometers
!
! program history log:
!   2017-07-19  T.-C. Wu - modified from cw2hydro_tl to use 6 instead of 2 hydrometeors
!
!   input argument list:
!    sval     - state variable
!    wbundle  - bundel for control variable
!    clouds - cloud names
!
!   output argument list:
!    sval     - state variable
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(gsi_bundle), intent(inout):: sval
type(gsi_bundle), intent(in   ):: wbundle
real(r_kind),     intent(in   ):: sv_tsen(lat2,lon2,nsig)

! Declare local variables
integer(i_kind) i,j,k,istatus
real(r_kind) coef, dcoefdt
real(r_kind) dicedt, dicedcw, dprecicedt, dprecicedcw
real(r_kind), pointer, dimension(:,:,:) :: cv_cw
real(r_kind), pointer, dimension(:,:,:) :: sv_cw, sv_ql, sv_qi, sv_qr, sv_qs, sv_qg, sv_qh

call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),  'fice', fice,istatus)
call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig), 'frain', frain,istatus)
call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'frimef', frimef,istatus)

! Split cw into cloud_liquid, cloud_ice, rain, snow, graupel, and hail
! (cloud_calc in cloud_efr_mod.f90)
call gsi_bundlegetpointer (wbundle, 'cw', cv_cw, istatus)
call gsi_bundlegetpointer (sval, 'cw', sv_cw, istatus)
call gsi_bundlegetpointer (sval, 'ql', sv_ql, istatus)
call gsi_bundlegetpointer (sval, 'qi', sv_qi, istatus)
call gsi_bundlegetpointer (sval, 'qr', sv_qr, istatus)
call gsi_bundlegetpointer (sval, 'qs', sv_qs, istatus)
call gsi_bundlegetpointer (sval, 'qg', sv_qg, istatus)
call gsi_bundlegetpointer (sval, 'qh', sv_qh, istatus)

sv_ql=zero
sv_qi=zero
sv_qr=zero
sv_qs=zero
sv_qg=zero
sv_qh=zero
sv_cw=zero

do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         sv_ql(i,j,k)=cv_cw(i,j,k)*(one-fice(i,j,k))*(one-frain(i,j,k)) ! ql is not a function of T
         sv_qr(i,j,k)=cv_cw(i,j,k)*(one-fice(i,j,k))*frain(i,j,k) ! qr is not a function of T

         if ( ges_tsen(i,j,k,ntguessig) > t0c-30.0_r_kind) then
           dicedcw = 0.05_r_kind*fice(i,j,k)
           dprecicedcw = 0.95_r_kind*fice(i,j,k)
           dicedt = zero
           dprecicedt = zero
         else
           coef=(ges_tsen(i,j,k,ntguessig)-t2)/(t1-t2)*coef1+ &
                (ges_tsen(i,j,k,ntguessig)-t1)/(t1-t2)*coef2
           dcoefdt = one/(t1-t2)*coef1+one/(t1-t2)*coef2
           dicedcw = coef*fice(i,j,k)
           dprecicedcw = (one-coef)*fice(i,j,k)
           dicedt = dcoefdt*cwgues(i,j,k)*fice(i,j,k)
           dprecicedt = -dcoefdt*cwgues(i,j,k)*fice(i,j,k)
         endif

         sv_qi(i,j,k)=cv_cw(i,j,k)*dicedcw+sv_tsen(i,j,k)*dicedt

         if (frimef(i,j,k)>=one .and. frimef(i,j,k)<=5.0_r_kind) then
           sv_qs(i,j,k)=cv_cw(i,j,k)*dprecicedcw+sv_tsen(i,j,k)*dprecicedt
         endif
         if (frimef(i,j,k)>5.0_r_kind .and. frimef(i,j,k)<=20.0_r_kind) then
           sv_qg(i,j,k)=cv_cw(i,j,k)*dprecicedcw+sv_tsen(i,j,k)*dprecicedt
         endif
         if (frimef(i,j,k)>20.0_r_kind) then
           sv_qh(i,j,k)=cv_cw(i,j,k)*dprecicedcw+sv_tsen(i,j,k)*dprecicedt
         endif

      end do
   end do
end do

sv_cw=cv_cw

return

end subroutine cw2hydro_tl_hwrf

subroutine cw2hydro_ad_hwrf(rval,wbundle,rv_tsen)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cw2hydro_ad_hwrf
!   prgmmr: Ting-Chi Wu
!
! abstract:  adjoint of cw2hydro_hwrf (subroutine cloud_calc)
!
! program history log:
!   2017-07-19  T.-C. Wu - modified from cw2hydro_ad to use 6 instead of 2 hydrometeors
!
!   input argument list:
!     rval   - state variable
!     wbundle - work bundle
!
!   output argument list:
!     wbundle 
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(gsi_bundle), intent(in   ):: rval
type(gsi_bundle), intent(inout):: wbundle
real(r_kind),intent(inout) :: rv_tsen(:,:,:)

! Declare local variables
integer(i_kind) i,j,k, istatus
real(r_kind) coef, dcoefdt
real(r_kind) dicedt, dicedcw, dprecicedt, dprecicedcw
real(r_kind) work
real(r_kind), pointer, dimension(:,:,:) :: cv_cw
real(r_kind), pointer, dimension(:,:,:) :: rv_cw, rv_ql, rv_qi, rv_qr, rv_qs, rv_qg, rv_qh

call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),  'fice', fice,istatus)
call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig), 'frain', frain,istatus)
call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'frimef', frimef,istatus)

call gsi_bundlegetpointer (wbundle, 'cw', cv_cw, istatus)
call gsi_bundlegetpointer (rval, 'cw', rv_cw, istatus)
call gsi_bundlegetpointer (rval, 'ql', rv_ql, istatus)
call gsi_bundlegetpointer (rval, 'qi', rv_qi, istatus)
call gsi_bundlegetpointer (rval, 'qr', rv_qr, istatus)
call gsi_bundlegetpointer (rval, 'qs', rv_qs, istatus)
call gsi_bundlegetpointer (rval, 'qg', rv_qg, istatus)
call gsi_bundlegetpointer (rval, 'qh', rv_qh, istatus)

do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         rv_cw(i,j,k)=rv_cw(i,j,k)+rv_ql(i,j,k)*(one-fice(i,j,k))*(one-frain(i,j,k))
         rv_ql(i,j,k)=zero
!         rv_tsen(i,j,k)=rv_tsen(i,j,k)+zero ! ql is not a function of T
      end do
   end do
end do
do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         rv_cw(i,j,k)=rv_cw(i,j,k)+rv_qr(i,j,k)*(one-fice(i,j,k))*frain(i,j,k)
         rv_qr(i,j,k)=zero
!         rv_tsen(i,j,k)=rv_tsen(i,j,k)+zero ! qr is not a function of T
      end do
   end do
end do


do k=1,nsig
   do j=1,lon2
      do i=1,lat2
        if ( ges_tsen(i,j,k,ntguessig) > t0c-30.0_r_kind) then
          dicedcw = 0.05_r_kind*fice(i,j,k)
          dprecicedcw = 0.95_r_kind*fice(i,j,k)
          dicedt = zero
          dprecicedt = zero
        else
          coef=(ges_tsen(i,j,k,ntguessig)-t2)/(t1-t2)*coef1+ &
               (ges_tsen(i,j,k,ntguessig)-t1)/(t1-t2)*coef2
          dcoefdt = one/(t1-t2)*coef1+one/(t1-t2)*coef2

          dicedcw = coef*fice(i,j,k)
          dprecicedcw = (one-coef)*fice(i,j,k)
          dicedt = dcoefdt*cwgues(i,j,k)*fice(i,j,k)
          dprecicedt = -dcoefdt*cwgues(i,j,k)*fice(i,j,k)
        endif

        work=zero
        work=work+rv_qi(i,j,k)*dicedt
        rv_cw(i,j,k)=rv_cw(i,j,k)+rv_qi(i,j,k)*dicedcw
        rv_qi(i,j,k)=zero
        rv_tsen(i,j,k)=rv_tsen(i,j,k)+work
        if (frimef(i,j,k)>=one .and. frimef(i,j,k)<=5.0_r_kind) then
          work=work+rv_qs(i,j,k)*dprecicedt
          rv_cw(i,j,k)=rv_cw(i,j,k)+rv_qs(i,j,k)*dprecicedcw
          rv_qs(i,j,k)=zero
          rv_tsen(i,j,k)=rv_tsen(i,j,k)+work
        endif
        if (frimef(i,j,k)>5.0_r_kind .and. frimef(i,j,k)<=20.0_r_kind) then
          work=work+rv_qg(i,j,k)*dprecicedt
          rv_cw(i,j,k)=rv_cw(i,j,k)+rv_qg(i,j,k)*dprecicedcw
          rv_qg(i,j,k)=zero
          rv_tsen(i,j,k)=rv_tsen(i,j,k)+work
        endif
        if (frimef(i,j,k)>20.0_r_kind) then
          work=work+rv_qh(i,j,k)*dprecicedt
          rv_cw(i,j,k)=rv_cw(i,j,k)+rv_qh(i,j,k)*dprecicedcw
          rv_qh(i,j,k)=zero
          rv_tsen(i,j,k)=rv_tsen(i,j,k)+work
        endif
      end do
   end do
end do

cv_cw=rv_cw
rv_cw=zero

return

end subroutine cw2hydro_ad_hwrf


end module cwhydromod
