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
implicit none

PRIVATE
PUBLIC cw2hydro_tl
PUBLIC cw2hydro_ad


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
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
            if (clouds(ic)=='ql') sv_rank3(i,j,k)=cwgues(i,j,k)*(one-work(i,j,k))
            if (clouds(ic)=='qi') sv_rank3(i,j,k)=cwgues(i,j,k)*work(i,j,k)
         end do
      end do
   end do
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
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
!           if (clouds(ic)=='ql') sv_rank3(i,j,k)=cv_cw(i,j,k)*(one-work0(i,j,k))-cwgues(i,j,k)*work(i,j,k)
!           if (clouds(ic)=='qi') sv_rank3(i,j,k)=cv_cw(i,j,k)*work0(i,j,k)+cwgues(i,j,k)*work(i,j,k)
            if (clouds(ic)=='ql') sv_rank3(i,j,k)=cv_cw(i,j,k)*(one-work0(i,j,k))
            if (clouds(ic)=='qi') sv_rank3(i,j,k)=cv_cw(i,j,k)*work0(i,j,k)
         end do
      end do
   end do
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
call gsi_bundlegetpointer (wbundle,'cw',cv_cw,istatus)
cv_cw=zero

do k=1,nsig
   do j=1,lon2
      do i=1,lat2
         work0(i,j,k)=-r0_05*(ges_tsen(i,j,k,ntguessig)-t0c)
         work0(i,j,k)=max(zero,work0(i,j,k))
         work0(i,j,k)=min(one,work0(i,j,k))
      end do
   end do
end do

do ic=1,nclouds
   call gsi_bundlegetpointer (rval,clouds(ic),rv_rank3,istatus)
   if (istatus/=0) cycle
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
            if (clouds(ic)=='ql') then
               cv_cw(i,j,k)=cv_cw(i,j,k)+rv_rank3(i,j,k)*(one-work0(i,j,k))
               rv_rank3(i,j,k)=zero
            end if

            if (clouds(ic)=='qi') then
               cv_cw(i,j,k)=cv_cw(i,j,k)+rv_rank3(i,j,k)*work0(i,j,k)
               rv_rank3(i,j,k)=zero
            end if

         end do
      end do
   end do
end do

return
end subroutine cw2hydro_ad

end module cwhydromod
