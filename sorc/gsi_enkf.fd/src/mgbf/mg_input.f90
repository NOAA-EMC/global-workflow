module mg_input
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_input
!   prgmmr: rancic           org: NCEP/EMC            date:
!
! abstract:  Module for data input
!            (Here will be defined uniform decomposition and padding
!            with zeros of control variables, required by the filter)
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   input_2d -
!   input_spec1_2d -
!   input_3d -
!
! Functions Included:
!
! remarks:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi

use mg_intstate, only : mg_intstate_type
public input_2d
public input_spec1_2d
public input_3d

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine input_2d &
!***********************************************************************
!                                                                      !
!   Define some function for testing redecomposition                   !
!   (for analysis grid)                                                !
!                                                                      !
!***********************************************************************
(obj_intstate,V,imin,jmin,imax,jmax,imax0,ampl)
!-----------------------------------------------------------------------
use kinds, only: r_kind,i_kind
implicit none
class (mg_intstate_type):: obj_intstate
integer(i_kind),intent(in):: imax,jmax
integer(i_kind),intent(in):: imin,jmin
integer(i_kind),intent(in):: imax0
integer(i_kind),intent(in):: ampl
real(r_kind),dimension(imin:imax,jmin:jmax),intent(out):: V
integer(i_kind):: ng,mg,L,m,n
!-----------------------------------------------------------------------

     do m=imin,jmax
       mg = (obj_intstate%my-1)*jmax+m
     do n=jmin,imax
       ng = (obj_intstate%nx-1)*imax+n
         V(n,m)=ampl*(mg*imax0+ng)
!         V(n,m)=0.
     enddo
     enddo

!-----------------------------------------------------------------------
endsubroutine input_2d

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine input_spec1_2d &
!***********************************************************************
!                                                                      !
!   Define some function for testing redecomposition                   !
!   (for analysis grid)                                                !
!                                                                      !
!***********************************************************************
(obj_intstate,V,nx0,my0,flag)
!-----------------------------------------------------------------------
use kinds, only: r_kind,i_kind
implicit none
class (mg_intstate_type):: obj_intstate
integer(i_kind),intent(in):: nx0,my0
real(r_kind),dimension(1:obj_intstate%nm,1:obj_intstate%mm),intent(out):: V
character(len=2), intent(in):: flag
integer(r_kind):: v0=1.
!-----------------------------------------------------------------------

    V(:,:)=0.

if(flag=='md') then
 if(obj_intstate%nx==nx0.and.obj_intstate%my==my0) then
    V(obj_intstate%nm/2,obj_intstate%mm/2)=v0
 endif
else &
if(flag=='rt') then
 if(obj_intstate%nx==nx0.and.obj_intstate%my==my0) then
    V(obj_intstate%nm,obj_intstate%mm)=v0
 endif
 if(obj_intstate%nx==nx0+1.and.obj_intstate%my==my0) then
    V(1,obj_intstate%mm)=v0
 endif
 if(obj_intstate%nx==nx0.and.obj_intstate%my==my0+1) then
    V(obj_intstate%nm,1)=v0
 endif
 if(obj_intstate%nx==nx0+1.and.obj_intstate%my==my0+1) then
    V(1,1)=v0
 endif
endif

!-----------------------------------------------------------------------
endsubroutine input_spec1_2d

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine input_3d &
!***********************************************************************
!                                                                      !
!   Define some function for testing redecomposition                   !
!   (for analysis grid)                                                !
!                                                                      !
!***********************************************************************
(obj_intstate,V,imin,jmin,lmin,imax,jmax,lmax,imax0,ampl,incrm)
!-----------------------------------------------------------------------
use kinds, only: r_kind,i_kind
implicit none
class (mg_intstate_type):: obj_intstate
integer(i_kind),intent(in):: imin,jmin,lmin
integer(i_kind),intent(in):: imax,jmax,lmax
integer(i_kind),intent(in):: imax0
integer(i_kind),intent(in):: ampl,incrm
real(r_kind),dimension(lmin:lmax,imin:imax,jmin:jmax),intent(out):: V
integer(i_kind):: ng,mg,L,m,n
!-----------------------------------------------------------------------

   do l=lmin,lmax
     do m=imin,jmax
       mg = (obj_intstate%my-1)*jmax+m
     do n=jmin,imax
       ng = (obj_intstate%nx-1)*imax+n
         V(l,n,m)=ampl*(mg*imax0+ng) +(l-1)*incrm
!         V(l,n,m)=0.
     enddo
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine input_3d

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end module mg_input
