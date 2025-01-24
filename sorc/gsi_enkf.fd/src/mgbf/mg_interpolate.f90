submodule(mg_intstate) mg_interpolate
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_interpolate
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  General mapping between 2d arrays using linerly squared
!            interpolations
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   def_offset_coef -
!   lsqr_mg_coef -
!   lwq_vertical_coef -
!   lwq_vertical_adjoint -
!   lwq_vertical_direct -
!   lwq_vertical_adjoint_spec -
!   lwq_vertical_direct_spec -
!   l_vertical_adjoint_spec -
!   l_vertical_direct_spec -
!   lsqr_direct_offset -
!   lsqr_adjoint_offset -
!   quad_direct_offset -
!   quad_adjoint_offset -
!   lin_direct_offset -
!   lin_adjoint_offset -
!   l_vertical_adjoint_spec2 -
!   l_vertical_direct_spec2 -
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

use kinds
use jp_pkind2, only: fpi

implicit none
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine def_offset_coef (this)                        
!***********************************************************************
implicit none
class(mg_intstate_type),target::this
                                                                      
real(r_kind):: r64,r32,r128
!-----------------------------------------------------------------------
 r64 = 1.0d0/64.0d0
 r32 = 1.0d0/32.0d0
 r128= 1.0d0/128.0d0

! p_coef =(/-3.,51,29,-3/)
! q_coef =(/-3.,19.0d0,51.0d0,-3.0d0/)
! p_coef = p_coef*r64
! q_coef = q_coef*r64

 this%p_coef =(/-9.,111.,29.,-3./)
 this%q_coef =(/-3.,29.,111.,-9./)
 this%p_coef = this%p_coef*r128
 this%q_coef = this%q_coef*r128

 this%a_coef =(/5.,30.,-3./)
 this%b_coef =(/-3.,30.,5./)
 this%a_coef=this%a_coef*r32
 this%b_coef=this%b_coef*r32
!-----------------------------------------------------------------------
endsubroutine def_offset_coef

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lsqr_mg_coef (this)                           
!***********************************************************************
!                                                                      !
!   Prepare coeficients for mapping between:                           !
!   filter grid on analysis decomposition:  W(1-ib:im+ib,1-jb:jm+jb)   !
!   and analysis grid:                      V(1:nm,1:mm)               !  
!                       - offset version -                             !
!                                                                      !
!              (  im < nm  and  jm < mm   )                            !
!                                                                      !
!***********************************************************************
implicit none
class(mg_intstate_type),target::this
real(r_kind), dimension(1:this%nm):: xa
real(r_kind), dimension(1-this%ib:this%im+this%ib):: xf
real(r_kind), dimension(1:this%mm):: ya
real(r_kind), dimension(1-this%jb:this%jm+this%jb):: yf
integer(i_kind):: i,j,n,m
real(r_kind) x1,x2,x3,x4,x
real(r_kind) x1x,x2x,x3x,x4x
real(r_kind) rx2x1,rx3x1,rx4x1,rx3x2,rx4x2,rx4x3
real(r_kind) y1,y2,y3,y4,y
real(r_kind) y1y,y2y,y3y,y4y
real(r_kind) ry2y1,ry3y1,ry4y1,ry3y2,ry4y2,ry4y3
real(r_kind) cfl1,cfl2,cfl3,cll
real(r_kind) cfr1,cfr2,cfr3,crr
real(r_kind) x1_x,x2_x,x3_x
real(r_kind) y1_y,y2_y,y3_y
!-----------------------------------------------------------------------
!
! Initialize
!
 
   do n=1,this%nm
     xa(n)=this%xa0+this%dxa*(n-1)
   enddo

   do i=1-this%ib,this%im+this%ib
     xf(i)=this%xf0+this%dxf*(i-1)
   enddo

   do m=1,this%mm
     ya(m)=this%ya0+this%dya*(m-1)
   enddo

   do j=1-this%jb,this%jm+this%jb
     yf(j)=this%yf0+this%dyf*(j-1)
   enddo

!
! Find iref and jref
!
   do n=1,this%nm
     do i=1-this%ib,this%im+this%ib-1
       if( xa(n)< xf(i)) then
         this%iref(n)=i-2
         this%irefq(n)=i-1
         this%irefL(n)=i-1
         exit
       endif
     enddo
   enddo

   do m=1,this%mm
     do j=1-this%jb,this%jm+this%jb-1
       if(ya(m) < yf(j)) then
         this%jref(m)=j-2
         this%jrefq(m)=j-1
         this%jrefL(m)=j-1
         exit
       endif
     enddo
   enddo

   do n=1,this%nm
     i=this%iref(n)
     x1=xf(i)
     x2=xf(i+1)
     x3=xf(i+2)
     x4=xf(i+3)
     x = xa(n)
       x1x = x1-x   
       x2x = x2-x   
       x3x = x3-x   
       x4x = x4-x   
       rx2x1 = 1./(x2-x1)
       rx3x1 = 1./(x3-x1)
       rx4x1 = 1./(x4-x1)
       rx3x2 = 1./(x3-x2)
       rx4x2 = 1./(x4-x2)
       rx4x3 = 1./(x4-x3)
     CFL1 = x2x*x3x*rx2x1*rx3x1
     CFL2 =-x1x*x3x*rx2x1*rx3x2
     CFL3 = x1x*x2x*rx3x1*rx3x2
     CLL = x3x*rx3x2
     CFR1 = x3x*x4x*rx3x2*rx4x2
     CFR2 =-x2x*x4x*rx3x2*rx4x3
     CFR3 = x2x*x3x*rx4x2*rx4x3
     CRR =-x2x*rx3x2
       this%cx0(n)=CFL1*CLL
       this%cx1(n)=CFL2*CLL+CFR1*CRR
       this%cx2(n)=CFL3*CLL+CFR2*CRR
       this%cx3(n)=CFR3*CRR
   enddo

   do m=1,this%mm
     j=this%jref(m)
     y1=yf(j)
     y2=yf(j+1)
     y3=yf(j+2)
     y4=yf(j+3)
     y = ya(m)
       y1y = y1-y   
       y2y = y2-y   
       y3y = y3-y   
       y4y = y4-y   
       ry2y1 = 1./(y2-y1)
       ry3y1 = 1./(y3-y1)
       ry4y1 = 1./(y4-y1)
       ry3y2 = 1./(y3-y2)
       ry4y2 = 1./(y4-y2)
       ry4y3 = 1./(y4-y3)
     CFL1 = y2y*y3y*ry2y1*ry3y1
     CFL2 =-y1y*y3y*ry2y1*ry3y2
     CFL3 = y1y*y2y*ry3y1*ry3y2
     CLL = y3y*ry3y2
     CFR1 = y3y*y4y*ry3y2*ry4y2
     CFR2 =-y2y*y4y*ry3y2*ry4y3
     CFR3 = y2y*y3y*ry4y2*ry4y3
     CRR =-y2y*ry3y2
       this%cy0(m)=CFL1*CLL
       this%cy1(m)=CFL2*CLL+CFR1*CRR
       this%cy2(m)=CFL3*CLL+CFR2*CRR
       this%cy3(m)=CFR3*CRR
   enddo

!
! Quadratic interpolations
!
   do n=1,this%nm
     i=this%irefq(n)
     x1=xf(i)
     x2=xf(i+1)
     x3=xf(i+2)
     x = xa(n)
       x1_x = x1-x
       x2_x = x2-x
       x3_x = x3-x
       rx2x1 = 1./(x2-x1)
       rx3x1 = 1./(x3-x1)
       rx3x2 = 1./(x3-x2)
       this%qx0(n) = x2_x*x3_x*rx2x1*rx3x1
       this%qx1(n) =-x1_x*x3_x*rx2x1*rx3x2
       this%qx2(n) = x1_x*x2_x*rx3x1*rx3x2
   enddo

   do m=1,this%mm
     i=this%jrefq(m)
     y1=yf(i)
     y2=yf(i+1)
     y3=yf(i+2)
     y = ya(m)
       y1_y = y1-y
       y2_y = y2-y
       y3_y = y3-y
       ry2y1 = 1./(y2-y1)
       ry3y1 = 1./(y3-y1)
       ry3y2 = 1./(y3-y2)
       this%qy0(m) = y2_y*y3_y*ry2y1*ry3y1
       this%qy1(m) =-y1_y*y3_y*ry2y1*ry3y2
       this%qy2(m) = y1_y*y2_y*ry3y1*ry3y2
   enddo
 
!
! Linear interpolations
!
   do n=1,this%nm
     i=this%irefL(n)
     x1=xf(i)
     x2=xf(i+1)
     x = xa(n)
       x1_x = x1-x
       x2_x = x2-x
       rx2x1 = 1./(x2-x1)
       this%Lx0(n) = x2_x*rx2x1
       this%Lx1(n) =-x1_x*rx2x1
   enddo

   do m=1,this%mm
     j=this%jrefL(m)
     y1=yf(j)
     y2=yf(j+1)
     y = ya(m)
       y1_y = y1-y
       y2_y = y2-y
       ry2y1 = 1./(y2-y1)
       this%Ly0(m) = y2_y*ry2y1
       this%Ly1(m) =-y1_y*ry2y1
   enddo
!-----------------------------------------------------------------------
endsubroutine lsqr_mg_coef

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lwq_vertical_coef &
!***********************************************************************
!                                                                      !
!  Prepare coeficients for vertical mapping between:                   !
!  analysis grid vertical resolution (nm) and                          !
!  generation one of filter grid vertical resoluition (im)             !
!                                                                      !
!              (  im <= nm )                                           !
!                                                                      !
!***********************************************************************
(this,nm_in,im_in,c1,c2,c3,c4,iref_out)
implicit none
class(mg_intstate_type),target::this

integer(i_kind), intent(in):: nm_in,im_in
real(r_kind), dimension(1:nm_in), intent(out):: c1,c2,c3,c4
integer(i_kind), dimension(1:nm_in), intent(out):: iref_out

real(r_kind), dimension(1:nm_in):: y
real(r_kind), dimension(0:im_in+1):: x
real(r_kind):: dy,x1,x2,x3,x4,dx1,dx2,dx3,dx4 
real(r_kind):: dx13,dx23,dx24

integer(i_kind):: i,n
!-----------------------------------------------------------------------

   do i=0,im_in+1
     x(i)=(i-1)*1.
   enddo

    dy = 1.*(im_in-1)/(nm_in-1)
  do n=1,nm_in
    y(n)=(n-1)*dy
  enddo
    y(nm_in)=x(im_in)
 
  do n=2,nm_in-1
    i = y(n)+1
      x1 = x(i-1)
      x2 = x(i)
      x3 = x(i+1)
      x4 = x(i+2)
    iref_out(n)=i
      dx1 = y(n)-x1
      dx2 = y(n)-x2
      dx3 = y(n)-x3
      dx4 = y(n)-x4
      dx13 = dx1*dx3
      dx23 = 0.5*dx2*dx3
      dx24 = dx2*dx4
    c1(n) = -dx23*dx3
    c2(n) =  (    dx13+0.5*dx24)*dx3
    c3(n) = -(0.5*dx13+    dx24)*dx2
    c4(n) = dx23*dx2

    if(iref_out(n)==1) then
      c3(n)=c3(n)+c1(n)
      c1(n)=0.
    endif
    if(iref_out(n)==im_in-1) then
      c2(n)=c2(n)+c4(n)
      c4(n)=0.
    endif
  enddo
     iref_out(1)=1; c1(1)=0.; c2(1)=1.; c3(1)=0.; c4(1)=0.
     iref_out(nm_in)=im_in; c1(nm_in)=0.; c2(nm_in)=1.; c3(nm_in)=0.; c4(n)=0.

!-----------------------------------------------------------------------
endsubroutine lwq_vertical_coef                            

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lwq_vertical_adjoint &
!***********************************************************************
!                                                                      !
!  Direct linerly weighted quadratic adjoint interpolation in vertical !
!  from reslution nm to resolution km                                  !
!                                                                      !
!              (  km <= nm )                                           !
!                                                                      !
!***********************************************************************
(this,nm_in,km_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,w,f)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: nm_in,km_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
integer(i_kind), dimension(1:nm_in), intent(in):: kref
real(r_kind), dimension(1:nm_in,imin:imax,jmin:jmax), intent(in):: w
real(r_kind), dimension(1:km_in,imin:imax,jmin:jmax), intent(out):: f
integer(i_kind):: k,n
!-----------------------------------------------------------------------
  f = 0.
do n=2,nm_in-1
  k = kref(n)
  if( k==1 ) then
    f(1,:,:) = f(1,:,:)+c2(n)*w(n,:,:)
    f(2,:,:) = f(2,:,:)+c3(n)*w(n,:,:)
    f(3,:,:) = f(3,:,:)+c4(n)*w(n,:,:)
  elseif &
    ( k==km_in-1) then
    f(km_in-2,:,:) = f(km_in-2,:,:)+c1(n)*w(n,:,:)
    f(km_in-1,:,:) = f(km_in-1,:,:)+c2(n)*w(n,:,:)
    f(km_in  ,:,:) = f(km_in  ,:,:)+c3(n)*w(n,:,:)
  elseif( k==km_in) then
    f(k  ,:,:) = f(k  ,:,:)+c2(n)*w(n,:,:)
  else
    f(k-1,:,:) = f(k-1,:,:)+c1(n)*w(n,:,:)
    f(k  ,:,:) = f(k  ,:,:)+c2(n)*w(n,:,:)
    f(k+1,:,:) = f(k+1,:,:)+c3(n)*w(n,:,:)
    f(k+2,:,:) = f(k+2,:,:)+c4(n)*w(n,:,:)
  endif
enddo
    f(1,:,:)=f(1,:,:)+w(1,:,:)
    f(km_in,:,:)=f(km_in,:,:)+w(nm_in,:,:)

!-----------------------------------------------------------------------
endsubroutine lwq_vertical_adjoint

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lwq_vertical_direct &
!***********************************************************************
!                                                                      !
!  Linerly weighted direct quadratic interpolation in vertical         !
!  from reslouion km to resolution nm                                  !
!                                                                      !
!              (  km <= nm )                                           !
!                                                                      !
!***********************************************************************
(this,km_in,nm_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,f,w)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: km_in,nm_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
integer(i_kind), dimension(1:nm_in), intent(in):: kref
real(r_kind), dimension(1:km_in,imin:imax,jmin:jmax), intent(in):: f
real(r_kind), dimension(1:nm_in,imin:imax,jmin:jmax), intent(out):: w
integer(i_kind):: k,n
!-----------------------------------------------------------------------
do n=2,nm_in-1
  k = kref(n)
  if( k==1 ) then
    w(n,:,:) =             c2(n)*f(k,:,:)+c3(n)*f(k+1,:,:)+c4(n)*f(k+2,:,:)
  elseif &
    ( k==km_in-1) then
    w(n,:,:) =c1(n)*f(k-1,:,:)+c2(n)*f(k,:,:)+c3(n)*f(k+1,:,:)
  elseif &
    ( k==km_in)   then
    w(n,:,:) =                 c2(n)*f(k,:,:)
  else
    w(n,:,:) =c1(n)*f(k-1,:,:)+c2(n)*f(k,:,: )+c3(n)*f(k+1,:,:)+c4(n)*f(k+2,:,:)
  endif
enddo
    w(1,:,:)=f(1,:,:)
    w(nm_in,:,:)=f(km_in,:,:)

!-----------------------------------------------------------------------
endsubroutine lwq_vertical_direct

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lwq_vertical_adjoint_spec &
!***********************************************************************
!                                                                      !
!  Direct linerly weighted quadratic adjoint interpolation in vertical !
!  from reslution nm to resolution km                                  !
!                                                                      !
!              (  km <= nm )                                           !
!                                                                      !
!***********************************************************************
(this,km3_in,nm_in,km_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,W,F)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: km3_in,nm_in,km_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
integer(i_kind), dimension(1:nm_in), intent(in):: kref
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(in):: W
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(out):: F
integer(i_kind):: k,n
!-----------------------------------------------------------------------
  F = 0.
do n=2,nm_in-1
  k = kref(n)
  if( k==1 ) then
    F(:,:,:,1) = F(:,:,:,1)+c2(n)*W(:,:,:,n)
    F(:,:,:,2) = F(:,:,:,2)+c3(n)*W(:,:,:,n)
    F(:,:,:,3) = F(:,:,:,3)+c4(n)*W(:,:,:,n)
  elseif &
    ( k==km_in-1) then
    F(:,:,:,km_in-2) = F(:,:,:,km_in-2)+c1(n)*W(:,:,:,n)
    F(:,:,:,km_in-1) = F(:,:,:,km_in-1)+c2(n)*W(:,:,:,n)
    F(:,:,:,km_in  ) = F(:,:,:,km_in  )+c3(n)*W(:,:,:,n)
  elseif( k==km_in) then
    F(:,:,:,k   ) = F(:,:,:,k   )+c2(n)*W(:,:,:,n)
  else
    F(:,:,:,k-1) = F(:,:,:,k-1)+c1(n)*W(:,:,:,n)
    F(:,:,:,k  ) = F(:,:,:,k  )+c2(n)*W(:,:,:,n)
    F(:,:,:,k+1) = F(:,:,:,k+1)+c3(n)*W(:,:,:,n)
    F(:,:,:,k+2) = F(:,:,:,k+2)+c4(n)*W(:,:,:,n)
  endif
enddo
    F(:,:,:,1 )=F(:,:,:,1 )+W(:,:,:,1 )
    F(:,:,:,km_in)=F(:,:,:,km_in)+W(:,:,:,nm_in)
!-----------------------------------------------------------------------
endsubroutine lwq_vertical_adjoint_spec

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lwq_vertical_direct_spec &
!***********************************************************************
!                                                                      !
!  Linerly weighted direct quadratic interpolation in vertical         !
!  from reslouion im to resolution nm                                  !
!                                                                      !
!              (  km <= nm )                                           !
!                                                                      !
!***********************************************************************
(this,km3_in,km_in,nm_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,F,W)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: km3_in,km_in,nm_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
integer(i_kind), dimension(1:nm_in), intent(in):: kref
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(in):: F
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(out):: W
integer(i_kind):: k,n
!-----------------------------------------------------------------------
do n=2,nm_in-1
  k = kref(n)
  if( k==1 ) then
    W(:,:,:,n) =                   c2(n)*F(:,:,:,k)+c3(n)*F(:,:,:,k+1)+c4(n)*F(:,:,:,k+2)
  elseif &
    ( k==km_in-1) then
    W(:,:,:,n) =c1(n)*F(:,:,:,k-1)+c2(n)*F(:,:,:,k)+c3(n)*F(:,:,:,k+1)
  elseif &
    ( k==km_in)   then
    W(:,:,:,n) =                   c2(n)*F(:,:,:,k)
  else
    W(:,:,:,n) =c1(n)*F(:,:,:,k-1)+c2(n)*F(:,:,:,k)+c3(n)*F(:,:,:,k+1)+c4(n)*F(:,:,:,k+2)
  endif
enddo
    W(:,:,:,1 )=F(:,:,:,1 )
    W(:,:,:,nm_in)=F(:,:,:,km_in)
!-----------------------------------------------------------------------
endsubroutine lwq_vertical_direct_spec

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine l_vertical_adjoint_spec &
!***********************************************************************
!                                                                      !
!  Adjoint of linear interpolations in vertical                        !
!  from reslution nm to resolution km                                  !
!                                                                      !
!              (  nm = 2*km-1 )                                        !
!                                                                      !
!***********************************************************************
(this,km3_in,nm_in,km_in,imin,imax,jmin,jmax,W,F)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: km3_in,nm_in,km_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(in):: W
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(out):: F
integer(i_kind):: k,n
!-----------------------------------------------------------------------
  F = 0.

      k=1
  do n=2,nm_in-1,2
    F(:,:,:,k  ) = F(:,:,:,k  )+0.5*W(:,:,:,n)
    F(:,:,:,k+1) = F(:,:,:,k+1)+0.5*W(:,:,:,n)
      k=k+1
  enddo

      k=1
  do n=1,nm_in,2
    F(:,:,:,k  ) = F(:,:,:,k  )+    W(:,:,:,n)
      k=k+1
  enddo
!-----------------------------------------------------------------------
endsubroutine l_vertical_adjoint_spec

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine l_vertical_direct_spec &
!***********************************************************************
!                                                                      !
!                                                                      !
!  Direct linear interpolations in vertical                            !
!  from reslution nm to resolution km                                  !
!                                                                      !
!              (  nm = 2*km-1 )                                        !
!                                                                      !
!***********************************************************************
(this,km3_in,km_in,nm_in,imin,imax,jmin,jmax,F,W)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: km3_in,km_in,nm_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(in):: F
real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(out):: W
integer(i_kind):: k,n
!-----------------------------------------------------------------------
      k=1
  do n=1,nm_in,2
    W(:,:,:,n) =F (:,:,:,k)
      k=k+1
  enddo

      k=1
  do n=2,nm_in-1,2
    W(:,:,:,n) = 0.5*(F(:,:,:,k)+F(:,:,:,k+1))
      k=k+1
  enddo
!-----------------------------------------------------------------------
endsubroutine l_vertical_direct_spec

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lsqr_direct_offset &
!***********************************************************************
!                                                                      !
! Given a source array  V(km,1-ib:im+ib,1-jb:jm+jb) perform            !
! direct interpolations to get target array W(km,1:nm,1:mm)            !
! using two passes of 1d interpolator                                  !
!                                                                      !
!***********************************************************************
(this,V_in,W,km_in,ibm,jbm)
!-----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km_in,ibm,jbm
real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(in):: V_in
real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(out):: W  

real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
integer(i_kind):: i,j,n,m
real(r_kind),dimension(km_in):: v0,v1,v2,v3     
!-----------------------------------------------------------------------
   do j=1-jbm,this%jm+jbm
   do n=1,this%nm
       i = this%iref(n)
     v0(:)=V_in(:,i  ,j)
     v1(:)=V_in(:,i+1,j)
     v2(:)=V_in(:,i+2,j)
     v3(:)=V_in(:,i+3,j)
     VX(:,n,j) = this%cx0(n)*v0(:)+this%cx1(n)*v1(:)+this%cx2(n)*v2(:)+this%cx3(n)*v3(:)
   enddo
   enddo

   do m=1,this%mm
     j = this%jref(m)
   do n=1,this%nm
     v0(:)=VX(:,n,j  ) 
     v1(:)=VX(:,n,j+1) 
     v2(:)=VX(:,n,j+2) 
     v3(:)=VX(:,n,j+3) 
     W(:,n,m) =  this%cy0(m)*v0(:)+this%cy1(m)*v1(:)+this%cy2(m)*v2(:)+this%cy3(m)*v3(:)
   enddo
   enddo
!-----------------------------------------------------------------------
endsubroutine lsqr_direct_offset

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lsqr_adjoint_offset &
!***********************************************************************
!                                                                      !
! Given a target array W(km,1:nm,1:mm) perform adjoint                 !
! interpolations to get source array V(km,1-ib:im+ib,1-jb:jm+jb)       !
! using two passes of 1d interpolator                                  !
!                      - offset version -                              !
!                                                                      !
!***********************************************************************
(this,W,V_out,km_in,ibm,jbm)
!-----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind):: km_in,ibm,jbm
real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(in):: W  
real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(out):: V_out
real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
real(r_kind), dimension(km_in):: wk
real(r_kind), dimension(km_in):: vxk
integer(i_kind):: i,j,n,m,l,k
real(r_kind):: c0,c1,c2,c3
!-----------------------------------------------------------------------
   V_out(:,:,:)=0.
   VX(:,:,:)=0.

   do m=1,this%mm
     j = this%jref(m)
     c0 = this%cy0(m)
     c1 = this%cy1(m)
     c2 = this%cy2(m)
     c3 = this%cy3(m)
   do n=1,this%nm
       wk(:)=W(:,n,m)
     VX(:,n,j  ) = VX(:,n,j  )+wk(:)*c0
     VX(:,n,j+1) = VX(:,n,j+1)+wk(:)*c1
     VX(:,n,j+2) = VX(:,n,j+2)+wk(:)*c2
     VX(:,n,j+3) = VX(:,n,j+3)+wk(:)*c3
   enddo
   enddo

   do n=1,this%nm
     i = this%iref(n)
     c0 = this%cx0(n)
     c1 = this%cx1(n)
     c2 = this%cx2(n)
     c3 = this%cx3(n)
   do j=1-jbm,this%jm+jbm
       vxk(:)=VX(:,n,j)
     V_out(:,i  ,j) = V_out(:,i  ,j)+vxk(:)*c0
     V_out(:,i+1,j) = V_out(:,i+1,j)+vxk(:)*c1
     V_out(:,i+2,j) = V_out(:,i+2,j)+vxk(:)*c2
     V_out(:,i+3,j) = V_out(:,i+3,j)+vxk(:)*c3
   enddo
   enddo
!-----------------------------------------------------------------------
endsubroutine lsqr_adjoint_offset

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine quad_direct_offset &
!***********************************************************************
!                                                                      !
! Given a source array  V(km,1-ib:im+ib,1-jb:jm+jb) perform            !
! direct interpolations to get target array W(km,1:nm,1:mm)            !
! using two passes of 1d interpolator                                  !
!                                                                      !
!***********************************************************************
(this,V_in,W,km_in,ibm,jbm)
!-----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km_in,ibm,jbm
real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(in):: V_in
real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(out):: W
real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
integer(i_kind):: i,j,n,m
real(r_kind),dimension(km_in):: v0,v1,v2
!-----------------------------------------------------------------------
   do n=1,this%nm
     i = this%irefq(n)
   do j=1-jbm,this%jm+jbm
     v0(:)=V_in(:,i  ,j)
     v1(:)=V_in(:,i+1,j)
     v2(:)=V_in(:,i+2,j)
     VX(:,n,j) = this%qx0(n)*v0(:)+this%qx1(n)*v1(:)+this%qx2(n)*v2(:)
   enddo
   enddo

   do m=1,this%mm
     j = this%jrefq(m)
   do n=1,this%nm
     v0(:)=VX(:,n,j  )
     v1(:)=VX(:,n,j+1)
     v2(:)=VX(:,n,j+2)
     W(:,n,m) =  this%qy0(m)*v0(:)+this%qy1(m)*v1(:)+this%qy2(m)*v2(:)
   enddo
   enddo
!-----------------------------------------------------------------------
endsubroutine quad_direct_offset

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine quad_adjoint_offset &
!***********************************************************************
!                                                                      !
! Given a target array W(km,1:nm,1:mm) perform adjoint                 !
! interpolations to get source array V(km,1-ib:im+ib,1-jb:jm+jb)       !
! using two passes of 1d interpolator                                  !
!                      - offset version -                              !
!                                                                      !
!***********************************************************************
(this,W,V_out,km_in,ibm,jbm)
!-----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind):: km_in,ibm,jbm
real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(in):: W
real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(out):: V_out
real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
real(r_kind), dimension(km_in):: wk
real(r_kind), dimension(km_in):: vxk
integer(i_kind):: i,j,n,m,l,k
real(r_kind):: c0,c1,c2
!-----------------------------------------------------------------------
   V_out(:,:,:)=0.
   VX(:,:,:)=0.

   do m=1,this%mm
     j = this%jrefq(m)
     c0 = this%qy0(m)
     c1 = this%qy1(m)
     c2 = this%qy2(m)
   do n=1,this%nm
       wk(:)=W(:,n,m)
     VX(:,n,j  ) = VX(:,n,j  )+wk(:)*c0
     VX(:,n,j+1) = VX(:,n,j+1)+wk(:)*c1
     VX(:,n,j+2) = VX(:,n,j+2)+wk(:)*c2
   enddo
   enddo


   do n=1,this%nm
     i = this%irefq(n)
     c0 = this%qx0(n)
     c1 = this%qx1(n)
     c2 = this%qx2(n)
   do j=1-jbm,this%jm+jbm
       vxk(:)=VX(:,n,j)
     V_out(:,i  ,j) = V_out(:,i  ,j)+vxk(:)*c0
     V_out(:,i+1,j) = V_out(:,i+1,j)+vxk(:)*c1
     V_out(:,i+2,j) = V_out(:,i+2,j)+vxk(:)*c2
   enddo
   enddo
!-----------------------------------------------------------------------
endsubroutine quad_adjoint_offset

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lin_direct_offset &
!***********************************************************************
!                                                                      !
! Given a source array  V(km,1-ib:im+ib,1-jb:jm+jb) perform            !
! direct interpolations to get target array W(km,1:nm,1:mm)            !
! using two passes of 1d linear interpolator                           !
!                                                                      !
!                      - offset version -                              !
!                                                                      !
!***********************************************************************
(this,V_in,W,km_in,ibm,jbm)
!-----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km_in,ibm,jbm
real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(in):: V_in
real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(out):: W
real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
integer(i_kind):: i,j,n,m
real(r_kind),dimension(km_in):: v0,v1
!-----------------------------------------------------------------------
   do n=1,this%nm
     i = this%irefL(n)
   do j=1-jbm,this%jm+jbm
     v0(:)=V_in(:,i  ,j)
     v1(:)=V_in(:,i+1,j)
     VX(:,n,j) = this%Lx0(n)*v0(:)+this%Lx1(n)*v1(:)
   enddo
   enddo

   do m=1,this%mm
     j = this%jrefL(m)
   do n=1,this%nm
     v0(:)=VX(:,n,j  )
     v1(:)=VX(:,n,j+1)
     W(:,n,m) =  this%Ly0(m)*v0(:)+this%Ly1(m)*v1(:)
   enddo
   enddo
!-----------------------------------------------------------------------
endsubroutine lin_direct_offset

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine lin_adjoint_offset &
!***********************************************************************
!                                                                      !
! Given a target array W(km,1:nm,1:mm) perform adjoint                 !
! interpolations to get source array V(km,1-ib:im+ib,1-jb:jm+jb)       !
! using two passes of 1d linear interpolator                           !
!                                                                      !
!                      - offset version -                              !
!                                                                      !
!***********************************************************************
(this,W,V_out,km_in,ibm,jbm)
!-----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind):: km_in,ibm,jbm
real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(in):: W
real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(out):: V_out
real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
real(r_kind), dimension(km_in):: wk
real(r_kind), dimension(km_in):: vxk
integer(i_kind):: i,j,n,m,l,k
real(r_kind):: c0,c1
!-----------------------------------------------------------------------
   V_out(:,:,:)=0.
   VX(:,:,:)=0.

   do m=1,this%mm
     j = this%jrefL(m)
     c0 = this%Ly0(m)
     c1 = this%Ly1(m)
   do n=1,this%nm
       wk(:)=W(:,n,m)
     VX(:,n,j  ) = VX(:,n,j  )+wk(:)*c0
     VX(:,n,j+1) = VX(:,n,j+1)+wk(:)*c1
   enddo
   enddo

   do n=1,this%nm
     i = this%irefL(n)
     c0 = this%Lx0(n)
     c1 = this%Lx1(n)
   do j=1-jbm,this%jm+jbm
       vxk(:)=VX(:,n,j)
     V_out(:,i  ,j) = V_out(:,i  ,j)+vxk(:)*c0
     V_out(:,i+1,j) = V_out(:,i+1,j)+vxk(:)*c1
   enddo
   enddo
!-----------------------------------------------------------------------
endsubroutine lin_adjoint_offset

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine l_vertical_adjoint_spec2 &
!***********************************************************************
!                                                                      !
!  Adjoint of linear interpolations in vertical                        !
!  from reslution nm to resolution km                                  !
!                                                                      !
!              (  nm = 2*km-1 )                                        !
!                                                                      !
!***********************************************************************
(this,en,nm_in,km_in,imin,imax,jmin,jmax,W,F)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: en,nm_in,km_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:nm_in*en,imin:imax,jmin:jmax), intent(in):: W
real(r_kind), dimension(1:km_in*en,imin:imax,jmin:jmax), intent(out):: F
integer(i_kind):: k,n,e,enm,ekm
!-----------------------------------------------------------------------
  F = 0.

do e=0,en-1
  enm = e*nm_in
  ekm = e*km_in
      k=1
  do n=2,nm_in-1,2
    F(ekm+k  ,:,:) = F(ekm+k  ,:,:)+0.5*W(enm+n,:,:)
    F(ekm+k+1,:,:) = F(ekm+k+1,:,:)+0.5*W(enm+n,:,:)
      k=k+1
  enddo

      k=1
  do n=1,nm_in,2
    F(ekm+k,:,:) = F(ekm+k,:,:) +  W(enm+n,:,:)
      k=k+1
  enddo
enddo
!-----------------------------------------------------------------------
endsubroutine l_vertical_adjoint_spec2

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine l_vertical_direct_spec2 &
!***********************************************************************
!                                                                      !
!                                                                      !
!  Direct linear interpolations in vertical                            !
!  from reslution nm to resolution km                                  !
!                                                                     !
!              (  nmax = 2*kmax-1 )                                    !
!                                                                      !
!***********************************************************************
(this,en,km_in,nm_in,imin,imax,jmin,jmax,F,W)
implicit none
!-----------------------------------------------------------------------
class(mg_intstate_type),target::this
integer(i_kind), intent(in):: en,km_in,nm_in,imin,imax,jmin,jmax
real(r_kind), dimension(1:km_in*en,imin:imax,jmin:jmax), intent(in):: F
real(r_kind), dimension(1:nm_in*en,imin:imax,jmin:jmax), intent(out):: W
integer(i_kind):: k,n,e,enm,ekm
!-----------------------------------------------------------------------
do e=0,en-1
  enm = e*nm_in
  ekm = e*km_in
      k=1
  do n=1,nm_in,2
    W(enm+n,:,:) =F (ekm+k,:,:)
      k=k+1
  enddo
      k=1
  do n=2,nm_in-1,2
    W(enm+n,:,:) = 0.5*(F(ekm+k,:,:)+F(ekm+k+1,:,:))
      k=k+1
  enddo
enddo
!-----------------------------------------------------------------------
endsubroutine l_vertical_direct_spec2

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_interpolate
