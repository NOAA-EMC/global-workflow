submodule(mg_parameter) jp_pbfil
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   jp_pbfil
!   prgmmr: purser           org: NOAA/EMC            date: 2019-03
!
! abstract:  Codes for the beta filters
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   cholaspect1 -
!   cholaspect2 -
!   cholaspect3 -
!   cholaspect4 -
!   getlinesum1 -
!   getlinesum2 -
!   getlinesum3 -
!   getlinesum4 -
!   rbeta1 -
!   rbeta2 -
!   rbeta3 -
!   rbeta4 -
!   vrbeta4 -
!   rbeta1T -
!   rbeta2T -
!   rbeta3T -
!   rbeta4T -
!   vrbeta4t -
!   vrbeta1 -
!   vrbeta2 -
!   vrbeta3 -
!   vrbeta1T -
!   vrbeta2T -
!   vrbeta3T -
!
! Functions Included:
!
! remarks:
!   The filters invoke the aspect tensor information encoded by the 
!   Cholesky lower-triangular factors, el, of the INVERSE aspect tensors.
!   The routines, "cholaspect", convert (in place) the field of given
!   aspect tensors A to the equivalent cholesky factors of A^(-1).
!   The routines, "getlinesum" precompute the normalization coefficients
!   for each line (row) of the implied matrix form of the beta filter
!   so that the normalized line sum associated with each point of
!   application becomes unity.
!   This makes the application of each filter significantly faster
!   than having to work out the normalization on the fly.
!   Be sure to have run cholaspect, and then getlinesum, prior to applying
!   the beta filters themselves.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi
use kinds, only: dp=>r_kind
use jp_pietc, only: u1
implicit none

contains

!=============================================================================
module subroutine cholaspect1(lx,mx, el)                        ! [cholaspect]
!=============================================================================
! Convert the given field, el, of aspect tensors into the equivalent
! field
! of Cholesky lower-triangular factors of the inverses of the aspect
! tensors.
!=============================================================================
use jp_pmat, only: inv, l1lm
integer,                  intent(in   ):: lx,mx
real(dp),dimension(1,1,lx:mx),intent(inout):: el
!-----------------------------------------------------------------------------
integer :: ix
!=============================================================================
do ix=lx,mx; el(1,1,ix)=u1/sqrt(el(1,1,ix)); enddo
end subroutine cholaspect1
!=============================================================================
module subroutine cholaspect2(lx,mx, ly,my, el)                 ! [cholaspect]
!=============================================================================
! Convert the given field, el, of aspect tensors into the equivalent
! field
! of Cholesky lower-triangular factors of the inverses of the aspect
! tensors.
!=============================================================================
use jp_pmat, only: inv, l1lm
integer,                            intent(in   ):: lx,mx, ly,my
real(dp),dimension(2,2,lx:mx,ly:my),intent(inout):: el
!-----------------------------------------------------------------------------
real(dp),dimension(2,2):: tel
integer                :: ix,iy
!=============================================================================
do iy=ly,my; do ix=lx,mx
   tel=el(:,:,ix,iy); call inv(tel); call l1lm(tel,el(:,:,ix,iy))
enddo;       enddo
end subroutine cholaspect2
!=============================================================================
module subroutine cholaspect3(lx,mx, ly,my, lz,mz, el)          ! [cholaspect]
!=============================================================================
! Convert the given field, el, of aspect tensors into the equivalent
! field
! of Cholesky lower-triangular factors of the inverses of the aspect
! tensors.
!=============================================================================
use jp_pmat, only: inv, l1lm
integer,                                  intent(in   ):: lx,mx, ly,my, lz,mz
real(dp),dimension(3,3,lx:mx,ly:my,lz:mz),intent(inout):: el
!-----------------------------------------------------------------------------
real(dp),dimension(3,3):: tel
integer                :: ix,iy,iz
!=============================================================================
do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   tel=el(:,:,ix,iy,iz); call inv(tel); call l1lm(tel,el(:,:,ix,iy,iz))
enddo;       enddo;       enddo
end subroutine cholaspect3
!=============================================================================
module subroutine cholaspect4(lx,mx, ly,my, lz,mz, lw,mw,el)    ! [cholaspect]
!=============================================================================
! Convert the given field, el, of aspect tensors into the equivalent
! field
! of Cholesky lower-triangular factors of the inverses of the aspect
! tensors.
!=============================================================================
use jp_pmat, only: inv, l1lm
integer,                         intent(in   ):: lx,mx, ly,my, lz,mz, lw,mw
real(dp),dimension(4,4,lx:mx,ly:my,lz:mz,lw:mw),&
                                 intent(inout):: el
!-----------------------------------------------------------------------------
real(dp),dimension(4,4):: tel
integer                :: ix,iy,iz,iw
!=============================================================================
do iw=lw,mw; do iz=lz,mz; do iy=ly,my; do ix=lx,mx
   tel=el(:,:,ix,iy,iz,iw); call inv(tel); call l1lm(tel,el(:,:,ix,iy,iz,iw))
enddo;       enddo;       enddo;       enddo
end subroutine cholaspect4

!=============================================================================
module subroutine getlinesum1(this,hx,lx,mx, el, ss)            ! [getlinesum]
!=============================================================================
! Get inverse of the line-sum of the matrix representing the
! unnormalized
! beta function with aspect tensor pasp=(el*el^T)^(-1), and invert the
! result 
! so it can be used subsequently in the normalized version of this
! filter.
!=============================================================================
class(mg_parameter_type)::this
integer,                  intent(in   ):: hx,Lx,mx
real(dp),dimension(1,1,Lx:Mx),intent(in   ):: el
real(dp),dimension(lx:mx),intent(  out):: ss
!-----------------------------------------------------------------------------
real(dp),parameter:: eps=1.e-12
real(dp)          :: s,rr,rrc,exx,x
integer           :: ix,gxl,gxm,gx
!=============================================================================
do ix=Lx,Mx
   s=0
   exx=el(1,1,ix)*this%rmom2_1
   x=u1/exx
   gxl=ceiling(-x+eps); gxm=floor( x-eps)
   if(gxl<-hx.or.gxm>hx)&
        stop 'In getlinesum1; filter reach fx becomes too large for hx'
   do gx=gxl,gxm
      x=gx
      rr=(x*exx)**2; rrc=u1-rr
      s=s+rrc**this%p
   enddo
   ss(ix)=u1/s
enddo
end subroutine getlinesum1
!=============================================================================
module subroutine getlinesum2(this,hx,lx,mx, hy,ly,my, el, ss)  ! [getlinesum]
!=============================================================================
class(mg_parameter_type)::this
integer,                            intent(in   ):: hx,Lx,mx, &
                                                    hy,ly,my
real(dp),dimension(2,2,Lx:Mx,Ly:My),intent(in   ):: el
real(dp),dimension(    lx:mx,ly:my),intent(  out):: ss
!-----------------------------------------------------------------------------
real(dp),parameter     :: eps=1.e-12
real(dp),dimension(2,2):: tel
real(dp)               :: s,rr,rrx,rrc,exx,eyy,eyx,x,y,xc
integer                :: ix,gx,gxl,gxm
integer                :: iy,gy,gyl,gym
!=============================================================================
do iy=Ly,My; do ix=Lx,Mx
   s=0
   tel=el(:,:,ix,iy)*this%rmom2_2 ! This el, rescaled
   exx=tel(1,1); eyy=tel(2,2)
   eyx=tel(2,1)
   y=u1/eyy
   gyl=ceiling(-y+eps); gym=floor( y-eps)
   if(gyl<-hy.or.gym>hy)&
        stop 'In getlinesum2; filter reach becomes too large for hy'
   do gy=gyl,gym
      y=gy; xc=-y*eyx
      rrx=(y*eyy)**2; x=sqrt(u1-rrx)
      gxl=ceiling((xc-x)/exx+eps); gxm=floor((xc+x)/exx-eps)
      if(gxl<-hx.or.gxm>hx)&
           stop 'In getlinesum2; filter reach becomes too large for hx'
      do gx=gxl,gxm
         x=gx
         rr=rrx+(x*exx-xc)**2; rrc=u1-rr
         s=s+rrc**this%p
      enddo! gx
   enddo! gy
   ss(ix,iy)=u1/s
enddo;  enddo! ix, iy
end subroutine getlinesum2
!=============================================================================
module subroutine getlinesum3(this,hx,lx,mx, hy,ly,my, hz,lz,mz, el, ss) ! [getlinesum]
!=============================================================================
class(mg_parameter_type)::this
integer,                                  intent(in   ):: hx,Lx,mx, &
                                                          hy,ly,my, &
                                                          hz,lz,mz
real(dp),dimension(3,3,Lx:Mx,Ly:My,Lz:Mz),intent(in   ):: el
real(dp),dimension(    lx:mx,ly:my,lz:mz),intent(  out):: ss
!-----------------------------------------------------------------------------
real(dp),parameter     :: eps=1.e-12
real(dp),dimension(3,3):: tel
real(dp)               :: s,rr,rrx,rry,rrc,&
                          exx,eyy,ezz,eyx,ezx,ezy, x,y,z,xc,yc
integer                :: ix,gx,gxl,gxm
integer                :: iy,gy,gyl,gym
integer                :: iz,gz,gzl,gzm
!=============================================================================
ss=0
do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   s=0
   tel=el(:,:,ix,iy,iz)*this%rmom2_3
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3)
   eyx=tel(2,1); ezx=tel(3,1)
   ezy=tel(3,2)
   z=u1/ezz
   gzl=ceiling(-z+eps); gzm=floor( z-eps)
   if(gzl<-hz.or.gzm>hz)&
        stop 'In getlinesum3; filter reach becomes too large for hz'
   do gz=gzl,gzm
      z=gz;           yc=-z*ezy
      rry=(z*ezz)**2; y =sqrt(u1-rry)
      gyl=ceiling((yc-y)/eyy+eps); gym=floor((yc+y)/eyy-eps)
      if(gyl<-hy.or.gym>hy)&
           stop 'In getlinesum3; filter reach becomes too large for hy'
      do gy=gyl,gym
         y=gy;                  xc=-y*eyx-z*ezx
         rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
         gxl=ceiling((xc-x)/exx+eps); gxm=floor((xc+x)/exx-eps)
         if(gxl<-hx.or.gxm>hx)&
              stop 'In getlinesum3; filter reach becomes too large for hx'
         do gx=gxl,gxm
            x=gx
            rr=rrx+(x*exx-xc)**2; rrc=u1-rr
            s=s+rrc**this%p
         enddo! gx
      enddo! gy
   enddo! gz
   ss(ix,iy,iz)=u1/s
enddo; enddo; enddo! ix, iy, iz
end subroutine getlinesum3
!=============================================================================
module subroutine getlinesum4(this,hx,lx,mx, hy,ly,my, hz,lz,mz, hw,lw,mw, &
     el, ss)                                                    ! [getlinesum]
!=============================================================================
class(mg_parameter_type)::this
integer,                                  intent(in   ):: hx,Lx,mx, &
                                                          hy,ly,my, &
                                                          hz,lz,mz, &
                                                          hw,lw,mw
real(dp),dimension(4,4,Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: el
real(dp),dimension(    lx:mx,ly:my,lz:mz,Lw:Mw),intent(  out):: ss
!-----------------------------------------------------------------------------
real(dp),parameter     :: eps=1.e-12
real(dp),dimension(4,4):: tel
real(dp)               :: s,rr,rrx,rry,rrz,rrc, &
                          exx,eyy,ezz,eww,eyx,ezx,ewx,ezy,ewy,ewz, x,y,z,w,&
                          xc,yc,zc
integer                :: ix,gx,gxl,gxm
integer                :: iy,gy,gyl,gym
integer                :: iz,gz,gzl,gzm
integer                :: iw,gw,gwl,gwm
!=============================================================================
ss=0
do iw=Lw,Mw; do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   s=0
   tel=el(:,:,ix,iy,iz,iw)*this%rmom2_4
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3); eww=tel(4,4)
   eyx=tel(2,1); ezx=tel(3,1); ewx=tel(4,1)
   ezy=tel(3,2); ewy=tel(4,2)
   ewz=tel(4,3)
   w=u1/eww
   gwl=ceiling(-w+eps); gwm=floor( w-eps)
   if(gwl<-hw.or.gwm>hw)&
        stop 'In getlinesum4; filter reach becomes too large for hw'
   do gw=gwl,gwm
      w=gw;           zc=-w*ewz
      rrz=(w-eww)**2; z =sqrt(u1-rrz)
      gzl=ceiling((zc-z)/ezz+eps); gzm=floor((zc+z)/ezz-eps)
      if(gzl<-hz.or.gzm>hz)&
           stop 'In getlinesum4; filter reach becomes too large for hz'
      do gz=gzl,gzm
         z=gz;                  yc=-z*ezy-w*ewy
         rry=rrz+(z*ezz-zc)**2; y =sqrt(u1-rry)
         gyl=ceiling((yc-y)/eyy+eps); gym=floor((yc+y)/eyy-eps)
         if(gyl<-hy.or.gym>hy)&
              stop 'In getlinesum4; filter reach becomes too large for hy'
         do gy=gyl,gym
            y=gy;                  xc=-y*eyx-z*ezx-w*ewx
            rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
            gxl=ceiling((xc-x)/exx+eps); gxm=floor((xc+x)/exx-eps)
            if(gxl<-hx.or.gxm>hx)&
                 stop 'In getlinesum4; filter reach becomes too large for hx'
            do gx=gxl,gxm
               x=gx
               rr=rrx+(x*exx-xc)**2; rrc=u1-rr
               s=s+rrc**this%p
            enddo! gx
         enddo! gy
      enddo! gz
   enddo! gw
   ss(ix,iy,iz,iw)=u1/s
enddo;  enddo;  enddo;  enddo! ix, iy, iz, iw
end subroutine getlinesum4

!=============================================================================
module subroutine rbeta1(this,hx,lx,mx, el,ss, a)                    ! [rbeta]
!=============================================================================
! Perform a radial beta-function filter in 1D.
! It averages the surrounding density values, and so preserves the value
! (in its target region) when presented with a constant-density input
! field.
! The input data occupy the extended region:
! Lx-hx <= jx <= mx+hx.
! The output data occupy the central region
! Lx <= ix <= Mx.
!=============================================================================
class(mg_parameter_type)::this
integer,                        intent(in   ):: hx,Lx,mx
real(dp),dimension(   Lx:Mx),   intent(in   ):: el
real(dp),dimension(   Lx:Mx),   intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter             :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx):: b
real(dp)                       :: x,tb,s,rr,rrc,frow,exx
integer                        :: ix,jx,gx
!=============================================================================
b=0
do ix=Lx,Mx
   tb=0; s=ss(ix)
   exx=el(ix)*this%rmom2_1
   x=u1/exx
   do gx=ceiling(-x+eps),floor( x-eps)
      jx=ix+gx;      x=gx
      rr=(x*exx)**2; rrc=u1-rr
      frow=s*rrc**this%p
      tb=tb+frow*a(jx)
   enddo
   b(ix)=tb
enddo
a=b
end subroutine rbeta1
!=============================================================================
module subroutine rbeta2(this,hx,lx,mx, hy,ly,my, el,ss, a)          ! [rbeta]
!=============================================================================
! Perform a radial beta-function filter in 2D.
! It averages the surrounding density values, and so preserves the value
! (in its target region) when presented with a constant-density input
! field.
! The input data occupy the extended region:
! Lx-hx <= jx <= mx+hx, Ly-hy <= Jy <= my+hy
! The output data occupy the central region
! Lx <= ix <= Mx, Ly <= iy <= My.
!=============================================================================
class(mg_parameter_type)::this
integer,                                    intent(in   ):: hx,Lx,mx, &
                                                            hy,ly,my
real(dp),dimension(2,2,Lx:Mx,Ly:My),        intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My),        intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                         :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy):: b
real(dp),dimension(2,2)                    :: tel
real(dp)                                   :: tb,s,rr,rrx,rrc,&
                                              frow,exx,eyy,eyx,x,y,xc
integer                                    :: ix,jx,gx
integer                                    :: iy,jy,gy
!=============================================================================
b=0
do iy=Ly,My; do ix=Lx,Mx
   tb=0; s=ss(ix,iy)
   tel=el(:,:,ix,iy)*this%rmom2_2 ! This el, rescaled
   exx=tel(1,1); eyy=tel(2,2)
   eyx=tel(2,1)
   y=u1/eyy
   do gy=ceiling(-y+eps),floor( y-eps)
      jy=iy+gy;       y=gy; xc=-y*eyx
      rrx=(y*eyy)**2;       x =sqrt(u1-rrx)
      do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
         jx=ix+gx; x=gx
         rr=rrx+(x*exx-xc)**2; rrc=u1-rr
         frow=s*rrc**this%p
         tb=tb+frow*a(jx,jy)
      enddo! gx
   enddo! gy
   b(ix,iy)=tb
enddo; enddo! ix, iy
a=b
end subroutine rbeta2
!=============================================================================
module subroutine rbeta3(this,hx,lx,mx, hy,ly,my, hz,lz,mz, el,ss,a) ! [rbeta]
!=============================================================================
! Perform a radial beta-function filter in 3D.
! It averages the surrounding density values, and so preserves the value
! (in its target region) when presented with a constant-density input
! field.
! The input data occupy the extended region:
! Lx-hx <= jx <= mx+hx, Ly-hy <= Jy <= my+hy, Lz-hz <= Jz <= mz+hz
! The output data occupy the central region
! Lx <= ix <= Mx, Ly <= iy <= My, Lz <= iz <= Mz.
!=============================================================================
class(mg_parameter_type)::this
integer,                                   intent(in   ):: hx,Lx,mx,&
                                                           hy,ly,my,&
                                                           hz,lz,mz
real(dp),dimension(3,3,Lx:Mx,Ly:My,Lz:Mz), intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz), intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,&
                              lz-hz:mz+hz),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                                     :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,lz-hz:mz+hz):: b
real(dp),dimension(3,3)                                :: tel
real(dp):: s,tb,rr,rrx,rry,rrc,frow,&
           exx,eyy,ezz,eyx,ezx,ezy,x,y,z,xc,yc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
!=============================================================================
b=0
do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   tb=0; s=ss(ix,iy,iz)
   tel=el(:,:,ix,iy,iz)*this%rmom2_3
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3)
   eyx=tel(2,1); ezx=tel(3,1); ezy=tel(3,2)
   z=u1/ezz
   do gz=ceiling(-z+eps),floor( z-eps)
      jz=iz+gz; z=gz; yc=-z*ezy
      rry=(z*ezz)**2; y =sqrt(u1-rry)
      do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
         jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx
         rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
         do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
            jx=ix+gx; x=gx
            rr=rrx+(x*exx-xc)**2; rrc=u1-rr
            frow=s*rrc**this%p
            tb=tb+frow*a(jx,jy,jz)
         enddo! gx
      enddo! gy
   enddo! gz
   b(ix,iy,iz)=tb
enddo;   enddo;    enddo! ix, iy, iz
a=b
end subroutine rbeta3
!=============================================================================
module subroutine rbeta4(this,hx,lx,mx, hy,ly,my, hz,lz,mz, hw,lw,mw, el,ss,a) ! [rbeta]
!=============================================================================
! Perform a radial beta-function filter in 4D.
! It averages the surrounding density values, and so preserves the value
! (in its target region) when presented with a constant-density input
! field.
! The input data occupy the extended region:
! Lx-hx <= jx <= mx+hx, Ly-hy <= Jy <= my+hy, Lz-hz <= Jz <= mz+hz, 
! Lw-hw <= Jw <= mw+hw
! The output data occupy the central region
! Lx <= ix <= Mx, Ly <= iy <= My, Lz <= iz <= Mz, Lw <= iw <= Mw.
!=============================================================================
class(mg_parameter_type)::this
integer,                                        intent(in   ):: hx,Lx,mx,&
                                                                hy,ly,my,&
                                                                hz,lz,mz,&
                                                                hw,lw,mw
real(dp),dimension(4,4,Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy, &
     lz-hz:mz+hz,lw-hw:mw+hw),                  intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                         :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,&
     lz-hz:mz+hz,lw-hw:mw+hw)              :: b
real(dp),dimension(4,4)                    :: tel
real(dp):: s,tb,rr,rrx,rry,rrz,rrc,frow,&
           exx,eyy,ezz,eww,eyx,ezx,ewx,ezy,ewy,ewz,x,y,z,w,xc,yc,zc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
integer :: iw,jw,gw
!=============================================================================
b=0
do iw=lw,mw; do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   tb=0; s=ss(ix,iy,iz,iw)
   tel=el(:,:,ix,iy,iz,iw)*this%rmom2_4
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3); eww=tel(4,4)
   eyx=tel(2,1); ezx=tel(3,1); ewx=tel(4,1)
   ezy=tel(3,2); ewy=tel(4,2)
   ewz=tel(4,3)
   w=u1/eww
   do gw=ceiling(-w+eps),floor( w-eps)
      jw=iw+gw; w=gw; zc=-w*ewz
      rrz=(w*eww)**2; z =sqrt(u1-rrz)
      do gz=ceiling((zc-z)/ezz+eps),floor((zc+z)/ezz-eps)
         jz=iz+gz; z=gz;        yc=-z*ezy-w*ewy
         rry=rrz+(z*ezz-zc)**2; y =sqrt(u1-rry)
         do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
            jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx-w*ewx
            rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
            do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
               jx=ix+gx; x=gx
               rr=rrx+(x*exx-xc)**2; rrc=u1-rr
               frow=s*rrc**this%p
               tb=tb+frow*a(jx,jy,jz,jw)
            enddo! gx
         enddo! gy
      enddo! gz
   enddo! gw
   b(ix,iy,iz,iw)=tb
enddo;   enddo;   enddo;   enddo! ix, iy, iz, iw
a=b
end subroutine rbeta4

!=============================================================================
! Vector versions of the above routines:
!=============================================================================
module subroutine vrbeta4(this,nv,hx,lx,mx, hy,ly,my, hz,lz,mz, hw,lw,mw, &
     el,ss,a)                                                        ! [rbeta]
!=============================================================================
! Vector version of rbeta4 filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                                       intent(in   ):: nv, &
                                                               hx,Lx,mx,&
                                                               hy,ly,my,&
                                                               hz,lz,mz,&
                                                               hw,lw,mw
real(dp),dimension(4,4,Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy, &
     lz-hz:mz+hz,lw-hw:mw+hw),                  intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                                     :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,&
     lz-hz:mz+hz,lw-hw:mw+hw)                          :: b
real(dp),dimension(nv)                                 :: tb
real(dp),dimension(4,4)                                :: tel
real(dp):: s,rr,rrx,rry,rrz,rrc,frow,&
           exx,eyy,ezz,eww, eyx,ezx,ewx, ezy,ewy, ewz,&
           x,y,z,w,xc,yc,zc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
integer :: iw,jw,gw
!=============================================================================
b=0
do iw=lw,mw; do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   tb=0; s=ss(ix,iy,iz,iw)
   tel=el(:,:,ix,iy,iz,iw)*this%rmom2_4
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3); eww=tel(4,4)
   eyx=tel(2,1); ezx=tel(3,1); ewx=tel(4,1)
   ezy=tel(3,2); ewy=tel(4,2)
   ewz=tel(4,3)
   w=u1/eww
   do gw=ceiling(-w+eps),floor( w-eps)
      jw=iw+gw; w=gw; zc=-w*ewz
      rrz=(w*eww)**2; z =sqrt(u1-rrz)
      do gz=ceiling((zc-z)/ezz+eps),floor((zc+z)/ezz-eps)
         jz=iz+gz; z=gz;        yc=-z*ezy-w*ewy
         rry=rrz+(z*ezz-zc)**2; y =sqrt(u1-rry)
         do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
            jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx-w*ewx
            rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
            do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
               jx=ix+gx; x=gx
               rr=rrx+(x*exx-xc)**2; rrc=u1-rr
               frow=s*rrc**this%p
               tb=tb+frow*a(:,jx,jy,jz,jw)
            enddo! gx
         enddo! gy
      enddo! gz
   enddo! gw
   b(:,ix,iy,iz,iw)=tb
enddo;  enddo;  enddo;  enddo! ix, iy, iz, iw
a=b
end subroutine vrbeta4

!=============================================================================
module subroutine rbeta1T(this,hx,lx,mx, el,ss, a)                  ! [rbetat]
!=============================================================================
! Perform an ADJOINT radial beta-function filter in 1D.
! It conserves "masses" initially distributed only at the closure of 
! the central domain, 
! Lx <= ix <= Mx.
! The output field of the redistributed masses occupies the
! the extended domain, 
! Lx-hx <= jx <= mx+hx.
!=============================================================================
class(mg_parameter_type)::this
integer,                        intent(in   ):: hx,Lx,mx
real(dp),dimension(1,1,Lx:Mx),  intent(in   ):: el
real(dp),dimension(  Lx:Mx),    intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter             :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx):: b
real(dp)                       :: ta,s,rr,rrc,frow,exx,x
integer                        :: ix,jx,gx
!=============================================================================
b=0
do ix=Lx,Mx
   ta=a(ix); s=ss(ix)
   exx=el(1,1,ix)*this%rmom2_1
   x=u1/exx
   do gx=ceiling(-x+eps),floor( x-eps)
      jx=ix+gx;      x=gx
      rr=(x*exx)**2; rrc=u1-rr
      frow=s*rrc**this%p
      b(jx)=b(jx)+frow*ta
   enddo
enddo
a=b
end subroutine rbeta1t
!=============================================================================
module subroutine rbeta2T(this,hx,lx,mx, hy,ly,my, el,ss, a)        ! [rbetat]
!=============================================================================
! Perform an ADJOINT radial beta-function filter in 2D.
! It conserved "masses" initially distributed only at the closure of 
! the central domain, 
! Lx <= ix <= Mx, Ly <= iy <= My.
! The output field of the redistributed masses occupies the
! the extended domain, 
! Lx-hx <= jx <= mx+hx, Ly-hy <= Jy <= my+hy
!=============================================================================
class(mg_parameter_type)::this
integer,                                    intent(in   ):: hx,Lx,mx, &
                                                            hy,ly,my
real(dp),dimension(2,2,Lx:Mx,Ly:My),        intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My),        intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                         :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy):: b
real(dp),dimension(2,2)                    :: tel
real(dp)                                   :: ta,s,rr,rrx,rrc, &
                                              frow,exx,eyy,eyx,x,y,xc
integer                                    :: ix,jx,gx
integer                                    :: iy,jy,gy
!=============================================================================
b=0
do iy=Ly,My; do ix=Lx,Mx
   ta=a(ix,iy); s=ss(ix,iy)
   tel=el(:,:,ix,iy)*this%rmom2_2 ! sThis el, rescaled
   exx=tel(1,1); eyy=tel(2,2)
   eyx=tel(2,1)
   y=u1/eyy
   do gy=ceiling(-y+eps),floor( y-eps)
      jy=iy+gy; y=gy; xc=-y*eyx
      rrx=(y*eyy)**2; x =sqrt(u1-rrx)
      do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
         jx=ix+gx; x=gx
         rr=rrx+(x*exx-xc)**2; rrc=u1-rr
         frow=s*rrc**this%p
         b(jx,jy)=b(jx,jy)+frow*ta
      enddo! gx 
   enddo! gy
enddo;  enddo! ix, iy
a=b
end subroutine rbeta2t
!=============================================================================
module subroutine rbeta3T(this,hx,lx,mx, hy,ly,my, hz,lz,mz, el,ss, a) ! [rbetat]
!=============================================================================
! Perform an ADJOINT radial beta-function filter in 3D.
! It conserves "masses" initially distributed only at the closure of 
! the central domain, 
! Lx <= ix <= Mx, Ly <= iy <= My, Lz <= iz <= Mz.
! The output field of the redistributed masses occupies the
! the extended domain, 
! Lx-hx <= jx <= Mx+hx, Ly-hy <= Jy <= My+hy, Lz-hz <= Jz <= Mz+hz.
!=============================================================================
class(mg_parameter_type)::this
integer,                                    intent(in   ):: hx,Lx,mx,&
                                                            hy,ly,my,&
                                                            hz,lz,mz
real(dp),dimension(3,3,Lx:Mx,Ly:My,Lz:Mz),  intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz),  intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,&
                               lz-hz:mz+hz),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                                     :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,lz-hz:mz+hz):: b
real(dp),dimension(3,3)                                :: tel
real(dp):: ta,s,rr,rrx,rry,rrc,frow,&
           exx,eyy,ezz,eyx,ezx,ezy,x,y,z,xc,yc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
!=============================================================================
b=0
do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   ta=a(ix,iy,iz); s=ss(ix,iy,iz)
   tel=el(:,:,ix,iy,iz)*this%rmom2_3
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3)
   eyx=tel(2,1); ezx=tel(3,1); ezy=tel(3,2)
   z=u1/ezz
   do gz=ceiling(-z+eps),floor( z-eps)
      jz=iz+gz; z=gz; yc=-z*ezy
      rry=(z*ezz)**2; y =sqrt(u1-rry)
      do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
         jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx
         rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
         do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
            jx=ix+gx; x=gx
            rr=rrx+(x*exx-xc)**2; rrc=u1-rr
            frow=s*rrc**this%p
            b(jx,jy,jz)=b(jx,jy,jz)+frow*ta
         enddo! gx
      enddo! gy
   enddo ! gz
enddo;  enddo;  enddo ! ix, iy, iz
a=b
end subroutine rbeta3t
!=============================================================================
module subroutine rbeta4T(this,hx,lx,mx, hy,ly,my, hz,lz,mz, hw,lw,mw, &
     el,ss, a)                                                      ! [rbetat]
!=============================================================================
! Perform an ADJOINT radial beta-function filter in 4D.
! It conserves "masses" initially distributed only at the closure of 
! the central domain, 
! Lx <= ix <= Mx, Ly <= iy <= My, Lz <= iz <= Mz, Lw <= iw <= Mw.
! The output field of the redistributed masses occupies the
! the extended domain, 
! Lx-hx <= jx <= Mx+hx, Ly-hy <= Jy <= My+hy, Lz-hz <= Jz <= Mz+hz, 
!     Lw-hw <= Jw <= Mw+hw.
!=============================================================================
class(mg_parameter_type)::this
integer,                                        intent(in   ):: hx,Lx,mx,&
                                                                hy,ly,my,&
                                                                hz,lz,mz,&
                                                                hw,lw,mw
real(dp),dimension(4,4,Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: ss
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,&
                   lz-hz:mz+hz,lw-hw:mw+hw),    intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                         :: eps=1.e-12
real(dp),dimension(lx-hx:mx+hx,ly-hy:my+hy,&
     lz-hz:mz+hz,lw-hw:mw+hw)              :: b
real(dp),dimension(4,4)                    :: tel
real(dp):: ta,s,rr,rrx,rry,rrz,rrc,frow,&
           exx,eyy,ezz,eww,eyx,ezx,ewx,ezy,ewy,ewz,x,y,z,w,xc,yc,zc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
integer :: iw,jw,gw
!=============================================================================
b=0
do iw=Lw,Mw; do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   ta=a(ix,iy,iz,iw); s=ss(ix,iy,iz,iw)
   tel=el(:,:,ix,iy,iz,iw)*this%rmom2_4
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3); eww=tel(4,4)
   eyx=tel(2,1); ezx=tel(3,1); ewx=tel(4,1)
   ezy=tel(3,2); ewy=tel(4,2)
   ewz=tel(4,3)
   z=u1/ezz
   do gw=ceiling(-w+eps),floor( w-eps)
      jw=iw+gw; w=gw; zc=-w*ewz
      rrz=(w*eww)**2; z =sqrt(u1-rrz)
      do gz=ceiling((zc-z)/ezz+eps),floor((zc+z)/ezz-eps)
         jz=iz+gz; z=gz;        yc=-z*ezy-w*ewy
         rry=rrz+(z*ezz-zc)**2; y =sqrt(u1-rry)
         do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
            jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx-w*ewx
            rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
            do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
               jx=ix+gx; x=gx
               rr=rrx+(x*exx-xc)**2; rrc=u1-rr
               frow=s*rrc**this%p
               b(jx,jy,jz,jw)=b(jx,jy,jz,jw)+frow*ta
            enddo! gx
         enddo! gy
      enddo! gz
   enddo! gw
enddo;  enddo;  enddo;  enddo! ix, iy, iz, iw
a=b
end subroutine rbeta4t


!=============================================================================
module subroutine vrbeta4t(this,nv,hx,lx,mx, hy,ly,my, hz,lz,mz, &
                                              hw,lw,mw, el,ss, a)   ! [rbetat]
!=============================================================================
! Vector version of rbeta4t filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                                        intent(in   ):: nv, &
                                                                hx,Lx,mx,&
                                                                hy,ly,my,&
                                                                hz,lz,mz,&
                                                                hw,lw,mw
real(dp),dimension(4,4,Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz,Lw:Mw),intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,&
     lz-hz:mz+hz,lw-hw:mw+hw),                  intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                         :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,&
     lz-hz:mz+hz,lw-hw:mw+hw)              :: b
real(dp),dimension(nv)                     :: ta
real(dp),dimension(4,4)                    :: tel
real(dp):: s,rr,rrx,rry,rrz,rrc,frow,&
           exx,eyy,ezz,eww,eyx,ezx,ewx,ezy,ewy,ewz,x,y,z,w,xc,yc,zc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
integer :: iw,jw,gw
!=============================================================================
b=0
do iw=Lw,Mw; do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   ta=a(:,ix,iy,iz,iw); s=ss(ix,iy,iz,iw)
   tel=el(:,:,ix,iy,iz,iw)*this%rmom2_4
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3); eww=tel(4,4)
   eyx=tel(2,1); ezx=tel(3,1); ewx=tel(4,1)
   ezy=tel(3,2); ewy=tel(4,2)
   ewz=tel(4,3)
   z=u1/ezz
   do gw=ceiling(-w+eps),floor( w-eps)
      jw=iw+gw; w=gw; zc=-w*ewz
      rrz=(w*eww)**2; z =sqrt(u1-rrz)
      do gz=ceiling((zc-z)/ezz+eps),floor((zc+z)/ezz-eps)
         jz=iz+gz; z=gz;        yc=-z*ezy-w*ewy
         rry=rrz+(z*ezz-zc)**2; y =sqrt(u1-rry)
         do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
            jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx-w*ewx
            rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
            do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
               jx=ix+gx; x=gx
               rr=rrx+(x*exx-xc)**2; rrc=u1-rr
               frow=s*rrc**this%p
               b(:,jx,jy,jz,jw)=b(:,jx,jy,jz,jw)+frow*ta
            enddo! gx
         enddo! gy
      enddo! gz
   enddo! gw
enddo; enddo; enddo; enddo! ix, iy, iz, iw
a=b
end subroutine vrbeta4t

! Vector versions of the above routines:
!=============================================================================
module subroutine vrbeta1(this,nv,hx,lx,mx, el,ss, a)                ! [rbeta]
!=============================================================================
! Vector version of rbeta1 filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                           intent(in   ):: nv,hx,Lx,mx
real(dp),dimension(1,1, Lx:Mx),    intent(in   ):: el
real(dp),dimension(   Lx:Mx),      intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx):: b
real(dp),dimension(nv)            :: tb
real(dp)                          :: x,s,rr,rrc,frow,exx
integer                           :: ix,jx,gx
!=============================================================================
b=0
do ix=Lx,Mx
   tb=0; s=ss(ix)
   exx=el(1,1,ix)*this%rmom2_1
   x=u1/exx
   do gx=ceiling(-x+eps),floor( x-eps)
      jx=ix+gx;      x=gx
      rr=(x*exx)**2; rrc=u1-rr
      frow=s*rrc**this%p
      tb=tb+frow*a(:,jx)
   enddo
   b(:,ix)=tb
enddo
a=b
end subroutine vrbeta1

!=============================================================================
module subroutine vrbeta2(this,nv,hx,lx,mx, hy,ly,my, el,ss, a)      ! [rbeta]
!=============================================================================
! Vector version of rbeta2 filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                                       intent(in   ):: nv, &
                                                               hx,Lx,mx, &
                                                               hy,ly,my
real(dp),dimension(  2,2,Lx:Mx,Ly:My),         intent(in   ):: el
real(dp),dimension(      Lx:Mx,Ly:My),         intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                            :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy):: b
real(dp),dimension(nv)                        :: tb
real(dp),dimension(2,2)                    :: tel
real(dp)                                   :: s,rr,rrx,rrc,&
                                              frow,exx,eyy,eyx,x,y,xc
integer                                    :: ix,jx,gx
integer                                    :: iy,jy,gy
!=============================================================================
b=0
do iy=Ly,My; do ix=Lx,Mx
   tb=0; s=ss(ix,iy)
   tel=el(:,:,ix,iy)*this%rmom2_2 ! This el, rescaled
   exx=tel(1,1); eyy=tel(2,2)
   eyx=tel(2,1)
   y=u1/eyy
   do gy=ceiling(-y+eps),floor( y-eps)
      jy=iy+gy;       y=gy; xc=-y*eyx
      rrx=(y*eyy)**2;       x =sqrt(u1-rrx)
      do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
         jx=ix+gx; x=gx
         rr=rrx+(x*exx-xc)**2; rrc=u1-rr
         frow=s*rrc**this%p
         tb=tb+frow*a(:,jx,jy)
      enddo! gx
   enddo! gy
   b(:,ix,iy)=tb
enddo;   enddo! ix, iy
a=b
end subroutine vrbeta2

!=============================================================================
module subroutine vrbeta3(this,nv, hx,lx,mx, hy,ly,my, hz,lz,mz, el,ss,a) ! [rbeta]
!=============================================================================
! Vector version of rbeta3 filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                                       intent(in   ):: nv, &
                                                               hx,Lx,mx,&
                                                               hy,ly,my,&
                                                               hz,lz,mz
real(dp),dimension(3,3,Lx:Mx,Ly:My,Lz:Mz),     intent(in   ):: el
real(dp),dimension(    Lx:Mx,Ly:My,Lz:Mz),     intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,&
                                  lz-hz:mz+hz),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                                        :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,lz-hz:mz+hz):: b
real(dp),dimension(nv)                                    :: tb
real(dp),dimension(3,3)                                   :: tel
real(dp):: s,rr,rrx,rry,rrc,frow,&
           exx,eyy,ezz,eyx,ezx,ezy,x,y,z,xc,yc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
!=============================================================================
b=0
do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   tb=0; s=ss(ix,iy,iz)
   tel=el(:,:,ix,iy,iz)*this%rmom2_3
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3)
   eyx=tel(2,1); ezx=tel(3,1); ezy=tel(3,2)
   z=u1/ezz
   do gz=ceiling(-z+eps),floor( z-eps)
      jz=iz+gz; z=gz; yc=-z*ezy
      rry=(z*ezz)**2; y =sqrt(u1-rry)
      do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
         jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx
         rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
         do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
            jx=ix+gx; x=gx
            rr=rrx+(x*exx-xc)**2; rrc=u1-rr
            frow=s*rrc**this%p
            tb=tb+frow*a(:,jx,jy,jz)
         enddo! gx
      enddo! gy
   enddo! gz
   b(:,ix,iy,iz)=tb
enddo;   enddo;    enddo! ix, iy, iz
a=b
end subroutine vrbeta3

! Vector versions of the above routines:
!=============================================================================
module subroutine vrbeta1T(this,nv, hx,lx,mx, el,ss, a)             ! [rbetat]
!=============================================================================
! Vector version of rbeta1t filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                           intent(in   ):: nv,hx,Lx,mx
real(dp),dimension(1,1,Lx:Mx),     intent(in   ):: el
real(dp),dimension(   Lx:Mx),      intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx):: b
real(dp),dimension(nv)            :: ta
real(dp)                          :: s,rr,rrc,frow,exx,x
integer                           :: ix,jx,gx
!=============================================================================
b=0
do ix=Lx,Mx
   ta=a(:,ix); s=ss(ix)
   exx=el(1,1,ix)*this%rmom2_1
   x=u1/exx
   do gx=ceiling(-x+eps),floor( x-eps)
      jx=ix+gx;      x=gx
      rr=(x*exx)**2; rrc=u1-rr
      frow=s*rrc**this%p
      b(:,jx)=b(:,jx)+frow*ta
   enddo
enddo
a=b
end subroutine vrbeta1t
!=============================================================================
module subroutine vrbeta2T(this,nv,hx,lx,mx, hy,ly,my, el,ss, a)    ! [rbetat]
!=============================================================================
! Vector version of rbeta2t filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                                       intent(in   ):: nv, &
                                                               hx,Lx,mx, &
                                                               hy,ly,my
real(dp),dimension(   2,2,Lx:Mx,Ly:My),        intent(in   ):: el
real(dp),dimension(       Lx:Mx,Ly:My),        intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                            :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy):: b
real(dp),dimension(nv)                        :: ta
real(dp),dimension(2,2)                       :: tel
real(dp)                                      :: s,rr,rrx,rrc, &
                                                 frow,exx,eyy,eyx,x,y,xc
integer                                       :: ix,jx,gx
integer                                       :: iy,jy,gy
!=============================================================================
b=0
do iy=Ly,My; do ix=Lx,Mx
   ta=a(:,ix,iy); s=ss(ix,iy)
   tel=el(:,:,ix,iy)*this%rmom2_2 ! This el, rescaled
   exx=tel(1,1); eyy=tel(2,2)
   eyx=tel(2,1)
   y=u1/eyy
   do gy=ceiling(-y+eps),floor( y-eps)
      jy=iy+gy; y=gy; xc=-y*eyx
      rrx=(y*eyy)**2; x =sqrt(u1-rrx)
      do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
         jx=ix+gx; x=gx
         rr=rrx+(x*exx-xc)**2; rrc=u1-rr
         frow=s*rrc**this%p
         b(:,jx,jy)=b(:,jx,jy)+frow*ta
      enddo! gx
   enddo! gy
enddo; enddo ! ix, iy
a=b
end subroutine vrbeta2t

!=============================================================================
module subroutine vrbeta3T(this,nv,hx,lx,mx, hy,ly,my, hz,lz,mz, el,ss, a) ! [rbetat]
!=============================================================================
! Vector version of rbeta3t filtering nv fields at once.
!=============================================================================
class(mg_parameter_type)::this
integer,                                    intent(in   ):: nv,      &
                                                            hx,Lx,mx,&
                                                            hy,ly,my,&
                                                            hz,lz,mz
real(dp),dimension(   3,3,Lx:Mx,Ly:My,Lz:Mz),intent(in   ):: el
real(dp),dimension(       Lx:Mx,Ly:My,Lz:Mz),intent(in   ):: ss
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,&
                                lz-hz:mz+hz),intent(inout):: a
!-----------------------------------------------------------------------------
real(dp),parameter                         :: eps=1.e-12
real(dp),dimension(nv,lx-hx:mx+hx,ly-hy:my+hy,&
                               lz-hz:mz+hz):: b
real(dp),dimension(nv)                     :: ta
real(dp),dimension(3,3)                    :: tel
real(dp):: s,rr,rrx,rry,rrc,frow,&
           exx,eyy,ezz,eyx,ezx,ezy,x,y,z,xc,yc
integer :: ix,jx,gx
integer :: iy,jy,gy
integer :: iz,jz,gz
!=============================================================================
b=0
do iz=Lz,Mz; do iy=Ly,My; do ix=Lx,Mx
   ta=a(:,ix,iy,iz); s=ss(ix,iy,iz)
   tel=el(:,:,ix,iy,iz)*this%rmom2_3
   exx=tel(1,1); eyy=tel(2,2); ezz=tel(3,3)
   eyx=tel(2,1); ezx=tel(3,1); ezy=tel(3,2)
   z=u1/ezz
   do gz=ceiling(-z+eps),floor( z-eps)
      jz=iz+gz; z=gz; yc=-z*ezy
      rry=(z*ezz)**2; y =sqrt(u1-rry)
      do gy=ceiling((yc-y)/eyy+eps),floor((yc+y)/eyy-eps)
         jy=iy+gy; y=gy;        xc=-y*eyx-z*ezx
         rrx=rry+(y*eyy-yc)**2; x =sqrt(u1-rrx)
         do gx=ceiling((xc-x)/exx+eps),floor((xc+x)/exx-eps)
            jx=ix+gx; x=gx
            rr=rrx+(x*exx-xc)**2; rrc=u1-rr
            frow=s*rrc**this%p
            b(:,jx,jy,jz)=b(:,jx,jy,jz)+frow*ta
         enddo! gx
      enddo! gy
   enddo! gz
enddo; enddo; enddo! ix, iy, iz
a=b
end subroutine vrbeta3t

end submodule jp_pbfil

