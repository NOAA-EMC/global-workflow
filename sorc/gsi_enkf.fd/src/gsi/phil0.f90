!                               **************************************
!                               *           Module phil0             *
!                               *   R. J. Purser NOAA/NCEP/EMC 2009  *
!                               *       jim.purser@noaa.gov          *
!                               **************************************
!
! Module procedures pertaining to Hilbert curve transformations and
! representations.
!
! Direct dependencies
! Modules: kinds, pietc
!
!=============================================================================
module phil0
!=============================================================================
! Several different ways of expressing location are indicated in the names
! of the routines that perform the transformations from one representation
! to another. Single and double precsion versions are also indicated by the
! suffixes _s or _d, but mostly, only the _d versions are supported. 
!
! The location representations are indicated as follows:
! r:   Real parameter of the Hilbert curve 
! hil#: Base-# expansion of parameter r, # =4, 8, 16, or 48 for mixed base 4,8
! dig#: Base-# digits in standard expansion of cartesian location in sq or cube
! xy*: Index (optional) of sq or cube, and Cartesian location within it.
! xs:  3D unit-Cartesian vector of point on the unit sphere
! ea:  Equal-area coordinates of square map panel of cubed sphere
! gn:  Gnomonic coordinates of square map panel of cubed sphere.
!
! The PUBLIC interfaces relate only to module procedures explicitly required
! by the general user; the PRIVATE ones are internally used as intermediate
! steps.
! PUBLIC:
! r_to_hil#,   where # = 4,8,16: Encode the Hilbert parameter in base-#,
!              or, when # = 48: mixed base 4 and 8.
! hil#_to_r:   Inverse of r_to_hil# within the allowed precision.
! hil#_to_x*:  Transform from Hilbert curve code to Cartesian location in
!              unit square (#=4, x* = xy), cube (#=8, x*=xyz) or hypercube
!              (#=16, x*=xyza).
! xy*_to_hil#: Inverse of hil#_to_xy*
! hil4_to_xs:  Transform base-4 Hilbert parameter expansion to Cartesian
!              3-vector of an equal-area cubed sphere. The 0th digit of the
!              Hilbert code expansion, hil, denotes the index, from 0 to 23,
!              of the principal square of this mapping, these square being
!              grouped in triplets around each of the 8 cube-vertices. (This
!              convention was designed so that one hemisphere is filled by
!              the first half of the Hilbert curve, the other hemisphere by
!              the second half.)
! xs_to_hil4:  The inverse of hil4_to_xs
! hil48_to_xs: Like hil4_to_xs except the mixed basis, 4, then 8, allows
!              for the case of a spherical shell whose thickness, or 3rd
!              dimension is invoked once the base becomes 8 instead of 4.
!              But, in addition to unit 3-vector output, xs, there is also
!              a real scalar output variable, r, that denotes a scaled
!              radial, or vertical, coordinate variable whose resolution is
!              the same as the horizontal resolution of xs when the latter
!              is regarded as being composed of 24 deformed squares around
!              the spherical surface.
! xs_to_hil48: Inverse of hil48_to_xs.
!
! PRIVATE:
! hil#_to_dig#: Convert from hilbert base-# expansion to ordinary Cartesian
!               base-# expansion within a square (# =4), cube (# = 8) or
!               hypercube (# = 16).
! dig#_to_hil#: Exact inverse of hil#_to_dig#
! dig#_to_x*:   Base-# expansion (#=4, 8, 16) to real Cartesians within unit
!               square (x* = x), cube (x* =xyz), hypercube (x* =xyza).
! x*_to_dig#:   Inverse of dig#_to_x*.
! gn_to_ea:     Cube face's gnomonic coordinates to equal-area coordinates
! ea_to_gn:     Inverse of gn_to_ea.
!=============================================================================
use kinds, only: sp,dp,i_kind
use pietc, only: T,F,u0,u1,u2,o2,u4,o4,pi
implicit none
real(dp),parameter:: u8=8.0_dp,o8=u1/8.0_dp,u16=16.0_dp,o16=u1/16.0_dp,pio6=pi/6.0_dp
private
public  r_to_hil4,  r_to_hil8,   r_to_hil16,    r_to_hil48,                   &
        hil4_to_r,  hil8_to_r,   hil16_to_r,    hil48_to_r,                   &
        hil4_to_xy, hil8_to_xyz, hil16_to_xyza, hil48_to_xyz,                 &
        xy_to_hil4, xyz_to_hil8, xyza_to_hil16, xyz_to_hil48,                 &
        hil4_to_xs,                             hil48_to_xs,                  &
        xs_to_hil4,                             xs_to_hil48,                  &
!        hil4_to_dig4,  hil8_to_dig8,  hil16_to_dig16,                      &
!        dig4_to_hil4,  dig8_to_hil8,  dig16_to_hil16,                      &
!        dig4_to_xy,    dig8_to_xyz,   dig16_to_xyza,                       &
!        xy_to_dig4,    xyz_to_dig8,   xyza_to_dig16,                       &
!        gn_to_ea,ea_to_gn,                                                 &
        hil4_to_rz, hil8_to_rz, hil16_to_rz

!-- Public interfaces:
interface r_to_hil4;  module procedure r_to_hil4_s,r_to_hil4_d;   end interface
interface r_to_hil8;  module procedure r_to_hil8_d;              end interface
interface r_to_hil16; module procedure r_to_hil16_d;             end interface
interface r_to_hil48; module procedure r_to_hil48_d;             end interface
interface hil4_to_r;  module procedure hil4_to_r_d;              end interface
interface hil8_to_r;  module procedure hil8_to_r_d;              end interface
interface hil16_to_r; module procedure hil16_to_r_d;             end interface
interface hil48_to_r; module procedure hil48_to_r_d;             end interface
!..
interface hil4_to_xy;   module procedure hil4_to_xy_d;            end interface
interface hil8_to_xyz;  module procedure hil8_to_xyz_d;           end interface
interface hil16_to_xyza;module procedure hil16_to_xyza_d;         end interface
interface hil48_to_xyz; module procedure hil48_to_xyz_d;          end interface
!..
interface xy_to_hil4
   module procedure xy_to_hil4z_s,xy_to_hil4z_d,xy_to_hil4_s,xy_to_hil4_d
end interface
interface xyz_to_hil8
   module procedure xyz_to_hil8z_d,xyz_to_hil8_d
end interface
interface xyza_to_hil16
   module procedure xyza_to_hil16z_d,xyza_to_hil16_d
end interface
interface xyz_to_hil48
   module procedure xyz_to_hil48_d
end interface
interface hil4_to_xs;   module procedure hil4_to_xs_d;            end interface
interface hil48_to_xs;  module procedure hil48_to_xs_d;           end interface
interface xs_to_hil4;   module procedure xs_to_hil4_d;            end interface
interface xs_to_hil48;  module procedure xs_to_hil48_d;           end interface
!--
!-- private interfaces:
interface hil4_to_dig4;   module procedure hil4_to_dig4;    end interface
interface hil8_to_dig8;   module procedure hil8_to_dig8;    end interface
interface hil16_to_dig16; module procedure hil16_to_dig16;  end interface
!..
interface dig4_to_hil4;   module procedure dig4_to_hil4;   end interface
interface dig8_to_hil8;   module procedure dig8_to_hil8;   end interface
interface dig16_to_hil16; module procedure dig16_to_hil16; end interface
!..
interface dig4_to_xy;     module procedure dig4_to_xy_d;    end interface
interface dig8_to_xyz;    module procedure dig8_to_xyz_d;   end interface
interface dig16_to_xyza;  module procedure dig16_to_xyza_d; end interface
!..
interface xy_to_dig4
   module procedure xy_to_dig4_s,xy_to_dig4_d
end interface
interface xyz_to_dig8
   module procedure xyz_to_dig8_s,xyz_to_dig8_d
end interface
interface xyza_to_dig16
   module procedure xyza_to_dig16_d
end interface
!..
interface gn_to_ea  ;   module procedure gn_to_ea_s,gn_to_ea_d;   end interface
interface ea_to_gn  ;   module procedure ea_to_gn_s,ea_to_gn_d;   end interface
!
!== Earlier versions of hil*_to_r, still public, but deprecated
!   (scheduled for future deletion):
interface hil4_to_rz; module procedure hil4_to_rz_s,hil4_to_rz_d;end interface
interface hil8_to_rz; module procedure hil8_to_rz_d;             end interface
interface hil16_to_rz; module procedure hil16_to_rz_d;           end interface

contains
 
!=============================================================================
subroutine r_to_hil4_s(lgen,ngen,r,hil4)!                          [r_to_hil4]
!=============================================================================
! Take a real number r and peel off the base-4 digits from place lgen to ngen
! putting them into hil4 and leaving r as the remainder in [0,1). Note that
! by doing things this way, we can concatenate the similar operations 
! and even change bases from one link to the next.
!=============================================================================
implicit none
integer(i_kind),                     intent(IN   ):: lgen,ngen
real(sp),                    intent(inout):: r
integer(i_kind),dimension(lgen:ngen),intent(  out):: hil4
!-----------------------------------------------------------------------------
integer(i_kind):: i,j
!=============================================================================
do i=lgen,ngen
   if(i>0)r=4.0_sp*r
   j=r; r=r-j; hil4(i)=j
enddo
end subroutine r_to_hil4_s
!=============================================================================
subroutine r_to_hil4_d(lgen,ngen,r,hil4)!                          [r_to_hil4]
!=============================================================================
implicit none
integer(i_kind),                     intent(IN   ):: lgen,ngen
real(dp),                    intent(inout):: r
integer(i_kind),dimension(lgen:ngen),intent(  out):: hil4
!-----------------------------------------------------------------------------
integer(i_kind):: i,j
!=============================================================================
do i=lgen,ngen
   if(i>0)r=u4*r
   j=r; r=r-j; hil4(i)=j
enddo
end subroutine r_to_hil4_d

!=============================================================================
subroutine r_to_hil8_d(lgen,ngen,r,hil8)!                          [r_to_hil8]
!=============================================================================
implicit none
integer(i_kind),                     intent(IN   ):: lgen,ngen
real(dp),                    intent(inout):: r
integer(i_kind),dimension(lgen:ngen),intent(  out):: hil8
!-----------------------------------------------------------------------------
integer(i_kind):: i,j
!=============================================================================
do i=lgen,ngen
   if(i>0)r=u8*r
   j=r; r=r-j; hil8(i)=j
enddo
end subroutine r_to_hil8_d

!=============================================================================
subroutine r_to_hil16_d(lgen,ngen,r,hil16)!                       [r_to_hil16]
!=============================================================================
implicit none
integer(i_kind),                     intent(IN   ):: lgen,ngen
real(dp),                    intent(inout):: r
integer(i_kind),dimension(lgen:ngen),intent(  out):: hil16
!-----------------------------------------------------------------------------
integer(i_kind):: i,j
!=============================================================================
do i=lgen,ngen
   if(i>0)r=u16*r
   j=r; r=r-j; hil16(i)=j
enddo
end subroutine r_to_hil16_d

!=============================================================================
subroutine r_to_hil48_d(lgen,ngen4,ngen48,r,hil)!                 [r_to_hil48]
!=============================================================================
implicit none
integer(i_kind),                       intent(in   ):: lgen,ngen4,ngen48
real(dp),                      intent(inout):: r
integer(i_kind),dimension(lgen:ngen48),intent(  out):: hil
!-----------------------------------------------------------------------------
call r_to_hil4(lgen,   ngen4, r,hil(lgen:ngen4))
call r_to_hil8(ngen4+1,ngen48,r,hil(ngen4+1:ngen48))
end subroutine r_to_hil48_d

!=============================================================================
subroutine hil4_to_r_d(lgen,ngen,hil,r)!                           [hil4_to_r]
!=============================================================================
! Be sure to define r on input !
!=============================================================================
implicit none
integer(i_kind),                     intent(in   ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(in   ):: hil
real(dp),                    intent(inout):: r
!-----------------------------------------------------------------------------
integer(i_kind):: i
!=============================================================================
do i=ngen,lgen,-1
   r=r+hil(i)
   if(i==0)return
   r=r*o4
enddo
end subroutine hil4_to_r_d

!=============================================================================
subroutine hil8_to_r_d(lgen,ngen,hil,r)!                           [hil8_to_r]
!=============================================================================
! Be sure to define r on input !
!=============================================================================
implicit none
integer(i_kind),                     intent(in   ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(in   ):: hil
real(dp),                    intent(inout):: r
!-----------------------------------------------------------------------------
integer(i_kind):: i
!=============================================================================
do i=ngen,lgen,-1
   r=r+hil(i)
   if(i==0)return
   r=r*o8
enddo
end subroutine hil8_to_r_d

!=============================================================================
subroutine hil16_to_r_d(lgen,ngen,hil,r)!                         [hil16_to_r]
!=============================================================================
! Be sure to define r on input !
!=============================================================================
implicit none
integer(i_kind),                     intent(in   ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(in   ):: hil
real(dp),                    intent(inout):: r
!-----------------------------------------------------------------------------
integer(i_kind)           :: i
!=============================================================================
do i=ngen,lgen,-1
   r=r+hil(i)
   if(i==0)return
   r=r*o16
enddo
end subroutine hil16_to_r_d

!=============================================================================
subroutine hil48_to_r_d(lgen,ngen4,ngen48,hil,r)!                 [hil48_to_r]
!=============================================================================
! Be sure to define r on input !
!=============================================================================
implicit none
integer(i_kind),                       intent(in   ):: lgen,ngen4,ngen48
integer(i_kind),dimension(lgen:ngen48),intent(in   ):: hil
real(dp),                      intent(inout):: r
!=============================================================================
call hil8_to_r(ngen4+1,ngen48,hil(ngen4+1:ngen48),r)
call hil4_to_r(lgen,   ngen4, hil(lgen:ngen4),    r)
end subroutine hil48_to_r_d

!=============================================================================
subroutine hil4_to_xy_d(hil4,x,y)!                              [hil4_to_xy]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(in ):: hil4
real(dp),            intent(out):: x,y
!-----------------------------------------------------------------------------
real(dp)                     :: frac
integer(i_kind),dimension(size(hil4)):: dig4
integer(i_kind)                      :: presor,ngen
!============================================================================
dig4=hil4
presor=0; call hil4_to_dig4(presor,dig4)
call dig4_to_xy(dig4,x,y)
ngen=size(hil4)
frac=o2**(ngen+1); x=x+frac; y=y+frac ! <- to ensure final result is unbiased
end subroutine hil4_to_xy_d

!=============================================================================
subroutine hil8_to_xyz_d(hil8,x,y,z)!                          [hil8_to_xyz]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(in ):: hil8
real(dp),            intent(out):: x,y,z
!-----------------------------------------------------------------------------
real(dp)                     :: frac
integer(i_kind),dimension(size(hil8)):: dig8
integer(i_kind)                      :: presor,ngen
!============================================================================
dig8=hil8
presor=0; call hil8_to_dig8(presor,dig8)
call dig8_to_xyz(dig8,x,y,z)
ngen=size(hil8)
frac=o2**(ngen+1); x=x+frac; y=y+frac; z=z+frac ! <- final result is unbiased
end subroutine hil8_to_xyz_d

!=============================================================================
subroutine hil16_to_xyza_d(hil16,x,y,z,a)!                   [hil16_to_xyza]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(in ):: hil16
real(dp),            intent(out):: x,y,z,a
!-----------------------------------------------------------------------------
real(dp)                      :: frac
integer(i_kind),dimension(size(hil16)):: dig16
integer(i_kind)                       :: presor,ngen
!============================================================================
dig16=hil16
presor=0; call hil16_to_dig16(presor,dig16)
call dig16_to_xyza(dig16,x,y,z,a)
ngen=size(hil16)
frac=o2**(ngen+1); x=x+frac; y=y+frac; z=z+frac; a=a+frac ! result is unbiased
end subroutine hil16_to_xyza_d

!=============================================================================
subroutine hil48_to_xyz_d(ngen4,hil,x,y,z)!                     [hil48_to_xyz]
!=============================================================================
! Take a mixed (4 and 8) radix hilbert curve parameter, hil48, whose first
! ngen4 digits are the base-4 part, while the rest are base-8, and output
! the implied unbiased x,y,z coordinates, where x and y are in the unit square
! while z lies within [0,1/2**ngen4]. The resolution of the curve is
! 1/2**(ngen48) where ngen48=size(hil48), and to ensure the results are
! unbiased, the raw output from the final stage, dig8_to_xyz, has all the
! coordinates incremented by half the final resolution.
!=============================================================================
implicit none
integer(i_kind),             intent(in ):: ngen4
integer(i_kind),dimension(:),intent(in ):: hil
real(dp),            intent(out):: x,y,z
!-----------------------------------------------------------------------------
real(dp)                          :: frac8,frac,x4,y4
integer(i_kind),dimension(ngen4)          :: dig4
integer(i_kind),dimension(size(hil)-ngen4):: dig8
integer(i_kind),dimension(0:7)            :: p4to8
integer(i_kind)                           :: presor,ngen48
data p4to8/0,4,9,7,1,6,10,3/ ! Convert h4 orientation code to h8 code
!=============================================================================
ngen48=size(hil)
presor=0
if(ngen4>0)then      ! Treat the radix-4 part (if there is one)
   dig4=hil(1:ngen4)
   call hil4_to_dig4(presor,dig4)
   call dig4_to_xy(dig4,x4,y4)
else
   x4=0.0_dp; y4=0.0_dp
endif
if(ngen48>ngen4)then ! Treat the radix-8 part (if there is one)
   dig8=hil(ngen4+1:ngen48)
   presor=p4to8(presor)
   call hil8_to_dig8(presor,dig8)
   call dig8_to_xyz(dig8,x,y,z)
else   
   x=0.0_dp; y=0.0_dp; z=0.0_dp
endif
frac8=o2**ngen4
frac =o2**(ngen48+1)
x=x4+frac8*x+frac; y=y4+frac8*y+frac; z=   frac8*z+frac
end subroutine hil48_to_xyz_d

!=============================================================================
subroutine xy_to_hil4_s(x,y,hil4)!                               [xy_to_hil4]
!=============================================================================
! Convert an (x,y)-representation of a point in the proper interior of the
! unit square to an ngen-digit base-4 representation of the parameter of 
! a space-filling Hilbert curve.
!=============================================================================
implicit none
real(sp),            intent(IN ):: x,y
integer(i_kind),dimension(:),intent(OUT):: hil4
!-----------------------------------------------------------------------------
real(sp):: xr,yr
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y
call xy_to_dig4(xr,yr,hil4)
presor=0; call dig4_to_hil4(presor,hil4)
end subroutine xy_to_hil4_s
!=============================================================================
subroutine xy_to_hil4_d(x,y,hil4)!                               [xy_to_hil4]
!=============================================================================
! Convert an (x,y)-representation of a point in the proper interior of the
! unit square to an ngen-digit base-4 representation of the parameter of 
! a space-filling Hilbert curve.
!=============================================================================
implicit none
real(dp),            intent(IN ):: x,y
integer(i_kind),dimension(:),intent(OUT):: hil4
!-----------------------------------------------------------------------------
real(dp):: xr,yr
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y
call xy_to_dig4(xr,yr,hil4)
presor=0; call dig4_to_hil4(presor,hil4)
end subroutine xy_to_hil4_d

!=============================================================================
subroutine xyz_to_hil8_d(x,y,z,hil8)!                            [xyz_to_hil8]
!=============================================================================
implicit none
real(dp),            intent(IN ):: x,y,z
integer(i_kind),dimension(:),intent(OUT):: hil8
!-----------------------------------------------------------------------------
real(dp):: xr,yr,zr
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y; zr=z
call xyz_to_dig8(xr,yr,zr,hil8)
presor=0; call dig8_to_hil8(presor,hil8)
end subroutine xyz_to_hil8_d

!=============================================================================
subroutine xyza_to_hil16_d(x,y,z,a,hil16)!                     [xyza_to_hil16]
!=============================================================================
implicit none
real(dp),            intent(IN ):: x,y,z,a
integer(i_kind),dimension(:),intent(OUT):: hil16
!-----------------------------------------------------------------------------
real(dp):: xr,yr,zr,ar
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y; zr=z; ar=a
call xyza_to_dig16(xr,yr,zr,ar,hil16)
presor=0; call dig16_to_hil16(presor,hil16)
end subroutine xyza_to_hil16_d

!=============================================================================
subroutine xyz_to_hil48_d(ngen4,x,y,z,hil)!                    [xyz_to_hil48]
!=============================================================================
implicit none
integer(i_kind),             intent(in ):: ngen4
real(dp),            intent(in ):: x,y,z
integer(i_kind),dimension(:),intent(out):: hil
!-----------------------------------------------------------------------------
real(dp)              :: xr,yr,zr
integer(i_kind),dimension(0:7):: p4to8
integer(i_kind)               :: presor,ngen48
data p4to8/0,4,9,7,1,6,10,3/ ! Convert h4 orientation code to h8 code
!=============================================================================
ngen48=size(hil)
xr=x; yr=y; zr=z*u2**ngen4
presor=0
if(ngen4 >0    )then ! Treat radix-4 part (if any):
   call xy_to_dig4 (xr,yr,   hil(1:ngen4)       )
   call dig4_to_hil4(presor, hil(1:ngen4)       )
endif
presor=p4to8(presor)
if(ngen48>ngen4)then ! Treat radix-8 part (if any):
   call xyz_to_dig8(xr,yr,zr,hil(ngen4+1:ngen48))
   call dig8_to_hil8(presor, hil(ngen4+1:ngen48))
endif
end subroutine xyz_to_hil48_d

!=============================================================================
subroutine hil4_to_xs_d(ngen,hil,xs)!                             [hil4_to_xs]
!=============================================================================
implicit none
integer(i_kind),                  intent(IN ):: ngen
integer(i_kind),dimension(0:ngen),intent(IN ):: hil
real(dp),dimension(3),    intent(OUT):: xs
!-----------------------------------------------------------------------------
integer(i_kind) :: m,m6
real(dp):: x,y,q,xx,yy
!=============================================================================
call hil4_to_xy(hil(1:ngen),x,y)
m=hil(0)
m6=mod(m,6)
select case(m6)
   case(0); q=x;          x=y;        y=q
   case(1); q=x;          x=1.0_dp-y; y=q
   case(2); x=1.0_dp-x;   y=1.0_dp-y
   case(3); x=x;          y=1.0_dp-y
   case(4); q=x;          x=1.0_dp-y; y=1.0_dp-q
   case(5); q=x;          x=y;        y=1.0_dp-q
end select
call ea_to_gn(x,y,xx,yy) 
x=xx; y=yy
select case(m)
   case(1,2,9,10,13,14,21,22);    xs(1)= x
   case(3,4,7, 8,15,16,19,20);    xs(1)=-x
   case(0,11,12,23);              xs(1)= 1.0_dp
   case(5, 6,17,18);              xs(1)=-1.0_dp
end select
select case(m)
   case(6,11,12,17);              xs(2)= x
   case(0,5,18,23);               xs(2)=-x
   case(7,10,13,16);              xs(2)= y
   case(1,4,19,22);               xs(2)=-y
   case(8,9,14,15);               xs(2)= 1.0_dp
   case(2,3,20,21);               xs(2)=-1.0_dp
end select
select case(m)
   case(0,2,3,5,6,8,9,11);        xs(3)= y
   case(12,14,15,17,18,20,21,23); xs(3)=-y
   case(1,4,7,10);                xs(3)= 1.0_dp
   case(13,16,19,22);             xs(3)=-1.0_dp
end select
q=sqrt( dot_product(xs,xs) ); xs=xs/q
end subroutine hil4_to_xs_d

!=============================================================================
subroutine hil48_to_xs_d(ngen4,ngen48,hil,xs,r)!                 [hil48_to_xs]
!=============================================================================
implicit none
integer(i_kind),                    intent(IN ):: ngen4,ngen48
integer(i_kind),dimension(0:ngen48),intent(IN ):: hil
real(dp),dimension(3),      intent(OUT):: xs
real(dp),                   intent(out):: r
!-----------------------------------------------------------------------------
integer(i_kind) :: m,m6
real(dp):: x,y,q,xx,yy
!=============================================================================
call hil48_to_xyz(ngen4,hil(1:ngen48),x,y,r)
m=hil(0); m6=mod(m,6)
select case(m6)
   case(0); q=x;   x=y;        y=q
   case(1); q=x;   x=1.0_dp-y; y=q
   case(2);        x=1.0_dp-x; y=1.0_dp-y
   case(3);        x=x;        y=1.0_dp-y
   case(4); q=x;   x=1.0_dp-y; y=1.0_dp-q
   case(5); q=x;   x=y;        y=1.0_dp-q
end select
call ea_to_gn(x,y,xx,yy)
x=xx; y=yy
select case(m)
   case(1,2,9,10,13,14,21,22);    xs(1)= x
   case(3,4,7, 8,15,16,19,20);    xs(1)=-x
   case(0,11,12,23);              xs(1)= 1.0_dp
   case(5, 6,17,18);              xs(1)=-1.0_dp
end select
select case(m)
   case(6,11,12,17);              xs(2)= x
   case(0,5,18,23);               xs(2)=-x
   case(7,10,13,16);              xs(2)= y
   case(1,4,19,22);               xs(2)=-y
   case(8,9,14,15);               xs(2)= 1.0_dp
   case(2,3,20,21);               xs(2)=-1.0_dp
end select
select case(m)
   case(0,2,3,5,6,8,9,11);        xs(3)= y
   case(12,14,15,17,18,20,21,23); xs(3)=-y
   case(1,4,7,10);                xs(3)= 1.0_dp
   case(13,16,19,22);             xs(3)=-1.0_dp
end select
q=sqrt( dot_product(xs,xs) ); xs=xs/q
end subroutine hil48_to_xs_d

!=============================================================================
subroutine xs_to_hil4_d(ngen,xs,hil)!                             [xs_to_hil4]
!=============================================================================
implicit none
integer(i_kind),                   intent(IN ):: ngen
real(dp),dimension(3),     intent(IN ):: xs
integer(i_kind), dimension(0:ngen),intent(OUT):: hil
!-----------------------------------------------------------------------------
real(dp)                  :: x,y,z,ax,ay,az,q,xx,yy
integer(i_kind)                   :: k,L,m,m6
integer(i_kind),dimension(0:7)    :: Ltab,ktab
data Ltab/1,0,2,0,1,1,2,1/
data ktab/6,7,5,4,1,0,2,3/
!=============================================================================
x=xs(1); ax=abs(x)
y=xs(2); ay=abs(y)
z=xs(3); az=abs(z)
k=0; if( x>0 )k=k+1; if( y>0 )k=k+2; if( z>0 )k=k+4
L=0; if(ax>ay)L=L+1; if(ay>az)L=L+2; if(az>ax)L=L+4
L=Ltab(L)
k=ktab(k)
if(mod(k,2)==0)then; m=k*3+L
else;                m=k*3+2-L
endif
hil(0)=m
select case(m)
   case(0,5,6,11,12,17,18,23); x=ay/ax; y=az/ax
   case(2,3,8,9,14,15,20,21);  x=ax/ay; y=az/ay
   case(1,4,7,10,13,16,19,22); x=ax/az; y=ay/az
end select

m6=mod(m,6)
call gn_to_ea(x,y,xx,yy); x=xx; y=yy

select case(m6)
   case(0); q=x;   x=y;        y=q
   case(1); q=x;   x=y;        y=1.0_dp-q
   case(2);        x=1.0_dp-x; y=1.0_dp-y
   case(3);        x=x;        y=1.0_dp-y
   case(4); q=x;   x=1.0_dp-y; y=1.0_dp-q
   case(5); q=x;   x=1.0_dp-y; y=q
end select
call xy_to_hil4(x,y,hil(1:ngen))
end subroutine xs_to_hil4_d

!=============================================================================
subroutine xs_to_hil48_d(ngen4,ngen48,xs,r,hil)!                 [xs_to_hil48]
!=============================================================================
implicit none
integer(i_kind),                     intent(in ):: ngen4,ngen48
real(dp),dimension(3),       intent(in ):: xs
real(dp),                    intent(in ):: r
integer(i_kind), dimension(0:ngen48),intent(out):: hil
!-----------------------------------------------------------------------------
real(dp)                  :: x,y,z,ax,ay,az,q,xx,yy
integer(i_kind)                   :: k,L,m,m6
integer(i_kind),dimension(0:7)    :: Ltab,ktab
data Ltab/1,0,2,0,1,1,2,1/
data ktab/6,7,5,4,1,0,2,3/
!=============================================================================
x=xs(1); ax=abs(x)
y=xs(2); ay=abs(y)
z=xs(3); az=abs(z)
k=0; if( x>0 )k=k+1; if( y>0 )k=k+2; if( z>0 )k=k+4
L=0; if(ax>ay)L=L+1; if(ay>az)L=L+2; if(az>ax)L=L+4
L=Ltab(L)
k=ktab(k)
if(mod(k,2)==0)then; m=k*3+L
else;                m=k*3+2-L
endif
hil(0)=m
select case(m)
   case(0,5,6,11,12,17,18,23); x=ay/ax; y=az/ax
   case(2,3,8,9,14,15,20,21);  x=ax/ay; y=az/ay
   case(1,4,7,10,13,16,19,22); x=ax/az; y=ay/az
end select

! Insert equal-area transformation of (x,y) here
m6=mod(m,6)
call gn_to_ea(x,y,xx,yy) ; x=xx; y=yy

select case(m6)
   case(0); q=x;   x=y;        y=q
   case(1); q=x;   x=y;        y=1.0_dp-q
   case(2);        x=1.0_dp-x; y=1.0_dp-y
   case(3);        x=x;        y=1.0_dp-y
   case(4); q=x;   x=1.0_dp-y; y=1.0_dp-q
   case(5); q=x;   x=1.0_dp-y; y=q
end select
call xyz_to_hil48(ngen4,x,y,r,hil(1:ngen48))
end subroutine xs_to_hil48_d

!-----------------
! Private routines:
!=============================================================================
subroutine hil4_to_dig4(presor,hil4)!                           [hil4_to_dig4]
!=============================================================================
implicit none
integer(i_kind),             intent(inout):: presor
integer(i_kind),dimension(:),intent(inout):: hil4
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:3, 0:7):: dtable,ptable
integer(i_kind)                    :: igen,h
data dtable/0,2,3,1, 1,0,2,3, 3,1,0,2, 2,3,1,0, &
            0,1,3,2, 2,0,1,3, 3,2,0,1, 1,3,2,0/
data ptable/4,0,0,6, 7,1,1,5, 6,2,2,4, 5,3,3,7, &
            0,4,4,2, 3,5,5,1, 2,6,6,0, 1,7,7,3/
!=============================================================================
do igen=1,size(hil4)
   h=hil4(igen)
   hil4(igen)=dtable(h,presor)
   presor    =ptable(h,presor)
enddo
end subroutine hil4_to_dig4

!=============================================================================
subroutine hil8_to_dig8(presor,hil8)!                           [hil8_to_dig8]
!=============================================================================
implicit none
integer(i_kind),             intent(inout):: presor
integer(i_kind),dimension(:),intent(inout):: hil8
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:7,0:23):: dtable,ptable
integer(i_kind)                    :: igen,h
data dtable/                                       &
0,2,6,4,5,7,3,1, 0,4,5,1,3,7,6,2, 0,1,3,2,6,7,5,4, &
1,5,7,3,2,6,4,0, 1,0,4,5,7,6,2,3, 1,3,2,0,4,6,7,5, &
2,6,4,0,1,5,7,3, 2,3,7,6,4,5,1,0, 2,0,1,3,7,5,4,6, &
3,1,5,7,6,4,0,2, 3,7,6,2,0,4,5,1, 3,2,0,1,5,4,6,7, &
4,0,2,6,7,3,1,5, 4,5,1,0,2,3,7,6, 4,6,7,5,1,3,2,0, &
5,7,3,1,0,2,6,4, 5,1,0,4,6,2,3,7, 5,4,6,7,3,2,0,1, &
6,4,0,2,3,1,5,7, 6,2,3,7,5,1,0,4, 6,7,5,4,0,1,3,2, &
7,3,1,5,4,0,2,6, 7,6,2,3,1,0,4,5, 7,5,4,6,2,0,1,3/
data ptable/                                                                 &
 1, 2, 2,18,18,17,17,10,   2, 0, 0,16,16, 9, 9,20,   0, 1, 1,11,11,19,19,15, &
 5, 4, 4,21,21, 7, 7,14,   3, 5, 5,13,13,23,23, 6,   4, 9, 9, 8, 8,12,12,22, &
 8, 7, 7,12,12, 4, 4,23,   6, 8, 8,22,22,14,14, 3,   7, 6, 6, 5, 5,21,21,13, &
10,11,11,15,15,20,20, 1,  11, 9, 9,19,19, 0, 0,17,   9,10,10, 2, 2,16,16,18, &
14,13,13, 6, 6,22,22, 5,  12,14,14, 4, 4, 8, 8,21,  13,12,12,23,23,33, 3, 7, &
16,17,17, 9, 9, 2, 2,19,  17,15,15, 1, 1,18,18,11,  15,16,16,20,20,10,10, 0, &
19,20,20, 0, 0,11,11,16,  20,18,18,10,10,15,15, 2,  18,19,19,17,17, 1, 1, 9, &
23,22,22, 3, 3,13,13, 8,  21,23,23, 7, 7, 5, 5,12,  22,21,21,14,14, 6, 6, 4/
!=============================================================================
do igen=1,size(hil8)
   h=hil8(igen)
   hil8(igen)=dtable(h,presor)
   presor    =ptable(h,presor)
enddo
end subroutine hil8_to_dig8

!=============================================================================
subroutine hil16_to_dig16(presor,hil16)!                      [hil16_to_dig16]
!=============================================================================
implicit none
integer(i_kind),             intent(inout):: presor
integer(i_kind),dimension(:),intent(inout):: hil16
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:15,0:63):: dtable,ptable
integer(i_kind)                     :: igen,h
data dtable/                                     &
 0, 8,12, 4, 6,14,10, 2, 3,11,15, 7, 5,13, 9, 1, &
 0, 8, 9, 1, 5,13,12, 4, 6,14,15, 7, 3,11,10, 2, &
 0, 8,10, 2, 3,11, 9, 1, 5,13,15, 7, 6,14,12, 4, &
 0, 1, 3, 2, 6, 7, 5, 4,12,13,15,14,10,11, 9, 8, &
 1, 9,11, 3, 7,15,13, 5, 4,12,14, 6, 2,10, 8, 0, &
 1, 9,13, 5, 4,12, 8, 0, 2,10,14, 6, 7,15,11, 3, &
 1, 9, 8, 0, 2,10,11, 3, 7,15,14, 6, 4,12,13, 5, &
 1, 3, 2, 0, 4, 6, 7, 5,13,15,14,12, 8,10,11, 9, &
 2,10, 8, 0, 4,12,14, 6, 7,15,13, 5, 1, 9,11, 3, &
 2,10,14, 6, 7,15,11, 3, 1, 9,13, 5, 4,12, 8, 0, &
 2,10,11, 3, 1, 9, 8, 0, 4,12,13, 5, 7,15,14, 6, &
 2, 0, 1, 3, 7, 5, 4, 6,14,12,13,15,11, 9, 8,10, &
 3,11,15, 7, 5,13, 9, 1, 0, 8,12, 4, 6,14,10, 2, &
 3,11,10, 2, 6,14,15, 7, 5,13,12, 4, 0, 8, 9, 1, &
 3,11, 9, 1, 0, 8,10, 2, 6,14,12, 4, 5,13,15, 7, &
 3, 2, 0, 1, 5, 4, 6, 7,15,14,12,13, 9, 8,10,11, &
 4,12,14, 6, 2,10, 8, 0, 1, 9,11, 3, 7,15,13, 5, &
 4,12, 8, 0, 1, 9,13, 5, 7,15,11, 3, 2,10,14, 6, &
 4,12,13, 5, 7,15,14, 6, 2,10,11, 3, 1, 9, 8, 0, &
 4, 6, 7, 5, 1, 3, 2, 0, 8,10,11, 9,13,15,14,12, &
 5,13, 9, 1, 3,11,15, 7, 6,14,10, 2, 0, 8,12, 4, &
 5,13,12, 4, 0, 8, 9, 1, 3,11,10, 2, 6,14,15, 7, &
 5,13,15, 7, 6,14,12, 4, 0, 8,10, 2, 3,11, 9, 1, &
 5, 4, 6, 7, 3, 2, 0, 1, 9, 8,10,11,15,14,12,13, &
 6,14,10, 2, 0, 8,12, 4, 5,13, 9, 1, 3,11,15, 7, &
 6,14,15, 7, 3,11,10, 2, 0, 8, 9, 1, 5,13,12, 4, &
 6,14,12, 4, 5,13,15, 7, 3,11, 9, 1, 0, 8,10, 2, &
 6, 7, 5, 4, 0, 1, 3, 2,10,11, 9, 8,12,13,15,14, &
 7,15,13, 5, 1, 9,11, 3, 2,10, 8, 0, 4,12,14, 6, &
 7,15,11, 3, 2,10,14, 6, 4,12, 8, 0, 1, 9,13, 5, &
 7,15,14, 6, 4,12,13, 5, 1, 9, 8, 0, 2,10,11, 3, &
 7, 5, 4, 6, 2, 0, 1, 3,11, 9, 8,10,14,12,13,15, &
 8, 0, 2,10,14, 6, 4,12,13, 5, 7,15,11, 3, 1, 9, &
 8, 0, 4,12,13, 5, 1, 9,11, 3, 7,15,14, 6, 2,10, &
 8, 0, 1, 9,11, 3, 2,10,14, 6, 7,15,13, 5, 4,12, &
 8,10,11, 9,13,15,14,12, 4, 6, 7, 5, 1, 3, 2, 0, &
 9, 1, 5,13,15, 7, 3,11,10, 2, 6,14,12, 4, 0, 8, &
 9, 1, 0, 8,12, 4, 5,13,15, 7, 6,14,10, 2, 3,11, &
 9, 1, 3,11,10, 2, 0, 8,12, 4, 6,14,15, 7, 5,13, &
 9, 8,10,11,15,14,12,13, 5, 4, 6, 7, 3, 2, 0, 1, &
10, 2, 6,14,12, 4, 0, 8, 9, 1, 5,13,15, 7, 3,11, &
10, 2, 3,11,15, 7, 6,14,12, 4, 5,13, 9, 1, 0, 8, &
10, 2, 0, 8, 9, 1, 3,11,15, 7, 5,13,12, 4, 6,14, &
10,11, 9, 8,12,13,15,14, 6, 7, 5, 4, 0, 1, 3, 2, &
11, 3, 1, 9,13, 5, 7,15,14, 6, 4,12, 8, 0, 2,10, &
11, 3, 7,15,14, 6, 2,10, 8, 0, 4,12,13, 5, 1, 9, &
11, 3, 2,10, 8, 0, 1, 9,13, 5, 4,12,14, 6, 7,15, &
11, 9, 8,10,14,12,13,15, 7, 5, 4, 6, 2, 0, 1, 3, &
12, 4, 0, 8,10, 2, 6,14,15, 7, 3,11, 9, 1, 5,13, &
12, 4, 5,13, 9, 1, 0, 8,10, 2, 3,11,15, 7, 6,14, &
12, 4, 6,14,15, 7, 5,13, 9, 1, 3,11,10, 2, 0, 8, &
12,13,15,14,10,11, 9, 8, 0, 1, 3, 2, 6, 7, 5, 4, &
13, 5, 7,15,11, 3, 1, 9, 8, 0, 2,10,14, 6, 4,12, &
13, 5, 1, 9, 8, 0, 4,12,14, 6, 2,10,11, 3, 7,15, &
13, 5, 4,12,14, 6, 7,15,11, 3, 2,10, 8, 0, 1, 9, &
13,15,14,12, 8,10,11, 9, 1, 3, 2, 0, 4, 6, 7, 5, &
14, 6, 4,12, 8, 0, 2,10,11, 3, 1, 9,13, 5, 7,15, &
14, 6, 2,10,11, 3, 7,15,13, 5, 1, 9, 8, 0, 4,12, &
14, 6, 7,15,13, 5, 4,12, 8, 0, 1, 9,11, 3, 2,10, &
14,12,13,15,11, 9, 8,10, 2, 0, 1, 3, 7, 5, 4, 6, &
15, 7, 3,11, 9, 1, 5,13,12, 4, 0, 8,10, 2, 6,14, &
15, 7, 6,14,10, 2, 3,11, 9, 1, 0, 8,12, 4, 5,13, &
15, 7, 5,13,12, 4, 6,14,10, 2, 0, 8, 9, 1, 3,11, &
15,14,12,13, 9, 8,10,11, 3, 2, 0, 1, 5, 4, 6, 7/
data ptable/ &
 3, 2, 2,49,49,26,26,40,40,14,14,61,61,22,22,39,&
 3, 0, 0,38,38,20,20,49,49,24,24,62,62,12,12,43,&
 3, 1, 1,40,40,13,13,38,38,21,21,60,60,25,25,51,&
 0, 1, 1,14,14,25,25,23,23,49,49,62,62,41,41,36,&
 7, 5, 5,46,46,29,29,52,52,17,17,58,58, 9, 9,35,&
 7, 6, 6,52,52,18,18,33,33,10,10,56,56,30,30,47,&
 7, 4, 4,33,33, 8, 8,46,46,28,28,57,57,16,16,55,&
 5, 4, 4,10,10,16,16,31,31,52,52,58,58,32,32,45,&
11, 9, 9,34,34,17,17,56,56,29,29,54,54, 5, 5,47,&
11,10,10,56,56,30,30,45,45, 6, 6,52,52,18,18,35,&
11, 8, 8,45,45, 4, 4,34,34,16,16,53,53,28,28,59,&
 9, 8, 8, 6, 6,28,28,19,19,56,56,54,54,44,44,33,&
15,14,14,61,61,22,22,36,36, 2, 2,49,49,26,26,43,&
15,12,12,42,42,24,24,61,61,20,20,50,50, 0, 0,39,&
15,13,13,36,36, 1, 1,42,42,25,25,48,48,21,21,63,&
12,13,13, 2, 2,21,21,27,27,61,61,50,50,37,37,40,&
19,17,17,58,58, 9, 9,32,32, 5, 5,46,46,29,29,55,&
19,18,18,32,32, 6, 6,53,53,30,30,44,44,10,10,59,&
19,16,16,53,53,28,28,58,58, 8, 8,45,45, 4, 4,35,&
17,16,16,30,30, 4, 4,11,11,32,32,46,46,52,52,57,&
23,22,22,37,37,14,14,60,60,26,26,41,41, 2, 2,51,&
23,20,20,50,50, 0, 0,37,37,12,12,42,42,24,24,63,&
23,21,21,60,60,25,25,50,50, 1, 1,40,40,13,13,39,&
20,21,21,26,26,13,13, 3, 3,37,37,42,42,61,61,48,&
27,26,26,41,41, 2, 2,48,48,22,22,37,37,14,14,63,&
27,24,24,62,62,12,12,41,41, 0, 0,38,38,20,20,51,&
27,25,25,48,48,21,21,62,62,13,13,36,36, 1, 1,43,&
24,25,25,22,22, 1, 1,15,15,41,41,38,38,49,49,60,&
31,29,29,54,54, 5, 5,44,44, 9, 9,34,34,17,17,59,&
31,30,30,44,44,10,10,57,57,18,18,32,32, 6, 6,55,&
31,28,28,57,57,16,16,54,54, 4, 4,33,33, 8, 8,47,&
29,28,28,18,18, 8, 8, 7, 7,44,44,34,34,56,56,53,&
35,33,33,10,10,57,57,16,16,53,53,30,30,45,45, 7,&
35,34,34,16,16,54,54, 5, 5,46,46,28,28,58,58,11,&
35,32,32, 5, 5,44,44,10,10,56,56,29,29,52,52,19,&
33,32,32,46,46,52,52,59,59,16,16,30,30, 4, 4, 9,&
39,38,38,21,21,62,62,12,12,42,42,25,25,50,50, 3,&
39,36,36, 2, 2,48,48,21,21,60,60,26,26,40,40,15,&
39,37,37,12,12,41,41, 2, 2,49,49,24,24,61,61,23,&
36,37,37,42,42,61,61,51,51,21,21,26,26,13,13, 0,&
43,42,42,25,25,50,50, 0, 0,38,38,21,21,62,62,15,&
43,40,40,14,14,60,60,25,25,48,48,22,22,36,36, 3,&
43,41,41, 0, 0,37,37,14,14,61,61,20,20,49,49,27,&
40,41,41,38,38,49,49,63,63,25,25,22,22, 1, 1,12,&
47,45,45, 6, 6,53,53,28,28,57,57,18,18,33,33,11,&
47,46,46,28,28,58,58, 9, 9,34,34,16,16,54,54, 7,&
47,44,44, 9, 9,32,32, 6, 6,52,52,17,17,56,56,31,&
45,44,44,34,34,56,56,55,55,28,28,18,18, 8, 8, 5,&
51,50,50, 1, 1,42,42,24,24,62,62,13,13,38,38,23,&
51,48,48,22,22,36,36, 1, 1,40,40,14,14,60,60,27,&
51,49,49,24,24,61,61,22,22,37,37,12,12,41,41, 3,&
48,49,49,62,62,41,41,39,39, 1, 1,14,14,25,25,20,&
55,53,53,30,30,45,45, 4, 4,33,33,10,10,57,57,19,&
55,54,54, 4, 4,34,34,17,17,58,58, 8, 8,46,46,31,&
55,52,52,17,17,56,56,30,30,44,44, 9, 9,32,32, 7,&
53,52,52,58,58,32,32,47,47, 4, 4,10,10,16,16,29,&
59,57,57,18,18,33,33, 8, 8,45,45, 6, 6,53,53,31,&
59,58,58, 8, 8,46,46,29,29,54,54, 4, 4,34,34,19,&
59,56,56,29,29,52,52,18,18,32,32, 5, 5,44,44,11,&
57,56,56,54,54,44,44,35,35, 8, 8, 6, 6,28,28,17,&
63,62,62,13,13,38,38,20,20,50,50, 1, 1,42,42,27,&
63,60,60,26,26,40,40,13,13,36,36, 2, 2,48,48,23,&
63,61,61,20,20,49,49,26,26,41,41, 0, 0,37,37,15,&
60,61,61,50,50,37,37,43,43,13,13, 2, 2,21,21,24/
!=============================================================================
do igen=1,size(hil16)
   h=hil16(igen)
   hil16(igen)=dtable(h,presor)
   presor     =ptable(h,presor)
enddo
end subroutine hil16_to_dig16

!=============================================================================
subroutine dig4_to_hil4(presor,hil4)!                           [dig4_to_hil4]
!=============================================================================
! On input, hil4 contains the ngen base-4 digits defining the location of the
! target point in the unit square, and on output, it will contain the first
! ngen base-4 digits of the corresponding parameter, in [0,1), of the 
! Hilbert curve. On input, presor contains the orientation code of the whole
! curve; on output, it contains the orientation code of the final segment.
! The orientation code is identified with the oriented edge segments of the
! unit square in the following way:
! ORIENTATION CODE       EDGE SEGMENT
!          0        (0,0)-->(1,0)
!          1        (1,0)-->(1,1)
!          2        (1,1)-->(0,1)
!          3        (0,1)-->(1,0)
!          4        (0,0)-->(0,1)
!          5        (0,1)-->(1,1)
!          6        (1,1)-->(1,0)
!          7        (1,0)-->(0,0) 
!=============================================================================
implicit none
integer(i_kind),             intent(inout):: presor
integer(i_kind),dimension(:),intent(inout):: hil4
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:3, 0:7)        :: hil4table, presortable
integer(i_kind)                            :: igen,j
data hil4table  /0,3,1,2, 1,0,2,3, 2,1,3,0, 3,2,0,1, &
                 0,1,3,2, 1,2,0,3, 2,3,1,0, 3,0,2,1/
data presortable/4,6,0,0, 1,7,1,5, 2,2,4,6, 7,3,5,3, &
                 0,4,2,4, 5,5,3,1, 6,0,6,2, 3,1,7,7/
!=============================================================================
! At successive refinements, update the present orientation and refine the
! segment on the hilbert curve according to the quadrant of the present
! square occupied by the next generation of refinement's square:
do igen=1,size(hil4)
   j=hil4(igen)
   hil4(igen)=hil4table  (j,presor)
   presor    =presortable(j,presor)
enddo
end subroutine dig4_to_hil4 

!=============================================================================
subroutine dig8_to_hil8(presor,hil8)!                           [dig8_to_hil8]
!=============================================================================
! Like dig4_to_hil4, but for a cube. Orientation codes now run from 0 thru
! 23 and are grouped in triplets according to the vertex the associated edge
! begins with; within each triplet, the orientations are in the x, y, z order.
! The path of 8 stations through the 2*2*2 cube associated with overall
! orientation p is given by the array, hil8table(:,p); by finding the "0"
! and the "7" in this path, one can locate the path start and endpoint, and
! hence interpret the orientation that p refers to.
!=============================================================================
implicit none
integer(i_kind),             intent(inout):: presor
integer(i_kind),dimension(:),intent(inout):: hil8
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:7, 0:23)       :: hil8table, presortable
integer(i_kind)                            :: igen,j
data hil8table /                                              &
     0,7,1,6,3,4,2,5,    0,3,7,4,1,2,6,5,    0,1,3,2,7,6,4,5, &
     7,0,4,3,6,1,5,2,    1,0,6,7,2,3,5,4,    3,0,2,1,4,7,5,6, &
     3,4,0,7,2,5,1,6,    7,6,0,1,4,5,3,2,    1,2,0,3,6,5,7,4, &
     6,1,7,0,5,2,4,3,    4,7,3,0,5,6,2,1,    2,3,1,0,5,4,6,7, &
     1,6,2,5,0,7,3,4,    3,2,4,5,0,1,7,6,    7,4,6,5,0,3,1,2, &
     4,3,5,2,7,0,6,1,    2,1,5,6,3,0,4,7,    6,7,5,4,1,0,2,3, &
     2,5,3,4,1,6,0,7,    6,5,1,2,7,4,0,3,    4,5,7,6,3,2,0,1, &
     5,2,6,1,4,3,7,0,    5,4,2,3,6,7,1,0,    5,6,4,7,2,1,3,0/
data presortable/                                             &
 1,10, 2,17,18,18, 2,17,   2,16,20,16, 0, 0, 9, 9,   0, 1,11, 1,15,19,11,19, &
14, 5,21,21, 7, 4, 7, 4,   5, 3,23, 6, 5,13,23,13,   8, 4, 9, 9, 8,22,12,12, &
12,12, 8,23, 7, 4, 7, 4,   3,14, 6, 8,22,14,22, 8,   6, 6, 7, 5,21,21,13, 5, &
20,11, 1,10,20,11,15,15,  19,17,19,11, 0, 0, 9, 9,  10, 2,10, 9,16, 2,16,18, &
13,22,13,22,14, 5, 6, 6,   4,14, 4, 8,12,14,21, 8,   7,23, 3, 3,13,23,12,12, &
 9, 9, 2,17,19,16, 2,17,  15,15,18,18, 1,17, 1,11,  10, 0,10,20,16,15,16,20, &
20,11, 0, 0,20,11,19,16,  15,15,18,18, 2,10,20,10,  17, 1, 9, 1,17,19,18,19, &
13,22,13,22, 3, 3, 8,23,   5, 7,23, 7, 5,12,23,21,   6, 6,14, 4,21,21,14,22/
!=============================================================================
! At successive refinements, update the present orientation and refine the
! segment on the hilbert curve according to the quadrant of the present
! square occupied by the next generation of refinement's cube:
do igen=1,size(hil8)
   j=hil8(igen)
   hil8(igen)=hil8table  (j,presor)
   presor    =presortable(j,presor)
enddo
end subroutine dig8_to_hil8

!=============================================================================
subroutine dig16_to_hil16(presor,hil16)!                      [dig16_to_hil16]
!=============================================================================
! Like dig4_to_hil4, but for a 4D hypercube. 
!=============================================================================
implicit none
integer(i_kind),             intent(inout):: presor
integer(i_kind),dimension(:),intent(inout):: hil16
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:15, 0:63) :: hil16table,presortable
integer(i_kind)                       :: igen,j
data hil16table / &
0,15,7,8,3,12,4,11,1,14,6,9,2,13,5,10,  0,3,15,12,7,4,8,11,1,2,14,13,6,5,9,10,&
0,7,3,4,15,8,12,11,1,6,2,5,14,9,13,10,  0,1,3,2,7,6,4,5,15,14,12,13,8,9,11,10,&
15,0,12,3,8,7,11,4,14,1,13,2,9,6,10,5,  7,0,8,15,4,3,11,12,6,1,9,14,5,2,10,13,&
3,0,4,7,12,15,11,8,2,1,5,6,13,14,10,9,  3,0,2,1,4,7,5,6,12,15,13,14,11,8,10,9,&
3,12,0,15,4,11,7,8,2,13,1,14,5,10,6,9,  15,8,0,7,12,11,3,4,14,9,1,6,13,10,2,5,&
7,4,0,3,8,11,15,12,6,5,1,2,9,10,14,13,  1,2,0,3,6,5,7,4,14,13,15,12,9,10,8,11,&
8,7,15,0,11,4,12,3,9,6,14,1,10,5,13,2,  12,15,3,0,11,8,4,7,13,14,2,1,10,9,5,6,&
4,3,7,0,11,12,8,15,5,2,6,1,10,13,9,14,  2,3,1,0,5,4,6,7,13,12,14,15,10,11,9,8,&
7,8,4,11,0,15,3,12,6,9,5,10,1,14,2,13,  3,4,12,11,0,7,15,8,2,5,13,10,1,6,14,9,&
15,12,8,11,0,3,7,4,14,13,9,10,1,2,6,5,  7,4,6,5,0,3,1,2,8,11,9,10,15,12,14,13,&
12,3,11,4,15,0,8,7,13,2,10,5,14,1,9,6,  4,7,11,8,3,0,12,15,5,6,10,9,2,1,13,14,&
8,15,11,12,7,0,4,3,9,14,10,13,6,1,5,2,  6,7,5,4,1,0,2,3,9,8,10,11,14,15,13,12,&
4,11,3,12,7,8,0,15,5,10,2,13,6,9,1,14,  8,11,7,4,15,12,0,3,9,10,6,5,14,13,1,2,&
12,11,15,8,3,4,0,7,13,10,14,9,2,5,1,6,  4,5,7,6,3,2,0,1,11,10,8,9,12,13,15,14,&
11,4,8,7,12,3,15,0,10,5,9,6,13,2,14,1,  11,12,4,3,8,15,7,0,10,13,5,2,9,14,6,1,&
11,8,12,15,4,7,3,0,10,9,13,14,5,6,2,1,  5,6,4,7,2,1,3,0,10,9,11,8,13,14,12,15,&
1,14,2,13,6,9,5,10,0,15,3,12,7,8,4,11,  1,6,14,9,2,5,13,10,0,7,15,8,3,4,12,11,&
1,2,6,5,14,13,9,10,0,3,7,4,15,12,8,11,  15,12,14,13,8,11,9,10,0,3,1,2,7,4,6,5,&
14,1,9,6,13,2,10,5,15,0,8,7,12,3,11,4,  2,1,13,14,5,6,10,9,3,0,12,15,4,7,11,8,&
6,1,5,2,9,14,10,13,7,0,4,3,8,15,11,12,  14,15,13,12,9,8,10,11,1,0,2,3,6,7,5,4,&
6,9,1,14,5,10,2,13,7,8,0,15,4,11,3,12,  14,13,1,2,9,10,6,5,15,12,0,3,8,11,7,4,&
2,5,1,6,13,10,14,9,3,4,0,7,12,11,15,8,  12,13,15,14,11,10,8,9,3,2,0,1,4,5,7,6,&
13,2,14,1,10,5,9,6,12,3,15,0,11,4,8,7,  9,14,6,1,10,13,5,2,8,15,7,0,11,12,4,3,&
5,6,2,1,10,9,13,14,4,7,3,0,11,8,12,15,  13,14,12,15,10,9,11,8,2,1,3,0,5,6,4,7,&
2,13,5,10,1,14,6,9,3,12,4,11,0,15,7,8,  6,5,9,10,1,2,14,13,7,4,8,11,0,3,15,12,&
14,9,13,10,1,6,2,5,15,8,12,11,0,7,3,4,  8,9,11,10,15,14,12,13,7,6,4,5,0,1,3,2,&
9,6,10,5,14,1,13,2,8,7,11,4,15,0,12,3,  5,2,10,13,6,1,9,14,4,3,11,12,7,0,8,15,&
13,14,10,9,2,1,5,6,12,15,11,8,3,0,4,7,  11,8,10,9,12,15,13,14,4,7,5,6,3,0,2,1,&
5,10,6,9,2,13,1,14,4,11,7,8,3,12,0,15,  13,10,2,5,14,9,1,6,12,11,3,4,15,8,0,7,&
9,10,14,13,6,5,1,2,8,11,15,12,7,4,0,3,  9,10,8,11,14,13,15,12,6,5,7,4,1,2,0,3,&
10,5,13,2,9,6,14,1,11,4,12,3,8,7,15,0,  10,9,5,6,13,14,2,1,11,8,4,7,12,15,3,0,&
10,13,9,14,5,2,6,1,11,12,8,15,4,3,7,0,  10,11,9,8,13,12,14,15,5,4,6,7,2,3,1,0/

data presortable/ &
 3,39,40,40,49,61,49,61, 2,22,26,14, 2,22,26,14,&
 3,38,43,62,49,38,49,62, 0, 0,12,12,20,20,24,24,&
 3,38,40,40,51,38,60,60, 1,13, 1,13,25,21,25,21,&
 0, 1,14, 1,23,25,14,25,36,41,62,41,23,49,62,49,&
35, 7,58,46,52,52,58,46, 9, 5, 9, 5,17,29,17,29,&
33, 7,33,47,52,52,56,56,18, 6,10,30,18, 6,10,30,&
33, 7,33,46,57,55,57,46, 4, 4, 8, 8,16,16,28,28,&
10, 5, 4, 4,10,31,16,16,58,45,32,32,58,31,52,52,&
34,54,11,47,34,54,56,56, 9, 5, 9, 5,17,29,17,29,&
35,45,11,45,52,52,56,56,18, 6,10,30,18, 6,10,30,&
34,45,11,45,34,53,59,53, 4, 4, 8, 8,16,16,28,28,&
 8, 8, 9, 6,28,28,19, 6,44,44,33,54,56,56,19,54,&
36,36,43,15,49,61,49,61, 2,22,26,14, 2,22,26,14,&
50,39,42,15,50,61,42,61, 0, 0,12,12,20,20,24,24,&
36,36,42,15,48,48,42,63, 1,13, 1,13,25,21,25,21,&
13, 2,13,12,21, 2,21,27,37,50,37,40,61,50,61,27,&
32,32,58,46,19,55,58,46, 9, 5, 9, 5,17,29,17,29,&
32,32,44,44,19,53,59,53,18, 6,10,30,18, 6,10,30,&
35,45,58,45,19,53,58,53, 4, 4, 8, 8,16,16,28,28,&
11,30, 4, 4,17,30,16,16,11,46,32,32,57,46,52,52,&
41,37,41,37,51,23,60,60, 2,22,26,14, 2,22,26,14,&
50,37,42,37,50,23,42,63, 0, 0,12,12,20,20,24,24,&
50,39,40,40,50,23,60,60, 1,13, 1,13,25,21,25,21,&
13, 3,13,26,21,20,21,26,37, 3,37,42,61,48,61,42,&
41,37,41,37,48,48,27,63, 2,22,26,14, 2,22,26,14,&
41,38,41,62,51,38,27,62, 0, 0,12,12,20,20,24,24,&
36,36,43,62,48,48,27,62, 1,13, 1,13,25,21,25,21,&
22, 1,15, 1,22,25,24,25,38,41,15,41,38,49,60,49,&
34,54,44,44,34,54,59,31, 9, 5, 9, 5,17,29,17,29,&
32,32,44,44,57,55,57,31,18, 6,10,30,18, 6,10,30,&
33,54,33,47,57,54,57,31, 4, 4, 8, 8,16,16,28,28,&
 8, 8,18, 7,28,28,18,29,44,44,34, 7,56,56,34,53,&
33,45,33,45,57,53,57,53,35, 7,10,30,16,16,10,30,&
34,54,58,46,34,54,58,46,35, 5,11, 5,16,16,28,28,&
32,32,44,44,52,52,56,56,35, 5,10, 5,19,29,10,29,&
 9,30, 4, 4,59,30,16,16,33,46,32,32,59,46,52,52,&
50,38,42,62,50,38,42,62, 3,39,12,12,25,21,25,21,&
36,36,40,40,48,48,60,60, 2,39,26,15, 2,21,26,21,&
41,37,41,37,49,61,49,61, 2,39,12,12, 2,23,24,24,&
13, 0,13,26,21,51,21,26,37,36,37,42,61,51,61,42,&
50,38,42,62,50,38,42,62, 0, 0,43,15,25,21,25,21,&
36,36,40,40,48,48,60,60, 3,22,43,14,25,22,25,14,&
41,37,41,37,49,61,49,61, 0, 0,43,14,20,20,27,14,&
22, 1,12, 1,22,25,63,25,38,41,40,41,38,49,63,49,&
33,45,33,45,57,53,57,53,18, 6,11,47,18, 6,28,28,&
34,54,58,46,34,54,58,46, 9, 7, 9,47,16,16,28,28,&
32,32,44,44,52,52,56,56, 9, 6, 9,47,17, 6,17,31,&
 8, 8,18, 5,28,28,18,55,44,44,34,45,56,56,34,55,&
50,38,42,62,50,38,42,62, 1,13, 1,13,51,23,24,24,&
36,36,40,40,48,48,60,60, 1,22, 1,14,51,22,27,14,&
41,37,41,37,49,61,49,61, 3,22,12,12,51,22,24,24,&
39, 1,14, 1,20,25,14,25,39,41,62,41,48,49,62,49,&
33,45,33,45,57,53,57,53, 4, 4,10,30,19,55,10,30,&
34,54,58,46,34,54,58,46, 4, 4, 8, 8,17,55,17,31,&
32,32,44,44,52,52,56,56, 9, 7, 9,30,17,55,17,30,&
10,47, 4, 4,10,29,16,16,58,47,32,32,58,53,52,52,&
33,45,33,45,57,53,57,53,18, 6, 8, 8,18, 6,59,31,&
34,54,58,46,34,54,58,46, 4, 4, 8, 8,19,29,59,29,&
32,32,44,44,52,52,56,56,18, 5,11, 5,18,29,59,29,&
 8, 8,35, 6,28,28,17, 6,44,44,35,54,56,56,57,54,&
50,38,42,62,50,38,42,62, 1,13, 1,13,20,20,27,63,&
36,36,40,40,48,48,60,60, 2,13,26,13, 2,23,26,63,&
41,37,41,37,49,61,49,61, 0, 0,26,15,20,20,26,63,&
13, 2,13,43,21, 2,21,24,37,50,37,43,61,50,61,60/
!=============================================================================
! At successive refinements, update the present orientation and refine the
! segment on the hilbert curve according to the quadrant of the present
! square occupied by the next generation of refinement's hypercube:
do igen=1,size(hil16)
   j=hil16(igen)
   hil16(igen)=hil16table (j,presor)
   presor     =presortable(j,presor)
enddo
end subroutine dig16_to_hil16

!=============================================================================
subroutine dig4_to_xy_d(dig,x,y)!                                [dig4_to_xy]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(in ):: dig
real(dp),            intent(out):: x,y
!-----------------------------------------------------------------------------
real(dp)              :: s
logical,dimension(0:3):: xofd4,yofd4
integer(i_kind)               :: igen,d
data xofd4/F,T,F,T/
data yofd4/F,F,T,T/
!=============================================================================
x=u0; y=u0; s=u1
do igen=size(dig),1,-1
   d=dig(igen); if(xofd4(d))x=x+s; if(yofd4(d))y=y+s
   s=s*u2
enddo
x=x/s; y=y/s
end subroutine dig4_to_xy_d

!=============================================================================
subroutine dig8_to_xyz_d(dig,x,y,z)!                             [dig8_to_xyz]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(in ):: dig
real(dp),            intent(out):: x,y,z
!-----------------------------------------------------------------------------
real(dp)              :: s
logical,dimension(0:7):: xofd8,yofd8,zofd8
integer(i_kind)               :: igen,d
data xofd8/F,T,F,T,F,T,F,T/
data yofd8/F,F,T,T,F,F,T,T/
data zofd8/F,F,F,F,T,T,T,T/
!=============================================================================
x=u0; y=u0; z=u0; s=u1
do igen=size(dig),1,-1
   d=dig(igen); if(xofd8(d))x=x+s; if(yofd8(d))y=y+s; if(zofd8(d))z=z+s
   s=s*u2
enddo
x=x/s; y=y/s; z=z/s
end subroutine dig8_to_xyz_d

!=============================================================================
subroutine dig16_to_xyza_d(dig,x,y,z,a)!                       [dig16_to_xyza]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(in ):: dig
real(dp),            intent(out):: x,y,z,a
!-----------------------------------------------------------------------------
real(dp)               :: s
logical,dimension(0:15):: xofd16,yofd16,zofd16,aofd16
integer(i_kind)                :: igen,d
data xofd16/F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T/
data yofd16/F,F,T,T,F,F,T,T,F,F,T,T,F,F,T,T/
data zofd16/F,F,F,F,T,T,T,T,F,F,F,F,T,T,T,T/
data aofd16/F,F,F,F,F,F,F,F,T,T,T,T,T,T,T,T/
!=============================================================================
x=u0; y=u0; z=u0; a=u0; s=u1
do igen=size(dig),1,-1
   d=dig(igen)
   if(xofd16(d))x=x+s;if(yofd16(d))y=y+s;if(zofd16(d))z=z+s;if(aofd16(d))a=a+s
   s=s*u2
enddo
x=x/s; y=y/s; z=z/s; a=a/s
end subroutine dig16_to_xyza_d

!=============================================================================
subroutine xy_to_dig4_s(x,y,dig4)!                                [xy_to_dig4]
!=============================================================================
! Convert an (x,y)-representation of a point in the square, [0,1]*[0,1]
! to an ngen-digit base-4 number, dig4, where ngen is the size of array dig4.
!=============================================================================
implicit none
real(sp),            intent(inout):: x,y
integer(i_kind),dimension(:),intent(  out):: dig4
!-----------------------------------------------------------------------------
integer(i_kind):: igen
!=============================================================================
if(x< 0.0_sp)stop 'In xy_to_dig4; x< 0.0_sp'
if(x> 1.0_sp)stop 'In xy_to_dig4; x> 1.0_sp'
if(y< 0.0_sp)stop 'In xy_to_dig4; y< 0.0_sp'
if(y> 1.0_sp)stop 'In xy_to_dig4; y> 1.0_sp'
dig4=0
do igen=1,size(dig4)
   x=x*2.0_sp; y=y*2.0_sp
   if(x>=1.0_sp)then; dig4(igen)=dig4(igen)+1; x=x-1.0_sp; endif
   if(y>=1.0_sp)then; dig4(igen)=dig4(igen)+2; y=y-1.0_sp; endif
enddo
end subroutine xy_to_dig4_s
!=============================================================================
subroutine xy_to_dig4_d(x,y,dig4)!                                [xy_to_dig4]
!=============================================================================
implicit none
real(dp),            intent(inout):: x,y
integer(i_kind),dimension(:),intent(  out):: dig4
!-----------------------------------------------------------------------------
integer(i_kind):: igen
!=============================================================================
if(x< u0)stop 'In xy_to_dig4; x< 0.0_dp'
if(x> u1)stop 'In xy_to_dig4; x> 1.0_dp'
if(y< u0)stop 'In xy_to_dig4; y< 0.0_dp'
if(y> u1)stop 'In xy_to_dig4; y> 1.0_dp'
dig4=0
do igen=1,size(dig4)
   x=x*u2; y=y*u2
   if(x>=u1)then; dig4(igen)=dig4(igen)+1; x=x-u1; endif
   if(y>=u1)then; dig4(igen)=dig4(igen)+2; y=y-u1; endif
enddo
end subroutine xy_to_dig4_d
   
!=============================================================================
subroutine xyz_to_dig8_s(x,y,z,dig8)!                            [xyz_to_dig8]
!=============================================================================
! Convert an (x,y,z)-representation of a point in the cube, [0,1]*[0,1]*[0,1]
! to an ngen-digit base-8 number, dig8.
!=============================================================================
implicit none
real(sp),            intent(inout):: x,y,z
integer(i_kind),dimension(:),intent(  out):: dig8
!-----------------------------------------------------------------------------
integer(i_kind):: igen
!=============================================================================
if(x< 0.0_sp)stop 'In xyz_to_dig8; x< 0.0_sp'
if(x> 1.0_sp)stop 'In xyz_to_dig8; x> 1.0_sp'
if(y< 0.0_sp)stop 'In xyz_to_dig8; y< 0.0_sp'
if(y> 1.0_sp)stop 'In xyz_to_dig8; y> 1.0_sp'
if(z< 0.0_sp)stop 'In xyz_to_dig8; z< 0.0_sp'
if(z> 1.0_sp)stop 'In xyz_to_dig8; z> 1.0_sp'
dig8=0
do igen=1,size(dig8)
   x=x*2.0_sp; y=y*2.0_sp; z=z*2.0_sp
   if(x>=1.0_sp)then; dig8(igen)=dig8(igen)+1; x=x-1.0_sp; endif
   if(y>=1.0_sp)then; dig8(igen)=dig8(igen)+2; y=y-1.0_sp; endif
   if(z>=1.0_sp)then; dig8(igen)=dig8(igen)+4; z=z-1.0_sp; endif
enddo
end subroutine xyz_to_dig8_s
!=============================================================================
subroutine xyz_to_dig8_d(x,y,z,dig8)!                            [xyz_to_dig8]
!=============================================================================
implicit none
real(dp),            intent(inout):: x,y,z
integer(i_kind),dimension(:),intent(  out):: dig8
!-----------------------------------------------------------------------------
integer(i_kind):: igen
!=============================================================================
if(x< u0)stop 'In xyz_to_dig8; x< 0.0_dp'
if(x> u1)stop 'In xyz_to_dig8; x> 1_0_dp'
if(y< u0)stop 'In xyz_to_dig8; y< 0_0_dp'
if(y> u1)stop 'In xyz_to_dig8; y> 1_0_dp'
if(z< u0)stop 'In xyz_to_dig8; z< 0_0_dp'
if(z> u1)stop 'In xyz_to_dig8; z> 1_0_dp'
dig8=0
do igen=1,size(dig8)
   x=x*u2; y=y*u2; z=z*u2
   if(x>=u1)then; dig8(igen)=dig8(igen)+1; x=x-u1; endif
   if(y>=u1)then; dig8(igen)=dig8(igen)+2; y=y-u1; endif
   if(z>=u1)then; dig8(igen)=dig8(igen)+4; z=z-u1; endif
enddo
end subroutine xyz_to_dig8_d

!=============================================================================
subroutine xyza_to_dig16_d(x,y,z,a,dig16)!                     [xyza_to_dig16]
!=============================================================================
! Convert an (x,y,z,t)-representation of a point in the hypercube, 
! [0,1]*[0,1]*[0,1]*[0,1]
! to an ngen-digit base-16 number, dig16.
!=============================================================================
implicit none
real(dp),            intent(inout):: x,y,z,a
integer(i_kind),dimension(:),intent(  out):: dig16
!-----------------------------------------------------------------------------
integer(i_kind):: igen
!=============================================================================
if(x< u0)stop 'In xyza_to_dig16; x< 0_0_dp'
if(x> u1)stop 'In xyza_to_dig16; x> 1_0_dp'
if(y< u0)stop 'In xyza_to_dig16; y< 0_0_dp'
if(y> u1)stop 'In xyza_to_dig16; y> 1_0_dp'
if(z< u0)stop 'In xyza_to_dig16; z< 0_0_dp'
if(z> u1)stop 'In xyza_to_dig16; z> 1_0_dp'
if(a< u0)stop 'In xyza_to_dig16; a< 0_0_dp'
if(a> u1)stop 'In xyza_to_dig16; a> 1_0_dp'
dig16=0
do igen=1,size(dig16)
   x=x*u2; y=y*u2; z=z*u2; a=a*u2
   if(x>=u1)then; dig16(igen)=dig16(igen)+1; x=x-u1; endif
   if(y>=u1)then; dig16(igen)=dig16(igen)+2; y=y-u1; endif
   if(z>=u1)then; dig16(igen)=dig16(igen)+4; z=z-u1; endif
   if(a>=u1)then; dig16(igen)=dig16(igen)+8; a=a-u1; endif
enddo
end subroutine xyza_to_dig16_d

!=============================================================================
subroutine gn_to_ea_s(x1,y1, x2,y2)!                                [gn_to_ea]
!=============================================================================
! Gnomonic to equal-area cubic
!=============================================================================
implicit none
real(sp),intent(IN ):: x1,y1
real(sp),intent(OUT):: x2,y2
!-----------------------------------------------------------------------------
integer(i_kind)  :: iquad
real(sp) :: x,q,xx,xxp,rxxp,p
!=============================================================================
iquad=1
if(y1 >  x1)iquad=iquad+1
if(x1 < -y1)iquad=iquad+2
if(x1== 0.0_sp  .and. y1==0.0_sp )then; x2=0.0_sp; y2=0.0_sp; return; endif
select case(iquad)
   case(1,4); x=abs(x1); q=y1/x
   case(2,3); x=abs(y1); q=x1/x
end select
xx=x*x; xxp=xx+1.0_sp; rxxp=sqrt(xxp)
q=q*sqrt((xx+xxp)/(xxp+q*q*xx)) 
p=sqrt(asin(xx/xxp)/pio6)

select case(iquad)
   case(1); x2= p; y2=p*q
   case(2); y2= p; x2=p*q
   case(3); y2=-p; x2=p*q
   case(4); x2=-p; y2=p*q
end select
end subroutine gn_to_ea_s
!=============================================================================
subroutine gn_to_ea_d(x1,y1, x2,y2)!                                [gn_to_ea]
!=============================================================================
! Gnomonic to equal-area cubic
!============================================================================
implicit none
real(dp),intent(IN ):: x1,y1
real(dp),intent(OUT):: x2,y2
!-----------------------------------------------------------------------------
integer(i_kind) :: iquad
real(dp):: x,q,xx,xxp,rxxp,p
!=============================================================================
iquad=1
if(y1 >  x1)iquad=iquad+1
if(x1 < -y1)iquad=iquad+2
if(x1==0.0_dp .and. y1==0.0_dp)then; x2=0.0_dp; y2=0.0_dp; return; endif
select case(iquad)
   case(1,4); x=abs(x1); q=y1/x
   case(2,3); x=abs(y1); q=x1/x
end select
xx=x*x; xxp=xx+1; rxxp=sqrt(xxp)
q=q*sqrt((xx+xxp)/(xxp+q*q*xx)) 
p=sqrt(asin(xx/xxp)/pio6)

select case(iquad)
   case(1); x2= p; y2=p*q
   case(2); y2= p; x2=p*q
   case(3); y2=-p; x2=p*q
   case(4); x2=-p; y2=p*q
end select
end subroutine gn_to_ea_d


!=============================================================================
subroutine ea_to_gn_s(x2,y2, x1,y1)!                                [ea_to_gn]
!=============================================================================
! Equal-area cubic to gnomonic
!============================================================================
implicit none
real(sp),intent(IN ):: x2,y2
real(sp),intent(OUT):: x1,y1
!-----------------------------------------------------------------------------
integer(i_kind) :: iquad
real(sp):: x,q,xx,xxp,p,pp,s
!=============================================================================
iquad=1
if(y2 >  x2)iquad=iquad+1
if(x2 < -y2)iquad=iquad+2
if(x2==0.0_sp .and. y2==0.0_sp)then; x1=0.0_sp; y1=0.0_sp; return; endif

select case(iquad)
   case(1); p= x2; q=y2/p
   case(2); p= y2; q=x2/p
   case(3); p=-y2; q=x2/p
   case(4); p=-x2; q=y2/p
end select
pp=p*p
s=sin(pio6*pp)
xx=max(0._sp,s/(1.0_sp-s))
xxp=xx+1.0_sp
x=sqrt(xx)

q=q * sqrt(  xxp/(xxp+xx*(1.0_sp-q*q)) )

select case(iquad)
   case(1); x1= x; y1=x*q
   case(2); y1= x; x1=x*q
   case(3); y1=-x; x1=x*q
   case(4); x1=-x; y1=x*q
end select
end subroutine ea_to_gn_s
!=============================================================================
subroutine ea_to_gn_d(x2,y2, x1,y1)!                                [ea_to_gn]
!=============================================================================
! equal-area cubic to gnomonic
!=============================================================================
implicit none
real(dp),intent(IN ):: x2,y2
real(dp),intent(OUT):: x1,y1
!-----------------------------------------------------------------------------
integer(i_kind) :: iquad
real(dp):: x,q,xx,xxp,p,pp,s
!=============================================================================
iquad=1
if(y2 >  x2)iquad=iquad+1
if(x2 < -y2)iquad=iquad+2
if(x2==0.0_dp .and. y2==0.0_dp)then; x1=0.0_dp; y1=0.0_dp; return; endif

select case(iquad)
   case(1); p= x2; q=y2/p
   case(2); p= y2; q=x2/p
   case(3); p=-y2; q=x2/p
   case(4); p=-x2; q=y2/p
end select
pp=p*p
s=sin(pio6*pp)
xx=max(u0,s/(1.0_dp-s))
xxp=xx+u1
x=sqrt(xx)

q=q * sqrt(  xxp/(xxp+xx*(1.0_dp-q*q)) )

select case(iquad)
   case(1); x1= x; y1=x*q
   case(2); y1= x; x1=x*q
   case(3); y1=-x; x1=x*q
   case(4); x1=-x; y1=x*q
end select
end subroutine ea_to_gn_d

!=============================================================================
!=============================================================================
! Routines whose usage is deprecated (better versions are now available)

!=============================================================================
subroutine hil4_to_rz_s(lgen,ngen,hil4,r)!                        [hil4_to_rz]
!=============================================================================
! Deprecated; replace by hil4_to_r with r also predefined
implicit none
integer(i_kind),                     intent(IN ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(IN ):: hil4
real(sp),                    intent(OUT):: r
!-----------------------------------------------------------------------------
real(sp),parameter:: o4=1._sp/4_sp
real(sp)          :: p
integer(i_kind)           :: i
!=============================================================================
r=0.0_sp; if(lgen==0)r=hil4(lgen)
p=o4
do i=1,ngen
   r=r+p*hil4(i)
   p=p*o4
enddo
end subroutine hil4_to_rz_s
!=============================================================================
subroutine hil4_to_rz_d(lgen,ngen,hil4,r)!                        [hil4_to_rz]
!=============================================================================
! Deprecated; replace by hil4_to_r with r also predefined
implicit none
integer(i_kind),                     intent(IN ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(IN ):: hil4
real(dp),                    intent(OUT):: r
!-----------------------------------------------------------------------------
real(dp):: p
integer(i_kind) :: i
!=============================================================================
r=0.0_dp; if(lgen==0)r=hil4(lgen)
p=o4
do i=1,ngen
   r=r+p*hil4(i)
   p=p*o4
enddo
end subroutine hil4_to_rz_d

!=============================================================================
subroutine hil8_to_rz_d(lgen,ngen,hil8,r)!                        [hil8_to_rz]
!=============================================================================
! Deprecated; replace by hil8_to_r with r also predefined
implicit none
integer(i_kind),                     intent(IN ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(IN ):: hil8
real(dp),                    intent(OUT):: r
!-----------------------------------------------------------------------------
real(dp):: p
integer(i_kind) :: i
!=============================================================================
r=0.0_dp; if(lgen==0)r=hil8(lgen)
p=o8
do i=1,ngen
   r=r+p*hil8(i)
   p=p*o8
enddo
end subroutine hil8_to_rz_d

!=============================================================================
subroutine hil16_to_rz_d(lgen,ngen,hil16,r)!                     [hil16_to_rz]
!=============================================================================
! Deprecated; replace by hil16_to_r with r also predefined
implicit none
integer(i_kind),                     intent(IN ):: lgen,ngen
integer(i_kind),dimension(lgen:ngen),intent(IN ):: hil16
real(dp),                    intent(OUT):: r
!-----------------------------------------------------------------------------
real(dp):: p
integer(i_kind) :: i
!=============================================================================
r=0.0_dp; if(lgen==0)r=hil16(lgen)
p=o16
do i=1,ngen
   r=r+p*hil16(i)
   p=p*o16
enddo
end subroutine hil16_to_rz_d

!=============================================================================
subroutine xy_to_hil4z_s(ngen,x,y,hil4)!                          [xy_to_hil4]
!=============================================================================
! DEPRECATED (since ngen is a redundant variable)
! Convert an (x,y)-representation of a point in the proper interior of the
! unit square to an ngen-digit base-4 representation of the parameter of 
! a space-filling Hilbert curve.
!=============================================================================
implicit none
integer(i_kind),                intent(IN ):: ngen
real(sp),               intent(IN ):: x,y
integer(i_kind),dimension(ngen),intent(OUT):: hil4
!-----------------------------------------------------------------------------
real(sp):: xr,yr
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y
call xy_to_dig4(xr,yr,hil4)
presor=0
call dig4_to_hil4(presor,hil4)
end subroutine xy_to_hil4z_s

!=============================================================================
subroutine xy_to_hil4z_d(ngen,x,y,hil4)!                          [xy_to_hil4]
!=============================================================================
! DEPRECATED (since ngen is a redundant variable)
implicit none
integer(i_kind),                intent(IN ):: ngen
real(dp),               intent(IN ):: x,y
integer(i_kind),dimension(ngen),intent(OUT):: hil4
!-----------------------------------------------------------------------------
real(dp):: xr,yr
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y
call xy_to_dig4(xr,yr,hil4)
presor=0
call dig4_to_hil4(presor,hil4)
end subroutine xy_to_hil4z_d
!=============================================================================
subroutine xyz_to_hil8z_d(ngen,x,y,z,hil8)!                      [xyz_to_hil8]
!=============================================================================
! DEPRECATED (since ngen is a redundant variable)
implicit none
integer(i_kind),                intent(IN ):: ngen
real(dp),               intent(IN ):: x,y,z
integer(i_kind),dimension(ngen),intent(OUT):: hil8
!-----------------------------------------------------------------------------
real(dp):: xr,yr,zr
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y; zr=z
call xyz_to_dig8(xr,yr,zr,hil8)
presor=0
call dig8_to_hil8(presor,hil8)
end subroutine xyz_to_hil8z_d

!=============================================================================
subroutine xyza_to_hil16z_d(ngen,x,y,z,a,hil16)!               [xyza_to_hil16]
!=============================================================================
! DEPRECATED (since ngen is a redundant variable)
implicit none
integer(i_kind),                intent(IN ):: ngen
real(dp),               intent(IN ):: x,y,z,a
integer(i_kind),dimension(ngen),intent(OUT):: hil16
!-----------------------------------------------------------------------------
real(dp):: xr,yr,zr,ar
integer(i_kind) :: presor
!=============================================================================
xr=x; yr=y; zr=z; ar=a
call xyza_to_dig16(xr,yr,zr,ar,hil16)
presor=0
call dig16_to_hil16(presor,hil16)
end subroutine xyza_to_hil16z_d

end module phil0
  
