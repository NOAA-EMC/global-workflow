!
!                                **********************************************
!                                *             MODULES pmat6                  *
!                                *  R. J. Purser, NOAA/NCEP/EMC          2017 * 
!                                *  jim.purser@noaa.gov                       *
!                                *                                            *
!                                **********************************************
! 3*3 Matrix, and quaternion representations of polyhedral groups and
! their subgroups (a work in progress).
!
! DEPENDENCIES
! Modules: kinds, pietc, peuc
!=============================================================================
module pmat6
!=============================================================================
use kinds, only: dp,i_kind
implicit none

interface getrot24; module procedure getrot24;          end interface
interface getqq48;  module procedure getqq48,getqq48x;  end interface

contains

!=============================================================================
subroutine getrot24(rot24)!                                         [getrot24]
!=============================================================================
! Deliver the set of 24 proper rotation matrices that constitute the "standard"
! representation of the octahedral (or cubic) group.
!=============================================================================
use peuc,  only: qtorot
implicit none
real(dp),dimension(3,3,0:23),intent(out):: rot24
!-----------------------------------------------------------------------------
real(dp),dimension(0:3,0:47):: qq48
integer(i_kind)                     :: i,i2
!=============================================================================
call getqq48(qq48)
do i=0,23
   i2=i*2
   call qtorot(qq48(:,i2),rot24(:,:,i))
enddo
end subroutine getrot24

!=============================================================================
subroutine getqq48(qq48)!                                            [getqq48]
!=============================================================================
! Deliver the set of 48 quaternions that constitute the "standard"
! representation of the binary octahedral group. The first 24 are the
! Hurwitz units and constitute the binary tetrahedral group.
!=============================================================================
use pietc, only: o2,or2
implicit none
real(dp),dimension(0:3,0:47),intent(out):: qq48
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:3,0:7):: h1,h2,h3,h4,h5,h6
data h1/ 2,0,0,0,    -2,0,0,0,     0,2,0,0,     0,-2,0,0, &
         0,0,2,0,     0,0,-2,0,    0,0,0,2,     0,0,0,-2/
data h2/ 1,1,1,1,    -1,-1,-1,-1,  1,1,-1,-1,  -1,-1,1,1, &
         1,-1,1,-1,  -1,1,-1,1,    1,-1,-1,1,  -1,1,1,-1/
data h3/ 1,-1,-1,-1, -1,1,1,1,    -1,1,-1,-1,   1,-1,1,1, &
        -1,-1,1,-1,   1,1,-1,1,   -1,-1,-1,1,   1,1,1,-1/
data h4/ 1,1,0,0,    -1,-1,0,0,    1,-1,0,0,   -1,1,0,0,  &
         0,0,1,1,     0,0,-1,-1,   0,0,1,-1,    0,0,-1,1/
data h5/ 1,0,1,0,    -1,0,-1,0,    1,0,-1,0,   -1,0,1,0,  &
         0,1,0,1,     0,-1,0,-1,   0,1,0,-1,    0,-1,0,1/
data h6/ 1,0,0,1,    -1,0,0,-1,    1,0,0,-1,   -1,0,0,1,  &
         0,1,1,0,     0,-1,-1,0,   0,1,-1,0,    0,-1,1,0/
!=============================================================================
qq48(:, 0: 7)=h1*o2;  qq48(:, 8:15)=h2*o2;  qq48(:,16:23)=h3*o2
qq48(:,24:31)=h4*or2; qq48(:,32:39)=h5*or2; qq48(:,40:47)=h6*or2
end subroutine getqq48
!=============================================================================
subroutine getqq48x(qq48,qcage)!                                    [getqq48x]
!=============================================================================
! Like getqq48, but also supply the "cage" quaternions as a set consisting
! of the identity plus the 14 nearest members of qq48 (6 cartesian neighbors
! and 8 diagonal neighbors of the fundamental voronoi cell at the identity).
!=============================================================================
use kinds, only: dp
implicit none
real(dp),dimension(0:3,0:47),intent(out):: qq48
real(dp),dimension(0:3,0:14),intent(out):: qcage
!-----------------------------------------------------------------------------
integer(i_kind),dimension(0:14):: cage
integer(i_kind)                :: i
data cage/0, 24,26,32,34,40,42,  8,10,12,14,16,19,21,23/
!=============================================================================
call getqq48(qq48)
do i=0,14; qcage(:,i)=qq48(:,cage(i)); enddo
end subroutine getqq48x

end module pmat6
