module stpwspd10mmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpwspd10mmod    module for stpwspd10m
!  prgmmr:
!
! abstract: module for stpwspd10m
!
! program history log:
!   2014-03-19  pondeca
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpwspd10m
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode    , only: obsNode
use m_wspd10mNode, only: wspd10mNode
use m_wspd10mNode, only: wspd10mNode_typecast
use m_wspd10mNode, only: wspd10mNode_nextcast
implicit none

PRIVATE
PUBLIC stpwspd10m

contains

subroutine stpwspd10m(wspd10mhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpwspd10m      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for 10m-wind speed
!            with addition of nonlinear qc
!
! program history log:
!   2014-03-19  pondeca  - update
!   2015-07-10  pondeca  - force return if no wspd10m data available
!
!   input argument list:
!     wspd10mhead
!     rwspd10m     - search direction for wspd10m
!     swspd10m     - analysis increment for wspd10m
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional wspd10m - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  class(obsNode),pointer              ,intent(in   ) :: wspd10mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_wspd10m,wspd10m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_wspd10m
  real(r_kind),pointer,dimension(:) :: swspd10m
  real(r_kind),pointer,dimension(:) :: rwspd10m
  type(wspd10mNode), pointer :: wspd10mptr

  out=zero_quad

! If no wspd10m data return
  if(.not. associated(wspd10mhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'wspd10m',swspd10m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'wspd10m',rwspd10m,istatus);ier=istatus+ier
  if(ier/=0)return

  !wspd10mptr => wspd10mhead
  wspd10mptr => wspd10mNode_typecast(wspd10mhead)
  do while (associated(wspd10mptr))
     if(wspd10mptr%luse)then
        if(nstep > 0)then
           j1=wspd10mptr%ij(1)
           j2=wspd10mptr%ij(2)
           j3=wspd10mptr%ij(3)
           j4=wspd10mptr%ij(4)
           w1=wspd10mptr%wij(1)
           w2=wspd10mptr%wij(2)
           w3=wspd10mptr%wij(3)
           w4=wspd10mptr%wij(4)

           val =w1*rwspd10m(j1)+w2*rwspd10m(j2)+w3*rwspd10m(j3)+w4*rwspd10m(j4)
           val2=w1*swspd10m(j1)+w2*swspd10m(j2)+w3*swspd10m(j3)+w4*swspd10m(j4)-wspd10mptr%res

           do kk=1,nstep
              wspd10m=val2+sges(kk)*val
              pen(kk)= wspd10m*wspd10m*wspd10mptr%err2
           end do
        else
           pen(1)=wspd10mptr%res*wspd10mptr%res*wspd10mptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. wspd10mptr%pg > tiny_r_kind .and.  &
                             wspd10mptr%b  > tiny_r_kind) then
           pg_wspd10m=wspd10mptr%pg*varqc_iter
           cg_wspd10m=cg_term/wspd10mptr%b
           wnotgross= one-pg_wspd10m
           wgross = pg_wspd10m*cg_wspd10m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*wspd10mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*wspd10mptr%raterr2
        end do
     end if

     !wspd10mptr => wspd10mptr%llpoint
     wspd10mptr => wspd10mNode_nextcast(wspd10mptr)

  end do
  
  return
end subroutine stpwspd10m

end module stpwspd10mmod
