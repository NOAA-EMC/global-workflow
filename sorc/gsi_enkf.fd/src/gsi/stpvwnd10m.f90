module stpvwnd10mmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpvwnd10mmod    module for stpvwnd10m
!  prgmmr:
!
! abstract: module for stpvwnd10m
!
! program history log:
!   2016-05-05  pondeca
!   2017-03-19  yang     - modify code to use polymorphic obsNode
!
! subroutines included:
!   sub stpvwnd10m
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode    , only: obsNode
use m_vwnd10mNode, only: vwnd10mNode
use m_vwnd10mNode, only: vwnd10mNode_typecast
use m_vwnd10mNode, only: vwnd10mNode_nextcast

implicit none

PRIVATE
PUBLIC stpvwnd10m

contains

subroutine stpvwnd10m(vwnd10mhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpvwnd10m      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for 10m-vwind
!            with addition of nonlinear qc
!
! program history log:
!   2016-05-05  pondeca
!
!   input argument list:
!     vwnd10mhead
!     rvwnd10m     - search direction for vwnd10m
!     svwnd10m     - analysis increment for vwnd10m
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional vwnd10m - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: vwnd10mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_vwnd10m,vwnd10m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_vwnd10m
  real(r_kind),pointer,dimension(:) :: svwnd10m
  real(r_kind),pointer,dimension(:) :: rvwnd10m
  type(vwnd10mNode), pointer :: vwnd10mptr

  out=zero_quad

! If no vwnd10m data return
  if(.not. associated(vwnd10mhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vwnd10m',svwnd10m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vwnd10m',rvwnd10m,istatus);ier=istatus+ier
  if(ier/=0)return

! vwnd10mptr => vwnd10mhead
  vwnd10mptr => vwnd10mNode_typecast(vwnd10mhead)
  do while (associated(vwnd10mptr))
     if(vwnd10mptr%luse)then
        if(nstep > 0)then
           j1=vwnd10mptr%ij(1)
           j2=vwnd10mptr%ij(2)
           j3=vwnd10mptr%ij(3)
           j4=vwnd10mptr%ij(4)
           w1=vwnd10mptr%wij(1)
           w2=vwnd10mptr%wij(2)
           w3=vwnd10mptr%wij(3)
           w4=vwnd10mptr%wij(4)

           val =w1*rvwnd10m(j1)+w2*rvwnd10m(j2)+w3*rvwnd10m(j3)+w4*rvwnd10m(j4)
           val2=w1*svwnd10m(j1)+w2*svwnd10m(j2)+w3*svwnd10m(j3)+w4*svwnd10m(j4)-vwnd10mptr%res

           do kk=1,nstep
              vwnd10m=val2+sges(kk)*val
              pen(kk)= vwnd10m*vwnd10m*vwnd10mptr%err2
           end do
        else
           pen(1)=vwnd10mptr%res*vwnd10mptr%res*vwnd10mptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. vwnd10mptr%pg > tiny_r_kind .and.  &
                             vwnd10mptr%b  > tiny_r_kind) then
           pg_vwnd10m=vwnd10mptr%pg*varqc_iter
           cg_vwnd10m=cg_term/vwnd10mptr%b
           wnotgross= one-pg_vwnd10m
           wgross = pg_vwnd10m*cg_vwnd10m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*vwnd10mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*vwnd10mptr%raterr2
        end do
     end if

!    vwnd10mptr => vwnd10mptr%llpoint
     vwnd10mptr => vwnd10mNode_nextcast(vwnd10mptr)

  end do
  
  return
end subroutine stpvwnd10m

end module stpvwnd10mmod
