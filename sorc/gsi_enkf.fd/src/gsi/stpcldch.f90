module stpcldchmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpcldchmod    module for stpcldch
!  prgmmr:
!
! abstract: module for stpcldch
!
! program history log:
!   2015-07-10  pondeca
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpcldch
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode  , only: obsNode
use m_cldchNode, only: cldchNode
use m_cldchNode, only: cldchNode_typecast
use m_cldchNode, only: cldchNode_nextcast
implicit none

PRIVATE
PUBLIC stpcldch

contains

subroutine stpcldch(cldchhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpcldch      calculate penalty and contribution to stepsize
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: calculate penalty and contribution to stepsize for cloud ceiling  height
!            with addition of nonlinear qc
!
! program history log:
!   2015-07-10  pondeca  - update
!
!   input argument list:
!     cldchhead
!     rcldch     - search direction for cldch
!     scldch     - analysis increment for cldch
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional cldch - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: cldchhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_cldch,cldch,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_cldch
  real(r_kind),pointer,dimension(:) :: scldch
  real(r_kind),pointer,dimension(:) :: rcldch
  type(cldchNode), pointer :: cldchptr

  out=zero_quad

! If no cldch data return
  if(.not. associated(cldchhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'cldch',scldch,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'cldch',rcldch,istatus);ier=istatus+ier
  if(ier/=0)return

  !cldchptr => cldchhead
  cldchptr => cldchNode_typecast(cldchhead)
  do while (associated(cldchptr))
     if(cldchptr%luse)then
        if(nstep > 0)then
           j1=cldchptr%ij(1)
           j2=cldchptr%ij(2)
           j3=cldchptr%ij(3)
           j4=cldchptr%ij(4)
           w1=cldchptr%wij(1)
           w2=cldchptr%wij(2)
           w3=cldchptr%wij(3)
           w4=cldchptr%wij(4)

           val =w1*rcldch(j1)+w2*rcldch(j2)+w3*rcldch(j3)+w4*rcldch(j4)
           val2=w1*scldch(j1)+w2*scldch(j2)+w3*scldch(j3)+w4*scldch(j4)-cldchptr%res

           do kk=1,nstep
              cldch=val2+sges(kk)*val
              pen(kk)= cldch*cldch*cldchptr%err2
           end do
        else
           pen(1)=cldchptr%res*cldchptr%res*cldchptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. cldchptr%pg > tiny_r_kind .and.  &
                             cldchptr%b  > tiny_r_kind) then
           pg_cldch=cldchptr%pg*varqc_iter
           cg_cldch=cg_term/cldchptr%b
           wnotgross= one-pg_cldch
           wgross = pg_cldch*cg_cldch/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*cldchptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*cldchptr%raterr2
        end do
     end if

     !cldchptr => cldchptr%llpoint
     cldchptr => cldchNode_nextcast(cldchptr)

  end do
  
  return
end subroutine stpcldch

end module stpcldchmod
