module stpmitmmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpmitmmod    module for stpmitm
!  prgmmr:
!
! abstract: module for stpmitm
!
! program history log:
!   2014-04-10  pondeca
!   2015-07-10  pondeca  - force return if no mitm data available
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpmitm
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode , only: obsNode
use m_mitmNode, only: mitmNode
use m_mitmNode, only: mitmNode_typecast
use m_mitmNode, only: mitmNode_nextcast
implicit none

PRIVATE
PUBLIC stpmitm

contains

subroutine stpmitm(mitmhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpmitm      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for daily minimum temperature
!            with addition of nonlinear qc
!
! program history log:
!   2014-03-19  pondeca
!
!   input argument list:
!     mitmhead
!     rmitm     - search direction for mitm
!     smitm     - analysis increment for mitm
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional mitm - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: mitmhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_mitm,mitm,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_mitm
  real(r_kind),pointer,dimension(:) :: smitm
  real(r_kind),pointer,dimension(:) :: rmitm
  type(mitmNode), pointer :: mitmptr

  out=zero_quad

! If no mitm data return
  if(.not. associated(mitmhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'mitm',smitm,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'mitm',rmitm,istatus);ier=istatus+ier
  if(ier/=0)return

  !mitmptr => mitmhead
  mitmptr => mitmNode_typecast(mitmhead)
  do while (associated(mitmptr))
     if(mitmptr%luse)then
        if(nstep > 0)then
           j1=mitmptr%ij(1)
           j2=mitmptr%ij(2)
           j3=mitmptr%ij(3)
           j4=mitmptr%ij(4)
           w1=mitmptr%wij(1)
           w2=mitmptr%wij(2)
           w3=mitmptr%wij(3)
           w4=mitmptr%wij(4)

           val =w1*rmitm(j1)+w2*rmitm(j2)+w3*rmitm(j3)+w4*rmitm(j4)
           val2=w1*smitm(j1)+w2*smitm(j2)+w3*smitm(j3)+w4*smitm(j4)-mitmptr%res

           do kk=1,nstep
              mitm=val2+sges(kk)*val
              pen(kk)= mitm*mitm*mitmptr%err2
           end do
        else
           pen(1)=mitmptr%res*mitmptr%res*mitmptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. mitmptr%pg > tiny_r_kind .and.  &
                             mitmptr%b  > tiny_r_kind) then
           pg_mitm=mitmptr%pg*varqc_iter
           cg_mitm=cg_term/mitmptr%b
           wnotgross= one-pg_mitm
           wgross = pg_mitm*cg_mitm/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*mitmptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*mitmptr%raterr2
        end do
     end if

     !mitmptr => mitmptr%llpoint
     mitmptr => mitmNode_nextcast(mitmptr)

  end do
  
  return
end subroutine stpmitm

end module stpmitmmod
