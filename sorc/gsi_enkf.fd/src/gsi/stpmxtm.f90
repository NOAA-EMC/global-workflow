module stpmxtmmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpmxtmmod    module for stpmxtm
!  prgmmr:
!
! abstract: module for stpmxtm
!
! program history log:
!   2014-04-10  pondeca
!   2015-07-10  pondeca  - force return if no mxtm data available
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stpmxtm
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode , only: obsNode
use m_mxtmNode, only: mxtmNode
use m_mxtmNode, only: mxtmNode_typecast
use m_mxtmNode, only: mxtmNode_nextcast
implicit none

PRIVATE
PUBLIC stpmxtm

contains

subroutine stpmxtm(mxtmhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpmxtm      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for daily maximum temperature
!            with addition of nonlinear qc
!
! program history log:
!   2014-03-19  pondeca
!
!   input argument list:
!     mxtmhead
!     rmxtm     - search direction for mxtm
!     smxtm     - analysis increment for mxtm
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional mxtm - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: mxtmhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_mxtm,mxtm,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_mxtm
  real(r_kind),pointer,dimension(:) :: smxtm
  real(r_kind),pointer,dimension(:) :: rmxtm
  type(mxtmNode), pointer :: mxtmptr

  out=zero_quad

! If no mxtm data return
  if(.not. associated(mxtmhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'mxtm',smxtm,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'mxtm',rmxtm,istatus);ier=istatus+ier
  if(ier/=0)return

  !mxtmptr => mxtmhead
  mxtmptr => mxtmNode_typecast(mxtmhead)
  do while (associated(mxtmptr))
     if(mxtmptr%luse)then
        if(nstep > 0)then
           j1=mxtmptr%ij(1)
           j2=mxtmptr%ij(2)
           j3=mxtmptr%ij(3)
           j4=mxtmptr%ij(4)
           w1=mxtmptr%wij(1)
           w2=mxtmptr%wij(2)
           w3=mxtmptr%wij(3)
           w4=mxtmptr%wij(4)

           val =w1*rmxtm(j1)+w2*rmxtm(j2)+w3*rmxtm(j3)+w4*rmxtm(j4)
           val2=w1*smxtm(j1)+w2*smxtm(j2)+w3*smxtm(j3)+w4*smxtm(j4)-mxtmptr%res

           do kk=1,nstep
              mxtm=val2+sges(kk)*val
              pen(kk)= mxtm*mxtm*mxtmptr%err2
           end do
        else
           pen(1)=mxtmptr%res*mxtmptr%res*mxtmptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. mxtmptr%pg > tiny_r_kind .and.  &
                             mxtmptr%b  > tiny_r_kind) then
           pg_mxtm=mxtmptr%pg*varqc_iter
           cg_mxtm=cg_term/mxtmptr%b
           wnotgross= one-pg_mxtm
           wgross = pg_mxtm*cg_mxtm/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*mxtmptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*mxtmptr%raterr2
        end do
     end if

     !mxtmptr => mxtmptr%llpoint
     mxtmptr => mxtmNode_nextcast(mxtmptr)

  end do
  
  return
end subroutine stpmxtm

end module stpmxtmmod
