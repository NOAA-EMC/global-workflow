module stppmslmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppmslmod    module for stppmsl
!  prgmmr:
!
! abstract: module for stppmsl
!
! program history log:
!   2014-04-10  pondeca
!   2015-07-10  pondeca  - force return if no pmsl data available
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stppmsl
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode , only: obsNode
use m_pmslNode, only: pmslNode
use m_pmslNode, only: pmslNode_typecast
use m_pmslNode, only: pmslNode_nextcast
implicit none

PRIVATE
PUBLIC stppmsl

contains

subroutine stppmsl(pmslhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppmsl      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for pressure at mean sea level
!            with addition of nonlinear qc
!
! program history log:
!   2014-03-19  pondeca
!
!   input argument list:
!     pmslhead
!     rpmsl     - search direction for pmsl
!     spmsl     - analysis increment for pmsl
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional pmsl - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: pmslhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_pmsl,pmsl,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_pmsl
  real(r_kind),pointer,dimension(:) :: spmsl
  real(r_kind),pointer,dimension(:) :: rpmsl
  type(pmslNode), pointer :: pmslptr

  out=zero_quad

! If no pmsl data return
  if(.not. associated(pmslhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'pmsl',spmsl,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'pmsl',rpmsl,istatus);ier=istatus+ier
  if(ier/=0)return

  !pmslptr => pmslhead
  pmslptr => pmslNode_typecast(pmslhead)
  do while (associated(pmslptr))
     if(pmslptr%luse)then
        if(nstep > 0)then
           j1=pmslptr%ij(1)
           j2=pmslptr%ij(2)
           j3=pmslptr%ij(3)
           j4=pmslptr%ij(4)
           w1=pmslptr%wij(1)
           w2=pmslptr%wij(2)
           w3=pmslptr%wij(3)
           w4=pmslptr%wij(4)

           val =w1*rpmsl(j1)+w2*rpmsl(j2)+w3*rpmsl(j3)+w4*rpmsl(j4)
           val2=w1*spmsl(j1)+w2*spmsl(j2)+w3*spmsl(j3)+w4*spmsl(j4)-pmslptr%res

           do kk=1,nstep
              pmsl=val2+sges(kk)*val
              pen(kk)= pmsl*pmsl*pmslptr%err2
           end do
        else
           pen(1)=pmslptr%res*pmslptr%res*pmslptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pmslptr%pg > tiny_r_kind .and.  &
                             pmslptr%b  > tiny_r_kind) then
           pg_pmsl=pmslptr%pg*varqc_iter
           cg_pmsl=cg_term/pmslptr%b
           wnotgross= one-pg_pmsl
           wgross = pg_pmsl*cg_pmsl/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*pmslptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*pmslptr%raterr2
        end do
     end if

     !pmslptr => pmslptr%llpoint
     pmslptr => pmslNode_nextcast(pmslptr)

  end do
  
  return
end subroutine stppmsl

end module stppmslmod
