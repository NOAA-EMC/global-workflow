module stptd2mmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stptd2mmod    module for stptd2m
!  prgmmr:
!
! abstract: module for stptd2m
!
! program history log:
!   2014-04-10  pondeca
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stptd2m
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode , only: obsNode
use m_td2mNode, only: td2mNode
use m_td2mNode, only: td2mNode_typecast
use m_td2mNode, only: td2mNode_nextcast
implicit none

PRIVATE
PUBLIC stptd2m

contains

subroutine stptd2m(td2mhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stptd2m      calculate penalty and contribution to stepsize
!
! abstract: calculate penalty and contribution to stepsize for 2m-dew point
!            with addition of nonlinear qc
!
! program history log:
!   2014-03-19  pondeca
!   2015-07-10  pondeca  - force return if no td2m data available
!
!   input argument list:
!     td2mhead
!     rtd2m     - search direction for td2m
!     std2m     - analysis increment for td2m
!     sges     - step size estimate (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for conventional td2m - sges(1:nstep)
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
  class(obsNode),pointer              ,intent(in   ) :: td2mhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val,val2
  real(r_kind) cg_td2m,td2m,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) pg_td2m
  real(r_kind),pointer,dimension(:) :: std2m
  real(r_kind),pointer,dimension(:) :: rtd2m
  type(td2mNode), pointer :: td2mptr

  out=zero_quad

! If no td2m data return
  if(.not. associated(td2mhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'td2m',std2m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'td2m',rtd2m,istatus);ier=istatus+ier
  if(ier/=0)return

  !td2mptr => td2mhead
  td2mptr => td2mNode_typecast(td2mhead)
  do while (associated(td2mptr))
     if(td2mptr%luse)then
        if(nstep > 0)then
           j1=td2mptr%ij(1)
           j2=td2mptr%ij(2)
           j3=td2mptr%ij(3)
           j4=td2mptr%ij(4)
           w1=td2mptr%wij(1)
           w2=td2mptr%wij(2)
           w3=td2mptr%wij(3)
           w4=td2mptr%wij(4)

           val =w1*rtd2m(j1)+w2*rtd2m(j2)+w3*rtd2m(j3)+w4*rtd2m(j4)
           val2=w1*std2m(j1)+w2*std2m(j2)+w3*std2m(j3)+w4*std2m(j4)-td2mptr%res

           do kk=1,nstep
              td2m=val2+sges(kk)*val
              pen(kk)= td2m*td2m*td2mptr%err2
           end do
        else
           pen(1)=td2mptr%res*td2mptr%res*td2mptr%err2
        end if
 
!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. td2mptr%pg > tiny_r_kind .and.  &
                             td2mptr%b  > tiny_r_kind) then
           pg_td2m=td2mptr%pg*varqc_iter
           cg_td2m=cg_term/td2mptr%b
           wnotgross= one-pg_td2m
           wgross = pg_td2m*cg_td2m/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*td2mptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*td2mptr%raterr2
        end do
     end if

     !td2mptr => td2mptr%llpoint
     td2mptr => td2mNode_nextcast(td2mptr)

  end do
  
  return
end subroutine stptd2m

end module stptd2mmod
