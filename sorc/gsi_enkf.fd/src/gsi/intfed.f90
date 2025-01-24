module intfedmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intfedmod    module for intfed and its tangent linear intfed_tl
!   prgmmr:
!
! abstract: module for intfed and its tangent linear intfed_tl
!
! program history log:
!   2023-08-24  H. Wang - add tangent linear of fed operator to directly assimilate FED 
!
! subroutines included:
!   sub intfed_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_fedNode, only: fedNode
use m_fedNode, only: fedNode_typecast
use m_fedNode, only: fedNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intfed

interface intfed; module procedure &
          intfed_
end interface

contains

subroutine intfed_(fedhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intfed       apply nonlin qc operator for GLM FED 
!
! abstract: apply observation operator for radar winds
!             with nonlinear qc operator
!
! program history log:
!   2023-08-24  H.Wang   - modified based on intdbz.f90 
!                        - using tangent linear fed operator 

!
!   input argument list:
!     fedhead   - obs type pointer to obs structure     
!     sfed      - current fed solution increment
!
!   output argument list:
!     rfed       - fed results from fed observation operator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  use wrf_vars_mod, only : fed_exist
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: fedhead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
! real(r_kind) penalty
  real(r_kind) val,w1,w2,w3,w4,w5,w6,w7,w8,valfed
  real(r_kind) cg_fed,p0,grad,wnotgross,wgross,pg_fed
  real(r_kind),pointer,dimension(:) :: sfed
  real(r_kind),pointer,dimension(:) :: rfed
  type(fedNode), pointer :: fedptr

!  If no fed obs type data return
  if(.not. associated(fedhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  if(fed_exist)then
    call gsi_bundlegetpointer(sval,'fed',sfed,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'fed',rfed,istatus);ier=istatus+ier
  else
    return
  end if

  if(ier/=0)return


  fedptr => fedNode_typecast(fedhead)
  do while (associated(fedptr))
     j1=fedptr%ij(1)
     j2=fedptr%ij(2)
     j3=fedptr%ij(3)
     j4=fedptr%ij(4)
     j5=fedptr%ij(5)
     j6=fedptr%ij(6)
     j7=fedptr%ij(7)
     j8=fedptr%ij(8)
     w1=fedptr%wij(1)
     w2=fedptr%wij(2)
     w3=fedptr%wij(3)
     w4=fedptr%wij(4)
     w5=fedptr%wij(5)
     w6=fedptr%wij(6)
     w7=fedptr%wij(7)
     w8=fedptr%wij(8)


!    Forward model
     if( fed_exist )then
       val = w1* sfed(j1)+w2* sfed(j2)+w3* sfed(j3)+w4* sfed(j4)+ &
             w5* sfed(j5)+w6* sfed(j6)+w7* sfed(j7)+w8* sfed(j8)
     end if

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*fedptr%raterr2*fedptr%err2
           !-- fedptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(fedptr%diags,jiter=jiter,obssen=grad)

        else
           !-- if (fedptr%luse) fedptr%diags%tldepart(jiter)=val
           if (fedptr%luse) call obsdiagNode_set(fedptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs ) val=val-fedptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. fedptr%pg > tiny_r_kind .and. &
                                fedptr%b  > tiny_r_kind) then
              pg_fed=fedptr%pg*varqc_iter
              cg_fed=cg_term/fedptr%b
              wnotgross= one-pg_fed
              wgross = pg_fed*cg_fed/wnotgross
              p0   = wgross/(wgross+exp(-half*fedptr%err2*val**2))
              val = val*(one-p0)
           endif

           if( ladtest_obs)  then
              grad = val
           else
              grad = val*fedptr%raterr2*fedptr%err2
           end if

        endif

!       Adjoint
        if(fed_exist)then
          valfed = grad
          rfed(j1)=rfed(j1)+w1*valfed
          rfed(j2)=rfed(j2)+w2*valfed
          rfed(j3)=rfed(j3)+w3*valfed
          rfed(j4)=rfed(j4)+w4*valfed
          rfed(j5)=rfed(j5)+w5*valfed
          rfed(j6)=rfed(j6)+w6*valfed
          rfed(j7)=rfed(j7)+w7*valfed
          rfed(j8)=rfed(j8)+w8*valfed
        end if
 
     endif

     !fedptr => fedptr%llpoint
     fedptr => fedNode_nextcast(fedptr)
  end do
  return
end subroutine intfed_

end module intfedmod
