module adjtest

!$$$ module documentation block
!           .      .    .                                       .
! module:   adjtest
!  prgmmr: tremolet
!
! abstract: Routines and data to perform adjoint test
!
! program history log:
!   2007-05-09  tremolet - initial code
!   2009-08-14  lueken   - update documentation
!   2010-01-07  todling  - add test for state-vector
!                        - bias-component only contributes to dotp on pe=0
!   2010-08-19  lueken   - add only to module use
!
! subroutines included:
!   sub adtest
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use gsi_4dvar, only: lsqrtb, nsubwin
use constants, only: zero, two
use jfunc, only: nrclen
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv,random_cv, &
    deallocate_cv,dot_product,assignment(=)
use state_vectors, only: allocate_state,deallocate_state,dot_product
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use bias_predictors, only: predictors,allocate_preds,deallocate_preds, &
    assignment(=)

implicit none
private
public adtest,ltestadj

type(control_vector),save :: xtest1,xtest2
logical :: ltestadj = .false.

! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine adtest(xhat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    adtest
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!    xhat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(control_vector), optional, intent(in   ) :: xhat

! Declare local variables  
type(gsi_bundle) :: stest1(nsubwin),stest2(nsubwin)
type(predictors) :: sbias1,sbias2
integer(i_kind) :: ii,idig
real(r_kind) :: zz1,zz2,zz3

if (mype==0) write(6,*)'ADTEST starting'

! ----------------------------------------------------------------------
! Allocate local variables
call allocate_cv(xtest1)
call allocate_cv(xtest2)
do ii=1,nsubwin
   call allocate_state(stest1(ii))
   call allocate_state(stest2(ii))
end do
call allocate_preds(sbias1)
call allocate_preds(sbias2)

! Initialize control space vectors
if (present(xhat)) then
   xtest1=xhat
   if (mype==0) write(6,*)'ADTEST use input xhat'
else
   call random_cv(xtest1)
   if (mype==0) write(6,*)'ADTEST use random_cv(xhat)'
endif
xtest2=zero

! Initialize state vectors
do ii=1,nsubwin
   stest1(ii)=zero
   stest2(ii)=zero
enddo
sbias1=zero
sbias2=zero

! Run test
if(lsqrtb)then
   call control2model(xtest1,stest1,sbias1)
else
   call control2state(xtest1,stest1,sbias1)
endif
do ii=1,nsubwin
   stest2(ii)=stest1(ii)
enddo
sbias2=sbias1
if(lsqrtb)then
   call control2model_ad(stest2,sbias2,xtest2)
else
   call control2state_ad(stest2,sbias2,xtest2)
endif

! Diagnostics
zz1=dot_product(xtest1,xtest2)

zz2=zero
do ii=1,nsubwin
   zz2=zz2+dot_product(stest1(ii),stest1(ii))
enddo
DO ii=1,nrclen
   zz2=zz2+sbias1%values(ii)*sbias1%values(ii)
ENDDO

if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
   zz3=two*abs(zz1-zz2)/(zz1+zz2)
else
   zz3=abs(zz1-zz2)
endif
idig= int(-log(zz3+tiny(zz3))/log(10.0_r_kind))

if (mype==0) then
   write(6,'(A)')' ADTEST            0.123456789012345678'
   write(6,'(A,ES25.18)')' ADTEST <F*F.Y,X>= ',zz1
   write(6,'(A,ES25.18)')' ADTEST <F.Y,F.Y>= ',zz2
   write(6,'(A,i3,   A)')' ADTEST ',idig,' digits are identical'
   write(6,'(A,ES25.18)')' ADTEST rel. err.= ',zz3
   write(6,'(A,ES25.18)')' ADTEST mach.eps = ',epsilon(zz3)
endif

! Release local variables
call deallocate_cv(xtest1)
call deallocate_cv(xtest2)
do ii=1,nsubwin
   call deallocate_state(stest1(ii))
   call deallocate_state(stest2(ii))
enddo
call deallocate_preds(sbias1)
call deallocate_preds(sbias2)
! ----------------------------------------------------------------------

if (mype==0) write(6,*)'ADTEST finished'

return
end subroutine adtest
! ----------------------------------------------------------------------
end module adjtest
