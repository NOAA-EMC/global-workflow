subroutine precond(grady)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    precond  change of preconditioner
!   prgmmr: zhu               org: np22                date: 2009-10-29
!
! abstract: change of preconditioner (Dee's paper)
!          
!
! program history log:
!   2009-10-29  zhu - change of preconditioner 
!
!   input argument list:
!     grady    - input field  
!
!   output
!     grady    - additional preconditioner structure * grady 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use berror, only: vprecond
  use gsi_4dvar, only: lsqrtb
  use radinfo, only: newpc4pred
  use control_vectors, only: control_vector
  use timermod, only: timer_ini,timer_fnl
  implicit none

! Declare passed variables
  type(control_vector),intent(inout):: grady

! Declare local variables
  integer(i_kind) i

! if(newpc4pred)then
    if (lsqrtb) then
      write(6,*)'precond: not for use with lsqrtb'
      call stop2(334)
    end if


!   Initialize timer
    call timer_ini('precond')

    do i=1,grady%lencv
      grady%values(i)=grady%values(i)*vprecond(i)
    enddo

!   Finalize timer
    call timer_fnl('precond')
! end if



  return
end subroutine precond
