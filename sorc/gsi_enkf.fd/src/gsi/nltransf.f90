module nltransf

!$$$   module documentation block
! module:    nltransf
! program history log:
! 2018-01-18 yang/Guo ! Jim Purser's nonlinear transformation for vis and cldch.
!  will work on the document late 
!$$$   end documentation block
!                .      .    .                                       .

  use kinds, only: r_kind, i_kind
  use constants, only: zero

  implicit none
  private

  public:: nltransf_forward
        interface nltransf_forward ; module procedure  forward; end interface
  public:: nltransf_inverse
        interface nltransf_inverse ; module procedure  inverse; end interface

!

CONTAINS

subroutine forward(zin,zout,powerp,scale_cv)

!--------------------------------------------------------------
! input argument:
!   zin - vis or cldch
!   powerp - parameter for nltr
! output argument:
!   zout - the vis or cldch after the nonlinear transformation 
!--------------------------------------------------------------
  implicit none
  real(r_kind),intent(in):: zin
  real(r_kind),intent(in):: powerp
  real(r_kind),intent(in):: scale_cv
  real(r_kind)           :: zout

! local variable
  real(r_kind) :: temp                      ! after the nltransformation
! do not choose negative powerp 
  if (abs(powerp) > zero) then
    !non-log conversion
     temp =(zin/scale_cv)**powerp
     zout =(temp-1.0_r_kind)/powerp
  else
    !log conversion
     zout=log(zin/scale_cv)
  endif
  return
end subroutine forward

subroutine inverse(zin,zout,powerp,scale_cv)
  implicit none
  real(r_kind),intent(in):: zin
  real(r_kind),intent(in):: powerp
  real(r_kind),intent(in):: scale_cv
  real(r_kind)           :: zout

! Local variable
  real(r_kind) :: powerpinv
  real(r_kind) :: z1

!change zin from nltr space back to physical space
  if (abs(powerp)> zero) then
    ! non-log conversion
     powerpinv=1.0_r_kind/powerp
     z1=(powerp*zin + 1.0_r_kind)
     z1=z1**powerpinv
     zout=z1*scale_cv
  else
    ! log conversion
    zout=exp(zin)*scale_cv
  endif
  return
end subroutine inverse

end module nltransf
