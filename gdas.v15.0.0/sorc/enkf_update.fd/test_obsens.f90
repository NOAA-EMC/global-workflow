subroutine test_obsens(xincr,xsens)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    test_obsens
!   prgmmr: tremolet
!
! abstract: Test accuracy of increment sensitivity to observations
!
! program history log:
!   2007-11-15  tremolet
!   2009-01-18  todling - carry summations in quad precision
!   2009-08-13  lueken - update documentation
!   2010-08-19  lueken - add only to module use;no machine code, so use .f90
!
!   input argument list:
!    xincr - current increment in control space
!    xsens - A_k^{-T}*xincr where A_k is the estimate of the Hessian
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
! ------------------------------------------------------------------------------
use kinds, only: i_kind,r_quad
use constants, only: zero
use jfunc, only: xhatsave
use obsmod, only: lsaveobsens,l_do_adjoint
use mpimod, only: mype
use obs_sensitivity, only: iobsconv, lobsensincr, dot_prod_obs
use control_vectors, only: control_vector,allocate_cv,deallocate_cv, &
    dot_product,assignment(=)
! ------------------------------------------------------------------------------
implicit none
type(control_vector), intent(in   ) :: xincr,xsens

real(r_quad) :: zx,zy,zb,zz,zcost
type(control_vector) :: gwork
integer(i_kind) :: nprt
logical :: lupdfgs

if (.not.(lobsensincr.or.iobsconv>0)) then
   write(6,*)'test_obsens: only for validation',lobsensincr,iobsconv
   call stop2(315)
end if

! Apply R^{-1} H and save output in obsdiags
if (iobsconv>0) then
   lsaveobsens=.true.
   l_do_adjoint=.false.
   nprt=1
   lupdfgs=.false.
   call allocate_cv(gwork)
   gwork=zero

   call evaljgrad(xsens,zcost,gwork,lupdfgs,nprt,'test_obsens')

   call deallocate_cv(gwork)
   lsaveobsens=.false.
   l_do_adjoint=.true.
endif

! Compute <dJ/dy,d>
zy=dot_prod_obs()

! Scalar products in control space
!    <dJ/dx,dx>=<dx,dx>
zx=dot_product(xincr,xincr,r_quad)
!    <A_k^{-1}dx,b_j>
zb=dot_product(xsens,xhatsave,r_quad)

! Observations impact validation
if (mype==0) then
   write(6,'(A)')'Incr sensitivity to obs. 0.123456789012345678'
   write(6,'(A,ES25.18)')'Incr sensit  <dF/dy,d>= ',zy
   write(6,'(A,ES25.18)')'Incr sensit  <b,dx>   = ',zb
   write(6,'(A,ES25.18)')'Incr sensit  <dx,dx>  = ',zx
   if ( abs(zx) > sqrt(tiny(zz)) ) then
      zz=abs((zy-zb-zx)/zx)
      write(6,'(A,ES25.18)')'Incr sensit  rel error= ',zz
   else
      zz=abs(zy-zb-zx)
      write(6,'(A,ES25.18)')'Incr sensit  abs error= ',zz
   endif
endif

return
end subroutine test_obsens
