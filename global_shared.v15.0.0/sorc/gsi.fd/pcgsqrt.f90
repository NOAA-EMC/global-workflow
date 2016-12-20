subroutine pcgsqrt(xhat,costf,gradx,itermax,nprt)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcgsqrt
!   prgmmr:      tremolet
!
! abstract: solve inner loop of analysis equation using conjugate
!           gradient preconditioned with sqrt(B).
!
! program history log:
!   2007-04-27  tremolet - initial code
!   2009-01-14  todling  - zero-obs fix
!   2009-01-18  todling  - carry summations in quad precision
!   2010-08-19  lueken   - add only to module use
!   2010-10-01  el akkraoui - add orthogonalization using first few iterates
!   2011-03-02  todling   - revisit memory release: first-in, last-out
!   2012-02-08  el akkraoui - fix estimate (remove xhat) per Selime Gurol
!
!   input argument lits:
!    xhat,gradx
!    costf
!    itermax,nprt
!
!   output argument list:
!    xhat,gradx
!    costf
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use jfunc, only: iter,jiter
use gsi_4dvar, only: iorthomax
use constants, only: zero,zero_quad,tiny_r_kind
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv, &
    deallocate_cv,dot_product,assignment(=)
use timermod,only: timer_ini,timer_fnl

implicit none

! Declare arguments
type(control_vector), intent(inout) :: xhat,gradx
real(r_kind)        , intent(inout) :: costf
integer(i_kind)     , intent(in   ) :: itermax,nprt

! Declare local variables
character(len=*), parameter :: myname='pcgsqrt'
type(control_vector)        :: dirx,xtry,grtry,grad0,gradf
type(control_vector), allocatable, dimension(:) :: cglwork
real(r_quad)                :: beta,alpha,zg0,zgk,zgnew,zfk,dkqk,zdla
integer(i_kind)             :: ii,jj,iortho
logical                     :: lsavinc

!**********************************************************************
call timer_ini('pcgsqrt')

! Allocate local variables
call allocate_cv(dirx)
call allocate_cv(xtry)
call allocate_cv(grtry)
call allocate_cv(grad0)
if (nprt>=1) call allocate_cv(gradf)

! Initializations
dirx=zero
beta=zero_quad
lsavinc=.false.

if(iorthomax>0) then 
   allocate(cglwork(iorthomax+1))
   do ii=1,iorthomax+1
      call allocate_cv(cglwork(ii))
      cglwork(ii)=zero
   enddo
end if 

! Save initial cost function and gradient
grad0=gradx
zg0=dot_product(grad0,grad0,r_quad)
zgk=zg0

if(iorthomax>0) then
  do ii=1,dirx%lencv
     cglwork(1)%values(ii)=gradx%values(ii)/sqrt(zg0)
  end do
end if

! Perform inner iteration
inner_iteration: do iter=1,itermax
   if (mype==0) write(6,*)trim(myname),': Minimization iteration',iter

!  Search direction
   do ii=1,dirx%lencv
      dirx%values(ii)=-gradx%values(ii)+beta*dirx%values(ii)
   end do

!  Estimate
   do ii=1,xtry%lencv
      xtry%values(ii)=dirx%values(ii)
   end do

!  Evaluate cost and gradient
   call evaljgrad(xtry,zfk,grtry,lsavinc,0,myname)

!  Get A q_k
   do ii=1,grtry%lencv
      grtry%values(ii)=grtry%values(ii)-grad0%values(ii)
   end do

!  Calculate stepsize
   dkqk=dot_product(dirx,grtry,r_quad)
   alpha=zero_quad
   if(abs(dkqk)>tiny_r_kind) alpha = zgk/dkqk

!  Update estimates
   do ii=1,xhat%lencv
      xhat%values(ii) =xhat%values(ii) +alpha* dirx%values(ii)
      gradx%values(ii)=gradx%values(ii)+alpha*grtry%values(ii)
   end do

!  Orthogonormalize against previous gradient 
   if(iorthomax>0) then 
      iortho=min(iter,iorthomax)
      do jj=iortho,1,-1
        zdla = dot_product(gradx,cglwork(jj))
        do ii=1,gradx%lencv
           gradx%values(ii) = gradx%values(ii) - zdla*cglwork(jj)%values(ii)
        enddo
     enddo
   end if 

!  Save gradients for orthonormalization
   zgnew=dot_product(gradx,gradx,r_quad)
   if(iorthomax>0 .and. iter <= iortho) then 
      do ii=1,xhat%lencv
         cglwork(iter+1)%values(ii) = gradx%values(ii)/sqrt(zgnew)
      enddo   
   end if
!
   beta=zero_quad
   if(abs(zgk)>tiny_r_kind) beta=zgnew/zgk
   zgk=zgnew

!  Evaluate cost for printout
   if (nprt>=1) call evaljgrad(xhat,zfk,gradf,lsavinc,nprt,myname)

   if (mype==0) then
      if (abs(zg0)>tiny_r_kind) then
         write(6,999)trim(myname),': grepgrad grad,reduction,step=',jiter,iter,sqrt(real(zgk,r_kind)),&
              sqrt(real(zgk,r_kind)/real(zg0,r_kind)),real(alpha,r_kind)
      else
         write(6,999)trim(myname),': grepgrad grad,reduction,step=',jiter,iter,sqrt(real(zgk,r_kind)),&
              zero,real(alpha,r_kind)
      endif
   endif

end do inner_iteration
costf=zfk

! Release memory
if(iorthomax>0) then 
  do ii=iorthomax+1,1,-1
     call deallocate_cv(cglwork(ii))
  enddo
  deallocate(cglwork)
end if 
if (nprt>=1) call deallocate_cv(gradf)
call deallocate_cv(grad0)
call deallocate_cv(grtry)
call deallocate_cv(xtry)
call deallocate_cv(dirx)

999 format(2A,2(1X,I3),3(1X,ES25.18))

call timer_fnl('pcgsqrt')
return
end subroutine pcgsqrt
