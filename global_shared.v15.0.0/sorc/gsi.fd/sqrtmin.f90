subroutine sqrtmin()

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sqrtmin
!   prgmmr: tremolet
!
! abstract: Minimize cost function using sqrt(B) preconditioner
!
! program history log:
!   2007-05-01  tremolet - initial code
!   2007-11-23  todling  - add timers
!   2008-01-04  tremolet - forecast sensitivity option
!   2009-01-18  todling  - calc dot-prod in quad precision
!   2009-08-12  lueken   - updated documentation
!   2010-05-15  todling  - add only for all used variables
!   2010-10-20  hclin    - added prt_guesschem for aod
!   2012-02-15  todling  - allow pcgsqrt to echo true cost when requested
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$

use kinds, only: r_kind,i_kind,r_quad
use gsi_4dvar, only: l4dvar, lsqrtb, lcongrad, lbfgsmin, ltlint, &
                     ladtest, lgrtest, lanczosave, jsiga, nwrvecs, ltcost
use jfunc, only: jiter,miter,niter,xhatsave,jiterstart
use constants, only: zero
use mpimod, only: mype
use obs_sensitivity, only: lobsensadj, lobsensmin, lobsensfc, lobsensincr, &
                           iobsconv, fcsens, llancdone, dot_prod_obs
use obsmod, only: lsaveobsens,l_do_adjoint
use qnewton3, only: m1qn3
use lanczos, only: congrad,setup_congrad,save_precond,congrad_ad,read_lanczos
use adjtest, only: adtest
use grdtest, only: grtest
use control_vectors, only: control_vector
use control_vectors, only: allocate_cv,deallocate_cv,write_cv,inquire_cv
use control_vectors, only: dot_product,assignment(=)
use obs_ferrscale, only: lferrscale, apply_hrm1h
use timermod, only: timer_ini, timer_fnl

implicit none

character(len=*), parameter :: myname='sqrtmin'

! Declare local variables  
type(control_vector) :: xhat, gradx, gradf
real(r_kind) :: costf,eps,zy
real(r_quad) :: zf0,zg0,zff,zgf,zge,zgg,rdx
integer(i_kind) :: nprt,itermax,ii,itest,iprt
logical :: lsavinc, lsavev
character(len=20) :: what
character(len=12) :: clfile

!**********************************************************************

if (.not.lsqrtb) then
   write(6,*)'sqrtmin: lsqrtb false'
   call stop2(307)
end if

! Initialize timer
call timer_ini('sqrtmin')

! Initialize
lsavinc=.false.

! Allocate control vectors
call allocate_cv(xhat)
call allocate_cv(gradx)

! Run tests if required
rdx=1.0e-10_r_quad

itest=8
if (lgrtest) call grtest(rdx,itest)
if (ladtest) call adtest()
zgg=dot_product(xhatsave,xhatsave,r_quad)
if (mype==0) write(6,888)trim(myname),': Norm xhatsave=',sqrt(zgg)
call prt_guess('guess')
call prt_guesschem('chemguess')
if (lobsensfc.and.lobsensmin) lsaveobsens=.true.

! Get initial cost function and gradient
nprt=2
xhat=zero
call evaljgrad(xhat,zf0,gradx,lsavinc,nprt,myname)

zg0=dot_product(gradx,gradx,r_quad)
zg0=sqrt(zg0)
if (mype==0) then
   write(6,888)trim(myname),': Initial cost function =',zf0
   write(6,888)trim(myname),': Initial gradient norm =',zg0
endif

! Minimization
nprt=1
itermax=niter(jiter)
eps=1.0e-8_r_kind
costf=zf0

if (lbfgsmin) then
   call m1qn3  (xhat,costf,gradx,eps,itermax,nprt,nwrvecs)

elseif (lcongrad) then
!  Setup CONGRAD
   if (.not.ltlint) then
      write(6,*)'sqrtmin: congrad requires ltlint'
      call stop2(308)
   end if
   call setup_congrad(mype,nprt,jiter,jiterstart,itermax,nwrvecs, &
                      l4dvar,lanczosave,ltcost)
   lsavev=(.not.lobsensfc)
   if(jsiga<miter) lsavev=lsavev.and.(jiter<miter)

   if (lobsensfc.and.lobsensadj) then
!     Get Lanczos vectors
      if ( llancdone ) then        ! Lanczos vecs already computed, read them in
         call read_lanczos(itermax)
      else                         ! Do forward min to get Lanczos vectors
         call congrad(xhat,costf,gradx,eps,itermax,iobsconv,lsavev)
      endif

!     Compute sensitivity
      zgg=dot_product(fcsens,fcsens,r_quad)
      if (mype==0) write(6,888)trim(myname),': Norm fcsens=',sqrt(zgg)
      xhat=fcsens
      call congrad_ad(xhat,itermax)
   else
!     Compute increment
      call congrad(xhat,costf,gradx,eps,itermax,iobsconv,lsavev)

!     Calculate estimate of analysis errors
      if(jsiga==jiter) call getsiga()
   endif

!  Finish CONGRAD
   call save_precond(lsavev)

else  ! plain conjugate gradient
   if (.not.ltlint) then
      write(6,*)'sqrtmin: pcgsqrt requires ltlint'
      call stop2(309)
   end if
   iprt=0
   if (ladtest .or. lgrtest .or. ltcost) iprt=1
   call pcgsqrt(xhat,costf,gradx,itermax,iprt)
endif

if (mype==0) write(6,*)trim(myname),': Minimization final diagnostics'

if (lferrscale .and. jiter==miter) call apply_hrm1h(2)

! Get final cost function and gradient
! and save increment (or update guess)
if (lobsensfc) then
   lsaveobsens=.true.
   l_do_adjoint=.false.
   nprt=1
else
   lsavinc=.true.
   nprt=2
endif
call allocate_cv(gradf)
gradf=zero
call evaljgrad(xhat,zff,gradf,lsavinc,nprt,myname)

if (lobsensfc) then
!  Compute <dJ/dy,d>
   if (lobsensincr) then
      call test_obsens(fcsens,xhat)
   else
      zy=dot_prod_obs()
      if (mype==0) write(6,'(A,ES25.18)')'Obs impact <dF/dy,d>= ',zy
   endif
!  Save gradient for next (backwards) outer loop
   if (jiter>1) then
      do ii=1,xhat%lencv
         fcsens%values(ii) = fcsens%values(ii) - xhat%values(ii)
      enddo
      clfile='fgsens.ZZZ'
      write(clfile(8:10),'(I3.3)') jiter
      call write_cv(fcsens,clfile)
   endif
   lsaveobsens=.false.
   l_do_adjoint=.true.

else
!  Update xhatsave
   do ii=1,xhat%lencv
      xhatsave%values(ii) = xhatsave%values(ii) + xhat%values(ii)
   end do

   zgg=dot_product(xhat,xhat,r_quad)
   if (mype==0) write(6,888)trim(myname),': Norm xhat=',sqrt(zgg)
   zgg=dot_product(xhatsave,xhatsave,r_quad)
   if (mype==0) write(6,888)trim(myname),': Norm xhatsave=',sqrt(zgg)

!  Print diagnostics
   zgf=dot_product(gradf,gradf,r_quad)
   zgf=sqrt(zgf)
   if (mype==0) then
      write(6,888)trim(myname),': Final cost function =',real(zff,r_kind)
      write(6,888)trim(myname),': Final gradient norm =',real(zgf,r_kind)
      write(6,888)trim(myname),': Final/Initial cost function=',real(zff,r_kind)/real(zf0,r_kind)
      write(6,888)trim(myname),': Final/Initial gradient norm=',real(zgf,r_kind)/real(zg0,r_kind)
   endif

   if (l4dvar) then
      what ='increment'
   else
      what ='analysis'
   endif
   call prt_guess(trim(what))
   call prt_guesschem(trim(what))

   zge=dot_product(gradx,gradx,r_quad)
   zge=sqrt(zge)

   do ii=1,xhat%lencv
      gradf%values(ii) = gradf%values(ii) - gradx%values(ii)
   end do
   zgg=dot_product(gradf,gradf,r_quad)
   zgg=sqrt(zgg)
   if (mype==0) then
      write(6,888)trim(myname),': Gradient norm estimated,actual,error:',zge,zgf,zgg
   endif

   if (zgg>0.1_r_quad) then
      if (mype==0) write(6,*)'*** sqrtmin: inaccurate estimated gradient ***'
!     With conjugate gradient, estimated gradient norm will differ from
!     actual gradient norm without re-orthogonalisation so don't abort.
!yt    if (lcongrad.or.lbfgsmin) then
!         write(6,*)'sqrtmin: error estimated gradient',lcongrad,lbfgsmin
!         call stop2(310)
!      end if
   endif

   if (lgrtest) call grtest(rdx,itest,xhat)  
   if (ladtest) call adtest(xhat)

endif

! Release memory
call deallocate_cv(xhat)
call deallocate_cv(gradx)
call deallocate_cv(gradf)
call inquire_cv

! Finalize timer
call timer_fnl('sqrtmin')

888 format(2A,3(1X,ES25.18))

return
end subroutine sqrtmin
