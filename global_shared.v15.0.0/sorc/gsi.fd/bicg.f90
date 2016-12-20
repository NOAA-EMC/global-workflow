subroutine bicg()

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bicg
!   prgmmr: tremolet
!
! abstract: Solve GSI minimization using the Bi-Conjugate Gradient Method
!
! program history log:
!   2010-10-01  el akkraoui - initial code, based on Tremolet 2009 code
!   2011-04-19  el akkraoui - minor adjustments for preconditioning
!   2012-07-10  todling - add ability to invoke hybrid ensemble
!   2012-12-06  todling - half-backed implementation of adjoint analysis
!                         (for now, only works in single outer loop case)
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds,     only: r_kind,i_kind,r_quad
use gsi_4dvar, only: l4dvar, lsqrtb, ltlint, &
                     ladtest, lgrtest, lanczosave, ltcost, nwrvecs
use jfunc,     only: jiter,miter,niter,xhatsave,yhatsave,jiterstart
use constants, only: zero,tiny_r_kind
use mpimod,    only: mype
use obs_sensitivity, only: lobsensadj, lobsensmin, lobsensfc, lobsensincr, &
                           fcsens, llancdone, dot_prod_obs
use obsmod,    only: lsaveobsens,l_do_adjoint,write_diag
use adjtest,   only: adtest
use grdtest,   only: grtest
use control_vectors, only: control_vector
use control_vectors, only: allocate_cv,deallocate_cv,write_cv,inquire_cv
use control_vectors, only: dot_product,assignment(=)
use obs_ferrscale, only: lferrscale, apply_hrm1h
use hybrid_ensemble_parameters,only : l_hyb_ens,aniso_a_en
use hybrid_ensemble_isotropic, only: beta12mult,bkerror_a_en
use timermod,      only: timer_ini, timer_fnl
use bicglanczos, only:  pcglanczos, setup_pcglanczos, save_pcgprecond, pcgprecond, LMPCGL 

implicit none

character(len=*), parameter :: myname='bicg'

! Declare local variables  
type(control_vector) :: xhat, gradx, gradf,grads
real(r_kind)         :: costf,eps,zy
real(r_quad)         :: zf0,zg0,zff,zgf,zge,zgg,zdx
integer(i_kind)      :: itermax,ii,itest
logical              :: lsavinc, lsavev
character(len=12)    :: clfile

real(r_kind) :: zeps
integer(i_kind) :: jtermax
type(control_vector) :: yhat,grady
real(r_kind)         :: zgk
integer(i_kind)      :: ilen,nprt


!**********************************************************************

!if (.not.ltlint) then
!   write(6,*)'bicg : ltlint false'
!   call stop2(307)
!end if

! Initialize timer
call timer_ini('bicg')

! Initialize
lsavinc=.false.
if (lobsensfc.and.lobsensmin) lsaveobsens=.true.

! Allocate control vectors
call allocate_cv(xhat)
call allocate_cv(yhat)  ! yhatsave used in PCGSOI (updated in STPCALC)
call allocate_cv(gradx)
call allocate_cv(grady)
call allocate_cv(gradf)
call allocate_cv(grads)


! Get initial cost function and gradient
nprt=2
xhat=zero
yhat=zero
call jgrad(xhat,yhat,zf0,gradx,lsavinc,nprt,myname)
if(LMPCGL) then 
   call pcgprecond(gradx,grady)
else 
   call bkerror(gradx,grady)

   ! If hybrid ensemble run, then multiply ensemble control variable a_en 
   !                                 by its localization correlation
   if(l_hyb_ens) then

     if(aniso_a_en) then
   !   call anbkerror_a_en(gradx,grady)    !  not available yet
       write(6,*)' ANBKERROR_A_EN not written yet, program stops'
       call stop2 (999)
     else
       call bkerror_a_en(gradx,grady)
     end if

   ! multiply static (Jb) part of grady by betas_inv(:), and
   ! multiply ensemble (Je) part of grady by betae_inv(:). [Default:  betae_inv(:)  =  1 - betas_inv(:)]
   !   (this determines relative contributions from static background Jb and
   !   ensemble background Je)

     call beta12mult(grady)

   end if
endif

zg0=dot_product(gradx,grady,r_quad)
zgk=zg0
zg0=sqrt(zg0)
if (mype==0) then
   write(6,888)trim(myname),': Initial cost function =',zf0
   write(6,888)trim(myname),': Initial gradient norm =',zg0
endif

! Initializations for minimization
nprt=1
ilen=xhat%lencv
itermax=niter(jiter)
eps=1.0e-8_r_kind
costf=zf0


zeps=eps
jtermax=itermax
lsavev=lanczosave

call setup_pcglanczos(mype,nprt,jiter,jiterstart,itermax,nwrvecs, &
                      l4dvar,lanczosave,ltcost)

if(jiter>=miter) lsavev=.false.
           
call pcglanczos(xhat,yhat,costf,gradx,grady,eps,itermax,lsavev)

call save_pcgprecond(lsavev)

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

gradf=zero
call jgrad(xhat,yhat,zff,gradf,lsavinc,nprt,myname) 

if (lobsensfc) then

!  Compute <dJ/dy,d>
   if (lobsensincr) then
!     call test_obsens(fcsens,xhat) ! _RTodling: needs proper grad eval
   else
      zy=dot_prod_obs()
      if (mype==0) write(6,'(A,ES25.18)')'Obs impact <dF/dy,d>= ',zy
   endif
!  Save gradient for next (backwards) outer loop
   if (jiter>1) then ! _RTodling: not correct for jiter>1 
      do ii=1,xhat%lencv
         fcsens%values(ii) = fcsens%values(ii) - xhat%values(ii)
      enddo
      clfile='fgsens.ZZZ'
      write(clfile(8:10),'(I3.3)') jiter
      call write_cv(fcsens,clfile)
   endif
   lsaveobsens=.false.
   l_do_adjoint=.true.

else ! not sensitivity run

   call bkerror(gradf,grads)

! If hybrid ensemble run, then multiply ensemble control variable a_en 
!                                 by its localization correlation
   if(l_hyb_ens) then

     if(aniso_a_en) then
!      call anbkerror_a_en(gradf,grads)    !  not available yet
       write(6,*)' ANBKERROR_A_EN not written yet, program stops'
       stop
     else
       call bkerror_a_en(gradf,grads)
     end if

! multiply static (Jb) part of grady by betas_inv(:), and
! multiply ensemble (Je) part of grady by betae_inv(:). [Default : betae_inv(:) =  1 - betas_inv(:) ]
!   (this determines relative contributions from static background Jb and
!   ensemble background Je)

     call beta12mult(grads)

   end if

!  Update xhatsave
   do ii=1,xhat%lencv
      xhatsave%values(ii) = xhatsave%values(ii) + xhat%values(ii)
      yhatsave%values(ii) = yhatsave%values(ii) + yhat%values(ii)
   end do

   zgg=dot_product(xhat,xhat,r_quad)
   if (mype==0) write(6,888)trim(myname),': Norm xhat=',sqrt(zgg)
   zgg=dot_product(xhatsave,xhatsave,r_quad)
   if (mype==0) write(6,888)trim(myname),': Norm xhatsave=',sqrt(zgg)

!  Print diagnostics
   zgf=dot_product(gradf,grads,r_quad)
   zgf=sqrt(zgf)
   if (mype==0) then
      write(6,888)trim(myname),': Final cost function =',real(zff,r_kind)
      write(6,888)trim(myname),': Final gradient norm =',real(zgf,r_kind)
      write(6,888)trim(myname),': Final/Initial cost function=',real(zff,r_kind)/real(zf0,r_kind)
      write(6,888)trim(myname),': Final/Initial gradient norm=',real(zgf,r_kind)/real(zg0,r_kind)
   endif

   if (l4dvar) then
      call prt_guess('increment')
   else
      call prt_guess('analysis')
   endif

   zge=dot_product(gradx,gradx,r_quad)
   zge=sqrt(zge)
   do ii=1,xhat%lencv
       gradf%values(ii) = gradf%values(ii) - gradx%values(ii)
   end do
   zgg=dot_product(gradf,grads,r_quad)
   zgg=sqrt(max(tiny_r_kind,zgg))
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

   if (lgrtest) call grtest(zdx,itest,xhat)  
   if (ladtest) call adtest(xhat)

endif ! sensitivity option

! Release memory
call deallocate_cv(xhat)
call deallocate_cv(yhat)
call deallocate_cv(gradx)
call deallocate_cv(grady)
call deallocate_cv(gradf)
call deallocate_cv(grads)

call inquire_cv
!_RT call inquire_state

! Finalize timer
call timer_fnl('bicg')

888 format(2A,3(1X,ES25.18))

return
end subroutine bicg
