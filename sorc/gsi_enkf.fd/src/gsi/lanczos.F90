module lanczos
!$$$ module documentation block
!           .      .    .                                       .
! module:   lanczos
!   prgmmr: tremolet
!
! abstract: Contains variables and routines for preconditioned
!           Lanczos minimizer following Mike Fisher's algorithm.
!
! program history log:
!   2007-05-16  tremolet
!   2007-07-11  tremolet - increment sensitivity to obs
!   2007-11-23  todling  - add timers
!   2009-01-18  todling  - minimal changes to interface w/ quad-based evaljgrad
!                          NOTE: no attempt made to reproduce across pe's yet
!   2009-08-18  lueken   - update documentation
!   2010-03-10  treadon  - add ESSL interface
!   2010-03-17  todling  - add analysis error estimate (congrad_siga)
!   2010-08-19  lueken   - add only to module use
!   2010-09-06  todling  - add ltcost parameter to allow writing out true cost
!   2010-09-20  el akkraoui - properly repositioned call related to ltcost  
!   2011-04-07  todling  - rename precond to lanczos_precond
!   2011-04-19  el akkraoui - avoid convert precond vectors for the next outer loop;
!                             and avoid lanczos decomposition at each iteration
!   2011-07-04  todling  - determine precision based on kinds
!
! Subroutines Included:
!   congrad       - Main minimization routine
!   setup_precond - Prepare the preconditioner
!   save_precond  - Save eigenvectors for constructing the next preconditioner
!   lanczos_precond - Preconditioner itself (called from congrad, internal)
!
! Variable Definitions:
!   LMPCGL  : .T. ====> precondition conjugate-gradient minimization
!   R_MAX_CNUM_PC : Maximum allowed condition number for the preconditioner
!   NPCVECS : number of vectors which make up the preconditioner
!
!   YVCGLPC: eigenvectors (from an earlier minimization)
!            that are used to construct the preconditioner.
!   RCGLPC : eigenvalues (from an earlier minimization)
!            that are used to construct the preconditioner.
!   NVCGLPC: the number of eigenpairs used to form the preconditioner.
!
!   YVCGLEV: eigenvectors for the current minimization.
!   RCGLEV : eigenvalues for the current minimization.
!   NVCGLEV: the number of eigenpairs for the current minimization.
!   LTCOST : .T. to calculate true cost function (unscalled), this adds
!           considerable computation cost; only used in test mode
!
!   YVCGLWK: work array of eigenvectors
!
! attributes:
!   language: f90
!   machine:
!
! ------------------------------------------------------------------------------
use kinds, only: r_kind,i_kind,r_quad,r_single,r_double
use constants, only: zero, one, two, one_tenth
use jfunc, only: iter
use control_vectors, only: control_vector,allocate_cv,inquire_cv,deallocate_cv, &
    write_cv,read_cv,dot_product,assignment(=)
use file_utility, only : get_lun
use timermod, only: timer_ini, timer_fnl
! ------------------------------------------------------------------------------

implicit none
save
private
public congrad, setup_congrad, save_precond, congrad_ad, read_lanczos, &
       congrad_siga

! ------------------------------------------------------------------------------

logical :: LTCOST_= .false.
logical :: LMPCGL = .false.
logical :: LCONVERT = .false. !if true, convert the preconditioner vectors for the next outer loop.
logical :: LDECOMP  = .false. !if true, carry lanczos decomposition at each iteration
real(r_kind) :: R_MAX_CNUM_PC = 10.0_r_kind
real(r_kind) :: xmin_ritz = one
real(r_kind) :: pkappa = one_tenth

integer(i_kind) :: NPCVECS, NVCGLPC, NVCGLEV, NWRVECS
REAL(r_kind), ALLOCATABLE :: RCGLPC(:)
REAL(r_kind), ALLOCATABLE :: RCGLEV(:)

integer(i_kind) :: mype,nprt,jiter,maxiter
logical :: l4dvar,lanczosave
REAL(r_kind), allocatable :: zlancs(:,:)

TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YVCGLPC
TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YVCGLEV
TYPE(control_vector), ALLOCATABLE, DIMENSION(:) :: YVCGLWK
type(control_vector), allocatable, dimension(:) :: cglwork

! --------------------------------------
integer(i_kind), PARAMETER :: N_DEFAULT_REAL_KIND = r_single
integer(i_kind), PARAMETER :: N_DOUBLE_KIND       = r_double
! --------------------------------------

! ------------------------------------------------------------------------------
contains
! ------------------------------------------------------------------------------
! CONGRAD
! ------------------------------------------------------------------------------
subroutine setup_congrad(kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs, &
                         ld4dvar,ldsave,ltcost)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    setup_congrad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs
!    ld4dvar,ldsave,ltcost
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_kind), intent(in   ) :: kpe,kprt,kiter,kiterstart,kmaxit,kwrvecs
logical        , intent(in   ) :: ld4dvar,ldsave,ltcost

integer(i_kind) :: ii

mype=kpe
nprt=kprt
jiter=kiter
maxiter=kmaxit
nwrvecs=kwrvecs
l4dvar=ld4dvar
lanczosave=ldsave
ltcost_=ltcost

if (allocated(zlancs)) deallocate(zlancs)
allocate(zlancs(maxiter+1,4))
zlancs=zero

allocate(cglwork(maxiter+1))
DO ii=1,kmaxit+1
   CALL allocate_cv(cglwork(ii))
   cglwork(ii)=zero
ENDDO

if (jiter==kiterstart) then
   NPCVECS=0
   NVCGLPC=0
   NVCGLEV=0
endif

if (jiter>1) call setup_precond()

if (mype==0) write(6,*)'setup_congrad end'
call inquire_cv

end subroutine setup_congrad
! ------------------------------------------------------------------------------
subroutine congrad(xhat,pcost,gradx,preduc,kmaxit,iobsconv,lsavevecs)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    congrad
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    xhat
!    gradx
!    preduc
!    kmaxit
!    iobsconv
!
!   output argument list:
!    xhat
!    pcost
!    gradx
!    preduc
!    kmaxit
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

IMPLICIT NONE

type(control_vector), intent(inout) :: xhat
real(r_kind)        , intent(  out) :: pcost
type(control_vector), intent(inout) :: gradx
real(r_kind)        , intent(inout) :: preduc
integer(i_kind)     , intent(inout) :: kmaxit
integer(i_kind)     , intent(in   ) :: iobsconv

logical             , intent(in   ) :: lsavevecs

character(len=*), parameter :: myname='congrad'
type(control_vector)        :: grad0,zww
type(control_vector)        :: gradf
type(control_vector)        :: xiter,xsens
real(r_quad)                :: pcostq
real(r_kind)                :: zbeta(2:kmaxit+1),zdelta(kmaxit),zv(kmaxit+1,kmaxit+1),&
   zbnds(kmaxit),zritz(kmaxit+1),zsave(kmaxit+1,4),&
   zqg0(kmaxit+1),zsstwrk(2*kmaxit)
real(r_kind)                :: zdla, zeta, preduc_norm
real(r_kind)                :: zbndlm, zgnorm, znorm2l1, zreqrd, ztheta1
integer(i_kind)             :: ingood,itheta1,jm,imaxevecs,ii,jj,jk,isize
integer(i_kind)             :: kminit, kmaxevecs,iunit,iprt
logical                     :: lsavinc, lldone
character(len=17)           :: clfile

! --------------------------------------

!--- initialize timer
call timer_ini('congrad')

iprt=nprt
if(ltcost_) iprt=0
kminit = kmaxit
kmaxevecs = kmaxit
imaxevecs = 0
lldone=.false.
if (kmaxit>maxiter) then
   write(6,*)'setup_congrad: kmaxit>maxiter',kmaxit,maxiter
   call stop2(138)
end if

if (mype==0) write(6,*) '---- Lanczos Solver ----'

!--- allocate distributed storage

call allocate_cv(grad0)
call allocate_cv(zww)

!--- 'zeta' is an upper bound on the relative error of the gradient.

zeta  = 1.0e-4_r_kind

zreqrd = preduc

!--- change of variable to account for preconditioning

if (LMPCGL) call lanczos_precond(xhat,+2)

zgnorm = SQRT( DOT_PRODUCT (gradx,gradx))

if (mype==0) write (6,*)'grepmin Starting point: Estimated gradient norm=',zgnorm

if (LMPCGL) call lanczos_precond(gradx,-2)

cglwork(1) = gradx
znorm2l1 = DOT_PRODUCT(cglwork(1),cglwork(1))
cglwork(1)%values = cglwork(1)%values / SQRT(znorm2l1)

!--- save initial control vector and gradient

grad0 = gradx

zqg0(1) = DOT_PRODUCT(cglwork(1),grad0)

if(nprt>=1.and.ltcost_) then
   if (mype==0) then
      write(6,*)' True cost function (at considerable additional computational cost): '
      write(6,*)' ----------------------------------------------------------------- '
   endif
endif

!--- Lanczos iteration starts here

ingood = 0
iter   = 1
Lanczos_loop : DO

!--- evaluate the Hessian applied to the latest Lanczos vector

   do jj=1,zww%lencv
      zww%values(jj) = xhat%values(jj) + cglwork(iter)%values(jj)
   enddo

   if (LMPCGL) call lanczos_precond(zww,-2)

   lsavinc=.false.
   call evaljgrad(zww,pcostq,gradx,lsavinc,iprt,myname)
   pcost=pcostq

   if (LMPCGL) call lanczos_precond(gradx,-2)

   do jj=1,gradx%lencv
      gradx%values(jj) = gradx%values(jj) - grad0%values(jj)
   enddo

!--- calculate zdelta

   zdelta(iter) = DOT_PRODUCT(cglwork(iter),gradx)

   if (zdelta(iter)<=zero) then
      if (mype==0) write(6,*)'congrad stopping: J" not positive definite',zdelta(iter)
      iter = iter-1
      EXIT Lanczos_loop
   endif

!--- Calculate the new Lanczos vector (This is the Lanczos recurrence)

   do jj=1,gradx%lencv
      gradx%values(jj) = gradx%values(jj) - zdelta(iter) * cglwork(iter)%values(jj)
   enddo
   if (iter>1) then
      do jj=1,gradx%lencv
         gradx%values(jj) = gradx%values(jj) - zbeta(iter) * cglwork(iter-1)%values(jj)
      enddo
   endif

!--- orthonormalize gradient against previous gradients

   do jm=iter,1,-1
      zdla = DOT_PRODUCT(gradx,cglwork(jm))
      do jj=1,gradx%lencv
         gradx%values(jj) = gradx%values(jj) - zdla*cglwork(jm)%values(jj)
      enddo
   enddo

   zbeta(iter+1) = SQRT(DOT_PRODUCT(gradx,gradx))

   do jj=1,gradx%lencv
      cglwork(iter+1)%values(jj) = gradx%values(jj) / zbeta(iter+1)
   enddo

   zqg0(iter+1) = DOT_PRODUCT(cglwork(iter+1),grad0)

!--- calculate the reduction in the gradient norm and cost

   zlancs(1:iter,1) =  zdelta(1:iter)
   zlancs(2:iter,2) =  zbeta (2:iter)
   zlancs(1:iter,3) = -zqg0  (1:iter)
 
   call ptsv

   do jj=1,zww%lencv
      zww%values(jj) = grad0%values(jj) &
         + (zbeta(iter+1)*zlancs(iter,3))*cglwork(iter+1)%values(jj)
   enddo

   do jj=1,iter
      do ii=1,zww%lencv
         zww%values(ii)  = zww%values(ii)  - cglwork(jj)%values(ii)*zqg0(jj)
      enddo
   enddo

   if (LMPCGL) call lanczos_precond(zww,+2)

   preduc_norm = SQRT(DOT_PRODUCT(zww,zww))
   preduc = preduc_norm/zgnorm
   if (mype==0) write (6,'(2(1X,A,ES25.18))') &
      'Estimated gradient norm=',preduc_norm,' reduction = ',preduc


!--- determine eigenvalues and eigenvectors of the tri-diagonal problem
   if((LDECOMP .or. (iter==kmaxit)) .and. lsavevecs) then 
      zlancs(1:iter  ,4) = zdelta(1:iter)
      zlancs(1:iter-1,1) = zbeta (2:iter)

      if (iter /= 1) then
         call steqr
      else
         zv(1,1) = one
      endif

      zritz(1:iter) = zlancs(1:iter,4)

      if (mype==0) write(6,*)'congrad: ritz values are: ',zritz(1:iter)

!--- estimate error bounds

      zbndlm = zeta*zritz(iter)
 
      zbnds(1:iter) = abs(zbeta(iter+1)*zv(iter,1:iter))
      if (mype==0) write (6,*)'congrad: error bounds are: ',zbnds(1:iter)

!--- Check for exploding or negative Ritz values

      if (ANY(zritz(1:iter)<zero)) then
         if (mype==0) write(6,*)'congrad stopping: negative ritz value'
         iter = iter-1
         zlancs(1:iter  ,4) = zdelta(1:iter)
         zlancs(1:iter-1,1) = zbeta(2:iter)
 
         if (iter > 1) then
            call steqr
         else
            zv(1,1) = one
         endif

         zritz(1:iter) = zlancs(1:iter,4)
 
         zbnds(1:iter) = abs(zbeta(iter+1)*zv(iter,1:iter))
         EXIT Lanczos_loop
      endif

      if (ingood>0) then
         if (zritz(itheta1)>1.01_r_kind*ztheta1) then
            if (mype==0) write(6,*)'congrad stopping: ritz values exploding'
            if (mype==0) write(6,*)'leading ritz value=',zritz(itheta1)
            if (mype==0) write(6,*)'leading converged eigenvalue=',ztheta1
         endif
      endif

!--- Count the converged eigenvectors

      ingood = 0
      do jm=1,iter
         if (zbnds(jm)<=zbndlm) then
            ingood = ingood+1
            if (mype==0) write(6,*)'congrad: converged eigenvalue ',zritz(jm)
         endif
      enddo

!--- save leading converged eigenvalue for explosion test

      if (ingood > 0) then
         do jm=iter,1,-1
            if (zbnds(jm) <= zbndlm) then
               ztheta1 = zritz(jm)
               itheta1 = jm
               exit
            endif
         enddo
      endif

      if (mype==0) write(6,*)'congrad: End of iteration: ',iter
      if (mype==0) write(6,'(/)')
 
!     count how many eigenpairs have converged to PKAPPA precision and have
!     eigenvalue > xmin_ritz (which is 1 by default)
!     (For the analysis, all eigenvalues should be >1. For the singular vector calculation,
!     we are not interested in decaying modes.)
!     However, when SVs are computed with projection operators, 1 may not
!     be an appropriate choice for xmin_ritz

      imaxevecs = COUNT(zbnds(1:iter)/zritz(1:iter)<=pkappa .AND. zritz(1:iter)>xmin_ritz)

      if (imaxevecs >= kmaxevecs) then
         if (mype==0) write(6,*)imaxevecs,' eigenpairs converged to precision ',pkappa
         if (mype==0) write(6,'(/)')
         EXIT Lanczos_loop
      endif

   end if

!  Tests for end of iterations
   if (iter >= kmaxit .or. (preduc <= zreqrd .and. iter >= kminit)) &
      EXIT Lanczos_loop

 

   if (nprt>=1.and.ltcost_.and.iobsconv==0) then

!     Compute actual increment
      zsave=zlancs
      zlancs(1:iter,1) =  zdelta(1:iter)
      zlancs(2:iter,2) =  zbeta (2:iter)
      zlancs(1:iter,3) = -zqg0  (1:iter)
      call ptsv
 
      call allocate_cv(xiter)
      xiter=zero
      do jj=1,iter
         do ii=1,xiter%lencv
            xiter%values(ii) = xiter%values(ii)  + cglwork(jj)%values(ii)*zlancs(jj,3)
         enddo
      enddo

      call allocate_cv(gradf)
      gradf=zero
      if (LMPCGL) then
         call lanczos_precond(xiter,-2)
      endif
      call evaljgrad(xiter,pcostq,gradf,lsavinc,nprt,myname)

      call deallocate_cv(gradf)
      call deallocate_cv(xiter)
      zlancs=zsave

   endif

!  Test convergence in observation space
   if (iobsconv>=2) then

!     Compute actual increment
      zsave=zlancs
      zlancs(1:iter,1) =  zdelta(1:iter)
      zlancs(2:iter,2) =  zbeta (2:iter)
      zlancs(1:iter,3) = -zqg0  (1:iter)
      call ptsv
 
      call allocate_cv(xiter)
      call allocate_cv(xsens)
      xiter=zero
      do jj=1,iter
         do ii=1,xiter%lencv
            xiter%values(ii) = xiter%values(ii)  + cglwork(jj)%values(ii)*zlancs(jj,3)
         enddo
      enddo
      if (LMPCGL) call lanczos_precond(xiter,-2)
      xsens=xiter

!     Compute observation impact
      call congrad_ad(xsens,iter)
      call test_obsens(xiter,xsens)

!     Clean-up
      call deallocate_cv(xiter)
      call deallocate_cv(xsens)
      zlancs=zsave
   endif

!--- Increment the iteration counter

   iter = iter+1
   if (ingood>0) itheta1 = itheta1+1

ENDDO Lanczos_loop

!--- end of Lanczos iteration

lldone=.true.

if (mype==0) then
   write(6,*)'Summary of Lanczos iteration:'
   write(6,*)'   Number of iterations performed: ',iter
   write(6,*)'   Maximum allowed number of iterations: ',kmaxit
   write(6,*)'   Minimum allowed number of iterations: ',kminit
   write(6,*)'   Required reduction in norm of gradient: ',zreqrd
   write(6,*)'   Achieved reduction in norm of gradient: ',preduc
   if (preduc > zreqrd) then
      write(6,*)'   *** Failed to meet convergence criterion ***'
   endif
   write(6,*)'   Number of sufficiently-converged eigenpairs: ',imaxevecs
endif

!--- Calculate the solution vector and gradient

zlancs(1:iter,1) =  zdelta(1:iter)
zlancs(2:iter,2) =  zbeta (2:iter)
zlancs(1:iter,3) = -zqg0  (1:iter)

call ptsv

do jj=1,gradx%lencv
   gradx%values(jj) = grad0%values(jj) &
      + zbeta(iter+1)*cglwork(iter+1)%values(jj)*zlancs(iter,3)
enddo

do jj=1,iter
   do ii=1,xhat%lencv
      xhat%values(ii)  = xhat%values(ii)  + cglwork(jj)%values(ii)*zlancs(jj,3)
      gradx%values(ii) = gradx%values(ii) - cglwork(jj)%values(ii)*zqg0(jj)
   enddo
enddo

!--- transform control variable and gradient back to unpreconditioned space

if (LMPCGL) then
   call lanczos_precond(xhat ,-2)
   call lanczos_precond(gradx,+2)
endif

!--- Compute observation impact
if (iobsconv>=1) then
   call allocate_cv(xsens)
   xsens=xhat

   call congrad_ad(xsens,iter)
   call test_obsens(xhat,xsens)

   call deallocate_cv(xsens)
endif

!--- Save lanczos vectors (if required for adjoint)

if (lanczosave) then
   do jj=1,iter
      clfile='lanczvec.XXX.YYYY'
      WRITE(clfile(10:12),'(I3.3)') jiter
      WRITE(clfile(14:17),'(I4.4)') jj
      call write_cv(cglwork(jj),clfile)
   ENDDO

   if (mype==0) then
      iunit=get_lun()
      clfile='zlanczos.XXX'
      WRITE(clfile(10:12),'(I3.3)') jiter
      write(6,*)'Writing Lanczos coef. to file ',clfile
 
      open(iunit,file=trim(clfile),form='unformatted')
      write(iunit)maxiter
      write(iunit)zlancs(1:maxiter+1,1:4)
      close(iunit)
   endif
endif

!--- Calculate sufficiently converged eigenvectors of the preconditioned Hessian

if (l4dvar .and. lsavevecs) then
   zbnds(1:iter) = zbnds(1:iter)/zritz(1:iter)

   NVCGLEV = 0
   do jk=iter,1,-1
      if (zbnds(jk) <= pkappa .AND. zritz(jk) > xmin_ritz) then
         NVCGLEV=NVCGLEV+1
      endif
   ENDDO
   if (mype==0) write(6,*) &
      'Number of eigenpairs converged to requested accuracy NVCGLEV=',NVCGLEV

   NVCGLEV= min(NVCGLEV,NWRVECS)
   if (mype==0) write(6,*) &
      'Number of eigenevctors to be calculated is NVCGLEV=',NVCGLEV

   ALLOCATE(RCGLEV(NVCGLEV))
   ALLOCATE (YVCGLEV(NVCGLEV))
   DO ii=1,NVCGLEV
      CALL allocate_cv(YVCGLEV(ii))
   ENDDO

   ii=0
   do jk=iter,1,-1
      if (zbnds(jk) <= pkappa .AND. zritz(jk) > xmin_ritz .AND. ii<NWRVECS) then
         ii = ii+1
         RCGLEV(ii) = zritz(jk)
         YVCGLEV(ii) = zero
         isize=size(YVCGLEV(ii)%values)
         do jm=1,iter
            do jj=1,isize
               YVCGLEV(ii)%values(jj)=YVCGLEV(ii)%values(jj) + cglwork(jm)%values(jj)*zv(jm,jk)
            enddo
         enddo

         do jm=1,ii-1
            zdla=DOT_PRODUCT (YVCGLEV(jm),YVCGLEV(ii))
            do jj=1,isize
               YVCGLEV(ii)%values(jj) = YVCGLEV(ii)%values(jj) - zdla*YVCGLEV(jm)%values(jj)
            enddo
         enddo

         zdla=DOT_PRODUCT (YVCGLEV(ii),YVCGLEV(ii))
         YVCGLEV(ii)%values = YVCGLEV(ii)%values / sqrt(zdla)
      endif
   ENDDO

   if (mype==0.and.NVCGLEV>0) then
      write(6,'(/)')
      write(6,*)'Calculated eigenvectors for the following eigenvalues:'
      write(6,*)'RCGLEV=',RCGLEV(1:NVCGLEV)
      write(6,'(/)')
   endif

endif

!--- release memory, etc.

call deallocate_cv(grad0)
call deallocate_cv(zww)

!--- return the number of iterations actually performed

kmaxit=iter

!--- finalize timer
call timer_fnl('congrad')

return

!-----------------------------------------------------------------------
contains
!-----------------------------------------------------------------------
!   STEQR - Simplified interface to LAPACK routines SSTEQR/DSTEQR
!           and to ESSL routines SGEEV/DGEEV
!-----------------------------------------------------------------------
  subroutine steqr
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    steqr
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2010-03-10  treadon - add ESSL interface
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
    implicit none

    integer(i_kind) :: info

#ifdef ibm_sp
    logical,allocatable:: select(:)
    integer(i_kind):: n,i,j,jj,ldz
    integer(i_kind):: iopt,lda,naux
    integer(i_kind),allocatable:: indx(:)
    real(r_kind),allocatable:: a(:,:),aux(:),w_order(:)
    complex(r_kind),allocatable:: w(:),z(:,:)

!   Use ESSL
    iopt=1
    n=iter
    lda=n
    ldz=kmaxit+1
    naux=2*n
    allocate(select(n),indx(n),a(lda,n),w(n),z(ldz,n),aux(naux),w_order(n))

!   Load matrix A.
    a=zero
    do i=1,n-1
       a(i,  i)=zlancs(i,4)  ! load diagonal
       a(i+1,i)=zlancs(i,1)  ! load sub-diagnonal
       a(i,i+1)=zlancs(i,1)  ! load super-diagonal
    end do
    a(n,n)=zlancs(n,4)       ! load diagonal

!   Additional initializations
    select=.false.    ! select not used for iopt=1
    w=zero
    z=zero
    aux=zero

!   Call ESSL routines
    if (r_kind == N_DEFAULT_REAL_KIND) then
       call SGEEV(iopt, a, lda, w, z, ldz, select, n, aux, naux)
       do i=1,n
          w_order(i)=real(w(i),r_kind)
       end do
       call SSORTX(w_order,1,n,indx)  ! sort eigenvalues into ascending order
    ELSEIF (r_kind == N_DOUBLE_KIND) then
       call DGEEV(iopt, a, lda, w, z, ldz, select, n, aux, naux)
       do i=1,n
          w_order(i)=real(w(i),r_kind)
       end do
       call DSORTX(w_order,1,n,indx)  ! sort eigenvalues into ascending order
    else
       write(6,*)'STEQR: r_kind is neither default real nor double precision'
       call stop2(319)
    endif

!   Load ESSL eigenvalues and eigenvectors into output arrays
    do j=1,n
       zlancs(j,4)=w_order(j)          ! eigenvalues
       jj=indx(j)
       do i=1,ldz
          zv(i,j)=real(z(i,jj),r_kind) ! eigenvectors
       end do
    end do

!   Deallocate work arrays    
    deallocate(select,indx,a,w,z,aux,w_order)
    
#else

!   Use LAPACK
    if (r_kind == N_DEFAULT_REAL_KIND) then
       call SSTEQR ('I',iter,zlancs(1,4),zlancs,zv,kmaxit+1,zsstwrk,info)
    ELSEIF (r_kind == N_DOUBLE_KIND) then
       call DSTEQR ('I',iter,zlancs(1,4),zlancs,zv,kmaxit+1,zsstwrk,info)       
    else
       write(6,*)'STEQR: r_kind is neither default real nor double precision'
       call stop2(319)
    endif
    if (info /= 0) then
       write (6,*)'Error in congrad: SSTEQR/DSTEQR returned info=',info
       write(6,*) 'STEQR: SSTEQR/DSTEQR returned non-zero info'
       call stop2(320)
    endif
#endif


end subroutine steqr

!-----------------------------------------------------------------------
!   PTSV - Simplified interface to LAPACK routines SPTSV/DPTSV
!          and to ESSL routines SPTF,S/DPTF,S
!-----------------------------------------------------------------------
subroutine ptsv
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    ptsv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2010-03-10  treadon- add ESSL interface
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
  implicit none
  
  integer(i_kind) :: info

#ifdef ibm_sp
  integer(i_kind) :: i,n,iopt
  real(r_kind),allocatable:: c(:),d(:),bx(:)

! Use ESSL
  iopt=0
  n=iter
  allocate(c(n),d(n),bx(n))

! Load matrices
  c=zero
  do i=1,n-1
     c(i+1) = zlancs(i+1,2) ! lower subdiagonal of A
     d(i) = zlancs(i,1)     ! main diagonal of A
     bx(i)=zlancs(i,3)      ! right hand side B
  end do
  d(n) =zlancs(n,1)
  bx(n)=zlancs(n,3)          

! Factorize and solve system of equations using ESSL routines
  if (r_kind == N_DEFAULT_REAL_KIND) then
     call sptf(n,c,d,iopt)   ! factorize
     call spts(n,c,d,bx)     ! solve
  ELSEIF (r_kind == N_DOUBLE_KIND) then
     call dptf(n,c,d,iopt)   ! factorize
     call dpts(n,c,d,bx)     ! solve
  else
     write(6,*) 'r_kind is neither default real nor double precision'
     call stop2(321)
  endif
  
! Load ESSL result into output arrays
  do i=1,n
     zlancs(i,3)=bx(i)
  end do
  
! Deallocate work arrays
  deallocate(c,d,bx)

#else

! Use LAPACK
  if (r_kind == N_DEFAULT_REAL_KIND) then
     call SPTSV (iter,1,zlancs(1,1),zlancs(2,2),zlancs(1,3),kmaxit+1,info)
  ELSEIF (r_kind == N_DOUBLE_KIND) then
     call DPTSV (iter,1,zlancs(1,1),zlancs(2,2),zlancs(1,3),kmaxit+1,info)
  else
     write(6,*) 'r_kind is neither default real nor double precision'
     call stop2(321)
  endif
  if (info /= 0) then
     write (6,*) 'Error in congrad: SPTSV/DPTSV returned ',info
     write(6,*)'CONGRAD: SPTSV/DPTSV returned non-zero info'
     call stop2(322)
  endif

#endif

end subroutine ptsv

! ------------------------------------------------------------------------------
end subroutine congrad
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
subroutine congrad_ad(xsens,kiter)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    congrad_ad
!   prgmmr:
!
! abstract: Apply product of adjoint of estimated Hessian to a vector.
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    xsens
!    kiter
!
!   output argument list:
!    xsens
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

type(control_vector), intent(inout) :: xsens
integer(i_kind)     , intent(in   ) :: kiter

real(r_kind) :: zaa(kiter),zzz
integer(i_kind) :: ii,jj

!--- initialize timer
call timer_ini('congrad_ad')

zzz=dot_product(xsens,xsens)
if (mype==0) write(6,888)'congrad_ad: Norm  input=',sqrt(zzz)

if (LMPCGL) call lanczos_precond(xsens,-2)

zaa=zero
do jj=1,kiter
   zaa(jj)=dot_product(xsens,cglwork(jj))
enddo
do jj=2,kiter
   zaa(jj)=zaa(jj)-zlancs(jj,2)*zaa(jj-1)
enddo
zaa(kiter)=zaa(kiter)/zlancs(kiter,1)
do jj=kiter-1,1,-1
   zaa(jj)=zaa(jj)/zlancs(jj,1) - zaa(jj+1)*zlancs(jj+1,2)
enddo
xsens=zero
do jj=1,kiter
   do ii=1,xsens%lencv
      xsens%values(ii) = xsens%values(ii) + zaa(jj) * cglwork(jj)%values(ii)
   enddo
enddo

if (LMPCGL) call lanczos_precond(xsens,-2)

zzz=dot_product(xsens,xsens)
if (mype==0) write(6,888)'congrad_ad: Norm output=',sqrt(zzz)
888 format(A,3(1X,ES25.18))

!--- finalize timer
call timer_fnl('congrad_ad')

return
end subroutine congrad_ad
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
subroutine congrad_siga(siga,ivecs,rc)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    congrad_siga
!   prgmmr: todling
!
! abstract: Calculate estimate of analysis error
!
! program history log:
!  2010-03-17  todling  - initia code
!  2010-05-16  todling  - update to use gsi_bundle
!  2013-01-26  parrish  - WCOSS debug compile flagged type mismatch error for
!                          "call bkg_stddev(aux,mval(ii))".
!                         I changed to 
!                          "call bkg_stddev(aux%step(ii),mval(ii))".
!                         Don't know if this is the correct modification.
!
!   input argument list:
!    siga
!
!   output argument list:
!    siga
!    ivecs
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use gsi_4dvar, only : nsubwin
use bias_predictors, only: predictors,allocate_preds,deallocate_preds
use state_vectors, only: allocate_state,deallocate_state
use gsi_bundlemod, only: gsi_bundlehadamard
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
implicit none
type(gsi_bundle),intent(inout) :: siga  ! analysis errors
integer(i_kind), intent(  out) :: ivecs ! 
integer(i_kind), intent(  out) :: rc    ! error return code
! local variables
type(control_vector) :: aux
type(gsi_bundle)     :: mval(nsubwin)
type(predictors)     :: sbias
integer(i_kind)      :: ii,jj
real(r_kind)         :: zz

rc=0
NPCVECS = NVCGLEV
ivecs=MIN(npcvecs,nwrvecs)
if (ivecs<1) then
  if (mype==0) write(6,*)'save_precond: cannot get siga, ivecs=', ivecs
  rc=1
  return
endif

call allocate_preds(sbias)
do ii=1,nsubwin
   call allocate_state(mval(ii))
end do
call allocate_cv(aux)

!-- calculate increment on analysis error covariance diag(Delta P)
siga=zero
DO jj=1,ivecs
  zz=sqrt(one-one/sqrt(RCGLEV(jj)))
  aux%values = zz * YVCGLEV(jj)%values
  call control2model(aux,mval,sbias)
  do ii=1,nsubwin
     call gsi_bundlehadamard(siga,mval(ii),mval(ii))
  enddo
ENDDO

do ii=1,nsubwin
!-- get B standard deviations
   call bkg_stddev(aux%step(ii),mval(ii))
!-- calculate diag(Pa) = diag(B) - diag(Delta P)
!   i.e., add diag(B) as rank-1 update to diag(Delta P)
   call gsi_bundlehadamard(siga,mval(ii),mval(ii))
enddo

call deallocate_cv(aux)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do
call deallocate_preds(sbias)

end subroutine congrad_siga
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
!   SAVE_PRECOND - Save eigenvectors from CONGRAD for next minimization
! ------------------------------------------------------------------------------
subroutine save_precond(ldsave)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    save_precond
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    ldsave
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

IMPLICIT NONE

logical, intent(in   ) :: ldsave

REAL(r_kind), ALLOCATABLE :: zmat(:,:)
INTEGER(i_kind) :: ii,jj, info, iunit, ivecs
REAL(r_kind) :: zz
CHARACTER(LEN=13) :: clfile

if (ldsave) then

!--- read eigenvalues of the preconditioner

   NPCVECS = NVCGLEV+NVCGLPC
   if (mype==0) write(6,*)'save_precond: NVCGLEV,NVCGLPC,NPCVECS=', &
                                       & NVCGLEV,NVCGLPC,NPCVECS

   ALLOCATE(YVCGLWK(npcvecs))
   ii=0
   if(.not.lCONVERT) then
      DO jj=1,NVCGLEV
         ii=ii+1
         !  zz=sqrt(RCGLPC(jj)-one)
         CALL allocate_cv(YVCGLWK(ii))
         YVCGLWK(ii)%values = YVCGLEV(jj)%values
         CALL deallocate_cv(YVCGLEV(jj))
      ENDDO
      IF (ALLOCATED(YVCGLEV)) DEALLOCATE(YVCGLEV)
      
      NVCGLEV=0

   else
!--- copy preconditioner vectors to work file

      if (mype==0.and.NVCGLPC>0) write(6,*)'save_precond: RCGLPC=',RCGLPC
      DO jj=1,NVCGLPC
         ii=ii+1
         zz=sqrt(RCGLPC(jj)-one)
         CALL allocate_cv(YVCGLWK(ii))
         YVCGLWK(ii)%values = zz * YVCGLPC(jj)%values
         CALL deallocate_cv(YVCGLPC(jj))
      ENDDO
      IF (ALLOCATED(YVCGLPC)) DEALLOCATE(YVCGLPC)
!     IF (ALLOCATED( RCGLPC)) deallocate( RCGLPC)
      NVCGLPC=0

!--- copy and transform eigenvectors of preconditioned Hessian

      if (mype==0.and.NVCGLEV>0) write(6,*)'save_precond: RCGLEV=',RCGLEV
      DO jj=1,NVCGLEV
         ii=ii+1
         zz=sqrt(RCGLEV(jj)-one)
         CALL allocate_cv(YVCGLWK(ii))
         YVCGLWK(ii)%values = zz * YVCGLEV(jj)%values
         CALL deallocate_cv(YVCGLEV(jj))
      ENDDO
      IF (ALLOCATED(YVCGLEV)) DEALLOCATE(YVCGLEV)
      IF (ALLOCATED( RCGLEV)) deallocate( RCGLEV)
      NVCGLEV=0

      if (mype==0) write(6,*)'save_precond: NVCGLPC,NVCGLEV,npcvecs,ii=', &
                      NVCGLPC,NVCGLEV,npcvecs,ii
      if (ii/=npcvecs) then
         write(6,*)'save_precond: error number of vectors',ii,npcvecs
         call stop2(139)
      end if

!---  form the inner matrix for the Shermann-Morrison-Woodbury inversion

      ALLOCATE(zmat(npcvecs,npcvecs))
      do jj=1,npcvecs
         do ii=jj,npcvecs
            zmat(ii,jj) = DOT_PRODUCT (YVCGLWK(jj),YVCGLWK(ii))
         ENDDO
         zmat(jj,jj) = zmat(jj,jj) + one
      ENDDO

!--- Cholesky decompose

      if (mype==0) write(6,*)'save_precond: call dpotrf npcvecs=',npcvecs
      if (r_kind==N_DEFAULT_REAL_KIND) then
         call SPOTRF('L',npcvecs,zmat,npcvecs,info)
      ELSEIF (r_kind==N_DOUBLE_KIND) then
         call DPOTRF('L',npcvecs,zmat,npcvecs,info)
      else
         write(6,*)'save_precond: r_kind is neither default real nor double precision'
         call stop2(323)
      endif

      if (info/=0) then
         write(6,*)'save_precond: error computing Cholesky decomposition'
         write(6,*)'SPOTRF/DPOTRF returns info=',info
         call stop2(324)
      endif

!--- transform vectors

      do jj=1,npcvecs
         do ii=1,jj-1
            YVCGLWK(jj)%values = YVCGLWK(jj)%values - zmat(jj,ii)*YVCGLWK(ii)%values
         enddo
         YVCGLWK(jj)%values = YVCGLWK(jj)%values / zmat(jj,jj)
      ENDDO

   endif

!--- Save the eigenvectors

   if (l4dvar) then
      ivecs=MIN(npcvecs,nwrvecs)
      DO jj=1,ivecs
         clfile='evec.XXX.YYYY'
         WRITE(clfile(6:8) ,'(I3.3)') jiter
         WRITE(clfile(10:13),'(I4.4)') jj
         call write_cv(YVCGLWK(jj),clfile)
      ENDDO

      if (mype==0) then
         iunit=78
         clfile='eval.XXX'
         WRITE(clfile(6:8),'(I3.3)') jiter
         open(iunit,file=clfile)
         write(iunit,*)RCGLEV
         close(iunit)
      endif

      if (mype==0) then
         iunit=78
         clfile='numpcvecs.XXX'
         WRITE(clfile(11:13),'(I3.3)') jiter
         open(iunit,file=clfile)
         write(iunit,*)ivecs
         close(iunit)
      endif

      DO ii=1,npcvecs
         CALL deallocate_cv(YVCGLWK(ii))
      ENDDO
      DEALLOCATE(YVCGLWK)
   else
      do ii=nwrvecs+1,npcvecs
         CALL deallocate_cv(YVCGLWK(ii))
      enddo
      npcvecs=MIN(npcvecs,nwrvecs)
   endif

   if (ALLOCATED(zmat)) DEALLOCATE(zmat)
endif

do ii=1,maxiter+1
   call deallocate_cv(cglwork(ii))
enddo
deallocate(cglwork)

return
end subroutine save_precond

! ------------------------------------------------------------------------------
!   SETUP_PRECOND - Calculates the preconditioner for congrad
! ------------------------------------------------------------------------------
subroutine setup_precond()
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    setup_precond
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2010-03-10  treadon - add ESSL interface
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

  IMPLICIT NONE

  INTEGER(i_kind), allocatable :: indarr(:)
  REAL(r_kind), allocatable :: zq(:),zlam(:),zU(:,:),zUUT(:,:),zwork(:),zzz(:)
  INTEGER(i_kind) :: info,ik,inpcv,ji,jj,jk,ii,iunit
  REAL(r_kind) :: za, zps
  CHARACTER(LEN=13) :: clfile

#ifdef ibm_sp
! Declare variables and Work arrays used by ESSL
  integer(i_kind):: iopt, ldz, n, naux
  real(r_kind), allocatable ::  w(:),z(:,:),ap(:),aux(:)
#endif

!--- read vectors, apply change of variable and copy to work file

   if (l4dvar) then
      iunit=78
      clfile='numpcvecs.XXX'
      WRITE(clfile(11:13),'(I3.3)') jiter-1
      open(iunit,file=clfile)
      read(iunit,*)npcvecs
      close(iunit)
     
      if (npcvecs<1) then
         write(6,*)'SETUP_PRECOND: no vectors for preconditioner',npcvecs
         call stop2(140)
      end if
     
      ALLOCATE(YVCGLWK(npcvecs))
      DO ii=1,npcvecs
         CALL allocate_cv(YVCGLWK(ii))
      ENDDO
     
      do jj=1,npcvecs
         clfile='evec.XXX.YYYY'
         WRITE(clfile(6:8) ,'(I3.3)') jiter-1
         WRITE(clfile(10:13),'(I4.4)') jj
         call read_cv(yvcglwk(jj),clfile)
      ENDDO
   endif
   if(.not. LCONVERT) then 
      NVCGLPC=npcvecs   
      if(NVCGLPC > 0) then   
         ALLOCATE (RCGLPC(NVCGLPC))

         iunit=78
         clfile='eval.XXX'
         WRITE(clfile(6:8),'(I3.3)') jiter-1
         open(iunit,file=clfile)
         read(iunit,*)RCGLPC
         close(iunit)
         do ii=1,NVCGLPC
            RCGLPC(ii) = MIN(R_MAX_CNUM_PC,RCGLPC(ii))
         end do
     

         ALLOCATE (YVCGLPC(NVCGLPC))
         DO jj=1,NVCGLPC
            CALL ALLOCATE_CV(YVCGLPC(jj))
         ENDDO
         DO jj=1,NVCGLPC
            YVCGLPC(jj) = zero
            do jk=1,YVCGLPC(jj)%lencv
               YVCGLPC(jj)%values(jk) = yvcglwk(jj)%values(jk)
            enddo
         ENDDO
 
         LMPCGL = .true.
      else
         NVCGLPC = 0
         LMPCGL = .false.
      endif
   else
      if (mype==0) write(6,*)'allocate arrays with npcvecs=',npcvecs
      allocate(indarr(npcvecs))
      allocate(zq(npcvecs),zlam(npcvecs),zU(npcvecs,npcvecs))
      allocate(zUUT(npcvecs,npcvecs),zwork(3*npcvecs),zzz(npcvecs))

!--- Perform Householder transformations to reduce the matrix of vectors
!--- to upper triangular

      do jj=1,npcvecs
         CALL ALLGATHER_CVSECTION(yvcglwk(jj),zq(1:jj),1,jj)
     
         zps = DOT_PRODUCT(yvcglwk(jj),yvcglwk(jj)) - DOT_PRODUCT(zq(1:jj),zq(1:jj))
     
         if (zq(jj) < zero) then
            zU(jj,jj) = -sqrt(zps+zq(jj)*zq(jj))
         else
            zU(jj,jj) =  sqrt(zps+zq(jj)*zq(jj))
         endif
     
         zq(jj) = zq(jj) - zU(jj,jj)

         do jk=1,jj-1
            zU(jk,jj) = zq(jk)
         ENDDO
     
         zps = zps + zq(jj)*zq(jj)
     
         zzz(1:jj-1)=zero
         zzz(jj)=zq(jj)
         CALL SET_CVSECTION(zzz(1:jj),yvcglwk(jj),1,jj)

         do jk=1,yvcglwk(jj)%lencv
            yvcglwk(jj)%values(jk) = yvcglwk(jj)%values(jk) * sqrt(two/zps)
         enddo

!--- we now have the Householder vector in yvcglwk(jj), and the non-zero
!--- elements of the transformed vector in ZU. Now apply the Householder
!--- transformations to the remaining vectors.
     
         do ji=jj+1,npcvecs
            zps = DOT_PRODUCT (yvcglwk(jj),yvcglwk(ji))
            do jk=1,yvcglwk(ji)%lencv
               yvcglwk(ji)%values(jk) = yvcglwk(ji)%values(jk) - zps*yvcglwk(jj)%values(jk)
            enddo
         ENDDO
      ENDDO

!--- Multiply the upper triangle by its transpose and find eigenvectors
!--- and eigenvalues

      do jj=1,npcvecs
         do ji=jj+1,npcvecs
            zU(ji,jj) = zero
         enddo
      enddo

      do jj=1,npcvecs
         do ji=jj,npcvecs
            zUUT(ji,jj) = zero
            do jk=ji,npcvecs
               zUUT(ji,jj) = zUUT(ji,jj) + zU(ji,jk)*zU(jj,jk)
           ENDDO
        ENDDO
     ENDDO


#ifdef ibm_sp

!    USE ESSL
     iopt=1
     n=npcvecs
     ldz=n
     naux=3*n

     allocate(w(n),z(n,n),aux(naux),ap(n*n))
     w=zero
     z=zero
     aux=zero

!    Load zuut in ESSL lower-packed storage mode
     ap=zero
     jk=0
     do jj=1,n
        do ii=jj,n
           jk=jk+1
           ap(jk)=zuut(ii,jj)
        end do
     end do

!    Call ESSL routines
     if (r_kind==N_DEFAULT_REAL_KIND) then
        call sspev(iopt,ap,w,z,ldz,n,aux,naux)
     ELSEIF (r_kind==N_DOUBLE_KIND) then
        call dspev(iopt,ap,w,z,ldz,n,aux,naux)
     else
        write(6,*)'SETUP_PRECOND: r_kind is neither default real nor double precision'
        call stop2(325)
     endif

!    Load ESSL results into output arrays
     do jj=1,n
        zlam(jj)=w(jj)
        do ii=1,n
           zuut(ii,jj)=z(ii,jj)
        end do
     end do
 
!    Deallocate work arrays
     deallocate(w,z,aux,ap)
 

#else
!    Use LAPACK routines
     if (r_kind==N_DEFAULT_REAL_KIND) then
        call SSYEV('V','L',npcvecs,zUUT,npcvecs,zlam,zwork,SIZE(zwork),info)
     ELSEIF (r_kind==N_DOUBLE_KIND) then
        call DSYEV('V','L',npcvecs,zUUT,npcvecs,zlam,zwork,SIZE(zwork),info)
     else
        write(6,*)'SETUP_PRECOND: r_kind is neither default real nor double precision'
        call stop2(325)
     endif
     if (info/=0) then
        write(6,*)'SETUP_PRECOND: SSYEV/DSYEV returned with info=',info
        write(6,*)'SETUP_PRECOND: SSYEV/DSYEV returned non-zero return code'
        call stop2(326)
     endif
  
#endif

!--- convert to eigenvalues of the preconditioner

     do jk=1,npcvecs
        zlam(jk) = one / (one - zlam(jk))
     ENDDO
  
     if (mype==0) write(6,*)'SETUP_PRECOND: eigenvalues found are: ',(zlam(ji),ji=1,npcvecs)

!--- sort eigenvalues with eigenvalues larger than 1 after eigenvalues
!--- smaller than 1 and with eigenvalues larger than 1 sorted in decreasing
!--- order

     do ji=1,npcvecs
        indarr(ji) = ji
     ENDDO
  
!--- straight insertion sort courtesy of Numerical Recipies

     do jj=2,npcvecs
        za = zlam(jj)
        ik = indarr(jj)
        do ji=jj-1,1,-1
           if (zlam(ji)>one .and. (zlam(ji)>=za .or. za<=one)) then
              ii=ji
              exit
           else
              ii=0
           endif
           zlam(ji+1) = zlam(ji)
           indarr(ji+1) = indarr(ji)
        ENDDO
        zlam(ii+1) = za
        indarr(ii+1) = ik
     ENDDO
  
     inpcv = npcvecs
  
     do while (zlam(inpcv) <= zero)
        if (mype==0) write(6,*)'Warning - eigenvalue less than 1: ',zlam(inpcv)
        inpcv = inpcv-1
        if (inpcv == 0) then
           if (mype==0) write(6,*)'SETUP_PRECOND: cannot form preconditioner - '//&
              'no positive eigenvalues.'
           if (mype==0) write(6,*)'SETUP_PRECOND: minimisation will not be preconditioned.'
           EXIT
        endif
     enddo
  
     IF (inpcv>0) THEN
        if (mype==0) write(6,*)'Number of preconditioning vectors selected is ',inpcv
        if (mype==0) write(6,*)'SETUP_PRECOND: selected eigenvalues are: ',(zlam(ji),ji=1,inpcv)
     
        IF (ALLOCATED(YVCGLPC)) THEN
           DO jj=1,NVCGLPC
              CALL DEALLOCATE_CV(YVCGLPC(jj))
           ENDDO
           DEALLOCATE(YVCGLPC)
           NVCGLPC=0
        ENDIF
        IF (ALLOCATED(RCGLPC)) DEALLOCATE(RCGLPC)

        !--- Save eigenvalues
        NVCGLPC = inpcv
        ALLOCATE (RCGLPC(NVCGLPC))
        RCGLPC(:) = MIN(R_MAX_CNUM_PC,zlam(1:NVCGLPC))
 
        ALLOCATE (YVCGLPC(NVCGLPC))
        DO jj=1,NVCGLPC
           CALL ALLOCATE_CV(YVCGLPC(jj))
        ENDDO

!--- apply Householder transformations to the eigenvectors to get the
!--- eigenvectors of the preconditioner

        DO jj=1,NVCGLPC
           YVCGLPC(jj) = zero
           CALL SET_CVSECTION(zuut(1:npcvecs,indarr(jj)),YVCGLPC(jj),1,npcvecs)
 
           do ji=npcvecs,1,-1
              zps = DOT_PRODUCT (yvcglwk(ji),YVCGLPC(jj))
              do jk=1,YVCGLPC(jj)%lencv
                 YVCGLPC(jj)%values(jk) = YVCGLPC(jj)%values(jk) - zps*yvcglwk(ji)%values(jk)
              enddo
           ENDDO
        ENDDO
        LMPCGL = .true.
     ELSE
        NVCGLPC = 0
        LMPCGL = .false.
     ENDIF

     deallocate(indarr)
     deallocate(zq,zlam,zU,zUUT,zwork,zzz)
  endif 
  NPCVECS = 0
  DO jj=1,npcvecs
     CALL DEALLOCATE_CV(YVCGLWK(jj))
  ENDDO
  DEALLOCATE(YVCGLWK)
  
  
  return
end subroutine setup_precond

! ------------------------------------------------------------------------------
!   PRECOND - Preconditioner for minimization
! ------------------------------------------------------------------------------
subroutine lanczos_precond(ycvx,kmat)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    precond
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2011-04-07  todling - renamed to from old precond name
!
!   input argument list:
!    ycvx
!    kmat
!
!   output argument list:
!    ycvx
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

IMPLICIT NONE

TYPE(CONTROL_VECTOR),INTENT(INOUT) :: ycvx
INTEGER(i_kind)     ,INTENT(IN   ) :: kmat

REAL(r_kind) :: zevals(NVCGLPC),zdp(NVCGLPC)
INTEGER(i_kind) :: jk, ji

if     (kmat== 1    ) then
   zevals(:) = RCGLPC(:)
ELSEIF (kmat==-1    ) then
   zevals(:) = one/RCGLPC(:)
ELSEIF (kmat== 2) then
   zevals(1:NVCGLPC) = sqrt(RCGLPC(:))
ELSEIF (kmat==-2) then
   zevals(1:NVCGLPC) = one/sqrt(RCGLPC(:))
else
   write(6,*)'Error: invalid value for kmat in precond: ',kmat
   write(6,*)'PRECOND: invalid value for kmat' 
   call stop2(327)
endif

do jk=1,NVCGLPC
   zdp(jk) = (zevals(jk)-one)*DOT_PRODUCT(ycvx,YVCGLPC(jk))
enddo

DO jk=1,NVCGLPC
   DO ji=1,ycvx%lencv
      ycvx%values(ji) = ycvx%values(ji) + YVCGLPC(jk)%values(ji) * zdp(jk)
   ENDDO
ENDDO

return
end subroutine lanczos_precond
! ------------------------------------------------------------------------------
subroutine read_lanczos(kmaxit)
!$$$  subprogram documentation block
!                .      .    .                                         .
! subprogram:    read_lanczos
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    kmaxit
!
!   output argument list:
!    kmaxit
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

IMPLICIT NONE

integer(i_kind) , intent(inout) :: kmaxit

integer(i_kind) :: jj, iunit, kiter, ilen
character(len=17) :: clfile

if (kmaxit>maxiter) then
   write(6,*) 'read_lanczos: kmaxit>maxiter',kmaxit,maxiter
   call stop2(141)
end if

do jj=1,kmaxit
   clfile='lanczvec.XXX.YYYY'
   write(clfile(10:12),'(I3.3)') jiter
   write(clfile(14:17),'(I4.4)') jj
   call read_cv(cglwork(jj),clfile)
enddo

if (mype==0) then
   iunit=get_lun()
   clfile='zlanczos.XXX'
   WRITE(clfile(10:12),'(I3.3)') jiter
   write(6,*)'Reading Lanczos coef. from file ',clfile

   open(iunit,file=trim(clfile),form='unformatted')
   read(iunit)kiter
   if (kiter>maxiter) then
      write(6,*)'read_laczos: kiter>maxiter',kiter,maxiter
      call stop2(142)
   end if
   read(iunit)zlancs(1:kiter+1,1:4)
   close(iunit)
endif
ilen=(kmaxit+1)*4
call mpl_bcast(0,ilen,zlancs)

end subroutine read_lanczos
! ------------------------------------------------------------------------------
end module lanczos
