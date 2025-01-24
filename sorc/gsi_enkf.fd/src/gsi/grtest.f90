module grdtest
!$$$ module documentation block
!           .      .    .                                       .
! module:   grdtest
!  prgmmr: todling
!
! abstract: Routines and data to perform gradient test
!
! program history log:
!   2009-01-18 todling
!   2010-02-19 treadon - wrap module
!
! subroutines included:
!   sub grtest
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
private
public grtest

contains

subroutine grtest(pdx,itertest,xhat_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grtest
!   prggmr: todling
!
! abstract: The aim is to characterize mathematically the cost-function J on a
!           line in the KDIM-dimensional space defined by point X and direction H.
!           Arbitrarily H is defined by -gradJ(X), the useful direction for
!           minimization. Let us denote by f the real function f(a)=J(X+a.H). A
!           sequence of characteristic quantities are computed and displayed for
!           various values of a :
!                    a=PDX, PDX*10, PDX*100 ... PDX*10**itertest
!           The sequence is prematuraly terminated if f(a)-f(0) changes of sign,
!           which normally indicates we are overshooting the minimum of f (assuming
!           J is convex and H points downwards the slope of J). The characteristic
!           quantities TC0,T1,TC1,T2... test increasingly high orders of regularity.
!           Refer to the comments below for the explanation of each quantity.
!             Owing to numerical truncation errors, the tests normally fail for very 
!           small perturbations. The maximum quality of the test results, even for a
!           bug-free simulator, is limited to a few digits, depending on the machine.
!
! program history log:
!   2009-01-18 todling - some quad precision changes (incomplete)
!   2010-05-05 treadon - use r_kind constant in huge()
!   2010-08-19 lueken  - add only to module use
!
!   input argument list:
!    xhat
!    pdx
!    itertest
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: i_kind, r_kind, r_quad
use constants, only: zero,zero_quad, one_quad
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv, &
    random_cv,deallocate_cv,dot_product,assignment(=)

implicit none

real(r_quad)        , intent(in   ) :: pdx 
integer(i_kind)     , intent(in   ) :: itertest 
type(control_vector), optional, intent(in   ) :: xhat_in

! Local variables
real(r_quad), parameter :: half_quad=0.5_r_quad
real(r_quad), parameter ::  two_quad=2.0_r_quad
type(control_vector) :: xdir,yhat,grad,xhat
real(r_quad) :: zabuf(itertest),zfabuf(itertest),ztf2buf(itertest)
real(r_quad) :: zfy,zf0,zdf0,za,zfa,zdfa
real(r_quad) :: ZT1,ZB,ZFB,ztco,ZTC1,ZT2,ZTC1A,ZTC2,ZTF2
real(r_quad) :: ZTF2L
real(r_quad) :: ZTC00,ZTC02,ZTC10,ZTC12
real(r_quad) :: ZERMIN,ZT1TST,ZREF
integer(i_kind) :: ibest,idig,jj,nprt,ii
logical :: lsavinc

!-----------------------------------------------------------------------------

if (mype==0) write(6,*)'grtest: starting'
if (pdx<=EPSILON(pdx)) then
   if (mype==0) write(6,*)'grtest, pdx=',pdx
   write(6,*)'grtest: pdx too small',pdx
   call stop2(131)
endif
lsavinc=.false.
nprt=1

call allocate_cv(xdir)
call allocate_cv(yhat)
call allocate_cv(grad)
call allocate_cv(xhat)

!       1.0 Initial point
!           -------------

if (present(xhat_in)) then
   xhat=xhat_in
   if (mype==0) write(6,*)'grtest: use input xhat'
else
   call random_cv(xhat)
   if (mype==0) write(6,*)'grtest: use random_cv(xhat)'
endif
yhat=xhat
call evaljgrad(yhat,zfy,grad,lsavinc,nprt,'grtest')
zfy=half_quad*zfy

!          1.1 Define perturbation direction ZH

if (mype==0) write(6,*) 'The test direction is the opposite of the gradient'
do ii=1,xdir%lencv
   xdir%values(ii)=-grad%values(ii)
end do

!          1.2 Set function f value and derivative at origin

zf0=zfy
zdf0=dot_product(grad,xdir,r_quad)
if (mype==0) write(6,*)'grtest: F(0)=',zf0,' DF(0)=',zdf0

IF (ZDF0>zero_quad.and.mype==0) write(6,*) 'GRTEST Warning, DF should be negative'
IF (ABS(ZDF0) < SQRT(EPSILON(ZDF0))) THEN
   if (mype==0) write(6,*) 'GRTEST WARNING, DERIVATIVE IS TOO SMALL'
ENDIF

!       2. Loop on test point
!          ------------------
ztf2buf(1)=zero_quad
DO jj=1,itertest

   za=pdx*(10.0_r_quad**(jj-1))

   if (mype==0) write(6,*)'grtest iter=',jj,' alpha=',za

!           2.1 Compute f and df at new point y=x+a.h

   do ii=1,yhat%lencv
      yhat%values(ii) = xhat%values(ii) + za * xdir%values(ii)
   end do

   call evaljgrad(yhat,zfy,grad,lsavinc,nprt,'grtest')
   zfy=half_quad*zfy

   zfa=zfy
   zdfa=dot_product(grad,xdir,r_quad)

   if (mype==0) write(6,*)'grtest: alpha=',za,' F(a)=',zfa,' DF(a)=',zdfa

   zabuf(jj)=za
   zfabuf(jj)=zfa

!          2.2 Quantity TC0=f(a)/f(0)-1

!         if f is continuous then TC0->1 at origin,
!         at least linearly with a.

   IF (ABS(zf0)<=TINY(zf0)) THEN
!           do not compute T1 in this unlikely case
      if (mype==0) write(6,*) 'grtest: Warning: zf0 is suspiciously small.'
      if (mype==0) write(6,*) 'grtest: F(a)-F(0)=',zfa-zf0
   ELSE
      ztco=zfa/zf0-one_quad
      if (mype==0) write(6,*)'grtest: continuity TC0=',ztco
   ENDIF

!                           f(a)-f(0)
!          2.3 Quantity T1=-----------
!                            a.df(0)

!         if df is the gradient then T1->1 at origin,
!         linearly with a. T1 is undefined if df(0)=0.
   IF (ABS(za*zdf0)<=SQRT(TINY(zf0))) THEN
      if (mype==0) write(6,*)'grtest: Warning: could not compute ',&
       & 'gradient test T1, a.df(0)=',za*zdf0  
   ELSE
      zt1=(zfa-zf0)/(za*zdf0)
      if (mype==0) write(6,*)'grtest: gradient T1=',zt1
   ENDIF

!         2.4 Quantity TC1=( f(a)-f(0)-a.df(0) )/a

!         if df is the gradient and df is continuous,
!         then TC1->0 linearly with a.
   ZTC1=(ZFA-ZF0-ZA*ZDF0)/ZA
   if (mype==0) write(6,*)'grtest: grad continuity TC1=',ZTC1

!         2.5 Quantity T2=( f(a)-f(0)-a.df(0) )*2/a**2

!         if d2f exists then T2 -> d2f(0) linearly with a.
   ZT2=(ZFA-ZF0-ZA*ZDF0)*two_quad/(ZA**2)
   if (mype==0) write(6,*)'grtest: second derivative T2=',ZT2

!         2.6 Quantity TC1A=df(a)-df(0)

!         if df is the gradient in a and df is continuous,
!         then TC1A->0 linearly with a.
   ZTC1A=ZDFA-ZDF0
   if (mype==0) write(6,*)'grtest: a-grad continuity TC1A=',ZTC1A

!         2.7 Quantity TC2=( 2(f(0)-f(a))+ a(df(0)+df(a))/a**2

!         if f is exactly quadratic, then TC2=0, always: numerically
!         it has to -> 0 when a is BIG. Otherwise TC2->0 linearly for
!         small a is trivially implied by TC1A and T2.
   ZTC2=(two_quad*(ZF0-ZFA)+ZA*(ZDF0+ZDFA))/(ZA**2)
   if (mype==0) write(6,*)'grtest: quadraticity TC2=',ZTC2

!                           2   f(0)-f(b)   f(a)-f(b)
!         2.8 Quantity TF2=---( --------- + --------- )
!                           a       b          a-b
!         if 0, a and b are distinct and f is quadratic then
!         TF2=d2f, always. The estimate is most stable when a,b are big.
!         This test works only after two loops, but it is immune against
!         gradient bugs. 
   IF (jj>=2) THEN
      ZB =ZABUF (jj-1)
      ZFB=ZFABUF(jj-1)
      ZTF2=two_quad/ZA*((ZF0-ZFB)/ZB+(ZFA-ZFB)/(ZA-ZB))
      if (mype==0) write(6,*)'grtest: convexity ZTF2=',ZTF2
      ztf2buf(jj)=ztf2
   ENDIF

! End loop
ENDDO

call deallocate_cv(xdir)
call deallocate_cv(yhat)
call deallocate_cv(grad)
call deallocate_cv(xhat)

!          3. Comment on the results

!       TC0(0)/TC0(2)<.011 -> df looks continuous
!       item with (T1<1 and 1-T1 is min) = best grad test item
!       reldif(TF2(last),TF2(last-1)) = precision on quadraticity

!          3.1 Fundamental checks

if (mype==0) then
   write(6,*) 'GRTEST: TENTATIVE CONCLUSIONS :'

   ZTC00=ABS(zfabuf(1)-zf0)
   ZTC02=ABS(zfabuf(3)-zf0)
   IF( ZTC00/zabuf(1)  <=  1.5_r_quad*(ZTC02/zabuf(3)) )THEN
      write(6,*) 'GRTEST: function f looks continous.'
   ELSE
      write(6,*) 'GRTEST: WARNING f does not look continuous',&
       & ' (perhaps truncation problem)'  
   ENDIF

!          3.2 Gradient quality

   IF (ABS(zdf0)<=SQRT(TINY(zf0))) THEN
      write(6,*) 'GRTEST: The gradient is 0, which is unusual !'
      ZTC10=ABS(zfabuf(1)-zf0)
      ZTC12=ABS(zfabuf(3)-zf0)
      IF( ZTC10/zabuf(1)**2  <=  1.1_r_quad*ZTC12/zabuf(3)**2)THEN
         write(6,*)'GRTEST: The gradient looks good anyway.'
      ENDIF
   ELSE
!     Find best gradient test index
      ZERMIN=HUGE(zero)
      ibest=-1
      DO jj=1,itertest
         ZT1TST=(zfabuf(jj)-zf0)/(zabuf(jj)*zdf0)
         ZT1TST=ABS(ZT1TST-one_quad)
         IF (ZT1TST<ZERMIN) THEN
            ibest=jj
            ZERMIN=ZT1TST
         ENDIF
      ENDDO
      IF(ibest == -1)THEN
         write(6,*)'GRTEST: gradient test problem : bad ',&
          & 'gradient, non-convex cost, or truncation errors ?'  
      ELSE
         idig=INT(-LOG(ZERMIN+TINY(ZERMIN))/LOG(10.0_r_quad))
         write(6,*)'GRTEST: the best gradient test found has ',&
          & idig,' satisfactory digits.'  
         IF(idig <= 1)THEN
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS VERY BAD.'
         ELSEIF(idig <= 3)THEN
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS SUSPICIOUS.'
         ELSEIF(idig <= 5)THEN
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS ACCEPTABLE.'
         ELSE
            write(6,*)'GRTEST: SAYS: THE GRADIENT IS EXCELLENT.'
         ENDIF

         IF (ibest<=itertest-2) THEN
            ZTC10=ABS(zfabuf(ibest         )-zf0-zabuf(ibest         )*zdf0)/zabuf(ibest         )
            ZTC12=ABS(zfabuf(ibest+2)-zf0-zabuf(ibest+2)*zdf0)/zabuf(ibest+2)
            IF(ZTC10/zabuf(ibest) <=  1.1_r_quad*ZTC12/zabuf(ibest+2) )THEN
               write(6,*)'GRTEST: Gradient convergence looks good.'
            ELSE
               write(6,*)'GRTEST: Gradient convergence is suspicious.'
            ENDIF
         ELSE
            write(6,*)'GRTEST: could not check grad convergence.'
         ENDIF
      ENDIF
   ENDIF

!            3.3 Quadraticity
!         finite difference quadraticity test (gradient-free)

   ZTF2=ztf2buf(itertest)
   ZTF2L=ztf2buf(itertest-1)
   write(6,*) 'GRTEST: finite diff. d2f estimate no1:',ZTF2
   write(6,*) 'GRTEST: finite diff. d2f estimate no2:',ZTF2L
   ZREF=(ABS(ZTF2L)+ABS(ZTF2))/two_quad
   IF (ZREF<=TINY(ZREF)) THEN
      write(6,*) 'GRTEST: they are too small to decide whether ',&
       & 'they agree or not.'  
   ELSE
      idig=INT(-LOG(ABS(ZTF2L-ZTF2)/ZREF+TINY(ZTF2))/LOG(10.0_r_quad))
      write(6,*) 'GRTEST: the fin.dif. estimates of d2f ',&
       & 'have ',idig,' satisfactory digits.'
      IF(idig <= 1)THEN
         write(6,*) 'GRTEST: THE FD-QUADRATICITY IS BAD.'
      ELSEIF(idig <= 3)THEN
         write(6,*) 'GRTEST:: THE FD-QUADRATICITY IS SUSPICIOUS.'
      ELSEIF(idig <= 5)THEN
         write(6,*) 'GRTEST: THE FD-QUADRATICITY IS ACCEPTABLE.'
      ELSE
         write(6,*) 'GRTEST: THE FD-QUADRATICITY IS EXCELLENT.'
      ENDIF
   ENDIF

   write(6,*) 'grtest: Goodbye.'
endif

return
END SUBROUTINE grtest
! ----------------------------------------------------------------------
end module grdtest
