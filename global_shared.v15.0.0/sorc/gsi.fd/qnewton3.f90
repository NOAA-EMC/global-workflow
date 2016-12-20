module qnewton3
!$$$ module documentation block
!           .      .    .                                       .
! module:   qnewton3
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-10  lueken - added module doc block
!   2010-08-19  lueken - add only to module use;no machine code, so use .f90
!
! subroutines included:
!   sub m1qn3
!   sub dd
!   sub mlis0
!   sub ecube
!
! Notes:
!
!     This file contains the M1QN3 algorithm and supporting routines.
!     Modified routines from:
!     J.Ch. Gilbert and C. Lemarechal (1989). Some numerical experiments with
!     variable-storage quasi-Newton algorithms. Mathematical Programming 45, 407-435.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use constants, only: zero, one
use mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv, &
    deallocate_cv,inquire_cv,maxval,dot_product, &
    assignment(=)

implicit none

private
public m1qn3

real(r_kind), parameter :: GTOL = 0.9_r_kind
real(r_kind), parameter :: FTOL = 1.0e-4_r_kind

! ------------------------------------------------------------------------------
CONTAINS
! ------------------------------------------------------------------------------
subroutine m1qn3(x,f,g,epsg,nsim,nprt,maxvecs)
! ------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1qn3
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-10  lueken - added subprogram doc block
!
!   input argument list:
!    x,g
!    f
!    epsg
!    nsim
!    maxvecs
!    nprt
!
!   output argument list:
!    x,g
!    f
!    epsg
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use constants, only: two
use qcmod, only: nlnqc_iter
use jfunc, only: iter,jiter,niter,niter_no_qc
use timermod, only: timer_ini,timer_fnl

implicit none

type(control_vector), intent(inout) :: x,g
real(r_kind)        , intent(inout) :: f
real(r_kind)        , intent(inout) :: epsg
integer(i_kind)     , intent(in   ) :: nsim
integer(i_kind)     , intent(in   ) :: maxvecs
integer(i_kind)     , intent(in   ) :: nprt

type(control_vector) :: ybar(maxvecs), sbar(maxvecs)
type(control_vector) :: d,gg,aux,ztemp
integer(i_kind) :: isim,jcour,jmin,jmax,ii,jj
real(r_kind) :: r1,t,tmin,tmax,gnorm,eps1,ff,precos,ys,ps,hp0,dxmin,df1
real(r_kind) :: yy,ss

call timer_ini('m1qn3')
!
!---- impressions initiales et controle des arguments
!
dxmin=sqrt(EPSILON(dxmin))
df1=f/10.0_r_kind

if (mype==0) then
   write(6,*)'m1qn3: number of updates (maxvecs):',maxvecs
   write(6,*)'m1qn3: absolute precision on x (dxmin):',dxmin
   write(6,*)'m1qn3: expected decrease for f (df1):',df1
   write(6,*)'m1qn3: relative precision on g (epsg):',epsg
   write(6,*)'m1qn3: maximal number of iterations (niter):',niter(jiter)
   write(6,*)'m1qn3: maximal number of simulations (nsim):',nsim
endif

if ((niter(jiter)<=0).or.(nsim<=0).or.(maxvecs<=0).or.(dxmin<=zero).or.(epsg<=zero).or.(epsg>one)) then
   write(6,*)'m1qn3: inconsistent call',niter(jiter),nsim,maxvecs,dxmin,epsg,epsg
   call stop2(164)
endif
!
!---- initialisation
!
call allocate_cv(d)
call allocate_cv(gg)
call allocate_cv(aux)
call allocate_cv(ztemp)
do ii=1,maxvecs
   call allocate_cv(sbar(ii))
   call allocate_cv(ybar(ii))
   sbar(ii)=zero ! just to check memory
   ybar(ii)=zero ! just to check memory
enddo
call inquire_cv

!
iter=0
isim=1
eps1=one
tmax=1.e+20_r_kind
!
ps = dot_product(g,g)
gnorm=sqrt(ps)

if (gnorm<sqrt(EPSILON(gnorm))) then
   write(6,*)'m1qn3: initial gradient is too small',gnorm
   call stop2(165)
end if
!
! --- initialisation pour dd
!
jmin=1
jmax=0
jcour=jmax
!
! --- mise a l'echelle de la premiere direction de descente
! --- use Fletcher's scaling
!
precos=two*df1/gnorm**2
do jj=1,d%lencv
   d%values(jj)  = -g%values(jj) * precos
enddo
!
!---- Debut de l'iteration. on cherche x(k+1) de la forme x(k) + t*d,
!     avec t > 0. On connait d.
!
iter_loop:do
!
! --- initialisation pour mlis0
!
   hp0 = dot_product(d,g)
   if (hp0>=zero) then
      write(6,*)'m1qn3 iteration',iter, &
                'd is not a descent direction: (g,d) =',hp0
      write(6,*)'m1qn3: d is not a descent direction'
      call stop2(166)
   endif
!
   iter=iter+1
   if (mype==0.and.nprt>=1) then
      write(6,910) iter,isim,f,hp0
      910 format (" m1qn3: iter ",i3,", simul ",i3,", f=",es15.7,", h'(0)=",es15.7)
   endif
   nlnqc_iter = iter >= niter_no_qc(jiter)
!
   gg=g
   ff=f
!
! --- recherche lineaire et nouveau point x(k+1)
!
! --- calcul de tmin
!
   do jj=1,d%lencv
      ztemp%values(jj)=abs(d%values(jj))
   enddo
   tmin=maxval(ztemp)
   tmin=dxmin/tmin
   t=one
   r1=hp0
!
   call mlis0(x,f,r1,t,tmin,tmax,d,g,nprt,isim,nsim,aux,ztemp)
!
   eps1=dot_product(g,g)
   if (mype==0) write(6,*)'m1qn3: iteration=',iter,' t,grad =',t,eps1
   eps1=sqrt(eps1)/gnorm

!
! --- mise a jour des pointeurs
!
   jmax=jmax+1
   if (jmax>maxvecs) jmax=1
   if (iter>maxvecs) then
      jmin=jmin+1
      if (jmin>maxvecs) jmin=1
   endif
   jcour=jmax
!
! --- y, s et (y,s)
!
   do jj=1,sbar(jcour)%lencv
      sbar(jcour)%values(jj) = t * d%values(jj)
   enddo
   do jj=1,ybar(jcour)%lencv
      ybar(jcour)%values(jj) = g%values(jj) - gg%values(jj)
   enddo

   yy = DOT_PRODUCT (ybar(jcour),ybar(jcour))
   ss = DOT_PRODUCT (sbar(jcour),sbar(jcour))
   ys = DOT_PRODUCT (ybar(jcour),sbar(jcour))
   if (mype==0) write(6,*)'m1qn3: iteration=',iter,' ys,yy,ss =',ys,yy,ss
   if (ys<=zero) then
      write(6,*)'m1qn3: iteration=',iter,' (y,s) =',ys
      write(6,*)'m1qn3: the scalar product (y,s) is not positive'
      call stop2(167)
   endif
!
! --- ybar et sbar
!
   r1=sqrt(one/ys)
   do jj=1,sbar(jcour)%lencv
      sbar(jcour)%values(jj) = r1 * sbar(jcour)%values(jj)
   enddo
   do jj=1,ybar(jcour)%lencv
      ybar(jcour)%values(jj) = r1 * ybar(jcour)%values(jj)
   enddo
!
! --- compute the scalar preconditioner
!
   precos = one/DOT_PRODUCT(ybar(jcour),ybar(jcour))
!
! --- tests d'arret
!
   if (mype==0) write(6,*)'m1qn3: gradient norm reduction=',eps1
   if (eps1<epsg) exit iter_loop

   if (iter==niter(jiter)) then
      if (mype==0) write(6,*)'m1qn3: maximal number of iterations reached'
      exit iter_loop
   endif

   if (isim>=nsim) then
      if (mype==0) write(6,*)'m1qn3: maximal number of simulations reached'
      exit iter_loop
   endif
!
! --- calcul de la nouvelle direction de descente d = - H.g
!
   do jj=1,d%lencv
      d%values(jj) = -g%values(jj)
   enddo
   call dd(maxvecs,d,jmin,jmax,precos,ybar,sbar)
!
!---- on poursuit les iterations
!
enddo iter_loop
!
!---- retour
!
epsg=eps1
!
!---- impressions finales
!
if (mype==0) then
   write(6,*)'m1qn3: number of iterations =',iter
   write(6,*)'m1qn3: number of simulations =',isim
   write(6,*)'m1qn3: achieved relative precision on g=',epsg
endif

call deallocate_cv(d)
call deallocate_cv(gg)
call deallocate_cv(aux)
call deallocate_cv(ztemp)
do ii=1,maxvecs
   call deallocate_cv(sbar(ii))
   call deallocate_cv(ybar(ii))
enddo

call timer_fnl('m1qn3')
return
end subroutine m1qn3
! ------------------------------------------------------------------------------
subroutine dd(maxvecs,ycv,jmin,jmax,precos,ybar,sbar)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dd
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-10  lueken - added subprogram doc block
!
!   input argument list:
!    maxvecs,jmin,jmax
!    precos
!    ycv
!    ybar,sbar
!
!   output argument list:
!    ycv
!
! Notes:
!
!     calcule le produit H.g ou
!         H est une matrice construite par la formule de bfgs inverse
!           (cf. J. Nocedal, Math. of Comp. 35/151 (1980) 773-782)
!         ycv est un vecteur de dimension n (en general le gradient)
!
!     la matrice H est memorisee par les vecteurs des tableaux
!     ybar, sbar et les pointeurs jmin, jmax
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

integer(i_kind)     , intent(in   ) :: maxvecs,jmin,jmax
real(r_kind)        , intent(in   ) :: precos
type(control_vector), intent(inout) :: ycv
type(control_vector), intent(in   ) :: ybar(maxvecs),sbar(maxvecs)

integer(i_kind) :: jfin,ii,jp,jj
real(r_kind) :: rr,ps,alpha(maxvecs)

jfin=jmax
if (jfin<jmin) jfin=jmax+maxvecs
!
!     phase de descente
!
do ii=jfin,jmin,-1
   jp=ii
   if (jp>maxvecs) jp=jp-maxvecs
   ps = DOT_PRODUCT (ycv,sbar(jp))
   rr=ps
   alpha(jp)=rr
   do jj=1,ycv%lencv
      ycv%values(jj) = ycv%values(jj) -rr * ybar(jp)%values(jj)
   enddo
enddo
!
!     preconditionnement
!
do jj=1,ycv%lencv
   ycv%values(jj) = ycv%values(jj) * precos
enddo
!
!     remontee
!
do ii=jmin,jfin
   jp=ii
   if (jp>maxvecs) jp=jp-maxvecs
   ps = DOT_PRODUCT (ycv,ybar(jp))
   rr=alpha(jp)-ps
   do jj=1,ycv%lencv
      ycv%values(jj) = ycv%values(jj) + rr * sbar(jp)%values(jj)
   enddo
enddo

return
end subroutine dd
! ------------------------------------------------------------------------------
subroutine mlis0(xn,fn,fpn,t,tmin,tmax,d,g,imp,nap,napmax,x,ztemp)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mlis0
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-01-18 Todling - minimal change to interface w/ evaljgrad (quad) properly
!                        NOTE: no attempt made to make code reproduce across pe's yet
!   2009-08-10  lueken - added subprogram doc block
!
!   input argument list:
!    imp,napmax
!    nap
!    fn,fpn,t,tmin,tmax
!    xn,g,x
!    d
!    ztemp
!
!   output argument list:
!    nap
!    fn,fpn,t,tmin,tmax
!    xn,g,x
!    ztemp
!
! Notes:
!        en sortie logic =
!
!        0          descente serieuse
!        1          descente bloquee
!        4          nap > napmax
!        5          retour a l'utilisateur
!        6          fonction et gradient pas d'accord
!        < 0        contrainte implicite active
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use constants, only: two,five
implicit none

integer(i_kind)      , intent(in   ) :: imp,napmax
integer(i_kind)      , intent(inout) :: nap
real(r_kind)         , intent(inout) :: fn,fpn,t,tmin,tmax
type (control_vector), intent(inout) :: xn,g,x
type (control_vector), intent(in   ) :: d
type (control_vector), intent(inout) :: ztemp

real(r_quad) :: fquad
real(r_kind) :: tesf,tesd,tg,fg,fpg,td,ta,fa,fpa,d2,f,fp,ffn,fd,fpd, &
     z,test,barmin,barmul,barmax,barr,gauche,droite, &
     taa,zmaxp
integer(i_kind) :: jj,io,logic
logical :: lsavinc

if (.not.(fpn<zero .and. t>zero &
         .and. tmax>zero .and. ftol>zero &
         .and. gtol>ftol .and. gtol<one)) then
   write(6,*)'mlis0: error input parameters',fpn,t,tmax,ftol,gtol
   call stop2(168)
endif

tesf  = ftol*fpn
tesd  = gtol*fpn
barmin= 0.01_r_kind
barmul= five
barmax= 0.3_r_kind
barr  = barmin
td    = zero
tg    = zero
fg    = fn
fpg   = fpn
ta    = zero
fa    = fn
fpa   = fpn
d2    = dot_product(d,d)
!
!               elimination d'un t initial ridiculement petit
!
if(t<=tmin) then
   t=tmin
   if(t>tmax) then
      if (imp>0) write (io,1007)
      tmin=tmax
   endif
endif

do
   if (fn+t*fpn<fn+0.9_r_kind*t*fpn) exit
   t=two*t
enddo

logic=0
if (t>tmax) then
   t=tmax
   logic=1
endif
if (imp>=4) write (io,1000) fpn,d2,tmin,tmax
!
!     --- nouveau x
!
do jj=1,x%lencv
   x%values(jj) = xn%values(jj) + t * d%values(jj)
enddo
!
! --- boucle
!
boucle:do
   nap=nap+1
   if (nap>napmax) then
      logic=4
      fn=fg
      do jj=1,xn%lencv
         xn%values(jj) = xn%values(jj) + tg * d%values(jj)
      enddo
      exit boucle
   endif
!
! --- appel simulateur
!
   lsavinc=.false.
   call evaljgrad(x,fquad,g,lsavinc,imp,'mlis0')
   f=fquad
!
! --- les tests elementaires sont faits, on y va
!
   fp = dot_product(d,g)
!
! --- premier test de Wolfe
!
   ffn=f-fn
   if (ffn>t*tesf) then
      td   =t
      fd   =f
      fpd  =fp
      logic=0
      if (imp>=4) write (io,1002) t,ffn,fp
      go to 500
   endif
!
! --- test 1 ok, donc deuxieme test de Wolfe
!
   if (imp>=4) write (io,1003) t,ffn,fp
   if (fp>tesd) then
      logic=0
      go to 320
   endif
   if (logic==0) go to 350
!
!     --- test 2 ok, donc pas serieux, on sort
!
320 continue
   fn=f
   xn =x
   exit
!
!
!
350 continue
   tg=t
   fg=f
   fpg=fp
!
!      extrapolation
!
   if (td==zero) then
      taa   =t
      gauche=(one+barmin)*t
      droite=10._r_kind*t
      call ecube(t,f,fp,ta,fa,fpa,gauche,droite)
      ta=taa
      if(t>=tmax) then
         logic=1
         t=tmax
      endif
      go to 900
   endif
!
!      interpolation
!
500  continue
   test=barr*(td-tg)
   gauche=tg+test
   droite=td-test
   taa=t
   call ecube(t,f,fp,ta,fa,fpa,gauche,droite)
   ta=taa
   if (t>gauche .and. t<droite) then
      barr=max(barmin,barr/barmul)
!      barr=barmin
   else
      barr=min(barmul*barr,barmax)
   endif
!
! --- fin de boucle
!     - t peut etre bloque sur tmax
!       (venant de l'extrapolation avec logic=1)
!
900 continue
   fa=f
   fpa=fp
!
! --- faut-il continuer ?
!
   if (td/=zero) then
      if (td-tg>=tmin) then
!
!     --- limite de precision machine (arret de secours) ?
!
         ztemp=zero
         do jj=1,d%lencv
            z               =xn%values(jj)+t*d%values(jj)
            ztemp%values(jj)=max(abs(z-xn%values(jj)),abs(z-x%values(jj)))
         enddo
         zmaxp=maxval(ztemp)

         if(zmaxp/=zero) go to 950
!
! --- arret sur dxmin ou de secours
!
      endif
      logic=6
!
!     si tg=0, xn = xn_depart,
!     sinon on prend xn=x_gauche qui fait decroitre f
!
      if (tg/=zero) then
         fn=fg
         do jj=1,xn%lencv
            xn%values(jj) = xn%values(jj) + tg * d%values(jj)
         enddo
      endif

      write (io,1001)
      write (io,1005) tg,fg,fpg
      if (logic==6) write (io,1005) td,fd,fpd
      if (logic==7) write (io,1006) td
      exit
   endif

950 continue
!
!               recopiage de x et boucle
!
   do jj=1,x%lencv
      x%values(jj) = xn%values(jj) + t * d%values(jj)
   enddo

enddo boucle

return

 1000 format (/4x," mlis0   ",4x,"fpn=",e10.3," d2=",e9.2,"  tmin=",e9.2," tmax=",e9.2)
 1001 format (/4x," mlis0",3x,"stop on tmin",8x,"step",11x,"functions",5x,"derivatives")
 1002 format (4x," mlis0",37x,e10.3,2e11.3)
 1003 format (4x," mlis0",e14.3,2e11.3)
 1004 format (4x," mlis0",37x,e10.3)
 1005 format (4x," mlis0",14x,2e18.8,e11.3)
 1006 format (4x," mlis0",14x,e18.8)
 1007 format (/4x," mlis0",10x,"tmin forced to tmax")
 1008 format (/4x," mlis0",10x,"inconsistent call")

end subroutine mlis0
! ------------------------------------------------------------------------------
subroutine ecube(t,f,fp,ta,fa,fpa,tlower,tupper)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ecube
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-10  lueken - added subprogram doc block
!
!   input argument list:
!    t,f,fp,ta,fa,fpa,tlower,tupper
!
!   output argument list:
!    t,f,fp,ta,fa,fpa,tlower,tupper
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use constants, only: three
implicit none

real(r_kind), intent(inout) :: t,f,fp,ta,fa,fpa,tlower,tupper

real(r_kind) :: z1,b,discri,sign,den,anum
!
!  Using f and fp at t and ta, computes new t by cubic formula
!  safeguarded inside [tlower,tupper].
!
z1=fp+fpa-three*(fa-f)/(ta-t)
b =z1+fp
!
!  first compute the discriminant (without overflow)
!
if (abs(z1)<=one) then
   discri=z1*z1-fp*fpa
else
   discri=fp/z1
   discri=discri*fpa
   discri=z1-discri
   if (z1>=zero .and. discri>=zero) then
      discri=sqrt(z1)*sqrt(discri)
      go to 120
   endif
   if (z1<=zero .and. discri<=zero) then
      discri=sqrt(-z1)*sqrt(-discri)
      go to 120
   endif
   discri=-one
endif
if (discri<zero) then
   if (fp<zero)  t=tupper
   if (fp>=zero) t=tlower
   go to 900
endif
!
!  discriminant nonnegative, compute solution (without overflow)
!
discri=sqrt(discri)
120 continue
if (t-ta<zero) discri=-discri
sign=(t-ta)/abs(t-ta)
if (b*sign>zero) then
   t=t+fp*(ta-t)/(b+discri)
else
   den =z1+b+fpa
   anum=b-discri
   if (abs((t-ta)*anum)<(tupper-tlower)*abs(den)) then
      t=t+anum*(ta-t)/den
   else
      t=tupper
   endif
endif
900 continue
t=max(t,tlower)
t=min(t,tupper)

return
end subroutine ecube
! ------------------------------------------------------------------------------
end module qnewton3
