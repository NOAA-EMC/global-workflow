!                                      ***************************************
!                                      *            Module pvqc              *
!                                      *  R. J. Purser, NOAA/NCEP/EMC 2017   *
!                                      *  jim.purser@noaa.gov                *
!                                      *                                     *
!                                      ***************************************
!
! Utility routines for tasks related to the specification of appropriate
! weight-factors to modulate the "standard" observation weight in a 
! variational assimilation. The observations are taken to be subject to the 
! forms of variational ("nonlinear") quality control based on the super-logistic
! family of distributions described in Office Note 468, or the simplified
! Huber-like approximate idealizations of these distributions (whose inner
! segments of O-A are taken to be exactly Gaussian and whose tails are
! taken to be the pure asymptote functions assumed by the corresponding
! super-logistic distributions).
! 
! DIRECT DEPENDENCIES:
! Modules: kinds, pietc, pvqc_tables
!==============================================================================
module pvqc
!==============================================================================
use kinds, only: dp,i_kind
use pietc, only: T,F,u0,u1,u2,o2,pi,pih
implicit none
private
public:: atote,wipevqctables,writevqcascfile,readvqcascfile,&
         writevqcdatfile,readvqcdatfile,                    &
         vqch,vqcs
interface atote;           module procedure atote,atoted,atotedd; end interface
interface wipevqctables;   module procedure wipevqctables;        end interface
interface writevqcascfile; module procedure writevqcascfile;      end interface
interface readvqcascfile;  module procedure readvqcascfile;       end interface
interface writevqcdatfile; module procedure writevqcdatfile;      end interface
interface readvqcdatfile;  module procedure readvqcdatfile;       end interface
interface vqch
   module procedure vqch_iii,vqch_ii,vqch_i, vqch_r;              end interface
interface vqcs
   module procedure vqcs_iii,vqcs_ii,vqcs_i, vqcs_r;              end interface

contains

!=============================================================================
subroutine atote(alpha,beta,kappa,x,y)!                                [atote]
!=============================================================================
! For shape parameters alpha, beta, kappa, and standardardized residual
! x, reurn the value, y,  of the standard asymptote at x of the superlogistic.
!=============================================================================
implicit none
real(dp),intent(in ):: alpha,beta,kappa,x
real(dp),intent(out):: y
!-----------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4_dp
real(dp)          :: b,c,bap,bac
!=============================================================================
c=kappa+1_dp
b=tan(pio4*(u2-beta))/c
bap=b*(u1+alpha)
bac=b*(u1-alpha)
if(x<0_dp)then; y=-bap*(-x)**c
else;        y=-bac*( x)**c
endif
end subroutine atote
!=============================================================================
subroutine atoted(alpha,beta,kappa,x,y,yd)!                            [atote]
!=============================================================================
! Like atote, but also return the 1st derivative, yd=dy/dx, at x.
!=============================================================================
implicit none
real(dp),intent(in ):: alpha,beta,kappa,x
real(dp),intent(out):: y,yd
!-----------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4_dp
real(dp)          :: b,c,bap,bac
!=============================================================================
c=kappa+1_dp
b=tan(pio4*(u2-beta))/c
bap=b*(u1+alpha)
bac=b*(u1-alpha)
if(x<0_dp)then; y=-bap*(-x)**c; yd=c*y/x
else;        y=-bac*( x)**c; yd=c*y/x
endif
end subroutine atoted
!=============================================================================
subroutine atotedd(alpha,beta,kappa,x,y,yd,ydd)!                       [atote]
!=============================================================================
! Like atoted, but also return the 2nd derivative, ydd, at x.
!=============================================================================
implicit none
real(dp),intent(in ):: alpha,beta,kappa,x
real(dp),intent(out):: y,yd,ydd
!-----------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4_dp
real(dp)          :: b,c,bap,bac
!=============================================================================
c=kappa+1_dp
b=tan(pio4*(u2-beta))/c
bap=b*(u1+alpha)
bac=b*(u1-alpha)
if(x<0_dp)then; y=-bap*(-x)**c; yd=c*y/x; ydd=(c-1_dp)*yd/x
else;           y=-bac*( x)**c; yd=c*y/x; ydd=(c-1_dp)*yd/x
endif
end subroutine atotedd

!=============================================================================
subroutine wipevqctables!                                      [wipevqctables]
!=============================================================================
use pvqc_tables, only: sgt,swt,x1t,x2t,xat,yat,linitvqc
implicit none
if(allocated(sgt))deallocate(sgt)
if(allocated(swt))deallocate(swt)
if(allocated(x1t))deallocate(x1t)
if(allocated(x2t))deallocate(x2t)
if(allocated(xat))deallocate(xat)
if(allocated(yat))deallocate(yat)
linitvqc=.false.
end subroutine wipevqctables

!==============================================================================
subroutine writevqcascfile(vqcascfile,&!                      [writevqcascfile]
     npx_a,npa_a,npb_a,npk_a,nx_a,na_a)
!==============================================================================
! If VQC parameters and tables have been created, or read in, and now exist in
! the module, pvqc_tables.mod, with its "initialized" flag, linitvqc indicated
! (=.true.), this routine will write an ascii copy of the entire table to the
! file specified by the 12-character filename given by the character argument,
! vqcascfile, provided that the specification parameters in pvqc_tables
! match those listed in the rest of the argument list.
!==============================================================================
use pvqc_tables, only: sgt,swt,x1t,x2t,xat,yat, &
                       npx,npa,npb,npk,nx,na, linitvqc

implicit none
character(len=12),intent(in ):: vqcascfile
integer(i_kind),  intent(in ):: npx_a,npa_a,npb_a,npk_a,nx_a,na_a
!------------------------------------------------------------------------------
integer(i_kind),parameter:: lunit=11,nunit=99
integer(i_kind)          :: iunit
logical                  :: ex,op
!==============================================================================
if(.not.linitvqc)&
     stop 'In writevqcascfile; VQC parameters and tables are not yet initialized'
if(npx_a/=npx)stop 'In writevqcascfile; mismatched specified npx'
if(npa_a/=npa)stop 'In writevqcascfile; mismatched specified npa'
if(npb_a/=npb)stop 'In writevqcascfile; mismatched specified npb'
if(npk_a/=npk)stop 'In writevqcascfile; mismatched specified npk'
if(nx_a /=nx )stop 'In writevqcascfile; mismatched specified nx'
if(na_a /=na )stop 'In writevqcascfile; mismatched specified na'

do iunit=lunit,nunit
   inquire(unit=iunit, exist=ex, opened=op)
   if(.not.ex)exit
   if(.not.op)exit
enddo
if(.not.ex .or. iunit>nunit)&
     stop 'In writevqcascfile; No available unit number for writing'
open(unit=iunit,file=vqcascfile,access='sequential',form='formatted')
write(iunit,600)npx,npa,npb,npk,nx,na
write(iunit,601)sgt
write(iunit,601)swt
write(iunit,601)x1t
write(iunit,601)x2t
write(iunit,601)xat
write(iunit,601)yat
close(iunit)
600 format(6i10)
601 format(4(e19.12,1x))
end subroutine writevqcascfile

!==============================================================================
subroutine readvqcascfile(vqcascfile,&!                        [readvqcascfile]
     npx_a,npa_a,npb_a,npk_a,nx_a,na_a)
!==============================================================================
! If VQC parameters already exist in the module, pvqc_tables.mod, wipe them
! clean with a call to witevqctables; then read in the integer records from
! the ascii data set given by its 12-character name, vqcascfile, and check
! whether these parameters for the tables match those specified in the 
! argument list and if not, stop. Assuming the parameters match, 
! allocate sufficient space in module pvqc_tables and read in the real-valued
! tables from the rest of the dataset and close it. Compute the resolutions
! dx,da,db,dk, from the given integer parameters and set the "initialized"
! flag, linitvqc, to .true.
!==============================================================================
use pvqc_tables, only: sgt,swt,x1t,x2t,xat,yat, dx,da,db,dk, &
                       npx,npa,npb,npk,nx,na,npb2, linitvqc
implicit none
character(len=12),intent(in ):: vqcascfile
integer(i_kind),  intent(in ):: npx_a,npa_a,npb_a,npk_a,nx_a,na_a
!------------------------------------------------------------------------------
integer(i_kind),parameter:: lunit=11,nunit=99
integer(i_kind)          :: iunit,nkm
logical                  :: ex,op
!==============================================================================
if(linitvqc)call wipevqctables
do iunit=lunit,nunit
   inquire(unit=iunit, exist=ex, opened=op)
   if(.not.ex)exit
   if(.not.op)exit
enddo
if(.not.ex .or. iunit>nunit)&
     stop 'In readvqcascfile; No available unit number for reading'
open(unit=iunit,file=vqcascfile,access='sequential',form='formatted')
read(iunit,600)npx,npa,npb,npk,nx,na
if(npx_a/=npx)stop 'In readvqcascfile; mismatched specified npx'
if(npa_a/=npa)stop 'In readvqcascfile; mismatched specified npa'
if(npb_a/=npb)stop 'In readvqcascfile; mismatched specified npb'
if(npk_a/=npk)stop 'In readvqcascfile; mismatched specified npk'
if(nx_a /=nx )stop 'In readvqcascfile; mismatched specified nx'
if(na_a /=na )stop 'In readvqcascfile; mismatched specified na'
nkm=npk-1
npb2=npb*2
allocate(sgt(-nx:nx,0:na,-nkm:nkm),swt(-nx:nx,0:na,-nkm:nkm),&
     x1t(0:na,-nkm:nkm),x2t(0:na,-nkm:nkm),&
     xat(0:na,-nkm:nkm),yat(0:na,-nkm:nkm))
read(iunit,601)sgt
read(iunit,601)swt
read(iunit,601)x1t
read(iunit,601)x2t
read(iunit,601)xat
read(iunit,601)yat
close(iunit)
dx=u1/npx
da=u1/npa
db=u1/npb
dk=u1/npk
linitvqc=T
600 format(6i10)
601 format(4(e19.12,1x))
end subroutine readvqcascfile

!==============================================================================
subroutine writevqcdatfile(vqcdatfile,&!                      [writevqcdatfile]
     npx_a,npa_a,npb_a,npk_a,nx_a,na_a)
!==============================================================================
! If VQC parameters and tables have been created, or read in, and now exist in
! the module, pvqc_tables.mod, with its "initialized" flag, linitvqc indicated
! (=.true.), this routine will write a binary copy of the entire table to the
! file specified by the 12-character filename given by the character argument,
! vqcdatfile, provided that the specification parameters in pvqc_tables
! match those listed in the rest of the argument list.
!==============================================================================
use pvqc_tables, only: sgt,swt,x1t,x2t,xat,yat, &
                       npx,npa,npb,npk,nx,na, linitvqc
implicit none
character(len=12),intent(in ):: vqcdatfile
integer(i_kind),  intent(in ):: npx_a,npa_a,npb_a,npk_a,nx_a,na_a
!------------------------------------------------------------------------------
integer(i_kind),parameter:: lunit=11,nunit=99
integer(i_kind)          :: iunit
logical          :: ex,op
!==============================================================================
if(.not.linitvqc)&
     stop 'In writevqcdatfile; VQC parameters and tables are not yet initialized'
if(npx_a/=npx)stop 'In writevqcdatfile; mismatched specified npx'
if(npa_a/=npa)stop 'In writevqcdatfile; mismatched specified npa'
if(npb_a/=npb)stop 'In writevqcdatfile; mismatched specified npb'
if(npk_a/=npk)stop 'In writevqcdatfile; mismatched specified npk'
if(nx_a /=nx )stop 'In writevqcdatfile; mismatched specified nx'
if(na_a /=na )stop 'In writevqcdatfile; mismatched specified na'

do iunit=lunit,nunit
   inquire(unit=iunit, exist=ex, opened=op)
   if(.not.ex)exit
   if(.not.op)exit
enddo
if(.not.ex .or. iunit>nunit)&
     stop 'In writevqcdatfile; No available unit number for writing'
open(unit=iunit,file=vqcdatfile,access='sequential',form='unformatted')
write(unit=iunit)npx,npa,npb,npk,nx,na
write(iunit)sgt
write(iunit)swt
write(iunit)x1t
write(iunit)x2t
write(iunit)xat
write(iunit)yat
close(iunit)
end subroutine writevqcdatfile

!==============================================================================
subroutine readvqcdatfile(vqcdatfile,&!                        [readvqcdatfile]
           npx_a,npa_a,npb_a,npk_a,nx_a,na_a)
!==============================================================================
! If VQC parameters already exist in the module, pvqc_tables.mod, wipe them
! clean with a call to witevqctables; then read in the integer records from
! the binary data set given by its 12-character name, vqcdatfile, and check
! whether these parameters for the tables match those specified in the 
! argument list and if not, stop. Assuming the parameters match, 
! allocate sufficient space in module pvqc_tables and read in the real-valued
! tables from the rest of the dataset and close it. Compute the resolutions
! dx,da,db,dk, from the given integer parameters and set the "initialized"
! flag, linitvqc, to .true.
!==============================================================================
use pvqc_tables, only: sgt,swt,x1t,x2t,xat,yat, dx,da,db,dk, &
                       npx,npa,npb,npk,nx,na,npb2, linitvqc
character(len=12),intent(in ):: vqcdatfile
integer(i_kind),  intent(in ):: npx_a,npa_a,npb_a,npk_a,nx_a,na_a
!------------------------------------------------------------------------------
integer(i_kind),parameter:: lunit=11,nunit=99
integer(i_kind)          :: iunit,nkm
logical                  :: ex,op
!==============================================================================
if(linitvqc)call wipevqctables
do iunit=lunit,nunit
   inquire(unit=iunit, exist=ex, opened=op)
   if(.not.ex)exit
   if(.not.op)exit
enddo
if(.not.ex .or. iunit>nunit)&
     stop 'In readvqcdatfile; No available unit number for reading'
open(unit=iunit,file=vqcdatfile,access='sequential',form='unformatted')
read(iunit)npx,npa,npb,npk,nx,na
if(npx_a/=npx)stop 'In readvqcdatfile; mismatched specified npx'
if(npa_a/=npa)stop 'In readvqcdatfile; mismatched specified npa'
if(npb_a/=npb)stop 'In readvqcdatfile; mismatched specified npb'
if(npk_a/=npk)stop 'In readvqcdatfile; mismatched specified npk'
if(nx_a /=nx )stop 'In readvqcdatfile; mismatched specified nx'
if(na_a /=na )stop 'In readvqcdatfile; mismatched specified na'
nkm=npk-1
npb2=npb*2
allocate(sgt(-nx:nx,0:na,-nkm:nkm),swt(-nx:nx,0:na,-nkm:nkm),&
     x1t(0:na,-nkm:nkm),x2t(0:na,-nkm:nkm),&
     xat(0:na,-nkm:nkm),yat(0:na,-nkm:nkm))
read(iunit)sgt
read(iunit)swt
read(iunit)x1t
read(iunit)x2t
read(iunit)xat
read(iunit)yat
close(iunit)
dx=u1/npx
da=u1/npa
db=u1/npb
dk=u1/npk
linitvqc=T
end subroutine readvqcdatfile

!==============================================================================
subroutine vqch_iii(ia,ib,ik,x,g,w)!                                     [vqch]
!==============================================================================
! Huber-like analog of the superlogistic routine vqcs. The results, g and w,
! in the central section are ezactly as if the density is a standardized
! Gaussian; outside points where this Gaussian becomes tangent to the
! idealized superlogistic's asymptotes, the results returned are those that
! take these asymptotes to be the effective log-probability (with suitable
! small nudges, xa in x, and ya in the g direction, to ensure the tengential
! intersections occur when the log-gaussian (parabola) is properly centered 
! and peaks at g=0).
!==============================================================================
use pvqc_tables, only: x1t,x2t,xat,yat,da,db,dk,&
                       npk,na,npb2,linitvqc
implicit none
integer(i_kind), intent(in ):: ia,ib,ik
real(dp),        intent(in ):: x
real(dp),        intent(out):: g,w
!------------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4_dp
real(dp)          :: bc,p,q,qx,sx,alpha,beta,kappa, &
                     x1,x2,xa,ya,xx
integer(i_kind)           :: ja
!==============================================================================
if(.not.linitvqc)stop 'In vqch; VQC tables are not initialized'
if(ia<0)then; sx=-x; ja=-ia
else;         sx= x; ja= ia
endif
if(ja>na              )stop 'In vqch; ia out of bounds'
if(ib<=0.or.ib>=npb2  )stop 'In vqch; ib out of bounds'
if(ik<=-npk.or.ik>=npk)stop 'In vqch; ik out of bounds'
x1=x1t(ja,ik)
x2=x2t(ja,ik)
xa=xat(ja,ik)
ya=yat(ja,ik)
alpha=ja*da
beta =ib*db
kappa=ik*dk
bc=tan(pio4*(u2-beta))
p=bc**(u2/(u1-kappa))
q=u1/sqrt(p)
qx=q*sx
xx=qx+xa
if(qx<x1.or.qx>x2)then
   call atote(alpha,u1,kappa,xx,g,w)
   g=g-ya
   w=-w/xx
else
   g=-qx**2/2_dp
   w=1_dp
endif
g=p*g
end subroutine vqch_iii
!==============================================================================
subroutine vqch_ii(ib,ik,x,g,w)!                                         [vqch]
!==============================================================================
! Wrapper for the symmetric restriction (no alpha index) of the Huber-like
! analogs of the superlogistic family.
!==============================================================================
implicit none
integer(i_kind), intent(in ):: ib,ik
real(dp),        intent(in ):: x
real(dp),        intent(out):: g,w
!------------------------------------------------------------------------------
call vqch(0,ib,ik,x,g,w)
end subroutine vqch_ii
!==============================================================================
subroutine vqch_i(ib,x,g,w)!                                             [vqch]
!==============================================================================
! Wrapper for the symmetric (no alpha index) and neutrally-convex
! (no kappa index) restriction of the Huber-like analogs of the 
! superlogistic family.
!==============================================================================
integer(i_kind), intent(in ):: ib
real(dp),        intent(in ):: x
real(dp),        intent(out):: g,w
!------------------------------------------------------------------------------
call vqch(0,ib,0,x,g,w)
end subroutine vqch_i
!==============================================================================
subroutine vqch_r(beta,x,g,w)!                                           [vqch]
!==============================================================================
! Huber-norm analog of the simplest versions of the superlogistic
! (symmetric, neutral convexity), with tail broadness specified by a real
! parameter, beta.
!==============================================================================
use pvqc_tables, only: x1t,x2t,yat,linitvqc
implicit none
real(dp),intent(in ):: beta,x
real(dp),intent(out):: g,w
!------------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4_dp
real(dp)          :: bc,p,q,qx,x1,x2,ya,xx
!==============================================================================
if(.not.linitvqc)stop 'In vqch; VQC tables are not initialized'
if(beta<=u0.or.beta>=u2)stop 'In vqch; beta out of bounds'
x1=x1t(0,0)
x2=x2t(0,0)
ya=yat(0,0)
bc=tan(pio4*(u2-beta))
p=bc**2
q=u1/bc
qx=q*x
xx=qx
if(qx<x1.or.qx>x2)then
   call atote(u0,u1,u0,xx,g,w)
   g=g-ya
   w=-w/xx
else
   g=-qx**2/2_dp
   w=1_dp
endif
g=p*g
end subroutine vqch_r

!==============================================================================
subroutine vqcs_iii(ia,ib,ik,x,g,w)!                                     [vqcs]
!==============================================================================
! By specifying the tail-shape parameters, Alpha (Asymmetry), Beta (Broadness)
! and Konvexity (Kappa) by their integer indices, ia, ib, ik, subject to the
! understood convention that there are npa index steps per unit alpha, npb
! per unit beta, npk per unit kappa, this routine will return the 
! log-probability, g (which is the negative of the cost function contribution)
! and the weight-factor, w, appropriate to the standardized O-A given by x.
! Provided that the the (x,beta) combination maintains the rescaling
! of this x, namely qx, within the span of the prepared tables, the g and w
! are evaluated by Hermite interpolation (in qx) from the superlogistic
! function table, but when the rescaled residual is too large to fit within
! the span of the table, the returned values are those of the almost
! equivalent Huber-like analog contrived to possess exactly the same
! asymptotic form in the limit |x| --> infinity, and slightly adjusted to
! ensure continuity of g at the transition.
!==============================================================================
use pvqc_tables, only: sgt,swt,dx,db,dk,npb,npk,nx,na,npb2,linitvqc
implicit none
integer(i_kind), intent(in ):: ia,ib,ik
real(dp),        intent(in ):: x
real(dp),        intent(out):: g,w
!------------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4
real(dp)          :: bc,p,q,qx,sx,w1,w2,xodx,beta,kappa,xe,ge,we,&
                     ww,dfa,fl,dfl,f1,f2,df1,df2,g1,g2
integer(i_kind)   :: ix1,ix2,ja
!==============================================================================
if(.not.linitvqc)stop 'In vqcs; VQC tables are not initialized'
if(ia<0)then; sx=-x; ja=-ia
else;         sx= x; ja= ia
endif
if(ja>na              )stop 'In vqcs; ia out of bounds'
if(ib<=0.or.ib>=npb2  )stop 'In vqcs; ib out of bounds'
if(ik<=-npk.or.ik>=npk)stop 'In vqcs; ik out of bounds'
beta =ib*db
kappa=ik*dk
bc=tan(pio4*(u2-beta))
p=bc**(u2/(u1-kappa))
q=u1/sqrt(p)
qx=q*sx
xodx=qx/dx
ix1=floor(xodx); ix2=ix1+1
if    (ix1<-nx)then
   xe=-nx*dx
   call vqch(ja,npb,ik,qx,g, w )
   call vqch(ja,npb,ik,xe,ge,we)
   g=p*(sgt(-nx,ja,ik)+g-ge)
elseif(ix2>nx)then
   xe= nx*dx
   call vqch(ja,npb,ik,qx,g, w )
   call vqch(ja,npb,ik,xe,ge,we)
   g=p*(sgt( nx,ja,ik)+g-ge)
else
   w1=ix2-xodx
   w2=xodx-ix1
   f1=-sgt(ix1,ja,ik)
   f2=-sgt(ix2,ja,ik)
   df1=swt(ix1,ja,ik)
   df2=swt(ix2,ja,ik)
   fl=w1*f1+w2*f2
   ww=w1*w2
   if(ww==u0)then
      g=-p*fl
      w= w1*df1+w2*df2
   else
! Hermite interpolation for g and its consistent derivative used for w:
      df1=df1*ix1*dx
      df2=df2*ix2*dx
      dfl=w1*df1+w2*df2
      dfa=(f2-f1)/dx
      g1=df1-dfa
      g2=df2-dfa
      g=-p*(fl+ww*dx*(w1*g1-w2*g2))
      w=(dfl-3*ww*(g1+g2))/qx
   endif
endif
end subroutine vqcs_iii
!==============================================================================
subroutine vqcs_ii(ib,ik,x,g,w)!                                         [vqcs]
!==============================================================================
! On the assumption that asymmetry (alpha) is rarely needed, this wrapper
! dispenses with the associated index parameter ia for the convenience of the
! user, but retains broadness and convexity indices, ib and ik.
!==============================================================================
implicit none
integer(i_kind), intent(in ):: ib,ik
real(dp),        intent(in ):: x
real(dp),        intent(out):: g,w
!------------------------------------------------------------------------------
call vqcs(0,ib,ik,x,g,w)
end subroutine vqcs_ii
!==============================================================================
subroutine vqcs_i(ib,x,g,w)!                                             [vqcs]
!==============================================================================
! For the restricted case of the symmetric powers of the classical logistic
! this wrapper routine takes only the beta parameter index, ib, to define 
! the broadness.
!==============================================================================
implicit none
integer(i_kind), intent(in ):: ib
real(dp),        intent(in ):: x
real(dp),        intent(out):: g,w
!------------------------------------------------------------------------------
call vqcs(0,ib,0,x,g,w)
end subroutine vqcs_i
!==============================================================================
subroutine vqcs_r(beta,x,g,w)!                                           [vqcs]
!==============================================================================
! A special version of vqcs with vanishing asymmetry and neutral convexity,
! but accepting a real parameter, beta, for broadness in order to make it
! more compatible with the older version of VQC that uses these simpler forms
! of the superlogistic family.
!==============================================================================
use pvqc_tables, only: sgt,swt,dx,npb,nx,linitvqc
implicit none
real(dp),intent(in ):: beta,x
real(dp),intent(out):: g,w
!------------------------------------------------------------------------------
real(dp),parameter:: pio4=pi/4_dp
real(dp)          :: bc,p,q,qx,w1,w2,xodx,xe,ge,we,&
                     ww,dfa,fl,dfl,f1,f2,df1,df2,g1,g2
integer(i_kind)   :: ix1,ix2
!==============================================================================
if(.not.linitvqc)stop 'In vqcs; VQC tables are not initialized'
bc=tan(pio4*(u2-beta))
p=bc**2
q=u1/bc
qx=q*x
xodx=qx/dx
ix1=floor(xodx); ix2=ix1+1
if(ix1<-nx)then
   xe=-nx*dx
   call vqch(npb,qx,g, w )
   call vqch(npb,xe,ge,we)
   g=p*(sgt(-nx,0,0)+g-ge)
elseif(ix2>nx)then
   xe= nx*dx
   call vqch(npb,qx,g, w )
   call vqch(npb,xe,ge,we)
   g=p*(sgt( nx,0,0)+g-ge)
else
   w1=ix2-xodx
   w2=xodx-ix1
   f1=-sgt(ix1,0,0)
   f2=-sgt(ix2,0,0)
   df1=swt(ix1,0,0)
   df2=swt(ix2,0,0)
   fl=w1*f1+w2*f2
   ww=w1*w2
   if(ww==u0)then
      g=-p*fl
      w= w1*df1+w2*df2
   else
! Hermite interpolation for g and its consistent derivative used for w:
      df1=df1*ix1*dx
      df2=df2*ix2*dx
      dfl=w1*df1+w2*df2
      dfa=(f2-f1)/dx
      g1=df1-dfa
      g2=df2-dfa
      g=-p*(fl+ww*dx*(w1*g1-w2*g2))
      w=(dfl-3*ww*(g1+g2))/qx
   endif
endif
end subroutine vqcs_r

end module pvqc
