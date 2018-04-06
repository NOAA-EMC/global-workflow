module pran
implicit none
private:: uniform_d,expone_d,gauss_d,gammar,gammar_d,gammad_d,           &
     chisqd,chisqd_d,chisqr_d,gammln_d,gammq_d,gcf_d,gser_d,fgam,fgam_d, &
     randomrot_d,setrot,setrot_d,outerprod3,outerprod3_d

interface chisqq
	module procedure chisqq,chisqq_d, chisqi, chisqi_d
end interface
interface promptseed;module procedure promptseed;        end interface
interface plant;   module procedure plant, plant1;       end interface
interface skewkurt;module procedure skewkurt, skewkurt_d;end interface
interface gammq;   module procedure gammq,  gammq_d;     end interface
interface gammln;  module procedure gammln, gammln_d;    end interface
interface uniform; module procedure uniform,uniform_d;   end interface
interface expone;  module procedure expone, expone_d;    end interface
interface gauss;   module procedure gauss,  gauss_d;     end interface
interface gcf;     module procedure gcf,    gcf_d;       end interface
interface gser;    module procedure gser,   gser_d;      end interface
interface fgam;    module procedure fgam,   fgam_d;      end interface
interface gammad
	module procedure gammar, gammar_d, gammad, gammad_d
end interface
interface chisqr  
   module procedure chisqr, chisqr_d, chisqd, chisqd_d
end interface
interface ranperm; module procedure ranperm;             end interface
interface randomrot; module procedure randomrot,randomrot_d; end interface
contains
subroutine skewkurt(u0,u1,u2,u3,u4,bias,sdev,skew,kurt)
real(4),intent(IN ):: u0,u1,u2,u3,u4
real(4),intent(OUT):: bias,sdev,skew,kurt
real(4)            :: t1,t2,t3,t4,s2
t1=u1/u0
t2=u2/u0
t3=u3/u0
t4=u4/u0
t4=t4-4*t3*t1+6*t2*t1**2-3*t1**4
t3=t3-3*t2*t1           +2*t1**3
t2=t2                   -1*t1**2
bias=t1
sdev=sqrt(t2)
skew=t3/sdev**3
kurt=(t4/t2**2)-3
end subroutine skewkurt

subroutine skewkurt_d(u0,u1,u2,u3,u4,bias,sdev,skew,kurt)
real(8),intent(IN ):: u0,u1,u2,u3,u4
real(8),intent(OUT):: bias,sdev,skew,kurt
real(8)            :: t1,t2,t3,t4,s2
t1=u1/u0
t2=u2/u0
t3=u3/u0
t4=u4/u0
t4=t4-4*t3*t1+6*t2*t1**2-3*t1**4
t3=t3-3*t2*t1           +2*t1**3
t2=t2                   -1*t1**2
bias=t1
sdev=sqrt(t2)
skew=t3/sdev**3
kurt=(t4/t2**2)-3
end subroutine skewkurt_d

subroutine gammln(g,xx)
real(4),intent(IN) :: xx
real(4),intent(OUT):: g
REAL(8)         :: cof(6),stp,half,one,fpf,x,tmp,ser
integer         :: j
DATA cof,stp/76.18009173d0,-86.50532033d0,24.01409822d0, &
     -1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
DATA half,one,fpf/0.5d0,1.0d0,5.5d0/
x=xx-one
tmp=x+fpf
tmp=(x+half)*LOG(tmp)-tmp
ser=one
DO j=1,6
   x=x+one
   ser=ser+cof(j)/x
ENDDO
g=tmp+LOG(stp*ser)
END subroutine gammln

subroutine gammln_d(g,xx)
real(8),intent(IN ) :: xx
real(8),intent(OUT) :: g
REAL(8)             :: cof(6),stp,half,one,fpf,x,tmp,ser
integer             :: j
DATA cof,stp/76.18009173d0,-86.50532033d0,24.01409822d0, &
     -1.231739516d0,.120858003d-2,-.536382d-5,2.50662827465d0/
DATA half,one,fpf/0.5d0,1.0d0,5.5d0/
x=xx-one
tmp=x+fpf
tmp=(x+half)*LOG(tmp)-tmp
ser=one
DO j=1,6
   x=x+one
   ser=ser+cof(j)/x
ENDDO
g=tmp+LOG(stp*ser)
END subroutine gammln_d

subroutine gammq(g,a,x)
REAL(4),INTENT(IN) :: a,x
real(4),intent(OUT):: g
REAL(4)            :: gln,gammcf,gamser
IF(x<0. .OR. a<=0.)STOP 'In gammq; invalid argument'
IF(x<a+1.)THEN
   CALL gser(gamser,a,x,gln)
   g=1.-gamser
ELSE
   CALL gcf(gammcf,a,x,gln)
   g=gammcf
ENDIF
END subroutine gammq

subroutine gammq_d(g,a,x)
REAL(8),INTENT(IN ):: a,x
real(8),intent(OUT):: g
REAL(8)            :: gln,gammcf,gamser
IF(x<0. .OR. a<=0.)STOP 'In gammq; invalid argument'
IF(x<a+1.)THEN
   CALL gser(gamser,a,x,gln)
   g=1.-gamser
ELSE
   CALL gcf(gammcf,a,x,gln)
   g=gammcf
ENDIF
END subroutine gammq_d

subroutine promptseed
integer::seed
print '(" Input seed to initialize random number sequence")'
read(*,*)seed
call plant(seed)
end subroutine promptseed

subroutine plant
integer,parameter :: nn=12
integer,dimension(nn):: jseed
integer:: iran,jran
print '('' input positive integer seed for random number generator'')'
read(*,*)iran
jran=mod(iran,32778)
jseed(1)=jran+1
jseed(2)=jran**2-jran+1
call random_seed(put=jseed(1:nn))
end subroutine plant

subroutine plant1(iran)
integer,intent(IN):: iran
integer,parameter :: nn=12
integer,dimension(nn):: jseed
integer:: jran
jran=mod(iran,32778)
jseed(1)=jran+1
jseed(2)=jran**2-jran+1
call random_seed(put=jseed(1:nn))
end subroutine plant1

subroutine uniform(x)
real(4),intent(OUT):: x
real(4)            :: x1,x2
1 call random_number(x1)
call random_number(x2)
x=x1+x2*1.e-6
if(x>=1.)x=x-1.
if(x==0.)goto 1
end subroutine uniform

subroutine uniform_d(x)
real(8),intent(OUT):: x
real(8)            :: x1,x2
1 call random_number(x1)
call random_number(x2)
x=x1+x2*1.e-8
if(x>=1.d0)x=x-1.d0
if(x==0.d0)goto 1
end subroutine uniform_d

subroutine expone(x)
real(4),intent(OUT):: x
call uniform(x)
x=-log(x)
end subroutine expone

subroutine expone_d(x)
real(8),intent(OUT):: x
call uniform(x)
x=-log(x)
end subroutine expone_d

subroutine gauss(x)
real(4),intent(OUT):: x
integer:: iset
real(4):: gset,v1,v2,r,f
save gset,iset
data iset/0/
if(iset==0)then
1  call uniform(v1); v1=2*v1-1
   call uniform(v2); v2=2*v2-1
   r=v1**2+v2**2
   if(r>=1. .or. r==0.)goto 1
   f=sqrt(-2.*log(r)/r)
   gset=v1*f
   x=v2*f
   iset=1
else
   x=gset
   iset=0
endif
end subroutine gauss

subroutine gauss_d(x)
real(8),intent(OUT):: x
integer            :: iset
real(8)            :: gset,v1,v2,r,f
save gset,iset
data iset/0/
if(iset==0)then
1  call uniform(v1); v1=2*v1-1
   call uniform(v2); v2=2*v2-1
   r=v1**2+v2**2
   if(r>=1.d0 .or. r==0.d0)goto 1
   f=sqrt(-2.d0*log(r)/r)
   gset=v1*f
   x=v2*f
   iset=1
else
   x=gset
   iset=0
endif
end subroutine gauss_d

subroutine gammad(ia,x)
integer,intent(IN ):: ia
real(4),   intent(OUT):: x
integer:: j
real(4):: am,e,s,v1,v2,y
if(ia<0)stop 'In gammad; invalid ia'
if(ia<6)then
   x=1.
   do j=1,ia
      call uniform(y)
      x=x*y
   enddo
   x=-log(x)
else
1  call uniform(v1)
   call uniform(y)
   v2=2*y-1
   if(v1**2+v2**2>1.)goto 1
   y=v2/v1
   am=ia-1
   s=sqrt(2.*am+1.)
   x=s*y+am
   if(x<=0.)goto 1
   e=(1.+y**2)*exp(am*log(x/am)-s*y)
   call uniform(y)
   if(y>e)goto 1
endif
end subroutine gammad

subroutine gammad_d(ia,x)
integer,intent(IN ):: ia
real(8),intent(OUT):: x
integer            :: j
real(8)            :: am,e,s,v1,v2,y
if(ia<0)stop 'In gammad; invalid ia'
if(ia<6)then
   x=1.
   do j=1,ia
      call uniform(y)
      x=x*y
   enddo
   x=-log(x)
else
1  call uniform(v1)
   call uniform(y)
   v2=2*y-1
   if(v1**2+v2**2>1.d0)goto 1
   y=v2/v1
   am=ia-1
   s=sqrt(2.d0*am+1.d0)
   x=s*y+am
   if(x<=0.d0)goto 1
   e=(1.+y**2)*exp(am*log(x/am)-s*y)
   call uniform(y)
   if(y>e)goto 1
endif
end subroutine gammad_d

subroutine chisqd(ia,x)
integer,intent(IN ):: ia
real(4),   intent(OUT):: x
integer            :: ih
real(4)               :: y
if(ia<0)stop 'In chisqd; invalid ia'
ih=ia/2
call gammad(ih,x)
x=x*2
if(2*ih<ia)then
   call gauss(y)
   x=x+y**2
endif
end subroutine chisqd

subroutine chisqd_d(ia,x)
integer,intent(IN ):: ia
real(8),intent(OUT):: x
integer            :: ih
real(8)            :: y
if(ia<0)stop 'In chisqd; invalid ia'
ih=ia/2
call gammad(ih,x)
x=x*2
if(2*ih<ia)then
   call gauss(y)
   x=x+y**2
endif
end subroutine chisqd_d

subroutine chisqr(a,x)
real(4),intent(IN ):: a
real(4),intent(OUT):: x
real(4)            :: ah
ah=a/2
if(ah<0.)stop 'In chisqr; invalid degrees of freedom, a'
call gammad(ah,x)
x=x*2
end subroutine chisqr

subroutine chisqr_d(a,x)
real(8),intent(IN ):: a
real(8),intent(OUT):: x
real(8)            :: ah
ah=a/2
if(ah<0.)stop 'In chisqr; invalid degrees of freedom, a'
call gammad(ah,x)
x=x*2
end subroutine chisqr_d

SUBROUTINE gcf(gammcf,a,x,gln)
INTEGER, PARAMETER :: itmax=100
REAL(4),    PARAMETER :: eps=3.e-7
REAL(4),   INTENT(out):: gammcf,gln
REAL(4),   INTENT(in ):: a,x
INTEGER            :: n
REAL(4)               :: gold,a0,a1,b0,b1,fac,an,ana,anf,g 
call gammln(gln,a)
gold=0.
a0=1.
a1=x
b0=0.
b1=1.
fac=1.
DO n=1,itmax
   an=REAL(n)
   ana=an-a
   a0=(a1+a0*ana)*fac
   b0=(b1+b0*ana)*fac
   anf=an*fac
   a1=x*a0+anf*a1
   b1=x*b0+anf*b1
   IF(a1/=0.)THEN
      fac=1./a1
      g=b1*fac
      IF(ABS((g-gold)/g)<eps)go to 1
      gold=g
   ENDIF
ENDDO
STOP 'in gcf; a too large, itmax too small'
1 gammcf=EXP(-x+a*log(x)-gln)*g
END SUBROUTINE gcf

SUBROUTINE gcf_d(gammcf,a,x,gln)
INTEGER, PARAMETER :: itmax=100
REAL(8), PARAMETER :: eps=3.d-14
REAL(8),INTENT(out):: gammcf,gln
REAL(8),INTENT(in ):: a,x
INTEGER            :: n
REAL(8)            :: gold,a0,a1,b0,b1,fac,an,ana,anf,g 
call gammln(gln,a)
gold=0.
a0=1.
a1=x
b0=0.
b1=1.
fac=1.
DO n=1,itmax
   an=DBLE(n)
   ana=an-a
   a0=(a1+a0*ana)*fac
   b0=(b1+b0*ana)*fac
   anf=an*fac
   a1=x*a0+anf*a1
   b1=x*b0+anf*b1
   IF(a1/=0.d0)THEN
     fac=1./a1
      g=b1*fac
      IF(ABS((g-gold)/g)<eps)go to 1
      gold=g
   ENDIF
ENDDO
STOP 'in gcf; a too large, itmax too small'
1 gammcf=EXP(-x+a*log(x)-gln)*g
END SUBROUTINE gcf_d

SUBROUTINE gser(gamser,a,x,gln)
INTEGER,PARAMETER  :: itmax=100
REAL(4),   PARAMETER  :: eps=3.e-7
REAL(4),   INTENT(IN ):: a,x
REAL(4),   INTENT(OUT):: gamser,gln
INTEGER            :: n
REAL(4)               :: ap,sum,del 
call gammln(gln,a)
IF(x<=0.)THEN
   IF(x<0.)PAUSE
   gamser=0.
   RETURN
ENDIF
ap=a
sum=1./a
del=sum
DO n=1,itmax
   ap=ap+1.
   del=del*x/ap
   sum=sum+del
   IF(ABS(del)<ABS(sum)*eps)go to 1
ENDDO
STOP 'In gser; a too large, itmax too small'
1     gamser=sum*EXP(-x+a*LOG(x)-gln)
END SUBROUTINE gser

SUBROUTINE gser_d(gamser,a,x,gln)
INTEGER,PARAMETER  :: itmax=100
REAL(8),PARAMETER  :: eps=3.d-14
REAL(8),INTENT(IN ):: a,x
REAL(8),INTENT(OUT):: gamser,gln
INTEGER            :: n
REAL(8)            :: ap,sum,del 
call gammln(gln,a)
IF(x<=0.d0)THEN
   IF(x<0.d0)PAUSE
   gamser=0.
   RETURN
ENDIF
ap=a
sum=1./a
del=sum
DO n=1,itmax
   ap=ap+1.
   del=del*x/ap
   sum=sum+del
   IF(ABS(del)<ABS(sum)*eps)go to 1
ENDDO
STOP 'In gser; a too large, itmax too small'
1     gamser=sum*EXP(-x+a*LOG(x)-gln)
END SUBROUTINE gser_d

subroutine fgam(a,x)
! Generate gamma deviate with fractional degrees of freedom
real(4),intent(IN) :: a
real(4),intent(OUT):: x
real(4)            :: z,one,c,d,am,ai
data one/1./
if(a<=0. .or. a>=1.)stop 'In fgam; invalid argument'
am=a-1
ai=1/a
call gammq(c,a,one)
call uniform(x)
if(x<c)then
1  call uniform(z)
   x=1-log(z)
   d=x**am
   call uniform(z)
   if(z>d)goto 1
else
2  call uniform(z)
   x=z**ai
   d=exp(-x)
   call uniform(z)
   if(z>d)goto 2
endif
end subroutine fgam

subroutine fgam_d(a,x)
! Generate gamma deviate with fractional degrees of freedom
real(8),intent(IN) :: a
real(8),intent(OUT):: x
real(8)            :: z,one,c,d,am,ai
data one/1.d0/
if(a<=0.d0 .or. a>=1.d0)stop 'In fgam; invalid argument'
am=a-1
ai=1/a
call gammq(c,a,one)
call uniform(x)
if(x<c)then
1  call uniform(z)
   x=1-log(z)
   d=x**am
   call uniform(z)
   if(z>d)goto 1
else
2  call uniform(z)
   x=z**ai
   d=exp(-x)
   call uniform(z)
   if(z>d)goto 2
endif
end subroutine fgam_d

subroutine gammar(a,x)
real(4),intent(IN ):: a
real(4),intent(OUT):: x
integer         :: ia
real(4)            :: r,z
if(a<0.)stop 'In gammar; invalid argument'
ia=a
call gammad(ia,x)
r=a-ia
if(r>0.)then
   call fgam(r,z)
   x=x+z
endif
end subroutine gammar

subroutine gammar_d(a,x)
real(8),intent(IN ):: a
real(8),intent(OUT):: x
integer            :: ia
real(8)            :: r,z
if(a<0.d0)stop 'In gammar; invalid argument'
ia=a
call gammad(ia,x)
r=a-ia
if(r>0.d0)then
   call fgam(r,z)
   x=x+z
endif
end subroutine gammar_d

subroutine chisqq(g,a,x)
real(4),intent(IN ):: a,x
real(4),intent(OUT):: g
real(4):: b,y
b=a/2
y=x/2
call gammq(g,b,y)
end subroutine chisqq

subroutine chisqq_d(g,a,x)
real(8),intent(IN ):: a,x
real(8),intent(OUT):: g
real(8):: b,y
b=a/2
y=x/2
call gammq(g,b,y)
end subroutine chisqq_d

subroutine chisqi(g,ia,x)
integer,intent(IN):: ia
real(4),intent(IN ):: x
real(4),intent(OUT):: g
real(4):: b,y
b=ia/2.
y=x/2
call gammq(g,b,y)
end subroutine chisqi

subroutine chisqi_d(g,ia,x)
integer,intent(IN ):: ia
real(8),intent(IN ):: x
real(8),intent(OUT):: g
real(8):: b,y
b=ia/2.d0
y=x/2
call gammq(g,b,y)
end subroutine chisqi_d

!=============================================================================
subroutine ranperm(n,p)
!=============================================================================
! Deliver a random permutation of {1:n} in array p
!=============================================================================
implicit none
integer,             intent(IN ):: n
integer,dimension(n),intent(OUT):: p
integer,dimension(n)            :: pool
real(8)                         :: xran
integer                         :: i,j,jc
!=============================================================================
do i=1,n
   pool(i)=i
enddo
do j=1,n
   jc=n+1-j
   call uniform(xran)
   i=1+xran*jc
   p(j)=pool(i)
   pool(i:jc-1)=pool(i+1:jc)
enddo
end subroutine ranperm

!==============================================================================
subroutine randomrot(rot)
!==============================================================================
! Create a random 3*3 rotation matrix
real(4),dimension(3,3),intent(OUT):: rot
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(4),dimension(4):: v4
real(4)             :: s,alpha,beta,gamma
integer          :: i
!-----------------------------------------------------------------------------
4 continue
do i=1,4; call gauss(v4(i)); enddo
s=dot_product(v4,v4);           if(s==0)goto 4;gamma=2*acos(v4(4)/sqrt(s))
s=dot_product(v4(1:3),v4(1:3)); if(s==0)goto 4;alpha=acos(v4(3)/sqrt(s))
beta=atan2(v4(2),v4(1))
call setrot(alpha,beta,gamma,rot)
end subroutine randomrot

!==============================================================================
subroutine randomrot_d(rot)
!==============================================================================
! Create a random 3*3 rotation matrix
real(8),dimension(3,3),intent(OUT):: rot
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(8),dimension(4):: v4
real(8)             :: s,alpha,beta,gamma
integer             :: i
!-----------------------------------------------------------------------------
4 continue
do i=1,4; call gauss(v4(i)); enddo
s=dot_product(v4,v4);           if(s==0)goto 4;gamma=2*acos(v4(4)/sqrt(s))
s=dot_product(v4(1:3),v4(1:3)); if(s==0)goto 4;alpha=acos(v4(3)/sqrt(s))
beta=atan2(v4(2),v4(1))
call setrot_d(alpha,beta,gamma,rot)
end subroutine randomrot_d

!==============================================================================
subroutine setrot(alpha,beta,gamma,rot)
!==============================================================================
! Construct a 3*3 orthogonal matrix that represents a rotation by an angle
! gamma counterclockwise about an axis given by the unit vector, "axis3".
! axis3 points in the direction with colatitude alpha and longitude beta.
!==============================================================================
implicit none
real(4),               intent(IN ):: alpha,beta,gamma
real(4),dimension(3,3),intent(OUT):: rot
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(4),dimension(3,3)            :: t,q,r,u
real(4),dimension(3)              :: axis1,axis2,axis3,v3
real(4)                           :: ca,sa,cb,sb,cg,sg, &
                                  cca,csa,ssa,ccb,csb,ssb,pi2
!-----------------------------------------------------------------------------
ca=cos(alpha)
sa=sin(alpha)
cb=cos(beta)
sb=sin(beta)
cg=cos(gamma)
sg=sin(gamma)
cca=ca*ca
ssa=sa*sa
csa=ca*sa
ccb=cb*cb
ssb=sb*sb
csb=cb*sb
axis1(1)=cb*ca
axis1(2)=sb*ca
axis1(3)=sa
axis2(1)=-sb
axis2(2)=cb
axis2(3)=0
axis3(1)=-cb*sa
axis3(2)=-sb*sa
axis3(3)=ca
call outerprod3(axis3,axis3,t)
call outerprod3(axis1,axis1,q)
call outerprod3(axis2,axis2,r)
q=q+r
r=0
r(1,2)=-ca;     r(1,3)=-sb*sa;  r(2,3)=cb*sa
r(2,1)=-r(1,2); r(3,1)=-r(1,3); r(3,2)=-r(2,3)
rot=t + cg*q + sg*r
end subroutine setrot

!==============================================================================
subroutine setrot_d(alpha,beta,gamma,rot)
!==============================================================================
! Construct a 3*3 orthogonal matrix that represents a rotation by an angle
! gamma counterclockwise about an axis given by the unit vector, "axis3".
! axis3 points in the direction with colatitude alpha and longitude beta.
!==============================================================================
implicit none
real(8),               intent(IN ):: alpha,beta,gamma
real(8),dimension(3,3),intent(OUT):: rot
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(8),dimension(3,3)            :: t,q,r,u
real(8),dimension(3)              :: axis1,axis2,axis3,v3
real(8)                           :: ca,sa,cb,sb,cg,sg, &
                                     cca,csa,ssa,ccb,csb,ssb,pi2
!-----------------------------------------------------------------------------
ca=cos(alpha)
sa=sin(alpha)
cb=cos(beta)
sb=sin(beta)
cg=cos(gamma)
sg=sin(gamma)
cca=ca*ca
ssa=sa*sa
csa=ca*sa
ccb=cb*cb
ssb=sb*sb
csb=cb*sb
axis1(1)=cb*ca
axis1(2)=sb*ca
axis1(3)=sa
axis2(1)=-sb
axis2(2)=cb
axis2(3)=0
axis3(1)=-cb*sa
axis3(2)=-sb*sa
axis3(3)=ca
call outerprod3_d(axis3,axis3,t)
call outerprod3_d(axis1,axis1,q)
call outerprod3_d(axis2,axis2,r)
q=q+r
r=0
r(1,2)=-ca;     r(1,3)=-sb*sa;  r(2,3)=cb*sa
r(2,1)=-r(1,2); r(3,1)=-r(1,3); r(3,2)=-r(2,3)
rot=t + cg*q + sg*r
end subroutine setrot_d

!=============================================================================
subroutine outerprod3(va,vb,vv)
!=============================================================================
implicit none
real(4),dimension(3),  intent(IN ):: va,vb
real(4),dimension(3,3),intent(OUT):: vv
integer                        :: i
!-----------------------------------------------------------------------------
do i=1,3
   vv(:,i)=va(:)*vb(i)
enddo
end subroutine outerprod3

!=============================================================================
subroutine outerprod3_d(va,vb,vv)
!=============================================================================
implicit none
real(8),dimension(3),  intent(IN ):: va,vb
real(8),dimension(3,3),intent(OUT):: vv
integer                           :: i
!-----------------------------------------------------------------------------
do i=1,3
   vv(:,i)=va(:)*vb(i)
enddo
end subroutine outerprod3_d

end module pran
