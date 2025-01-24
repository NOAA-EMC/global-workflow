subroutine rfdpar1(be,rate,m)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfdpar
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine finds the quadratic factors and (if present) the
!            single linear factor of the generic quasi-Gaussian recursive
!            filter of degree m. 
!            
!
! program history log:
!   1998-01-01  purser
!   2004-06-22  treadon - update documentation
!   2004-06-23  purser  - update documentation
!   2011-07-03  todling - use general interface to FORTRAN intrisic math
!
!   input argument list:
!     m    - the degree of the recursive filter
!
!   output argument list:
!     be   - Recursive filter "beta" coefficients for characteristic modes
!     rate - Decay rate factors for the characteristic modes of the filter
!
!   remarks:
!            This is one of several possible variants of 
!            the construction procedure described in Purser et al, 2003, MWR
!            131, 1524--1535. An earlier version, with additional material,
!            is Purser et al. 2001: NCEP Office Note 431. The style of
!            filtering implemented here treats the (in general, complex)
!            characteristic eigenmodes of the idealized degree-m recursive
!            filter additively. This style was motivated by evidence that
!            this approach led to better control of amplitude in the case
!            of spatial variations of filtering or grid scale. However, the
!            exact construction and application of the filter parameters in
!            this style differs from the published descriptions (referenced
!            above) which do not attempt to analyze and separate the spatial
!            eigenmodes of the filter as is done here.
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,one
  implicit none

  integer(i_kind)          ,intent(in   ) :: m
  real(r_kind),dimension(*),intent(  out) :: be,rate

  integer(i_kind),parameter:: nn=12
  real(r_kind),parameter:: qcrit=0.001_r_kind


  logical polish
  integer(i_kind) j2,jreal,ipow,jimag,kmod2,i
  real(r_kind) qa,q,r
  real(r_kind),dimension(nn,nn):: van
  complex(r_kind) ca,cb,cc
  complex(r_kind),dimension(nn):: croot
  complex(r_kind),dimension(0:nn):: cof

  data polish/.true./

  polish=.true.
  kmod2=mod(m,2)
! Set up the coefficients of the polynomial in z of degree m that approximates
! the function, exp(z/2):
  cof=zero
  cof(0)=one
  do i=1,m
     cof(i)=half*cof(i-1)/real(i,r_kind)
  enddo
! Locate the m roots of this polynomial:
  call zroots(cof,m,croot,polish)

! If m is even, all roots are complex; if odd, there is one real root,
! which is the first in the list, croot, returned by subr. zroots:
  if(kmod2==1)then    ! treat the single real root:
     r=-real(croot(1),r_kind)
     q=-aimag(croot(1))
     qa=abs(q)
     if(qa>qcrit) then
        write(6,*)'RFDPAR1:  imaginary root qa=',qa,' > qcrit=',qcrit
        call stop2(69)
     endif
     r=sqrt(r)
     rate(1)=r
     van(1,1)=one
     van(2,1)=r
     do i=3,m
        van(i,1)=van(i-1,1)*r*r
     enddo
  endif
! The complex roots occur in conjugate pairs and emerge from zroots
! ordered by their real parts. It is therefore sufficient to use stride-2
! when looping through the part of the list, croot, that contains the
! complex roots.
  do j2=2,m,2         ! loop over remaining independent complex roots
     jreal=kmod2+j2-1
     jimag=kmod2+j2
     ca=-croot(j2)
     cb=sqrt(ca)
     r=real(cb,r_kind)
     q=aimag(cb)
     if(r<zero)then
        cb=-cb
        r=-r
        q=-q
     endif
     rate(jreal)=r
     rate(jimag)=q
     van(1,jreal)=one
     van(1,jimag)=zero
     do i=2,m
        ipow=i*2-3
        cc=cb**ipow
        van(i,jreal)=real(cc,r_kind)
        van(i,jimag)=-aimag(cc)
     enddo
  enddo
  be(1)=one
  do i=2,m
     be(i)=zero
  enddo
  call linmm(van,be,m,1,nn,m)
  return
end subroutine rfdpar1


subroutine rfdpar2(be,rate,turn,samp,m)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfdpar2
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine sets the "turning conditions" for a recursive
!            that intercepts a domain boundary.
!
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser - added documentation
!   2011-07-03  todling - use mathkinds for math-intrisic
!
!   input argument list:
!     be   - Recursive filter coefficients listed by characteristic modes
!     rate - Decay rate factors for the characteristic modes of the filter
!     m    - degree of the recursive filter
!
!   output argument list:
!     turn - square matrix of "turning coefficients" for bounded domain
!     samp - amplitude adjustment factor related to normalization.
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,half,two
  implicit none

  integer(i_kind)            ,intent(in   ) :: m
  real(r_kind),dimension(m)  ,intent(in   ) :: be,rate
  real(r_kind),dimension(m,m),intent(  out) :: turn
  real(r_kind)               ,intent(  out) :: samp
  
  integer(i_kind) kr,li,lr,ki,kmod2
  real(r_kind) beki,bekr,be1,r1,s
  complex(r_kind) cbe,crl,cl1,c1l,crk,crj,cbeh,clk,clj
  complex(r_kind) clkr,clki

  kmod2=mod(m,2)
  s=zero
  if(kmod2==1)then     ! the first root is real:
     r1=rate(1)
     be1=be(1)
     turn(1,1)=be1/(two*r1)
     s=s+turn(1,1)*be1
     do lr=kmod2+1,m,2
        li=lr+1
        cbe=cmplx(be(lr),be(li),r_kind)
        crl=cmplx(rate(lr),rate(li),r_kind)
        cl1=cbe/(r1+crl)
        turn(lr,1)=real(cl1,r_kind)
        turn(li,1)=aimag(cl1)
        c1l=be1/(r1+crl)
        turn(1,lr)=real(c1l,r_kind)
        turn(1,li)=-aimag(c1l)
        s=s+turn(lr,1)*be1+turn(1,lr)*be(lr)+turn(1,li)*be(li)
     enddo
  endif
  do kr=kmod2+1,m,2
     ki=kr+1
     crk=cmplx(rate(kr),rate(ki),r_kind)
     crj=conjg(crk)
     bekr=be(kr)
     beki=be(ki)
     do lr=kmod2+1,m,2
        li=lr+1
        cbeh=half*cmplx(be(lr),be(li),r_kind)
        crl=cmplx(rate(lr),rate(li),r_kind)
        clk=cbeh/(crl+crk)
        clj=cbeh/(crl+crj)
        clkr=clk+clj
        clki=clk-clj
        turn(lr,kr)= real(clkr,r_kind)
        turn(li,kr)= aimag(clkr)
        turn(lr,ki)=-aimag(clki)
        turn(li,ki)= real(clki,r_kind)
        s=s+turn(lr,kr)*bekr+turn(lr,ki)*beki
     enddo
  enddo
  samp=half/s
  return
end subroutine rfdpar2


subroutine rfdparv(dsh,rate,al,n,m)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfdparv
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract: Convert filter mode decay rates and effective grid steps into
!           actual recursive filter "alpha" coefficients for each 
!           characteristic mode of the filter.
!            
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser - added documentation
!   2004-08-18  derber - remove complex variables, add intent
!
!   input argument list:
!     dsh  - Effective grid step in covariance-scale units
!     rate - Decay rate factors for the characteristic modes of the filter
!     n    - size of the grid in the active direction
!     m    - degree of the recursive filter
!
!   output argument list:
!     al   - "alpha" coefficients for each of the characteristic modes 
!
!$$$
  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind)            ,intent(in   ) :: n,m
  real(r_kind),dimension(n)  ,intent(in   ) :: dsh
  real(r_kind),dimension(m)  ,intent(in   ) :: rate
  real(r_kind),dimension(n,m),intent(  out) :: al

  integer(i_kind) i,kr,ki,kmod2
  real(r_kind):: c0i,c0r,c1,c2
  
  kmod2=mod(m,2)
  if(kmod2 == 1)then
     c0r=-rate(1)
     do i=1,n
        al(i,1)=exp(c0r*dsh(i))
     enddo
  endif
  do kr=kmod2+1,m,2
     ki=kr+1
     c0i=-rate(ki)
     c0r=-rate(kr)
     do i=1,n
        c1=c0i*dsh(i)
        c2=exp(c0r*dsh(i))
        al(i,kr)=c2*cos(c1)
        al(i,ki)=c2*sin(c1)
     end do
  enddo
  return
end subroutine rfdparv


subroutine linmm(a,b,m,mm,na,nb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    linmm
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine carries out the linear inversion for a system of
!            m equations with mm right-hand side vectors.
!
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser  - added documentation
!   2013-01-26  parrish - WCOSS debug compile error -- change input a from intent(in) to intent(inout)
!
!   input argument list:
!     a    - the system matrix (not preserved on output)
!     b    - the right-hand side vectors (output as solution vectors)
!     m    - the order of the system matrix
!     mm   - the number of right-hand side vectors.
!     na   - first fortran dimension of array, a
!     nb   - forst fortran dimension of array, b
!
!   output argument list:
!     b    - the mm solution vectors
!
!$$$
  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind)             ,intent(in   ) :: m,mm,na,nb
  real(r_kind),dimension(na,*),intent(inout) :: a
  real(r_kind),dimension(nb,*),intent(inout) :: b

  integer(i_kind),dimension(m):: ipiv    ! <- numerical pivot sequence
  real(r_kind) d

  call ldum(a,ipiv,d,m,na) ! <- perform an L-D-U decomposition with pivoting
  call udlmm(a,b,ipiv,m,mm,na,nb) ! <- perform back-substitutions

  return
end subroutine linmm


subroutine ldum(a,ipiv,d,m,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ldum
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine performs an L-D-U decomposition (in place) with
!            a partial pivoting sequence
!
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser  - added documentation
!
!   input argument list:
!     a    - matrix of a linear system 
!     m    - order of the system matrix
!     na   - first fortran dimension of matrix, a
!
!   output argument list:
!     a    - pivoted L-D-U decomposed system matrix
!     ipiv - pivoting sequence code (list of successive row-swap targets)
!     d    - (real) determinant sign-change flag, +1 or -1.
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  implicit none

  integer(i_kind)                ,intent(in   ) :: m,na
  integer(i_kind),dimension(m)   ,intent(  out) :: ipiv
  real(r_kind)   ,dimension(na,*),intent(inout) :: a
  real(r_kind)                   ,intent(  out) :: d

  integer(i_kind),parameter:: nn=500

  integer(i_kind) k,ibig,jm,i,jp,j
  real(r_kind) aa,aam,t,abig,ajji,aij,ajj
  real(r_kind),dimension(nn):: s

  if(m>nn) then
     write(6,*)'LDUM:  matrix order m=',m,' greater than assumed limit nn=',nn
     call stop2(67)
  endif
  do i=1,m
     aam=zero
     do j=1,m
        aa=abs(a(i,j))
        if(aa>aam) then
           aam=aa
        end if
     enddo
     if (aam==zero) then
        write(6,*)'LDUM:  row ',i,' of matrix in ldum vanishes'
        call stop2(65)
     endif
     s(i)=one/aam
  enddo
  d=one
  ipiv(m)=m ! <- set default to "no swap"
  do j=1,m-1
     jp=j+1
     abig=s(j)*abs(a(j,j))
     ibig=j
     do i=jp,m
        aa=s(i)*abs(a(i,j))
        if(aa>abig)then
           ibig=i
           abig=aa
        endif
     enddo
!  swap rows, recording changed sign of determinant
     ipiv(j)=ibig
     if(ibig/=j)then
        d=-d ! <- change sign of determinant each time a row-swap occurs
        do k=1,m
           t=a(j,k)
           a(j,k)=a(ibig,k)
           a(ibig,k)=t
        enddo
        s(ibig)=s(j)
     endif
     ajj=a(j,j)
     if(ajj==zero)then
        jm=j-1
        write(6,*)'LDUM:  ***failure*** matrix singular, rank=',jm
        call stop2(66)
     endif
     ajji=one/ajj
     do i=jp,m
        aij=ajji*a(i,j)
        a(i,j)=aij
        do k=jp,m
           a(i,k)=a(i,k)-aij*a(j,k)
        enddo
     enddo
  enddo
  return
end subroutine ldum


subroutine udlmm(a,b,ipiv,m,mm,na,nb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    udlmm
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine performs the L, the D and the U back-substitution
!            steps on each of the mm right-hand side vectors, taking due 
!            account of the encoded pivoting sequence. Note: the calls to
!            subr. dsbvr could be replaced by the f90 intrinsic, "dot_product",
!            (or "sum"), not available when this routine was originally coded. 
!
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser  - added documentation
!
!   input argument list:
!     a    - L-D-U pivoted factors of matrix, stored in place
!     b    - mm right-hand sides
!     ipiv - pivoting sequence of transposition image indexes
!     m    - order of matrix
!     mm   - number of right-hand sides
!     na   - first fortran dimension of a
!     nb   - first fortran dimension of b
!
!   output argument list:
!     b    - mm solution vectors
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: one
  implicit none

  integer(i_kind)                ,intent(in   ) :: na,nb,m,mm
  integer(i_kind),dimension(m)   ,intent(in   ) :: ipiv
  real(r_kind)   ,dimension(na,*),intent(in   ) :: a
  real(r_kind)   ,dimension(nb,*),intent(inout) :: b

  integer(i_kind) k,i,l
  real(r_kind) s,aiii
  do k=1,mm !loop over columns of b
     do i=1,m
        l=ipiv(i)
        s=b(l,k)
        b(l,k)=b(i,k)
        call dsbvr(b(1,k),a(i,1),s,i-1,na)
        b(i,k)=s
     enddo
     b(m,k)=b(m,k)/a(m,m)
     do i=m-1,1,-1
        aiii=one/a(i,i)
        call dsbvr(b(i+1,k),a(i,i+1),b(i,k),m-i,na)
        b(i,k)=b(i,k)*aiii
     enddo
  enddo
  return
end subroutine udlmm


subroutine dsbvr(d,a,s,m,na)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dsbvr
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  Subtract the dot product of vector d and matrix row a(1,:)
!            from scalar s. (Note: this is a carry-over from the pre-f90 era;
!            a more succinct version of the same task can now be accomplished
!            by using the dot_product intrinsic in the routine that calls this
!            one.)
!
! program history log:
!   1998-01-01  purser
!   2009-08-10  lueken updated subprogram doc block
!
!   input argument list:
!    na,m
!    s
!    d
!    a
!
!   output argument list:
!    s
!
! attributes:
!   language: f90
!   machine:
!
!$$$
  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind)             ,intent(in   ) :: na,m
  real(r_kind)                ,intent(inout) :: s
  real(r_kind),dimension(m)   ,intent(in   ) :: d
  real(r_kind),dimension(na,*),intent(in   ) :: a

  integer(i_kind) i

  do i=1,m
     s=s-d(i)*a(1,i)
  enddo
  return
end subroutine dsbvr


subroutine zroots(a,m,roots,polish)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    zroots
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine is the root-finding procedure adapted from
!            Press et al. 1992: Numerical Recipes for fortran 77, Cambridge.
!
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser  - added documentation
!   2004-10-28  treadon - replace "tiny" with "tiny_r_kind"
!   2005-03-29  treadon - define small = sqrt(tiny_r_kind)
!   2011-07-03  todling - use mathkinds for math-intrisic
!
!   input argument list:
!     a    - complex array of m+1 polynomial coefficients
!     m    - degree of polynomial
!     polish - logical flag to determine whether to "polish" the root values
!
!   output argument list:
!     roots - array of m complex roots listed in increasing order of real prts
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,two,tiny_r_kind
  implicit none

  logical                     ,intent(in   ) :: polish
  integer(i_kind)             ,intent(in   ) :: m
  complex(r_kind),dimension(*),intent(in   ) :: a
  complex(r_kind),dimension(m),intent(  out) :: roots

  integer(i_kind),parameter:: maxm=101

  integer(i_kind) j,i,jj
  real(r_kind)::  small,twosmall2
  complex(r_kind) x,b,c
  complex(r_kind),dimension(maxm):: ad


  small=sqrt(tiny_r_kind)
  twosmall2=two*small*small

  do j=1,m+1
     ad(j)=a(j)
  end do
  do j=m,1,-1
     x=cmplx(zero,zero,r_kind)
     call laguer(ad,j,x,small,.false.)
     if(abs(aimag(x)) <= twosmall2*abs(real(x,r_kind))) then
        x=cmplx(real(x,r_kind),zero,r_kind)
     end if
     roots(j)=x
     b=ad(j+1)
     do jj=j,1,-1
        c=ad(jj)
        ad(jj)=b
        b=x*b+c
     end do
  end do
  if (polish) then
     do j=1,m
        call laguer(a,m,roots(j),small,.true.)
     end do
  endif
  do j=2,m
     x=roots(j)
     do i=j-1,1,-1
        if(real(roots(i),r_kind)<=real(x,r_kind))then
           roots(i+1)=x
           cycle
        end if
        roots(i+1)=roots(i)
     end do
     roots(1)=x
  end do
  return
end subroutine zroots


subroutine laguer(a,m,x,small,polish)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    laguer
!     prgmmr:    purser      org:  np20              date:  1998-01-01
!
! abstract:  This routine is an adaptation of the "Numerical Recipes"
!            root-refining "Laguerre's Method" subroutine. See Numerical
!            Recipes in Fortran 77, 2nd Ed., by Press, Teukolsky, Vetterling
!            and Flannery, Cambridge.
!
! program history log:
!   1998-01-01  purser
!   2004-06-23  purser  - update documentation
!   2011-07-02  todling - adjust smalls for single precision
!
!   input argument list:
!     a    - complex array of m+1 polynomial coefficients
!     m    - degree of the polynomial
!     small - a small real +ve number comparable with machine precision
!     polish - logical flag to determine whether to "polish" result to
!              high precision
!
!   output argument list:
!     x    - one of the complex roots of the polynomial
! 
!$$$
  use kinds, only: r_kind,i_kind,r_single,r_double
  use constants, only: zero,two
  implicit none

  logical                     ,intent(in   ) :: polish
  complex(r_kind),dimension(*),intent(in   ) :: a(*)
  complex(r_kind)             ,intent(inout) :: x
  integer(i_kind)             ,intent(in   ) :: m
  real(r_kind)                ,intent(in   ) :: small

  integer(i_kind),parameter:: maxit=100

  integer(i_kind) iter,j
  real(r_kind) abx,cdx,err,dxold
  real(r_kind) smalls
  complex(r_kind) dx,x1,b,d,f,g,h,sq,gp,gm,g2

  if(r_kind==r_single) then
     smalls=6.8e-4_r_kind
  else if(r_kind==r_double) then
     smalls=6.8e-8_r_kind
  else
     write(6,*)'LAGUER:  unsupported precision r_kind=', r_kind
     call stop2(99)
  endif

  dxold=abs(x)
  do iter=1,maxit
     b=a(m+1)
     err=abs(b)
     d=zero
     f=zero
     abx=abs(x)
     do j=m,1,-1
        f=x*f+d
        d=x*d+b
        b=x*b+a(j)
        err=abs(b)+abx*err
     end do
     err=smalls*err
     if(abs(b)<=err) then
        return
     else
        g=d/b
        g2=g*g
        h=g2-two*f/b
        sq=sqrt((m-1)*(m*h-g2))
        gp=g+sq
        gm=g-sq
        if(abs(gp)<abs(gm)) then
           gp=gm
        end if
        dx=m/gp
     endif
     x1=x-dx
     if(x==x1)return
     x=x1
     cdx=abs(dx)
     dxold=cdx
     if(.not.polish)then
        if(cdx<=small*abs(x))return
     endif
  end do
  write(6,*)'LAGUER:  ***warning*** too many iterations'
  return
end subroutine laguer

