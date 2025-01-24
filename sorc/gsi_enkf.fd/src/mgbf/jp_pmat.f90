module jp_pmat
!$$$  module documentation block
!                .      .    .                                       .
! module:   jp_pmat
!   prgmmr: fujita           org: NOAA/EMC            date: 1993
!
! abstract:  Utility routines for various linear inversions and Cholesky
!
! module history log:
!   2002        purser
!   2009        purser
!   2012        purser
!
! Subroutines Included:
!    swpvv -
!    inv -
!    ldum -
!    udlmm -
!    l1lm -
!    ldlm -
!    invu -
!    invl -
!
! Functions Included:
!
! remarks:
!   Originally, these routines were copies of the purely "inversion" members
!   of pmat1.f90 (a most extensive collection of matrix routines -- not just
!   inversions). As well as having both single and double precision versions
!   of each routine, these versions also make provision for a more graceful
!   termination in cases where the system matrix is detected to be 
!   essentially singular (and therefore noninvertible). This provision takes
!   the form of an optional "failure flag", FF, which is normally returned
!   as .FALSE., but is returned as .TRUE. when inversion fails.
!   In Sep 2012, these routines were collected together into jp_pmat.f90 so
!   that all the main matrix routines could be in the same library, jp_pmat.a.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi
use jp_pkind, only: sp,dp,spc,dpc
use jp_pietc, only: t,f
implicit none
private
public:: ldum,udlmm,inv,L1Lm,LdLm,invl,invu
interface swpvv;  module procedure sswpvv,dswpvv,cswpvv;        end interface
interface ldum
   module procedure sldum,dldum,cldum,sldumf,dldumf,cldumf;     end interface
interface udlmm
   module procedure sudlmm,dudlmm,cudlmm,sudlmv,dudlmv,cudlmv;  end interface
interface inv
   module procedure                                                           &
sinvmt, dinvmt, cinvmt, slinmmt, dlinmmt, clinmmt, slinmvt, dlinmvt, clinmvt, &
sinvmtf,dinvmtf,cinvmtf,slinmmtf,dlinmmtf,clinmmtf,slinmvtf,dlinmvtf,clinmvtf,&
iinvf
                                                               end interface
interface L1Lm;   module procedure sL1Lm,dL1Lm,sL1Lmf,dL1Lmf;  end interface
interface LdLm;   module procedure sLdLm,dLdLm,sLdLmf,dLdLmf;  end interface
interface invl;   module procedure sinvl,dinvl,slinlv,dlinlv;  end interface
interface invu;   module procedure sinvu,dinvu,slinuv,dlinuv;  end interface

contains

!=============================================================================
subroutine sswpvv(d,e)!                                                [swpvv]
!=============================================================================
! Swap vectors
!-------------
real(sp),    intent(inout) :: d(:), e(:)
real(sp)                   :: tv(size(d))
!=============================================================================
tv = d; d = e; e = tv
end subroutine sswpvv
!=============================================================================
subroutine dswpvv(d,e)!                                                [swpvv]
!=============================================================================
real(dp), intent(inout) :: d(:), e(:)
real(dp)                :: tv(size(d))
!=============================================================================
tv = d; d = e; e = tv
end subroutine dswpvv
!=============================================================================
subroutine cswpvv(d,e)!                                                [swpvv]
!=============================================================================
complex(dpc),intent(inout) :: d(:), e(:)
complex(dpc)               :: tv(size(d))
!=============================================================================
tv = d; d = e; e = tv
end subroutine cswpvv

!=============================================================================
subroutine sinvmt(a)!                                                    [inv]
!=============================================================================
real(sp),dimension(:,:),intent(INOUT):: a
logical                              :: ff
call sinvmtf(a,ff)
if(ff)stop 'In sinvmt; Unable to invert matrix'
end subroutine sinvmt
!=============================================================================
subroutine dinvmt(a)!                                                    [inv]
!=============================================================================
real(dp),dimension(:,:),intent(inout):: a
logical                              :: ff
call dinvmtf(a,ff)
if(ff)stop 'In dinvmt; Unable to invert matrix'
end subroutine dinvmt
!=============================================================================
subroutine cinvmt(a)!                                                    [inv]
!=============================================================================
complex(dpc),dimension(:,:),intent(inout):: a
logical                                  :: ff
call cinvmtf(a,ff)
if(ff)stop 'In cinvmt; Unable to invert matrix'
end subroutine cinvmt
!=============================================================================
subroutine sinvmtf(a,ff)!                                                [inv]
!=============================================================================
! Invert matrix (or flag if can't)
!----------------
real(sp),dimension(:,:),intent(inout):: a
logical,                intent(  out):: ff 
integer                              :: m,i,j,jp,l
real(sp)                             :: d
integer,dimension(size(a,1))         :: ipiv
!=============================================================================
m=size(a,1)
if(m /= size(a,2))stop 'In sinvmtf; matrix passed to sinvmtf is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
call sldumf(a,ipiv,d,ff)
if(ff)then
   print '(" In sinvmtf; failed call to sldumf")'
   return
endif

! Invert upper triangular portion U in place:
do i=1,m; a(i,i)=1./a(i,i); enddo
do i=1,m-1
   do j=i+1,m; a(i,j)=-a(j,j)*dot_product(a(i:j-1,j),a(i,i:j-1)); enddo
enddo

! Invert lower triangular portion L in place:
do j=1,m-1; jp=j+1
   do i=jp,m; a(i,j)=-a(i,j)-dot_product(a(jp:i-1,j),a(i,jp:i-1)); enddo
enddo

!  Form the product of U**-1 and L**-1 in place
do j=1,m-1; jp=j+1
   do i=1,j; a(i,j)=a(i,j)+dot_product(a(jp:m,j),a(i,jp:m)); enddo
   do i=jp,m; a(i,j)=dot_product(a(i:m,j),a(i,i:m));         enddo
enddo

!  Permute columns according to ipiv
do j=m-1,1,-1; l=ipiv(j); call sswpvv(a(:,j),a(:,l)); enddo
end subroutine sinvmtf
!=============================================================================
subroutine dinvmtf(a,ff)!                                                [inv]
!=============================================================================
real(DP),dimension(:,:),intent(INOUT):: a
logical,                intent(  OUT):: ff
integer                              :: m,i,j,jp,l
real(DP)                             :: d
integer, dimension(size(a,1))        :: ipiv
!=============================================================================
m=size(a,1)
if(m /= size(a,2))stop 'In inv; matrix passed to dinvmtf is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
call dldumf(a,ipiv,d,ff)
if(ff)then
   print '(" In dinvmtf; failed call to dldumf")'
   return
endif

! Invert upper triangular portion U in place:
do i=1,m; a(i,i)=1/a(i,i); enddo
do i=1,m-1
   do j=i+1,m; a(i,j)=-a(j,j)*dot_product(a(i:j-1,j),a(i,i:j-1)); enddo
enddo

! Invert lower triangular portion L in place:
do j=1,m-1; jp=j+1
   do i=jp,m; a(i,j)=-a(i,j)-dot_product(a(jp:i-1,j),a(i,jp:i-1)); enddo
enddo

!  Form the product of U**-1 and L**-1 in place
do j=1,m-1; jp=j+1
   do i=1,j; a(i,j)=a(i,j)+dot_product(a(jp:m,j),a(i,jp:m)); enddo
   do i=jp,m; a(i,j)=dot_product(a(i:m,j),a(i,i:m));         enddo
enddo

!  Permute columns according to ipiv
do j=m-1,1,-1; l=ipiv(j); call dswpvv(a(:,j),a(:,l)); enddo
end subroutine dinvmtf
!=============================================================================
subroutine cinvmtf(a,ff)!                                                [inv]
!=============================================================================
complex(dpc),dimension(:,:),intent(INOUT):: a
logical,                    intent(  OUT):: ff
integer                                  :: m,i,j,jp,l
complex(dpc)                             :: d
integer, dimension(size(a,1))            :: ipiv
!=============================================================================
m=size(a,1)
if(m /= size(a,2))stop 'In inv; matrix passed to cinvmtf is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
call cldumf(a,ipiv,d,ff)
if(ff)then
   print '(" In cinvmtf; failed call to cldumf")'
   return
endif

! Invert upper triangular portion U in place:
do i=1,m; a(i,i)=1/a(i,i); enddo
do i=1,m-1
   do j=i+1,m; a(i,j)=-a(j,j)*sum(a(i:j-1,j)*a(i,i:j-1)); enddo
enddo

! Invert lower triangular portion L in place:
do j=1,m-1; jp=j+1
   do i=jp,m; a(i,j)=-a(i,j)-sum(a(jp:i-1,j)*a(i,jp:i-1)); enddo
enddo

!  Form the product of U**-1 and L**-1 in place
do j=1,m-1; jp=j+1
   do i=1,j; a(i,j)=a(i,j)+sum(a(jp:m,j)*a(i,jp:m)); enddo
   do i=jp,m; a(i,j)=sum(a(i:m,j)*a(i,i:m));         enddo
enddo

!  Permute columns according to ipiv
do j=m-1,1,-1; l=ipiv(j); call cswpvv(a(:,j),a(:,l)); enddo
end subroutine cinvmtf

!=============================================================================
subroutine slinmmt(a,b)!                                                 [inv]
!=============================================================================
real(sp),dimension(:,:),intent(inout):: a,b
logical                              :: ff
call slinmmtf(a,b,ff)
if(ff)stop 'In slinmmt; unable to invert linear system'
end subroutine slinmmt
!=============================================================================
subroutine dlinmmt(a,b)!                                                 [inv]
!=============================================================================
real(dp),dimension(:,:),intent(inout):: a,b
logical                              :: ff
call dlinmmtf(a,b,ff)
if(ff)stop 'In dlinmmt; unable to invert linear system'
end subroutine dlinmmt
!=============================================================================
subroutine clinmmt(a,b)!                                                 [inv]
!=============================================================================
complex(dpc),dimension(:,:),intent(inout):: a,b
logical                                  :: ff
call clinmmtf(a,b,ff)
if(ff)stop 'In clinmmt; unable to invert linear system'
end subroutine clinmmt
!=============================================================================
subroutine slinmmtf(a,b,ff)!                                             [inv]
!=============================================================================
real(SP),   dimension(:,:),intent(INOUT):: a,b
logical,                   intent(  OUT):: ff
integer,dimension(size(a,1))            :: ipiv
integer                                 :: m
real(sp)                                :: d
!=============================================================================
m=size(a,1)
if(m /= size(a,2))stop 'In inv; matrix passed to slinmmtf is not square'
if(m /= size(b,1))&
     stop 'In inv; matrix and vectors in slinmmtf have unmatched sizes'
call sldumf(a,ipiv,d,ff)
if(ff)then
   print '("In slinmmtf; failed call to sldumf")'
   return
endif
call sudlmm(a,b,ipiv)
end subroutine slinmmtf
!=============================================================================
subroutine dlinmmtf(a,b,ff)!                                             [inv]
!=============================================================================
real(dp),dimension(:,:),   intent(inout):: a,b
logical,                   intent(  out):: ff
integer, dimension(size(a,1))           :: ipiv
integer                                 :: m 
real(dp)                                :: d
!=============================================================================
m=size(a,1)
if(m /= size(a,2))stop 'In inv; matrix passed to dlinmmtf is not square'
if(m /= size(b,1))&
     stop 'In inv; matrix and vectors in dlinmmtf have unmatched sizes'
call dldumf(a,ipiv,d,ff)
if(ff)then
   print '("In dlinmmtf; failed call to dldumf")'
   return
endif
call dudlmm(a,b,ipiv)
end subroutine dlinmmtf
!=============================================================================
subroutine clinmmtf(a,b,ff)!                                             [inv]
!=============================================================================
complex(dpc),dimension(:,:),intent(INOUT):: a,b
logical,                    intent(  OUT):: ff
integer, dimension(size(a,1))            :: ipiv
integer                                  :: m 
complex(dpc)                             :: d
!=============================================================================
m=size(a,1)
if(m /= size(a,2))stop 'In inv; matrix passed to dlinmmtf is not square'
if(m /= size(b,1))&
     stop 'In inv; matrix and vectors in dlinmmtf have unmatched sizes'
call cldumf(a,ipiv,d,ff)
if(ff)then
   print '("In clinmmtf; failed call to cldumf")'
   return
endif
call cudlmm(a,b,ipiv)
end subroutine clinmmtf

!=============================================================================
subroutine slinmvt(a,b)!                                                 [inv]
!=============================================================================
real(sp),   dimension(:,:),intent(inout):: a
real(sp),   dimension(:),  intent(inout):: b
logical                                 :: ff
call slinmvtf(a,b,ff)
if(ff)stop 'In slinmvt; matrix singular, unable to continue'
end subroutine slinmvt
!=============================================================================
subroutine dlinmvt(a,b)!                                                 [inv]
!=============================================================================
real(dp),   dimension(:,:),intent(inout):: a
real(dp),   dimension(:),  intent(inout):: b
logical                                 :: ff
call dlinmvtf(a,b,ff)
if(ff)stop 'In dlinmvt; matrix singular, unable to continue'
end subroutine dlinmvt
!=============================================================================
subroutine clinmvt(a,b)!                                                 [inv]
!=============================================================================
complex(dpc),   dimension(:,:),intent(inout):: a
complex(dpc),   dimension(:),  intent(inout):: b
logical                                     :: ff
call clinmvtf(a,b,ff)
if(ff)stop 'In clinmvt; matrix singular, unable to continue'
end subroutine clinmvt
!=============================================================================
subroutine slinmvtf(a,b,ff)!                                             [inv]
!=============================================================================
real(sp),dimension(:,:),intent(inout):: a
real(sp),dimension(:),  intent(inout):: b
logical,                intent(  out):: ff
integer,dimension(size(a,1))         :: ipiv
real(sp)                             :: d
!=============================================================================
if(size(a,1) /= size(a,2).or. size(a,1) /= size(b))&
     stop 'In inv; In slinmvtf; incompatible array dimensions'
call sldumf(a,ipiv,d,ff)
if(ff)then
   print '("In slinmvtf; failed call to sldumf")'
   return
endif
call sudlmv(a,b,ipiv) 
end subroutine slinmvtf
!=============================================================================
subroutine dlinmvtf(a,b,ff)!                                             [inv]
!=============================================================================
real(dp),dimension(:,:),intent(inout):: a
real(dp),dimension(:),  intent(inout):: b
logical,                intent(  out):: ff
integer, dimension(size(a,1))        :: ipiv
real(dp)                             :: d
!=============================================================================
if(size(a,1) /= size(a,2).or. size(a,1) /= size(b))&
     stop 'In inv; incompatible array dimensions passed to dlinmvtf'
call dldumf(a,ipiv,d,ff)
if(ff)then
   print '("In dlinmvtf; failed call to dldumf")'
   return
endif
call dudlmv(a,b,ipiv)
end subroutine dlinmvtf
!=============================================================================
subroutine clinmvtf(a,b,ff)!                                             [inv]
!=============================================================================
complex(dpc),dimension(:,:),intent(inout):: a
complex(dpc),dimension(:),  intent(inout):: b
logical,                    intent(  out):: ff
integer, dimension(size(a,1))            :: ipiv
complex(dpc)                             :: d
!=============================================================================
if(size(a,1) /= size(a,2).or. size(a,1) /= size(b))&
     stop 'In inv; incompatible array dimensions passed to clinmvtf'
call cldumf(a,ipiv,d,ff)
if(ff)then
   print '("In clinmvtf; failed call to cldumf")'
   return
endif
call cudlmv(a,b,ipiv)
end subroutine clinmvtf

!=============================================================================
subroutine iinvf(imat,ff)!                                               [inv]
!=============================================================================
! Invert integer square array, imat, if possible, but flag ff=.true.
! if not possible. (Determinant of imat must be +1 or -1
!=============================================================================
integer,dimension(:,:),intent(INOUT):: imat
logical,               intent(  OUT):: ff
!-----------------------------------------------------------------------------
real(dp),parameter                           :: eps=1.e-10_dp
real(dp),dimension(size(imat,1),size(imat,1)):: dmat
integer                                      :: m,i,j
!=============================================================================
m=size(imat,1)
if(m /= size(imat,2))stop 'In inv; matrix passed to iinvf is not square'
dmat=imat; call inv(dmat,ff)
if(.not.ff)then
   do j=1,m
      do i=1,m
         imat(i,j)=nint(dmat(i,j)); if(abs(dmat(i,j)-imat(i,j))>eps)ff=t
      enddo
   enddo
endif
end subroutine iinvf

!=============================================================================
subroutine sldum(a,ipiv,d)!                                             [ldum]
!=============================================================================
real(sp),intent(inout) :: a(:,:) 
real(sp),intent(out  ) :: d
integer, intent(out  ) :: ipiv(:)
logical                :: ff
call sldumf(a,ipiv,d,ff)
if(ff)stop 'In sldum; matrix singular, unable to continue'
end subroutine sldum
!=============================================================================
subroutine dldum(a,ipiv,d)!                                             [ldum]
!=============================================================================
real(dp),intent(inout) :: a(:,:) 
real(dp),intent(out  ) :: d
integer, intent(out  ) :: ipiv(:)
logical:: ff
call dldumf(a,ipiv,d,ff)
if(ff)stop 'In dldum; matrix singular, unable to continue'
end subroutine dldum
!=============================================================================
subroutine cldum(a,ipiv,d)!                                             [ldum]
!=============================================================================
complex(dpc),intent(inout) :: a(:,:) 
complex(dpc),intent(out  ) :: d
integer,     intent(out  ) :: ipiv(:)
logical:: ff
call cldumf(a,ipiv,d,ff)
if(ff)stop 'In cldum; matrix singular, unable to continue'
end subroutine cldum
!=============================================================================
subroutine sldumf(a,ipiv,d,ff)!                                         [ldum]
!=============================================================================
!   R.J.Purser, NCEP, Washington D.C.	1996
!		    SUBROUTINE	LDUM
!  perform l-d-u decomposition of square matrix a in place with
!  pivoting.
!
!  <-> a    square matrix to be factorized
!  <-- ipiv array encoding the pivoting sequence
!  <-- d    indicator for possible sign change of determinant
!  <-- ff:  failure flag, set to .true. when determinant of a vanishes.
!=============================================================================
real(SP),intent(INOUT) :: a(:,:) 
real(SP),intent(OUT  ) :: d
integer, intent(OUT  ) :: ipiv(:)
logical, intent(OUT  ) :: ff
integer                :: m,i, j, jp, ibig, jm
real(SP)               :: s(size(a,1)),  aam, aa, abig,  ajj, ajji, aij
!=============================================================================
ff=f
m=size(a,1)
do i=1,m
  aam=0
  do j=1,m
    aa=abs(a(i,j))
    if(aa > aam)aam=aa
  enddo
  if(aam == 0)then
    print '("In sldumf; row ",i6," of matrix vanishes")',i
    ff=t
    return
  endif
  s(i)=1/aam
enddo
d=1.
ipiv(m)=m
do j=1,m-1
  jp=j+1
  abig=s(j)*abs(a(j,j))
  ibig=j
  do i=jp,m
    aa=s(i)*abs(a(i,j))
    if(aa > abig)then
      ibig=i
      abig=aa
    endif
  enddo
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  if(ibig /= j)then
    d=-d
    call sswpvv(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  endif
  ajj=a(j,j)
  if(ajj == 0)then
    jm=j-1
    print '(" failure in sldumf:"/" matrix singular, rank=",i3)',jm
    ff=t
    return
  endif
  ajji=1/ajj
  do i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  enddo
enddo
end subroutine sldumf
!=============================================================================
subroutine DLDUMf(A,IPIV,D,ff)!                                         [ldum]
!=============================================================================
real(DP), intent(INOUT) :: a(:,:) 
real(DP), intent(OUT  ) :: d
integer,  intent(OUT  ) :: ipiv(:)
logical,  intent(OUT  ) :: ff
integer                 :: m,i, j, jp, ibig, jm
real(DP)                :: s(size(a,1)),  aam, aa, abig,  ajj, ajji, aij
!=============================================================================
ff=f
m=size(a,1)
do i=1,m
  aam=0
  do j=1,m
    aa=abs(a(i,j))
    if(aa > aam)aam=aa
  enddo
  if(aam == 0)then
    print '("In dldumf;  row ",i6," of matrix vanishes")',i
    ff=t
    return
  endif
  s(i)=1/aam
enddo
d=1.
ipiv(m)=m
do j=1,m-1
  jp=j+1
  abig=s(j)*abs(a(j,j))
  ibig=j
  do i=jp,m
    aa=s(i)*abs(a(i,j))
    if(aa > abig)then
      ibig=i
      abig=aa
    endif
  enddo
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  if(ibig /= j)then
    d=-d
    call dswpvv(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  endif
  ajj=a(j,j)
  if(ajj == 0)then
    jm=j-1
    print '(" Failure in dldumf:"/" matrix singular, rank=",i3)',jm
    ff=t
    return
  endif
  ajji=1/ajj
  do i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  enddo
enddo
end subroutine DLDUMf
!=============================================================================
subroutine cldumf(a,ipiv,d,ff)!                                         [ldum]
!=============================================================================
use jp_pietc, only: c0
complex(dpc), intent(INOUT)  :: a(:,:) 
complex(dpc), intent(OUT  )  :: d
integer,      intent(OUT  )  :: ipiv(:)
logical,      intent(OUT  )  :: ff
integer                      :: m,i, j, jp, ibig, jm
complex(dpc)                 :: ajj, ajji, aij
real(dp)                     :: aam,aa,abig
real(dp),dimension(size(a,1)):: s
!=============================================================================
ff=f
m=size(a,1)
do i=1,m
  aam=0
  do j=1,m
    aa=abs(a(i,j))
    if(aa > aam)aam=aa
  enddo
  if(aam == 0)then
    print '("In cldumf;  row ",i6," of matrix vanishes")',i
    ff=t
    return
  endif
  s(i)=1/aam
enddo
d=1.
ipiv(m)=m
do j=1,m-1
  jp=j+1
  abig=s(j)*abs(a(j,j))
  ibig=j
  do i=jp,m
    aa=s(i)*abs(a(i,j))
    if(aa > abig)then
      ibig=i
      abig=aa
    endif
  enddo
!  swap rows, recording changed sign of determinant
  ipiv(j)=ibig
  if(ibig /= j)then
    d=-d
    call cswpvv(a(j,:),a(ibig,:))
    s(ibig)=s(j)
  endif
  ajj=a(j,j)
  if(ajj == c0)then
    jm=j-1
    print '(" Failure in cldumf:"/" matrix singular, rank=",i3)',jm
    ff=t
    return
  endif
  ajji=1/ajj
  do i=jp,m
    aij=ajji*a(i,j)
    a(i,j)=aij
    a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
  enddo
enddo
end subroutine cldumf

!=============================================================================
subroutine sudlmm(a,b,ipiv)!                                           [udlmm]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMM
!  use l-u factors in A to back-substitute for several rhs in B, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrux
!  <-> B    rt-hand-sides vectors on input, corresponding solutions on return
!  --> IPIV array encoding the pivoting sequence
!=============================================================================
integer, dimension(:),  intent(in)    :: ipiv 
real(sp),dimension(:,:),intent(in)    :: a 
real(sp),dimension(:,:),intent(inout) :: b 
integer                               :: m,i, k, l
real(sp)                              :: s,aiii
!=============================================================================
m=size(a,1)
do k=1,size(b,2) !loop over columns of b
  do i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - sum(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  enddo
  b(m,k)=b(m,k)/a(m,m)
  do i=m-1,1,-1
    aiii=1/a(i,i)
    b(i,k) = b(i,k) - sum(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  enddo
enddo
end subroutine sudlmm
!=============================================================================
subroutine dudlmm(a,b,ipiv)!                                           [udlmm]
!=============================================================================
integer,  dimension(:),  intent(in   ) :: ipiv 
real(dp), dimension(:,:),intent(in   ) :: a 
real(dp), dimension(:,:),intent(inout) :: b 
integer                                :: m,i, k, l
real(dp)                               :: s,aiii
!=============================================================================
m=size(a,1)
do k=1, size(b,2)!loop over columns of b
  do i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - sum(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  enddo
  b(m,k)=b(m,k)/a(m,m)
  do i=m-1,1,-1
    aiii=1/a(i,i)
    b(i,k) = b(i,k) - sum(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  enddo
enddo
end subroutine dudlmm
!=============================================================================
subroutine cudlmm(a,b,ipiv)!                                           [udlmm]
!=============================================================================
integer,     dimension(:),  intent(in   ) :: ipiv 
complex(dpc),dimension(:,:),intent(in   ) :: a 
complex(dpc),dimension(:,:),intent(inout) :: b 
integer                                   :: m,i, k, l
complex(dpc)                              :: s,aiii
!=============================================================================
m=size(a,1)
do k=1, size(b,2)!loop over columns of b
  do i=1,m
    l=ipiv(i)
    s=b(l,k)
    b(l,k)=b(i,k)
    s = s - sum(b(1:i-1,k)*a(i,1:i-1))
    b(i,k)=s
  enddo
  b(m,k)=b(m,k)/a(m,m)
  do i=m-1,1,-1
    aiii=1/a(i,i)
    b(i,k) = b(i,k) - sum(b(i+1:m,k)*a(i,i+1:m))
    b(i,k)=b(i,k)*aiii
  enddo
enddo
end subroutine cudlmm

!=============================================================================
subroutine sudlmv(a,b,ipiv)!                                           [udlmv]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1993
!		    SUBROUTINE UDLMV
!  use l-u factors in A to back-substitute for 1 rhs in B, using ipiv to
!  define the pivoting permutation used in the l-u decomposition.
!
!  --> A    L-D-U factorization of linear system matrix
!  <-> B    right-hand-side vector on input, corresponding solution on return
!  --> IPIV array encoding the pivoting sequence
!=============================================================================
integer, dimension(:),  intent(in)    :: ipiv 
real(sp),dimension(:,:),intent(in)    :: a 
real(sp),dimension(:),  intent(inout) :: b 
integer                               :: m,i, l
real(sp)                              :: s,aiii
!=============================================================================
m=size(a,1)
do i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - sum(b(1:i-1)*a(i,1:i-1))
   b(i)=s
enddo
b(m)=b(m)/a(m,m)
do i=m-1,1,-1
   aiii=1/a(i,i)
   b(i) = b(i) - sum(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
enddo
end subroutine sudlmv
!=============================================================================
subroutine dudlmv(a,b,ipiv)!                                           [udlmv]
!=============================================================================
integer,   dimension(:),  intent(in   ) :: ipiv(:) 
real(dp),  dimension(:,:),intent(in   ) :: a(:,:) 
real(dp),  dimension(:),  intent(inout) :: b(:) 
integer                                 :: m,i, l
real(dp)                                :: s,aiii
!=============================================================================
m=size(a,1)
do i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - sum(b(1:i-1)*a(i,1:i-1))
   b(i)=s
enddo
b(m)=b(m)/a(m,m)
do i=m-1,1,-1
   aiii=1/a(i,i)
   b(i) = b(i) - sum(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
enddo
end subroutine dudlmv
!=============================================================================
subroutine cudlmv(a,b,ipiv)!                                           [udlmv]
!=============================================================================
integer,     dimension(:),  intent(in   ) :: ipiv(:) 
complex(dpc),dimension(:,:),intent(in   ) :: a(:,:) 
complex(dpc),dimension(:),  intent(inout) :: b(:) 
integer                                   :: m,i, l
complex(dpc)                              :: s,aiii
!=============================================================================
m=size(a,1)
do i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - sum(b(1:i-1)*a(i,1:i-1))
   b(i)=s
enddo
b(m)=b(m)/a(m,m)
do i=m-1,1,-1
   aiii=1/a(i,i)
   b(i) = b(i) - sum(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
enddo
end subroutine cudlmv

!=============================================================================
subroutine sl1lm(a,b) !                                                 [l1lm]
!=============================================================================
!  Cholesky, M -> L*U, U(i,j)=L(j,i)
!=============================================================================
real(sp), intent(in   ) :: a(:,:)
real(sp), intent(inout) :: b(:,:)
!-----------------------------------------------------------------------------
logical:: ff
call sl1lmf(a,b,ff)
if(ff)stop 'In sl1lm; matrix singular, unable to continue'
end subroutine sl1lm
!=============================================================================
subroutine dl1lm(a,b) !                                                 [l1lm]
!=============================================================================
!  Cholesky, M -> L*U, U(i,j)=L(j,i)
!=============================================================================
real(dp), intent(in   ) :: a(:,:)
real(dp), intent(inout) :: b(:,:)
!-----------------------------------------------------------------------------
logical:: ff
call dl1lmf(a,b,ff)
if(ff)stop 'In dl1lm; matrix singular, unable to continue'
end subroutine dl1lm

!=============================================================================
subroutine sl1lmf(a,b,ff)!                                              [L1Lm] 
!=============================================================================
! Cholesky, M -> L*U, U(i,j)=L(j,i)
!=============================================================================
real(sp), intent(IN   ) :: a(:,:)
real(sp), intent(INOUT) :: b(:,:)
logical                 :: ff
!-----------------------------------------------------------------------------
integer                 :: m,j, jm, jp, i
real(sp)                :: s, bjji
!=============================================================================
m=size(a,1)
ff=f
do j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - sum(b(j,1:jm)*b(j,1:jm))
  ff=(S <= 0)
  if(ff)then
     print '("sL1Lmf detects nonpositive a, rank=",i6)',jm
     return
  endif
  b(j,j)=sqrt(s)
  bjji=1/b(j,j)
  do i=jp,m
    s = a(i,j) - sum(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  enddo
  b(1:jm,j) = 0
enddo
end subroutine sl1lmf
!=============================================================================
subroutine dl1lmf(a,b,ff) !                                             [L1Lm]
!=============================================================================
real(dp), intent(IN   ) :: a(:,:) 
real(dp), intent(INOUT) :: b(:,:) 
logical                 :: ff
!-----------------------------------------------------------------------------
integer                 :: m,j, jm, jp, i
real(dp)                :: s, bjji
!=============================================================================
m=size(a,1)
ff=f
do j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - sum(b(j,1:jm)*b(j,1:jm))
  ff=(s <= 0)
  if(ff)then
     print '("dL1LMF detects nonpositive A, rank=",i6)',jm
     return
  endif
  b(j,j)=sqrt(s)
  bjji=1/b(j,j)
  do i=jp,m
    s = a(i,j) - sum(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  enddo
  b(1:jm,j) = 0
enddo
return
end subroutine dl1lmf

!=============================================================================
subroutine sldlm(a,b,d)!                                                [LdLm]
!=============================================================================
! Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
!=============================================================================
real(sp), intent(IN   ):: a(:,:)
real(sp), intent(INOUT):: b(:,:)
real(sp), intent(  OUT):: d(:)
!-----------------------------------------------------------------------------
logical:: ff
call sldlmf(a,b,d,ff)
if(ff)stop 'In sldlm; matrix singular, unable to continue'
end subroutine sldlm
!=============================================================================
subroutine dldlm(a,b,d)!                                                [LdLm]
!=============================================================================
real(dp), intent(IN   ):: a(:,:)
real(dp), intent(INOUT):: b(:,:)
real(dp), intent(  OUT):: d(:)
!-----------------------------------------------------------------------------
logical:: ff
call dldlmf(a,b,d,ff)
if(ff)stop 'In dldlm; matrix singular, unable to continue'
end subroutine dldlm

!=============================================================================
subroutine sldlmf(a,b,d,ff) !                                           [LDLM]
!=============================================================================
! Modified Cholesky decompose Q --> L*D*U
!=============================================================================
real(sp), intent(IN   ):: a(:,:)
real(sp), intent(INOUT):: b(:,:)
real(sp), intent(  OUT):: d(:)
logical,  intent(  OUT):: ff
!-----------------------------------------------------------------------------
integer                :: m,j, jm, jp, i
real(sp)               :: bjji
!=============================================================================
m=size(a,1)
ff=f
do j=1,m
  jm=j-1
  jp=j+1
  d(j)=a(j,j) - sum(b(1:jm,j)*b(j,1:jm))
  b(j,j) = 1
  ff=(d(j) == 0)
  if(ff)then
     print '("In sldlmf; singularity of matrix detected")'
     print '("Rank of matrix: ",i6)',jm
     return
  endif
  bjji=1/d(j)
  do i=jp,m
     b(j,i)=a(i,j) - dot_product(b(1:jm,j),b(i,1:jm))
     b(i,j)=b(j,i)*bjji
  enddo
  b(1:jm,j)=0
enddo
end subroutine sldlmf
!=============================================================================
subroutine dldlmf(a,b,d,ff) !                                           [LDLM]
!=============================================================================
! Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
!=============================================================================
real(dp), intent(IN   ) :: a(:,:)
real(dp), intent(INOUT) :: b(:,:)
real(dp), intent(  OUT) :: d(:)
logical,  intent(  OUT) :: ff
!-----------------------------------------------------------------------------
integer                 :: m,j, jm, jp, i
real(dp)                :: bjji
!=============================================================================
m=size(a,1)
ff=f
do j=1,m; jm=j-1; jp=j+1
  d(j)=a(j,j) - sum(b(1:jm,j)*b(j,1:jm))
  b(j,j) = 1
  ff=(d(j) == 0)
  if(ff)then
     print '("In dldlmf; singularity of matrix detected")'
     print '("Rank of matrix: ",i6)',jm
     return
  endif
  bjji=1/d(j)
  do i=jp,m
     b(j,i)=a(i,j) - dot_product(b(1:jm,j),b(i,1:jm))
     b(i,j)=b(j,i)*bjji
  enddo
  b(1:jm,j)=0
enddo
end subroutine dldlmf

!==============================================================================
subroutine sinvu(a)!                                                     [invu]
!==============================================================================
! Invert the upper triangular matrix in place by transposing, calling
! invl, and transposing again.
!==============================================================================
real,dimension(:,:),intent(inout):: a
a=transpose(a); call sinvl(a); a=transpose(a)
end subroutine sinvu
!==============================================================================
subroutine dinvu(a)!                                                     [invu]
!==============================================================================
real(dp),dimension(:,:),intent(inout):: a
a=transpose(a); call dinvl(a); a=transpose(a)
end subroutine dinvu
!==============================================================================
subroutine sinvl(a)!                                                     [invl]
!==============================================================================
!     Invert lower triangular matrix in place
!==============================================================================
real(sp), intent(inout) :: a(:,:) 
integer                 :: m,j, i
m=size(a,1)
do j=m,1,-1
  a(1:j-1,j) = 0.0
  a(j,j)=1./a(j,j)
  do i=j+1,m
    a(i,j)=-a(i,i)*sum(a(j:i-1,j)*a(i,j:i-1))
  enddo
enddo
end subroutine sinvl
!==============================================================================
subroutine dinvl(a)!                                                     [invl]
!==============================================================================
real(dp), intent(inout) :: a(:,:) 
integer                 :: m,j, i
m=size(a,1)
do j=m,1,-1
  a(1:j-1,j) = 0.0
  a(j,j)=1./a(j,j)
  do i=j+1,m
    a(i,j)=-a(i,i)*sum(a(j:i-1,j)*a(i,j:i-1))
  enddo
enddo
end subroutine dinvl

!==============================================================================
subroutine slinlv(a,u)!                                                  [invl]
!==============================================================================
!     Solve linear system involving lower triangular system matrix.
!==============================================================================
real,     intent(in   ) :: a(:,:)
real,     intent(inout) :: u(:)
integer                 :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In slinlv; incompatible array dimensions'
do i=1,size(u); u(i)=(u(i) - sum(u(:i-1)*a(i,:i-1)))/a(i,i); enddo
end subroutine slinlv
!==============================================================================
subroutine dlinlv(a,u)!                                                  [invl]
!==============================================================================
real(dp), intent(in   ) :: a(:,:)
real(dp), intent(inout) :: u(:)
integer                 :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In dlinlv; incompatible array dimensions'
do i=1,size(u); u(i)=(u(i) - sum(u(:i-1)*a(i,:i-1)))/a(i,i); enddo
end subroutine dlinlv

!==============================================================================
subroutine slinuv(a,u)!                                                  [invu]
!==============================================================================
!     Solve linear system involving upper triangular system matrix.
!==============================================================================
real,    intent(in   ) :: a(:,:)
real,    intent(inout) :: u(:)
integer             :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In linuv; incompatible array dimensions'
do i=size(u),1,-1; u(i)=(u(i) - sum(a(i+1:,i)*u(i+1:)))/a(i,i); enddo
end subroutine slinuv
!==============================================================================
subroutine dlinuv(a,u)!                                                  [invu]
!==============================================================================
real(dp), intent(in   ) :: a(:,:)
real(dp), intent(inout) :: u(:)
integer                 :: i
if(size(a,1) /= size(a,2) .or. size(a,1) /= size(u))&
     stop 'In dlinuv; incompatible array dimensions'
do i=size(u),1,-1; u(i)=(u(i) - sum(a(i+1:,i)*u(i+1:)))/a(i,i); enddo
end subroutine dlinuv

end module jp_pmat

