!============================================================================
! peuc.f90                                             Purser, October 2005
! Package for handy vector and matrix operations in Euclidean geometry.
! This package is primarily intended for 3D operations and three of the
! functions (Cross_product, Triple_product and Axial) do not possess simple
! generalizations to a generic number N of dimensions. The others, while
! admitting such N-dimensional generalizations, have not all been provided
! with such generic forms here at the time of writing, though some of these 
! may be added at a future date.
!
!   FUNCTION:
! Normalized:     Normalized version of given vector
! Cross_product:  Vector cross-product of the given 2 vectors
! Outer_product:  outer-product matrix of the given 2 vectors
! Triple_product: Scalar triple product of given 3 vectors
! Det:            Determinant of given matrix
! Axial:          Convert axial-vector <--> 2-form (antisymmetric matrix)
! Diag:           Diagnl of given matrix, or diagonal matrix of given elements
! Trace:          Trace of given matrix
! Identity:       Identity 3*3 matrix, or identity n*n matrix for a given n
! Sarea:          Spherical area subtended by three vectors
! Huarea:         Spherical area subtended by right-angled spheical triangle
!   SUBROUTINE:
! Gram:           Right-handed orthogonal basis and rank, nrank. The first 
!                 nrank basis vectors span the column range of matrix given,
!                 OR  ("plain" version) simple unpivoted gram-schmidt of a
!                 square matrix.
!============================================================================
module peuc
!============================================================================
use kinds
implicit none
private
public:: normalized,cross_product,outer_product,triple_product,det,axial, &
         diag,trace,identity,sarea,huarea,gram

interface normalized
   module procedure normalized_s,normalized_d
end interface
interface cross_product
   module procedure cross_product_s,cross_product_d
end interface
interface outer_product
   module procedure outer_product_s,outer_product_d
end interface
interface triple_product
   module procedure triple_product_s,triple_product_d
end interface
interface det
   module procedure det_s,det_d
end interface
interface axial
   module procedure axial3_s,axial3_d,axial33_s,axial33_d
end interface
interface diag
   module procedure diagn_s,diagn_d,diagnn_s,diagnn_d
end interface
interface trace
   module procedure trace_s,trace_d
end interface
interface identity
   module procedure identity_i,identity3_i
end interface
interface huarea
   module procedure huarea_s,huarea_d
end interface
interface sarea
   module procedure sarea_s,sarea_d
end interface
interface gram
   module procedure gram_s,gram_d,plaingram_s,plaingram_d
end interface

contains

!=============================================================================
function normalized_s(a)result(b)
!=============================================================================
real(sp),dimension(:),intent(IN):: a
real(sp),dimension(size(a))     :: b
real(sp)                        :: s
s=sqrt(dot_product(a,a)); if(s==0)then; b=0;else;b=a/s;endif
end function normalized_s
!=============================================================================
function normalized_d(a)result(b)
!=============================================================================
real(dp),dimension(:),intent(IN):: a
real(dp),dimension(size(a))     :: b
real(dp)                        :: s
s=sqrt(dot_product(a,a)); if(s==0)then; b=0;else;b=a/s;endif
end function normalized_d

!=============================================================================
function cross_product_s(a,b)
!=============================================================================
real(sp),dimension(3)           :: cross_product_s
real(sp),dimension(3),intent(in):: a,b
cross_product_s(1)=a(2)*b(3)-a(3)*b(2)
cross_product_s(2)=a(3)*b(1)-a(1)*b(3)
cross_product_s(3)=a(1)*b(2)-a(2)*b(1)
end function cross_product_s
!=============================================================================
function cross_product_d(a,b)result(cross_product)
!=============================================================================
real(dp),dimension(3)           :: cross_product
real(dp),dimension(3),intent(in):: a,b
cross_product(1)=a(2)*b(3)-a(3)*b(2)
cross_product(2)=a(3)*b(1)-a(1)*b(3)
cross_product(3)=a(1)*b(2)-a(2)*b(1)
end function cross_product_d

!=============================================================================
function outer_product_s(a,b)result(c)
!=============================================================================
real(sp),dimension(:),  intent(in ):: a
real(sp),dimension(:),  intent(in ):: b
real(sp),DIMENSION(size(a),size(b)):: c
integer                            :: nb,i
nb=size(b)
do i=1,nb; c(:,i)=a*b(i); enddo
end function outer_product_s
!=============================================================================
function outer_product_d(a,b)result(c)
!=============================================================================
real(dp),dimension(:),  intent(in ):: a
real(dp),dimension(:),  intent(in ):: b
real(dp),dimension(size(a),size(b)):: c
integer                            :: nb,i
nb=size(b)
do i=1,nb; c(:,i)=a*b(i); enddo
end function outer_product_d

!=============================================================================
function triple_product_s(a,b,c)result(tripleproduct)
!=============================================================================
real(sp),dimension(3),intent(IN ):: a,b,c
real(sp)                         :: tripleproduct
tripleproduct=dot_product( cross_product(a,b),c )
end function triple_product_s
!=============================================================================
function triple_product_d(a,b,c)result(tripleproduct)
!=============================================================================
real(dp),dimension(3),intent(IN ):: a,b,c
real(dp)                         :: tripleproduct
tripleproduct=dot_product( cross_product(a,b),c )
end function triple_product_d

!=============================================================================
function det_s(a)result(det)
!=============================================================================
real(sp),dimension(:,:),intent(IN )    ::a
real(sp)                               :: det
real(sp),dimension(size(a,1),size(a,1)):: b
integer                                :: n,nrank
n=size(a,1)
if(n==3)then
   det=triple_product(a(:,1),a(:,2),a(:,3))
else
   call gram(a,b,nrank,det)
   if(nrank<n)det=0
endif
end function det_s
!=============================================================================
function det_d(a)result(det)
!=============================================================================
real(dp),dimension(:,:),intent(IN )    ::a
real(dp)                               :: det
real(dp),dimension(size(a,1),size(a,1)):: b
integer                                :: n,nrank
n=size(a,1)
if(n==3)then
   det=triple_product(a(:,1),a(:,2),a(:,3))
else
   call gram(a,b,nrank,det)
   if(nrank<n)det=0
endif
end function det_d


!=============================================================================
function axial3_s(a)result(b)
!=============================================================================
real(sp),dimension(3),intent(IN ):: a
real(sp),dimension(3,3)          :: b
b=0;b(3,2)=a(1);b(1,3)=a(2);b(2,1)=a(3);b(2,3)=-a(1);b(3,1)=-a(2);b(1,2)=-a(3)
end function axial3_s
!=============================================================================
function axial3_d(a)result(b)
!=============================================================================
real(dp),dimension(3),intent(IN ):: a
real(dp),dimension(3,3)          :: b
b=0;b(3,2)=a(1);b(1,3)=a(2);b(2,1)=a(3);b(2,3)=-a(1);b(3,1)=-a(2);b(1,2)=-a(3)
end function axial3_d
!=============================================================================
function axial33_s(b)result(a)
!=============================================================================
real(sp),dimension(3,3),intent(IN ):: b
real(sp),dimension(3)              :: a
a(1)=(b(3,2)-b(2,3))/2; a(2)=(b(1,3)-b(3,1))/2; a(3)=(b(2,1)-b(1,2))/2
end function axial33_s
!=============================================================================
function axial33_d(b)result(a)
!=============================================================================
real(dp),dimension(3,3),intent(IN ):: b
real(dp),dimension(3)              :: a
a(1)=(b(3,2)-b(2,3))/2; a(2)=(b(1,3)-b(3,1))/2; a(3)=(b(2,1)-b(1,2))/2
end function axial33_d

!=============================================================================
function diagn_s(a)result(b)
!=============================================================================
real(sp),dimension(:),intent(IN )  :: a
real(sp),dimension(size(a),size(a)):: b
integer                            :: n,i
n=size(a)
b=0; do i=1,n; b(i,i)=a(i); enddo
end function diagn_s
!=============================================================================
function diagn_d(a)result(b)
!=============================================================================
real(dp),dimension(:),intent(IN )  :: a
real(dp),dimension(size(a),size(a)):: b
integer                            :: n,i
n=size(a)
b=0; do i=1,n; b(i,i)=a(i); enddo
end function diagn_d
!=============================================================================
function diagnn_s(b)result(a)
!=============================================================================
real(sp),dimension(:,:),intent(IN ):: b
real(sp),dimension(size(b,1))      :: a
integer                            :: n,i
n=size(b,1)
do i=1,n; a(i)=b(i,i); enddo
end function diagnn_s
!=============================================================================
function diagnn_d(b)result(a)
!=============================================================================
real(dp),dimension(:,:),intent(IN ):: b
real(dp),dimension(size(b,1))      :: a
integer                            :: n,i
n=size(b,1)
do i=1,n; a(i)=b(i,i); enddo
end function diagnn_d

!=============================================================================
function trace_s(b)result(s)
!=============================================================================
real(sp),dimension(:,:),intent(IN ):: b
real(sp)                           :: s
s=sum(diag(b))
end function trace_s
!=============================================================================
function trace_d(b)result(s)
!=============================================================================
real(dp),dimension(:,:),intent(IN ):: b
real(dp)                           :: s
s=sum(diag(b))
end function trace_d

!=============================================================================
function identity_i(n)result(a)
!=============================================================================
integer,intent(IN )   :: n
integer,dimension(n,n):: a
integer               :: i
a=0; do i=1,n; a(i,i)=1; enddo
end function identity_i
!=============================================================================
function identity3_i()result(a)
!=============================================================================
integer,dimension(3,3):: a
integer               :: i
a=0; do i=1,3; a(i,i)=1; enddo
end function identity3_i

!=============================================================================
function huarea_s(sa,sb)result(area)
!=============================================================================
implicit none
real(sp),intent(IN ):: sa,sb
real(sp)            :: area
real(sp)            :: ca,cb
!-----------------------------------------------------------------------------
ca=sqrt(1-sa**2)
cb=sqrt(1-sb**2)
area=asin(sa*sb/(1+ca*cb))
end function huarea_s

!=============================================================================
function huarea_d(sa,sb)result(area)
!=============================================================================
implicit none
real(dp),intent(IN ):: sa,sb
real(dp)            :: area
real(dp)            :: ca,cb
!-----------------------------------------------------------------------------
ca=sqrt(1-sa**2)
cb=sqrt(1-sb**2)
area=asin(sa*sb/(1+ca*cb))
end function huarea_d


!=============================================================================
function sarea_s(v1,v2,v3)result(area)
!=============================================================================
! Compute the area of the spherical triangle, {v1,v2,v3}, measured in the
! right-handed sense, by dropping a perpendicular to u0 on the longest side so
! that the area becomes the sum of areas of the two simpler right-angled
! triangles.
!=============================================================================
real(sp),dimension(3),intent(IN ):: v1,v2,v3
real(sp)                         :: area
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
real(sp)                         :: s123,a1,a2,b,d1,d2,d3
real(sp),dimension(3)            :: u0,u1,u2,u3,x,y
!=============================================================================
area=0
u1=normalized(v1); u2=normalized(v2); u3=normalized(v3)
s123=triple_product(u1,u2,u3)
if(s123==0)return

d1=dot_product(u3-u2,u3-u2)
d2=dot_product(u1-u3,u1-u3)
d3=dot_product(u2-u1,u2-u1)

! Triangle that is not degenerate. Cyclically permute, so side 3 is longest:
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
y=normalized( cross_product(u1,u2) )
b=dot_product(y,u3)
u0=normalized( u3-y*b )
x=cross_product(y,u0) 
a1=-dot_product(x,u1-u0); a2= dot_product(x,u2-u0)
area=huarea(a1,b)+huarea(a2,b)

contains
!-----------------------------------------------------------------------------
   subroutine cyclic(u1,u2,u3,d1,d2,d3)
!-----------------------------------------------------------------------------
   real(sp),dimension(3),intent(INOUT):: u1,u2,u3
   real(sp),             intent(INOUT):: d1,d2,d3
   real(sp),dimension(3)              :: ut
   real(sp)                           :: dt
   dt=d1; d1=d2; d2=d3; d3=dt
   ut=u1; u1=u2; u2=u3; u3=ut
   end subroutine cyclic
end function sarea_s

!=============================================================================
function sarea_d(v1,v2,v3)result(area)
!=============================================================================
real(dp),dimension(3),intent(IN ):: v1,v2,v3
real(dp)                         :: area
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
real(dp)                         :: s123,a1,a2,b,d1,d2,d3
real(dp),dimension(3)            :: u0,u1,u2,u3,x,y
!=============================================================================
area=0
u1=normalized(v1); u2=normalized(v2); u3=normalized(v3)
s123=triple_product(u1,u2,u3)
if(s123==0)return

d1=dot_product(u3-u2,u3-u2)
d2=dot_product(u1-u3,u1-u3)
d3=dot_product(u2-u1,u2-u1)

! Triangle that is not degenerate. Cyclically permute, so side 3 is longest:
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
if(d3<d1 .or. d3<d2)call cyclic(u1,u2,u3,d1,d2,d3)
y=normalized( cross_product(u1,u2) )
b=dot_product(y,u3)
u0=normalized( u3-y*b )
x=cross_product(y,u0) 
a1=-dot_product(x,u1-u0); a2= dot_product(x,u2-u0)
area=huarea(a1,b)+huarea(a2,b)

contains
!-----------------------------------------------------------------------------
   subroutine cyclic(u1,u2,u3,d1,d2,d3)
!-----------------------------------------------------------------------------
   real(dp),dimension(3),intent(INOUT):: u1,u2,u3
   real(dp),             intent(INOUT):: d1,d2,d3
   real(dp),dimension(3)              :: ut
   real(dp)                           :: dt
   dt=d1; d1=d2; d2=d3; d3=dt
   ut=u1; u1=u2; u2=u3; u3=ut
   end subroutine cyclic
end function sarea_d

!=============================================================================
subroutine gram_s(as,b,nrank,det)
!=============================================================================
real(sp),dimension(:,:),intent(IN )      :: as
real(sp),dimension(:,:),intent(OUT)      :: b
integer,                intent(OUT)      :: nrank
real(sp),               intent(OUT)      :: det
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(sp),parameter                       :: crit=1.e-5_sp
real(sp),dimension(size(as,1),size(as,2)):: a
real(sp),dimension(size(as,2),size(as,1)):: ab
real(sp),dimension(size(as,1))           :: tv,w
real(sp)                                 :: val,s,vcrit
integer                                  :: i,j,k,l,m,n
integer,dimension(2)                     :: ii
!-----------------------------------------------------------------------------
n=size(as,1)
m=size(as,2)
if(n/=size(b,1) .or. n/=size(b,2))stop 'In gram; incompatible dimensions'
a=as
b=identity(n)
det=1
val=maxval(abs(a))
if(val==0)then
   nrank=0
   return
endif
vcrit=val*crit
nrank=min(n,m)
do k=1,n
   if(k>nrank)exit
   ab(k:m,k:n)=matmul( transpose(a(:,k:m)),b(:,k:n) )
   ii =maxloc( abs( ab(k:m,k:n)) )+k-1
   val=maxval( abs( ab(k:m,k:n)) )
   if(val<=vcrit)then
      nrank=k-1
      exit
   endif
   i=ii(1)
   j=ii(2)
   tv=b(:,j)
   b(:,j)=-b(:,k)
   b(:,k)=tv
   tv=a(:,i)
   a(:,i)=-a(:,k)
   a(:,k)=tv
   w(k:n)=matmul( transpose(b(:,k:n)),tv )
   b(:,k)=matmul(b(:,k:n),w(k:n) )
   s=dot_product(b(:,k),b(:,k))
   s=sqrt(s)
   if(w(k)<0)s=-s
   det=det*s
   b(:,k)=b(:,k)/s
   do l=k,n
      do j=l+1,n
         s=dot_product(b(:,l),b(:,j))
         b(:,j)=normalized( b(:,j)-b(:,l)*s )
      enddo
   enddo
enddo
end subroutine gram_s
   
!=============================================================================
subroutine gram_d(as,b,nrank,det)
!=============================================================================
real(dp),dimension(:,:),intent(IN )      :: as
real(dp),dimension(:,:),intent(OUT)      :: b
integer,                intent(OUT)      :: nrank
real(dp),               intent(OUT)      :: det
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(dp),parameter                       :: crit=1.e-9_dp
real(dp),dimension(size(as,1),size(as,2)):: a
real(dp),dimension(size(as,2),size(as,1)):: ab
real(dp),dimension(size(as,1))           :: tv,w
real(dp)                                 :: val,s,vcrit
integer                                  :: i,j,k,l,m,n
integer,dimension(2)                     :: ii
!-----------------------------------------------------------------------------
n=size(as,1)
m=size(as,2)
if(n/=size(b,1) .or. n/=size(b,2))stop 'In gram; incompatible dimensions'
a=as
b=identity(n)
det=1
val=maxval(abs(a))
if(val==0)then
   nrank=0
   return
endif
vcrit=val*crit
nrank=min(n,m)
do k=1,n
   if(k>nrank)exit
   ab(k:m,k:n)=matmul( transpose(a(:,k:m)),b(:,k:n) )
   ii =maxloc( abs( ab(k:m,k:n)) )+k-1
   val=maxval( abs( ab(k:m,k:n)) )
   if(val<=vcrit)then
      nrank=k-1
      exit
   endif
   i=ii(1)
   j=ii(2)
   tv=b(:,j)
   b(:,j)=-b(:,k)
   b(:,k)=tv
   tv=a(:,i)
   a(:,i)=-a(:,k)
   a(:,k)=tv
   w(k:n)=matmul( transpose(b(:,k:n)),tv )
   b(:,k)=matmul(b(:,k:n),w(k:n) )
   s=dot_product(b(:,k),b(:,k))
   s=sqrt(s)
   if(w(k)<0)s=-s
   det=det*s
   b(:,k)=b(:,k)/s
   do l=k,n
      do j=l+1,n
         s=dot_product(b(:,l),b(:,j))
         b(:,j)=normalized( b(:,j)-b(:,l)*s )
      enddo
   enddo
enddo
end subroutine gram_d
   
!=============================================================================
subroutine plaingram_s(b,nrank)
!=============================================================================
! A "plain" (unpivoted) version of Gram-Schmidt, for square matrices only.
real(sp),dimension(:,:),intent(INOUT)    :: b
integer,                intent(  OUT)    :: nrank
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(sp),parameter                       :: crit=1.e-5_sp
real(sp)                                 :: val,vcrit
integer                                  :: j,k,n
!-----------------------------------------------------------------------------
n=size(b,1); if(n/=size(b,2))stop 'In gram; matrix needs to be square'
val=maxval(abs(b))
nrank=0
if(val==0)then
   b=0
   return
endif
vcrit=val*crit
do k=1,n
   val=sqrt(dot_product(b(:,k),b(:,k)))
   if(val<=vcrit)then
      b(:,k:n)=0
      return
   endif
   b(:,k)=b(:,k)/val
   nrank=k
   do j=k+1,n
      b(:,j)=b(:,j)-b(:,k)*dot_product(b(:,k),b(:,j))
   enddo
enddo
end subroutine plaingram_s

!=============================================================================
subroutine plaingram_d(b,nrank)
!=============================================================================
! A "plain" (unpivoted) version of Gram-Schmidt, for square matrices only.
real(dp),dimension(:,:),intent(INOUT)    :: b
integer,                intent(  OUT)    :: nrank
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
real(dp),parameter                       :: crit=1.e-9_dp
real(dp)                                 :: val,vcrit
integer                                  :: j,k,n
!-----------------------------------------------------------------------------
n=size(b,1); if(n/=size(b,2))stop 'In gram; matrix needs to be square'
val=maxval(abs(b))
nrank=0
if(val==0)then
   b=0
   return
endif
vcrit=val*crit
do k=1,n
   val=sqrt(dot_product(b(:,k),b(:,k)))
   if(val<=vcrit)then
      b(:,k:n)=0
      return
   endif
   b(:,k)=b(:,k)/val
   nrank=k
   do j=k+1,n
      b(:,j)=b(:,j)-b(:,k)*dot_product(b(:,k),b(:,j))
   enddo
enddo
end subroutine plaingram_d

end module peuc
