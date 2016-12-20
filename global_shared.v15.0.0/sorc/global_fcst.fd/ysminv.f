      subroutine iminv (a,n,d,l,m)
cc
      use machine
      implicit none
cc
      integer              i,ij,ik,iz,j,ji,jk,jp,jq,jr,k,ki,kj
      integer              kk,n,nk
cc
      real(kind=kind_evod) biga,d,hold
cc
c
c     ..................................................................
c
c        ................
c
c        purpose
c           invert a matrix
c
c        usage
c           call iminv (a,n,d,l,m)
c
c        description of parameters
c           a - input matrix, destroyed in computation and replaced by
c               resultant inverse.
c           n - order of matrix a
c           d - resultant determinant
c           l - work vector of length n
c           m - work vector of length n
c
c        remarks
c           matrix a must be a general matrix
c
c        .............................................
c           none
c
c        method
c           the standard gauss-jordan method is used. the determinant
c           is also calculated. a determinant of zero indicates that
c           the matrix is singular.
c
c     ..................................................................
c
      real(kind=kind_evod) a(n*n)
cc
      integer              l(n),m(n)
c
c        ...............................................................
c
c        if a double precision version of this routine is desired, the
c        c in column 1 should be removed from the double precision
c        statement which follows.
c
c     double precision a, d, biga, hold
c
c        the c must also be removed from double precision statements
c        appearing in other routines used in conjunction with this
c        routine.
c
c        the double precision version of this sr........ must also
c        contain double precision fortran functions.  abs in statemen
c        10 must be changed to dabs  .
c
c        ...............................................................
c
c        search for largest element
c
cc
      real(kind=kind_evod) cons0,cons1     !constant
cc
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
cc
      d=cons1     !constant
      nk=-n
      do 80 k=1,n
      nk=nk+n
      l(k)=k
      m(k)=k
      kk=nk+k
      biga=a(kk)
      do 20 j=k,n
      iz=n*(j-1)
      do 20 i=k,n
      ij=iz+i
c  10 if (dabs(biga)-dabs(a(ij))) 15,20,20
   10 if(abs(biga)-abs(a(ij))) 15,20,20
   15 biga=a(ij)
      l(k)=i
      m(k)=j
   20 continue
c
c        interchange rows
c
      j=l(k)
      if(j-k) 35,35,25
   25 ki=k-n
      do 30 i=1,n
      ki=ki+n
      hold=-a(ki)
      ji=ki-k+j
      a(ki)=a(ji)
   30 a(ji) =hold
c
c        interchange columns
c
   35 i=m(k)
      if(i-k) 45,45,38
   38 jp=n*(i-1)
      do 40 j=1,n
      jk=nk+j
      ji=jp+j
      hold=-a(jk)
      a(jk)=a(ji)
   40 a(ji) =hold
c
c        divide column by minus pivot (value of pivot element is
c        contained in biga)
c
   45 if(biga) 48,46,48
   46 d=cons0     !constant
      return
   48 do 55 i=1,n
      if(i-k) 50,55,50
   50 ik=nk+i
      a(ik)=a(ik)/(-biga)
   55 continue
c
c        reduce matrix
c
      do 65 i=1,n
      ik=nk+i
      ij=i-n
      do 65 j=1,n
      ij=ij+n
      if(i-k) 60,65,60
   60 if(j-k) 62,65,62
   62 kj=ij-i+k
      a(ij)=a(ik)*a(kj)+a(ij)
   65 continue
c
c        divide row by pivot
c
      kj=k-n
      do 75 j=1,n
      kj=kj+n
      if(j-k) 70,75,70
   70 a(kj)=a(kj)/biga
   75 continue
c
c        product of pivots
c
      d=d*biga
c
c        replace pivot by reciprocal
c
      a(kk)=cons1/biga     !constant
   80 continue
c
c        final row and column interchange
c
      k=n
  100 k=(k-1)
      if(k) 150,150,105
  105 i=l(k)
      if(i-k) 120,120,108
  108 jq=n*(k-1)
      jr=n*(i-1)
      do 110 j=1,n
      jk=jq+j
      hold=a(jk)
      ji=jr+j
      a(jk)=-a(ji)
  110 a(ji) =hold
  120 j=m(k)
      if(j-k) 100,100,125
  125 ki=k-n
      do 130 i=1,n
      ki=ki+n
      hold=a(ki)
      ji=ki-k+j
      a(ki)=-a(ji)
  130 a(ji) =hold
      go to 100
  150 return
      end
