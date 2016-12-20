      subroutine dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
!     .. scalar arguments ..
!     double precision alpha,beta
      real * 8 alpha,beta
      integer   k,lda,ldb,ldc,m,n
      character transa,transb
!     ..
!     .. array arguments ..
!     double precision a(lda,*),b(ldb,*),c(ldc,*)
      real * 8 a(lda,*),b(ldb,*),c(ldc,*)
!     ..
!
!  purpose
!  =======
!
!  dgemm  performs one of the matrix-matrix operations
!
!     c := alpha*op( a )*op( b ) + beta*c,
!
!  where  op( x ) is one of
!
!     op( x ) = x   or   op( x ) = x**t,
!
!  alpha and beta are scalars, and a, b and c are matrices, with op( a )
!  an m by k matrix,  op( b )  a  k by n matrix and  c an m by n matrix.
!
!  arguments
!  ==========
!
!  transa - character*1.
!           on entry, transa specifies the form of op( a ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'n' or 'n',  op( a ) = a.
!
!              transa = 't' or 't',  op( a ) = a**t.
!
!              transa = 'c' or 'c',  op( a ) = a**t.
!
!           unchanged on exit.
!
!  transb - character*1.
!           on entry, transb specifies the form of op( b ) to be used in
!           the matrix multiplication as follows:
!
!              transb = 'n' or 'n',  op( b ) = b.
!
!              transb = 't' or 't',  op( b ) = b**t.
!
!              transb = 'c' or 'c',  op( b ) = b**t.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry,  m  specifies  the number  of rows  of the  matrix
!           op( a )  and of the  matrix  c.  m  must  be at least  zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry,  n  specifies the number  of columns of the matrix
!           op( b ) and the number of columns of the matrix c. n must be
!           at least zero.
!           unchanged on exit.
!
!  k      - integer.
!           on entry,  k  specifies  the number of columns of the matrix
!           op( a ) and the number of rows of the matrix op( b ). k must
!           be at least  zero.
!           unchanged on exit.
!
!  alpha  - double precision.
!           on entry, alpha specifies the scalar alpha.
!           unchanged on exit.
!
!  a      - double precision array of dimension ( lda, ka ), where ka is
!           k  when  transa = 'n' or 'n',  and is  m  otherwise.
!           before entry with  transa = 'n' or 'n',  the leading  m by k
!           part of the array  a  must contain the matrix  a,  otherwise
!           the leading  k by m  part of the array  a  must contain  the
!           matrix a.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. when  transa = 'n' or 'n' then
!           lda must be at least  max( 1, m ), otherwise  lda must be at
!           least  max( 1, k ).
!           unchanged on exit.
!
!  b      - double precision array of dimension ( ldb, kb ), where kb is
!           n  when  transb = 'n' or 'n',  and is  k  otherwise.
!           before entry with  transb = 'n' or 'n',  the leading  k by n
!           part of the array  b  must contain the matrix  b,  otherwise
!           the leading  n by k  part of the array  b  must contain  the
!           matrix b.
!           unchanged on exit.
!
!  ldb    - integer.
!           on entry, ldb specifies the first dimension of b as declared
!           in the calling (sub) program. when  transb = 'n' or 'n' then
!           ldb must be at least  max( 1, k ), otherwise  ldb must be at
!           least  max( 1, n ).
!           unchanged on exit.
!
!  beta   - double precision.
!           on entry,  beta  specifies the scalar  beta.  when  beta  is
!           supplied as zero then c need not be set on input.
!           unchanged on exit.
!
!  c      - double precision array of dimension ( ldc, n ).
!           before entry, the leading  m by n  part of the array  c must
!           contain the matrix  c,  except when  beta  is zero, in which
!           case c need not be set on entry.
!           on exit, the array  c  is overwritten by the  m by n  matrix
!           ( alpha*op( a )*op( b ) + beta*c ).
!
!  ldc    - integer.
!           on entry, ldc specifies the first dimension of c as declared
!           in  the  calling  (sub)  program.   ldc  must  be  at  least
!           max( 1, m ).
!           unchanged on exit.
!
!  further details
!  ===============
!
!  level 3 blas routine.
!
!  -- written on 8-february-1989.
!     jack dongarra, argonne national laboratory.
!     iain duff, aere harwell.
!     jeremy du croz, numerical algorithms group ltd.
!     sven hammarling, numerical algorithms group ltd.
!
!  =====================================================================
!
!     .. external functions ..
      logical lsame
      external lsame
!     ..
!     .. external subroutines ..
      external xerbla
!     ..
!     .. intrinsic functions ..
      intrinsic max
!     ..
!     .. local scalars ..
      double precision temp
      integer i,info,j,l,ncola,nrowa,nrowb
      logical nota,notb
!     ..
!     .. parameters ..
      double precision one,zero
      parameter (one=1.0d+0,zero=0.0d+0)
!     ..
!
!     set  nota  and  notb  as  true if  a  and  b  respectively are not
!     transposed and set  nrowa, ncola and  nrowb  as the number of rows
!     and  columns of  a  and the  number of  rows  of  b  respectively.
!
      nota = lsame(transa,'n')
      notb = lsame(transb,'n')
      if (nota) then
         nrowa = m
         ncola = k
      else
         nrowa = k
         ncola = m
      end if
      if (notb) then
         nrowb = k
      else
         nrowb = n
      end if
!
!     test the input parameters.
!
      info = 0
      if ((.not.nota) .and. (.not.lsame(transa,'c')) .and.
     &    (.not.lsame(transa,'t'))) then
          info = 1
      else if ((.not.notb) .and. (.not.lsame(transb,'c')) .and.
     &         (.not.lsame(transb,'t'))) then
          info = 2
      else if (m < 0) then
          info = 3
      else if (n < 0) then
          info = 4
      else if (k < 0) then
          info = 5
      else if (lda < max(1,nrowa)) then
          info = 8
      else if (ldb < max(1,nrowb)) then
          info = 10
      else if (ldc < max(1,m)) then
          info = 13
      end if
      if (info /= 0) then
          call xerbla('dgemm ',info)
          return
      end if
!
!     quick return if possible.
!
      if ((m == 0) .or. (n == 0) .or.
     &    (((alpha == zero).or. (k == 0)).and. (beta == one))) return
!
!     and if  alpha.eq.zero.
!
      if (alpha == zero) then
        if (beta == zero) then
          do j = 1,n
            do i = 1,m
              c(i,j) = zero
            enddo
          enddo
        else
          do j = 1,n
            do i = 1,m
              c(i,j) = beta*c(i,j)
            enddo
          enddo
        end if
        return
      end if
!
!     start the operations.
!
      if (notb) then
        if (nota) then
!                                     form  c := alpha*a*b + beta*c.
          if (beta == zero) then
            do j = 1,n
              do i = 1,m
                c(i,j) = zero
              enddo
            enddo
          else if (beta /= one) then
            do j = 1,n
              do i = 1,m
                c(i,j) = beta*c(i,j)
              enddo
            enddo
          end if
          do j = 1,n
            do l = 1,k
              temp = alpha*b(l,j)
              do i = 1,m
                c(i,j) = c(i,j) + temp*a(i,l)
              enddo
            enddo
          enddo
        else
!                                     form  c := alpha*a**t*b + beta*c
          if (beta == zero) then
            do j = 1,n
              do i = 1,m
                temp = zero
                do l = 1,k
                  temp = temp + a(l,i)*b(l,j)
                enddo
                c(i,j) = alpha*temp
              enddo
            enddo
          else
            do j = 1,n
              do i = 1,m
                temp = zero
                do l = 1,k
                  temp = temp + a(l,i)*b(l,j)
                enddo
                c(i,j) = alpha*temp + beta*c(i,j)
              enddo
            enddo
          end if
    
        end if
      else
        if (nota) then
!                                     form  c := alpha*a*b**t + beta*c
          if (beta == zero) then
            do j = 1,n
              do i = 1,m
                c(i,j) = zero
              enddo
            enddo
          else if (beta /= one) then
            do j = 1,n
              do i = 1,m
                c(i,j) = beta*c(i,j)
              enddo
            enddo
          end if
          do j = 1,n
            do l = 1,k
              temp = alpha*b(j,l)
              do i = 1,m
                c(i,j) = c(i,j) + temp*a(i,l)
              enddo
            enddo
          enddo
        else
!                                     form  c := alpha*a**t*b**t + beta*c
          if (beta == zero) then
            do j = 1,n
              do i = 1,m
                temp = zero
                do l = 1,k
                  temp = temp + a(l,i)*b(j,l)
                enddo
                c(i,j) = alpha*temp
              enddo
            enddo
          else
            do j = 1,n
              do i = 1,m
                temp = zero
                do l = 1,k
                  temp = temp + a(l,i)*b(j,l)
                enddo
                c(i,j) = alpha*temp + beta*c(i,j)
              enddo
            enddo
          end if
        end if
      end if
!
      return
!
!     end of dgemm .
!
      end
      subroutine sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
!     .. scalar arguments ..
      real * 4 alpha,beta
      integer k,lda,ldb,ldc,m,n
      character transa,transb
!     ..
!     .. array arguments ..
      real * 4 a(lda,*),b(ldb,*),c(ldc,*)
!     ..
!
!  purpose
!  =======
!
!  sgemm  performs one of the matrix-matrix operations
!
!     c := alpha*op( a )*op( b ) + beta*c,
!
!  where  op( x ) is one of
!
!     op( x ) = x   or   op( x ) = x**t,
!
!  alpha and beta are scalars, and a, b and c are matrices, with op( a )
!  an m by k matrix,  op( b )  a  k by n matrix and  c an m by n matrix.
!
!  arguments
!  ==========
!
!  transa - character*1.
!           on entry, transa specifies the form of op( a ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'n' or 'n',  op( a ) = a.
!
!              transa = 't' or 't',  op( a ) = a**t.
!
!              transa = 'c' or 'c',  op( a ) = a**t.
!
!           unchanged on exit.
!
!  transb - character*1.
!           on entry, transb specifies the form of op( b ) to be used in
!           the matrix multiplication as follows:
!
!              transb = 'n' or 'n',  op( b ) = b.
!
!              transb = 't' or 't',  op( b ) = b**t.
!
!              transb = 'c' or 'c',  op( b ) = b**t.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry,  m  specifies  the number  of rows  of the  matrix
!           op( a )  and of the  matrix  c.  m  must  be at least  zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry,  n  specifies the number  of columns of the matrix
!           op( b ) and the number of columns of the matrix c. n must be
!           at least zero.
!           unchanged on exit.
!
!  k      - integer.
!           on entry,  k  specifies  the number of columns of the matrix
!           op( a ) and the number of rows of the matrix op( b ). k must
!           be at least  zero.
!           unchanged on exit.
!
!  alpha  - real            .
!           on entry, alpha specifies the scalar alpha.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, ka ), where ka is
!           k  when  transa = 'n' or 'n',  and is  m  otherwise.
!           before entry with  transa = 'n' or 'n',  the leading  m by k
!           part of the array  a  must contain the matrix  a,  otherwise
!           the leading  k by m  part of the array  a  must contain  the
!           matrix a.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. when  transa = 'n' or 'n' then
!           lda must be at least  max( 1, m ), otherwise  lda must be at
!           least  max( 1, k ).
!           unchanged on exit.
!
!  b      - real             array of dimension ( ldb, kb ), where kb is
!           n  when  transb = 'n' or 'n',  and is  k  otherwise.
!           before entry with  transb = 'n' or 'n',  the leading  k by n
!           part of the array  b  must contain the matrix  b,  otherwise
!           the leading  n by k  part of the array  b  must contain  the
!           matrix b.
!           unchanged on exit.
!
!  ldb    - integer.
!           on entry, ldb specifies the first dimension of b as declared
!           in the calling (sub) program. when  transb = 'n' or 'n' then
!           ldb must be at least  max( 1, k ), otherwise  ldb must be at
!           least  max( 1, n ).
!           unchanged on exit.
!
!  beta   - real            .
!           on entry,  beta  specifies the scalar  beta.  when  beta  is
!           supplied as zero then c need not be set on input.
!           unchanged on exit.
!
!  c      - real             array of dimension ( ldc, n ).
!           before entry, the leading  m by n  part of the array  c must
!           contain the matrix  c,  except when  beta  is zero, in which
!           case c need not be set on entry.
!           on exit, the array  c  is overwritten by the  m by n  matrix
!           ( alpha*op( a )*op( b ) + beta*c ).
!
!  ldc    - integer.
!           on entry, ldc specifies the first dimension of c as declared
!           in  the  calling  (sub)  program.   ldc  must  be  at  least
!           max( 1, m ).
!           unchanged on exit.
!
!  further details
!  ===============
!
!  level 3 blas routine.
!
!  -- written on 8-february-1989.
!     jack dongarra, argonne national laboratory.
!     iain duff, aere harwell.
!     jeremy du croz, numerical algorithms group ltd.
!     sven hammarling, numerical algorithms group ltd.
!
!  =====================================================================
!
!     .. external functions ..
      logical lsame
      external lsame
!     ..
!     .. external subroutines ..
      external xerbla
!     ..
!     .. intrinsic functions ..
      intrinsic max
!     ..
!     .. local scalars ..
      real temp
      integer i,info,j,l,ncola,nrowa,nrowb
      logical nota,notb
!     ..
!     .. parameters ..
      real one,zero
      parameter (one=1.0e+0,zero=0.0e+0)
!     ..
!
!     set  nota  and  notb  as  true if  a  and  b  respectively are not
!     transposed and set  nrowa, ncola and  nrowb  as the number of rows
!     and  columns of  a  and the  number of  rows  of  b  respectively.
!
      nota = lsame(transa,'n')
      notb = lsame(transb,'n')
      if (nota) then
          nrowa = m
          ncola = k
      else
          nrowa = k
          ncola = m
      end if
      if (notb) then
          nrowb = k
      else
          nrowb = n
      end if
!
!     test the input parameters.
!
      info = 0
      if ((.not.nota) .and. (.not.lsame(transa,'c')) .and.
     +    (.not.lsame(transa,'t'))) then
          info = 1
      else if ((.not.notb) .and. (.not.lsame(transb,'c')) .and.
     +         (.not.lsame(transb,'t'))) then
          info = 2
      else if (m.lt.0) then
          info = 3
      else if (n.lt.0) then
          info = 4
      else if (k.lt.0) then
          info = 5
      else if (lda.lt.max(1,nrowa)) then
          info = 8
      else if (ldb.lt.max(1,nrowb)) then
          info = 10
      else if (ldc.lt.max(1,m)) then
          info = 13
      end if
      if (info.ne.0) then
          call xerbla('sgemm ',info)
          return
      end if
!
!     quick return if possible.
!
      if ((m.eq.0) .or. (n.eq.0) .or.
     +    (((alpha.eq.zero).or. (k.eq.0)).and. (beta.eq.one))) return
!
!     and if  alpha.eq.zero.
!
      if (alpha.eq.zero) then
          if (beta.eq.zero) then
              do 20 j = 1,n
                  do 10 i = 1,m
                      c(i,j) = zero
   10             continue
   20         continue
          else
              do 40 j = 1,n
                  do 30 i = 1,m
                      c(i,j) = beta*c(i,j)
   30             continue
   40         continue
          end if
          return
      end if
!
!     start the operations.
!
      if (notb) then
          if (nota) then
!
!           form  c := alpha*a*b + beta*c.
!
              do 90 j = 1,n
                  if (beta.eq.zero) then
                      do 50 i = 1,m
                          c(i,j) = zero
   50                 continue
                  else if (beta.ne.one) then
                      do 60 i = 1,m
                          c(i,j) = beta*c(i,j)
   60                 continue
                  end if
                  do 80 l = 1,k
                      if (b(l,j).ne.zero) then
                          temp = alpha*b(l,j)
                          do 70 i = 1,m
                              c(i,j) = c(i,j) + temp*a(i,l)
   70                     continue
                      end if
   80             continue
   90         continue
          else
!
!           form  c := alpha*a**t*b + beta*c
!
              do 120 j = 1,n
                  do 110 i = 1,m
                      temp = zero
                      do 100 l = 1,k
                          temp = temp + a(l,i)*b(l,j)
  100                 continue
                      if (beta.eq.zero) then
                          c(i,j) = alpha*temp
                      else
                          c(i,j) = alpha*temp + beta*c(i,j)
                      end if
  110             continue
  120         continue
          end if
      else
          if (nota) then
!
!           form  c := alpha*a*b**t + beta*c
!
              do 170 j = 1,n
                  if (beta.eq.zero) then
                      do 130 i = 1,m
                          c(i,j) = zero
  130                 continue
                  else if (beta.ne.one) then
                      do 140 i = 1,m
                          c(i,j) = beta*c(i,j)
  140                 continue
                  end if
                  do 160 l = 1,k
                      if (b(j,l).ne.zero) then
                          temp = alpha*b(j,l)
                          do 150 i = 1,m
                              c(i,j) = c(i,j) + temp*a(i,l)
  150                     continue
                      end if
  160             continue
  170         continue
          else
!
!           form  c := alpha*a**t*b**t + beta*c
!
              do 200 j = 1,n
                  do 190 i = 1,m
                      temp = zero
                      do 180 l = 1,k
                          temp = temp + a(l,i)*b(j,l)
  180                 continue
                      if (beta.eq.zero) then
                          c(i,j) = alpha*temp
                      else
                          c(i,j) = alpha*temp + beta*c(i,j)
                      end if
  190             continue
  200         continue
          end if
      end if
!
      return
!
!     end of sgemm .
!
      end
      logical function lsame(ca,cb)
!
!  -- lapack auxiliary routine (version 3.1) --
!     univ. of tennessee, univ. of california berkeley and nag ltd..
!     november 2006
!
!     .. scalar arguments ..
      character ca,cb
!     ..
!
!  purpose
!  =======
!
!  lsame returns .true. if ca is the same letter as cb regardless of
!  case.
!
!  arguments
!  =========
!
!  ca      (input) character*1
!
!  cb      (input) character*1
!          ca and cb specify the single characters to be compared.
!
! =====================================================================
!
!     .. intrinsic functions ..
      intrinsic ichar
!     ..
!     .. local scalars ..
      integer inta,intb,zcode
!     ..
!
!     test if the characters are equal
!
      lsame = ca .eq. cb
      if (lsame) return
!
!     now test for equivalence if both characters are alphabetic.
!
      zcode = ichar('z')
!
!     use 'z' rather than 'a' so that ascii can be detected on prime
!     machines, on which ichar returns a value with bit 8 set.
!     ichar('a') on prime machines returns 193 which is the same as
!     ichar('a') on an ebcdic machine.
!
      inta = ichar(ca)
      intb = ichar(cb)
!
      if (zcode.eq.90 .or. zcode.eq.122) then
!
!        ascii is assumed - zcode is the ascii code of either lower or
!        upper case 'z'.
!
          if (inta.ge.97 .and. inta.le.122) inta = inta - 32
          if (intb.ge.97 .and. intb.le.122) intb = intb - 32
!
      else if (zcode.eq.233 .or. zcode.eq.169) then
!
!        ebcdic is assumed - zcode is the ebcdic code of either lower or
!        upper case 'z'.
!
          if (inta.ge.129 .and. inta.le.137 .or.
     +        inta.ge.145 .and. inta.le.153 .or.
     +        inta.ge.162 .and. inta.le.169) inta = inta + 64
          if (intb.ge.129 .and. intb.le.137 .or.
     +        intb.ge.145 .and. intb.le.153 .or.
     +        intb.ge.162 .and. intb.le.169) intb = intb + 64
!
      else if (zcode.eq.218 .or. zcode.eq.250) then
!
!        ascii is assumed, on prime machines - zcode is the ascii code
!        plus 128 of either lower or upper case 'z'.
!
          if (inta.ge.225 .and. inta.le.250) inta = inta - 32
          if (intb.ge.225 .and. intb.le.250) intb = intb - 32
      end if
      lsame = inta .eq. intb
!
!     return
!
!     end of lsame
!
      end
      subroutine xerbla( srname, info )
!
!  -- lapack auxiliary routine (preliminary version) --
!     univ. of tennessee, univ. of california berkeley and nag ltd..
!     november 2006
!
!     .. scalar arguments ..
      character*(*)      srname
      integer            info
!     ..
!
!  purpose
!  =======
!
!  xerbla  is an error handler for the lapack routines.
!  it is called by an lapack routine if an input parameter has an
!  invalid value.  a message is printed and execution stops.
!
!  installers may consider modifying the stop statement in order to
!  call system-specific exception-handling facilities.
!
!  arguments
!  =========
!
!  srname  (input) character*(*)
!          the name of the routine which called xerbla.
!
!  info    (input) integer
!          the position of the invalid parameter in the parameter list
!          of the calling routine.
!
! =====================================================================
!
!     .. intrinsic functions ..
      intrinsic          len_trim
!     ..
!     .. executable statements ..
!
      write( *, fmt = 9999 )srname( 1:len_trim( srname ) ), info
!
      stop
!
 9999 format( ' ** on entry to ', a, ' parameter number ', i2, ' had ',
     $      'an illegal value' )
!
!     end of xerbla
!
      end
