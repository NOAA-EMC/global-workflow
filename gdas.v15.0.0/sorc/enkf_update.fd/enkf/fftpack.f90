subroutine fftpack_rfftb ( n, r, wsave )
!
!*******************************************************************************
!
!! RFFTB computes a real periodic sequence from its Fourier coefficients.
!
!
!  Discussion:
!
!    This process is sometimes called Fourier synthesis.
!
!    The transform is unnormalized.  A call to RFFTF followed by a call to
!    RFFTB will multiply the input sequence by N.
!
!    If N is even, the transform is defined by:
!
!      R_out(I) = R_in(1) + (-1)**(I-1) * R_in(N) + sum ( 2 <= K <= N/2 )
!
!        + 2 * R_in(2*K-2) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!        - 2 * R_in(2*K-1) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!    If N is odd, the transform is defined by:
!
!      R_out(I) = R_in(1) + sum ( 2 <= K <= (N+1)/2 )
!
!        + 2 * R_in(2*K-2) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!        - 2 * R_in(2*K-1) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Clever Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    P N Swarztrauber, 
!    Vectorizing the FFT's, 
!    in Parallel Computations,
!    G. Rodrigue, editor, 
!    Academic Press, 1982, pages 51-83.
!
!    B L Buzbee, 
!    The SLATEC Common Math Library, 
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer N, the length of the array to be transformed.  The 
!    method is more efficient when N is the product of small primes.
!
!    Input/output, real R(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real WSAVE(2*N+15), a work array.  The WSAVE array must be
!    initialized by calling RFFTI.  A different WSAVE array must be used 
!    for each different value of N.
!
  implicit none
!
  integer n
!
  real r(n)
  real wsave(2*n+15)
  integer ifac(15)
!
  if ( n <= 1 ) then
    return
  end if

  ifac = wsave(2*n+1:2*n+15)
  call fftpack_rfftb1 ( n, r, wsave(1), wsave(n+1), ifac)

  return
end
subroutine fftpack_rfftb1 ( n, c, ch, wa, ifac )
!
!*******************************************************************************
!
!! RFFTB1 is a lower level routine used by RFFTB.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the length of the array to be transformed.  
!
!    Input/output, real C(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real CH(N).
!
!    Input, real WA(N).
!
!    Input, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none
!
  integer n
!
  real c(n)
  real ch(n)
  integer idl1
  integer ido
  integer ifac(15)
  integer ip
  integer iw
  integer ix2
  integer ix3
  integer ix4
  integer k1
  integer l1
  integer l2
  integer na
  integer nf
  real wa(n)
!
  nf = ifac(2)
  na = 0
  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    l2 = ip * l1
    ido = n / l2
    idl1 = ido * l1

    if ( ip == 4 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido

      if ( na == 0 ) then
        call fftpack_radb4 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call fftpack_radb4 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

      na = 1 - na

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call fftpack_radb2 ( ido, l1, c, ch, wa(iw) )
      else
        call fftpack_radb2 ( ido, l1, ch, c, wa(iw) )
      end if

      na = 1 - na

    else if ( ip == 3 ) then

      ix2 = iw + ido

      if ( na == 0 ) then
        call fftpack_radb3 ( ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call fftpack_radb3 ( ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

      na = 1 - na

    else if ( ip == 5 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido
      ix4 = ix3 + ido

      if ( na == 0 ) then
        call fftpack_radb5 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call fftpack_radb5 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

      na = 1 - na

    else

      if ( na == 0 ) then
        call fftpack_radbg ( ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
      else
        call fftpack_radbg ( ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
      end if

      if ( ido == 1 ) then
        na = 1 - na
      end if

    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * ido

  end do

  if ( na /= 0 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine fftpack_rfftf ( n, r, wsave )
!
!*******************************************************************************
!
!! RFFTF computes the Fourier coefficients of a real periodic sequence.
!
!
!  Discussion:
!
!    This process is sometimes called Fourier analysis.
! 
!    The transform is unnormalized.  A call to RFFTF followed by a call 
!    to RFFTB will multiply the input sequence by N.
!
!    The transform is defined by:
!
!      R_out(1) = sum ( 1 <= I <= N ) R_in(I)
!
!    Letting L = (N+1)/2, then for K = 2,...,L
!
!      R_out(2*K-2) = sum ( 1 <= I <= N )
!
!        R_in(I) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!      R_out(2*K-1) = sum ( 1 <= I <= N )
!
!        -R_in(I) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!    And, if N is even, then:
!
!      R_out(N) = sum ( 1 <= I <= N ) (-1)**(I-1) * R_in(I)
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Clever Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    P N Swarztrauber, 
!    Vectorizing the FFT's, 
!    in Parallel Computations,
!    G. Rodrigue, editor, 
!    Academic Press, 1982, pages 51-83.
!
!    B L Buzbee, 
!    The SLATEC Common Math Library, 
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer N, the length of the array to be transformed.  The 
!    method is more efficient when N is the product of small primes.
!
!    Input/output, real R(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real WSAVE(2*N+15), a work array.  The WSAVE array must be
!    initialized by calling RFFTI.  A different WSAVE array must be used 
!    for each different value of N.  
!
  implicit none
!
  integer n
!
  real r(n)
  real wsave(2*n+15)
  integer ifac(15)
!
  if ( n <= 1 ) then
    return
  end if

  ifac = wsave(2*n+1:2*n+15)
  call fftpack_rfftf1 ( n, r, wsave(1), wsave(n+1), ifac)

  return
end
subroutine fftpack_rfftf1 ( n, c, ch, wa, ifac )
!
!*******************************************************************************
!
!! RFFTF1 is a lower level routine used by RFFTF and RSINT.
!
!
!  Modified:
!
!    12 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the length of the array to be transformed.  
!
!    Input/output, real C(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real CH(N).
!
!    Input, real WA(N).
!
!    Input, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none
!
  integer n
!
  real c(n)
  real ch(n)
  integer idl1
  integer ido
  integer ifac(15)
  integer ip
  integer iw
  integer ix2
  integer ix3
  integer ix4
  integer k1
  integer kh
  integer l1
  integer l2
  integer na
  integer nf
  real wa(n)
!
  nf = ifac(2)
  na = 1
  l2 = n
  iw = n

  do k1 = 1, nf

    kh = nf - k1
    ip = ifac(kh+3)
    l1 = l2 / ip
    ido = n / l2
    idl1 = ido * l1
    iw = iw - ( ip - 1 ) * ido
    na = 1 - na

    if ( ip == 4 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido

      if ( na == 0 ) then
        call fftpack_radf4 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call fftpack_radf4 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call fftpack_radf2 ( ido, l1, c, ch, wa(iw) )
      else
        call fftpack_radf2 ( ido, l1, ch, c, wa(iw) )
      end if

    else if ( ip == 3 ) then

      ix2 = iw + ido

      if ( na == 0 ) then
        call fftpack_radf3 ( ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call fftpack_radf3 ( ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

    else if ( ip == 5 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido
      ix4 = ix3 + ido

      if ( na == 0 ) then
        call fftpack_radf5 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call fftpack_radf5 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

    else

      if ( ido == 1 ) then
        na = 1 - na
      end if

      if ( na == 0 ) then
        call fftpack_radfg ( ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
        na = 1
      else
        call fftpack_radfg ( ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
        na = 0
      end if

    end if

    l2 = l1

  end do

  if ( na /= 1 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine fftpack_rffti ( n, wsave )
!
!*******************************************************************************
!
!! RFFTI initializes WSAVE, used in RFFTF and RFFTB. 
!
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the 
!    trigonometric functions are computed and stored in WSAVE.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Clever Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    P N Swarztrauber, 
!    Vectorizing the FFT's, 
!    in Parallel Computations,
!    G. Rodrigue, editor, 
!    Academic Press, 1982, pages 51-83.
!
!    B L Buzbee, 
!    The SLATEC Common Math Library, 
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer N, the length of the sequence to be transformed. 
!
!    Output, real WSAVE(2*N+15), contains data, dependent on the value
!    of N, which is necessary for the RFFTF and RFFTB routines.  
!
  implicit none
!
  integer n
!
  real wsave(2*n+15)
  integer ifac(15)
!
  if ( n <= 1 ) then
    return
  end if

  call fftpack_rffti1 ( n, wsave(n+1), ifac)
  wsave(2*n+1:2*n+15) = ifac

  return
end
subroutine fftpack_rffti1 ( n, wa, ifac )
!
!*******************************************************************************
!
!! RFFTI1 is a lower level routine used by RFFTI.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the length of the sequence to be transformed. 
!
!    Input, real WA(N).
!
!    Input, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none
!
  integer n
!
  real arg
  real argh
  real argld
  real fi
  integer i
  integer ido
  integer ifac(15)
  integer ii
  integer ip
  integer is
  integer j
  integer k1
  integer l1
  integer l2
  integer ld
  integer nf
  real r_pi
  real wa(n)
!
  call i_factor ( n, ifac )

  nf = ifac(2)

  argh = 2.0E+00 * r_pi ( ) / real ( n )
  is = 0
  l1 = 1

  do k1 = 1, nf-1

    ip = ifac(k1+2)
    ld = 0
    l2 = l1 * ip
    ido = n / l2

    do j = 1, ip-1

      ld = ld + l1
      i = is
      argld = real ( ld ) * argh
      fi = 0.0E+00

      do ii = 3, ido, 2
        i = i + 2
        fi = fi + 1.0E+00
        arg = fi * argld
        wa(i-1) = cos ( arg )
        wa(i) = sin ( arg )
      end do

      is = is + ido

    end do

    l1 = l2

  end do

  return
end
subroutine fftpack_radb2 ( ido, l1, cc, ch, wa1 )
!
!*******************************************************************************
!
!! RADB2 is a lower level routine used by RFFTB1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,2,l1)
  real ch(ido,l1,2)
  integer i
  integer ic
  integer k
  real ti2
  real tr2
  real wa1(ido)
!
  ch(1,1:l1,1) = cc(1,1,1:l1) + cc(ido,2,1:l1)
  ch(1,1:l1,2) = cc(1,1,1:l1) - cc(ido,2,1:l1)

  if ( ido < 2 ) then
    return
  end if

  if ( ido > 2 ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        ch(i-1,k,1) = cc(i-1,1,k) + cc(ic-1,2,k)
        tr2         = cc(i-1,1,k) - cc(ic-1,2,k)
        ch(i,k,1)   = cc(i,1,k)   - cc(ic,2,k)
        ti2         = cc(i,1,k)   + cc(ic,2,k)

        ch(i-1,k,2) = wa1(i-2) * tr2 - wa1(i-1) * ti2
        ch(i,k,2)   = wa1(i-2) * ti2 + wa1(i-1) * tr2

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  ch(ido,1:l1,1) =    cc(ido,1,1:l1) + cc(ido,1,1:l1)
  ch(ido,1:l1,2) = -( cc(1,2,1:l1)   + cc(1,2,1:l1) )

  return
end
subroutine fftpack_radb3 ( ido, l1, cc, ch, wa1, wa2 )
!
!*******************************************************************************
!
!! RADB3 is a lower level routine used by RFFTB1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,3,l1)
  real ch(ido,l1,3)
  real ci2
  real ci3
  real cr2
  real cr3
  real di2
  real di3
  real dr2
  real dr3
  integer i
  integer ic
  integer k
  real taui
  real, parameter :: taur = -0.5E+00
  real ti2
  real tr2
  real wa1(ido)
  real wa2(ido)
!
  taui = sqrt ( 3.0E+00 ) / 2.0E+00

  do k = 1, l1

    tr2 = cc(ido,2,k) + cc(ido,2,k)
    cr2 = cc(1,1,k) + taur * tr2
    ch(1,k,1) = cc(1,1,k) + tr2
    ci3 = taui * ( cc(1,3,k) + cc(1,3,k) )

    ch(1,k,2) = cr2 - ci3
    ch(1,k,3) = cr2 + ci3

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      tr2 = cc(i-1,3,k) + cc(ic-1,2,k)
      cr2 = cc(i-1,1,k) + taur * tr2
      ch(i-1,k,1) = cc(i-1,1,k) + tr2

      ti2 = cc(i,3,k) - cc(ic,2,k)
      ci2 = cc(i,1,k) + taur * ti2
      ch(i,k,1) = cc(i,1,k) + ti2

      cr3 = taui * ( cc(i-1,3,k) - cc(ic-1,2,k) )
      ci3 = taui * ( cc(i,3,k)   + cc(ic,2,k) )

      dr2 = cr2 - ci3
      dr3 = cr2 + ci3
      di2 = ci2 + cr3
      di3 = ci2 - cr3

      ch(i-1,k,2) = wa1(i-2) * dr2 - wa1(i-1) * di2
      ch(i,k,2)   = wa1(i-2) * di2 + wa1(i-1) * dr2
      ch(i-1,k,3) = wa2(i-2) * dr3 - wa2(i-1) * di3
      ch(i,k,3)   = wa2(i-2) * di3 + wa2(i-1) * dr3

    end do
  end do

  return
end
subroutine fftpack_radb4 ( ido, l1, cc, ch, wa1, wa2, wa3 )
!
!*******************************************************************************
!
!! RADB4 is a lower level routine used by RFFTB1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,4,l1)
  real ch(ido,l1,4)
  real ci2
  real ci3
  real ci4
  real cr2
  real cr3
  real cr4
  integer i
  integer ic
  integer k
  real, parameter :: sqrt2 = 1.414213562373095E+00
  real ti1
  real ti2
  real ti3
  real ti4
  real tr1
  real tr2
  real tr3
  real tr4
  real wa1(ido)
  real wa2(ido)
  real wa3(ido)
!
  do k = 1, l1

    tr1 = cc(1,1,k) - cc(ido,4,k)
    tr2 = cc(1,1,k) + cc(ido,4,k)
    tr3 = cc(ido,2,k) + cc(ido,2,k)
    tr4 = cc(1,3,k) + cc(1,3,k)

    ch(1,k,1) = tr2 + tr3
    ch(1,k,2) = tr1 - tr4
    ch(1,k,3) = tr2 - tr3
    ch(1,k,4) = tr1 + tr4

  end do

  if ( ido < 2 ) then
    return
  end if

  if ( ido > 2 ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        ti1 = cc(i,1,k) + cc(ic,4,k)
        ti2 = cc(i,1,k) - cc(ic,4,k)
        ti3 = cc(i,3,k) - cc(ic,2,k)
        tr4 = cc(i,3,k) + cc(ic,2,k)

        tr1 = cc(i-1,1,k) - cc(ic-1,4,k)
        tr2 = cc(i-1,1,k) + cc(ic-1,4,k)
        ti4 = cc(i-1,3,k) - cc(ic-1,2,k)
        tr3 = cc(i-1,3,k) + cc(ic-1,2,k)

        ch(i-1,k,1) = tr2 + tr3
        cr3         = tr2 - tr3
        ch(i,k,1)   = ti2 + ti3
        ci3         = ti2 - ti3

        cr2 = tr1 - tr4
        cr4 = tr1 + tr4
        ci2 = ti1 + ti4
        ci4 = ti1 - ti4

        ch(i-1,k,2) = wa1(i-2) * cr2 - wa1(i-1) * ci2
        ch(i,k,2)   = wa1(i-2) * ci2 + wa1(i-1) * cr2
        ch(i-1,k,3) = wa2(i-2) * cr3 - wa2(i-1) * ci3
        ch(i,k,3)   = wa2(i-2) * ci3 + wa2(i-1) * cr3
        ch(i-1,k,4) = wa3(i-2) * cr4 - wa3(i-1) * ci4
        ch(i,k,4)   = wa3(i-2) * ci4 + wa3(i-1) * cr4

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  do k = 1, l1

    ti1 = cc(1,2,k)   + cc(1,4,k)
    ti2 = cc(1,4,k)   - cc(1,2,k)
    tr1 = cc(ido,1,k) - cc(ido,3,k)
    tr2 = cc(ido,1,k) + cc(ido,3,k)

    ch(ido,k,1) = tr2 + tr2
    ch(ido,k,2) = sqrt2 * ( tr1 - ti1 )
    ch(ido,k,3) = ti2 + ti2
    ch(ido,k,4) = -sqrt2 * ( tr1 + ti1 )

  end do

  return
end
subroutine fftpack_radb5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )
!
!*******************************************************************************
!
!! RADB5 is a lower level routine used by RFFTB1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,5,l1)
  real ch(ido,l1,5)
  real ci2
  real ci3
  real ci4
  real ci5
  real cr2
  real cr3
  real cr4
  real cr5
  real di2
  real di3
  real di4
  real di5
  real dr2
  real dr3
  real dr4
  real dr5
  integer i
  integer ic
  integer k
  real, parameter :: ti11 =  0.951056516295154E+00
  real, parameter :: ti12 =  0.587785252292473E+00
  real ti2
  real ti3
  real ti4
  real ti5
  real, parameter :: tr11 =  0.309016994374947E+00
  real, parameter :: tr12 = -0.809016994374947E+00
  real tr2
  real tr3
  real tr4
  real tr5
  real wa1(ido)
  real wa2(ido)
  real wa3(ido)
  real wa4(ido)
!
  do k = 1, l1

    ti5 = cc(1,3,k) + cc(1,3,k)
    ti4 = cc(1,5,k) + cc(1,5,k)
    tr2 = cc(ido,2,k) + cc(ido,2,k)
    tr3 = cc(ido,4,k) + cc(ido,4,k)

    ch(1,k,1) = cc(1,1,k) + tr2 + tr3
    cr2 = cc(1,1,k) + tr11 * tr2 + tr12 * tr3
    cr3 = cc(1,1,k) + tr12 * tr2 + tr11 * tr3
    ci5 = ti11 * ti5 + ti12 * ti4
    ci4 = ti12 * ti5 - ti11 * ti4

    ch(1,k,2) = cr2 - ci5
    ch(1,k,3) = cr3 - ci4
    ch(1,k,4) = cr3 + ci4
    ch(1,k,5) = cr2 + ci5

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      ti5 = cc(i,3,k) + cc(ic,2,k)
      ti2 = cc(i,3,k) - cc(ic,2,k)
      ti4 = cc(i,5,k) + cc(ic,4,k)
      ti3 = cc(i,5,k) - cc(ic,4,k)
      tr5 = cc(i-1,3,k) - cc(ic-1,2,k)
      tr2 = cc(i-1,3,k) + cc(ic-1,2,k)
      tr4 = cc(i-1,5,k) - cc(ic-1,4,k)
      tr3 = cc(i-1,5,k) + cc(ic-1,4,k)

      ch(i-1,k,1) = cc(i-1,1,k) + tr2 + tr3
      ch(i,k,1)   = cc(i,1,k) + ti2 + ti3

      cr2 = cc(i-1,1,k) + tr11 * tr2 + tr12 * tr3
      ci2 = cc(i,1,k)   + tr11 * ti2 + tr12 * ti3
      cr3 = cc(i-1,1,k) + tr12 * tr2 + tr11 * tr3
      ci3 = cc(i,1,k)   + tr12 * ti2 + tr11 * ti3

      cr5 = ti11 * tr5 + ti12 * tr4
      ci5 = ti11 * ti5 + ti12 * ti4
      cr4 = ti12 * tr5 - ti11 * tr4
      ci4 = ti12 * ti5 - ti11 * ti4

      dr3 = cr3 - ci4
      dr4 = cr3 + ci4
      di3 = ci3 + cr4
      di4 = ci3 - cr4
      dr5 = cr2 + ci5
      dr2 = cr2 - ci5
      di5 = ci2 - cr5
      di2 = ci2 + cr5

      ch(i-1,k,2) = wa1(i-2) * dr2 - wa1(i-1) * di2
      ch(i,k,2)   = wa1(i-2) * di2 + wa1(i-1) * dr2
      ch(i-1,k,3) = wa2(i-2) * dr3 - wa2(i-1) * di3
      ch(i,k,3)   = wa2(i-2) * di3 + wa2(i-1) * dr3
      ch(i-1,k,4) = wa3(i-2) * dr4 - wa3(i-1) * di4
      ch(i,k,4)   = wa3(i-2) * di4 + wa3(i-1) * dr4
      ch(i-1,k,5) = wa4(i-2) * dr5 - wa4(i-1) * di5
      ch(i,k,5)   = wa4(i-2) * di5 + wa4(i-1) * dr5

    end do
  end do

  return
end
subroutine fftpack_radbg ( ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )
!
!*******************************************************************************
!
!! RADBG is a lower level routine used by RFFTB1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer idl1
  integer ido
  integer ip
  integer l1
!
  real ai1
  real ai2
  real ar1
  real ar1h
  real ar2
  real ar2h
  real arg
  real c1(ido,l1,ip)
  real c2(idl1,ip)
  real cc(ido,ip,l1)
  real ch(ido,l1,ip)
  real ch2(idl1,ip)
  real dc2
  real dcp
  real ds2
  real dsp
  integer i
  integer ic
  integer idij
  integer ik
  integer ipph
  integer is
  integer j
  integer j2
  integer jc
  integer k
  integer l
  integer lc
  integer nbd
  real r_pi
  real wa(*)
!
  arg = 2.0E+00 * r_pi ( ) / real ( ip )
  dcp = cos ( arg )
  dsp = sin ( arg )
  nbd = ( ido - 1 ) / 2
  ipph = ( ip + 1 ) / 2
  ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  do j = 2, ipph
    jc = ip + 2 - j
    j2 = j + j
    ch(1,1:l1,j) =  cc(ido,j2-2,1:l1) + cc(ido,j2-2,1:l1)
    ch(1,1:l1,jc) = cc(1,j2-1,1:l1)   + cc(1,j2-1,1:l1)
  end do

  if ( ido /= 1 ) then

    if ( nbd >= l1 ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            ic = ido + 2 - i
            ch(i-1,k,j)  = cc(i-1,2*j-1,k) + cc(ic-1,2*j-2,k)
            ch(i-1,k,jc) = cc(i-1,2*j-1,k) - cc(ic-1,2*j-2,k)
            ch(i,k,j)    = cc(i,2*j-1,k)   - cc(ic,2*j-2,k)
            ch(i,k,jc)   = cc(i,2*j-1,k)   + cc(ic,2*j-2,k)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          ic = ido + 2 - i
          ch(i-1,1:l1,j)  = cc(i-1,2*j-1,1:l1) + cc(ic-1,2*j-2,1:l1)
          ch(i-1,1:l1,jc) = cc(i-1,2*j-1,1:l1) - cc(ic-1,2*j-2,1:l1)
          ch(i,1:l1,j)    = cc(i,2*j-1,1:l1)   - cc(ic,2*j-2,1:l1)
          ch(i,1:l1,jc)   = cc(i,2*j-1,1:l1)   + cc(ic,2*j-2,1:l1)
        end do
      end do

    end if

  end if

  ar1 = 1.0E+00
  ai1 = 0.0E+00

  do l = 2, ipph

    lc = ip + 2 - l
    ar1h = dcp * ar1 - dsp * ai1
    ai1 =  dcp * ai1 + dsp * ar1
    ar1 = ar1h

    do ik = 1, idl1
      c2(ik,l)  = ch2(ik,1) + ar1 * ch2(ik,2)
      c2(ik,lc) =             ai1 * ch2(ik,ip)
    end do

    dc2 = ar1
    ds2 = ai1
    ar2 = ar1
    ai2 = ai1

    do j = 3, ipph

      jc = ip + 2 - j
      ar2h = dc2 * ar2 - ds2 * ai2
      ai2  = dc2 * ai2 + ds2 * ar2
      ar2 = ar2h

      do ik = 1, idl1
        c2(ik,l)  = c2(ik,l)  + ar2 * ch2(ik,j)
        c2(ik,lc) = c2(ik,lc) + ai2 * ch2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + ch2(1:idl1,j)
  end do

  do j = 2, ipph
    jc = ip + 2 - j
    ch(1,1:l1,j)  = c1(1,1:l1,j) - c1(1,1:l1,jc)
    ch(1,1:l1,jc) = c1(1,1:l1,j) + c1(1,1:l1,jc)
  end do

  if ( ido /= 1 ) then

    if ( nbd >= l1 ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            ch(i-1,k,j)  = c1(i-1,k,j) - c1(i,k,jc)
            ch(i-1,k,jc) = c1(i-1,k,j) + c1(i,k,jc)
            ch(i,k,j)    = c1(i,k,j)   + c1(i-1,k,jc)
            ch(i,k,jc)   = c1(i,k,j)   - c1(i-1,k,jc)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          ch(i-1,1:l1,j)  = c1(i-1,1:l1,j) - c1(i,1:l1,jc)
          ch(i-1,1:l1,jc) = c1(i-1,1:l1,j) + c1(i,1:l1,jc)
          ch(i,1:l1,j)    = c1(i,1:l1,j)   + c1(i-1,1:l1,jc)
          ch(i,1:l1,jc)   = c1(i,1:l1,j)   - c1(i-1,1:l1,jc)
        end do
      end do

    end if

  end if

  if ( ido == 1 ) then
    return
  end if

  c2(1:idl1,1) = ch2(1:idl1,1)
  c1(1,1:l1,2:ip) = ch(1,1:l1,2:ip)

  if ( nbd <= l1 ) then

    is = -ido

    do j = 2, ip
      is = is + ido
      idij = is
      do i = 3, ido, 2
        idij = idij + 2
        c1(i-1,1:l1,j) = wa(idij-1) * ch(i-1,1:l1,j) - wa(idij) * ch(i,1:l1,j)
        c1(i,1:l1,j)   = wa(idij-1) * ch(i,1:l1,j)   + wa(idij) * ch(i-1,1:l1,j)
      end do
    end do

  else

    is = -ido
    do j = 2, ip
      is = is + ido
      do k = 1, l1
        idij = is
        do i = 3, ido, 2
          idij = idij + 2
          c1(i-1,k,j) = wa(idij-1) * ch(i-1,k,j) - wa(idij) * ch(i,k,j)
          c1(i,k,j)   = wa(idij-1) * ch(i,k,j)   + wa(idij) * ch(i-1,k,j)
        end do
      end do
    end do

  end if

  return
end
subroutine fftpack_radf2 ( ido, l1, cc, ch, wa1 )
!
!*******************************************************************************
!
!! RADF2 is a lower level routine used by RFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,l1,2)
  real ch(ido,2,l1)
  integer i
  integer ic
  integer k
  real ti2
  real tr2
  real wa1(ido)
!
  ch(1,1,1:l1)   = cc(1,1:l1,1) + cc(1,1:l1,2)
  ch(ido,2,1:l1) = cc(1,1:l1,1) - cc(1,1:l1,2)

  if ( ido < 2 ) then
    return
  end if

  if ( ido > 2 ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        tr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
        ti2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)

        ch(i,1,k) = cc(i,k,1) + ti2
        ch(ic,2,k) = ti2 - cc(i,k,1)
        ch(i-1,1,k) = cc(i-1,k,1) + tr2
        ch(ic-1,2,k) = cc(i-1,k,1) - tr2

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  ch(1,2,1:l1) = -cc(ido,1:l1,2)
  ch(ido,1,1:l1) = cc(ido,1:l1,1)

  return
end
subroutine fftpack_radf3 ( ido, l1, cc, ch, wa1, wa2 )
!
!*******************************************************************************
!
!! RADF3 is a lower level routine used by RFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,l1,3)
  real ch(ido,3,l1)
  real ci2
  real cr2
  real di2
  real di3
  real dr2
  real dr3
  integer i
  integer ic
  integer k
  real taui
  real, parameter :: taur = -0.5E+00
  real ti2
  real ti3
  real tr2
  real tr3
  real wa1(ido)
  real wa2(ido)
!
  taui = sqrt ( 3.0E+00 ) / 2.0E+00

  do k = 1, l1
    cr2 = cc(1,k,2) + cc(1,k,3)
    ch(1,1,k) = cc(1,k,1) + cr2
    ch(1,3,k) = taui * ( cc(1,k,3) - cc(1,k,2) )
    ch(ido,2,k) = cc(1,k,1) + taur * cr2
  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      dr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
      di2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
      dr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
      di3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)

      cr2 = dr2 + dr3
      ci2 = di2 + di3

      ch(i-1,1,k) = cc(i-1,k,1) + cr2
      ch(i,1,k)   = cc(i,k,1) + ci2

      tr2 = cc(i-1,k,1) + taur * cr2
      ti2 = cc(i,k,1) + taur * ci2
      tr3 = taui * ( di2 - di3 )
      ti3 = taui * ( dr3 - dr2 )

      ch(i-1,3,k) = tr2 + tr3
      ch(ic-1,2,k) = tr2 - tr3
      ch(i,3,k) = ti2 + ti3
      ch(ic,2,k) = ti3 - ti2

    end do
  end do

  return
end
subroutine fftpack_radf4 ( ido, l1, cc, ch, wa1, wa2, wa3 )
!
!*******************************************************************************
!
!! RADF4 is a lower level routine used by RFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,l1,4)
  real ch(ido,4,l1)
  real ci2
  real ci3
  real ci4
  real cr2
  real cr3
  real cr4
  real hsqt2
  integer i
  integer ic
  integer k
  real ti1
  real ti2
  real ti3
  real ti4
  real tr1
  real tr2
  real tr3
  real tr4
  real wa1(ido)
  real wa2(ido)
  real wa3(ido)
!
  hsqt2 = sqrt ( 2.0E+00 ) / 2.0E+00

  do k = 1, l1
    tr1 = cc(1,k,2) + cc(1,k,4)
    tr2 = cc(1,k,1) + cc(1,k,3)
    ch(1,1,k) = tr1 + tr2
    ch(ido,4,k) = tr2 - tr1
    ch(ido,2,k) = cc(1,k,1) - cc(1,k,3)
    ch(1,3,k) = cc(1,k,4) - cc(1,k,2)
  end do

  if ( ido < 2 ) then
    return
  end if

  if ( ido > 2 ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        cr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
        ci2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
        cr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
        ci3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)
        cr4 = wa3(i-2) * cc(i-1,k,4) + wa3(i-1) * cc(i,k,4)
        ci4 = wa3(i-2) * cc(i,k,4)   - wa3(i-1) * cc(i-1,k,4)

        tr1 = cr2 + cr4
        tr4 = cr4 - cr2
        ti1 = ci2 + ci4
        ti4 = ci2 - ci4
        ti2 = cc(i,k,1) + ci3
        ti3 = cc(i,k,1) - ci3
        tr2 = cc(i-1,k,1) + cr3
        tr3 = cc(i-1,k,1) - cr3

        ch(i-1,1,k)  = tr1 + tr2
        ch(ic-1,4,k) = tr2 - tr1
        ch(i,1,k)    = ti1 + ti2
        ch(ic,4,k)   = ti1 - ti2
        ch(i-1,3,k)  = ti4 + tr3
        ch(ic-1,2,k) = tr3 - ti4
        ch(i,3,k)    = tr4 + ti3
        ch(ic,2,k)   = tr4 - ti3

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  do k = 1, l1

    ti1 = -hsqt2 * ( cc(ido,k,2) + cc(ido,k,4) )
    tr1 =  hsqt2 * ( cc(ido,k,2) - cc(ido,k,4) )

    ch(ido,1,k) = tr1 + cc(ido,k,1)
    ch(ido,3,k) = cc(ido,k,1) - tr1

    ch(1,2,k) = ti1 - cc(ido,k,3)
    ch(1,4,k) = ti1 + cc(ido,k,3)

  end do

  return
end
subroutine fftpack_radf5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )
!
!*******************************************************************************
!
!! RADF5 is a lower level routine used by RFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none
!
  integer ido
  integer l1
!
  real cc(ido,l1,5)
  real ch(ido,5,l1)
  real ci2
  real ci3
  real ci4
  real ci5
  real cr2
  real cr3
  real cr4
  real cr5
  real di2
  real di3
  real di4
  real di5
  real dr2
  real dr3
  real dr4
  real dr5
  integer i
  integer ic
  integer k
  real, parameter :: ti11 =  0.951056516295154E+00
  real, parameter :: ti12 =  0.587785252292473E+00
  real ti2
  real ti3
  real ti4
  real ti5
  real, parameter :: tr11 =  0.309016994374947E+00
  real, parameter :: tr12 = -0.809016994374947E+00
  real tr2
  real tr3
  real tr4
  real tr5
  real wa1(ido)
  real wa2(ido)
  real wa3(ido)
  real wa4(ido)
!
  do k = 1, l1

    cr2 = cc(1,k,5) + cc(1,k,2)
    ci5 = cc(1,k,5) - cc(1,k,2)
    cr3 = cc(1,k,4) + cc(1,k,3)
    ci4 = cc(1,k,4) - cc(1,k,3)

    ch(1,1,k)   = cc(1,k,1) + cr2 + cr3
    ch(ido,2,k) = cc(1,k,1) + tr11 * cr2 + tr12 * cr3
    ch(1,3,k)   = ti11 * ci5 + ti12 * ci4
    ch(ido,4,k) = cc(1,k,1) + tr12 * cr2 + tr11 * cr3
    ch(1,5,k)   = ti12 * ci5 - ti11 * ci4

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      dr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
      di2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
      dr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
      di3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)
      dr4 = wa3(i-2) * cc(i-1,k,4) + wa3(i-1) * cc(i,k,4)
      di4 = wa3(i-2) * cc(i,k,4)   - wa3(i-1) * cc(i-1,k,4)
      dr5 = wa4(i-2) * cc(i-1,k,5) + wa4(i-1) * cc(i,k,5)
      di5 = wa4(i-2) * cc(i,k,5)   - wa4(i-1) * cc(i-1,k,5)

      cr2 = dr2 + dr5
      ci5 = dr5 - dr2
      cr5 = di2 - di5
      ci2 = di2 + di5
      cr3 = dr3 + dr4
      ci4 = dr4 - dr3
      cr4 = di3 - di4
      ci3 = di3 + di4

      ch(i-1,1,k) = cc(i-1,k,1) + cr2 + cr3
      ch(i,1,k)   = cc(i,k,1)   + ci2 + ci3

      tr2 = cc(i-1,k,1) + tr11 * cr2 + tr12 * cr3
      ti2 = cc(i,k,1)   + tr11 * ci2 + tr12 * ci3
      tr3 = cc(i-1,k,1) + tr12 * cr2 + tr11 * cr3
      ti3 = cc(i,k,1)   + tr12 * ci2 + tr11 * ci3

      tr5 = ti11 * cr5 + ti12 * cr4
      ti5 = ti11 * ci5 + ti12 * ci4
      tr4 = ti12 * cr5 - ti11 * cr4
      ti4 = ti12 * ci5 - ti11 * ci4

      ch(i-1,3,k)  = tr2 + tr5
      ch(ic-1,2,k) = tr2 - tr5
      ch(i,3,k)    = ti2 + ti5
      ch(ic,2,k)   = ti5 - ti2
      ch(i-1,5,k)  = tr3 + tr4
      ch(ic-1,4,k) = tr3 - tr4
      ch(i,5,k)    = ti3 + ti4
      ch(ic,4,k)   = ti4 - ti3

    end do
  end do

  return
end
subroutine fftpack_radfg ( ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )
!
!*******************************************************************************
!
!! RADFG is a lower level routine used by RFFTF1.
!
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer IDO, ?
!
!    Input, integer IP, ?
!
!    Input, integer L1, ?
!
!    Input, integer IDL1, ?
!
!    ?, real CC(IDO,IP,L1), ?
!
!    ?, real C1(IDO,L1,IP), ?
!
!    ?, real C2(IDL1,IP), ?
!
!    ?, real CH(IDO,L1,IP), ?
!
!    ?, real CH2(IDL1,IP), ?
!
!    ?, real WA(*), ?
!
  implicit none
!
  integer idl1
  integer ido
  integer ip
  integer l1
!
  real ai1
  real ai2
  real ar1
  real ar1h
  real ar2
  real ar2h
  real arg
  real c1(ido,l1,ip)
  real c2(idl1,ip)
  real cc(ido,ip,l1)
  real ch(ido,l1,ip)
  real ch2(idl1,ip)
  real dc2
  real dcp
  real ds2
  real dsp
  integer i
  integer ic
  integer idij
  integer ik
  integer ipph
  integer is
  integer j
  integer j2
  integer jc
  integer k
  integer l
  integer lc
  integer nbd
  real r_pi
  real wa(*)
!
  arg = 2.0E+00 * r_pi ( ) / real ( ip )
  dcp = cos ( arg )
  dsp = sin ( arg )
  ipph = ( ip + 1 ) / 2
  nbd = ( ido - 1 ) / 2

  if ( ido == 1 ) then

    c2(1:idl1,1) = ch2(1:idl1,1)

  else

    ch2(1:idl1,1) = c2(1:idl1,1)
    ch(1,1:l1,2:ip) = c1(1,1:l1,2:ip)

    if ( nbd <= l1 ) then

      is = -ido
      do j = 2, ip
        is = is + ido
        idij = is
        do i = 3, ido, 2
          idij = idij + 2
          do k = 1, l1
            ch(i-1,k,j) = wa(idij-1) * c1(i-1,k,j) + wa(idij) * c1(i,k,j)
            ch(i,k,j)   = wa(idij-1) * c1(i,k,j)   - wa(idij) * c1(i-1,k,j)
          end do
        end do
      end do

    else

      is = -ido

      do j = 2, ip
        is = is + ido
        do k = 1, l1
          idij = is
          do i = 3, ido, 2
            idij = idij + 2
            ch(i-1,k,j) = wa(idij-1) * c1(i-1,k,j) + wa(idij) * c1(i,k,j)
            ch(i,k,j)   = wa(idij-1) * c1(i,k,j)   - wa(idij) * c1(i-1,k,j)
          end do
        end do
      end do

    end if

    if ( nbd >= l1 ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            c1(i-1,k,j)  = ch(i-1,k,j)  + ch(i-1,k,jc)
            c1(i-1,k,jc) = ch(i,k,j)    - ch(i,k,jc)
            c1(i,k,j)    = ch(i,k,j)    + ch(i,k,jc)
            c1(i,k,jc)   = ch(i-1,k,jc) - ch(i-1,k,j)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          c1(i-1,1:l1,j)  = ch(i-1,1:l1,j)  + ch(i-1,1:l1,jc)
          c1(i-1,1:l1,jc) = ch(i,1:l1,j)    - ch(i,1:l1,jc)
          c1(i,1:l1,j)    = ch(i,1:l1,j)    + ch(i,1:l1,jc)
          c1(i,1:l1,jc)   = ch(i-1,1:l1,jc) - ch(i-1,1:l1,j)
        end do
      end do

    end if

  end if

  do j = 2, ipph
    jc = ip + 2 - j
    c1(1,1:l1,j)  = ch(1,1:l1,j)  + ch(1,1:l1,jc)
    c1(1,1:l1,jc) = ch(1,1:l1,jc) - ch(1,1:l1,j)
  end do

  ar1 = 1.0E+00
  ai1 = 0.0E+00

  do l = 2, ipph

    lc = ip + 2 - l
    ar1h = dcp * ar1 - dsp * ai1
    ai1 =  dcp * ai1 + dsp * ar1
    ar1 = ar1h

    do ik = 1, idl1
      ch2(ik,l) = c2(ik,1) + ar1 * c2(ik,2)
      ch2(ik,lc) =           ai1 * c2(ik,ip)
    end do

    dc2 = ar1
    ds2 = ai1
    ar2 = ar1
    ai2 = ai1

    do j = 3, ipph

      jc = ip + 2 - j
      ar2h = dc2 * ar2 - ds2 * ai2
      ai2 =  dc2 * ai2 + ds2 * ar2
      ar2 = ar2h

      do ik = 1, idl1
        ch2(ik,l) =  ch2(ik,l)  + ar2 * c2(ik,j)
        ch2(ik,lc) = ch2(ik,lc) + ai2 * c2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + c2(1:idl1,j)
  end do

  cc(1:ido,1,1:l1) = ch(1:ido,1:l1,1)

  do j = 2, ipph
    jc = ip + 2 - j
    j2 = j + j
    cc(ido,j2-2,1:l1) = ch(1,1:l1,j)
    cc(1,j2-1,1:l1)   = ch(1,1:l1,jc)
  end do

  if ( ido == 1 ) then
    return
  end if

  if ( nbd >= l1 ) then

    do j = 2, ipph
      jc = ip + 2 - j
      j2 = j + j
      do k = 1, l1
        do i = 3, ido, 2
          ic = ido + 2 - i
          cc(i-1,j2-1,k)  = ch(i-1,k,j) + ch(i-1,k,jc)
          cc(ic-1,j2-2,k) = ch(i-1,k,j) - ch(i-1,k,jc)
          cc(i,j2-1,k)    = ch(i,k,j)   + ch(i,k,jc)
          cc(ic,j2-2,k)   = ch(i,k,jc)  - ch(i,k,j)
        end do
      end do
    end do

  else

    do j = 2, ipph
      jc = ip + 2 - j
      j2 = j + j
      do i = 3, ido, 2
        ic = ido + 2 - i
        cc(i-1,j2-1,1:l1)  = ch(i-1,1:l1,j) + ch(i-1,1:l1,jc)
        cc(ic-1,j2-2,1:l1) = ch(i-1,1:l1,j) - ch(i-1,1:l1,jc)
        cc(i,j2-1,1:l1)    = ch(i,1:l1,j)   + ch(i,1:l1,jc)
        cc(ic,j2-2,1:l1)   = ch(i,1:l1,jc)  - ch(i,1:l1,j)
      end do
    end do

  end if

  return
end
subroutine i_factor ( n, ifac )
!
!*******************************************************************************
!
!! I_FACTOR factors an integer.
!
!
!  Modified:
!
!    14 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer N, the number to be factored.
!
!    Output, integer IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none
!
  integer i
  integer ifac(15)
  integer j
  integer n
  integer nf
  integer nl
  integer nq
  integer nr
  integer ntry
!
  ifac(1) = n

  nf = 0
  nl = n

  if ( n == 0 ) then
    nf = 1
    ifac(2) = nf
    ifac(2+nf) = 0
    return
  end if

  if ( n < 1 ) then
    nf = nf + 1
    ifac(2+nf) = -1
    nl = - n
  end if

  if ( nl == 1 ) then
    nf = nf + 1
    ifac(2) = nf
    ifac(2+nf) = 1
    return
  end if

  j = 0

  do while ( nl > 1 )

    j = j + 1
!
!  Choose a trial divisor, NTRY.
!
    if ( j == 1 ) then
      ntry = 4
    else if ( j == 2 ) then
      ntry = 2
    else if ( j == 3 ) then
      ntry = 3
    else if ( j == 4 ) then
      ntry = 5
    else
      ntry = ntry + 2
    end if
!
!  Divide by the divisor as many times as possible.
!
    do

      nq = nl / ntry
      nr = nl - ntry * nq

      if ( nr /= 0 ) then
        exit
      end if

      nl = nq
      nf = nf + 1
!
!  Make sure factors of 2 appear in the front of the list.
!
      if ( ntry /= 2 ) then

        ifac(2+nf) = ntry

      else

        do i = nf, 2, -1
          ifac(i+2) = ifac(i+1)
        end do
        ifac(3) = 2

      end if

    end do

  end do

  ifac(2) = nf

  return
end
function r_pi ()
!
!*******************************************************************************
!
!! R_PI returns the value of pi.
!
!
!  Modified:
!
!    08 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real R_PI, the value of PI.
!
  implicit none
!
  real r_pi
!
  r_pi = 3.14159265358979323846264338327950288419716939937510E+00

  return
end
