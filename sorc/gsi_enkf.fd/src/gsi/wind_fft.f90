!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
module wind_fft
!$$$   module documentation block
!                .      .    .                                       .
! module:    wind_fft
! prgmmr: pondeca          org: np23                date: 2006-08-01
!
! abstract: contains subroutines for 2-dimensional FFTs on the plane.  
!           adapted from jeff whitaker's August 1, 1991 two-layer 
!           qg-model (qg2l)
!
! program history log:
!   2006-08-01  pondeca
!
! subroutines included:
!   sub divvort_to_psichi
!   sub fft2d
!   sub rfft
!   sub cfft
!   sub FFT2
!   sub PASS2
!   sub PREFFT
!   sub FACTOR
!
! variable definitions
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind,r_single,r_kind
  use constants, only: zero,half,one,two,four
  implicit none

! set default to private
  private
! set subroutines to public
  public :: divvort_to_psichi
  public :: fft2d
  public :: rfft
  public :: cfft
  public :: FFT2
  public :: PASS2
  public :: PREFFT
  public :: FACTOR

  integer(i_kind) nx,ny,mmax,nwavesx,nmax,nwavesy
  integer(i_kind) nfax0,nfay0

  real(r_kind),allocatable::rk(:),rl(:)
  real(r_kind),allocatable::indxy(:),trigx(:,:),trigy(:,:)

  real(r_kind),allocatable::delimn(:,:)
  
  complex(r_kind),allocatable::scrmn(:,:,:,:),scr(:,:),qmn(:,:,:)

  integer(i_kind) ifax0(20),ifay0(20)

!-------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
subroutine divvort_to_psichi(nx0,ny0,mmax0,nmax0,rld0,qg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    divvort_to_psichi
! prgmmr: pondeca          org: np22                date: 2006-08-01
!
!
! abstract: use two-dimensional FFTs to convert gridded 
!           fields of divergence and vorticity into 
!           stream-function and velocity-potential
!
! program history log:
!   2005-02-08  pondeca
!
!   input argument list:
!     nx0,ny0           - number of grid points in x/y
!     mmax0,nmax0
!     rld0
!
!   output argument list:
!     qg
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
      implicit none

      integer(i_kind), intent(in   ) :: nx0,ny0
      integer(i_kind), intent(in   ) :: mmax0,nmax0
      real(r_single) , intent(in   ) :: rld0(nx0,ny0)
      real(r_single) , intent(  out) :: qg(nx0,ny0,2)

      real(r_kind) xmax,ymax,pi,wavey,delmn
      integer(i_kind) i,j,k,ny2,m,mm,indx

      nx=nx0
      ny=ny0
      mmax=mmax0
      nmax=nmax0
      nwavesx = mmax+1
      nwavesy = 2*nmax+1

!     write(6,*) ' in divvort_to_psichi: nx,ny,nwavesx,nwavesy=',nx,ny,nwavesx,nwavesy

      allocate(scrmn(nwavesx,nwavesy,2,4))
      allocate(scr(nwavesx,ny))
      allocate(qmn(nwavesx,nwavesy,2))
      allocate(delimn(nwavesx,nwavesy))

      allocate(rk(nwavesx))
      allocate(rl(nwavesy))
      allocate(indxy(nwavesy))
      allocate(trigx(2,nx))
      allocate(trigy(2,ny))

      pi = four*atan(one)

      xmax = real(nx-1,r_kind)
      ymax = real(ny-1,r_kind)
!     write(6,*) ' in divvort_to_psichi: xmax,ymax,=',xmax,ymax
!
!==> compute trig tables for fft routines.
!
      call prefft(nx,nfax0,ifax0,trigx)
      call prefft(ny,nfay0,ifay0,trigy)
!
!==> set up wavenumbers used in fourier differentiation.
      do 100 i=1,nwavesx
         rk(i) = two*pi*real(i-1,r_kind)/xmax
100   continue

      ny2 = (ny/2)+1
      do 200 j=1,ny
         mm = j/ny2
         m = mm*ny+1
         wavey = two*pi*real(j-m,r_kind)/ymax
         if (j<=nmax+1 .or. j>=ny-nmax+1) then
         indx = j-m+nmax+1
         indxy(indx) = j
         rl(indx) = wavey
         end if
200   continue
!
      do 300 j=1,nwavesy
      do 300 i=1,nwavesx
         delmn = -(rk(i)*rk(i) + rl(j)*rl(j))
         delimn(i,j) = zero
         if (delmn/=zero) delimn(i,j)=one/delmn
300   continue
!     write(6,*) 'divvort_to_psichi: delimn,min,max=',minval(delimn),maxval(delimn)

      do 600 k=1,2
          scr=(zero,zero)
          call fft2d(qg(1,1,k),qmn(1,1,k),scr,+1)
          do 500 j=1,nwavesy
          do 500 i=1,nwavesx
             qmn(i,j,k)=delimn(i,j)*qmn(i,j,k)
500       continue
          scr=(zero,zero)
          call fft2d(qg(1,1,k),qmn(1,1,k),scr,-1)
          qg(:,:,k)=qg(:,:,k)*rld0(:,:)**2
600   continue

!     write(6,*) ' in divvort_to_psichi: all done'

      deallocate(scrmn)
      deallocate(scr)
      deallocate(qmn)
      deallocate(delimn)
      deallocate(rk)
      deallocate(rl)
      deallocate(indxy)
      deallocate(trigx)
      deallocate(trigy)

      return
end  subroutine divvort_to_psichi
!
      subroutine fft2d(data,coeff,scr,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fft2d
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    idir
!    data
!    scr,coeff
!
!   output argument list:
!    data
!    scr,coeff
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      integer(i_kind), intent(in   ) :: idir
      real(r_single) , intent(inout) :: data(nx,ny)
      complex(r_kind), intent(inout) :: scr(nwavesx,ny),coeff(nwavesx,nwavesy)
!     write(6,*) 'IN fft2d,nx,ny,nwavesx,nwavesy=',nx,ny,nwavesx,nwavesy
!-------------------------------------------------
!==> forward transform.
!-------------------------------------------------
      if (idir==1) then
         call rfft(data,scr,1)
         call cfft(scr,coeff,1)
!-------------------------------------------------
!==> inverse transform.
!-------------------------------------------------
      else if (idir==-1) then
         call cfft(scr,coeff,-1)
         call rfft(data,scr,-1)
!
      else
         write(6,*) ' idir must be +1 or -1 in fft2d!'
         write(6,*) ' execution terminated.'
         stop
!
      end if
!
      return
end subroutine fft2d 
!
      subroutine rfft(data,coeff,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rfft
!   prgmmr:
!
! abstract:  performs fast multiple real fft's pairwise using a
!            multiple complex fft routine.  the number of fft's
!            to be performed must be even.
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    idir
!
!   output argument list:
!    data
!    coeff
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      integer(i_kind),intent(in   ) :: idir
      real(r_single) ,intent(  out) :: data(nx,ny)
      complex(r_kind),intent(  out) :: coeff(nwavesx,ny)

      integer(i_kind) i,j,n,npts,ndata,ndatah
      real(r_kind) a(ny/2,nx,2),c(ny/2,nx,2)

      npts = nx
      ndata = ny
      ndatah = ndata/2
!
!----------------------
!==> forward transform.
!----------------------
      if (idir==+1) then
!
!==> copy the data into the work array.
!    transforms are computed pairwise using a complex fft.
!    
         do 10 j=1,npts
         do 10 i=1,ndatah
            a(i,j,1) = data(j,i)
            a(i,j,2) = data(j,i+ndatah)
10       continue
!
         call fft2(a,c,npts,npts-1,nfax0,ifax0,-1,trigx,ndatah)
!
         do 20 i=1,ndatah
         coeff(1,i) = c(i,1,1)
         coeff(1,i+ndatah) = c(i,1,2)
         do 20 n=2,nwavesx
            coeff(n,i) = half*cmplx(c(i,n,1),c(i,n,2)) + & 
            half*cmplx(c(i,npts-n+2,1),-c(i,npts-n+2,2))
            coeff(n,i+ndatah) = half*cmplx(c(i,n,2),-c(i,n,1)) + & 
            half*cmplx(c(i,npts-n+2,2),c(i,npts-n+2,1))
20       continue
!----------------------
!==> inverse transform.
!----------------------
      else if (idir==-1) then
!
         do 25 j=1,npts
         do 25 i=1,ndatah
            c(i,j,1) = zero
            c(i,j,2) = zero
25       continue
         do 30 i=1,ndatah
         c(i,1,1) = real(coeff(1,i))
         c(i,1,2) = real(coeff(1,i+ndatah))
         do 30 n=2,nwavesx
            c(i,npts-n+2,1) = real(coeff(n,i))+aimag(coeff(n,i+ndatah))
            c(i,n,1) = real(coeff(n,i))-aimag(coeff(n,i+ndatah))
            c(i,n,2) = aimag(coeff(n,i))+real(coeff(n,i+ndatah))
            c(i,npts-n+2,2) = real(coeff(n,i+ndatah))-aimag(coeff(n,i))
30       continue
!
         call fft2(c,a,npts,npts-1,nfax0,ifax0,+1,trigx,ndatah)
!
         do 40 j=1,npts
         do 40 i=1,ndatah
            data(j,i) = a(i,j,1)
            data(j,i+ndatah) = a(i,j,2)
40       continue
!
      else
         write(6,*) ' idir must be +1 or -1 in rfft!'
         write(6,*) ' execution terminated.'
         stop
      end if
!
      return
end subroutine rfft
!
      subroutine cfft(data,coeff,idir)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cfft
!   prgmmr:
!
! abstract: performs fast multiple complex fft's.
!           array data(ndata,npts) contains ndata distinct complex
!           data sets of length npts to be transformed.
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    idir
!
!   output argument list:
!    data
!    coeff
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      integer(i_kind),intent(in   ) :: idir
      complex(r_kind),intent(  out) :: data(nwavesx,ny),coeff(nwavesx,nwavesy)

      integer(i_kind) i,j,jj,npts,ndata
      real(r_kind) a(nwavesx,ny,2),c(nwavesx,ny,2)

      npts = ny
      ndata = nwavesx

!----------------------
!==> forward transform.
!----------------------
      if (idir==+1) then
!
!==> copy the data into the work array.
!    
         do 10 j=1,npts
         do 10 i=1,ndata
            a(i,j,1) = real(data(i,j))
            a(i,j,2) = aimag(data(i,j))
10       continue
!
         call fft2(a,c,npts,npts-1,nfax0,ifax0,-1,trigy,ndata)
!
         do 20 j=1,nwavesy
         do 20 i=1,ndata
            jj = indxy(j)
            coeff(i,j) = cmplx(c(i,jj,1),c(i,jj,2))
20       continue
!----------------------
!==> inverse transform.
!----------------------
      else if (idir==-1) then
!
         do 25 j=1,npts
         do 25 i=1,ndata
            c(i,j,1) = zero
            c(i,j,2) = zero
25       continue
         do 30 j=1,nwavesy
         do 30 i=1,ndata
            jj = indxy(j)
            c(i,jj,1) = real(coeff(i,j))
            c(i,jj,2) = aimag(coeff(i,j))
30       continue
!
         call fft2(c,a,npts,npts-1,nfax0,ifax0,+1,trigy,ndata)
!
         do 40 j=1,npts
         do 40 i=1,ndata
            data(i,j) = cmplx(a(i,j,1),a(i,j,2))
40       continue
!
      else
         write(6,*) ' idir must be +1 or -1 in cfft!'
         write(6,*) ' execution terminated.'
         stop
      end if
!
      return
end subroutine cfft
!
      SUBROUTINE FFT2(A,C,N,NDIM,NFAX,IFAX,ISIGN,TRIG,LEN)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    FFT2
!   prgmmr:
!
! abstract: PERFORMS A COMPLEX FFT ON MULTIPLE DATA IN A
!           AND RETURNS THE RESULT IN C
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    A                      - INPUT ARRAY (DESTROYED DURING CALCULATION).
!                             DIMENSIONED AS A(LEN,0;NDIM,2), WHERE THE 1ST INDEX
!                             LABELS DISTINCT DATA TO BE TRANSFORMED, & 2ND INDEX LABELS
!                             THE N POINTS TO BE TRANSFORMED, & 3RD INDEX  LABELS
!                             REAL (1) OR IMAGINARY (2) PARTS
!    N                      - NO. OF POINTS IN TRANSFORM DIRECTION, MUST HAVE ONLY
!                             PRIME FACTORS OF 2 & 3
!    NDIM                   - 2ND DIMENSION OF A & C
!    NFAX                   - NO. OF PRIME FACTORS OF N
!    IFAX
!    ISIGN                  - SET ISIGN = -1 TO COMPUTE FOURIER COEFFICIENTS,
!                                       = +1 TO COMPUTE GRID VALUES.
!    TRIG                   -  ARRAY CONTAINING TRIGONOMETRIC FACTORS:
!                              DIMENSIONED TRIG(2,0:N-1)
!                                       TRIG(1,J) = COS (2.*PI*J/N)
!                                       TRIG(2,J) = SIN (2.*PI*J/N)
!    LEN                    - NO. OF DISTINCT TRANSFORMS TO BE PERFORMED.
!
!   output argument list:
!    C                      - OUTPUT ARRAY (MUST BE DISTINCT FROM INPUT ARRAY),
!                             DIMENSIONED THE SAME AS ARRAY A
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      INTEGER(i_kind),intent(in   ) :: IFAX(*),LEN,NDIM,NFAX,ISIGN,N
      REAL(r_kind)   ,intent(inout) :: A(LEN,0:NDIM,2),TRIG(2,0:N-1)
      REAL(r_kind)   ,intent(inout) :: C(LEN,0:NDIM,2)

      INTEGER(i_kind) I,IJ,LA,IFAC
      REAL(r_kind) XNI
      LOGICAL ODD

      REAL(r_kind),PARAMETER::PI=3.1415926535898_r_kind
!
      LA=1  
      ODD=.TRUE.
      DO 10 I=1,NFAX  
         IFAC=IFAX(I)
         IF (ODD) THEN 
            CALL PASS2(A,C,N,NDIM,ISIGN,IFAC,LA,TRIG,LEN)
         ELSE  
            CALL PASS2(C,A,N,NDIM,ISIGN,IFAC,LA,TRIG,LEN)
         END IF
         ODD=.NOT.ODD
         LA=LA*IFAC
   10 CONTINUE
!
      IF (ODD) THEN 
         DO 30 I=0,N-1
            DO 20 IJ=1,LEN
               C(IJ,I,1) = A(IJ,I,1)
               C(IJ,I,2) = A(IJ,I,2)
   20       CONTINUE  
   30    CONTINUE
      END IF
      IF (ISIGN==-1) THEN
         XNI=one/N
         DO 50 I=0,N-1
            DO 40 IJ=1,LEN
               C(IJ,I,1) = XNI * C(IJ,I,1)
               C(IJ,I,2) = XNI * C(IJ,I,2)
   40       CONTINUE 
   50    CONTINUE
      END IF
      RETURN
END SUBROUTINE FFT2
!  ------------------------------------------------------------
      SUBROUTINE PASS2(A,C,N,NDIM,ISIGN,IFAC,LA,TRIG,LEN)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    PASS2
!   prgmmr:
!
! abstract: PERFORMS ONE PASS OF  FFT2
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    A
!    N
!    NDIM
!    ISIGN
!    IFAC
!    LA
!    TRIG
!    LEN
!
!   output argument list:
!    C
!
! note:
!
!  THIS ROUTINE IS NEVER CALLED DIRECTLY BY THE USER.
!  THE ARGUMENTS ARE SIMILAR TO THOSE OF FFT2.
!
!  THE INNER LOOPS IN THIS SUBROUTINE (THOSE OVER THE INDEX IJ)
!  EXTEND OVER PART OF THE 2ND DIMENSIONS OF THE ARRAYS A & C.
!  THIS PRODUCES LONGER VECTOR LENGTHS.
!
!   THE FOLLOWING CONSTANTS PRESUME A 64-BIT MACHINE. THEY ARE
!   SINES OF 45 AND 60 DEGREES.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      INTEGER(i_kind),intent(in   ) :: N,NDIM,ISIGN,IFAC,LA,LEN
      REAL(r_kind)   ,intent(in   ) :: TRIG(2,0:N-1)
      REAL(r_kind)   ,intent(inout) :: A(LEN,0:NDIM,2),C(LEN,0:NDIM,2)

      INTEGER(i_kind) I,I0,I01,I1,I11,J,J0,J01,J1,J11,K,M,LLA,IND(0:20),JND(0:20),JUMP,IJ,IJ1
      INTEGER(i_kind) I2,I21,J2,J21
      REAL(r_kind) SN60,CC,SS,AM1,AM2,AP1,AP2,TA1,TA2,C1,C2,S1,S2,T1,T2

      REAL(r_kind),PARAMETER::ASN60 = half*1.732050807569_r_kind

      SN60=ISIGN * ASN60
      M = N/IFAC
!
!
!     SET UP INDEXING  
!
      DO 10 K=0,IFAC-1
         IND(K) = K*M
         JND(K) = K*LA
   10 CONTINUE
      LLA =LA * LEN 
!
!     PERFORM THE ARITHMETIC
!
      I = 0
      J = 0
      JUMP = (IFAC-1) * LA
      DO 130 K = 0,M-LA,LA
         IF (IFAC==2) THEN
            I0 = IND(0) + I
            I1 = IND(1) + I
            J0 = JND(0) + J
            J1 = JND(1) + J
            CC = TRIG(1,K)
            SS = ISIGN * TRIG(2,K)
            IF (K==0) THEN
               DO 20 IJ = 1,LLA
                  if(ij>len) then
                     ij1=mod(ij-1,len)+1
                     i01=(ij-1)/len+i0; i11=(ij-1)/len+i1
                     j01=(ij-1)/len+j0; j11=(ij-1)/len+j1
                     if(i01<=ndim.and.i11<=ndim.and.&
                        j01<=ndim.and.j11<=ndim) then
                        C(IJ1,J01,1) = A(IJ1,I01,1) + A(IJ1,I11,1)
                        C(IJ1,J01,2) = A(IJ1,I01,2) + A(IJ1,I11,2)
                        C(IJ1,J11,1) = A(IJ1,I01,1) - A(IJ1,I11,1)
                        C(IJ1,J11,2) = A(IJ1,I01,2) - A(IJ1,I11,2)
                     end if
                  else
                     C(IJ,J0,1) = A(IJ,I0,1) + A(IJ,I1,1)
                     C(IJ,J0,2) = A(IJ,I0,2) + A(IJ,I1,2)
                     C(IJ,J1,1) = A(IJ,I0,1) - A(IJ,I1,1)
                     C(IJ,J1,2) = A(IJ,I0,2) - A(IJ,I1,2)
                  end if
         20    CONTINUE
            ELSE
               DO 50 IJ = 1,LLA  
                  if(ij>len) then
                     ij1=mod(ij-1,len)+1
                     i01=(ij-1)/len+i0; i11=(ij-1)/len+i1
                     j01=(ij-1)/len+j0; j11=(ij-1)/len+j1
                     if(i01<=ndim.and.i11<=ndim.and.&
                        j01<=ndim.and.j11<=ndim) then
                        C(IJ1,J01,1) = A(IJ1,I01,1) + A(IJ1,I11,1)
                        C(IJ1,J01,2) = A(IJ1,I01,2) + A(IJ1,I11,2)
                        AM1 = A(IJ1,I01,1) - A(IJ1,I11,1)
                        AM2 = A(IJ1,I01,2) - A(IJ1,I11,2)
                        C(IJ1,J11,1) = CC * AM1 - SS * AM2
                        C(IJ1,J11,2) = SS * AM1 + CC * AM2
                     end if
                  else
                     C(IJ,J0,1) = A(IJ,I0,1) + A(IJ,I1,1)
                     C(IJ,J0,2) = A(IJ,I0,2) + A(IJ,I1,2)
                     AM1 = A(IJ,I0,1) - A(IJ,I1,1)
                     AM2 = A(IJ,I0,2) - A(IJ,I1,2)
                     C(IJ,J1,1) = CC * AM1 - SS * AM2
                     C(IJ,J1,2) = SS * AM1 + CC * AM2
                  end if
            50 CONTINUE
            END IF
         ELSEIF (IFAC==3) THEN  
            I0 = IND(0) + I
            I1 = IND(1) + I
            I2 = IND(2) + I
            J0 = JND(0) + J
            J1 = JND(1) + J
            J2 = JND(2) + J
            IF (K==0) THEN
               DO 60 IJ = 1,LLA  
                  if(ij>len) then
                     ij1=mod(ij-1,len)+1
                     i01=(ij-1)/len+i0; i11=(ij-1)/len+i1; i21=(ij-1)/len+i2
                     j01=(ij-1)/len+j0; j11=(ij-1)/len+j1; j21=(ij-1)/len+j2
                     if(i01<=ndim.and.i11<=ndim.and.i21<=ndim.and. &
                        j01<=ndim.and.j11<=ndim.and.i21<=ndim) then
                        AP1 = A(IJ1,I11,1) + A(IJ1,I21,1)
                        AP2 = A(IJ1,I11,2) + A(IJ1,I21,2)
                        C(IJ1,J01,1) = A(IJ1,I01,1) + AP1
                        C(IJ1,J01,2) = A(IJ1,I01,2) + AP2
                        TA1 = A(IJ1,I01,1) - half * AP1
                        TA2 = A(IJ1,I01,2) - half * AP2
                        AM1 = SN60 * (A(IJ1,I11,1) - A(IJ1,I21,1))
                        AM2 = SN60 * (A(IJ1,I11,2) - A(IJ1,I21,2))
                        C(IJ1,J11,1) = TA1 - AM2
                        C(IJ1,J11,2) = TA2 + AM1
                        C(IJ1,J21,1) = TA1 + AM2
                        C(IJ1,J21,2) = TA2 - AM1
                     endif
                  else
                     AP1 = A(IJ,I1,1) + A(IJ,I2,1)
                     AP2 = A(IJ,I1,2) + A(IJ,I2,2)
                     C(IJ,J0,1) = A(IJ,I0,1) + AP1
                     C(IJ,J0,2) = A(IJ,I0,2) + AP2
                     TA1 = A(IJ,I0,1) - half * AP1
                     TA2 = A(IJ,I0,2) - half * AP2
                     AM1 = SN60 * (A(IJ,I1,1) - A(IJ,I2,1))
                     AM2 = SN60 * (A(IJ,I1,2) - A(IJ,I2,2))
                     C(IJ,J1,1) = TA1 - AM2
                     C(IJ,J1,2) = TA2 + AM1
                     C(IJ,J2,1) = TA1 + AM2
                     C(IJ,J2,2) = TA2 - AM1
                  end if
            60 CONTINUE
            ELSE
               C1 = TRIG(1,K)
               C2 = TRIG(1,2*K)
               S1 = ISIGN * TRIG(2,K)
               S2 = ISIGN * TRIG(2,2*K)
               DO 70 IJ = 1,LLA  
                  if(ij>len) then
                     ij1=mod(ij-1,len)+1
                     i01=(ij-1)/len+i0; i11=(ij-1)/len+i1; i21=(ij-1)/len+i2
                     j01=(ij-1)/len+j0; j11=(ij-1)/len+j1; j21=(ij-1)/len+j2
                     if(i01<=ndim.and.i11<=ndim.and.i21<=ndim.and. &
                        j01<=ndim.and.j11<=ndim.and.i21<=ndim) then
                        AP1 = A(IJ1,I11,1) +A(IJ1,I21,1)
                        AP2 = A(IJ1,I11,2) +A(IJ1,I21,2)
                        C(IJ1,J01,1) = A(IJ1,I01,1) +AP1
                        C(IJ1,J01,2) = A(IJ1,I01,2) +AP2
                        TA1 = A(IJ1,I01,1) - half*AP1
                        TA2 = A(IJ1,I01,2) - half*AP2
                        AM1 = SN60 * (A(IJ1,I11,1) - A(IJ1,I21,1))
                        AM2 = SN60 * (A(IJ1,I11,2) - A(IJ1,I21,2))
                        T1 = TA1 - AM2
                        T2 = TA2 + AM1
                        C(IJ1,J11,1) = C1 * T1 - S1 * T2
                        C(IJ1,J11,2) = S1 * T1 + C1 *T2
                        T1 = TA1 + AM2
                        T2 = TA2 - AM1
                        C(IJ1,J21,1) = C2 * T1 - S2 * T2
                        C(IJ1,J21,2) = S2 * T1 + C2 * T2
                     endif
                  else
                     AP1 = A(IJ,I1,1) +A(IJ,I2,1)
                     AP2 = A(IJ,I1,2) +A(IJ,I2,2)
                     C(IJ,J0,1) = A(IJ,I0,1) +AP1
                     C(IJ,J0,2) = A(IJ,I0,2) +AP2
                     TA1 = A(IJ,I0,1) - half*AP1
                     TA2 = A(IJ,I0,2) - half*AP2
                     AM1 = SN60 * (A(IJ,I1,1) - A(IJ,I2,1))
                     AM2 = SN60 * (A(IJ,I1,2) - A(IJ,I2,2))
                     T1 = TA1 - AM2
                     T2 = TA2 + AM1
                     C(IJ,J1,1) = C1 * T1 - S1 * T2
                     C(IJ,J1,2) = S1 * T1 + C1 *T2
                     T1 = TA1 + AM2
                     T2 = TA2 - AM1
                     C(IJ,J2,1) = C2 * T1 - S2 * T2
                     C(IJ,J2,2) = S2 * T1 + C2 * T2
                  end if
         70    CONTINUE
            END IF
         END IF
         I = I+LA
         J = J+LA
         J = J+JUMP
  130 CONTINUE
      RETURN
END SUBROUTINE PASS2 
!-------------------------------------------------------------
      SUBROUTINE PREFFT (N,NFAX,IFAX,TRIG)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    PREFFT
!   prgmmr:
!
! abstract: C0MPUTES PRELIMINARY QUANTITIES FOR FFT ROUTINES
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    N                     - NO. OF POINTS IN DATA, MUST HAVE PRIME FACTORS 2 & 3
!    TRIG                  - ARRAY CONTAINING TRIGONOMETRIC FACTORS, DIMNSN (2,0;N-1)
!                            TRIG(1,J) = COS (2*PI*J/N)
!                            TRIG(2,J) = SIN (2.*PI*J/N)
!
!   output argument list:
!    NFAX                  - NO. OF PRIME FACTORS OF N (OUTPUT)
!    IFAX                  - ARRAY CONTAINING PRIME FACTORS OF N (OUTPUT)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      INTEGER(i_kind),intent(in   ) :: N
      REAL(r_kind)   ,intent(inout) :: TRIG(2,0:N-1)

      INTEGER(i_kind),intent(  out) :: IFAX(*),NFAX

      INTEGER(i_kind) K
      REAL(r_kind) ARG
      REAL(r_kind),PARAMETER::PI=3.1415926535898_r_kind

      CALL FACTOR (N,NFAX,IFAX)
      DO 10 K=0,N-1
         ARG = two*PI*K/N
         TRIG(1,K) = COS(ARG)
         TRIG(2,K) = SIN(ARG)
   10 CONTINUE
      RETURN
END SUBROUTINE PREFFT 
!  --------------------------------------------------------------
      SUBROUTINE FACTOR (N,NFAX,IFAX)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    FACTOR
!   prgmmr:
!
! abstract: COMPUTES THE FACTORS OF N
!
! program history log:
!   2009-10-02  lueken - added subprogram doc block
!
!   input argument list:
!    N
!
!   output argument list:
!    NFAX
!    IFAX
!
! note:
!
!    THIS ROUTINE IS NOT CALLED DIRECTLY BY THE USER
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      implicit none

      INTEGER(i_kind),intent(in   ) :: N
      INTEGER(i_kind),intent(  out) :: IFAX(*),NFAX

      INTEGER(i_kind) NN,II

      NFAX = 0
      NN= N

!     EXTRACT FACTORS OF 3
      DO 10 II = 1,20
         IF (NN==3*(NN/3)) THEN
            NFAX = NFAX+1
            IFAX(NFAX) = 3
            NN = NN/3
         ELSE
            EXIT
         END IF
   10 CONTINUE
!     EXTRACT FACTORS OF 2
      DO 30 II = NFAX+1,20
         IF (NN==2*(NN/2)) THEN
            NFAX = NFAX +1
            IFAX(NFAX) =2
            NN = NN/2
         ELSE
            EXIT
         END IF
   30 CONTINUE
      IF (NN/=1) THEN
         write(6,*) 'PORRA 4'
         STOP
      END IF
      RETURN
END SUBROUTINE FACTOR
!=======================================================================
!=======================================================================
end module wind_fft
