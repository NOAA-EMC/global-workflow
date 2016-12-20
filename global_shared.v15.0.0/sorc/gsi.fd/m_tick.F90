module m_tick
!$$$ module documentation block
!           .      .    .                                       .
! module:   m_tick
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-01-08 todling - encapsulated in this module
!   2009-08-06 lueken  - added module doc block
!
! subroutines included:
!   sub tick
!   sub INCYMD
!   sub leap_year
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: i_kind

implicit none

! set default to private
  private
! set subroutines to public
  public :: tick

contains

#ifdef ibm_sp

  subroutine tick (nymd, nhms, ndt)
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   tick
!   prgmmr:
!
! abstract: Dummy routine for ibm_sp.
!
! program history log:
!   2009-08-06 lueken  - added subprogram doc block
!
!   input argument list:
!    ndt
!    nymd
!    nhms
!
!   output argument list:
!    nymd
!    nhms
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: ndt
  integer(i_kind),intent(inout) :: nymd
  integer(i_kind),intent(inout) :: nhms

  end subroutine tick

#else

      subroutine tick (nymd, nhms, ndt)
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   tick
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06 lueken  - added subprogram doc block
!
!   input argument list:
!    ndt
!    nymd
!    nhms
!
!   output argument list:
!    nymd
!    nhms
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

! Input:
      integer(i_kind),intent(in   ) :: ndt                     ! TIME-STEP
! Input/Output:
      integer(i_kind),intent(inout) :: nymd                 ! CURRENT YYYYMMDD
      integer(i_kind),intent(inout) :: nhms                 ! CURRENT HHMMSS
! Local:
      integer(i_kind) :: NSECF, NHMSF, NSEC, N

! Origin:     L.L. Takacs
! Revision:   S.-J. Lin Mar 2000

       NSECF(N)   = N/10000*3600 + MOD(N,10000)/100* 60 + MOD(N,100)
       NHMSF(N)   = N/3600*10000 + MOD(N,3600 )/ 60*100 + MOD(N, 60)

       NSEC = NSECF(NHMS) + ndt

       IF (NSEC>86400)  THEN
          DO WHILE (NSEC>86400)
             NSEC = NSEC - 86400
             NYMD = INCYMD (NYMD,1)
          ENDDO
       ENDIF

       IF (NSEC==86400)  THEN
          NSEC = 0
          NYMD = INCYMD (NYMD,1)
       ENDIF

       IF (NSEC < 0)  THEN
          DO WHILE (NSEC < 0)
             NSEC = 86400 + NSEC
             NYMD = INCYMD (NYMD,-1)
          ENDDO
       ENDIF

       NHMS = NHMSF (NSEC)
      return
      end subroutine tick

      integer(i_kind) FUNCTION INCYMD (NYMD,M)
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   INCYMD
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06 lueken  - added subprogram doc block
!
!   input argument list:
!    NYMD
!    M
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

!  PURPOSE
!     INCYMD:  NYMD CHANGED BY ONE DAY
!     MODYMD:  NYMD CONVERTED TO JULIAN DATE
!  DESCRIPTION OF PARAMETERS
!     NYMD     CURRENT DATE IN YYMMDD FORMAT
!     M        +/- 1 (DAY ADJUSTMENT)

      INTEGER(i_kind) NDPM(12)
      DATA    NDPM /31, 28, 31, 30, 31, 30, &
                    31, 31, 30, 31, 30, 31/
      INTEGER(i_kind),intent(in):: NYMD, M
      INTEGER(i_kind) NY, NM, ND

      NY = NYMD / 10000
      NM = MOD(NYMD,10000) / 100
      ND = MOD(NYMD,100) + M

      IF (ND==0) THEN
         NM = NM - 1
         IF (NM==0) THEN
            NM = 12
            NY = NY - 1
         ENDIF
         ND = NDPM(NM)
         IF (NM==2 .AND. leap_year(NY))  ND = 29
      ENDIF

      IF (ND==29 .AND. NM==2 .AND. leap_year(ny))  GO TO 20

      IF (ND>NDPM(NM)) THEN
         ND = 1
         NM = NM + 1
         IF (NM>12) THEN
            NM = 1
            NY = NY + 1
         ENDIF
      ENDIF

   20 CONTINUE
      INCYMD = NY*10000 + NM*100 + ND
      RETURN
      END function INCYMD

      logical function leap_year(ny)
!$$$ subprogram documentation block
!               .      .    .                                       .
! subprogram:   leap_year
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06 lueken  - added subprogram doc block
!
!   input argument list:
!    ny
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
!
! Determine if year ny is a leap year
!
! Author: S.-J. Lin
      implicit none

      integer(i_kind),intent(in   ) :: ny

      integer(i_kind) ny00

!
! No leap years prior to 1900
!
      parameter ( ny00 = 1900 )   ! The threshold for starting leap-year 

      if( mod(ny,4) == 0 .and. ny >= ny00 ) then
         leap_year = .true.
      else
         leap_year = .false.
      endif

      return 
      end function leap_year
#endif
end module m_tick
