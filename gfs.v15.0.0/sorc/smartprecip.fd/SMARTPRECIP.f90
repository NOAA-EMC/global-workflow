       PROGRAM SMARTPRECP
!                .      .    .                                       .
! SUBPROGRAM:    SMARTPECIP
!   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  07-03-07

! ABSTRACT: PRODUCES 3,6 or 12-HOUR TOTAL AND CONVECTIVE PRECIPITATION BUCKETS
!              AS WELL AS SNOWFALL ON THE ETA NATIVE GRID FOR SMARTINIT 

! PROGRAM HISTORY LOG:
!   07-03-07  GEOFF MANIKIN 
!   10-25-12  JEFF MCQUEEN
! REMARKS:
!   10-25-12 JTM UNIFIED make and add precip for different accum hours
!                addprecip6, addprecip12 and makeprecip all combined in
!                smartprecip
!                To call, must set all 4 fhrs
!                for 3 or 6 hour buckets, set fhr3,fh4 to -99
!                For 12 hour buckets: 
!                    smartprecip  fhr fhr-3 fhr-6 fhr-9 
!  02-20-14 JTM Added DGEX option addsub to compute 6 hr precip
!               from 3 dgex files
!               fhr1 (fhr) 3 hr precip
!               fhr2 (fh3 old) 6 hr precip
!               fhr3 (fh6 old) 3 hr precip
!                 then mk6p=fhr+(fhr3-fhr6) for DGEX
!               Added DGEX 12 hr precip option
!               fhr1=fhr6 6hr precip
!               fhr2=fhr  6hr precip
!                 12hrp=fhr6 + fhr

! ATTRIBUTES:
!   LANGUAGE: FORTRAN-90
!   MACHINE:  WCOSS     
!======================================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER SHR1,FHR1, FHR2, FHR3, FHR4
      CHARACTER*80 FNAME
      LOGICAL*1 LSUB, MK3P,MK6P,MK12P,LADDSUB,LRD3,LRD4

      REAL,     ALLOCATABLE :: GRID(:)
      REAL,     ALLOCATABLE :: APCP1(:),APCP2(:),APCP3(:),APCP4(:)
      REAL,     ALLOCATABLE :: CAPCP1(:),CAPCP2(:),CAPCP3(:),CAPCP4(:)
      REAL,     ALLOCATABLE :: SNOW1(:),SNOW2(:),SNOW3(:),SNOW4(:)
      REAL,     ALLOCATABLE :: APCPOUT(:),CAPCPOUT(:),SNOWOUT(:)
      LOGICAL,  ALLOCATABLE :: MASK(:)
!--------------------------------------------------------------------------
      INTERFACE
      SUBROUTINE RDHDRS(LUB,LUI,JPDS,JGDS,IGDN,IMAX,JMAX,KMAX,NUMV)
      INTEGER,  INTENT(IN)     :: lub,lui                  ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: igdn,imax,jmax,kmax,numv ! grid size
      END SUBROUTINE rdhdrs
!--------------------------------------------------------------------------
      SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
      INTEGER,  INTENT(IN)     :: lub,lui,numv             ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: k,kf,kpds(200),kgds(200) ! grid info
      INTEGER,  INTENT(OUT)    :: iret,istat               ! grid info
      LOGICAL,  INTENT(INOUT)  :: MASK(:)                ! L/S mask 
      REAL,     INTENT(INOUT)  :: GRID(:)                  ! grib data
      REAL,     INTENT(OUT)    :: VARB(:)                  ! output varb
      END SUBROUTINE setvar
      END INTERFACE
!--------------------------------------------------------------------------

      FNAME='fort.  '
!====================================================================
!     FHR3 = -99 signals a 6 hour summation requested
!     FHR4 GT 00 signals a 12 hour summation requested
!     FHR3 GT 0 but FHR4 = -99 signals a 6 hour summation For DGEX (fhr1+(fhr2-fh3))
!     FHR1 GT FHR2 signals do a 3 hour subtraction of files
!     FHR2 GT FHR1 signals do an addition (eg: DGEX, fhr1+fhr2=6h pr)
!====================================================================
      READ (5,*) FHR1, FHR2,FHR3,FHR4
      print *,' SMARTPRECIP ', FHR1,FHR2,FHR3,FHR4

      LRD3=.FALSE.;LRD4=.FALSE.
!==>  Make 3 hour buckets by subtracting the 1st file from the 2nd
      LSUB=.FALSE.
      LADDSUB=.FALSE.
      IF (FHR1.GT.FHR2) THEN
        MK3P=.TRUE.
        SHR1=FHR2
        FHR2=FHR1   ! T
        FHR1=SHR1   ! T-3
        IF (FHR3.GT.0) SHR1=FHR3  ! DGEX, T-6
        LSUB=.TRUE.

!==>    Set SHR1 to read NAM Accumulated snowfall bucket : 3,6,9 or 12 hr snow
!         =initial hour for snow bucket (T-3, -6, -9, -12
        if (FHR3 .lt. 0) then    ! For NON-DGEX grids
!         NAM has 12 hr snow buckets at 00 and 12 UTC Valid times
          IF (MOD(FHR2,12).EQ. 0.) THEN   
            SHR1=FHR2-12
          ELSE
            SHR1=FHR2-MOD(FHR2,12) 
          ENDIF
        else
          SHR1=FHR2-6
        endif
        print *, 'SUB: Create 3 hr precip from two files'
        print *, ' FHR1=', FHR1,' FHR2=',FHR2,' Snow SHR1=',SHR1

      ELSE

!==>    ADD or ADDSUB: Make 6 hr precip 
        LSUB=.FALSE.
        if (FHR4.LT.0) then 
          MK6P=.TRUE.
          if (FHR3 .LT. 0) then   ! NAM grid
            SHR1=FHR1-3  ! T-6
            print *,'ADD: Create 6 hr precip from two files: '
            print *,'FHR1=',FHR1,' FHR2=',FHR2,'Snow SHR1=',SHR1
          else
            SHR1=FHR1-3  ! T-3
            LADDSUB=.TRUE.  ! T-6   DGEX grid, 3rd file 
            LRD3=.TRUE.
            print *,'ADDSUB: Create 6 hr precip from 3 files:'
            print *, 'FHR3:',FHR3,'+ (FHR2:',FHR2,' - FHR1:',FHR1,')'
          endif

        else 

!==>      ADD: make 12 hr precip 
          MK12P=.TRUE.
          if ( FHR3 .GT. FHR2 ) Then
            SHR1=FHR1-3  ! T-12
            print *, 'ADD: Create 12 hr precip from four 3-hr buckets'
            print *, 'FHR1=',FHR1,' FHR2=',FHR2,' FHR3=',FHR3,' FHR4=',FHR4
            print *, 'Snow SHR1=',SHR1
            LRD3=.TRUE.;LRD4=.TRUE.
          else
            SHR1=FHR1-6  ! T-12, Add 2 6 hr precip buckets
            print *, 'ADD: Create 12 hr precip from two 6-hr buckets'  
            print *, 'FHR1=',FHR1,' FHR2=',FHR2
            print *, 'Snow SHR1=',SHR1
            LRD3=.FALSE.;LRD4=.FALSE.
          endif
        endif
      ENDIF

      LUGB=13;LUGI=14; LUGB2=15;LUGI2=16
      LUGB3=17;LUGI3=18;LUGB4=19;LUGI4=20
      LUGB5=50; LUGB6=51; LUGB7=52
      ISTAT = 0

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS 
!=======================================================
      CALL RDHDRS(LUGB,LUGI,JPDS,JGDS,IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

! -== GET PRECIP FIELDS ==-

      ALLOCATE (MASK(NUMVAL),GRID(NUMVAL),STAT=kret)
      ALLOCATE (APCP1(NUMVAL),CAPCP1(NUMVAL),SNOW1(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!   1st PRECIP FILE
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      print *;print *,'FHR1= ',FHR1, ' READ 1st PRECIP FILE ', LUGB,LUGI
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,      &
                K,KPDS,KGDS,MASK,GRID,APCP1,IRET,ISTAT)

!  CONVECTIVE PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,       &
                 K,KPDS,KGDS,MASK,GRID,CAPCP1,IRET,ISTAT)

!  1st SNOWFALL File
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
      JPDS(14) = SHR1   ! T-6 or T-3 accumulated snow
      JPDS(15) = FHR1   ! T
      print *,'FHRS ',JPDS(14),JPDS(15),' READ 1st SNOW FILE ', LUGB,LUGI
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,SNOW1,IRET,ISTAT)

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS for 2nd file
!=======================================================
      print *;print *,'RD 2nd File', FHR2,LUGB2,LUGI2
      CALL RDHDRS(LUGB2,LUGI2,JPDS,JGDS,IGDNUM,IMAX,JMAX,KMAX,NUMVAL)
      JGDS=-1

      ALLOCATE (APCP2(NUMVAL),CAPCP2(NUMVAL),SNOW2(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP 
      J = -1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1 
      print *;print *,' FHR2= ',FHR2,' READ 2nd PRECIP FILE ', LUGB2,LUGI2
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,APCP2,IRET,ISTAT)

!     ACCUMULATED CONVECTIVE PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,CAPCP2,IRET,ISTAT)

!     2nd SNOWFALL File
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
      JPDS(14) = FHR1  
      JPDS(15) = FHR2 
      IF (LSUB) JPDS(14)=SHR1   ! T-3  Shouldnt need this ??
      print *,'FHRS ',JPDS(14),JPDS(15),' READ 2nd SNOW FILE ', LUGB2,LUGI2
      CALL SETVAR(LUGB2,LUGI2,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,SNOW2,IRET,ISTAT)

      IF (LRD3) THEN
!=======================================================
!  READ INDEX FILE TO GET GRID SPECS for 3rd file
!  For 12 hr precip summations and DGEX 6 hr calculations
!=======================================================
      CALL RDHDRS(LUGB3,LUGI3,JPDS,JGDS,              &
                  IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

      ALLOCATE (APCP3(NUMVAL),CAPCP3(NUMVAL),SNOW3(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP 
      J = 1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      print *;print *,'FHR3=',FHR3,' READ 3rd PRECIP FILE ', LUGB3,LUGI3
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,APCP3,IRET,ISTAT)

!     ACCUMULATED CONVECTIVE PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,CAPCP3,IRET,ISTAT)

!     3rd SNOWFALL File (for NAM 12 hour or DGEX 6 hour buckets)
      J = 0 ;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        JPDS(14) = FHR2  ! T-6
        JPDS(15) = FHR3  ! T-3
      print *,JPDS(14),JPDS(15),' READ 3rd SNOW FILE ', LUGB3,LUGI3
      CALL SETVAR(LUGB3,LUGI3,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,SNOW3,IRET,ISTAT)
      ENDIF

!=======================================================
!  READ INDEX FILE TO GET GRID SPECS for 4th file (FOR 12 hour NAM precip)
!=======================================================
      IF (MK12P .and. LRD4) THEN
      CALL RDHDRS(LUGB4,LUGI4,JPDS,JGDS,                  &
                  IGDNUM,IMAX,JMAX,KMAX,NUMVAL)

      ALLOCATE (APCP4(NUMVAL),CAPCP4(NUMVAL),SNOW4(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP 
      J = 1;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 061;JPDS(6) = 001
      JPDS(13) = 1
      print *;print *,'FHR4=',FHR4,' READ 4th PRECIP FILE ', LUGB4,LUGI4
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,     &
                 K,KPDS,KGDS,MASK,GRID,APCP4,IRET,ISTAT)

!     ACCUMULATED CONVECTIVE PRECIP
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 063;JPDS(6) = 001
      JPDS(13) = 1
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,       &
                 K,KPDS,KGDS,MASK,GRID,CAPCP4,IRET,ISTAT)

!     4th SNOWFALL File
      J = 0 ;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        JPDS(14) = FHR3  ! T-3
        JPDS(15) = FHR4  ! T
      print *,JPDS(14),JPDS(15),' READ 4th SNOW FILE ', LUGB4,LUGI4
      CALL SETVAR(LUGB4,LUGI4,NUMVAL,J,JPDS,JGDS,KF,      &
                 K,KPDS,KGDS,MASK,GRID,SNOW4,IRET,ISTAT)

      ENDIF 

!=======================================================
!     OUTPUT 3, 6 or 12 hr PRECIP BUCKETS
!=======================================================
      ALLOCATE (APCPOUT(NUMVAL),CAPCPOUT(NUMVAL),SNOWOUT(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

      print *
      IF (LSUB) THEN
       APCPOUT=APCP2-APCP1
       CAPCPOUT=CAPCP2-CAPCP1
       SNOWOUT=SNOW2-SNOW1
       KPDS(14)=FHR1
       KPDS(15)=FHR2
       print *, 'OUTPUT 3 HR PRECIP: SUB ', FHR1,FHR2, maxval(apcpout)

      ELSE
        APCPOUT=APCP2+APCP1
        CAPCPOUT=CAPCP2+CAPCP1
        SNOWOUT=SNOW2+SNOW1

!       6 hr precip 
        IF (MK6P) THEN
          KPDS(14)=FHR3
          IF (LADDSUB) THEN   
            KPDS(15)=FHR1
            APCPOUT=APCP3+(APCP2-APCP1)
            CAPCOUT=CAPC3+(CAPC2-CAPC1)
            SNOWOUT=SNOW3+(SNOW2-SNOW1)
            print *, 'OUTPUT 06 HR PRECIP: ADDSUB',FHR1,FHR2,FHR3,maxval(apcpout)
          ELSE
            KPDS(15)=FHR2
            print *,'OUTPUT 6 HR PRECIP: ADD', FHR1,FHR2,maxval(apcpout)
          ENDIF
        ENDIF
 
!       12 hr precip
        IF (MK12P) THEN
          KPDS(14)=SHR1
          KPDS(15)=FHR4
          IF (LRD4) THEN
            APCPOUT=APCPOUT+APCP3+APCP4
            CAPCPOUT=CAPCPOUT+CAPCP3+CAPCP4
            SNOWOUT=SNOWOUT+SNOW3+SNOW4
            print *, ' OUTPUT 12 HR PRECIP: ADD 4 ',FHR1,FHR2,FHR3,FHR4,maxval(apcpout)
          ELSE
            print *, ' OUTPUT 12 HR PRECIP: ADD 2 ',FHR1,FHR2,maxval(apcpout)
          ENDIF
        ENDIF

      ENDIF


      KPDS(5)=61
      print *, 'writing precip', KPDS(5),KPDS(14),KPDS(15),LUGB5,MAXVAL(APCPOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB5
      CALL BAOPEN(LUGB5,FNAME,IRETGB)
      CALL PUTGB(LUGB5,NUMVAL,KPDS,KGDS,MASK,APCPOUT,IRET)
      CALL BACLOSE(LUGB5,IRET)

      KPDS(5)=63
      print *, 'writing CAPCP', KPDS(5),KPDS(14),KPDS(15),LUGB6 , MAXVAL(CAPCPOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB6
      CALL BAOPEN(LUGB6,FNAME,IRET)
      CALL PUTGB(LUGB6,NUMVAL,KPDS,KGDS,MASK,CAPCPOUT,IRET)
      CALL BACLOSE(LUGB6,IRET)

      KPDS(5)=65
      print *, 'writing SNOW', KPDS(5),KPDS(14),KPDS(15),LUGB7, MAXVAL(SNOWOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB7
      CALL BAOPEN(LUGB7,FNAME,IRET)
      CALL PUTGB(LUGB7,NUMVAL,KPDS,KGDS,MASK,SNOWOUT,IRET)
      CALL BACLOSE(LUGB7,IRET)

      STOP
      END

      SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field and initializes a 2-D variable
!     Requested from w3lib GETGRB routine
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================
      INTEGER,  INTENT(IN)     :: lub,lui,numv             ! unit numbers
      INTEGER,  INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,  INTENT(OUT)    :: k,kf,kpds(200),kgds(200) ! grid info
      INTEGER,  INTENT(OUT)    :: iret,istat               ! grid info
      LOGICAL,  INTENT(INOUT) :: MASK(:)                  ! L/S mask 
      REAL,     INTENT(INOUT)  :: GRID(:)                  ! grib data
      REAL,     INTENT(OUT)    :: VARB(:)                  ! output varb

!     Get GRIB Variable
      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, NUMV
          VARB(KK) = GRID(KK)
        ENDDO
        WRITE(6,100) JPDS(5),JPDS(6),JPDS(7),J,MAXVAL(VARB),KF,K
 100    FORMAT('VARB UNPACKED ', 4I7,G12.4,I8, ' RECORD',I5)
      ELSE
        WRITE(6,*)'===================================================='
        WRITE(6,*)'SETVAR : COULD NOT UNPACK VARB',JPDS(5),JPDS(6)
        WRITE(6,*)' J ',J,' GRID ',JPDS(3),'GETGB RETURN CODE',IRET
        WRITE(6,*)'UNIT', LUB,LUI,'NUMVAL ', NUMV,KF,' RECORD',K
        WRITE(6,*)'===================================================='
        print *, 'JPDS', JPDS(1:25)
        print *, 'JGDS', JGDS(1:25)
        ISTAT = IRET

! GUAM only has accumulated precip, not convective precip or snow
!FOR GUAM        STOP 'UNIFPRECIP ABORT:   VARB NOT UNPACKED'
      ENDIF

      RETURN
      END

      SUBROUTINE RDHDRS(LUB,LUI,JPDS,JGDS,IGDN,IMAX,JMAX,KMAX,NUMV)
!=============================================================================
!     This Routine Reads GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!
!     10-2012  Jeff McQueen
!==============================================================================
      INTEGER,    INTENT(IN)     :: lub,lui                  ! unit numbers
      INTEGER,    INTENT(INOUT)  :: jpds(200),jgds(200)      ! grib parms
      INTEGER,    INTENT(OUT)    :: igdn,imax,jmax,kmax,numv ! grid size

      INTEGER,PARAMETER :: MBUF = 2000000 !Character length of bufr varb
      CHARACTER*80 FNAME
      CHARACTER CBUF(MBUF)
      INTEGER KPDS(200),KGDS(200)
      INTEGER JENS(200),KENS(200)

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPEN(LUB,FNAME,IRETGB)
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPEN(LUI,FNAME,IRETGI)

      CALL GETGI(LUI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,' UNIT ',LUB,LUI,' NLEN',NLEN
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT',IRGI
        ISTAT = IRGI
        STOP 'RDHDRS ABORT GETGI'
      ENDIF

       DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,    &
                     KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT',IRGS
          ISTAT = IRGS
          STOP 'RDHDRS ABORT: GETBS1S'
        ENDIF
        IGDN = KPDS(3)
        IMAX = KGDS(2)
        JMAX = KGDS(3)
        NUMV = IMAX*JMAX
        KMAX = 0    ! HARDWIRED FOR PRECIP
      ENDDO

      WRITE(6,280) IGDN,JPDS(4),JPDS(5),IMAX,JMAX,KMAX
  280 FORMAT(' IGDN, IMAX,JMAX,KMAX ',6I5)
      RETURN
      END
