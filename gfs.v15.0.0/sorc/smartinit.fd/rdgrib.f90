  MODULE rdgrib
  use grddef
!=======================================================================
!  routines to  read/write grib data 
!=======================================================================
     
   REAL,      ALLOCATABLE  :: GRID(:)
   LOGICAL*1, ALLOCATABLE  :: MASK(:)

contains
     SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field, GRID, and initializes a 2-D variable,
!     VARB.
!     CALLS: w3lib GETGB routine
!     INPUT 
!        LUB : Input GRIB GRID file unit number
!        LUI : Input GRIB GRID Index file unit number
!        NUMV: Total number of grid points (NX * NY)
!           J: Input record number to skip to 
!        JPDS: Array containing grid variable definition parms.
!              At minimum The following two ids should be defined
!               JPDS(5) = grib variable id  (see grib office note 388, Table 2)
!               JPDS(6) = grib vertical level type (see grib office note 388,
!               Table 3)
!        JGDS: Array containing grid definitions, default set = -1
!     OUTPUT: 
!       GRID, MASK : 1D nx*ny array of grib field and land sea mask
!       VARB       : 2-D grib field variable array (NX,NY)
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================

   REAL,      INTENT(INOUT)  :: GRID(:),VARB(:,:)
   LOGICAL*1, INTENT(INOUT)  :: MASK(:)
!-----------------------------------------------------------------------------------------
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

!     Get GRIB Variable
      
      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IMAX=KGDS(2)
      IF(IRET.EQ.0) THEN
        DO KK = 1, NUMV
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          VARB(M,N) = GRID(KK)
        ENDDO
       IF(JPDS(6).ne.109 .or. JPDS(6).eq.109.and.J.le.100) &
        WRITE(6,100) JPDS(5),JPDS(6),JPDS(7),J,MINVAL(VARB),MAXVAL(VARB)
 100   FORMAT('VARB UNPACKED ', 4I7,2F14.4)
      ELSE
       WRITE(6,*)'====================================================='
       WRITE(6,*)' GETGB ERROR: ',IRET
       WRITE(6,*)'COULD NOT UNPACK VARB FOR J= ',J,'GRID', JPDS(3)
       WRITE(6,*) 'VARB',JPDS(5),'LVL TYP',JPDS(6),'VERT LVL',JPDS(7)
       WRITE(6,*)'UNIT', LUB,LUI,'NUMV',NUMV,'KF',KF
       WRITE(6,*)'====================================================='
       print *,'JPDS',jpds(1:25)
       ISTAT = IRET
! 01-29-13 JTM : past hour 60 nam output only to level 35
       if (JPDS(5).eq.191 .or. JPDS(6).eq.109 .or. JPDS(6).eq.245) then 
         print *, 'GRIB VARB READ ERROR: program continuing'
!       else
!         STOP 'ABORT: GRIB VARB READ ERROR'
       endif
      ENDIF

      RETURN
      END SUBROUTINE setvar


      SUBROUTINE RDHDRS(LUB,LUI,IGDN,GDIN,NUMV)
      use grddef
!=============================================================
!     This Routine Reads a GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!     Calls : BAOPEN,GETGI and GETGB1S
!     INPUT: 
!       LUB,LUI --> Input grib file and grib index file unit numbers 
!     OUTPUT: 
!       IGDN    --> Grib Grid domain number  (KPDS(3), see on 388, Table B)
!       GDIN TYPE variable with following grid information defined in module
!       grddef
!       GDIN%NX --> number of X grid points: also KGDS(2)
!       GDIN%NY --> number of Y grid points: also KGDS(3)
!       NUMV    --> Total number of horizontal grid points
!
!     10-2012  Jeff McQueen
!=============================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      PARAMETER(MBUF=2000000)
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      INTEGER JENS(200),KENS(200)
      TYPE (GINFO)  ::  GDIN

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPENR(LUB,FNAME,IRETGB)
      print *,'BAOPENR',LUB,'IRET ',IRETGB
      if (iretgb.ne.0) then
        print *,' COULD NOT OPEN GRIB FILE UNIT ',LUB
        stop 99
      endif
   
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPENR(LUI,FNAME,IRETGI)
      print *,'BAOPENR',LUI,'IRET ',IRETGI
      if (iretgi.ne.0) print *,'COULD NOT OPEN INDEX FILE UNIT',LUI
      CALL GETGI(LUI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,LUB,LUI,NLEN,NNUM
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE '
        ISTAT = IRGI
!TEST        STOP 'ABORT RDHDRS: GRIB INDEX FILE READ ERROR '
      ENDIF


      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)

        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          WRITE(6,280) IGDN,JPDS(4),JPDS(5)
          ISTAT = IRGS
          STOP 'ABORT RDHDRS: GRIB HDR READ ERROR '
        ENDIF
        IGDN = KPDS(3)
        GDIN%IMAX = KGDS(2)
        GDIN%JMAX = KGDS(3)
        NUMV = GDIN%IMAX*GDIN%JMAX
      ENDDO

  280 FORMAT(' IGDN, IMAX,JMAX, ',4I5)
      RETURN
      END SUBROUTINE rdhdrs

  END MODULE rdgrib
