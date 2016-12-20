   SUBROUTINE GETGRIB(ISNOW,IZR,IIP,IRAIN,VEG,WETFRZ,  &
   P03M,P06M,P12M,SN03,SN06,S3REF01,S3REF10,S3REF50,S6REF01,  &
   S6REF10,S6REF50,S12REF01,S12REF10,S12REF50, THOLD,DHOLD,GDIN,VALIDPT)

    use grddef
    use aset3d
    use aset2d
    use rdgrib

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 06-09-14
!
! ABSTRACT:
!   READS GRIB FILE for smartinit downscaling

!   Precip read are conditioned on whether on or off cycle run
!   ON-CYCLE GRIB FILES have 6,12 hour precip buckets
!   OFF-CYCLE GRIB FILES have 3 hr precip buckets

! PROGRAM HISTORY LOG:
!   06-09-14  G MANIKIN  - ADAPT CODE TO NAM 
!   12-10-01  J.MCQUEEN  - Reduced code thru use of rdhdrs,setvar
!   subrountines
!   12-10-01  J.McQueen - Combined on and off-cycle reads into getgrib
!   13-07-15  J.McQueen - Modified to read pressure level fields over GM (non nam, hiresw model)

! USAGE:    CALL SMARTINIT 
!   INPUT ARGUMENT LIST:

!   OUTPUT ARGUMENT LIST:
!     NONE

!   OUTPUT FILES:
!     NONE

      TYPE (GINFO) :: GDIN
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER YEAR,MON,DAY,IHR,DATE,IFHR

      PARAMETER(MBUF=2000000)
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1, REGION
      LOGICAL*1 LCYCON,LHR3,LHR6,LHR12,LFULL,LANL,LLIMITED
      LOGICAL LNEST   ! for nests
      INTEGER JENS(200),KENS(200),CYC

!   REAL,        ALLOCATABLE   :: GRID(:)
!   LOGICAL*1,   ALLOCATABLE   :: MASK(:)
!-----------------------------------------------------------------------------------------

!  TYPE(ISET), INTENT(INOUT) :: iprcp
   INTEGER, INTENT(INOUT) :: ISNOW(:,:),IZR(:,:),IIP(:,:),IRAIN(:,:)

!  TYPE PCPSET, INTENT(INOUT) :: prcp
   REAL,    INTENT(INOUT) :: P03M(:,:),P06M(:,:),P12M(:,:),SN03(:,:),SN06(:,:)
   REAL,    INTENT(INOUT) :: WETFRZ(:,:)
   REAL,    INTENT(INOUT) :: THOLD(:,:,:),DHOLD(:,:,:)

   REAL,    INTENT(INOUT) :: VEG(:,:)

!  TYPE POPSET, INTENT(INOUT) :: pop
   REAL,    INTENT(INOUT) :: S3REF01(:,:),S3REF10(:,:),S3REF50(:,:)
   REAL,    INTENT(INOUT) :: S6REF01(:,:),S6REF10(:,:),S6REF50(:,:)
   REAL,    INTENT(INOUT) :: S12REF01(:,:),S12REF10(:,:),S12REF50(:,:)

   LOGICAL, INTENT(INOUT) :: VALIDPT(:,:) 

!-----------------------------------------------------------------------------------------

!    09-2012 JTM : Modified I/O for WCOSS fort. file name nomenclature
!                  ENVVAR not needed
!                  Introduced RDHDRS and SETVAR routines to eliminate redundancies
!    10-2012 JTM : Added dynamic allocation for arrays
!    11-2012 JTM : Added options for hi, ak, pr domains
!    12-2012 JTM : Added options for conusnest (validpt sets)

      IHROFF=0;LHR12=.FALSE.; LHR6=.FALSE.; LHR3=.FALSE.
      LFULL=.FALSE.;LANL=.FALSE.;LLIMITED=.FALSE.;LCYCON=.FALSE.

      FHR=GDIN%FHR;IFHR=FHR;CYC=GDIN%CYC;LNEST=GDIN%LNEST
      REGION=GDIN%REGION
      print *, 'REGION=', GDIN%REGION
      IF (IFHR.EQ.0) THEN
        LANL=.TRUE.
      ELSE
        IF (MOD(IFHR,3).EQ.0) THEN 
          LFULL=.TRUE.
        ELSE
          LLIMITED=.TRUE.
        ENDIF
      ENDIF
       
!    SET LOGICALS FOR when to read precip or max/min from special files

!                    NON-Nest(on)      NON-Nest(off)     NESTS
!----------------------------------------------------------------
!    F3,15, 27            X              3 hr prcp        X
!    other mod(3h)    3 hr prcp          3 hr prcp        X
!    mod(6 fhrs)         X              3,6 hr prcp      6 hr prcp
!    mod(12 fhrs)     3,6 hr prcp        3,6 hr prcp      6,12 hr prcp
!    MAX/MIN 12 hrs   prev 11 hrs        prev 11 hrs
!----------------------------------------------------------------

      IF (CYC.EQ.12.OR.CYC.EQ.00) LCYCON=.TRUE.

!     Set full, sref and special precip file unit numbers
      LUGB=11; LUGI=12   !DEFAULT MODEL FULL GRIB FILE UNITS
      LUGB2=13; LUGI2=14 !DEFAULT SREF POP GRIB FILE UNITS
      if (.not.lanl) then
       LUGP3=11;  LUGP3i=12
       LUGS3=11;  LUGS3i=12
       LUGP6=11;  LUGP6i=12
       LUGS6=11;  LUGS6i=12
       LUGP12=11; LUGP12i=12

       IF(MOD(IFHR,3).EQ.0) LHR3=.TRUE.  
       IF(MOD(IFHR,6).EQ.0) LHR6=.TRUE.
       IF(LCYCON) THEN
         IF(MOD(IFHR,12).EQ.9)  LHR9=.TRUE.
         IF(MOD(IFHR,12).EQ.0) LHR12=.TRUE.
       ELSE
         IF(IFHR.GT.6 .AND. MOD(IFHR-6,12).EQ.0) LHR12=.TRUE.
       ENDIF
      
!     Set precip unit numbers for nests
       IF (lnest) THEN
         LUGP6=15;LUGP6i=16
         LUGS6=17;LUGS6i=18
         LUGP12=19;LUGP12i=20
         LHR9=.FALSE.   ! nests have 3 hour precip in std parent grid (01-28-13, JTM)
       else
         IF(LCYCON) THEN 
           IF(MOD(IFHR,12).NE.3) THEN  !all 3 hrs except 3,15,27,39,51,63,75
             LUGP3=15; LUGP3i=16  
             LUGS3=17; LUGS3i=18   
           ENDIF
           IF(LHR12) THEN
             LUGP6=17; LUGP6i=18
             LUGS3=19; LUGS3i=20 
             LUGS6=21; LUGS6i=22
           ENDIF
         ELSE
           LHR9=.FALSE.    ! off-cycle have 3 hr precip in std parent grid
           IF (LHR6) THEN    
             LUGP6=15; LUGP6i=16   !SPECIAL PRECIP FILE
             LUGS6=17; LUGS6i=18
           ENDIF
           IF (LHR12) LUGP12=19;LUGP12i=20   
         ENDIF
        endif  !lnest
      endif  !lanl

      print *, 'IFHR',IFHR,'LHR3',LHR3,'LHR6',LHR6,'LHR12',LHR12
      P06M=0.0;S06M=0.0; P12M=0.0

!     SET MAX/MIN FILE UNIT NUMBERS
!     FOR 12-hr on-cycle TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
!     DATA FOR THE PREVIOUS 11 HOURS
      IF(LHR12) THEN
       LUGT1=23
       IF (.not.LCYCON .or. lnest) LUGT1=21
       LUGT2=LUGT1+1
       LUGT3=LUGT1+2
       LUGT4=LUGT1+3
       LUGT5=LUGT1+4
       LUGT1I=LUGT1+5
       LUGT2I=LUGT1+6
       LUGT3I=LUGT1+7
       LUGT4I=LUGT1+8
       LUGT5I=LUGT1+9
        print *,'====================================================='
        print *, 'Read  11 hrs of MAX,MIN TEMP', IFHR,lugt1,lugt2,lugt3
        print *, 'Read  3 hr precip from unit',lugp3,lugs3
        print *, 'Read  6 hr precip from unit',lugp6,lugs6
        print *, 'Read  12 hr precip from unit',lugp12
        print *,'====================================================='

      ELSE IF(LHR6.OR.LHR9) THEN
!      However Off-Hour cycle runs do not have 6 hour buckets
         LUGT1=19; LUGT2=20; LUGT1I=21; LUGT2I=22
       print *,'======================================================='
       print *, 'Read previous 2 hrs of  MAX,MIN TEMP', IFHR, lugt1,lugt2
       print *, 'Read  3 hr precip from unit',lugp3,lugs3
       if(lhr6)  print *,'Read 6 hr precip from unit',lugp6,lugs6
       print *,'======================================================='

      ELSE IF(LHR3) THEN
        LUGT1=15;LUGT2=16; LUGT1I=17; LUGT2I=18
       print *,'================================================================='
       print *, 'Read previous 2 hours of MAX,MIN TEMP ', IFHR,lugt1,lugt2
       print *, 'Read 3 hr prcp from std grid ',lugp3,lugs3
       print *,'================================================================='
      ELSE

!      IN-BETWEEN HOURS DON'T NEED ANYTHING FANCY
      print *,'====================================================='
       print *, 'IN-Between HOURS, small change ', IFHR
       print *,'====================================================='
      ENDIF

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,GDIN%DATE
      DATE=GDIN%DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

!==========================================================
!     READ INDEX FILE TO GET GRID SPECS
!==========================================================
      CALL RDHDRS(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,gdin%imax,jmax,kmax,numlev,itot

      if (lfull) then
      if (REGION .ne. 'GM') then
        print *, ' READING SREF HDRS',LUGB2,LUGI2
        CALL RDHDRS(LUGB2,LUGI2,IGDNUM2,GDIN,NUMVAL2)
      endif
! GSM  READ 3-HR PRECIP AND SNOW FILES WHICH ARE NEEDED
!      IF NOT A 3-HR ACCUMULATION TIME (F15,F27,F39...) 
!      OR AN "OFF-TIME" (F13,F14,F16....)

      print *, 'READ 3-hr precip HDRS from Unit ', LUGP3,LUGP3I
      CALL RDHDRS(LUGP3,LUGP3I,IGDNUM3,GDIN,NUMVAL3)
      print *, 'READ 3-hr snow HDRS from Unit ', LUGS3,LUGS3I
      CALL RDHDRS(LUGS3,LUGS3I,IGDNUMSN3,GDIN,NUMVALSN3)
      IGDNUM5=IGDNUMSN3

!     READ 6-HR PRECIP/SNOW FILES AT F12,F24,F36.....
!     OR 12-hr PRECIP FOR OFF-CYCLE RUNS
      IF (LHR6.OR.LHR9.OR.LHR12) THEN
        print *, 'READING 6 hr precip HDR from Unit ', LUGP6,LUG6PI
        CALL RDHDRS(LUGP6,LUGP6I,IGDNUM6,GDIN,NUMVAL6)
        print *, 'READING 6 hr SNOW HDR from Unit ', LUGS6,LUGS6I
        CALL RDHDRS(LUGS6,LUGS6I,IGDNUMSN6,GDIN,NUMVALSN6)
        IF(LHR12) THEN
          print *, 'READING 12 hr precip HDR from Unit ', LUGP12,LUGP12I
          CALL RDHDRS(LUGP12,LUGP12I,IGDNUM12,GDIN,NUMVAL12)
        ENDIF
      ENDIF

!==================================================================
! GSM  READ TEMPERATURE HDR FILES FOR 12-HR MIN/MAX
!      READ INDEX FILE TO GET GRID SPECS
!==================================================================
!     GET GRID NUMBER FROM PDS AND PROCESS GRIB FILE
!     NOTE: WE'LL ASSUME THE GRID NUMBER IS THE SAME FOR
!     ALL OF THESE MIN/MAX FILES AND NOT DO THIS FOR EACH

      print *, "Reading min/max Temp HDR from UNIT:",LUGT1, LUGT1I
      CALL RDHDRS(LUGT1,LUGT1I,IGDNUMT,GDIN,NUMVALT)

!     Fill the max/min T/Td holders with 0's to
!      1) account for this array at times other than f12,24,36...
!      2) temporarily fill the 12th time slot, since we have only 11 here
      THOLD=0.
      DHOLD=0.

      print *, "Reading min/max Temp HDR from UNIT:",LUGT2, LUGT2I, NUMVALT
      CALL RDHDRS(LUGT2,LUGT2I,IGDNUMT,GDIN,NUMVALT)

      IF (LHR12) THEN
        LUGT=LUGT3    
        LUGTI=LUGT3I   
        DO  IT=3,5
          print *, "Reading min/max Temp  UNIT:",LUGT, LUGTI
          CALL RDHDRS(LUGT,LUGTI,IGDNUMT,GDIN,NUMVALT)
          LUGT=LUGT+1
          LUGTI=LUGTI+1
        ENDDO 
      ENDIF
      endif !LFULL

!  get sfc height 
      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
      print *,'GRID ALLOCATED',ITOT,' kret',kret
      J=0;JPDS=-1;JGDS=-1;JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,ZSFC,IRET,ISTAT)
      WHERE (ZSFC < 0.0) ZSFC=0.0

! get surface pressure
      JPDS(5) = 001
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,PSFC,IRET,ISTAT)

! get 4 INTEGER precip types 
      if (lfull) then
      J=0;JPDS=-1;JPDS(3)=IGDNUM 
      JPDS(5) = 143 
      JPDS(6) = 001

!     Get INTEGER GRIB Variable  
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          ISNOW(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

      JPDS=-1;J=0
      JPDS(5) = 142
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          IIP(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

! frz rain
      JPDS=-1;J=0
      JPDS(5) = 141
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K, KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          IZR(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

! rain
      JPDS=-1;J=0
      JPDS(5) = 140
      JPDS(6) = 001
!     Get INTEGER GRIB Variable  
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          IRAIN(M,N) = GRID(KK)
        ENDDO
        WRITE(6,*) JPDS(5),JPDS(6),JPDS(7),J,KF,K
      ELSE
       WRITE(6,*)'COULD NOT UNPACK VARB',J,JPDS(3),JPDS(5),IRET
       ISTAT = IRET
      ENDIF

      endif !lfull
    
! visibility 
! Moved to hourly reads for hourly writes for RTMA (03-19-2013)
      JPDS=-1;J=0
      JPDS(5) = 020
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,VIS,IRET,ISTAT)

! 2-m temp
      JPDS=-1;J=0
      JPDS(5) = 11 
      JPDS(6) = 105 
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T2,IRET,ISTAT)

! 2-m spec hum
      JPDS=-1;J=0
      JPDS(5) = 51 
      JPDS(6) = 105
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,Q2,IRET,ISTAT)

! 2-m dew point 
      JPDS=-1;J=0
      JPDS(5) = 17 
      JPDS(6) = 105
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,D2,IRET,ISTAT)

! 10-m U
      JPDS=-1;J=0;JPDS(3)=IGDNUM
      JPDS(5) = 33
      JPDS(6) = 105
      JPDS(7) = 10
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,U10,IRET,ISTAT)

! 10-m V
      JPDS=-1;J=0;JPDS(3)=IGDNUM
      JPDS(5) = 34
      JPDS(6) = 105
      JPDS(7) = 10
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,V10,IRET,ISTAT)

! vegetation TYPE or Land Mask(0-1)
! Veg type Not available in HIRESW domains
! Read land mask instead (id 81)
! to use in NDFDgrid to perform land adjustment
        JPDS=-1;J=0;JPDS(3) = IGDNUM
        JPDS(5) = 225
        if (GDIN%REGION.EQ.'GM') JPDS(5)=81
        JPDS(6) = 001

        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VEG,IRET,ISTAT)

      if (lfull.or.lanl) then
! lowest wet bulb zero level
      if (REGION .ne. 'GM') then
      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 7 
      JPDS(6) = 245 
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,WETFRZ,IRET,ISTAT)
      endif

! Best Liftex Index 
      JPDS=-1;J=0;JPDS(3) = IGDNUM
      JPDS(5) = 132 
      JPDS(6) = 116 
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,BLI,IRET,ISTAT)
      endif

!====================================================================
!      READ PRECIP FROM FULL (LUGB=11) or SPECIAL GRIB FILE (unit 15-21)
!  STANDARD FULL GRIB FILE PRECIP 
!      For on-Cycles it will have either a 3-hr, 6-hr, 9-hr, or 12-hr accumulation
!      For off-Cycles,  3 and 12 hr accumulations are in full grib file
!====================================================================
      if (lfull) then
      JPDS=-1;J=0
      NUMVP=NUMVAL;NUMVS=NUMVAL
      IF (LHR3) THEN

! Read 3-hr Precip 
       JPDS=-1;J=0
       JPDS(3) = IGDNUM3 
       NUMVP = NUMVAL3
       JPDS(5) = 61 
       JPDS(6) = 001 
       print *, 'FHR ',IFHR,'  READ 3 hr PRECIP from file unit',LUGP3,LUGS3,IGDNUM3
       CALL SETVAR(LUGP3,LUGP3I,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P03M,IRET,ISTAT)

! 3-hr Snow 
       JPDS=-1;J=0
       JPDS(3) = IGDNUMSN3
       NUMVS=NUMVALSN3
       JPDS(5) = 65
       JPDS(6) = 001 
       CALL SETVAR(LUGS3,LUGS3I,NUMVS,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN03,IRET,ISTAT)
      ENDIF

!   ON-CYC : 6 hr buckets at 12 Fhrs in special file
!   OFF-CYC: All 6 hr buckets are in special file
      IF (LHR6.OR.LHR12) THEN

! Read 6-hr Precip 
        JPDS=-1;J=0
        JPDS(5) = 61
        JPDS(6) = 001 
        JPDS(3) = IGDNUM6;NUMVP=NUMVAL6
        print *, LCYCON,IFHR,'READ 6 hr PRECIP from  file',LUGP6,IGDNUM6
        CALL SETVAR(LUGP6,LUGP6I,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P06M,IRET,ISTAT)

!     6-hr snow 
       JPDS=-1;J=0
       JPDS(3) = IGDNUMSN6
       NUMVS=NUMVAL6
       JPDS(5) = 65
       JPDS(6) = 001 
       CALL SETVAR(LUGS6,LUGS6I,NUMVS,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,SN06,IRET,ISTAT)

! Read  12-hr Precip
        IF(LHR12) THEN
          JPDS=-1; J=0
          JPDS(5) = 61
          JPDS(6) = 001
          JPDS(3) = IGDNUM
          JPDS(3) = IGDNUM12;NUMVP=NUMVAL12
          print *, LCYCON,IFHR,'READ 12 hr PRECIP from file ',LUGP12,IGDNUM12
          CALL SETVAR(LUGP12,LUGP12i,NUMVP,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,P12M,IRET,ISTAT)
        ENDIF
      ENDIF

!  READ min/max temperature values for previous 2 hours
      print *, 'Reading max/min for previous 2 hours',LUGT1,LUGT2,IGDNUMT
      JPDS=-1;J=0;JPDS(3) = IGDNUMT
      JPDS(5) = 11
      JPDS(6) = 001
      CALL SETVAR(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,2),IRET,ISTAT)

      JPDS=-1;J=0;JPDS(3) = IGDNUMT
      JPDS(5) = 17
      JPDS(6) = 001
      CALL SETVAR(LUGT1,LUGT1I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,2),IRET,ISTAT)

      JPDS=-1;J=0;JPDS(3) = IGDNUMT
      JPDS(5) = 11
      JPDS(6) = 001
      CALL SETVAR(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,3),IRET,ISTAT)

      JPDS=-1;J=0;JPDS(3) = IGDNUMT
      JPDS(5) = 17
      JPDS(6) = 001
      CALL SETVAR(LUGT2,LUGT2I,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,3),IRET,ISTAT)

! Get min/max temperature values for full 12-hr period for F12,24...
      IF (LHR12) THEN
       IFHR4=IFHR-3
       IFHR12=IFHR-11
       KT=4
       LUGTA=LUGT3
       LUGTB=LUGT3I
       DO IIH=IFHR4,IFHR12,-1
         IF (IIH.LE.IFHR-6) THEN
           LUGTA=LUGT4
           LUGTB=LUGT4I
         ENDIF
         IF (IIH.LE.IFHR-9) THEN
           LUGTA=LUGT5
           LUGTB=LUGT5I
         ENDIF
 
         JPDS = -1;J=0
         JPDS(3) = IGDNUMT
         JPDS(5) = 11 
         JPDS(6) = 001
         JPDS(14) = IIH   
         print *, 'READING TEMP for hr', IIH, LUGTA,LUGTB,KT
         CALL SETVAR(LUGTA,LUGTB,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,THOLD(:,:,KT),IRET,ISTAT)
         JPDS = -1;J=0
         JPDS(3) = IGDNUMT
         JPDS(5) = 17 
         JPDS(6) = 001
         JPDS(14) = IIH   
         print *, 'READING  DPT for hr', IIH, LUGTA,LUGTB,KT 
         CALL SETVAR(LUGTA,LUGTB,NUMVALT,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,DHOLD(:,:,KT),IRET,ISTAT)
         KT=KT+1
       ENDDO
      ENDIF
      endif !lfull

!   get the vertical profile of pressure 
      print *,'READ UPPER LEVEL fields from unit ', LUGB,'KMAX',KMAX
      J=0
      KLTYP=109   !Hybrid vertical levels
      DO LL=1,KMAX
        IF (REGION.EQ.'GM') THEN
          KLTYP=100  !Pressure level file
          do ii=1,IMAX
          do jj=1,JMAX
          PMID(ii,jj,1)=1000;PMID(ii,jj,2)=925;PMID(ii,jj,3)=900;PMID(ii,jj,4)=850;PMID(ii,jj,5)=750
          PMID(ii,jj,6)=700;PMID(ii,jj,7)=600;PMID(ii,jj,8)=500;PMID(ii,jj,9)=400;PMID(ii,jj,10)=300
! ADD COMPUTE SPEC HUM
          enddo
          enddo

! Set pressure level to read
         J=0
         JPDS(7)=PMID(1,1,LL)
        ELSE

!  Vertical profile of pressure on hybrid sfcs
          JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=001; JPDS(6)=KLTYP
          CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,PMID(:,:,LL),IRET,ISTAT) 

!   get the vertical profile of q on hybrid sfcs
          JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=051; JPDS(6)=KLTYP
          CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,Q(:,:,LL),IRET,ISTAT)

        ENDIF !PRES Chk

!   get the vertical profile of height 
         JPDS(3)=IGDNUM; JPDS(5)=007; JPDS(6)=KLTYP
         CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),IRET,ISTAT)

!   get the vertical profile of temperature
         JPDS(3)=IGDNUM; JPDS(5)=011; JPDS(6)=KLTYP
         CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T(:,:,LL),IRET,ISTAT)

! note points that are within bitmap
         VALIDPT=.TRUE.
         WHERE(T(:,:,1).LE.10.) VALIDPT = .FALSE.

! JTM 01-28-13: Added check for where previous temps are not at validpts
         do ii=1,imax
         do jj=1,jmax
           if(validpt(ii,jj).and.T(ii,jj,1).le.10) then 
            print *,' Inconsistent valid pt at :', ii,jj,' Temperature=',T(ii,jj,1)
            validpt(ii,jj)=.false.
           endif
         enddo
         enddo
         print *,'VALIDPT=',validpt(20,20),'max/min Temp at lvl 1',maxval(T),minval(T)

!   get the vertical profile of u 
         JPDS(3)=IGDNUM; JPDS(5)=033; JPDS(6)=KLTYP
         CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,UWND(:,:,LL),IRET,ISTAT)

!   get the vertical profile of v
         JPDS(3)=IGDNUM; JPDS(5)=034; JPDS(6)=KLTYP
         CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VWND(:,:,LL),IRET,ISTAT)

         IF (REGION.NE.'GM')  J=K
      ENDDO  !LL LOOP
      if (llimited) return

!   get the vertical profile of cloud fraction for non-nests
      if (.not. lnest) then
       J=0
       DO LL=1,KMAX  
        JPDS=-1; JPDS(3)=IGDNUM; JPDS(5)=071; JPDS(6)=KLTYP
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,CFR(:,:,LL),IRET,ISTAT)
        J=K
       ENDDO
      endif


!   950 mb temperature
      J=0
      JPDS(3) = IGDNUM
      JPDS(5) = 011
      JPDS(6) = 100
      if (REGION.NE.'GM')then
        JPDS(7) = 950
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T950,IRET,ISTAT)
      endif

!   850 mb temperature
      JPDS(7) = 850
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T850,IRET,ISTAT)

!   700 mb temperature
      JPDS(7) = 700
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T700,IRET,ISTAT)

!   500 mb temperature
      JPDS(7) = 500
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,T500,IRET,ISTAT)

!   850 mb RH
      J=0
      JPDS(5) = 052
      JPDS(6) = 100
      JPDS(7) = 850
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,RH850,IRET,ISTAT)

!   700 mb RH
      JPDS(7) = 700
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,RH700,IRET,ISTAT)

      if (REGION.EQ.'GM') RETURN     !HI RES WINDOW FILES 

!  sfc wind gust 
      J=0
      JPDS=-1
      JPDS(3) = IGDNUM
      JPDS(5) = 180 
      JPDS(6) = 001
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,GUST,IRET,ISTAT)

! composite reflectivity
      JPDS(5) = 212
      JPDS(6) = 200
      CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,REFC,IRET,ISTAT)

      if (lanl) return

!     nests already have computed cld fracs...
      if (lnest) then
        J=0;JPDS=-1
        JPDS(3)=IGDNUM
        JPDS(5) = 71
        JPDS(6) = 200
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,TCLD,IRET,ISTAT)
        JPDS(5) = 73
        JPDS(6) = 214
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,LCLD,IRET,ISTAT)
        JPDS(5) = 74
        JPDS(6) = 224
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,MCLD,IRET,ISTAT)
        JPDS(5) = 75
        JPDS(6) = 234
        CALL SETVAR(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,HCLD,IRET,ISTAT)
       endif 
!  READ SREF precip
      print*; print *,'READ SREF Precip Probs', LUGB2


! 3-hr probability of .01"
      J=0     !J= number of records to skip in SREFPCP file
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF01,IRET,ISTAT)

! probability of .1"
      J = 2
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF10,IRET,ISTAT)
      IF(IRET .NE. 0 )RETURN

! probability of 0.5"
      J = 4
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S3REF50,IRET,ISTAT)

      IF (IFHR .EQ. 3) THEN
        IF (.NOT.LCYCON) THEN
          print *, 'FHR=3 so zero 6 and 12-hr sref probabilities'
          S6REF01(M,N) = 0.0
          S6REF10(M,N) = 0.0
          S6REF50(M,N) = 0.0
          S12REF01(M,N) = 0.0
          S12REF10(M,N) = 0.0
          S12REF50(M,N) = 0.0
        ENDIF
       print *, 'bailing out of sref pcp early IFHR=',IFHR
       RETURN
      ENDIF

! 6-hr probability of 0.01"
      J = 5
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF01,IRET,ISTAT)

! 6-hr probability of 0.1"
      J = J+2
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF10,IRET,ISTAT)

! 6-hr probability of 0.5"
      J = J+2
      JPDS=-1;JGDS=-1
      JPDS(3) = IGDNUM2
      JPDS(5) = 191 
      JPDS(6) = 001
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S6REF50,IRET,ISTAT)

! 12-hr probability of 0.01"
      J = 10
        IF (IFHR .EQ. 6 .OR. IFHR .EQ. 9) THEN
        print *, 'FHR=6 or 9 so 12-hr sref probabilities not available'
          S12REF01 = 0.0
          S12REF10 = 0.0
          S12REF50 = 0.0
          RETURN
        ENDIF

      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF01,IRET,ISTAT) 
! 12-hr probability of 0.1"
      J = J+2 
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF10,IRET,ISTAT)

! 12-hr probability of 0.5"
      J = J+2
      CALL SETVAR(LUGB2,LUGI2,NUMVAL2,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,S12REF50,IRET,ISTAT)


      RETURN 
      END SUBROUTINE getgrib
