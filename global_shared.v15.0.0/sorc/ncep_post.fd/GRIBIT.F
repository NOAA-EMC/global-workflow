      SUBROUTINE GRIBIT(IFLD,ILVL,GRID,IMOUT,JMOUT)  
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    GRIBIT      POST FIELDS IN GRIB1
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-06-18       
!     
! ABSTRACT:
!     THIS ROUTINE POSTS THE DATA IN THE PASSED ARRAY GRID
!     TO THE OUTPUT FILE IN GRIB1 FORMAT.
!     
! PROGRAM HISTORY LOG:
!   93-06-18  RUSS TREADON
!   93-11-23  RUSS TREADON - REMOVED CODE GENERATING GRIB INDEX FILE.
!   98-07-17  MIKE BALDWIN - REMOVED LABL84, NOW USING ID
!   02-06-17  MIKE BALDWIN - WRF VERSION
!   05-12-05  H CHUANG - ADD CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
!               NO IMPACTS ON ON-HOUR FORECAST
!   07-29-09  J HALLEY GOTWAY - MODIFY HANDLING OF OFF-HOUR FORECASTS TO
!                MAKE USE OF THE 1/2 AND 1/4 HOUR TIME RANGE INDICATORS.
!     
! USAGE:    CALL GRIBIT(IFLD,ILVL,GRID,IMOUT,JMOUT)
!   INPUT ARGUMENT LIST:
!     IFLD     - FIELD ID TAG.
!     ILVL     - INTEGER TAG FOR LEVEL OF FIELD.
!     GRID     - FIELD TO BE POSTED IN GRIB.
!     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
!     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
!
!   OUTPUT ARGUMENT LIST: 
!     
!   OUTPUT FILES:
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!     GETENV   - CRAY SUBROUTINE TO GET VALUE OF ENVIRONMENT VARIABLE.
!     MINMAX   - DETERMINES MIN/MAX VALUES IN AN ARRAY.
!     WRYTE    - WRITE DATA OUT BY BYTES.
!     GET_BITS   - COMPUTE NUMBER OF BITS 
!     VARIOUS W3LIB ROUTINES
!     LIBRARY:
!       COMMON   - CTLBLK
!                  RQSTFLD
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
      use CTLBLK_mod,   only: me, sdat, ihrst,tprec, modelname, vtimeunits,   &
                              imdlty, spl, spval, ioform, num_servers,        &
                              mpi_comm_inter, im, jm, imin, ifhr, ifmin
      use params_mod,   only: d01, tfrz, small
      use RQSTFLD_mod,  only: id, kgtype, iq, is, dec, igds, ident, field,    &
                              ritehd, datset, iget
      use GRIDSPEC_mod, only: maptype, latstart, lonstart, standlon, dxval,   &
                              dyval, truelat2, truelat1, psmapf, latlast,     &
                              lonlast, cenlat, cenlon, latstartv, lonstartv,  &
                              cenlatv, cenlonv, latlastv, lonlastv
!- - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      INCLUDE 'mpif.h'
!
!     INCLUDE GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
!
!     GRIB1 PARAMETERS.
!        MNBIT  = MINIMUM NUMBER OF BITS TO USE IN PACKING.
!        MXBIT  = MAXIMUM NUMBER OF BITS TO USE IN PACKING.
!        LENPDS = LENGTH OF GRIB1 PDS.
!        LENGDS = LENGTH OF GRIB1 GDS.
!     
#ifdef GSDRAPR
      integer,PARAMETER :: MNBIT=0,MXBIT=20,LENPDS=28,LENGDS=32
#else
      integer,PARAMETER :: MNBIT=0,MXBIT=16,LENPDS=28,LENGDS=32
#endif
!
!     DECLARE VARIABLES.
      integer,intent(in) :: IFLD,ILVL,IMOUT,JMOUT
      REAL,intent(in)    :: GRID(IM,JM)
!     
      LOGICAL OLDRD,STRD
      LOGICAL NORTH
      CHARACTER*1   KBUF(30+LENPDS+LENGDS+IM*JM*(MXBIT+2)/8)
      CHARACTER*1   KBUF_S(30+LENPDS+LENGDS+IM*JM*(MXBIT+2)/8)
      CHARACTER*1   IFLAG
      CHARACTER*4   RESTHR,BLANK
      CHARACTER*6   CRUN,PROJ
      CHARACTER*10  DESCR2,DESCR3
      CHARACTER*28  PDS
      CHARACTER*255 ENVAR
      CHARACTER*255 FNAME,FNAME_S,OPATH,PGBOUT,IPVOUT,D3DOUT
      CHARACTER*90 CMD
      character    CFHOUR*40,CFORM*40
      integer ndig
      INTEGER IBDSFL(9)
      INTEGER IGRD(IM,JM),IBMASK(IM,JM)
      REAL    GRIDO(IM,JM)
!jw
      real(8) ist, rtc    !in ctlblk:time_output, time_e2out
      real  AYEAR0,AMNTH0,ADAY0,AGMT0,SGDG,GMAX,GMIN
      integer I,J,L,LUNOUT,IJOUT,IGRID,IGFLAG,ICOMP,K,         &
              IPFLAG,IDECI,IBITL,ITYPE,IBFLAG,KENV,KDAT,IERR,KTHR,IHR,     &
              NPTS,IBLEN,IER,ITOT,IBITM,ICENT,NBIT,IMM,IYY,IBX,            &
              IDD,KGTYP,IBM,ISVALUE,INTERVAL
!
!     THE BELOW VARIABLE ARE ONLY NEEDED FOR THE CALL TO W3FI63.
      REAL DATAFLD(IM,JM)
      INTEGER IBMAP(IM,JM)
      INTEGER KGDS(20),KPTR(16)
      LOGICAL KBMS(IM,JM)
      LOGICAL DONE, NEWFILE, NEWFILE_S
      INTEGER IH(5)
      INTEGER ICHECK, ILOAD
      INTEGER STATUS(MPI_STATUS_SIZE)
      INTEGER LUNOUT_S
      INTEGER TOTMIN,DIV
!     
!     SET DEFAULT GRIB1 PARAMETERS.  
!     PARAMETERS MNBIT, MXBIT, IBX, AND NBIT ARE USED 
!     IN THE CALL TO GET_BITS.
!        IBX    = DESIRED BINARY PRECISION.
!        NBIT   = NUMBER OF BITS TO USE IN PACKING DATA.
!     
      DATA IBX,NBIT / 0, 12 /
      DATA BLANK /'    '/
      DATA DONE /.FALSE./
      DATA ICHECK / 1 /
      DATA ILOAD / 1 /
      DATA IH / 5* MPI_REQUEST_NULL /
      SAVE OPATH
!      SAVE IH, NEWFILE, NEWFILE_S, KBUF, KBUF_S, 
!     *     DONE, FNAME, FNAME_S,
!     *     LUNOUT_S
!
!*****************************************************************************
!     START GRIBIT HERE.
!
!     ALL TASKS MUST CALL COLLECT BUT ONLY TASK 0 CAN EXECUTE THE REMAINDER 
!      OF GRIBIT
!
      LUNOUT = 70
      CALL COLLECT(GRID,GRIDO)

      IF ( ME == 0 ) THEN
!       ist = rtc()

!$omp parallel do private(i,j)
        DO J=1,JM    ! zero small values to prevent FPE
          DO I=1,IM
            if ( ABS(GRIDO(I,J)) < 1.E-30 ) GRIDO(I,J) = 0.
          enddo
        ENDDO

        NEWFILE = .FALSE.
!jjt
!     SET NUMBER OF OUTPUT GRID POINTS.
!       IJOUT = IMOUT*JMOUT
        IJOUT = IM*JM
!     
!     PREPARE GRIB PDS
!     
!     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
!        ID(1)  = NUMBER OF BYTES IN PRODUCT DEFINITION SECTION (PDS)
!        ID(2)  = PARAMETER TABLE VERSION NUMBER
!        ID(3)  = IDENTIFICATION OF ORIGINATING CENTER
!        ID(4)  = MODEL IDENTIFICATION (ALLOCATED BY ORIGINATING CENTER)
!        ID(5)  = GRID IDENTIFICATION
!        ID(6)  = 0 IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
!        ID(7)  = 0 IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
!        ID(8)  = INDICATOR OF PARAMETER AND UNITS (TABLE 2)
!        ID(9)  = INDICATOR OF TYPE OF LEVEL       (TABLE 3)
!        ID(10) = VALUE 1 OF LEVEL (=0 FOR 1-100,102,103,105,107,
!          109,111,113,115,117,119,125,160,200,201 LEVEL IS IN ID WORD 11)
!        ID(11) = VALUE 2 OF LEVEL
!        ID(12) = YEAR OF CENTURY
!        ID(13) = MONTH OF YEAR
!        ID(14) = DAY OF MONTH
!        ID(15) = HOUR OF DAY
!        ID(16) = MINUTE OF HOUR   (IN MOST CASES SET TO 0)
!        ID(17) = FCST TIME UNIT
!        ID(18) = P1 PERIOD OF TIME
!        ID(19) = P2 PERIOD OF TIME
!        ID(20) = TIME RANGE INDICATOR
!        ID(21) = NUMBER INCLUDED IN AVERAGE
!        ID(22) = NUMBER MISSING FROM AVERAGES
!        ID(23) = CENTURY
!        ID(24) = RESERVED - SET TO 0
!        ID(25) = SCALING POWER OF 10
!
!     
!        PREPARE DATE PART OF GRIB PDS RECORD.
         ICENT      = (SDAT(3)-1)/100 + 1
         IYY        = SDAT(3) - (ICENT-1)*100
         IMM        = SDAT(1)
         IDD        = SDAT(2)
         AYEAR0     = IYY
         AMNTH0     = IMM
         ADAY0      = IDD
         AGMT0      = IHRST
         ID(01)     = 28
         IF ( (ID(2).NE.129) .AND. (ID(2).NE.130) .AND.         &
	      (ID(2).NE.133) .AND.  (ID(2).NE.141) ) THEN
             ID(2)     = 2
         END IF 

! *** Use GSD center code - 59
! *** Use NCEP center code - 7 
#ifdef GSDRAPR
         ID(03)     = 59
#else
         ID(03)     = 7
#endif
         ID(12)     = IYY
         ID(13)     = IMM
         ID(14)     = IDD
         ID(15)     = IHRST
!        ID(16)     = 0
         ID(16)     = IMIN
         ID(17)     = 1
!
!    ASSUMING ID(18-20), (P1, P2, TIME RANGE INDICATOR) 
!    ARE PASSED IN CORRECTLY IF NOT AN INSTANTANEOUS FIELD
!   
         IF (ID(20).EQ.0) THEN
          ID(18)     = IFHR 
          ID(19)     = 0
         ENDIF
	 
         if(IFHR>255)then
	  if(ID(20)==0)then
	   ID(20)=10 ! use timerange 10 to store fhr with 2 bytes
	  else if (ID(20)==2 .OR. ID(20)==3 .OR. ID(20)==4)then
!	   INTERVAL=12 ! use 3,6 or 12 hourly to specify time ranges
	   INTERVAL=NINT(TPREC) ! use fcst unit based on precip bucket
	   IF(INTERVAL==3)then
	    if(IFHR<=INTERVAL*255)THEN
	     ID(17)=10
	     ID(18)=ID(18)/INTERVAL
	     ID(19)=ID(19)/INTERVAL
	    else
	     ID(20)=10 ! beyond Grib limitation, go back to non-accumulated quantity
	     ID(18)=IFHR
	     ID(19)=0
	    end if 
	   else if(INTERVAL==6)then
	    if(IFHR<=INTERVAL*255)THEN
	     ID(17)=11
	     ID(18)=ID(18)/INTERVAL
	     ID(19)=ID(19)/INTERVAL
	    ELSE
	     ID(20)=10 ! beyond Grib limitation, go back to non-accumulated quantity
	     ID(18)=IFHR
	     ID(19)=0
	    end if    
	   else if(INTERVAL==12)then 
	    if(IFHR<=INTERVAL*255)THEN
	     ID(17)=12
	     ID(18)=ID(18)/INTERVAL
	     ID(19)=ID(19)/INTERVAL
	    ELSE
	     ID(20)=10 ! beyond Grib limitation, go back to non-accumulated quantity
	     ID(18)=IFHR
	     ID(19)=0
	    end if 
	   else if(INTERVAL==24)then 
	    if(IFHR<=INTERVAL*255)THEN
	     ID(17)=2
	     ID(18)=ID(18)/INTERVAL
	     ID(19)=ID(19)/INTERVAL
	    ELSE
	     ID(20)=10 ! beyond Grib limitation, go back to non-accumulated quantity
	     ID(18)=IFHR
	     ID(19)=0
	    end if
	   end if
	  end if
	 end if
! operational GFS uses time range 10 even for hours less than 256, will unify this soon	 
         IF (ID(20)==0 .AND. MODELNAME=='GFS')ID(20)=10
!	    	 
! CHUANG: TO OUTPUT OFF-HOUR FORECAST, I USED MIN INSTEAD OF HOUR AS FORECAST UNIT
! ALOS, SINCE ONLT TIME RANGE TYPE 10 USES 2 BYTES TO STORE TIME, MODIFICATION WAS
! MADE TO USE TYPE 10 AS TIME RANGE INDICATOE WHEN FORECST MINS ARE LARGER THAN 254,	
! WHICH MEANS ALL THE ACCUMULATED AND TIME-AVERAGED QUANTITY ARE VERIFIED AT ONE TIME
! INSTEAD OF AT A TIME RANGE. 
!    J. HALLEY GOTWAY, MODIFY HOW THE TIME INFORMATION IS STORED IN ID(17-20),
!    (FCST TIME UNIT, P1, P2, TIME RANGE INDICATOR).
!    CHECK IF THE NUMBER OF FORECAST MINUTES IS ZERO FOR HOURS OR NON-ZERO FOR
!    OFF-HOUR FORECASTS.  FOR OFF-HOUR FORECASTS, CHECK IF THE TOTAL NUMBER
!    OF MINUTES IS DIVISIBLE BY 30, 15, OR NEITHER, AND USE THE APPROPRIATE
!    FCST TIME UNIT VALUE.  FOR ANY FIELD OTHER THAN AN INSTANTANEOUS FIELD,
!    ASSUME ID(18-20), ARE PASSED IN CORRECTLY.

	 IF(IFMIN .GE. 1)THEN
!	   ID(17)     = 0
!    COMPUTE THE TOTAL FORECAST MINUTES.
	   TOTMIN=IFHR*60+IFMIN

!    CHECK FOR 1/2 HOURLY INCREMENTS.
            IF (MOD(TOTMIN, 30) == 0) THEN
               ID(17) = 14
               DIV    = 30
!    CHECK FOR 1/4 HOURLY INCREMENTS.
            ELSEIF (MOD(TOTMIN, 15) == 0) THEN
               ID(17) = 13
               DIV    = 15
!    OTHERWISE, USE MINUTES.
            ELSE
               ID(17) = 0
               DIV    = 1
            ENDIF
!    SET THE VALUES FOR P1 AND P2.  USE TOTMIN FOR INSTANTANEOUS FIELDS.
            IF ( (ID(20) == 0) .OR. (ID(20) == 10) ) THEN
               ID(20) = 10
               ID(18) = TOTMIN/DIV
               ID(19) = 0
!    USE THE VALUES IN ID(18-19) FOR NON-INSTANTANEOUS FIELDS.
            ELSE
               ID(18) = ID(18)/DIV
               ID(19) = ID(19)/DIV

!    CHECK FOR P1 OR P2 GREATER THAN 256 FOR NON-INSTANTANEOUS FIELDS.
!   OVERFLOW - SET ID(18) TO THE GREATEST 2-BYTE VALUE
               IF ( (ID(18) > 256) .OR. (ID(19) > 256) ) THEN
                  ID(20) = 10
                  ID(18) = ID(19)
                  ID(19) = 0
               ENDIF
            ENDIF
	   
	  END IF ! end of off-hour time stamp processing
!      
! CRA HARDWIRE MINUTES FOR VALID TIME UNITS IF REQUESTED
         IF(MODELNAME=='RAPR' .AND. VTIMEUNITS=='FMIN') THEN
            ID(17)     = 0
            ID(18)     = IFHR*60+IFMIN
            ID(19)     = 0
            ID(20)     = 10
            print*,'HARDWIRING PDS OCTET 18 =', ID(17)
            print*,'HARDWIRING PDS OCTET 19 =', ID(18)
            print*,'HARDWIRING PDS OCTET 20 =', ID(19)
            print*,'HARDWIRING PDS OCTET 21 =', ID(20)
         ENDIF
! CRA       
! 
         ID(21)     = 0
         ID(22)     = 0
         ID(23)     = ICENT
         ID(24)     = 0
!
!     
!        SET OUTPUT GRID TYPE.  WE ASSUME KGYTPE HOLDS THE GRIB
!        ID FOR THE OUTPUT GRID.  
!
         KGTYP = KGTYPE
!     
!        SET GRID TYPE ID(5)
!        GENERATING PROGRAM ID(4)
!
!         IJOUT      = IMOUT*JMOUT
         IJOUT      = IM*JM
         ID(4) = IMDLTY
         ID(5) = KGTYP
!
!        ID(6) =0 IF NO GDS SECTION, =1 IF GDS INCLUDED, 
!                 ALWAYS INCLUDE GDS
!
         ID(6) = 1
!     
!        SET DATA TYPE ID(8) AND SURFACE ID(9).
!
!     DONOT SET PARAMETER IF PRECIP TYPE, SINCE THERE ARE
!     4 PARAMETER NUMBERS FOR THE SAME IFLD
!
!         IF (ID(8).LT.140.OR.ID(8).GT.143) ID(8) = IQ(IDENT(IFLD))
!   05-08-24  GEOFF MANIKIN - ADDED IN DOMINANT PRECIP TYPE
!                              TO PTYPE IF STATEMENT
         IF (ID(8).LT.140.OR.ID(8).GT.143) THEN
           IF (ID(8).LT.203.OR.ID(8).GT.206.OR.ID(2).NE.129) THEN
!   15-12-04  WM LEWIS ADDED CONDITION TO ALLOW FOR WRF_PARM.CNTRL
!             DIRECTION OF SATELLITE CHANNEL SELECTION USING ILVL SWITCHES
             IF (ID(8).LT.168.OR.ID(8).GT.244) THEN
               ID(8)=IQ(IDENT(IFLD))
             ENDIF
            ENDIF
          ENDIF
! Iredell decided to change GRIB ID OF GSS SHUELL SLP TO 1 BECAUSE THE UNIFIED
! POST DOES NOT PERFORM FILTERING.  THE GFS FILTERINF OF SLP WILL BE DONE IN
! THE POSTGP SCRIPT BY USING COPYGB.  THE GRIB ID OF NEW FILTERED GFS SLP WILL
! BE 2 
	 IF (MODELNAME=='GFS' .AND. ID(8)==2)ID(8) = 1
	 

         IF (ID(9).EQ.0) ID(9) = IS(IDENT(IFLD))
!     
!        SET VALUE OF LEVEL IF ON PRESSURE OR ETA SURFACE.
!        OTHERWISE, WE ASSUME ID(10) AND (11) ARE SET 
!        APPROPRIATELY PRIOR TO ENTERING GRIBIT.
!     
         IF (ID(9).EQ.100)  THEN
            ISVALUE = NINT(SPL(ILVL)*D01)
            ID(10) = 0
            ID(11) = ISVALUE
!MEB     ELSEIF (ID(9).EQ.119) THEN
!MEB        ISVALUE = ILVL
!MEB        ISVALUE = NINT(AETA(ILVL)*10000.)
!
!   TKE IS ON THE ETA INTERFACE AT THE BOTTOM OF THE LAYER ILVL
!
!MEB        IF (ID(8).EQ.158) ISVALUE = NINT(ETA(ILVL+1)*10000.)
!MEB        ID(10) = 0
!MEB        ID(11) = ISVALUE
         ELSEIF (ID(9) .EQ. 109) THEN
#ifdef GSDRAPR
            IF(MODELNAME == 'RAPR') ID(9) = 107
#else
            IF(MODELNAME == 'RAPR') ID(9) = 109
#endif
            ISVALUE = ILVL
            ID(10) = 0
            ID(11) = ISVALUE
         ENDIF
! GFS uses different ID for convective clouds, hoping to unify soon
         IF (MODELNAME=='GFS')THEN
	  IF(ID(8)==72 .AND. ID(9)==200 )THEN
	   ID(8) = 71
	   ID(9)=244
	  END IF
! GFS uses Grib ID 11 for soil temperature
	  IF(ID(8)==85 .AND. ID(9)==112 )THEN
	   ID(8) = 11
	  END IF
! GFS uses Grib ID 76 for total column cloud water
	  IF(ID(8)==136 .AND. ID(9)==200 )THEN
	   ID(8) = 76
	   ID(2)=2
	  END IF
! GFS uses level ID=1 for surface lifted index
	  IF(ID(8)==131 .AND. ID(9)==101 )THEN
	   ID(9)=1
	   ID(10)=0
	   ID(11)=0
	   where(GRIDO/=SPVAL)GRIDO=GRIDO-TFRZ	       
	  END IF
! GFS uses level ID=1 for best lifted index
	  IF(ID(8)==132 .AND. ID(9)==116 )THEN
	   ID(9)=1
	   ID(10)=0
	   ID(11)=0     
	  END IF	  
	 END IF 
!     
!     END OF GRIB PDS LABEL PREPARATION.
!

!     
!     SET DECIMAL SCALING (IDECI) FROM LIST IN INCLUDE FILE 
!     RQSTFLD.  A CALL TO GET_BITS WILL COMPUTE THE NUMBER OF
!     BITS NECESSARY TO PACK THE DATA BASED ON THE RANGE OF 
!     THE FIELD.  THE FIELD IS SCALED TO THIS PRECISION AND
!     RETURNED FOR PACKING BY THE GRIB PACKER.
!     
      IBM = 0
      IBITM = 0
      SGDG  = DEC(IFLD)
!     set bitmap
!      DO J=1,JMOUT
!      DO I=1,IMOUT
      DO J=1,JM
      DO I=1,IM
        IF(ABS(GRIDO(I,J)-SPVAL).GT.SMALL) THEN
             ibmap(i,j) = 1
             ibitm = ibitm+1
        ELSE
             ibmap(i,j) = 0
        ENDIF
      ENDDO
      ENDDO
!     set bitmap
!
!        ID(7) =0 IF NO BMS SECTION, =1 IF BMS INCLUDED
!
      IF (IBITM.EQ.IJOUT) THEN
        ID(7) = 0
        IBM = 0
      ELSE
        ID(7) = 1
        IBM = 1
      ENDIF
      CALL GET_BITS(IBM,SGDG,IJOUT,IBMAP,GRIDO,      & 
     &                IDECI,GRIDO,GMIN,GMAX,NBIT)
!
!        ID(25) = SCALING POWER OF 10
!
      ID(25) = IDECI
!     
!     GENERATE COMPLETE GRIB1 MESSAGE USING W3FI72.
!        ITYPE  = 0 SPECIFIES REAL DATA TO BE PACKED.
!        IGRD   = DUMMY ARRAY FOR INTEGER DATA.
!        IBITL  = NBIT TELLS W3FI72 TO PACK DATA USING NBIT BITS.
!        IPFLAG = 0 IS PDS INFORMATION IN USER ARRAY ID.
!                 1 IS PDS (GENERATED ABOVE BY W3FP12).
!        ID     = (DUMMY) ARRAY FOR USER DEFINED PDS.
!        IGFLAG = 0 TELLS W3FI72 TO MAKE GDS USING IGRID.
!                 1 IS GDS GENERATED BY USER IN ARRAY IGDS
!        IGRID  = GRIB1 GRID TYPE (TABLE B OF ON388).
!        IGDS   = ARRAY FOR USER DEFINED GDS.
!        ICOMP  = 0 FOR EARTH ORIENTED WINDS,
!                 1 FOR GRID ORIENTED WINDS.
!        IBFLAG = 0 TELLS W3FI72 TO MAKE BIT MAP FROM USER
!                 SUPPLIED DATA.
!        IBMASK = ARRAY CONTAINING USER DEFINED BIT MAP.
!        IBLEN  = LENGTH OF ARRAY IBMASK.
!        IBDSFL = ARRAY CONTAINING TABLE 11 (ON388) FLAG INFORMATION.
!        NPTS   = LENGTH OF ARRAY GRID OR IGRD.  MUST AGREE WITH IBLEN.
!     
!     INTIALIZE VARIABLES.

      ITYPE  = 0
!
      IBITL  = MIN(NBIT,MXBIT)
!
      IPFLAG = 0
!
!MEB  IGFLAG = 0
      IGFLAG = 1  ! set to 1 so that IGDS is defined here instead of w3lib
      IGRID  = ID(5)
      print*,'GRID NUMBER = ',IGRID
      IF (IGRID.EQ.26) IGRID=6

! IF INPUT IS GRIB, THE IGDS WERE MADE IN INITPOST
      print*,'IOFORM in GRIBIT =', IOFORM
      IF (TRIM(IOFORM) /= 'grib' )THEN      
       DO 20 K = 1,18
         IGDS(K) = 0
 20    CONTINUE
      END IF 
      IF(MAPTYPE.EQ.1)THEN  !Lambert Conformal
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 3
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART 
         IGDS( 7) = LONSTART
       ! IGDS( 8) = 8
       !   u- and v- grid relative
         IGDS( 8) = 136
         IGDS( 9) = STANDLON
         IGDS(10) = DXVAL
         IGDS(11) = DYVAL
         IF(TRUELAT2<0)then
          IGDS(12) = 128  !for southern hemisphere
         else
          IGDS(12) = 0
         end if
         IGDS(13) = 64
         IGDS(14) = 0
         IGDS(15) = TRUELAT2
         IGDS(16) = TRUELAT1

         IF (TRUELAT1 .LT. 0) THEN
           IGDS(17) = -90000
           IGDS(18) = 0
         ELSE
           IGDS(17) = 0
           IGDS(18) = 0
         END IF

      ELSE IF(MAPTYPE.EQ.2)THEN  !Polar stereographic
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 5
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART
         IGDS( 8) = 136
!         IGDS( 9) = CENLON
         IGDS( 9) = STANDLON
         IGDS(10) = NINT (DXVAL/PSMAPF)
         IGDS(11) = NINT (DYVAL/PSMAPF)
         IF (TRUELAT1 .LT. 0) THEN
            IGDS(12) = 128
         ELSE
            IGDS(12) = 0
         ENDIF
         IGDS(13) = 64
      ELSE IF(MAPTYPE.EQ.3)THEN  !Mercator
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 1
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART
         IGDS( 8) = 136
         IGDS( 9) = LATLAST 
         IGDS(10) = LONLAST 
         IGDS(11) = DYVAL
         IGDS(12) = DXVAL 
         IGDS(13) = TRUELAT1 
         IGDS(14) = 64 
      ELSE IF(MAPTYPE.EQ.203)THEN  !ARAKAWA STAGGERED E-GRID
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 203 
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART
         IGDS( 8) = 136
         IGDS( 9) = CENLAT
         IGDS(10) = CENLON
         IGDS(11) = DXVAL
         IGDS(12) = DYVAL
         IGDS(13) = 64 
         IGDS(14) = 0
      ELSE IF(MAPTYPE.EQ.205)THEN  !ARAKAWA STAGGERED B-GRID
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 205 
         IGDS( 4) = IM
         IGDS( 5) = JM
	 IF(ID(8)/=33 .AND. ID(8)/=34)THEN
           IGDS( 6) = LATSTART
           IGDS( 7) = LONSTART
           IGDS( 8) = 136
           IGDS( 9) = CENLAT
           IGDS(10) = CENLON
           IGDS(14) = LATLAST
           IGDS(15) = LONLAST
	 ELSE
	   IGDS( 6) = LATSTARTV
           IGDS( 7) = LONSTARTV
           IGDS( 8) = 136
           IGDS( 9) = CENLATV
           IGDS(10) = CENLONV
           IGDS(14) = LATLASTV
           IGDS(15) = LONLASTV
	 END IF   
         IGDS(11) = DXVAL
         IGDS(12) = DYVAL
         IGDS(13) = 64
      ELSE IF(MAPTYPE.EQ.6)THEN  !Lat-Lon A grid
       IF(STANDLON==CENLON)THEN !regular latlon
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 0
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART
         IGDS( 8) = 136
         IGDS( 9) = LATLAST
         IGDS(10) = LONLAST
         IGDS(11) = DXVAL
         IGDS(12) = DYVAL
         IGDS(13) = 64
         IGDS(14) = 0
       ELSE ! rotated latlon
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 205
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART
         IGDS( 8) = 136
         IGDS( 9) = CENLAT
         IGDS(10) = CENLON
         IGDS(11) = DXVAL
         IGDS(12) = DYVAL
         IGDS(13) = 64
         IGDS(14) = LATLAST
         IGDS(15) = LONLAST 
       END IF   	 
! Only define Gaussian grid again if it is not defined in
! INITPOST_GFS
      ELSE IF(MAPTYPE.EQ.4 .AND. IGDS(4)==0)THEN  !Gaussian grid
         print*,'set up IGDS in GRIBIT for Gaussian grid'
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 4 
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART 
         IGDS( 8) = 128
         IGDS( 9) = LATLAST 
         IGDS(10) = LONLAST
         IGDS(11) = NINT(JM/2.0)
         IGDS(12) = NINT(360./IM*1000.)
         IGDS(13) = 0 
         IGDS(14) = 0
	 IGDS(15) = 0
         IGDS(16) = 0 
         IGDS(17) = 0
         IGDS(18) = 0	 
! Only define Latlon grid again if it is not defined in
! INITPOST_GFS
      ELSE IF(MAPTYPE.EQ.0 .AND. IGDS(4)==0)THEN  !Latlon grid
         print*,'set up IGDS in GRIBIT for Latlon grid'
         IGDS( 1) = 0
         IGDS( 2) = 255
         IGDS( 3) = 0
         IGDS( 4) = IM
         IGDS( 5) = JM
         IGDS( 6) = LATSTART
         IGDS( 7) = LONSTART
         IGDS( 8) = 128
         IGDS( 9) = LATLAST
         IGDS(10) = LONLAST
         IGDS(11) = NINT(180./(JM-1)*1000.)
         IGDS(12) = NINT(360./(IM)*1000.)
         IGDS(13) = 0
         IGDS(14) = 0
         IGDS(15) = 0
         IGDS(16) = 0
         IGDS(17) = 0
         IGDS(18) = 0
      END IF 
!	
	write(6,*) 'IGDS in GRIBIT= ', IGDS
!       LAMBERT CONFORMAL:
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!                                              1=SOUTH POLE ON PLANE,
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = NOT USED
!           IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
!                      SECANT CONE CUTS THE SPERICAL EARTH
!           IGDS(16) = SECOND LATITUDE ...
!           IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
!           IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
!
      ICOMP  = 1
!      IF (INDEX(PROJ,'LOLA').NE.0) ICOMP = 0
      IF(IGDS(8)==128)ICOMP = 0
!      print*,'ICOMP in GRIBIT=',ICOMP
      IBFLAG = 0
      IBLEN  = IJOUT
      DO 30 K = 1,9
         IBDSFL(K) = 0
 30   CONTINUE
!
      CALL W3FI72(ITYPE,GRIDO,IGRD,IBITL,                           &
                  IPFLAG,ID,PDS,                                    &
                  IGFLAG,IGRID,IGDS,ICOMP,                          &
                  IBFLAG,IBMAP,IBLEN,                               &
                  IBDSFL,                                           &
                  NPTS,KBUF,ITOT,IER)
!     
!     EXPLICITLY SET BYTE 12 OF KBUF (BYTE 4 OF THE PDS)
!     TO 2.  THIS WILL REFER ALL QUANTITIES TO PARAMETER
!     TABLE VERSION 2 OF WHICH TABLE VERSION 1 IS A SUBSET.
!     THIS IS NEEDED BECAUSE THE W3 ROUTINES HARDWIRE THIS
!     VALUE TO 1 YET SOME OF THE OUTPUT VARIABLES ARE ONLY 
!     DEFINED IN VERSION 2 OF THE PARAMETER TABLE.
!
!--- Comment out; BYTE 4 (PDS Octet 4) = 2 or 129 (see ON388, Table 2)
!
!!      KBUF(12)=CHAR(2)
!
      IF (IER.NE.0) THEN
         WRITE(6,1040) IER,FIELD(IFLD)
 1040    FORMAT('GRIBIT:  ***W3FI72 ERROR IER=',I8,' FOR ',A20)
         WRITE(6,*)'GRIBIT:  DID NOT POST THIS FIELD'
!         time_output = time_output + rtc() - ist
         RETURN
      ENDIF
!     
!     ON FIRST ENTRY MAKE OUTPUT DIRECTORY.  SET SWITCH (RITEHD)
!     TO FALSE FOR SUBSEQUENT ENTRIES.
      IF (RITEHD) THEN
!
!        PUT FORECAST HOUR INTO DIR PREFIX FOR GRIB FILE.
         IHR = IFHR
!     
!        GET FULL PATH FOR OUTPUT FILE FROM ENVIRONMENT VARIABLE
!        COMSP WHICH IS SET IN THE SCRIPT RUNNING THE MODEL.
!     
!        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
         ENVAR = ' '
         RESTHR = ' '
	 PGBOUT = ' '
	 IPVOUT = ' '
	 D3DOUT = ' '
         CALL GETENV('COMSP',ENVAR)
         CALL GETENV('tmmark',RESTHR)
	 CALL GETENV('PGBOUT',PGBOUT)
	 CALL GETENV('IPVOUT',IPVOUT)
	 CALL GETENV('D3DOUT',D3DOUT)
         KDAT = INDEX(DATSET,' ') -1
         IF (KDAT.LE.0) KDAT = LEN(DATSET)
         KENV = INDEX(ENVAR,' ') -1
         IF (KENV.LE.0) KENV = LEN(ENVAR)
         KTHR = INDEX(RESTHR,' ') -1
         IF (KTHR.LE.0) KTHR = LEN(RESTHR)
!     
!        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
         IF(MODELNAME=='GFS')THEN
          IF(D3DOUT(1:4).NE.BLANK .AND. &
             ((IGET(354).GT.0).OR.(IGET(355).GT.0).OR.  &
             (IGET(356).GT.0).OR.(IGET(357).GT.0).OR.  &
             (IGET(358).GT.0).OR.(IGET(359).GT.0).OR.  &
             (IGET(360).GT.0).OR.(IGET(361).GT.0).OR.  &
             (IGET(362).GT.0).OR.(IGET(363).GT.0).OR.  &
             (IGET(364).GT.0).OR.(IGET(365).GT.0).OR.  &
             (IGET(366).GT.0).OR.(IGET(367).GT.0).OR.  &
             (IGET(368).GT.0).OR.(IGET(369).GT.0).OR.  &
             (IGET(370).GT.0).OR.(IGET(371).GT.0).OR.  &
             (IGET(372).GT.0).OR.(IGET(373).GT.0).OR.  &
             (IGET(374).GT.0).OR.(IGET(375).GT.0)))THEN
              FNAME = D3DOUT
              PRINT*,' FNAME FROM D3DOUT=',FNAME
          ELSE IF(IPVOUT(1:4).NE.BLANK .AND.  &
             ((IGET(332).GT.0).OR.(IGET(333).GT.0).OR.  &
             (IGET(334).GT.0).OR.(IGET(335).GT.0).OR.  &
             (IGET(351).GT.0).OR.(IGET(352).GT.0).OR.  &
             (IGET(353).GT.0).OR.(IGET(378).GT.0)))THEN
              FNAME = IPVOUT
              PRINT*,' FNAME FROM IPVOUT=',FNAME
          ELSE IF(PGBOUT(1:4).NE.BLANK)THEN
            FNAME = PGBOUT
            PRINT*,' FNAME FROM PGBOUT=',FNAME
          ELSE
              NDIG=MAX(LOG10(IHR+0.5)+1.,2.)
!          WRITE(CFORM,'("('.GrbF',I",I1,".",I1,")")') NDIG,NDIG
              WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
              WRITE(CFHOUR,CFORM) IHR
              FNAME = DATSET(1:KDAT) //'.GrbF'// CFHOUR
              print *,' FNAME=',FNAME
          END IF
!         IF(MODELNAME=='GFS'.AND.PGBOUT(1:4).NE.BLANK)THEN
!	   FNAME = PGBOUT
!	   PRINT*,' FNAME FROM PGBOUT=',FNAME	        
!     
         ELSEIF (ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).EQ.BLANK) THEN
	  IF(IFMIN .GE. 1)THEN
	   WRITE(DESCR2,1011) IHR
	   WRITE(DESCR3,1011) IFMIN
	   FNAME = DATSET(1:KDAT) // DESCR2  //':'// DESCR3(1:2)
          ELSE 	  
           NDIG=MAX(LOG10(IHR+0.5)+1.,2.)
!          WRITE(CFORM,'("('.GrbF',I",I1,".",I1,")")') NDIG,NDIG
           WRITE(CFORM,'("(I",I1,".",I1,")")') NDIG,NDIG
           WRITE(CFHOUR,CFORM) IHR
           FNAME = DATSET(1:KDAT) //'.GrbF'// CFHOUR
      print *,' FNAME=',FNAME
!
!          IF(IHR.LT.100)THEN
!           WRITE(DESCR2,1011) IHR
!          ELSE
!           WRITE(DESCR2,1013) IHR
!          END IF
 1011      FORMAT('.GrbF',I2.2)
!1013      FORMAT('.GrbF',I3.3)
!          FNAME = DATSET(1:KDAT) // DESCR2
	  END IF
!
         ELSEIF(ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).NE.BLANK) THEN
	  IF(IFMIN .GE. 1)THEN
	   WRITE(DESCR3,1012) IFMIN
           IF (IHR.LT.100) THEN
	      WRITE(DESCR2,1012) IHR
              FNAME = DATSET(1:KDAT) // DESCR2(1:2)  //':'// DESCR3(1:2) &  
      	         //'.'// RESTHR
           ELSE
	      WRITE(DESCR2,1014) IHR
              FNAME = DATSET(1:KDAT) // DESCR2(1:3)  //':'// DESCR3(1:2) &
      	         //'.'// RESTHR
           ENDIF
	  ELSE
           IF (IHR.LT.100) THEN
             WRITE(DESCR2,1012) IHR
             FNAME = DATSET(1:KDAT) // DESCR2(1:2)  //'.'// RESTHR
           ELSE
             WRITE(DESCR2,1014) IHR
             FNAME = DATSET(1:KDAT) // DESCR2(1:3)  //'.'// RESTHR
           ENDIF
          end if
         ELSE
	  IF(IFMIN .GE. 1)THEN
	   WRITE(DESCR3,1012) IFMIN
           IF (IHR.LT.100) THEN
	     WRITE(DESCR2,1012) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:2)  &
      	     //':'// DESCR3(1:2) //'.'// RESTHR
           ELSE
	     WRITE(DESCR2,1014) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:3)  &
      	     //':'// DESCR3(1:2) //'.'// RESTHR
           ENDIF
	  ELSE
           IF (IHR.LT.100) THEN
             WRITE(DESCR2,1012) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:2) &
                    //'.'// RESTHR
 1012        FORMAT(I2.2)
 1014        FORMAT(I3.3)
           ELSE
             WRITE(DESCR2,1014) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:3) &
                    //'.'// RESTHR
           ENDIF
          end if
         ENDIF
!
!        ASSIGN AND OPEN UNIT FOR GRIB DATA FILE.
         if ( num_servers .eq. 0 ) then
         CLOSE(LUNOUT)
         CALL BAOPENWT(LUNOUT,FNAME,IER)
         IF (IER.NE.0) WRITE(6,*)                                   &
            'GRIBIT:  BAOPEN ERROR FOR GRIB DATA ','FILE.  IER=',IER
         WRITE(6,*)'GRIBIT:  OPENED ',LUNOUT,                       &
              ' FOR GRIB DATA ',FNAME
         end if
!     
!        SET OPEN-UNIT FLAGS TO FALSE.
         RITEHD = .FALSE.
         NEWFILE = .TRUE.
      ENDIF
!
!     SEND DATA TO I/O SERVERS
!
!
       if ( num_servers .gt. 0 ) then
       DO I = 1, 5
          CALL MPI_WAIT(IH(I),STATUS,IERR)
       END DO
       NEWFILE_S = NEWFILE
       LUNOUT_S = LUNOUT
       FNAME_S = FNAME
       KBUF_S(1:ITOT) = KBUF(1:ITOT)
       CALL MPI_ISEND(DONE,1,MPI_LOGICAL,                             &  
                  0,1,MPI_COMM_INTER,IH(1),IERR)
       CALL MPI_ISEND(NEWFILE_S,1,MPI_LOGICAL,                        &
                  0,2,MPI_COMM_INTER,IH(2),IERR)
       CALL MPI_ISEND(LUNOUT_S,1,MPI_INTEGER,                         &
                  0,3,MPI_COMM_INTER,IH(3),IERR)
       CALL MPI_ISEND(FNAME_S,80,MPI_CHARACTER,                       &
                  0,4,MPI_COMM_INTER,IH(4),IERR)
       CALL MPI_ISEND(KBUF_S,ITOT,MPI_CHARACTER,                      &
                  0,5,MPI_COMM_INTER,IH(5),IERR)
!
      else
    
!          
!     WRITE GRIB1 MESSAGE TO OUTPUT FILE.
      CALL WRYTE(LUNOUT,ITOT,KBUF)
      end if
!     
!     WRITE DIAGNOSTIC MESSAGE.
!        ID(8)  = INDICATOR OF PARAMETER AND UNITS (TABLE 2)
!        ID(9)  = INDICATOR OF TYPE OF LEVEL       (TABLE 3)
!        ID(10) = VALUE 1 OF LEVEL  (0 FOR 1-100,102,103,105,107
!              111,160   LEVEL IS IN ID WORD 11)
!        ID(11) = VALUE 2 OF LEVEL
666   WRITE(6,1050) ID(8),FIELD(IFLD),ID(9),ID(10),ID(11)
 1050 FORMAT('GRIBIT:  ',I3,1X,A20,1X,I3,1X,I5,1X,I5)
!     
!     END OF ROUTINE.
!     
      END IF
!      time_output = time_output + rtc() - ist
      RETURN
      END
!        IGDS VARIES DEPENDING ON GRID REPRESENTATION TYPE.
!
!       LAT/LON GRID:
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!           IGDS( 4) = NO. OF POINTS ALONG A LATITUDE
!           IGDS( 5) = NO. OF POINTS ALONG A LONGITUDE MERIDIAN
!           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH - IVE)
!           IGDS( 7) = LONGITUDE OF ORIGIN (WEST -IVE)
!           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!           IGDS( 9) = LATITUDE OF EXTREME POINT (SOUTH - IVE)
!           IGDS(10) = LONGITUDE OF EXTREME POINT (WEST - IVE)
!           IGDS(11) = LATITUDE INCREMENT
!           IGDS(12) = LONGITUDE INCREMENT
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = ... THROUGH ...
!           IGDS(18) =   ... NOT USED FOR THIS GRID
!           IGDS(19) - IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS
!                      IN EACH OF 73 ROWS.
!
!       GAUSSIAN GRID:
!           IGDS( 1) = ... THROUGH ...
!           IGDS(10) =   ... SAME AS LAT/LON GRID
!           IGDS(11) = NUMBER OF LATITUDE LINES BETWEEN A POLE
!                      AND THE EQUATOR
!           IGDS(12) = LONGITUDE INCREMENT
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = ... THROUGH ...
!           IGDS(18) =   ... NOT USED FOR THIS GRID
!
!       SPHERICAL HARMONICS:
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!           IGDS( 4) = J - PENTAGONAL RESOLUTION PARAMETER
!           IGDS( 5) = K - PENTAGONAL RESOLUTION PARAMETER
!           IGDS( 6) = M - PENTAGONAL RESOLUTION PARAMETER
!           IGDS( 7) = REPRESENTATION TYPE (CODE TABLE 9)
!           IGDS( 8) = REPRESENTATION MODE (CODE TABLE 10)
!           IGDS( 9) = ... THROUGH ...
!           IGDS(18) =   ... NOT USED FOR THIS GRID
!
!       POLAR STEREOGRAPHIC:
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!                                              1=SOUTH POLE ON PLANE,
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = ... THROUGH ...
!           IGDS(18) =   .. NOT USED FOR THIS GRID
!
!       MERCATOR:
!           IGDS( 1) = ... THROUGH ...
!           IGDS(12) =   ... SAME AS LAT/LON GRID
!           IGDS(13) = LATITUDE AT WHICH PROJECTION CYLINDER
!                        INTERSECTS EARTH
!           IGDS(14) = SCANNING MODE FLAGS
!           IGDS(15) = ... THROUGH ...
!           IGDS(18) =   .. NOT USED FOR THIS GRID
!
!       LAMBERT CONFORMAL:
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!                                              1=SOUTH POLE ON PLANE,
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = NOT USED
!           IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
!                      SECANT CONE CUTS THE SPERICAL EARTH
!           IGDS(16) = SECOND LATITUDE ...
!           IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
!           IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
!
!       ARAKAWA SEMI-STAGGERED E-GRID ON ROTATED LAT/LON GRID
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [201]
!           IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
!                            INCLUDED ON GRID
!           IGDS( 5) = NJ  - DUMMY SECOND DIMENSION; SET=1
!           IGDS( 6) = LA1 - LATITUDE  OF FIRST GRID POINT
!           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!           IGDS( 9) = LA2 - NUMBER OF MASS POINTS ALONG
!                            SOUTHERNMOST ROW OF GRID
!           IGDS(10) = LO2 - NUMBER OF ROWS IN EACH COLUMN
!           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = ... THROUGH ...
!           IGDS(18) = ... NOT USED FOR THIS GRID (SET TO ZERO)
!
!       ARAKAWA FILLED E-GRID ON ROTATED LAT/LON GRID
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [202]
!           IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
!                            INCLUDED ON GRID
!           IGDS( 5) = NJ  - DUMMY SECOND DIMENTION; SET=1
!           IGDS( 6) = LA1 - LATITUDE LATITUDE OF FIRST GRID POINT
!           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!           IGDS( 9) = LA2 - NUMBER OF (ZONAL) POINTS IN EACH ROW
!           IGDS(10) = LO2 - NUMBER OF (MERIDIONAL) POINTS IN EACH
!                            COLUMN
!           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = ... THROUGH ...
!           IGDS(18) = ... NOT USED FOR THIS GRID
!
!       ARAKAWA STAGGERED E-GRID ON ROTATED LAT/LON GRID
!           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!           IGDS( 2) = PV, PL OR 255
!           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [203]
!           IGDS( 4) = NI  - NUMBER OF DATA POINTS IN EACH ROW
!           IGDS( 5) = NJ  - NUMBER OF ROWS
!           IGDS( 6) = LA1 - LATITUDE OF FIRST GRID POINT
!           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!           IGDS( 9) = LA2 - CENTRAL LATITUDE
!           IGDS(10) = LO2 - CENTRAL LONGTITUDE
!           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!           IGDS(14) = ... THROUGH ...
!           IGDS(18) = ... NOT USED FOR THIS GRID
!
