      SUBROUTINE GET_BITS(IBM,SGDS,LEN,MG,G,ISCALE,GROUND,           &
                          GMIN,GMAX,NBIT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    GET_BITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
!   PRGMMR: IREDELL          ORG: W/NP23     DATE: 92-10-31
!
! ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
!   AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
!   THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
!   THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
!   GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
!
! PROGRAM HISTORY LOG:
!   92-10-31  IREDELL
!   95-04-14  BALDWIN - MODIFY FOLLOWING KEITH BRILL'S CODE
!                       TO USE SIG DIGITS TO COMPUTE DEC SCALE
!
! USAGE:   CALL GET_BITS(IBM,ISGDS,LEN,MG,G,ISCALE,GROUND,GMIN,GMAX,NBIT)
!   INPUT ARGUMENT LIST:
!     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!     SGDS     - MAXIMUM SIGNIFICANT DIGITS TO KEEP
!                (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
!                OR BINARY PRECISION IF <0
!                (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
!                           -3.0 "                    " 1/8
!                         2**SGDS PRECISION)
!     LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
!     MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!     G        - REAL (LEN) FIELD
!
!   OUTPUT ARGUMENT LIST:
!     ISCALE   - INTEGER DECIMAL SCALING
!     GROUND   - REAL (LEN) FIELD ROUNDED TO DECIMAL SCALING
!     GMIN     - REAL MINIMUM VALID ROUNDED FIELD VALUE
!     GMAX     - REAL MAXIMUM VALID ROUNDED FIELD VALUE
!     NBIT     - INTEGER NUMBER OF BITS TO PACK
!
! SUBPROGRAMS CALLED:
!   ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!$$$
!
      implicit none
!
      REal,DIMENSION(LEN),intent(in):: G
      real,DIMENSION(LEN),intent(inout) ::  GROUND
      integer,DIMENSION(LEN),intent(in):: MG
      integer,intent(in) :: IBM,LEN
      integer,intent(inout) :: ISCALE,NBIT
      real,intent(out) :: GMAX,GMIN
      integer I1,I,IRETT
      real SGDS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  DETERMINE EXTREMES WHERE BITMAP IS ON
!
      IF(IBM.EQ.0) THEN
        GMAX=G(1)
        GMIN=G(1)
        DO I=2,LEN
          GMAX=MAX(GMAX,G(I))
          GMIN=MIN(GMIN,G(I))
        ENDDO
      ELSE
        I1=0
        DO I=1,LEN
          IF(MG(I).NE.0.AND.I1.EQ.0) I1=I
        ENDDO
        IF(I1.GT.0.AND.I1.LE.LEN) THEN
          GMAX=G(I1)
          GMIN=G(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GMAX=MAX(GMAX,G(I))
              GMIN=MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
!
!
!
      CALL FNDBIT  ( GMIN, GMAX, SGDS, NBIT, ISCALE, IRETT)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
	SUBROUTINE FNDBIT  ( rmin, rmax, rdb, nmbts, iscale, iret )
!************************************************************************
!* FNDBIT								*
!*									*
!* This subroutine computes the number of packing bits given the	*
!* maximum number of significant digits to preserve or the binary	*
!* precision to store the data.  The binary precision is given as a	*
!* negative integer, ISCALE will always be zero in this case.		*
!*									*
!* The binary precision translates as follows:				*
!*     -1  =>  store data to nearest 1/2				*
!*     -2  =>  store data to nearest 1/4				*
!*     -3  =>  store data to nearest 1/8				*
!*									*
!* Note that a fractional number of significant digits is allowed.	*
!*									*
!* FNDBIT ( AMIN, AMAX, RDB, NBITS, ISCALE, IRET )			*
!*									*
!* Input parameters:							*
!*	AMIN 		REAL		Minimum value			*
!*	AMAX		REAL		Maximum value			*
!*	RDB		REAL		Maximum # of significant digits	*
!*					  OR binary precision if < 0	*
!*									*
!* Output parameters:							*
!*	NBITS		INTEGER		Number of bits for packing	*
!*	ISCALE		INTEGER		Power of 10 scaling to use	*
!*	IRET		INTEGER		Return code			*
!*					  0 = normal return		*
!**									*
!* Log:									*
!* K. Brill/NMC		06/92						*
!* K. Brill/EMC		12/95	Added binary precision			*
!* M. Baldwin           10/96   Added fix for negative nmbts
!************************************************************************
!*
!
    implicit none
!
    integer,intent(inout) ::  iscale,nmbts
    real,intent(inout)    ::  rmin,rmax,rdb
    real                  ::  range,rr,rng2,po,rln2
    integer               ::  iret,icnt,ipo,le,ibin
!    
	DATA		rln2/0.69314718/
!-----------------------------------------------------------------------
	iret = 0
	icnt = 0
	iscale = 0
	range = rmax - rmin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF
!*
	IF ( rdb .eq. 0.0 ) THEN
	    nmbts = 8
	    RETURN
	ELSE IF ( rdb .gt. 0.0 ) THEN
	    ipo = INT (ALOG10 ( range ))
	    IF ( range .lt. 1.00 ) ipo = ipo - 1
	    po = float(ipo) - rdb + 1.
	    iscale = - INT ( po )
	    rr = range * 10. ** ( -po )
	    nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
	ELSE
	    ibin = NINT ( -rdb )
	    rng2 = range * 2. ** ibin
	    nmbts = INT ( ALOG ( rng2 ) / rln2 ) + 1
	END IF
!*
        IF(NMBTS.LE.0) THEN
          NMBTS=0
          IF(ABS(RMIN).GE.1.) THEN
            ISCALE=-INT(ALOG10(ABS(RMIN)))
          ELSE IF (ABS(RMIN).LT.1.0.AND.ABS(RMIN).GT.0.0) THEN
            ISCALE=-INT(ALOG10(ABS(RMIN)))+1
          ELSE
            ISCALE=0
          ENDIF
        ENDIF
	RETURN
	END
