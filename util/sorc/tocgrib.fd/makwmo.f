      SUBROUTINE MAKWMO (BULHED,IDAY,IHOUR,KWBX,HEADER)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM: MAKWMO         FORMAT THE WMO HEADER
C   PRGMMR: FARLEY           ORG: W/NMC42    DATE: 84-07-06
C
C ABSTRACT: FORMS THE WMO HEADER FOR A GIVEN BULLETIN.
C
C PROGRAM HISTORY LOG:
C   84-07-06  FARLEY      ORIGINAL AUTHOR
C   94-10-10  R.E.JONES   CHANGES FOR CRAY
C   95-10-18  R.E.JONES   ADD PARAMETER KWBX TO CALL 
C   98-06-16  Gilbert     Changed argument list to pass in day and hour
C                         instead of the old O.N. 84 date word.
C 2003-03-28  Gilbert     Removed equivalences.
C
C USAGE:    CALL MAKWMO(BULHED,IDAY,IHOUR,KWBX,HEADER)
C   INPUT ARGUMENT LIST:
C     BULHED   -  TTAAII BULLETIN HEADER                    FT10
C     IDAY     -  Day of Month
C     IHOUR    -  Hour of Day.
C     KWBX     -  4 CHARACTERS (KWBC TO KWBQ)
C
C   OUTPUT ARGUMENT LIST:
C     HEADER   -  COMPLETE WMO HEADER IN ASCII
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
C
      CHARACTER * 6 BULHED
      CHARACTER * 1 HEADER (*)
      CHARACTER * 1 WMOHDR (21)
      CHARACTER * 4 KWBX
      CHARACTER * 2 CTEMP
C
C--------------------------------------------------------------------
C
C$            1.     CREATE WMO HEADER.
C
C$            1.1    CONVERT BULHED FROM EBCDIC TO ASCII.
C
C     WRITE (6,FMT='('' MADE IT TO MAKWMO'')')
C
      DO I = 1,6
        WMOHDR(I) = BULHED(I:I)
      END DO
      WMOHDR(7)=char(32)    !  ASCII BLANK
C
C     MOVE KWBX INTO WMO HEADER
C
      DO I = 1,4
        WMOHDR(I+7) = KWBX(I:I)
      END DO
      WMOHDR(12)=char(32)    !  ASCII BLANK
C
C$            1.2    PICK OFF THE DAY OF MONTH (YY)
C$                   AND CONVERT TO ASCII.
C
      write(ctemp,fmt='(I2.2)') IDAY
      WMOHDR(13)=ctemp(1:1)
      WMOHDR(14)=ctemp(2:2)
C
C$            1.3    PICK OFF THE HOUR(GG) AND CONVERT TO ASCII.
C
      write(ctemp,fmt='(I2.2)') IHOUR
      WMOHDR(15)=ctemp(1:1)
      WMOHDR(16)=ctemp(2:2)
C
C             1.4    FIL IN REST OF HEADER
C
      WMOHDR(17)=char(48)    !  ASCII "0"
      WMOHDR(18)=char(48)    !  ASCII "0"
      WMOHDR(19)=char(13)    !  ASCII CR = '\r'
      WMOHDR(20)=char(13)    !  ASCII CR = '\r'
      WMOHDR(21)=char(10)    !  ASCII LF = '\n'
C
C--------------------------------------------------------------------
C
C$            2.     MOVE WMOHDR TO OUTPUT FIELD.
C
      DO 200 I = 1,21
        HEADER(I) = WMOHDR(I)
  200 CONTINUE
C
      RETURN
      END
