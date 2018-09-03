      SUBROUTINE IDCHCK(NSLOTS,NSLOT,ISUBV,ISUB,VARFND,FAXFND,AFOFND)
C                                                                       00396400
C  THIS SUBROUTINE CHECKS THE SUBSET NUMBERS READ IN FROM A GRAPHICS    00396500
C  SCHEDULE FORMAT AND DETERMINES IF THE MAIN PROGRAM PRODUCES AN       00396600
C  AFOS, VARIAN, FAX, OR BOTH FAX/VARIAN MAPS.  THIS INFORMATION IS     00396700
C  THEN PLACED INTO THE FOURTH BYTE OF IFID(1) WHICH IS EVENTUALLY      00396800
C  DISPLAYED ON IPAK.                                                   00396900
C                                                                       00397000
C  THIS VERSION IS DATED 06-18-86                                       00397100
C                                                                       00397200
      COMMON/ILY/II,LL,IFID(14),JFID(14)
C                                                                       00397400
      LOGICAL  FAXFND,VARFND,AFOFND
C                                                                       00397600
      DATA IBOTH/'   B'/
      DATA IAFO/'   X'/
      DATA IFAX/'   F'/
      DATA IVAR/'   V'/
      DATA ID/'    '/
      DATA IBLANK/'    '/
C                                                                       00398300
      IF(NSLOTS .NE. 1)GO TO 1000
C                                                                       00398500
C  IF ONLY A VARIAN MAP IS PRODUCED                                     00398600
C                                                                       00398700
      IF(ISUBV .NE. 99)VARFND=.TRUE.
      IF(NSLOTS.EQ.NSLOT)GO TO 3000
      GO TO 4000
C                                                                       00399100
C  IF NOT ONLY A VARIAN MAP IS PRODUCED                                 00399200
C                                                                       00399300
 1000 IF(ISUBV .NE. ISUB)GO TO 2000
C                                                                       00399500
C  THE SUBSET NUMBER GIVEN IS A VARIAN MAP                              00399600
C                                                                       00399700
      IF(ISUBV .NE. 99)VARFND=.TRUE.
      IF(NSLOTS.EQ.NSLOT)GO TO 3000
      GO TO 4000
C                                                                       00400100
C  THE SUBSET NUMBER GIVEN IS A FAX OR AN AFOS MAP                      00400200
C                                                                       00400300
 2000 IF(ISUB .LT. 5000)FAXFND=.TRUE.
      IF(ISUB .GE. 5000)AFOFND=.TRUE.
      IF(NSLOTS.EQ.NSLOT)GO TO 3000
      GO TO 4000
C                                                                       00400800
C  GO HERE IF ALL SUBSET NUMBERS IN RUN HAVE BEEN REVIEWED              00400900
C                                                                       00401000
 3000 IF(AFOFND)ID=IAFO
      IF(FAXFND)ID=IFAX
      IF(VARFND)ID=IVAR
      IF((FAXFND).AND.(VARFND))ID=IBOTH
C                                                                       00401500
      IFID(1) = ID
      AFOFND = .FALSE.
      FAXFND = .FALSE.
      VARFND = .FALSE.
C                                                                       00402000
      IF(ID .EQ. IBLANK)PRINT 100
  100 FORMAT(3X,'>>>>> FOURTH BYTE OF IFID(1) IS BLANK <<<<<')
C                                                                       00402300
 4000 CONTINUE
C                                                                       00402500
      RETURN
      END
