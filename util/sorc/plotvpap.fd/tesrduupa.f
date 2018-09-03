       subroutine  tesrduupa
C                                                 22-JAN-1997/dss
C     ... copied tesrdupa into tesrduupa

C                                                  6-JAN-1996/DSS
C     ... COPIED LARRY'S MAIN.F INTO tesrdupa
C
C     This is a test program to use the subroutine rd_uupa          
C     to return one upper air station at a time from the 
C     rd_uupa data file
C
C     Usage:
C    
C            CALL rd_uupa(LUNUPA,LINITQ, LVLIX, HEDER, LONELVL, IRET )
C
C     where the input parameters are,
C
C            LUNUPA   -   Input file number
C            LINITQ   -   LOGICAL SWITCH 
C                      = .TRUE. ;  TO COMMAND RD_UUPA TO REWIND
C                            THE INPUT FILE AND READ THE FILE HEADER
C                            BEFORE READING THE OBSERVATIONS;
C                            SUBR RD_UUPA WILL RESET THAT SWITCH TO =.F.
C                            SO THAT ON SUBSEQUENT CALLS TO GET THE
C                            NEXT REPORTS, IT WILL NOT INITIALIZE.
C                            
C            LVLIX    -   MILLIBAR LEVEL BY INDEX NUMBER
C    
C     and the output parameters are,
C          
C            HEDER  -   The rd_uupa file header record.
C            LONELVL(18) - ONE LEVEL'S DATA IN BINARY
C
C            IRET   -   Return code:        0 = report returned
C                                          -1 = end of file
C
       INTEGER      MAXWRDLVL
       PARAMETER   (MAXWRDLVL=24)
       INTEGER      NWRDLVLPAK
       PARAMETER   (NWRDLVLPAK=MAXWRDLVL/2)

       INTEGER      MAXWRDHDR
       PARAMETER   (MAXWRDHDR=50)

C     -----------------------------------------------------------------
      INTEGER    MAXOBS
      PARAMETER (MAXOBS=20000)
      INTEGER    NDATASIZ
      PARAMETER (NDATASIZ=MAXOBS+1)
      INTEGER    LMTWRDPOB
      PARAMETER (LMTWRDPOB=10)
      INTEGER    LMTHFWPOB
      PARAMETER (LMTHFWPOB=2*LMTWRDPOB)		!... =(20)

      COMMON  /OBSLVLDB/NOBSDB,LVLIX,IOBS2PK
      INTEGER           IOBS2PK(LMTWRDPOB,NDATASIZ)

      COMMON  /JSPACE/IDREC,NDATA
      INTEGER         IDREC(6)
      INTEGER         NDATA(3,NDATASIZ)

      
C     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
       INTEGER      IMAX
       PARAMETER   (IMAX=53)
       INTEGER      JMAX
       PARAMETER   (JMAX=57)

       REAL         FLD(IMAX,JMAX)

       logical      LINITQ
      INTEGER       LVLIX
      INTEGER       LONELVL(MAXWRDLVL)
      REAL         FLONELVL(MAXWRDLVL)

      INTEGER       IONELVLPAK(NWRDLVLPAK)


      CHARACTER*8   CHGTS(11)
      CHARACTER*8   HEDER(MAXWRDHDR)
C
C
C
      DATA  CHGTS  /'1000    ',' 925    ',' 850    ',' 700    ',
     1              ' 500    ',' 400    ',' 300    ',' 250    ',
     1              ' 200    ',' 150    ',' 100    '/
C
       INTEGER    KRUN
       INTEGER    KEIL
       INTEGER    ITOUT
       INTEGER    KLLGO2
       LOGICAL    LINTERPQQ
       REAL       HGTM
       LOGICAL    LINBOUNDQQ
       REAL       ALAT
       REAL       ALONG


      integer      namstn
      character*8  cnamstn
      equivalence (namstn,cnamstn)

C      ... ARGS FOR HALF-PACKING VIA SBYTES ...
       INTEGER     NOFFSET
       DATA        NOFFSET   /  0 /
       INTEGER     KBITPGRP
       DATA        KBITPGRP  / 32 /
       INTEGER     KPADBITS
       DATA        KPADBITS  /  0 /
       INTEGER     NGRPS2DO  
C      . . . . . . . . . . . . . . . . . . . .
       INTEGER   ISCRDATA(3,NDATASIZ)
       INTEGER   ISKEY
       DATA      ISKEY   / 1 /
       INTEGER   KEYML
       DATA      KEYML   / 6 /
       INTEGER   KEYMH
       DATA      KEYMH   / 41 /
       INTEGER   IRET_SOR
C      . . . . . . . . . . . . . . . . . . . .
       integer     mskJIJ
       data        mskJIJ    / X'000001FFFFFFFFE0' /
       INTEGER     MSKJJJ
       DATA        MSKJJJ     / X'000001FF00000000' /
       INTEGER     NRITSHF
       DATA        NRITSHF    / 32 /
       
       INTEGER     NDIXMAX		!... NO OF J-ROWS IN ANY GRID
       PARAMETER  (NDIXMAX=400)	
       INTEGER     IDIXAD(NDIXMAX)
       INTEGER     IDIXCO(NDIXMAX)

       INTEGER     NDIX
       integer     lvldes

       integer   istat

       SAVE
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

       istat = 0
       NOBSDB = 0

       ITOUT = 2		!... ITOUT SHOULD BE PASSED INTO THIS

       NGRPS2DO = MAXWRDLVL

C      ... DEFINE FOR BOUNDARY-TESTER ...
       DO    J = 1,JMAX
         DO  I = 1,IMAX
           FLD(I,J) = 0.0
         ENDDO
       ENDDO

       KLLGO2 = 3
       KRUN = 2
       KEIL = 1		!... LFM GRID
       LINTERPQQ = .TRUE.
C      . . . . . . . . . . . . . . . . . .

       DO  888 LVLOOP = 1,2
         LVLIX = 3		!... GET THE 850-MB DATA
         IF(LVLOOP .EQ. 1) THEN
           LVLIX = 3
         ELSE IF(LVLOOP .EQ. 2) THEN
           LVLIX = 5
         ENDIF

C      READ A REPORT FROM THE rd_uupa FILE
C
       PRINT *,' Beginning TEST rd_uupa program'
       IRET = 0
       LUNUPA = 41
       LINITQ = .TRUE.		!... RD_UUPA WILL RESET TO .F.
       IREC = 0
       IX = 0

C      ... TO INITIALIZE THE DATABASE TO ALL ZEROS,
       do  j = 1,NDATASIZ
         do  i = 1,LMTWRDPOB
           IOBS2PK(I,J) = 0
         ENDDO
       ENDDO
       NOBSDB = 0
C      . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

  200  continue 	!... do until logical end of file ...
C
C         READ AN UPPER AIR REPORT FROM THE rd_uupa DATA SET
C
          CALL rd_uupa(LUNUPA,LINITQ,LVLIX, HEDER, LONELVL, IRET)
          if(iret .ne. 0) then
            if(iret .EQ. -1) then	!... normal LEOF found
              go to 800
            else
              istat = iret
              go to 999
            endif
          endif

C         ... otherwise, got a report, so process it ...
             IREC = IREC + 1
C 
             DO  IOB = 1,MAXWRDLVL
               IF((IOB .EQ. 11) .OR. (IOB .EQ. 12)) THEN
                 FLONELVL(IOB) = 0.0
               ELSE
                 FLONELVL(IOB) = FLOAT(LONELVL(IOB)) / 4096.0
               ENDIF
             ENDDO

C            ... next perform a bndry test on this lat/lon ...
             ALAT  = FLONELVL(1)
             ALONG = FLONELVL(2)

             call wibound(KRUN,KEIL,KLLGO2,LINTERPQQ,ALAT,ALONG,
     1                     FLD,IMAX,JMAX,HGTM,LINBOUNDQQ)

             print *, IONELVLPAK(2), LONELVL(2),NOFFSET,KBITPGRP,
     1         KPADBITS,  NGRPS2DO        
            CALL SBYTESCCS(IONELVLPAK,LONELVL,NOFFSET,KBITPGRP,KPADBITS,
     1                   NGRPS2DO)
c             call byteswap(IONELVLPAK, 8, MAXWRDLVL)

             IF((LONELVL(14+8) .NE. 0) .AND. (LINBOUNDQQ)) THEN
C               ... STASH INTO DATABASE ...
                IF(NOBSDB .LT. MAXOBS) THEN
                   NOBSDB = NOBSDB + 1
C                  ... TO SKIP THE 7TH WORD; STASH 1:6; 8:11
                   IDEST = 0
                   DO LW = 1,11
                     IF(LW .NE. 7) THEN
                       IDEST = IDEST + 1
                       IOBS2PK(IDEST,NOBSDB) = IONELVLPAK(LW)
                     ENDIF
                   ENDDO
                ELSE
                  WRITE(6,FMT='(1H ,''TESRDUUPA: FAILED ON INADEQUATE'',
     1                          '' SPACE ALLOCATED FOR DATABASE OF '',
     2                          ''OBS BY LVL'',
     2                  /1H ,7X,''NUMBER OF OBS LIMITED TO ='',I8)')
     A                  MAXOBS

                  ISTAT = 1
                  GO TO 999
                ENDIF
             ENDIF
                

             IF(MOD(IREC,10) .EQ. 0) THEN
C              ... PRINT A REPORT EVERY ONCE IN A WHILE ...
C
          

C ...               WRITE(6,116)(LONELVL(I),I=1,MAXWRDLVL)
C ...  116          FORMAT((4Z17.16))

               NAMSTN = IONELVLPAK(6)
               IF(NAMSTN .EQ. 0) THEN
                 CNAMSTN(1:8) = ' '
               ENDIF
               write(6,117) cnamstn(1:8),FLONELVL(1),FLONELVL(2),
     1                      FLONELVL(7)
  117          format(1h ,A8,' LAT ',F7.2,' LONG ',F8.2,' ELEV ',F6.0)
               
               WRITE(6,1172)(IONELVLPAK(I),I=1,NWRDLVLPAK)
 1172          FORMAT((4Z17.16))

               write(6,118)CHGTS(LVLIX),(FLONELVL(I),I=15,21)
  118          format(2X,A6,3X,F7.0,4X,F5.1,
     1              4X,F5.1,2X,F6.0,4X,F5.0,4X,F6.0,2X,F8.0)
               write(6,119)LINBOUNDQQ
  119          FORMAT(1H ,'wibound: LINBOUNDQQ= .',L1,'.',
     1               /1H   )
             ENDIF

       go to 200		!... enddo by report

  800  continue
       WRITE(6,FMT='(1H ,'' rd_uupa test completed with '',I8,
     1                   '' reports;'',
     2             /1H ,7X,''STASHED-INTO-DATABASE COUNT ='',I8)')
     A         IREC,NOBSDB

       DO  J = 1,NDATASIZ
         DO  I = 1,3
           NDATA(I,J) = 0
         ENDDO
       ENDDO

       lvldes = lvlix - 1
       call mksorwrd(lvldes,itout,itct,iret_mks)
       write(6,815)ITCT,iret_mks
  815  format(1h ,'tesrduupa::mksorwrd: ITCT=',I8,
     1            '; return-code=',I4)
       do  JJ = 1,ITCT
         iacc = iand(ndata(1,JJ),mskJIJ)
         IF(IACC .EQ. 0) THEN
           WRITE(6,817)jj
  817      FORMAT(1H ,'tesrduupa::mksorwrd: results terminated at ',
     1                I8,'th word with zero value')
           go to 818
         endif
       enddo
  818  continue

       call presort(KRUN)   

       do  JJ = 1,ITCT
         iacc = iand(ndata(1,JJ),mskJIJ)
         IF(IACC .EQ. 0) THEN
           WRITE(6,819)jj
  819      FORMAT(1H ,'tesrduupa::presort: results terminated at ',
     1                I8,'th word with zero value')
           go to 820
         endif
       enddo

  820  continue
       call sortem(NDATA,3,NDATASIZ,ISCRDATA,ISKEY,KEYML,KEYMH,
     1             IRET_SOR)
       WRITE(6,825)iret_sor
  825  FORMAT(1H ,'tesrduupa::sortem: return-code=',I3)
       IF(IRET_SOR .NE. 0) THEN
         GO TO 999
       ENDIF

       NDIX = JMAX		!... JUST A GUESS AT JROW MAX
       call dixie(NDATA,3,NDATASIZ,ISKEY,MSKJJJ,NRITSHF,
     1            IDIXAD,IDIXCO,NDIX,IRET_DIX)
       WRITE(6,835) IRET_DIX
  835  FORMAT(1H ,'tesrduupa::dixie: RETURN-CODE=',I5)

       call thin_upa(NDATA,3,NDATASIZ,ISKEY,IDIXAD,IDIXCO,NDIX,ITOUT)


  888  CONTINUE		!... ENDDO ON LVLIX . . . . . . . . . . . . . 

  999  continue
C      CALL EXIT(ISTAT)
       return
C
       END
