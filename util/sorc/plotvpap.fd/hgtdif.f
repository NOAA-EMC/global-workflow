       subroutine hgtdif(ihgtra1,ihgtra2,idifra,jdimen,ndifs,iret_dif)
C                                                       12-AUG-1996/DSS
       INTEGER   IHGTRA1(2,JDIMEN)
       INTEGER   IHGTRA2(2,JDIMEN)
       INTEGER   IDIFRA(2,JDIMEN)
       INTEGER   NDIFS
       INTEGER   IRET_DIF

       INTEGER   IJBSIRCH
       EXTERNAL  IJBSIRCH  		!... DEFINE INT FUNCTION

       INTEGER   NAME
       INTEGER   NOBS1,NOBS2
       INTEGER   IERR1,IERR2
       INTEGER   IHGTVAL1

       SAVE

       IRET_DIF = 0
       NDIFS = 0

       CALL ISORT2D(IHGTRA1,JDIMEN,NOBS1,IERR1)

       IF(IERR1 .GT. 0) THEN
          IRET_DIF = 1
          GO TO 999
       ENDIF

       CALL ISORT2D(IHGTRA2,JDIMEN,NOBS2,IERR2)

       IF(IERR2 .GT. 0) THEN
          IRET_DIF = 2
          GO TO 999
       ENDIF

       NDIFS = 0
       DO  JSTN = 1,NOBS1
         NAME = IHGTRA1(1,JSTN)
         IHGTVAL1 = IHGTRA1(2,JSTN)
         JPTR = IJBSIRCH(NAME,IHGTRA2,2,NOBS2)
         IF(JPTR .GT. 0) THEN
           NDIFS = NDIFS + 1
           IDIFRA(1,NDIFS) = NAME
           IDIFRA(2,NDIFS) = IHGTVAL1 - IHGTRA2(2,JPTR)
C          ... THIS IS INACCURATE AT THE UNIT POSITION ...
C          ... GIVEN DATA SHOULD HOLD MORE ACCURACY IN ORDER TO 
C          ... GET ACCURACY AT THE UNIT LEVEL.

         ENDIF
       ENDDO

       IF(NDIFS .LT. JDIMEN) THEN
         IDIFRA(1,NDIFS+1) = 0
         IDIFRA(2,NDIFS+1) = 0
       ENDIF

  999  continue
       RETURN
       END
