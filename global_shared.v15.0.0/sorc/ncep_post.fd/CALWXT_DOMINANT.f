       SUBROUTINE CALWXT_DOMINANT_POST(PREC,RAIN,FREEZR,SLEET,SNOW,     &
     &                                 DOMR,DOMZR,DOMIP,DOMS)
!
!     WRITTEN: 24 AUGUST 2005, G MANIKIN 
!      
!     THIS ROUTINE TAKES THE PRECIP TYPE SOLUTIONS FROM DIFFERENT
!       ALGORITHMS AND SUMS THEM UP TO GIVE A DOMINANT TYPE
!
!     use params_mod
      use ctlblk_mod, only: jsta, jend, pthresh, im, jsta_2l, jend_2u
!     use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      integer,PARAMETER :: NALG=5
!    INPUT:
      REAL PREC(IM,jsta_2l:jend_2u)
      real,DIMENSION(IM,jsta:jend),     intent(inout) ::  DOMS,DOMR,DOMZR,DOMIP
      real,DIMENSION(IM,jsta:jend,NALG),intent(in)    ::  RAIN,SNOW,SLEET,FREEZR
      integer I,J,L
      real TOTSN,TOTIP,TOTR,TOTZR
!--------------------------------------------------------------------------
!$omp  parallel do  private(i,j)
      DO J=JSTA,JEND
        DO I=1,IM
          DOMR(I,J)  = 0.
          DOMS(I,J)  = 0.
          DOMZR(I,J) = 0.
          DOMIP(I,J) = 0.
        ENDDO
      ENDDO
!
!$omp  parallel do private(i,j,totsn,totip,totr,totzr)
      DO J=JSTA,JEND
        DO I=1,IM
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
          IF (PREC(I,J) <= PTHRESH) cycle
          TOTSN = 0
          TOTIP = 0
          TOTR  = 0
          TOTZR = 0 
!   LOOP OVER THE NUMBER OF DIFFERENT ALGORITHMS THAT ARE USED
          DO L = 1, NALG
            IF (RAIN(I,J,L) >  0) THEN
              TOTR = TOTR + 1
              cycle
            ENDIF

            IF (SNOW(I,J,L) >  0) THEN
              TOTSN = TOTSN + 1
              cycle
            ENDIF

            IF (SLEET(I,J,L) >  0) THEN
              TOTIP = TOTIP + 1
              cycle
            ENDIF

            IF (FREEZR(I,J,L) >  0) THEN
              TOTZR = TOTZR + 1
            ENDIF
          enddo

!   TIES ARE BROKEN TO FAVOR THE MOST DANGEROUS FORM OF PRECIP
!     FREEZING RAIN > SNOW > SLEET > RAIN 
          IF (TOTSN > TOTIP) THEN
            IF (TOTSN > TOTZR) THEN
              IF (TOTSN  >=  TOTR) THEN
                DOMS(I,J) = 1
              ELSE
                DOMR(I,J) = 1 
              ENDIF
            ELSE IF (TOTZR  >=  TOTR) THEN
              DOMZR(I,J) = 1
            ELSE
              DOMR(I,J) = 1
            ENDIF 
          ELSE IF (TOTIP  >  TOTZR) THEN
            IF (TOTIP  >= TOTR) THEN
              DOMIP(I,J) = 1
            ELSE
              DOMR(I,J) = 1
            ENDIF
          ELSE IF (TOTZR >= TOTR) THEN
            DOMZR(I,J) = 1
          ELSE
            DOMR(I,J) = 1
          ENDIF
        enddo
      enddo
!
      RETURN
      END
