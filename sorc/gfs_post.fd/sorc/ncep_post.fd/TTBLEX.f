      SUBROUTINE TTBLEX(TREF,TTBL,ITB,JTB,KARR,PMIDL         &
                       ,PL,QQ,PP,RDP,THE0,STHE,RDTHE,THESP   &
      ,                 IPTB,ITHTB)
!FPP$ NOCONCUR R
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    TTBLEX      COMPUTES T ALONG A MOIST ADIABAT
!   PRGRMMR: BLACK           ORG: W/NP2      DATE: ??-??-??
!
! ABSTRACT:
!     THIS ROUTINE COMPUTES THE TEMPERATURE ALONG A MOIST
!     ADIABAT GIVEN THE SATURATION POTENTIAL TEMPERATURE
!     AND THE PRESSURE
!   .
!
! PROGRAM HISTORY LOG:
!   ??-??-??  T BLACK - ORIGINATOR
!   98-06-12  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-22  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-01-15  MIKE BALDWIN - WRF VERSION
!
!   OUTPUT FILES:
!     NONE
!
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!----------------------------------------------------------------------
      use ctlblk_mod, only: jsta, jend, im, jsta_2l, jend_2u, me
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!----------------------------------------------------------------------

      integer,intent(in) :: ITB,JTB
      integer,intent(in) ::  KARR(IM,jsta:jend)
      real,dimension(JTB,ITB),intent(in)             :: TTBL
      real,dimension(IM,JSTA_2L:JEND_2U),intent(in)  :: PMIDL
      real,dimension(IM,JSTA_2L:JEND_2U),intent(out) :: TREF
      real,dimension(IM,jsta:jend),intent(out)       :: QQ,PP
      real,dimension(IM,jsta:jend),intent(in)        :: THESP
      real,dimension(ITB),  intent(in)               :: THE0,STHE
      integer,dimension(IM,jsta:jend),intent(out)    :: IPTB,ITHTB
      real,intent(in)                                :: PL,RDP,RDTHE

!
      integer I,J,ITH,IP,IPTBK
      real PK,TPK,T00K,T10K,T01K,T11K,BTHE00K,STHE00K,BTHK,STHK, &
           TTHK,BTHE10K,STHE10K
!-----------------------------------------------------------------------
!$omp  parallel do                                                      &
!$omp& private(i,j,bthe00k,bthe10k,bthk,ip,iptbk,ith,pk,sthe00k,sthe10k,&
!$omp&         sthk,t00k,t01k,t10k,t11k,tpk,tthk)
      DO J=JSTA,JEND
        DO I=1,IM
          IF(KARR(I,J) > 0) THEN
!--------------SCALING PRESSURE & TT TABLE INDEX------------------------
            PK  = PMIDL(I,J)
            TPK = (PK-PL)*RDP
            QQ(I,J)   = TPK-AINT(TPK)
            IPTB(I,J) = INT(TPK) + 1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
            IF(IPTB(I,J) < 1) THEN
              IPTB(I,J) = 1
              QQ(I,J)   = 0.
            ENDIF
!
            IF(IPTB(I,J) >= ITB) THEN
              IPTB(I,J) = ITB-1
              QQ(I,J)   = 0.
            ENDIF
!--------------BASE AND SCALING FACTOR FOR THE--------------------------
            IPTBK   = IPTB(I,J)
            BTHE00K = THE0(IPTBK)
            STHE00K = STHE(IPTBK)
            BTHE10K = THE0(IPTBK+1)
            STHE10K = STHE(IPTBK+1)
!--------------SCALING THE & TT TABLE INDEX-----------------------------
            BTHK    = (BTHE10K-BTHE00K)*QQ(I,J)+BTHE00K
            STHK    = (STHE10K-STHE00K)*QQ(I,J)+STHE00K
            TTHK    = (THESP(I,J)-BTHK)/STHK*RDTHE
            PP(I,J) = TTHK-AINT(TTHK)
!     write(1000+me,*)' i=',i,' j=',j,' tthk=',tthk,' thesp=',thesp(i,j) &
!            , ' bthk=',bthk,' sthk=',sthk,' rdthe=',rdthe

            ITHTB(I,J) = INT(TTHK)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
            IF(ITHTB(I,J) < 1) THEN
              ITHTB(I,J) = 1
              PP(I,J)    = 0.
            ENDIF
!
            IF(ITHTB(I,J) >= JTB) THEN
              ITHTB(I,J) = JTB-1
              PP(I,J)    = 0.
            ENDIF
!--------------TEMPERATURE AT FOUR SURROUNDING TT TABLE PTS.------------
            ITH  = ITHTB(I,J)
            IP   = IPTB(I,J)
            T00K = TTBL(ITH  ,IP  )
            T10K = TTBL(ITH+1,IP  )
            T01K = TTBL(ITH  ,IP+1)
            T11K = TTBL(ITH+1,IP+1)
!--------------PARCEL TEMPERATURE-------------------------------------
            TREF(I,J) = (T00K+(T10K-T00K)*PP(I,J)+(T01K-T00K)*QQ(I,J)    &
                      + (T00K-T10K-T01K+T11K)*PP(I,J)*QQ(I,J))
          ENDIF
        ENDDO
      ENDDO
!
      RETURN
      END
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
