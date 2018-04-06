       SUBROUTINE WETBULB(T,Q,PMID,HTM,KARR,TWET)
!
!     FILE: WETBULB.f
!     WRITTEN: 10 SEPTEMBER 1993, MICHAEL BALDWIN
!     REVISIONS: 
!     CONVERSION TO 2-D: 12 JUNE 1998 (T BLACK)
!     MPI VERSION: 04 Jan 2000 ( JIM TUCCILLO )
!     MODIFIED FOR HYBRID: OCT 2001, H CHUANG
!     02-01-15  MIKE BALDWIN - WRF VERSION
!
!-----------------------------------------------------------------------
!     ROUTINE TO COMPUTE WET BULB TEMPERATURES USING THE LOOK UP TABLE
!     APPROACH THAT IS USED IN CUCNVC
!  
!     FOR A GIVEN POINT K AND LAYER L:
!      THETA E IS COMPUTED FROM THETA AND Q BY LIFTING THE PARCEL TO
!      ITS SATURATION POINT.
!      THEN THE WET BULB TEMPERATURE IS FOUND BY FOLLOWING THE THETA E
!      DOWN TO THE ORIGINAL PRESSURE LEVEL (USING SUBROUTINE TTBLEX).     
!
!   
      use lookup_mod, only: thl, rdth, jtb, qs0, sqs, rdq, itb, ptbl, plq, ttbl,&
              pl, rdp, the0, sthe, rdthe, ttblq, itbq, jtbq, rdpq, the0q, stheq,&
              rdtheq
      use ctlblk_mod, only: jsta, jend, im, jsta_2l, jend_2u, lm
      use cuparm_mod, only: h10e5, capa, epsq, d00, elocp
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!-----------------------------------------------------------------------
!  LIST OF VARIABLES NEEDED
!    PARAMETERS:
!      INCLUDED IN "cuparm" AND "parm.tbl"
!    INPUT:
!      T,Q,HTM,PMID(3-D),KARR (2-D)
!    OUTPUT: 
!      TWET (3-D)
!    SUBROUTINES CALLED:
!      TTBLEX
!
      real,dimension(IM,jsta_2l:jend_2u,LM),intent(in)  :: T,Q,    &
                                                           PMID,HTM
      integer,dimension(IM,jsta:jend),      intent(in)  :: KARR
      real,dimension(IM,jsta_2l:jend_2u,LM),intent(out) :: TWET


      real,    dimension(im,jsta:jend) :: THESP, QQ, PP
      integer, dimension(im,jsta:jend) :: KLRES,KHRES,IPTB,ITHTB
!
      integer I,J,L,ITTB1,ITTBK,IQTBK,IT,KNUML,KNUMH,IQ
      real TBTK,QBTK,APEBTK,TTHBTK,TTHK,QQK,BQS00K,SQS00K,BQS10K,      &
           SQS10K,BQK,SQK,TQK,PPK,TPSPK,APESPK,PRESPK,P00K,P10K,P01K,  &
           P11K,PRESK
!
!--------------COMPUTE WET BULB TEMPERATURES----------------------------
!!$omp  parallel do
!!$omp& private(apebtk,apespk,bqk,bqs00k,bqs10k,iq,iqtbk,it,ittb1,ittbk,
!!$omp&         karr,khres,klres,knumh,knuml,p00k,p01k,p10k,p11k,ppk,
!!$omp&         presk,qbtk,qqk,sqk,sqs00k,sqs10k,tbtk,thesp,tpspk,
!!$omp&         tqk,tthbtk,tthk)
!-----------------------------------------------------------------------
                             DO 300 L=1,LM
      DO 125 J=JSTA,JEND
      DO 125 I=1,IM
        IF (HTM(I,J,L).LT.1.0) THEN
          THESP(I,J)=273.15
          GOTO 125
        ENDIF
        TBTK  =T(I,J,L)
        QBTK  =Q(I,J,L)
        PRESK =PMID(I,J,L)
        APEBTK=(H10E5/PRESK)**CAPA
        IF(QBTK.LT.EPSQ) QBTK=HTM(I,J,L)*EPSQ
!--------------SCALING POTENTIAL TEMPERATURE & TABLE INDEX--------------
        TTHBTK  =TBTK*APEBTK
        TTHK    =(TTHBTK-THL)*RDTH
        QQK     =TTHK-AINT(TTHK)
        ITTB1   =INT(TTHK)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(ITTB1.LT.1) THEN
          ITTB1  =1
          QQK    =D00
        ENDIF
!
        IF(ITTB1.GE.JTB) THEN
        ITTB1  =JTB-1
        QQK    =D00
        ENDIF
!--------------BASE AND SCALING FACTOR FOR SPEC. HUMIDITY---------------
        ITTBK=ITTB1
        BQS00K=QS0(ITTBK)
        SQS00K=SQS(ITTBK)
        BQS10K=QS0(ITTBK+1)
        SQS10K=SQS(ITTBK+1)
!--------------SCALING SPEC. HUMIDITY & TABLE INDEX---------------------
        BQK=(BQS10K-BQS00K)*QQK+BQS00K
        SQK=(SQS10K-SQS00K)*QQK+SQS00K
        TQK=(QBTK-BQK)/SQK*RDQ
        PPK=TQK-AINT(TQK)
        IQTBK=INT(TQK)+1
!--------------KEEPING INDICES WITHIN THE TABLE-------------------------
        IF(IQTBK.LT.1) THEN
          IQTBK =1
          PPK   =D00
        ENDIF
!
        IF(IQTBK.GE.ITB) THEN
          IQTBK=ITB-1
          PPK  =D00
        ENDIF
!--------------SATURATION PRESSURE AT FOUR SURROUNDING TABLE PTS.-------
        IQ=IQTBK
        IT=ITTB1
        P00K=PTBL(IQ  ,IT  )
        P10K=PTBL(IQ+1,IT  )
        P01K=PTBL(IQ  ,IT+1)
        P11K=PTBL(IQ+1,IT+1)
!--------------SATURATION POINT VARIABLES AT THE BOTTOM-----------------
        TPSPK=P00K+(P10K-P00K)*PPK+(P01K-P00K)*QQK        &
                +(P00K-P10K-P01K+P11K)*PPK*QQK
        APESPK=(H10E5/TPSPK)**CAPA
        THESP(I,J)=TTHBTK*EXP(ELOCP*QBTK*APESPK/TTHBTK)
!      ENDIF
  125 CONTINUE
!--------------SCALING PRESSURE & TT TABLE INDEX------------------------
      KNUML=0
      KNUMH=0
!
      DO 280 J=JSTA,JEND
      DO 280 I=1,IM
      KLRES(I,J)=0
      KHRES(I,J)=0
!
!      IF(KARR(I,J).GT.0)THEN
        PRESK=PMID(I,J,L)
!
        IF(PRESK.LT.PLQ)THEN
          KNUML=KNUML+1
          KLRES(I,J)=1
        ELSE
          KNUMH=KNUMH+1
          KHRES(I,J)=1
        ENDIF
!      ENDIF
 280  CONTINUE
!***
!***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE<PL
!**
      IF(KNUML.GT.0)THEN
        CALL TTBLEX(TWET(1,jsta_2l,L),TTBL,ITB,JTB,KLRES     &
      ,PMID(1,jsta_2l,L),PL,QQ,PP,RDP,THE0,STHE              &
      ,RDTHE,THESP,IPTB,ITHTB)
      ENDIF
!***
!***  COMPUTE PARCEL TEMPERATURE ALONG MOIST ADIABAT FOR PRESSURE>PL
!**
      IF(KNUMH.GT.0)THEN
       CALL TTBLEX(TWET(1,jsta_2l,L),TTBLQ,ITBQ,JTBQ,KHRES   &
      ,PMID(1,jsta_2l,L),PLQ,QQ,PP,RDPQ,THE0Q,STHEQ          &
      ,RDTHEQ,THESP,IPTB,ITHTB)
      ENDIF
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  300 CONTINUE
      RETURN
      END
