!**********************************************************************c
      SUBROUTINE CALVIS(QV,QC,QR,QI,QS,TT,PP,VIS)
!
!   This routine computes horizontal visibility at the
!   surface or lowest model layer, from qc, qr, qi, and qs.  
!   qv--water vapor mixing ratio (kg/kg)
!   qc--cloud water mixing ratio (kg/kg)
!   qr--rain water mixing ratio  (kg/kg)
!   qi--cloud ice mixing ratio   (kg/kg)
!   qs--snow mixing ratio        (kg/kg)
!   tt--temperature              (k)
!   pp--pressure                 (Pa)
!
!   If iice=0:
!      qprc=qr     qrain=qr and qclw=qc if T>0C
!      qcld=qc          =0          =0  if T<0C
!                  qsnow=qs and qclice=qc  if T<0C
!                       =0            =0   if T>0C
!   If iice=1:
!      qprc=qr+qs   qrain=qr and qclw=qc
!      qcld=qc+qi   qsnow=qs and qclice=qc
!
!   Independent of the above definitions, the scheme can use different
!   assumptions of the state of hydrometeors:
!        meth='d': qprc is all frozen if T<0, liquid if T>0
!        meth='b': Bocchieri scheme used to determine whether qprc
!           is rain or snow. A temperature assumption is used to
!           determine whether qcld is liquid or frozen.
!        meth='r': Uses the four mixing ratios qrain, qsnow, qclw,
!           and qclice
!
!   The routine uses the following
!   expressions for extinction coefficient, beta (in km**-1),
!   with C being the mass concentration (in g/m**3):
!
!      cloud water:  beta = 144.7 * C ** (0.8800)
!      rain water:   beta =  2.24 * C ** (0.7500)
!      cloud ice:    beta = 327.8 * C ** (1.0000)
!      snow:         beta = 10.36 * C ** (0.7776)
!
!   These expressions were obtained from the following sources:
!
!      for cloud water: from Kunkel (1984)
!      for rainwater: from M-P dist'n, with No=8e6 m**-4 and
!         rho_w=1000 kg/m**3
!      for cloud ice: assume randomly oriented plates which follow
!         mass-diameter relationship from Rutledge and Hobbs (1983)
!      for snow: from Stallabrass (1985), assuming beta = -ln(.02)/vis
!
!   The extinction coefficient for each water species present is
!   calculated, and then all applicable betas are summed to yield
!   a single beta. Then the following relationship is used to
!   determine visibility (in km), where epsilon is the threshhold
!   of contrast, usually taken to be .02:
!
!      vis = -ln(epsilon)/beta      [found in Kunkel (1984)]
!
!------------------------------------------------------------------
    use params_mod, only: h1, d608, rd
    use ctlblk_mod, only: jsta, jend, im, jsta_2l, jend_2u
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
!

      real,dimension(IM,jsta_2l:jend_2u),intent(in)    :: QV,QC,QR,QI,QS,TT,PP
      real,dimension(IM,jsta_2l:jend_2u),intent(inout) :: VIS
 
      CHARACTER METH*1
      real CELKEL,TICE,COEFLC,COEFLP,COEFFC,COEFFP,EXPONLC,      &
           EXPONLP,CONST1,RHOICE,RHOWAT,QPRC,QCLD,QRAIN,QSNOW,   &
           QCLW,QCLICE,TV,RHOAIR,VOVERMD,CONCLC,CONCLD,CONCFC,   &
           CONCFD,BETAV,EXPONFC,EXPONFP,CONCLP,CONCFP
      integer I,J
!------------------------------------------------------------------
!------------------------------------------------------------------
      CELKEL=273.15
      TICE=CELKEL-10.
      COEFLC=144.7
      COEFLP=2.24
      COEFFC=327.8
      COEFFP=10.36
      EXPONLC=0.8800
      EXPONLP=0.7500
      EXPONFC=1.0000
      EXPONFP=0.7776
      CONST1=-LOG(.02)
      RHOICE=917.
      RHOWAT=1000.
!
      DO J=JSTA,JEND
      DO I=1,IM
!       IF(IICE.EQ.0)THEN
!         QPRC=QR
!         QCLD=QC
!         IF(TT.LT.CELKEL)THEN
!           QRAIN=0.
!           QSNOW=QPRC
!           QCLW=0.
!           QCLICE=QCLD
!         ELSE
!           QRAIN=QPRC
!           QSNOW=0.
!           QCLW=QCLD
!           QCLICE=0.
!         ENDIF
!       ELSE
          QPRC=QR(I,J)+QS(I,J)
          QCLD=QC(I,J)+QI(I,J)
          QRAIN=QR(I,J)
          QSNOW=QS(I,J)
          QCLW=QC(I,J)
          QCLICE=QI(I,J)
!       ENDIF
!       TV=VIRTUAL(TT,QV)
        TV=TT(I,J)*(H1+D608*QV(I,J))
        RHOAIR=PP(I,J)/(RD*TV)
!       IF(METH.EQ.'D')THEN
!         IF(TT.LT.CELKEL)THEN
!           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOICE
!           CONCLC = 0.
!           CONCLP = 0.
!           CONCFC = QCLD/VOVERMD*1000.
!           CONCFP = QPRC/VOVERMD*1000.
!         ELSE
!           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOWAT
!           CONCLC = QCLD/VOVERMD*1000.
!           CONCLP = QPRC/VOVERMD*1000.
!           CONCFC = 0.
!           CONCFP = 0.
!         ENDIF
!       ELSEIF(METH.EQ.'B')THEN
!         IF(TT.LT.TICE)THEN
!           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOICE
!           CONCLC = 0.
!           CONCLP = 0.
!           CONCFC = QCLD/VOVERMD*1000.
!           CONCFP = QPRC/VOVERMD*1000.
!         ELSEIF(PRSNOW.GE.50.)THEN
!           VOVERMD=(1.+QV)/RHOAIR+QPRC/RHOICE+QCLD/RHOWAT
!           CONCLC = QCLD/VOVERMD*1000.
!           CONCLP = 0.
!           CONCFC = 0.
!           CONCFP = QPRC/VOVERMD*1000.
!         ELSE
!           VOVERMD=(1.+QV)/RHOAIR+(QPRC+QCLD)/RHOWAT
!           CONCLC = QCLD/VOVERMD*1000.
!           CONCLP = QPRC/VOVERMD*1000.
!           CONCFC = 0.
!           CONCFP = 0.
!         ENDIF
!       ELSEIF(METH.EQ.'R')THEN
          VOVERMD=(1.+QV(I,J))/RHOAIR+(QCLW+QRAIN)/RHOWAT+        &
                  (QCLICE+QSNOW)/RHOICE
          CONCLC = MAX(0., QCLW/VOVERMD*1000.)
          CONCLP = MAX(0., QRAIN/VOVERMD*1000.)
          CONCFC = MAX(0., QCLICE/VOVERMD*1000.)
          CONCFP = MAX(0., QSNOW/VOVERMD*1000.)
!       ENDIF
        BETAV=COEFFC*CONCFC**EXPONFC+COEFFP*CONCFP**EXPONFP        &
             +COEFLC*CONCLC**EXPONLC+COEFLP*CONCLP**EXPONLP        &
             +1.E-10
! CHANGED GSM 3-10-00 -->  no point in distinguishing values
!       above 20 km, so make that value the max (prev max was 80)
!        VIS(I,J)=1.E3*MIN(20.,CONST1/BETAV)   ! max of 20km
! Chuang: Per Geoff, the max visibility was changed to be cosistent with visibility ceiling in obs
        VIS(I,J) = 1.E3*MIN(24.135,CONST1/BETAV)   ! change max to be consistent with obs
      ENDDO
      ENDDO
!
      RETURN
      END
