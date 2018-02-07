Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C DoPhase is a subroutine written and provided by Jim Ramer at NOAA/FSL
C
C    Ramer, J, 1993: An empirical technique for diagnosing precipitation
C           type from model output.  Preprints, 5th Conf. on Aviation
C           Weather Systems, Vienna, VA, Amer. Meteor. Soc., 227-230.
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
      SUBROUTINE CALWXT1(pq,tq,qq,twq,tdq,nq,lm,ppt,ptyp,trace)
c      SUBROUTINE dophase(pq,   !  input pressure sounding mb
c     +    tq,   !  input temperature sounding K
c     +    pq,   |  input pressure
c     +    qq,   !  input spec humidityfraction
c     +   twq,   !  input wet-bulb temperature
c     +    nq,   !  input number of levels in sounding
c     +    twq,  !  output wet-bulb sounding K
c     +    icefrac,      !  output ice fraction
c     +    ptyp) !  output(2) phase 2=Rain, 3=Frzg, 4=Solid,
C                                               6=IP     JC  9/16/99
      LOGICAL trace
c     PARAMETER (trace = .false.)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516)
      PARAMETER (G=9.80665,CP=1004.686,RCP=0.2857141,LECP=1572.5)
      PARAMETER (twice=266.55,rhprcp=0.80,deltag=1.02,prcpmin=0.3,
     *             emelt=0.045,rlim=0.04,slim=0.85)
      PARAMETER (twmelt=273.15,tz=273.15,efac=1.0,PTHRES=0.25)
c  pthres is in unit of mm and is equivalent to .01 inch
C
      INTEGER*4 i, k1, lll, k2, toodry, iflag, nq
C
      INTEGER ptyp
C
      REAL rcp, flg, flag, xxx, pq(lm), tq(lm), twq(lm), rhq(lm), mye,
     *              qq(lm), icefrac, tqtmp(lm), pqtmp(lm), rhqtmp(lm)
     *             ,twtmp(lm),qqtmp(lm),tdqtmp(lm),tdq(lm)
C
      COMMON /flagflg/ flag, flg
      DATA iflag / -9/
C
C  Initialize.
      IF (trace) print *,  '******* NEW STATION ******'
      IF (trace) print *, 'Twmelt,Twice,rhprcp,Emelt'
      IF (trace) print *, twmelt, twice, rhprcp, emelt
      icefrac = flag
      ptyp = 0 
c     IF (PPT.LE.PTHRES) RETURN
C
C GSM compute RH, convert pressure to mb, and reverse order

      DO 88 i = 1, nq
        LEV=NQ-I+1
c       QC=PQ0/PQ(I) * EXP(A2*(TQ(I)-A3)/(TQ(I)-A4))
        call svp(qc,es,pq(i),tq(i))
        RHQTMP(LEV)=QQ(I)/QC
        PQTMP(LEV)=PQ(I)/100.
        TQTMP(LEV)=TQ(I)
        TWTMP(LEV)=TWQ(I)
        QQTMP(LEV)=QQ(I)
        TDQTMP(LEV)=TDQ(I)
   88 CONTINUE

      do 92 i=1,nq
         TQ(I)=TQTMP(I)
         PQ(I)=PQTMP(I)
         RHQ(I)=RHQTMP(I)
         TWQ(I)=TWTMP(I)
         QQ(I)=QQTMP(I)
         TDQ(I)=TDQTMP(I)
   92 continue


C     See if there was too little precip reported.
C
CCC   RATE RESTRICTION REMOVED BY JOHN CORTINAS 3/16/99
C
C     Construct wet-bulb sounding, locate generating level.
      twmax = -999.0
      rhmax = 0.0
      k1 = 0    !  top of precip generating layer
      k2 = 0    !  layer of maximum rh
C
      IF (trace) WRITE (20,*) 'rhq(1)', rhq(1)
      IF (rhq(1).lt.rhprcp) THEN
          toodry = 1
      ELSE
          toodry = 0
      END IF
C
C     toodry=((Rhq(1).lt.rhprcp).and.1)
      pbot = pq(1)
      DO 10 i = 1, nq
c         xxx = tdofesat(esat(tq(i))*rhq(i))
c         call tdew(xxx,tq(i),qq(i),pq(i)*100.)
          xxx = tdq(i)
          IF (trace) print *, 'T,Rh,Td,P,nq ', tq(i), rhq(i), xxx, 
     +        pq(i), nq
c          twq(i) = xmytw(tq(i),xxx,pq(i))
          IF (trace) print *, 'Twq(i),i ', twq(i), i
          twmax = amax1(twq(i),twmax)
          IF (trace) print *, 'Tw,Rh,P ', twq(i) - 273.15, rhq(i), 
     +        pq(i)
          IF (pq(i).ge.400.0) THEN
              IF (rhq(i).gt.rhmax) THEN
                  rhmax = rhq(i)
                  k2 = i
                  IF (trace) print *, 'rhmax,k2,i', rhmax, k2, i
              END IF
C
              IF (i.ne.1) THEN
                  IF (trace) print *, 'ME: toodry,i', toodry, i
                  IF (rhq(i).ge.rhprcp.or.toodry.eq.0) THEN
                      IF (toodry.ne.0) THEN
                          dpdrh = alog(pq(i)/pq(i-1)) / (rhq(i)-
     +                        rhq(i-1))
                          pbot = exp(alog(pq(i))+(rhprcp-rhq(i))*dpdrh)
C
Clin                      dpdrh=(Pq(i)-Pq(i-1))/(Rhq(i)-Rhq(i-1))
Clin                      pbot=Pq(i)+(rhprcp-Rhq(i))*dpdrh
                          ptw = pq(i)
                          toodry = 0
                          IF (trace) print *, 'dpdrh,pbot,rhprcp-rhq
     +(i),i,ptw,         toodry', dpdrh, pbot, rhprcp - rhq(i), i, ptw,
     +                        toodry
                      ELSE IF (rhq(i).ge.rhprcp) THEN
                          ptw = pq(i)
                          IF (trace) print *, 'HERE1: ptw,toodry', 
     +                        ptw, toodry
                      ELSE
                          toodry = 1
                          dpdrh = alog(pq(i)/pq(i-1)) / (rhq(i)-
     +                        rhq(i-1))
                          ptw = exp(alog(pq(i))+(rhprcp-rhq(i))*dpdrh)
                          IF (trace) print *, 
     +                        'HERE2:dpdrh,pbot,i,ptw,toodry', dpdrh, 
     +                        pbot, i, ptw, toodry
Clin                  dpdrh=(Pq(i)-Pq(i-1))/(Rhq(i)-Rhq(i-1))
Clin                  ptw=Pq(i)+(rhprcp-Rhq(i))*dpdrh
C
                      END IF
C
                      IF (trace) print *, 'HERE3:pbot,ptw,deltag', 
     +                    pbot, ptw, deltag
                      IF (pbot/ptw.ge.deltag) THEN
Clin                      If (pbot-ptw.lt.deltag) Goto 2003
                          k1 = i
                          ptop = ptw
                      END IF
                  END IF
              END IF
          END IF
C
   10 CONTINUE
C
C     Gross checks for liquid and solid precip which dont require generating level.
C
c      print *, 'twq1 ', twq(1)
      IF (twq(1).ge.273.15+2.0) THEN
          ptyp = 8   ! liquid
          IF (trace) PRINT *, 'liquid'
          icefrac = 0.0
          RETURN
      END IF
C
      print *, 'twmax ', twmax
      IF (twmax.le.twice) THEN
          icefrac = 1.0
          ptyp = 1   !  solid
          RETURN
      END IF
C
C     Check to see if we had no success with locating a generating level.
C
      IF (trace) print *, 'HERE6: k1,ptyp', k1, ptyp
      IF (k1.eq.0) THEN
          rate = flag
          RETURN
      END IF
C
      IF (ptop.eq.pq(k1)) THEN
          twtop = twq(k1)
          rhtop = rhq(k1)
          k2 = k1
          k1 = k1 - 1
      ELSE
          k2 = k1
          k1 = k1 - 1
          wgt1 = alog(ptop/pq(k2)) / alog(pq(k1)/pq(k2))
Clin      wgt1=(ptop-Pq(k2))/(Pq(k1)-Pq(k2))
          wgt2 = 1.0 - wgt1
          twtop = twq(k1) * wgt1 + twq(k2) * wgt2
          rhtop = rhq(k1) * wgt1 + rhq(k2) * wgt2
      END IF
C
      IF (trace) print *, 
     +    'HERE7: ptop,k1,pq(k1),twtop,rhtop,k2,wgt1,     wgt2', ptop, 
     +    k1, pq(k1), twtop, rhtop, k2, wgt1, wgt2
C
C     Calculate temp and wet-bulb ranges below precip generating level.
      DO 20 i = 1, k1
          twmax = amax1(twq(i),twmax)
   20 CONTINUE
C
C     Gross check for solid precip, initialize ice fraction.
      IF (trace) print *, twmax
      IF (twtop.le.twice) THEN
          icefrac = 1.0
          IF (twmax.le.twmelt) THEN     ! gross check for solid precip.
              IF (trace) PRINT *, 'solid'
              ptyp = 1       !   solid precip
              RETURN
          END IF
          lll = 0
      ELSE
          icefrac = 0.0
          lll = 1
      END IF
C
C     Loop downward through sounding from highest precip generating level.
   30 CONTINUE
C
      IF (trace) PRINT *, ptop, twtop - 273.15, icefrac
      IF (trace) print *, 'P,Tw,frac,twq(k1)', ptop, twtop - 273.15,
     +    icefrac, twq(k1)
      IF (icefrac.ge.1.0) THEN  !  starting as all ice
          IF (trace) print *, 'ICEFRAC=1', icefrac
          print *, 'twq twmwelt twtop ', twq(k1), twmelt, twtop
          IF (twq(k1).lt.twmelt) GO TO 40       ! cannot commence melting
          IF (twq(k1).eq.twtop) GO TO 40        ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(k1)) / (twtop-twq(k1))
          rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) / 2
          dtavg = (twmelt-twq(k1)) / 2
          dpk = wgt1 * alog(pq(k1)/ptop)        !lin   dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) print *, 
     +        'HERE8: wgt1,rhavg,dtavg,dpk,mye,icefrac', wgt1, rhavg, 
     +        dtavg, dpk, mye, icefrac
      ELSE IF (icefrac.le.0.0) THEN     !  starting as all liquid
          IF (trace) print *, 'HERE9: twtop,twq(k1),k1,lll', twtop,
     +        twq(k1), k1, lll
          lll = 1
C         If (Twq(k1).le.Twice) icefrac=1.0 ! autoconvert
C         Goto 1020
          IF (twq(k1).gt.twice) GO TO 40        ! cannot commence freezing
          IF (twq(k1).eq.twtop) THEN
              wgt1 = 0.5
          ELSE
              wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
          END IF
          rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) / 2
          dtavg = twmelt - (twq(k1)+twice) / 2
          dpk = wgt1 * alog(pq(k1)/ptop)        !lin  dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) print *, 'HERE10: wgt1,rhtop,rhq(k1),dtavg', 
     +        wgt1, rhtop, rhq(k1), dtavg
      ELSE IF ((twq(k1).le.twmelt).and.(twq(k1).lt.twmelt)) THEN       ! mix
          rhavg = (rhq(k1)+rhtop) / 2
          dtavg = twmelt - (twq(k1)+twtop) / 2
          dpk = alog(pq(k1)/ptop)       !lin   dpk=Pq(k1)-Ptop
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
           
          IF (trace) print *, 'HERE11: twq(K1),twtop', twq(k1), 
     +        twtop
      ELSE      ! mix where Tw curve crosses twmelt in layer
          IF (twq(k1).eq.twtop) GO TO 40        ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(k1)) / (twtop-twq(k1))
          wgt2 = 1.0 - wgt1
          rhavg = rhtop + wgt2 * (rhq(k1)-rhtop) / 2
          dtavg = (twmelt-twtop) / 2
          dpk = wgt2 * alog(pq(k1)/ptop)        !lin   dpk=wgt2*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          icefrac = amin1(1.0,amax1(icefrac,0.0))
          IF (trace) print *, 'HERE12: twq(k1),twtop,icefrac,wgt1,wg
     +t2,rhavg,rhtop,rhq(k1),dtavg,k1', twq(k1), twtop, icefrac, wgt1, 
     +        wgt2, rhavg, rhtop, rhq(k1), dtavg, k1
          IF (icefrac.le.0.0) THEN
C             If (Twq(k1).le.Twice) icefrac=1.0 ! autoconvert
C             Goto 1020
              IF (twq(k1).gt.twice) GO TO 40    ! cannot commence freezin
              wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
              dtavg = twmelt - (twq(k1)+twice) / 2
              IF (trace) WRITE (20,*) 'IN IF'
          ELSE
              dtavg = (twmelt-twq(k1)) / 2
              IF (trace) WRITE (20,*) 'IN ELSE'
          END IF
          IF (trace) print *, 'NEW ICE FRAC CALC'
          rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) / 2
          dpk = wgt1 * alog(pq(k1)/ptop)        !lin  dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) print *, 'HERE13: icefrac,k1,dtavg,rhavg', 
     +        icefrac, k1, dtavg, rhavg
      END IF
C
      icefrac = amin1(1.0,amax1(icefrac,0.0))
      IF (trace) print *, 'NEW ICEFRAC:', icefrac, icefrac
C
C     Get next level down if there is one, loop back.
   40 IF (k1.gt.1) THEN
          IF (trace) WRITE (20,*) 'LOOPING BACK'
          twtop = twq(k1)
          ptop = pq(k1)
          rhtop = rhq(k1)
          k1 = k1 - 1
          GO TO 30
      END IF
C
C
C     Determine precip type based on snow fraction and surface wet-bulb.
C     If (trace) Print *,Pq(k1),Twq(k1)-273.15,icefrac
C
      IF (trace) print *, 'P,Tw,frac,lll', pq(k1), twq(k2) - 273.15,
     +    icefrac, lll
C
c      print *, 'icefrac ', icefrac
      IF (icefrac.ge.slim) THEN
          IF (lll.ne.0) THEN
              ptyp = 2       ! Ice Pellets   JC 9/16/99
              IF (trace) print *, 'frozen'
          ELSE
              ptyp = 1       !  Snow
              print *, 'snow'
              IF (trace) print *, 'snow'
          END IF
      ELSE IF (icefrac.le.rlim) THEN
          IF (twq(1).lt.tz) THEN
              print *, 'aha! frz'
              ptyp = 4       !  Freezing Precip
              IF (trace) print *, 'freezing'
          ELSE
              ptyp = 8       !  Rain
              print *, 'rain'
              IF (trace) print *, 'liquid'
          END IF
      ELSE
          IF (trace) print *, 'Mix'
          IF (twq(1).lt.tz) THEN
              IF (trace) print *, 'freezing'
cGSM not sure what to do when 'mix' is predicted;  I chose sleet as
cGSK      a shaky best option 
      
              ptyp = 2       !  Ice Pellets 
c              ptyp = 5       !  Mix
          ELSE
c              ptyp = 5       !  Mix
              ptyp = 2       !  Ice Pellets
          END IF
      END IF
      IF (trace) print *, "Returned ptyp is:ptyp,lll ", ptyp, lll
      IF (trace) print *, "Returned icefrac is: ", icefrac
      RETURN
C
      END
