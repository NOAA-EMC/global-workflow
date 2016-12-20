!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! DoPhase is a subroutine written and provided by Jim Ramer at NOAA/FSL
!
!    Ramer, J, 1993: An empirical technique for diagnosing precipitation
!           type from model output.  Preprints, 5th Conf. on Aviation
!           Weather Systems, Vienna, VA, Amer. Meteor. Soc., 227-230.
!
!   CODE ADAPTED FOR WRF POST  24 AUGUST 2005    G MANIKIN
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      SUBROUTINE CALWXT_RAMER_POST(T,Q,PMID,PINT,LMH,PREC,PTYP)

!      SUBROUTINE dophase(pq,   !  input pressure sounding mb
!     +    t,   !  input temperature sounding K
!     +    pmid,   !  input pressure
!     +    pint,   !  input interface pressure
!     +    q,   !  input spec humidityfraction
!     +    lmh,   !  input number of levels in sounding
!     +    prec,      ! input amount of precipitation
!     +    ptyp) !  output(2) phase 2=Rain, 3=Frzg, 4=Solid,
!                                               6=IP     JC  9/16/99
      use params_mod, only: pq0, a2, a3, a4
      use CTLBLK_mod, only: me, im, jsta_2l, jend_2u, lm, lp1, jsta, jend, pthresh
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      LOGICAL,parameter :: trace = .false.
!      real,PARAMETER :: RCP=0.2857141,LECP=1572.5
      real,PARAMETER :: twice=266.55,rhprcp=0.80,deltag=1.02,prcpmin=0.3, &
     &                  emelt=0.045,rlim=0.04,slim=0.85
      real,PARAMETER :: twmelt=273.15,tz=273.15,efac=1.0 ! specify in params now 
!
      INTEGER*4 i, k1, lll, k2, toodry, iflag, nq
!
      REAL xxx ,mye, icefrac,flg,flag
      real,DIMENSION(IM,jsta_2l:jend_2u,LM), intent(in)    :: T,Q,PMID
      real,DIMENSION(IM,jsta_2l:jend_2u,LP1),intent(in)    :: PINT
      real,DIMENSION(IM,jsta_2l:jend_2u),    intent(in)    :: LMH,PREC
      integer,DIMENSION(IM,jsta:jend),       intent(inout) :: PTYP
!
      real,DIMENSION(IM,jsta_2l:jend_2u,LM) :: P,TQ,PQ,RHQ
      real,DIMENSION(IM,jsta:jend,LM)       :: TWQ
!     REAL, ALLOCATABLE :: TWET(:,:,:)
!
      integer J,L,LEV,LNQ,LMHK,ii
      real RHMAX,TWMAX,PTOP,dpdrh,twtop,rhtop,wgt1,wgt2,    &
           rhavg,dtavg,dpk,ptw,rate,pbot,qc, b
      real,external :: xmytw_post,esat,tdofesat
!
      DATA iflag / -9/
!
!  Initialize.
      IF (trace) WRITE (20,*) '******* NEW STATION ******'
      IF (trace) WRITE (20,*) 'Twmelt,Twice,rhprcp,Emelt'
      IF (trace) WRITE (20,*) twmelt, twice, rhprcp, emelt
      flag=0.;flg=0.
      icefrac = flag
!
      DO J=JSTA,JEND
        DO I=1,IM
          PTYP(I,J) = 0
          NQ=LMH(I,J)
          DO L = 1,NQ
            LEV = NQ-L+1
            P(I,J,L) = PMID(I,J,L)
            QC = PQ0/P(I,J,L) * EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            TQ(I,J,LEV)  = T(I,J,L)
            PQ(I,J,LEV)  = P(I,J,L)/100.
            RHQ(I,J,LEV) = max(0.0, Q(I,J,L)/QC)
          enddo
        enddo
      enddo

!  BIG LOOP
      DO 800 J=JSTA,JEND
      DO 800 I=1,IM
!
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
!
      IF (PREC(I,J).LE.PTHRESH) GOTO 800
      LMHK=NINT(LMH(I,J))

!
!
!CC   RATE RESTRICTION REMOVED BY JOHN CORTINAS 3/16/99
!
!     Construct wet-bulb sounding, locate generating level.
      twmax = -999.0
      rhmax = 0.0
      k1 = 0    !  top of precip generating layer
      k2 = 0    !  layer of maximum rh
!
      IF (trace) WRITE (20,*) 'rhq(1)', rhq(i,j,1),'me=',me
      IF (rhq(I,J,1).lt.rhprcp) THEN
          toodry = 1
      ELSE
          toodry = 0
      END IF
!
!     toodry=((Rhq(I,J,1).lt.rhprcp).and.1)
      pbot = pq(I,J,1)
      NQ=LMH(I,J)
      DO 10 L = 1, nq
!         xxx = tdofesat(esat(tq(I,J,L),flag,flg)*rhq(I,J,L),flag,flg)
          xxx = max(0.0,min(pq(i,j,l),esat(tq(I,J,L),flag,flg))*rhq(I,J,L))
          xxx = tdofesat(xxx,flag,flg)
          twq(I,J,L) = xmytw_post(tq(I,J,L),xxx,pq(I,J,L))

          IF(I .EQ. 324 .and. J .EQ. 390) THEN
            print *, 'tw ramer ', L, Twq(I,J,L),'me=',me
          ENDIF
          IF (trace) WRITE (*,*) 'Twq(I,J,L),L ', twq(I,J,L), L,'me=',me
          twmax = max(twq(I,J,L),twmax)
          IF (trace) WRITE (*,*) 'Tw,Rh,P ', twq(I,J,L) - 273.15,     &
              rhq(I,J,L), pq(I,J,L),'me=',me
          IF (pq(I,J,L).ge.400.0) THEN
              IF (rhq(I,J,L).gt.rhmax) THEN
                  rhmax = rhq(I,J,L)
                  k2 = l
                  IF (trace) WRITE (*,*) 'rhmax,k2,L', rhmax, k2, L
              END IF
!
              IF (L.ne.1) THEN
                IF (trace) WRITE (*,*) 'ME: toodry,L', toodry, L
                 IF (rhq(I,J,L).ge.rhprcp.or.toodry.eq.0) THEN
                  IF (toodry.ne.0) THEN
                    dpdrh = alog(pq(I,J,L)/pq(I,J,L-1)) /              &
                           (rhq(I,J,L)-RHQ(I,J,L-1))
                    pbot = exp(alog(pq(I,J,L))+(rhprcp-rhq(I,J,L))*dpdrh)
!
!lin                dpdrh=(Pq(I,J,L)-Pq(I,J,L-1))/
!lin                       (Rhq(I,J,L)-Rhq(I,J,L-1))
!lin                pbot=Pq(I,J,L)+(rhprcp-Rhq(I,J,L))*dpdrh
                    ptw = pq(I,J,L)
                    toodry = 0
                    IF (trace) WRITE (*,*)  &
                     'dpdrh,pbot,rhprcp-rhq(I,J,L),L,ptw,toodry',  &
                      dpdrh, pbot, rhprcp - rhq(I,J,L),      &
                            L,ptw,toodry
                    ELSE IF (rhq(I,J,L).ge.rhprcp) THEN
                      ptw = pq(I,J,L)
                      IF (trace) WRITE (*,*) 'HERE1: ptw,toodry',ptw, toodry
                    ELSE
                      toodry = 1
                      dpdrh = alog(pq(I,J,L)/pq(I,J,L-1)) /                 &
                          (rhq(I,J,L)-rhq(I,J,L-1))
                      ptw = exp(alog(pq(I,J,L))+(rhprcp-rhq(I,J,L))*dpdrh)
                      IF (trace) WRITE (*,*)'HERE2:dpdrh,pbot,L,ptw,toodry', &
                            dpdrh, pbot, L, ptw, toodry
!lin                dpdrh=(Pq(i)-Pq(i-1))/(Rhq(i)-Rhq(i-1))
!lin                ptw=Pq(i)+(rhprcp-Rhq(i))*dpdrh
!
                      END IF
!
                      IF (trace) WRITE (*,*) 'HERE3:pbot,ptw,deltag',     &
     &                    pbot, ptw, deltag
                      IF (pbot/ptw.ge.deltag) THEN
!lin                      If (pbot-ptw.lt.deltag) Goto 2003
                          k1 = L
                          ptop = ptw
                      END IF
                  END IF
              END IF
          END IF
!
   10 CONTINUE

!
!     Gross checks for liquid and solid precip which dont require generating level.
!
      IF (twq(I,J,1).ge.273.15+2.0) THEN
          ptyp(i,j) = 8   ! liquid
          IF (trace) PRINT *, 'liquid',i,j,'me=',me
          icefrac = 0.0
          goto 800 
      END IF
!
      IF (twmax.le.twice) THEN
          icefrac = 1.0
          ptyp(i,j) = 1   !  solid
          goto 800 
      END IF
!
!     Check to see if we had no success with locating a generating level.
!
      IF (trace) WRITE (*,*) 'HERE6: k1,ptyp', k1, ptyp(i,j),'me=',me
      IF (k1.eq.0) THEN
          rate = flag
          goto 800 
      END IF
!
      IF (ptop.eq.pq(I,J,k1)) THEN
          twtop = twq(I,J,k1)
          rhtop = rhq(I,J,k1)
          k2 = k1
          k1 = k1 - 1
      ELSE
          k2 = k1
          k1 = k1 - 1
          wgt1 = alog(ptop/pq(I,J,k2)) / alog(pq(I,J,k1)/pq(I,J,k2))
!lin      wgt1=(ptop-Pq(I,J,k2))/(Pq(I,J,k1)-Pq(I,J,k2))
          wgt2 = 1.0 - wgt1
          twtop = twq(I,J,k1) * wgt1 + twq(I,J,k2) * wgt2
          rhtop = rhq(I,J,k1) * wgt1 + rhq(I,J,k2) * wgt2
      END IF
!

!     Calculate temp and wet-bulb ranges below precip generating level.
      DO 20 L = 1, k1
          twmax = amax1(twq(i,j,l),twmax)
   20 CONTINUE
!
!     Gross check for solid precip, initialize ice fraction.
      IF (i.eq.1.and.j.eq.1) WRITE (*,*) 'twmax=',twmax,twice,'twtop=',twtop
      IF (twtop.le.twice) THEN
          icefrac = 1.0
          IF (twmax.le.twmelt) THEN     ! gross check for solid precip.
              IF (trace) PRINT *, 'solid'
              ptyp(i,j) = 1       !   solid precip
              goto 800 
          END IF
          lll = 0
      ELSE
          icefrac = 0.0
          lll = 1
      END IF
!
!     Loop downward through sounding from highest precip generating level.
   30 CONTINUE
!
      IF (trace) PRINT *, ptop, twtop - 273.15, icefrac,'me=',me
      IF (trace) WRITE (*,*) 'P,Tw,frac,twq(I,J,k1)', ptop,             &
     &    twtop - 273.15, icefrac, twq(I,J,k1),'me=',me
      IF (icefrac.ge.1.0) THEN  !  starting as all ice
          IF (trace) WRITE (*,*) 'ICEFRAC=1', icefrac
!          print *, 'twq twmwelt twtop ', twq(I,J,k1), twmelt, twtop
          IF (twq(I,J,k1).lt.twmelt) GO TO 40       ! cannot commence melting
          IF (twq(I,J,k1).eq.twtop) GO TO 40        ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(I,J,k1)) / (twtop-twq(I,J,k1))
          rhavg = rhq(I,J,k1) + wgt1 * (rhtop-rhq(I,J,k1)) / 2
          dtavg = (twmelt-twq(I,J,k1)) / 2
          dpk = wgt1 * alog(pq(I,J,k1)/ptop)        !lin   dpk=wgt1*(Pq(k1)-Ptop)
!         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) WRITE (*,*)                                       &
     &        'HERE8: wgt1,rhavg,dtavg,dpk,mye,icefrac', wgt1, rhavg,   &
     &        dtavg, dpk, mye, icefrac,'me=',me
      ELSE IF (icefrac.le.0.0) THEN     !  starting as all liquid
          IF (trace) WRITE (*,*) 'HERE9: twtop,twq(I,J,k1),k1,lll'     &
     &    , twtop, twq(I,J,k1), k1, lll
          lll = 1
!         If (Twq(I,J,k1).le.Twice) icefrac=1.0 ! autoconvert
!         Goto 1020
          IF (twq(I,J,k1).gt.twice) GO TO 40        ! cannot commence freezing
          IF (twq(I,J,k1).eq.twtop) THEN
              wgt1 = 0.5
          ELSE
              wgt1 = (twice-twq(I,J,k1)) / (twtop-twq(I,J,k1))
          END IF
          rhavg = rhq(I,J,k1) + wgt1 * (rhtop-rhq(I,J,k1)) / 2
          dtavg = twmelt - (twq(I,J,k1)+twice) / 2
          dpk = wgt1 * alog(pq(I,J,k1)/ptop)      !lin  dpk=wgt1*(Pq(k1)-Ptop)
!         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) WRITE (*,*) 'HERE10: wgt1,rhtop,rhq(I,J,k1),dtavg', &
              wgt1, rhtop, rhq(I,J,k1), dtavg,'me=',me
      ELSE IF ((twq(I,J,k1).le.twmelt).and.(twq(I,J,k1).lt.twmelt)) THEN ! mix
          rhavg = (rhq(I,J,k1)+rhtop) / 2
          dtavg = twmelt - (twq(I,J,k1)+twtop) / 2
          dpk = alog(pq(I,J,k1)/ptop)       !lin   dpk=Pq(I,J,k1)-Ptop
!         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
           
          IF (trace) WRITE (*,*) 'HERE11: twq(i,j,K1),twtop',        &
              twq(i,j,k1),twtop,'me=',me
      ELSE      ! mix where Tw curve crosses twmelt in layer
          IF (twq(I,J,k1).eq.twtop) GO TO 40   ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(I,J,k1)) / (twtop-twq(I,J,k1))
          wgt2 = 1.0 - wgt1
          rhavg = rhtop + wgt2 * (rhq(I,J,k1)-rhtop) / 2
          dtavg = (twmelt-twtop) / 2
          dpk = wgt2 * alog(pq(I,J,k1)/ptop)     !lin   dpk=wgt2*(Pq(k1)-Ptop)
!         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          icefrac = amin1(1.0,amax1(icefrac,0.0))
          IF (trace) WRITE (*,*) 'HERE12: twq(I,J,k1),twtop,icefrac,wgt1,wg'//  &
              't2,rhavg,rhtop,rhq(I,J,k1),dtavg,k1', &
               twq(I,J,k1), twtop,       &
              icefrac,wgt1,wgt2, rhavg, rhtop, rhq(I,J,k1), dtavg, k1 ,'me=',me  
          IF (icefrac.le.0.0) THEN
!             If (Twq(I,J,k1).le.Twice) icefrac=1.0 ! autoconvert
!             Goto 1020
              IF (twq(I,J,k1).gt.twice) GO TO 40    ! cannot commence freezin
              wgt1 = (twice-twq(I,J,k1)) / (twtop-twq(I,J,k1))
              dtavg = twmelt - (twq(I,J,k1)+twice) / 2
              IF (trace) WRITE (*,*) 'IN IF','me=',me
          ELSE
              dtavg = (twmelt-twq(I,J,k1)) / 2
              IF (trace) WRITE (*,*) 'IN ELSE','me=',me
          END IF
          IF (trace) WRITE (*,*) 'NEW ICE FRAC CALC','me=',me
          rhavg = rhq(I,J,k1) + wgt1 * (rhtop-rhq(I,J,k1)) / 2
          dpk = wgt1 * alog(pq(I,J,k1)/ptop)     !lin  dpk=wgt1*(Pq(k1)-Ptop)
!         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          IF (trace) WRITE (*,*) 'HERE13: icefrac,k1,dtavg,rhavg',      &
              icefrac, k1, dtavg, rhavg,'me=',me
      END IF
!
      icefrac = amin1(1.0,amax1(icefrac,0.0))
      IF (i.eq.1.and.j.eq.1) WRITE (*,*) 'NEW ICEFRAC:', icefrac, icefrac,'me=',me
!
!     Get next level down if there is one, loop back.
   40 IF (k1.gt.1) THEN
          IF (trace) WRITE (*,*) 'LOOPING BACK','me=',me
          twtop = twq(I,J,k1)
          ptop = pq(I,J,k1)
          rhtop = rhq(I,J,k1)
          k1 = k1 - 1
          GO TO 30
      END IF
!
!
!     Determine precip type based on snow fraction and surface wet-bulb.
!
      IF (trace) WRITE (*,*) 'P,Tw,frac,lll', pq(I,J,k1),               &
          twq(I,J,k2) - 273.15, icefrac, lll,'me=',me
!
      IF (icefrac.ge.slim) THEN
          IF (lll.ne.0) THEN
              ptyp(i,j) = 2       ! Ice Pellets   JC 9/16/99
              IF (trace) WRITE (*,*) 'frozen',i,j,'me=',me
          ELSE
              ptyp(i,j) = 1       !  Snow
              IF (trace) WRITE (*,*) 'snow',i,j,'me=',me
          END IF
      ELSE IF (icefrac.le.rlim) THEN
          IF (twq(i,j,1).lt.tz) THEN
              ptyp(i,j) = 4       !  Freezing Precip
              IF (trace) WRITE (*,*) 'freezing',i,j,'me=',me
          ELSE
              ptyp(i,j) = 8       !  Rain
              IF (trace) WRITE (*,*) 'liquid',i,j,'me=',me
          END IF
      ELSE
          IF (trace) WRITE (*,*) 'Mix',i,j
          IF (twq(i,j,1).lt.tz) THEN
              IF (trace) WRITE (*,*) 'freezing',i,j
!GSM not sure what to do when 'mix' is predicted;   In previous
!GSM   versions of this code for which I had to have an answer,
!GSM   I chose sleet.  Here, though, since we have 4 other
!GSM   algorithms to provide an answer, I will not declare a
!GSM   type from the Ramer in this situation and allow the
!GSM   other algorithms to make the call.
      
              ptyp(i,j) = 0       !  don't know 
!              ptyp = 5       !  Mix
          ELSE
!              ptyp = 5       !  Mix
              ptyp(i,j) = 0       !  don't know 
          END IF
      END IF
      IF (trace) WRITE (*,*) "Returned ptyp is:ptyp,lll ", ptyp, lll,'me=',me
      IF (trace) WRITE (*,*) "Returned icefrac is: ", icefrac,'me=',me
 800  CONTINUE 
      DO 900 J=JSTA,JEND
      DO 900 I=1,IM
 900  CONTINUE
      RETURN
!
      END
!
!--------------------------------------------------------------------------
      REAL*4 FUNCTION esat(t,flag,flg)
!
!*  Calculates saturation vapor pressure in millibars as a function of
!*  either Kelvin of Celcius temperature.
!
      IMPLICIT NONE
!
      REAL*4 t, k
!
      REAL*4 flag, flg
!
!  Account for both Celsius and Kelvin.
      k = t
      IF (k.lt.100.) k = k + 273.15
!     
!     Flag ridiculous values.
      IF (k.lt.0.0.or.k.gt.373.15) THEN
          esat = flag
          RETURN
      END IF
!     
!     Avoid floating underflow.
      IF (k.lt.173.15) THEN
          esat = 3.777647E-05
          RETURN
      END IF
!     
!     Calculation for normal range of values.
      esat = exp(26.660820-0.0091379024*k-6106.3960/k)
!     
      RETURN
      END
!--------------------------------------------------------------------------
      REAL*4 FUNCTION tdofesat(es,flag,flg)
!
!*  As a function of saturation vapor pressure in millibars, returns
!*  dewpoint in degrees K.
!
      IMPLICIT NONE
!
      REAL*4 es, lim1, lim2, b
!
      DATA lim1, lim2 /3.777647E-05, 980.5386/
!
      REAL*4 flag, flg
!      COMMON /flagflg/ flag, flg
!
!  Flag ridiculous values.
      IF (es.lt.0.0.or.es.gt.lim2) THEN
          tdofesat = flag
          RETURN
      END IF
!     
!     Avoid floating underflow.
      IF (es.lt.lim1) THEN
          tdofesat = 173.15
          RETURN
      END IF
!     
!     Calculations for normal range of values.
      b = 26.66082 - alog(es)
      tdofesat = (b-sqrt(b*b-223.1986)) / 0.0182758048
!     
      RETURN
      END
!
!--------------------------------------------------------------------------
!      REAL*4 FUNCTION mytw(t,td,p)
      FUNCTION xmytw_post(t,td,p)
!
      IMPLICIT NONE
!
      INTEGER*4 cflag, l,i,j
      REAL*4 f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,        &
     &          de, xmytw_post
      DATA f, c0, c1, c2 /0.0006355, 26.66082, 0.0091379024, 6106.3960/
!
!
      xmytw_post= (t+td) / 2
      IF (td.ge.t) RETURN
!
      IF (t.lt.100.0) THEN
          k = t + 273.15
          kd = td + 273.15
          IF (kd.ge.k) RETURN
          cflag = 1
      ELSE
          k = t
          kd = td
          cflag = 0
      END IF
      if (kd == 0.0) write(0,*)' kd=',kd,' t=',t,' p=',p,' td=',td
!
      ed = c0 - c1 * kd - c2 / kd
      IF (ed.lt.-14.0.or.ed.gt.7.0) RETURN
      ed = exp(ed)
      ew = c0 - c1 * k - c2 / k
      IF (ew.lt.-14.0.or.ew.gt.7.0) RETURN
      ew = exp(ew)
      fp = p * f
      s = (ew-ed) / (k-kd)
      kw = (k*fp+kd*s) / (fp+s)
!
      DO 10 l = 1, 5
          ew = c0 - c1 * kw - c2 / kw
          IF (ew.lt.-14.0.or.ew.gt.7.0) RETURN
          ew = exp(ew)
          de = fp * (k-kw) + ed - ew
          IF (abs(de/ew).lt.1E-5) GO TO 20
          s = ew * (c1-c2/(kw*kw)) - fp
          kw = kw - de / s
   10 CONTINUE
   20 CONTINUE
!
!      print *, 'kw ', kw
      IF (cflag.ne.0) THEN
          xmytw_post= kw - 273.15
      ELSE
          xmytw_post = kw
      END IF
!
      RETURN
      END
