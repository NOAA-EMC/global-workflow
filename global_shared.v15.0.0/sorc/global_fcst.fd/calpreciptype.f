      subroutine calpreciptype(kdt,nrcm,im,ix,lm,lp1,randomno,      &
                               xlat,xlon,                           &
                               gt0,gq0,prsl,prsi,prec,              & !input
                               phii,n3dfercld,tskin,sr,phy_f3d,     & !input
                               domr,domzr,domip,doms)  !output
!      subroutine calpreciptype(nrcm,randomno,im,lm,lp1,t,q,pmid,pint,prec, & !input
!                           zint,n3dfercld,tskin,sr,f_rimef,  & !input
!			   domr,domzr,domip,doms)  !output
!$$$  subprogram documentation block
!                .      .    .     
! subprogram:    calpreciptype      compute dominant precip type
!   prgrmmr: chuang         org: w/np2      date: 2008-05-28
!          
!     
! abstract:
!     this routine computes precipitation type.
!   . it is adopted from post but was made into a column to used by gfs model    
!     
!
!      use vrbls3d   
!      use vrbls2d   
!      use soil
!      use masks
!      use params_mod
!      use ctlblk_mod
!      use rqstfld_mod
      use funcphys, only : fpvs,ftdp,fpkap,ftlcl,stma,fthe
      use physcons
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!      include "mpif.h"
!     
!     in ngm subroutine output we find the following comment.
!     "if the following threshold values are changed, contact
!     tdl/synoptic-scale techniques branch (paul dallavalle
!     and john jensenius).  they may be using it in one of 
!     their packing codes."  the threshold value is 0.01 inch
!     or 2.54e-4 meter.  precipitation values less than this
!     threshold are set to minus one times this threshold.

      real,parameter :: pthresh = 0.0
!     
!     set celcius to kelvin and second to hour conversion.
      integer,parameter :: nalg    = 5
!     
!     declare variables.
!     
      integer,intent(in) :: nrcm,im,ix,lm,lp1,n3dfercld,kdt
      real,intent(in) :: xlat(im),xlon(im) 
      real(kind=kind_phys),dimension(im),intent(in) :: prec,sr,tskin
      real,intent(in) :: randomno(ix,nrcm)
      real(kind=kind_phys),dimension(ix,lm),intent(in) :: gt0,gq0,prsl,phy_f3d
      real(kind=kind_phys),dimension(ix,lp1),intent(in) :: prsi,phii
      
      real(kind=kind_phys),dimension(im),intent(out) :: domr,domzr,domip,doms
      
      real(kind=kind_phys) :: es,qc,pv
      integer,dimension(nalg) :: sleet,rain,freezr,snow
      real(kind=kind_phys),dimension(lm) :: t,q,pmid,f_rimef
      real(kind=kind_phys),dimension(lp1) :: pint,zint
      real(kind=kind_phys), allocatable :: twet(:),rh(:),td(:)
!
      integer i,iwx,isno,iip,izr,irain,k,k1
      real(kind=kind_phys) tdpd,pr,tr,pk,tlcl,thelcl,qwet   

      allocate ( twet(lm),rh(lm),td(lm) )

      do i=1,im
       if (prec(i) > pthresh) then
!     
!       computes wet bulb here since two algorithms use it
!        lp1=lm+1
!       convert geopotential to height
!        do l=1,lp1
!          zint(l)=zint(l)/con_g
!        end do
!       don't forget to flip 3d arrays around because gfs counts from bottom up      
        do k=1,lm
          k1          = lm-k+1
          t(k1)       = gt0(i,k)
          q(k1)       = gq0(i,k)
          pmid(k1)    = prsl(i,k)
          f_rimef(k1) = phy_f3d(i,k) 
!
! compute wet bulb
!                  
          pv     = pmid(k1)*q(k1)/(con_eps-con_epsm1*q(k1))
          td(k1) = ftdp(pv)
          tdpd   = t(k1)-td(k1)
!         if(pmid(k1)>=50000.)then ! only compute twet below 500mb to save time
            if(tdpd.gt.0.) then
              pr     = pmid(k1)
              tr     = t(k1)
              pk     = fpkap(pr)
              tlcl   = ftlcl(tr,tdpd)
              thelcl = fthe(tlcl,pk*tlcl/tr)
              call stma(thelcl,pk,twet(k1),qwet)
            else
              twet(k1)=t(k1)
            endif
!         endif 
          es     = fpvs(t(k1))
          es     = min(es,pmid(k1))
          qc     = con_eps*es/(pmid(k1)+con_epsm1*es)
          rh(k1) = max(con_epsq,q(k1))/qc
 
          k1       = lp1-k+1
          pint(k1) = prsi(i,k)
          zint(k1) = phii(i,k)/con_g

        enddo
        pint(1) = prsi(i,lp1)
        zint(1) = phii(i,lp1)/con_g

!     instantaneous precipitation type.

        call calwxt(lm,lp1,t,q,pmid,pint,  &
                    con_fvirt,con_rog,con_epsq,   &
                    zint,iwx,twet)
        snow(1)   = mod(iwx,2)
        sleet(1)  = mod(iwx,4)/2
        freezr(1) = mod(iwx,8)/4
        rain(1)   = iwx/8


!     dominant precipitation type

!gsm  if dominant precip type is requested, 4 more algorithms
!gsm    will be called.  the tallies are then summed in
!gsm    calwxt_dominant

        call calwxt_ramer(lm,lp1,t,q,pmid,rh,td, &
                  pint,iwx)

!
        snow(2)   = mod(iwx,2)
        sleet(2)  = mod(iwx,4)/2
        freezr(2) = mod(iwx,8)/4
        rain(2)   = iwx/8

! bourgouin algorithm
!      iseed=44641*(int(sdat(1)-1)*24*31+int(sdat(2))*24+ihrst)+   &
!     &  mod(ifhr*60+ifmin,44641)+4357

        call calwxt_bourg(lm,lp1,randomno(i,1),con_g,                  &
     &                    t,q,pmid,pint,zint,iwx)

!
        snow(3)   = mod(iwx,2)
        sleet(3)  = mod(iwx,4)/2
        freezr(3) = mod(iwx,8)/4
        rain(3)   = iwx/8

!
! revised ncep algorithm
!

        call calwxt_revised(lm,lp1,t,q,pmid,pint,  &
                            con_fvirt,con_rog,con_epsq,zint,twet,iwx)

!
        snow(4)   = mod(iwx,2)
        sleet(4)  = mod(iwx,4)/2
        freezr(4) = mod(iwx,8)/4
        rain(4)   = iwx/8
              
! explicit algorithm (under 18 not admitted without parent 
!     or guardian)
 
        if(n3dfercld == 3) then ! ferrier's scheme
          call calwxt_explicit(lm,tskin(i),sr(i),f_rimef,iwx)
          snow(5)   = mod(iwx,2)
          sleet(5)  = mod(iwx,4)/2
          freezr(5) = mod(iwx,8)/4
          rain(5)   = iwx/8
        else
          snow(5)   = 0
          sleet(5)  = 0
          freezr(5) = 0
          rain(5)   = 0
        endif
!
!               

        call calwxt_dominant(nalg,rain(1),freezr(1),sleet(1), &
                            snow(1),domr(i),domzr(i),domip(i),doms(i))


       else     !  prec < pthresh
        domr(i)=0.
        domzr(i)=0.
        domip(i)=0.
        doms(i)=0.
       end if
      enddo ! end loop for i


      deallocate (twet,rh,td)        
      return
      end
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
       subroutine calwxt(lm,lp1,t,q,pmid,pint,                      &
                 d608,rog,epsq,zint,iwx,twet)
! 
!     file: calwxt.f
!     written: 11 november 1993, michael baldwin
!     revisions:
!               30 sept 1994-setup new decision tree (m baldwin)
!               12 june 1998-conversion to 2-d (t black)
!     01-10-25  h chuang - modified to process hybrid model output
!     02-01-15  mike baldwin - wrf version
!                              
!
!     routine to compute precipitation type using a decision tree
!     approach that uses variables such as integrated wet bulb temp
!     below freezing and lowest layer temperature
!
!     see baldwin and contorno preprint from 13th weather analysis
!     and forecasting conference for more details
!     (or baldwin et al, 10th nwp conference preprint)
! 
!      use params_mod
!      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!    input:
!      t,q,pmid,htm,lmh,zint
!
      integer,intent(in):: lm,lp1
      real,dimension(lm),intent(in) :: t,q,pmid,twet
      real,dimension(lp1),intent(in) :: zint,pint
      integer,intent(out)  :: iwx
      real,intent(in) :: d608,rog,epsq
!     real,intent(out)  :: zwet


!    output:
!      iwx - instantaneous weather type.
!        acts like a 4 bit binary
!          1111 = rain/freezing rain/ice pellets/snow
!          where the one's digit is for snow
!                the two's digit is for ice pellets
!                the four's digit is for freezing rain
!            and the eight's digit is for rain
!
!    internal:
!
!      real, allocatable :: twet(:)
      real, parameter :: d00=0.0 
      integer karr,licee
      real tcold,twarm

!    subroutines called:
!     wetbulb
!     
!
!     initialize weather type array to zero (ie, off).
!     we do this since we want iwx to represent the
!     instantaneous weather type on return.
!     
!
!     allocate local storage
!

      integer l,lice,iwrml,ifrzl
      real    psfck,tdchk,a,tdkl,tdpre,tlmhk,twrmk,areas8,areap4,       &
              surfw,surfc,dzkl,area1,pintk1,pintk2,pm150,pkl,tkl,qkl

!      allocate ( twet(lm) )
!
!!$omp  parallel do
      iwx = 0
!      zwet=spval
!
!!$omp  parallel do
!!$omp& private(a,pkl,psfck,qkl,tdchk,tdkl,tdpre,tkl)

!
!   find coldest and warmest temps in saturated layer between
!   70 mb above ground and 500 mb
!   also find highest saturated layer in that range
!
!meb
      psfck=pint(lm+1)
!meb
      tdchk=2.0
  760 tcold=t(lm)
      twarm=t(lm)
      licee=lm
!
      do 775 l=1,lm
        qkl=q(l)
        qkl=max(epsq,qkl)
        tkl=t(l)
        pkl=pmid(l)
!
!   skip past this if the layer is not between 70 mb above ground
!       and 500 mb
!
        if (pkl.lt.50000.0.or.pkl.gt.psfck-7000.0) goto 775
        a=log(qkl*pkl/(6.1078*(0.378*qkl+0.622)))
        tdkl=(237.3*a)/(17.269-a)+273.15
        tdpre=tkl-tdkl
        if (tdpre.lt.tdchk.and.tkl.lt.tcold) tcold=tkl
        if (tdpre.lt.tdchk.and.tkl.gt.twarm) twarm=tkl
        if (tdpre.lt.tdchk.and.l.lt.licee) licee=l
  775 continue
!
!    if no sat layer at dew point dep=tdchk, increase tdchk
!     and start again (but don't make tdchk > 6)
!
      if (tcold==t(lm).and.tdchk<6.0) then
        tdchk=tdchk+2.0
        goto 760
      endif
!
!    lowest layer t
!
      karr=0
      tlmhk=t(lm)
!
!    decision tree time
!
      if (tcold>269.15) then
          if (tlmhk.le.273.15) then
!             turn on the flag for
!             freezing rain = 4
!             if its not on already
!             izr=mod(iwx(i,j),8)/4
!             if (izr.lt.1) iwx(i,j)=iwx(i,j)+4
            iwx=iwx+4
            goto 850
          else
!             turn on the flag for
!             rain = 8
!             if its not on already
!             irain=iwx(i,j)/8
!             if (irain.lt.1) iwx(i,j)=iwx(i,j)+8
            iwx=iwx+8
            goto 850
          endif
      endif
      karr=1
  850 continue
!
!   compute wet bulb only at points that need it
!
!      call wetbulb(lm,t,q,pmid,karr,twet)
!      call wetfrzlvl(twet,zwet)
!
!!$omp  parallel do
!!$omp& private(area1,areap4,areas8,dzkl,ifrzl,iwrml,lice,
!!$omp&         lmhk,pintk1,pintk2,pm150,psfck,surfc,surfw,
!!$omp&         tlmhk,twrmk)

      if(karr.gt.0)then
        lice=licee
!meb
        psfck=pint(lm+1)
!meb
        tlmhk=t(lm)
        twrmk=twarm
!
!    twet area variables
!     calculate only what is needed
!      from ground to 150 mb above surface
!      from ground to tcold layer
!      and from ground to 1st layer where wet bulb t < 0.0
!
!     pintk1 is the pressure at the bottom of the layer
!     pintk2 is the pressure at the top of the layer
!
!     areap4 is the area of twet above -4 c below highest sat lyr 
!
        areas8=d00
        areap4=d00
        surfw =d00
        surfc =d00
!
        do 1945 l=lm,lice,-1
        dzkl=zint(l)-zint(l+1)
        area1=(twet(l)-269.15)*dzkl
        if (twet(l).ge.269.15) areap4=areap4+area1
 1945   continue
!
        if (areap4.lt.3000.0) then
!             turn on the flag for
!             snow = 1
!             if its not on already
!             isno=mod(iwx(i,j),2)
!             if (isno.lt.1) iwx(i,j)=iwx(i,j)+1
          iwx=iwx+1
          go to 1900
        endif
!
!     areas8 is the net area of twet w.r.t. freezing in lowest 150mb
!
        pintk1=psfck
        pm150=psfck-15000.
!
        do 1955 l=lm,1,-1
        pintk2=pint(l)
        if(pintk1.lt.pm150)go to 1950
        dzkl=zint(l)-zint(l+1)
!
!    sum partial layer if in 150 mb agl layer
!
        if(pintk2.lt.pm150)                                      &
          dzkl=t(l)*(q(l)*d608+1.0)*rog*log(pintk1/pm150)
        area1=(twet(l)-273.15)*dzkl
        areas8=areas8+area1
 1950   pintk1=pintk2
 1955   continue
!
!     surfw is the area of twet above freezing between the ground
!       and the first layer above ground below freezing
!     surfc is the area of twet below freezing between the ground
!       and the warmest sat layer
!
        ifrzl=0
        iwrml=0
!
        do 2050 l=lm,1,-1
        if (ifrzl.eq.0.and.t(l).lt.273.15) ifrzl=1
        if (iwrml.eq.0.and.t(l).ge.twrmk) iwrml=1
!
        if (iwrml.eq.0.or.ifrzl.eq.0) then
!	  if(pmid(l) < 50000.)print*,'need twet above 500mb'
          dzkl=zint(l)-zint(l+1)
          area1=(twet(l)-273.15)*dzkl
          if(ifrzl.eq.0.and.twet(l).ge.273.15)surfw=surfw+area1
          if(iwrml.eq.0.and.twet(l).le.273.15)surfc=surfc+area1
        endif
 2050   continue
        if(surfc.lt.-3000.0.or.   &
          (areas8.lt.-3000.0.and.surfw.lt.50.0)) then
!             turn on the flag for
!             ice pellets = 2
!             if its not on already
!             iip=mod(iwx(i,j),4)/2
!             if (iip.lt.1) iwx(i,j)=iwx(i,j)+2
          iwx=iwx+2
          goto 1900
        endif
!
        if(tlmhk.lt.273.15) then
!             turn on the flag for
!             freezing rain = 4
!             if its not on already
!             izr=mod(iwx(k),8)/4
!             if (izr.lt.1) iwx(k)=iwx(k)+4
          iwx=iwx+4
        else
!             turn on the flag for
!             rain = 8
!             if its not on already
!             irain=iwx(k)/8
!             if (irain.lt.1) iwx(k)=iwx(k)+8
          iwx=iwx+8
        endif
      endif
 1900 continue
!---------------------------------------------------------
!      deallocate (twet)

      return
      end
!
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! dophase is a subroutine written and provided by jim ramer at noaa/fsl
!
!    ramer, j, 1993: an empirical technique for diagnosing precipitation
!           type from model output.  preprints, 5th conf. on aviation
!           weather systems, vienna, va, amer. meteor. soc., 227-230.
!
!   code adapted for wrf post  24 august 2005    g manikin
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      subroutine calwxt_ramer(lm,lp1,      &
                          t,q,pmid,rh,td,pint,ptyp)

!      subroutine dophase(pq,   !  input pressure sounding mb
!     +    t,   !  input temperature sounding k
!     +    pmid,   !  input pressure
!     +    pint,   !  input interface pressure
!     +    q,   !  input spec humidityfraction
!     +    lmh,   !  input number of levels in sounding
!     +    ptyp) !  output(2) phase 2=rain, 3=frzg, 4=solid,
!                                               6=ip     jc  9/16/99
!      use params_mod
!      use ctlblk_mod 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      real,parameter :: twice=266.55,rhprcp=0.80,deltag=1.02,             &
     &                  emelt=0.045,rlim=0.04,slim=0.85
      real,parameter :: twmelt=273.15,tz=273.15,efac=1.0 ! specify in params now 
!
      integer*4 i, k1, lll, k2, toodry
!
      real xxx ,mye, icefrac
      integer,intent(in) :: lm,lp1
      real,dimension(lm),intent(in) :: t,q,pmid,rh,td
      real,dimension(lp1),intent(in) :: pint
      integer,intent(out) :: ptyp
!
      real,dimension(lm) :: tq,pq,rhq
      real,dimension(lm) :: twq
!
      integer j,l,lev,ii
      real    rhmax,twmax,ptop,dpdrh,twtop,rhtop,wgt1,wgt2,    &
              rhavg,dtavg,dpk,ptw,pbot
!     real b,qtmp,rate,qc
      real,external :: xmytw
!
!  initialize.
      icefrac = -9999.
!

      ptyp = 0
      do l = 1,lm
        lev = lp1 - l
!        p(l)=pmid(l)
!        qc=pq0/p(l) * exp(a2*(t(l)-a3)/(t(l)-a4))
!gsm forcing q (qtmp) to be positive to deal with negative q values
!       causing problems later in this subroutine
!        qtmp=max(h1m12,q(l))	
!        rhqtmp(lev)=qtmp/qc
        rhq(lev) = rh(l)
        pq(lev)  = pmid(l) * 0.01
        tq(lev)  = t(l)
      enddo


!
!cc   rate restriction removed by john cortinas 3/16/99
!
!     construct wet-bulb sounding, locate generating level.
      twmax = -999.0
      rhmax = 0.0
      k1 = 0    !  top of precip generating layer
      k2 = 0    !  layer of maximum rh
!
      if (rhq(1) < rhprcp) then
        toodry = 1
      else
        toodry = 0
      end if
!
      pbot = pq(1)
!      nq=lm
      do l = 1, lm
!       xxx = tdofesat(esat(tq(l))*rhq(l))
        xxx = td(l)            !hc: use td consistent with gfs ice physics
        if (xxx < -500.) return
        twq(l) = xmytw(tq(l),xxx,pq(l))
        twmax = max(twq(l),twmax)
        if (pq(l) >= 400.0) then
          if (rhq(l) > rhmax) then
            rhmax = rhq(l)
            k2    = l
          end if
!
          if (l /= 1) then
            if (rhq(l) >= rhprcp .or. toodry == 0) then
              if (toodry /= 0) then
                 dpdrh = log(pq(l)/pq(l-1)) / (rhq(l)-rhq(l-1))
                 pbot  = exp(log(pq(l))+(rhprcp-rhq(l))*dpdrh)
!
                 ptw = pq(l)
                 toodry = 0
              else if (rhq(l)>= rhprcp) then
                 ptw = pq(l)
              else
                 toodry = 1
                 dpdrh  = log(pq(l)/pq(l-1)) / (rhq(l)-rhq(l-1))
                   ptw  = exp(log(pq(l))+(rhprcp-rhq(l))*dpdrh)

!lin             dpdrh  = (pq(i)-pq(i-1))/(rhq(i)-rhq(i-1))
!lin             ptw    = pq(i)+(rhprcp-rhq(i))*dpdrh
!
              end if
!
              if (pbot/ptw >= deltag) then
!lin            if (pbot-ptw.lt.deltag) goto 2003
                k1   = l
                ptop = ptw
              end if
            end if
          end if
        end if
      enddo
!
!     gross checks for liquid and solid precip which dont require generating level.
!
      if (twq(1) >= 273.15+2.0) then
         ptyp    = 8   ! liquid
         icefrac = 0.0
         return
      end if
!
      if (twmax <= twice) then
         icefrac = 1.0
         ptyp    = 1   !  solid
         return
      end if
!
!     check to see if we had no success with locating a generating level.
!
      if (k1 == 0) return
!
      if (ptop == pq(k1)) then
        twtop = twq(k1)
        rhtop = rhq(k1)
        k2    = k1
        k1    = k1 - 1
      else
        k2    = k1
        k1    = k1 - 1
        wgt1  = log(ptop/pq(k2)) / log(pq(k1)/pq(k2))
        wgt2  = 1.0 - wgt1
        twtop = twq(k1) * wgt1 + twq(k2) * wgt2
        rhtop = rhq(k1) * wgt1 + rhq(k2) * wgt2
      end if
!
!     calculate temp and wet-bulb ranges below precip generating level.
      do l = 1, k1
        twmax = max(twq(l),twmax)
      enddo
!
!     gross check for solid precip, initialize ice fraction.
!     if (i.eq.1.and.j.eq.1) write (*,*) 'twmax=',twmax,twice,'twtop=',twtop

      if (twtop <= twice) then
        icefrac = 1.0
        if (twmax <= twmelt) then    ! gross check for solid precip.
           ptyp = 1                  ! solid precip
           return
        end if
        lll = 0
      else
        icefrac = 0.0
        lll = 1
      end if
!
!     loop downward through sounding from highest precip generating level.
   30 continue
!
      if (icefrac >= 1.0) then  !  starting as all ice
        if (twq(k1) < twmelt) go to 40       ! cannot commence melting
        if (twq(k1) == twtop) go to 40        ! both equal twmelt, nothing h
        wgt1  = (twmelt-twq(k1)) / (twtop-twq(k1))
        rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) * 0.5
        dtavg = (twmelt-twq(k1)) * 0.5
        dpk   = wgt1 * log(pq(k1)/ptop)        !lin   dpk=wgt1*(pq(k1)-ptop)
!       mye=emelt*(1.0-(1.0-rhavg)*efac)
        mye = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
      else if (icefrac <= 0.0) then     !  starting as all liquid
        lll = 1
!       goto 1020
        if (twq(k1) > twice) go to 40        ! cannot commence freezing
        if (twq(k1) == twtop) then
            wgt1 = 0.5
        else
            wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
        end if
        rhavg   = rhq(k1) + wgt1 * (rhtop-rhq(k1)) * 0.5
        dtavg   = twmelt - (twq(k1)+twice) * 0.5
        dpk     = wgt1 * log(pq(k1)/ptop)      !lin  dpk=wgt1*(pq(k1)-ptop)
!       mye     = emelt*(1.0-(1.0-rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
      else if ((twq(k1) <= twmelt).and.(twq(k1) < twmelt)) then ! mix
        rhavg   = (rhq(k1)+rhtop) * 0.5
        dtavg   = twmelt - (twq(k1)+twtop) * 0.5
        dpk     = log(pq(k1)/ptop)       !lin   dpk=pq(k1)-ptop
!       mye     = emelt*(1.0-(1.0-rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye           
      else                 ! mix where tw curve crosses twmelt in layer
        if (twq(k1) == twtop) go to 40   ! both equal twmelt, nothing h
        wgt1    = (twmelt-twq(k1)) / (twtop-twq(k1))
        wgt2    = 1.0 - wgt1
        rhavg   = rhtop + wgt2 * (rhq(k1)-rhtop) * 0.5
        dtavg   = (twmelt-twtop) * 0.5
        dpk     = wgt2 * log(pq(k1)/ptop)     !lin   dpk=wgt2*(pq(k1)-ptop)
!       mye     = emelt*(1.0-(1.0-rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
        icefrac = min(1.0,max(icefrac,0.0))   
        if (icefrac <= 0.0) then
!           goto 1020
            if (twq(k1) > twice) go to 40    ! cannot commence freezin
            wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
            dtavg = twmelt - (twq(k1)+twice) * 0.5
        else
            dtavg = (twmelt-twq(k1)) * 0.5
        end if
        rhavg   = rhq(k1) + wgt1 * (rhtop-rhq(k1)) * 0.5
        dpk     = wgt1 * log(pq(k1)/ptop)     !lin  dpk=wgt1*(pq(k1)-ptop)
!       mye     = emelt*(1.0-(1.0-rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
      end if
!
      icefrac = min(1.0,max(icefrac,0.0))

!     if (i.eq.1.and.j.eq.1) write (*,*) 'new icefrac:', icefrac, icefrac
!
!     get next level down if there is one, loop back.
   40 continue
      if (k1 > 1) then
        twtop = twq(k1)
        ptop  = pq(k1)
        rhtop = rhq(k1)
        k1    = k1 - 1
        go to 30
      end if
!
!     determine precip type based on snow fraction and surface wet-bulb.
!
      if (icefrac >= slim) then
        if (lll /= 0) then
          ptyp = 2       ! ice pellets   jc 9/16/99
        else
          ptyp = 1       !  snow
        end if
      else if (icefrac <= rlim) then
        if (twq(1).lt.tz) then
          ptyp = 4       !  freezing precip
        else
          ptyp = 8       !  rain
        end if
      else
        if (twq(1) < tz) then
!gsm not sure what to do when 'mix' is predicted;   in previous
!gsm   versions of this code for which i had to have an answer,
!gsm   i chose sleet.  here, though, since we have 4 other
!gsm   algorithms to provide an answer, i will not declare a
!gsm   type from the ramer in this situation and allow the
!gsm   other algorithms to make the call.
      
           ptyp = 0       !  don't know 
!          ptyp = 5       !  mix
        else
!          ptyp = 5       !  mix
           ptyp = 0       !  don't know 
        end if
      end if

      return
!
      end
!
!
!--------------------------------------------------------------------------
!      real*4 function mytw(t,td,p)
      function xmytw(t,td,p)
!
      implicit none
!
      integer*4 cflag, l
!     real*4 f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,        &
      real   f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,        &
     &          de, xmytw
      data f, c0, c1, c2 /0.0006355, 26.66082, 0.0091379024, 6106.3960/
!
!
      xmytw = (t+td) / 2
      if (td.ge.t) return
!
      if (t.lt.100.0) then
          k = t + 273.15
          kd = td + 273.15
          if (kd.ge.k) return
          cflag = 1
      else
          k = t
          kd = td
          cflag = 0
      end if
!
      ed = c0 - c1 * kd - c2 / kd
      if (ed.lt.-14.0.or.ed.gt.7.0) return
      ed = exp(ed)
      ew = c0 - c1 * k - c2 / k
      if (ew.lt.-14.0.or.ew.gt.7.0) return
      ew = exp(ew)
      fp = p * f
      s = (ew-ed) / (k-kd)
      kw = (k*fp+kd*s) / (fp+s)
!
      do 10 l = 1, 5
          ew = c0 - c1 * kw - c2 / kw
          if (ew.lt.-14.0.or.ew.gt.7.0) return
          ew = exp(ew)
          de = fp * (k-kw) + ed - ew
          if (abs(de/ew).lt.1e-5) go to 20
          s = ew * (c1-c2/(kw*kw)) - fp
          kw = kw - de / s
   10 continue
   20 continue
!
!      print *, 'kw ', kw
      if (cflag.ne.0) then
          xmytw = kw - 273.15
      else
          xmytw = kw
      end if
!
      return
      end
!
!
!$$$  subprogram documentation block
!
! subprogram: calwxt_bourg    calculate precipitation type (bourgouin)
!   prgmmr: baldwin      org: np22        date: 1999-07-06
!
! abstract: this routine computes precipitation type
!    using a decision tree approach that uses the so-called
!    "energy method" of bourgouin of aes (canada) 1992
!
! program history log:
!   1999-07-06  m baldwin
!   1999-09-20  m baldwin  make more consistent with bourgouin (1992)
!   2005-08-24  g manikin  added to wrf post
!   2007-06-19  m iredell  mersenne twister, best practices
!   2008-03-03  g manikin  added checks to prevent stratospheric warming
!                           episodes from being seen as "warm" layers
!                           impacting precip type
!
! usage:    call calwxt_bourg(im,jm,jsta_2l,jend_2u,jsta,jend,lm,lp1,   &
!    &                        iseed,g,                          &
!    &                        t,q,pmid,pint,lmh,zint,ptype)
!   input argument list:
!     im       integer i dimension
!     jm       integer j dimension
!     jsta_2l  integer j dimension start point (including haloes)
!     jend_2u  integer j dimension end point (including haloes)
!     jsta     integer j dimension start point (excluding haloes)
!     jend     integer j dimension end point (excluding haloes)
!     lm       integer k dimension
!     lp1      integer k dimension plus 1
!     iseed    integer random number seed
!     g        real gravity (m/s**2)
!     t        real(im,jsta_2l:jend_2u,lm) mid layer temp (k)
!     q        real(im,jsta_2l:jend_2u,lm) specific humidity (kg/kg)
!     pmid     real(im,jsta_2l:jend_2u,lm) mid layer pressure (pa)
!     pint     real(im,jsta_2l:jend_2u,lp1) interface pressure (pa)
!     lmh      real(im,jsta_2l:jend_2u) max number of layers
!     zint     real(im,jsta_2l:jend_2u,lp1) interface height (m)
!   output argument list:
!     ptype    integer instantaneous weather type ()
!              acts like a 4 bit binary
!                1111 = rain/freezing rain/ice pellets/snow
!                where the one's digit is for snow
!                      the two's digit is for ice pellets
!                      the four's digit is for freezing rain
!                  and the eight's digit is for rain
!              in other words...
!                ptype=1 snow
!                ptype=2 ice pellets/mix with ice pellets
!                ptype=4 freezing rain/mix with freezing rain
!                ptype=8 rain
!
! modules used:
!   mersenne_twister pseudo-random number generator
!
! subprograms called:
!   random_number    pseudo-random number generator
!
! attributes:
!   language: fortran 90
!
! remarks: vertical order of arrays must be layer   1 = top
!                                       and layer lmh = bottom
!
!$$$
      subroutine calwxt_bourg(lm,lp1,rn,g,      &
     &                        t,q,pmid,pint,zint,ptype)
!      use mersenne_twister
      implicit none
!
!    input:
      integer,intent(in):: lm,lp1
!      integer,intent(in):: iseed
      real,intent(in):: g,rn
      real,intent(in):: t(lm)
      real,intent(in):: q(lm)
      real,intent(in):: pmid(lm)
      real,intent(in):: pint(lp1)
      real,intent(in):: zint(lp1)
!
!    output:
      integer,intent(out):: ptype
!
      integer ifrzl,iwrml,l,lhiwrm
      real pintk1,areane,tlmhk,areape,pintk2,surfw,area1,dzkl,psfck
!
!     initialize weather type array to zero (ie, off).
!     we do this since we want ptype to represent the
!     instantaneous weather type on return.
!     
!!$omp  parallel do

      ptype = 0

!
!      call random_number(rn,iseed)
!
!       attention r1, r2 are removed 
!!$omp  parallel do
!!$omp& private(a,tlmhk,iwrml,psfck,lhiwrm,pintk1,pintk2,area1,
!!$omp&         areape,dzkl,surfw,r1,r2)

      psfck=pint(lm+1)
!     find the depth of the warm layer based at the surface
!     this will be the cut off point between computing
!     the surface based warm air and the warm air aloft
!
!
!     lowest layer t
!
      tlmhk = t(lm)
      iwrml = lm + 1
      if (tlmhk.ge.273.15) then
        do l = lm, 2, -1
         if (t(l).ge.273.15.and.t(l-1).lt.273.15.and.           &
     &            iwrml.eq.lm+1) iwrml = l
          end do
      end if
!
!     now find the highest above freezing level
!
      lhiwrm = lm + 1
      do l = lm, 1, -1
! gsm  added 250 mb check to prevent stratospheric warming situations
!       from counting as warm layers aloft      
          if (t(l).ge.273.15 .and. pmid(l).gt.25000.) lhiwrm = l
      end do

!     energy variables
!     surfw is the positive energy between the ground
!     and the first sub-freezing layer above ground
!     areane is the negative energy between the ground
!     and the highest layer above ground
!     that is above freezing
!     areape is the positive energy "aloft"
!     which is the warm energy not based at the ground
!     (the total warm energy = surfw + areape)
!
!     pintk1 is the pressure at the bottom of the layer
!     pintk2 is the pressure at the top of the layer
!     dzkl is the thickness of the layer
!     ifrzl is a flag that tells us if we have hit
!     a below freezing layer
!
      pintk1 = psfck
      ifrzl = 0
      areane = 0.0
      areape = 0.0
      surfw = 0.0                                         

      do l = lm, 1, -1
          if (ifrzl.eq.0.and.t(l).le.273.15) ifrzl = 1
          pintk2=pint(l)
          dzkl=zint(l)-zint(l+1)
          area1 = log(t(l)/273.15) * g * dzkl
          if (t(l).ge.273.15.and. pmid(l).gt.25000.) then
              if (l.lt.iwrml) areape = areape + area1
              if (l.ge.iwrml) surfw = surfw + area1
          else
              if (l.gt.lhiwrm) areane = areane + abs(area1)
          end if
          pintk1 = pintk2
      end do
      
!
!     decision tree time
!
      if (areape.lt.2.0) then
!         very little or no positive energy aloft, check for
!         positive energy just above the surface to determine rain vs. snow
          if (surfw.lt.5.6) then
!             not enough positive energy just above the surface
!             snow = 1
              ptype = 1
          else if (surfw.gt.13.2) then
!             enough positive energy just above the surface
!             rain = 8
              ptype = 8
          else
!             transition zone, assume equally likely rain/snow
!             picking a random number, if <=0.5 snow
              if (rn.le.0.5) then
!                 snow = 1
                  ptype = 1
              else
!                 rain = 8
                  ptype = 8
              end if
          end if
!
      else
!         some positive energy aloft, check for enough negative energy
!         to freeze and make ice pellets to determine ip vs. zr
          if (areane.gt.66.0+0.66*areape) then
!             enough negative area to make ip,
!             now need to check if there is enough positive energy
!             just above the surface to melt ip to make rain
              if (surfw.lt.5.6) then
!                 not enough energy at the surface to melt ip
!                 ice pellets = 2
                  ptype = 2
              else if (surfw.gt.13.2) then
!                 enough energy at the surface to melt ip
!                 rain = 8
                  ptype = 8
              else
!                 transition zone, assume equally likely ip/rain
!                 picking a random number, if <=0.5 ip
                  if (rn.le.0.5) then
!                     ice pellets = 2
                      ptype = 2
                  else
!                     rain = 8
                      ptype = 8
                  end if
              end if
          else if (areane.lt.46.0+0.66*areape) then
!             not enough negative energy to refreeze, check surface temp
!             to determine rain vs. zr
              if (tlmhk.lt.273.15) then
!                 freezing rain = 4
                  ptype = 4
              else
!                 rain = 8
                  ptype = 8
              end if
          else
!             transition zone, assume equally likely ip/zr
!             picking a random number, if <=0.5 ip
              if (rn.le.0.5) then
!                 still need to check positive energy
!                 just above the surface to melt ip vs. rain
                  if (surfw.lt.5.6) then
!                     ice pellets = 2
                      ptype = 2
                  else if (surfw.gt.13.2) then
!                     rain = 8
                      ptype = 8
                  else
!                     transition zone, assume equally likely ip/rain
!                     picking a random number, if <=0.5 ip
                      if (rn.le.0.25) then
!                         ice pellets = 2
                          ptype = 2
                      else
!                         rain = 8
                          ptype = 8
                      end if
                  end if
              else
!                 not enough negative energy to refreeze, check surface temp
!                 to determine rain vs. zr
                  if (tlmhk.lt.273.15) then
!                     freezing rain = 4
                      ptype = 4
                  else
!                     rain = 8
                      ptype = 8
                  end if
              end if
          end if
      end if
!      end do
!      end do
      return
      end
!
!
       subroutine calwxt_revised(lm,lp1,t,q,pmid,pint,  &
                 d608,rog,epsq,    &
     &             zint,twet,iwx)
! 
!     file: calwxt.f
!     written: 11 november 1993, michael baldwin
!     revisions:
!               30 sept 1994-setup new decision tree (m baldwin)
!               12 june 1998-conversion to 2-d (t black)
!     01-10-25  h chuang - modified to process hybrid model output
!     02-01-15  mike baldwin - wrf version
!     05-07-07  binbin zhou  - add prec for rsm
!     05-08-24  geoff manikin - modified the area requirements
!                to make an alternate algorithm 
!                              
!
!     routine to compute precipitation type using a decision tree
!     approach that uses variables such as integrated wet bulb temp
!     below freezing and lowest layer temperature
!
!     see baldwin and contorno preprint from 13th weather analysis
!     and forecasting conference for more details
!     (or baldwin et al, 10th nwp conference preprint)
!
!     since the original version of the algorithm has a high bias
!      for freezing rain and sleet, the goal is to balance that bias
!      with a version more likely to predict snow
!
!     use params_mod
!     use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!  list of variables needed
!    parameters:
!      d608,rog,h1,d00
!hc       parameter(d608=0.608,rog=287.04/9.8,h1=1.0,d00=0.0)
!
!    input:
!      t,q,pmid,htm,lmh,zint
      integer,intent(in):: lm,lp1
      real,dimension(lm),intent(in) ::  t,q,pmid,twet
      real,dimension(lp1),intent(in) ::  pint,zint 
      real,intent(in) ::  d608,rog,epsq
!    output:
!      iwx - instantaneous weather type.
!        acts like a 4 bit binary
!          1111 = rain/freezing rain/ice pellets/snow
!          where the one's digit is for snow
!                the two's digit is for ice pellets
!                the four's digit is for freezing rain
!            and the eight's digit is for rain
      integer, intent(out) ::  iwx
!    internal:
!
      real, parameter :: d00=0.0  
      integer karr,licee
      real tcold,twarm
!
      integer l,lmhk,lice,iwrml,ifrzl
      real psfck,tdchk,a,tdkl,tdpre,tlmhk,twrmk,areas8,areap4,area1,  &
           surfw,surfc,dzkl,pintk1,pintk2,pm150,qkl,tkl,pkl,area0,    &
           areap0

!    subroutines called:
!     wetbulb
!     
!
!     initialize weather type array to zero (ie, off).
!     we do this since we want iwx to represent the
!     instantaneous weather type on return.
!     
!
!     allocate local storage
!
!
!!$omp  parallel do
      iwx = 0

!!$omp  parallel do
!!$omp& private(a,lmhk,pkl,psfck,qkl,tdchk,tdkl,tdpre,tkl)

      lmhk=lm
!
!   find coldest and warmest temps in saturated layer between
!   70 mb above ground and 500 mb
!   also find highest saturated layer in that range
!
!meb
      psfck=pint(lp1)
!meb
      tdchk=2.0
  760 tcold=t(lmhk)
      twarm=t(lmhk)
      licee=lmhk
!
      do 775 l=1,lmhk
      qkl=q(l)
      qkl=max(epsq,qkl)
      tkl=t(l)
      pkl=pmid(l)
!
!   skip past this if the layer is not between 70 mb above ground
!       and 500 mb
!
      if (pkl.lt.50000.0.or.pkl.gt.psfck-7000.0) goto 775
      a=log(qkl*pkl/(6.1078*(0.378*qkl+0.622)))
      tdkl=(237.3*a)/(17.269-a)+273.15
      tdpre=tkl-tdkl
      if (tdpre.lt.tdchk.and.tkl.lt.tcold) tcold=tkl
      if (tdpre.lt.tdchk.and.tkl.gt.twarm) twarm=tkl
      if (tdpre.lt.tdchk.and.l.lt.licee) licee=l
  775 continue
!
!    if no sat layer at dew point dep=tdchk, increase tdchk
!     and start again (but don't make tdchk > 6)
!
      if (tcold.eq.t(lmhk).and.tdchk.lt.6.0) then
        tdchk=tdchk+2.0
        goto 760
      endif
!
!    lowest layer t
!
      karr=0
      lmhk=lm
      tlmhk=t(lmhk)
!
!    decision tree time
!
      if (tcold.gt.269.15) then
          if (tlmhk.le.273.15) then
!             turn on the flag for
!             freezing rain = 4
!             if its not on already
!             izr=mod(iwx,8)/4
!             if (izr.lt.1) iwx=iwx+4
              iwx=iwx+4
            goto 850
          else
!             turn on the flag for
!             rain = 8
!             if its not on already
!             irain=iwx/8
!             if (irain.lt.1) iwx=iwx+8
              iwx=iwx+8
            goto 850
          endif
      endif
      karr=1
  850 continue
!
!!$omp  parallel do
!!$omp& private(area1,areap4,areap0,areas8,dzkl,ifrzl,iwrml,lice,
!!$omp&         lmhk,pintk1,pintk2,pm150,psfck,surfc,surfw,
!!$omp&         tlmhk,twrmk)

      if(karr.gt.0)then
        lmhk=lm
        lice=licee
!meb
        psfck=pint(lp1)
!meb
        tlmhk=t(lmhk)
        twrmk=twarm
!
!    twet area variables
!     calculate only what is needed
!      from ground to 150 mb above surface
!      from ground to tcold layer
!      and from ground to 1st layer where wet bulb t < 0.0
!
!     pintk1 is the pressure at the bottom of the layer
!     pintk2 is the pressure at the top of the layer
!
!     areap4 is the area of twet above -4 c below highest sat lyr 
!     areap0 is the area of twet above 0 c below highest sat lyr
!
        areas8=d00
        areap4=d00
        areap0=d00
        surfw =d00
        surfc =d00
        
!
        do 1945 l=lmhk,lice,-1
        dzkl=zint(l)-zint(l+1)
        area1=(twet(l)-269.15)*dzkl
        area0=(twet(l)-273.15)*dzkl
        if (twet(l).ge.269.15) areap4=areap4+area1
        if (twet(l).ge.273.15) areap0=areap0+area0
 1945   continue
!
!        if (areap4.lt.3000.0) then
!             turn on the flag for
!             snow = 1
!             if its not on already
!             isno=mod(iwx,2)
!             if (isno.lt.1) iwx=iwx+1
!          iwx=iwx+1
!          go to 1900
!        endif
        if (areap0.lt.350.0) then
!             turn on the flag for
!             snow = 1
              iwx=iwx+1
            goto 1900
       endif
!
!     areas8 is the net area of twet w.r.t. freezing in lowest 150mb
!
        pintk1=psfck
        pm150=psfck-15000.
!
        do 1955 l=lmhk,1,-1
        pintk2=pint(l)
        if(pintk1.lt.pm150)go to 1950
        dzkl=zint(l)-zint(l+1)
!
!    sum partial layer if in 150 mb agl layer
!
        if(pintk2.lt.pm150)                                   &
          dzkl=t(l)*(q(l)*d608+1.0)*rog*                      &
               log(pintk1/pm150)
        area1=(twet(l)-273.15)*dzkl
        areas8=areas8+area1
 1950   pintk1=pintk2
 1955   continue
!
!     surfw is the area of twet above freezing between the ground
!       and the first layer above ground below freezing
!     surfc is the area of twet below freezing between the ground
!       and the warmest sat layer
!
        ifrzl=0
        iwrml=0
!
        do 2050 l=lmhk,1,-1
        if (ifrzl.eq.0.and.t(l).lt.273.15) ifrzl=1
        if (iwrml.eq.0.and.t(l).ge.twrmk) iwrml=1
!
        if (iwrml.eq.0.or.ifrzl.eq.0) then
!	  if(pmid(l) .lt. 50000.)print*,'twet needed above 500mb'
          dzkl=zint(l)-zint(l+1)
          area1=(twet(l)-273.15)*dzkl
          if(ifrzl.eq.0.and.twet(l).ge.273.15)surfw=surfw+area1
          if(iwrml.eq.0.and.twet(l).le.273.15)surfc=surfc+area1
        endif
 2050   continue
        if(surfc.lt.-3000.0.or.                                    &
     &    (areas8.lt.-3000.0.and.surfw.lt.50.0)) then
!             turn on the flag for
!             ice pellets = 2
!             if its not on already
!             iip=mod(iwx,4)/2
!             if (iip.lt.1) iwx=iwx+2
          iwx=iwx+2
          goto 1900
        endif
!
        if(tlmhk.lt.273.15) then
!             turn on the flag for
!             freezing rain = 4
!             if its not on already
!             izr=mod(iwx(k),8)/4
!             if (izr.lt.1) iwx(k)=iwx(k)+4
          iwx=iwx+4
        else
!             turn on the flag for
!             rain = 8
!             if its not on already
!             irain=iwx(k)/8
!             if (irain.lt.1) iwx(k)=iwx(k)+8
          iwx=iwx+8
        endif
      endif
 1900 continue

      return
      end
!
!
      subroutine calwxt_explicit(lm,tskin,sr,f_rimef,iwx)
! 
!     file: calwxt.f
!     written: 24 august 2005, g manikin and b ferrier 
!
!     routine to compute precipitation type using explicit fields
!       from the model microphysics

!      use params_mod
!      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!  list of variables needed
!    parameters:
!
!    input:
      integer, intent(in):: lm
      real,intent(in)::  tskin, sr
      real,intent(in):: f_rimef(lm)
      integer,intent(out) :: iwx
      real snow
!     real psfc
!
!     allocate local storage
!
!!$omp  parallel do
      iwx = 0

!gsm  the rsm is currently incompatible with this routine
!gsm   according to b ferrier, there may be a way to write
!gsm   a version of this algorithm to work with the rsm
!gsm   microphysics, but it doesn't exist at this time
!!$omp  parallel do
!!$omp& private(psfc,tskin)

!
!  a snow ratio less than 0.5 eliminates snow and sleet
!   use the skin temperature to distinguish rain from freezing rain
!   note that 2-m temperature may be a better choice if the model
!   has a cold bias for skin temperature
! 
      if (sr.lt.0.5) then
!        surface (skin) potential temperature and temperature.
!         psfc=pmid(lm)
!         tskin=ths*(psfc/p1000)**capa 

         if (tskin.lt.273.15) then
!          freezing rain = 4
           iwx=iwx+4
         else
!          rain = 8
           iwx=iwx+8
         endif
      else
!  
!  distinguish snow from sleet with the rime factor
! 
        if(f_rimef(lm).ge.10) then
!          sleet = 2
           iwx=iwx+2
        else
           snow = 1
           iwx=iwx+1 
        endif
      endif
 810  return 
      end
!
!
       subroutine calwxt_dominant(nalg,rain,freezr,sleet,snow,     &
     &         domr,domzr,domip,doms)
!
!     written: 24 august 2005, g manikin 
!      
!     this routine takes the precip type solutions from different
!       algorithms and sums them up to give a dominant type
!
!      use params_mod
!      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!    input:
      integer,intent(in) :: nalg
      real,intent(out) ::  doms,domr,domzr,domip
      integer,dimension(nalg),intent(in) ::  rain,snow,sleet,freezr
      integer l
      real totsn,totip,totr,totzr
!--------------------------------------------------------------------------
!
      totsn = 0
      totip = 0
      totr  = 0
      totzr = 0 
!   loop over the number of different algorithms that are used
      do 820 l = 1, nalg
        if (rain(l).gt. 0) then
           totr = totr + 1
           goto 830
        endif

        if (snow(l).gt. 0) then
           totsn = totsn + 1
           goto 830
        endif

        if (sleet(l).gt. 0) then
           totip = totip + 1
           goto 830
        endif

        if (freezr(l).gt. 0) then
           totzr = totzr + 1
           goto 830
        endif
 830    continue
 820  continue

      domr = 0.
      doms = 0.
      domzr = 0.
      domip = 0.

!   ties are broken to favor the most dangerous form of precip
!     freezing rain > snow > sleet > rain 
      if (totsn .gt. totip) then
        if (totsn .gt. totzr) then
          if (totsn .ge. totr) then
           doms = 1.
           goto 800 
          else
           domr = 1.
           goto 800 
          endif
        else if (totzr .ge. totr) then
          domzr = 1.
          goto 800 
        else
          domr = 1.
          goto 800 
        endif 
      else if (totip .gt. totzr) then
        if (totip .ge. totr) then
          domip = 1.
          goto 800 
        else
          domr = 1.
          goto 800 
        endif
      else if (totzr .ge. totr) then
         domzr = 1.
         goto 800 
      else
          domr = 1.
          goto 800 
      endif
 800  continue 
      return
      end

      
            
      
      
