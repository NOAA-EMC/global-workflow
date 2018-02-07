SUBROUTINE CALPRECIPTYPE(kdt,nrcm,im,ix,lm,lp1,randomno,      &
                         xlat,xlon,                           &
                         gt0,gq0,prsl,prsi,PREC,              & !input
                         phii,n3dfercld,TSKIN,SR,phy_f3d,     & !input
                         DOMR,DOMZR,DOMIP,DOMS)  !output
!      SUBROUTINE CALPRECIPTYPE(nrcm,randomno,im,lm,lp1,T,Q,PMID,PINT,PREC, & !input
!                           ZINT,n3dfercld,TSKIN,SR,F_RimeF,  & !input
!			   DOMR,DOMZR,DOMIP,DOMS)  !output
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALPRECIPTYPE      COMPUTE DOMINANT PRECIP TYPE
!   PRGRMMR: CHUANG         ORG: W/NP2      DATE: 2008-05-28
!          
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES PRECIPITATION TYPE.
!   . It is adopted from post but was made into a column to used by GFS model    
!     
!
!      use vrbls3d   
!      use vrbls2d   
!      use soil
!      use masks
!      use params_mod
!      use ctlblk_mod
!      use rqstfld_mod
      USE FUNCPHYS, ONLY : gfuncphys,fpvs,ftdp,fpkap,ftlcl,stma,fthe
      USE PHYSCONS
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!      INCLUDE "mpif.h"
!     
!     IN NGM SUBROUTINE OUTPUT WE FIND THE FOLLOWING COMMENT.
!     "IF THE FOLLOWING THRESHOLD VALUES ARE CHANGED, CONTACT
!     TDL/SYNOPTIC-SCALE TECHNIQUES BRANCH (PAUL DALLAVALLE
!     AND JOHN JENSENIUS).  THEY MAY BE USING IT IN ONE OF 
!     THEIR PACKING CODES."  THE THRESHOLD VALUE IS 0.01 INCH
!     OR 2.54E-4 METER.  PRECIPITATION VALUES LESS THAN THIS
!     THRESHOLD ARE SET TO MINUS ONE TIMES THIS THRESHOLD.

      real,PARAMETER :: PTHRESH = 0.0
!     
!     SET CELCIUS TO KELVIN AND SECOND TO HOUR CONVERSION.
      integer,PARAMETER :: NALG    = 5
!     
!     DECLARE VARIABLES.
!     
      integer,intent(in) :: kdt,nrcm,im,ix,lm,lp1,n3dfercld
      real,intent(in) :: xlat(im),xlon(im) 
      real,dimension(im),intent(in) :: PREC,SR,TSKIN
      real,intent(in) :: randomno(ix,nrcm)
      real,dimension(ix,LM),intent(in) :: gt0,gq0,prsl,phy_f3d
      real,dimension(ix,lp1),intent(in) :: prsi,phii
      real,dimension(im),intent(out) :: DOMR,DOMZR,DOMIP,DOMS
      INTEGER :: IWX1,IWX4,IWX5
      REAL :: IWX2,IWX3
      REAL :: ES,QC
      REAL :: SLEET(NALG),RAIN(NALG),FREEZR(NALG),SNOW(NALG)
      real,dimension(LM) :: T,Q,PMID,F_RimeF
      real,dimension(lp1) :: pint,zint
      REAL, ALLOCATABLE :: RH(:)
      REAL(kind=kind_phys), ALLOCATABLE :: TD8(:)
      integer :: I,IWX,ISNO,IIP,IZR,IRAIN,k,k1
      real :: time_vert,time_ncep,time_ramer,time_bourg,time_revised,&
          time_dominant,btim,timef
      real(kind=kind_phys) :: pv8,pr8,pk8,tr8,tdpd8,tlcl8,thelcl8
      real(kind=kind_phys) :: qwet8,t8(lm)
      real(kind=kind_phys),allocatable :: twet8(:)

! convert geopotential to height
!      do l=1,lp1
!        zint(l)=zint(l)/con_g
!      end do
! DON'T FORGET TO FLIP 3D ARRAYS AROUND BECAUSE GFS COUNTS FROM BOTTOM UP      
      	    
      ALLOCATE ( RH(LM),TD8(LM),TWET8(LM) )

! Create look up table
      call gfuncphys

      time_vert    = 0.
      time_ncep    = 0.
      time_ramer   = 0.
      time_bourg   = 0.
      time_revised = 0.

      do i=1,im
!        print *, 'in calprecip xlat/xlon=', xlat(im),xlon(im),'levs=',lm
        do k=1,lm
	  k1          = lm-k+1
	  t8(k1)      = gt0(i,k)
	  q(k1)       = gq0(i,k)
	  pmid(k1)    = prsl(i,k) 
	  f_rimef(k1) = phy_f3d(i,k) 
	  pv8     = pmid(k1)*q(k1)/(con_eps-con_epsm1*q(k1))
	  td8(k1) = ftdp(pv8)
          tdpd8   = t8(k1)-td8(k1)
          if(pmid(k1)>=50000.)then ! only compute twet below 500mb to save time
            if(tdpd8.gt.0.) then
              pr8     = pmid(k1)
              tr8     = t8(k1)
              pk8     = fpkap(pr8)
              tlcl8   = ftlcl(tr8,tdpd8)
              thelcl8 = fthe(tlcl8,pk8*tlcl8/tr8)
              call stma(thelcl8,pk8,twet8(k1),qwet8)
            else
              twet8(k1)=t8(k1)
            endif
	  endif 
          ES       = FPVS(T8(k1))
	  ES       = MIN(ES,PMID(k1))
	  QC       = CON_EPS*ES/(PMID(k1)+CON_EPSM1*ES)
          RH(k1)   = MAX(con_epsq,Q(k1))/QC
	  k1       = lp1-k+1
	  pint(k1) = prsi(i,k) 
	  zint(k1) = phii(i,k)  !height in meters
	enddo
	pint(1) = prsi(i,lp1) 
	zint(1) = phii(i,lp1)

!         print*,'debug in calpreciptype: i,im,lm,lp1,xlon,xlat,prec,tskin,sr,nrcm,randomno,n3dfercld ', &
!         i,im,lm,lp1,xlon(i)*57.29578,xlat(i)*57.29578,prec(i),tskin(i),sr(i),  &
!	 nrcm,randomno(i,1:nrcm),n3dfercld
!         do l=1,lm
!          print*,'debug in calpreciptype: l,t,q,p,pint,z,twet', &
!	  l,t(l),q(l), &
!          pmid(l),pint(l),zint(l),twet(l)
!         end do
!	 print*,'debug in calpreciptype: lp1,pint,z ', lp1,pint(lp1),zint(lp1)
!        end if  
! end debug print statement		

        CALL CALWXT(lm,lp1,T8(1),Q(1),PMID(1),PINT(1),PREC(i),  &
                    PTHRESH,con_fvirt,con_rog,con_epsq,   &
                    ZINT(1),IWX1,TWET8(1))
        IWX       = IWX1
        ISNO      = MOD(IWX,2)
        IIP       = MOD(IWX,4)/2
        IZR       = MOD(IWX,8)/4
        IRAIN     = IWX/8
        SNOW(1)   = ISNO*1.0
        SLEET(1)  = IIP*1.0
        FREEZR(1) = IZR*1.0
        RAIN(1)   = IRAIN*1.0
!        print *, 'inside calprecip after calwxt iwx =',iwx
!     DOMINANT PRECIPITATION TYPE
!GSM  IF DOMINANT PRECIP TYPE IS REQUESTED, 4 MORE ALGORITHMS
!GSM    WILL BE CALLED.  THE TALLIES ARE THEN SUMMED IN
!GSM    CALWXT_DOMINANT


!     write(0,*)' i=',i,' lm=',lm,' lp1=',lp1,' T=',T(1),q(1),pmid(1) &
!    &,' pint=',pint(1),' prec=',prec(i),' pthresh=',pthresh

        CALL CALWXT_RAMER(lm,lp1,T8(1),Q(1),PMID(1),RH(1),TD8(1), &
	                  PINT(1),PREC(i),PTHRESH,IWX2)
!
        IWX       = NINT(IWX2)
        ISNO      = MOD(IWX,2)
        IIP       = MOD(IWX,4)/2
        IZR       = MOD(IWX,8)/4
        IRAIN     = IWX/8
        SNOW(2)   = ISNO*1.0
        SLEET(2)  = IIP*1.0
        FREEZR(2) = IZR*1.0
        RAIN(2)   = IRAIN*1.0
!        print *, 'inside calprecip after ramer iwx=',iwx
! BOURGOUIN ALGORITHM
        CALL CALWXT_BOURG(LM,LP1,randomno(i,1),con_g,PTHRESH,                  &
     &                    T8(1),Q(1),PMID(1),PINT(1),PREC(i),ZINT(1),IWX3)

!
        IWX       = NINT(IWX3)
        ISNO      = MOD(IWX,2)
        IIP       = MOD(IWX,4)/2
        IZR       = MOD(IWX,8)/4
        IRAIN     = IWX/8
        SNOW(3)   = ISNO*1.0
        SLEET(3)  = IIP*1.0
        FREEZR(3) = IZR*1.0
        RAIN(3)   = IRAIN*1.0
!        print *, 'inside calprecip after bourg iwx=',iwx

! REVISED NCEP ALGORITHM
        CALL CALWXT_REVISED(LM,LP1,T8(1),Q(1),PMID(1),PINT(1),PREC(i),PTHRESH,  &
                            con_fvirt,con_rog,con_epsq,ZINT(1),TWET8(1),IWX4)

!
        IWX       = IWX4
        ISNO      = MOD(IWX,2)
        IIP       = MOD(IWX,4)/2
        IZR       = MOD(IWX,8)/4
        IRAIN     = IWX/8
        SNOW(4)   = ISNO*1.0
        SLEET(4)  = IIP*1.0
        FREEZR(4) = IZR*1.0
        RAIN(4)   = IRAIN*1.0
!        print *, 'inside calprecip after revised iwx=',iwx 
! EXPLICIT ALGORITHM (UNDER 18 NOT ADMITTED WITHOUT PARENT 
!     OR GUARDIAN)
 
        IF(n3dfercld == 3) then ! Ferrier's scheme
          CALL CALWXT_EXPLICIT(LM,PTHRESH,                           &
                               TSKIN(i),PREC(i),SR(i),F_RimeF(1),IWX5)
        else
          IWX5 = 0
        endif
!
        IWX       = IWX5
        ISNO      = MOD(IWX,2)
        IIP       = MOD(IWX,4)/2
        IZR       = MOD(IWX,8)/4
        IRAIN     = IWX/8
        SNOW(5)   = ISNO*1.0
        SLEET(5)  = IIP*1.0
        FREEZR(5) = IZR*1.0
        RAIN(5)   = IRAIN*1.0
!               
        CALL CALWXT_DOMINANT(NALG,PREC(i),PTHRESH,RAIN(1),FREEZR(1),SLEET(1), &
                            SNOW(1),DOMR(i),DOMZR(i),DOMIP(i),DOMS(i))

!        if (DOMS(i).eq.1.) then
!          print *, 'Found SNOW at xlat/xlon',xlat,xlon
!        elseif (DOMR(i).eq.1.) then
!          print *, 'Found RAIN at xlat/xlon',xlat,xlon
!        elseif(DOMZR(i).eq.1.) then
!          print *, 'Found FREEZING RAIN at xlat/xlon',xlat,xlon
!        elseif(DOMIP(i).eq.1.) then
!          print *, 'Found ICE at xlat/xlon',xlat,xlon
!        endif
!        print *, 'In calpre DOMS,DOMR,DOMZR,DOMIP =', int(DOMS),int(DOMR),int(DOMZR),int(DOMIP)

      enddo ! end loop for i

      DEALLOCATE (TWET8,RH,TD8)        
      RETURN
      END
!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!
       SUBROUTINE CALWXT(lm,lp1,T,Q,PMID,PINT,PREC,  &
                 PTHRESH,D608,ROG,EPSQ,    &
		 ZINT,IWX,TWET)
! 
!     FILE: CALWXT.f
!     WRITTEN: 11 NOVEMBER 1993, MICHAEL BALDWIN
!     REVISIONS:
!               30 SEPT 1994-SETUP NEW DECISION TREE (M BALDWIN)
!               12 JUNE 1998-CONVERSION TO 2-D (T BLACK)
!     01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!     02-01-15  MIKE BALDWIN - WRF VERSION
!                              
!
!     ROUTINE TO COMPUTE PRECIPITATION TYPE USING A DECISION TREE
!     APPROACH THAT USES VARIABLES SUCH AS INTEGRATED WET BULB TEMP
!     BELOW FREEZING AND LOWEST LAYER TEMPERATURE
!
!     SEE BALDWIN AND CONTORNO PREPRINT FROM 13TH WEATHER ANALYSIS
!     AND FORECASTING CONFERENCE FOR MORE DETAILS
!     (OR BALDWIN ET AL, 10TH NWP CONFERENCE PREPRINT)
! 
!      use params_mod
!      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!    INPUT:
!      T,Q,PMID,HTM,LMH,PREC,ZINT
!
      integer,intent(in):: lm,lp1
!     real,intent(in):: pthresh
      real,dimension(LM),intent(in) :: Q,PMID
      real*8,dimension(LM),intent(in) :: T,TWET
      real,dimension(LP1),intent(in) :: ZINT,PINT
      integer,intent(out)  :: IWX
      real,intent(in) :: PREC,PTHRESH,D608,ROG,EPSQ
!     real,intent(out)  :: ZWET


!    OUTPUT:
!      IWX - INSTANTANEOUS WEATHER TYPE.
!        ACTS LIKE A 4 BIT BINARY
!          1111 = RAIN/FREEZING RAIN/ICE PELLETS/SNOW
!          WHERE THE ONE'S DIGIT IS FOR SNOW
!                THE TWO'S DIGIT IS FOR ICE PELLETS
!                THE FOUR'S DIGIT IS FOR FREEZING RAIN
!            AND THE EIGHT'S DIGIT IS FOR RAIN
!
!    INTERNAL:
!
!      REAL, ALLOCATABLE :: TWET(:)
      real, parameter :: D00=0.0 
      integer KARR,LICEE
      real TCOLD,TWARM

!    SUBROUTINES CALLED:
!     WETBULB
!     
!
!     INITIALIZE WEATHER TYPE ARRAY TO ZERO (IE, OFF).
!     WE DO THIS SINCE WE WANT IWX TO REPRESENT THE
!     INSTANTANEOUS WEATHER TYPE ON RETURN.
!     
!
!     ALLOCATE LOCAL STORAGE
!

      integer L,LICE,IWRML,IFRZL
      real    PSFCK,TDCHK,A,TDKL,TDPRE,TLMHK,TWRMK,AREAS8,AREAP4,       &
              SURFW,SURFC,DZKL,AREA1,PINTK1,PINTK2,PM150,PKL,TKL,QKL

!      ALLOCATE ( TWET(LM) )
!
!!$omp  parallel do
      IWX = 0
!      ZWET=SPVAL
!
!!$omp  parallel do
!!$omp& private(a,pkl,psfck,qkl,tdchk,tdkl,tdpre,tkl)

!
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
!
      IF (PREC.LE.PTHRESH) GOTO 800
!
!   FIND COLDEST AND WARMEST TEMPS IN SATURATED LAYER BETWEEN
!   70 MB ABOVE GROUND AND 500 MB
!   ALSO FIND HIGHEST SATURATED LAYER IN THAT RANGE
!
!meb
      PSFCK=PINT(LM+1)
!meb
      TDCHK=2.0
  760 TCOLD=T(LM)
      TWARM=T(LM)
      LICEE=LM
!
      DO 775 L=1,LM
        QKL=Q(L)
        QKL=MAX(EPSQ,QKL)
        TKL=T(L)
        PKL=PMID(L)
!
!   SKIP PAST THIS IF THE LAYER IS NOT BETWEEN 70 MB ABOVE GROUND
!       AND 500 MB
!
        IF (PKL.LT.50000.0.OR.PKL.GT.PSFCK-7000.0) GOTO 775
        A=LOG(QKL*PKL/(6.1078*(0.378*QKL+0.622)))
        TDKL=(237.3*A)/(17.269-A)+273.15
        TDPRE=TKL-TDKL
        IF (TDPRE.LT.TDCHK.AND.TKL.LT.TCOLD) TCOLD=TKL
        IF (TDPRE.LT.TDCHK.AND.TKL.GT.TWARM) TWARM=TKL
        IF (TDPRE.LT.TDCHK.AND.L.LT.LICEE) LICEE=L
  775 CONTINUE
!
!    IF NO SAT LAYER AT DEW POINT DEP=TDCHK, INCREASE TDCHK
!     AND START AGAIN (BUT DON'T MAKE TDCHK > 6)
!
      IF (TCOLD==T(LM).AND.TDCHK<6.0) THEN
        TDCHK=TDCHK+2.0
        GOTO 760
      ENDIF
  800 CONTINUE
!
!    LOWEST LAYER T
!
      KARR=0
      IF (PREC.LE.PTHRESH) GOTO 850
      TLMHK=T(LM)
!
!    DECISION TREE TIME
!
      IF (TCOLD>269.15) THEN
          IF (TLMHK.LE.273.15) THEN
!             TURN ON THE FLAG FOR
!             FREEZING RAIN = 4
!             IF ITS NOT ON ALREADY
!             IZR=MOD(IWX(I,J),8)/4
!             IF (IZR.LT.1) IWX(I,J)=IWX(I,J)+4
            IWX=IWX+4
            GOTO 850
          ELSE
!             TURN ON THE FLAG FOR
!             RAIN = 8
!             IF ITS NOT ON ALREADY
!             IRAIN=IWX(I,J)/8
!             IF (IRAIN.LT.1) IWX(I,J)=IWX(I,J)+8
            IWX=IWX+8
            GOTO 850
          ENDIF
      ENDIF
      KARR=1
  850 CONTINUE
!
!   COMPUTE WET BULB ONLY AT POINTS THAT NEED IT
!
!      CALL WETBULB(lm,T,Q,PMID,KARR,TWET)
!      CALL WETFRZLVL(TWET,ZWET)
!
!!$omp  parallel do
!!$omp& private(area1,areap4,areas8,dzkl,ifrzl,iwrml,lice,
!!$omp&         lmhk,pintk1,pintk2,pm150,psfck,surfc,surfw,
!!$omp&         tlmhk,twrmk)

      IF(KARR.GT.0)THEN
        LICE=LICEE
!meb
        PSFCK=PINT(LM+1)
!meb
        TLMHK=T(LM)
        TWRMK=TWARM
!
!    TWET AREA VARIABLES
!     CALCULATE ONLY WHAT IS NEEDED
!      FROM GROUND TO 150 MB ABOVE SURFACE
!      FROM GROUND TO TCOLD LAYER
!      AND FROM GROUND TO 1ST LAYER WHERE WET BULB T < 0.0
!
!     PINTK1 IS THE PRESSURE AT THE BOTTOM OF THE LAYER
!     PINTK2 IS THE PRESSURE AT THE TOP OF THE LAYER
!
!     AREAP4 IS THE AREA OF TWET ABOVE -4 C BELOW HIGHEST SAT LYR 
!
        AREAS8=D00
        AREAP4=D00
        SURFW =D00
        SURFC =D00
!
        DO 1945 L=LM,LICE,-1
        DZKL=ZINT(L)-ZINT(L+1)
        AREA1=(TWET(L)-269.15)*DZKL
        IF (TWET(L).GE.269.15) AREAP4=AREAP4+AREA1
 1945   CONTINUE
!
        IF (AREAP4.LT.3000.0) THEN
!             TURN ON THE FLAG FOR
!             SNOW = 1
!             IF ITS NOT ON ALREADY
!             ISNO=MOD(IWX(I,J),2)
!             IF (ISNO.LT.1) IWX(I,J)=IWX(I,J)+1
          IWX=IWX+1
          GO TO 1900
        ENDIF
!
!     AREAS8 IS THE NET AREA OF TWET W.R.T. FREEZING IN LOWEST 150MB
!
        PINTK1=PSFCK
        PM150=PSFCK-15000.
!
        DO 1955 L=LM,1,-1
        PINTK2=PINT(L)
        IF(PINTK1.LT.PM150)GO TO 1950
        DZKL=ZINT(L)-ZINT(L+1)
!
!    SUM PARTIAL LAYER IF IN 150 MB AGL LAYER
!
        IF(PINTK2.LT.PM150)                                      &
          DZKL=T(L)*(Q(L)*D608+1.0)*ROG*LOG(PINTK1/PM150)
        AREA1=(TWET(L)-273.15)*DZKL
        AREAS8=AREAS8+AREA1
 1950   PINTK1=PINTK2
 1955   CONTINUE
!
!     SURFW IS THE AREA OF TWET ABOVE FREEZING BETWEEN THE GROUND
!       AND THE FIRST LAYER ABOVE GROUND BELOW FREEZING
!     SURFC IS THE AREA OF TWET BELOW FREEZING BETWEEN THE GROUND
!       AND THE WARMEST SAT LAYER
!
        IFRZL=0
        IWRML=0
!
        DO 2050 L=LM,1,-1
        IF (IFRZL.EQ.0.AND.T(L).LT.273.15) IFRZL=1
        IF (IWRML.EQ.0.AND.T(L).GE.TWRMK) IWRML=1
!
        IF (IWRML.EQ.0.OR.IFRZL.EQ.0) THEN
!	  if(pmid(l) < 50000.)print*,'need twet above 500mb'
          DZKL=ZINT(L)-ZINT(L+1)
          AREA1=(TWET(L)-273.15)*DZKL
          IF(IFRZL.EQ.0.AND.TWET(L).GE.273.15)SURFW=SURFW+AREA1
          IF(IWRML.EQ.0.AND.TWET(L).LE.273.15)SURFC=SURFC+AREA1
        ENDIF
 2050   CONTINUE
        IF(SURFC.LT.-3000.0.OR.   &
          (AREAS8.LT.-3000.0.AND.SURFW.LT.50.0)) THEN
!             TURN ON THE FLAG FOR
!             ICE PELLETS = 2
!             IF ITS NOT ON ALREADY
!             IIP=MOD(IWX(I,J),4)/2
!             IF (IIP.LT.1) IWX(I,J)=IWX(I,J)+2
          IWX=IWX+2
          GOTO 1900
        ENDIF
!
        IF(TLMHK.LT.273.15) THEN
!             TURN ON THE FLAG FOR
!             FREEZING RAIN = 4
!             IF ITS NOT ON ALREADY
!             IZR=MOD(IWX(K),8)/4
!             IF (IZR.LT.1) IWX(K)=IWX(K)+4
          IWX=IWX+4
        ELSE
!             TURN ON THE FLAG FOR
!             RAIN = 8
!             IF ITS NOT ON ALREADY
!             IRAIN=IWX(K)/8
!             IF (IRAIN.LT.1) IWX(K)=IWX(K)+8
          IWX=IWX+8
        ENDIF
      ENDIF
 1900 CONTINUE
!---------------------------------------------------------
!      DEALLOCATE (TWET)

      RETURN
      END
!
!
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
      SUBROUTINE CALWXT_RAMER(lm,lp1,      &
                          T,Q,PMID,RH,TD,PINT,PREC,PTHRESH,PTYP)

!      SUBROUTINE dophase(pq,   !  input pressure sounding mb
!     +    t,   !  input temperature sounding K
!     +    pmid,   !  input pressure
!     +    pint,   !  input interface pressure
!     +    q,   !  input spec humidityfraction
!     +    lmh,   !  input number of levels in sounding
!     +    prec,      ! input amount of precipitation
!     +    ptyp) !  output(2) phase 2=Rain, 3=Frzg, 4=Solid,
!                                               6=IP     JC  9/16/99
!      use params_mod
!      use CTLBLK_mod 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      real,PARAMETER :: twice=266.55,rhprcp=0.80,deltag=1.02,             &
     &                  emelt=0.045,rlim=0.04,slim=0.85
      real,PARAMETER :: twmelt=273.15,tz=273.15,efac=1.0 ! specify in params now 
!
      INTEGER*4 i, k1, lll, k2, toodry
!
      REAL xxx ,mye, icefrac
      integer,intent(in) :: lm,lp1
      real,DIMENSION(LM),intent(in) :: Q,PMID,RH
      real*8,DIMENSION(LM),intent(in) :: T,TD
      real,DIMENSION(LP1),intent(in) :: PINT
      real,intent(in) :: PREC,PTHRESH
      real,intent(out) :: PTYP
!
      real,DIMENSION(LM) :: TQ,PQ,RHQ
      real,DIMENSION(LM) :: TWQ
!
      integer J,L,LEV,ii
      real    RHMAX,TWMAX,PTOP,dpdrh,twtop,rhtop,wgt1,wgt2,    &
              rhavg,dtavg,dpk,ptw,pbot
!     real b,qtmp,rate,qc
      real,external :: xmytw
!
!  Initialize.
      icefrac = -9999.
!

      PTYP = 0
      DO L = 1,LM
        LEV = LP1 - L
!        P(L)=PMID(L)
!        QC=PQ0/P(L) * EXP(A2*(T(L)-A3)/(T(L)-A4))
!GSM forcing Q (QTMP) to be positive to deal with negative Q values
!       causing problems later in this subroutine
!        QTMP=MAX(H1M12,Q(L))	
!        RHQTMP(LEV)=QTMP/QC
	RHQ(LEV) = RH(L)
        PQ(LEV)  = PMID(L) * 0.01
        TQ(LEV)  = T(L)
      enddo


!
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
!
      IF (PREC <= PTHRESH) return

!
!CC   RATE RESTRICTION REMOVED BY JOHN CORTINAS 3/16/99
!
!     Construct wet-bulb sounding, locate generating level.
      twmax = -999.0
      rhmax = 0.0
      k1 = 0    !  top of precip generating layer
      k2 = 0    !  layer of maximum rh
!
      IF (rhq(1) < rhprcp) THEN
        toodry = 1
      ELSE
        toodry = 0
      END IF
!
      pbot = pq(1)
!      NQ=LM
      DO L = 1, lm
!       xxx = tdofesat(esat(tq(L))*rhq(L))
        xxx = td(l)            !HC: use TD consistent with GFS ice physics
        if (xxx < -500.) return
        twq(L) = xmytw(tq(L),xxx,pq(L))
        twmax = max(twq(L),twmax)
        IF (pq(L) >= 400.0) THEN
          IF (rhq(L) > rhmax) THEN
            rhmax = rhq(L)
            k2    = L
          END IF
!
          IF (L /= 1) THEN
            IF (rhq(L) >= rhprcp .or. toodry == 0) THEN
              IF (toodry /= 0) THEN
                 dpdrh = log(pq(L)/pq(L-1)) / (rhq(L)-RHQ(L-1))
                 pbot  = exp(log(pq(L))+(rhprcp-rhq(L))*dpdrh)
!
                 ptw = pq(L)
                 toodry = 0
              ELSE IF (rhq(L)>= rhprcp) THEN
                 ptw = pq(L)
              ELSE
                 toodry = 1
                 dpdrh  = log(pq(L)/pq(L-1)) / (rhq(L)-rhq(L-1))
                   ptw  = exp(log(pq(L))+(rhprcp-rhq(L))*dpdrh)

!lin             dpdrh  = (Pq(i)-Pq(i-1))/(Rhq(i)-Rhq(i-1))
!lin             ptw    = Pq(i)+(rhprcp-Rhq(i))*dpdrh
!
              END IF
!
              IF (pbot/ptw >= deltag) THEN
!lin            If (pbot-ptw.lt.deltag) Goto 2003
                k1   = L
                ptop = ptw
              END IF
            END IF
          END IF
        END IF
      enddo
!
!     Gross checks for liquid and solid precip which dont require generating level.
!
      IF (twq(1) >= 273.15+2.0) THEN
         ptyp    = 8   ! liquid
         icefrac = 0.0
         return
      END IF
!
      IF (twmax <= twice) THEN
         icefrac = 1.0
         ptyp    = 1   !  solid
         return
      END IF
!
!     Check to see if we had no success with locating a generating level.
!
      IF (k1 == 0) return
!
      IF (ptop == pq(k1)) THEN
        twtop = twq(k1)
        rhtop = rhq(k1)
        k2    = k1
        k1    = k1 - 1
      ELSE
        k2    = k1
        k1    = k1 - 1
        wgt1  = log(ptop/pq(k2)) / log(pq(k1)/pq(k2))
        wgt2  = 1.0 - wgt1
        twtop = twq(k1) * wgt1 + twq(k2) * wgt2
        rhtop = rhq(k1) * wgt1 + rhq(k2) * wgt2
      END IF
!
!     Calculate temp and wet-bulb ranges below precip generating level.
      DO L = 1, k1
        twmax = max(twq(l),twmax)
      enddo
!
!     Gross check for solid precip, initialize ice fraction.
!     IF (i.eq.1.and.j.eq.1) WRITE (*,*) 'twmax=',twmax,twice,'twtop=',twtop

      IF (twtop <= twice) THEN
        icefrac = 1.0
        IF (twmax <= twmelt) THEN    ! gross check for solid precip.
           ptyp = 1                  ! solid precip
           return
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
      IF (icefrac >= 1.0) THEN  !  starting as all ice
        IF (twq(k1) < twmelt) GO TO 40       ! cannot commence melting
        IF (twq(k1) == twtop) GO TO 40        ! both equal twmelt, nothing h
        wgt1  = (twmelt-twq(k1)) / (twtop-twq(k1))
        rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) * 0.5
        dtavg = (twmelt-twq(k1)) * 0.5
        dpk   = wgt1 * log(pq(k1)/ptop)        !lin   dpk=wgt1*(Pq(k1)-Ptop)
!       mye=emelt*(1.0-(1.0-Rhavg)*efac)
        mye = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
      ELSE IF (icefrac <= 0.0) THEN     !  starting as all liquid
        lll = 1
!       Goto 1020
        IF (twq(k1) > twice) GO TO 40        ! cannot commence freezing
        IF (twq(k1) == twtop) THEN
            wgt1 = 0.5
        ELSE
            wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
        END IF
        rhavg   = rhq(k1) + wgt1 * (rhtop-rhq(k1)) * 0.5
        dtavg   = twmelt - (twq(k1)+twice) * 0.5
        dpk     = wgt1 * log(pq(k1)/ptop)      !lin  dpk=wgt1*(Pq(k1)-Ptop)
!       mye     = emelt*(1.0-(1.0-Rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
      ELSE IF ((twq(k1) <= twmelt).and.(twq(k1) < twmelt)) THEN ! mix
        rhavg   = (rhq(k1)+rhtop) * 0.5
        dtavg   = twmelt - (twq(k1)+twtop) * 0.5
        dpk     = log(pq(k1)/ptop)       !lin   dpk=Pq(k1)-Ptop
!       mye     = emelt*(1.0-(1.0-Rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye           
      ELSE                 ! mix where Tw curve crosses twmelt in layer
        IF (twq(k1) == twtop) GO TO 40   ! both equal twmelt, nothing h
        wgt1    = (twmelt-twq(k1)) / (twtop-twq(k1))
        wgt2    = 1.0 - wgt1
        rhavg   = rhtop + wgt2 * (rhq(k1)-rhtop) * 0.5
        dtavg   = (twmelt-twtop) * 0.5
        dpk     = wgt2 * log(pq(k1)/ptop)     !lin   dpk=wgt2*(Pq(k1)-Ptop)
!       mye     = emelt*(1.0-(1.0-Rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
        icefrac = min(1.0,max(icefrac,0.0))   
        IF (icefrac <= 0.0) THEN
!           Goto 1020
            IF (twq(k1) > twice) GO TO 40    ! cannot commence freezin
            wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
            dtavg = twmelt - (twq(k1)+twice) * 0.5
        ELSE
            dtavg = (twmelt-twq(k1)) * 0.5
        END IF
        rhavg   = rhq(k1) + wgt1 * (rhtop-rhq(k1)) * 0.5
        dpk     = wgt1 * log(pq(k1)/ptop)     !lin  dpk=wgt1*(Pq(k1)-Ptop)
!       mye     = emelt*(1.0-(1.0-Rhavg)*efac)
        mye     = emelt * rhavg ** efac
        icefrac = icefrac + dpk * dtavg / mye
      END IF
!
      icefrac = min(1.0,max(icefrac,0.0))

!     IF (i.eq.1.and.j.eq.1) WRITE (*,*) 'NEW ICEFRAC:', icefrac, icefrac
!
!     Get next level down if there is one, loop back.
   40 continue
      IF (k1 > 1) THEN
        twtop = twq(k1)
        ptop  = pq(k1)
        rhtop = rhq(k1)
        k1    = k1 - 1
        GO TO 30
      END IF
!
!     Determine precip type based on snow fraction and surface wet-bulb.
!
      IF (icefrac >= slim) THEN
        IF (lll /= 0) THEN
          ptyp = 2       ! Ice Pellets   JC 9/16/99
        ELSE
          ptyp = 1       !  Snow
        END IF
      ELSE IF (icefrac <= rlim) THEN
        IF (twq(1).lt.tz) THEN
          ptyp = 4       !  Freezing Precip
        ELSE
          ptyp = 8       !  Rain
        END IF
      ELSE
        IF (twq(1) < tz) THEN
!GSM not sure what to do when 'mix' is predicted;   In previous
!GSM   versions of this code for which I had to have an answer,
!GSM   I chose sleet.  Here, though, since we have 4 other
!GSM   algorithms to provide an answer, I will not declare a
!GSM   type from the Ramer in this situation and allow the
!GSM   other algorithms to make the call.
      
           ptyp = 0       !  don't know 
!          ptyp = 5       !  Mix
        ELSE
!          ptyp = 5       !  Mix
           ptyp = 0       !  don't know 
        END IF
      END IF

      RETURN
!
      END
!
!
!--------------------------------------------------------------------------
!      REAL*4 FUNCTION mytw(t,td,p)
      FUNCTION xmytw(t,td,p)
!
      IMPLICIT NONE
!
      INTEGER*4 cflag, l
!     REAL*4 f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,        &
      REAL   f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,        &
     &          de, xmytw
      DATA f, c0, c1, c2 /0.0006355, 26.66082, 0.0091379024, 6106.3960/
!
!
      xmytw = (t+td) / 2
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
          xmytw = kw - 273.15
      ELSE
          xmytw = kw
      END IF
!
      RETURN
      END
!
!
!$$$  Subprogram documentation block
!
! Subprogram: calwxt_bourg    Calculate precipitation type (Bourgouin)
!   Prgmmr: Baldwin      Org: np22        Date: 1999-07-06
!
! Abstract: This routine computes precipitation type
!    using a decision tree approach that uses the so-called
!    "energy method" of Bourgouin of AES (Canada) 1992
!
! Program history log:
!   1999-07-06  M Baldwin
!   1999-09-20  M Baldwin  make more consistent with bourgouin (1992)
!   2005-08-24  G Manikin  added to wrf post
!   2007-06-19  M Iredell  mersenne twister, best practices
!   2008-03-03  G Manikin  added checks to prevent stratospheric warming
!                           episodes from being seen as "warm" layers
!                           impacting precip type
!
! Usage:    call calwxt_bourg(im,jm,jsta_2l,jend_2u,jsta,jend,lm,lp1,   &
!    &                        iseed,g,pthresh,                          &
!    &                        t,q,pmid,pint,lmh,prec,zint,ptype)
!   Input argument list:
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
!     pthresh  real precipitation threshold (m)
!     t        real(im,jsta_2l:jend_2u,lm) mid layer temp (K)
!     q        real(im,jsta_2l:jend_2u,lm) specific humidity (kg/kg)
!     pmid     real(im,jsta_2l:jend_2u,lm) mid layer pressure (Pa)
!     pint     real(im,jsta_2l:jend_2u,lp1) interface pressure (Pa)
!     lmh      real(im,jsta_2l:jend_2u) max number of layers
!     prec     real(im,jsta_2l:jend_2u) precipitation (m)
!     zint     real(im,jsta_2l:jend_2u,lp1) interface height (m)
!   Output argument list:
!     ptype    real(im,jm) instantaneous weather type ()
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
! Modules used:
!   mersenne_twister pseudo-random number generator
!
! Subprograms called:
!   random_number    pseudo-random number generator
!
! Attributes:
!   Language: Fortran 90
!
! Remarks: vertical order of arrays must be layer   1 = top
!                                       and layer lmh = bottom
!
!$$$
      subroutine calwxt_bourg(lm,lp1,rn,g,pthresh,      &
     &                        t,q,pmid,pint,prec,zint,ptype)
!      use mersenne_twister
      implicit none
!
!    input:
      integer,intent(in):: lm,lp1
!      integer,intent(in):: iseed
      real,intent(in):: g,pthresh,rn
      real*8,intent(in):: t(lm)
      real,intent(in):: q(lm)
      real,intent(in):: pmid(lm)
      real,intent(in):: pint(lp1)
      real,intent(in):: prec
      real,intent(in):: zint(lp1)
!
!    output:
      real,intent(out):: ptype
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
!!$omp  parallel do
!!$omp& private(a,tlmhk,iwrml,psfck,lhiwrm,pintk1,pintk2,area1,
!!$omp&         areape,dzkl,surfw,r1,r2)

      psfck=pint(lm+1)
!
!   skip this point if no precip this time step 
!
      if (prec.le.pthresh) return
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
       SUBROUTINE CALWXT_REVISED(LM,LP1,T,Q,PMID,PINT,PREC,  &
                 PTHRESH,D608,ROG,EPSQ,    &
     &             ZINT,TWET,IWX)
! 
!     FILE: CALWXT.f
!     WRITTEN: 11 NOVEMBER 1993, MICHAEL BALDWIN
!     REVISIONS:
!               30 SEPT 1994-SETUP NEW DECISION TREE (M BALDWIN)
!               12 JUNE 1998-CONVERSION TO 2-D (T BLACK)
!     01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!     02-01-15  MIKE BALDWIN - WRF VERSION
!     05-07-07  BINBIN ZHOU  - ADD PREC FOR RSM
!     05-08-24  GEOFF MANIKIN - MODIFIED THE AREA REQUIREMENTS
!                TO MAKE AN ALTERNATE ALGORITHM 
!                              
!
!     ROUTINE TO COMPUTE PRECIPITATION TYPE USING A DECISION TREE
!     APPROACH THAT USES VARIABLES SUCH AS INTEGRATED WET BULB TEMP
!     BELOW FREEZING AND LOWEST LAYER TEMPERATURE
!
!     SEE BALDWIN AND CONTORNO PREPRINT FROM 13TH WEATHER ANALYSIS
!     AND FORECASTING CONFERENCE FOR MORE DETAILS
!     (OR BALDWIN ET AL, 10TH NWP CONFERENCE PREPRINT)
!
!     SINCE THE ORIGINAL VERSION OF THE ALGORITHM HAS A HIGH BIAS
!      FOR FREEZING RAIN AND SLEET, THE GOAL IS TO BALANCE THAT BIAS
!      WITH A VERSION MORE LIKELY TO PREDICT SNOW
!
!     use params_mod
!     use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!  LIST OF VARIABLES NEEDED
!    PARAMETERS:
!      D608,ROG,H1,D00
!HC       PARAMETER(D608=0.608,ROG=287.04/9.8,H1=1.0,D00=0.0)
!
!    INPUT:
!      T,Q,PMID,HTM,LMH,PREC,ZINT
      integer,intent(in):: lm,lp1
      REAL,dimension(LM),intent(in) ::  Q,PMID
      REAL*8,dimension(LM),intent(in) ::  T,TWET
      REAL,dimension(LP1),intent(in) ::  PINT,ZINT 
      REAL,intent(in) ::  PREC,PTHRESH,D608,ROG,EPSQ
!    OUTPUT:
!      IWX - INSTANTANEOUS WEATHER TYPE.
!        ACTS LIKE A 4 BIT BINARY
!          1111 = RAIN/FREEZING RAIN/ICE PELLETS/SNOW
!          WHERE THE ONE'S DIGIT IS FOR SNOW
!                THE TWO'S DIGIT IS FOR ICE PELLETS
!                THE FOUR'S DIGIT IS FOR FREEZING RAIN
!            AND THE EIGHT'S DIGIT IS FOR RAIN
      integer, intent(out) ::  IWX
!    INTERNAL:
!
      real, parameter :: D00=0.0  
      integer KARR,LICEE
      real TCOLD,TWARM
!
      integer L,LMHK,LICE,IWRML,IFRZL
      real PSFCK,TDCHK,A,TDKL,TDPRE,TLMHK,TWRMK,AREAS8,AREAP4,AREA1,  &
           SURFW,SURFC,DZKL,PINTK1,PINTK2,PM150,QKL,TKL,PKL,AREA0,    &
           AREAP0

!    SUBROUTINES CALLED:
!     WETBULB
!     
!
!     INITIALIZE WEATHER TYPE ARRAY TO ZERO (IE, OFF).
!     WE DO THIS SINCE WE WANT IWX TO REPRESENT THE
!     INSTANTANEOUS WEATHER TYPE ON RETURN.
!     
!
!     ALLOCATE LOCAL STORAGE
!
!
!!$omp  parallel do
      IWX = 0

!!$omp  parallel do
!!$omp& private(a,lmhk,pkl,psfck,qkl,tdchk,tdkl,tdpre,tkl)

      LMHK=LM
!
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
!
      IF (PREC.LE.PTHRESH) GOTO 800
!
!   FIND COLDEST AND WARMEST TEMPS IN SATURATED LAYER BETWEEN
!   70 MB ABOVE GROUND AND 500 MB
!   ALSO FIND HIGHEST SATURATED LAYER IN THAT RANGE
!
!meb
      PSFCK=PINT(LP1)
!meb
      TDCHK=2.0
  760 TCOLD=T(LMHK)
      TWARM=T(LMHK)
      LICEE=LMHK
!
      DO 775 L=1,LMHK
      QKL=Q(L)
      QKL=MAX(EPSQ,QKL)
      TKL=T(L)
      PKL=PMID(L)
!
!   SKIP PAST THIS IF THE LAYER IS NOT BETWEEN 70 MB ABOVE GROUND
!       AND 500 MB
!
      IF (PKL.LT.50000.0.OR.PKL.GT.PSFCK-7000.0) GOTO 775
      A=LOG(QKL*PKL/(6.1078*(0.378*QKL+0.622)))
      TDKL=(237.3*A)/(17.269-A)+273.15
      TDPRE=TKL-TDKL
      IF (TDPRE.LT.TDCHK.AND.TKL.LT.TCOLD) TCOLD=TKL
      IF (TDPRE.LT.TDCHK.AND.TKL.GT.TWARM) TWARM=TKL
      IF (TDPRE.LT.TDCHK.AND.L.LT.LICEE) LICEE=L
  775 CONTINUE
!
!    IF NO SAT LAYER AT DEW POINT DEP=TDCHK, INCREASE TDCHK
!     AND START AGAIN (BUT DON'T MAKE TDCHK > 6)
!
      IF (TCOLD.EQ.T(LMHK).AND.TDCHK.LT.6.0) THEN
        TDCHK=TDCHK+2.0
        GOTO 760
      ENDIF
  800 CONTINUE
!
!    LOWEST LAYER T
!
      KARR=0
      IF (PREC.LE.PTHRESH) GOTO 850
      LMHK=LM
      TLMHK=T(LMHK)
!
!    DECISION TREE TIME
!
      IF (TCOLD.GT.269.15) THEN
          IF (TLMHK.LE.273.15) THEN
!             TURN ON THE FLAG FOR
!             FREEZING RAIN = 4
!             IF ITS NOT ON ALREADY
!             IZR=MOD(IWX,8)/4
!             IF (IZR.LT.1) IWX=IWX+4
              IWX=IWX+4
            GOTO 850
          ELSE
!             TURN ON THE FLAG FOR
!             RAIN = 8
!             IF ITS NOT ON ALREADY
!             IRAIN=IWX/8
!             IF (IRAIN.LT.1) IWX=IWX+8
              IWX=IWX+8
            GOTO 850
          ENDIF
      ENDIF
      KARR=1
  850 CONTINUE
!
!!$omp  parallel do
!!$omp& private(area1,areap4,areap0,areas8,dzkl,ifrzl,iwrml,lice,
!!$omp&         lmhk,pintk1,pintk2,pm150,psfck,surfc,surfw,
!!$omp&         tlmhk,twrmk)

      IF(KARR.GT.0)THEN
        LMHK=LM
        LICE=LICEE
!meb
        PSFCK=PINT(LP1)
!meb
        TLMHK=T(LMHK)
        TWRMK=TWARM
!
!    TWET AREA VARIABLES
!     CALCULATE ONLY WHAT IS NEEDED
!      FROM GROUND TO 150 MB ABOVE SURFACE
!      FROM GROUND TO TCOLD LAYER
!      AND FROM GROUND TO 1ST LAYER WHERE WET BULB T < 0.0
!
!     PINTK1 IS THE PRESSURE AT THE BOTTOM OF THE LAYER
!     PINTK2 IS THE PRESSURE AT THE TOP OF THE LAYER
!
!     AREAP4 IS THE AREA OF TWET ABOVE -4 C BELOW HIGHEST SAT LYR 
!     AREAP0 IS THE AREA OF TWET ABOVE 0 C BELOW HIGHEST SAT LYR
!
        AREAS8=D00
        AREAP4=D00
	AREAP0=D00
        SURFW =D00
        SURFC =D00
        
!
        DO 1945 L=LMHK,LICE,-1
        DZKL=ZINT(L)-ZINT(L+1)
        AREA1=(TWET(L)-269.15)*DZKL
        AREA0=(TWET(L)-273.15)*DZKL
        IF (TWET(L).GE.269.15) AREAP4=AREAP4+AREA1
        IF (TWET(L).GE.273.15) AREAP0=AREAP0+AREA0
 1945   CONTINUE
!
!        IF (AREAP4.LT.3000.0) THEN
!             TURN ON THE FLAG FOR
!             SNOW = 1
!             IF ITS NOT ON ALREADY
!             ISNO=MOD(IWX,2)
!             IF (ISNO.LT.1) IWX=IWX+1
!          IWX=IWX+1
!          GO TO 1900
!        ENDIF
        IF (AREAP0.LT.350.0) THEN
!             TURN ON THE FLAG FOR
!             SNOW = 1
              IWX=IWX+1
            GOTO 1900
       ENDIF
!
!     AREAS8 IS THE NET AREA OF TWET W.R.T. FREEZING IN LOWEST 150MB
!
        PINTK1=PSFCK
        PM150=PSFCK-15000.
!
        DO 1955 L=LMHK,1,-1
        PINTK2=PINT(L)
        IF(PINTK1.LT.PM150)GO TO 1950
        DZKL=ZINT(L)-ZINT(L+1)
!
!    SUM PARTIAL LAYER IF IN 150 MB AGL LAYER
!
        IF(PINTK2.LT.PM150)                                   &
          DZKL=T(L)*(Q(L)*D608+1.0)*ROG*               &
               LOG(PINTK1/PM150)
        AREA1=(TWET(L)-273.15)*DZKL
        AREAS8=AREAS8+AREA1
 1950   PINTK1=PINTK2
 1955   CONTINUE
!
!     SURFW IS THE AREA OF TWET ABOVE FREEZING BETWEEN THE GROUND
!       AND THE FIRST LAYER ABOVE GROUND BELOW FREEZING
!     SURFC IS THE AREA OF TWET BELOW FREEZING BETWEEN THE GROUND
!       AND THE WARMEST SAT LAYER
!
        IFRZL=0
        IWRML=0
!
        DO 2050 L=LMHK,1,-1
        IF (IFRZL.EQ.0.AND.T(L).LT.273.15) IFRZL=1
        IF (IWRML.EQ.0.AND.T(L).GE.TWRMK) IWRML=1
!
        IF (IWRML.EQ.0.OR.IFRZL.EQ.0) THEN
!	  if(pmid(l) .lt. 50000.)print*,'twet needed above 500mb'
          DZKL=ZINT(L)-ZINT(L+1)
          AREA1=(TWET(L)-273.15)*DZKL
          IF(IFRZL.EQ.0.AND.TWET(L).GE.273.15)SURFW=SURFW+AREA1
          IF(IWRML.EQ.0.AND.TWET(L).LE.273.15)SURFC=SURFC+AREA1
        ENDIF
 2050   CONTINUE
        IF(SURFC.LT.-3000.0.OR.                                    &
     &    (AREAS8.LT.-3000.0.AND.SURFW.LT.50.0)) THEN
!             TURN ON THE FLAG FOR
!             ICE PELLETS = 2
!             IF ITS NOT ON ALREADY
!             IIP=MOD(IWX,4)/2
!             IF (IIP.LT.1) IWX=IWX+2
          IWX=IWX+2
          GOTO 1900
        ENDIF
!
        IF(TLMHK.LT.273.15) THEN
!             TURN ON THE FLAG FOR
!             FREEZING RAIN = 4
!             IF ITS NOT ON ALREADY
!             IZR=MOD(IWX(K),8)/4
!             IF (IZR.LT.1) IWX(K)=IWX(K)+4
          IWX=IWX+4
        ELSE
!             TURN ON THE FLAG FOR
!             RAIN = 8
!             IF ITS NOT ON ALREADY
!             IRAIN=IWX(K)/8
!             IF (IRAIN.LT.1) IWX(K)=IWX(K)+8
          IWX=IWX+8
        ENDIF
      ENDIF
 1900 CONTINUE
!      print *, 'revised check ', IWX(500,800)

      RETURN
      END
!
!
      SUBROUTINE CALWXT_EXPLICIT(LM,PTHRESH,TSKIN,PREC,SR,F_RIMEF,IWX)
! 
!     FILE: CALWXT.f
!     WRITTEN: 24 AUGUST 2005, G MANIKIN and B FERRIER 
!
!     ROUTINE TO COMPUTE PRECIPITATION TYPE USING EXPLICIT FIELDS
!       FROM THE MODEL MICROPHYSICS

!      use params_mod
!      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!  LIST OF VARIABLES NEEDED
!    PARAMETERS:
!
!    INPUT:
      integer, intent(in):: lm
      real,intent(in)::  TSKIN, PREC, SR,PTHRESH
      REAL,intent(in):: F_RimeF(LM)
      integer,intent(out) :: IWX
      real SNOW
!     real PSFC
!
!     ALLOCATE LOCAL STORAGE
!
!!$omp  parallel do
      IWX = 0

!GSM  THE RSM IS CURRENTLY INCOMPATIBLE WITH THIS ROUTINE
!GSM   ACCORDING TO B FERRIER, THERE MAY BE A WAY TO WRITE
!GSM   A VERSION OF THIS ALGORITHM TO WORK WITH THE RSM
!GSM   MICROPHYSICS, BUT IT DOESN'T EXIST AT THIS TIME
!!$omp  parallel do
!!$omp& private(psfc,tskin)

!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
!
      IF (PREC.LE.PTHRESH) GOTO 800
!
!  A SNOW RATIO LESS THAN 0.5 ELIMINATES SNOW AND SLEET
!   USE THE SKIN TEMPERATURE TO DISTINGUISH RAIN FROM FREEZING RAIN
!   NOTE THAT 2-M TEMPERATURE MAY BE A BETTER CHOICE IF THE MODEL
!   HAS A COLD BIAS FOR SKIN TEMPERATURE
! 
      IF (SR.LT.0.5) THEN
!        SURFACE (SKIN) POTENTIAL TEMPERATURE AND TEMPERATURE.
!         PSFC=PMID(LM)
!         TSKIN=THS*(PSFC/P1000)**CAPA 

         IF (TSKIN.LT.273.15) THEN
!          FREEZING RAIN = 4
           IWX=IWX+4
         ELSE
!          RAIN = 8
           IWX=IWX+8
         ENDIF
      ELSE
!  
!  DISTINGUISH SNOW FROM SLEET WITH THE RIME FACTOR
! 
        IF(F_RimeF(LM).GE.10) THEN
!          SLEET = 2
           IWX=IWX+2
        ELSE
           SNOW = 1
           IWX=IWX+1 
        ENDIF
      ENDIF
 800  CONTINUE
 810  RETURN 
      END
!
!
       SUBROUTINE CALWXT_DOMINANT(NALG,PREC,PTHRESH,RAIN,FREEZR,SLEET,SNOW,     &
     &         DOMR,DOMZR,DOMIP,DOMS)
!
!     WRITTEN: 24 AUGUST 2005, G MANIKIN 
!      
!     THIS ROUTINE TAKES THE PRECIP TYPE SOLUTIONS FROM DIFFERENT
!       ALGORITHMS AND SUMS THEM UP TO GIVE A DOMINANT TYPE
!
!      use params_mod
!      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!    INPUT:
      integer,intent(in) :: NALG
      REAL, intent(in) :: PREC,PTHRESH
      real,intent(out) ::  DOMS,DOMR,DOMZR,DOMIP
      real,DIMENSION(NALG),intent(in) ::  RAIN,SNOW,SLEET,FREEZR
      integer L
      real TOTSN,TOTIP,TOTR,TOTZR
!--------------------------------------------------------------------------
!      write(6,*) 'into dominant'
!!$omp  parallel do
      DOMR = 0.
      DOMS = 0.
      DOMZR = 0.
      DOMIP = 0.
!
!!$omp  parallel do
!!$omp& private(totsn,totip,totr,totzr)
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
      IF (PREC.LE.PTHRESH) GOTO 800
      TOTSN = 0.
      TOTIP = 0.
      TOTR  = 0.
      TOTZR = 0.
!   LOOP OVER THE NUMBER OF DIFFERENT ALGORITHMS THAT ARE USED
      DO 820 L = 1, NALG
        IF (RAIN(L).GT. 0) THEN
           TOTR = TOTR + 1
           GOTO 830
        ENDIF

        IF (SNOW(L).GT. 0) THEN
           TOTSN = TOTSN + 1
           GOTO 830
        ENDIF

        IF (SLEET(L).GT. 0) THEN
           TOTIP = TOTIP + 1
           GOTO 830
        ENDIF

        IF (FREEZR(L).GT. 0) THEN
           TOTZR = TOTZR + 1
           GOTO 830
        ENDIF
 830    CONTINUE
 820  CONTINUE
!       print *, 'Calprecip Total Rain, snow, sleet, freeze= ', &
!       TOTR,TOTSN,TOTIP,TOTZR

!   TIES ARE BROKEN TO FAVOR THE MOST DANGEROUS FORM OF PRECIP
!     FREEZING RAIN > SNOW > SLEET > RAIN 
      IF (TOTSN .GT. TOTIP) THEN
        IF (TOTSN .GT. TOTZR) THEN
          IF (TOTSN .GE. TOTR) THEN
           DOMS = 1
           GOTO 800 
          ELSE
           DOMR = 1 
           GOTO 800 
          ENDIF
        ELSE IF (TOTZR .GE. TOTR) THEN
          DOMZR = 1
          GOTO 800 
        ELSE
          DOMR = 1
          GOTO 800 
        ENDIF 
      ELSE IF (TOTIP .GT. TOTZR) THEN
        IF (TOTIP .GE. TOTR) THEN
          DOMIP = 1
          GOTO 800 
        ELSE
          DOMR = 1
          GOTO 800 
        ENDIF
      ELSE IF (TOTZR .GE. TOTR) THEN
         DOMZR = 1
         GOTO 800 
      ELSE
          DOMR = 1
          GOTO 800 
      ENDIF
 800  CONTINUE 
      RETURN
      END

      
            
      
      
