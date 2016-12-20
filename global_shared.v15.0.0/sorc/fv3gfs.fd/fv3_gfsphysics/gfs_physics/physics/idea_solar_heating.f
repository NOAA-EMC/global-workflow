      module idea_solar
!---------------------------------------------------------------------------
! hold effuv,effeuv ro (density (kg/m3)),nps (start pressure levels index)
! Apr 06 2012   Henry Juang, initial implement for nems
!---------------------------------------------------------------------------
      use machine, only : kind_phys
      implicit none
!hmhj save
      real (kind=kind_phys), allocatable :: effuv(:), effeuv(:)
      integer nps
      end module idea_solar
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine idea_solar_init(levs)
!----------------------------------------------------------------------------
! calculete effuv,effeuv,pr index to start solar heating calc 
!----------------------------------------------------------------------------
      use idea_composition,  pr=> pr_idea
      use physcons,  amo2=>con_amo2,avgd => con_avgd
      use machine, only : kind_phys
      use idea_solar
      implicit none
!
! define some constants
      integer, intent(in)  :: levs           !number of pressure level
!c    real effeuv17(17),effuv17(17),p17(17),z17(17),dz,noh(17),z(levs), &
!c   &nol(17),no17(17),f107
      real effeuv17(17),effuv17(17),p17(17),z17(17),dz,z(levs),f107
      integer k,i,kref
!
      allocate (effeuv(levs))
      allocate (effuv(levs))
!c    allocate (no_idea(levs))                 !no number density (/cm3)
!
!  ** EUV and UV heating efficiency on 17 pressure levels
      DATA EFFEUV17/8*1.0,.75,.6,.62,.54,.49,.41,.33,.30,.30/
      DATA EFFUV17/5*.28,.29,.32,.38,.4,.4,.4,.39,.34,.26,.19,.17,.16/
!c    data nol/11.88,11.88,11.68,11.83,12.02,12.19,12.38,12.54,12.51,   &
!c   &12.54,12.38,12.19,11.68,10.96,10.24,9.5,8.76/
!c    data noh/12.57,12.57,12.57,12.92,13.06,13.33,13.24,13.39,13.06,   &
!c   &13.02,12.65,12.33,11.61,10.96,10.21,9.47,8.73/
! find nps (2Pa)
      do k=1,levs
        if(pr(k).le..02) then
          nps=k
          go to 10
        endif
      enddo
   10 continue
! get 17 levels no at f107
      f107=f107_idea(1)
!c    dz=(f107-67.)/(243.-67.)
!c    do k=1,17
!c      no17(k)=dz*noh(k)+(1.-dz)*nol(k)
!c    enddo
! get effuv,effeuv from interplating effuv17, effeuv17 to 150 levs 
! get no from interplating no17 to 150 levs  
      do k=1,17
        p17(k)=5.2285*exp(1.-k)
        z17(k)=-1.*log(p17(k))
      enddo
      do k=1,levs
        z(k)=-1.*log(pr(k)*100.)
      enddo
      do k=1,levs
        kref=0
        do i=1,16
          if(z(k).ge.z17(i).and.z(k).le.z17(i+1)) then
            kref=i
            dz=(z(k)-z17(i))/(z17(i+1)-z17(i))
          endif
        enddo
        if(kref.ne.0) then
          effuv(k)=dz*effuv17(kref+1)+(1.-dz)*effuv17(kref)
          effeuv(k)=dz*effeuv17(kref+1)+(1.-dz)*effeuv17(kref)
!c        no_idea(k)=dz*no17(kref+1)+(1.-dz)*no17(kref)
        elseif(z(k).lt.z17(1)) then
          effuv(k)=effuv17(1)
          effeuv(k)=effeuv17(1)
!c        no_idea(k)=no17(1)
        elseif(z(k).gt.z17(17)) then
          effuv(k)=effuv17(17)
          effeuv(k)=effeuv17(17)
!c        no_idea(k)=no17(17)
        endif
      enddo
!     print*,'effuv'
!     print'(10f6.3)',effuv
!     print*,'effeuv'
!     print'(10f6.3)',effeuv
! result in /cm3
!c    no_idea=1.e-6*10.**(no_idea)
!
      return
      end subroutine
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine idea_sheat(im,ix,levs,te,dt,cospass,o_n,o2_n,n2_n,     &
     &ro,cp,lat,dayno,prsl,zg,grav,am,maglat,dt6dt)
!----------------------------------------------------------------------------
! calculete solar heating, NO coooling from 2Pa up
!----------------------------------------------------------------------------
!c    use idea_solar, no_n => no_idea
      use idea_solar
      use idea_composition,  pr=>pr_idea 
      use physcons, rgas=>con_rgas, amo2=>con_amo2,                     &
     &               avgd => con_avgd
      use machine, only : kind_phys
      implicit none
!
      integer, intent(in) :: im       !number of data piont in te
      integer, intent(in) :: ix       !maxmum data point in te
      integer, intent(in) :: levs     !number of press level
      integer, intent(in) :: lat      ! latitude index
      integer, intent(in) :: dayno    ! calender day
      real, intent(in) :: te(ix,levs) !temperature
      real, intent(in)    :: cospass(im)  ! cos zenith angle
      real, intent(in)    :: maglat(im)   ! 
      real, intent(in)    :: cp(ix,levs)  ! 
      real, intent(in)    :: o_n(ix,levs) !number density of O(/cm3) 
      real, intent(in)    :: o2_n(ix,levs)!number density of O2
      real, intent(in)    :: n2_n(ix,levs)!number density of N2
      real, intent(in)    :: am(ix,levs)  !mass of mix (kg)
      real, intent(in)    :: prsl(ix,levs)!layer press (Pa)
      real, intent(in)    :: zg(ix,levs)!layer height (m)
      real, intent(in)    :: grav(ix,levs)! (m/s2)
      real, intent(in)    :: ro(ix,levs)  ! density (kg/m3) 
      real, intent(inout) :: dt6dt(ix,levs,6)  ! 
      real, intent(out)    :: dt(ix,levs) ! (K/s)temp change due to solar heating
      integer  i,k
      real t(levs),n2(levs),no(levs),o(levs),o2(levs),ho(levs),         &
     &ho2(levs),hn2(levs),sheat(levs),qno(levs),f107,no_new(levs),      &
     &amm(levs),prr(levs),alt(levs),nn(levs),sh1(levs),sh2(levs)
!c        no=no_n*1.e6            
!     rtime1=3600.*6.
      f107=f107_idea(1)
      do i=1,im
        do k=1,levs
          o(k)=o_n(i,k)*1.e6      !/m3
          o2(k)=o2_n(i,k)*1.e6        
          n2(k)=n2_n(i,k)*1.e6        
        enddo
        do k=1,levs
          t(k)=te(i,k)
          ho(k)=1.e3*rgas*t(k)/(amo*grav(i,k))  !m
          ho2(k)=1.e3*rgas*t(k)/(amo2*grav(i,k))   
          hn2(k)=1.e3*rgas*t(k)/(amn2*grav(i,k))   
        enddo
! try Tim's data
!       call gettimdata(pr,n2,no,o,o2,ho,ho2,hn2,t,ro1)
!       call gettimdata17(n2,no,o,o2,ho,ho2,hn2,t,ro1)
!       call solar_heat(17,1,o,o2,n2,ho,ho2,hn2,effeuv17,effuv17,       &
!    &   f107,cospass,sheat)
!       call COOLNO1(17,1,t,o,no,qno)                   
! get heating
        call solar_heat(levs,nps,o,o2,n2,ho,ho2,hn2,effeuv,effuv,       &
     &   f107,cospass(i),sheat,sh1,sh2)
        do k=1,levs
!         alt(k)=phil(i,k)/g*1.e-3  !km
          alt(k)=zg(i,k)*1.e-3  !km
          prr(k)=prsl(i,k)
          nn(k)=o(k)+o2(k)+n2(k)
          amm(k)=am(i,k)*1.e3*avgd
        enddo
        call getno(1,1,levs,maglat(i),dayno,alt,prr,nn,amm,no_new)
! get no cooling
!c      call COOLNO1(levs,nps,t,o,no,qno)                   
        call COOLNO1(levs,nps,t,o,no_new,qno)                   
!     print*,'www2',no_new(104:123)
!c    print*,'www2-old',no(104:123)
!       do k=1,17
!         dt=(sheat(k)-qno(k))*dtp/(cp(k)*ro1(k))
!         te(i,k)=t(k)+dt*dtp
!     print'(i4,5f7.2,3e11.3)',k,log10(o(k)),log10(o2(k)),log10(n2(k)), &
!    &t(k),log10(no(k)),ro1(k),sheat(k)/ro1(k)*.55,qno(k)/ro1(k)
        do k=nps,levs
!         dt6dt(i,k,1)=qno(k)*rtime1/(cp(i,k)*ro(i,k))
          dt6dt(i,k,1)=qno(k)/(cp(i,k)*ro(i,k))
          dt6dt(i,k,3)=sh1(k)/(cp(i,k)*ro(i,k))
          dt6dt(i,k,4)=sh2(k)/(cp(i,k)*ro(i,k))
          dt(i,k)=(sheat(k)-qno(k))/(cp(i,k)*ro(i,k))
!         dt6dt(i,k,5)=(sheat(k)-qno(k))/(cp(i,k)*ro(i,k))
!         print*,'www2',k,sheat(k),qno(k),dt(i,k)
        enddo ! k
        do k=1,nps-1
          dt(i,k)=0.
          dt6dt(i,k,1)=0.
          dt6dt(i,k,3)=0.
          dt6dt(i,k,4)=0.
          dt6dt(i,k,5)=0.
        enddo ! k
!     print'(i4,5f7.2,3e11.3)',k,log10(o(150)),log10(o2(150)),          &
!    &log10(n2(150)),                                                   &
!    &t(150),log10(no(150)),ro(150,lan),sheat(150)/ro(150,lan)*.55,     &
!    &qno(150)/ro(150,lan)
      enddo !i
! print for pictures
!       if(lat.eq.47) then
!       do i=1,im
!       if(abs(cospass(i)-1.).le..01) then
!       do k=87,150
!     print'(i4,5f7.2,1e11.3)',k,log10(o(k)),log10(o2(k)),log10(n2(k)), &
!    &t(k),log10(no(k)),ro(k,lan)
!       enddo
!       endif
!       enddo
!       endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE solar_heat(np,nps,O,O2,N2,HO,HO2,HN2,effeuv,effuv,     &
     &F107,COSPASS,sheat,sh1,sh2)
!-------------------------------------------------------------------------
! calculate solar heating from Tim Fuller-Rowell
!-------------------------------------------------------------------------
!c  **
!c  calculates solar heating from EUV and SRC wavelengths
!c  assumes a heating efficiency profile on pressure levels
!c  code was written in SI units
!c  Input:
!c  O atomic oxygen number density profile m-3
!c  O2 molecular oxygen number density profile m-3
!c  N2 molecular nitrogen number density profile m-3
!c  HO atomic oxygen scale height profile m
!c  HO2 molecular oxygen scale height profile m
!c  HN2 molecular nitrogen scale height profile m
!c  solar flux F10.7
!c  COSPASS cosine of solar zenith angle
!c  Output:
!c  SHEAT heating rate profile J/m-3
!---------------------------------------------------------------------------
      implicit none
      integer, intent(in) :: np    ! number of pressure levels
      integer, intent(in) :: nps    !pressure index to start  
      real, intent(in)    :: o(np),o2(np),n2(np) ! number density/m3
      real, intent(in)    :: ho(np),ho2(np),hn2(np) ! scale height(m)
      real, intent(in)    :: effeuv(np),effuv(np) !heating efficiency 
      real, intent(in)    :: f107    !f10.7cm 
      real, intent(in)    :: cospass !cos zenith angle
      real, intent(out)   :: sheat(np),sh1(np),sh2(np)   !J/m3 heating rate
      real SO(np),SO2(np),SN2(np),                                      &
     &SFL(57),CSAO(57),CSAO2(57),CSAN2(57),CSIO(57),                    &
     &CSIO2(57),CSIN2(57),SFH(57),SF(57),PAEUV(np,65),SFUV(8),          &
     &A(8),UVXS(8),RLAM(65)
      real coschi,rnight,seco,seco2,secn2,wo,wo2,wn2,tau,tauo,tauo2,    &
     & taun2,pcc
      integer i,j,jj
!c  **
!c  number of pressure levels to process for solar heating
!c  pressure levels defined by pressure(n)=5.2285*exp(1-n)
!c  **
!c  wavelength/energy conversion SI E=hc/lamda
      PCC=1.985E-25
!c  **
!C  WAVELENGTHS Angstroms
!C  **
      DATA RLAM/18.6,19.0,21.6,21.8,22.1,28.5,28.8,29.5,30.0,           &
     &30.4,33.7,41.0,43.8,44.0,44.2,45.7,46.4,46.7,47.9,49.2,           &
     &75.,125.,175.,225.,256.3,284.15,275.,303.31,303.78,               &
     &325.,368.07,375.,425.,465.22,475.,525.,554.37,584.33,             &
     &575.,609.76,629.73,625.,675.,730.36,725.,765.15,770.41,           &
     &789.36,775.,825.,875.,925.,977.62,975.,1025.72,1031.91,           &
     &1025.,1387.5,1425.,1475.,1525.,1575.,1625.,1675.,1725./
!C  **
!C  REVISED FLUXES BY TORR AND TORR 85 JGR 90 6675
!C  WITH ADDITIONAL VALUES 1 TO 20 FOR WAVELENGTHS BELOW 50A.
!C  **
           DATA SFL/
     &.0001,.0001,.0003,.0001,.0003,.0005,.0025,.0022,.0012,            &
     &.0006,.0011,.0006,.0021,.0008,.0009,.0005,.0027,.0052,            &
     &.0059,.0043,                                                      &
     &.38,.13,1.84,.92,.27,.1,.84,.24,6.,.87,.74,.21,.39,.18,           &
     &.31,.51,.80,1.58,.48,.45,1.5,.17,.22,.39,.17,.2,.24,              &
     &.79,.87,1.93,4.43,4.22,5.96,1.79,4.38,3.18,3.64/
!c  **
           DATA SFH/                                                    &
     &.0016,.0053,.0048,.0016,.0048,.0072,.0211,.0186,.0024,            &
     &.0104,.0158,.0073,.0130,.0097,.0109,.0061,.0168,.0107,            &
     &.0121,.0267,                                                      &
     &1.37,.468,5.7,7.14,1.08,5.72,12.16,4.69,14.39,6.83,1.53,          &
     &2.54,1.53,.736,1.82,1.64,1.52,4.3,1.048,2.48,3.87,1.37,           &
     &.539,.746,.429,.439,1.19,1.514,2.454,4.85,12.219,9.85,            &
     &10.217,4.078,11.85,6.1,6.09/           
!c  **
!c  UV FLUX IN SRC AND O2 X-SECTIONS FROM M.R.TORR ET AL 
!c  JGR 1980 6063
!c  **
      DATA A/9.73,17.93,27.38,51.57,70.99,97.4,205.,374.24/
      DATA UVXS/1.2E-17,1.5E-17,1.3E-17,1.0E-17,6.0E-18,3.4E-18,        &
     &1.5E-18,5.0E-19/
!C  **
!C  REVISED VALUES FROM SAMSON AND PAREEK 85
!C  **
           DATA CSAO/                                                   &
     &.34,.36,.5,.51,.52,.05,.05,.06,.06,.06,.08,.13,.15,               &
     &.15,.16,.17,.18,.18,.19,.21,                                      &
     &0.7,1.7,3.0,5.1,6.2,7.3,7.0,7.7,7.7,8.5,10.,10.,                  &
     &11.21,11.25,11.64,11.91,12.13,12.17,11.9,12.23,12.22,             &
     &12.21,10.04,11.35,8.0,4.18,4.18,4.28,4.23,4.38,4.18,2.12,         &
     &0.,0.,0.,0.,0./
!c  **
           DATA CSAO2/                                                  &
     &.69,.72,.99,1.01,1.05,.10,.11,.11,.12,.12,.16,.26,.3,.31,         &
     &.31,.34,.35,.36,.38,.41,                                          &
     &1.18,3.61,7.27,10.5,12.8,14.8,13.65,15.98,16.,17.19,18.40,        &
     &18.17,19.39,20.4,21.59,24.06,25.59,22.0,25.04,26.1,25.8,          &
     &26.02,26.27,25.,29.05,21.98,25.18,26.66,27.09,20.87,9.85,         &
     &15.54,4.0,16.53,1.6,1.0,1.1/
!c  **
           DATA CSAN2/                                                  &
     &.44,.47,.65,.67,.69,1.13,1.13,1.12,1.11,1.10,.1,.16,.19,          &
     &.19,.19,.21,.22,.22,.24,.25,                                      &
     &.6,2.32,5.4,8.15,9.65,10.6,10.8,11.58,11.6,                       &
     &14.6,18.0,17.51,21.07,21.8,                                       &
     &21.85,24.53,24.69,23.2,22.38,23.1,23.2,23.22,29.75,26.3,          &
     &30.94,35.46,26.88,19.26,30.71,15.05,46.63,16.99,.7,               &
     &36.16,0.,0.,0./
!c  **
      COSCHI=COSPASS
      rnight=1.0
      if(coschi.lt.0.07)then
        coschi=1.0
        rnight=1.e-6
      end if
      do j=1,57
        SF(j)=1.e9*((SFH(j)-SFL(j))*F107/172.-0.413*SFH(j)+1.413*SFL(j))
        if(sf(j).lt.0.0)sf(j)=0.0
      enddo
      do  j=1,8
        SFUV(j)=A(j)*1.E9*(0.00086*F107+0.94)
      enddo
      do i=nps,np
!c  **
        SECO=1./COSCHI
        SECO2=SECO
        SECN2=SECO
        WO=O(i)*HO(i)*SECO*1.e-4
        WO2=O2(i)*HO2(i)*SECO2*1.e-4
        WN2=N2(i)*HN2(i)*SECN2*1.e-4
!c  **
!c  loop over all wavelengths bands
!c  **
        sheat(i)=0.0
        sh1(i)=0.
        sh2(i)=0.
        do j=1,57
          TAUO=CSAO(j)*WO*1.e-18
          TAUO2=CSAO2(j)*WO2*1.e-18
          TAUN2=CSAN2(j)*WN2*1.e-18
          TAU=TAUO+TAUO2+TAUN2
          PAEUV(i,j)=SF(j)*EXP(-TAU)*(CSAO(j)*O(i)+                     &
     &    CSAO2(j)*O2(i)+CSAN2(j)*N2(i))*PCC*rnight*1.e-8/RLAM(j)
          sheat(i)=sheat(i)+paeuv(i,j)*effeuv(i)
          sh1(i)=sh1(i)+paeuv(i,j)*effeuv(i)
        enddo
!c  **
!c  add SRC channels
!c  ** 
        do j=58,65
          JJ=j-57
          TAU=UVXS(JJ)*WO2
          PAEUV(i,j)=SFUV(JJ)*EXP(-TAU)*UVXS(JJ)*O2(i)*PCC*rnight       &
     &    /RLAM(j)*1.e10
          sheat(i)=sheat(i)+paeuv(i,j)*effuv(i)
          sh2(i)=sh2(i)+paeuv(i,j)*effuv(i)
        enddo
      enddo
      if(nps.ge.2) then
        do i=1,nps-1
          sheat(i)=0.
          sh1(i)=0.
          sh2(i)=0.
        enddo
      endif
      RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE COOLNO1(np,nps,T,O,NO,QNO)                   
!-------------------------------------------------------------------------
! calculate NO cooling from Tim Fuller-Rowell
!-------------------------------------------------------------------------
!c  **
!c  input:
!c  T temperature profile K
!c  O atomic oxygen number density profile m-3
!c  NO nitric oxide number density profile m-3
!c  output:
!c  QNO: NO cooling rate J/m-3
!c  **
      implicit none
      integer, intent(in):: np           !numer of pressure levels
      integer, intent(in):: nps          ! pressure index to start
      real, intent(in)   :: O(np),NO(np) !number density/m3        
      real, intent(in)   :: T(np)        !temp (K)        
      real, intent(out)  :: QNO(np)         
      real K10,HV,A10,BZ,A1,A2,A3,OM1,OM,G        
      integer i
      K10=3.6E-17                                                       
      HV=3.726E-20                                                      
      A10=13.3                                                          
      G=1.0                                                             
      BZ=1.38E-23                                                       
      A2=5.4E-6*(1./(EXP(HV/BZ/5800.)-1.))
      A3=0.5*EXP(-HV/BZ/247.5)
      do i=nps,np                                                   
        OM1=K10*O(i)          
        OM=OM1/(OM1+A10)
        A1=EXP(-HV/BZ/T(i)) 
        QNO(i)=HV*NO(i)*OM*A10*G*(A1-A2-A3)
      enddo
      if(nps.ge.2) then
        do i=1,nps-1                                                   
          QNO(i)=0.
        enddo
      endif
      return                                                            
      end                                                               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine gettimdata(pr,n2,no,o,o2,ho,ho2,hn2,t,ro)
!--------------------------------------------------------------------------
! this subrutine is for test solar heating code using Tim's 17 pressure 
! level data, interpolate to levs levels. All the argument are output except pr
!-------------------------------------------------------------------------`
      implicit none
      integer  np,i,levs,k,kref,np1
      real  r,amo,amn2,amo2,g,avgd,dz
      PARAMETER(np=17,r=8.314472,amo=15.9994e-3,amn2=28.013e-3,         &
     &amo2=31.9999e-3,g=9.80665,avgd=6.0221415e23,np1=150)
      REAL n217(np),no17(np),o17(np),o217(np),ho17(np),ho217(np),       &
     &hn217(np),p17(np),t17(np),ro17(np)
      REAL n2(np1),no(np1),o(np1),o2(np1),ho(np1),ho2(np1),hn2(np1),    &
     &pr(np1),aa(6,np),ax(4,np),t(np1),ro(np1)
      real z17(np),z(np1)
      data aa/15.70,  20.54,  21.14, 220.00,  70.00,  28.80,            &
     &   15.26,  20.14,  20.74, 203.46,  76.69,  28.80,                 &
     &   15.46,  19.72,  20.32, 198.00,  82.76,  28.81,                 &
     &   17.30,  19.32,  19.92, 181.44,  88.55,  28.78,                 &
     &   17.81,  18.90,  19.51, 169.96,  93.90,  28.59,                 &
     &   17.75,  18.41,  19.05, 179.45,  99.22,  28.25,                 &
     &   17.51,  17.85,  18.52, 216.21, 105.33,  27.75,                 &
     &   17.09,  17.21,  17.92, 310.67, 113.57,  27.25,                 &
     &   16.76,  16.25,  17.25, 500.30, 126.79,  25.56,                 &
     &   16.34,  15.65,  16.60, 701.86, 147.91,  24.31,                 &
     &   15.93,  14.77,  15.97, 936.50, 178.44,  22.62,                 &
     &   15.53,  14.24,  15.32,1117.86, 219.99,  20.91,                 &
     &   15.15,  13.47,  14.68,1208.59, 271.13,  19.19,                 &
     &   14.77,  12.72,  13.99,1240.81, 329.35,  17.83,                 &
     &   14.37,  11.92,  13.28,1250.56, 392.23,  16.96,                 &
     &   13.95,  11.14,  12.56,1253.07, 457.87,  16.50,                 &
     &   13.52,  10.35,  11.84,1253.07, 524.92,  16.25/
      data ax/ 3.28,  10.56,  13.40,  13.45,                            &
     & 3.28,  10.56,  13.45,  13.45,                                    &
     & 4.33,  10.67,  13.55,  13.55,                                    &
     & 5.31,  10.73,  13.93,  13.93,                                    &
     & 6.81,  10.93,  14.05,  14.06,                                    &
     & 8.29,  11.36,  14.32,  14.32,                                    &
     & 9.12,  11.73,  14.16,  14.17,                                    &
     & 9.79,  12.17,  13.81,  13.82,                                    &
     &10.28,  12.53,  13.23,  13.31,                                    &
     &10.63,  12.64,  13.01,  13.17,                                    &
     &11.55,  13.20,  12.62,  13.30,                                    &
     &11.88,  13.28,  12.30,  13.34,                                    &
     &11.91,  13.16,  11.59,  13.19,                                    &
     &11.58,  12.78,  10.93,  12.82,                                    &
     &11.23,  12.43,  10.19,  12.46,                                    &
     &10.86,  12.06,   9.44,  12.09,                                    &
     &10.49,  11.69,   8.71,  11.72/
      levs=np1
c
      do k=1,np
        p17(k)=5.2285*exp(1.-k)
        o17(k)=10.**(aa(1,k))  !/m3
        o217(k)=10.**(aa(2,k))  !/m3
        n217(k)=10.**(aa(3,k))  !/m3
        t17(k)=aa(4,k) !K
        ho17(k)=r*t17(k)/amo/g  !m
        ho217(k)=r*t17(k)/amo2/g !m
        hn217(k)=r*t17(k)/amn2/g !m
        ro17(k)=(o17(k)*amo+o217(k)*amo2+n217(k)*amn2)/avgd   !kg/m3
! for interp
        o17(k)=aa(1,k)  !/m3
        o217(k)=aa(2,k)  !/m3
        n217(k)=aa(3,k)  !/m3
        no17(k)=ax(3,k)  !/m3
      enddo
!     print*,t17
! interp
      do k=1,np
        p17(k)=5.2285*exp(1.-k)
        z17(k)=-1.*log(p17(k))
      enddo
      do k=1,levs
        z(k)=-1.*log(pr(k)*100.)
      enddo
      do k=1,levs
        kref=0
        do i=1,16
          if(z(k).ge.z17(i).and.z(k).le.z17(i+1)) then
            kref=i
            dz=(z(k)-z17(i))/(z17(i+1)-z17(i))
          endif
        enddo
!       print*,k,kref,dz
        if(kref.ne.0) then
          no(k)=dz*no17(kref+1)+(1.-dz)*no17(kref)
          o(k)=dz*o17(kref+1)+(1.-dz)*o17(kref)
          n2(k)=dz*n217(kref+1)+(1.-dz)*n217(kref)
          o2(k)=dz*o217(kref+1)+(1.-dz)*o217(kref)
          ho(k)=dz*ho17(kref+1)+(1.-dz)*ho17(kref)
          ho2(k)=dz*ho217(kref+1)+(1.-dz)*ho217(kref)
          hn2(k)=dz*hn217(kref+1)+(1.-dz)*hn217(kref)
          t(k)=dz*t17(kref+1)+(1.-dz)*t17(kref)
          ro(k)=dz*ro17(kref+1)+(1.-dz)*ro17(kref)
        elseif(z(k).lt.z17(1)) then
          no(k)=no17(1)
          o(k)=o17(1)
          n2(k)=n217(1)
          o2(k)=o217(1)
          ho(k)=ho17(1)
          ho2(k)=ho217(1)
          hn2(k)=hn217(1)
          ro(k)=ro17(1)
          t(k)=t17(1)
        elseif(z(k).gt.z17(17)) then
          no(k)=no17(17)
          o(k)=o17(17)
          n2(k)=n217(17)
          o2(k)=o217(17)
          ho(k)=ho17(17)
          ho2(k)=ho217(17)
          hn2(k)=hn217(17)
          ro(k)=ro17(17)
          t(k)=t17(17)
        endif
        o(k)=10.**o(k)  !/m3
        o2(k)=10.**o2(k)  !/m3
        no(k)=10.**no(k)  !/m3
        n2(k)=10.**n2(k)  !/m3
      enddo
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine gettimdata17(n217,no17,o17,o217,ho17,ho217,hn217,      &
     &t17,ro17)
!--------------------------------------------------------------------------
! this subrutine is for test solar heating code using Tim's 17 pressure 
! level data, All the argument are output
!-------------------------------------------------------------------------`
      implicit none
      integer  np,i,levs,k
      real  r,amo,amn2,amo2,g,avgd
      PARAMETER(np=17,r=8.314472,amo=15.9994e-3,amn2=28.013e-3,         &
     &amo2=31.9999e-3,g=9.80665,avgd=6.0221415e23)
      REAL n217(np),no17(np),o17(np),o217(np),ho17(np),ho217(np),       &
     &hn217(np),p17(np),t17(np),ro17(np)
      REAL aa(6,np),ax(4,np)
      data aa/15.70,  20.54,  21.14, 220.00,  70.00,  28.80,            &
     &   15.26,  20.14,  20.74, 203.46,  76.69,  28.80,                 &
     &   15.46,  19.72,  20.32, 198.00,  82.76,  28.81,                 &
     &   17.30,  19.32,  19.92, 181.44,  88.55,  28.78,                 &
     &   17.81,  18.90,  19.51, 169.96,  93.90,  28.59,                 &
     &   17.75,  18.41,  19.05, 179.45,  99.22,  28.25,                 &
     &   17.51,  17.85,  18.52, 216.21, 105.33,  27.75,                 &
     &   17.09,  17.21,  17.92, 310.67, 113.57,  27.25,                 &
     &   16.76,  16.25,  17.25, 500.30, 126.79,  25.56,                 &
     &   16.34,  15.65,  16.60, 701.86, 147.91,  24.31,                 &
     &   15.93,  14.77,  15.97, 936.50, 178.44,  22.62,                 &
     &   15.53,  14.24,  15.32,1117.86, 219.99,  20.91,                 &
     &   15.15,  13.47,  14.68,1208.59, 271.13,  19.19,                 &
     &   14.77,  12.72,  13.99,1240.81, 329.35,  17.83,                 &
     &   14.37,  11.92,  13.28,1250.56, 392.23,  16.96,                 &
     &   13.95,  11.14,  12.56,1253.07, 457.87,  16.50,                 &
     &   13.52,  10.35,  11.84,1253.07, 524.92,  16.25/
      data ax/ 3.28,  10.56,  13.40,  13.45,                            &
     & 3.28,  10.56,  13.45,  13.45,                                    &
     & 4.33,  10.67,  13.55,  13.55,                                    &
     & 5.31,  10.73,  13.93,  13.93,                                    &
     & 6.81,  10.93,  14.05,  14.06,                                    &
     & 8.29,  11.36,  14.32,  14.32,                                    &
     & 9.12,  11.73,  14.16,  14.17,                                    &
     & 9.79,  12.17,  13.81,  13.82,                                    &
     &10.28,  12.53,  13.23,  13.31,                                    &
     &10.63,  12.64,  13.01,  13.17,                                    &
     &11.55,  13.20,  12.62,  13.30,                                    &
     &11.88,  13.28,  12.30,  13.34,                                    &
     &11.91,  13.16,  11.59,  13.19,                                    &
     &11.58,  12.78,  10.93,  12.82,                                    &
     &11.23,  12.43,  10.19,  12.46,                                    &
     &10.86,  12.06,   9.44,  12.09,                                    &
     &10.49,  11.69,   8.71,  11.72/
      levs=np
c
      do k=1,np
        o17(k)=10.**(aa(1,k))  !/m3
        o217(k)=10.**(aa(2,k))  !/m3
        n217(k)=10.**(aa(3,k))  !/m3
        no17(k)=10.**(ax(3,k))  !/m3
        t17(k)=aa(4,k) !K
        ho17(k)=r*t17(k)/amo/g  !m
        ho217(k)=r*t17(k)/amo2/g !m
        hn217(k)=r*t17(k)/amn2/g !m
        ro17(k)=(o17(k)*amo+o217(k)*amo2+n217(k)*amn2)/avgd   !kg/m3
! for interp
      enddo
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE presolar(IM,IX,SOLHR,SLAG,                             &
     &                   SINLAT,COSLAT,SDEC,CDEC,xlon,xlat              &
     &                   ,XMU,dayno,utsec,sda                           &
     &                   ,maglat,maglon,btot,dipang,essa)
!------------------------------------------------------------------------
! calculate solar zenith angle
!------------------------------------------------------------------------
      USE MACHINE     , ONLY : kind_phys
      USE PHYSCONS, PI => con_PI
      use date_def
      implicit none
!Argument
! input
      integer              IM,IX
      real(kind=kind_phys) sdec,slag,solhr,cdec
      real(kind=kind_phys) SINLAT(ix),COSLAT(ix),XLON(ix),xlat(ix)
! output
      real     XMU(ix)       !cos solar zenith angle
! Output Magnetic and electric parameters 
!     REAL, INTENT(OUT) :: elx(im)
!     REAL, INTENT(OUT) :: ely(im)     !electric field
      REAL, INTENT(OUT) :: maglon(im)  !magnetic longitude (rad)
      REAL, INTENT(OUT) :: maglat(im)  !magnetic latitude (rad)
      REAL, INTENT(OUT) :: btot(im)    !mapgnetic field strength
      REAL, INTENT(OUT) :: dipang(im)  !Dip angle (degree)
      REAL, INTENT(OUT) :: essa(im)    !magnetic local time
      REAL, INTENT(OUT) :: sda         ! solar declination angle (rad)
! Output time parameters 
      REAL, INTENT(OUT) :: utsec       !universal time
      INTEGER, INTENT(OUT) :: dayno    !calendar day
! local vareable
      INTEGER   i,idat(8),jdat(8),jdow,jday 
      real(kind=kind_phys) cns,ss,cc,ch,rinc(5),ty
!  COMPUTE COSINE OF SOLAR ZENITH ANGLE FOR BOTH HEMISPHERES.
      CNS = PI*(SOLHR-12.)/12.+SLAG
      DO I=1,IM
        SS     = SINLAT(I) * SDEC
        CC     = COSLAT(I) * CDEC
        CH     = CC * COS(XLON(I)+CNS)
        XMU(I) = CH + SS
      ENDDO
! get day number year number UTsec
      idat=0
      idat(1)=idate(4)
      idat(2)=idate(2)
      idat(3)=idate(3)
      idat(5)=idate(1)
      rinc=0.
      rinc(2)=fhour
      call w3movdat(rinc,idat,jdat)
      call w3doxdat(jdat,jdow,dayno,jday)
!     print*,'www',dayno,fhour
      utsec=solhr*3600.
! get solar declination angle
      ty = (dayno+15.5)*12./365.
      IF ( ty > 12.0 ) ty = ty - 12.0
      sda = ATAN(0.434*SIN(PI/6.0*(ty-3.17)))
!     sda = asin(sdec)
!     print*,'www8',sda,asin(sdec)
! get maglat maglon
      call getmag(ix,im,utsec,xlat,xlon,sda,                            &
     &btot,dipang,maglon,maglat,essa)
      btot=btot*1.e-9
      RETURN
      END
      subroutine getno(im,ix,levs,mlat,doy,alt,pr,n,am,no)
      use idea_composition 
      implicit none
!Argument 
      integer, intent(in)  :: im           !number of Mag latitude 
      integer, intent(in)  :: ix           !number of forst dimension 
      integer, intent(in)  :: levs         !number of pressure level
      integer, intent(in)  :: doy          ! calenderday   
      real,    intent(in)  :: mlat(im)     ! magnetic latitude in degree
      real,    intent(in)  :: alt(ix,levs) !in km
      real,    intent(in)  :: pr(levs)     !in Pa
      real,    intent(in)  :: am(ix,levs)  !avg mass g/mol
      real,    intent(in)  :: n(ix,levs)   !/m3 number density
      real,    intent(out)  :: no(ix,levs) ! number density of NO (/m3) 
! local
      real eof(33,16,3),nom(33,16),z16(16),dx(levs),kp,f107,            &
     &lat33(33),dz(levs),dl(im),m1,m2,m3,theta0,dec,zm(16)
      integer iref(im),kref(levs),i,k,il,k1,k2
c
      data eof/-5247514.,-6069866.,-7046389.,-7712040.,-7475652.,       &
     &-6521992.,-5779488.,-4680550.,-3503355.,-2386292.,-1809640.,      &
     &-1232984.,-802252.5,-372340.8,-233625.,-247101.9,-152984.2,       &
     &-378208.4,-602054.8,-704482.2,-1130871.,-1553946.,-2005179.,      &
     & -2550362.,-3406166.,-3918725.,-4902982.,-5478586.,-5895833.,     &
     & -5706694.,-5069246.,-3955380.,-2953330.,                         &
     &-6038898.,-7092100.,-8222954.,-8946294.,-8772684.,-7650816.,      &
     & -6768808.,-5482080.,-4198284.,-3027250.,-2116341.,-1498316.,     &
     &-920285.5,-590980.9,-373686.9,-190410.6,-239284.8,-381577.1,      &
     &-654327.9,-843799.7,-1368418.,-1729119.,-2461879.,-3166525.,      &
     &-4184515.,-4789708.,-5817856.,-6588944.,-7023782.,-6818994.,      &
     &-5932706.,-4507758.,-3687829.,                                    &
     &-6969458.,-8250042.,-9574323.,-1.051643e+07,-1.013241e+07,        &
     &-9114658.,-7905622.,-6488483.,-4945043.,-3636914.,-2527723.,      &
     &-1808088.,-1109162.,-633211.2,-415173.8,-253677.3,-307635.4,      &
     &-407807.,-769923.1,-1152213.,-1609747.,-2170163.,-2871240.,       &
     &-3794409.,-4839386.,-5881801.,-7136790.,-8115624.,-8560489.,      &
     &-8109927.,-6876457.,-5306579.,-4396376.,                          &
     &-8046132.,-9649696.,-1.09464e+07,-1.204336e+07,-1.146817e+07,     &
     &-1.089248e+07,-9494700.,-7821164.,-6030920.,-4454593.,-3061204.,  &
     &-2223948.,-1433917.,-740196.4,-599572.5,-432156.3,-419470.9,      &
     &-706885.4,-1032811.,-1439622.,-1897172.,-2741821.,-3532401.,      &
     &-4442855.,-5662857.,-7226030.,-8847662.,-1.005568e+07,            &
     &-1.036662e+07,-9516017.,-8073862.,-5997690.,-5221294.,            &
     &-9129519.,-1.115334e+07,-1.229467e+07,-1.377534e+07,-1.336406e+07,&
     &-1.261571e+07,-1.127514e+07,-9378255.,-7344430.,-5450040.,        &
     &-3878616.,-2748751.,-1786833.,-1141964.,-984780.8,-706063.4,      &
     &-649638.8,-1016723.,-1419590.,-1880392.,-2193050.,-3247191.,      &
     &-4214684.,-5243782.,-6833144.,-8950396.,-1.102497e+07,            &
     &-1.23908e+07,-1.257e+07,-1.140126e+07,-9301236.,-7032514.,        &
     &-5679984.,                                                        &
     &-1.056073e+07,-1.226184e+07,-1.409111e+07,-1.60509e+07,           &
     &-1.593818e+07,-1.478319e+07,-1.342692e+07,-1.138179e+07,          &
     &-9208966.,-6722660.,-5084906.,-3512189.,-2327560.,-1806689.,      &
     &-1514532.,-1080780.,-1051349.,-1523702.,-1805517.,-2314509.,      &
     &-2736008.,-3959913.,-4995312.,-6450705.,-8425629.,-1.123888e+07,  &
     &-1.374101e+07,-1.551322e+07,-1.528153e+07,-1.362144e+07,          &
     &-1.089468e+07,-8475252.,-5989985.,                                &
     &-1.151357e+07,-1.344059e+07,-1.628318e+07,-1.831117e+07,          &
     &-1.868885e+07,-1.793377e+07,-1.627429e+07,-1.407746e+07,          &
     &-1.163246e+07,-8556876.,-6473252.,-4540662.,-3254795.,-2627365.,  &
     &-2067724.,-1621486.,-1696266.,-2003412.,-2413986.,-2967150.,      &
     &-3663475.,-4878293.,-5978525.,-7894496.,-1.060695e+07,            &
     &-1.412827e+07,-1.709277e+07,-1.938935e+07,-1.894935e+07,          &
     &-1.645988e+07,-1.301559e+07,-1.007491e+07,-7017845.,              &
     &-1.278202e+07,-1.546777e+07,-1.839902e+07,-2.126996e+07,          &
     &-2.224764e+07,-2.164401e+07,-1.993985e+07,-1.737438e+07,          &
     &-1.438734e+07,-1.110813e+07,-8382776.,-6098025.,-4643484.,        &
     &-3396795.,-2781976.,-2367481.,-2464792.,-2681164.,-3189622.,      &
     &-3833930.,-4773418.,-5781984.,-7389096.,-9414154.,-1.31758e+07,   &
     &-1.75727e+07,-2.155447e+07,-2.432159e+07,-2.384924e+07,           &
     &-2.059898e+07,-1.591331e+07,-1.187653e+07,-8291959.,              &
     &-1.499052e+07,-1.808507e+07,-2.113212e+07,-2.543446e+07,          &
     &-2.703178e+07,-2.664454e+07,-2.451988e+07,-2.133364e+07,          &
     &-1.783693e+07,-1.410975e+07,-1.077553e+07,-8007652.,-6111540.,    &
     &-4467203.,-3706028.,-3252944.,-3303344.,-3827547.,-4401341.,      &
     &-5043524.,-5920190.,-7222662.,-9096529.,-1.179622e+07,            &
     &-1.603885e+07,-2.141876e+07,-2.670152e+07,-3.057304e+07,          &
     &-3.058411e+07,-2.663012e+07,-2.033983e+07,-1.459393e+07,          &
     &-1.001286e+07,                                                    &
     &-1.831681e+07,-2.22191e+07,-2.55585e+07,-3.140621e+07,            &
     &-3.361916e+07,-3.326336e+07,-2.997734e+07,-2.614696e+07,          &
     &-2.194762e+07,-1.702339e+07,-1.318224e+07,-9849552.,-7534498.,    &
     &-6071376.,-4926836.,-4352171.,-4336826.,-4878423.,-5463988.,      &
     &-6115170.,-7130386.,-8790494.,-1.102214e+07,-1.490303e+07,        &
     &-1.958257e+07,-2.590418e+07,-3.235639e+07,-3.777672e+07,          &
     &-3.93543e+07,-3.492081e+07,-2.707286e+07,-1.944128e+07,           &
     &-1.316116e+07,                                                    &
     &-2.377451e+07,-2.792029e+07,-3.290989e+07,-4.000966e+07,          &
     &-4.212796e+07,-4.055546e+07,-3.617584e+07,-3.089867e+07,          &
     &-2.566711e+07,-1.954078e+07,-1.52248e+07,-1.143733e+07,           &
     &-8857369.,-7301910.,-6248646.,-5475254.,-5320822.,-5696930.,      &
     &-6025682.,-6856452.,-8183936.,-1.02042e+07,-1.324781e+07,         &
     &-1.754751e+07,-2.334451e+07,-3.096561e+07,-3.909159e+07,          &
     &-4.662714e+07,-5.03075e+07,-4.601564e+07,-3.606484e+07,           &
     &-2.686934e+07,-1.765236e+07,                                      &
     &-3.055708e+07,-3.602473e+07,-4.312628e+07,-5.083733e+07,          &
     &-5.134407e+07,-4.783135e+07,-4.119072e+07,-3.408647e+07,          &
     &-2.759005e+07,-2.074799e+07,-1.588136e+07,-1.217633e+07,          &
     &-9702639.,-7796016.,-6927890.,-6371548.,-6159546.,-6192992.,      &
     &-6571300.,-7418758.,-8830681.,-1.126385e+07,-1.447512e+07,        &
     &-1.933939e+07,-2.584862e+07,-3.476215e+07,-4.531472e+07,          &
     &-5.626805e+07,-6.28497e+07,-5.956211e+07,-4.689528e+07,           &
     &-3.554812e+07,-2.294755e+07,                                      &
     &-3.615576e+07,-4.475599e+07,-5.378362e+07,-6.034626e+07,          &
     &-5.831976e+07,-5.200245e+07,-4.278119e+07,-3.373483e+07,          &
     &-2.638541e+07,-1.963828e+07,-1.494567e+07,-1.179412e+07,          &
     &-9506393.,-7834650.,-7171875.,-6708864.,-6231308,-6242322.,       &
     &-6637734.,-7218400.,-8757761.,-1.104336e+07,-1.398759e+07,        &
     &-1.897072e+07,-2.570359e+07,-3.506066e+07,-4.73507e+07,           &
     &-6.203831e+07,-7.339174e+07,-7.23998e+07,-5.747045e+07,           &
     &-4.238988e+07,-2.744211e+07,                                      &
     &-3.871022e+07,-4.920625e+07,-5.955764e+07,-6.360899e+07,          &
     &-5.876267e+07,-4.942194e+07,-3.87213e+07,-2.908438e+07,           &
     &-2.138209e+07,-1.59806e+07,-1.242386e+07,-1.012207e+07,           &
     &-8467976,-7318800,-6853774,-6213230,-5668098.,-5685148.,          &
     &-5958562.,-6409051.,-7596764.,-9116276.,-1.175655e+07,            &
     &-1.581505e+07,-2.220477e+07,-3.103819e+07,-4.345313e+07,          &
     &-5.977948e+07,-7.641549e+07,-7.934251e+07,-6.347728e+07,          &
     &-4.462917e+07,-2.834482e+07,                                      &
     &-3.57445e+07,-4.646349e+07,-5.553925e+07,-5.707475e+07,           &
     &-5.03307e+07,-3.960313e+07,-2.911814e+07,-2.095385e+07,           &
     &-1.41673e+07,-1.090228e+07,-8578298.,-7370734.,-6619473.,         &
     &-6290864.,-5831174.,-5703259.,-5150547.,-4824520.,-4816210.,      &
     &-5154248.,-5639925.,-6234228.,-7960974.,-1.115471e+07,            &
     &-1.671155e+07,-2.400684e+07,-3.496165e+07,-5.034799e+07,          &
     &-6.865742e+07,-7.517185e+07,-6.079964e+07,-4.097551e+07,          &
     &-2.525763e+07,                                                    &
     &-2.709748e+07,-3.74319e+07,-4.330479e+07,-4.227697e+07,           &
     &-3.572979e+07,-2.630141e+07,-1.828288e+07,-1.227928e+07,          &
     &-8122793.,-6214670.,-4957134.,-4658456.,-4642014.,-4874148.,      &
     &-4937673.,-4905970.,-4485041.,-3991234.,-3730586.,-3562796.,      &
     &-3596555.,-3772070.,-4680470.,-6938800.,-1.118435e+07,            &
     &-1.607187e+07,-2.44831e+07,-3.691874e+07,-5.237388e+07,           &
     &-5.850094e+07,-4.780516e+07,-3.156013e+07,-1.88358e+07,           &
!    &-5.850094e+07,-4.780516e+07,-3.156013e+07,-1.88358e+07/
!     data eof2/-3550722.,-4999868.,-6029446.,-6607816.,-6798872.,      &
     &-3550722.,-4999868.,-6029446.,-6607816.,-6798872.,                &
     &-6556119.,                                                        &
     &-5851844.,-5105981.,-4227900.,-3332964.,-2709887.,-2012516.,      &
     &-1347839.,-858926.1,-398978.9,285539.2,1116062.,2062128.,         &
     &2772248.,3378420.,3903072.,4770542.,5352842.,5933570.,6471496.,   &
     &6704952.,6349468.,5615903.,4622920.,3660178.,2579021.,1603797.,   &
     &1167462.,                                                         &
     &-4453830.,-5830580.,-6664141.,-7325691.,-7336244.,-7197354.,      &
     &-6489768.,-5646758.,-4684139.,-3745124.,-3111786.,-2403234.,      &
     &-1450249.,-921302.1,-353973.4,463077.1,1367812.,2282826.,         &
     &3134991.,3858350.,4516656.,5441888.,6147992.,6914916.,7288510.,   &
     &7147760.,6462147.,5621066.,4603148.,3210620.,2387941.,1321321.,   &
     &1046460.,                                                         &
     &-4708342.,-6382134.,-6983833.,-7804986.,-7873086.,-7695303.,      &
     &-6937803.,-5955056.,-5131108.,-4221447.,-3475260.,-2447103.,      &
     &-1724581.,-1091036.,-347057.8,504642.2,1608005.,2635887.,3526253.,&
     &4465333.,5261456.,6084238.,6908202.,7746244.,7951130.,7597998.,   &
     &6592439.,5277250.,3843090,2512705.,1588969.,703104.9,927493.4,    &
     &-4685088.,-6567670.,-6971900.,-7850017.,-8074569.,-7805665.,      &
     &-7095178.,-6286066.,-5510380.,-4574820.,-3782092.,-2760814.,      &
     &-2044360.,-1090093.,-265385.7,609069.1,1723909.,3128296.,4069255.,&
     &5084559.,5997733.,6820324.,7716854.,8281178.,8245150.,7663310.,   &
     &6265819.,4249998.,2349481.,982298.2,-20058.97,-64966.82,613551.1, &
     &-4697492.,-6440296.,-6497082.,-7212652.,-7385836.,-7325560.,      &
     &-6856076.,-6334871.,-5608214.,-4778506.,-3950506.,-3102535.,      &
     &-2286452.,-1233180.,-315501.1,604756.6,2000721.,3537246.,4564460.,&
     &5642454.,6562336.,7604769.,8382037.,8633313.,8450591.,7239076.,   &
     &5160092.,2471956.,320319.,-1445840.,-1933416.,-1197185.,307289.1, &
     &-4498376.,-6264558.,-5793368.,-5828142.,-6042533.,-6500122.,      &
     &-6153824.,-5854354.,-5337567.,-4908172.,-4187290.,-3227545.,      &
     &-2388492.,-1485795.,-493459.5,705965.1,2318738.,3912988.,5223462.,&
     &6211511.,7344585.,8498396.,8951045.,9033699.,8503844.,6452912.,   &
     &3220496.,-136648.8,-2728090.,-4737084.,-4446586.,-2933130.,       &
     &-105684.,                                                         &
     &-3582402.,-4924574.,-4255880.,-3520741.,-3968931.,-4755766.,      &
     &-4953638.,-5166255.,-4887805.,-4717018.,-4223476.,-3530105.,      &
     &-2519241.,-1623204.,-545867.8,784475.6,2406287.,4298338.,         &
     &5810763.,6884838.,8025496.,9266424.,9595524.,9538755.,8247420.,   &
     &5519249.,1025285.,-3582593.,-6813954.,-8874215.,-8354157.,        &
     &-5715388.,-1160188.,                                              &
     &-1826248.,-2728064.,-1482540.,-303616.3,-1170324.,-2487811.,      &
     &-3301422.,-3970471.,-4256004.,-4484016.,-4290775.,-3965588.,      &
     &-2838772.,-1742834.,-613410.1,730439.4,2616912.,4670268.,         &
     &6436481.,7715498.,8908482.,1.024675e+07,1.045545e+07,1.004943e+07,&
     &8060278.,4473366.,-1478069.,-7895714.,-1.209433e+07,-1.416813e+07,&
     &-1.349315e+07,-9079429.,-3614306.,                                &
     &1495344.,365904.4,2686130.,3628202.,2206317.,249844.3,            &
     &-1440797.,-2490130.,-3533194.,-4495477.,-4718896.,-4409241.,      &
     &-3443172.,-2150731.,-788457.2,648578.7,2877919.,5436550.,         &
     &7079374.,8784376.,1.023441e+07,1.134754e+07,1.18273e+07,          &
     &1.095829e+07,8295394.,3432043.,-4429692.,-1.294127e+07,           &
     &-1.86596e+07,-2.172068e+07,-2.092779e+07,-1.469124e+07,-6513079., &
     &7566286.,5669165.,8313054.,8916941.,6676137.,3327969.,            &
     &612284.9,-1089442.,-2940050.,-4542409.,-5137538.,-4963334.,       &
     &-4124808.,-2813968.,-1249376.,638691.3,3150280.,6079466.,         &
     &7932640.,9931984.,1.182114e+07,1.291923e+07,1.362208e+07,         &
     &1.227164e+07,8982291.,2310723.,-7345317.,-1.824287e+07,           &
     &-2.636511e+07,-3.161505e+07,-3.145881e+07,-2.33066e+07,           &
     &-1.10414e+07,                                                     &
     &1.713696e+07,1.426501e+07,1.602415e+07,1.560995e+07,              &
     &1.169762e+07,6779151.,3042913.,-112688.,-2928124.,-4686552.,      &
     &-5598730.,-5347326.,-4528528.,-2927713.,-1207992.,994963.9,       &
     &3811544.,6820194.,9034366.,1.112203e+07,1.347644e+07,1.523938e+07,&
     &1.56908e+07,1.403863e+07,9744265.,1380405.,-1.045777e+07,         &
     &-2.363129e+07,-3.421596e+07,-4.232818e+07,-4.397456e+07,          &
     &-3.379269e+07,-1.831733e+07,                                      &
     &2.941875e+07,2.569069e+07,2.592953e+07,2.348308e+07,1.784609e+07, &
     &1.168591e+07,5910614.,620696.9,-2942742.,-5104643.,-5987772.,     &
     &-5068594.,-3756314.,-1676618.,407250.2,2591781.,5491882.,         &
     &8457570.,1.06792e+07,1.300713e+07,1.556461e+07,1.807675e+07,      &
     &1.845039e+07,1.651338e+07,1.106265e+07,1363794.,-1.236063e+07,    &
     &-2.667743e+07,-3.860363e+07,-4.945452e+07,-5.444406e+07,          &
     &-4.473249e+07,-2.528791e+07,                                      &
     &4.398493e+07,3.902301e+07,3.703364e+07,3.291616e+07,2.66609e+07,  &
     &1.881227e+07,9952347.,2832138.,-2061332.,-5120051.,-5810617.,     &
     &-4621194.,-2539200.,5480.091,2613887.,5206930.,8533702.,          &
     &1.140208e+07,1.353265e+07,1.578132e+07,1.84296e+07,2.065287e+07,  &
     &2.100688e+07,1.874341e+07,1.244249e+07,2941914,-1.051553e+07,     &
     &-2.417196e+07,-3.537312e+07,-4.797264e+07,-5.571877e+07,          &
     &-5.059598e+07,-2.957026e+07,                                      &
     &5.950389e+07,5.268292e+07,4.882452e+07,4.391716e+07,3.700496e+07, &
     &2.783868e+07,1.657407e+07,7688949.,880993.7,-3441981.,-4827442.,  &
     &-4436195.,-2571542.,277526.4,3510918.,7135644.,1.140301e+07,      &
     &1.422313e+07,1.589656e+07,1.787341e+07,1.924645e+07,2.017422e+07, &
     &2.002363e+07,1.747199e+07,1.169677e+07,3826066.,-7398834.,        &
     &-1.855361e+07,-2.815611e+07,-3.89391e+07,-4.88555e+07,            &
     &-4.920498e+07,-3.014652e+07,                                      &
     &7.190888e+07,6.576423e+07,5.922896e+07,5.277818e+07,              &
     &4.534657e+07,3.586671e+07,2.449271e+07,1.405145e+07,5944550.,     &
     &669667.2,-1976898.,-2822942.,-2409899.,-392324.2,2919008.,        &
     &7121085.,1.155499e+07,1.403946e+07,1.523995e+07,1.64521e+07,      &
     &1.653957e+07,1.6117e+07,1.489731e+07,1.217794e+07,7301966.,       &
     &934352.8,-8035936.,-1.695871e+07,-2.455439e+07,-3.288879e+07,     &
     &-4.343476e+07,-4.602783e+07,-2.791496e+07,                        &
     &7.31755e+07,6.847934e+07,6.042688e+07,5.195875e+07,4.625756e+07,  &
     &3.804738e+07,2.859586e+07,1.759356e+07,9442344.,4235804.,1198366.,&
     &-370487.3,-1000932.,-791819.4,1073756.,4216116.,7964550.,         &
     &1.050299e+07,1.137061e+07,1.227149e+07,1.237492e+07,1.154301e+07, &
     &9690036.,6569700.,1841862.,-4916258.,-1.337377e+07,-2.205146e+07, &
     &-2.780837e+07,-3.468295e+07,-4.391114e+07,-4.427488e+07,          &
!    &-2.567899e+07/
     &-2.567899e+07,                                                    &
!     data eof3/314622.4,446157.7,148496.8,-499168.8,-984649.5,         &
     &314622.4,446157.7,148496.8,-499168.8,-984649.5,                   &
     &-1573071.,                                                        &
     &-2062517.,-2301275.,-2360370.,-2388202.,-2384845.,-2463211.,      &
     &-2464774.,-2289638.,-2075672.,-2070251.,-2006550.,-1703989.,      &
     &-1755430.,-1612282.,-1459101.,-1434679.,-1182009.,-758098.2,      &
     &-591213.4,9495.456,492213.9,858164.4,1426467.,1966256.,2271615.,  &
     &1465998.,1343640.,                                                &
     &830839.1,777802.1,486524.3,-189759.3,-911942.7,-1625400.,         &
     &-2354770.,-2611929.,-2809774.,-2826629.,-2819800.,-2847167.,      &
     &-2729571.,-2553134.,-2478144.,-2303860.,-2302584.,-2031971.,      &
     &-2039559.,-1879516.,-1694394.,-1562735.,-1364759.,-872511.6,      &
     &-656795.5,33083.57,557997.2,1113055.,1628759.,2457284.,2648616.,  &
     &1674851.,1625487.,                                                &
     &1409924.,1019259.,883528.6,232237.5,-726269.4,-1708267.,-2430381.,&
     &-2978283.,-3171439.,-3163603.,-3234439.,-3252424.,-3001476.,      &
     &-2865035.,-2928616.,-2601377.,-2539364.,-2472430.,-2427976.,      &
     &-2146706.,-2112735.,-1784020.,-1652875.,-1233708.,-507537.,       &
     &110354.9,750772.4,1623374.,2083827.,3006348.,3095268.,1818044.,   &
     &1877051.,                                                         &
     &1554329.,1321874.,1500238.,817836.6,-143493.1,-1426526.,-2522952.,&
     &-3295829.,-3467110.,-3623347.,-3678847.,-3697243.,-3496563.,      &
     &-3338830.,-3344515.,-3088807.,-2954478.,-2810228.,-2761952.,      &
     &-2629170.,-2548421.,-2191438.,-2080080.,-1366911.,-560841.6,      &
     &84850.73,1128723.,2202335.,2901654.,3544543.,3390544.,2107183.,   &
     &2068180.,                                                         &
     &2104222.,1963978.,2377411.,1629444.,680337.6,-978751.7,-2429675,  &
     &-3598742.,-4087000.,-4188937.,-4276332.,-4169694.,-4061433.,      &
     &-3901978.,-3836869.,-3532741.,-3523958.,-3291452.,-3176570.,      &
     &-3165892.,-2979024.,-2630462.,-2306031.,-1650492.,-735568.4,      &
     &203098.6,1561690.,2727543.,3631779.,4043107.,3605264.,2252031.,   &
     &2239855.,                                                         &
     &3237609.,2645918.,3307854.,3065398.,1997126.,-117285.6,-2216635., &
     &-3839580.,-4672208.,-4908996.,-4916144.,-4835156.,-4755798.,      &
     &-4547915.,-4524278.,-4193978.,-4171761.,-3956728.,-3786645.,      &
     &-3742429.,-3451387.,-3167889.,-2732456.,-2144256.,-843505.1,      &
     &200478.3,1661352.,3333076.,4285979.,4586444.,4023968.,2263111.,   &
     &2269976.,                                                         &
     &4136324.,3685260.,4534184.,5182235.,3862269.,917375.2,-1806512.,  &
     &-4026184.,-5332140.,-5646671.,-5825562.,-5684395.,-5717667.,      &
     &-5540103.,-5296860.,-5168160.,-4956880.,-4770304.,-4736606.,      &
     &-4496550.,-4157802.,-3914586.,-3454898.,-2611904.,-1165725.,      &
     &244.4664,1982738.,4039884.,5297042.,5591376.,4278206.,2220066.,   &
     &2366916.,                                                         &
     &4996856.,5349637.,6983288.,7890998.,6178988.,2371306.,-1390749.,  &
     &-4386270.,-6130552.,-6473580.,-7121288.,-6808043.,-6868183.,      &
     &-6781514.,-6398884.,-6347426.,-6171113.,-6093226.,-6071333.,      &
     &-5642187.,-5228752.,-4949383.,-4255686.,-3223986.,-1720856.,      &
     &-159110.9,2444674.,4918448.,6666186.,6823784.,4425610.,2192190.,  &
     &2701361.,                                                         &
     &6690670.,7588610.,1.049304e+07,1.170568e+07,9152625.,3904538.,    &
     &-955672.4,-4989505.,-7254350.,-8016101.,-8635893.,-8580988.,      &
     &-8466865.,-8466263.,-8250060.,-8159519.,-7985556.,-7881800.,      &
     &-7653272.,-7354888.,-6916090.,-6360451.,-5320331.,-4118812.,      &
     &-2363889.,-459975.6,2553537.,5872735.,8262008.,8103138.,4863052., &
     &1876987.,2857482.,                                                &
     &9939955.,1.14642e+07,1.534415e+07,1.698387e+07,1.276268e+07,      &
     &5420422.,-915903.6,-6198363.,-9079450.,-1.020251e+07,             &
     &-1.094061e+07,-1.098656e+07,-1.101355e+07,-1.081624e+07,          &
     &-1.067883e+07,-1.059778e+07,-1.03633e+07,-1.013347e+07,-9833245., &
     &-9736407.,-9079433.,-8300264.,-6936270.,-5537809.,-3477706.,      &
     &-1152812.,2425573.,6665913.,1.035781e+07,1.019174e+07,5766342.,   &
     &1730073.,3162636.,                                                &
     &1.494946e+07,1.726946e+07,2.214678e+07,2.367381e+07,1.70876e+07,  &
     &7260400.,-1434200.,-7895262.,-1.145331e+07,-1.284597e+07,         &
     &-1.378986e+07,-1.398675e+07,-1.400286e+07,-1.380998e+07,          &
     &-1.336018e+07,-1.336776e+07,-1.322488e+07,-1.305702e+07,          &
     &-1.266709e+07,-1.265451e+07,-1.195389e+07,-1.086899e+07,          &
     &-9396855.,-7644320.,-5433612.,-2396577.,1558667.,6935419.,        &
     &1.2749e+07,1.354243e+07,7440482.,1822069.,3576621.,               &
     &2.213311e+07,2.505027e+07,3.084354e+07,3.112081e+07,2.165618e+07, &
     &8888783.,-1594052.,-9273081.,-1.372354e+07,-1.545695e+07,         &
     &-1.661899e+07,-1.687489e+07,-1.677181e+07,-1.651928e+07,          &
     &-1.614854e+07,-1.583899e+07,-1.5904e+07,-1.570557e+07,            &
     &-1.544649e+07,-1.547859e+07,-1.492712e+07,-1.376983e+07,          &
     &-1.211128e+07,-1.030239e+07,-8309643.,-5251016.,-1408855.,        &
     &4889775.,1.325489e+07,1.743002e+07,9871301.,1404936.,3010461.,    &
     &3.027075e+07,3.35656e+07,3.887665e+07,3.756202e+07,2.462149e+07,  &
     &9886700.,-1876612.,-9858467.,-1.469433e+07,-1.654207e+07,         &
     &-1.800527e+07,-1.831783e+07,-1.817502e+07,-1.781354e+07,          &
     &-1.759599e+07,-1.740031e+07,-1.711981e+07,-1.687534e+07,          &
     &-1.706992e+07,-1.695779e+07,-1.666422e+07,-1.582873e+07,          &
     &-1.475557e+07,-1.375978e+07,-1.245482e+07,-1.044283e+07,          &
     &-7293340.,-1420241.,9347988.,1.824247e+07,1.148862e+07,67750.86,  &
     &624175.9,                                                         &
     &3.515765e+07,3.98675e+07,4.256014e+07,3.825883e+07,2.298996e+07,  &
     &9057747.,-2523603.,-9560562.,-1.344351e+07,-1.526331e+07,         &
     &-1.650179e+07,-1.683278e+07,-1.69626e+07,-1.685137e+07,           &
     &-1.652839e+07,-1.68815e+07,-1.646901e+07,-1.609741e+07,           &
     &-1.619644e+07,-1.593374e+07,-1.599815e+07,-1.601084e+07,          &
     &-1.608505e+07,-1.625913e+07,-1.58795e+07,-1.518091e+07,           &
     &-1.350664e+07,-9224277.,920863.5,1.245405e+07,8706156.,-3269526., &
     &-3591942.,                                                        &
     &3.495094e+07,3.954528e+07,3.865825e+07,3.040881e+07,1.634854e+07, &
     &6431264.,-2748187.,-8106492.,-1.082093e+07,-1.198362e+07,         &
     &-1.264837e+07,-1.284358e+07,-1.312204e+07,-1.345812e+07,          &
     &-1.349921e+07,-1.393504e+07,-1.418651e+07,-1.372457e+07,          &
     &-1.307195e+07,-1.282841e+07,-1.285081e+07,-1.331461e+07,          &
     &-1.406216e+07,-1.486777e+07,-1.549234e+07,-1.569016e+07,          &
     &-1.603876e+07,-1.411711e+07,-7560620.,2211198.,1879234.,          &
     &-6793946.,-6259334.,                                              &
     &2.680579e+07,2.889269e+07,2.573243e+07,1.673862e+07,8003554.,     &
     &3180288.,-2928452.,-6162048.,-7837038.,-8195929.,-8039965.,       &
     &-8158206.,-8137698.,-8740260.,-9274056.,-9451374.,-9828972.,      &
     &-9618083.,-8969771.,-8284313.,-8544111.,-8548799.,-9328801.,      &
     &-1.038032e+07,-1.160811e+07,-1.27403e+07,-1.426203e+07,           &
     &-1.46988e+07,-1.130563e+07,-5754158.,-4201252.,-6628762.,         &
     &-4627180./
      data nom/2.163787e+07,2.264605e+07,2.274347e+07,2.173493e+07,     &
     &2.039849e+07,1.880722e+07,1.76798e+07,1.680914e+07,1.630783e+07,  &
     &1.610751e+07,1.623668e+07,1.647026e+07,1.677793e+07,1.710239e+07, &
     &1.732502e+07,1.741656e+07,1.718139e+07,1.704608e+07,1.717123e+07, &
     &1.701905e+07,1.683441e+07,1.701138e+07,1.713214e+07,1.755397e+07, &
     &1.83162e+07,1.941603e+07,2.079224e+07,2.216768e+07,2.381818e+07,  &
     &2.465965e+07,2.488819e+07,2.360805e+07,2.185616e+07,              &
     &2.395929e+07,2.571351e+07,2.600882e+07,2.482161e+07,2.319194e+07, &
     &2.13226e+07,1.985675e+07,1.892028e+07,1.825046e+07,1.803146e+07,  &
     &1.813286e+07,1.844819e+07,1.870602e+07,1.903964e+07,1.9304e+07,   &
     &1.940898e+07,1.915396e+07,1.899582e+07,1.921486e+07,1.895607e+07, &
     &1.882105e+07,1.887761e+07,1.906222e+07,1.955113e+07,2.049175e+07, &
     &2.176387e+07,2.329378e+07,2.51547e+07,2.720574e+07,2.827226e+07,  &
     &2.831094e+07,2.653534e+07,2.478283e+07,                           &
     &2.70667e+07,2.944748e+07,2.974632e+07,2.840199e+07,2.645661e+07,  &
     &2.412907e+07,2.234219e+07,2.118365e+07,2.04225e+07,2.019091e+07,  &
     &2.016388e+07,2.044097e+07,2.073596e+07,2.116464e+07,2.146072e+07, &
     &2.131395e+07,2.125275e+07,2.105004e+07,2.118592e+07,2.096015e+07, &
     &2.085018e+07,2.078729e+07,2.104133e+07,2.164236e+07,2.277652e+07, &
     &2.426221e+07,2.627279e+07,2.884485e+07,3.105395e+07,3.267815e+07, &
     &3.229646e+07,3.001993e+07,2.756194e+07,                           &
     &3.071515e+07,3.357783e+07,3.420302e+07,3.281923e+07,3.017425e+07, &
     &2.748872e+07,2.544925e+07,2.393332e+07,2.290231e+07,2.257314e+07, &
     &2.24508e+07,2.255884e+07,2.299098e+07,2.326585e+07,2.346156e+07,  &
     &2.334174e+07,2.319696e+07,2.304891e+07,2.309934e+07,2.302795e+07, &
     &2.275552e+07,2.279855e+07,2.324276e+07,2.396374e+07,2.516545e+07, &
     &2.717029e+07,2.98037e+07,3.308022e+07,3.561846e+07,3.756591e+07,  &
     &3.721541e+07,3.389425e+07,3.092844e+07,                           &
     &3.484532e+07,3.880602e+07,3.974362e+07,3.804968e+07,3.466702e+07, &
     &3.150972e+07,2.890467e+07,2.709869e+07,2.573434e+07,2.495072e+07, &
     &2.486717e+07,2.488102e+07,2.524911e+07,2.515065e+07,2.533103e+07, &
     &2.529204e+07,2.526981e+07,2.505555e+07,2.514982e+07,2.513823e+07, &
     &2.496043e+07,2.492324e+07,2.544142e+07,2.635059e+07,2.779676e+07, &
     &3.04932e+07,3.392106e+07,3.802177e+07,4.127946e+07,4.353376e+07,  &
     &4.277174e+07,3.819595e+07,3.469122e+07,                           &
     &3.96806e+07,4.456492e+07,4.632832e+07,4.457974e+07,4.065932e+07,  &
     &3.67155e+07,3.313204e+07,3.082811e+07,2.911806e+07,2.795613e+07,  &
     &2.759524e+07,2.733264e+07,2.729889e+07,2.72596e+07,2.74638e+07,   &
     &2.742819e+07,2.745385e+07,2.725184e+07,2.744125e+07,2.741077e+07, &
     &2.738589e+07,2.745291e+07,2.785498e+07,2.906646e+07,3.110815e+07, &
     &3.458355e+07,3.903459e+07,4.436781e+07,4.844528e+07,5.112772e+07, &
     &4.960073e+07,4.337774e+07,3.865246e+07,                           &
     &4.568726e+07,5.171574e+07,5.464674e+07,5.303889e+07,4.848093e+07, &
     &4.334518e+07,3.890633e+07,3.579912e+07,3.340917e+07,3.168858e+07, &
     &3.08861e+07,3.033714e+07,2.988109e+07,3.002307e+07,2.990195e+07,  &
     &3.007158e+07,3.006888e+07,2.993791e+07,3.024532e+07,3.025844e+07, &
     &3.058776e+07,3.0703e+07,3.130527e+07,3.281924e+07,3.550659e+07,   &
     &4.004791e+07,4.594895e+07,5.26851e+07,5.820912e+07,6.093561e+07,  &
     &5.846027e+07,4.96592e+07,4.359543e+07,                            &
     &5.368096e+07,6.131866e+07,6.568315e+07,6.438405e+07,5.892377e+07, &
     &5.224846e+07,4.684366e+07,4.252441e+07,3.934412e+07,3.695518e+07, &
     &3.555099e+07,3.457401e+07,3.401536e+07,3.395793e+07,3.362273e+07, &
     &3.398065e+07,3.42364e+07,3.428816e+07,3.480887e+07,3.491505e+07,  &
     &3.544976e+07,3.586662e+07,3.66769e+07,3.847001e+07,4.197232e+07,  &
     &4.78369e+07,5.546106e+07,6.415266e+07,7.134121e+07,7.420132e+07,  &
     &6.979627e+07,5.72952e+07,4.993969e+07,                            &
     &6.53104e+07,7.453446e+07,8.100446e+07,8.015896e+07,7.345339e+07,  &
     &6.493999e+07,5.79416e+07,5.203926e+07,4.782874e+07,4.491525e+07,  &
     &4.273095e+07,4.12204e+07,4.053718e+07,4.013189e+07,4.007326e+07,  &
     &4.043734e+07,4.095824e+07,4.181807e+07,4.258386e+07,4.302553e+07, &
     &4.362337e+07,4.456518e+07,4.56506e+07,4.7806e+07,5.219165e+07,    &
     &5.91082e+07,6.871569e+07,8.002242e+07,8.94702e+07,9.290822e+07,   &
     &8.539098e+07,6.837956e+07,5.788086e+07,                           &
     &8.241265e+07,9.371211e+07,1.024948e+08,1.019437e+08,9.335783e+07, &
     &8.249221e+07,7.330364e+07,6.608382e+07,6.057803e+07,5.651128e+07, &
     &5.38513e+07,5.189693e+07,5.087567e+07,5.0381e+07,5.051617e+07,    &
     &5.102923e+07,5.193482e+07,5.372715e+07,5.464499e+07,5.584988e+07, &
     &5.694717e+07,5.818657e+07,5.960329e+07,6.233852e+07,6.760946e+07, &
     &7.555777e+07,8.737924e+07,1.018224e+08,1.144415e+08,1.187061e+08, &
     &1.07561e+08,8.470163e+07,6.950075e+07,                            &
     &1.068594e+08,1.20901e+08,1.319313e+08,1.308652e+08,1.192304e+08,  &
     &1.053605e+08,9.363336e+07,8.427575e+07,7.728734e+07,7.227872e+07, &
     &6.918546e+07,6.704826e+07,6.561225e+07,6.514464e+07,6.50228e+07,  &
     &6.602658e+07,6.737998e+07,6.989546e+07,7.13997e+07,7.293089e+07,  &
     &7.467998e+07,7.630994e+07,7.836858e+07,8.191821e+07,8.817689e+07, &
     &9.77793e+07,1.120285e+08,1.303363e+08,1.467539e+08,1.524302e+08,  &
     &1.366695e+08,1.058218e+08,8.650502e+07,                           &
     &1.367971e+08,1.535834e+08,1.670364e+08,1.643997e+08,1.483795e+08, &
     &1.306866e+08,1.153159e+08,1.033887e+08,9.493343e+07,8.927709e+07, &
     &8.57227e+07,8.329807e+07,8.154209e+07,8.05483e+07,8.017469e+07,   &
     &8.092742e+07,8.283765e+07,8.59034e+07,8.830183e+07,9.039071e+07,  &
     &9.290147e+07,9.540966e+07,9.833059e+07,1.02818e+08,1.101918e+08,  &
     &1.214563e+08,1.385164e+08,1.60778e+08,1.81532e+08,1.90058e+08,    &
     &1.691848e+08,1.305031e+08,1.053786e+08,                           &
     &1.640431e+08,1.828668e+08,1.972162e+08,1.925819e+08,1.720869e+08, &
     &1.499314e+08,1.308151e+08,1.164679e+08,1.066657e+08,1.000556e+08, &
     &9.604018e+07,9.317108e+07,9.095914e+07,8.960517e+07,8.908926e+07, &
     &8.926566e+07,9.118091e+07,9.473096e+07,9.755328e+07,1.004717e+08, &
     &1.037069e+08,1.070067e+08,1.107363e+08,1.15808e+08,1.241896e+08,  &
     &1.363584e+08,1.552138e+08,1.79958e+08,2.055298e+08,2.189036e+08,  &
     &1.94399e+08,1.508483e+08,1.208035e+08,                            &
     &1.761562e+08,1.940855e+08,2.072946e+08,2.008541e+08,1.766197e+08, &
     &1.519577e+08,1.31129e+08,1.147103e+08,1.033798e+08,9.583854e+07,  &
     &9.139016e+07,8.865574e+07,8.694153e+07,8.67141e+07,8.721718e+07,  &
     &8.802787e+07,8.907978e+07,9.154098e+07,9.354178e+07,9.541453e+07, &
     &9.77203e+07,1.006788e+08,1.050258e+08, 1.109418e+08,1.199948e+08, &
     &1.328088e+08,1.511808e+08,1.762166e+08,2.050085e+08,2.235726e+08, &
     &2.006752e+08,1.5661e+08,1.249316e+08,                             &
     &1.657279e+08,1.800129e+08,1.883615e+08,1.793923e+08,1.55644e+08,  &
     &1.324502e+08,1.128458e+08,9.618882e+07,8.407297e+07,7.604598e+07, &
     &7.161786e+07,6.979342e+07,7.032928e+07,7.246097e+07,7.520003e+07, &
     &7.675004e+07,7.677053e+07,7.644136e+07,7.633523e+07,7.566713e+07, &
     &7.557913e+07,7.72312e+07,8.146875e+07,8.81042e+07,9.775342e+07,   &
     &1.103463e+08,1.275029e+08,1.508316e+08,1.778392e+08,1.985617e+08, &
     &1.808708e+08,1.411881e+08,1.101232e+08,                           &
     &1.414566e+08,1.496452e+08,1.503433e+08,1.387637e+08,1.192389e+08, &
     &1.002085e+08,8.43259e+07,6.953819e+07,5.813887e+07,5.053431e+07,  &
     &4.624784e+07,4.569546e+07,4.75228e+07,5.108193e+07,5.464553e+07,  &
     &5.582265e+07,5.497499e+07,5.299077e+07,5.203736e+07,4.993874e+07, &
     &4.893956e+07,4.963887e+07,5.297103e+07,5.864174e+07,6.762337e+07, &
     &7.840697e+07,9.357496e+07,1.128747e+08,1.333854e+08,1.49082e+08,  &
     &1.359941e+08,1.054389e+08,7.915589e+07/
      data lat33/-80.,-75.,-70.,-65.,-60.,-55.,-50.,-45.,-40.,-35.,     &
     &-30.,-25.,-20.,-15.,-10.,-5.,0.,5.,10.,15.,20.,25.,30.,35.,40.,   &
     &45.,50.,55.,60.,65.,70.,75.,80./
      data z16/150.0003,146.667,143.3337,140.0003,136.667,133.3337,     &
     &130.0003,126.667,123.3337,120.0003,116.667,113.3337,110.0003,     &
     &106.667,103.3337,100.0003/
!
      kp=kp_idea(1)
      f107=f107_idea(1)
      if(kp.lt.0.7) kp=0.7
      if(f107.lt.70.) f107=70.
! d logp using for extent up
      do k=2,levs
        dx(k)=log(pr(k-1))-log(pr(k))
      enddo
!     print*,'dx',dx
! find interp latitude
      do il=1,im
        do i=1,32
          if(mlat(il).ge.lat33(i).and.mlat(il).le.lat33(i+1)) then
            iref(il)=i
            dl(il)=(mlat(il)-lat33(i))/(lat33(i+1)-lat33(i))
          endif
        enddo
      enddo
!     print*,zm
!... eof1 - kp
      m1 =  kp * 0.689254 - 1.53366
!... eof2 - declination
      theta0 = 360.*float(doy - 1)/365.*3.1415926/180.
      dec = 0.006918                                                    &
     &    - 0.399912 * cos(theta0)   + 0.070257 * sin(theta0)           &
     &    - 0.006758 * cos(2.*theta0) + 0.000907 * sin(2.*theta0)       &
     &    - 0.002697 * cos(3.*theta0) + 0.001480 * sin(3.*theta0)
      dec = dec * 180./3.1415927
      m2 = -0.31978                                                     &
     &   + dec    * 0.097309                                            &
     &   + dec**2 * 0.00048979                                          &
     &   - dec**3 * 0.00010360
!... eof3 - f107
      m3 =  alog10(f107) * 6.35777 - 13.8163
!... zonal mean distrib. is sum of mean and eofs
      do il=1,im
        do k=1,16
          zm(k) =dl(il)*(nom(iref(il)+1,k)                              &
     &        - m1 * eof(iref(il)+1,k,1)                                &
     &        + m2 * eof(iref(il)+1,k,2)                                &
     &        - m3 * eof(iref(il)+1,k,3))+                              &
     &         (1.-dl(il))*(nom(iref(il),k)                             &
     &        - m1 * eof(iref(il),k,1)                                  &
     &        + m2 * eof(iref(il),k,2)                                  &
     &        - m3 * eof(iref(il),k,3))
        enddo
        zm = zm*1.e6 ! zm in m-3
! vertical interp, from k1 to k2-1, extend k2 to levs, keep 
! cons 1 to k1-1
        k1=0
        k2=0
        kref=0
        do k=2,levs-1
          if(alt(il,k).lt.z16(16).or.alt(il,k).gt.z16(1)) go to 20
          if(kref(k-1).eq.0.and.k1.eq.0) k1=k
           do i=1,15
             if(alt(il,k).ge.z16(i+1).and.alt(il,k).le.z16(i)) then
              kref(k)=i
              dz(k)=(alt(il,k)-z16(i))/(z16(i+1)-z16(i))         
             endif
           enddo
          if(kref(k).ne.0)                                              &
     & no(il,k)=dz(k)*zm(kref(k)+1)+(1.-dz(k))*zm(kref(k))
          go to 30
  20      continue
          if((kref(k-1).ne.0).and.k2.eq.0.and.k1.ne.0)  k2=k
  30      continue
        enddo
!       if(k1.eq.0.or.k2.eq.0)print*,'www7',il,k1,k2,alt(il,1:150)
        no(il,1:k1-1)=no(il,k1) 
! extend up
        if(k2.gt.100) then
        do k=k2,levs
          no(il,k)=no(il,k-1)*n(il,k)/n(il,k-1)*                        &
     &    exp(dx(k)*(1.-.5*amno*(1./am(il,k-1)+1./am(il,k))))
        enddo
        endif
        do k=1,levs
          no(il,k)=max(no(il,k),0.)
        enddo
!
      enddo !il
      return
      end
