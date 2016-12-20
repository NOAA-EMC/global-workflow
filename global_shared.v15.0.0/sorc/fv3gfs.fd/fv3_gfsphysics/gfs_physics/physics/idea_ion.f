! Apr 06 2012 Henry Juang, initial implement for nems
! Dec    2012    Jun Wang, move init out of column physics
!========================================================
!=                    GetIonParams                      =
!========================================================
      subroutine idea_ion(solhr,cospass,zg,o_n,o2_n,n2_n,cp,            &
     &adu,adv,adt,dudt,dvdt,dtdt,rho,rlat,rlon,ix,im,levs,              &
     &dayno,utsec,sda,maglon,maglat,btot,dipang,essa) 
      use idea_composition, f107 =>f107_idea, kp =>kp_idea
      use physcons, pi => con_pi
!     use date_def
      implicit none
      REAL, PARAMETER ::DTR=3.141592653/180.0
      REAL, INTENT(IN)     :: o_n(ix,levs) ! number density O (/m3)
      REAL, INTENT(IN)     :: o2_n(ix,levs)
      REAL, INTENT(IN)     :: n2_n(ix,levs)
      REAL, INTENT(IN)     :: rho(ix,levs)  ! mass density (kg/m3)
      REAL, INTENT(IN)     :: zg(ix,levs)   !  height (m)
      REAL, INTENT(IN)     :: cp(ix,levs)   !  (J/kg/k)
      REAL, INTENT(IN)     :: cospass(im)! cos solar zenith angle (rad) 
      REAL, INTENT(IN)     :: rlat(im) ! latitude (rad)
      REAL, INTENT(IN)     :: rlon(im) ! longitude (rad)
      REAL, INTENT(IN)     :: solhr     ! universal time (h)
      INTEGER, INTENT(IN)     :: ix !longitude dim size
      INTEGER, INTENT(IN)     :: im !number of logitude
      INTEGER, INTENT(IN)     :: levs ! number of pres grid
!     INTEGER, INTENT(IN)     :: lev1 ! lowest pres level to start
      INTEGER, INTENT(IN)     :: dayno !calender day 
      REAL, INTENT(IN)     :: adt(ix,levs)  ! temperature (k)
      REAL, INTENT(IN)     :: adu(ix,levs)  ! zonal wind (m/s)
      REAL, INTENT(IN)     :: adv(ix,levs)  ! meridional wind (m/s)
! input Magnetic and electric parameters 
!     REAL, INTENT(in) :: elx(im)
!     REAL, INTENT(in) :: ely(im)     !electric field
      REAL, INTENT(in) :: maglon(im)  !magnetic longitude (rad)
      REAL, INTENT(in) :: maglat(im)  !magnetic latitude (rad)
      REAL, INTENT(in) :: btot(im)    !mapgnetic field strength
      REAL, INTENT(in) :: dipang(im)  !Dip angle (degree)
      REAL, INTENT(in) :: essa(im)    !magnetic local time
      REAL, INTENT(in) :: sda         ! solar declination angle (rad)
      REAL, INTENT(in) :: utsec       !universal time
! output
      REAL, INTENT(out)     :: dtdt(ix,levs)  ! temperature change (k/s)
      REAL, INTENT(out)     :: dudt(ix,levs)  ! zonal wind change (m/s2)
      REAL, INTENT(out)     :: dvdt(ix,levs)  ! meridional change wind (m/s2)
! local
      real rlt(im),sza(im),jh(ix,levs)                                  &
     &,rinc(5)
      INTEGER   i,k
! get sza in rad
      sza=acos(cospass)
! get local time in rad
      rlt=(rlon/(15.*pi/180.)+solhr)/24.*2.*pi
! get ion_drag
      call GetIonParams(dayno,utsec,f107(1),kp(1),sda,sza,rlat,zg,      &
     &   o_n, o2_n, n2_n,adu,adv,adt,rho,rlt,rlon,ix,im,levs,k91,       &
     &   btot,dipang,maglon,maglat,essa,                                &
     &   dudt,dvdt,jh) 
      do i=1,im
      do k=1,levs
      dtdt(i,k)=jh(i,k)/cp(i,k)
      enddo
      enddo
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE GetIonParams(dayno,utsec,f107,kp,sda,sza,rlat,ht,      &
     &   o_n, o2_n, n2_n,adu,adv,adt,rho,rlt,rlon,ix,im,levs,lev1,      &
     &   btot,dipang,maglon,maglat,essa,                                &
     &   dudt,dvdt,jh) 
      use physcons,  pi => con_pi
      implicit none
      REAL, PARAMETER ::DTR=3.141592653/180.0, ELCH=1.602e-19
      REAL, INTENT(IN)     :: o_n(ix,levs) ! number density O (/m3)
      REAL, INTENT(IN)     :: o2_n(ix,levs)
      REAL, INTENT(IN)     :: n2_n(ix,levs)
      REAL, INTENT(IN)     :: adt(ix,levs)  ! temperature (k)
      REAL, INTENT(IN)     :: adu(ix,levs)  ! zonal wind (m/s)
      REAL, INTENT(IN)     :: adv(ix,levs)  ! meridional wind (m/s)
      REAL, INTENT(IN)     :: rho(ix,levs)  ! mass density (kg/m3)
      REAL, INTENT(IN)     :: ht(ix,levs)   ! geopotential height (m)
      REAL, INTENT(IN)     :: f107 
      REAL, INTENT(IN)     :: kp 
      REAL, INTENT(IN)     :: sda    ! solar declination angle (rad)
      REAL, INTENT(IN)     :: sza(im)! solar zenith angle (rad) 
      REAL, INTENT(IN)     :: rlt(im)  ! local time (rad) 
      REAL, INTENT(IN)     :: rlat(im) ! latitude (rad)
      REAL, INTENT(IN)     :: rlon(im) ! longitude (rad)
      REAL, INTENT(IN)     :: utsec ! universal time (s)
!     REAL, INTENT(in) :: elx(im)
!     REAL, INTENT(in) :: ely(im)     !electric field
      REAL, INTENT(in) :: maglon(im)  !magnetic longitude (rad)
      REAL, INTENT(in) :: maglat(im)  !magnetic latitude (rad)
      REAL, INTENT(in) :: btot(im)    !mapgnetic field strength
      REAL, INTENT(in) :: dipang(im)  !Dip angle (degree)
      REAL, INTENT(in) :: essa(im)    !magnetic local time
      INTEGER,    INTENT(IN)     :: dayno  !day 
      INTEGER,    INTENT(IN)     :: ix !longitude dim size
      INTEGER,    INTENT(IN)     :: im !number of logitude
      INTEGER,    INTENT(IN)     :: levs ! number of pres grid
      INTEGER,    INTENT(IN)     :: lev1 ! lowest pres level to start

      REAL, INTENT(OUT) :: dvdt(ix,levs)!(m/s2)
      REAL, INTENT(OUT) :: dudt(ix,levs)!(m/s2)
      REAL, INTENT(OUT) :: jh(ix,levs)! (J/kg/s)
! local
      real ht1(levs),v1(levs),nden(levs),o2n(levs),on(levs),            &
     &    n2n(levs),elx(im),ely(im),ssa,elz(im),ee1(im),                &
     &    ee2(im),cosdif,sindif,sdip,cdip,btheta,bphi,elecx,            &
     &    elecy,dif,dlat,dlon
      INTEGER   k,i       
!     Ion drag variables :
! 
!     teff(levs)      1d local array of temperature
!     pion1(levs)     number density O+
!     pion2(levs)     number desntiy NO+
!     pion3(levs)     number density O2+
!     r                
!     sigped           pedersen conductivity
!     sighall          hall conductivity
!     jphi(levs)     eastward curreil
!     jth(levs)      southward
!     rvin(levs)     Ion/Neutral collision  frequency param    
!     ramin(levs)    mean ion mass
!     a5               Meridional ion drag term
!     b5               Zonal ion drag term
!     c7               Joule heating term
!     
      REAL      :: teff(levs), pion1(levs), pion2(levs)
      REAL      :: sigped, sighal, pion3(levs)
      REAL      :: rvin(levs), ramin(levs)
      REAL      :: r, brad, bth, dip
      REAL      :: a5, b5, c7
      REAL      :: jth,jrad  
      REAL      :: jphi 
      REAL      :: eden(ix,levs)  !electron density
!===================================================================
!=              Calculate Electric Field and magnetic field               =
!===================================================================
      call idea_geteb(im,ix,dayno,utsec,f107,kp,maglat,maglon,          &
     &essa,ee1,ee2)
!     ee1=0.
!     ee2=0.
! ===================================================================
! =                   Calculate Electron Density                    =
! ===================================================================
! CHIU ionosphere for electron density (Earth_chiu_model.f90). 
      DO i = 1,im 
         do k=1,levs
           ht1(k)=ht(i,k)
         enddo
         CALL EARTH_CHIU_MODEL(sda,sza(i),maglat(i),                    &
     &         maglon(i),rlt(i), rlat(i), f107,                         &
     &         dipang(i)*DTR, dayno, ht1, eden,i,lev1,                  &
     &         levs,ix)
      ENDDO
!     print*,'chiuok'
!     print*,'eden',eden(1,lev1:levs)

!r=================================================================
!r=         Calculate Ion Drag, Joule Heating and Particle        =
!r=                     Precipitation terms                       =
!r=================================================================
      DO i = 1, im 
        DO k = lev1,levs
          nden(k)=o_n(i,k)+o2_n(i,k)+n2_n(i,k)
          teff(k) = adt(i,k)
          v1(k)=-1.*adv(i,k) ! v1 positive south
          on(k)=o_n(i,k)
          o2n(k)=o2_n(i,k)
          n2n(k)=n2_n(i,k)
        enddo
        do k=lev1,levs
          dudt(i,k)  = 0.
          dvdt(i,k)  = 0.
          jh(i,k) = 0.
          pion1(k) = on(k)
          pion2(k) = 0.5*(o2n(k)+n2n(k)) 
          pion3(k) = 0.5*(o2n(k)+n2n(k))
        ENDDO
! Get ion neutral collision frequency
      CALL IONNEUT(on,o2n,n2n,pion1,pion2,pion3,teff,rvin,ramin,levs,   &
     &lev1)
!     print*,'ionneut ok'
! Calculate ion drag and electron deposition
! jth                     - N/S electrical conductivity
! jphi                    - E/W electrical conductivity
!     DO k=lev1,levs 
          dip     = dipang(i)*DTR
          sdip    =sin(dip)                              !new
          cdip    =cos(dip)                              !new
!
          elecx   =ee2(i)*sdip
          elecy   =ee1(i)
!
          ssa=rlon(i)+(utsec/3600.-12.)*pi/12.
          dif     =essa(i)*pi/180.-ssa ! check unit
          cosdif  =cos(dif)
          sindif  =sin(dif)
!
          elz(i)  =-1.*ee2(i)*cdip
          if(sdip.ge.0.) then
          elx(i)  =elecx*cosdif-elecy*sindif
          ely(i)  =elecx*sindif+elecy*cosdif
          else
          elx(i)  =elecx*cosdif+elecy*sindif
          ely(i)  =-1.*elecx*sindif+elecy*cosdif
          endif
!     dlat=rlat(i)*180./3.14159
!     dlon=rlon(i)*180./3.14159
!     if(abs(dlat-60.).lt.1..and.abs(dlon-270.).lt.                     &
!    &1.) then
!     print*,'www1',utsec,ee1(i),ee2(i),elx(i),ely(i),cosdif,           &
!    &sindif
!     print*,'www2',rlon(i),ssa,essa(i),dif,sdip,elecx,elecy
!     endif
!
          btheta  =-1.*btot(i)*cdip*cosdif               !new
          bphi    =-1.*btot(i)*cdip*sindif               !new
          bth     = btot(i)    ! In teslas, so no *1.e-9
          brad    = -1.*bth*sdip
      DO k=lev1,levs 
          r       = (ramin(k)*rvin(k))/(ELCH*bth)
          sigped  = (eden(i,k)*ELCH*r)/(bth*(1.0+r**2))
          sighal  = sigped*r
          jphi = sigped*(ely(i)-v1(k)*brad)                             &
     &         - sighal*(elx(i) + adu(i,k)*brad)/sdip !new
          jth  = sigped*(elx(i) + adu(i,k)*brad) +                      &
     &         sighal*(ely(i)-v1(k)*brad)*sdip             !new
          jrad = sigped*(elz(i) - adu(i,k)*btheta+v1(k)*bphi)           &
     &        -sighal*(ely(i)-v1(k)*brad)*cdip             !new
          a5 =(jphi*brad-jrad*bphi)/rho(i,k)               !new
             b5 = -1.*(jth*brad-jrad*btheta)/rho(i,k)      !new
          c7 =(jth*(elx(i)+adu(i,k)*brad)+                              &
     &         jphi*(ely(i)-v1(k)*brad)+jrad*elz(i))/rho(i,k) !new
! Calculation of ion drag terms END
          dvdt(i,k)   =-1.* a5
          dudt(i,k)   = b5
          jh(i,k)   = c7
        ENDDO
        DO k=1,lev1-1 
          dvdt(i,k) = 0.
          dudt(i,k) = 0.
          jh(i,k) = 0.
        enddo
      ENDDO 
      RETURN
      END SUBROUTINE GetIonParams
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE IONNEUT(P1,P2,P3,PI1,PI2,PI3, T,VIN,AMIn,NMAx,n0)

      !*** Start of declarations inserted by SPAG
      REAL :: a , AMIn , amu , b , factor , P1 , P2 , P3 ,              &
     &      PI1 , PI2 ,  PI3,                                           &
     &      sum , T , v1 , v2 , VIN
      INTEGER :: n , NMAx, n0
      !*** END of declarations inserted by SPAG
      DIMENSION P1(NMAx) , P2(NMAx) , P3(NMAx) , T(NMAx) , VIN(NMAx) ,  &
     &     AMIn(NMAx) ,                                                 &
     &     a(3) , b(3) , PI1(NMAx) , PI2(NMAx), PI3(NMAx)
      REAL :: mi1 , mi2, mi3, summol
      
      DATA mi1 , mi2, mi3/16. , 30., 32./
      !********************************************************************
      ! The following a,b, are cooeficients used to caculate ion-neutral
      ! collision frequency. Tim's Thesis  3.5a,3.5b.  mjh 1.9.97
      !********************************************************************
      
      DATA a/3.42E-11 , 6.66E-10 , 6.82E-10/
      DATA b/2.44E-10 , 4.28E-10 , 4.34E-10/
      amu = 1.66E-27
!c  **
!c  **
      factor = 1.0
!c  **
!c  **
      

      DO 100 n = n0 , NMAx
         summol = PI2(n) + PI3(n)
         sum = PI1(n) + PI2(n) + PI3(n)
         v2 = b(1)*P1(n) + b(2)*P2(n) + b(3)*P3(n)
         v1 = a(3)*P3(n) + a(2)*P2(n) + a(1)*P1(n)*factor*SQRT(T(n))    &
     &       *(1.08-0.139*LOG10(T(n))+4.51E-03*LOG10(T(n))**2)
         if(summol.lt.1.e-90) summol=0.0
         if(v1.lt.1.e-90) v1=0.0
         if(v2.lt.1.e-90) v2=0.0
!        if(pi1(n).lt.1.e-90) pi1(n)=0.0
!     if(iout.eq.1) write(6,*) 'here 5',n
         VIN(n) = (v1*PI1(n)+v2*summol)*1.E-06/sum
         AMIn(n) = (PI1(n)*mi1+PI2(n)*mi2+PI3(n)*mi3)*amu/sum
 100  CONTINUE

      RETURN
      END SUBROUTINE IONNEUT
!**  $Id: chiu_model.f90,v 1.1.1.1 2006/06/04 18:19:13 cwplot Exp $
!r
!r   chui_model.f       Chiu ionosphere, to return electron density.
!r                      Converted to run F90, but not changed. mjh
!r
!r
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE Earth_CHIU_MODEL(sda,sza,thmag,phimr,rlt,rlat,         &
     &f107, dip, nday,ht1d,eden3d,ilon,lev1,ht_dim,lon_dim)
  
  
      REAL,     INTENT(IN)   :: sda, sza, thmag, phimr, rlt
      REAL,     INTENT(IN)   :: rlat, f107, dip
      INTEGER,  INTENT(IN)   :: nday,ht_dim,lon_dim,lev1
      REAL,     INTENT(IN)   :: ht1d(ht_dim)
  
      REAL,     INTENT(OUT)  :: eden3d(lon_dim,ht_dim)
  
!*** Start of declarations inserted by SPAG
      REAL :: abstmg , beta , cbp , cosrlt , costmg , cosza , dipf ,    &
     &    DTR , e , f , flong , g , g5 , g6 , g7 , g8 , gel , gel1 ,    &
     &    gsm,  a, rh
      REAL :: P , pb , PI , qel , rd , rgamma , RHO , rk , rl ,         &
     &    rt , s , sap ,sintmg 
      REAL ::  ty1 , ty2 , u , V , w , wr , x , y, alp,rr,fz, fn
      INTEGER    :: i
!*** End of declarations inserted by SPAG
  
!- define parameters
  
      PARAMETER (PI=3.141592653)
      PARAMETER (DTR=PI/180.0)
  
      DIMENSION f(3) , pb(3) , s(3) , rd(3) , rl(3) , rt(3) , e(3) ,    &
     & u(3) , V(3) , P(3) , flong(3) , dipf(3), alp(3), a(3)            &
     & ,rh(3), rr(3), fz(3), fn(3)
  
      REAL :: z(ht_dim)
  
      INTEGER :: n
  
! absolutely no idea what these are. Imported from tucan.f
      DATA alp/.5 , .5 , 1./
      DATA p/110. , 180. , 0./
      DATA a/1.36 , 2.44 , 0.66/
      DATA rh/10. , 34. , 0./
  
      rho = (f107-50.)/100.
      ty = (nday+15.5)*12./365.
      IF ( ty > 12.0 ) ty = ty - 12.0
  
      abstmg = ABS(THMag)
      cosza = COS(SZA)
      sintmg = SIN(THMag)
      costmg = COS(THMag)
      cosrlt = COS(RLT)


      ty1 = SIN(PI/12.0*TY)
      ty2 = COS(PI/6.0*TY)
      P(1) = 110.
      P(2) = 180.
      f(1) = 0.0
      f(2) = 0.0
      pb(1) = 1.0
      pb(2) = 1.0
      s(1) = SQRT(1.0+1.15*RHO)
      s(2) = SQRT(1.0+1.24*RHO+0.25*RHO**2)
      rl(1) = 1.0
      rl(2) = 1.0
      e(1) = 1.0
      e(2) = 1.0
      flong(1) = 1.0
      flong(2) = 1.0
      dipf(1) = 1.0
      dipf(2) = 1.0
      g5 = SIN(PHImr)
      g6 = SIN(PHImr/2.0)
      g7 = SQRT(ABS(g5))
      g8 = COS(PHImr/2.0-PI/20.0)
      sap = SIN(SDA)*sintmg
      f(3) = EXP(-(2.92*SIN(PI/2.0-abstmg))**6)
      IF ( THMag <= 0.0 ) THEN
         cbp = 0.0
         IF ( g7 /= 0.0 ) cbp = ty1*(0.5*g6-0.5*g5-g6**8)-(1.0+ty1)     &
     &    *ty2*g5/g7*EXP(-4.0*g6*g6)
         pb(3) = (2.5+2.0*RHO+ty2*(0.5+(1.3+0.5*RHO)*g8**4)             &
     &    +(1.3+0.5*RHO)*COS(RLT-PI*(1.0+cbp)))                         &
     &    *(1.0+0.4*ty1*ty1*EXP(-ty1*g8**4))
      ELSE
        wr = EXP(-1.2*(COS(THMag-DTR*23.5*cosrlt)-costmg))
        pb(3) = (2.0+1.0*RHO)*wr*(1.0+0.3*ty1)
      ENDIF
      s(3) = (1.0+RHO+0.204*RHO**2+0.05*RHO**3)
      IF ( RHO > 1.1 ) s(3) = 2.41 + 1.53*(s(3)-2.41)*(sintmg)**2
      P(3) = 240 + 75.0*RHO + 83.0*RHO*sap*costmg +                     &
     & 30.0*COS(RLT-4.5*ABS(THMag)-PI)                                  &
     & + 10.0*costmg*COS(PI/3.0*(TY-4.5))
      rd(1) = EXP(2.0*(cosza/ABS(cosza)*SQRT(ABS(cosza))-1.0))
      rd(2) = EXP((1.0+0.5*LOG(1.0+30.0*RHO))*(cosza/ABS(cosza)*SQRT(ABS& 
     & (cosza))-1.0))
      rd(3) = (0.9+0.32*sap)*(1.0+sap*(COS(RLT+PI/4.0))**2)             &
     & *EXP(-1.1*(1.0+COS(RLT-0.873)))
      qel = 1.0 - 0.15*EXP(-SQRT((12.0*THMag+1.05)**2+(TY/2.0-3.0)**2))
      rl(3) = (1.2-0.5*(costmg)**2)                                     &
     & *(1.0+0.05*RHO*(sintmg)**3*COS(PI*TY/6.0))                       &
     & *(EXP(3.0*COS(0.5*THMag*(SIN(RLT)-1.0))))*qel
      w = COS(RLAt+SDA*cosrlt) - COS(RLAt)
      rt(1) = EXP(-0.4*w)
      rt(2) = EXP(-0.25*w)
      beta = 1.3 + 0.139*RHO**2 + 0.009*RHO**3
      rk = 1.0 + 0.085*(COS(THMag-PI/6.0)*(COS(PI/12.0*(TY-2.0)))       &
     & **3+COS(THMag+PI/4.0)*(COS(PI/12.0*(TY-8.0)))**2)
      x = 0.7*(rk+0.178*RHO**2/s(3)*COS(PI/3.0*(TY-4.3)))               &
     & *EXP(-beta*(COS(THMag+SDA*cosrlt)-costmg))
      y = 0.2*(1.0-SIN(abstmg-0.524))*(1.0+0.6*COS(PI/3.0*(TY-3.94)))   &
     & *COS(PI/6.0*(TY-1.0)) + (0.13-0.06*SIN(ABS(abstmg-PI/9.0)))      &
     & *COS(PI/3.*(TY-4.5)) - (0.15+0.3*SIN(abstmg))*(1.-cosrlt)        &
     & **0.25*(COS(THMag+SDA))**3
      rt(3) = x + y/s(3)
      g = (1.0+0.6*SQRT(RHO)-0.2*RHO)*EXP(0.25*(1.0+COS(RLT-4.01)))
      gel = (costmg)**8*(COS(abstmg-0.262))**12
      gel1 = 1.0 + 0.05*(0.5-COS(PI*TY/3.0)+COS(PI*TY/6.0))
      e(3) = (1.0-0.4*(costmg)**10)                                     &
     & *(1.0+0.6*(costmg)**10*(COS(RLT+PI/4.0))**2)*(1.0+g*gel)         &
     & *gel1
      rgamma = 1.0 + 0.03*(0.5-COS(PI*TY/3.0)+COS(PI*TY/6.0))
      gsm=0.15-(1.0+RHO)*(SIN(THMag/2.0))**2*EXP(-0.33*(TY-6.0)**2)
      flong(3) = 1.0 + 0.1*(costmg)**3*COS(2.0*(PHImr-7.0*PI/18.0))
      dipf(3)=rgamma*(1.0+gsm*EXP(-18.0*(ABS(DIP)-2.0*PI/9.0)**2))
      DO i = 1 , 3
         u(i) = s(i)*rd(i)*rl(i)*rt(i)*e(i)*flong(i)*dipf(i)
         V(i) = f(i)*pb(i) + (1.0-f(i))*u(i)
      enddo
      DO n = lev1 , ht_dim
         z(n) = ht1d(n)/1000.
         IF ( z(n) <= p(3) ) rh(3) = 2.0*(20.0+0.1*z(n))
         IF ( z(n) > p(3) ) rh(3) = 2.0*(20.0+0.1*p(3))
      DO i = 1 , 3
        rr(i) = (z(n)-p(i))/rh(i)
        fz(i) = EXP(alp(i)*(1.0-rr(i)-EXP(-rr(i))))
        fn(i) = a(i)*fz(i)*v(i)
      enddo
       eden3d(ilon,n) = (fn(1)+fn(2)+fn(3))*1.E11
      ENDDO
      do n=1,lev1-1
      eden3d(ilon,n)=0.
      enddo
      RETURN
      END SUBROUTINE EARTH_CHIU_MODEL
! idea
      subroutine idea_geteb(im,ix,dayno,utsec,f107,kp,maglat,maglon,    &
     &essa,ee1,ee2)
      use efield
      use date_def
      use physcons, pi => con_pi
      implicit none
      integer, intent(in) :: im  ! number of data points in efield 
      integer, intent(in) :: ix  ! max data points in efield
      integer, intent(in) :: dayno  ! calender day
      real, intent(in) :: utsec  ! second
      real, intent(in) :: f107  ! 
      real, intent(in) :: kp  ! 
      real, intent(in) :: maglat(im)  ! magnetic latitude (rad)
      real, intent(in) :: maglon(im)  ! magnetic longitude (rad)
      real, intent(in) :: essa(im)  ! degree
      real, intent(out)   :: ee1(im)    ! electric field x direction mV/m
      real, intent(out)   :: ee2(im)    ! electric field y direction mV/m
!     character*(*), intent(in) ::   dir    ! directory located coef files
! local
      integer i,k,iref,jref
      real utsec_last,dx,dy,aa,bb,maglond,maglatd,                      &
     &ed11(0:nmlon,0:nmlat),ed22(0:nmlon,0:nmlat)
!
      data utsec_last/-1./
      save utsec_last,ed11,ed22
!hmhj save ylatm1,ylonm1
! initiate
! calculate efield only if diff time step
      if(utsec.ne.utsec_last) then
        utsec_last=utsec
        iday = dayno                   ! day of year
        imo=idate(2)
        iday_m=idate(3) 
        iyear = 1995
        f107d=f107
        ut=utsec/3600.
        bz = .433726 - kp*(.0849999*kp + .0810363)                      &
     &        + f107d*(.00793738 - .00219316*kp)
        by=0.
        call get_efield
!       print*,'www'
!       print'(8f10.4)',potent(0:180,68)
        ed11=ed1
        ed22=ed2
!     print*,'ed2',ed2(149,65)
      endif
!
!     call locate(maglon,maglat,ed1,ed2,elx,ely)
      do k=1,im
        maglatd=maglat(k)/pi*180.
!hmhj 
        jref=0
        dy=0.0
        do i=0,nmlat-1
!hmhj     if(maglatd.ge.ylatm1(i)-90..and.maglatd.le.ylatm1(i+1)-90.)   &
          if(maglatd.ge.ylatm (i)-90..and.maglatd.le.ylatm (i+1)-90.)   &
     &then
            jref=i
!hmhj       dy=(maglatd-ylatm1(i)+90.)/(ylatm1(i+1)-ylatm1(i))
            dy=(maglatd-ylatm (i)+90.)/(ylatm (i+1)-ylatm (i))
          endif
        enddo 
!       print*,'wwwlat',k,maglatd,jref,dy
!       maglond=maglon(k)/pi*180.
        maglond=essa(k)+180.
        if(maglond.lt.0.) maglond=maglond+360.
        if(maglond.gt.360.) maglond=maglond-360.
!hmhj 
        iref=0
        dx=0.0
        do i=0,nmlon-1
!hmhj     if(maglond.ge.ylonm1(i).and.maglond.le.ylonm1(i+1)) then
          if(maglond.ge.ylonm (i).and.maglond.le.ylonm (i+1)) then
            iref=i
!hmhj       dx=(maglond-ylonm1(i))/(ylonm1(i+1)-ylonm1(i))
            dx=(maglond-ylonm (i))/(ylonm (i+1)-ylonm (i))
          endif
        enddo 
!       print*,'wwwlon',k,maglond,iref,dx
        aa=(1.-dx)*ed11(iref,jref)+dx*ed11(iref+1,jref)
        bb=(1.-dx)*ed11(iref,jref+1)+dx*ed11(iref+1,jref+1)
        ee1(k)=(1.-dy)*aa+dy*bb
        aa=(1.-dx)*ed22(iref,jref)+dx*ed22(iref+1,jref)
        bb=(1.-dx)*ed22(iref,jref+1)+dx*ed22(iref+1,jref+1)
        ee2(k)=(1.-dy)*aa+dy*bb
!       if(ely(k).gt.100.) print*,'ely',utsec,ed22(iref,jref),          &
!    &ed22(iref+1,jref),ed22(iref+1,jref),ed22(iref+1,jref+1),          &
!    &maglond,maglatd,iref,jref
!       if(ely(k).gt.100.) ely(k)=0.
      enddo
!     ee1=1000.*ee1
!     ee2=1000.*ee2
! correct direction? 365.25day?
      return
      end
!
!r===========================================================
!r=           Earth Electric and Magnetic Field
!r===========================================================
!r
      SUBROUTINE getmag(ix,im,utsec,rlat,rlon,sda,                      &
     &btot,dipang,maglon,maglat,essa)

      use physcons, pi => con_pi
      IMPLICIT NONE

!     REAL(prcn) high_lat_limit  Limit in degrees above 
!     which foster used. Below this limit, Richmond 
!     field used
!     real, PARAMETER ::high_lat_limit=60.
      real, PARAMETER:: R0e = 6.370E06
      real, PARAMETER:: DTR = pi/180.
      real, PARAMETER:: ELCH = 1.062e-19
! Input parameters
      INTEGER,    INTENT(IN)     :: ix  !longitude dimension
      INTEGER,    INTENT(IN)     :: im  !number of longitude
      REAL,       INTENT(IN)     :: utsec !UT second
      REAL,       INTENT(IN)     :: rlat(im)  ! geo latitude (rad)
      REAL,       INTENT(IN)     :: rlon(im)  ! geo longitude (rad)
      REAL,       INTENT(IN)     :: sda  ! solar diclination angle (rad)
! Output Magnetic and electric parameters 
!     REAL, INTENT(OUT) :: elx(im)
!     REAL, INTENT(OUT) :: ely(im)     !electric field
      REAL, INTENT(OUT) :: maglon(im)  !magnetic longitude (rad)
      REAL, INTENT(OUT) :: maglat(im)  !magnetic latitude (rad)
      REAL, INTENT(OUT) :: btot(im)    !mapgnetic field strength
      REAL, INTENT(OUT) :: dipang(im)  !Dip angle (degree)
      REAL, INTENT(OUT) :: essa(im)    !magnetic local time
! Local 
      real cormag(im),cmorg(im)
      integer i
! set elx ely zero first
!     elx=0.
!     ely=0.
! get cormag btot dipang in grid
      call interp_field(ix,im,rlat,rlon,cormag,btot,dipang)
! get maglon,maglat
      call SPOLE(ix,im,RLAT,rlon,utsec,SDA,maglon,ESSA,CMORG)
      do i=1,im
      maglat(i)=pi/2.-cormag(i)*DTR
      enddo
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine interp_field(ix,im,rlat,rlon,cormago,btoto,dipango)
      implicit none
      integer,intent(in)  :: ix            ! max number of longitude
      integer,intent(in)  :: im            ! number of longitude
      real,   intent(in)  :: rlat(im)      ! latitude (rad)
      real,   intent(in)  :: rlon(im)       ! longitude (rad)
      real,   intent(out) :: cormago(im),btoto(im),dipango(im) ! field value 
! local variable
      real cormag(20,91),btot(20,91),dipang(20,91),glat(91),glon(20)
      real dll,dl,ddlat,ddlon,a1,a2,b1,b2,aa,bb
      integer i,iref,jref,jref1
      data cormag/163.68,163.68,163.68,163.68,163.68,                   &
     &163.68,163.68,163.68,163.68,163.68,                               &
     &163.68,163.68,163.68,163.68,163.68,                               &
     &163.68,163.68,163.68,163.68,163.68,                               &
     &162.60,163.12,163.64,164.18,164.54,                               &
     &164.90,165.16,165.66,166.00,165.86,                               &
     &165.20,164.38,163.66,162.94,162.42,                               &
     &162.00,161.70,161.70,161.80,162.14,                               &
     &161.20,162.18,163.26,164.44,165.62,                               &
     &166.60,167.42,167.80,167.38,166.82,                               &
     &166.00,164.66,163.26,162.16,161.18,                               &
     &160.40,159.94,159.80,159.98,160.44,                               &
     &159.80,161.14,162.70,164.50,166.26,                               &
     &167.90,169.18,169.72,169.36,168.24,                               &
     &166.70,164.80,162.90,161.18,159.74,                               &
     &158.60,157.94,157.80,157.98,158.72,                               &
     &158.40,160.10,162.02,164.28,166.64,                               &
     &169.00,170.80,171.72,171.06,169.46,                               &
     &167.10,164.64,162.18,160.02,158.20,                               &
     &156.80,156.04,155.80,156.16,157.02,                               &
     &157.00,158.96,161.24,163.86,166.72,                               &
     &169.80,172.42,173.72,172.82,170.34,                               &
     &167.30,164.22,161.34,158.74,156.60,                               &
     &155.00,154.08,153.90,154.36,155.36,                               &
     &155.50,157.72,160.36,163.32,166.60,                               &
     &170.20,173.70,175.64,174.18,170.80,                               &
     &167.10,163.56,160.24,157.36,154.96,                               &
     &153.10,152.08,151.92,152.46,153.76,                               &
     &154.10,156.52,159.36,162.52,166.24,                               &
     &170.30,174.62,177.48,175.04,170.82,                               &
     &166.60,162.70,159.02,155.88,153.22,                               &
     &151.20,150.08,149.92,150.64,152.20,                               &
     &152.80,155.32,158.28,161.70,165.58,                               &
     &170.00,174.84,178.46,175.18,170.38,                               &
     &165.80,161.64,157.80,154.38,151.52,                               &
     &149.30,148.18,148.02,148.92,150.60,                               &
     &151.40,154.08,157.18,160.68,164.78,                               &
     &169.40,174.34,177.44,174.28,169.44,                               &
     &164.70,160.34,156.30,152.78,149.72,                               &
     &147.40,146.18,146.04,147.12,149.04,                               &
     &150.10,152.88,156.00,159.58,163.78,                               &
     &168.50,173.28,175.60,172.86,168.14,                               &
     &163.40,158.98,154.88,151.10,147.98,                               &
     &145.50,144.18,144.14,145.40,147.48,                               &
     &148.80,151.68,154.88,158.48,162.68,                               &
     &167.40,171.76,173.60,171.12,166.68,                               &
     &162.00,157.48,153.28,149.50,146.18,                               &
     &143.50,142.18,142.24,143.68,145.98,                               &
     &147.50,150.54,153.68,157.28,161.42,                               &
     &166.10,170.10,171.48,169.22,164.98,                               &
     &160.40,155.88,151.68,147.80,144.34,                               &
     &141.60,140.18,140.26,141.98,144.62,                               &
     &146.30,149.34,152.48,155.98,160.08,                               &
     &164.60,168.34,169.38,167.20,163.18,                               &
     &158.60,154.18,149.98,146.02,142.54,                               &
     &139.70,138.18,138.46,140.26,143.16,                               &
     &145.10,148.14,151.18,154.60,158.68,                               &
     &163.10,166.48,167.28,165.18,161.32,                               &
     &156.90,152.48,148.28,144.32,140.74,                               &
     &137.80,136.22,136.48,138.64,141.76,                               &
     &143.90,146.98,149.98,153.30,157.24,                               &
     &161.40,164.52,165.16,162.86,159.42,                               &
     &155.00,150.68,146.48,142.52,138.94,                               &
     &135.90,134.22,134.68,137.02,140.40,                               &
     &142.70,145.84,148.76,151.92,155.74,                               &
     &159.70,162.52,162.96,160.98,157.42,                               &
     &153.10,148.84,144.68,140.82,137.20,                               &
     &134.00,132.32,132.80,135.42,139.10,                               &
     &141.60,144.74,147.46,150.52,154.20,                               &
     &158.00,160.46,160.76,158.86,155.36,                               &
     &151.20,146.94,142.88,139.02,135.40,                               &
     &132.10,130.32,131.00,133.80,137.74,                               &
     &140.50,143.58,146.24,149.12,152.60,                               &
     &156.20,158.40,158.66,156.76,153.36,                               &
     &149.30,145.04,141.08,137.30,133.60,                               &
     &130.30,128.42,129.12,132.28,136.44,                               &
     &139.30,142.48,144.94,147.64,150.48,                               &
     &154.30,156.34,156.36,154.56,151.26,                               &
     &147.30,143.14,139.20,135.50,131.90,                               &
     &128.40,126.52,127.32,130.76,135.18,                               &
     &138.20,141.28,143.72,146.24,149.26,                               &
     &152.40,154.24,154.16,152.36,149.16,                               &
     &145.30,141.24,137.30,133.70,130.10,                               &
     &126.60,124.62,125.54,129.16,133.92,                               &
     &137.10,140.18,142.42,144.66,147.62,                               &
     &150.50,152.18,151.96,150.16,147.10,                               &
     &143.30,139.24,135.50,131.90,128.36,                               &
     &124.80,122.72,123.74,127.64,132.62,                               &
     &135.90,139.02,141.12,143.18,145.92,                               &
     &148.60,149.98,149.76,148.04,145.00,                               &
     &141.20,137.30,133.60,130.10,126.60,                               &
     &123.00,120.86,121.96,126.12,131.36,                               &
     &134.80,137.88,139.80,141.68,144.08,                               &
     &146.60,147.88,147.56,145.84,142.90,                               &
     &139.20,135.30,131.70,128.28,124.86,                               &
     &121.30,118.96,120.18,124.70,130.16,                               &
     &133.60,136.72,138.48,140.10,142.38,                               &
     &144.60,145.72,145.34,143.64,140.80,                               &
     &137.10,133.30,129.72,126.48,123.10,                               &
     &119.50,117.16,118.48,123.18,128.86,                               &
     &132.40,135.42,137.08,138.50,140.54,                               &
     &142.60,143.52,143.06,141.44,138.70,                               &
     &135.10,131.30,127.82,124.58,121.40,                               &
     &117.70,115.26,116.70,121.66,127.60,                               &
     &131.20,134.22,135.66,136.82,138.70,                               &
     &140.60,141.36,140.86,139.24,136.50,                               &
     &133.00,129.30,125.92,122.78,119.60,                               &
     &116.00,113.40,114.92,120.16,126.30,                               &
     &130.00,132.92,134.24,135.14,136.80,                               &
     &138.60,139.16,138.64,137.12,134.40,                               &
     &130.90,127.20,123.92,120.96,117.90,                               &
     &114.20,111.56,113.12,118.64,124.90,                               &
     &128.70,131.56,132.74,133.44,134.90,                               &
     &136.50,137.00,136.36,134.82,132.30,                               &
     &128.70,125.16,121.94,119.06,116.10,                               &
     &112.50,109.70,111.42,117.14,123.60,                               &
     &127.30,130.16,131.22,131.66,133.00,                               &
     &134.50,134.80,134.14,132.62,130.14,                               &
     &126.60,123.06,119.94,117.16,114.30,                               &
     &110.70,107.80,109.64,115.62,122.24,                               &
     &125.90,128.76,129.62,129.96,131.06,                               &
     &132.40,132.60,131.86,130.42,128.00,                               &
     &124.50,120.96,117.96,115.26,112.54,                               &
     &108.90,105.94,107.86,114.02,120.84,                               &
     &124.05,126.79,127.55,127.83,128.90,                               &
     &130.21,130.41,129.71,128.33,125.96,                               &
     &122.49,118.96,115.97,113.26,110.52,                               &
     &106.89,104.01,106.00,112.21,119.06,                               &
     &122.19,124.82,125.48,125.69,126.73,                               &
     &128.03,128.22,127.55,126.23,123.92,                               &
     &120.47,116.97,113.97,111.26,108.50,                               &
     &104.89,102.08,104.14,110.41,117.29,                               &
     &120.34,122.85,123.40,123.56,124.57,                               &
     &125.84,126.03,125.40,124.14,121.88,                               &
     &118.46,114.97,111.98,109.26,106.48,                               &
     &102.88,100.15,102.28,108.60,115.51,                               &
     &118.49,120.88,121.33,121.42,122.40,                               &
     &123.65,123.84,123.24,122.04,119.83,                               &
     &116.45,112.97,109.98,107.26,104.46,                               &
     &100.87, 98.22,100.42,106.79,113.74,                               &
     &116.63,118.91,119.26,119.29,120.24,                               &
     &121.47,121.65,121.09,119.95,117.79,                               &
     &114.43,110.98,107.99,105.26,102.44,                               &
     & 98.87, 96.29, 98.56,104.98,111.96,                               &
     &114.78,116.94,117.19,117.15,118.07,                               &
     &119.28,119.46,118.93,117.86,115.75,                               &
     &112.42,108.98,106.00,103.26,100.42,                               &
     & 96.86, 94.36, 96.70,103.18,110.19,                               &
     &112.93,114.97,115.12,115.02,115.91,                               &
     &117.09,117.27,116.78,115.76,113.71,                               &
     &110.41,106.98,104.00,101.26, 98.40,                               &
     & 94.85, 92.43, 94.84,101.37,108.41,                               &
     &111.07,113.00,113.04,112.88,113.74,                               &
     &114.91,115.08,114.62,113.67,111.67,                               &
     &108.39,104.99,102.01, 99.26, 96.38,                                &
     & 92.85, 90.51, 92.97, 99.56,106.64,                               &
     &109.22,111.03,110.97,110.75,111.58,                               &
     &112.72,112.89,112.47,111.57,109.63,                               &
     &106.38,102.99,100.01, 97.26, 94.36,                                &
     & 90.84, 88.58, 91.11, 97.75,104.86,                               &
     &107.37,109.06,108.90,108.61,109.41,                               &
     &110.53,110.70,110.31,109.48,107.59,                               &
     &104.37,100.99, 98.02, 95.26, 92.34,                               &
     & 88.83, 86.65, 89.25, 95.95,103.09,                               &
     &105.51,107.09,106.83,106.48,107.25,                               &
     &108.35,108.51,108.16,107.39,105.55,                               &
     &102.35, 99.00, 96.03, 93.26, 90.32,                               &
     & 86.83, 84.72, 87.39, 94.14,101.31,                               &
     &103.66,105.12,104.76,104.34,105.08,                               &
     &106.16,106.32,106.00,105.29,103.50,                               &
     &100.34, 97.00, 94.03, 91.26, 88.30,                               &
     & 84.82, 82.79, 85.53, 92.33, 99.54,                               &
     &101.81,103.15,102.68,102.21,102.92,                               &
     &103.97,104.13,103.85,103.20,101.46,                               &
     & 98.33, 95.00, 92.04, 89.26, 86.28,                               &
     & 82.81, 80.86, 83.67, 90.52, 97.76,                               &
     & 99.95,101.18,100.61,100.07,100.75,                               &
     &101.79,101.94,101.69,101.10, 99.42,                               &
     & 96.31, 93.01, 90.04, 87.26, 84.26,                               &
     & 80.81, 78.93, 81.81, 88.72, 95.99,                               &
     & 98.10, 99.21, 98.54, 97.94, 98.59,                               &
     & 99.60, 99.75, 99.54, 99.01, 97.38,                               &
     & 94.30, 91.01, 88.05, 85.26, 82.24,                               &
     & 78.80, 77.00, 79.95, 86.91, 94.21,                               &
     & 96.25, 97.24, 96.47, 95.81, 96.43,                               &
     & 97.41, 97.56, 97.39, 96.92, 95.34,                               &
     & 92.29, 89.01, 86.06, 83.26, 80.22,                               &
     & 76.79, 75.07, 78.09, 85.10, 92.43,                               &
     & 94.39, 95.27, 94.40, 93.67, 94.26,                               &
     & 95.23, 95.37, 95.23, 94.82, 93.30,                               &
     & 90.27, 87.02, 84.06, 81.26, 78.20,                               &
     & 74.79, 73.14, 76.23, 83.30, 90.66,                               &
     & 92.54, 93.30, 92.32, 91.54, 92.10,                               &
     & 93.04, 93.18, 93.08, 92.73, 91.26,                               &
     & 88.26, 85.02, 82.07, 79.26, 76.18,                               &
     & 72.78, 71.21, 74.37, 81.49, 88.88,                               &
     & 90.69, 91.33, 90.25, 89.40, 89.93,                               &
     & 90.85, 90.99, 90.92, 90.63, 89.21,                               &
     & 86.25, 83.02, 80.07, 77.26, 74.16,                               &
     & 70.77, 69.28, 72.51, 79.68, 87.11,                               &
     & 88.83, 89.36, 88.18, 87.27, 87.77,                               &
     & 88.67, 88.80, 88.77, 88.54, 87.17,                               &
     & 84.23, 81.03, 78.08, 75.26, 72.14,                               &
     & 68.77, 67.35, 70.65, 77.87, 85.33,                               &
     & 86.98, 87.39, 86.11, 85.13, 85.60,                               &
     & 86.48, 86.61, 86.61, 86.45, 85.13,                               &
     & 82.22, 79.03, 76.09, 73.26, 70.12,                               &
     & 66.76, 65.42, 68.79, 76.07, 83.56,                               &
     & 85.13, 85.42, 84.04, 83.00, 83.44,                               &
     & 84.29, 84.42, 84.46, 84.35, 83.09,                               &
     & 80.21, 77.03, 74.09, 71.26, 68.10,                               &
     & 64.75, 63.49, 66.93, 74.26, 81.78,                               &
     & 83.27, 83.45, 81.96, 80.86, 81.27,                               &
     & 82.11, 82.23, 82.30, 82.26, 81.05,                               &
     & 78.19, 75.04, 72.10, 69.26, 66.08,                               &
     & 62.75, 61.57, 65.06, 72.45, 80.01,                               &
     & 81.42, 81.48, 79.89, 78.73, 79.11,                               &
     & 79.92, 80.04, 80.15, 80.16, 79.01,                               &
     & 76.18, 73.04, 70.10, 67.26, 64.06,                               &
     & 60.74, 59.64, 63.20, 70.64, 78.23,                               &
     & 79.57, 79.51, 77.82, 76.59, 76.94,                               &
     & 77.73, 77.85, 77.99, 78.07, 76.97,                               &
     & 74.17, 71.04, 68.11, 65.26, 62.04,                               &
     & 58.73, 57.71, 61.34, 68.84, 76.46,                               &
     & 77.71, 77.54, 75.75, 74.46, 74.78,                               &
     & 75.55, 75.66, 75.84, 75.98, 74.93,                               &
     & 72.15, 69.05, 66.12, 63.26, 60.02,                               &
     & 56.73, 55.78, 59.48, 67.03, 74.68,                               &
     & 75.86, 75.57, 73.68, 72.32, 72.61,                               &
     & 73.36, 73.47, 73.68, 73.88, 72.88,                               &
     & 70.14, 67.05, 64.12, 61.26, 58.00,                               &
     & 54.72, 53.85, 57.62, 65.22, 72.91,                               &
     & 74.01, 73.60, 71.60, 70.19, 70.45,                               &
     & 71.17, 71.28, 71.53, 71.79, 70.84,                               &
     & 68.13, 65.05, 62.13, 59.26, 55.98,                               &
     & 52.71, 51.92, 55.76, 63.41, 71.13,                               &
     & 72.15, 71.63, 69.53, 68.05, 68.28,                               &
     & 68.99, 69.09, 69.37, 69.69, 68.80,                               &
     & 66.11, 63.06, 60.13, 57.26, 53.96,                               &
     & 50.71, 49.99, 53.90, 61.61, 69.36,                               &
     & 70.30, 69.66, 67.46, 65.92, 66.12,                               &
     & 66.80, 66.90, 67.22, 67.60, 66.76,                               &
     & 64.10, 61.06, 58.14, 55.26, 51.94,                               &
     & 48.70, 48.06, 52.04, 59.80, 67.58,                               &
     & 67.70, 67.06, 65.08, 63.72, 63.98,                               &
     & 64.60, 64.80, 65.12, 65.60, 64.86,                               &
     & 62.40, 59.26, 56.24, 53.18, 49.84,                               &
     & 46.60, 46.12, 50.12, 57.52, 64.80,                               &
     & 64.90, 64.42, 62.70, 61.62, 61.78,                               &
     & 62.40, 62.60, 63.04, 63.58, 63.00,                               &
     & 60.60, 57.46, 54.42, 51.18, 47.70,                               &
     & 44.60, 44.22, 48.02, 55.06, 61.92,                               &
     & 62.10, 61.72, 60.32, 59.50, 59.68,                               &
     & 60.20, 60.46, 60.94, 61.58, 61.00,                               &
     & 58.70, 55.66, 52.52, 49.18, 45.60,                               &
     & 42.50, 42.22, 46.00, 52.60, 58.98,                               &
     & 59.20, 59.18, 58.12, 57.32, 57.48,                               &
     & 58.00, 58.30, 58.84, 59.48, 59.04,                               &
     & 56.90, 53.86, 50.62, 47.10, 43.50,                               &
     & 40.50, 40.28, 43.98, 50.22, 56.18,                               &
     & 56.40, 56.64, 55.84, 55.20, 55.38,                               &
     & 55.80, 56.16, 56.84, 57.48, 57.04,                               &
     & 55.10, 52.06, 48.70, 45.10, 41.40,                               &
     & 38.40, 38.28, 41.88, 47.94, 53.44,                               &
     & 53.70, 54.14, 53.56, 53.10, 53.24,                               &
     & 53.70, 54.06, 54.74, 55.38, 55.14,                               &
     & 53.20, 50.26, 46.80, 43.10, 39.34,                               &
     & 36.40, 36.38, 39.96, 45.56, 50.84,                               &
     & 51.10, 51.70, 51.36, 51.00, 51.14,                               &
     & 51.50, 51.96, 52.64, 53.38, 53.08,                               &
     & 51.30, 48.36, 44.90, 41.02, 37.24,                               &
     & 34.40, 34.38, 37.86, 43.28, 48.20,                               &
     & 48.50, 49.26, 49.18, 48.90, 49.04,                               &
     & 49.40, 49.86, 50.64, 51.28, 51.08,                               &
     & 49.40, 46.46, 42.98, 39.02, 35.14,                               &
     & 32.40, 32.48, 35.72, 41.00, 45.70,                               &
     & 46.00, 46.96, 46.98, 46.80, 46.94,                               &
     & 47.30, 47.76, 48.54, 49.28, 49.08,                               &
     & 47.40, 44.56, 41.08, 37.02, 33.14,                               &
     & 30.40, 30.58, 33.84, 38.72, 43.20,                               &
     & 43.50, 44.62, 44.80, 44.80, 44.94,                               &
     & 45.20, 45.76, 46.54, 47.18, 46.98,                               &
     & 45.50, 42.66, 39.08, 35.02, 31.14,                               &
     & 28.40, 28.58, 31.82, 36.52, 40.80,                               &
     & 41.20, 42.32, 42.54, 42.70, 42.84,                               &
     & 43.20, 43.66, 44.44, 45.08, 44.98,                               &
     & 43.50, 40.76, 37.08, 33.04, 29.04,                               &
     & 26.40, 26.68, 29.82, 34.34, 38.40,                               &
     & 38.80, 40.12, 40.60, 40.70, 40.84,                               &
     & 41.10, 41.62, 42.34, 42.98, 42.88,                               &
     & 41.50, 38.76, 35.18, 31.04, 27.14,                               &
     & 24.50, 24.78, 27.70, 32.14, 36.06,                               &
     & 36.50, 37.88, 38.50, 38.68, 38.84,                               &
     & 39.10, 39.56, 40.34, 40.88, 40.82,                               &
     & 39.40, 36.76, 33.18, 29.12, 25.14,                               &
     & 22.50, 22.88, 25.90, 29.96, 33.86,                               &
     & 34.30, 35.68, 36.42, 36.68, 36.84,                               &
     & 37.10, 37.56, 38.24, 38.88, 38.72,                               &
     & 37.40, 34.76, 31.18, 27.12, 23.14,                               &
     & 20.60, 20.98, 23.90, 27.88, 31.66,                               &
     & 32.10, 33.58, 34.32, 34.68, 34.84,                               &
     & 35.10, 35.56, 36.24, 36.78, 36.62,                               &
     & 35.30, 32.72, 29.18, 25.14, 21.24,                               &
     & 18.70, 19.08, 21.90, 25.88, 29.42,                               &
     & 29.90, 31.48, 32.32, 32.68, 32.84,                               &
     & 33.10, 33.56, 34.22, 34.68, 34.42,                               &
     & 33.20, 30.72, 27.28, 23.22, 19.34,                               &
     & 16.80, 17.24, 20.00, 23.78, 27.32,                               &
     & 27.70, 29.38, 30.24, 30.68, 30.94,                               &
     & 31.20, 31.66, 32.22, 32.58, 32.32,                               &
     & 31.10, 28.62, 25.28, 21.32, 17.48,                               &
     & 15.00, 15.38, 18.18, 21.80, 25.22,                               &
     & 25.70, 27.28, 28.24, 28.78, 29.04,                               &
     & 29.30, 29.66, 30.22, 30.50, 30.22,                               &
     & 29.00, 26.62, 23.30, 19.42, 15.64,                               &
     & 13.10, 13.54, 16.28, 19.80, 23.12,                               &
     & 23.60, 25.24, 26.24, 26.78, 27.14,                               &
     & 27.40, 27.76, 28.22, 28.40, 28.12,                               &
     & 26.80, 24.52, 21.30, 17.52, 13.78,                               &
     & 11.30, 11.74, 14.48, 17.90, 21.12,                               &
     & 21.60, 23.24, 24.34, 24.88, 25.24,                               &
     & 25.50, 25.86, 26.22, 26.40, 25.98,                               &
     & 24.70, 22.48, 19.40, 15.72, 12.04,                               &
     &  9.50,  9.94, 12.58, 16.02, 19.12,                               &
     & 19.60, 21.24, 22.34, 22.98, 23.34,                               &
     & 23.70, 24.00, 24.30, 24.40, 23.88,                               &
     & 22.60, 20.48, 17.52, 14.00, 10.34,                               &
     &  7.80,  8.18, 10.88, 14.22, 17.18,                               &
     & 17.60, 19.34, 20.44, 21.16, 21.54,                               &
     & 21.90, 22.16, 22.40, 22.32, 21.78,                               &
     & 20.60, 18.48, 15.62, 12.20,  8.68,                               &
     &  6.00,  6.44,  9.18, 12.42, 15.28,                               &
     & 15.80, 17.44, 18.54, 19.26, 19.74,                               &
     & 20.10, 20.30, 20.50, 20.32, 19.72,                               &
     & 18.50, 16.54, 13.84, 10.68,  7.14,                               &
     &  4.40,  4.74,  7.58, 10.74, 13.48,                               &
     & 14.00, 15.54, 16.74, 17.46, 17.94,                               &
     & 18.30, 18.50, 18.58, 18.32, 17.72,                               &
     & 16.50, 14.64, 12.24,  9.18,  5.84,                               &
     &  2.90,  3.30,  6.16,  9.14, 11.84,                               &
     & 12.30, 13.78, 14.94, 15.66, 16.24,                               &
     & 16.50, 16.70, 16.70, 16.42, 15.78,                               &
     & 14.60, 12.90, 10.66,  7.86,  4.88,                               &
     &  1.60,  1.72,  4.96,  7.84, 10.24,                               &
     & 10.70, 12.14, 13.24, 13.96, 14.44,                               &
     & 14.80, 14.90, 14.88, 14.52, 13.92,                               &
     & 12.80, 11.30,  9.28,  6.94,  4.32,                               &
     &  1.80,  1.94,  4.34,  6.78,  8.94,                               &
     &  9.40, 10.58, 11.64, 12.36, 12.74,                               &
     & 13.10, 13.20, 13.08, 12.72, 12.12,                               &
     & 11.10,  9.86,  8.30,  6.50,  4.60,                               &
     &  3.10,  3.16,  4.50,  6.20,  7.90,                               &
     &  8.40,  9.42, 10.14, 10.76, 11.14,                               &
     & 11.40, 11.40, 11.38, 11.02, 10.46,                               &
     &  9.70,  8.72,  7.64,  6.46,  5.42,                               &
     &  4.60,  4.70,  5.34,  6.24,  7.36,                               &
     &  7.90,  8.46,  8.92,  9.28,  9.54,                               &
     &  9.70,  9.70,  9.68,  9.42,  9.06,                               &
     &  8.60,  8.08,  7.56,  7.02,  6.56,                               &
     & 6.30,  6.30,  6.52,  6.96,  7.38,                                &
     & 8.15,  8.15,  8.15,  8.15,  8.15,                                &
     & 8.15,  8.15,  8.15,  8.15,  8.15,                                &
     & 8.15,  8.15,  8.15,  8.15,  8.15,                                &
     & 8.15,  8.15,  8.15,  8.15,  8.15/
!btot                               
      data btot/49163.,49163.,49163.,49162.,49162.,                     &
     &49162.,49162.,49162.,49162.,49162.,                               &
     &49162.,49162.,49162.,49162.,49162.,                               &
     &49162.,49163.,49163.,49163.,49163.,                               &
     &47958.,48108.,48361.,48693.,49069.,                               &
     &49452.,49801.,50081.,50266.,50338.,                               &
     &50293.,50136.,49884.,49561.,49197.,                               &
     &48826.,48484.,48202.,48009.,47924.,                               &
     &46690.,46983.,47489.,48160.,48925.,                               &
     &49701.,50403.,50959.,51316.,51444.,                               &
     &51340.,51020.,50519.,49882.,49167.,                               &
     &48438.,47761.,47200.,46810.,46633.,                               &
     &45370.,45799.,46556.,47571.,48736.,                               &
     &49915.,50974.,51796.,52306.,52470.,                               &
     &52293.,51806.,51060.,50121.,49067.,                               &
     &47991.,46989.,46155.,45571.,45296.,                               &
     &44008.,44566.,45571.,46935.,48509.,                               &
     &50100.,51514.,52591.,53233.,53410.,                               &
     &53144.,52486.,51503.,50273.,48893.,                               &
     &47481.,46165.,45068.,44295.,43925.,                               &
     &42618.,43297.,44545.,46261.,48252.,                               &
     &50261.,52026.,53341.,54091.,54257.,                               &
     &53887.,53055.,51842.,50335.,48643.,                               &
     &46906.,45286.,43939.,42989.,42527.,                               &
     &41210.,42003.,43488.,45557.,47970.,                               &
     &50400.,52511.,54044.,54875.,55005.,                               &
     &54515.,53509.,52077.,50308.,48315.,                               &
     &46263.,44352.,42770.,41658.,41113.,                               &
     &39796.,40694.,42410.,44830.,47669.,                               &
     &50520.,52966.,54697.,55580.,55649.,                               &
     &55025.,53844.,52206.,50190.,47910.,                               &
     &45553.,43364.,41564.,40310.,39693.,                               &
     &38387.,39383.,41321.,44089.,47353.,                               &
     &50622.,53389.,55294.,56201.,56184.,                               &
     &55414.,54061.,52229.,49984.,47430.,                               &
     &44779.,42323.,40326.,38951.,38278.,                               &
     &36996.,38080.,40230.,43340.,47027.,                               &
     &50705.,53778.,55830.,56734.,56609.,                               &
     &55682.,54158.,52149.,49693.,46879.,                               &
     &43944.,41235.,39062.,37591.,36878.,                               &
     &35633.,36796.,39144.,42589.,46691.,                               &
     &50769.,54128.,56301.,57175.,56922.,                               &
     &55828.,54140.,51968.,49320.,46263.,                               &
     &43054.,40106.,37779.,36238.,35504.,                               &
     &34309.,35539.,38072.,41841.,46349.,                               &
     &50811.,54434.,56702.,57521.,57123.,                               &
     &55858.,54010.,51690.,48872.,45587.,                               &
     &42117.,38943.,36486.,34903.,34167.,                               &
     &33035.,34318.,37021.,41101.,46002.,                               &
     &50830.,54692.,57028.,57771.,57216.,                               &
     &55775.,53772.,51322.,48353.,44859.,                               &
     &41141.,37757.,35193.,33597.,32878.,                               &
     &31822.,33144.,35995.,40371.,45649.,                               &
     &50822.,54897.,57275.,57923.,57204.,                               &
     &55585.,53434.,50868.,47770.,44086.,                               &
     &40136.,36558.,33910.,32329.,31647.,                               &
     &30678.,32022.,35001.,39655.,45291.,                               &
     &50785.,55045.,57440.,57978.,57090.,                               &
     &55296.,53003.,50335.,47128.,43277.,                               &
     &39112.,35357.,32649.,31110.,30485.,                               &
     &29614.,30961.,34043.,38954.,44928.,                               &
     &50716.,55134.,57521.,57940.,56883.,                               &
     &54916.,52487.,49731.,46436.,42438.,                               &
     &38079.,34165.,31419.,29950.,29401.,                               &
     &28635.,29968.,33125.,38271.,44559.,                               &
     &50615.,55160.,57519.,57809.,56587.,                               &
     &54454.,51895.,49062.,45697.,41578.,                               &
     &37048.,32994.,30232.,28858.,28403.,                               &
     &27749.,29049.,32252.,37607.,44185.,                               &
     &50478.,55121.,57432.,57592.,56210.,                               &
     &53918.,51235.,48336.,44919.,40704.,                               &
     &36027.,31854.,29097.,27840.,27495.,                               &
     &26960.,28210.,31427.,36965.,43805.,                               &
     &50305.,55019.,57262.,57291.,55761.,                               &
     &53318.,50518.,47560.,44107.,39821.,                               &
     &35027.,30756.,28023.,26903.,26682.,                               &
     &26273.,27457.,30656.,36345.,43419.,                               &
     &50097.,54851.,57012.,56912.,55245.,                               &
     &52662.,49751.,46741.,43266.,38935.,                               &
     &34053.,29708.,27017.,26051.,25965.,                               &
     &25687.,26794.,29943.,35751.,43028.,                               &
     &49852.,54620.,56685.,56460.,54670.,                               &
     &51957.,48942.,45886.,42401.,38050.,                               &
     &33113.,28718.,26085.,25286.,25344.,                               &
     &25202.,26226.,29291.,35183.,42633.,                               &
     &49572.,54327.,56283.,55940.,54041.,                               &
     &51211.,48098.,45001.,41517.,37171.,                               &
     &32212.,27792.,25231.,24606.,24816.,                               &
     &24815.,25756.,28707.,34646.,42234.,                               &
     &49256.,53973.,55809.,55357.,53363.,                               &
     &50429.,47227.,44092.,40617.,36301.,                               &
     &31353.,26933.,24457.,24011.,24375.,                               &
     &24522.,25386.,28194.,34140.,41832.,                               &
     &48906.,53560.,55268.,54715.,52641.,                               &
     &49616.,46333.,43164.,39705.,35442.,                               &
     &30539.,26146.,23765.,23496.,24015.,                               &
     &24317.,25114.,27757.,33669.,41428.,                               &
     &48523.,53090.,54662.,54018.,51878.,                               &
     &48774.,45420.,42222.,38786.,34597.,                               &
     &29772.,25432.,23153.,23057.,23727.,                               &
     &24191.,24940.,27398.,33234.,41022.,                               &
     &48107.,52565.,53995.,53268.,51075.,                               &
     &47907.,44494.,41272.,37864.,33769.,                               &
     &29052.,24790.,22620.,22686.,23505.,                               &
     &24137.,24860.,27120.,32838.,40615.,                               &
     &47658.,51986.,53270.,52468.,50234.,                               &
     &47015.,43556.,40316.,36943.,32961.,                               &
     &28381.,24221.,22162.,22378.,23338.,                               &
     &24146.,24868.,26921.,32481.,40207.,                               &
     &47178.,51356.,52488.,51620.,49355.,                               &
     &46100.,42609.,39360.,36029.,32176.,                               &
     &27759.,23724.,21777.,22126.,23219.,                               &
     &24207.,24954.,26800.,32163.,39797.,                               &
     &46665.,50675.,51654.,50727.,48441.,                               &
     &45163.,41655.,38408.,35126.,31417.,                               &
     &27187.,23296.,21461.,21924.,23141.,                               &
     &24312.,25109.,26753.,31884.,39386.,                               &
     &46121.,49947.,50770.,49789.,47490.,                               &
     &44204.,40697.,37463.,34240.,30689.,                               &
     &26664.,22938.,21210.,21767.,23097.,                               &
     &24450.,25321.,26772.,31640.,38972.,                               &
     &45546.,49172.,49839.,48809.,46506.,                               &
     &43226.,39738.,36531.,33377.,29996.,                               &
     &26194.,22647.,21021.,21649.,23081.,                               &
     &24614.,25577.,26849.,31429.,38554.,                               &
     &44939.,48353.,48865.,47791.,45489.,                               &
     &42230.,38779.,35615.,32544.,29343.,                               &
     &25778.,22424.,20891.,21567.,23090.,                               &
     &24794.,25862.,26974.,31247.,38131.,                               &
     &44303.,47494.,47853.,46738.,44443.,                               &
     &41219.,37825.,34722.,31747.,28737.,                               &
     &25418.,22269.,20818.,21518.,23119.,                               &
     &24983.,26164.,27134.,31089.,37702.,                               &
     &43637.,46598.,46808.,45655.,43373.,                               &
     &40199.,36880.,33857.,30994.,28183.,                               &
     &25119.,22182.,20801.,21501.,23165.,                               &
     &25172.,26469.,27318.,30951.,37267.,                               &
     &42946.,45672.,45738.,44547.,42283.,                               &
     &39173.,35949.,33026.,30292.,27687.,                               &
     &24885.,22164.,20840.,21514.,23226.,                               &
     &25357.,26764.,27513.,30826.,36826.,                               &
     &42231.,44720.,44648.,43423.,41181.,                               &
     &38150.,35040.,32237.,29647.,27257.,                               &
     &24720.,22217.,20935.,21557.,23301.,                               &
     &25532.,27039.,27709.,30712.,36380.,                               &
     &41497.,43752.,43550.,42290.,40074.,                               &
     &37136.,34160.,31495.,29068.,26899.,                               &
     &24630.,22344.,21086.,21632.,23388.,                               &
     &25693.,27286.,27897.,30605.,35932.,                               &
     &40751.,42775.,42451.,41158.,38973.,                               &
     &36142.,33317.,30809.,28562.,26619.,                               &
     &24618.,22545.,21294.,21737.,23486.,                               &
     &25837.,27499.,28070.,30504.,35484.,                               &
     &39999.,41799.,41365.,40038.,37887.,                               &
     &35177.,32520.,30187.,28134.,26423.,                               &
     &24690.,22823.,21561.,21877.,23595.,                               &
     &25964.,27675.,28224.,30408.,35044.,                               &
     &39251.,40838.,40301.,38940.,36828.,                               &
     &34251.,31777.,29635.,27791.,26316.,                               &
     &24848.,23177.,21887.,22051.,23714.,                               &
     &26075.,27814.,28357.,30322.,34617.,                               &
     &38518.,39903.,39275.,37878.,35806.,                               &
     &33375.,31098.,29161.,27539.,26304.,                               &
     &25096.,23607.,22273.,22263.,23845.,                               &
     &26172.,27921.,28473.,30248.,34212.,                               &
     &37811.,39008.,38298.,36863.,34834.,                               &
     &32559.,30491.,28770.,27382.,26389.,                               &
     &25435.,24112.,22719.,22515.,23989.,                               &
     &26262.,28001.,28578.,30195.,33841.,                               &
     &37144.,38169.,37384.,35908.,33922.,                               &
     &31812.,29964.,28468.,27322.,26573.,                               &
     &25865.,24691.,23224.,22809.,24150.,                               &
     &26351.,28066.,28678.,30170.,33515.,                               &
     &36531.,37399.,36548.,35026.,33081.,                               &
     &31144.,29521.,28258.,27362.,26858.,                               &
     &26385.,25340.,23788.,23149.,24331.,                               &
     &26447.,28127.,28786.,30183.,33245.,                               &
     &35987.,36715.,35802.,34227.,32320.,                               &
     &30559.,29168.,28142.,27501.,27243.,                               &
     &26992.,26056.,24407.,23535.,24538.,                               &
     &26562.,28198.,28913.,30245.,33045.,                               &
     &35528.,36130.,35159.,33522.,31649.,                               &
     &30064.,28905.,28120.,27738.,27724.,                               &
     &27682.,26834.,25080.,23971.,24776.,                               &
     &26706.,28293.,29072.,30366.,32927.,                               &
     &35167.,35659.,34630.,32922.,31074.,                               &
     &29662.,28734.,28190.,28070.,28299.,                               &
     &28450.,27668.,25803.,24458.,25052.,                               &
     &26889.,28427.,29277.,30555.,32901.,                               &
     &34917.,35312.,34224.,32433.,30599.,                               &
     &29354.,28653.,28351.,28493.,28961.,                               &
     &29290.,28555.,26572.,24996.,25372.,                               &
     &27122.,28613.,29537.,30818.,32975.,                               &
     &34787.,35100.,33949.,32063.,30230.,                               &
     &29139.,28658.,28596.,29001.,29705.,                               &
     &30196.,29488.,27384.,25586.,25744.,                               &
     &27414.,28862.,29861.,31161.,33154.,                               &
     &34787.,35027.,33810.,31814.,29967.,                               &
     &29016.,28744.,28921.,29588.,30523.,                               &
     &31161.,30461.,28233.,26228.,26171.,                               &
     &27772.,29181.,30256.,31585.,33440.,                               &
     &34918.,35097.,33808.,31689.,29811.,                               &
     &28981.,28908.,29321.,30247.,31407.,                               &
     &32176.,31469.,29116.,26918.,26659.,                               &
     &28199.,29575.,30723.,32087.,33831.,                               &
     &35181.,35309.,33942.,31687.,29760.,                               &
     &29030.,29144.,29789.,30970.,32348.,                               &
     &33234.,32507.,30029.,27655.,27207.,                               &
     &28695.,30044.,31261.,32665.,34323.,                               &
     &35572.,35658.,34208.,31807.,29812.,                               &
     &29160.,29446.,30319.,31750.,33338.,                               &
     &34328.,33569.,30966.,28435.,27817.,                               &
     &29259.,30584.,31864.,33309.,34907.,                               &
     &36083.,36137.,34599.,32042.,29963.,                               &
     &29366.,29810.,30905.,32580.,34368.,                               &
     &35450.,34649.,31925.,29254.,28485.,                               &
     &29885.,31189.,32526.,34011.,35572.,                               &
     &36703.,36733.,35107.,32389.,30210.,                               &
     &29644.,30231.,31544.,33453.,35430.,                               &
     &36591.,35742.,32901.,30108.,29207.,                               &
     &30567.,31851.,33237.,34761.,36307.,                               &
     &37420.,37436.,35719.,32838.,30548.,                               &
     &29993.,30708.,32230.,34364.,36516.,                               &
     &37744.,36842.,33890.,30991.,29975.,                               &
     &31294.,32558.,33987.,35547.,37098.,                               &
     &38219.,38229.,36423.,33381.,30972.,                               &
     &30407.,31237.,32960.,35306.,37620.,                               &
     &38902.,37944.,34888.,31897.,30783.,                               &
     &32057.,33301.,34763.,36357.,37932.,                               &
     &39083.,39097.,37206.,34010.,31476.,                               &
     &30886.,31817.,33730.,36274.,38733.,                               &
     &40058.,39041.,35891.,32821.,31622.,                               &
     &32847.,34067.,35555.,37180.,38796.,                               &
     &39999.,40024.,38055.,34713.,32055.,                               &
     &31426.,32446.,34538.,37265.,39850.,                               &
     &41204.,40129.,36894.,33758.,32483.,                               &
     &33652.,34845.,36351.,38008.,39677.,                               &
     &40949.,40994.,38955.,35482.,32703.,                               &
     &32027.,33123.,35381.,38273.,40965.,                               &
     &42334.,41200.,37893.,34702.,33359.,                               &
     &34464.,35626.,37144.,38829.,40565.,                               &
     &41920.,41992.,39893.,36307.,33415.,                               &
     &32686.,33846.,36255.,39293.,42070.,                               &
     &43440.,42249.,38883.,35648.,34240.,                               &
     &35274.,36400.,37923.,39638.,41449.,                               &
     &42897.,43002.,40857.,37177.,34186.,                               &
     &33400.,34614.,37158.,40320.,43160.,                               &
     &44514.,43269.,39859.,36589.,35120.,                               &
     &36076.,37161.,38682.,40426.,42320.,                               &
     &43868.,44012.,41834.,38083.,35008.,                               &
     &34166.,35424.,38085.,41349.,44227.,                               &
     &45551.,44253.,40815.,37521.,35993.,                               &
     &36862.,37902.,39416.,41191.,43171.,                               &
     &44822.,45008.,42813.,39017.,35875.,                               &
     &34982.,36272.,39031.,42373.,45264.,                               &
     &46542.,45195.,41744.,38437.,36853.,                               &
     &37630.,38618.,40120.,41927.,43995.,                               &
     &45748.,45980.,43783.,39968.,36781.,                               &
     &35842.,37156.,39991.,43385.,46265.,                               &
     &47480.,46087.,42641.,39333.,37694.,                               &
     &38374.,39306.,40792.,42632.,44787.,                               &
     &46637.,46916.,44735.,40928.,37719.,                               &
     &36741.,38068.,40956.,44376.,47220.,                               &
     &48359.,46924.,43500.,40204.,38513.,                               &
     &39092.,39963.,41431.,43304.,45543.,                               &
     &47480.,47808.,45660.,41888.,38681.,                               &
     &37673.,39002.,41920.,45338.,48122.,                               &
     &49171.,47698.,44313.,41042.,39306.,                               &
     &39783.,40590.,42036.,43942.,46257.,                               &
     &48272.,48647.,46549.,42841.,39659.,                               &
     &38631.,39951.,42873.,46262.,48961.,                               &
     &49909.,48405.,45075.,41845.,40070.,                               &
     &40444.,41186.,42608.,44546.,46928.,                               &
     &49005.,49425.,47395.,43777.,40645.,                               &
     &39606.,40906.,43805.,47136.,49729.,                               &
     &50567.,49038.,45781.,42606.,40801.,                               &
     &41075.,41752.,43149.,45115.,47551.,                               &
     &49674.,50136.,48190.,44689.,41630.,                               &
     &40591.,41858.,44707.,47951.,50419.,                               &
     &51141.,49595.,46426.,43321.,41498.,                               &
     &41676.,42289.,43660.,45650.,48124.,                               &
     &50274.,50775.,48927.,45568.,42606.,                               &
     &41576.,42798.,45569.,48698.,51023.,                               &
     &51625.,50072.,47005.,43986.,42158.,                               &
     &42248.,42800.,44145.,46150.,48645.,                               &
     &50803.,51336.,49600.,46407.,43563.,                               &
     &42552.,43716.,46381.,49369.,51534.,                               &
     &52018.,50466.,47516.,44599.,42779.,                               &
     &42790.,43287.,44604.,46616.,49112.,                               &
     &51255.,51816.,50204.,47197.,44492.,                               &
     &43508.,44602.,47135.,49956.,51950.,                               &
     &52316.,50778.,47957.,45157.,43361.,                               &
     &43306.,43753.,45042.,47048.,49522.,                               &
     &51631.,52212.,50734.,47932.,45385.,                               &
     &44435.,45447.,47823.,50454.,52266.,                               &
     &52520.,51008.,48328.,45660.,43905.,                               &
     &43796.,44201.,45459.,47445.,49874.,                               &
     &51927.,52523.,51187.,48605.,46232.,                               &
     &45323.,46244.,48440.,50860.,52484.,                               &
     &52633.,51159.,48632.,46107.,44409.,                               &
     &44264.,44634.,45858.,47807.,50167.,                               &
     &52144.,52747.,51558.,49209.,47025.,                               &
     &46164.,46984.,48981.,51174.,52604.,                               &
     &52659.,51236.,48870.,46502.,44877.,                               &
     &44711.,45054.,46240.,48135.,50401.,                               &
     &52284.,52886.,51847.,49740.,47755.,                               &
     &46948.,47661.,49445.,51397.,52633.,                               &
     &52603.,51243.,49048.,46848.,45311.,                               &
     &45141.,45464.,46606.,48428.,50577.,                               &
     &52348.,52942.,52054.,50194.,48417.,                               &
     &47668.,48270.,49830.,51533.,52578.,                               &
     &52475.,51190.,49173.,47148.,45714.,                               &
     &45558.,45865.,46957.,48685.,50696.,                               &
     &52340.,52920.,52179.,50568.,49003.,                               &
     &48317.,48808.,50138.,51589.,52447.,                               &
     &52284.,51084.,49250.,47410.,46091.,                               &
     &45964.,46260.,47292.,48908.,50761.,                               &
     &52266.,52825.,52227.,50861.,49509.,                               &
     &48890.,49272.,50373.,51575.,52255.,                               &
     &52043.,50937.,49291.,47640.,46447.,                               &
     &46362.,46649.,47611.,49097.,50777.,                               &
     &52133.,52664.,52203.,51074.,49931.,                               &
     &49383.,49661.,50540.,51501.,52013.,                               &
     &51765.,50761.,49304.,47846.,46787.,                               &
     &46755.,47032.,47915.,49255.,50748.,                               &
     &51949.,52447.,52112.,51208.,50268.,                               &
     &49791.,49976.,50646.,51379.,51738.,                               &
     &51465.,50567.,49299.,48035.,47116.,                               &
     &47144.,47408.,48202.,49383.,50681.,                               &
     &51725.,52185.,51962.,51268.,50520.,                               &
     &50115.,50218.,50697.,51222.,51444.,                               &
     &51157.,50370.,49287.,48215.,47439.,                               &
     &47531.,47778.,48472.,49486.,50585.,                               &
     &51471.,51889.,51764.,51258.,50687.,                               &
     &50353.,50389.,50701.,51042.,51147.,                               &
     &50857.,50180.,49279.,48395.,47759.,                               &
     &47913.,48138.,48726.,49567.,50470.,                               &
     &51201.,51572.,51529.,51186.,50773.,                               &
     &50508.,50492.,50665.,50851.,50861.,                               &
     &50579.,50012.,49284.,48579.,48079.,                               &
     &48291.,48487.,48964.,49633.,50344.,                               &
     &50927.,51247.,51266.,51060.,50783.,                               &
     &50583.,50532.,50596.,50661.,50599.,                               &
     &50335.,49876.,49311.,48774.,48401.,                               &
     &48660.,48821.,49187.,49689.,50220.,                               &
     &50662.,50928.,50988.,50889.,50723.,                               &
     &50582.,50514.,50501.,50479.,50373.,                               &
     &50138.,49782.,49366.,48982.,48723.,                               &
     &49016.,49138.,49396.,49742.,50107.,                               &
     &50421.,50627.,50707.,50682.,50600.,                               &
     &50511.,50441.,50385.,50314.,50191.,                               &
     &49996.,49738.,49456.,49204.,49043.,                               &
     &49353.,49434.,49591.,49798.,50018.,                               &
     &50214.,50356.,50433.,50450.,50424.,                               &
     &50376.,50318.,50252.,50169.,50058.,                               &
     &49915.,49750.,49582.,49441.,49358.,                               &
     &49665.,49705.,49774.,49864.,49961.,                               &
     &50052.,50126.,50177.,50201.,50203.,                               &
     &50185.,50151.,50105.,50046.,49976.,                               &
     &49898.,49819.,49746.,49690.,49661.,                               &
     &49945.,49945.,49945.,49945.,49945.,                               &
     &49945.,49945.,49945.,49945.,49945.,                               &
     &49945.,49945.,49945.,49945.,49945.,                               &
     &49945.,49945.,49945.,49945.,49945./
!dipang                               
      data dipang/-74.12,-74.12,-74.12,-74.12,-74.12,                   &
     &-74.12,-74.12,-74.12,-74.12,-74.12,                               &
     &-74.12,-74.12,-74.12,-74.12,-74.12,                               &
     &-74.12,-74.12,-74.12,-74.12,-74.12,                               &
     &-72.88,-73.07,-73.36,-73.73,-74.14,                               &
     &-74.55,-74.93,-75.23,-75.41,-75.46,                               &
     &-75.37,-75.15,-74.82,-74.44,-74.02,                               &
     &-73.62,-73.27,-73.01,-72.85,-72.80,                               &
     &-71.65,-72.01,-72.56,-73.26,-74.07,                               &
     &-74.91,-75.69,-76.31,-76.70,-76.80,                               &
     &-76.59,-76.12,-75.44,-74.65,-73.82,                               &
     &-73.03,-72.37,-71.87,-71.58,-71.50,                               &
     &-70.44,-70.94,-71.72,-72.74,-73.94,                               &
     &-75.21,-76.42,-77.39,-78.00,-78.13,                               &
     &-77.78,-77.02,-75.97,-74.75,-73.51,                               &
     &-72.36,-71.40,-70.71,-70.31,-70.22,                               &
     &-69.26,-69.89,-70.87,-72.18,-73.75,                               &
     &-75.46,-77.11,-78.47,-79.30,-79.45,                               &
     &-78.91,-77.82,-76.37,-74.74,-73.10,                               &
     &-71.60,-70.38,-69.52,-69.06,-68.98,                               &
     &-68.12,-68.86,-70.02,-71.59,-73.53,                               &
     &-75.67,-77.78,-79.54,-80.61,-80.74,                               &
     &-79.95,-78.49,-76.64,-74.61,-72.59,                               &
     &-70.75,-69.29,-68.30,-67.81,-67.77,                               &
     &-67.04,-67.87,-69.17,-70.99,-73.27,                               &
     &-75.84,-78.42,-80.62,-81.93,-81.98,                               &
     &-80.85,-79.00,-76.77,-74.37,-71.98,                               &
     &-69.82,-68.14,-67.06,-66.58,-66.61,                               &
     &-66.02,-66.93,-68.35,-70.38,-72.99,                               &
     &-75.99,-79.04,-81.69,-83.26,-83.13,                               &
     &-81.58,-79.33,-76.75,-74.01,-71.28,                               &
     &-68.82,-66.93,-65.79,-65.37,-65.50,                               &
     &-65.07,-66.04,-67.55,-69.78,-72.71,                               &
     &-76.11,-79.63,-82.75,-84.58,-84.13,                               &
     &-82.07,-79.44,-76.57,-73.54,-70.50,                               &
     &-67.75,-65.67,-64.49,-64.18,-64.45,                               &
     &-64.21,-65.22,-66.80,-69.19,-72.42,                               &
     &-76.21,-80.18,-83.80,-85.90,-84.90,                               &
     &-82.28,-79.33,-76.24,-72.97,-69.65,                               &
     &-66.61,-64.35,-63.18,-63.02,-63.48,                               &
     &-63.44,-64.48,-66.08,-68.62,-72.12,                               &
     &-76.28,-80.68,-84.80,-87.19,-85.32,                               &
     &-82.19,-79.01,-75.77,-72.32,-68.74,                               &
     &-65.43,-62.99,-61.85,-61.90,-62.59,                               &
     &-62.78,-63.81,-65.42,-68.08,-71.83,                               &
     &-76.33,-81.12,-85.71,-88.36,-85.30,                               &
     &-81.80,-78.49,-75.16,-71.59,-67.78,                               &
     &-64.20,-61.60,-60.51,-60.82,-61.78,                               &
     &-62.22,-63.24,-64.81,-67.55,-71.54,                               &
     &-76.35,-81.46,-86.43,-88.91,-84.84,                               &
     &-81.17,-77.79,-74.44,-70.78,-66.78,                               &
     &-62.94,-60.17,-59.17,-59.79,-61.07,                               &
     &-61.79,-62.77,-64.25,-67.06,-71.26,                               &
     &-76.34,-81.70,-86.81,-88.13,-84.06,                               &
     &-80.34,-76.95,-73.61,-69.90,-65.74,                               &
     &-61.66,-58.72,-57.83,-58.81,-60.46,                               &
     &-61.47,-62.40,-63.76,-66.59,-70.97,                               &
     &-76.28,-81.80,-86.71,-86.90,-83.06,                               &
     &-79.36,-75.99,-72.68,-68.97,-64.67,                               &
     &-60.35,-57.25,-56.50,-57.88,-59.95,                               &
     &-61.28,-62.14,-63.33,-66.15,-70.68,                               &
     &-76.18,-81.75,-86.14,-85.57,-81.94,                               &
     &-78.28,-74.93,-71.67,-67.97,-63.57,                               &
     &-59.03,-55.77,-55.17,-57.02,-59.55,                               &
     &-61.21,-61.99,-62.97,-65.73,-70.39,                               &
     &-76.02,-81.54,-85.25,-84.21,-80.73,                               &
     &-77.12,-73.79,-70.58,-66.91,-62.44,                               &
     &-57.70,-54.28,-53.86,-56.20,-59.23,                               &
     &-61.24,-61.95,-62.68,-65.35,-70.08,                               &
     &-75.79,-81.17,-84.17,-82.83,-79.47,                               &
     &-75.90,-72.59,-69.41,-65.78,-61.27,                               &
     &-56.35,-52.79,-52.56,-55.44,-59.00,                               &
     &-61.38,-62.02,-62.46,-64.98,-69.76,                               &
     &-75.50,-80.64,-82.98,-81.44,-78.17,                               &
     &-74.62,-71.32,-68.17,-64.59,-60.06,                               &
     &-54.99,-51.29,-51.26,-54.71,-58.83,                               &
     &-61.59,-62.18,-62.31,-64.63,-69.42,                               &
     &-75.14,-79.96,-81.71,-80.03,-76.84,                               &
     &-73.31,-69.99,-66.87,-63.33,-58.80,                               &
     &-53.61,-49.77,-49.97,-54.02,-58.71,                               &
     &-61.85,-62.44,-62.23,-64.31,-69.06,                               &
     &-74.69,-79.15,-80.37,-78.60,-75.48,                               &
     &-71.96,-68.61,-65.49,-62.00,-57.49,                               &
     &-52.20,-48.23,-48.67,-53.33,-58.61,                               &
     &-62.14,-62.77,-62.20,-63.99,-68.66,                               &
     &-74.17,-78.21,-78.99,-77.14,-74.08,                               &
     &-70.56,-67.18,-64.04,-60.58,-56.12,                               &
     &-50.74,-46.66,-47.35,-52.64,-58.51,                               &
     &-62.43,-63.15,-62.23,-63.68,-68.21,                               &
     &-73.55,-77.18,-77.55,-75.66,-72.65,                               &
     &-69.11,-65.69,-62.52,-59.08,-54.67,                               &
     &-49.23,-45.05,-46.00,-51.92,-58.38,                               &
     &-62.68,-63.56,-62.30,-63.37,-67.72,                               &
     &-72.84,-76.04,-76.07,-74.14,-71.17,                               &
     &-67.62,-64.13,-60.93,-57.49,-53.13,                               &
     &-47.65,-43.37,-44.60,-51.15,-58.19,                               &
     &-62.87,-63.96,-62.39,-63.04,-67.17,                               &
     &-72.03,-74.80,-74.53,-72.58,-69.64,                               &
     &-66.05,-62.51,-59.24,-55.80,-51.49,                               &
     &-45.98,-41.61,-43.12,-50.32,-57.93,                               &
     &-62.97,-64.32,-62.48,-62.69,-66.55,                               &
     &-71.12,-73.47,-72.93,-70.97,-68.05,                               &
     &-64.42,-60.80,-57.47,-54.00,-49.74,                               &
     &-44.20,-39.75,-41.55,-49.40,-57.57,                               &
     &-62.96,-64.60,-62.54,-62.28,-65.84,                               &
     &-70.10,-72.05,-71.27,-69.29,-66.39,                               &
     &-62.71,-59.01,-55.60,-52.10,-47.88,                               &
     &-42.30,-37.76,-39.88,-48.38,-57.10,                               &
     &-62.82,-64.76,-62.53,-61.81,-65.03,                               &
     &-68.96,-70.53,-69.54,-67.55,-64.65,                               &
     &-60.90,-57.13,-53.63,-50.08,-45.89,                               &
     &-40.27,-35.64,-38.07,-47.24,-56.52,                               &
     &-62.52,-64.78,-62.42,-61.25,-64.11,                               &
     &-67.71,-68.91,-67.73,-65.74,-62.82,                               &
     &-59.00,-55.13,-51.55,-47.94,-43.76,                               &
     &-38.07,-33.37,-36.13,-45.97,-55.81,                               &
     &-62.06,-64.61,-62.17,-60.56,-63.06,                               &
     &-66.32,-67.18,-65.84,-63.84,-60.90,                               &
     &-56.98,-53.02,-49.35,-45.67,-41.49,                               &
     &-35.72,-30.92,-34.03,-44.57,-54.97,                               &
     &-61.42,-64.23,-61.73,-59.72,-61.87,                               &
     &-64.80,-65.35,-63.85,-61.84,-58.87,                               &
     &-54.84,-50.78,-47.02,-43.27,-39.07,                               &
     &-33.18,-28.30,-31.76,-43.02,-53.98,                               &
     &-60.60,-63.61,-61.08,-58.71,-60.51,                               &
     &-63.14,-63.40,-61.77,-59.75,-56.72,                               &
     &-52.57,-48.39,-44.56,-40.73,-36.49,                               &
     &-30.47,-25.49,-29.33,-41.32,-52.85,                               &
     &-59.57,-62.73,-60.18,-57.50,-58.98,                               &
     &-61.33,-61.32,-59.58,-57.56,-54.46,                               &
     &-50.16,-45.87,-41.95,-38.05,-33.76,                               &
     &-27.57,-22.51,-26.73,-39.46,-51.58,                               &
     &-58.34,-61.59,-59.01,-56.06,-57.26,                               &
     &-59.36,-59.12,-57.28,-55.25,-52.07,                               &
     &-47.60,-43.18,-39.19,-35.22,-30.87,                               &
     &-24.49,-19.35,-23.96,-37.44,-50.15,                               &
     &-56.90,-60.16,-57.54,-54.38,-55.34,                               &
     &-57.22,-56.77,-54.85,-52.82,-49.55,                               &
     &-44.89,-40.33,-36.27,-32.25,-27.82,                               &
     &-21.24,-16.03,-21.03,-35.25,-48.55,                               &
     &-55.23,-58.44,-55.78,-52.44,-53.21,                               &
     &-54.91,-54.28,-52.29,-50.27,-46.90,                               &
     &-42.02,-37.31,-33.20,-29.13,-24.62,                               &
     &-17.83,-12.57,-17.95,-32.90,-46.79,                               &
     &-53.33,-56.43,-53.72,-50.23,-50.85,                               &
     &-52.41,-51.63,-49.60,-47.58,-44.10,                               &
     &-38.99,-34.12,-29.96,-25.86,-21.27,                               &
     &-14.28, -8.99,-14.74,-30.39,-44.84,                               &
     &-51.18,-54.13,-51.35,-47.74,-48.27,                               &
     &-49.72,-48.82,-46.76,-44.76,-41.17,                               &
     &-35.80,-30.76,-26.57,-22.44,-17.77,                               &
     &-10.59, -5.33,-11.41,-27.71,-42.71,                               &
     &-48.78,-51.53,-48.68,-44.97,-45.44,                               &
     &-46.83,-45.83,-43.77,-41.80,-38.08,                               &
     &-32.45,-27.23,-23.02,-18.89,-14.15,                               &
     & -6.81, -1.60, -7.99,-24.87,-40.37,                               &
     &-46.12,-48.64,-45.70,-41.93,-42.37,                               &
     &-43.74,-42.67,-40.62,-38.70,-34.86,                               &
     &-28.94,-23.54,-19.33,-15.21,-10.42,                               &
     & -2.96,  2.16, -4.50,-21.87,-37.82,                               &
     &-43.21,-45.46,-42.44,-38.61,-39.06,                               &
     &-40.43,-39.31,-37.31,-35.45,-31.50,                               &
     &-25.29,-19.72,-15.51,-11.43, -6.60,                               &
     &  0.93,  5.93, -0.96,-18.73,-35.05,                               &
     &-40.02,-42.01,-38.90,-35.02,-35.50,                               &
     &-36.90,-35.77,-33.83,-32.05,-28.00,                               &
     &-21.51,-15.76,-11.59, -7.55, -2.71,                               &
     &  4.84,  9.66,  2.60,-15.45,-32.05,                               &
     &-36.58,-38.27,-35.09,-31.18,-31.71,                               &
     &-33.15,-32.03,-30.18,-28.51,-24.38,                               &
     &-17.62,-11.71, -7.58, -3.61,  1.23,                               &
     &  8.72, 13.33,  6.15,-12.05,-28.83,                               &
     &-32.87,-34.28,-31.02,-27.11,-27.68,                               &
     &-29.19,-28.10,-26.37,-24.84,-20.65,                               &
     &-13.63, -7.59, -3.52,  0.37,  5.18,                               &
     & 12.57, 16.93,  9.67, -8.54,-25.39,                               &
     &-28.92,-30.04,-26.73,-22.83,-23.45,                               &
     &-25.03,-24.00,-22.41,-21.03,-16.81,                               &
     &-9.58, -3.44,  0.57,  4.37,  9.12,                                &
     & 16.34, 20.43, 13.15, -4.94,-21.73,                               &
     &-24.75,-25.57,-22.22,-18.36,-19.03,                               &
     &-20.68,-19.72,-18.29,-17.11,-12.90,                               &
     & -5.50,  0.72,  4.64,  8.34, 13.02,                               &
     & 20.02, 23.82, 16.56, -1.27,-17.86,                               &
     &-20.36,-20.91,-17.55,-13.74,-14.45,                               &
     &-16.17,-15.29,-14.05,-13.08, -8.94,                               &
     & -1.41,  4.85,  8.67, 12.27, 16.85,                               &
     & 23.60, 27.08, 19.88,  2.44,-13.82,                               &
     &-15.80,-16.10,-12.74, -9.02, -9.75,                               &
     &-11.52,-10.74, -9.71, -8.97, -4.94,                               &
     &  2.64,  8.90, 12.63, 16.12, 20.60,                               &
     & 27.05, 30.21, 23.11,  6.18, -9.62,                               &
     &-11.10,-11.16, -7.84, -4.23, -4.97,                               &
     & -6.76, -6.10, -5.29, -4.81, -0.93,                               &
     &  6.63, 12.85, 16.48, 19.88, 24.25,                               &
     & 30.38, 33.21, 26.23,  9.91, -5.31,                               &
     & -6.31, -6.15, -2.89,  0.58, -0.16,                               &
     & -1.95, -1.41, -0.82, -0.62,  3.05,                               &
     & 10.53, 16.67, 20.20, 23.52, 27.77,                               &
     & 33.57, 36.08, 29.24, 13.62, -0.91,                               &
     & -1.47, -1.13,  2.05,  5.36,  4.65,                               &
     &  2.88,  3.30,  3.65,  3.57,  6.99,                               &
     & 14.31, 20.34, 23.78, 27.02, 31.16,                               &
     & 36.63, 38.82, 32.14, 17.27,  3.53,                               &
     &  3.37,  3.87,  6.94, 10.08,  9.40,                               &
     &  7.67,  7.98,  8.09,  7.73, 10.85,                               &
     & 17.94, 23.83, 27.19, 30.38, 34.41,                               &
     & 39.55, 41.43, 34.92, 20.86,  7.97,                               &
     &  8.16,  8.80, 11.73, 14.68, 14.06,                               &
     & 12.40, 12.59, 12.48, 11.83, 14.62,                               &
     & 21.42, 27.14, 30.44, 33.58, 37.52,                               &
     & 42.33, 43.92, 37.59, 24.36, 12.37,                               &
     & 12.87, 13.60, 16.40, 19.15, 18.59,                               &
     & 17.02, 17.10, 16.77, 15.84, 18.28,                               &
     & 24.73, 30.26, 33.51, 36.63, 40.48,                               &
     & 44.99, 46.29, 40.14, 27.76, 16.69,                               &
     & 17.44, 18.25, 20.90, 23.45, 22.96,                               &
     & 21.49, 21.47, 20.93, 19.74, 21.80,                               &
     & 27.87, 33.20, 36.41, 39.52, 43.30,                               &
     & 47.53, 48.56, 42.59, 31.05, 20.89,                               &
     & 21.86, 22.71, 25.21, 27.56, 27.15,                               &
     & 25.80, 25.69, 24.96, 23.52, 25.20,                               &
     & 30.84, 35.94, 39.13, 42.25, 45.97,                               &
     & 49.95, 50.72, 44.93, 34.21, 24.95,                               &
     & 26.09, 26.97, 29.32, 31.47, 31.14,                               &
     & 29.92, 29.73, 28.82, 27.15, 28.45,                               &
     & 33.64, 38.51, 41.69, 44.84, 48.52,                               &
     & 52.26, 52.79, 47.18, 37.25, 28.86,                               &
     & 30.11, 31.01, 33.21, 35.18, 34.93,                               &
     & 33.85, 33.59, 32.51, 30.63, 31.55,                               &
     & 36.28, 40.92, 44.10, 47.28, 50.93,                               &
     & 54.46, 54.78, 49.34, 40.15, 32.58,                               &
     & 33.93, 34.83, 36.88, 38.68, 38.51,                               &
     & 37.57, 37.26, 36.03, 33.96, 34.51,                               &
     & 38.76, 43.17, 46.36, 49.60, 53.23,                               &
     & 56.57, 56.69, 51.41, 42.93, 36.12,                               &
     & 37.54, 38.42, 40.33, 41.98, 41.89,                               &
     & 41.09, 40.74, 39.37, 37.12, 37.32,                               &
     & 41.11, 45.29, 48.50, 51.79, 55.41,                               &
     & 58.59, 58.53, 53.40, 45.58, 39.47,                               &
     & 40.92, 41.79, 43.57, 45.08, 45.08,                               &
     & 44.42, 44.03, 42.53, 40.13, 40.01,                               &
     & 43.34, 47.28, 50.52, 53.88, 57.50,                               &
     & 60.53, 60.30, 55.32, 48.10, 42.63,                               &
     & 44.10, 44.94, 46.61, 47.99, 48.07,                               &
     & 47.55, 47.13, 45.53, 42.99, 42.57,                               &
     & 45.46, 49.18, 52.44, 55.86, 59.49,                               &
     & 62.40, 62.01, 57.17, 50.51, 45.61,                               &
     & 47.08, 47.89, 49.44, 50.72, 50.88,                               &
     & 50.51, 50.07, 48.36, 45.71, 45.01,                               &
     & 47.49, 50.98, 54.27, 57.76, 61.41,                               &
     & 64.19, 63.67, 58.96, 52.80, 48.41,                               &
     & 49.87, 50.64, 52.09, 53.27, 53.52,                               &
     & 53.29, 52.84, 51.04, 48.29, 47.34,                               &
     & 49.44, 52.72, 56.03, 59.59, 63.25,                               &
     & 65.93, 65.27, 60.70, 54.99, 51.04,                               &
     & 52.47, 53.21, 54.56, 55.66, 56.00,                               &
     & 55.91, 55.45, 53.57, 50.74, 49.58,                               &
     & 51.32, 54.40, 57.72, 61.35, 65.02,                               &
     & 67.60, 66.83, 62.38, 57.07, 53.51,                               &
     & 54.90, 55.60, 56.86, 57.90, 58.32,                               &
     & 58.37, 57.92, 55.98, 53.08, 51.73,                               &
     & 53.14, 56.04, 59.37, 63.05, 66.74,                               &
     & 69.23, 68.34, 64.01, 59.06, 55.83,                               &
     & 57.17, 57.84, 59.01, 60.00, 60.51,                               &
     & 60.70, 60.26, 58.25, 55.31, 53.80,                               &
     & 54.92, 57.64, 60.97, 64.70, 68.40,                               &
     & 70.80, 69.81, 65.59, 60.96, 58.01,                               &
     & 59.30, 59.91, 61.01, 61.96, 62.58,                               &
     & 62.89, 62.47, 60.42, 57.44, 55.80,                               &
     & 56.67, 59.22, 62.55, 66.32, 70.01,                               &
     & 72.32, 71.23, 67.12, 62.78, 60.07,                               &
     & 61.29, 61.85, 62.88, 63.81, 64.52,                               &
     & 64.97, 64.56, 62.48, 59.48, 57.74,                               &
     & 58.39, 60.78, 64.09, 67.89, 71.58,                               &
     & 73.80, 72.61, 68.61, 64.51, 62.00,                               &
     & 63.16, 63.67, 64.62, 65.55, 66.36,                               &
     & 66.93, 66.55, 64.44, 61.45, 59.63,                               &
     & 60.09, 62.33, 65.61, 69.42, 73.11,                               &
     & 75.24, 73.94, 70.04, 66.17, 63.83,                               &
     & 64.90, 65.36, 66.25, 67.18, 68.09,                               &
     & 68.78, 68.44, 66.32, 63.35, 61.48,                               &
     & 61.77, 63.88, 67.11, 70.93, 74.59,                               &
     & 76.62, 75.23, 71.42, 67.76, 65.55,                               &
     & 66.55, 66.94, 67.78, 68.72, 69.73,                               &
     & 70.54, 70.23, 68.12, 65.18, 63.28,                               &
     & 63.44, 65.41, 68.59, 72.39, 76.03,                               &
     & 77.96, 76.47, 72.76, 69.27, 67.19,                               &
     & 68.09, 68.43, 69.22, 70.18, 71.29,                               &
     & 72.20, 71.93, 69.84, 66.95, 65.05,                               &
     & 65.10, 66.94, 70.05, 73.82, 77.42,                               &
     & 79.25, 77.65, 74.03, 70.72, 68.73,                               &
     & 69.55, 69.83, 70.57, 71.56, 72.76,                               &
     & 73.77, 73.55, 71.50, 68.67, 66.79,                               &
     & 66.74, 68.47, 71.49, 75.20, 78.77,                               &
     & 80.48, 78.78, 75.25, 72.09, 70.19,                               &
     & 70.92, 71.14, 71.85, 72.87, 74.15,                               &
     & 75.26, 75.09, 73.08, 70.35, 68.49,                               &
     & 68.38, 69.99, 72.91, 76.55, 80.05,                               &
     & 81.66, 79.85, 76.41, 73.40, 71.58,                               &
     & 72.23, 72.39, 73.07, 74.11, 75.48,                               &
     & 76.67, 76.55, 74.61, 71.97, 70.17,                               &
     & 70.00, 71.50, 74.30, 77.85, 81.28,                               &
     & 82.77, 80.85, 77.51, 74.64, 72.89,                               &
     & 73.46, 73.58, 74.23, 75.30, 76.73,                               &
     & 77.99, 77.93, 76.07, 73.56, 71.82,                               &
     & 71.61, 73.00, 75.66, 79.09, 82.44,                               &
     & 83.81, 81.78, 78.54, 75.82, 74.13,                               &
     & 74.63, 74.71, 75.34, 76.44, 77.92,                               &
     & 79.24, 79.24, 77.48, 75.10, 73.44,                               &
     & 73.21, 74.48, 77.00, 80.29, 83.54,                               &
     & 84.77, 82.64, 79.51, 76.93, 75.31,                               &
     & 75.76, 75.80, 76.41, 77.52, 79.04,                               &
     & 80.40, 80.46, 78.82, 76.59, 75.03,                               &
     & 74.79, 75.95, 78.31, 81.43, 84.56,                               &
     & 85.65, 83.42, 80.42, 77.98, 76.43,                               &
     & 76.84, 76.86, 77.45, 78.57, 80.09,                               &
     & 81.47, 81.61, 80.11, 78.05, 76.60,                               &
     & 76.34, 77.40, 79.58, 82.52, 85.51,                               &
     & 86.44, 84.12, 81.26, 78.97, 77.50,                               &
     & 77.88, 77.89, 78.46, 79.57, 81.08,                               &
     & 82.47, 82.68, 81.33, 79.46, 78.13,                               &
     & 77.88, 78.83, 80.83, 83.55, 86.38,                               &
     & 87.13, 84.74, 82.05, 79.91, 78.53,                               &
     & 78.90, 78.90, 79.45, 80.53, 82.01,                               &
     & 83.37, 83.66, 82.49, 80.82, 79.62,                               &
     & 79.39, 80.23, 82.03, 84.52, 87.17,                               &
     & 87.70, 85.29, 82.78, 80.81, 79.52,                               &
     & 79.90, 79.89, 80.43, 81.46, 82.87,                               &
     & 84.19, 84.56, 83.58, 82.13, 81.08,                               &
     & 80.86, 81.60, 83.20, 85.45, 87.89,                               &
     & 88.14, 85.76, 83.47, 81.67, 80.48,                               &
     & 80.88, 80.88, 81.39, 82.36, 83.67,                               &
     & 84.92, 85.36, 84.61, 83.39, 82.49,                               &
     & 82.30, 82.94, 84.34, 86.32, 88.54,                               &
     & 88.44, 86.18, 84.12, 82.50, 81.42,                               &
     & 81.87, 81.87, 82.33, 83.22, 84.41,                               &
     & 85.57, 86.07, 85.55, 84.58, 83.85,                               &
     & 83.70, 84.24, 85.43, 87.14, 89.12,                               &
     & 88.58, 86.54, 84.73, 83.31, 82.35,                               &
     & 82.86, 82.86, 83.28, 84.06, 85.09,                               &
     & 86.12, 86.67, 86.40, 85.70, 85.15,                               &
     & 85.04, 85.50, 86.48, 87.90, 89.60,                               &
     & 88.59, 86.85, 85.32, 84.10, 83.27,                               &
     & 83.85, 83.86, 84.21, 84.86, 85.72,                               &
     & 86.60, 87.16, 87.13, 86.72, 86.36,                               &
     & 86.32, 86.69, 87.47, 88.58, 89.63,                               &
     & 88.52, 87.13, 85.89, 84.89, 84.20,                               &
     & 84.87, 84.87, 85.14, 85.65, 86.31,                               &
     & 87.00, 87.53, 87.71, 87.59, 87.45,                               &
     & 87.47, 87.77, 88.33, 89.04, 89.22,                               &
     & 88.39, 87.37, 86.44, 85.67, 85.14,                               &
     & 85.89, 85.89, 86.07, 86.41, 86.85,                               &
     & 87.34, 87.76, 88.04, 88.18, 88.24,                               &
     & 88.35, 88.56, 88.83, 89.00, 88.77,                               &
     & 88.23, 87.58, 86.96, 86.44, 86.08,                               &
     & 86.90, 86.89, 86.98, 87.14, 87.36,                               &
     & 87.61, 87.86, 88.08, 88.26, 88.40,                               &
     & 88.51, 88.57, 88.58, 88.50, 88.32,                               &
     & 88.05, 87.75, 87.45, 87.19, 87.00,                               &
     & 87.85, 87.85, 87.85, 87.85, 87.85,                               &
     & 87.85, 87.85, 87.85, 87.85, 87.85,                               &
     & 87.85, 87.85, 87.85, 87.85, 87.85,                               &
     & 87.85, 87.85, 87.85, 87.85, 87.85/
!     data cormag/163.68,163.68,163.68,163.68,163.68,                   &
!    ...............................................
!    & 8.15,  8.15,  8.15,  8.15,  8.15/
!btot
!     data btot/49163.,49163.,49163.,49162.,49162.,
!    ...............................................
!    &49945.,49945.,49945.,49945.,49945./
!dipang
!     data dipang/-74.12,-74.12,-74.12,-74.12,-74.12,
!    ..................................................
!    & 87.85, 87.85, 87.85, 87.85, 87.85/
      data glat/-1.570796327,-1.535889741,-1.500983156,-1.466076571,    &
     &-1.431169986,                                                     &
     &-1.396263401,-1.361356816,-1.326450231,-1.291543646,-1.256637061, &
     &-1.221730476,-1.186823891,-1.151917306,-1.117010721,-1.082104136, &
     &-1.047197551,-1.012290966,-0.977384381,-0.942477796,-0.907571211, &
     &-0.872664626,-0.837758041,-0.802851456,-0.767944871,-0.733038286, &
     &-0.698131701,-0.663225116,-0.628318531,-0.593411946,-0.558505361, &
     &-0.523598776,-0.488692190,-0.453785605,-0.418879020,-0.383972435, &
     &-0.349065850,-0.314159265,-0.279252680,-0.244346095,-0.209439510, &
     &-0.174532925,-0.139626340,-0.104719755,-0.069813170,-0.034906585, &
     & 0.000000000, 0.034906585, 0.069813170, 0.104719755, 0.139626340, &
     & 0.174532925, 0.209439510, 0.244346095, 0.279252680, 0.314159265, &
     & 0.349065850, 0.383972435, 0.418879020, 0.453785605, 0.488692190, &
     & 0.523598776, 0.558505361, 0.593411946, 0.628318531, 0.663225116, &
     & 0.698131701, 0.733038286, 0.767944871, 0.802851456, 0.837758041, &
     & 0.872664626, 0.907571211, 0.942477796, 0.977384381, 1.012290966, &
     & 1.047197551, 1.082104136, 1.117010721, 1.151917306, 1.186823891, &
     & 1.221730476, 1.256637061, 1.291543646, 1.326450231, 1.361356816, &
     & 1.396263401, 1.431169986, 1.466076571, 1.500983156, 1.535889741, &
     & 1.570796327/ 
      data glon/0.000000000, 0.314159265, 0.628318531, 0.942477796,     &
     & 1.256637061,                                                     &
     &1.570796327, 1.884955592, 2.199114857, 2.513274122, 2.827433388,  &
     &3.141592653, 3.455751918, 3.769911184, 4.084070449, 4.398229714,  &
     &4.712388980, 5.026548245, 5.340707510, 5.654866775, 5.969026041/
! lat lon interval
      ddlat= 3.4906585033333331E-002
      ddlon= 0.3141592653000000     
! 
      do i=1,im
! get latitude index
        iref=int(rlat(i)/ddlat)+46
        dl=(rlat(i)-glat(iref))/ddlat
! print*,iref,dl
! get longitude index
        jref=int(rlon(i)/ddlon)+1
        jref1=jref+1
        if(jref1.gt.20) jref1=jref1-20
        dll=(rlon(i)-glon(jref))/ddlon
! print*,i,jref,jref1,dll
!
        a1=cormag(jref,iref)
        a2=cormag(jref1,iref)
        b1=cormag(jref,iref+1)
        b2=cormag(jref1,iref+1)
        aa=(1.-dll)*a1+dll*a2
        bb=(1.-dll)*b1+dll*b2
        cormago(i)=(1.-dl)*aa+dl*bb
!
        a1=btot(jref,iref)
        a2=btot(jref1,iref)
        b1=btot(jref,iref+1)
        b2=btot(jref1,iref+1)
        aa=(1.-dll)*a1+dll*a2
        bb=(1.-dll)*b1+dll*b2
        btoto(i)=(1.-dl)*aa+dl*bb
!
        a1=dipang(jref,iref)
        a2=dipang(jref1,iref)
        b1=dipang(jref,iref+1)
        b2=dipang(jref1,iref+1)
        aa=(1.-dll)*a1+dll*a2
        bb=(1.-dll)*b1+dll*b2
        dipango(i)=(1.-dl)*aa+dl*bb
!
      enddo
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      SUBROUTINE SPOLE(ix,im,RLAT,PHIR,utsec,SDA,PHIMR,ESSA,CMORG)
      implicit none
      real, PARAMETER    ::PI=3.141592653,DTR=PI/180.
      integer,intent(in) :: ix          ! longitude dimension
      integer,intent(in) :: im          ! number of longitude 
      real,   intent(in) :: rlat(im)        !geo latitude (rad)
      real,   intent(in) :: phir(im)    !geo longitude (rad)
      real,   intent(in) :: utsec       !UT second
      real,   intent(in) :: sda         !solar declination angle (rad)
      real,   intent(out):: phimr(im)   !maglongitude (rad)        
      real,   intent(out):: essa(im)    !magnetic local time    
      real,   intent(out):: cmorg(im)          
! local variables
      real th,th1,phi1,sinth,sinth1,costh1,sinph1,cosph1,ac1,bc1,cc1,   &
     & ac2,bc2,cc2,phim,ssp,sspr,csda,as1,bs1,cs1,as2,bs2,cs2,gml,      &
     & cmag
      integer i
!
      do i=1,im
      th=pi/2.-rlat(i)
!
! SET POLE COORD. FOR EACH HEMIS.
!
      IF (RLAT(i).GE.0.0) THEN
       TH1=9.25*DTR
       PHI1=-78.0*DTR
      ELSE
       TH1=16.32*DTR
       PHI1=-54.0*DTR
      END IF
!
      SINTH=SIN(TH)
      SINTH1=SIN(TH1)
      COSTH1=COS(TH1)
      SINPH1=SIN(PHI1)
      COSPH1=COS(PHI1)
!
!     do i=1,im
      AC1=SINTH*COS(PHIR(i))
      BC1=SINTH*SIN(PHIR(i))
      CC1=COS(TH)
      AC2=AC1*COSTH1*COSPH1+BC1*COSTH1*SINPH1-CC1*SINTH1
      IF((ABS(AC2)).LT.0.001)AC2=0.001
      BC2=-AC1*SINPH1+BC1*COSPH1
      CC2=AC1*SINTH1*COSPH1+BC1*SINTH1*SINPH1+CC1*COSTH1
      CMORG(i)=ACOS(CC2)
      PHIMR(i)=ATAN2(BC2,AC2)
      PHIM=PHIMR(i)/DTR
!     SSP=360.-utsec/240.
      SSP=180.-utsec/240.
      SSPR=SSP*DTR
      CSDA=PI/2.-SDA
      AS1=COS(SSPR)*SIN(CSDA)
      BS1=SIN(SSPR)*SIN(CSDA)
      CS1=COS(CSDA)
      AS2=AS1*COSTH1*COSPH1+BS1*COSTH1*SINPH1-CS1*SINTH1
      IF((ABS(AS2)).LT.0.001)AS2=0.001
      BS2=-AS1*SINPH1+BS1*COSPH1
      CS2=AS1*SINTH1*COSPH1+BS1*SINTH1*SINPH1+CS1*COSTH1
      GML=ATAN2(BS2,AS2)/DTR
      ESSA(i)=PHIM-GML
      enddo
      RETURN
      END
