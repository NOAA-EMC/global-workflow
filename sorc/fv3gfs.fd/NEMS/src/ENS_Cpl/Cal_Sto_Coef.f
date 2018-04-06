!Program Main
!REAL(8), DIMENSION(14,14) :: Sto_Coef
!DO k=1,10
! Print *, 'k=',k
!CALL Cal_Sto_Coef(15, k, Sto_Coef)
!DO j=1,14
! Write (*,'(1x,14f10.5)') (Sto_Coef(i,j),i=1,14)
!ENDDO
!ENDDO
!STOP
!END
!Subroutine Cal_Sto_Coef(Total_member, Cpl_Run_Calling_Number, Sto_Coef,year,mm,dd,hh,fhr,eps0,nr0,eps,nr)
 Subroutine Cal_Sto_Coef(Total_member, t1, t2, delt, Sto_Coef,year,mm,dd,hh,fhr,eps0,nr0,eps,nr)

! This subroutine is used to compute the stochastic coefficients
! used to combine the tendency perturbation to generate the 
! stochastic forcing for each ensemble member 
!-------------------------------------------------------------

 USE pran 
 USE peuc
 USE prana

 implicit none
 INTEGER                                                             :: Total_member
!INTEGER                                                             :: Cpl_Run_Calling_Number
 INTEGER                 :: t,t1,t2, delt
 REAL(8), DIMENSION(Total_member - 1, Total_member - 1) :: Sto_Coef
!REAL(KIND=KIND_EVOD), DIMENSION(Total_member - 1, Total_member - 1) :: Sto_Coef
!REAL(KIND=KIND_EVOD), DIMENSION(Total_member - 1)                   :: rp

 INTEGER                 :: i0
 INTEGER                 :: n, nt
 INTEGER                 :: iseed0, iseed2
 INTEGER                 :: iseed1(1:2)
!INTEGER                 :: iseed3(1:2),iseed4(1:2)
!INTEGER                 :: year,mm,dd,hh,nth,fhr,delt,fflag
 INTEGER                 :: year,mm,dd,hh,nth,fhr,fflag
!year-Year,mm=month,dd=day,hh=hour,fhr=forecast_hour
 INTEGER                 :: i,j,k
 INTEGER                 :: ksize,nrank,r,t0,iunit,nr0,nr
 REAL(8)                 :: x,eps0,eps,g
 INTEGER                 :: yyc,mmc,ddc,hhc,fhrc,yyi,mmi,ddi,hhi

! Working arrays.
!----------------
 REAL(8), DIMENSION(Total_member-1,Total_member-1) :: a,b,c,d,e,ident,b0
!REAL(KIND=KIND_EVOD), DIMENSION(Total_member - 1, Total_member - 1) :: work1
!REAL(KIND=KIND_EVOD), DIMENSION(Total_member - 1, Total_member - 1) :: work2

!Setting the constants
!year = 2006 
!mm = 08
!dd = 16
!hh = 00
!fhr = 120
 print *,'STPS',year,mm,dd,hh,fhr
 print *,'STPS',Total_member, t1, t2, delt
 print *,'STPS',eps0,nr0,eps,nr

 n = Total_member - 1
!t = Cpl_Run_Calling_Number
 ident=identity(Total_member - 1)

!Setting the seed (seed2) based on the date(yyyymmddhh) 
!and generate the initial value of the Sto_Coef-Matric 
 call random_seed(size=ksize)
 print *,'STPS',ksize
      CALL INITDATE(year,mm,dd,hh,fhr,yyi,mmi,ddi,hhi)

do t=t1+1,t2,1
      fhrc=delt*t
      CALL CURRDATE(yyi,mmi,ddi,hhi,fhrc,yyc,mmc,ddc,hhc)
 iseed0=yyc+mmc+ddc+hhc+1
!iseed0=year+mm+dd+hh+1
 iseed1=iseed0

#ifdef IBM
 call random_seed(generator=2)
#endif

 call random_seed(put=iseed1(1:ksize))
!call random_seed(get=iseed3(1:ksize))
 call random_number(x)
!call random_seed(get=iseed4(1:ksize))
 iseed2=iseed0+nint(x*1000)
 print *, 'STPS ISEED_A:', x,iseed0,iseed1,iseed2
!print *, 'STPS ISEED_A:', x,iseed3,iseed4
!IF (Cpl_Run_Calling_Number .eq. 1 ) THEN
!IF (t1 .eq. 1 ) THEN
!IF (t1 .eq. 0 ) THEN    !up to Em
 IF (t .eq. 1 ) THEN     !starting in En
   call plant1(iseed2)
   call ranrot(a)
   Sto_Coef = a
!  print *,a
 ENDIF

!Generating the fixed rotation matrix (b0) based on eps0 and nr0
 if (eps0.lt.0.000001.or.nr0.lt.1) then
 b0=ident
 else
 iseed2=iseed2-1*delt*1000
 print *, 'STPS ISEED_B:', iseed2
 call plant1(iseed2)
 do j=1,n; do i=1,n; call gauss(b(i,j)); enddo; enddo
!print *,b
!apply gram process to the matrix (I+epsilon*A) where A=I+(b-bT)
 b=b-transpose(b)
 c=ident+eps0*b
 call gram(c,nrank);b=c
!NOW b is R matrix if nr0=1
 do r=1,nr0-1;
   b=matmul(b,c)
 enddo
! Now b is the R matrix after nr0 times of rotation   
 b0=b
 endif
!print *,b0

!do t=t1,t2,1
!do t=t1+1,t2,1
!Generate the random rotation matrix (b) based on eps and nr
 iseed2=iseed2+t*delt*1000
 print *, 'STPS ISEED_C:', iseed2
 call plant1(iseed2)
   do j=1,n; do i=1,n; call gauss(b(i,j)); enddo; enddo
!apply gram process to the matrix (I+epsilon*A) where A=I+(b-bT)
   b=b-transpose(b)
   c=ident+eps*b
   call gram(c,nrank);b=c
!NOW b is R matrix if nr=1
 do r=1,nr-1;
   b=matmul(b,c)
 enddo
! Now b is the R matrix after nr times of rotation   
!print *,b

! Generate the Current matrix (b=a X b0 X b)
   a=Sto_Coef
   b=matmul(b0,b)   !b=b0 x b1, R(t)=R0(t=0) x R1(t)
   d=matmul(a,b)   !d=a x b, C(t)=C(t-1) x R
! Updating a matrix (C)
   a=d
   Sto_Coef = d
enddo  !  the loop of t=t1+1,t2,1

! Print the coefficients!
  print *, 'STPS coefficients:'
 DO j=1,n
 if (n.eq.14) then
  Write (*,'(1x,14f10.5)') (Sto_Coef(i,j),i=1,n)
 elseif (n.eq.2) then
  Write (*,'(1x,2f10.5)') (Sto_Coef(i,j),i=1,n)
 elseif (n.eq.3) then
  Write (*,'(1x,3f10.5)') (Sto_Coef(i,j),i=1,n)
 elseif (n.eq.20) then
  Write (*,'(1x,20f10.5)') (Sto_Coef(i,j),i=1,n)
 else
  print *, 'Format is not provided for n=',n
  Write (*,'(1x,20f10.5)') (Sto_Coef(i,j),i=1,n) ! RLW 20080710 add for unspecified format
 endif
 ENDDO

 END SUBROUTINE Cal_Sto_Coef

      SUBROUTINE CURRDATE(yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc)
      integer yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
      integer ndpm(12),mdpm,fhrd,fhrh,itest
      data ndpm/31,28,31,30,31,30,31,31,30,31,30,31/

      yyc=yy
      mmc=mm
      ddc=dd
      hhc=hh

! Adgust dd and hh, while mm and yy unchanged
      fhrh=mod(fhr,24)
      fhrd=fhr/24
      hhc=hhc+fhrh
      ddc=ddc+fhrd
      if (hhc.ge.24) then
       hhc=hhc-24
       ddc=ddc+1
      endif

! Adjust mm and yy, and dd, Month by Month
      do itest=1,100
      mdpm=ndpm(mmc)
      if (mmc.eq.2.and.mod(yyc,4).eq.0) mdpm=mdpm+1
      if (mmc.eq.2.and.(mod(yyc,100).eq.0.and.mod(yyc,400).ne.0)) mdpm=mdpm-1
      if (mmc.le.12.and.ddc.le.mdpm) then
       print *, 'STPS INIT',yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
       return
      endif
      if (ddc.gt.mdpm) then
!Month advancement
        ddc=ddc-mdpm
        mmc=mmc+1
      endif
      if (mmc.gt.12) then
!Year advancement
           mmc=1
           yyc=yyc+1
      endif
      enddo

      print *, 'Forced stop in INITDATE subrountine'
      print *, yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
      stop
      end

      SUBROUTINE INITDATE(yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi)
      integer yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
      integer ndpm(12),mdpm,fhrd,fhrh,itest
      data ndpm/31,28,31,30,31,30,31,31,30,31,30,31/

      yyi=yy
      mmi=mm
      ddi=dd
      hhi=hh

! Adjust dd and hh, while mm and yy unchanged
      fhrh=mod(fhr,24)
      fhrd=fhr/24
      hhi=hhi-fhrh
      ddi=ddi-fhrd
      if (hhi.lt.0) then
       hhi=hhi+24
       ddi=ddi-1
      endif

!Adjust mm and yy, and dd, Month by Month
      do itest=1,100
      if (mmi.ge.1.and.ddi.ge.1) then
       print *, 'STPS INIT',yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
       return
      endif
      mdpm=ndpm(mmi-1)
      if (mmi.eq.1) mdpm=ndpm(12)
      if (mmi.eq.3.and.mod(yyi,4).eq.0) mdpm=mdpm+1
      if (mmi.eq.3.and.(mod(yyi,100).eq.0.and.mod(yyI,400).ne.0)) mdpm=mdpm-1
      if (ddi.lt.1) then
!Month advancement
        ddi=ddi+mdpm
        mmi=mmi-1
      endif
      if (mmi.lt.1) then
!Year advancement
           mmi=12
           yyi=yyi-1
      endif
      enddo

      print *, 'Forced stop in INITDATE subrountine'
      print *, yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
      stop
      end

