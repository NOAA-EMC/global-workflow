!program main
!real(8), dimension(14,14) :: sto_coef
!do k=1,10
! print *, 'k=',k
!call cal_sto_coef(15, k, sto_coef)
!do j=1,14
! write (*,'(1x,14f10.5)') (sto_coef(i,j),i=1,14)
!enddo
!enddo
!stop
!end
!subroutine cal_sto_coef(total_member, cpl_run_calling_number, sto_coef,year,mm,dd,hh,fhr,eps0,nr0,eps,nr)
 subroutine cal_sto_coef(total_member, t1, t2, delt, sto_coef,year,mm,dd,hh,fhr,eps0,nr0,eps,nr)

! this subroutine is used to compute the stochastic coefficients
! used to combine the tendency perturbation to generate the 
! stochastic forcing for each ensemble member 
!-------------------------------------------------------------

 use pran 
 use peuc
 use prana

 implicit none
 integer                                                             :: total_member
!integer                                                             :: cpl_run_calling_number
 integer                 :: t,t1,t2, delt
 real(8), dimension(total_member - 1, total_member - 1) :: sto_coef
!real(kind=kind_evod), dimension(total_member - 1, total_member - 1) :: sto_coef
!real(kind=kind_evod), dimension(total_member - 1)                   :: rp

 integer                 :: i0
 integer                 :: n, nt
 integer                 :: iseed0, iseed2
 integer                 :: iseed1(1:2)
!integer                 :: iseed3(1:2),iseed4(1:2)
!integer                 :: year,mm,dd,hh,nth,fhr,delt,fflag
 integer                 :: year,mm,dd,hh,nth,fhr,fflag
!year-year,mm=month,dd=day,hh=hour,fhr=forecast_hour
 integer                 :: i,j,k
 integer                 :: ksize,nrank,r,t0,iunit,nr0,nr
 real(8)                 :: x,eps0,eps,g
 integer                 :: yyc,mmc,ddc,hhc,fhrc,yyi,mmi,ddi,hhi

! working arrays.
!----------------
 real(8), dimension(total_member-1,total_member-1) :: a,b,c,d,e,ident,b0
!real(kind=kind_evod), dimension(total_member - 1, total_member - 1) :: work1
!real(kind=kind_evod), dimension(total_member - 1, total_member - 1) :: work2

!setting the constants
!year = 2006 
!mm = 08
!dd = 16
!hh = 00
!fhr = 120
 print *,'stps',year,mm,dd,hh,fhr
 print *,'stps',total_member, t1, t2, delt
 print *,'stps',eps0,nr0,eps,nr

 n = total_member - 1
!t = cpl_run_calling_number
 ident=identity(total_member - 1)

!setting the seed (seed2) based on the date(yyyymmddhh) 
!and generate the initial value of the sto_coef-matric 
 call random_seed(size=ksize)
 print *,'stps',ksize
      call initdate(year,mm,dd,hh,fhr,yyi,mmi,ddi,hhi)

do t=t1+1,t2,1
      fhrc=delt*t
      call currdate(yyi,mmi,ddi,hhi,fhrc,yyc,mmc,ddc,hhc)
 iseed0=yyc+mmc+ddc+hhc+1
!iseed0=year+mm+dd+hh+1
 iseed1=iseed0
!gwv call random_seed(generator=2)
 call random_seed(put=iseed1(1:ksize))
!call random_seed(get=iseed3(1:ksize))
 call random_number(x)
!call random_seed(get=iseed4(1:ksize))
 iseed2=iseed0+nint(x*1000)
 print *, 'stps iseed_a:', x,iseed0,iseed1,iseed2
!print *, 'stps iseed_a:', x,iseed3,iseed4
!if (cpl_run_calling_number .eq. 1 ) then
!if (t1 .eq. 1 ) then
!if (t1 .eq. 0 ) then    !up to em
 if (t .eq. 1 ) then     !starting in en
   call plant1(iseed2)
   call ranrot(a)
   sto_coef = a
!  print *,a
 endif

!generating the fixed rotation matrix (b0) based on eps0 and nr0
 if (eps0.lt.0.000001.or.nr0.lt.1) then
 b0=ident
 else
 iseed2=iseed2-1*delt*1000
 print *, 'stps iseed_b:', iseed2
 call plant1(iseed2)
 do j=1,n; do i=1,n; call gauss(b(i,j)); enddo; enddo
!print *,b
!apply gram process to the matrix (i+epsilon*a) where a=i+(b-bt)
 b=b-transpose(b)
 c=ident+eps0*b
 call gram(c,nrank);b=c
!now b is r matrix if nr0=1
 do r=1,nr0-1;
   b=matmul(b,c)
 enddo
! now b is the r matrix after nr0 times of rotation   
 b0=b
 endif
!print *,b0

!do t=t1,t2,1
!do t=t1+1,t2,1
!generate the random rotation matrix (b) based on eps and nr
 iseed2=iseed2+t*delt*1000
 print *, 'stps iseed_c:', iseed2
 call plant1(iseed2)
   do j=1,n; do i=1,n; call gauss(b(i,j)); enddo; enddo
!apply gram process to the matrix (i+epsilon*a) where a=i+(b-bt)
   b=b-transpose(b)
   c=ident+eps*b
   call gram(c,nrank);b=c
!now b is r matrix if nr=1
 do r=1,nr-1;
   b=matmul(b,c)
 enddo
! now b is the r matrix after nr times of rotation   
!print *,b

! generate the current matrix (b=a x b0 x b)
   a=sto_coef
   b=matmul(b0,b)   !b=b0 x b1, r(t)=r0(t=0) x r1(t)
   d=matmul(a,b)   !d=a x b, c(t)=c(t-1) x r
! updating a matrix (c)
   a=d
   sto_coef = d
enddo  !  the loop of t=t1+1,t2,1

! print the coefficients!
  print *, 'stps coefficients:'
 do j=1,n
 if (n.eq.14) then
  write (*,'(1x,14f10.5)') (sto_coef(i,j),i=1,n)
 elseif (n.eq.2) then
  write (*,'(1x,2f10.5)') (sto_coef(i,j),i=1,n)
 elseif (n.eq.3) then
  write (*,'(1x,3f10.5)') (sto_coef(i,j),i=1,n)
 elseif (n.eq.20) then
  write (*,'(1x,20f10.5)') (sto_coef(i,j),i=1,n)
 else
  print *, 'format is not provided for n=',n
  write (*,'(1x,20f10.5)') (sto_coef(i,j),i=1,n) ! rlw 20080710 add for unspecified format
 endif
 enddo

 end subroutine cal_sto_coef

      subroutine currdate(yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc)
      integer yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
      integer ndpm(12),mdpm,fhrd,fhrh,itest
      data ndpm/31,28,31,30,31,30,31,31,30,31,30,31/

      yyc=yy
      mmc=mm
      ddc=dd
      hhc=hh

! adgust dd and hh, while mm and yy unchanged
      fhrh=mod(fhr,24)
      fhrd=fhr/24
      hhc=hhc+fhrh
      ddc=ddc+fhrd
      if (hhc.ge.24) then
       hhc=hhc-24
       ddc=ddc+1
      endif

! adjust mm and yy, and dd, month by month
      do itest=1,100
      mdpm=ndpm(mmc)
      if (mmc.eq.2.and.mod(yyc,4).eq.0) mdpm=mdpm+1
      if (mmc.eq.2.and.(mod(yyc,100).eq.0.and.mod(yyc,400).ne.0)) mdpm=mdpm-1
      if (mmc.le.12.and.ddc.le.mdpm) then
       print *, 'stps init',yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
       return
      endif
      if (ddc.gt.mdpm) then
!month advancement
        ddc=ddc-mdpm
        mmc=mmc+1
      endif
      if (mmc.gt.12) then
!year advancement
           mmc=1
           yyc=yyc+1
      endif
      enddo

      print *, 'forced stop in initdate subrountine'
      print *, yy,mm,dd,hh,fhr,yyc,mmc,ddc,hhc
      stop
      end

      subroutine initdate(yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi)
      integer yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
      integer ndpm(12),mdpm,fhrd,fhrh,itest
      data ndpm/31,28,31,30,31,30,31,31,30,31,30,31/

      yyi=yy
      mmi=mm
      ddi=dd
      hhi=hh

! adjust dd and hh, while mm and yy unchanged
      fhrh=mod(fhr,24)
      fhrd=fhr/24
      hhi=hhi-fhrh
      ddi=ddi-fhrd
      if (hhi.lt.0) then
       hhi=hhi+24
       ddi=ddi-1
      endif

!adjust mm and yy, and dd, month by month
      do itest=1,100
      if (mmi.ge.1.and.ddi.ge.1) then
       print *, 'stps init',yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
       return
      endif
      mdpm=ndpm(mmi-1)
      if (mmi.eq.1) mdpm=ndpm(12)
      if (mmi.eq.3.and.mod(yyi,4).eq.0) mdpm=mdpm+1
      if (mmi.eq.3.and.(mod(yyi,100).eq.0.and.mod(yyi,400).ne.0)) mdpm=mdpm-1
      if (ddi.lt.1) then
!month advancement
        ddi=ddi+mdpm
        mmi=mmi-1
      endif
      if (mmi.lt.1) then
!year advancement
           mmi=12
           yyi=yyi-1
      endif
      enddo

      print *, 'forced stop in initdate subrountine'
      print *, yy,mm,dd,hh,fhr,yyi,mmi,ddi,hhi
      stop
      end

