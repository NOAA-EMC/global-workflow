      module idea_tracer_mod
!-----------------------------------------------------------------------
! hold jprofile
! Apr 06 2012    Henry Juang, initial implement for nems
!-----------------------------------------------------------------------
      implicit none
!hmhj save
      real, allocatable::  jj(:) 
      end module idea_tracer_mod
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine idea_tracer_init(levs)
      use idea_tracer_mod
      implicit none
      integer, intent(in):: levs !number of pres levels
      allocate (jj(levs))
      call jprofile(levs,jj)
      return
      end subroutine
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine idea_tracer(im,ix,levs,ntrac,ntrac_i,grav,prsi,prsl,   &
     &adt,q,dtp,n1,n2,n3,n,rho,am)
!
      use physcons, only : amo2=>con_amo2,avgd => con_avgd,             &
     &                     amo3 => con_amo3,amh2o => con_amw
      use idea_composition, only : bz,amo,amn2
      use idea_tracer_mod
      implicit none
! Argument
      integer, intent(in) :: im    ! number of data points in up,dudt(first dim)
      integer, intent(in) :: ix    ! max data points in fields
      integer, intent(in) :: levs  ! number of pressure levels
      integer, intent(in) :: ntrac ! number of tracer (total)
      integer, intent(in) :: ntrac_i ! number of tracer add by IDEA
      real, intent(in)    :: prsi(ix,levs+1) ! interface pressure in KPa
      real, intent(in)    :: prsl(ix,levs)   ! layer pressure in KPa
      real, intent(in)    :: grav(ix,levs)   ! (m/s2)
      real, intent(in)    :: adt(ix,levs)   ! input  temp at dt=0
      real, intent(in)    :: dtp   ! time step in second
      real, intent(inout) :: q(ix,levs,ntrac)   ! input output tracer
      real, intent(out)   :: n1(ix,levs)   ! number density of o (/cm3)
      real, intent(out)   :: n2(ix,levs)   ! number density of o2 (/cm3)
      real, intent(out)   :: n3(ix,levs)   ! number density of n2 (/cm3)
      real, intent(out)   :: n(ix,levs)   ! total number density (/cm3)
      real, intent(out)   :: rho(ix,levs)   ! density of  (kg/m3)
      real, intent(out)   :: am(ix,levs)   ! avg mass of mix  (kg)
! local argument
      real dq1(ix,levs,ntrac_i),dq2(ix,levs,ntrac_i),mh2o,mo3,          &
     &qin(ix,levs,ntrac_i), mo,mo2,mn2,qsumo(ix,levs)
      integer i,k,in
!
      do in=1,ntrac_i
        do i=1,im
          do k=1,levs
            qin(i,k,in)=max(q(i,k,ntrac-ntrac_i+in),0.)
          enddo
        enddo
      enddo
      do i=1,im
        do k=1,levs
          qsumo(i,k)=q(i,k,1)+q(i,k,2)
        enddo
      enddo
! change unit from g/mol to kg
      mo=amo*1.e-3/avgd
      mo2=amo2*1.e-3/avgd
      mn2=amn2*1.e-3/avgd
      mh2o=amh2o*1.e-3/avgd
      mo3=amo3*1.e-3/avgd
! at layer , here n,n1,n2 unit is /m3 , rho is in kg/m3
      do i=1,im
        do k=1,levs
          am(i,k)=1./(qin(i,k,1)/mo+qin(i,k,2)/mo2+q(i,k,1)/mh2o+       &
     & q(i,k,2)/mo3+(1.-qin(i,k,1)-qin(i,k,2)-qsumo(i,k))/mn2)
!         am(i,k)=1./(qin(i,k,1)/mo+qin(i,k,2)/mo2+                     &
!    &              (1.-qin(i,k,1)-qin(i,k,2))/mn2)
          n(i,k)=prsl(i,k)*1000./(bz*adt(i,k))
          n1(i,k)=qin(i,k,1)*am(i,k)*n(i,k)/mo
          n2(i,k)=qin(i,k,2)*am(i,k)*n(i,k)/mo2
!         rho(i,k)=n1(i,k)*mo+n2(i,k)*mo2+(n(i,k)-n1(i,k)-n2(i,k))*mn2
          rho(i,k)=am(i,k)*n(i,k)
        enddo
      enddo
!
      call idea_tracer_m(im,ix,levs,ntrac_i,grav,prsi,prsl,adt,dtp,     &
     &qin,am,dq1)
      call idea_tracer_c(im,ix,levs,ntrac_i,adt,dtp,jj,n1,n2,n,rho,     &
     &qin,dq2)
!     print*,'www5',q(1:im,levs,4),dq1(1:im,levs,1),dq2(1:im,levs,1)
!     print*,'www5',dq1(1:im,levs,1),adt(1:im,levs)
      do in=1,ntrac_i
        do i=1,im
          do k=1,levs
            q(i,k,in+ntrac-ntrac_i)=q(i,k,in+ntrac-ntrac_i)+            &
     &        dq1(i,k,in)+dq2(i,k,in)
            q(i,k,in+ntrac-ntrac_i)=max(q(i,k,in+ntrac-ntrac_i),0.)
          enddo
        enddo
      enddo
! change n unit from /m3 to /cm3 to use in dissipation and solar_heating
      do i=1,im
        do k=1,levs
          n1(i,k)=n1(i,k)*1.e-6
          n2(i,k)=n2(i,k)*1.e-6
          n(i,k)=n(i,k)*1.e-6
          n3(i,k)=n(i,k)-n1(i,k)-n2(i,k)
        enddo
      enddo
      return
      end
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
      subroutine idea_tracer_m(im,ix,levs,ntrac_i,grav,prsi,prsl,adt,   &
     &dtp,qin,am,dq)
!-----------------------------------------------------------------------
!
! calaulate tracer changes caused by molecular diffusion
!
!-----------------------------------------------------------------------
      use physcons, only :rgas=>con_rgas, amo2=>con_amo2,               &
     &               avgd => con_avgd
      use machine, only : kind_phys
      use idea_composition
      implicit none
! Argument
      integer, intent(in) :: im    ! number of data points in up,dudt(first dim)
      integer, intent(in) :: ix    ! max data points in fields
      integer, intent(in) :: levs  ! number of pressure levels
      integer, intent(in) :: ntrac_i ! number of tracer add by IDEA
      real,    intent(in) :: dtp   ! time step in second
      real, intent(in)    :: prsi(ix,levs+1) ! interface pressure in KPa
      real, intent(in)    :: prsl(ix,levs)   ! layer pressure in KPa
      real, intent(in)    :: grav(ix,levs)   ! (m/s2)
      real, intent(in) :: adt(ix,levs)   ! input  temp at dt=0
      real, intent(in) :: qin(ix,levs,ntrac_i)   ! input tracer
      real, intent(in)   :: am(ix,levs)   ! avg mass of mix  (kg)
      real, intent(out):: dq(ix,levs,ntrac_i) ! output tracer changes
!local  variables
      real n1_i(levs+1),n2_i(levs+1),n3_i(levs+1),n_i(levs+1)
      real t_i(levs+1),am_i(levs+1),qout(ix,levs,ntrac_i)
      real beta(2,2,levs+1),a(2,2,levs),b(2,2,levs),c(2,2,levs)
      real ggg(2,2),ee(2,2,levs+1),f(2,levs+1),                         &
     &     d12,d13,d23,a12,a13,a23,s12,s13,s23,mo,mo2,mn2,              &
     &     dp1(levs),dp1_i(levs+1)
      real partb_i(levs+1),parta(levs),hold1,dtp1,hold2
      integer k,i,kk,kk1,in
! change unit from g/mol to kg
      mo=amo*1.e-3/avgd
      mo2=amo2*1.e-3/avgd
      mn2=amn2*1.e-3/avgd
! some constants
      a12=9.69e18
      a13=9.69e18
      a23=8.3e18
c
      s12=0.774
      s13=0.774
      s23=0.724
! set boundary
      beta(1:2,1:2,1)=0.
      beta(1:2,1:2,levs+1)=0.
      a(1:2,1:2,1)=0.
      c(1:2,1:2,levs)=0.
      ee(1:2,1:2,levs+1)=0.
      f(1:2,levs+1)=0.
!
      dtp1=1./dtp
      t_i=0.
      am_i=0.
      n_i=0.
      n1_i=0.
      n2_i=0.
      n3_i=0.
!
! for each longitude
!
      do i=1,im
! calculate temp in interface pressure levels
! get compositions at interface pressure levels
        do k=2,levs
          t_i(k)=(adt(i,k-1)+adt(i,k))*.5
          am_i(k)=.5*(am(i,k-1)+am(i,k))
          n_i(k)=prsi(i,k)*1000./bz/t_i(k)
          n1_i(k)=.5*(qin(i,k,1)+qin(i,k-1,1))*am_i(k)*n_i(k)/mo
          n2_i(k)=.5*(qin(i,k,2)+qin(i,k-1,2))*am_i(k)*n_i(k)/mo2
          n3_i(k)=n_i(k)-n1_i(k)-n2_i(k)
        enddo
       if(i.eq.6) then
!      print*,'www6-n1_i',i,n1_i(2:levs)
!      print*,'www6-n_i',i,n_i(2:levs)
!      print*,'www6-t_i',i,t_i(2:levs)
!      print*,'www6-am_i',i,am_i(2:levs)
       endif
!printout
!       if(i.eq.1) then
!         do k=1,levs
!           print'(i3,6e11.4,2f5.0)',k,prsi(1,k),prsl(1,k),coef_i(k,1), &
!    &coef_i(k,2),cp(k),hs_i(k),t_i(k),up(i,k,3)
!         enddo
!        endif
! calculate beta at interface pressure
        do k=2,levs
          d12=a12*t_i(k)**(s12)
          d13=a13*t_i(k)**(s13)
          d23=a23*t_i(k)**(s23)
          hold1=1./(n1_i(k)*d23+n2_i(k)*d13+n3_i(k)*d12)
          beta(1,1,k)=hold1*d13*mo*(n1_i(k)*mn2*d23+                    &
     &            (n2_i(k)*mo2+n3_i(k)*mn2)*d12)
          beta(2,2,k)=hold1*d23*mo2*(n2_i(k)*mn2*d13+                   &
     &            (n1_i(k)*mo+n3_i(k)*mn2)*d12)
          beta(1,2,k)=hold1*d23*mo*n1_i(k)*(mn2*d13-mo2*d12)
          beta(2,1,k)=hold1*d13*mo2*n2_i(k)*(mn2*d23-mo*d12)
!      if(i.eq.6) print*,'www6-beta',i,k,beta(1,1,k),hold1,n1_i(k),     &
!    & n2_i(k),n3_i(k),d12,d13,d23,t_i(k),mo2,mn2
        enddo
!      if(i.eq.6) print*,'www6-beta',i,beta(1,1,2:levs)
! solve tridiagonal problem
        do k=1,levs
          dp1(k)=1./(prsi(i,k)-prsi(i,k+1))
          parta(k)=dtp*grav(i,k)*.001*dp1(k)/bz
        enddo
        do k=2,levs
          dp1_i(k)=1./(prsl(i,k-1)-prsl(i,k))
          partb_i(k)=.5*(grav(i,k)+grav(i,k-1))/t_i(k)
        enddo
        do k=2,levs
          hold1=parta(k)*partb_i(k)
          hold2=am(i,k-1)*prsl(i,k-1)*dp1_i(k)
          a(1,1,k)=hold1*beta(1,1,k)*(hold2/mo-.5)
          a(1,2,k)=hold1*beta(1,2,k)*(hold2/mo2-.5)
          a(2,1,k)=hold1*beta(2,1,k)*(hold2/mo-.5)
          a(2,2,k)=hold1*beta(2,2,k)*(hold2/mo2-.5)
         enddo
!      print*,'www6-a',i,a(1:2,1:2,levs-3:levs)
        do k=1,levs-1
          hold1=parta(k)*partb_i(k+1)
          hold2=am(i,k+1)*prsl(i,k+1)*dp1_i(k+1)
          c(1,1,k)=hold1*beta(1,1,k+1)*(hold2/mo+.5)
          c(1,2,k)=hold1*beta(1,2,k+1)*(hold2/mo2+.5)
          c(2,1,k)=hold1*beta(2,1,k+1)*(hold2/mo+.5)
          c(2,2,k)=hold1*beta(2,2,k+1)*(hold2/mo2+.5)
         enddo
        do k=2,levs-1
          hold1=am(i,k)*prsl(i,k)*dp1_i(k+1)
          hold2=am(i,k)*prsl(i,k)*dp1_i(k)
      b(1,1,k)=1.+parta(k)*(partb_i(k+1)*beta(1,1,k+1)*(hold1/mo-.5)    &
     &                    +partb_i(k)*beta(1,1,k)*(hold2/mo+.5))
      b(2,2,k)=1.+parta(k)*(partb_i(k+1)*beta(2,2,k+1)*(hold1/mo2-.5)   &
     &                    +partb_i(k)*beta(2,2,k)*(hold2/mo2+.5))
      b(1,2,k)=parta(k)*(partb_i(k+1)*beta(1,2,k+1)*(hold1/mo2-.5)      &
     &                    +partb_i(k)*beta(1,2,k)*(hold2/mo2+.5))
      b(2,1,k)=parta(k)*(partb_i(k+1)*beta(2,1,k+1)*(hold1/mo-.5)       &
     &                    +partb_i(k)*beta(2,1,k)*(hold2/mo+.5))
        enddo
          hold1=am(i,1)*prsl(i,1)*dp1_i(2)
      b(1,1,1)=1.+parta(1)*partb_i(2)*beta(1,1,2)*(hold1/mo-.5)
      b(2,2,1)=1.+parta(1)*partb_i(2)*beta(2,2,2)*(hold1/mo2-.5)
      b(1,2,1)=parta(1)*partb_i(2)*beta(1,2,2)*(hold1/mo2-.5)
      b(2,1,1)=parta(1)*partb_i(2)*beta(2,1,2)*(hold1/mo-.5)
          hold2=am(i,levs)*prsl(i,levs)*dp1_i(levs)
      b(1,1,levs)=1.+parta(levs)*partb_i(levs)*beta(1,1,levs)*          &
     &(hold2/mo+.5)
      b(2,2,levs)=1.+parta(levs)*partb_i(levs)*beta(2,2,levs)*          &
     &(hold2/mo2+.5)
      b(1,2,levs)=parta(levs)*partb_i(levs)*beta(1,2,levs)*             &
     &(hold2/mo2+.5)
      b(2,1,levs)=parta(levs)*partb_i(levs)*beta(2,1,levs)*             &
     &(hold2/mo+.5)
       do k=levs,1,-1
         ggg(1,1)=b(2,2,k)-c(2,1,k)*ee(1,2,k+1)-c(2,2,k)*ee(2,2,k+1)
         ggg(2,2)=b(1,1,k)-c(1,1,k)*ee(1,1,k+1)-c(1,2,k)*ee(2,1,k+1)
         ggg(1,2)=-1.*b(1,2,k)+c(1,1,k)*ee(1,2,k+1)+c(1,2,k)*ee(2,2,k+1)
         ggg(2,1)=-1.*b(2,1,k)+c(2,1,k)*ee(1,1,k+1)+c(2,2,k)*ee(2,1,k+1)
         hold1=1./(ggg(1,1)*ggg(2,2)-ggg(1,2)*ggg(2,1))
         ggg=ggg*hold1
         ee(1,1,k)=ggg(1,1)*a(1,1,k)+ggg(1,2)*a(2,1,k)       
         ee(1,2,k)=ggg(1,1)*a(1,2,k)+ggg(1,2)*a(2,2,k)       
         ee(2,1,k)=ggg(2,1)*a(1,1,k)+ggg(2,2)*a(2,1,k)       
         ee(2,2,k)=ggg(2,1)*a(1,2,k)+ggg(2,2)*a(2,2,k)       
      f(1,k)=ggg(1,1)*(qin(i,k,1)+c(1,1,k)*f(1,k+1)                      &
     &+c(1,2,k)*f(2,k+1))+ggg(1,2)*(qin(i,k,2)+c(2,1,k)*f(1,k+1)         &
     &+c(2,2,k)*f(2,k+1))
      f(2,k)=ggg(2,1)*(qin(i,k,1)+c(1,1,k)*f(1,k+1)                      &
     &+c(1,2,k)*f(2,k+1))+ggg(2,2)*(qin(i,k,2)+c(2,1,k)*f(1,k+1)         &
     &+c(2,2,k)*f(2,k+1))
        enddo
        do in=1,ntrac_i
          qout(i,1,in)=f(in,1)
          dq(i,1,in)=qout(i,1,in)-qin(i,1,in)
        enddo
        do k=2,levs
          qout(i,k,1)=ee(1,1,k)*qout(i,k-1,1)+ee(1,2,k)*qout(i,k-1,2)+  &
     &              f(1,k)
          qout(i,k,2)=ee(2,1,k)*qout(i,k-1,1)+ee(2,2,k)*qout(i,k-1,2)+  &
     &              f(2,k)
          do in=1,ntrac_i
            dq(i,k,in)=qout(i,k,in)-qin(i,k,in)
          enddo
        enddo
      enddo !i
      return
      end subroutine
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc!   
      subroutine idea_tracer_c(im,ix,levs,ntrac_i,adt,dtp,jj,n1,n2,     &
     &n,rho,qin,dq)
!-----------------------------------------------------------------------
!
! calaulate tracer changes caused by chemistry reaction
!
!-----------------------------------------------------------------------
      use physcons, only : rgas=>con_rgas, amo2=>con_amo2
      use physcons, only : avgd => con_avgd
      use machine, only : kind_phys
      use idea_composition
      implicit none
!
! Argument
      integer, intent(in) :: im    ! number of data points in up,dudt(first dim)
      integer, intent(in) :: ix    ! max data points in fields
      integer, intent(in) :: levs  ! number of pressure levels
      integer, intent(in) :: ntrac_i ! number of tracer add by IDEA
      real,    intent(in) :: dtp   ! time step in second
      real, intent(in) :: adt(ix,levs)   ! input  temp at dt=0
      real, intent(in) :: qin(ix,levs,ntrac_i)   ! input tracer
      real, intent(in) :: jj(levs)   ! input photo diss rate
      real, intent(in) :: n1(ix,levs)! number density of o
      real, intent(in) :: n2(ix,levs)! number density of o2
      real, intent(in) :: n(ix,levs)! number density of mixture
      real, intent(in) :: rho(ix,levs)! density of mixture
      real, intent(out):: dq(ix,levs,ntrac_i) ! output
! Local variables
      real k1,k2,p1,p2,L1,L2,mo,mo2,mn2,qout(ix,levs,ntrac_i)
      integer k,i
!
      mo=amo*1.e-3/avgd
      mo2=amo2*1.e-3/avgd
      mn2=amn2*1.e-3/avgd
!
      do k=1,levs
      do i=1,im
! get coefficent array o o2 n2
        k1=4.7e-45*(300./adt(i,k))**2
        k2=6.e-46*(300./adt(i,k))**(2.3)
        p1=2.*jj(k)*n2(i,k)*mo/rho(i,k)
        p2=k1*n1(i,k)**2*n(i,k)*mo2/rho(i,k)
        L1=2.*k1*n1(i,k)*n(i,k)+k2*n2(i,k)*n(i,k)
        L2=k2*n1(i,k)*n(i,k)+jj(k)
        qout(i,k,1)=(qin(i,k,1)+p1*dtp)/(1.+L1*dtp)
        qout(i,k,2)=(qin(i,k,2)+p2*dtp)/(1.+L2*dtp)
        dq(i,k,1)=qout(i,k,1)-qin(i,k,1)
        dq(i,k,2)=qout(i,k,2)-qin(i,k,2)
      enddo
      enddo
      return
      end subroutine
!-------------------------------------------------------------------------
      SUBROUTINE jprofile(levs,J)
! get photo dissociation rate
      use idea_composition, only : f107 => f107_idea
      implicit none
      integer, parameter :: np=17  !number of pressure levels of orig
      integer, intent(in) :: levs  !number of pressure levels of output 
      real,    intent(out):: J(levs)
! local variables
      real JI(np),FHT(np),C(np),J17(np)
      integer k
!
      DATA C/8*0.900,0.680,0.43,0.18,6*-0.066/
      DATA JI/.4e-8,.78e-8,1.5e-8,3.e-8,6.8e-8,.15e-6,.34e-6,.77e-6,    &
     &1.07e-6,1.35e-6,1.6e-6,1.81e-6,2.05e-6,2.23e-6,2.36e-6,2.5e-6,    &
     &2.57e-6/
      DATA FHT/8*1.2,1.85,2.50,3.150,6*3.8/
! calculate photo dissociation rate (/s) in Tims 17 pressure grid
      do k=1,17                                                   
        J17(k)=JI(k)*((FHT(k)-1.0)*f107(1)/176.+C(k))
      enddo
! interplate to GFS pressure grid
      call z17toz(levs,J17,J,0.)
      return
      end
!-------------------------------------------------------------------------
      subroutine z17toz(levs,ain,aout,down)
! interpolate 17 pressure levels (from Tim's grid) to
! idea pressure grid pr(levs)
      use idea_composition, only : pr=> pr_idea
      implicit none
      integer, parameter :: np=17  !number of pressure levels of input
      integer, intent(in) :: levs  !number of pressure levels of output 
      real,    intent(in) :: ain(np)  !input field in 17 pressure grid
      real,    intent(in) :: down     !field value under 5.2285Pa
      real,    intent(out):: aout(levs)!output in levs pressure grid
!local variable
      real p17(np),z17(np),z(levs),dz
      integer kref,k,i
!
      do k=1,np
        p17(k)=5.2285*exp(1.-k)
        z17(k)=-1.*log(p17(k))
      enddo
      do k=1,levs
        z(k)=-1.*log(pr(k)*100.)
      enddo
      do k=1,levs
        kref=0
        do i=1,np-1
          if(z(k).ge.z17(i).and.z(k).le.z17(i+1)) then
            kref=i
            dz=(z(k)-z17(i))/(z17(i+1)-z17(i))
          endif
        enddo
        if(kref.ne.0) then
          aout(k)=dz*ain(kref+1)+(1.-dz)*ain(kref)
        elseif(z(k).lt.z17(1)) then
          aout(k)=down
        elseif(z(k).gt.z17(17)) then
          aout(k)=ain(17)
        endif
      enddo
      return
      end
