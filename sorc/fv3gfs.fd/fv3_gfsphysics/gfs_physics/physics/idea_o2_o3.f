      subroutine idea_o2_o3(im,ix,levs,cosz,adt,o2_n,o3_n,rho,cp,       &
     &zg,grav,dth)
!
! Apr 06 2012  Henry Juang, initial implement for nems
! Jan 02 2013  Jun Wang,    move o3ini out of column physics
!
      use physcons,    pi=>con_pi, avgd=>con_avgd                       &
     &               , amo3=> con_amo3 , amo2=> con_amo2
      use idea_composition
!
      implicit none
! Argument
      integer, intent(in) :: im  ! number of data points in adt (first dim)
      integer, intent(in) :: ix  ! max data points in adt (first dim)
      integer, intent(in) :: levs   ! number of pressure levels
      real, intent(in)    :: cosz(im)        ! cos zenith angle
      real, intent(in)    :: adt(ix,levs)    !temp(k) 
      real, intent(in)    :: o2_n(ix,levs)    ! /m3
      real, intent(in)    :: o3_n(ix,levs)    ! /m3
      real, intent(in)    :: rho(ix,levs)    ! kg/m3
      real, intent(in)    :: cp(ix,levs)    ! J/kg/k
      real, intent(in)    :: zg(ix,levs)    ! height (m)
      real, intent(in)    :: grav(ix,levs)    ! (m/s2)
      real, intent(out)   :: dth(ix,levs)    ! heating rate k/s
!
      real hc,fc,dc,hha,fha,dha,hhu,i1,i2,m,dhu,lams,laml               &
     &,hhz,fhz,dhzo2,dhzo3,hsrb,fsrb,dsrb,ysrb,h1,rodfac
      real clmo2(levs),clmo3(levs) 
      integer i,k
!
!
      fc=370.  !J/m2/s
      dc=2.85E-25 !m2
      fha=5.13  !J/m2/s
      dha=8.7E-22 !m2
      i1=0.07   !J/m2/s/A
      i2=0.05
      m=0.01273   !/A
      lams=2805.
      laml=3015.
      dhu=1.15e-6 !m2
      fhz=1.5   !J/m2/s
      dhzo2=6.e-28  !m2
      dhzo3=4.e-22  !m2
      fsrb=0.0128   !J/m2/s
      dsrb=2.07e-24  !m2
      ysrb=0.0152
!
      dth=0.
      do i=1,im
        if(cosz(i).ge.0.) then
          rodfac=35./sqrt(1224.*cosz(i)**2+1.)
          clmo2(levs)=1.e3*o2_n(i,levs)*bz*adt(i,levs)*avgd/            &
     &           (grav(i,levs)*amo2)
          clmo3(levs)=1.e3*o3_n(i,levs)*bz*adt(i,levs)*avgd/            &
     &           (grav(i,levs)*amo3)
          do k=levs-1,1,-1
            clmo2(k)=clmo2(k+1)+.5*(o2_n(i,k+1)+o2_n(i,k))              &
!    &              *(phil(i,k+1)-phil(i,k))/g                          &
     &              *(zg(i,k+1)-zg(i,k))
            clmo3(k)=clmo3(k+1)+.5*(o3_n(i,k+1)+o3_n(i,k))              &
!    &              *(phil(i,k+1)-phil(i,k))/g                          &
     &              *(zg(i,k+1)-zg(i,k))
          enddo
          clmo2=clmo2*rodfac   !rad path
          clmo3=clmo3*rodfac
          do k=1,levs
            hc=fc*dc*exp(-1.*dc*clmo3(k))
            hha=fha*dha*exp(-1.*dha*clmo3(k))
            hhu=(i1+(i2-i1)*exp(-1.*dhu*clmo3(k)*exp(-1.*m*laml))       &
     &       -i2*exp(-1.*dhu*clmo3(k)*exp(-1.*m*lams)))                 &
     &       /(m*clmo3(k))
            hhz=fhz*(dhzo2*o2_n(i,k)+dhzo3*o3_n(i,k))*exp(-1.*          &
     &        dhzo2*clmo2(k)-dhzo3*clmo3(k))
            h1=sqrt(1.+4.*dsrb*clmo2(k)/(pi*ysrb))
            hsrb=fsrb*dsrb*o2_n(i,k)*exp(-.5*pi*ysrb*(h1-1.))/h1
!           dth(i,k)=((hc+hha+hhu)*o3_n(i,k)+hhz+hsrb)/                 &
!    &             (cp(i,k)*rho(i,k))
            dth(i,k)=((hc+hha*ef(k)+hhu)*o3_n(i,k)+hhz+hsrb)/           &
     &             (cp(i,k)*rho(i,k))
          enddo
        else
          dth(i,1:levs)=0.
        endif
      enddo
      return
      end
      subroutine o3pro(im,ix,levs,ntrac,adr,am,n,o3_n)
!
      use physcons, amo3=> con_amo3, avgd=> con_avgd 
      use idea_composition
!
      implicit none
! Argument
      integer, intent(in) :: im  ! number of data points in adt (first dim)
      integer, intent(in) :: ix  ! max data points in adt (first dim)
      integer, intent(in) :: levs   ! number of pressure levels
      integer, intent(in) :: ntrac   ! number of tracer
      real, intent(in)    :: adr(ix,levs,ntrac)    ! gfs tracer
      real, intent(in)    :: am(ix,levs)    ! mixture mol weight kg
      real, intent(in)    :: n(ix,levs)    ! number density  /m3
      real, intent(out)   :: o3_n(ix,levs)    ! /m3
!
      real rate,mo3
      integer i,k
!
      mo3=amo3*1.e-3/avgd
      do i=1,im
        do k=1,k71-1
          o3_n(i,k)=adr(i,k,2)*am(i,k)*n(i,k)/mo3
        enddo
          rate=adr(i,k71,2)/o3ra(k71)
        do k=k71,levs
          o3_n(i,k)=o3ra(k)*rate*am(i,k)*n(i,k)/mo3
        enddo
      enddo
      return
      end
      subroutine o3ini(levs)
!
      use idea_composition
!
      implicit none
      integer,intent(in) :: levs  ! number of pressure levels
      integer i
      real c0(2),c1(2),c2(2),c3(2),logp,x

!     data c0/0.66965,0.92621/
!     data c1/-0.009682,0.13396/
!     data c2/0.033093,-0.076863/
!     data c3/0.017938,0.006897/
      data c0/0.66965,0.932363/
      data c1/-0.009682,0.139425/
      data c2/0.033093,-0.076863/
      data c3/0.017938,0.005075/
!
      allocate (ef(levs))
      do i=1,levs
        logp=log10(pr_idea(i))
          if(logp.ge.0.) then
            ef(i)=c0(2)+c1(2)+c2(2)+c3(2)  
          elseif(logp.ge.-2) then
            x=1.+logp
            ef(i)=c0(2)+c1(2)*x+c2(2)*x**2+c3(2)*x**3  
          elseif(logp.ge.-4) then
            x=3.+logp
            ef(i)=c0(1)+c1(1)*x+c2(1)*x**2+c3(1)*x**3  
          else
            ef(i)=c0(1)-c1(1)+c2(1)-c3(1)  
          endif
      enddo
      return
      end
