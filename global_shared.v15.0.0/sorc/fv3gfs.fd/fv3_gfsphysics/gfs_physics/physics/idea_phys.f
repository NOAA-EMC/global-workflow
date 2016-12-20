!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine idea_phys(im,ix,levs,prsi,prsl,                        &
     &                     adu,adv,adt,adr,ntrac,dtp,lat,               &
     &                     solhr,slag,sdec,cdec,sinlat,coslat,          &
     &                     xlon,xlat,oro,cozen,swh,hlw,dt6dt,           &
     &                     thermodyn_id,sfcpress_id,gen_coord_hybrid,me,&
     &                     mpi_ior,mpi_comm)
!-----------------------------------------------------------------------
! add temp, wind changes due to viscosity and thermal conductivity
! also solar heating
! Apr 06 2012   Henry Juang, initial implement for NEMS
! Jul 26 2012   Jun Wang, add mpi info
! Sep 06 2012   Jun Wang, add changing pressure to cb
! Dec    2012   Jun Wang, change to new rad_merge (from Rashid and Fei)
! May    2013   Jun Wang, tmp updated after rad_merge
! Jun    2013   S. Moorthi Some optimization and cosmetic changes
! Oct    2013   Henry Juang, correct the sequence to get prsi from model top
!-----------------------------------------------------------------------
      use physcons,  amo2=>con_amo2,avgd => con_avgd
      use idea_composition
!
      implicit none
! Argument
      integer, intent(in) :: im              ! number of data points in adt (first dim)
      integer, intent(in) :: ix              ! max data points in adt (first dim)
      integer, intent(in) :: levs            ! number of pressure levels
      integer, intent(in) :: lat             ! latitude index
      integer, intent(in) :: ntrac           ! number of tracer
      integer, intent(in) :: me              ! my pe
      integer, intent(in) :: mpi_ior         ! mpi real for io
      integer, intent(in) :: mpi_comm        ! mpi communicator
!
      real,    intent(in) :: dtp             ! time step in second
      real, intent(inout) :: prsi(ix,levs+1) ! pressure
      real, intent(inout) :: prsl(ix,levs)   ! pressure
      real, intent(in)    :: hlw(ix,levs)    ! long wave rad (K/s)
      real, intent(in)    :: swh(ix,levs)    ! short wave rad (K/s)
      real, intent(in)    :: cozen(im)       ! time avg(1 hour) cos zenith angle
      real, intent(in)    :: oro(im)         ! surface height (m)
      real, intent(in)    :: solhr,slag,sdec,cdec ! for solar zenith angle
      real, intent(in)    :: xlon(im),xlat(im),coslat(im),sinlat(im)
      real, intent(inout) :: adr(ix,levs,ntrac) ! tracer
      real, intent(inout) :: adt(ix,levs)    ! temperature
      real, intent(inout) :: adu(ix,levs)    ! real u
      real, intent(inout) :: adv(ix,levs)    ! real v
      real, intent(inout) :: dt6dt(ix,levs,6)! diagnostic array 
      integer,intent(in)  :: thermodyn_id, sfcpress_id
      logical,intent(in)  :: gen_coord_hybrid
! Local variables
      real,parameter      :: pa2cb=0.001,cb2pa=1000.
!
      real cp(ix,levs),cospass(im),dt(ix,levs),rtime1,hold1,n(ix,levs) 
      real  o_n(ix,levs),o2_n(ix,levs),n2_n(ix,levs),o3_n(ix,levs),     &
     & am(ix,levs),dudt(ix,levs),dvdt(ix,levs),dtdt(ix,levs),xmu(im),   &
     & dtco2c(ix,levs),dtco2h(ix,levs)                                  &
     &,dth2oh(ix,levs),dth2oc(ix,levs),dto3(ix,levs),rho(ix,levs)       &
     &,wtot(ix,levs),zg(ix,levs)                                        &
     &,amin,amax,grav(ix,levs)                                          &
     &,prslk(ix,levs),prsik(ix,levs+1),phil(ix,levs),phii(ix,levs+1)
! solar
      real utsec,sda,maglat(im),maglon(im),btot(im),                    &
     &     dipang(im),essa(im),dlat,dlon
      integer i,k,dayno,j1,j2

! change to real windl !hmhj already real wind
!hmhj do i=1,im
!hmhj   adu(i,1:levs)=adu(i,1:levs)/coslat(i)
!hmhj   adv(i,1:levs)=adv(i,1:levs)/coslat(i)
!hmhj enddo ! i
! get phil geopotential from temperature
! change prsi and prsl to centibar from pascal

      do k=1,levs
        do i=1,im
          prsi(i,k) = prsi(i,k)*pa2cb
          prsl(i,k) = prsl(i,k)*pa2cb
        enddo
      enddo
      do i=1,im
        prsi(i,levs+1) = prsi(i,levs+1)*pa2cb
      enddo

!hmhj call GET_PHI_gc_h(im,ix,levs,ntrac,adt,adr,prsi,phii,phil)
      call get_phi(im,ix,levs,ntrac,adt,adr,                            &
     &             thermodyn_id, sfcpress_id,                           &
     &             gen_coord_hybrid,                                    &
     &             prsi,prsik,prsl,prslk,phii,phil)
! get height
      call phi2z(im,ix,levs,phil,oro,zg,grav)
!     print*,'wwwz',zg(1,1:150)
!     print*,'wwwg',grav(1,1:150)
!     print*,'wwwp',phil(1,1:150),oro(1)
! 
! get composition at layers (/cm3) and rho (kg/m3)
      call idea_tracer(im,ix,levs,ntrac,2,grav,prsi,prsl,adt,adr,       &
     &                 dtp,o_n,o2_n,n2_n,n,rho,am)
! calculate cp
      call getcp_idea(im,ix,levs,ntrac,adr,cp,                          &
     &                thermodyn_id,gen_coord_hybrid)
! dissipation
      call idea_phys_dissipation(im,ix,levs,grav,prsi,prsl,             &
     &                           adu,adv,adt,o_n,o2_n,n2_n,dtp,cp,dt6dt)
!
! get cos solar zenith angle (instant)
      call presolar(im,ix,solhr,slag,                                   &
     &              sinlat,coslat,sdec,cdec,xlon,xlat                   &
     &              ,cospass,dayno,utsec,sda                            &
     &              ,maglat,maglon,btot,dipang,essa)
! get solar heating and NO cooling then get temp adjustment
      call idea_sheat(im,ix,levs,adt,dt,cospass,o_n,o2_n,n2_n,rho,      &
     &                cp,lat,dayno,prsl,zg,grav,am,maglat,dt6dt)
!     rtime1=3600.*6.

      do k=1,levs
        do i=1,im
          adt(i,k)     = adt(i,k) + dt(i,k)*dtp
!         dt3dt(i,k,1) = dt(i,k)*rtime1
!
! ion_drag  -  change to /m3
          o_n(i,k)  = o_n(i,k)  * 1.e6
          o2_n(i,k) = o2_n(i,k) * 1.e6
          n2_n(i,k) = n2_n(i,k) * 1.e6
          n(i,k)    = n(i,k)    * 1.e6
        enddo
      enddo

      call idea_ion(solhr,cospass,zg,o_n,o2_n,n2_n,cp,                  &
     &              adu,adv,adt,dudt,dvdt,dtdt,rho,xlat,xlon,ix,im,levs,&
     &              dayno,utsec,sda,maglon,maglat,btot,dipang,essa) 

!     do i=1,im
!      dlat=xlat(i)*180./3.14159
!      dlon=xlon(i)*180./3.14159
!      if(abs(dlat-60.).le.1..and.abs(dlon-270.).le.1.) then
!      print*,'www0',solhr,dudt(i,140)*dtp,dvdt(i,140)*dtp,             &
!    &dtdt(i,140)*dtp,adu(i,140),adv(i,140)
!      endif

      do k=1,levs
        do i=1,im
          adu(i,k) = adu(i,k) + dtp*dudt(i,k)
          adv(i,k) = adv(i,k) + dtp*dvdt(i,k)
          adt(i,k) = adt(i,k) + dtp*dtdt(i,k)
        enddo
      enddo

! change u,V back !hmhj no need to change back, they are real wind
!hmhj do i=1,im
!hmhj   adu(i,1:levs)=adu(i,1:levs)*coslat(i)
!hmhj   adv(i,1:levs)=adv(i,1:levs)*coslat(i)
!hmhj enddo ! i
! radiation
! co2 cooling, heating

      call idea_co2(im,ix,levs,nlev_co2,ntrac,grav,cp,adr,adt,          &
     &              dtco2c,cospass,dtco2h)

!hmhj&'/mtb/save/wx20fw/fcst07rd',dtco2c,cospass,dtco2h)
! h2o cooling heating 110-41 down ward

      call idea_h2o(im,ix,levs,nlev_h2o,nlevc_h2o,ntrac,grav,cp,        &
     &              adr,adt,dth2oh,cospass,dth2oc)

         dt6dt(1:im,1:levs,4) = dtco2c
!        dt6dt(1:im,1:levs,5) = dth2oc
!        dt6dt(1:im,1:levs,6) = dth2oh
!     dth2oc=0.
!     dth2oh=0.
! o2 o3 heating

      call o3pro(im,ix,levs,ntrac,adr,am,n,o3_n)
      call idea_o2_o3(im,ix,levs,cospass,adt,o2_n,o3_n,rho,cp,          &
     &                zg,grav,dto3)
! get xmu
      do i=1,im
        if(cospass(i) > 0.0001 .and. cozen(i) > 0.0001) then
          xmu(i) = cospass(i)/cozen(i)
        else
          xmu(i) = 0.
        endif
      enddo
! dt6dt
!     do i=1,im
!     do k=1,levs
!     dt6dt(i,k,2)=dtco2c(i,k)
!     dt6dt(i,k,3)=dtco2h(i,k)
!     dt6dt(i,k,4)=dth2oc(i,k)
!     dt6dt(i,k,5)=dth2oh(i,k)
!     dt6dt(i,k,6)=dto3(i,k)
!     enddo
!     enddo
! merge
      call rad_merge(im,ix,levs,hlw,swh,prsi,prsl,wtot,
     &               xmu,dtco2c,dtco2h,dth2oh,dth2oc,dto3,dt6dt)
      do k=1,levs
        do i=1,im
          adt(i,k)     = adt(i,k) + dtp*wtot(i,k)
! dt6dt
          dt6dt(i,k,2) = wtot(i,k)
!                                     change prsi and prsl back to pascal
          prsi(i,k)    = prsi(i,k)*cb2pa
          prsl(i,k)    = prsl(i,k)*cb2pa
        enddo
      enddo
      do i=1,im
        prsi(i,levs+1) = prsi(i,levs+1)*cb2pa
      enddo

      return
      end subroutine
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getcp_idea(im,ix,levs,ntrac,adr,xcp,                   & 
     &                      thermodyn_id,gen_coord_hybrid)
!
      use tracer_const
!hmhj use resol_def , only: thermodyn_id
!hmhj use namelist_def , only: gen_coord_hybrid
!
      implicit none
      integer, intent(in) :: im  ! number of data points in adr (first dim)
      integer, intent(in) :: ix  ! max data points in adr (first dim)
      integer, intent(in) :: levs   ! number of pressure levels
      integer, intent(in) :: ntrac  ! number of tracer
      real, intent(in) :: adr(ix,levs,ntrac) ! tracer kg/kg
      real, intent(out) :: xcp(ix,levs) !CP (J/kg/k)
      integer thermodyn_id
      logical gen_coord_hybrid
!
! local
      real sumq(ix,levs),work1
      integer i,j,k,ntb
      sumq = 0.0
      xcp  = 0.0
      if( gen_coord_hybrid .and. thermodyn_id == 3 ) then
        ntb = 1
      elseif (ntrac >= 4) then
        ntb = 4
      else
        return
      endif
      do i=ntb,ntrac
        if( cpi(i) /= 0.0 ) then
          do k=1,levs
            do j=1,im
              work1     = adr(j,k,i)
              sumq(j,k) = sumq(j,k) + work1
              xcp(j,k)  = xcp(j,k)  + work1*cpi(i)
            enddo
           enddo
        endif
      enddo
      do k=1,levs
        do j=1,im
          xcp(j,k) = xcp(j,k) + (1.-sumq(j,k))*cpi(0)
        enddo
      enddo
      return
      end
      subroutine rad_merge(im,ix,levs,hlw,swh,prsi,prsl,wtot,           &
     &                     xmu,dtco2c,dtco2h,dth2oh,dth2oc,dto3,dt6dt)
!
      implicit none
      integer, intent(in) :: im  ! number of data points in hlw,dt..(first dim)
      integer, intent(in) :: ix  ! max data points in hlw,... (first dim)
      integer, intent(in) :: levs             ! number of pressure levels
      real, parameter     :: xb=7.5, xt=8.5, rdx=1./(xt-xb)
      real, intent(in)    :: hlw(ix,levs)     ! GFS lw rad (K/s)
      real, intent(in)    :: swh(ix,levs)     ! GFS sw rad (K/s)
      real, intent(in)    :: prsi(ix,levs+1)  ! pressure
      real, intent(in)    :: prsl(ix,levs)    ! pressure
      real, intent(in)    :: xmu(im)          ! mormalized cos zenith angle
      real, intent(in)    :: dtco2c(ix,levs)  ! idea co2 cooling(K/s)
      real, intent(in)    :: dtco2h(ix,levs)  ! idea co2 heating(K/s)
      real, intent(in)    :: dth2oc(ix,levs)  ! idea h2o cooling(K/s)
      real, intent(in)    :: dth2oh(ix,levs)  ! idea h2o heating(K/s)
      real, intent(in)    :: dto3(ix,levs)    ! idea o3 heating(K/s)
      real, intent(out)   :: wtot(ix,levs)    ! GFS idea combined  rad
      real, intent(inout) :: dt6dt(ix,levs,6)
!     local
      real xk,wl,wh
      integer i,k,j
!
      do k=1,levs
        do i=1,im
          xk = log(prsi(i,1)/prsl(i,k))
          wh = dtco2c(i,k)+dth2oc(i,k)+dtco2h(i,k)+dth2oh(i,k)+dto3(i,k)
          wl = hlw(i,k)+swh(i,k)*xmu(i)
          if(xk < xb) then
             wtot(i,k) = wl
          elseif(xk >= xb .and. xk <= xt) then
             wtot(i,k) = (wl*(xt-xk) + wh*(xk-xb))*rdx
          else
             wtot(i,k) = wh
          endif
        enddo
      enddo
      return
      end
!
      subroutine getmax(ain,n1,n,m,rmin,j1,rmax,j2)
      real ain(n1,m)
      rmin =  1.e36
      rmax = -1.e36
      i1 = 500
      j1 = 500
      i2 = 500
      j2 = 500
      do j=1,m
        do i=1,n
          if(rmin > ain(i,j)) then
            rmin = ain(i,j)
            i1 = i
            j1 = j
          endif
          if(rmax < ain(i,j)) then
            rmax = ain(i,j)
            i2 = i
            j2 = j
          endif
        enddo
      enddo
      return
      end
      subroutine getmax2(ain,ain1,n1,n,m,rmax,j2)
      real ain(n1,m),ain1(n1,m)
      rmax = -1.e36
      i1   = 500
      j1   = 500
      i2   = 500
      j2   = 500
      do j=1,m
        do i=1,n
          sq = sqrt(ain(i,j)*ain(i,j) + ain1(i,j)*ain1(i,j))
          if(rmax < sq) then
            rmax = sq
            i2   = i
            j2   = j
          endif
        enddo
      enddo
      return
      end
      subroutine phi2z(im,ix,levs,phi,soro,z,grav)

! Subroutine to calculate geometric height and gravity from geopotential
! in a hydrostatic atmosphere, assuming a spherically symmetric planet
! and Newton's gravity.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! File history

! Feb 26, 2010: Rashid Akmaev
! Loosely based on Hojun Wang's phi2hgt but generalized to rid of
! recursive calculations, include surface orography, and calculate 
! gravity.

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Define constants
! - Earth radius (m) and 
! - gravity at sea level (m/s**2) 

! If used with GFS/WAM codes "use" this module
      use physcons, only: re => con_rerth, g0 => con_g

      implicit none

! If the module is not available, comment out the "use" line above and
! uncomment this line
!      real, parameter:: re = 6.3712e+6, g0 = 9.80665e+0

      real, parameter:: g0re = g0*re, g0re2 = g0*re*re

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine parameters
! INPUTS
! - array dimensions (following GFS conventios): first actual, first 
! maximum, number of levels

      integer, intent(in):: im,ix,levs

! - geopotential (m**2/s**2)
! - surface orography (m)

      real, intent(in):: phi(ix,levs),soro(im)

! OUTPUTS
! - height (m)
      
      real, intent(out):: z(ix,levs)

! Optional output
! - gravity (m/s**2)

!     real, intent(out), optional:: grav(ix,levs)
      real, intent(out):: grav(ix,levs)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Local variables

      integer:: i,l
      real:: phis(im)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate surface geopotential

      do i = 1,im
        phis(i) = g0re*soro(i)/(re+soro(i))
      enddo

! Calculate height

      z(:,:) = 0.
      do l = 1,levs
        do i = 1,im
          z(i,l) = re*(phis(i)+phi(i,l))/(g0re-(phis(i)+phi(i,l)))
        enddo
      enddo

! ***Optionally*** calculate gravity

!     if(present(grav)) then
         grav(:,:) = 0.
         do l = 1,levs
           do i = 1,im
             grav(i,l) = g0re2/((re+z(i,l))*(re+z(i,l)))
           enddo
         enddo
!     endif
      end subroutine phi2z
!----------------------------------------------------------------------------
      subroutine gravco2(levs,phi,soro,gg)

! Subroutine is modified from phi2z above to compute gravity for co2cin,
! the first gaussian point is chosen to represent the whole data domain

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! File history

! Dec 26, 2012: Jun Wang        modified from phi2z from Rashid

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Define constants
! - Earth radius (m) and
! - gravity at sea level (m/s**2)

! If used with GFS/WAM codes "use" this module
      use physcons, only: re => con_rerth, g0 => con_g

      implicit none

! If the module is not available, comment out the "use" line above and
      real, parameter:: g0re = g0*re, g0re2 = g0*re**2

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine parameters
! INPUTS

      integer, intent(in):: levs

! - geopotential (m**2/s**2)
! - surface orography (m)

      real, intent(in):: phi(levs),soro

! OUTPUTS

! Optional output
! - gravity (m/s**2)

      real, intent(out):: gg(levs)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Local variables

      integer:: i,l
      real:: phis
      real:: z(levs)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Calculate surface geopotential

      phis = g0re*soro/(re+soro)
      print *,'in grevco2 phis=',phis,'phi=',phi(1:100:10),'soro=',soro,
     &   're=',re

! Calculate height

      z(:) = 0.
      do l = 1,levs
        z(l) = re*(phis+phi(l))/(g0re-(phis+phi(l)))
      enddo

! ***Optionally*** calculate gravity

      gg(:) = 0.
      do l = 1,levs
         gg(l) = g0re2/((re+z(l))*(re+z(l)))
      enddo
      print *,'in grevco2 gg=',gg(1:100:10)
!
      end subroutine gravco2
!----------------------------------------------------------------------------
      subroutine getphilvl(levs,ntrac,ps,t,q,dp,gen_coord_hybrid,
     &  thermodyn_id,phil,prsi)

! Subroutine computes phi on a single point on model levels from p,tmp,
!  and trcers for general

! hybrid for enthalpy

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! File history

! Dec 26, 2010: Jun Wang

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      use tracer_const, only : ri
      implicit none

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine parameters
! INPUTS

      integer, intent(in):: levs,ntrac
      logical, intent(in):: gen_coord_hybrid
      integer, intent(in):: thermodyn_id
!
! Local variables
      real,parameter :: pa2cb=0.001,zero=0.0
! - sfc pressure  (pascal)
! - pressure  thickness (pascal)
! - tmp (k)
! - tracers

      real, intent(in):: ps,t(levs),dp(levs),q(levs,ntrac)

! OUTPUTS

! Optional output
! - model layer enthalpy

      real, intent(out):: phil(levs),prsi(levs+1)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Local variables
      real:: tem,dphi,phii,sumq(levs),xr(levs)
      integer :: k,n

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! init

      phii = zero

! Calculate enthalpy
!
!       print *,'in getphilvl,thermodyn_id=',thermodyn_id,
!     &    thermodyn_id.eq.3

      if( gen_coord_hybrid ) then
        if( thermodyn_id == 3 ) then           ! Enthalpy case
!get r
          sumq = zero
          xr   = zero
          do n=1,ntrac
            if( ri(n) > 0.0 ) then
              do k=1,levs
                xr(k)   = xr(k)   + q(k,n) * ri(n)
                sumq(k) = sumq(k) + q(k,n)
              enddo
            endif
          enddo
          do k=1,levs
            xr(k)    = (1.-sumq(k))*ri(0)  + xr(k)
          enddo
!
!hmhj     prsi(1) = ps*pa2cb
!hmhj     do k=1,levs
!hmhj       prsi(k+1) = prsi(k)-dp(k)*pa2cb
!hmhj     enddo
!hmhj if compute prsi, we from ptop=0 with dp down to psfc
          prsi(levs+1) = 0.
          do k=levs,1,-1
            prsi(k) = prsi(k+1) + dp(k)*pa2cb
          enddo
!          print *,'in getphilvl,prsi=',prsi(1:100:10)
!
          do k = 1,levs
            tem         = xr(k) * T(k)
            dphi        = (prsi(k) - prsi(k+1)) * TEM
     &                   /(prsi(k) + prsi(k+1))
            phil(k)   = phii + dphi
            phii      = phil(k) + dphi
          enddo
!
        else
          print *,'ERROR: No phil is compute, this routine is ',
     &          'for gen-hybrid  with enthalpy'
!
        endif
      endif
!
      end subroutine getphilvl
!------------------------------------------------------------------

