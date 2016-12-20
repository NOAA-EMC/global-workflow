      subroutine meteorg(npoint,rlat,rlon,istat,elevstn,
     &             nf,nfile,fnsig,jdate,idate,
     &             iromb,maxwv,kwskip,levso,levs,kdim)

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    meteorg                           
!   PRGMMR: HUALU PAN        ORG: W/NMC23    DATE: 1999-07-21
!
! ABSTRACT: Creates BUFR meteogram files for the AVN and MRF.
!
! PROGRAM HISTORY LOG:
!   1999-07-21  HUALU PAN                            
!   2007-02-02  FANGLIN YANG  EXPAND FOR HYBRID COORDINATES USING SIGIO 
!   2009-07-24  FANGLIN YANG  CHANGE OUTPUT PRESSURE TO INTEGER-LAYER 
!                             PRESSURE (line 290)
!                             CORRECT THE TEMPERATURE ADJUSTMENT (line 238)
!   2014-03-27  DANA CARLIS   UNIFY CODE WITH GFS FORECAST MODEL PRECIP
!                             TYPE CALCULATION
!
! USAGE:    CALL PROGRAM meteorg
!   INPUT:
!     npoint           - number of points
!     rlat(npint)      - latitude
!     rlonnpoint)      - longtitude
!     istat(npoint)    - station id
!     elevstn(npoint)  - station elevation (m)
!     nf               - forecast cycle         
!     fnsig            - sigma file name        
!     idate(4)         - date                   
!     iromb            - for triangular truncation iromb==0
!     maxwv            - wave number
!     kwskip           - spectral array
!     levso            - output vertical layers
!     levs             - input vertical layers
!     kdim             - sfc file dimension         
!
!   OUTPUT:    
!     nfile            - output data file channel       
!     jdate            - date YYYYMMDDHH       
!
! ATTRIBUTES:
!   LANGUAGE: 
!   MACHINE:  IBM SP
!
!$$$
      use sigio_module
      use physcons
      use mersenne_twister
      use funcphys
      implicit none 
      include 'mpif.h'
      type(sigio_head):: sighead,sigheado
      type(sigio_data):: sigdata
      integer :: kwskip,iromb,maxwv,nfile,npoint,levso,levs,kdim
      real :: scalar(kwskip,2+levs*2)
      real :: vector(kwskip,levs*2)
      integer :: idate(4),nsig,nsfc,nflx,np,k,l,nf
      integer :: iret,iret1,jdate,nsil,leveta,lm,lp1
      character*150 :: fnsig
      real*8 :: data(6*levso+24)
      real :: fhour,pp,ppn,qs,qsn,esn,es,psfc,ppi,dtemp,iwx,nd
      real :: t,q,u,v,td,tlcl,plcl,qw,tw,xlat,xlon,iossil
      real,dimension(npoint,2+levso*3):: grids,gridsi
      real,dimension(npoint) :: rlat,rlon,pmsl,ps,psn,elevstn
      real,dimension(npoint,levso) :: gridu,gridv,omega,qnew,zp
      real,dimension(npoint,2):: gradx, grady
      real,dimension(npoint,levso) :: griddiv,gridui,gridvi,omegai
      real,dimension(npoint,levso) :: p1,p2,p3,pd1,pd2,pd3,tt,ttnew
      real,dimension(npoint,levso+1) :: pi3
      real :: zp2(2)
      real,dimension(kdim,npoint) :: sfc
      real,dimension(1,levso+1) :: prsi,phii
      real,dimension(1,levso) ::  gt0,gq0,prsl,phy_f3d
      real :: PREC,TSKIN,SR,randomno(1,2)
      real :: DOMR,DOMZR,DOMIP,DOMS
      integer :: n3dfercld,iseedl,time
      integer :: istat(npoint)
      logical :: trace

      nsig = 11
      nsfc = 12
      nflx = 6 * levso

      call sigio_sropen(nsig,trim(fnsig),iret)
      call sigio_srhead(nsig,sighead,iret1)
      if(iret.ne.0 .or. iret1.ne.0) then
        print*,"fail to open sigma file" 
        stop
      endif
      fhour=sighead%fhour
      idate=sighead%idate
      print *, ' processing forecast hour ', fhour
      print *, ' idate =', idate
      jdate = idate(4)*1000000 + idate(2)*10000+
     &        idate(3)*100 + idate(1)
      print *, 'jdate = ', jdate
      print *, 'Total number of stations = ', npoint
! Open grads output binary data file
!    open (125, file='gradsout.dat')

! read sigio data
      call sigio_aldata(sighead,sigdata,iret)
      call sigio_srdata(nsig,sighead,sigdata,iret)

! topography (m)
      scalar(:,1)=sigdata%hs
! surface pressure (Pa)
      scalar(:,2)=sigdata%ps
! virtual temperature (k)
      do k = 1, levs
        scalar(:,k+2)=sigdata%T(:,k)
      enddo
! divergence and vorticity
      do k = 1, levs
        vector(:,k)=sigdata%d(:,k)
        vector(:,k+levs)=sigdata%z(:,k)
      enddo
! specific humidity
      do k = 1, levs
        scalar(:,k+2+levs)=sigdata%q(:,k,1)
      enddo
!
! read surface data
      read(nsfc,err=910) sfc
!
!  use splib to do spectral transform
!
        call sptgpt(iromb,maxwv,2*levs+2,npoint,
     &       kwskip,npoint,1,1,rlat,rlon,scalar,gridsi)
        call sptgptsd(iromb,maxwv,2,npoint,
     &       kwskip,npoint,1,1,rlat,rlon,scalar,gridsi,gradx,grady)
        call sptgptv(iromb,maxwv,levs,npoint,
     &       kwskip,npoint,1,1,rlat,rlon,vector,vector(1,levs+1),
     &       gridui,gridvi)
        call sptgpt(iromb,maxwv,levs,npoint,
     &       kwskip,npoint,1,1,rlat,rlon,vector,griddiv)
        do np = 1, npoint
        !ps in kPa
          ps(np) = exp(gridsi(np,2))
        enddo
!
!   compute omega(Pa/s) and interface layer pressure (Pa) 
!
      do np=1,npoint
       call modstuff(levs,sighead%idvc,sighead%idsl,
     &       sighead%nvcoord,sighead%vcoord,
     &       ps(np)*1000,gradx(np,2),grady(np,2),griddiv(np,1:levs),
     &       gridui(np,1:levs),gridvi(np,1:levs),
     &       pd1(np,1:levs),p1(np,1:levs),omegai(np,1:levs))
      enddo
!
!  put omega (pa/s) in the tracer to prepare for interpolation
!
        do k = 1, levs
          do np = 1, npoint
            gridsi(np,2+levs*2+k) = omegai(np,k)
          enddo
        enddo
!        print *, ' omegai ='
!        print 6102, (omegai(1,k),k=1,levs)

!        -----------------
         sigheado=sighead
!        -----------------
        if(levs.ne.levso) then
          nsil = 13
          rewind nsil
          call newsig(nsil,sigheado%idvc,levso,
     &           sigheado%nvcoord,sigheado%vcoord,iossil)
          if(iossil.ne.0) print*, "fail to read new levels"

!  obtain new interface-layer pressure for new levso
          do np = 1, npoint
            call sigio_modpr(1,1,levso,sigheado%nvcoord,sigheado%idvc,
     &           sigheado%idsl,sigheado%vcoord,iret,
     &           ps=ps(np)*1000,pd=pd2(np,1:levso),pm=p2(np,1:levso))
          enddo
          do np = 1, npoint
            grids(np,1) = gridsi(np,1)
            grids(np,2) = gridsi(np,2)
          enddo
          call vintg(npoint,npoint,levs,levso,2,
     &      p1,gridui,gridvi,gridsi(1,3),gridsi(1,3+levs),
     &      p2,gridu, gridv, grids (1,3),grids (1,3+levso))
          do k = 1, levso
            do np = 1, npoint
              omega(np,k) = grids(np,2+levso*2+k) 
            enddo
          enddo
        else
          do k = 1, levs
            do np = 1, npoint
              p2(np,k)    = p1(np,k)
              gridu(np,k) = gridui(np,k)
              gridv(np,k) = gridvi(np,k)
              omega(np,k) = omegai(np,k) 
            enddo
          enddo
! Put topo(1),surf press(2),vir temp(3:66),and specifi hum(67:130) in grids
! for each station
          do k = 1, 2*levs+2
            do np = 1, npoint
              grids(np,k) = gridsi(np,k)
            enddo
          enddo
        endif  !END OF IF STATMENT LEVS .NE. LEVSO
        do np = 1, npoint
          ps(np) = exp(gridsi(np,2))
        enddo
!
!  move surface pressure to the station surface from the model surface
!
        do np = 1, npoint
!
!  when the station elevation information in the table says missing,
!    use the model elevation
!
!          print *, "elevstn = ", elevstn(np)
          if(elevstn(np).eq.-999.) elevstn(np) = grids(np,1)
          psn(np) = ps(np) * exp(-con_g*(elevstn(np)-grids(np,1)) /
     &              (con_rd * grids(np,3)))
          call sigio_modpr(1,1,levso,sigheado%nvcoord,sigheado%idvc,
     &         sigheado%idsl,sigheado%vcoord,iret,
     &         ps=psn(np)*1000,pd=pd3(np,1:levso),pm=p3(np,1:levso))
          grids(np,2) = log(psn(np))
        enddo
!
!  convert virtual temperature to temperature 
!
        do k = 1, levso
          do np = 1, npoint
            tt(np,k) = grids(np,k+2) / (1. + .608 *
     &                 grids(np,k+levso+2))
          enddo
        enddo
!
!  move t to new levels conserving theta
!  move q to new levels conserving RH
!
        do k = 1, levso
          do np = 1, npoint
            pp  = p2(np,k)            
            ppn = p3(np,k)              
            ttnew(np,k) = tt(np,k) * (ppn/pp)**(con_rocp)
            call svp(qsn,esn,ppn,ttnew(np,k))
            call svp(qs,es,pp,tt(np,k))
            qnew(np,k) = grids(np,k+levso+2) * qsn / qs
          enddo
        enddo
!
!  move the new values into the old
!
        do np = 1, npoint
          ps(np) = psn(np)
        enddo
        do k = 1, levso
          do np = 1, npoint
            grids(np,k+2) = ttnew(np,k)
            grids(np,k+levso+2) = qnew(np,k)
          enddo
        enddo
!
!  get sea-level pressure (Pa) and layer geopotential height
!
        do np=1,npoint
          call gslp(levso,elevstn(np),ps(np)*1000,
     &      p3(np,1:levso),ttnew(np,1:levso),qnew(np,1:levso),
     &      pmsl(np),zp(np,1:levso),zp2(1:2))
        enddo
!
!  prepare buffer data
!
        do np = 1, npoint
          pi3(np,1)=psn(np)*1000
          do k=1,levso
            pi3(np,k+1)=pi3(np,k)-pd3(np,k)    !layer pressure (Pa)
          enddo
          data(1) = ifix(fhour+.2) * 3600    ! FORECAST TIME (SEC)
          data(2) = istat(np)                ! STATION NUMBER
          data(3) = rlat(np)                 ! LATITUDE  (DEG N)
          data(4) = rlon(np)                 ! LONGITUDE (DEG E)
!         data(5) = grids(np,1)              ! STATION ELEVATION (M)
          data(5) = elevstn(np)              ! STATION ELEVATION (M)
          psfc = 10. * psn(np)               ! convert to MB
          leveta = 1
          do k = 1, levso
!
!  look for the layer above 500 mb for precip type computation
!
            if(pi3(np,k).ge.50000.) leveta = k
            ppi = pi3(np,k)                 
            t = grids(np,k+2)
            q = max(1.e-8,grids(np,2+k+levso))
            u = gridu(np,k)
            v = gridv(np,k)
!           data((k-1)*6+6) = pi3(np,k)                ! PRESSURE (PA) at interface layer
            data((k-1)*6+6) = p3(np,k)                 ! PRESSURE (PA) at integer layer
            data((k-1)*6+7) = t                        ! TEMPERATURE (K)
            data((k-1)*6+8) = u                        ! U WIND (M/S)
            data((k-1)*6+9) = v                        ! V WIND (M/S)
            data((k-1)*6+10) = q                       ! HUMIDITY (KG/KG)
            data((k-1)*6+11) = omega(np,k)             ! Omega (pa/sec)
          enddo
!
!  process surface flux file fields
!
          data(7+nflx) = psfc * 100.                   ! SURFACE PRESSURE (PA)
          data(6+nflx) = pmsl(np)                           
          dtemp = .0100 * (grids(np,1) - elevstn(np))
          sfc(37,np) = data(6+nflx) * .01
          sfc(39,np) = zp2(2)   !500 hPa height       
!
!  do height correction if there is no snow or if the temp is less than 0
!
          if(sfc(10,np).eq.0.) then
            sfc(30,np) = sfc(30,np) + dtemp
            sfc(5,np) = sfc(5,np) + dtemp
          endif
          if(sfc(10,np).gt.0..and.sfc(5,np).lt.273.16) then
            sfc(5,np) = sfc(5,np) + dtemp
            if(sfc(5,np).gt.273.16) then
              dtemp = sfc(5,np) - 273.16
              sfc(5,np) = 273.16
            endif
            sfc(30,np) = sfc(30,np) + dtemp
          endif
          data(8+nflx) = sfc(5,np)                       ! tsfc (K)
          data(9+nflx) = sfc(6,np)                       ! 10cm soil temp (K)
          data(10+nflx) = sfc(17,np)                     ! evaporation (w/m**2)
          data(11+nflx) = sfc(12,np)                     ! total precip (m)
          data(12+nflx) = sfc(11,np)                     ! convective precip (m)
          data(13+nflx) = sfc(10,np)                     ! water equi. snow (m)
          data(14+nflx) = sfc(27,np)                     ! low cloud (%)
          data(15+nflx) = sfc(26,np)                     ! mid cloud
          data(16+nflx) = sfc(25,np)                     ! high cloud
          data(17+nflx) = sfc(34,np)                     ! U10 (m/s)
          data(18+nflx) = sfc(35,np)                     ! V10 (m/s)
          data(19+nflx) = sfc(30,np)                     ! T2 (K)
          data(20+nflx) = sfc(31,np)                     ! Q2 (K)

          data(21+nflx) = 0.
          data(22+nflx) = 0.
          data(23+nflx) = 0.
          data(24+nflx) = 0.
          iwx = 0
          nd = 0
          trace = .false.
          DOMS=0.
          DOMR=0.
          DOMIP=0.
          DOMZR=0.
          if(np.eq.1.or.np.eq.2) nd = 1
          if(np.eq.1.or.np.eq.2) trace = .true.

          if(sfc(12,np).gt.0.) then !check for precip then calc precip type
          do k = 1, leveta+1
            pp = p3(np,k)              
            ppi = pi3(np,k)               
            t = grids(np,k+2)
            q = max(0.,grids(np,2+k+levso))
            u = gridu(np,k)
            v = gridv(np,k)
            if(q.gt.1.e-6.and.pp.ge.20000.) then
              call tdew(td,t,q,pp)
              call lcl(tlcl,plcl,t,pp,q)
              call mstadb(qw,tw,pp,q,tlcl,plcl)
            else
              td = t - 30.
              tw = t - 30.
            endif
!  Calpreciptype input variables
            gt0(1,k)= t
            gq0(1,k) = q
            prsl(1,k) = pp
            prsi(1,k)=ppi
            phii(1,k)=zp(np,k)     !height in meters 
          enddo
!       Use GFS routine calpreciptype.f to calculate precip type
            xlat=rlat(np)
            xlon=rlon(np)
            lm=leveta
            lp1=leveta+1
            PREC=data(11+nflx)
            n3dfercld=1  !if =3 then use Ferriers Explicit Precip Type
            TSKIN=1.     !used in Ferriers Explicit Precip Scheme
            SR=1.        !used in Ferriers Explicit Precip Scheme
            iseedl=jdate
            call random_setseed(iseedl)
            call random_number(randomno)
            call calpreciptype(1,1,1,1,lm,lp1,randomno,xlat,xlon, !input
     &      gt0,gq0,prsl,prsi,PREC,phii,n3dfercld,TSKIN,SR,phy_f3d, !input
     &      DOMR,DOMZR,DOMIP,DOMS)  ! Output vars
          endif
          data(nflx + 21) = DOMS
          data(nflx + 22) = DOMIP
          data(nflx + 23) = DOMZR
          data(nflx + 24) = DOMR
          if(np.eq.1.or.np.eq.100) then
            print *, ' surface fields for hour', nf, 'np =', np
            print *, (data(l+nflx),l=1,24)
            print *, ' temperature sounding'
            print 6101, (data((k-1)*6+7),k=1,levso)
            print *, ' omega sounding'
            print 6102, (data((k-1)*6+11),k=1,levso)
            print *, ' divergence sounding'
            print 6103, (griddiv(np,k),k=1,levs)
          endif
          write(nfile) data
        enddo  !End loop over stations np
        print *, 'Finished writing bufr data file'
 6101   format(2x,6f12.3)
 6102   format(2x,2p6f12.5)
 6103   format(2x,6p6f12.5)
!
!  write out surface binary file for grads
!
!       time=0.
!       nlev=1
!       nflag=1
!       write(gstid,'(i8)') istat(np)
!write(125,'(i4,x,i2,x,i2,x,i2,3x,a8,3x,f5.2,x,f7.2,x,4f5.2)')
!     &jdat(1),jdat(2),jdat(3),jdat(5),gstid,rlat(np),rlon(np),
!     &DOMR,DOMS,DOMIP,DOMZR

      call sigio_axdata(sigdata,iret)
      call sigio_sclose(nsig,iret)
      close(unit=nfile)
      return
 910  print *, ' error reading surface flux file'
      end

!-----------------------------------------------------------------------
