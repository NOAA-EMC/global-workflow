      subroutine gwdc(im,ix,iy,km,lat,u1,v1,t1,q1,
     &                pmid1,pint1,dpmid1,qmax,ktop,kbot,kcnv,cldf,
!    &                pmid1,pint1,dpmid1,qmax,cumchr1,ktop,kbot,kcnv,
     &                grav,cp,rd,fv,dlength,lprnt,ipr,fhour,
     &                utgwc,vtgwc,tauctx,taucty)
!    &                gwdcloc,critic,brunm1,rhom1)

!***********************************************************************
!        original code for parameterization of convectively forced
!        gravity wave drag from yonsei university, korea
!        based on the theory given by chun and baik (jas, 1998)
!        modified for implementation into the gfs/cfs by
!        ake johansson  --- aug 2005
!***********************************************************************

      use machine , only : kind_phys
      implicit none

!---------------------------- arguments --------------------------------
!
!  input variables
!
!  u        : midpoint zonal wind
!  v        : midpoint meridional wind
!  t        : midpoint temperatures
!  pmid     : midpoint pressures
!  pint     : interface pressures
!  dpmid    : midpoint delta p ( pi(k)-pi(k-1) )
!  lat      : latitude index
!  qmax     : deep convective heating
!  kcldtop  : vertical level index for cloud top    ( mid level ) 
!  kcldbot  : vertical level index for cloud bottom ( mid level )
!  kcnv     : (0,1) dependent on whether convection occur or not
!
!  output variables
!
!  utgwc    : zonal wind tendency
!  vtgwc    : meridional wind tendency
!
!-----------------------------------------------------------------------

      integer im, ix, iy, km, lat, ipr
      integer ktop(im),kbot(im),kcnv(im)

      real(kind=kind_phys) grav,cp,rd,fv,fhour,fhourpr
      real(kind=kind_phys), dimension(ix)      :: qmax
     &,                                           tauctx, taucty
      real(kind=kind_phys), dimension(im)      :: cldf,dlength
      real(kind=kind_phys), dimension(ix,km)   :: u1,v1,t1,q1,
     &                                            pmid1,dpmid1
!    &,                                           cumchr1
      real(kind=kind_phys), dimension(iy,km)   :: utgwc,vtgwc
      real(kind=kind_phys), dimension(ix,km+1) :: pint1

      logical lprnt

!------------------------- local workspace -----------------------------
!
!  i, k     : loop index
!  kk       : loop index
!  cldf     : deep convective cloud fraction at the cloud top.
!  ugwdc    : zonal wind after gwdc paramterization
!  vgwdc    : meridional wind after gwdc parameterization
!  plnmid   : log(pmid) ( mid level )
!  plnint   : log(pint) ( interface level )
!  dpint    : delta pmid ( interface level )
!  tauct    : wave stress at the cloud top calculated using basic-wind
!             parallel to the wind vector at the cloud top ( mid level )
!  tauctx   : wave stress at the cloud top projected in the east
!  taucty   : wave stress at the cloud top projected in the north
!  qmax     : maximum deep convective heating rate ( k s-1 ) in a  
!             horizontal grid point calculated from cumulus para-
!             meterization. ( mid level )
!  wtgwc    : wind tendency in direction to the wind vector at the cloud top level
!             due to convectively generated gravity waves ( mid level )
!  utgwcl   : zonal wind tendency due to convectively generated 
!             gravity waves ( mid level )
!  vtgwcl   : meridional wind tendency due to convectively generated
!             gravity waves ( mid level )
!  taugwci  : profile of wave stress calculated using basic-wind
!             parallel to the wind vector at the cloud top 
!  taugwcxi : profile of zonal component of gravity wave stress
!  taugwcyi : profile of meridional component of gravity wave stress 
!
!  taugwci, taugwcxi, and taugwcyi are defined at the interface level
!
!  bruni    : brunt-vaisala frequency ( interface level )
!  brunm    : brunt-vaisala frequency ( mid level )
!  rhoi     : air density ( interface level )
!  rhom     : air density ( mid level )
!  ti       : temperature ( interface level )
!  basicum  : basic-wind profile. basic-wind is parallel to the wind
!             vector at the cloud top level. (mid level) 
!  basicui  : basic-wind profile. basic-wind is parallel to the wind
!             vector at the cloud top level. ( interface level )
!  riloc    : local richardson number ( interface level )
!  rimin    : minimum richardson number including both the basic-state
!             and gravity wave effects ( interface level )
!  gwdcloc  : horizontal location where the gwdc scheme is activated.
!  break    : horizontal location where wave breaking is occurred.
!  critic   : horizontal location where critical level filtering is
!             occurred.
!  dogwdc   : logical flag whether the gwdc parameterization is           
!             calculated at a grid point or not.
!  
!  dogwdc is used in order to lessen cpu time for gwdc calculation.
!
!-----------------------------------------------------------------------

      integer i,ii,k,k1,kk,kb,ilev,npt,kcb,kcldm,npr
      integer, dimension(im) :: ipt

      real(kind=kind_phys) tem, tem1,  tem2, qtem, wtgwc, tauct,
     &                     windcltop,  shear, nonlinct, nonlin, nonlins,
     &                     n2,   dtdp,  crit1, crit2, pi, p1, p2,
     &                     gsqr,  onebg
!    &                     taus, n2,   dtdp,  crit1, crit2, pi, p1, p2

      integer,              allocatable :: kcldtop(:),kcldbot(:)
      logical,              allocatable :: do_gwc(:)
      real(kind=kind_phys), allocatable :: tauctxl(:), tauctyl(:),
     &                                     gwdcloc(:), break(:),
     &                                     critic(:),
!    &                                     critic(:),  angle(:),
     &                                     cosphi(:),  sinphi(:),
     &                                     xstress(:), ystress(:),
     &                                     ucltop(:),  vcltop(:),
     &                                     wrk(:),
     &                                     dlen(:),       gqmcldlen(:)
!     real(kind=kind_phys), allocatable :: plnint(:,:),   dpint(:,:),
!    &                                     taugwci(:,:),  taugwcxi(:,:),
!    &                                     taugwcyi(:,:), bruni(:,:),
!    &                                     taugwcyi(:,:), bruni(:,:),
      real(kind=kind_phys), allocatable :: plnint(:,:),
     &                                     taugwci(:,:),  bruni(:,:),
     &                                     rhoi(:,:),     basicui(:,:),
     &                                     ti(:,:),       riloc(:,:),
     &                                     rimin(:,:),    pint(:,:)
!     real(kind=kind_phys), allocatable :: ugwdc(:,:),    vgwdc(:,:),
      real(kind=kind_phys), allocatable :: 
!    &                                     plnmid(:,:),   wtgwc(:,:),
     &                                     plnmid(:,:),
     &                                     utgwcl(:,:),   vtgwcl(:,:),
     &                                     basicum(:,:),  u(:,:),v(:,:),
     &                                     t(:,:),        spfh(:,:),
     &                                     pmid(:,:),     dpmid(:,:),
!    &                                     pmid(:,:),     cumchr(:,:),
     &                                     brunm(:,:),    rhom(:,:)

!-----------------------------------------------------------------------
!
!  ucltop    : zonal wind at the cloud top ( mid level )
!  vcltop    : meridional wind at the cloud top ( mid level )
!  windcltop : wind speed at the cloud top ( mid level )
!  shear     : vertical shear of basic wind 
!  cosphi    : cosine of angle of wind vector at the cloud top
!  sinphi    : sine   of angle of wind vector at the cloud top
!  c1        : tunable parameter
!  c2        : tunable parameter
!  dlength   : grid spacing in the direction of basic wind at the cloud top
!  nonlinct  : nonlinear parameter at the cloud top
!  nonlin    : nonlinear parameter above the cloud top
!  nonlins   : saturation nonlinear parameter
!  taus      : saturation gravity wave drag == taugwci(i,k)
!  n2        : square of brunt-vaisala frequency
!  dtdp      : dt/dp
!  xstress   : vertically integrated zonal momentum change due to gwdc
!  ystress   : vertically integrated meridional momentum change due to gwdc
!  crit1     : variable 1 for checking critical level
!  crit2     : variable 2 for checking critical level
!
!-----------------------------------------------------------------------

      real(kind=kind_phys), parameter ::
     &                      c1=1.41,          c2=-0.38,     ricrit=0.25
     &,                     n2min=1.e-32,     zero=0.0,     one=1.0
     &,                     taumin=1.0e-20,   tauctmax=-5.
     &,                     qmin=1.0e-10,     shmin=1.0e-20
     &,                     rimax=1.0e+20,    rimaxm=0.99e+20
     &,                     rimaxp=1.01e+20,  rilarge=0.9e+20
     &,                     riminx=-1.0e+20,  riminm=-1.01e+20
     &,                     riminp=-0.99e+20, rismall=-0.9e+20

!
      npt = 0
      do i = 1,im
        ipt(i) = 0
        if (kcnv(i) /= 0 .and. qmax(i) > zero) then
          npt      = npt + 1
          ipt(npt) = i
        endif
      enddo
      do k=1,km
        do i=1,im
          utgwc(i,k) = 0.0
          vtgwc(i,k) = 0.0
!         brunm(i,k) = 0.0
!         rhom(i,k)  = 0.0
          enddo
      enddo
      do i=1,im
        tauctx(i) = 0.0
        taucty(i) = 0.0
      enddo
      if (npt == 0) return      ! no gwdc calculation done!

!***********************************************************************
!
!  begin gwdc
!
!***********************************************************************

!-----------------------------------------------------------------------
!        write out incoming variables
!-----------------------------------------------------------------------

      fhourpr = zero
!     if (lprnt) then
!       if (fhour >= fhourpr) then
!         print *,' '
!         write(*,*) 'inside gwdc raw input start print at fhour = ',
!    &               fhour
!         write(*,*) 'ix  im  km  ',ix,im,km
!         write(*,*) 'kbot ktop qmax dlength kcnv  ',
!    +     kbot(ipr),ktop(ipr),qmax(ipr),dlength(ipr),kcnv(ipr)
!         write(*,*) 'grav  cp  rd  ',grav,cp,rd

!-------- pressure levels ----------
!         write(*,9100)
!         ilev=km+1
!         write(*,9110) ilev,(10.*pint1(ipr,ilev))
!         do ilev=km,1,-1
!           write(*,9120) ilev,(10.*pmid1(ipr,ilev)),
!    &                         (10.*dpmid1(ipr,ilev))
!           write(*,9110) ilev,(10.*pint1(ipr,ilev))
!         enddo

!-------- u1 v1 t1 ----------
!         write(*,9130)
!         do ilev=km,1,-1
!           write(*,9140) ilev,u1(ipr,ilev),v1(ipr,ilev),t1(ipr,ilev)
!         enddo

!         print *,' '
!         print *,' inside gwdc raw input end print'
!       endif
!     endif

!9100 format(//,14x,'pressure levels',//,
!    +' ilev',6x,'pint1',7x,'pmid1',6x,'dpmid1',/)
!9110 format(i4,2x,f10.3)
!9120 format(i4,12x,2(2x,f10.3))
!9130 format(//,' ilev',7x,'u1',10x,'v1',10x,'t1',/)
!9140 format(i4,3(2x,f10.3))

!     allocate local arrays

      allocate (kcldtop(npt), kcldbot(npt), do_gwc(npt))
      allocate (tauctxl(npt), tauctyl(npt),
     &          gwdcloc(npt), break(npt), critic(npt),   cosphi(npt),
     &          sinphi(npt),  xstress(npt),  ystress(npt), wrk(npt),
     &          ucltop(npt),  vcltop(npt),dlen(npt),     gqmcldlen(npt))

!     allocate (plnint(npt,km+1),   dpint(npt,km+1),
!    &          taugwci(npt,km+1),  taugwcxi(npt,km+1),
!    &          taugwcyi(npt,km+1), bruni(npt,km+1),
      allocate (plnint(npt,km+1),
     &          taugwci(npt,km+1),  bruni(npt,km+1),
     &          rhoi(npt,km+1),     basicui(npt,km+1),
     &          ti(npt,km+1),       riloc(npt,km+1),
     &          rimin(npt,km+1),    pint(npt,km+1))

!     allocate (ugwdc(npt,km),   vgwdc(npt,km),
      allocate 
!    &         (plnmid(npt,km),  wtgwc(npt,km),
     &         (plnmid(npt,km),
     &          utgwcl(npt,km),  vtgwcl(npt,km),
     &          basicum(npt,km), u(npt,km),    v(npt,km),
     &          t(npt,km),       spfh(npt,km), pmid(npt,km),
     &          dpmid(npt,km),
!    &          dpmid(npt,km),   cumchr(npt,km),
     &          brunm(npt,km),   rhom(npt,km))

!-----------------------------------------------------------------------
!        create local arrays with reversed vertical indices
!        and initialize local variables
!-----------------------------------------------------------------------
      gsqr  = grav * grav
      onebg = one / grav

      if (lprnt) then
        npr = 1
        do i=1,npt
          if (ipr == ipt(i))then
            npr = i
            exit
          endif
        enddo
      endif

      do k=1,km
        k1 = km - k + 1
        do i=1,npt
          ii = ipt(i)
          u(i,k)        = u1(ii,k1)
          v(i,k)        = v1(ii,k1)
          t(i,k)        = t1(ii,k1)
          spfh(i,k)     = max(q1(ii,k1),qmin)
          pmid(i,k)     = pmid1(ii,k1)
          dpmid(i,k)    = dpmid1(ii,k1) * onebg
!         cumchr(i,k)   = cumchr1(ii,k1)

          rhom(i,k)     = pmid(i,k) / (rd*t(i,k)*(1.0+fv*spfh(i,k)))
          plnmid(i,k)   = log(pmid(i,k))
          utgwcl(i,k)   = zero
          vtgwcl(i,k)   = zero
!         ugwdc(i,k)    = zero
!         vgwdc(i,k)    = zero
          brunm(i,k)    = zero
          basicum(i,k)  = zero
        enddo
      enddo

      do k=1,km+1
        k1 = km - k + 2
        do i=1,npt
          ii = ipt(i)
          pint(i,k)     = pint1(ii,k1)
          plnint(i,k)   = log(pint(i,k))
          taugwci(i,k)  = zero
          bruni(i,k)    = zero
          rhoi(i,k)     = zero
          ti(i,k)       = zero
          basicui(i,k)  = zero
          riloc(i,k)    = zero
          rimin(i,k)    = zero
        enddo
      enddo

      do i = 1, npt
        ii = ipt(i)
        kcldtop(i)   = km - ktop(ii) + 1
        kcldbot(i)   = km - kbot(ii) + 1
        dlen(i)      = dlength(ii)
!                                    (g*qmax(ii)*cldf(ii)*dlength(ii))
        gqmcldlen(i) = grav*qmax(ii)*cldf(ii)*dlen(i)
      enddo

!     if (lprnt) then
!       if (fhour.ge.fhourpr) then
!         write(*,9200)
!         do i=1,im
!           write(*,9201) kcnv(i),kcldbot(i),kcldtop(i)
!         enddo
!       endif
!     endif

!9200 format(//,'  inside gwdc local variables start print',//,
!    +2x,'kcnv',2x,'kcldbot',2x,'kcldtop',//)
!9201 format(i4,2x,i5,4x,i5)

!***********************************************************************

      pi     = 2.*asin(1.)

!-----------------------------------------------------------------------
!
!                              pressure variables
!
!  interface 1 ======== pint(1)           *********
!  mid-level 1 --------          pmid(1)            dpmid(1)
!            2 ======== pint(2)           dpint(2)
!            2 --------          pmid(2)            dpmid(2)
!            3 ======== pint(3)           dpint(3)
!            3 --------          pmid(3)            dpmid(3)
!            4 ======== pint(4)           dpint(4)
!            4 --------          pmid(4)            dpmid(4)
!              ........
!           17 ======== pint(17)          dpint(17) 
!           17 --------          pmid(17)           dpmid(17)
!           18 ======== pint(18)          dpint(18)
!           18 --------          pmid(18)           dpmid(18)
!           19 ======== pint(19)          *********
!
!-----------------------------------------------------------------------

      do i = 1, npt
        tauctxl(i)    = zero
        tauctyl(i)    = zero

!-----------------------------------------------------------------------
!                              thermal variables
!
!  interface 1 ========       ti(1)           rhoi(1)            bruni(1)
!            1 -------- t(1)         rhom(1)           brunm(1)
!            2 ========       ti(2)           rhoi(2)            bruni(2)
!            2 -------- t(2)         rhom(2)           brunm(2)
!            3 ========       ti(3)           rhoi(3)            bruni(3)
!            3 -------- t(3)         rhom(3)           brunm(3)
!            4 ========       ti(4)           rhoi(4)            bruni(4)
!            4 -------- t(4)         rhom(4)           brunm(4)
!              ........
!           17 ========
!           17 -------- t(17)        rhom(17)          brunm(17)
!           18 ========       ti(18)          rhoi(18)           bruni(18)
!           18 -------- t(18)        rhom(18)          brunm(18)
!           19 ========       ti(19)          rhoi(19)           bruni(19)
!

!
!  top interface temperature is calculated assuming an isothermal 
!  atmosphere above the top mid level.

        ti(i,1)    = t(i,1)
        rhoi(i,1)  = pint(i,1)/(rd*ti(i,1))
        bruni(i,1) = sqrt ( gsqr / (cp*ti(i,1)) )
!
!  bottom interface temperature is calculated assuming an isothermal
!  atmosphere below the bottom mid level

        ti(i,km+1)    = t(i,km)
        rhoi(i,km+1)  = pint(i,km+1)/(rd*ti(i,km+1)*(1.0+fv*spfh(i,km)))
        bruni(i,km+1) = sqrt ( gsqr / (cp*ti(i,km+1)) )
      enddo

!-----------------------------------------------------------------------
!
!  calculate interface level temperature, density, and brunt-vaisala
!  frequencies based on linear interpolation of temp in ln(pressure)
!
!-----------------------------------------------------------------------

      do k = 2, km
        do i = 1, npt
          tem1 = (plnmid(i,k)-plnint(i,k)) / (plnmid(i,k)-plnmid(i,k-1))
          tem2 = one - tem1
          ti(i,k)    = t(i,k-1)    * tem1 + t(i,k)    * tem2
          qtem       = spfh(i,k-1) * tem1 + spfh(i,k) * tem2
          rhoi(i,k)  = pint(i,k) / ( rd * ti(i,k)*(1.0+fv*qtem) )
          dtdp       = (t(i,k)-t(i,k-1)) / (pmid(i,k)-pmid(i,k-1))
          n2         = gsqr / ti(i,k) * ( 1./cp - rhoi(i,k)*dtdp ) 
          bruni(i,k) = sqrt (max (n2min, n2))
        enddo
      enddo
 
      deallocate (spfh)
!-----------------------------------------------------------------------
!
!  determine the mid-level brunt-vaisala frequencies.
!             based on interpolated interface temperatures [ ti ]
!
!-----------------------------------------------------------------------

      do k = 1, km
        do i = 1, npt
          dtdp       = (ti(i,k+1)-ti(i,k)) / (pint(i,k+1)-pint(i,k))
          n2         = gsqr / t(i,k) * ( 1./cp - rhom(i,k)*dtdp ) 
          brunm(i,k) = sqrt (max (n2min, n2))
        enddo
      enddo

!-----------------------------------------------------------------------
!        printout
!-----------------------------------------------------------------------

!     if (lprnt) then
!       if (fhour.ge.fhourpr) then

!-------- pressure levels ----------
!         write(*,9101)
!         do ilev=1,km
!           write(*,9111) ilev,(0.01*pint(ipr,ilev)),
!    &                         (0.01*dpint(ipr,ilev)),plnint(ipr,ilev)
!           write(*,9121) ilev,(0.01*pmid(ipr,ilev)),
!    &                         (0.01*dpmid(ipr,ilev)),plnmid(ipr,ilev)
!         enddo
!         ilev=km+1
!         write(*,9111) ilev,(0.01*pint(ipr,ilev)),
!    &                       (0.01*dpint(ipr,ilev)),plnint(ipr,ilev)

!                2
!-------- u v t n  ----------
!         write(*,9102)
!         do ilev=1,km
!           write(*,9112) ilev,ti(ipr,ilev),(100.*bruni(ipr,ilev))
!           write(*,9122) ilev,u(ipr,ilev),v(ipr,ilev),
!    +                    t(ipr,ilev),(100.*brunm(ipr,ilev))
!         enddo
!         ilev=km+1
!         write(*,9112) ilev,ti(ipr,ilev),(100.*bruni(ipr,ilev))

!       endif
!     endif

!9101 format(//,14x,'pressure levels',//,
!    +' ilev',4x,'pint',4x,'pmid',4x,'dpint',3x,'dpmid',5x,'lnp',/)
!9111 format(i4,1x,f8.2,9x,f8.2,9x,f8.2)
!9121 format(i4,9x,f8.2,9x,f8.2,1x,f8.2)
!9102 format(//' ilev',5x,'u',7x,'v',5x,'ti',7x,'t',
!    +5x,'bruni',3x,'brunm',//)
!9112 format(i4,16x,f8.2,8x,f8.3)
!9122 format(i4,2f8.2,8x,f8.2,8x,f8.3)


!***********************************************************************
!
!        big loop over grid points                    only done if kcnv=1
!
!***********************************************************************

      kcldm = 1
      do i = 1, npt
        kk        = kcldtop(i)
        kb        = kcldbot(i)
        kcldm     = max(kcldm,kk)

!-----------------------------------------------------------------------
!
!  determine cloud top wind component, direction, and speed.
!  here, ucltop, vcltop, and windcltop are wind components and 
!  wind speed at mid-level cloud top index
!
!-----------------------------------------------------------------------

        ucltop(i) = u(i,kk)
        vcltop(i) = v(i,kk)
!       windcltop = sqrt( ucltop(i)*ucltop(i) + vcltop(i)*vcltop(i) )
        windcltop = 1.0 / sqrt( ucltop(i)*ucltop(i)
     &                        + vcltop(i)*vcltop(i) )
        cosphi(i) = ucltop(i)*windcltop
        sinphi(i) = vcltop(i)*windcltop
!       angle(i)  = acos(cosphi)*180./pi
      enddo

!-----------------------------------------------------------------------
!
!  calculate basic state wind projected in the direction of the cloud 
!  top wind.
!  input u(i,k) and v(i,k) is defined at mid level
!
!-----------------------------------------------------------------------

      do k=1,km
        do i=1,npt
          basicum(i,k) = u(i,k)*cosphi(i) + v(i,k)*sinphi(i)
        enddo
      enddo

!-----------------------------------------------------------------------
!
!  basic state wind at interface level is also calculated
!  based on linear interpolation in ln(pressure)
!
!  in the top and bottom boundaries, basic-state wind at interface level
!  is assumed to be vertically uniform.
!
!-----------------------------------------------------------------------

      do i=1,npt
        basicui(i,1)    = basicum(i,1)
        basicui(i,km+1) = basicum(i,km)
      enddo
      do k=2,km
        do i=1,npt
          tem1 = (plnmid(i,k)-plnint(i,k)) / (plnmid(i,k)-plnmid(i,k-1))
          tem2 = one - tem1
          basicui(i,k) = basicum(i,k)*tem2 + basicum(i,k-1)*tem1
        enddo
      enddo

!-----------------------------------------------------------------------
!
!  calculate local richardson number 
!
!  basicum   : u at mid level
!  basicui   : ui at interface level
!
!  interface 1 ========       ui(1)            rhoi(1)  bruni(1)  riloc(1)
!  mid-level 1 -------- u(1)
!            2 ========       ui(2)  dpint(2)  rhoi(2)  bruni(2)  riloc(2)
!            2 -------- u(2)
!            3 ========       ui(3)  dpint(3)  rhoi(3)  bruni(3)  riloc(3)
!            3 -------- u(3)
!            4 ========       ui(4)  dpint(4)  rhoi(4)  bruni(4)  riloc(4)
!            4 -------- u(4)
!              ........
!           17 ========       ui(17) dpint(17) rhoi(17) bruni(17) riloc(17)
!           17 -------- u(17)
!           18 ========       ui(18) dpint(18) rhoi(18) bruni(18) riloc(18)
!           18 -------- u(18)
!           19 ========       ui(19)           rhoi(19) bruni(19) riloc(19)
!
!-----------------------------------------------------------------------     

      do k=2,km
        do i=1,npt
          shear = grav*rhoi(i,k) * (basicum(i,k) - basicum(i,k-1))
     &                           / (pmid(i,k) - pmid(i,k-1))
          if ( abs(shear) < shmin ) then
            riloc(i,k) = rimax
          else
            tem = bruni(i,k) / shear
            riloc(i,k)  = tem * tem
            if (riloc(i,k) >= rimax ) riloc(i,k) = rilarge
          end if 
        enddo
      enddo
 
      do i=1,npt
        riloc(i,1)    = riloc(i,2)
        riloc(i,km+1) = riloc(i,km)
      enddo

!     if (lprnt.and.(i.eq.ipr)) then
!       if (fhour.ge.fhourpr) then
!         write(*,9104) ucltop,vcltop,windcltop,angle,kk
!         do ilev=1,km
!           write(*,9114) ilev,basicui(ipr,ilev),dpint(ipr,ilev),
!    +      rhoi(ipr,ilev),(100.*bruni(ipr,ilev)),riloc(ilev)
!           write(*,9124) ilev,(basicum(ipr,ilev))
!         enddo
!         ilev=km+1
!         write(*,9114) ilev,basicui(ipr,ilev),dpint(ipr,ilev),
!    +      rhoi(ipr,ilev),(100.*bruni(ipr,ilev)),riloc(ilev)
!       endif
!     endif

!9104 format(//,'wind vector at cloudtop = (',f6.2,' , ',f6.2,' ) = ',
!    +f6.2,' in direction ',f6.2,4x,'kk = ',i2,//,
!    +' ilev',2x,'basicum',2x,'basicui',4x,'dpint',6x,'rhoi',5x,
!    +'bruni',6x,'ri',/)
!9114 format(i4,10x,f8.2,4(2x,f8.2))
!9124 format(i4,1x,f8.2)

!-----------------------------------------------------------------------
!
!  calculate gravity wave stress at the interface level cloud top
!      
!  kcldtopi  : the interface level cloud top index
!  kcldtop   : the midlevel cloud top index
!  kcldbot   : the midlevel cloud bottom index
!
!  a : find deep convective heating rate maximum
!
!      if kcldtop(i) is less than kcldbot(i) in a horizontal grid point,
!      it can be thought that there is deep convective cloud. however,
!      deep convective heating between kcldbot and kcldtop is sometimes
!      zero in spite of kcldtop less than kcldbot. in this case,
!      maximum deep convective heating is assumed to be 1.e-30. 
!
!  b : kk is the vertical index for interface level cloud top
!
!  c : total convective fractional cover (cldf) is used as the
!      convective cloud cover for gwdc calculation instead of   
!      convective cloud cover in each layer (concld).
!                       a1 = cldf*dlength
!      you can see the difference between cldf(i) and concld(i)
!      in (4.a.2) in description of the ncar community climate    
!      model (ccm3).
!      in ncar ccm3, cloud fractional cover in each layer in a deep
!      cumulus convection is determined assuming total convective
!      cloud cover is randomly overlapped in each layer in the 
!      cumulus convection.
!
!  d : wave stress at cloud top is calculated when the atmosphere
!      is dynamically stable at the cloud top
!
!  e : cloud top wave stress and nonlinear parameter are calculated 
!      using density, temperature, and wind that are defined at mid
!      level just below the interface level in which cloud top wave
!      stress is defined.
!      nonlinct is defined at the interface level.
!  
!  f : if the atmosphere is dynamically unstable at the cloud top,
!      gwdc calculation in current horizontal grid is skipped.  
!
!  g : if mean wind at the cloud top is less than zero, gwdc
!      calculation in current horizontal grid is skipped.
!
!  h : maximum cloud top stress, tauctmax =  -20 n m^(-2),
!  h : max stress -5 (*j*)5/2015 tauctmax =  - 5 n m^(-2),
!      in order to prevent numerical instability.
!
!-----------------------------------------------------------------------
!d
      do i=1,npt
        kk = kcldtop(i)
        if ( abs(basicui(i,kk)) > zero .and. riloc(i,kk) > ricrit) then
!e
          tem       = basicum(i,kk)
          tem1      = tem * tem
          nonlinct  = gqmcldlen(i) / (bruni(i,kk)*t(i,kk)*tem1)    ! mu
          tem2      = c2*nonlinct
!                                  rhou^3c1(c2mu)^2/ndx
          tauct     = - rhom(i,kk) * tem * tem1 * c1 * tem2 * tem2
     &              /  (bruni(i,kk)*dlen(i))

          tauct         = max(tauctmax, tauct)
          tauctxl(i)    = tauct * cosphi(i)           ! x stress at cloud top
          tauctyl(i)    = tauct * sinphi(i)           ! y stress at cloud top
          taugwci(i,kk) = tauct                                    !  *1
          do_gwc(i)     = .true.
        else
!f
          tauctxl(i) = zero 
          tauctyl(i) = zero
          do_gwc(i) = .false.
        end if 
!h
      enddo

!       if (lprnt.and.(i.eq.ipr)) then
!         if (fhour.ge.fhourpr) then
!            write(*,9210) tauctx(ipr),taucty(ipr),tauct(ipr),angle,kk
!         endif
!       endif

!9210 format(/,5x,'stress vector = ( ',f8.3,' , ',f8.3,' ) = ',f8.3,
!    +' in direction ',f6.2,4x,'kk = ',i2,/)

!-----------------------------------------------------------------------
!
!  at this point, mean wind at the cloud top is larger than zero and
!  local ri at the cloud top is larger than ricrit (=0.25)
!
!  calculate minimum of richardson number including both basic-state
!  condition and wave effects.
!
!          g*q_0*alpha*dx                  ri_loc*(1 - mu*|c2|)
!  mu  =  ----------------  ri_min =  -----------------------------
!           c_p*n*t*u^2                (1 + mu*ri_loc^(0.5)*|c2|)^2
!
!  minimum ri is calculated for the following two cases
!
!  (1)   riloc < 1.e+20  
!  (2)   riloc = 1.e+20  ----> vertically uniform basic-state wind
!
!  riloc cannot be smaller than zero because n^2 becomes 1.e-32 in the
!  case of n^2 < 0.. thus the sign of rinum is determined by 
!  1 - nonlin*|c2|.
!
!-----------------------------------------------------------------------

      do k=kcldm,1,-1

        do i=1,npt
          if (do_gwc(i)) then
            kk = kcldtop(i)
            if (k > kk) cycle
            if ( k /= 1 ) then
              crit1 = ucltop(i)*(u(i,k)+u(i,k-1))*0.5
              crit2 = vcltop(i)*(v(i,k)+v(i,k-1))*0.5
            else
              crit1 = ucltop(i)*u(i,1)
              crit2 = vcltop(i)*v(i,1)
            end if

            if ( abs(basicui(i,k)) > zero .and. crit1 > zero 
     &                                    .and. crit2 > zero ) then
              tem = basicui(i,k) * basicui(i,k)
              nonlin   = gqmcldlen(i) / (bruni(i,k)*ti(i,k)*tem)        
              tem  = nonlin*abs(c2)
              if ( riloc(i,k)  <  rimaxm ) then
                tem1 = 1 + tem*sqrt(riloc(i,k))
                rimin(i,k) = riloc(i,k) * (1-tem) / (tem1*tem1)
              else if((riloc(i,k) > rimaxm) .and.
     &                (riloc(i,k) < rimaxp)) then
                rimin(i,k) = ( 1 - tem) / (tem*tem)
              end if
              if ( rimin(i,k) <= riminx ) then
                rimin(i,k) = rismall
              end if
            else
              rimin(i,k) = riminx
            end if

!-----------------------------------------------------------------------
!
!  if minimum ri at interface cloud top is less than or equal to 1/4,
!  gwdc calculation for current horizontal grid is skipped 
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!  calculate gravity wave stress profile using the wave saturation
!  hypothesis of lindzen (1981).   
!
!  assuming kcldtop(i)=10 and kcldbot=16,
!
!                             taugwci  riloc  rimin   utgwc
!
!  interface 1 ========       - 0.001         -1.e20
!            1 --------                               0.000
!            2 ========       - 0.001         -1.e20
!            2 --------                               0.000
!            3 ========       - 0.001         -1.e20
!            3 --------                               -.xxx 
!            4 ========       - 0.001  2.600  2.000
!            4 --------                               0.000
!            5 ========       - 0.001  2.500  2.000
!            5 --------                               0.000
!            6 ========       - 0.001  1.500  0.110
!            6 --------                               +.xxx 
!            7 ========       - 0.005  2.000  3.000
!            7 --------                               0.000
!            8 ========       - 0.005  1.000  0.222
!            8 --------                               +.xxx
!            9 ========       - 0.010  1.000  2.000
!            9 --------                               0.000
! kcldtopi  10 ========  $$$  - 0.010 
! kcldtop   10 --------  $$$                          yyyyy
!           11 ========  $$$  0
!           11 --------  $$$
!           12 ========  $$$  0
!           12 --------  $$$
!           13 ========  $$$  0
!           13 --------  $$$
!           14 ========  $$$  0
!           14 --------  $$$
!           15 ========  $$$  0
!           15 --------  $$$
!           16 ========  $$$  0
! kcldbot   16 --------  $$$
!           17 ========       0
!           17 -------- 
!           18 ========       0
!           18 -------- 
!           19 ========       0
!
!-----------------------------------------------------------------------
!
!   even though the cloud top level obtained in deep convective para-
!   meterization is defined in mid-level, the cloud top level for
!   the gwdc calculation is assumed to be the interface level just 
!   above the mid-level cloud top vertical level index.
!
!-----------------------------------------------------------------------
 
            if (k < kk .and. k > 1) then
              if ( abs(taugwci(i,k+1)) > taumin ) then                  ! taugwci
                if ( riloc(i,k) > ricrit ) then                         ! riloc
                  if ( rimin(i,k) > ricrit ) then                       ! rimin
                    taugwci(i,k) = taugwci(i,k+1)
                  elseif (rimin(i,k) > riminp) then
                    tem = 2.0 + 1.0 / sqrt(riloc(i,k))
                    nonlins = (1.0/abs(c2)) * (2.*sqrt(tem) - tem) 
                    tem1 = basicui(i,k)
                    tem2 = c2*nonlins*tem1
                    taugwci(i,k) = - rhoi(i,k) * c1 * tem1 * tem2 * tem2
     &                           /  (bruni(i,k)*dlen(i))
                  elseif (rimin(i,k) > riminm) then
                    taugwci(i,k) = zero 
                  end if                                              ! rimin
                else

!!!!!!!!!! in the dynamically unstable environment, there is no gravity wave stress

                  taugwci(i,k) = zero    
                end if                                                ! riloc
              else
                 taugwci(i,k) = zero
              end if                                                  ! taugwci

              if ( (basicum(i,k+1)*basicum(i,k) ) < 0. ) then
                 taugwci(i,k+1) = zero
                 taugwci(i,k)   = zero
              endif

              if (abs(taugwci(i,k)) > abs(taugwci(i,k+1))) then
                taugwci(i,k) = taugwci(i,k+1)
              end if

            elseif (k == 1) then

!!!!!! upper boundary condition - permit upward propagation of gravity wave energy

              taugwci(i,1) = taugwci(i,2)
            endif
          endif
        enddo                     ! end of i=1,npt loop
      enddo                       ! end of k=kcldm,1,-1 loop

!!!!!! vertical differentiation
!!!!!!

      do k=1,km
        do i=1,npt
          if (do_gwc(i)) then
            kk = kcldtop(i)
            if (k < kk) then
              wtgwc       = (taugwci(i,k+1) - taugwci(i,k)) / dpmid(i,k)
              utgwcl(i,k) = wtgwc * cosphi(i)
              vtgwcl(i,k) = wtgwc * sinphi(i)
            else
              utgwcl(i,k) = zero
              vtgwcl(i,k) = zero
            endif
          endif
        enddo
      enddo

!-----------------------------------------------------------------------
!
!  calculate momentum flux = stress deposited above cloup top
!  apply equal amount with opposite sign within cloud
!
!-----------------------------------------------------------------------

      do i=1,npt
        xstress(i) = zero
        ystress(i) = zero
      enddo
      do k=1,kcldm
        do i=1,npt
          if (do_gwc(i)) then
            xstress(i) = xstress(i) + utgwcl(i,k)*dpmid(i,k)
            ystress(i) = ystress(i) + vtgwcl(i,k)*dpmid(i,k)
          endif
        enddo
      enddo

!-----------------------------------------------------------------------
!        alt 1      only uppermost layer
!-----------------------------------------------------------------------

!     kk = kcldtop(i)
!     tem1 = g / dpmid(i,kk)
!     utgwc(i,kk) = - tem1 * xstress
!     vtgwc(i,kk) = - tem1 * ystress

!-----------------------------------------------------------------------
!        alt 2      sin(kt-kb)
!-----------------------------------------------------------------------

      do i=1,npt
        if (do_gwc(i)) then
          wrk(i) = 0.5 * pi / (pint(i,kcldbot(i)+1)-pint(i,kcldtop(i)))
        endif
      enddo
      do k=1,km
        do i=1,npt
          if (do_gwc(i)) then
            kk = kcldtop(i)
            if (k >= kk .and. k <= kcldbot(i)) then
              p1 = sin(wrk(i) * (pint(i,k)  -pint(i,kk)))
              p2 = sin(wrk(i) * (pint(i,k+1)-pint(i,kk)))
              tem = - (p2-p1) / dpmid(i,k)
              utgwcl(i,k) = tem*xstress(i)
              vtgwcl(i,k) = tem*ystress(i)
            endif
          endif
        enddo
      enddo

!-----------------------------------------------------------------------
!        alt 3      from kt to kb  proportional to conv heating
!-----------------------------------------------------------------------

!     do k=kcldtop(i),kcldbot(i)
!     p1=cumchr(i,k)
!     p2=cumchr(i,k+1)
!     utgwcl(i,k) = - g*xstress*(p1-p2)/dpmid(i,k)
!     enddo

!-----------------------------------------------------------------------
!
!  the gwdc should accelerate the zonal and meridional wind in the   
!  opposite direction of the previous zonal and meridional wind, 
!  respectively
!
!-----------------------------------------------------------------------

!     do k=1,kcldtop(i)-1

!      if (utgwcl(i,k)*u(i,k) .gt. 0.0) then

!-------------------- x-component-------------------

!       write(6,'(a)')   
!    +  '(gwdc) warning: the gwdc should accelerate the zonal wind '
!       write(6,'(a,a,i3,a,i3)')   
!    +  'in the opposite direction of the previous zonal wind', 
!    +  ' at i = ',i,' and j = ',lat
!       write(6,'(4(1x,e17.10))') u(i,kk),v(i,kk),u(i,k),v(i,k)
!       write(6,'(a,1x,e17.10))') 'vcld . v =',
!    +  u(i,kk)*u(i,k)+v(i,kk)*v(i,k)

!       if(u(i,kcldtop(i))*u(i,k)+v(i,kcldtop(i))*v(i,k).gt.0.0)then
!       do k1=1,km
!         write(6,'(i2,36x,2(1x,e17.10))')
!    +             k1,taugwcxi(i,k1),taugwci(i,k1)
!         write(6,'(i2,2(1x,e17.10))') k1,utgwcl(i,k1),u(i,k1) 
!       end do
!       write(6,'(i2,36x,1x,e17.10)') (km+1),taugwcxi(i,km+1)
!       end if

!-------------------- along wind at cloud top -----

!       do k1=1,km
!         write(6,'(i2,36x,2(1x,e17.10))')
!    +             k1,taugwci(i,k1)
!         write(6,'(i2,2(1x,e17.10))') k1,wtgwc(i,k1),basicum(i,k1) 
!       end do
!       write(6,'(i2,36x,1x,e17.10)') (km+1),taugwci(i,km+1)

!      end if

!      if (vtgwc(i,k)*v(i,k) .gt. 0.0) then
!       write(6,'(a)')
!    +  '(gwdc) warning: the gwdc should accelerate the meridional wind'
!       write(6,'(a,a,i3,a,i3)')
!    +  'in the opposite direction of the previous meridional wind',
!    +  ' at i = ',i,' and j = ',lat
!       write(6,'(4(1x,e17.10))') u(i,kcldtop(i)),v(i,kcldtop(i)),
!    +                            u(i,k),v(i,k)
!       write(6,'(a,1x,e17.10))') 'vcld . v =',
!    +                    u(i,kcldtop(i))*u(i,k)+v(i,kcldtop(i))*v(i,k)
!       if(u(i,kcldtop(i))*u(i,k)+v(i,kcldtop(i))*v(i,k).gt.0.0)then
!       do k1=1,km
!         write(6,'(i2,36x,2(1x,e17.10))')
!    +                        k1,taugwcyi(i,k1),taugwci(i,k1)
!         write(6,'(i2,2(1x,e17.10))') k1,vtgwc(i,k1),v(i,k1) 
!       end do
!       write(6,'(i2,36x,1x,e17.10)') (km+1),taugwcyi(i,km+1)
!       end if
!      end if

!     enddo

!1000 continue


!***********************************************************************

!     if (lprnt) then
!       if (fhour.ge.fhourpr) then
!-------- utgwc vtgwc ----------
!         write(*,9220)
!         do ilev=1,km
!           write(*,9221) ilev,(86400.*utgwcl(ipr,ilev)),
!    +                         (86400.*vtgwcl(ipr,ilev))
!         enddo
!       endif
!     endif

!9220 format(//,14x,'tendency due to gwdc',//,
!    +' ilev',6x,'utgwc',7x,'vtgwc',/)
!9221 format(i4,2(2x,f10.3))

!-----------------------------------------------------------------------
!
!  for gwdc performance analysis        
!
!-----------------------------------------------------------------------

!     do k = 1, kk-1
!       do i = 1, nct

!         kk = kcldtop(i)

!         if ( (abs(taugwci(i,kk)) > taumin) ) then

!           gwdcloc(i) = one

!        if ( abs(taugwci(i,k)-taugwci(i,kk)) > taumin ) then
!         break(i) = 1.0
!         go to 2000
!        endif 
!       enddo
!2000   continue

!       do k = 1, kk-1

!        if ( ( abs(taugwci(i,k)).lt.taumin ) .and.
!    &        ( abs(taugwci(i,k+1)).gt.taumin ) .and.
!    &        ( basicum(i,k+1)*basicum(i,k) .lt. 0. ) ) then
!         critic(i) = 1.0
!         print *,i,k,' inside gwdc  taugwci(k) = ',taugwci(i,k)
!         print *,i,k+1,' inside gwdc  taugwci(k+1) = ',taugwci(i,k+1)
!         print *,i,k,' inside gwdc  basicum(k) = ',basicum(i,k)
!         print *,i,k+1,' inside gwdc  basicum(k+1) = ',basicum(i,k+1)
!         print *,i,' inside gwdc  critic = ',critic(i)
!         goto 2010
!        endif
!       enddo
!2010   continue

!      endif

!     enddo

!-----------------------------------------------------------------------
!        convert back local gwdc tendency arrays to gfs model vertical indices
!        outgoing (fu1,fv1)=(utgwc,vtgwc)
!-----------------------------------------------------------------------

      do k=1,km
        k1 = km - k + 1
        do i=1,npt
          ii = ipt(i)
          utgwc(ii,k1) = utgwcl(i,k)

          vtgwc(ii,k1) = vtgwcl(i,k)

!         brunm(ii,kk) = brunm(i,k)
!         brunm(i,k)  = tem

!         rhom(ii,kk) = rhom(i,k)

        enddo
      enddo
      do i=1,npt
        ii = ipt(i)
        tauctx(ii) = tauctxl(i)
        taucty(ii) = tauctyl(i)
       enddo

!     if (lprnt) then
!       if (fhour.ge.fhourpr) then
!-------- utgwc vtgwc ----------
!         write(*,9225)
!         do ilev=km,1,-1
!           write(*,9226) ilev,(86400.*fu1(ipr,ilev)),
!    +                         (86400.*fv1(ipr,ilev))
!         enddo
!       endif
!     endif

!9225 format(//,14x,'tendency due to gwdc - to gbphys',//,
!    +' ilev',6x,'utgwc',7x,'vtgwc',/)
!9226 format(i4,2(2x,f10.3))

      deallocate (kcldtop,kcldbot,do_gwc)
      deallocate (tauctxl,  tauctyl,
     &            gwdcloc, break, critic,   cosphi,
     &            sinphi,         xstress,  ystress,
     &            dlen,    ucltop, vcltop,  gqmcldlen, wrk)

      deallocate (plnint,          taugwci,
     &            bruni, rhoi,     basicui,
     &            ti,       riloc, rimin,    pint)

      deallocate (plnmid, utgwcl, vtgwcl, basicum, u, v, t,
     &            pmid,   dpmid,  brunm,  rhom)

      return
      end
