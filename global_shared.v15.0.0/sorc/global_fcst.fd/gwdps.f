      subroutine gwdps(im,ix,iy,km,a,b,c,u1,v1,t1,q1,kpbl,
     &               prsi,del,prsl,prslk,phii, phil,deltim,kdt,
     &               hprime,oc,oa4,clx4,theta,sigma,gamma,elvmax, 
     &               dusfc,dvsfc,g, cp, rd, rv, imx, 
     &               nmtvr, cdmbgwd, me, lprnt, ipr)
!
!   ********************************************************************
! ----->  i m p l e m e n t a t i o n    v e r s i o n   <----------
!
!          --- not in this code --  history of gwdp at ncep----
!              ----------------     -----------------------
!  version 3  modified for gravity waves, location: .fr30(v3gwd)  *j*
!---       3.1 includes variable saturation flux profile cf isigst
!---       3.g includes ps combined w/ ph (glas and gfdl)
!-----         also included is ri  smooth over a thick lower layer
!-----         also included is decrease in de-acc at top by 1/2
!-----     the nmc gwd incorporating both glas(p&s) and gfdl(migwd)
!-----        mountain induced gravity wave drag 
!-----    code from .fr30(v3monnx) for monin3
!-----        this version (06 mar 1987)
!-----        this version (26 apr 1987)    3.g
!-----        this version (01 may 1987)    3.9
!-----    change to fortran 77 (feb 1989)     --- h. juang
!-----    20070601 elvmax bug fix (*j*)
!-----    add dissipation heating (jan 2014)  --- h. juang and f. yang
!
!   version 4
!                ----- this code -----
!
!-----   modified to implement the enhanced low tropospheric gravity
!-----   wave drag developed by kim and arakawa(jas, 1995).
!        orographic std dev (hprime), convexity (oc), asymmetry (oa4)
!        and lx (clx4) are input topographic statistics needed.
!
!-----   programmed and debugged by hong, alpert and kim --- jan 1996.
!-----   debugged again - moorthi and iredell --- may 1998.
!-----
!       further cleanup, optimization and modification
!                                       - s. moorthi may 98, march 99.
!-----   modified for usgs orography data (ncep office note 424)
!        and with several bugs fixed  - moorthi and hong --- july 1999.
!
!-----   modified & implemented into nrl nogaps
!                                       - young-joon kim, july 2000
!-----
!   version lm mb  (6): oz fix 8/2003
!                ----- this code -----
!
!------   changed to include the lott and miller mtn blocking
!         with some modifications by (*j*)  4/02
!        from a principal coordinate calculation using the
!        hi res 8 minute orography, the angle of the
!        mtn with that to the east (x) axis is theta, the slope
!        parameter sigma. the anisotropy is in gamma - all  are input
!        topographic statistics needed.  these are calculated off-line
!        as a function of model resolution in the fortran code ml01rg2.f,
!        with script mlb2.sh.   (*j*)
!-----   gwdps_mb.f version (following lmi) elvmax < hncrit (*j*)
!        mb3a expt to enhance elvmax mtn hgt see sigfac & hncrit
!        gwdps_gwdfix_v6.f fixgwd gf6.0 20070608 sigfac=4.
!-----
!----------------------------------------------------------------------c
!    use
!        routine is called from gbphys  (after call to monnin)
!
!    purpose
!        using the gwd parameterizations of ps-glas and ph-
!        gfdl technique.  the time tendencies of u v
!        are altered to include the effect of mountain induced
!        gravity wave drag from sub-grid scale orography including
!        convective breaking, shear breaking and the presence of
!        critical levels
!
!  input
!        a(iy,km)  non-lin tendency for v wind component
!        b(iy,km)  non-lin tendency for u wind component
!        c(iy,km)  non-lin tendency for temperature
!        u1(ix,km) zonal wind m/sec  at t0-dt
!        v1(ix,km) meridional wind m/sec at t0-dt
!        t1(ix,km) temperature deg k at t0-dt
!        q1(ix,km) specific humidity at t0-dt
!
!        deltim  time step    secs
!        si(n)   p/psfc at base of layer n
!        sl(n)   p/psfc at middle of layer n
!        del(n)  positive increment of p/psfc across layer n
!        kpbl(im) is the index of the top layer of the pbl
!        ipr & lprnt for diagnostics
!
!  output
!        a, b    as augmented by tendency due to gwdps
!                other input variables unmodified.
!   ********************************************************************
      use machine , only : kind_phys
      implicit none
      integer im, iy, ix, km, imx, kdt, ipr, me
      integer kpbl(im)                 ! index for the pbl top layer!
      real(kind=kind_phys) deltim, g, cp, rd, rv,      cdmbgwd(2)
!     real(kind=kind_phys) a(iy,km),    b(iy,km),      pstar(im)
      real(kind=kind_phys) a(iy,km),    b(iy,km),       c(iy,km),
     &                     u1(ix,km),   v1(ix,km),     t1(ix,km),
     &                     q1(ix,km),   prsi(ix,km+1), del(ix,km),
     &                     prsl(ix,km), prslk(ix,km),  phil(ix,km),
     &                     phii(ix,km+1)
      real(kind=kind_phys) oc(im),     oa4(iy,4), clx4(iy,4)
     &,                    hprime(im)
! for lm mtn blocking
      real(kind=kind_phys) elvmax(im),theta(im),sigma(im),gamma(im)
      real(kind=kind_phys) wk(im)
      real(kind=kind_phys) bnv2lm(im,km),pe(im),ek(im),zbk(im),up(im)
      real(kind=kind_phys) db(im,km),ang(im,km),uds(im,km)
      real(kind=kind_phys) zlen, dbtmp, r, phiang, cdmb, dbim
      real(kind=kind_phys) eng0, eng1
!
!     some constants
!
      real(kind=kind_phys) pi, dw2min, rimin, ric, bnv2min, efmin
     &,                    efmax,hpmax,hpmin, rad_to_deg, deg_to_rad
      parameter (pi=3.1415926535897931)
      parameter (rad_to_deg=180.0/pi, deg_to_rad=pi/180.0)
      parameter (dw2min=1., rimin=-100., ric=0.25, bnv2min=1.0e-5)
!     parameter (efmin=0.0, efmax=10.0, hpmax=200.0)
      parameter (efmin=0.0, efmax=10.0, hpmax=2400.0, hpmin=1.0)
!
      real(kind=kind_phys) frc,    ce,     ceofrc, frmax, cg, gmax
     &,                    veleps, factop, rlolev, rdi
      real(kind=kind_phys) critac
      parameter (frc=1.0, ce=0.8, ceofrc=ce/frc, frmax=100., cg=0.5)
      parameter (gmax=1.0, veleps=1.0, factop=0.5)
!     parameter (critac=5.0e-4)
      parameter (rlolev=50000.0) 
!     parameter (rlolev=500.0) 
!     parameter (rlolev=0.5)
!
       real(kind=kind_phys) dpmin,hminmt,hncrit,minwnd,sigfac
! --- for lm mtn blocking
!     parameter (cdmb = 1.0)     ! non-dim sub grid mtn drag amp (*j*)
      parameter (hncrit=8000.)   ! max value in meters for elvmax (*j*)
!  hncrit set to 8000m and sigfac added to enhance elvmax mtn hgt
      parameter (sigfac=4.0)     ! mb3a expt test for elvmax factor (*j*)
      parameter (hminmt=50.)     ! min mtn height (*j*)
      parameter (minwnd=0.1)     ! min wind component (*j*)

!     parameter (dpmin=00.0)     ! minimum thickness of the reference layer
!!    parameter (dpmin=05.0)     ! minimum thickness of the reference layer
!     parameter (dpmin=20.0)     ! minimum thickness of the reference layer
                                 ! in centibars
      parameter (dpmin=5000.0)   ! minimum thickness of the reference layer
                                 ! in pa
!
      real(kind=kind_phys) fdir
      integer mdir
      parameter(mdir=8, fdir=mdir/(pi+pi))
      integer nwdir(mdir)
      data nwdir/6,7,5,8,2,3,1,4/
      save nwdir
!
      logical icrilv(im)
!
!----   mountain induced gravity wave drag
!
      real(kind=kind_phys) taub(im),  xn(im),     yn(im),    ubar(im)
     &,                    vbar(im),  ulow(im),   oa(im),    clx(im)
     &,                    roll(im),  uloi(im),   dusfc(im), dvsfc(im)
     &,                    dtfac(im), xlinv(im),  delks(im), delks1(im)
!
      real(kind=kind_phys) bnv2(im,km),  taup(im,km+1), ri_n(im,km) 
     &,                    taud(im,km),  ro(im,km),     vtk(im,km)
     &,                    vtj(im,km),   scor(im),      velco(im,km-1)
     &,                    bnv2bar(im)
!
!     real(kind=kind_phys) velko(km-1)
      integer   kref(im), kint(im), iwk(im), ipt(im)
! for lm mtn blocking
      integer   kreflm(im), iwklm(im)
      integer   idxzb(im), ktrial, klevm1, nmtvr
!
      real(kind=kind_phys) gor,    gocp,  fv,    gr2,  bnv,  fr
     &,                    brvf,   cleff, tem,   tem1,  tem2, temc, temv
     &,                    wdir,   ti,    rdz,   dw2,   shr2, bvf2
     &,                    rdelks, efact, coefm, gfobnv
     &,                    scork,  rscor, hd,    fro,   rim,  sira
     &,                    dtaux,  dtauy, pkp1log, pklog
      integer kmm1, kmm2, lcap, lcapp1, kbps, kbpsp1,kbpsm1
     &, kmps, idir, nwd, i, j, k, klcap, kp1, kmpbl, npt, npr
     &, kmll
!    &, kmll,kmds,ihit,jhit
      logical lprnt
!
!     parameter (cdmb = 1.0)     ! non-dim sub grid mtn drag amp (*j*)
! non-dim sub grid mtn drag amp (*j*)
!     cdmb = 1.0/float(imx/192)
!     cdmb = 192.0/float(imx)
      cdmb = 4.0 * 192.0/float(imx)
      if (cdmbgwd(1) >= 0.0) cdmb = cdmb * cdmbgwd(1)
!
      npr = 0
      do i = 1, im
         dusfc(i) = 0.
         dvsfc(i) = 0.
      enddo
!
      do k = 1, km
        do i = 1, im
          db(i,k)  = 0.
          ang(i,k) = 0.
          uds(i,k) = 0.
        enddo
      enddo
!
      rdi  = 1.0 / rd
      gor  = g/rd
      gr2  = g*gor
      gocp = g/cp
      fv   = rv/rd - 1
!
!     ncnt   = 0
      kmm1   = km - 1
      kmm2   = km - 2
      lcap   = km
      lcapp1 = lcap + 1
!
!
      if ( nmtvr .eq. 14) then 
! ----  for lm and gwd calculation points
        ipt = 0
        npt = 0
        do i = 1,im
          if ( (elvmax(i) .gt. hminmt) 
     &       .and. (hprime(i) .gt. hpmin) )  then
             npt      = npt + 1
             ipt(npt) = i
             if (ipr .eq. i) npr = npt
          endif
        enddo
        if (npt .eq. 0) return     ! no gwd/mb calculation done!
!
!       if (lprnt) print *,' npt=',npt,' npr=',npr,' ipr=',ipr,' im=',im
!    &,' ipt(npt)=',ipt(npt)
!
! --- iwklm is the level above the height of the of the mountain.
! --- idxzb is the level of the dividing streamline.
! initialize dividing streamline (ds) control vector
!
        do i=1,npt
          iwklm(i)  = 2
          idxzb(i)  = 0 
          kreflm(i) = 0
        enddo
!       if (lprnt) 
!    &  print *,' in gwdps_lm.f npt,im,ix,iy,km,me=',npt,im,ix,iy,km,me
!
!
! start lm mtn blocking (mb) section
!
!..............................
!..............................
!
!  (*j*)  11/03:  test upper limit on kmll=km - 1
!      then do not need hncrit -- test with large hncrit first.
!       kmll  = km / 2 ! maximum mtnlm height : # of vertical levels / 2
        kmll = kmm1
! --- no mtn should be as high as kmll (so we do not have to start at 
! --- the top of the model but could do calc for all levels).
!
          do i = 1, npt
            j = ipt(i)
            elvmax(j) = min (elvmax(j) + sigfac * hprime(j), hncrit)
          enddo
!
        do k = 1,kmll
          do i = 1, npt
            j = ipt(i)
! --- interpolate to max mtn height for index, iwklm(i) wk[gz]
! --- elvmax is limited to hncrit because to hi res topo30 orog.
            pkp1log =  phil(j,k+1) / g
            pklog =  phil(j,k)   / g
!!!-------     elvmax(j) = min (elvmax(j) + sigfac * hprime(j), hncrit)
            if ( ( elvmax(j) .le.  pkp1log ) .and. 
     &           ( elvmax(j) .ge.   pklog  ) ) then
!     print *,' in gwdps_lm.f 1  =',k,elvmax(j),pklog,pkp1log,me
! ---        wk for diags but can be saved and reused.  
               wk(i)  = g * elvmax(j) / ( phil(j,k+1) - phil(j,k) )
               iwklm(i)  =  max(iwklm(i), k+1 ) 
!     print *,' in gwdps_lm.f 2 npt=',npt,i,j,wk(i),iwklm(i),me
            endif
!
! ---        find at prsl levels large scale environment variables
! ---        these cover all possible mtn max heights
            vtj(i,k)  = t1(j,k)  * (1.+fv*q1(j,k))
            vtk(i,k)  = vtj(i,k) / prslk(j,k)
            ro(i,k)   = rdi * prsl(j,k) / vtj(i,k) ! density kg/m**3
          enddo
        enddo
!
! testing for highest model level of mountain top
!
!         ihit = 2
!         jhit = 0
!        do i = 1, npt
!        j=ipt(i)
!          if ( iwklm(i) .gt. ihit ) then 
!            ihit = iwklm(i)
!            jhit = j
!          endif
!        enddo
!     print *, ' mb: kdt,max(iwklm),jhit,phil,me=',
!    &          kdt,ihit,jhit,phil(jhit,ihit),me
         
        klevm1 = kmll - 1
        do k = 1, klevm1  
          do i = 1, npt
           j   = ipt(i)
            rdz  = g   / ( phil(j,k+1) - phil(j,k) )
! ---                               brunt-vaisala frequency
            bnv2lm(i,k) = (g+g) * rdz * ( vtk(i,k+1)-vtk(i,k) )
     &                     / ( vtk(i,k+1)+vtk(i,k) )
            bnv2lm(i,k) = max( bnv2lm(i,k), bnv2min )
          enddo
        enddo
!    print *,' in gwdps_lm.f 3 npt=',npt,j,rdz,me
!
        do i = 1, npt
          j   = ipt(i)
          delks(i)  = 1.0 / (prsi(j,1) - prsi(j,iwklm(i)))
          delks1(i) = 1.0 / (prsl(j,1) - prsl(j,iwklm(i)))
          ubar (i)  = 0.0
          vbar (i)  = 0.0
          roll (i)  = 0.0
          pe   (i)  = 0.0
          ek   (i)  = 0.0
          bnv2bar(i) = (prsl(j,1)-prsl(j,2)) * delks1(i) * bnv2lm(i,1)
        enddo

! --- find the dividing stream line height 
! --- starting from the level above the max mtn downward
! --- iwklm(i) is the k-index of mtn elvmax elevation
        do ktrial = kmll, 1, -1
          do i = 1, npt
             if ( ktrial .lt. iwklm(i) .and. kreflm(i) .eq. 0 ) then
                kreflm(i) = ktrial
             endif
          enddo
        enddo
!     print *,' in gwdps_lm.f 4 npt=',npt,kreflm(npt),me
!
! --- in the layer kreflm(i) to 1 find pe (which needs n, elvmax)
! ---  make averages, guess dividing stream (ds) line layer.
! ---  this is not used in the first cut except for testing and
! --- is the vert ave of quantities from the surface to mtn top.
!   
        do i = 1, npt
          do k = 1, kreflm(i)
            j        = ipt(i)
            rdelks     = del(j,k) * delks(i)
            ubar(i)    = ubar(i)  + rdelks * u1(j,k) ! trial mean u below 
            vbar(i)    = vbar(i)  + rdelks * v1(j,k) ! trial mean v below 
            roll(i)    = roll(i)  + rdelks * ro(i,k) ! trial mean ro below 
            rdelks     = (prsl(j,k)-prsl(j,k+1)) * delks1(i)
            bnv2bar(i) = bnv2bar(i) + bnv2lm(i,k) * rdelks
! --- these vert ave are for diags, testing and gwd to follow (*j*).
          enddo
        enddo
!     print *,' in gwdps_lm.f 5  =',i,kreflm(npt),bnv2bar(npt),me
!
! --- integrate to get pe in the trial layer.
! --- need the first layer where pe>ek - as soon as 
! --- idxzb is not 0 we have a hit and zb is found.
!
        do i = 1, npt
          j = ipt(i)
          do k = iwklm(i), 1, -1
            phiang   =  atan2(v1(j,k),u1(j,k))*rad_to_deg
            ang(i,k) = ( theta(j) - phiang )
            if ( ang(i,k) .gt.  90. ) ang(i,k) = ang(i,k) - 180.
            if ( ang(i,k) .lt. -90. ) ang(i,k) = ang(i,k) + 180.
            ang(i,k) = ang(i,k) * deg_to_rad
!
            uds(i,k) = 
     &          max(sqrt(u1(j,k)*u1(j,k) + v1(j,k)*v1(j,k)), minwnd)
! --- test to see if we found zb previously
            if (idxzb(i) .eq. 0 ) then
              pe(i) = pe(i) + bnv2lm(i,k) * 
     &           ( g * elvmax(j) - phil(j,k) ) * 
     &           ( phii(j,k+1) - phii(j,k) ) / (g*g)
! --- ke
! --- wind projected on the line perpendicular to mtn range, u(zb(k)).
! --- kenetic energy is at the layer zb
! --- theta ranges from -+90deg |_ to the mtn "largest topo variations"
              up(i)  =  uds(i,k) * cos(ang(i,k))
              ek(i)  = 0.5 *  up(i) * up(i) 

! --- dividing stream lime  is found when pe =exceeds ek.
              if ( pe(i) .ge.  ek(i) ) idxzb(i) = k
! --- then mtn blocked flow is between zb=k(idxzb(i)) and surface
!
            endif
          enddo
        enddo
!
!     print *,' in gwdps_lm.f 6  =',phiang,theta(ipt(npt)),me
!     print *,' in gwdps_lm.f 7  =',idxzb(npt),pe(npt)
!
!     if (lprnt .and. npr .gt. 0) then
!       print *,' bnv2bar,bnv2lm=',bnv2bar(npr),bnv2lm(npr,1:klevm1)
!       print *,' npr,idxzb,uds=',npr,idxzb(npr),uds(npr,:)
!       print *,' pe,up,ek=',pe(npr),up(npr),ek(npr)
!     endif
!
        do i = 1, npt
          j    = ipt(i)
! --- calc if n constant in layers (zb guess) - a diagnostic only.
          zbk(i) = elvmax(j)
     &           - sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i))/bnv2bar(i)
        enddo
!
!     if (lprnt .and. npr .gt. 0) then
!       print *,' iwklm,zbk=',iwklm(npr),zbk(npr),idxzb(npr)
!       print *,' zb=',phil(ipr),idxzb(npr))/g
!     print *,' in gwdps_lm.f 8 npt =',npt,zbk(npt),up(npt),me
!     endif
!
! --- the drag for mtn blocked flow
! 
        do i = 1, npt
          j = ipt(i)
          zlen = 0.
!      print *,' in gwdps_lm.f 9  =',i,j,idxzb(i),me
          if ( idxzb(i) .gt. 0 ) then 
            do k = idxzb(i), 1, -1
              if ( phil(j,idxzb(i)) .gt.  phil(j,k) ) then
                zlen = sqrt( ( phil(j,idxzb(i)) - phil(j,k) ) / 
     &                       ( phil(j,k ) + g * hprime(j) ) )
! --- lm eq 14:
                r = (cos(ang(i,k))**2 + gamma(j) * sin(ang(i,k))**2) / 
     &              (gamma(j) * cos(ang(i,k))**2 + sin(ang(i,k))**2)
! --- (negitive of db -- see sign at tendency)
                dbtmp = 0.25 *  cdmb * 
     &                  max( 2. - 1. / r, 0. ) * sigma(j) * 
     &                  max(cos(ang(i,k)), gamma(j)*sin(ang(i,k))) *
     &                  zlen / hprime(j) 
                db(i,k) =  dbtmp * uds(i,k)    
!
!               if(lprnt .and. i .eq. npr) then 
!                 print *,' in gwdps_lmi.f 10 npt=',npt,i,j,idxzb(i)
!    &,           dbtmp,r' ang=',ang(i,k),' gamma=',gamma(j),' k=',k
!                 print *,' in gwdps_lmi.f 11   k=',k,zlen,cos(ang(i,k))
!                 print *,' in gwdps_lmi.f 12  db=',db(i,k),sin(ang(i,k))
!               endif
              endif
            enddo
!         if(lprnt) print *,' @k=1,zlen,dbtmp=',k,zlen,dbtmp
          endif
        enddo
! 
!.............................
!.............................
! end  mtn blocking section
!
      elseif ( nmtvr .ne. 14) then 
! ----  for mb not present and  gwd (nmtvr .ne .14) 
        ipt     = 0
        npt     = 0
        do i = 1,im
          if ( hprime(i) .gt. hpmin )  then
             npt      = npt + 1
             ipt(npt) = i
             if (ipr .eq. i) npr = npt
          endif
        enddo
        if (npt .eq. 0) return     ! no gwd/mb calculation done!
!
!       if (lprnt) print *,' npr=',npr,' npt=',npt,' ipr=',ipr
!      &,' ipt(npt)=',ipt(npt)
!
        do i=1,npt
          idxzb(i) = 0
        enddo
      endif
!
!.............................
!.............................
!
      kmpbl  = km / 2 ! maximum pbl height : # of vertical levels / 2
!
!  scale cleff between im=384*2 and 192*2 for t126/t170 and t62
!
      if (imx .gt. 0) then
!       cleff = 1.0e-5 * sqrt(float(imx)/384.0) !  this is inverse of cleff!
!       cleff = 1.0e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
!       cleff = 0.5e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
!       cleff = 1.0e-5 * sqrt(float(imx)/192)/float(imx/192)
!       cleff = 1.0e-5 / sqrt(float(imx)/192.0) !  this is inverse of cleff!
        cleff = 0.5e-5 / sqrt(float(imx)/192.0) !  this is inverse of cleff!
!       cleff = 2.0e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
!       cleff = 2.5e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
      endif
      if (cdmbgwd(2) >= 0.0) cleff = cleff * cdmbgwd(2)
!
      do k = 1,km
        do i =1,npt
          j         = ipt(i)
          vtj(i,k)  = t1(j,k)  * (1.+fv*q1(j,k))
          vtk(i,k)  = vtj(i,k) / prslk(j,k)
          ro(i,k)   = rdi * prsl(j,k) / vtj(i,k) ! density tons/m**3
          taup(i,k) = 0.0
        enddo
      enddo
      do k = 1,kmm1
        do i =1,npt
          j         = ipt(i)
          ti        = 2.0 / (t1(j,k)+t1(j,k+1))
          tem       = ti  / (prsl(j,k)-prsl(j,k+1))
          rdz       = g   / (phil(j,k+1) - phil(j,k))
          tem1      = u1(j,k) - u1(j,k+1)
          tem2      = v1(j,k) - v1(j,k+1)
          dw2       = tem1*tem1 + tem2*tem2
          shr2      = max(dw2,dw2min) * rdz * rdz
          bvf2      = g*(gocp+rdz*(vtj(i,k+1)-vtj(i,k))) * ti
          ri_n(i,k) = max(bvf2/shr2,rimin)   ! richardson number
!                                              brunt-vaisala frequency
!         tem       = gr2 * (prsl(j,k)+prsl(j,k+1)) * tem
!         bnv2(i,k) = tem * (vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
          bnv2(i,k) = (g+g) * rdz * (vtk(i,k+1)-vtk(i,k))
     &                            / (vtk(i,k+1)+vtk(i,k))
          bnv2(i,k) = max( bnv2(i,k), bnv2min )
        enddo
      enddo
!      print *,' in gwdps_lm.f gwd:14  =',npt,kmm1,bnv2(npt,kmm1)
!
!     apply 3 point smoothing on bnv2
!
!     do k=1,km
!       do i=1,im
!         vtk(i,k) = bnv2(i,k)
!       enddo
!     enddo
!     do k=2,kmm1
!       do i=1,im
!         bnv2(i,k) = 0.25*(vtk(i,k-1)+vtk(i,k+1)) + 0.5*vtk(i,k)
!       enddo
!     enddo
!
!     finding the first interface index above 50 hpa level
!
      do i=1,npt
        iwk(i) = 2
      enddo
      do k=3,kmpbl
        do i=1,npt
          j   = ipt(i)
          tem = (prsi(j,1) - prsi(j,k))
          if (tem .lt. dpmin) iwk(i) = k
        enddo
      enddo
!
      kbps = 1
      kmps = km
      do i=1,npt
        j         = ipt(i)
        kref(i)   = max(iwk(i), kpbl(j)+1 ) ! reference level 
        delks(i)  = 1.0 / (prsi(j,1) - prsi(j,kref(i)))
        delks1(i) = 1.0 / (prsl(j,1) - prsl(j,kref(i)))
        ubar (i)  = 0.0
        vbar (i)  = 0.0
        roll (i)  = 0.0
        kbps      = max(kbps,  kref(i))
        kmps      = min(kmps,  kref(i))
!
        bnv2bar(i) = (prsl(j,1)-prsl(j,2)) * delks1(i) * bnv2(i,1)
      enddo
!      print *,' in gwdps_lm.f gwd:15  =',kbps,kmps
      kbpsp1 = kbps + 1
      kbpsm1 = kbps - 1
      do k = 1,kbps
        do i = 1,npt
          if (k .lt. kref(i)) then
            j          = ipt(i)
            rdelks     = del(j,k) * delks(i)
            ubar(i)    = ubar(i)  + rdelks * u1(j,k)   ! mean u below kref
            vbar(i)    = vbar(i)  + rdelks * v1(j,k)   ! mean v below kref
!
            roll(i)    = roll(i)  + rdelks * ro(i,k)   ! mean ro below kref
            rdelks     = (prsl(j,k)-prsl(j,k+1)) * delks1(i)
            bnv2bar(i) = bnv2bar(i) + bnv2(i,k) * rdelks
          endif
        enddo
      enddo
!      print *,' in gwdps_lm.f gwd:15b =',bnv2bar(npt)
!
!     figure out low-level horizontal wind direction and find 'oa'
!
!             nwd  1   2   3   4   5   6   7   8
!              wd  w   s  sw  nw   e   n  ne  se
!
      do i = 1,npt
        j      = ipt(i)
        wdir   = atan2(ubar(i),vbar(i)) + pi
        idir   = mod(nint(fdir*wdir),mdir) + 1
        nwd    = nwdir(idir)
        oa(i)  = (1-2*int( (nwd-1)/4 )) * oa4(j,mod(nwd-1,4)+1)
        clx(i) = clx4(j,mod(nwd-1,4)+1)
      enddo
!
!-----xn,yn            "low-level" wind projections in zonal
!                                    & meridional directions
!-----ulow             "low-level" wind magnitude -        (= u)
!-----bnv2             bnv2 = n**2
!-----taub             base momentum flux
!-----= -(ro * u**3/(n*xl)*gf(fr) for n**2 > 0
!-----= 0.                        for n**2 < 0
!-----fr               froude    =   n*hprime / u
!-----g                gmax*fr**2/(fr**2+cg/oc)
!
!-----initialize some arrays
!
      do i = 1,npt
        xn(i)     = 0.0
        yn(i)     = 0.0
        taub (i)  = 0.0
        ulow (i)  = 0.0
        dtfac(i)  = 1.0
        icrilv(i) = .false. ! initialize critical level control vector
        
!
!----compute the "low level" wind magnitude (m/s)
!
        ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
        uloi(i) = 1.0 / ulow(i)
      enddo
!
      do  k = 1,kmm1
        do  i = 1,npt
          j            = ipt(i)
          velco(i,k)   = 0.5 * ((u1(j,k)+u1(j,k+1))*ubar(i)
     &                       +  (v1(j,k)+v1(j,k+1))*vbar(i))
          velco(i,k)   = velco(i,k) * uloi(i)
!         if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
!           velco(i,k) = veleps
!         endif
        enddo
      enddo
!      
!
!   find the interface level of the projected wind where
!   low levels & upper levels meet above pbl
!
!   note following not being used (kint reset to kref) so commented out
!     do i=1,npt
!       kint(i) = km
!     enddo
!     do k = 1,kmm1
!       do i = 1,npt
!         if (k .gt. kref(i)) then
!           if(velco(i,k) .lt. veleps .and. kint(i) .eq. km) then
!             kint(i) = k+1
!           endif
!         endif
!       enddo
!     enddo
!  warning  kint = kref !!!!!!!!!
      do i=1,npt
        kint(i) = kref(i)
      enddo
!
!     if(lprnt) print *,' ubar=',ubar
!    &,' vbar=',vbar,' ulow=',ulow,' veleps=',veleps
!
      do i = 1,npt
        j      = ipt(i)
        bnv    = sqrt( bnv2bar(i) )
        fr     = bnv     * uloi(i) * min(hprime(j),hpmax)
        fr     = min(fr, frmax)
        xn(i)  = ubar(i) * uloi(i)
        yn(i)  = vbar(i) * uloi(i)
!
!     compute the base level stress and store it in taub
!     calculate enhancement factor, number of mountains & aspect
!     ratio const. use simplified relationship between standard
!     deviation & critical hgt
!
        efact    = (oa(i) + 2.) ** (ceofrc*fr)
        efact    = min( max(efact,efmin), efmax )
!
        coefm    = (1. + clx(i)) ** (oa(i)+1.)
!
        xlinv(i) = coefm * cleff
!
        tem      = fr    * fr * oc(j)
        gfobnv   = gmax  * tem / ((tem + cg)*bnv)  ! g/n0
!
        taub(i)  = xlinv(i) * roll(i) * ulow(i) * ulow(i)
     &           * ulow(i)  * gfobnv  * efact         ! base flux tau0
!
!         tem      = min(hprime(i),hpmax)
!         taub(i)  = xlinv(i) * roll(i) * ulow(i) * bnv * tem * tem
!
        k        = max(1, kref(i)-1)
        tem      = max(velco(i,k)*velco(i,k), 0.1)
        scor(i)  = bnv2(i,k) / tem  ! scorer parameter below ref level
      enddo
!     if(lprnt) print *,' taub=',taub
!                                                                       
!----set up bottom values of stress
!
      do k = 1, kbps
        do i = 1,npt
          if (k .le. kref(i)) taup(i,k) = taub(i)
        enddo
      enddo
!
!   now compute vertical structure of the stress.
!
      do k = kmps, kmm1                   ! vertical level k loop!
        kp1 = k + 1
        do i = 1, npt
!
!-----unstable layer if ri < ric
!-----unstable layer if upper air vel comp along surf vel <=0 (crit lay)
!---- at (u-c)=0. crit layer exists and bit vector should be set (.le.)
!
          if (k .ge. kref(i)) then
            icrilv(i) = icrilv(i) .or. ( ri_n(i,k) .lt. ric)
     &                            .or. (velco(i,k) .le. 0.0)
          endif
        enddo
!
        do i = 1,npt
          if (k .ge. kref(i))   then
            if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
              temv = 1.0 / max(velco(i,k), 0.01)
!             if (oa(i) .gt. 0. .and.  prsi(ipt(i),kp1).gt.rlolev) then
              if (oa(i).gt.0. .and. kp1 .lt. kint(i)) then
                scork   = bnv2(i,k) * temv * temv
                rscor   = min(1.0, scork / scor(i))
                scor(i) = scork
              else 
                rscor   = 1.
              endif
!
              brvf = sqrt(bnv2(i,k))        ! brunt-vaisala frequency
!             tem1 = xlinv(i)*(ro(i,kp1)+ro(i,k))*brvf*velco(i,k)*0.5
              tem1 = xlinv(i)*(ro(i,kp1)+ro(i,k))*brvf*0.5
     &                       * max(velco(i,k),0.01)
              hd   = sqrt(taup(i,k) / tem1)
              fro  = brvf * hd * temv
!
!    rim is the  minimum-richardson number by shutts (1985)
!
              tem2   = sqrt(ri_n(i,k))
              tem    = 1. + tem2 * fro
              rim    = ri_n(i,k) * (1.-fro) / (tem * tem)
!
!    check stability to employ the 'saturation hypothesis'
!    of lindzen (1981) except at tropospheric downstream regions
!
!                                       ----------------------
              if (rim .le. ric .and.
!    &           (oa(i) .le. 0. .or.  prsi(ipt(i),kp1).le.rlolev )) then
     &           (oa(i) .le. 0. .or.  kp1 .ge. kint(i) )) then
                 temc = 2.0 + 1.0 / tem2
                 hd   = velco(i,k) * (2.*sqrt(temc)-temc) / brvf
                 taup(i,kp1) = tem1 * hd * hd
              else 
                 taup(i,kp1) = taup(i,k) * rscor
              endif
              taup(i,kp1) = min(taup(i,kp1), taup(i,k))
            endif
          endif
        enddo
      enddo
!
!     do i=1,im
!       taup(i,km+1) = taup(i,km)
!     enddo
!
      if(lcap .le. km) then
         do klcap = lcapp1, km+1
            do i = 1,npt
              sira          = prsi(ipt(i),klcap) / prsi(ipt(i),lcap)
              taup(i,klcap) = sira * taup(i,lcap)
            enddo
         enddo
      endif
!
!     calculate - (g/p*)*d(tau)/d(sigma) and decel terms dtaux, dtauy
!
      do k = 1,km
        do i = 1,npt
          taud(i,k) = g * (taup(i,k+1) - taup(i,k)) / del(ipt(i),k)
        enddo
      enddo
!
!------limit de-acceleration (momentum deposition ) at top to 1/2 value
!------the idea is some stuff must go out the 'top'
!
      do klcap = lcap, km
         do i = 1,npt
            taud(i,klcap) = taud(i,klcap) * factop
         enddo
      enddo
!
!------if the gravity wave drag would force a critical line in the
!------layers below sigma=rlolev during the next deltim timestep,
!------then only apply drag until that critical line is reached.
!
      do k = 1,kmm1
        do i = 1,npt
           if (k .gt. kref(i) .and. prsi(ipt(i),k) .ge. rlolev) then
             if(taud(i,k).ne.0.) then
               tem = deltim * taud(i,k)
               dtfac(i) = min(dtfac(i),abs(velco(i,k)/tem))
             endif
           endif
        enddo
      enddo
!
!     if(lprnt .and. npr .gt. 0) then
!       print *,' before  a=',a(npr,:)
!       print *,' before  b=',b(npr,:)
!     endif

      do k = 1,km
        do i = 1,npt
          j          = ipt(i)
          taud(i,k)  = taud(i,k) * dtfac(i)
          dtaux      = taud(i,k) * xn(i)
          dtauy      = taud(i,k) * yn(i)
          eng0       = 0.5*(u1(j,k)**2.0+v1(j,k)**2.0)
! ---  lm mb (*j*)  changes overwrite gwd
          if ( k .lt. idxzb(i) .and. idxzb(i) .ne. 0 ) then
            dbim = db(i,k) / (1.+db(i,k)*deltim)
            a(j,k)  = - dbim * v1(j,k) + a(j,k)
            b(j,k)  = - dbim * u1(j,k) + b(j,k)
            eng1    = eng0*(1.0-dbim*deltim)**2.0
!          if ( abs(dbim * u1(j,k)) .gt. .01 ) 
!    & print *,' in gwdps_lmi.f kdt=',kdt,i,k,db(i,k),
!    &                      dbim,idxzb(i),u1(j,k),v1(j,k),me
            dusfc(j)   = dusfc(j) - dbim * v1(j,k) * del(j,k)
            dvsfc(j)   = dvsfc(j) - dbim * u1(j,k) * del(j,k)
          else
!
            a(j,k)     = dtauy     + a(j,k)
            b(j,k)     = dtaux     + b(j,k)
            eng1       = 0.5*((u1(j,k)+dtaux*deltim)**2.0+
     &                        (v1(j,k)+dtauy*deltim)**2.0)
            dusfc(j)   = dusfc(j)  + dtaux * del(j,k)
            dvsfc(j)   = dvsfc(j)  + dtauy * del(j,k)
          endif
          c(j,k) = c(j,k) + max((eng0-eng1),0.0)/cp/deltim
        enddo
      enddo
!     if (lprnt) then
!       print *,' in gwdps_lm.f after  a=',a(ipr,:)
!       print *,' in gwdps_lm.f after  b=',b(ipr,:)
!       print *,' db=',db(ipr,:)
!     endif
      tem    = -1.0/g
      do i = 1,npt
        j          = ipt(i)
!       tem    = (-1.e3/g)
        dusfc(j) = tem * dusfc(j)
        dvsfc(j) = tem * dvsfc(j)
      enddo
!                                                                       
!    monitor for excessive gravity wave drag tendencies if ncnt>0
!
!     if(ncnt.gt.0) then
!        if(lat.ge.38.and.lat.le.42) then
!cmic$ guard 37
!           do 92 i = 1,im
!              if(ikount.gt.ncnt) go to 92
!              if(i.lt.319.or.i.gt.320) go to 92
!              do 91 k = 1,km
!                 if(abs(taud(i,k)) .gt. critac) then
!                    if(i.le.im) then
!                       ikount = ikount+1
!                       print 123,i,lat,kdt
!                       print 124,taub(i),bnv(i),ulow(i),
!    1                  gf(i),fr(i),roll(i),hprime(i),xn(i),yn(i)
!                       print 124,(taud(i,kk),kk = 1,km)
!                       print 124,(taup(i,kk),kk = 1,km+1)
!                       print 124,(ri_n(i,kk),kk = 1,km)
!                       do 93 kk = 1,kmm1
!                          velko(kk) =
!    1                  0.5*((u1(i,kk)+u1(i,kk+1))*ubar(i)+
!    2                  (v1(i,kk)+v1(i,kk+1))*vbar(i))*uloi(i)
!93                     continue
!                       print 124,(velko(kk),kk = 1,kmm1)
!                       print 124,(a    (i,kk),kk = 1,km)
!                       print 124,(dtauy(i,kk),kk = 1,km)
!                       print 124,(b    (i,kk),kk = 1,km)
!                       print 124,(dtaux(i,kk),kk = 1,km)
!                       go to 92
!                    endif
!                 endif
!91            continue
!92         continue
!cmic$ end guard 37
!123        format('  *** migwd print *** i=',i3,' lat=',i3,' kdt=',i3)
!124        format(2x,  10e13.6)
!        endif
!     endif
!
!      print *,' in gwdps_lm.f 18  =',a(ipt(1),idxzb(1))
!    &,                          b(ipt(1),idxzb(1)),me
      return
      end
