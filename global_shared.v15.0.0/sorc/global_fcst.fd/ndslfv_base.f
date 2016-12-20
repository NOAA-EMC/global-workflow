!----------------------------------------------------------------------------
 
      subroutine ndslfv_init(lonf,latg,coslat,colrad,wgt,lats_nodes_a,
     &                       global_lats_a,lonsperlat)
!
! initialize some constant variable/arrays for ndsl advection -hmhj
!
! program log
! 2011 02 20 : henry jaung, initiated for ndsl advection
!
      use physcons
      use layout1
!
      implicit none

      real   ,intent(in):: coslat(latg)
      real   ,intent(in):: colrad(latg/2)
      real   ,intent(in):: wgt   (latg/2)
      integer,intent(in):: lats_nodes_a(nodes), lonf, latg
      integer,intent(in):: global_lats_a(latg)
      integer,intent(in):: lonsperlat(latg)
!
      integer	jm2,jm,jmh,i,j,k
      real 	pi,hfpi,twopi,dlon
      real 	y0,y1,y2,y3,y4
      real 	t,t2,t3,xreg,xred,dxred,dxreg
!     real 	dyy(latg*2)
      integer   ired,ireg,lat,lan,lons_lat

      logical   lprint
!
      integer	n,nm,nr,lonp
!
      print *,' enter ndslfv_init '

      lprint = .false.
      pi = con_pi
      hfpi  = pi * 0.5
      twopi = pi * 2.0
      jm  = latg
      jm2 = jm*2
      jmh = jm/2
! ------------------------------------------------------------
! ------------------- gaussian latitude 
! cosglat   cos(lat)
! gglat     gaussian latitude count from 0 to 2 pi
!           from north pole to south pole back to north pole
! gglati    gaussian latitude at edges for gglat
! gslati    cos(lat) at gglati
! ggfact    interpolation factor from gglat to gglati for wind
!
      allocate ( cosglat(jm) )
      allocate ( gglat(jm2), gglati(jm2+1) )
      allocate ( gslati(jm2+1), ggfact(jm2,4) )
!
      do j=1,jm
        cosglat(j)=coslat(j)
      enddo
!
      do j=1,jmh
        gglat(      j) =         colrad(j)
        gglat(jm +1-j) =    pi - colrad(j)
        gglat(jm +  j) =    pi + colrad(j)
        gglat(jm2+1-j) = twopi - colrad(j)
      enddo
!
      gslati(    1)=0.0
      gslati(jm +1)=2.0
      gslati(jm2+1)=4.0
      do j=1,jmh
        gslati(    j+1) = gslati(    j  ) + wgt(j)
        gslati(jm -j+1) = gslati(jm -j+2) - wgt(j)
        gslati(jm +j+1) = gslati(jm +j  ) + wgt(j)
        gslati(jm2-j+1) = gslati(jm2-j+2) - wgt(j)
      enddo

      if( me == 0 .and. lprint ) then
      print *,' cell_edge=1    gslati=',gslati(1)
      do j=1,jm2
      print *,'    cell_mean=',j,'  dgslat= ',gslati(j+1)-gslati(j)
      print *,' cell_edge=',j+1,' gslati= ',gslati(j+1)
      enddo
      endif
!
! real latitude values first at edge, use temporary ggfact
      gglati(1) = hfpi
!     print *,'        gglati =',gglati(1)
      do j=1,jmh
        gglati(j+1) = asin( sin(gglati(j)) - wgt(j) )
        ggfact(j,1) = gglati(j) - gglati(j+1)
!       print *,' j dgglat ',j,ggfact(j,1)
!       print *,'        gglati =',gglati(j+1)
      enddo
!
! then relative latitude from 0 to 2pi
      gglati(    1)=0.0
      gglati(jm +1)=pi
      gglati(jm2+1)=twopi
      do j=1,jmh
        gglati(    j+1) = gglati(    j  ) + ggfact(j,1)
        gglati(jm -j+1) = gglati(jm -j+2) - ggfact(j,1)
        gglati(jm +j+1) = gglati(jm +j  ) + ggfact(j,1)
        gglati(jm2-j+1) = gglati(jm2-j+2) - ggfact(j,1)
      enddo
!
! -------------------------
      if( me == 0 .and. lprint ) then
      print *,' cell_edge=1    gglati=',gglati(1)
      do j=1,jm2
      print *,'  dg1 =',gglat(j)-gglati(j)
      print *,'                   cell_mean=',j,'  gglat= ',gglat(j)
      print *,'  dg2 =',gglati(j+1)-gglat(j)
      print *,' cell_edge=',j+1,' gglati= ',gglati(j+1)
      enddo
      endif
!
! ------------- interpolation factor ---------------
      do j=1,jm2
        y0 = gglati(j)
        if( j == 1 ) then
          y1 = -gglat(2)
          y2 = -gglat(1)
          y3 =  gglat(1)
          y4 =  gglat(2)
        else if( j.eq.2 ) then
          y1 = -gglat(1)
          y2 =  gglat(1)
          y3 =  gglat(2)
          y4 =  gglat(3)
        else if( j == jm2 ) then
          y1 = gglat(jm2-2)
          y2 = gglat(jm2-1)
          y3 = gglat(jm2)
          y4 = twopi+gglat(1)
        else
          y1 = gglat(j-2)
          y2 = gglat(j-1)
          y3 = gglat(j)
          y4 = gglat(j+1)
        endif
        ggfact(j,4) = (y0-y1)*(y0-y2)*(y0-y3)/((y4-y1)*(y4-y2)*(y4-y3))
        ggfact(j,3) = (y0-y1)*(y0-y2)*(y0-y4)/((y3-y1)*(y3-y2)*(y3-y4))
        ggfact(j,2) = (y0-y1)*(y0-y3)*(y0-y4)/((y2-y1)*(y2-y3)*(y2-y4))
        ggfact(j,1) = (y0-y2)*(y0-y3)*(y0-y4)/((y1-y2)*(y1-y3)*(y1-y4))
      enddo
!
! -------------------------------------------------------------
!  determin the longitude with full grid
!
      allocate( gglon(lonf), ggloni(lonf+1) )
      dlon = pi * 2. / lonf
      do i=1,lonf
        gglon(i)=(i-1)*dlon
      enddo
      do i=2,lonf
        ggloni(i)=0.5*(gglon(i-1)+gglon(i))
      enddo
      ggloni(     1)=ggloni(   2)-dlon
      ggloni(lonf+1)=ggloni(lonf)+dlon
!
      if( me == 0 .and. lprint ) then
      print *,'  dlon=',dlon
      print *,' edge number=1    ggloni=',ggloni(1)
      do i=1,lonf
      print *,'               cell number=',i,'  gglon= ',gglon(i)
      print *,' edge number=',i+1,' ggloni= ',ggloni(i+1)
      enddo
      endif
!
! --------------------- for parallel --------------------
!
      allocate( lonstr(nodes), lonlen(nodes) )
      allocate( latstr(nodes), latlen(nodes) )
!
      lonfull = lonf
      lonhalf = lonf / 2	! lonf has to be even
      lonpart = (lonhalf-1)/nodes+1
!
      lonp = lonhalf / nodes
      do n=1,nodes
        lonlen(n)=lonp
      enddo
      nr=mod(lonhalf,nodes)
      if( nr.ne.0 ) then
        do n=1,nr
          lonlen(n)=lonlen(n)+1
        enddo
      endif  
      nm=1
      do n=1,nodes
        lonstr(n) = nm
        nm = nm + lonlen(n)
!       print *,' node lonstr lonlen ',n,lonstr(n),lonlen(n)
      enddo
!! 
      latfull = latg * 2
      lathalf = latg
      latpart = 0
      do n=1,nodes
        latpart=max(latpart,lats_nodes_a(n))
        latlen(n) = lats_nodes_a(n)
      enddo

      nm=1
      do n=1,nodes
        latstr(n) = nm 
        nm = nm + latlen(n)
!       print *,' node latstr latlen ',n,latstr(n),latlen(n)
      enddo 
!
      lonlenmax=0
      latlenmax=0
      do n=1,nodes
        lonlenmax = max(lonlenmax,lonlen(n))
        latlenmax = max(latlenmax,latlen(n))
      enddo
!
      mylonlen = lonlen(me+1)
      mylatlen = latlen(me+1)
!
      dslon = abs(ggloni(lonfull+1)-ggloni(1)) / float(lonfull)
      dslat = abs(gglati(latfull+1)-gglati(1)) / float(latfull)
      scale = twopi
!!
      if( me == 0 .and. lprint ) then
        print *,' dslon ',dslon,' from ',ggloni(lonfull+1),ggloni(1)
        print *,' dslat ',dslat,' from ',gglati(latfull+1),gglati(1)
      endif
!!
! prepare index and coef for regular and reduced grids
!
      allocate ( i00_reg2red(lonfull,mylatlen) )
      allocate ( i11_reg2red(lonfull,mylatlen) )
      allocate ( h00_reg2red(lonfull,mylatlen) )
      allocate ( h10_reg2red(lonfull,mylatlen) )
      allocate ( h01_reg2red(lonfull,mylatlen) )
      allocate ( h11_reg2red(lonfull,mylatlen) )

      allocate ( i00_red2reg(lonfull,mylatlen) )
      allocate ( i11_red2reg(lonfull,mylatlen) )
      allocate ( h00_red2reg(lonfull,mylatlen) )
      allocate ( h10_red2reg(lonfull,mylatlen) )
      allocate ( h01_red2reg(lonfull,mylatlen) )
      allocate ( h11_red2reg(lonfull,mylatlen) )

!     print *,' allocate coef for reg red '

      do lan=1,lats_node_a
        lat=global_lats_a( ipt_lats_node_a-1+lan )
        lons_lat=lonsperlat(lat)
        dxreg = twopi / lonfull
        dxred = twopi / lons_lat
! regular grid to reduced grid
        do i=1,lons_lat
          xred = (i-1)*dxred 
          ireg = xred / dxreg + 1
          i00_reg2red(i,lan) = ireg
          i11_reg2red(i,lan) = ireg+1
          xreg = (ireg-1)*dxreg
          t = (xred-xreg)/dxreg
          t2 = t*t
          t3 = t2*t
          h00_reg2red(i,lan) = 2.0*t3-3.0*t2+1.0
          h01_reg2red(i,lan) = 1.0 - h00_reg2red(i,lan)
          h10_reg2red(i,lan) = (t3-2.0*t2+t)*dxreg
          h11_reg2red(i,lan) = (t3-t2)*dxreg
        enddo
! reduced grid to regular grid
        do i=1,lonfull
          xreg = (i-1)*dxreg 
          ired = xreg / dxred + 1
          i00_red2reg(i,lan) = ired
          if( ired.eq.lons_lat ) then
            i11_red2reg(i,lan) = 1
          else
            i11_red2reg(i,lan) = ired+1
          endif
          xred = (ired-1)*dxred
          t = (xreg-xred)/dxred
          t2 = t*t
          t3 = t2*t
          h00_red2reg(i,lan) = 2.0*t3-3.0*t2+1.0
          h01_red2reg(i,lan) = 1.0 - h00_red2reg(i,lan)
          h10_red2reg(i,lan) = (t3-2.0*t2+t)*dxred
          h11_red2reg(i,lan) = (t3-t2)*dxred
        enddo
      enddo
!     print *,' finish coef for reg red '
!
! allocate for ndsl use
!
!     allocate ( dxx_reg2dep(lonfull,levs,mylatlen) )

!     do j=1,mylatlen
!       do k=1,levs
!         do i=1,lonfull
!           dxx_reg2dep(i,k,j) = dslon
!         enddo
!       enddo
!     enddo
!     print *,' prepared coef for dep arr in x direction '

!     allocate ( dyy_reg2dep(latfull,levs,mylonlen) )

!     print *,' allocate coef for y direction '
!     do j=1,latfull-1
!       dyy(j) = gglat(j+1)-gglat(j)
!     enddo
!     dyy(latfull) = (scale+gglat(1)) - gglat(latfull)
!     do i=1,mylonlen
!       do k=1,levs
!         do j=1,latfull
!           dyy_reg2dep(j,k,i) = dyy(j)
!         enddo
!       enddo
!     enddo
!     print *,' prepared coef for dep arr in y direction '

      print *,' finish ndslfv_init '

      return
      end subroutine ndslfv_init

!!----------------------------------------------------------------
      subroutine def_cfl(im,dist,delx)
!
! rearrange xloc in monotonic increasing
!
      implicit none
!
      integer im
      real dist(im),delx
! local
      integer   m,n,count
      real      check
!
      triple_check: do m=1,3
!
        count=0
        check_defcfl: do n=1,im-1
          check = (dist(n+1)-dist(n))/delx
          if( abs(check).lt.1.0 ) then
            cycle check_defcfl
          else
! for departur
            if( check .ge. 1.0 ) then
              count=count+1
              dist(n+1)=dist(n)+0.98*delx
! for arrival
            else if( check.le.-1.0 ) then
              count=count+1
              dist(n+1)=dist(n)-0.98*delx
            endif
          endif
        enddo check_defcfl
        if(count.gt.0) then
!         print *,' === total non monotone is ',count,' at m=',m
          dist(1) = dist(im)
        else
          return
        endif
!
      enddo triple_check
!
      return
      end subroutine def_cfl

! ------------------------------------------------------------------------
      subroutine vertical_ppm_advect(lons,londim,levs,nvars,             &
     &                                deltim,ssi,wwi,qql,mass)
!
      implicit none

      integer 	londim,levs,nvars,lons,mass
      real 	deltim
      real	ssi(londim,levs+1)
      real	wwi(londim,levs+1)
      real	qql(londim,levs,nvars)

      real ssii(levs+1)
      real ssid(levs+1),ssia(levs+1)
      real dsfact(levs)
      real rqmm(levs,nvars),rqnn(levs,nvars),rqda(levs,nvars)
      integer i,k,n

      do i=1,lons                           

        do k=1,levs+1
          ssii(k)=ssi(i,k)
          ssid(k)=ssi(i,k)-wwi(i,k)*deltim
          ssia(k)=ssi(i,k)+wwi(i,k)*deltim
        enddo

        if( mass.eq.1 ) then
          do k=1,levs
            dsfact(k)=(ssid(k)-ssid(k+1))/(ssia(k)-ssia(k+1))
          enddo
        endif

        do n=1,nvars
          do k=1,levs
            rqmm(k,n) = qql(i,k,n)
          enddo
        enddo
        call vertical_cell_ppm_intp(ssii,rqmm,ssid,rqda,levs,nvars)
        if( mass.eq.1 ) then
          do n=1,nvars
            do k=1,levs
              rqda(k,n) = rqda(k,n) * dsfact(k)
            enddo
          enddo
        endif
        call vertical_cell_ppm_intp(ssia,rqda,ssii,rqnn,levs,nvars)
        do n=1,nvars
          do k=1,levs
            qql(i,k,n)=rqnn(k,n)
          enddo
        enddo

      enddo

      return
      end subroutine vertical_ppm_advect
! 
! ------------------------------------------------------------------------
      subroutine vertical_cell_ppm_intp(pp,qq,pn,qn,levs,nvars)
!
! mass conservation in vertical interpolation: interpolate a group
! of grid point  coordiante call pp at interface with quantity qq at
! cell averaged to a group of new grid point coordinate call pn at
! interface with quantity qn at cell average with ppm spline.
! in vertical with mass conservation is under the condition that
! pp(1)=pn(1), pp(levs+1)=pn(levs+1)
!
! pp    pressure at interfac level as input
! qq    quantity at layer as input
! pn    pressure at interface of new grid structure as input
! qn    quantity at layer as output
! levs  numer of verical layers
!
! author : henry.juang@noaa.gov
!
      implicit none
!
      real      pp(levs+1)
      real      qq(levs,nvars)
      real      pn(levs+1)
      real      qn(levs,nvars)
      integer   levs,nvars
!
      real      massm,massc,massp,massbot,masstop
      real      qmi(levs,nvars),qpi(levs,nvars)
      real      dql(nvars),dqh(nvars)
      real      hh(levs)
      real      dqi,dqimax,dqimin,dqmono(levs,nvars)
      real      tl
      real      th,th2,th3,thp,thm,thc
      real      dpp,dqq,c1,c2
      integer   i,k, kl, kh, kk, kkl, kkh,n
      integer, parameter :: mono=1
!
      if( pp(1).ne.pn(1) .or. pp(levs+1).ne.pn(levs+1) ) then
        print *,' error in vertical_cell_ppm_intp for domain values '
        print *,' i pp1 pn1 ppt pnt ',i,
     &          pp(1),pn(1),pp(levs+1),pn(levs+1)
        call abort
      endif
!
! prepare thickness for grid
!
      do k=1,levs
        hh(k) = pp(k+1)-pp(k)
      enddo
!
! prepare location with monotonic concerns
!
      do n=1,nvars
        massbot=(3.*hh(1)+hh(2))*qq(1,n)-2.*hh(1)*qq(2,n)
        massm = massbot/(hh(1)+hh(2))
        massc = qq(1  ,n)
        massp = qq(1+1,n)
        dqi = 0.25*(massp-massm)
        dqimax = max(massm,massc,massp) - massc
        dqimin = massc - min(massm,massc,massp)
        dqmono(1,n) = sign( min( abs(dqi), dqimin, dqimax ), dqi)
        do k=2,levs-1
          massp = qq(k+1,n)
          massc = qq(k  ,n)
          massm = qq(k-1,n)
          dqi = 0.25*(massp-massm)
          dqimax = max(massm,massc,massp) - massc
          dqimin = massc - min(massm,massc,massp)
          dqmono(k,n) = sign( min( abs(dqi), dqimin, dqimax ), dqi)
        enddo
        masstop=(3.*hh(levs)+hh(levs-1))*qq(levs,n)
     &             -2.*hh(levs)*qq(levs-1,n)
        massp = masstop/(hh(levs)+hh(levs-1))
        massc = qq(levs  ,n)
        massm = qq(levs-1,n)
        dqi = 0.25*(massp-massm)
        dqimax = max(massm,massc,massp) - massc
        dqimin = massc - min(massm,massc,massp)
        dqmono(levs,n) = sign( min( abs(dqi), dqimin, dqimax ), dqi)
!
! compute value at interface with momotone
!
        do k=2,levs
          qmi(k,n)=(qq(k-1,n)*hh(k)+qq(k,n)*hh(k-1))/(hh(k)+hh(k-1))
     &       +(dqmono(k-1,n)-dqmono(k,n))/3.0
        enddo
        do k=1,levs-1
          qpi(k,n)=qmi(k+1,n)
        enddo
        qmi(1,n)=qq(1,n)
        qpi(1,n)=qq(1,n)
        qmi(levs,n)=qq(levs,n)
        qpi(levs,n)=qq(levs,n)
      enddo
!
! do monotonicity
!
      if( mono.eq.1 ) then
        do n=1,nvars
        do k=1,levs
          c1=qpi(k,n)-qq(k,n)
          c2=qq(k,n)-qmi(k,n)
          if( c1*c2.le.0.0 ) then
            qmi(k,n)=qq(k,n)
            qpi(k,n)=qq(k,n)
          endif
        enddo
        do k=1,levs
          c1=(qpi(k,n)-qmi(k,n))*(qq(k,n)-0.5*(qpi(k,n)+qmi(k,n)))
          c2=(qpi(k,n)-qmi(k,n))*(qpi(k,n)-qmi(k,n))/6.
          if( c1.gt.c2 ) then
            qmi(k,n)=3.*qq(k,n)-2.*qpi(k,n)
          else if( c1.lt.-c2 ) then
            qpi(k,n)=3.*qq(k,n)-2.*qmi(k,n)
          endif
        enddo
        enddo
      endif
!
! start interpolation by integral of ppm spline
!
      kkl = 1
      tl=0
      do n=1,nvars
        dql(n)=0.0
      enddo

      do k=1,levs

        kl = k
        kh = k + 1
! find kkh
        do kk=kkl+1,levs+1
          if( pn(kh).ge.pp(kk) ) then
            kkh = kk-1
            go to 100
          endif
        enddo
        print *,' error in vertical_cell_ppm_intp for no lev found '
        print *,' i kh kl ',i,kh,kl
        print *,' pn ',(pn(kk),kk=1,levs+1)
        print *,' pp ',(pp(kk),kk=1,levs+1)
        call abort
 100    continue
        th=(pn(kh)-pp(kkh))/hh(kkh)
        th2=th*th
        th3=th2*th
        thp = th3-th2
        thm = th3-2.*th2+th
        thc = -2.*th3+3.*th2
        do n=1,nvars
          dqh(n)=thp*qpi(kkh,n)+thm*qmi(kkh,n)+thc*qq(kkh,n)
        enddo
! mass interpolate
        if( kkh.eq.kkl ) then
          do n=1,nvars
            qn(k,n) = (dqh(n)-dql(n))/(th-tl)
          enddo
        else if( kkh.gt.kkl ) then
          dpp  = (1.-tl)*hh(kkl) + th*hh(kkh)
          do kk=kkl+1,kkh-1
            dpp = dpp + hh(kk)
          enddo
          do n=1,nvars
            dql(n) = qq(kkl,n)-dql(n)
            dqq  = dql(n)*hh(kkl) + dqh(n)*hh(kkh)
            do kk=kkl+1,kkh-1
              dqq = dqq + qq(kk,n)*hh(kk)
            enddo
            qn(k,n) = dqq / dpp
          enddo
        else
          print *,' error in vertical_cell_ppm_intp for lev messed up '
          print *,' i kh kl ',i,kh,kl
          print *,' pn ',(pn(kk),kk=1,levs+1)
          print *,' pp ',(pp(kk),kk=1,levs+1)
          call abort
        endif
! next one
        kkl = kkh
        tl  = th
        do n=1,nvars
          dql(n) = dqh(n)
        enddo

      enddo	! end of k loop
!
      return
      end subroutine vertical_cell_ppm_intp
