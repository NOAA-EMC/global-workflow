!----------------------------------------------------------------------------
 
      subroutine ndsl_init(lonf,latg,coslat,colrad,wgt,lats_nodes_a)
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
!
      integer	jm2,jm,jmh,i,j
      real 	pi,hfpi,twopi,dlon
      real 	y0,y1,y2,y3,y4

      logical   lprint
!
      integer	n,nm,nr,lonp
!
      if( me.eq.0 ) then
        print *,' initialized all necessary arrays for '
        print *,' non-iterating dimensionally-split '
        print *,' semi-lagrangian advection juang(2011) '
      endif
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
!!
      return
      end subroutine ndsl_init

! ------------------------------------------------------------------------------
      subroutine cyclic_mono_advectx (im,lev,nvars,delt,uc,qq,mono)
!
! compute local advection with monotone cubic hermite iterpolation
! qq is advected by uc from past to next position
!
! author: hann-ming henry juang 2008
!
! im    number of grid point
! lev   number of vertical layer
! nvars number of variable
! delt  time step
! uc    scaled u wind in d(lamda)/dt
! qq    variable input at n-1, output as n+1
! mono  1 for mono, 0 for none
!
      use layout1
!
      implicit none
!
      integer	im,lev,nvars,mono
      real	delt
      real	uc(im,lev)
      real	qq(im,lev,nvars)
!
      real	past(im,nvars),next(im,nvars),da(im,nvars)
      real	dist(im+1),xpast(im),xnext(im)
      real 	ds,sc

      integer  	i,k,n,nv

!
! preparations ---------------------------
!
! x is equal grid spacing, so location can be specified by grid point number
!
        sc = ggloni(im+1)-ggloni(1)
        ds = sc/float(im)
        nv = nvars
!
        do k=1,lev
!
! compute past and next positions of cell interfaces
!
        do i=1,im
          dist(i)  = uc(i,k) * delt
        enddo
        dist(im+1)=dist(1)
        call def_cfl(im+1,dist,ds)
        do i=1,im
          xpast(i) = gglon(i) - dist(i)
          xnext(i) = gglon(i) + dist(i)
        enddo
!
!  mass positive advection
!
        do n=1,nv
          past(1:im,n) = qq(1:im,k,n)
        enddo
        call cyclic_hermite_intp(gglon,past,xpast,da,im,nv,sc,mono)
        call cyclic_hermite_intp(xnext,da,gglon,next,im,nv,sc,mono)
        do n=1,nv
          qq(1:im,k,n) = next(1:im,n)
        enddo

      enddo

      return
      end subroutine cyclic_mono_advectx
!
!-------------------------------------------------------------------
      subroutine cyclic_mono_advecty (jm,lev,nvars,delt,vc,qq,mono)

!
! compute local dvection with monotone cubic hermite interpolation
! qq will be advect by vc from past to next location with 2*delt
!
! author: hann-ming henry juang 2007
!
      use physcons
      use layout1
!
      implicit none
!
      integer   jm,lev,nvars,mono
      real	delt
      real	vc(jm,lev)
      real	qq(jm,lev,nvars)
!
      real	past(jm,nvars),da(jm,nvars),next(jm,nvars)
      real	ypast(jm),ynext(jm)
      real	dist (jm+1)
      real 	ds,sc

      integer  	n,k,j,jmh,nv
!
! preparations ---------------------------
!
      jmh  = jm / 2
      sc = gglati(jm+1)-gglati(1)
      ds = sc/float(jm)
      nv = nvars
!
      do k=1,lev
!
        do j=1,jmh
          dist(j)      =-vc(j    ,k) * delt
          dist(j+jmh)  = vc(j+jmh,k) * delt
        enddo
        dist(jm+1)=dist(1)
        call def_cfl(jm+1,dist,ds)
        do j=1,jm
          ypast(j) = gglat(j) - dist(j)
          ynext(j) = gglat(j) + dist(j)
        enddo
!
! advection all in y
!
        do n=1,nv
          past(1:jm,n) = qq(1:jm,k,n)
        enddo
        call cyclic_hermite_intp (gglat,past,ypast,da,jm,nv,sc,mono)
        call cyclic_hermite_intp (ynext,da,gglat,next,jm,nv,sc,mono)
        do n=1,nv
          qq(1:jm,k,n) = next(1:jm,n)
        enddo
! 
      enddo

      return
      end subroutine cyclic_mono_advecty 

! -------------------------------------------------------------------------
      subroutine cyclic_hermite_intp(pp,qq,pn,qn,lon,nvars,sc,mono)
!
! monotone cubic hermite in cyclic bc interpolation: interpolate a group
! of grid point coordiante call pp with quantity qq at grid-point
! cell averaged to a group of new grid point coordinate call pn at
! interface with quantity qn at cell average with ppm spline.
!
! pp    location at grid point as input
! qq    quantity at grid point as input
! pn    location at new grid point as input
! qn    quantity at new grid point as output
! lon   numer of cells for dimension
! sc    cyclic length
! mono  option to do monotonic(1) or not(0)
!
! author : henry.juang@noaa.gov
!
      use physcons
      use layout1
!
      implicit none
!
      real      pp(lon)
      real      qq(lon,nvars)
      real      pn(lon)
      real      qn(lon,nvars)
      integer   lon,nvars
      real      sc
      integer   mono
!
      real      locs  (3*lon)
      real      qp    (3*lon,nvars)
      real      qm    (3*lon,nvars)
      real      hh    (3*lon)
      real      dq    (3*lon)
      integer   kstr
      real      tl,tl2
      real      th,th2
      real      h00,h01,h10,h11,alfa,beta,tau
      integer   i,kl, kk, kkl, kkh, n, nv
!
! arrange input array cover output location with cyclic boundary condition
!
      nv = nvars
      locs(lon+1:2*lon) = pp(1:lon)
      do i=1,lon
        locs(i) = locs(i+lon) - sc
        locs(i+2*lon) = locs(i+lon) + sc
      enddo

      find_kstr : do i=1,3*lon
        if( pn(1).ge.locs(i) .and. pn(1).lt.locs(i+1) ) then
          kstr = i
          exit find_kstr
        else
          cycle find_kstr
        endif
      enddo find_kstr
      kstr=max(1,kstr)

!
! prepare grid spacing
!
      do i=lon+1,2*lon
        hh(i) = locs(i+1)-locs(i)
      enddo
      do i=1,lon
        hh(i) = hh(i+lon)
        hh(i+2*lon) = hh(i+lon)
      enddo
!
! prepare location with monotonic concerns
!
      do n=1,nv
!
      qp(lon+1:2*lon,n) = qq(1:lon,n)
      do i=1,lon
        qp(i,n) = qp(i+lon,n)
        qp(i+2*lon,n) = qp(i+lon,n)
      enddo
      do i=lon+1,2*lon
        dq(i) = (qp(i+1,n)-qp(i,n))/hh(i)
      enddo
      dq(lon) = dq(2*lon)
      do i=lon+1,2*lon
        qm(i,n) = (dq(i-1)+dq(i))*0.5
        if (sign(qm(i,n),dq(i)).ne.qm(i,n)) qm(i,n)=0.0
        if (sign(qm(i,n),dq(i-1)).ne.qm(i,n)) qm(i,n)=0.0
      enddo

      if( mono.eq.1 ) then
! do monotonicity
      i=lon+1
      do while ( i.le.2*lon ) 
        if( dq(i).eq.0.0 ) then
          qm(i,n)=0.0
          qm(i+1,n)=0.0
        else
          alfa=qm(i,n)/dq(i)
          beta=qm(i+1,n)/dq(i)
          tau = alfa*alfa + beta*beta
          if( tau.gt.9.0 ) then
            tau=3.0/sqrt(tau)
            qm(i,n)=tau*alfa*dq(i)
            qm(i+1,n)=tau*beta*dq(i)
          endif
        endif
        i=i+1
      enddo	! while
      endif	! option of monotonicity

      do i=1,lon
        qm(i,n) = qm(i+lon,n)
        qm(i+2*lon,n) = qm(i+lon,n)
      enddo
!
      enddo
!
!
! start interpolation by integral of ppm 
!
      kkl = kstr
      do i=1,lon
! find kkh
        do kk=kkl+1,3*lon
          if( pn(i).lt.locs(kk) ) then
            kkh = kk
            kkl = kk-1
            go to 100
          endif
        enddo
        print *,' location is not found in cyclic_hermite_intp '
 100    continue
        tl=(pn(i)-locs(kkl))/hh(kkl)
        tl2=tl*tl
        th=1.-tl
        th2=th*th
        h00=(1.+2.*tl)*th2
        h10=tl*th2
        h01=tl2*(3.-2.*tl)
        h11=-tl2*th
        do n=1,nv
        qn(i,n) = qp(kkl,n)*h00+hh(kkl)*qm(kkl,n)*h10                          &
     &           +qp(kkh,n)*h01+hh(kkl)*qm(kkh,n)*h11
        enddo
      enddo
!
      return
      end subroutine cyclic_hermite_intp
! -------------------------------------------------------------------------------
      subroutine cyclic_cell_massadvx(im,lev,nvars,delt,uc,qq,mass)
!
! compute local positive advection with mass conservation
! qq is advected by uc from past to next position
!
! author: hann-ming henry juang 2008
!
      use physcons
      use layout1
!
      implicit none
!
      integer	im,lev,nvars,mass
      real	delt
      real	uc(im,lev)
      real	qq(im,lev,nvars)
!
      real	past(im,nvars),next(im,nvars),da(im,nvars)
      real	dxfact(im)
      real	xpast(im+1),xnext(im+1)
      real	uint(im+1)
      real 	dist(im+1),sc,ds
      real, parameter :: fa1 = 9./16.
      real, parameter :: fa2 = 1./16.

      integer  	i,k,n,nv
      
      sc = ggloni(im+1)-ggloni(1)
      ds = sc / float(im)
      nv = nvars
!
      do k=1,lev
!
! 4th order interpolation from mid point to cell interfaces
!
        do i=3,im-1
          uint(i)=fa1*(uc(i,k)+uc(i-1,k))-fa2*(uc(i+1,k)+uc(i-2,k))
        enddo
        uint(2)=fa1*(uc(2,k)+uc(1 ,k))-fa2*(uc(3,k)+uc(im  ,k))
        uint(1)=fa1*(uc(1,k)+uc(im,k))-fa2*(uc(2,k)+uc(im-1,k))
        uint(im+1)=uint(1)
        uint(im  )=fa1*(uc(im,k)+uc(im-1,k))                            &
     &                 -fa2*(uc(1,k)+uc(im-2,k))
       
!
! compute past and next positions of cell interfaces
!
        do i=1,im+1
          dist(i)  = uint(i) * delt
        enddo
        call def_cfl(im+1,dist,ds)
        do i=1,im+1
          xpast(i) = ggloni(i) - dist(i)
          xnext(i) = ggloni(i) + dist(i)
        enddo
      
        if( mass.eq.1 ) then
         do i=1,im
          dxfact(i) = (xpast(i+1)-xpast(i)) / (xnext(i+1)-xnext(i))
         enddo
        endif
!
!  mass positive advection
!
        do n=1,nv
          past(1:im,n) = qq(1:im,k,n)
        enddo
        call cyclic_cell_ppm_intp(ggloni,past,xpast,da,im,nv,im,im,sc)
        if( mass.eq.1) then
          do n=1,nv
            da(1:im,n) = da(1:im,n) * dxfact(1:im)
          enddo
        endif
        call cyclic_cell_ppm_intp(xnext,da,ggloni,next,im,nv,im,im,sc)
        do n=1,nv
          qq(1:im,k,n) = next(1:im,n)
        enddo

      enddo
      
      return
      end subroutine cyclic_cell_massadvx
!
!-------------------------------------------------------------------
      subroutine cyclic_cell_massadvy(jm,lev,nvars,delt,vc,qq,mass)
!
! compute local positive advection with mass conserving
! qq will be advect by vc from past to next location with 2*delt
!
! author: hann-ming henry juang 2007
!
      use physcons
      use layout1
!
      implicit none
!
      integer   jm,lev,nvars,mass
      real	delt
      real	vc(jm,lev)
      real	qq(jm,lev,nvars)
!
      real	var(jm)
      real	past(jm,nvars),da(jm,nvars),next(jm,nvars)
      real	dyfact(jm)
      real	ypast(jm+1),ynext(jm+1)
      real	dist (jm+1)
      real 	sc,ds
      real, parameter :: fa1 = 9./16.
      real, parameter :: fa2 = 1./16.

      integer  	n,k,j,jmh,nv
!
! preparations ---------------------------
!
      jmh  = jm / 2
      sc = gglati(jm+1)-gglati(1)
      ds = sc / float(jm)
      nv   = nvars
!
      do k=1,lev
!
        do j=1,jmh
          var(j)     = -vc(j    ,k) * delt
          var(j+jmh) =  vc(j+jmh,k) * delt
        enddo

        do j=3,jm-1
          dist(j)=fa1*(var(j)+var(j-1))-fa2*(var(j+1)+var(j-2))
        enddo
        dist(2)=fa1*(var(2)+var(1 ))-fa2*(var(3)+var(jm  ))
        dist(1)=fa1*(var(1)+var(jm))-fa2*(var(2)+var(jm-1))
        dist(jm+1)=dist(1)
        dist(jm  )=fa1*(var(jm)+var(jm-1))-fa2*(var(1)+var(jm-2))

        call def_cfl(jm+1,dist,ds)
        do j=1,jm+1
          ypast(j) = gglati(j) - dist(j)
          ynext(j) = gglati(j) + dist(j)
        enddo
        if( mass.eq.1 ) then
         do j=1,jm
          dyfact(j) = (ypast(j+1)-ypast(j)) / (ynext(j+1)-ynext(j))
         enddo
        endif
!
! advection all in y
!
        do n=1,nv
          past(1:jm,n) = qq(1:jm,k,n)
        enddo
        call cyclic_cell_ppm_intp(gglati,past,ypast,da,jm,nv,jm,jm,sc)

        if( mass.eq.1 ) then
          do n=1,nv
            da(1:jm,n) = da(1:jm,n) * dyfact(1:jm)
          enddo
        endif
        call cyclic_cell_ppm_intp(ynext,da,gglati,next,jm,nv,jm,jm,sc)	
      
        do n=1,nv
          qq(1:jm,k,n) = next(1:jm,n)
        enddo
! 
      enddo
      
      return
      end subroutine cyclic_cell_massadvy
!
!-------------------------------------------------------------------------------
      subroutine cyclic_cell_intpx(lev,imp,imf,qq)
!
! do  mass conserving interpolation from different grid at given latitude
!
! author: hann-ming henry juang 2008
!
      use physcons
      use layout1
      implicit none
!
      integer	 lev, imp, imf
      real	 qq(lonfull,lev)
!
      real	old(lonfull,lev),new(lonfull,lev)
      real	xpast(lonfull+1),xnext(lonfull+1)
      real	two_pi,dxp,dxf,hfdxp,hfdxf,sc
!
      integer  	i,im
!
      im = lonfull
      
! ..................................  
      if( imp.ne.imf ) then
! ..................................
        two_pi = 2.0 * con_pi
        dxp = two_pi / imp
        dxf = two_pi / imf
        hfdxp = 0.5 * dxp
        hfdxf = 0.5 * dxf
            
        do i=1,imp+1
          xpast(i) = (i-1) * dxp - hfdxp
        enddo

        do i=1,imf+1
          xnext(i) = (i-1) * dxf - hfdxf
        enddo

        sc=two_pi
        
        old(1:imp,1:lev)=qq(1:imp,1:lev)
        call cyclic_cell_ppm_intp(xpast,old,xnext,new,im,lev,imp,imf,sc)
      	
        qq(1:imf,1:lev)=new(1:imf,1:lev)

! .................       
      endif
! .................

      return
      end subroutine cyclic_cell_intpx
!
! -------------------------------------------------------------------------
      subroutine cyclic_cell_ppm_intp(pp,qq,pn,qn,lons,nv,lonp,lonn,sc)
!
! mass conservation in cyclic bc interpolation: interpolate a group
! of grid point  coordiante call pp at interface with quantity qq at
! cell averaged to a group of new grid point coordinate call pn at
! interface with quantity qn at cell average with ppm spline.
! in horizontal with mass conservation is under the condition that
! variable value at pp(1)= pp(lons+1)=pn(lons+1)
!
! pp    location at interfac point as input
! qq    quantity at averaged-cell as input
! pn    location at interface of new grid structure as input
! qn    quantity at averaged-cell as output
! lons  numer of cells for dimension
! lonp  numer of cells for input
! lonn  numer of cells for output
! levs  number of vertical layers
! mono  monotonicity o:no, 1:yes
!
! author : henry.juang@noaa.gov
!
      use physcons
      use layout1
!
      implicit none
!
      real      pp(lons+1)
      real      qq(lons  ,nv)
      real      pn(lons+1)
      real      qn(lons  ,nv)
      integer   lons,lonp,lonn,nv
      real      sc
!
      real      locs  (3*lonp)
      real      mass  (3*lonp,nv)
      real      hh    (3*lonp)
      real      fm    (3*lonp)
      real      fn    (3*lonp)
      real      dqmono(3*lonp,nv)
      real      qmi   (3*lonp,nv)
      real      qpi   (3*lonp,nv)
      real      cyclic_length
      integer   kstr,kend
      real      pnmin,pnmax
      real      dqi,dqimax,dqimin
      real      tl,tl2,tl3,tlp,tlm,tlc
      real      th,th2,th3,thp,thm,thc
      real      dql(nv),dqh(nv)
      real      dpp,dqq,c1,c2,cc,r3,r6
      integer   i,kh, kk, kkl, kkh, n
      integer, parameter :: mono=1
!
!     cyclic_length = pp(lonp+1) - pp(1)
      cyclic_length = sc
!
! arrange input array cover output location with cyclic boundary condition
!
      locs(lonp+1:2*lonp) = pp(1:lonp)
      do i=1,lonp
        locs(i) = locs(i+lonp) - cyclic_length
        locs(i+2*lonp) = locs(i+lonp) + cyclic_length
      enddo
      mass(1       :  lonp,1:nv) = qq(1:lonp,1:nv)
      mass(1+  lonp:2*lonp,1:nv) = qq(1:lonp,1:nv)
      mass(1+2*lonp:3*lonp,1:nv) = qq(1:lonp,1:nv)
      
      pnmin = pn(1)
      pnmax = pn(lonn+1)
      do i=2,lonn
        pnmin = min( pnmin, pn(i) )
        pnmax = max( pnmax, pn(i) )
      enddo
 
      if( pnmin.lt.locs(lonp+1) ) then
        do i=lonp,1,-1
          if( pnmin.ge.locs(i) .and. pnmin.lt.locs(i+1) ) then
            kstr = i
            go to 10
          endif
        enddo
      else
        do i=lonp+1,2*lonp
          if( pnmin.ge.locs(i) .and. pnmin.lt.locs(i+1) ) then
            kstr = i
            go to 10
          endif
        enddo
      endif
      print *,' error: can not find kstr: pnmin locs(1) locs(2*lonp) ',
     &                                    pnmin,locs(1),locs(2*lonp)
      print *,' error: pn(1) pn(2) pn(3) ',pn(1),pn(2),pn(3)

 10   kstr=max(3,kstr)

      if( pnmax.lt.locs(2*lonp+1) ) then
        do i=2*lonp,lonp,-1
          if( pnmax.ge.locs(i) .and. pnmax.lt.locs(i+1) ) then
            kend = i+1
            go to 20
          endif
        enddo
      else
        do i=2*lonp+1,3*lonp-1
          if( pnmax.ge.locs(i) .and. pnmax.lt.locs(i+1) ) then
            kend = i+1
            go to 20
          endif
        enddo
      endif
      print *,' error: cannot get kend: pnmax locs(lonp) locs(3*lonp) ',
     &                            kend, pnmax,locs(lonp),locs(3*lonp)
      print *,' error: pn(lonn-1) pn(lonn) pn(lonn+1) ',
     &                 pn(lonn-1),pn(lonn),pn(lonn+1)

 20   kend=min(3*lonp-2,kend)
!
! prepare grid spacing
!
      do i=kstr-2,kend+2
        hh(i) = locs(i+1)-locs(i)
      enddo
      do i=kstr-1,kend+2
       cc = 1./(hh(i)+hh(i-1))
       fm(i) = hh(i  ) * cc
       fn(i) = hh(i-1) * cc
      enddo
!
! prepare location with monotonic concerns
!
      do n=1,nv
      do i=kstr-2,kend+2
        dqi = 0.25*(mass(i+1,n)-mass(i-1,n))
        dqimax = max(mass(i-1,n),mass(i,n),mass(i+1,n)) - mass(i,n)
        dqimin = mass(i,n) - min(mass(i-1,n),mass(i,n),mass(i+1,n))
        dqmono(i,n) = sign( min( abs(dqi), dqimin, dqimax ), dqi)
      enddo
      enddo
!
! compute value at interface with monotone
!
      r3 = 1./3.
      do n=1,nv
      do i=kstr-1,kend+2
        qmi(i,n)=mass(i-1,n)*fm(i)+mass(i,n)*fn(i)                            &
     &       +(dqmono(i-1,n)-dqmono(i,n))*r3
      enddo
      enddo
      qpi(kstr:kend+1,1:n) = qmi(kstr+1:kend+2,1:n)
!
! do less diffusive
!
!     do n=1,nv
!     do i=kstr-1,kend+2
!       qmi(i,n)=mass(i,n)-sign(min(abs(2.*dqmono(i,n)),                   &
!    &                      abs(qmi(i,n)-mass(i,n))),                      &
!    &                      2.*dqmono(i,n))
!       qpi(i,n)=mass(i,n)+sign(min(abs(2.*dqmono(i,n)),                   &
!    &                      abs(qpi(i,n)-mass(i,n))),                      &
!    &                      2.*dqmono(i,n))
!     enddo
!     enddo
!
! do monotonicity within cell
!
      r6 = 1./6.
      if( mono.eq.1 ) then
        do n=1,nv
        do i=kstr-1,kend+1
          c1=qpi(i,n)-mass(i,n)
          c2=mass(i,n)-qmi(i,n)
          if( c1*c2.le.0.0 ) then
            qmi(i,n)=mass(i,n)
            qpi(i,n)=mass(i,n)
          else
            cc=qpi(i,n)-qmi(i,n)
            c1=cc*(mass(i,n)-0.5*(qpi(i,n)+qmi(i,n)))
            c2=cc*cc*r6
            if( c1.gt.c2 ) then
              qmi(i,n)=3.*mass(i,n)-2.*qpi(i,n)
            else if( c1.lt.-c2 ) then
              qpi(i,n)=3.*mass(i,n)-2.*qmi(i,n)
            endif
          endif
        enddo
        enddo
      endif
!
! start interpolation by integral of ppm 
!
      kkl = kstr
      tl=(pn(1)-locs(kkl))/hh(kkl)
      tl2=tl*tl
      tl3=tl2*tl
      tlp = tl3-tl2
      tlm = tl3-2.*tl2+tl
      tlc = -2.*tl3+3.*tl2
      do n=1,nv
        dql(n)=tlp*qpi(kkl,n)+tlm*qmi(kkl,n)+tlc*mass(kkl,n)
      enddo

      do i=1,lonn

        kl = i
        kh = i + 1
! find kkh
        do kk=kkl+1,kend+2
          if( pn(kh).lt.locs(kk) ) then
            kkh = kk-1
            go to 100
          endif
        enddo
      
        print *,' error in cyclic_cell_ppm_intp location not found '
        print *,' lons=',lons,' lonp=',lonp,' lonn=',lonn
        print *,' pnmin=',pnmin,' pnmax=',pnmax
        print *,' pn(1)=',pn(1),' pn(lonn+1)=',pn(lonn+1)
        print *,' kstr =',kstr ,' kend =',kend 
        print *,' kh=',kh,' pn(kh)=',pn(kh)
        print *,' kkl +1=',kkl +1,' locs(kkl +1)=',locs(kkl +1)
        print *,' kend+1=',kend+1,' locs(kend+1)=',locs(kend+1)
        call abort

 100    continue
! mass interpolate
        th=(pn(kh)-locs(kkh))/hh(kkh)
        th2=th*th
        th3=th2*th
        thp = th3-th2
        thm = th3-2.*th2+th
        thc = -2.*th3+3.*th2
        do n=1,nv
          dqh(n)=thp*qpi(kkh,n)+thm*qmi(kkh,n)+thc*mass(kkh,n)
        enddo
        if( kkh.eq.kkl ) then
          do n=1,nv
            qn(i,n) = (dqh(n)-dql(n))/(th-tl)
          enddo
        else if( kkh.gt.kkl ) then
          dpp  = (1.-tl)*hh(kkl) + th*hh(kkh)
          do kk=kkl+1,kkh-1
            dpp = dpp + hh(kk)
          enddo
          do n=1,nv
            dql(n) = mass(kkl,n)-dql(n)
            dqq  = dql(n)*hh(kkl) + dqh(n)*hh(kkh)
            do kk=kkl+1,kkh-1
              dqq = dqq + mass(kk,n)*hh(kk)
            enddo
            qn(i,n) = dqq / dpp
          enddo
        else
          print *,' error in cyclic_cell_ppm_intp location messed up '
          print *,' kkl=',kkl,' kkh=',kkh
          print *,' kh=',kh,' pn(kh)=',pn(kh)
          print *,' kkl-1=',kkl-1,' locs(kkl-1)=',locs(kkl-1)
          call abort
        endif
	
! next one
        kkl = kkh
        tl = th
        do n=1,nv
          dql(n) = dqh(n)
        enddo

      enddo
!
      return
      end subroutine cyclic_cell_ppm_intp
!
! ------------------------------------------------------------------------
      subroutine vertical_cell_advect(lons,londim,levs,nvars,           &
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
      end subroutine vertical_cell_advect
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
!!
!!
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
          print *,' === total non monotone is ',count,' at m=',m
          dist(1) = dist(im)
        else
          return
        endif
!
      enddo triple_check
!
      return
      end subroutine def_cfl

!!
!!
