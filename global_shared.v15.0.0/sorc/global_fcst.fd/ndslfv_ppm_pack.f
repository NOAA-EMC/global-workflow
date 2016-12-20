! -------------------------------------------------------------------------------
      subroutine cyclic_ppm_massadvx1(im,imf,lev,nvars,delt,uc,qq,mass)
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
      integer	im,imf,lev,nvars,mass
      real	delt
      real	uc(imf,lev)
      real	qq(imf,lev,nvars)
!
      real	past(im,nvars),next(im,nvars),da(im,nvars)
      real	dxfact(im)
      real	xreg(im+1),xpast(im+1),xnext(im+1)
      real	uint(im+1)
      real 	dist(im+1),sc,ds
      real, parameter :: fa1 = 9./16.
      real, parameter :: fa2 = 1./16.

      integer  	i,k,n,nv
      
      sc = 2.0 * con_pi
      ds = sc / float(im)
      do i=1,im+1
        xreg(i) = (i-1.5)*ds
      enddo
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
          xpast(i) = xreg(i) - dist(i)
          xnext(i) = xreg(i) + dist(i)
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
        call cyclic_cell_ppm_intp(xreg,past,xpast,da,im,nv,im,im,sc)
        if( mass.eq.1) then
          do n=1,nv
            da(1:im,n) = da(1:im,n) * dxfact(1:im)
          enddo
        endif
        call cyclic_cell_ppm_intp(xnext,da,xreg,next,im,nv,im,im,sc)
        do n=1,nv
          qq(1:im,k,n) = next(1:im,n)
        enddo

      enddo
      
      return
      end subroutine cyclic_ppm_massadvx1
!
!-------------------------------------------------------------------
! -------------------------------------------------------------------------------
      subroutine cyclic_ppm_massadvx(im,lev,nvars,delt,uc,qq,mass)
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
      end subroutine cyclic_ppm_massadvx
!
!-------------------------------------------------------------------
      subroutine cyclic_ppm_massadvy(jm,lev,nvars,delt,vc,qq,mass)
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
      end subroutine cyclic_ppm_massadvy
!
!-------------------------------------------------------------------------------
      subroutine cyclic_ppm_intpx(lev,imp,imf,qq)
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
      end subroutine cyclic_ppm_intpx
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
      integer   i,kl, kh, kk, kkl, kkh, n
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
      qpi(kstr:kend+1,1:nv) = qmi(kstr+1:kend+2,1:nv)
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
