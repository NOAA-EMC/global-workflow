! ------------------------------------------------------------------------------
      subroutine cyclic_intp_red_reg(im,km,imi,imo,i00,i11,
     &                               h00,h10,h01,h11,dxi,qi)
!
      real qi(im,km),dxi
      integer i00(im),i11(im)
      real h00(im),h10(im),h01(im),h11(im)
      real qq(0:imi+2),dq(0:imi+1),qm(imi+1)
      real p0(imo),q0(imo)
      real p1(imo),q1(imo)
      real alfa,beta,tau
      integer i,k
!
!     print *,' enter cyclic_red_reg '

      do k=1,km
        qq(0)     = qi(imi,k)
        qq(1:imi) = qi(1:imi,k)
        qq(imi+1) = qi(1,k)
        qq(imi+2) = qi(2,k)
        dq(0) = (qq(1)-qq(0))/dxi
        do i=1,imi+1
          dq(i) = (qq(i+1)-qq(i))/dxi
          if( dq(i)*dq(i-1) .lt. 0.0 ) then
            qm(i) = 0.0
          else
            qm(i) = 0.5*(dq(i)+dq(i-1))
          endif
        enddo
        do i=1,imi
          if( dq(i).eq.0.0 ) then
            qm(i)=0.0
            qm(i+1)=0.0
          else
            alfa=qm(i  )/dq(i)
            beta=qm(i+1)/dq(i)
            tau = alfa*alfa + beta*beta
            if( tau.gt.9.0 ) then
              tau=3.0/sqrt(tau)
              qm(i  )=tau*alfa*dq(i)
              qm(i+1)=tau*beta*dq(i)
            endif
          endif
        enddo
        do i=1,imo
          p0(i) = qq(i00(i))
          q0(i) = qm(i00(i))
          p1(i) = qq(i11(i))
          q1(i) = qm(i11(i))
        enddo
        do i=1,imo
          qi(i,k) = h00(i)*p0(i)+h10(i)*q0(i)+
     &              h01(i)*p1(i)+h11(i)*q1(i)
        enddo
      enddo
!
!     print *,' finish cyclic_intp_red_reg '
!
      return
      end subroutine cyclic_intp_red_reg
! ------------------------------------------------------------------------------
      subroutine cyclic_intp_dep_arr(im,imf,km,nv,i00,i11,
     &                               h00,h10,h01,h11,dhh,qi)
!
      real qi(imf,km,nv),dhh(imf,km)
      integer i00(imf,km),i11(imf,km)
      real h00(imf,km),h10(imf,km)
      real h01(imf,km),h11(imf,km)
!
      real qq(0:im+2),dq(0:im+1),qm(im+1)
      real p0(im,nv),q0(im,nv)
      real p1(im,nv),q1(im,nv)
      real alfa,beta,tau
      integer i,k,n
!
!     print *,' enter cyclic_intp_dep_arr '
!
      do k=1,km
        do n=1,nv
          qq(   0) = qi(  im,k,n)
          qq(1:im) = qi(1:im,k,n)
          qq(im+1) = qi(1   ,k,n)
          qq(im+2) = qi(2   ,k,n)
          dq(   0) = (qq(1)-qq(0))/dhh(im,k)
          do i=1,im
            dq(i) = (qq(i+1)-qq(i))/dhh(i,k)
          enddo
          dq(im+1) = dq(1)
          do i=1,im+1
!           dq(i) = (qq(i+1)-qq(i))/dhh(i,k)
            if( dq(i)*dq(i-1) .lt. 0.0 ) then
              qm(i) = 0.0
            else
              qm(i) = 0.5*(dq(i)+dq(i-1))
            endif
          enddo
          do i=1,im
            if( dq(i).eq.0.0 ) then
              qm(i  )=0.0
              qm(i+1)=0.0
            else 
              alfa=qm(i  )/dq(i)
              beta=qm(i+1)/dq(i)
              tau = alfa*alfa + beta*beta
              if( tau.gt.9.0 ) then
                tau=3.0/sqrt(tau)
                qm(i  )=tau*alfa*dq(i)
                qm(i+1)=tau*beta*dq(i)
              endif
            endif
          enddo
          do i=1,im
            p0(i,n) = qq(i00(i,k))
            q0(i,n) = qm(i00(i,k))
            p1(i,n) = qq(i11(i,k))
            q1(i,n) = qm(i11(i,k))
          enddo
        enddo
        do n=1,nv
          do i=1,im
            qi(i,k,n) = h00(i,k)*p0(i,n)+h10(i,k)*q0(i,n)+
     &                  h01(i,k)*p1(i,n)+h11(i,k)*q1(i,n)
          enddo
        enddo

      enddo
!
!     print *,' finish cyclic_intp_dep_arr '
!
      return
      end subroutine cyclic_intp_dep_arr

! ------------------------------------------------------------------------------
      subroutine coefx_reg2dep (im,imf,km,dist,
     &           i00,i11,h00,h10,h01,h11,hdx,scale)
!
! pre compute index and coefficients for interpolation 
!      from model grid to departure grid
!
      integer im,imf,km
      integer i00(imf,km),i11(imf,km)
      real    h00(imf,km),h10(imf,km),h01(imf,km),h11(imf,km)
      real    reg(imf),dist(imf,km),hdx(imf,km),dx,scale
! 
      integer i,ii,ip
      real    depi,dep,tt,t2,t3
!
!     print *,' enter coefx_reg2dep im ',im
!
      dx = scale / im
      hdx = dx
      do i=1,im
        reg(i) = (i-1) * dx
      enddo
      do k=1,km
        do i=1,im
          depi = reg(i) - dist(i,k) 
          dep = mod(depi+scale,scale)
          ii  = int(dep/dx) + 1
          if( ii.eq.im ) then
            ip=1
          else
            ip = ii + 1
          endif
          i00(i,k) = ii
          i11(i,k) = ip
          tt  = (dep-reg(ii))/dx
          t2  = tt*tt
          t3  = t2*tt
          h11(i,k) = t3-t2
          h01(i,k) = t2-2.0*h11(i,k)
          h10(i,k) = h11(i,k)-t2+tt
          h00(i,k) = 1.0-h01(i,k)
          h10(i,k) = h10(i,k)*dx
          h11(i,k) = h11(i,k)*dx
        enddo
      enddo
   
!     print *,' finish coefx_reg2dep '
!
      return
      end subroutine coefx_reg2dep
      
! ------------------------------------------------------------------------------
      subroutine coefx_arr2reg (im,imf,km,dist,
     &           i00,i11,h00,h10,h01,h11,hdx,scale)
!
! pre compute index and coefficients for interpolation 
!      from model grid to departure grid
!
      integer im,imf,km
      real    reg(imf),dist(imf,km),hdx(imf,km),dx,scale
      integer i00(imf,km),i11(imf,km)
      real    h00(imf,km),h10(imf,km),h01(imf,km),h11(imf,km)
! 
      integer i,k,ii,ilo,ihi
      real    arr(im+1),adx(im),tt,t2,t3,arrlo,arrhi
!
!     print *,' enter coefx_arr2reg '
!
      dx = scale / im
      do i=1,im
        reg(i) = (i-1)*dx
      enddo
      do k=1,km
        do i=1,im
          arr(i) = reg(i) + dist(i,k)
        enddo
        arr(im+1) = arr(1) + scale
        do i=1,im
          adx(i) = arr(i+1) - arr(i)
        enddo
        do i=1,im+1
          arr(i) = mod(arr(i)+scale,scale)
        enddo
        arrlo = arr(1)
        ilo = arrlo/dx + 1
        do i=2,im+1
          arrhi = arr(i)
          ihi  = arrhi/dx + 1
          if( ihi.gt.ilo ) then
            do ii=ilo+1,ihi
              tt  = (reg(ii)-arrlo)/adx(i-1)
              t2  = tt*tt
              t3  = t2*tt
              h11(ii,k) = t3-t2
              h01(ii,k) = -2.*h11(ii,k)+t2
              h10(ii,k) = h11(ii,k)-t2+tt
              h00(ii,k) = 1.0-h01(ii,k)
              h10(ii,k) = h10(ii,k)*adx(i-1)
              h11(ii,k) = h11(ii,k)*adx(i-1)
              hdx(ii,k) = adx(i-1)
              i00(ii,k) = i-1
              i11(ii,k) = i
            enddo
          else if( ihi.lt.ilo ) then
            if( ilo.lt.im ) then
            do ii=ilo+1,im
              tt  = (reg(ii)-arrlo)/adx(i-1)
              t2  = tt*tt
              t3  = t2*tt
              h11(ii,k) = t3-t2
              h01(ii,k) = -2.*h11(ii,k)+t2
              h10(ii,k) = h11(ii,k)-t2+tt
              h00(ii,k) = 1.0-h01(ii,k)
              h10(ii,k) = h10(ii,k)*adx(i-1)
              h11(ii,k) = h11(ii,k)*adx(i-1)
              hdx(ii,k) = adx(i-1)
              i00(ii,k) = i-1
              i11(ii,k) = i
            enddo
            endif
            arrlolo = arrlo - scale
            do ii=1,ihi
              tt  = (reg(ii)-arrlolo)/adx(i-1)
              t2  = tt*tt
              t3  = t2*tt
              h11(ii,k) = t3-t2
              h01(ii,k) = -2.*h11(ii,k)+t2
              h10(ii,k) = h11(ii,k)-t2+tt
              h00(ii,k) = 1.0-h01(ii,k)
              h10(ii,k) = h10(ii,k)*adx(i-1)
              h11(ii,k) = h11(ii,k)*adx(i-1)
              hdx(ii,k) = adx(i-1)
              i00(ii,k) = i-1
              i11(ii,k) = i
            enddo
          endif
          ilo   = ihi
          arrlo = arrhi
        enddo
      enddo
   
!     print *,' finish coefx_arr2reg '
!
      return
      end subroutine coefx_arr2reg

! ------------------------------------------------------------------------------
      subroutine coefy_reg2dep (jm,km,reg,dist,
     &                          j00,j11,h00,h10,h01,h11,hdy,dy,scale)
!
! pre compute index and coefficients for interpolation 
!      from model grid to departure grid
!
      integer jm,km
      real    reg(jm),dist(jm,km),hdy(jm,km),dy,scale
      integer j00(jm,km),j11(jm,km)
      real    h00(jm,km),h10(jm,km),h01(jm,km),h11(jm,km)
! 
      real    dreg(jm)
      integer j,k,jj,jp
      real    dep,tt,t2,t3
!
!     print *,' enter coefy_reg2dep '
!
      do j=1,jm-1
        dreg(j) = reg(j+1)-reg(j)
      enddo
      dreg(jm) = (scale+reg(1)) - reg(jm)
      do k=1,km
        hdy(:,k)=dreg(:)
        do j=1,jm
          dep = reg(j) - dist(j,k) - reg(1)
          dep = mod(dep+scale,scale)
          jj  = int(dep/dy)+1
          do while( dep.lt.(reg(jj)-reg(1)) ) 
            jj = jj -1
          enddo
          do while( jj.lt.jm .and. dep.gt.(reg(jj+1)-reg(1)) )
            jj = jj + 1
          enddo
          if( jj.eq.jm ) then
            jp=1
          else
            jp = jj+1
          endif
          tt  = (dep+reg(1)-reg(jj))/hdy(jj,k)
          t2  = tt*tt
          t3  = t2*tt
          h11(j,k) = t3-t2
          h01(j,k) = t2-2.*h11(j,k)
          h10(j,k) = h11(j,k)-t2+tt
          h00(j,k) = 1.0-h01(j,k)
          h10(j,k) = h10(j,k)*hdy(jj,k)
          h11(j,k) = h11(j,k)*hdy(jj,k)
          j00(j,k) = jj
          j11(j,k) = jp
        enddo
      enddo
   
!     print *,' finish coefy_reg2dep '
!
      return
      end subroutine coefy_reg2dep
      
! ------------------------------------------------------------------------------
      subroutine coefy_arr2reg (jm,km,reg,dist,
     &                          j00,j11,h00,h10,h01,h11,hdy,dy,scale)
!
! pre compute index and coefficients for interpolation 
!      from model grid to departure grid
!
      integer jm,km
      real    reg(jm),dist(jm,km),hdy(jm,km),scale
      integer j00(jm,km),j11(jm,km)
      real    h00(jm,km),h10(jm,km),h01(jm,km),h11(jm,km)
! 
      integer j,k,jj,j1,j2
      real    arr(jm+1),ady(jm),tt,t2,t3
!
!     print *,' enter coefy_arr2reg '
!
      do k=1,km
        do j=1,jm
          arr(j) = reg(j) + dist(j,k)
        enddo
        arr(jm+1) = arr(1) + scale
        do j=1,jm
          ady(j) = arr(j+1) - arr(j)
        enddo
        do j=1,jm+1
          arr(j) = arr(j) - reg(1)
          arr(j) = mod(arr(j)+scale,scale)
        enddo
        arrlo = arr(1)
        jlo = arrlo/dy+1
        do while( arrlo.lt.(reg(jlo)-reg(1)) ) 
          jlo = jlo -1
        enddo
        do while( jlo.lt.jm .and. arrlo.gt.(reg(jlo+1)-reg(1)) )
          jlo = jlo + 1
        enddo
        do j=2,jm+1
          j1=j-1
          j2=j
          if( j2.gt.jm ) j2=1
          arrhi = arr(j)
          jhi  = arrhi/dy+1
          do while( arrhi.lt.(reg(jhi)-reg(1)) ) 
            jhi = jhi -1
          enddo
          do while( jhi.lt.jm .and. arrhi.gt.(reg(jhi+1)-reg(1)) )
            jhi = jhi + 1
          enddo
          if( jhi.gt.jlo ) then
            do jj=jlo+1,jhi
              tt  = (reg(jj)-arrlo-reg(1))/ady(j1)
              t2  = tt*tt
              t3  = t2*tt
              h11(jj,k) = t3-t2
              h01(jj,k) = -2.*h11(jj,k)+t2
              h10(jj,k) = h11(jj,k)-t2+tt
              h00(jj,k) = 1.0-h01(jj,k)
              h10(jj,k) = h10(jj,k)*ady(j1)
              h11(jj,k) = h11(jj,k)*ady(j1)
              hdy(jj,k) = ady(j1)
              j00(jj,k) = j1
              j11(jj,k) = j2
            enddo
          else if( jhi.lt.jlo ) then
            if( jlo.lt.jm ) then
            do jj=jlo+1,jm
              tt  = (reg(jj)-arrlo-reg(1))/ady(j1)
              t2  = tt*tt
              t3  = t2*tt
              h11(jj,k) = t3-t2
              h01(jj,k) = -2.*h11(jj,k)+t2
              h10(jj,k) = h11(jj,k)-t2+tt
              h00(jj,k) = 1.0-h01(jj,k)
              h10(jj,k) = h10(jj,k)*ady(j1)
              h11(jj,k) = h11(jj,k)*ady(j1)
              hdy(jj,k) = ady(j1)
              j00(jj,k) = j1
              j11(jj,k) = j2
            enddo
            endif
            arrlolo = arrlo - scale
            do jj=1,jhi
              tt  = (reg(jj)-arrlolo-reg(1))/ady(j1)
              t2  = tt*tt
              t3  = t2*tt
              h11(jj,k) = t3-t2
              h01(jj,k) = -2.*h11(jj,k)+t2
              h10(jj,k) = h11(jj,k)-t2+tt
              h00(jj,k) = 1.0-h01(jj,k)
              h10(jj,k) = h10(jj,k)*ady(j1)
              h11(jj,k) = h11(jj,k)*ady(j1)
              hdy(jj,k) = ady(j1)
              j00(jj,k) = j1
              j11(jj,k) = j2
            enddo
          endif
          arrlo = arrhi
          jlo = jhi
        enddo
      enddo
   
!     print *,' finish coefy_arr2reg '

      return
      end subroutine coefy_arr2reg
      
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
! interface with quantity qn at cell average with spline.
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
      integer   i,k, kk, kkl, kkh, n, nv
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
