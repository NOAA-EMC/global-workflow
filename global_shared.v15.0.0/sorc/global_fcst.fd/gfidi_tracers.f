      subroutine gfidi_hyb_resonan
     & (lon_dim_a,lon_dim_h,lons_lat,lat,
     &  dg,tg,ug,vg,ug_m,vg_m,ww,ww_m,dqdphi,dqdlam,qg,
     &  rcl,etaint,spdmax,deltim,nvcn,xvcn,
     &  dtdf,dtdl,
     &  td3_h1,td3_h2,rg1_h,ud3_h1,ud3_h2,vd3_h1,vd3_h2,
     &  sd_m,z_m,
     &  unlm,vnlm,tnlm,unla,vnla,tnla,
     &  dpdt_a,dpdt_d3_h1,dpdt_d3_h2,
     &  grad_gzlam,grad_gzphi,gz_grid,go_forward,batah,gg_tracers,tb_k,
     &  kdt,r_rt,omega_v)
!sk10302012 modifications for cont_eq_opt1
      use machine        , only : kind_evod
      use resol_def      , only : latg2,levm1,levp1,levs,ntrac
      use coordinate_def , only : ak5,bk5,ck,dbk,sv_ecm,t_ecm,y_ecm
      use namelist_def   , only : ref_temp,herm_x,herm_y,herm_z,lin_xyz,
     &                            lin_xy,time_extrap_etadot,
     &                            settls_dep3ds,settls_dep3dg,
     &                            cont_eq_opt1
      use layout1        , only : me
      use physcons       , only : rerth => con_rerth,
     &                               rd => con_rd,
     &                               cp => con_cp,
     &                            omega => con_omega,
     &                             cvap => con_cvap,
     &                             grav => con_g,
     &                               fv => con_fvirt
      implicit none

!  input/output variables
      integer lon_dim_a,lon_dim_h,lons_lat
      integer lat,nvcn,kdt
      real(kind=kind_evod) rcl,deltim,xvcn,batah,r_rt,omega_v
      real(kind=kind_evod),dimension(lon_dim_a,levs) :: dg,tg,dtdf,dtdl,
     &                                                  z_m,unlm,vnlm,
     &                                                  tnlm,unla,vnla,
     &                                                  tnla
      real(kind=kind_evod),dimension(lon_dim_h,levs) :: rg1_h
      real(kind=kind_evod),dimension(lon_dim_h,levs) :: ug,vg,ug_m,vg_m,
     &                   td3_h1,td3_h2,ud3_h1,ud3_h2,vd3_h1,vd3_h2,
     &                   dpdt_d3_h1,dpdt_d3_h2,ww,ww_m
      real(kind=kind_evod),dimension(lon_dim_a) :: dqdphi,dqdlam,qg,
     &                   grad_gzlam,grad_gzphi,gz_grid,sd_m,dpdt_a
!     real(kind=kind_evod),dimension(levs)  :: etamid,spdmax,tb_k
      real(kind=kind_evod),dimension(levs)  :: spdmax,tb_k
      real(kind=kind_evod),dimension(levp1) :: etaint
      logical gg_tracers,go_forward

!  local variables
      integer i,j,k,n,kk,kk1,kp1
      real, parameter :: cons0=0._kind_evod, cons1=1._kind_evod,
     &                   cons2=2._kind_evod
      real(kind=kind_evod), parameter :: rk=rd/cp
     &,                                  rk1=rk+cons1
     &,                                  rkr=cons1/rk
     &,                                  delta1=cvap/cp-cons1
     &,                                  clog2=log(cons2)
      real(kind=kind_evod) coriol,sinra,speed2
     &,                    ugsave,vgsave,wwsave,rdtref
     &,                    px4v,px4u,cofa
!    &,                    px4v,px4u,px5u,px5v,cofa,uprs,vprs
     &,                              batah2,px2u,px2v
!    &,                    px1u,px1v,batah2,px2u,px2v
     &,                    uphi,vphi,rcoslat,coslat,sadx,tem
!    &real wgtw(7)
!
      real(kind=kind_evod),dimension(lon_dim_h,levp1) :: zz
      real(kind=kind_evod),dimension(lon_dim_h,levs)  :: u_n,u_l,v_n,
     &                                                   v_l,t_n,t_l
      real(kind=kind_evod),dimension(lons_lat,levp1)  :: pk5,dot,pk5i
!     real,dimension(lons_lat,levs)                   :: zlam,zphi
!     real(kind=kind_evod),dimension(lons_lat,levs)   :: dpk,advz,cg,cb
      real(kind=kind_evod),dimension(lons_lat,levs)   :: dpk,     cg,cb
     &,                                                  db,workcmb
     &,                                                  cofb,alfa,rlnp
     &,                                                  px2,px3u,px3v
     &,                                                  sadt,z_n,rdel
     &,                                                  rdel2
      real(kind=kind_evod),dimension(lons_lat)        :: expq,dqdt,sd_n
     &,                                                  z_n_sum
     &,                                                  wrk1, wrk2
      real(kind=kind_evod),dimension(levs)            :: sv

!
!!      data wgtw /0.015625,-0.09375,0.234375,0.6875,
!!     .           0.234375,-0.09375,0.015625/

!!    data wgtw /-.03125,0.,.28125,.5,.28125,0.,-.03125/

!moor   data wgtw /0.015625, 0.09375,0.234375,0.3125,
!    .             0.234375, 0.09375,0.015625/

      logical test_mode

!--------------------------------------------------

      do k=1,levs/2
        kk = levp1-k
        do i=1,lons_lat
          tem      = dg(i,kk)
          dg(i,kk) = dg(i,k)
          dg(i,k)  = tem

          tem      = tg(i,kk)
          tg(i,kk) = tg(i,k)
          tg(i,k)  = tem

          tem      = ug(i,kk)
          ug(i,kk) = ug(i,k)
          ug(i,k)  = tem

          tem      = vg(i,kk)
          vg(i,kk) = vg(i,k)
          vg(i,k)  = tem

          tem        = dtdf(i,kk)
          dtdf(i,kk) = dtdf(i,k)
          dtdf(i,k)  = tem

          tem        = dtdl(i,kk)
          dtdl(i,kk) = dtdl(i,k)
          dtdl(i,k)  = tem
        enddo
      enddo

!     call top2bot_array(lon_dim_a,lons_lat,dg)
!     call top2bot_array(lon_dim_a,lons_lat,tg)
!     call top2bot_array(lon_dim_h,lons_lat,ug)
!     call top2bot_array(lon_dim_h,lons_lat,vg)
!     call top2bot_array(lon_dim_a,lons_lat,dtdf)
!     call top2bot_array(lon_dim_a,lons_lat,dtdl)

!
!     if (me == 1 .and. lat == 48) then
!     if (lat == 48) then
!     print *,' sp.hum in gfidi=',rg1_h(1,1:5,1),' at lat=',lat,' me=',me
!     endif

      sinra  = sqrt(cons1-cons1/rcl)
      coriol = cons2*omega*sinra
      if (lat > latg2) coriol = -coriol
!     coriol=0.

      rcoslat = sqrt(rcl)       ! =1/cos(lat)
      coslat  = 1.0 / rcoslat   ! cos(lat)


      do j=1,lons_lat
        expq(j)   = exp(qg(j))
        alfa(j,1) = clog2
        rlnp(j,1) = cons0
        pk5(j,1)  = ak5(1) + bk5(1)*expq(j)
        dqdlam(j) = dqdlam(j)*rcoslat
        dqdphi(j) = dqdphi(j)*rcoslat
      enddo

      spdmax = 0.
      do k=1,levs
        kp1 = k + 1
        do j=1,lons_lat
          pk5(j,kp1)  = ak5(kp1) + bk5(kp1)*expq(j)
          dpk(j,k)    = pk5(j,kp1) - pk5(j,k)
          rdel(j,k)   = cons1/dpk(j,k)
          rdel2(j,k)  = 0.5*rdel(j,k)*gz_grid(j)
          pk5i(j,kp1) = 1.0 / pk5(j,kp1)

          ug(j,k)   = ug(j,k)*rcoslat ! ug at input is scaled u=u*cos(theta)
          vg(j,k)   = vg(j,k)*rcoslat ! vg at input is scaled v=v*cos(theta)

          dtdl(j,k) = dtdl(j,k)*coslat !dtdl in=(dt/dlamda  )/cos**2(theta)
          dtdf(j,k) = dtdf(j,k)*coslat !dtdf in=(dt/d(theta))/cos**2(theta)

          cg(j,k) = ug(j,k)*dqdlam(j) + vg(j,k)*dqdphi(j)

          speed2    = ug(j,k)*ug(j,k) + vg(j,k)*vg(j,k)

          spdmax(k) = max(spdmax(k),speed2)

        enddo
      enddo
      do  k=2,levs
        do j=1,lons_lat
          rlnp(j,k) = log( pk5(j,k+1)*pk5i(j,k) )
          alfa(j,k) = cons1 - pk5(j,k) * rdel(j,k) * rlnp(j,k)
        enddo
      enddo

      k = 1
      do j=1,lons_lat
        db(j,k) = dg(j,k)*dpk(j,k)
        cb(j,k) = cg(j,k)*dbk(k)
      enddo

      do k=1,levm1
        do j=1,lons_lat
          db(j,k+1) = db(j,k) + dg(j,k+1)*dpk(j,k+1)
          cb(j,k+1) = cb(j,k) + cg(j,k+1)*dbk(k+1)
        enddo
      enddo

!sela dqdt=partial(ln(p_s)/partial(t)

      do j=1,lons_lat
        dqdt(j)      = -db(j,levs)/expq(j)-cb(j,levs) !old eulerian dqdt
        dot(j,    1) = cons0
        dot(j,levp1) = cons0

        zz(j,    1) = 0.0
        zz(j,levp1) = 0.0
      enddo

!sela dot=( eta_dot*d(p)/d(eta) )(k+1/2) as in eulerian 

      do k=2,levs
        do j=1,lons_lat
          dot(j,k) = -expq(j)*(bk5(k)*dqdt(j) + cb(j,k-1)) -db(j,k-1)
!sela ww=eta_dot
          zz(j,k) = dot(j,k)
     &     *( etaint(k+1)-etaint(k-1) )/( pk5(j,k+1)-pk5(j,k-1) )
        enddo
      enddo

!     do j=1,lons_lat
!$$$       wfilt(j,    1)=0.0
!$$$       wfilt(j,levp1)=0.0
!     enddo
!$$$!    filter ertical velocities
!$$$!     k=2
!$$$        do i=1,lons_lat
!$$$         wfilt(i,2)=-zz(i,3)*wgtw(1)
!$$$     .              -zz(i,2)*wgtw(2)
!$$$! zz(i,1)=0         +zz(i,1)*wgtw(3)
!$$$     .              +zz(i,2)*wgtw(4)
!$$$     .              +zz(i,3)*wgtw(5)
!$$$     .              +zz(i,4)*wgtw(6)
!$$$     .              +zz(i,5)*wgtw(7)
!$$$        enddo
!$$$!     k=3
!$$$        do i=1,lons_lat
!$$$         wfilt(i,3)=-zz(i,2)*wgtw(1)
!$$$! zz(i,1)=0         -zz(i,1)*wgtw(2)
!$$$     .              +zz(i,2)*wgtw(3)
!$$$     .              +zz(i,3)*wgtw(4)
!$$$     .              +zz(i,4)*wgtw(5)
!$$$     .              +zz(i,5)*wgtw(6)
!$$$     .              +zz(i,6)*wgtw(7)
!$$$        enddo
!$$$
!$$$       do k=4,levs-2
!$$$        do i=1,lons_lat
!$$$         wfilt(i,k)=zz(i,k-3)*wgtw(1)
!$$$     .             +zz(i,k-2)*wgtw(2)
!$$$     .             +zz(i,k-1)*wgtw(3)
!$$$     .             +zz(i,k  )*wgtw(4)
!$$$     .             +zz(i,k+1)*wgtw(5)
!$$$     .             +zz(i,k+2)*wgtw(6)
!$$$     .             +zz(i,k+3)*wgtw(7)
!$$$        enddo
!$$$       enddo
!$$$!     k=levs-1
!$$$        do i=1,lons_lat
!$$$         wfilt(i,levs-1)= zz(i,levs-4)*wgtw(1)
!$$$     .                   +zz(i,levs-3)*wgtw(2)
!$$$     .                   +zz(i,levs-2)*wgtw(3)
!$$$     .                   +zz(i,levs-1)*wgtw(4)
!$$$     .                   +zz(i,levs  )*wgtw(5)
!$$$! zz(levs+1)=0.          +zz(i,levs+1)*wgtw(6)
!$$$     .                   -zz(i,levs  )*wgtw(7)
!$$$        enddo
!$$$!     k=levs   
!$$$        do i=1,lons_lat
!$$$         wfilt(i,levs  )= zz(i,levs-3)*wgtw(1)
!$$$     .                   +zz(i,levs-2)*wgtw(2)
!$$$     .                   +zz(i,levs-1)*wgtw(3)
!$$$     .                   +zz(i,levs  )*wgtw(4)
!$$$! zz(levs+1)=0           +zz(i,levs+1)*wgtw(5)
!$$$     .                   -zz(i,levs  )*wgtw(6)
!$$$     .                   -zz(i,levs-1)*wgtw(7)
!$$$        enddo
!sela do k=1,levs
!sela    do j=1,lons_lat
!sela       ww(j,k)=0.5*(wfilt(j,k)+wfilt(j,k+1) )
!sela    enddo
!$$$! fin   filter ertical velocities

!
!sela check sign of rdel2
!
      k = levs
      do j=1,lons_lat
        sadt(j,1) = rdel2(j,1) * dot(j,2) * (tb_k(2)-tb_k(1))
        sadt(j,k) = rdel2(j,k) * dot(j,k) * (tb_k(k)-tb_k(k-1))
      enddo

      do k=2,levm1
        do j=1,lons_lat
          sadt(j,k) = rdel2(j,k) * (dot(j,k+1) * (tb_k(k+1)-tb_k(k))
     &                           +  dot(j,k)   * (tb_k(k)-tb_k(k-1)))
        enddo
      enddo

!
      do k=1,levs
        do j=1,lons_lat
          ww(j,k)   = 0.5*(zz(j,k)+zz(j,k+1) )
          sadx      = ug(j,k)*grad_gzlam(j) + vg(j,k)*grad_gzphi(j)     
!         advz(j,k) = sadx*r_rt  
          z_n(j,k)  = sadx*r_rt  
          sadt(j,k) = tb_k(k)*sadx + sadt(j,k)
        enddo
      enddo
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!!for testing input fields=output or any locally computed fields=output
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     test_mode = .false.
!     if (test_mode) then !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!       print*,' enter test mode in gfidi_tracers with lin temp.'
!       do k=1,levs
!         do j=1,lons_lat
!          ud3_h1(j,k) = ug(j,k)*rcl !watch scaling for test
!          vd3_h1(j,k) = vg(j,k)*rcl !watch scaling for test
!           dpdt_d3_h1(j,k) = qg(j)
!           tg(j,k)         =  sadt(j,k) !for testing with ncar graphics thru td3_h
!           td3_h1(j,k)     = tg(j,k)
!         enddo
!       enddo
!       do j=1,lons_lat
!         dpdt_a(j) = qg(j)
!       enddo
!       return
!     endif
!      if (test_mode) return !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      batah2 = batah*0.5
      do k=1,levs
        sv(k) = batah2*sv_ecm(k)
      enddo

      do j=1,lons_lat
        z_n_sum(j) = 0.
        sd_n(j)    = 0.
      enddo
      do k=1,levs
        do j=1,lons_lat
!         z_n(j,k)   = dbk(k)*( dqdt(j) + cg(j,k) + advz(j,k) )
          z_n(j,k)   = dbk(k)*( dqdt(j) + cg(j,k) + z_n(j,k) )
          z_n_sum(j) = z_n_sum(j) + z_n(j,k)

          sd_n(j)    = sd_n(j)    + sv(k)*dg(j,k)
        enddo
      enddo

!sk splitting of dpdt_d3 into dpdt_d3_h1 and dpdt_d3_h2
!sk to facilitate lin_xy = t or cont_eq_opt1 = t

      if (go_forward) then
        do j=1,lons_lat
          dpdt_a(j) = deltim*( 0.5*z_n_sum(j)+sd_n(j) ) ! at arrival points
          sd_m(j)   = sd_n(j)
        enddo
        if (cont_eq_opt1) then
          do k=1,levs
            do j=1,lons_lat
              dpdt_d3_h1(j,k) = deltim*( 0.5*z_n(j,k) )
              dpdt_d3_h2(j,k) = ( qg(j) + gz_grid(j)*r_rt )*dbk(k)
              z_m(j,k)        = z_n(j,k)
            enddo
          enddo
        elseif (lin_xy) then
          do k=1,levs
            do j=1,lons_lat
              dpdt_d3_h1(j,k) = qg(j)*dbk(k)
              dpdt_d3_h2(j,k) = deltim*( 0.5*z_n(j,k) )
     &                        + ( qg(j) + gz_grid(j)*r_rt )*dbk(k)
     &                        - dpdt_d3_h1(j,k)
              z_m(j,k)        = z_n(j,k)
            enddo
          enddo
        else                                        !default
          do k=1,levs
            do j=1,lons_lat
              dpdt_d3_h1(j,k) = deltim*( 0.5*z_n(j,k) )
     &                        + ( qg(j) + gz_grid(j)*r_rt )*dbk(k)
              dpdt_d3_h2(j,k) = 0.0
              z_m(j,k)        = z_n(j,k)
            enddo
          enddo
        endif

      else  ! go_forward

        if (cont_eq_opt1) then
          do k=1,levs
            do j=1,lons_lat
              dpdt_d3_h1(j,k) = deltim*( z_n(j,k)-0.5*z_m(j,k) )
              dpdt_d3_h2(j,k) = ( qg(j) + gz_grid(j)*r_rt )*dbk(k)
     &                        + dbk(k)*deltim*( sd_n(j) - sd_m(j) )
              z_m(j,k)        = z_n(j,k)
            enddo
          enddo
        elseif (lin_xy) then
          do k=1,levs
            do j=1,lons_lat
              dpdt_d3_h1(j,k) = qg(j)*dbk(k)
              dpdt_d3_h2(j,k) = deltim*( z_n(j,k)-0.5*z_m(j,k) )
     &                        + ( qg(j) + gz_grid(j)*r_rt )*dbk(k)
     &                        + dbk(k)*deltim*( sd_n(j) - sd_m(j) )
     &                        - dpdt_d3_h1(j,k)
              z_m(j,k)        = z_n(j,k)
            enddo
          enddo
        else                                      !default
          do k=1,levs
            do j=1,lons_lat
              dpdt_d3_h1(j,k) = deltim*( z_n(j,k)-0.5*z_m(j,k) )
     &                        + ( qg(j) + gz_grid(j)*r_rt )*dbk(k)
     &                        + dbk(k)*deltim*( sd_n(j) - sd_m(j) )
              dpdt_d3_h2(j,k) = 0.0
              z_m(j,k)        = z_n(j,k)
            enddo
          enddo
        endif
        do j=1,lons_lat
          dpdt_a(j) = deltim*( 0.5*z_n_sum(j)+sd_n(j) ) ! at arrival points
          sd_m(j)   = sd_n(j)
        enddo
      endif !go_forward
!
      do j=1,lons_lat
        cofb(j,1)    = -rdel(j,1) * (alfa(j,1)*dbk(1))
        px2(j,levs)  = cons0
        px3u(j,levs) = cons0
        px3v(j,levs) = cons0

        workcmb(j,1) = expq(j)*cg(j,1)*dbk(1) - alfa(j,1)
     &               * (dg(j,1)*dpk(j,1)+expq(j)*cb(j,1)*dbk(1))

        wrk1(j)    = expq(j) * dqdlam(j)
        wrk2(j)    = expq(j) * dqdphi(j)
      enddo
!
      do k=2,levs
        kk  = levp1 - k + 1
        kk1 = kk + 1
        do j=1,lons_lat
          cofb(j,k)    = -rdel(j,k)*(bk5(k)*rlnp(j,k)+alfa(j,k)*dbk(k))

          px2(j,kk-1)  = px2(j,kk)
     &       - rd*(bk5(kk1)*pk5i(j,kk1)-bk5(kk)*pk5i(j,kk)) * tg(j,kk)
!
          px3u(j,kk-1) = px3u(j,kk) - rd*rlnp(j,kk)*dtdl(j,kk)
          px3v(j,kk-1) = px3v(j,kk) - rd*rlnp(j,kk)*dtdf(j,kk)

!
          workcmb(j,k) = expq(j) * cg(j,k)
     &                 * ( dbk(k) + ck(k)*rlnp(j,k)*rdel(j,k) )
     &       - rlnp(j,k) * (db(j,k-1)       +expq(j)*cb(j,k-1))
     &       - alfa(j,k) * (dg(j,k)*dpk(j,k)+expq(j)*cg(j,k)*dbk(k))
      
        enddo
      enddo
      do k=1,levs
        do j=1,lons_lat
!sela     u_n(j,k)=0.
!sela     v_n(j,k)=0.
!
!         uprs = cofb(j,k)*rd*tg(j,k) * wrk1(j)
!         vprs = cofb(j,k)*rd*tg(j,k) * wrk2(j)
!
          cofa = -rdel(j,k)*( bk5(k+1)*pk5(j,k)*pk5i(j,k+1) - bk5(k)
     &         +  rlnp(j,k) * (bk5(k)-pk5(j,k)*dbk(k)*rdel(j,k)) )
!
!         px1u = -grad_gzlam(j)  ! grad_gz is expected to be true grad
!         px1v = -grad_gzphi(j)  ! grad_gz is expected to be true grad
!
          tem  = px2(j,k) + (cofb(j,k) - cofa)*rd*tg(j,k)
          px2u = tem * wrk1(j)
          px2v = tem * wrk2(j)


          px4u = -rd*alfa(j,k)*dtdl(j,k)           ! in scaled version /rcl
          px4v = -rd*alfa(j,k)*dtdf(j,k)           ! in scaled version /rcl
!
!         px5u = -cofa*rd*tg(j,k) * wrk1(j)
!         px5v = -cofa*rd*tg(j,k) * wrk2(j)
!
!                                                  grad_gz is expected to be true grad
          uphi = -grad_gzlam(j) + px2u + px3u(j,k) + px4u
          vphi = -grad_gzphi(j) + px2v + px3v(j,k) + px4v
!
          u_n(j,k) =  vg(j,k)*coriol + uphi      ! ug,vg are not scaled in this version
          v_n(j,k) = -ug(j,k)*coriol + vphi       ! ug,vg are not scaled in this version

          t_n(j,k) = rk*workcmb(j,k) * tg(j,k)*(cons1+fv*rg1_h(j,k))
     &             / (cons1+delta1*rg1_h(j,k)) * rdel(j,k)

        enddo
      enddo


!$$$  do k=1,levs
!$$$  do j=1,lons_lat
!$$$   u_l(j,k)= 0.
!$$$   v_l(j,k)= 0.
!$$$   t_l(j,k)= 0.
!$$$  enddo
!$$$  enddo

!$$$  do k_row=1,levs
!$$$   do k_col=1,levs
!$$$    do j=1,lons_lat
!$$$     u_l(j,k_row)=u_l(j,k_row)+y_ecm(k_row,k_col)*dtdl(j,k_col)
!$$$     v_l(j,k_row)=v_l(j,k_row)+y_ecm(k_row,k_col)*dtdf(j,k_col)
!$$$     t_l(j,k_row)=t_l(j,k_row)+t_ecm(k_row,k_col) * dg(j,k_col)
!$$$    enddo
!$$$   enddo
!$$$  enddo

      if ( kind_evod == 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               lons_lat, levs, levs, cons1,
     &               dtdl(1,1), lon_dim_a,
     &               y_ecm(1,1), levs, cons0,
     &               u_l(1,1), lon_dim_h)
         call dgemm ('n', 't',
     &               lons_lat, levs, levs, cons1,
     &               dtdf(1,1), lon_dim_a,
     &               y_ecm(1,1), levs, cons0,
     &               v_l(1,1), lon_dim_h)
         call dgemm ('n', 't',
     &               lons_lat, levs, levs, cons1,
     &               dg(1,1), lon_dim_a,
     &               t_ecm(1,1), levs, cons0,
     &               t_l(1,1), lon_dim_h)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               lons_lat, levs, levs, cons1,
     &               dtdl(1,1), lon_dim_a,
     &               y_ecm(1,1), levs, cons0,
     &               u_l(1,1), lon_dim_h)
         call sgemm ('n', 't',
     &               lons_lat, levs, levs, cons1,
     &               dtdf(1,1), lon_dim_a,
     &               y_ecm(1,1), levs, cons0,
     &               v_l(1,1), lon_dim_h)
         call sgemm ('n', 't',
     &               lons_lat, levs, levs, cons1,
     &               dg(1,1), lon_dim_a,
     &               t_ecm(1,1), levs, cons0,
     &               t_l(1,1), lon_dim_h)
      endif !-----------------------------------------------------------

      rdtref = rd*ref_temp
      do k=1,levs
        do j=1,lons_lat
          u_l(j,k) = rdtref*dqdlam(j) + u_l(j,k) ! in scaled version /rcl
          v_l(j,k) = rdtref*dqdphi(j) + v_l(j,k) ! in scaled version /rcl
        enddo
      enddo


      if (go_forward) then
        do k=1,levs
          do j=1,lons_lat
            unla(j,k) = u_n(j,k) + batah*u_l(j,k)
            vnla(j,k) = v_n(j,k) + batah*v_l(j,k)
            tnla(j,k) = t_n(j,k) + batah*t_l(j,k) - sadt(j,k)

!sela       ud3_h(j,k)  = ug(j,k) + omega_v+
            ud3_h1(j,k) = ug(j,k)
            ud3_h2(j,k) = deltim*(0.5*unla(j,k)-batah2*u_l(j,k))       

            vd3_h1(j,k) = vg(j,k)
            vd3_h2(j,k) = deltim*(0.5*vnla(j,k)-batah2*v_l(j,k))

            td3_h1(j,k) = tg(j,k)
            td3_h2(j,k) = deltim*(0.5*tnla(j,k)-batah2*t_l(j,k))
     &                  - tb_k(k)*gz_grid(j)

            unlm(j,k)  = unla(j,k)
            vnlm(j,k)  = vnla(j,k)
            tnlm(j,k)  = tnla(j,k)

            ug_m(j,k)  = ug(j,k) !for midpoint value in dep. point calc
            vg_m(j,k)  = vg(j,k) !for midpoint value in dep. point calc
            ww_m(j,k)  = ww(j,k)
          enddo
        enddo

      else  ! go_forward

        do k=1,levs
          do j=1,lons_lat
            unla(j,k)   = u_n(j,k) + batah*u_l(j,k)
            vnla(j,k)   = v_n(j,k) + batah*v_l(j,k)
            tnla(j,k)   = t_n(j,k) + batah*t_l(j,k)-sadt(j,k)

!sela       ud3_h(j,k)  = ug(j,k)  + omega_v+

            ud3_h1(j,k) = ug(j,k)
            vd3_h1(j,k) = vg(j,k)
            td3_h1(j,k) = tg(j,k) 

            ud3_h2(j,k) = deltim
     &                  * (unla(j,k)-0.5*unlm(j,k)-batah2*u_l(j,k))
            vd3_h2(j,k) = deltim
     &                  * (vnla(j,k)-0.5*vnlm(j,k)-batah2*v_l(j,k))
            td3_h2(j,k) = deltim
     &                  * (tnla(j,k)-0.5*tnlm(j,k)-batah2*t_l(j,k))
     &                  -  tb_k(k)*gz_grid(j)

            unlm(j,k)  = unla(j,k)
            vnlm(j,k)  = vnla(j,k)
            tnlm(j,k)  = tnla(j,k)
          enddo
        enddo

      endif ! go_forward


!my if any hermite interpolation then linxyz and cubwgtlin should not be used in slgscan 
!my separation of linear and nonlinear terms is not needed
!my also if lin_xyz is false do not separate
      if (herm_x .or. herm_y .or. herm_z .or. .not. lin_xyz) then
         do k=1,levs
           do j=1,lons_lat
             ud3_h1(j,k)     = ud3_h1(j,k)     + ud3_h2(j,k)
             vd3_h1(j,k)     = vd3_h1(j,k)     + vd3_h2(j,k)
             td3_h1(j,k)     = td3_h1(j,k)     + td3_h2(j,k)
             dpdt_d3_h1(j,k) = dpdt_d3_h1(j,k) + dpdt_d3_h2(j,k)
           enddo
         enddo
      endif
      if (.not.lin_xy .and. .not.cont_eq_opt1) then
         do k=1,levs
           do j=1,lons_lat
             dpdt_d3_h1(j,k) = dpdt_d3_h1(j,k) + dpdt_d3_h2(j,k)
           enddo
         enddo
      endif

!my12272012
      do k=1,levs
        do j=1,lons_lat
          ugsave    = ug(j,k)
          ug(j,k)   = 1.5*ug(j,k) - 0.5*ug_m(j,k)
          ug_m(j,k) = ugsave
          vgsave    = vg(j,k)
          vg(j,k)   = 1.5*vg(j,k) - 0.5*vg_m(j,k)
          vg_m(j,k) = vgsave
        enddo
      enddo
      if (time_extrap_etadot) then
        do k=1,levs
          do j=1,lons_lat
            wwsave    = ww(j,k)
            ww(j,k)   = 1.5*ww(j,k) - 0.5*ww_m(j,k)
            ww_m(j,k) = wwsave
          enddo
        enddo
      endif
!sk10052012
      if (settls_dep3ds .or. settls_dep3dg) then
        do k=1,levs
          do j=1,lons_lat
            ugsave    = ug(j,k)
            ug(j,k)   = ugsave + ugsave - ug_m(j,k)
            ug_m(j,k) = ugsave
            vgsave    = vg(j,k)
            vg(j,k)   = vgsave + vgsave - vg_m(j,k)
            vg_m(j,k) = vgsave
            wwsave    = ww(j,k)
            ww(j,k)   = wwsave + wwsave - ww_m(j,k)
            ww_m(j,k) = wwsave
          enddo
        enddo
      endif

      do k=1,levs/2
        kk = levp1-k
        tem        = spdmax(kk)
        spdmax(kk) = spdmax(k)
        spdmax(k)  = tem
      enddo
!     call top2bot_array(1,1,spdmax)


!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!!for testing input fields=output or any locally computed fields=output
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!     test_mode =.false.
!     if (test_mode) then !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!       print*,' enter test mode in gfidi_tracers with lin temp.'

!$$$    do j=1,lons_lat
!$$$       dpdt_a(j)=qg(j)
!$$$    enddo
!       do k=1,levs
!         do j=1,lons_lat

!           unlm(j,k) = unla(j,k) * coslat 
!           vnlm(j,k) = vnla(j,k) * coslat
!           tnlm(j,k) = tnla(j,k) ! for testing with ncar graphics thru td3_h
!          enddo
!       enddo
!     endif
!     if(test_mode) return !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      return
      end

      subroutine top2bot_array(lon_dim,lons_lat,array)
      use machine   , only : kind_evod
      use resol_def , only : levp1,levs
!
      implicit none
      integer,intent(in) :: lon_dim,lons_lat
      real(kind=kind_evod),dimension(lon_dim,levs),intent(inout):: array

      integer i,k,kk
      real(kind=kind_evod) :: tempo

!
      do k=1,levs/2
        kk = levp1-k
        do i=1,lons_lat
          tempo       = array(i,kk)
          array(i,kk) = array(i,k)
          array(i,k)  = tempo   
        enddo
      enddo
      return
      end
      subroutine top2bot_ntrac(lon_dim,lons_lat,array)
      use machine   , only : kind_evod
      use resol_def , only : levp1,levs,ntrac
!
      implicit none
      integer,intent(in) :: lon_dim,lons_lat
      real(kind=kind_evod),dimension(lon_dim,levs,ntrac),
     &                     intent(inout):: array

      integer i,k,n,kk
      real(kind=kind_evod) :: tempo

      do n=1,ntrac
        do k=1,levs/2
          kk = levp1-k
          do i=1,lons_lat
            tempo         = array(i,kk,n)
            array(i,kk,n) = array(i,k,n)
            array(i,k,n)  = tempo
          enddo
        enddo
      enddo
      return
      end
