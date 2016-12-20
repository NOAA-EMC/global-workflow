      subroutine slgscan_h(i_1, i_2, s_lat_in_pe, n_lat_in_pe,
     &                     lan, lat, lon_dim_h,
     &                     ztodt  , iter ,  etamid  , lats_node_a,
     &                     uu_gr_h_2, vv_gr_h_2, ww_gr_h_2, 
     &                     ug_m_2, vg_m_2, ww_m_2,
     &                     lammp_a, phimp_a, sigmp_a, me,
     &                     rgt_h,      rgt_a,
     &                     ud3_h1,     ud3_h2,     ud3_a,
     &                     vd3_h1,     vd3_h2,     vd3_a, 
     &                     td3_h1,     td3_h2,     td3_a,
     &                     dpdt_d3_h1, dpdt_d3_h2, dpdt_d3_a,
     &                     global_lats_a, lonsperlat, ini_slg, ini_dp,
     &                     lprint)
!
      use machine             , only : kind_evod
      use resol_def           , only : latg,levs,lonf,ntrac
      use namelist_def        , only : redgg_a, phigs
     &,                                herm_x, herm_y, herm_z
     &,                                lin_xyz,wgt_cub_lin_xyz,lin_xy
     &,                                settls_dep3ds,settls_dep3dg
     &,                                cont_eq_opt1
     &,                                opt1_3d_qcubic
     &,                                wgt_cub_lin_xyz_trc
!sela use pmgrid              , only : pmap,plon,platd,
      use pmgrid              , only :      plon,platd,
     &                                 plev,plat,plevp,quamon,pi
      use slgshr              , only : phi,dphi,lbasiy,lam,rdlam,dphii,
     &                                 dlam,ra,lbasdy,nlonex,rdlam6
      use gg_def              , only : rcs2_a
!moor use layout_grid_tracers , only : yhalo
      use layout1             , only : ipt_lats_node_a,lats_dim_a

      implicit none

!   input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer lan,lat,lon_dim_h,lats_node_a,me,ini_slg,ini_dp
      integer,dimension(latg) :: global_lats_a,lonsperlat

      real(kind=kind_evod)  ztodt
      real(kind=kind_evod),dimension(levs) :: etamid        ! eta at levels

      real(kind=kind_evod),dimension(lon_dim_h*levs ,
     &                                s_lat_in_pe:n_lat_in_pe) ::
     &        uu_gr_h_2,vv_gr_h_2,ww_gr_h_2,ug_m_2,vg_m_2,ww_m_2

      real(kind=kind_evod),dimension(lonf,levs,lats_node_a) ::
     &        lammp_a,phimp_a,sigmp_a,
     &        ud3_a,vd3_a,td3_a,dpdt_d3_a

      real(kind=kind_evod),dimension(lon_dim_h,levs ,
     &        s_lat_in_pe:n_lat_in_pe,ntrac) :: rgt_h

      real(kind=kind_evod),dimension(lonf,levs,lats_dim_a,ntrac)::rgt_a
      real(kind=kind_evod),dimension(lon_dim_h,levs ,
     &                               s_lat_in_pe:n_lat_in_pe) ::
     &        ud3_h1,ud3_h2,vd3_h1,vd3_h2,td3_h1,td3_h2,
     &        dpdt_d3_h1,dpdt_d3_h2

!   local variables

      integer lan_loc,lat_loc
      integer jj,jj1,k1,i,j,k,n,jcen,iter,i_branch,jtem
     &,       jm1,jp1,jp2
      integer kdim,kdimm2,kk,pmap,bisection,nt
      integer,dimension(i_1:i_2,levs)   :: jdp,kdp
      integer,dimension(i_1:i_2,levs,4) :: idp
      integer,dimension(i_1:i_2,plev)   :: kkdp
      integer,allocatable      :: kdpmpf(:)  ! mapping from artificial array
                                             ! to model level

!     real(kind=kind_evod) udmin,udmax
      real(kind=kind_evod) lam0,cos_f_a,sin_f_a,mindetam,twopi,degrad
      real(kind=kind_evod) dlam_con
      real(kind=kind_evod) x1mx2,               !  |  weights for lcbas_h
     &                     x1mx3,               !  |
     &                     x2mx3                !  |
      real(kind=kind_evod) x_1,                 !  | - for lcdbas_h
     &                     x_2,                 !  |- grid values
     &                     x_3,                 !  |
     &                     x_4,                 !  |
!    &                     x1mx2,               !  |
!    &                     x1mx3,               !  |
     &                     x1mx4,               !  |- differences of grid values
!    &                     x2mx3,               !  |
     &                     x2mx4,               !  |
     &                     x3mx4                !  |

      real(kind=kind_evod) :: x1mx2i,x1mx3i,x1mx4i,x2mx3i,x2mx4i,x3mx4i
     &,                       del,leps,dp                        ! for eta_stuff
     &,                       rdel,dphibr,phibs,tem

      real(kind=kind_evod) :: cos_l_diff,sin_l_diff            ! from rotate_uv_h
!    &,                       alfa_ua,alfa_va,beta_ua,beta_va

      real(kind=kind_evod),dimension(i_1:i_2     ) :: cos_l_a,sin_l_a
      real(kind=kind_evod),dimension(i_1:i_2,levs) ::
     &                     lamdp,phidp,sigdp,
     &                     cos_l_d,sin_l_d,cos_f_d,sin_f_d


      real(kind=kind_evod),dimension(i_1:i_2,plev) ::
     &                     term1y,term2y,term3y,term4y,
     &                     yb,yt,ht,hb,dht,dhb,zt,zb,term1z,term2z,
     &                     term3z,term4z,ys,yn,hs,hn,dhs,dhn
!    &                     term3z,term4z,ys,yn,hs,hn,dhs,dhn,rdphi

      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) ::
     &                     term1x,term2x,term3x,term4x,
     &                     hl,hr,dhl,dhr
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   ::
     &                     x2,x3,xl,xr

!     real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) ::
!    &                     hl,hr,dhl,dhr
!!   &                     x2,x3,xl,xr,hl,hr,dhl,dhr,finty

      real(kind=kind_evod),dimension(lonf,plev)        :: ud3_d,vd3_d

      real(kind=kind_evod), allocatable                :: finty(:,:,:)
      real(kind=kind_evod), allocatable                :: fintx(:,:,:,:)
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4,4) :: fintx

      real(kind=kind_evod),allocatable :: lbasdz(:,:,:)  ! basis funcs for vert
                                                         ! deriv est. at level
      real(kind=kind_evod),allocatable :: lbasiz(:,:,:)  ! lagrange cubic interp
                                                         ! wghts (vert)
      real(kind=kind_evod) ,allocatable :: detam(:)      ! delta eta at levels
     &,                                    detami(:)     ! one / detam

!     ------------------------------------------------------------------
      logical her_x,her_y,her_z,her_a
!    -------------------------------------------------------------------
      logical lprint
      logical qmx_trac,qmy_trac,qmz_trac
      logical qmx_u,qmy_u,qmz_u
      logical qmx_v,qmy_v,qmz_v
      logical qmx_t,qmy_t,qmz_t
!    -------------------------------------------------------------------

!     integer     i_count
!     save        i_count
!     data        i_count   / 0 /

      save kdpmpf, lbasdz, lbasiz, detam, rdel, pmap, detami

!     i_count = i_count + 1
!
!     pi     = 4.*atan(1.)
!     degrad = 180./pi

!    -------------------------------------------------------------------

      if ( ini_slg == 1 ) then

        if (.not.allocated(kdpmpf)) allocate (kdpmpf(plev-1))
        if (.not.allocated(lbasdz)) allocate (lbasdz(4,2,plev))
        if (.not.allocated(lbasiz)) allocate (lbasiz(4,2,plev))
        if (.not.allocated(lbasdy)) allocate (lbasdy(4,2,platd))
        if (.not.allocated(lbasiy)) allocate (lbasiy(4,2,platd))
        if (.not.allocated(phi))    allocate (phi(platd))
        if (.not.allocated(dphi))   allocate (dphi(platd))
        if (.not.allocated(dphii))  allocate (dphii(platd))
        if (.not.allocated(detam))  allocate (detam(plev))
        if (.not.allocated(detami)) allocate (detami(plev))


!       if (me == 0) print*,' calling grdini_h from slgscan_h '
!       call grdini_h(rcs2_a,lonsperlat)

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

!       inline  grdini_h

!       if (me == 0) write(0,*)' reduced grid inside grdini_h  '

        twopi  = pi + pi

        do i=1,plat/2
          tem = acos(sqrt(1./rcs2_a(i)))
          phi(i+2)       = -tem
          phi(platd-1-i) =  tem
        enddo

        phi(1)      = -pi - phi(3)
        phi(2)      = -pi*0.5

        phi(plat+3) =  pi*0.5
        phi(platd)  =  pi - phi(plat+2)

!       do j=1,platd
!          write(6,100)j,phi(j)*degrad
!       enddo
100     format(' in grdini    lat=',i4,2x,' phi=',f9.3)

        do jj = 2,plat+2
!         call lcdbas_h( phi(jj-1), lbasdy(1,1,jj), lbasdy(1,2,jj) )

!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

!      inline  lcdbas_h(grd,dbas2,dbas3)

          jj1 = jj - 1
          x_1 = phi(jj1+0)
          x_2 = phi(jj1+1)
          x_3 = phi(jj1+2)
          x_4 = phi(jj1+3)

          x1mx2 = x_1 - x_2
          x1mx3 = x_1 - x_3
          x1mx4 = x_1 - x_4
          x2mx3 = x_2 - x_3
          x2mx4 = x_2 - x_4
          x3mx4 = x_3 - x_4

          x1mx2i = 1.0 / x1mx2
          x1mx3i = 1.0 / x1mx3
          x1mx4i = 1.0 / x1mx4
          x2mx3i = 1.0 / x2mx3
          x2mx4i = 1.0 / x2mx4
          x3mx4i = 1.0 / x3mx4

          lbasdy(1,1,jj) =   x2mx3 * x2mx4 * x1mx2i * x1mx3i * x1mx4i
          lbasdy(2,1,jj) = - x1mx2i + x2mx3i + x2mx4i
          lbasdy(3,1,jj) = - x1mx2 * x2mx4 * x1mx3i * x2mx3i * x3mx4i
          lbasdy(4,1,jj) =   x1mx2 * x2mx3 * x1mx4i * x2mx4i * x3mx4i

          lbasdy(1,2,jj) = - x2mx3 * x3mx4 * x1mx2i * x1mx3i * x1mx4i
          lbasdy(2,2,jj) =   x1mx3 * x3mx4 * x1mx2i * x2mx3i * x2mx4i
          lbasdy(3,2,jj) = - x1mx3i - x2mx3i + x3mx4i
          lbasdy(4,2,jj) = - x1mx3 * x2mx3 * x1mx4i * x2mx4i * x3mx4i


!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

!         call lcbas_h( phi(jj-1), lbasiy(1,1,jj), lbasiy(1,2,jj) )

!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

!     inline  lcbas_h( grd, bas1, bas2 )

!
          lbasiy(1,1,jj) = x_1
          lbasiy(2,1,jj) = x_2
          lbasiy(3,1,jj) = x_3
          lbasiy(4,1,jj) = x_4

          lbasiy(1,2,jj) =   x1mx2i * x1mx3i * x1mx4i
          lbasiy(2,2,jj) = - x1mx2i * x2mx3i * x2mx4i
          lbasiy(3,2,jj) =   x1mx3i * x2mx3i * x3mx4i
          lbasiy(4,2,jj) = - x1mx4i * x2mx4i * x3mx4i

!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

        end do  !  do jj

        do j = 1,platd-1
          dphi(j)  = phi(j+1) - phi(j)
          dphii(j) = 1.0 / dphi(j)
        end do

!my nlonex is extended lonsperlat and reversed so south to north sn
!my real lats

        do j=1,plat
           jj = platd -2 -j+1 
           nlonex(jj) = lonsperlat(j)
        enddo
!my sp
        nlonex(1) = lonsperlat(plat)
        nlonex(2) = lonsperlat(plat)
!my np
        nlonex(plat+3) = lonsperlat(1)
        nlonex(plat+4) = lonsperlat(1)

        lam0 = 0.0
        do j=1,platd
           dlam(j)   = twopi/float(nlonex(j))
           rdlam(j)  = 1.0/dlam(j)
           rdlam6(j) = (1.0/6.0)*rdlam(j)
           do i = 1,nlonex(j)+3
             lam(i,j) = float(i-2)*dlam(j) + lam0
           end do
        enddo

         if (me == 0) print*,' fini    grdini_h from slgscan_h '

!        call eta_stuff(lats_dim_a,lats_node_a,etamid,etaint,
!    &     kdpmpf,kdpmph,detam,detai,lbasdz,lbassd,lbasiz,lbassi)

!     inline  eta_stuff

        do k = 2,plev-2

!          call lcbas_h(  etamid(k-1), lbasiz(1,1,k),
!    &                               lbasiz(1,2,k) )
!          call lcdbas_h( etamid(k-1), lbasdz(1,1,k),
!    &                               lbasdz(1,2,k) )

!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

!     inline  lcbas_h( grd, bas1, bas2 )

           k1 = k - 1

           x_1 = etamid(k1+0)
           x_2 = etamid(k1+1)
           x_3 = etamid(k1+2)
           x_4 = etamid(k1+3)

           x1mx2 = x_1 - x_2
           x1mx3 = x_1 - x_3
           x1mx4 = x_1 - x_4
           x2mx3 = x_2 - x_3
           x2mx4 = x_2 - x_4
           x3mx4 = x_3 - x_4

           x1mx2i = 1.0 / x1mx2
           x1mx3i = 1.0 / x1mx3
           x1mx4i = 1.0 / x1mx4
           x2mx3i = 1.0 / x2mx3
           x2mx4i = 1.0 / x2mx4
           x3mx4i = 1.0 / x3mx4

           lbasdz(1,1,k) =   x2mx3 * x2mx4 * x1mx2i * x1mx3i * x1mx4i
           lbasdz(2,1,k) = - x1mx2i + x2mx3i + x2mx4i
           lbasdz(3,1,k) = - x1mx2 * x2mx4 * x1mx3i * x2mx3i * x3mx4i
           lbasdz(4,1,k) =   x1mx2 * x2mx3 * x1mx4i * x2mx4i * x3mx4i

           lbasdz(1,2,k) = - x2mx3 * x3mx4 * x1mx2i * x1mx3i * x1mx4i
           lbasdz(2,2,k) =   x1mx3 * x3mx4 * x1mx2i * x2mx3i * x2mx4i
           lbasdz(3,2,k) = - x1mx3i - x2mx3i + x3mx4i
           lbasdz(4,2,k) = - x1mx3 * x2mx3 * x1mx4i * x2mx4i * x3mx4i

           lbasiz(1,1,k) = x_1
           lbasiz(2,1,k) = x_2
           lbasiz(3,1,k) = x_3
           lbasiz(4,1,k) = x_4

           lbasiz(1,2,k) =   x1mx2i * x1mx3i * x1mx4i
           lbasiz(2,2,k) = - x1mx2i * x2mx3i * x2mx4i
           lbasiz(3,2,k) =   x1mx3i * x2mx3i * x3mx4i
           lbasiz(4,2,k) = - x1mx4i * x2mx4i * x3mx4i

!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

!     inline  lcdbas_h(grd,dbas2,dbas3)

!     . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 

        end do  !  do k
!
        mindetam = 999999.9999
        do k = 1,plev-1
          detam (k) = etamid(k+1) - etamid(k)       !min(detam) < pmap
          if( detam(k) < mindetam ) mindetam = detam(k)
          detami(k) = 1.0 / detam(k)
        end do
!
        k = plev + 1
        leps = 1.e-05
        pmap = (etamid(plev) - etamid(1)) / mindetam + 1
        del  = (etamid(plev) - etamid(1)) / float(pmap)
        rdel = float(pmap)/(etamid(levs) - etamid(1))

        if (me == 0) then
          print*,'calculated pmap value is = ',pmap
          print*,'mindetam,del=',mindetam,del,' plev=',plev
          print *,' etamid=',etamid(1:plev)
        endif
!
        kdpmpf(1) = 1
        k = 2
         do kk = 2,pmap
            dp = etamid  (1) + float(kk-1)*del
            if (dp > etamid(k)+leps) then
               kdpmpf(k) = kk
               k = k + 1
            endif
         enddo
      endif  ! fin ini_slg

!     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      if ( ini_dp == 1 ) then

        do lan_loc=1,lats_node_a   !sela begin lan_loc over grids without halos
          lat_loc  = latg + 1
     &            - global_lats_a(ipt_lats_node_a+lats_node_a-lan_loc)
          dlam_con = dlam(2+lat_loc)
          do k=1,levs
            do i=1,lonsperlat(lat_loc)
               lammp_a(i,k,lan_loc) = float(i-1)*dlam_con
               phimp_a(i,k,lan_loc) = phi(2+lat_loc)
               sigmp_a(i,k,lan_loc) = etamid(k)
            enddo
          enddo
          do i=1,lonsperlat(lat_loc)
            sigmp_a(i,plev,lan_loc) = sigmp_a(i,plev,lan_loc) - 1.e-12
          enddo

        enddo

      endif ! fin ini_dp

      if ( ini_slg == 1 .or. ini_dp == 1) return

!sela  completed initialization of slg array and base functions

!    -------------------------------------------------------------------

!sk---------------------------------------------------------------------
      if (quamon) then         !quasi-monotone in lagrange
         qmx_trac = .true.     !in x
         qmy_trac = .true.     !in y
         qmz_trac = .true.     !in z
         qmx_u    = .true.
         qmy_u    = .true.
         qmz_u    = .false.
         qmx_v    = .true.
         qmy_v    = .true.
         qmz_v    = .false.
         qmx_t    = .true.
         qmy_t    = .true.
         qmz_t    = .false.
      else                      !default
         qmx_trac = .false.     !in x
         qmy_trac = .false.     !in y
         qmz_trac = .false.     !in z
         qmx_u    = .false.
         qmy_u    = .false.
         qmz_u    = .false.
         qmx_v    = .false.
         qmy_v    = .false.
         qmz_v    = .false.
         qmx_t    = .false.
         qmy_t    = .false.
         qmz_t    = .false.
      endif

      jcen = 2 + lat

      her_x = herm_x
      her_y = herm_y
      her_z = herm_z
      her_a = her_x .and. her_y .and. her_z

!     if(lat.eq.1 .and. me.eq.0)
!     if(i_count.eq.2 .and. me.eq.0)
!    &  print*,'her_x=',her_x,' her_y=',her_y,' her_z=',her_z

      do i=i_1,i_2
         cos_l_a(i) = cos(lam(i+1,jcen))
         sin_l_a(i) = sin(lam(i+1,jcen))
      enddo
 
       cos_f_a = cos(phi(jcen))
       sin_f_a = sin(phi(jcen))

      if(abs(phi(jcen)) >= phigs) then

!        i_branch = 0
!$$$       print*,' phi(jcen)=',phi(jcen)*57.295,' jcen=',jcen,
!$$$     $ ' dep3dg i_branch=',i_branch

!sk10052012
         if (settls_dep3dg) then
           call settls_dep3dg_h(i_1, i_2, s_lat_in_pe, n_lat_in_pe,
     &                          jcen, ztodt, iter, ra,
     &                          uu_gr_h_2(1,s_lat_in_pe),
     &                          vv_gr_h_2(1,s_lat_in_pe),
     &                          ww_gr_h_2(1,s_lat_in_pe),
     &                          lam, phi, dphii, etamid, detami, kdpmpf,
     &                          lammp_a(1,1,lan), phimp_a(1,1,lan),
     &                          sigmp_a(1,1,lan),
     &                          lamdp, phidp, sigdp, me,
     &                          sin_l_d, cos_l_d, sin_f_d, cos_f_d, 
     &                          ug_m_2(1,s_lat_in_pe),
     &                          vg_m_2(1,s_lat_in_pe),
     &                          ww_m_2(1,s_lat_in_pe),
     &                          rdel)
         else
           call dep3dg_h(i_1, i_2, s_lat_in_pe, n_lat_in_pe,
     &                   jcen, ztodt, iter, ra,
     &                   uu_gr_h_2(1,s_lat_in_pe),
     &                   vv_gr_h_2(1,s_lat_in_pe),
     &                   ww_gr_h_2(1,s_lat_in_pe),
     &                   lam, phi, dphii, etamid, detami, kdpmpf,
     &                   lammp_a(1,1,lan), phimp_a(1,1,lan),
     &                   sigmp_a(1,1,lan),
     &                   lamdp, phidp, sigdp, me,
     &                   sin_l_d, cos_l_d, sin_f_d, cos_f_d, rdel)
         endif

      else

!       i_branch = 1
!$$$       write(0,*)' phi(jcen)=',phi(jcen)*57.295,' jcen=',jcen,
!$$$     $ ' dep3ds i_branch=',i_branch

!sk10052012
         if (settls_dep3ds) then
           call settls_dep3ds_h(i_1, i_2, s_lat_in_pe, n_lat_in_pe,
     &                          jcen, ztodt, iter, ra,
     &                          uu_gr_h_2(1,s_lat_in_pe),
     &                          vv_gr_h_2(1,s_lat_in_pe),
     &                          ww_gr_h_2(1,s_lat_in_pe),
     &                          lam, phi, dphii, etamid, detami, kdpmpf,
     &                          lammp_a(1,1,lan), phimp_a(1,1,lan),
     &                          sigmp_a(1,1,lan),
     &                          lamdp, phidp, sigdp, me,
     &                          sin_l_d, cos_l_d, sin_f_d, cos_f_d,
     &                          ug_m_2(1,s_lat_in_pe),
     &                          vg_m_2(1,s_lat_in_pe),
     &                          ww_m_2(1,s_lat_in_pe),
     &                          rdel)
         else
           call dep3ds_h(i_1, i_2, s_lat_in_pe, n_lat_in_pe,
     &                   jcen, ztodt, iter, ra,
     &                   uu_gr_h_2(1,s_lat_in_pe),
     &                   vv_gr_h_2(1,s_lat_in_pe),
     &                   ww_gr_h_2(1,s_lat_in_pe),
     &                   lam, phi, dphii, etamid, detami, kdpmpf,
     &                   lammp_a(1,1,lan), phimp_a(1,1,lan),
     &                   sigmp_a(1,1,lan),
     &                   lamdp, phidp, sigdp, me,
     &                   sin_l_d, cos_l_d, sin_f_d, cos_f_d, rdel)
         endif

      endif

      dphibr = 1./( phi(platd/2+1) - phi(platd/2) )
      phibs  = phi(1)

!my compute jdp first before idp since rdlam is now lat dependent 

      do k = 1,levs
        do i=i_1,i_2
          jtem = int ( (phidp(i,k) - phibs)*dphibr + 1. )
          if( phidp(i,k) >= phi(jtem+1) ) jtem = jtem + 1
          jdp(i,k) = jtem
          jm1 = max(1,jtem-1)
          jp1 = min(platd,jtem+1)
          jp2 = min(platd,jtem+2)

          idp(i,k,2) = 2 + int( lamdp(i,k) * rdlam(jtem) )
          if (redgg_a) then
            idp(i,k,1) = 2 + int( lamdp(i,k) * rdlam(jm1) )
            idp(i,k,3) = 2 + int( lamdp(i,k) * rdlam(jp1) )
            idp(i,k,4) = 2 + int( lamdp(i,k) * rdlam(jp2))
          else
            idp(i,k,1) = idp(i,k,2)
            idp(i,k,3) = idp(i,k,2)
            idp(i,k,4) = idp(i,k,2)
          endif
!
          kdp(i,k) = bisection(kdpmpf,plev-1,
     &                 int((sigdp(i,k) - etamid(1))*rdel + 1. ))
          if(sigdp(i,k) >= etamid(kdp(i,k)+1)) then
             kdp(i,k) = kdp(i,k) + 1
          endif
!
        enddo
      enddo
       
!     write(0,*)' before calling herx i_1=',i_1,' i_2=',i_2
      call herxinit_h(i_1,i_2,idp,jdp,lamdp,xl,xr,hl,hr,dhl,dhr)

      call lagxinit_h(i_1,i_2,lam,lamdp,idp,jdp,x2,x3,
     &                term1x,term2x,term3x,term4x)

      call heryinit_h(i_1,i_2,phi,dphi,dphii,phidp,jdp,ys,yn,
     &                hs,hn,dhs,dhn)

      call lagyinit_h(i_1,i_2,phi,dphii,lbasiy,phidp,jdp,
     &                yb,yt,term1y,term2y,term3y,term4y)

      kdim   = plev
      kdimm2 = kdim - 2
      do k = 1,plev
        do i = i_1,i_2
          kkdp(i,k) = min(kdimm2, max(2, kdp(i,k)))
        enddo
      enddo
!
       if (her_z) then
          call herzinit_h(i_1,i_2,kdim,etamid,detam,detami,sigdp,kdp,
     &                    hb,ht,dhb,dht)
       else
          call lagzinit_h_n(i_1,i_2,kdim,lbasiz,etamid,detam,sigdp,
     &                      kdp,kkdp,hb,ht,dhb,dht,zb,zt,
     &                      term1z,term2z,term3z,term4z)
       endif

!-----------------------------------------------------------------------

       if (her_a) then             ! pure hermite interpolation in 3d
                                   ! --------------------------------

         do nt=1,ntrac
           call her_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                       idp,jdp,kdp,kkdp,xl,xr,hl,hr,dhl,dhr,
     &                       rgt_h(1,1,s_lat_in_pe,nt),
     &                             ys,yn,hs,hn,dhs,dhn,dphii,
     &                       kdim,lbasdz,hb,ht,dhb,dht,detam,
     &                       rgt_a(1,1,lan,nt))
         enddo

         call her_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                     idp,jdp,kdp,kkdp,xl,xr,hl,hr,dhl,dhr,ud3_h1,
     &                           ys,yn,hs,hn,dhs,dhn,dphii,
     &                     kdim,lbasdz,hb,ht,dhb,dht,detam,ud3_d)

         call her_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                     idp,jdp,kdp,kkdp,xl,xr,hl,hr,dhl,dhr,vd3_h1,
     &                           ys,yn,hs,hn,dhs,dhn,dphii,
     &                     kdim,lbasdz,hb,ht,dhb,dht,detam,vd3_d)

          call her_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                      idp,jdp,kdp,kkdp,xl,xr,hl,hr,dhl,dhr,td3_h1,
     &                            ys,yn,hs,hn,dhs,dhn,dphii,
     &                      kdim,lbasdz,hb,ht,dhb,dht,detam,
     &                      td3_a(1,1,lan))

         call int2d_h_levs(i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim,
     &                     dpdt_d3_h1,idp,jdp,
     &                     x2,x3,term1x,term2x,term3x,term4x,
     &                     term1y,term2y,term3y,term4y,
     &                     dpdt_d3_a(1,1,lan))

       else                        ! mix hermite and/or lagrange or pure lagrange interpolation
                                   ! ----------------------------------------------------------
         if (.not. allocated(fintx)) allocate (fintx(i_1:i_2,plev,4,4))
         if (.not. allocated(finty)) allocate (finty(i_1:i_2,plev,4))

         do nt=1,ntrac
           if (her_x) then
             call herxin_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,idp,jdp,kdp,
     &                     kkdp,xl,xr,hl,hr,dhl,dhr,
     &                     rgt_h(1,1,s_lat_in_pe,nt),fintx)
           else
             call lagxin_h_m(i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim,
     &                       rgt_h(1,1,s_lat_in_pe,nt),idp,jdp,kdp,kkdp,
     &                       x2,x3,term1x,term2x,term3x,term4x,fintx,
     &                       qmx_trac)
           endif
           if (her_y) then
             call heryin_h(i_1,i_2,jdp,ys,yn,hs,hn,dhs,dhn,dphii
     &,                    fintx,finty)
           else
             call lagyin_h_m(i_1,i_2,fintx,finty,yb,yt,term1y,term2y
     &,                      term3y,term4y,qmy_trac)
           endif

           if (her_z) then
             call herzin_h(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht
     &,                    detam,term1z,term2z,term3z,term4z
     &,                    rgt_a(1,1,lan,nt))
           else
             call lagzin_h_m(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht
     &,                      detam,term1z,term2z,term3z,term4z
     &,                      rgt_a(1,1,lan,nt),qmz_trac)
           endif

!my weighted cubic linear interpolation for generalized number of tracers
           if (wgt_cub_lin_xyz_trc) then
             call wgt_cub_lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_ in_pe
     &,                                rgt_h(1,1,s_lat_in_pe,nt),idp,jdp
     &,                                kdp,xl,xr,ys,yn ,zb,zt
     &,                                rgt_a(1,1,lan,nt))
           endif
         enddo
!-----------------------------------------------------------------------
         if (her_x) then
           call herxin_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,idp,jdp,kdp,
     &                   kkdp,xl,xr,hl,hr,dhl,dhr,ud3_h1,fintx)
         else
          call lagxin_h_m(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                    kdim,ud3_h1,idp,jdp,kdp,kkdp,
     &                    x2,x3,term1x,term2x,term3x,term4x,fintx,qmx_u)
         endif

         if (her_y) then
           call heryin_h(i_1,i_2,jdp,ys,yn,hs,hn,dhs,dhn,dphii,
     &                   fintx,finty)
         else
           call lagyin_h_m(i_1,i_2,fintx,finty,yb,yt,term1y,term2y,
     &                     term3y,term4y,qmy_u)
         endif
         if (her_z) then
           call herzin_h(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,
     &                   detam,term1z,term2z,term3z,term4z,ud3_d)
         else
           call lagzin_h_m(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,
     &                     detam,term1z,term2z,term3z,term4z,ud3_d,
     &                     qmz_u)
          endif
!
         if (her_x) then
           call herxin_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,idp,jdp,kdp,
     &                   kkdp,xl,xr,hl,hr,dhl,dhr,vd3_h1,fintx)
         else
           call lagxin_h_m(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                     kdim,vd3_h1,idp,jdp,kdp,kkdp,
     &                     x2,x3,term1x,term2x,term3x,term4x,
     &                     fintx,qmx_v)
         endif

         if (her_y) then
           call heryin_h(i_1,i_2,jdp,ys,yn,hs,hn,dhs,dhn,dphii,
     &                   fintx,finty)
         else
           call lagyin_h_m(i_1,i_2,fintx,finty,yb,yt,term1y,term2y,
     &                     term3y,term4y,qmy_v)
         endif
         if (her_z) then
             call herzin_h(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,
     &                     detam,term1z,term2z,term3z,term4z,vd3_d)
         else
             call lagzin_h_m(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,
     &                       dht,detam,term1z,term2z,term3z,term4z,
     &                       vd3_d,qmz_v)
         endif

!-----------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
         if (her_x) then
           call herxin_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,idp,jdp,kdp,
     &                   kkdp,xl,xr,hl,hr,dhl,dhr,td3_h1,fintx)
         else
           call lagxin_h_m(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                     kdim,td3_h1,idp,jdp,kdp,kkdp,x2,x3,
     &                     term1x,term2x,term3x,term4x,fintx,qmx_t)

         endif
         if (her_y) then
           call heryin_h(i_1,i_2,jdp,ys,yn,hs,hn,dhs,dhn,dphii,
     &                   fintx,finty)
         else
           call lagyin_h_m(i_1,i_2,fintx,finty,yb,yt,term1y,term2y,
     &                     term3y,term4y,qmy_t)
         endif
         if (her_z) then
           call herzin_h(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,
     &                   detam,term1z,term2z,term3z,term4z,
     &                   td3_a(1,1,lan))
         else
           call lagzin_h_m(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,
     &                     detam,term1z,term2z,term3z,term4z,
     &                     td3_a(1,1,lan),qmz_t)
         endif

!-----------------------------------------------------------------------
!my weighted cubic linear interpolation
         if (wgt_cub_lin_xyz) then
           call wgt_cub_lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                               ud3_h1,idp,jdp,kdp,xl,xr,ys,yn,
     &                               zb,zt,ud3_d)

           call wgt_cub_lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                               vd3_h1,idp,jdp,kdp,xl,xr,ys,yn,
     &                               zb,zt,vd3_d)

           call wgt_cub_lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                               td3_h1,idp,jdp,kdp,xl,xr,ys,yn,
     &                               zb,zt,td3_a(1,1,lan))

         endif

!my use linear interpolation for u,v,t for nonlinear terms and add to advected terms ug,vg,tg

         if (lin_xyz) then
           call lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                       ud3_h2,vd3_h2,td3_h2,idp,jdp,kdp,xl,xr,
     &                       ys,yn,zb,zt,ud3_d,vd3_d,td3_a(1,1,lan))
         endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         if (lin_xyz.and.cont_eq_opt1) then
           if (opt1_3d_qcubic) then
!sk10302012 (ia) tricubic interpolation for dpdt_d3_h1
             call lagxin_h_m(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                       kdim,dpdt_d3_h1,idp,jdp,kdp,kkdp,x2,x3,
     &                       term1x,term2x,term3x,term4x,fintx,quamon)
             call lagyin_h_m(i_1,i_2,fintx,finty,yb,yt,term1y,term2y,
     &                       term3y,term4y,quamon)
             call lagzin_h_m(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,
     &                       dhb,dht,detam,term1z,term2z,term3z,term4z,
     &                       dpdt_d3_a(1,1,lan),quamon)
           else 
!sk11022012 (ib) tricubic interpolation for dpdt_d3_h1
             call linxyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                        dpdt_d3_h1,idp,jdp,kdp,xl,xr,ys,yn,zb,zt,
     &                        dpdt_d3_a(1,1,lan))
           endif
!sk10302012 (ii) bicubic interpolation for dpdt_d3_h2
           call int2d_h_levs_mxy_(i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim,
     &                            dpdt_d3_h2,idp,jdp,
     &                            x2,x3,term1x,term2x,term3x,term4x,
     &                            term1y,term2y,term3y,term4y,
     &                            dpdt_d3_a(1,1,lan))

         elseif (lin_xyz .and. lin_xy) then
           call int2d_h_levs_mxy(i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim,
     &                           dpdt_d3_h1,idp,jdp,
     &                           x2,x3,term1x,term2x,term3x,term4x,
     &                           term1y,term2y,term3y,term4y,
     &                           dpdt_d3_a(1,1,lan))
!use linear interp in x and y for nonlinear term and add to advected lnps 
           call lin_xy_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                      kdim,dpdt_d3_h2,idp,jdp,x2,x3,yb,yt,
     &                      dpdt_d3_a(1,1,lan))
         else
           call int2d_h_levs(i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim,
     &                       dpdt_d3_h1,idp,jdp,
     &                       x2,x3,term1x,term2x,term3x,term4x,
     &                       term1y,term2y,term3y,term4y,
     &                       dpdt_d3_a(1,1,lan))
         endif

         if (allocated(fintx)) deallocate (fintx)
         if (allocated(finty)) deallocate (finty)

      endif  !  if (her_a) then
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!sela udmax=-9999.
!sela udmin= 9999.
!sela do k=1,levs
!sela  do i=1,lonf
!sela   if(ud3_d(i,k) > udmax)udmax=ud3_d(i,k)
!sela   if(ud3_d(i,k) < udmin)udmin=ud3_d(i,k)
!sela  enddo
!sela enddo
!sela print*,'lan=',lan,' lonf=',lonf,' udmax udmin=',udmax,udmin
!sela udmax=-9999.
!sela udmin= 9999.
!sela do k=1,levs
!sela  do i=1,lonf
!sela   if(vd3_d(i,k) > udmax)udmax=vd3_d(i,k)
!sela   if(vd3_d(i,k) < udmin)udmin=vd3_d(i,k)
!sela  enddo
!sela enddo
!sela print*,'lan=',lan,' lonf=',lonf,' vdmax vdmin=',udmax,udmin

!     call rotate_uv_h(i_1,i_2,
!    &                 ud3_d,vd3_d, ud3_a(1,1,lan),vd3_a(1,1,lan),
!    &                 cos_l_a,sin_l_a,cos_f_a,sin_f_a,
!    &                 cos_l_d,sin_l_d,cos_f_d,sin_f_d)

      do k = 1,plev
        do i = i_1,i_2
         cos_l_diff = cos_l_a(i)*cos_l_d(i,k) + sin_l_a(i)*sin_l_d(i,k)
         sin_l_diff = sin_l_a(i)*cos_l_d(i,k) - cos_l_a(i)*sin_l_d(i,k)


!
!     ud3_d, vd3_d are bottom to top, coming from lagzin.
!     alfas,betas  are bottom to top, coming from previous loops.
!therefore:
!     ud3_a, vd3_a are bottom to top, as expected in gloopa.

         ud3_a(i,k,lan) =  cos_l_diff                       * ud3_d(i,k)
     &                  +  sin_l_diff*sin_f_d(i,k)          * vd3_d(i,k)

         vd3_a(i,k,lan) = -sin_l_diff*sin_f_a               * ud3_d(i,k)
     &                  + (cos_f_a*cos_f_d(i,k) +
     &                     cos_l_diff*sin_f_a*sin_f_d(i,k)) * vd3_d(i,k)
        end do
      end do

!sela udmax=-9999.
!sela udmin= 9999.
!sela do k=1,levs
!sela  do i=1,lonf
!sela   if(ud3_a(i,k,lan) > udmax)udmax=ud3_a(i,k,lan)
!sela   if(ud3_a(i,k,lan) < udmin)udmin=ud3_a(i,k,lan)
!sela  enddo
!sela enddo
!sela print*,'lan=',lan,' lonf=',lonf,' uamax uamin=',udmax,udmin
!sela udmax=-9999.
!sela udmin= 9999.
!sela do k=1,levs
!sela  do i=1,lonf
!sela   if(vd3_a(i,k,lan) > udmax)udmax=vd3_a(i,k,lan)
!sela   if(vd3_a(i,k,lan) < udmin)udmin=vd3_a(i,k,lan)
!sela  enddo
!sela enddo
!sela print*,'lan=',lan,' lonf=',lonf,' vamax vamin=',udmax,udmin
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      return
      end
      subroutine dep3dg_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                    jcen,dt,iterdp,ra,ub,vb,wb,
     &                    lam,phib,dphibi,etamid,detami,
     &                    kdpmpf,lammp_a,phimp_a,sigmp_a,
     &                    lamdp,phidp,sigdp,me,
     &                    sin_l_d,cos_l_d,sin_f_d,cos_f_d,rdel)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : mprec,plat,platd,
     &                                plev,plon,plond,pi
      use slgshr             , only : dlam,rdlam
      use namelist_def       , only : redgg_a
!
      implicit none

!  input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer jcen,iterdp,me
      integer,dimension(plev-1) :: kdpmpf   ! artificial vert grid indic

      real(kind=kind_evod) dt,ra,rdel
!     real(kind=kind_evod) dt,ra,rdel,pi
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                            :: ub,vb,wb
      real(kind=kind_evod),dimension(plev)        :: detami
      real(kind=kind_evod),dimension(plev)        :: etamid     ! eta at levels
      real(kind=kind_evod),dimension(plond,platd) :: lam
      real(kind=kind_evod),dimension(platd)       :: phib,dphibi
      real(kind=kind_evod),dimension(plon,plev)   :: lammp_a,phimp_a,
     &                                               sigmp_a
      real(kind=kind_evod),dimension(i_1:i_2,plev)::
     &                                lamdp,phidp,sigdp,sin_l_d,cos_l_d,
     &                                sin_f_d,cos_f_d

!  local variables
      integer iter,i,k,kk,bisection,jtem,jm1,jp1,jp2
      integer,dimension(i_1:i_2,plev)  :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4):: idp

      real(kind=kind_evod) :: sphisc,cphisc,clamsc,phibs,dphibr,fac
      real(kind=kind_evod) :: phicen,cphic,sphic,hfdt,cphic2i

      real(kind=kind_evod) :: twopi,pi2,sgnphi,sphipr,cphipr,
     &                        clampr,slam2,phipi2,slampr,dlamx,
     &                        coeff,distmx,dist
      real(kind=kind_evod) :: cdlam,clamp,cphimp,cphip,sdlam,
     &                        slamp,sphimp,sphip,cosphidp
!     real(kind=kind_evod),dimension(i_1:i_2,plev):: ys,yn,ump,vmp,wmp
      real(kind=kind_evod),dimension(i_1:i_2,plev)::       ump,vmp,wmp
     &,                                              upr,vpr,lampr,phipr
     &,                                              sigpr
!     real(kind=kind_evod),dimension(i_1:i_2,plev,2:3):: xl
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4):: xl,xr
      parameter ( fac = 1. - 1.e-12 )

!
      twopi   = pi  + pi
      pi2     = 0.5 * pi
      hfdt    = 0.5 * dt
      phicen  = phib(jcen)
      cphic   = cos( phicen )
      sphic   = sin( phicen )
      cphic2i = 1.0 / (cphic*cphic)
!
      do k = 1,plev
        do i = i_1,i_2
          sphisc     = sin( phimp_a(i,k) )
          cphisc     = cos( phimp_a(i,k) )
          clamsc     = cos( lam(i+1,jcen) - lammp_a(i,k) )
          phipr(i,k) = asin( sphisc*cphic - cphisc*sphic*clamsc )
        end do
      end do
!
      dphibr = 1./( phib(platd/2+1) - phib(platd/2) )
      phibs  = phib(1)

      do iter=1,iterdp

!my compute jdp first since rdlam is now lat dependent 

        do k = 1,plev

          do i=i_1,i_2
            jtem = int ( (phimp_a(i,k) - phibs)*dphibr + 1. )

! commented by moorthi on 10/18/2011
!        if (phimp_a(i,k) < -900.0) then
!        write(999,*) 
!    .   'inside dep3dg i,k,phimp_a(i,k),phibs,dphibr,jdp = ',
!    .      i,k,phimp_a(i,k),phibs,dphibr,jdp(i,k)
!         close(999)
!         stop 999
!        endif

            if( phimp_a(i,k) >= phib(jtem+1) ) jtem = jtem + 1

            jdp(i,k) = jtem
            jm1 = max(1,jtem-1)
            jp1 = min(platd,jtem+1)
            jp2 = min(platd,jtem+2)

            idp(i,k,2) = 2 + int( lammp_a(i,k) * rdlam(jtem) )
            if (redgg_a) then
              idp(i,k,1) = 2 + int( lammp_a(i,k) * rdlam(jm1) )
              idp(i,k,3) = 2 + int( lammp_a(i,k) * rdlam(jp1) )
              idp(i,k,4) = 2 + int( lammp_a(i,k) * rdlam(jp2) )
            else
              idp(i,k,1) = idp(i,k,2)
              idp(i,k,3) = idp(i,k,2)
              idp(i,k,4) = idp(i,k,2)
            endif
!
            kdp(i,k) = bisection(kdpmpf,plev-1,
     &                 int((sigmp_a(i,k) - etamid(1))*rdel + 1. ))
            if(sigmp_a(i,k) >= etamid(kdp(i,k)+1)) then
              kdp(i,k) = kdp(i,k) + 1
            end if
          enddo
        enddo
!
!       call xywgts_h(i_1,i_2,lam,phib,dphib,idp,jdp,
!    &                lammp_a,phimp_a,xl,ys)
!!   &                lammp_a,phimp_a,xl,xr,ys,yn)
!
        call int3dv_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                ub,vb,etamid,detami,sigmp_a,idp,jdp,kdp,
     &                lam, phib, dphibi, lammp_a,phimp_a,
     &                ump,vmp,wb,wmp)
!    &                xl,xr,ys,yn,ump,vmp,wb,wmp)
!sela&                wb,wmp,etaint,detai,kdph)
!
        do k = 1,plev
          do i = i_1,i_2
            ump(i,k) = ump(i,k)*ra
            vmp(i,k) = vmp(i,k)*ra
!
            sdlam  = sin( lam(i+1,jcen) - lammp_a(i,k) )
            cdlam  = cos( lam(i+1,jcen) - lammp_a(i,k) )
            sphimp = sin( phimp_a(i,k) )
            cphimp = cos( phimp_a(i,k) )
            sphip  = sphimp*cphic - cphimp*sphic*cdlam
            cphip  = cos( asin( sphip ) )
            slamp  = -sdlam*cphimp/cphip
            clamp  = cos( asin( slamp ) )
            vpr(i,k) = (vmp(i,k)*(cphimp*cphic + sphimp*sphic*cdlam) -
     &                  ump(i,k)*sphic*sdlam)/cphip
            upr(i,k) = (ump(i,k)*cdlam + vmp(i,k)*sphimp*sdlam +
     &                  vpr(i,k)*slamp*sphip)/clamp
!
            lampr(i,k) = -hfdt* upr(i,k) / cos( phipr(i,k) )
            phipr(i,k) = -hfdt* vpr(i,k)
          enddo
        enddo

        coeff  = (1.1*hfdt)**2
        distmx = (sign(pi2,phicen) - phicen)**2/coeff
        sgnphi = sign( 1., phicen )
!
        do k=1,plev
          do i=i_1,i_2
            sphipr      = sin( phipr(i,k) )
            cphipr      = cos( phipr(i,k) )
            slampr      = sin( lampr(i,k) )
            clampr      = cos( lampr(i,k) )
            phimp_a(i,k)=asin((sphipr*cphic + cphipr*sphic*clampr)*fac)
            if ( abs(phimp_a(i,k)) >= phib(3+plat)*fac )
     &        phimp_a(i,k) = sign( phib(3+plat),phimp_a(i,k) )*fac
            dlamx = asin((slampr*cphipr/cos(phimp_a(i,k)))*fac)
            dist  = upr(i,k)*upr(i,k) + vpr(i,k)*vpr(i,k)
!
            if (dist > distmx) then
              slam2  = slampr*slampr
              phipi2 = asin((sqrt((slam2-1.)/(slam2-cphic2i)))*fac)
              if (sgnphi*phipr(i,k) > phipi2) then
                dlamx = sign(pi,lampr(i,k)) - dlamx
              end if
            end if
!
            lammp_a(i,k) = lam(i+1,jcen) + dlamx
            if( lammp_a(i,k) >= twopi ) then
              lammp_a(i,k) = lammp_a(i,k) - twopi
            elseif( lammp_a(i,k) <  0.0 ) then
              lammp_a(i,k) = lammp_a(i,k) + twopi
            endif
!
            sigpr(i,k)   =  -hfdt*wmp(i,k)
            sigmp_a(i,k) = etamid(k) + sigpr(i,k)
!
            sigmp_a(i,k) = max(etamid(1),
     &                     min(sigmp_a(i,k),etamid(plev)-mprec))
          enddo
        enddo
      enddo          ! do iter=1,iterdp ends here
!
      do k  = 1,plev
         do i  = i_1,i_2
            lampr(i,k) = lampr(i,k) + lampr(i,k)
            phipr(i,k) = phipr(i,k) + phipr(i,k)
            sigdp(i,k) = etamid(k) + sigpr(i,k) + sigpr(i,k)
         end do
      end do

      coeff  = (1.1*dt)**2
      distmx = (sign(pi2,phicen) - phicen)**2/coeff
      sgnphi = sign( 1., phicen )
!
      do k=1,plev
        kk = plev+1-k
        do i=i_1,i_2
          sphipr     = sin( phipr(i,k) )
          cphipr     = cos( phipr(i,k) )
          slampr     = sin( lampr(i,k) )
          clampr     = cos( lampr(i,k) )
          phidp(i,k) = asin((sphipr*cphic + cphipr*sphic*clampr)*fac)
          if ( abs(phidp(i,k)) >= phib(3+plat)*fac )
     &    phidp(i,k) = sign( phib(3+plat),phidp(i,k) )*fac

          cosphidp   = cos(phidp(i,k))

          cos_f_d(i,kk) = cosphidp        ! for vector allignment
          sin_f_d(i,kk) = sin(phidp(i,k)) ! for vector allignment
 
          dlamx = asin((slampr*cphipr/cosphidp)*fac)
          dist  = upr(i,k)*upr(i,k) + vpr(i,k)*vpr(i,k)
!
          if (dist > distmx) then
            slam2 = slampr*slampr
            phipi2 = asin((sqrt((slam2-1.)/(slam2-cphic2i)))*fac)
            if (sgnphi*phipr(i,k) > phipi2) then
              dlamx = sign(pi,lampr(i,k)) - dlamx
            end if
          end if
!
          lamdp(i,k) = lam(i+1,jcen) + dlamx
          if( lamdp(i,k) >= twopi ) then
            lamdp(i,k) = lamdp(i,k) - twopi
          elseif( lamdp(i,k) < 0.0  ) then
            lamdp(i,k) = lamdp(i,k) + twopi
          endif

          cos_l_d(i,kk) = cos(lamdp(i,k)) ! for vector allignment
          sin_l_d(i,kk) = sin(lamdp(i,k)) ! for vector allignment
!

          sigdp(i,k) = max(etamid(1),
     &                 min(sigdp(i,k),etamid(plev)-mprec))
        enddo
      enddo
      return
      end
      subroutine dep3ds_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                    jcen,dt,iterdp,ra,ub,vb,wb,
     &                    lam,phib,dphibi,etamid,detami,
     &                    kdpmpf,lammp_a,phimp_a,sigmp_a,
     &                    lamdp,phidp,sigdp,me,
     &                    sin_l_d,cos_l_d,sin_f_d,cos_f_d,rdel)

      use machine            , only : kind_evod
      use      pmgrid        , only : mprec,platd,plev,
     &                                plon,plond,pi
      use slgshr             , only : dlam,rdlam
      use namelist_def       , only : redgg_a

      implicit none

!  input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer jcen,iterdp,me
      integer,dimension(plev-1) :: kdpmpf   ! artificial vert grid indic
      real(kind=kind_evod) dt,ra,rdel
!     real(kind=kind_evod) dt,ra,rdel,pi
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                            :: ub,vb,wb
      real(kind=kind_evod),dimension(plev)        :: detami
      real(kind=kind_evod),dimension(plev)        :: etamid    ! eta at levels
      real(kind=kind_evod),dimension(plond,platd) :: lam
      real(kind=kind_evod),dimension(platd)       :: phib,dphibi
      real(kind=kind_evod),dimension(plon,plev)   :: lammp_a,phimp_a,
     &                                               sigmp_a
      real(kind=kind_evod),dimension(i_1:i_2,plev)::
     &     lamdp,phidp,sigdp,sin_l_d,cos_l_d,sin_f_d,cos_f_d

!  local variables
      integer iter,i,k,kk,bisection,jtem,jm1,jp1,jp2
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp
      real(kind=kind_evod)              :: phibs,dphibr
      real(kind=kind_evod)              :: phicen,twopi,hfdt
!     real(kind=kind_evod),dimension(i_1:i_2,plev)  :: ys,yn,ump,vmp,wmp
      real(kind=kind_evod),dimension(i_1:i_2,plev)  ::       ump,vmp,wmp
     &,                                                lampr,phipr,sigpr
!     real(kind=kind_evod),dimension(i_1:i_2,plev,2:3):: xl
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4):: xl,xr

      twopi  = pi  + pi
      hfdt   = 0.5 * dt
      phicen = phib(jcen)
!
      dphibr = 1./( phib(platd/2+1) - phib(platd/2) )
      phibs  = phib(1)
!
      do iter=1,iterdp

!my compute jdp first since rdlam is now lat dependent 

        do k = 1,plev
          do i=i_1,i_2
            jtem = int ( (phimp_a(i,k) - phibs)*dphibr + 1. )

! commented by moorthi on 10/18/2011
!        if (phimp_a(i,k) < -900.0) then
!        write(999,*) 
!    .   'inside dep3ds i,k,phimp_a(i,k),phibs,dphibr,jdp = ',
!    .      i,k,phimp_a(i,k),phibs,dphibr,jdp(i,k)
!         close(999)
!         stop 999
!        endif

!$$$      print*,'inside dep3ds jcen,i,k,phimp_a(i,k),phibs,dphibr,jdp = ',
!$$$     .      jcen,i,k,phimp_a(i,k),phibs,dphibr,jdp(i,k)

            if( phimp_a(i,k) >= phib(jtem+1) ) jtem = jtem + 1

            jdp(i,k) = jtem
            jm1 = max(1,jtem-1)
            jp1 = min(platd,jtem+1)
            jp2 = min(platd,jtem+2)

!111   format('jcen=',i5,' i=',i4,2x,' k=',i3,2x,' jdp-jcen=',i6)

            idp(i,k,2) = 2 + int( lammp_a(i,k) * rdlam(jtem) )
            if (redgg_a) then
               idp(i,k,1) = 2 + int( lammp_a(i,k) * rdlam(jm1) )
               idp(i,k,3) = 2 + int( lammp_a(i,k) * rdlam(jp1) )
               idp(i,k,4) = 2 + int( lammp_a(i,k) * rdlam(jp2) )
            else
               idp(i,k,1) = idp(i,k,2)
               idp(i,k,3) = idp(i,k,2)
               idp(i,k,4) = idp(i,k,2)
            endif
!

            kdp(i,k) = bisection(kdpmpf,plev-1,
     &                 int((sigmp_a(i,k) - etamid(1))*rdel + 1. ))
            if(sigmp_a(i,k) >= etamid(kdp(i,k)+1)) then
              kdp(i,k) = kdp(i,k) + 1
            end if
          enddo
        enddo

!       call xywgts_h(i_1,i_2,lam,phib,dphib,idp,jdp,
!    &                lammp_a,phimp_a,xl,ys)
!    &                lammp_a,phimp_a,xl,xr,ys,yn)

        call int3dv_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                ub,vb,etamid,detami,sigmp_a,idp,jdp,kdp,
     &                lam, phib, dphibi, lammp_a,phimp_a,
     &                ump,vmp,wb,wmp)
!    &                xl,xr,ys,yn,ump,vmp,wb,wmp)
!sela&                wb,wmp,etaint,detai,kdph)

        do k = 1,plev
          do i = i_1,i_2
            ump(i,k) = ump(i,k)*ra
            vmp(i,k) = vmp(i,k)*ra
!
            lampr(i,k)   =  -hfdt*ump(i,k) / cos(phimp_a(i,k))
            phipr(i,k)   =  -hfdt*vmp(i,k)
            sigpr(i,k)   =  -hfdt*wmp(i,k)
            lammp_a(i,k) = lam(i+1,jcen) + lampr(i,k)
            phimp_a(i,k) = phicen        + phipr(i,k)
            sigmp_a(i,k) = etamid(k)     + sigpr(i,k)
!
            if(lammp_a(i,k) >= twopi) then
              lammp_a(i,k) = lammp_a(i,k) - twopi
            elseif(lammp_a(i,k) <    0.0) then
              lammp_a(i,k) = lammp_a(i,k) + twopi
            endif
!
            sigmp_a(i,k) = max(etamid(1),
     &                     min(sigmp_a(i,k),etamid(plev)-mprec))
          enddo
        enddo
      enddo                                           !do iter=1,iterdp loop

      do k  = 1,plev
        kk = plev+1-k
        do i=i_1,i_2
          lamdp(i,k) = lam(i+1,jcen) + lampr(i,k) + lampr(i,k)
          phidp(i,k) = phicen        + phipr(i,k) + phipr(i,k)
          sigdp(i,k) = etamid(k)     + sigpr(i,k) + sigpr(i,k)

          if(lamdp(i,k) >= twopi) then
            lamdp(i,k) = lamdp(i,k) - twopi
          elseif(lamdp(i,k) <   0.0) then
            lamdp(i,k) = lamdp(i,k) + twopi
          endif

          cos_l_d(i,kk) = cos(lamdp(i,k))           ! for vector allignment
          sin_l_d(i,kk) = sin(lamdp(i,k))           ! for vector allignment

          cos_f_d(i,kk) = cos(phidp(i,k))           ! for vector allignment
          sin_f_d(i,kk) = sin(phidp(i,k))           ! for vector allignment

          sigdp(i,k) = max(etamid(1),
     &                 min(sigdp(i,k),etamid(plev)-mprec))
        enddo
      enddo
      return
      end
      subroutine settls_dep3dg_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                           jcen,dt,iterdp,ra,ub,vb,wb,
     &                           lam,phib,dphibi,etamid,detami,
     &                           kdpmpf,lamdp_a,phidp_a,sigdp_a,
     &                           lamdp,phidp,sigdp,me,
     &                           sin_l_d,cos_l_d,sin_f_d,cos_f_d,
     &                           un,vn,wn,rdel)
!     sk 06142012
!     settls for trajectory computations
!     ub,vb,wb -> time t+dt, time-extrapolated wind components
!     un,vn,wn -> time t, grid-point wind components
!     lamdp_a,phidp_a,sigdp_a -> iterated values of lamda,phi,sig at dp
!     code adapted from subroutine dep3dg_h

      use machine            , only : kind_evod
      use      pmgrid        , only : mprec,plat,platd,
     &                                plev,plon,plond,pi
      use slgshr             , only : dlam,rdlam
      use namelist_def       , only : redgg_a,iter_one_no_interp
!
      implicit none

!   input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer jcen,iterdp,me
      integer kdpmpf(plev-1)   ! artificial vert grid indic
      real dt,ra,rdel
!     real dt,ra,rdel,pi
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                              ::      ub,vb,wb,un,vn,wn
      real(kind=kind_evod),dimension(plev)        :: detami
      real(kind=kind_evod),dimension(plev)        :: etamid    ! eta at levels
      real(kind=kind_evod),dimension(plond,platd) :: lam
      real(kind=kind_evod),dimension(platd)       :: phib,dphibi
      real(kind=kind_evod),dimension(plon,plev)   :: lamdp_a,phidp_a,
     &                                               sigdp_a
      real(kind=kind_evod),dimension(i_1:i_2,plev)::
     &     lamdp,phidp,sigdp,sin_l_d,cos_l_d,sin_f_d,cos_f_d

!   local variables
      integer iter,i,k,kk,ii,bisection,jtem,jm1,jp1,jp2
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod) sphisc,cphisc,clamsc,phibs,dphibr,upr,vpr
     &,                    twopi,pi2,sgnphi,sphipr,cphipr,clampr,cphic2i
     &,                    slam2,phipi2,slampr,dlamx,coeff,distmx,dist
     &,                    lampr,cdlam,clamp,cphidp,cphip,sdlam,slamp
     &,                    sphidp,sphip,hfdt,phicen,cphic,sphic

!     real(kind=kind_evod),dimension(i_1:i_2,plev) :: ys,yn,udp,vdp,wdp,
      real(kind=kind_evod),dimension(i_1:i_2,plev) ::       udp,vdp,wdp,
     &                                                phipr,sigpr
!     real(kind=kind_evod),dimension(i_1:i_2,plev,2:3):: xl
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4):: xl,xr
      real(kind=kind_evod), parameter :: fac = 1.0-1.e-12
!
      twopi   = pi  + pi
      pi2     = 0.5 * pi
      hfdt    = 0.5 * dt
      phicen  = phib(jcen)
      cphic   = cos( phicen )
      sphic   = sin( phicen )
      cphic2i = 1.0 / (cphic*cphic)
      dphibr  = 1./( phib(platd/2+1) - phib(platd/2) )
      phibs   = phib(1)
!
      coeff  = (1.1*hfdt)**2
      distmx = (sign(pi2,phicen) - phicen)**2/coeff
      sgnphi = sign( 1., phicen )
!
      do k = 1,plev
         do i = i_1,i_2
            sphisc = sin( phidp_a(i,k) )
            cphisc = cos( phidp_a(i,k) )
            clamsc = cos( lam(i+1,jcen) - lamdp_a(i,k) )
            phipr(i,k) = asin( sphisc*cphic - cphisc*sphic*clamsc )
         end do
      end do
!
      settls_loop: do iter=1,iterdp
        if ((iter == 1) .and. iter_one_no_interp) then
          do k = 1,plev
            do i = i_1,i_2
              udp(i,k) = ub(i+1,k,jcen)
              vdp(i,k) = vb(i+1,k,jcen)
              wdp(i,k) = wb(i+1,k,jcen)
            enddo
          enddo
        else
          do k = 1,plev
            do i=i_1,i_2
              jtem = int ( (phidp_a(i,k) - phibs)*dphibr + 1. )
              if( phidp_a(i,k) >= phib(jtem+1) ) jtem = jtem + 1
              jdp(i,k) = jtem
              jm1 = max(1,jtem-1)
              jp1 = min(platd,jtem+1)
              jp2 = min(platd,jtem+2)
!
              idp(i,k,2) = 2 + int( lamdp_a(i,k) * rdlam(jtem) )
              if (redgg_a) then
                 idp(i,k,1) = 2 + int( lamdp_a(i,k) * rdlam(jm1) )
                 idp(i,k,3) = 2 + int( lamdp_a(i,k) * rdlam(jp1) )
                 idp(i,k,4) = 2 + int( lamdp_a(i,k) * rdlam(jp2) )
              else
                 idp(i,k,1) = idp(i,k,2)
                 idp(i,k,3) = idp(i,k,2)
                 idp(i,k,4) = idp(i,k,2)
              endif
!
              kdp(i,k) = bisection(kdpmpf,plev-1,
     &                   int((sigdp_a(i,k) - etamid(1))*rdel + 1. ))
              if(sigdp_a(i,k) >= etamid(kdp(i,k)+1)) then
                kdp(i,k) = kdp(i,k) + 1
              end if
            enddo
          enddo
!
!         call xywgts_h(i_1,i_2,lam,phib,dphib,idp,jdp,lamdp_a,phidp_a,
!    &                  xl,ys)
!!   &                  xl,xr,ys,yn)
          call int3dv_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                  ub,vb,etamid,detami,sigdp_a,idp,jdp,kdp,
     &                  lam, phib, dphibi, lamdp_a, phidp_a,
     &                  udp,vdp,wb,wdp)
!    &                  xl,xr,ys,yn,udp,vdp,wb,wdp)
        endif
!
        do k = 1,plev
          do i = i_1,i_2
            udp(i,k) = udp(i,k)*ra
            vdp(i,k) = vdp(i,k)*ra
 
            sdlam  = sin( lam(i+1,jcen) - lamdp_a(i,k) )
            cdlam  = cos( lam(i+1,jcen) - lamdp_a(i,k) )
            sphidp = sin( phidp_a(i,k) )
            cphidp = cos( phidp_a(i,k) )
            sphip  = sphidp*cphic - cphidp*sphic*cdlam
            cphip  = cos( asin( sphip ) )
            slamp  = -sdlam*cphidp/cphip
            clamp  = cos( asin( slamp ) )

            vpr    = (vdp(i,k)*(cphidp*cphic + sphidp*sphic*cdlam) -
     &                udp(i,k)*sphic*sdlam)/cphip

            upr    = (udp(i,k)*cdlam + vdp(i,k)*sphidp*sdlam +
     &                vpr*slamp*sphip)/clamp
 
            lampr  = -hfdt*( upr/cos(phipr(i,k))+un(i+1,k,jcen)*ra )

            phipr(i,k)   = -hfdt * (vpr + vn(i+1,k,jcen)*ra)

            sphipr       = sin( phipr(i,k) )
            cphipr       = cos( phipr(i,k) )
            slampr       = sin( lampr )
            clampr       = cos( lampr )

            phidp_a(i,k) = asin((sphipr*cphic+cphipr*sphic*clampr)*fac)
            if ( abs(phidp_a(i,k)) >= phib(3+plat)*fac )
     &      phidp_a(i,k) = sign( phib(3+plat),phidp_a(i,k) )*fac

            dlamx  = asin((slampr*cphipr/cos(phidp_a(i,k)))*fac)
            dist   = upr*upr + vpr*vpr
!
            if (dist > distmx) then
              slam2  = slampr*slampr
              phipi2 = asin((sqrt((slam2-1.)/(slam2-cphic2i)))*fac)
              if (sgnphi*phipr(i,k) > phipi2) then
                dlamx = sign(pi,lampr) - dlamx
              endif
            endif
!
            lamdp_a(i,k) = lam(i+1,jcen) + dlamx
            if( lamdp_a(i,k) >= twopi ) then
              lamdp_a(i,k) = lamdp_a(i,k) - twopi
            elseif( lamdp_a(i,k) <  0.0 ) then
              lamdp_a(i,k) = lamdp_a(i,k) + twopi
            endif
!
            sigpr(i,k)   =  -hfdt * (wdp(i,k) + wn(i+1,k,jcen))
            sigdp_a(i,k) = etamid(k) + sigpr(i,k)
            sigdp_a(i,k) = max(etamid(1),
     &                     min(sigdp_a(i,k),etamid(plev)-mprec))
          enddo
        enddo
      enddo settls_loop 
!
      do k  = 1,plev
        kk = plev+1-k
        do i=i_1,i_2
           lamdp(i,k) = lamdp_a(i,k)
           phidp(i,k) = phidp_a(i,k)
           sigdp(i,k) = sigdp_a(i,k)
           cos_l_d(i,kk) = cos(lamdp(i,k)) ! for vector allignment
           sin_l_d(i,kk) = sin(lamdp(i,k)) ! for vector allignment
           cos_f_d(i,kk) = cos(phidp(i,k)) ! for vector allignment
           sin_f_d(i,kk) = sin(phidp(i,k)) ! for vector allignment
        enddo
      enddo
      return
      end subroutine

      subroutine settls_dep3ds_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                           jcen,dt,iterdp,ra,ub,vb,wb,
     &                           lam,phib,dphibi,etamid,detami,
     &                           kdpmpf,lamdp_a,phidp_a,sigdp_a,
     &                           lamdp,phidp,sigdp,me,
     &                           sin_l_d,cos_l_d,sin_f_d,cos_f_d,
     &                           un,vn,wn,rdel)
!     sk 06122012
!     settls for trajectory computations
!     ub,vb,wb -> time t+dt, time-extrapolated wind components
!     un,vn,wn -> time t, grid-point wind components
!     lamdp_a,phidp_a,sigdp_a -> iterated values of lamda,phi,sig at dp
!     code adapted from subroutine dep3ds_h

      use machine            , only : kind_evod
      use      pmgrid        , only : mprec,platd,plev,
     &                                plon,plond,pi
      use slgshr             , only : dlam,rdlam
      use namelist_def       , only : redgg_a,iter_one_no_interp
      implicit none

!   input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,jcen,iterdp,me
      integer,dimension(plev-1) :: kdpmpf   ! artificial vert grid indic

      real dt,ra,rdel
!     real dt,ra,rdel,pi
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                             ::    ub,vb,wb,un
     &,                                                  vn,wn
      real(kind=kind_evod),dimension(plev)         :: detami
     &,                                               etamid        ! eta at levels
      real(kind=kind_evod),dimension(plond,platd)  :: lam
      real(kind=kind_evod),dimension(platd)        :: phib,dphibi
      real(kind=kind_evod),dimension(plon,plev)    :: lamdp_a,phidp_a
     & ,                                              sigdp_a
      real(kind=kind_evod),dimension(i_1:i_2,plev) ::
     &     lamdp,phidp,sigdp,sin_l_d,cos_l_d,sin_f_d,cos_f_d

!   local variables
      integer iter,i,k,kk,ii,bisection,jtem,jm1,jp1,jp2
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod) :: phicen,lampr,phipr,sigpr,twopi,hfdt
     &,                       dphibr,phibs,cphici
!     real(kind=kind_evod),dimension(i_1:i_2,plev)  :: ys,yn,udp,vdp,wdp
      real(kind=kind_evod),dimension(i_1:i_2,plev)  ::       udp,vdp,wdp
!     real(kind=kind_evod),dimension(i_1:i_2,plev,2:3):: xl
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4):: xl,xr

      twopi  = pi  + pi
      hfdt   = 0.5 * dt
      phicen = phib(jcen)
      cphici = 1.0 / cos(phicen)
!
      dphibr = 1./( phib(platd/2+1) - phib(platd/2) )
      phibs  = phib(1)

!
      settls_loop: do iter=1,iterdp
        if ((iter == 1) .and. iter_one_no_interp) then
          do k = 1,plev
            do i = i_1,i_2
              udp(i,k) = ub(i+1,k,jcen)
              vdp(i,k) = vb(i+1,k,jcen)
              wdp(i,k) = wb(i+1,k,jcen)
            enddo
          enddo
        else
          do k = 1,plev
            do i=i_1,i_2
              jtem = int ( (phidp_a(i,k) - phibs)*dphibr + 1. )
              if( phidp_a(i,k) >= phib(jtem+1) ) jtem = jtem + 1
              jdp(i,k) = jtem
              jm1 = max(1,jtem-1)
              jp1 = min(platd,jtem+1)
              jp2 = min(platd,jtem+2)
!
              idp(i,k,2) = 2 + int( lamdp_a(i,k) * rdlam(jtem) )
              if (redgg_a) then
                 idp(i,k,1) = 2 + int( lamdp_a(i,k) * rdlam(jm1) )
                 idp(i,k,3) = 2 + int( lamdp_a(i,k) * rdlam(jp1) )
                 idp(i,k,4) = 2 + int( lamdp_a(i,k) * rdlam(jp2) )
              else
                 idp(i,k,1) = idp(i,k,2)
                 idp(i,k,3) = idp(i,k,2)
                 idp(i,k,4) = idp(i,k,2)
              endif
!
              kdp(i,k) = bisection(kdpmpf,plev-1,
     &                   int((sigdp_a(i,k) - etamid(1))*rdel + 1. ))
              if(sigdp_a(i,k) >= etamid(kdp(i,k)+1)) then
                kdp(i,k) = kdp(i,k) + 1
              end if
            enddo
          enddo
!
!         call xywgts_h(i_1,i_2,lam,phib,dphib,idp,jdp,
!    &                  lamdp_a,phidp_a,xl,ys)
!!   &                  lamdp_a,phidp_a,xl,xr,ys,yn)
          call int3dv_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                  ub,vb,etamid,detami,sigdp_a,idp,jdp,kdp,
     &                  lam, phib, dphibi, lamdp_a, phidp_a,
     &                  udp,vdp,wb,wdp)
!    &                  xl,xr,ys,yn,udp,vdp,wb,wdp)
        endif
!
        do k = 1,plev
          do i = i_1,i_2
            udp(i,k) = udp(i,k)*ra
            vdp(i,k) = vdp(i,k)*ra
!
            lampr =  -hfdt * (udp(i,k)/cos(phidp_a(i,k))
     &                     +  un(i+1,k,jcen)*ra*cphici)
            phipr =  -hfdt * (vdp(i,k) + vn(i+1,k,jcen)*ra)
            sigpr =  -hfdt * (wdp(i,k) + wn(i+1,k,jcen))

            lamdp_a(i,k) = lam(i+1,jcen) + lampr
            phidp_a(i,k) = phicen        + phipr
            sigdp_a(i,k) = etamid(k)     + sigpr

            if(lamdp_a(i,k) >= twopi) then
              lamdp_a(i,k) = lamdp_a(i,k) - twopi
            elseif(lamdp_a(i,k) <    0.0) then
              lamdp_a(i,k) = lamdp_a(i,k) + twopi
            endif
!
            sigdp_a(i,k) = max(etamid(1),
     &                     min(sigdp_a(i,k),etamid(plev)-mprec))
          enddo
        enddo
      enddo settls_loop
      do k  = 1,plev
         kk = plev+1-k
         do i=i_1,i_2
            lamdp(i,k) = lamdp_a(i,k)
            phidp(i,k) = phidp_a(i,k)
            sigdp(i,k) = sigdp_a(i,k)

            cos_l_d(i,kk) = cos(lamdp(i,k)) ! for vector allignment
            sin_l_d(i,kk) = sin(lamdp(i,k)) ! for vector allignment
            cos_f_d(i,kk) = cos(phidp(i,k)) ! for vector allignment
            sin_f_d(i,kk) = sin(phidp(i,k)) ! for vector allignment
        enddo
      enddo
      return
      end
      subroutine herxinit_h(i_1,i_2,idp,jdp,lamdp,xl,xr,hl,hr,
     &                      dhl,dhr)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,platd
      use slgshr             , only : lam,dlam,rdlam
      implicit none
!
      integer i_1,i_2
      integer,dimension(i_1:i_2,plev)   :: jdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(i_1:i_2,plev)     ::  lamdp     ! x-coord of dep pt
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   ::  xl,xr
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) ::  hl,hr,dhl,dhr

!   local variables

      integer i,k,n,j,jj
      real    teml, temr, tem1, tem2
!
      do k=1,plev
!     write(0,*)' in herx k=',k,' i_1=',i_1,'i_2=',i_2
        do  i=i_1,i_2
          j  = jdp(i,k) - 2
!                                                      n=1
          jj = min(j+1,platd)
          xl(i,k,1) = (lam(idp(i,k,1)+1,jj) - lamdp(i,k))*
     &                 rdlam(jj)
          xr(i,k,1) = 1. - xl(i,k,1)
!                                                      n=2
          jj = min(j+2,platd)
          teml = (lam(idp(i,k,2)+1,jj) - lamdp(i,k)) * rdlam(jj)
          temr = 1.0 - teml
          xl(i,k,2) = teml
          xr(i,k,2) = temr
!
          tem1 = teml * teml
          tem2 = temr * temr
          hl (i,k,2) = ( 3.0 - teml - teml ) * tem1
          hr (i,k,2) = ( 3.0 - temr - temr ) * tem2

          dhl(i,k,2) =   dlam(jj) * temr * tem1
          dhr(i,k,2) = - dlam(jj) * teml * tem2
!                                                      n=3

          jj = min(j+3,platd)
          teml = (lam(idp(i,k,3)+1,jj) - lamdp(i,k)) * rdlam(jj)
          temr = 1.0 - teml
          xl(i,k,3) = teml
          xr(i,k,3) = temr
!
          tem1 = teml * teml
          tem2 = temr * temr
          hl (i,k,3) = ( 3.0 - teml - teml ) * tem1
          hr (i,k,3) = ( 3.0 - temr - temr ) * tem2

          dhl(i,k,3) =   dlam(jj) * temr * tem1
          dhr(i,k,3) = - dlam(jj) * teml * tem2
!                                                      n=4
          jj = min(j+4,platd)
          xl(i,k,4) = (lam(idp(i,k,4)+1,jj) - lamdp(i,k))*
     &                 rdlam(jj)
          xr(i,k,4) = 1. - xl(i,k,4)
        end do
      end do

      return
      end
      subroutine int2d_h_levs(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                        kdim,fb1,idp,jdp,
     &                        x2,x3,term1x,term2x,term3x,term4x,
     &                        term1y,term2y,term3y,term4y, fdp1)
!
      use     machine        , only : kind_evod
      use      pmgrid        , only : plev,plon,plond
      implicit none

!   input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim
      integer,dimension(i_1:i_2,plev)   :: jdp
      integer,dimension(i_1:i_2,plev,4) :: idp
      real(kind=kind_evod),dimension(plond,kdim,s_lat_in_pe:n_lat_in_pe)
     &                                                 :: fb1
      real(kind=kind_evod),dimension(plon,plev)        :: fdp1
      real(kind=kind_evod),dimension(i_1:i_2,plev)     :: term1y,term2y,
     &                                                    term3y,term4y
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: x2,x3
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: term1x,term2x,
     &                                                    term3x,term4x

!   local variables

      integer i,k, kk, ii1, ii2, ii3, ii4, jj, jjm1, jjp1, jjp2
     &,       ii1p1, ii2m1, ii2p1, ii2p2, ii3m1, ii3p1, ii3p2, ii4p1

!     real(kind=kind_evod) :: f1,f2,f3,f4

!
      do k=1,plev
        kk = plev+1-k
        do i=i_1,i_2
          ii1   = idp(i,k,1)
          ii1p1 = ii1 + 1
          ii2   = idp(i,k,2)
          ii2m1 = ii2 - 1
          ii2p1 = ii2 + 1
          ii2p2 = ii2 + 2
          ii3   = idp(i,k,3)
          ii3m1 = ii3 - 1
          ii3p1 = ii3 + 1
          ii3p2 = ii3 + 2
          ii4   = idp(i,k,4)
          ii4p1 = ii4 + 1

          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjm1  = max(jj-1,s_lat_in_pe)
          jjp1  = min(jj+1,n_lat_in_pe)
          jjp2  = min(jj+2,n_lat_in_pe)

          fdp1(i,kk) =
     &               (fb1 (ii1p1,k,jjm1) * x2    (i,k,1)
     &              - fb1 (ii1  ,k,jjm1) * x3    (i,k,1)) * term1y(i,k)

     &            +  (fb1 (ii2m1,k,jj  ) * term1x(i,k,2)
     &              + fb1 (ii2  ,k,jj  ) * term2x(i,k,2)
     &              + fb1 (ii2p1,k,jj  ) * term3x(i,k,2)
     &              + fb1 (ii2p2,k,jj  ) * term4x(i,k,2)) * term2y(i,k)

     &            +  (fb1 (ii3m1,k,jjp1) * term1x(i,k,3)
     &              + fb1 (ii3  ,k,jjp1) * term2x(i,k,3)
     &              + fb1 (ii3p1,k,jjp1) * term3x(i,k,3)
     &              + fb1 (ii3p2,k,jjp1) * term4x(i,k,3)) * term3y(i,k)

     &            +  (fb1 (ii4p1,k,jjp2) * x2    (i,k,4)
     &              - fb1 (ii4  ,k,jjp2) * x3    (i,k,4)) * term4y(i,k)
!
!         f1 = fb1 (ii1+1,k,jj-1) * x2    (i,k,1)
!    &       - fb1 (ii1  ,k,jj-1) * x3    (i,k,1)
!         f2 = fb1 (ii2-1,k,jj  ) * term1x(i,k,2)
!    &       + fb1 (ii2  ,k,jj  ) * term2x(i,k,2)
!    &       + fb1 (ii2+1,k,jj  ) * term3x(i,k,2)
!    &       + fb1 (ii2+2,k,jj  ) * term4x(i,k,2)
!         f3 = fb1 (ii3-1,k,jj+1) * term1x(i,k,3)
!    &       + fb1 (ii3  ,k,jj+1) * term2x(i,k,3)
!    &       + fb1 (ii3+1,k,jj+1) * term3x(i,k,3)
!    &       + fb1 (ii3+2,k,jj+1) * term4x(i,k,3)
!         f4 = fb1 (ii4+1,k,jj+2) * x2    (i,k,4)
!    &       - fb1 (ii4  ,k,jj+2) * x3    (i,k,4)
!
!         fdp1(i,kk) = f1*term1y(i,k) + f2*term2y(i,k)
!    &               + f3*term3y(i,k) + f4*term4y(i,k)
        end do
      end do
      return
      end
      subroutine int2d_h_levs_mxy(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                            kdim,fb1,idp,jdp,
     &                            x2,x3,term1x,term2x,term3x,term4x,
     &                            term1y,term2y,term3y,term4y,fdp1)
!
!sk 10/13/2012
!sk modified original subroutine int2d_d_levs for quasi-monotone
!sk quasi-cubic 2d lagrange interpolation in x and y, and renamed
!sk as int2d_h_mxy
!sk
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plon,plond,quamon
      implicit none
!
!   input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim
      integer,dimension(i_1:i_2,plev)   :: jdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(plond,kdim,s_lat_in_pe:n_lat_in_pe)
     &                                                 :: fb1
      real(kind=kind_evod),dimension(plon,plev)        :: fdp1
      real(kind=kind_evod),dimension(i_1:i_2,plev)     :: term1y,term2y,
     &                                                    term3y,term4y
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: x2,x3
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: term1x,term2x,
     &                                                    term3x,term4x

!   local variables

      integer i,k, kk, ii1, ii2, ii3, ii4, jj, jjm1, jjp1, jjp2
     &,       ii1p1, ii4p1, ii2m1, ii2p1, ii2p2, ii3m1, ii3p1, ii3p2
      real(kind=kind_evod) :: f1,f2,f3,f4,tem1,tem2
!
      do k=1,plev
        kk = plev+1-k
        do i=i_1,i_2
          ii1   = idp(i,k,1)
          ii1p1 = ii1 + 1
          ii2   = idp(i,k,2)
          ii2m1 = ii2 - 1
          ii2p1 = ii2 + 1
          ii2p2 = ii2 + 2
          ii3   = idp(i,k,3)
          ii3m1 = ii3 - 1
          ii3p1 = ii3 + 1
          ii3p2 = ii3 + 2
          ii4   = idp(i,k,4)
          ii4p1 = ii4 + 1

          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjm1  = max(jj-1, s_lat_in_pe)
          jjp1  = min(jj+1, n_lat_in_pe)
          jjp2  = min(jj+2, n_lat_in_pe)

          f1 = fb1(ii1p1,k,jjm1)*x2(i,k,1) - fb1(ii1,k,jjm1)*x3(i,k,1)

          f2 = fb1(ii2m1,k,jj  ) * term1x(i,k,2)
     &       + fb1(ii2  ,k,jj  ) * term2x(i,k,2)
     &       + fb1(ii2p1,k,jj  ) * term3x(i,k,2)
     &       + fb1(ii2p2,k,jj  ) * term4x(i,k,2)

          f3 = fb1(ii3m1,k,jjp1) * term1x(i,k,3)
     &       + fb1(ii3  ,k,jjp1) * term2x(i,k,3)
     &       + fb1(ii3p1,k,jjp1) * term3x(i,k,3)
     &       + fb1(ii3p2,k,jjp1) * term4x(i,k,3)

          f4 = fb1(ii4p1,k,jjp2)*x2(i,k,4) - fb1(ii4,k,jjp2)*x3(i,k,4)

          if (.not. quamon) then
            fdp1(i,kk) = f1*term1y(i,k) + f2*term2y(i,k)
     &                 + f3*term3y(i,k) + f4*term4y(i,k)
          else
!           call quasim(fb1(ii2,k,jj),  fb1(ii2+1,k,jj),  f2)
!           call quasim(fb1(ii3,k,jj+1),fb1(ii3+1,k,jj+1),f3)

            tem1 = min(fb1(ii2,  k,jj),fb1(ii2p1,k,jj))
            tem2 = max(fb1(ii2,  k,jj),fb1(ii2p1,k,jj))
            f2   = max(tem1,min(tem2,f2))

            tem1 = min(fb1(ii3,  k,jjp1),fb1(ii3p1,k,jjp1))
            tem2 = max(fb1(ii3,  k,jjp1),fb1(ii3p1,k,jjp1))
            f3   = max(tem1,min(tem2,f3))
!
            fdp1(i,kk) = f1*term1y(i,k) + f2*term2y(i,k)
     &                 + f3*term3y(i,k) + f4*term4y(i,k)
!           call quasim(f2,f3,fdp1(i,kk))
            tem1 = min(f2,f3)
            tem2 = max(f2,f3)
            fdp1(i,kk) = max(tem1,min(tem2,fdp1(i,kk)))
          endif
        end do
      end do
      return
      end
      subroutine int2d_h_levs_mxy_(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                            kdim,fb1,idp,jdp,
     &                            x2,x3,term1x,term2x,term3x,term4x,
     &                            term1y,term2y,term3y,term4y,fdp1)
!sk 10/30/2012 
!sk minor changes to int2d_h_levs_mxy to facilitate cont_eq_opt1
!sk 10/13/2012
!sk modified original subroutine int2d_d_levs for quasi-monotone
!sk quasi-cubic 2d lagrange interpolation in x and y, and renamed
!sk as int2d_h_mxy
!sk
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plon,plond,quamon
      implicit none
!
!   input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim
      integer,dimension(i_1:i_2,plev)   :: jdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(plond,kdim,s_lat_in_pe:n_lat_in_pe)
     &                                                 :: fb1
      real(kind=kind_evod),dimension(plon,plev)        :: fdp1
      real(kind=kind_evod),dimension(i_1:i_2,plev)     :: term1y,term2y,
     &                                                    term3y,term4y
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: x2,x3
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: term1x,term2x,
     &                                                    term3x,term4x

!   local variables

      integer i,k, kk, ii1, ii2, ii3, ii4, jj, jjm1, jjp1, jjp2
     &,       ii1p1, ii2m1, ii2p1, ii2p2, ii3m1, ii3p1, ii3p2, ii4p1
      real(kind=kind_evod):: f1,f2,f3,f4,fdp2,tem1,tem2
!
      do k=1,plev
        kk = plev+1-k
        do i=i_1,i_2
          ii1   = idp(i,k,1)
          ii1p1 = ii1 + 1
          ii2   = idp(i,k,2)
          ii2m1 = ii2 - 1
          ii2p1 = ii2 + 1
          ii2p2 = ii2 + 2
          ii3   = idp(i,k,3)
          ii3m1 = ii3 - 1
          ii3p1 = ii3 + 1
          ii3p2 = ii3 + 2
          ii4   = idp(i,k,4)
          ii4p1 = ii4 + 1

          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjm1  = max(jj-1,s_lat_in_pe)
          jjp1  = min(jj+1,n_lat_in_pe)
          jjp2  = min(jj+2,n_lat_in_pe)

          f1  = fb1 (ii1p1,k,jjm1) * x2    (i,k,1)
     &        - fb1 (ii1  ,k,jjm1) * x3    (i,k,1)
          f2  = fb1 (ii2m1,k,jj  ) * term1x(i,k,2)
     &        + fb1 (ii2  ,k,jj  ) * term2x(i,k,2)
     &        + fb1 (ii2p1,k,jj  ) * term3x(i,k,2)
     &        + fb1 (ii2p2,k,jj  ) * term4x(i,k,2)
          f3  = fb1 (ii3m1,k,jjp1) * term1x(i,k,3)
     &        + fb1 (ii3  ,k,jjp1) * term2x(i,k,3)
     &        + fb1 (ii3p1,k,jjp1) * term3x(i,k,3)
     &        + fb1 (ii3p2,k,jjp1) * term4x(i,k,3)
          f4  = fb1 (ii4p1,k,jjp2) * x2(i,k,4)
     &        - fb1 (ii4  ,k,jjp2) * x3(i,k,4)

          if (.not. quamon) then
            fdp2       = f1*term1y(i,k) + f2*term2y(i,k)
     &                 + f3*term3y(i,k) + f4*term4y(i,k)
          else
!           call quasim(fb1(ii2,k,jj),  fb1(ii2+1,k,jj),f2)
!           call quasim(fb1(ii3,k,jj+1),fb1(ii3+1,k,jj+1),f3)

            tem1 = min(fb1(ii2,  k,jj),fb1(ii2p1,k,jj))
            tem2 = max(fb1(ii2,  k,jj),fb1(ii2p1,k,jj))
            f2   = max(tem1,min(tem2,f2))

            tem1 = min(fb1(ii3,  k,jjp1),fb1(ii3p1,k,jjp1))
            tem2 = max(fb1(ii3,  k,jjp1),fb1(ii3p1,k,jjp1))
            f3   = max(tem1,min(tem2,f3))
!
            fdp2       = f1*term1y(i,k) + f2*term2y(i,k)
     &                 + f3*term3y(i,k) + f4*term4y(i,k)
!           call quasim(f2,f3,fdp2)
            tem1 = min(f2,f3)
            tem2 = max(f2,f3)
            fdp2 = max(tem1,min(tem2,fdp2))
          endif
          fdp1(i,kk) = fdp1(i,kk) + fdp2
        end do
      end do
      return
      end
      subroutine int3dv_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                    fb1,fb2,etamid,detami,zdp,idp,jdp,kdp,
     &                    x, y, dyi, xdp, ydp,
     &                    fdp1,fdp2,fb3,fdp3)
!    &                    xl,xr,ys,yn,fdp1,fdp2,
!    &                    fb3,fdp3)
!sela&                    fb3,fdp3,etaint,detai,kdph)
!
      use     machine        , only : kind_evod
      use      pmgrid        , only : plev,plon,plond,platd
      use      slgshr        , only : rdlam
      implicit none

!  input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                               :: fb1,fb2,fb3
      real(kind=kind_evod),dimension(plond,platd)    :: x
      real(kind=kind_evod),dimension(platd)          :: y,dyi
      real(kind=kind_evod),dimension(plon,plev)      :: xdp,ydp,zdp

      real(kind=kind_evod),dimension(plev)           :: detami,etamid
      real(kind=kind_evod),dimension(i_1:i_2,plev)   :: fdp1,fdp2,fdp3
!    &                                                  ys
!    &                                                  ys,yn
!     real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: xl
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: xl,xr
      real(kind=kind_evod)                           :: xl2,xl3,xr2, xr3
     &,                                                 ys, yn

!   local variables

      integer i,jj,kk,k,l,ii2,ii3,jjp1,kkp1,ii2p1,ii3p1
      real(kind=kind_evod):: zt,zb
!
!
      do k=1,plev
        do i=i_1,i_2

          ii2   = idp(i,k,2)
          ii3   = idp(i,k,3)
          ii2p1 = ii2 + 1
          ii3p1 = ii3 + 1
          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjp1  = min(jj+1,n_lat_in_pe)
          kk    = kdp(i,k)
          kkp1  = kk + 1


          xl2 = (x(ii2p1,jj)   - xdp(i,k)) * rdlam(jj)
          xl3 = (x(ii3p1,jjp1) - xdp(i,k)) * rdlam(jjp1)
          xr2  = 1.0 - xl2
          xr3  = 1.0 - xl3
          ys   = (y(jjp1) - ydp(i,k)) * dyi(jj)
          yn   = 1.0 - ys
!
          zt   = (etamid(kkp1) - zdp(i,k)) * detami(kk)
          zb   = 1.0 - zt
!
          fdp1(i,k) = (( fb1 (ii2  ,kk  ,jj  ) * xl2
     &              +    fb1 (ii2p1,kk  ,jj  ) * xr2 ) * ys
     &              +  ( fb1 (ii3  ,kk  ,jjp1) * xl3
     &              +    fb1 (ii3p1,kk  ,jjp1) * xr3 ) * yn ) * zt
     &              + (( fb1 (ii2  ,kkp1,jj  ) * xl2
     &              +    fb1 (ii2p1,kkp1,jj  ) * xr2 ) * ys
     &              +  ( fb1 (ii3  ,kkp1,jjp1) * xl3
     &              +    fb1 (ii3p1,kkp1,jjp1) * xr3 ) * yn ) * zb
!
!$$$       print*,' inside int3dv i,k,i2,i3,kdp,jdp,xl2,xl3,xr,ys,yn,zt = ',
!$$$     . i,k,idp(i,k,2),idp(i,k,3),kdp(i,k),jdp(i,k),xl(i,k,2),xl(i,k,3),
!$$$     . ys(i,k),yn(i,k),zt

          fdp2(i,k) = (( fb2 (ii2  ,kk  ,jj  ) * xl2
     &              +    fb2 (ii2p1,kk  ,jj  ) * xr2 ) * ys
     &              +  ( fb2 (ii3  ,kk  ,jjp1) * xl3
     &              +    fb2 (ii3p1,kk  ,jjp1) * xr3 ) * yn ) * zt
     &              + (( fb2 (ii2  ,kkp1,jj  ) * xl2
     &              +    fb2 (ii2p1,kkp1,jj  ) * xr2 ) * ys
     &              +  ( fb2 (ii3  ,kkp1,jjp1) * xl3
     &              +    fb2 (ii3p1,kkp1,jjp1) * xr3 ) * yn ) * zb
!
          fdp3(i,k) = (( fb3 (ii2  ,kk  ,jj  ) * xl2
     &              +    fb3 (ii2p1,kk  ,jj  ) * xr2 ) * ys
     &              +  ( fb3 (ii3  ,kk  ,jjp1) * xl3
     &              +    fb3 (ii3p1,kk  ,jjp1) * xr3 ) * yn ) * zt
     &              + (( fb3 (ii2  ,kkp1,jj  ) * xl2
     &              +    fb3 (ii2p1,kkp1,jj  ) * xr2 ) * ys
     &              +  ( fb3 (ii3  ,kkp1,jjp1) * xl3
     &              +    fb3 (ii3p1,kkp1,jjp1) * xr3 ) * yn ) * zb
        enddo
      enddo
      return
      end
      subroutine lagxin_h_m(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                      kdim,fb,idp,jdp,kdp,kkdp,
     &                      x2,x3,term1,term2,term3,term4,fint,quamon)
!sk 2/25/2012
!sk modified original subroutine lagxin_h for quasi-monotone 
!sk quasi-cubic lagrange interpolation in x
!sk 10/08/2012
!sk renamed lagxin_h as lagxin_h_m and added quamon to argument list

      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plond
      implicit none 

      integer, parameter :: ppdy=4,      ! length of interp. grid stencil in y
     &                      ppdz=4       ! length of interp. grid stencil in z

!   input variables

      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp,kkdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                                 :: fb
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: x2,x3
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: term1,term2,
     &                                                    term3,term4
      real(kind=kind_evod),dimension(i_1:i_2,plev,ppdy,ppdz) :: fint
      logical quamon

!   local variables
      integer i,k,kdimm1,kdimm2,ii1,ii2,ii3,ii4,jj,kk
     &,       ii1p1,ii2m1,ii2p1,ii2p2,ii3m1,ii3p1,ii3p2,ii4p1
     &,       jjm1,jjp1,jjp2,kkm1,kkp1,kkp2
!     integer, parameter ::  nclamp=93
      real(kind=kind_evod) tem1, tem2
 
      kdimm1 = kdim - 1
      kdimm2 = kdim - 2

      do k=1,plev
        do i=i_1,i_2
          ii1 = idp(i,k,1)
          ii2 = idp(i,k,2)
          ii3 = idp(i,k,3)
          ii4 = idp(i,k,4)

          ii1p1 = ii1 + 1
          ii2m1 = ii2 - 1
          ii2p1 = ii2 + 1
          ii2p2 = ii2 + 2

          ii3m1 = ii3 - 1
          ii3p1 = ii3 + 1
          ii3p2 = ii3 + 2
          ii4p1 = ii4 + 1


          jj   = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjm1 = max(jj-1,s_lat_in_pe)
          jjp1 = min(jj+1,n_lat_in_pe)
          jjp2 = min(jj+2,n_lat_in_pe)

          kk   = kkdp(i,k)
          kkm1 = kk - 1
          kkp1 = kk + 1
          kkp2 = kk + 2
!
          fint(i,k,2,1) = fb (ii2p1,kkm1,jj  ) * x2   (i,k,2)
     &                  - fb (ii2  ,kkm1,jj  ) * x3   (i,k,2)

          fint(i,k,3,1) = fb (ii3p1,kkm1,jjp1) * x2   (i,k,3)
     &                  - fb (ii3  ,kkm1,jjp1) * x3   (i,k,3)

          fint(i,k,1,2) = fb (ii1p1,kk  ,jjm1) * x2   (i,k,1)
     &                  - fb (ii1  ,kk  ,jjm1) * x3   (i,k,1)

          fint(i,k,2,2) = fb (ii2m1,kk  ,jj  ) * term1(i,k,2)
     &                  + fb (ii2  ,kk  ,jj  ) * term2(i,k,2)
     &                  + fb (ii2p1,kk  ,jj  ) * term3(i,k,2)
     &                  + fb (ii2p2,kk  ,jj  ) * term4(i,k,2)

          fint(i,k,3,2) = fb (ii3m1,kk  ,jjp1) * term1(i,k,3)
     &                  + fb (ii3  ,kk  ,jjp1) * term2(i,k,3)
     &                  + fb (ii3p1,kk  ,jjp1) * term3(i,k,3)
     &                  + fb (ii3p2,kk  ,jjp1) * term4(i,k,3)

          if (quamon) then
!           call quasim(fb(ii2,kk,jj),  fb(ii2+1,kk,jj),  fint(i,k,2,2))
!           call quasim(fb(ii3,kk,jj+1),fb(ii3+1,kk,jj+1),fint(i,k,3,2))

            tem1 = min(fb(ii2,  kk,jj),fb(ii2p1,kk,jj))
            tem2 = max(fb(ii2,  kk,jj),fb(ii2p1,kk,jj))
            fint(i,k,2,2) = max(tem1, min(tem2,fint(i,k,2,2)))

            tem1 = min(fb(ii3,  kk,jjp1),fb(ii3p1,kk,jjp1))
            tem2 = max(fb(ii3,  kk,jjp1),fb(ii3p1,kk,jjp1))
            fint(i,k,3,2) = max(tem1,min(tem2,fint(i,k,3,2)))

          endif

          fint(i,k,4,2) = fb (ii4p1,kk  ,jjp2) * x2   (i,k,4)
     &                  - fb (ii4  ,kk  ,jjp2) * x3   (i,k,4)

          fint(i,k,1,3) = fb (ii1p1,kkp1,jjm1) * x2   (i,k,1)
     &                  - fb (ii1 , kkp1,jjm1) * x3   (i,k,1)

          fint(i,k,2,3) = fb (ii2m1,kkp1,jj  ) * term1(i,k,2)
     &                  + fb (ii2  ,kkp1,jj  ) * term2(i,k,2)
     &                  + fb (ii2p1,kkp1,jj  ) * term3(i,k,2)
     &                  + fb (ii2p2,kkp1,jj  ) * term4(i,k,2)

          fint(i,k,3,3) = fb (ii3m1,kkp1,jjp1) * term1(i,k,3)
     &                  + fb (ii3 , kkp1,jjp1) * term2(i,k,3)
     &                  + fb (ii3p1,kkp1,jjp1) * term3(i,k,3)
     &                  + fb (ii3p2,kkp1,jjp1) * term4(i,k,3)
         if (quamon) then
!          call quasim(fb(ii2,kk+1,jj),   fb(ii2+1,kk+1,jj),
!    &                                    fint(i,k,2,3))
!          call quasim(fb(ii3,kk+1,jj+1), fb(ii3+1,kk+1,jj+1),
!    &                                    fint(i,k,3,3))

           tem1 = min(fb(ii2,  kkp1,jj),fb(ii2p1,kkp1,jj))
           tem2 = max(fb(ii2,  kkp1,jj),fb(ii2p1,kkp1,jj))
           fint(i,k,2,3) = max(tem1, min(tem2,fint(i,k,2,3)))

           tem1 = min(fb(ii3,  kkp1,jjp1),fb(ii3p1,kkp1,jjp1))
           tem2 = max(fb(ii3,  kkp1,jjp1),fb(ii3p1,kkp1,jjp1))
           fint(i,k,3,3) = max(tem1,min(tem2,fint(i,k,3,3)))
         endif

          fint(i,k,4,3) = fb (ii4p1,kkp1,jjp2) * x2   (i,k,4)
     &                  - fb (ii4  ,kkp1,jjp2) * x3   (i,k,4)

          fint(i,k,2,4) = fb (ii2p1,kkp2,jj  ) * x2   (i,k,2)
     &                  - fb (ii2  ,kkp2,jj  ) * x3   (i,k,2)

          fint(i,k,3,4) = fb (ii3p1,kkp2,jjp1) * x2   (i,k,3)
     &                  - fb (ii3  ,kkp2,jjp1) * x3   (i,k,3)

!
          if(kdp (i,k) ==  1) then
            fint(i,k,2,1) = fint(i,k,2,4)
            fint(i,k,3,1) = fint(i,k,3,4)
            fint(i,k,1,3) = fint(i,k,1,2)
            fint(i,k,2,3) = fint(i,k,2,2)
            fint(i,k,3,3) = fint(i,k,3,2)
            fint(i,k,4,3) = fint(i,k,4,2)

            fint(i,k,1,2) = fb (ii1p1,1,jjm1) * x2   (i,k,1)
     &                    - fb (ii1  ,1,jjm1) * x3   (i,k,1)

            fint(i,k,2,2) = fb (ii2m1,1,jj  ) * term1(i,k,2)
     &                    + fb (ii2  ,1,jj  ) * term2(i,k,2)
     &                    + fb (ii2p1,1,jj  ) * term3(i,k,2)
     &                    + fb (ii2p2,1,jj  ) * term4(i,k,2)

            fint(i,k,3,2) = fb (ii3m1,1,jjp1) * term1(i,k,3)
     &                    + fb (ii3  ,1,jjp1) * term2(i,k,3)
     &                    + fb (ii3p1,1,jjp1) * term3(i,k,3)
     &                    + fb (ii3p2,1,jjp1) * term4(i,k,3)


            if (quamon) then
!             call quasim(fb(ii2,1,jj),   fb(ii2+1,1,jj),
!    &                                    fint(i,k,2,2))
!             call quasim(fb(ii3,1,jj+1), fb(ii3+1,1,jj+1),
!    &                                    fint(i,k,3,2))

              tem1 = min(fb(ii2,  1,jj),fb(ii2p1,1,jj))
              tem2 = max(fb(ii2,  1,jj),fb(ii2p1,1,jj))
              fint(i,k,2,2) = max(tem1,min(tem2,fint(i,k,2,2)))

              tem1 = min(fb(ii3,  1,jjp1),fb(ii3p1,1,jjp1))
              tem2 = max(fb(ii3,  1,jjp1),fb(ii3p1,1,jjp1))
              fint(i,k,3,2) = max(tem1,min(tem2,fint(i,k,3,2)))
            endif

            fint(i,k,4,2) = fb (ii4p1,1,jjp2) * x2   (i,k,4)
     &                    - fb (ii4  ,1,jjp2) * x3   (i,k,4)

            fint(i,k,2,4) = fb (ii2p1,3,jj  ) * x2   (i,k,2)
     &                    - fb (ii2  ,3,jj  ) * x3   (i,k,2)

            fint(i,k,3,4) = fb (ii3p1,3,jjp1) * x2   (i,k,3)
     &                    - fb (ii3  ,3,jjp1) * x3   (i,k,3)

          elseif(kdp (i,k) == kdimm1) then
            fint(i,k,2,4) = fint(i,k,2,1)
            fint(i,k,3,4) = fint(i,k,3,1)
            fint(i,k,1,2) = fint(i,k,1,3)
            fint(i,k,2,2) = fint(i,k,2,3)
            fint(i,k,3,2) = fint(i,k,3,3)
            fint(i,k,4,2) = fint(i,k,4,3)

            fint(i,k,2,1) = fb (ii2p1,kdimm2,jj  ) * x2   (i,k,2)
     &                    - fb (ii2  ,kdimm2,jj  ) * x3   (i,k,2)

            fint(i,k,3,1) = fb (ii3p1,kdimm2,jjp1) * x2   (i,k,3)
     &                    - fb (ii3  ,kdimm2,jjp1) * x3   (i,k,3)
            fint(i,k,1,3) = fb (ii1p1,kdim  ,jjm1) * x2   (i,k,1)
     &                    - fb (ii1  ,kdim  ,jjm1) * x3   (i,k,1)

            fint(i,k,2,3) = fb (ii2m1,kdim  ,jj  ) * term1(i,k,2)
     &                    + fb (ii2  ,kdim  ,jj  ) * term2(i,k,2)
     &                    + fb (ii2p1,kdim  ,jj  ) * term3(i,k,2)
     &                    + fb (ii2p2,kdim  ,jj  ) * term4(i,k,2)
            fint(i,k,3,3) = fb (ii3m1,kdim  ,jjp1) * term1(i,k,3)
     &                    + fb (ii3  ,kdim  ,jjp1) * term2(i,k,3)
     &                    + fb (ii3p1,kdim  ,jjp1) * term3(i,k,3)
     &                    + fb (ii3p2,kdim  ,jjp1) * term4(i,k,3)
            if (quamon) then
!             call quasim(fb(ii2,kdim,jj),   fb(ii2p1,kdim,jj),
!    &                                       fint(i,k,2,3))
!             call quasim(fb(ii3,kdim,jj+1), fb(ii3p1,kdim,jjp1),
!    &                                       fint(i,k,3,3))

              tem1 = min(fb(ii2,  kdim,jj),fb(ii2p1,kdim,jj))
              tem2 = max(fb(ii2,  kdim,jj),fb(ii2p1,kdim,jj))
              fint(i,k,2,3) = max(tem1,min(tem2,fint(i,k,2,3)))

              tem1 = min(fb(ii3,  kdim,jjp1),fb(ii3p1,kdim,jjp1))
              tem2 = max(fb(ii3,  kdim,jjp1),fb(ii3p1,kdim,jjp1))
              fint(i,k,3,3) = max(tem1,min(tem2,fint(i,k,3,3)))
            endif

            fint(i,k,4,3) = fb (ii4p1,kdim,jjp2) * x2(i,k,4)
     &                    - fb (ii4  ,kdim,jjp2) * x3(i,k,4)
          endif
        enddo
      enddo
      return
      end

      subroutine lagxinit_h(i_1,i_2,x,xdp,idp,jdp,x2,x3,
     &                      term1,term2,term3,term4)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : platd,plev,plond
      use slgshr             , only : rdlam
      implicit none
!
!   input variables
      integer i_1,i_2
      integer,dimension(i_1:i_2,plev,4)  :: idp
      integer,dimension(i_1:i_2,plev)    :: jdp
      real(kind=kind_evod),dimension(plond,platd)      :: x
      real(kind=kind_evod),dimension(i_1:i_2,plev)     :: xdp
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: x2,x3
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: term1,term2,
     &                                                    term3,term4

!   local variables
      integer i,kk,k,l,n,j,jj
      real(kind=kind_evod), parameter  :: denom1=-1./6., denom2=0.5,
     &                                    denom3=-0.5,   denom4=1./6.
      real(kind=kind_evod)             :: coef12, coef34, tem
!
      do k=1,plev
        do i=i_1,i_2
          j = jdp(i,k) - 2
!                                                           n=1
          jj        = min(j+1,platd)
          x2(i,k,1) = (xdp(i,k) - x(idp(i,k,1),jj)) * rdlam(jj)
          x3(i,k,1) = x2(i,k,1) - 1.
!                                                           n=2
          jj        = min(j+2,platd)
          tem       = (xdp(i,k) - x(idp(i,k,2),jj)) * rdlam(jj)
          x2(i,k,2) = tem
          x3(i,k,2) = tem - 1.

          coef12       = x3(i,k,2)*(tem - 2.)
          coef34       = (tem + 1.)*tem
          term1(i,k,2) = denom1*coef12*tem
          term2(i,k,2) = denom2*coef12*(tem + 1.)
          term3(i,k,2) = denom3*coef34*(tem - 2.)
          term4(i,k,2) = denom4*coef34*x3(i,k,2)
!                                                           n=3
          jj        = min(j+3,platd)
          tem       = (xdp(i,k) - x(idp(i,k,3),jj)) * rdlam(jj)
          x2(i,k,3) = tem
          x3(i,k,3) = tem - 1.

          coef12       = x3(i,k,3)*(tem - 2.)
          coef34       = (tem + 1.)*tem
          term1(i,k,3) = denom1*coef12*tem
          term2(i,k,3) = denom2*coef12*(tem + 1.)
          term3(i,k,3) = denom3*coef34*(tem - 2.)
          term4(i,k,3) = denom4*coef34*x3(i,k,3)
!                                                             n=4
          jj        = min(j+4,platd)
          x2(i,k,4) = (xdp(i,k) - x(idp(i,k,4),jj)) * rdlam(jj)
          x3(i,k,4) = x2(i,k,4) - 1.
        enddo
      enddo
      return
      end
      subroutine xywgts_h(i_1,i_2,x,y,dy,idp,jdp,xdp,ydp,xl,ys)
!     subroutine xywgts_h(i_1,i_2,x,y,dy,idp,jdp,xdp,ydp,xl,xr,ys,yn)
!
      use      machine       , only : kind_evod
      use      pmgrid        , only : platd,plev,plon,plond
      use      slgshr        , only : rdx => dlam
!     use layout1            , only : me
      implicit none
!
!   input variables

      integer i_1,i_2
      integer,dimension(i_1:i_2,plev)   :: jdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(plond,platd)    :: x
      real(kind=kind_evod),dimension(platd)          :: y,dy
      real(kind=kind_evod),dimension(plon,plev)      :: xdp,ydp
      real(kind=kind_evod),dimension(i_1:i_2,plev)   :: ys
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: xl
!     real(kind=kind_evod),dimension(i_1:i_2,plev)   :: ys,yn
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: xl,xr

!   local variables

      integer i,j,k,jj2,jj3
!     real(kind=kind_evod),dimension(platd) :: rdlam

!     do j=1,platd
!       rdx(j) = 1. / (x(2,j) - x(1,j))
!     enddo
      do k=1,plev
        do i=i_1,i_2
          jj2 = jdp(i,k)
          jj3 = jj2 + 1
          xl(i,k,2) = (x(idp(i,k,2)+1,jj2) - xdp(i,k))*rdx(jj2)
          xl(i,k,3) = (x(idp(i,k,3)+1,jj3) - xdp(i,k))*rdx(jj3)
!         xr(i,k,2) = 1. - xl(i,k,2)
!         xr(i,k,3) = 1. - xl(i,k,3)
          ys(i,k)   = (y(jj3) - ydp(i,k)) / dy(jj2)
!         yn(i,k)   = 1. - ys(i,k)
        enddo
      enddo

      return
      end
      subroutine rotate_uv_h(i_1,i_2,
     &                       ud3_d,vd3_d,ud3_a,vd3_a,
     &                       cos_l_a,sin_l_a,cos_f_a,sin_f_a,
     &                       cos_l_d,sin_l_d,cos_f_d,sin_f_d)

      use      machine       , only : kind_evod
      use      pmgrid        , only : plev,plon
      implicit none

!  input variables

      integer i_1,i_2
      real(kind=kind_evod)                        :: cos_f_a,sin_f_a
      real(kind=kind_evod),dimension(plon,plev)   :: ud3_d,vd3_d,
     &                                               ud3_a,vd3_a
      real(kind=kind_evod),dimension(i_1:i_2)     :: cos_l_a,sin_l_a
      real(kind=kind_evod),dimension(i_1:i_2,plev):: cos_l_d,sin_l_d,
     &                                               cos_f_d,sin_f_d

!  local variables

      integer i,k
      real(kind=kind_evod) :: alfa_ua,alfa_va,beta_ua,beta_va
     &,                       cos_l_diff,sin_l_diff

! the trigonometric functions are bottom to top, they are inverted
! in dep3dg and dep3ds, therefore alfas and betas are bottom to top.

      do k = 1,plev
       do i = i_1,i_2
          cos_l_diff = cos_l_a(i)*cos_l_d(i,k)+sin_l_a(i)*sin_l_d(i,k)
          sin_l_diff = sin_l_a(i)*cos_l_d(i,k)-sin_l_d(i,k)*cos_l_a(i)

          alfa_ua    = cos_l_diff
          alfa_va    = sin_f_d(i,k)*sin_l_diff

          beta_ua    = -sin_f_a*sin_l_diff
          beta_va    = cos_f_d(i,k)*cos_f_a
     &               + sin_f_d(i,k)*sin_f_a*cos_l_diff
!
!sela     do k = 1,plev
!sela      do i = i_1,i_2
!sela      alfa_ua(i,k)=1.
!sela      alfa_va(i,k)=0.
!sela
!sela      beta_ua(i,k)=0.
!sela      beta_va(i,k)=1.
!sela      enddo
!sela     enddo
!     ud3_d, vd3_d are bottom to top, coming from lagzin.
!     alfas,betas  are bottom to top, coming from previous loops.
!therefore:
!     ud3_a, vd3_a are bottom to top, as expected in gloopa.

          ud3_a(i,k) = alfa_ua * ud3_d(i,k) + alfa_va * vd3_d(i,k)
          vd3_a(i,k) = beta_ua * ud3_d(i,k) + beta_va * vd3_d(i,k)
        end do
      end do
      return
      end
!
      function bisection(x,plev,u)
      implicit none
      integer u,i,j,k,plev,bisection
      integer,dimension(plev) :: x

       i = 1
       j = plev
       do
         k = (i+j)/2
         if (u < x(k)) then
           j = k
         else
           i = k
         end if
         if (i+1 >= j)  then
           bisection = i
           return
         endif
       end do
       return
      end
      subroutine endrun_h
      implicit none
      print*,' **** endrun_h was called - execution stopped '
      stop13
      end
      subroutine lagyin_h_m(i_1,i_2,
     &               fintx,finty,yb,yt,term1,term2,term3,term4,quamon)
!sk 2/25/2012
!sk modified original subroutine lagyin_h for quasi-monotone
!sk quasi-cubic lagrange interpolation in y
!sk 10/08/2012
!sk renamed lagyin_h as lagyin_h_m and added quamon to argument list

      use machine            , only : kind_evod
      use      pmgrid        , only : plev
      implicit none

!   input variable
      integer i_1,i_2
      real,dimension(i_1:i_2,plev,4,4) :: fintx  ! x-interpolants
      real,dimension(i_1:i_2,plev,4)   :: finty  ! interpolants at the horiz. depart
      real,dimension(i_1:i_2,plev)     :: yb,yt,term1,term2,term3,term4
      logical quamon

!   local variable
      integer i,k
      real(kind=kind_evod) tem1, tem2
!
      do k=1,plev
        do i=i_1,i_2
          finty(i,k,1) =   fintx(i,k,2,1)*yb   (i,k)
     &                   + fintx(i,k,3,1)*yt   (i,k)
          finty(i,k,2) =   fintx(i,k,1,2)*term1(i,k)
     &                   + fintx(i,k,2,2)*term2(i,k)
     &                   + fintx(i,k,3,2)*term3(i,k)
     &                   + fintx(i,k,4,2)*term4(i,k)
          finty(i,k,3) =   fintx(i,k,1,3)*term1(i,k)
     &                   + fintx(i,k,2,3)*term2(i,k)
     &                   + fintx(i,k,3,3)*term3(i,k)
     &                   + fintx(i,k,4,3)*term4(i,k)
          if (quamon) then
!           call quasim(fintx(i,k,2,2),fintx(i,k,3,2),finty(i,k,2))
!           call quasim(fintx(i,k,2,3),fintx(i,k,3,3),finty(i,k,3))

            tem1 = min(fintx(i,k,2,2),fintx(i,k,3,2))
            tem2 = max(fintx(i,k,2,2),fintx(i,k,3,2))
            finty(i,k,2) = max(tem1,min(tem2,finty(i,k,2)))

            tem1 = min(fintx(i,k,2,3),fintx(i,k,3,3))
            tem2 = max(fintx(i,k,2,3),fintx(i,k,3,3))
            finty(i,k,3) = max(tem1,min(tem2,finty(i,k,3)))
          endif
          finty(i,k,4) = fintx(i,k,2,4) * yb(i,k)
     &                 + fintx(i,k,3,4) * yt(i,k)
        enddo
      enddo
      return
      end
      subroutine lagyinit_h(i_1,i_2,y,dyi,lbasiy,ydp,jdp,
     &                      yb,yt,term1,term2,term3,term4)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : platd,plev
      implicit none

!   input variables
      integer i_1,i_2
      real(kind=kind_evod),dimension(platd)        :: y,dyi
      real(kind=kind_evod),dimension(4,2,platd)    :: lbasiy  ! y-interpolation weights
      real(kind=kind_evod),dimension(i_1:i_2,plev) :: ydp     ! y-coordinates of departure pts.
     &,                                               yb,yt,term1,term2
     &,                                               term3,term4
      integer,dimension(i_1:i_2,plev) :: jdp    ! j-index of departure point coord.


!   local variables
      integer i, k, m, jj
      real(kind=kind_evod) :: ymy1,                ! |
     &                        ymy2,                ! |
     &                        ymy3,                ! |
     &                        ymy4,                ! |
     &                        coef12,              ! |
     &                        coef34               ! | -- interpolation weights/coeffs.
!

       do k=1,plev
         do i=i_1,i_2
           jj = jdp(i,k)
           yb(i,k)    = ( y(jj+1) - ydp(i,k) ) * dyi(jj)
           yt(i,k)    = 1. - yb(i,k)
           ymy1       = ydp(i,k) - lbasiy(1,1,jj)
           ymy2       = ydp(i,k) - lbasiy(2,1,jj)
           ymy3       = ydp(i,k) - lbasiy(3,1,jj)
           ymy4       = ydp(i,k) - lbasiy(4,1,jj)
           coef12     = ymy3 * ymy4
           coef34     = ymy1 * ymy2
           term1(i,k) = coef12 * ymy2 * lbasiy(1,2,jj)
           term2(i,k) = coef12 * ymy1 * lbasiy(2,2,jj)
           term3(i,k) = coef34 * ymy4 * lbasiy(3,2,jj)
           term4(i,k) = coef34 * ymy3 * lbasiy(4,2,jj)
         end do
       end do

      return
      end
      subroutine lagzin_h(i_1,i_2,
     &                  kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,detam,
     &                  term1,term2,term3,term4,fdp)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon
      implicit none

!   input variables
      integer i_1,i_2,kdim
      integer,dimension(i_1:i_2,plev) :: kdp
      real(kind=kind_evod),dimension(i_1:i_2,plev,4):: finty
      real(kind=kind_evod),dimension(i_1:i_2,plev)  :: hb,ht,dhb,dht,
     &                                                 term1,term2,
     &                                                 term3,term4
      real(kind=kind_evod),dimension(4,2,kdim)      :: lbasdz
      real(kind=kind_evod),dimension(plon,plev)     :: fdp
      real(kind=kind_evod),dimension(plev)          :: detam  ! delta eta at levels

!   local variables
      integer i,k,m,kdimm1,kdimm2,kk,kr
      real(kind=kind_evod) :: ftop,fbot
!
      kdimm1 = kdim - 1
      kdimm2 = kdim - 2

      do k = 1,plev
        kr = plevp-k
        do i = i_1,i_2
          kk = kdp(i,k)
          if(kk == 1) then
            ftop       = (finty(i,k,3) - finty(i,k,2)) / detam(1)
            fbot       = lbasdz(1,1,2) * finty(i,k,2)
     &                 + lbasdz(2,1,2) * finty(i,k,3)
     &                 + lbasdz(3,1,2) * finty(i,k,4)
     &                 + lbasdz(4,1,2) * finty(i,k,1)
            fdp(i,kr)  = finty(i,k,2)  * ht (i,k)
     &                 + ftop          * dht(i,k)
     &                 + finty(i,k,3)  * hb (i,k)
     &                 + fbot          * dhb(i,k)
          elseif(kk == kdimm1) then
            ftop       = lbasdz(1,2,kdimm2) * finty(i,k,4)
     &                 + lbasdz(2,2,kdimm2) * finty(i,k,1)
     &                 + lbasdz(3,2,kdimm2) * finty(i,k,2)
     &                 + lbasdz(4,2,kdimm2) * finty(i,k,3)
!$$$        fbot       = 0.0
            fdp(i,kr)  = finty(i,k,2) * ht (i,k)
     &                 + ftop         * dht(i,k)
     &                 + finty(i,k,3) * hb (i,k)
!$$$ &                 + fbot         * dhb(i,k)
          else
            fdp(i,kr)  = finty(i,k,1) * term1(i,k)
     &                 + finty(i,k,2) * term2(i,k)
     &                 + finty(i,k,3) * term3(i,k)
     &                 + finty(i,k,4) * term4(i,k)
          endif
        enddo
      enddo
      return
      end
      subroutine lagzinit_h(i_1,i_2,kdim,lbasiz,
     &                      etamid,detam,sigdp,kdp,kkdp,
     &                      hb,ht,dhb,dht,term1z,term2z,term3z,term4z)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev
      implicit none
!
!   input variables
      integer i_1,i_2,kdim
      integer,dimension(i_1:i_2,plev) :: kdp,kkdp
      real(kind=kind_evod),dimension(4,2,kdim)     :: lbasiz
      real(kind=kind_evod),dimension(kdim)         :: etamid,detam
      real(kind=kind_evod),dimension(i_1:i_2,plev) :: sigdp,
     &               hb,ht,dhb,dht,term1z,term2z,term3z,term4z

!
!   local variables
      integer i,k,m,kk,jj
      real(kind=kind_evod) :: dzk,zt,zb,zt2,zb2
      real(kind=kind_evod) :: zmz1,                 ! |
     &                        zmz2,                 ! |
     &                        zmz3,                 ! |
     &                        zmz4,                 ! |
     &                        coef12,               ! |
     &                        coef34                ! | -- interpolation weights/coeffs.
     &,                       stem
!
      do k=1,plev
        do i=i_1,i_2
          jj   = kdp(i,k)
          dzk  =  detam(jj)
          stem = sigdp(i,k)
          zt   = ( etamid(jj+1) - stem )/dzk
          zb   = 1. - zt
          zt2  = zt * zt
          zb2  = zb * zb
          kk   = kkdp(i,k)
          ht(i,k)     = ( 3.0 - 2.0*zt ) * zt2
          hb(i,k)     = ( 3.0 - 2.0*zb ) * zb2
          dht(i,k)    = -dzk*( zt - 1. ) * zt2
          dhb(i,k)    =  dzk*( zb - 1. ) * zb2

          zmz1        = stem - lbasiz(1,1,kk)
          zmz2        = stem - lbasiz(2,1,kk)
          zmz3        = stem - lbasiz(3,1,kk)
          zmz4        = stem - lbasiz(4,1,kk)

          coef12      = zmz3 * zmz4
          coef34      = zmz1 * zmz2

          term1z(i,k) = coef12 * zmz2 * lbasiz(1,2,kk)
          term2z(i,k) = coef12 * zmz1 * lbasiz(2,2,kk)
          term3z(i,k) = coef34 * zmz4 * lbasiz(3,2,kk)
          term4z(i,k) = coef34 * zmz3 * lbasiz(4,2,kk)
        end do
      end do
      return
      end
      subroutine lcbas_h( grd, bas1, bas2 )
      use machine         , only : kind_evod
      implicit none
      real(kind=kind_evod),dimension(4) :: grd               ! grid stencil
      real(kind=kind_evod),dimension(4) :: bas1,             ! grid values on stencil
     &                                     bas2              ! lagrangian basis functions
      real(kind=kind_evod) :: x0mx1,               ! |
     &                        x0mx2,               ! |
     &                        x0mx3,               ! |- grid value differences used in
     &                        x1mx2,               ! |  weights
     &                        x1mx3,               ! |
     &                        x2mx3                ! |

      x0mx1   = grd(1) - grd(2)
      x0mx2   = grd(1) - grd(3)
      x0mx3   = grd(1) - grd(4)
      x1mx2   = grd(2) - grd(3)
      x1mx3   = grd(2) - grd(4)
      x2mx3   = grd(3) - grd(4)
      bas1(1) = grd(1)
      bas1(2) = grd(2)
      bas1(3) = grd(3)
      bas1(4) = grd(4)
      bas2(1) =  1./ ( x0mx1 * x0mx2 * x0mx3 )
      bas2(2) = -1./ ( x0mx1 * x1mx2 * x1mx3 )
      bas2(3) =  1./ ( x0mx2 * x1mx2 * x2mx3 )
      bas2(4) = -1./ ( x0mx3 * x1mx3 * x2mx3 )
      return
      end
      subroutine lcdbas_h(grd,dbas2,dbas3)
      use machine         , only : kind_evod
      implicit none

!
!   input variables
      real(kind=kind_evod),dimension(4) :: grd,              ! grid stencil
     &                                     dbas2,            ! derivatives at grid point 2.
     $                                     dbas3             ! derivatives at grid point 3.
      real(kind=kind_evod) :: x1,                  ! |
     $                        x2,                  ! |- grid values
     $                        x3,                  ! |
     $                        x4,                  ! |
     $                        x1mx2,               !  |
     $                        x1mx3,               !  |
     $                        x1mx4,               !  |- differences of grid values
     $                        x2mx3,               !  |
     $                        x2mx4,               !  |
     $                        x3mx4                !  |

      x1 = grd(1)
      x2 = grd(2)
      x3 = grd(3)
      x4 = grd(4)
      x1mx2 = x1 - x2
      x1mx3 = x1 - x3
      x1mx4 = x1 - x4
      x2mx3 = x2 - x3
      x2mx4 = x2 - x4
      x3mx4 = x3 - x4
      dbas2(1) =   x2mx3 * x2mx4 / ( x1mx2 * x1mx3 * x1mx4 )
      dbas2(2) =   -1./x1mx2 + 1./x2mx3 + 1./x2mx4
      dbas2(3) = - x1mx2 * x2mx4 / ( x1mx3 * x2mx3 * x3mx4 )
      dbas2(4) =   x1mx2 * x2mx3 / ( x1mx4 * x2mx4 * x3mx4 )
      dbas3(1) = - x2mx3 * x3mx4 / ( x1mx2 * x1mx3 * x1mx4 )
      dbas3(2) =   x1mx3 * x3mx4 / ( x1mx2 * x2mx3 * x2mx4 )
      dbas3(3) =   -1./x1mx3 - 1./x2mx3 + 1./x3mx4
      dbas3(4) = - x1mx3 * x2mx3 / ( x1mx4 * x2mx4 * x3mx4 )
      return
      end
      subroutine set_pmgrid_h(lonf,latg,levs)
      use pmgrid , only : mprec,pgls,plat,platd,
     &                    plev,plevd,plevp,plon,plond,pi
      implicit none
!  input variables
      integer lonf,latg,levs

!  local variables
      integer nxpt,jintmx
!
      pi     = 4.*atan(1.)
      plon   = lonf
      plev   = levs
      plat   = latg
      plevp  = plev + 1
      nxpt   = 1
      jintmx = 1
      plond  = plon + 1 + 2*nxpt
      platd  = plat + 2*nxpt + 2*jintmx
      plevd  = plev
      mprec  = 1.e-12
      pgls   = plon*plev

      return
      end
      subroutine herzinit_h(i_1,i_2,kdim,
     &                      etamid,detam,detami,sigdp,kdp,
     &                      hb,ht,dhb,dht)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev
      implicit none
!
!   input variables
      integer i_1,i_2,kdim
      integer,dimension(i_1:i_2,plev) :: kdp
      real(kind=kind_evod),dimension(kdim)         :: etamid,detam
     &,                                               detami
      real(kind=kind_evod),dimension(i_1:i_2,plev) :: sigdp,hb,ht,
     &                                                dhb,dht

!   local variables
      integer i,k,m,kk
      real dzk,zb2,zt2,zt,zb

      do k=1,plev
        do i=i_1,i_2
           kk   = kdp(i,k)
           dzk  = detam(kk)
!          zt   = ( etamid(kk+1) - sigdp(i,k) ) / dzk
           zt   = ( etamid(kk+1) - sigdp(i,k) ) * detami(kk)
           zb   = 1. - zt
           zt2  = zt * zt
           zb2  = zb * zb

           ht(i,k)  = ( 3.0 - zt -zt ) * zt2
           hb(i,k)  = ( 3.0 - zb -zb ) * zb2
           dht(i,k) = -dzk * (zt - 1.) * zt2
           dhb(i,k) =  dzk * (zb - 1.) * zb2
        enddo
      enddo
      return
      end
      subroutine heryinit_h(i_1,i_2,phi,dphi,dphii,phidp,jdp,ys,yn,
     &                      hs,hn,dhs,dhn)
     

      use machine            , only : kind_evod
      use      pmgrid        , only : platd,plev
      implicit none

!   input variables
      integer i_1,i_2
      integer,dimension(i_1:i_2,plev) :: jdp            ! j-index of departure point coord.
      real(kind=kind_evod),dimension(platd)        :: phi,dphi,dphii
      real(kind=kind_evod),dimension(i_1:i_2,plev) :: phidp,   ! y-coordinates of departure pts.
     &                                                ys,yn,hs,hn,dhs,
     &                                                dhn

! local variables
      real dyj,ys2,yn2
      integer i,k,jj
!
      do k=1,plev
         do i=i_1,i_2
            jj  = jdp(i,k)
            dyj = dphi(jj)

!
            ys(i,k)  = (phi(jj+1) - phidp(i,k)) * dphii(jj)
            yn(i,k)  = 1. - ys(i,k)
            ys2      = ys(i,k) * ys(i,k)
            yn2      = yn(i,k) * yn(i,k)
!
            hs(i,k)  = (3.0 - ys(i,k) - ys(i,k)) * ys2
            hn(i,k)  = (3.0 - yn(i,k) - yn(i,k)) * yn2
!
            dhs(i,k) = -dyj * (ys(i,k) - 1.) * ys2
            dhn(i,k) =  dyj * (yn(i,k) - 1.) * yn2
        enddo
      enddo
!

      return
      end
      subroutine her_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                        idp,jdp,kdp,kkdp,xl,xr,hl,hr,dhl,dhr,fb,
     &                              ys,yn,hs,hn,dhs,dhn,dphii,      
!    &                        fintx,ys,yn,hs,hn,dhs,dhn,rdphi,finty,
     &                        kdim,lbasdz,hb,ht,dhb,dht,detam,fdp)
!    &,        lprnt,jp,lan)
!-----------------------------------------------------------------------
      use      machine       , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon,plond,platd
      use      slgshr        , only : lbasdy,rdlam,rdlam6
!-----------------------------------------------------------------------
      implicit none
!-----------------------------------------------------------------------
!   input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim
      integer,dimension(i_1:i_2,plev,4) :: idp
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp,kkdp

      real(kind=kind_evod),dimension(i_1:i_2,plev)     :: ys,yn,hs,hn
     &,                                                   dhs, dhn
!    &,                                                   dhs, dhn,rdphi
     &,                                                   hb,ht,dhb,dht
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: xl,xr
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: hl,hr,dhl,dhr
!    &                                                    dhr,finty
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                                 :: fb
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4,4) :: fintx
      real(kind=kind_evod),dimension(4,2,kdim)         :: lbasdz
      real(kind=kind_evod),dimension(plon,plev)        :: fdp
      real(kind=kind_evod),dimension(plev)             :: detam  ! delta eta at levels
      real(kind=kind_evod),dimension(platd)            :: dphii
      logical lprnt

! local variables
      integer kdimm1,kdimm2,m,ii1,ii2,ii3,ii4,jj,kk,i,k
     &,       ii1p1,ii2m1,ii2p1,ii2p2,ii3m1,ii3p1,ii3p2,ii4p1
     &,       jjm1,jjp1,jjp2,kkm1,kkp1,kkp2
      real(kind=kind_evod) fxl,fxr,deli,tmp1,tmp2,
     &                     rdlamjj,rdlamjjp1,rdlam6jj,rdlam6jjp1,fac
      real(kind=kind_evod),dimension(4,4)            :: fintx
      real(kind=kind_evod),dimension(4)              :: finty
!     real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: ftop,fbot
      real(kind=kind_evod),dimension(4)              :: ftop,fbot

!-----------------------------------------------------------------------
!     if (lprnt) then
!     print*,'hello  from her_xyz_in_h '
!     print*,' i_1=',i_1,' plon=',plon,' plev=',plev
!     print*,' i_2=',i_2
!     print*,' s_lat_in_pe=',s_lat_in_pe
!     print*,' n_lat_in_pe=',n_lat_in_pe
!     print*,' kdim=',kdim
!     print*,' fb=', fb(2,1:5,jp)
!!!   print*,' fdp=', fdp(1,1)
!!!   print*,' =',
!     print*,'goodby from her_xyz_in_h '
!     endif
!-----------------------------------------------------------------------
!
      fac  = 3.*(1. - 10.*epsilon(fac))
 !    print*,' her_xyz_in_h fac = ',fac

      kdimm1 = kdim - 1
      kdimm2 = kdim - 2
!
! part 1:  x-interpolation
!
! loop over fields.
! ..x interpolation at each height needed for z interpolation.
! ...x interpolation at each latitude needed for y interpolation.
!
      do k=1,plev
        do i=i_1,i_2
          ii1 = idp(i,k,1)
          ii2 = idp(i,k,2)
          ii3 = idp(i,k,3)
          ii4 = idp(i,k,4)
          kk  = kkdp(i,k)
!
          ii1p1 = ii1 + 1
          ii2m1 = ii2 - 1
          ii2p1 = ii2 + 1
          ii2p2 = ii2 + 2
          ii3m1 = ii3 - 1
          ii3p1 = ii3 + 1
          ii3p2 = ii3 + 2
          ii4p1 = ii4 + 1

          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjm1  = max(jj-1,s_lat_in_pe)
          jjp1  = min(jj+1,n_lat_in_pe)
          jjp2  = min(jj+2,n_lat_in_pe)

          kkm1  = kk  - 1
          kkp1  = kk  + 1
          kkp2  = kk  + 2

          rdlamjj   = rdlam(jj)
          rdlamjjp1 = rdlam(jj+1)
!
          rdlam6jj   = rdlam6(jj)
          rdlam6jjp1 = rdlam6(jj+1)

!sela     if (ii2-1 .le. 0) then
!sela        print*,' zero index in herxin ii',ii2
!sela        stop221
!sela     endif
!sela     if (jj-1 .le. 0) then
!sela        print*,' zero index in herxin jj',jj
!sela        stop222
!sela     endif
!sela     if (kk-1 .le. 0) then
!sela        print*,' zero index in herxin kk',kk
!sela        stop223
!sela     endif
!

!
! height level 1:  linear interpolation on inner two latitudes only
!
!!!       fintx(1,1) = not used

          fintx(2,1) = fb(ii2  ,kkm1,jj  ) * xl(i,k,2) +
     &                 fb(ii2p1,kkm1,jj  ) * xr(i,k,2)
          fintx(3,1) = fb(ii3  ,kkm1,jjp1) * xl(i,k,3) +
     &                 fb(ii3p1,kkm1,jjp1) * xr(i,k,3)

!!!       fintx(4,1) = not used
!
! height level 2
!
!   latitude 1:  linear interpolation
!
          fintx(1,2) = fb(ii1  ,kk,jjm1) * xl(i,k,1) +
     &                 fb(ii1p1,kk,jjm1) * xr(i,k,1)
!
!   latitude 2:  cubic interpolation
!
          fxl = (   - 2.*fb(ii2m1,kk,jj) 
     &              - 3.*fb(ii2  ,kk,jj) 
     &              + 6.*fb(ii2p1,kk,jj) 
     &              -    fb(ii2p2,kk,jj) )*rdlam6jj
          fxr = (        fb(ii2m1,kk,jj) 
     &              - 6.*fb(ii2  ,kk,jj) 
     &              + 3.*fb(ii2p1,kk,jj) 
     &              + 2.*fb(ii2p2,kk,jj) )*rdlam6jj
!
          deli = (       fb(ii2p1,kk,jj) - 
     &                   fb(ii2  ,kk,jj) )*rdlamjj
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(2,2) = fb(ii2  ,kk,jj) * hl(i,k,2) 
     &               + fb(ii2p1,kk,jj) * hr(i,k,2) 
     &               + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)
!
!   latitude 3:  cubic interpolation
!
          fxl = (   - 2.*fb(ii3m1,kk  ,jjp1) 
     &              - 3.*fb(ii3  ,kk  ,jjp1) 
     &              + 6.*fb(ii3p1,kk  ,jjp1) 
     &              -    fb(ii3p2,kk  ,jjp1) )*rdlam6jjp1
          fxr = (        fb(ii3m1,kk  ,jjp1) 
     &              - 6.*fb(ii3  ,kk  ,jjp1) 
     &              + 3.*fb(ii3p1,kk  ,jjp1) 
     &              + 2.*fb(ii3p2,kk  ,jjp1) )*rdlam6jjp1
!
          deli = (       fb(ii3p1,kk  ,jjp1) - 
     &                   fb(ii3  ,kk  ,jjp1) )*rdlamjjp1
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(3,2) = fb(ii3  ,kk  ,jjp1) * hl(i,k,3) 
     &               + fb(ii3p1,kk  ,jjp1) * hr(i,k,3) 
     &               + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
          fintx(4,2) = fb(ii4  ,kk,jjp2) * xl(i,k,4) 
     &               + fb(ii4p1,kk,jjp2) * xr(i,k,4)
!
! height level 3
!
!   latitude 1:  linear interpolation
!
          fintx(1,3) = fb(ii1  ,kkp1,jjm1) * xl (i,k,1) 
     &               + fb(ii1p1,kkp1,jjm1) * xr (i,k,1)
!
!   latitude 2:  cubic interpolation
!
          fxl = (   - 2.*fb(ii2m1,kkp1,jj  ) 
     &              - 3.*fb(ii2  ,kkp1,jj  ) 
     &              + 6.*fb(ii2p1,kkp1,jj  ) 
     &              -    fb(ii2p2,kkp1,jj  ) )*rdlam6jj
          fxr = (        fb(ii2m1,kkp1,jj  ) 
     &              - 6.*fb(ii2  ,kkp1,jj  ) 
     &              + 3.*fb(ii2p1,kkp1,jj  ) 
     &              + 2.*fb(ii2p2,kkp1,jj  ) )*rdlam6jj
!
          deli = (       fb(ii2p1,kkp1,jj  ) - 
     &                   fb(ii2  ,kkp1,jj  ) )*rdlamjj
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(2,3) = fb(ii2  ,kkp1,jj ) * hl(i,k,2) 
     &               + fb(ii2p1,kkp1,jj ) * hr(i,k,2) 
     &               + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)
!
!   latitude 3:  cubic interpolation
!
          fxl = (- 2.*fb(ii3m1,kkp1,jjp1) 
     &           - 3.*fb(ii3  ,kkp1,jjp1) 
     &           + 6.*fb(ii3p1,kkp1,jjp1) 
     &           -    fb(ii3p2,kkp1,jjp1) )*rdlam6jjp1
          fxr = (     fb(ii3m1,kkp1,jjp1) 
     &           - 6.*fb(ii3  ,kkp1,jjp1) 
     &           + 3.*fb(ii3p1,kkp1,jjp1) 
     &           + 2.*fb(ii3p2,kkp1,jjp1) )*rdlam6jjp1
!
          deli = (    fb(ii3p1,kkp1,jjp1) - 
     &                fb(ii3  ,kkp1,jjp1) )*rdlamjjp1
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl  <= 0.0  ) fxl = 0.
          if( deli*fxr  <= 0.0  ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(3,3) = fb(ii3  ,kkp1,jjp1) * hl(i,k,3) 
     &               + fb(ii3p1,kkp1,jjp1) * hr(i,k,3) 
     &               + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
          fintx(4,3) = fb(ii4  ,kkp1,jjp2) * xl(i,k,4) 
     &               + fb(ii4p1,kkp1,jjp2) * xr(i,k,4)
!
! height level 4:  linear interpolation on inner two latitudes only
!
!!!       fintx(1,4) = not used
          fintx(2,4) = fb(ii2  ,kkp2,jj  ) * xl(i,k,2) 
     &               + fb(ii2p1,kkp2,jj  ) * xr(i,k,2)
          fintx(3,4) = fb(ii3  ,kkp2,jjp1) * xl(i,k,3) 
     &               + fb(ii3p1,kkp2,jjp1) * xr(i,k,3)
!!!       fintx(4,4) = not used

! top interval
!
!the following loop computes x-derivatives for those cases when the
! departure point lies in either the top or bottom interval of the
! model grid.  in this special case, data are shifted up or down to
! keep the departure point in the middle interval of the 4-point
! stencil. therefore, some derivatives that were computed above will
! be over-written.
          if(kdp (i,k) ==  1) then
!
! shift levels 4 and 2 data to levels 1 and 3, respectively
!
             fintx(2,1) = fintx(2,4)
             fintx(3,1) = fintx(3,4)
!
             fintx(1,3) = fintx(1,2)
             fintx(2,3) = fintx(2,2)
             fintx(3,3) = fintx(3,2)
             fintx(4,3) = fintx(4,2)
!
! height level 1 (placed in level 2 of stencil):
!
!   latitude 1:  linear interpolation
!
!     print *,' i=',i,' k=',k,' ii1=',ii1,' jj=',jj,' in her_xyz'

             fintx(1,2) = fb(ii1  ,1,jjm1) * xl(i,k,1)
     &                  + fb(ii1p1,1,jjm1) * xr(i,k,1)
!
!   latitude 2:  cubic interpolation
!
             fxl = (- 2.*fb(ii2m1,1,jj) - 3.*fb(ii2  ,1,jj)
     &              + 6.*fb(ii2p1,1,jj) -    fb(ii2p2,1,jj) )*rdlam6jj

             fxr = (     fb(ii2m1,1,jj) - 6.*fb(ii2  ,1,jj)
     &              + 3.*fb(ii2p1,1,jj) + 2.*fb(ii2p2,1,jj) )*rdlam6jj
!
             deli = (    fb(ii2p1,1,jj) -    fb(ii2  ,1,jj) )*rdlamjj

             tmp1 = fac*deli
             tmp2 = abs( tmp1 )
             if( deli*fxl  <= 0.0  ) fxl = 0.
             if( deli*fxr  <= 0.0  ) fxr = 0.
             if( abs( fxl ) > tmp2 ) fxl = tmp1
             if( abs( fxr ) > tmp2 ) fxr = tmp1
!
             fintx(2,2) = fb(ii2  ,1,jj) * hl(i,k,2)
     &                  + fb(ii2p1,1,jj) * hr(i,k,2)
     &                  + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)
!
!   latitude 3:  cubic interpolation
!
             fxl = (- 2.*fb(ii3m1,1,jjp1) - 3.*fb(ii3  ,1,jjp1)
     &              + 6.*fb(ii3p1,1,jjp1) -    fb(ii3p2,1,jjp1) )
     &                                    * rdlam6jjp1
             fxr = (  fb(ii3m1,1,jjp1) - 6.*fb(ii3  ,1,jjp1)
     &           + 3.*fb(ii3p1,1,jjp1) + 2.*fb(ii3p2,1,jjp1))*rdlam6jjp1
!
             deli = (fb (ii3p1,1,jjp1) - fb(ii3  ,1,jjp1))*rdlamjjp1
             tmp1 = fac*deli
             tmp2 = abs( tmp1 )
             if( deli*fxl  <= 0.0  ) fxl = 0.
             if( deli*fxr  <= 0.0  ) fxr = 0.
             if( abs( fxl ) > tmp2 ) fxl = tmp1
             if( abs( fxr ) > tmp2 ) fxr = tmp1
!
             fintx(3,2) = fb(ii3  ,1,jjp1) * hl(i,k,3)
     &                  + fb(ii3p1,1,jjp1) * hr(i,k,3)
     &                  + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
             fintx(4,2) = fb(ii4  ,1,jjp2) * xl(i,k,4)
     &                  + fb(ii4p1,1,jjp2) * xr(i,k,4)
!
! height level 3 (placed in level 4 of stencil):
!  linear interpolation on inner two latitudes only
!
!!!          fintx(1,4) = not used
             fintx(2,4) = fb(ii2  ,3,jj  ) * xl(i,k,2)
     &                  + fb(ii2p1,3,jj  ) * xr(i,k,2)
             fintx(3,4) = fb(ii3  ,3,jjp1) * xl(i,k,3)
     &                  + fb(ii3p1,3,jjp1) * xr(i,k,3)
!!!          fintx(4,4) = not used
!
! bot interval
!
          elseif(kdp (i,k) == kdimm1) then
!
! shift levels 1 and 3 data to levels 4 and 2, respectively
!
             fintx(2,4) = fintx(2,1)
             fintx(3,4) = fintx(3,1)
!
             fintx(1,2) = fintx(1,3)
             fintx(2,2) = fintx(2,3)
             fintx(3,2) = fintx(3,3)
             fintx(4,2) = fintx(4,3)
!
! height level 2 (placed in level 1 of stencil):
!  linear interpolation on inner two latitudes only
!
!!!          fintx(1,1) =  not used
             fintx(2,1) = fb(ii2  ,kdimm2,jj  ) * xl (i,k,2)
     &                  + fb(ii2p1,kdimm2,jj  ) * xr (i,k,2)
             fintx(3,1) = fb(ii3  ,kdimm2,jjp1) * xl (i,k,3)
     &                  + fb(ii3p1,kdimm2,jjp1) * xr (i,k,3)
!!!          fintx(4,1) =  not used
!
! height level 4 (placed in level 3 of stencil):
!
!   latitude 1:  linear interpolation
!
             fintx(1,3) = fb(ii1  ,kdim,jjm1) * xl (i,k,1)
     &                  + fb(ii1p1,kdim,jjm1) * xr (i,k,1)
!
!   latitude 2:  cubic interpolation
!
             fxl = (   - 2.*fb(ii2m1,kdim,jj  )
     &                 - 3.*fb(ii2  ,kdim,jj  )
     &                 + 6.*fb(ii2p1,kdim,jj  )
     &                 -    fb(ii2p2,kdim,jj  ) )*rdlam6jj
             fxr = (        fb(ii2m1,kdim,jj  )
     &                 - 6.*fb(ii2  ,kdim,jj  )
     &                 + 3.*fb(ii2p1,kdim,jj  )
     &                 + 2.*fb(ii2p2,kdim,jj  ) )*rdlam6jj
!
             deli = (       fb(ii2p1,kdim,jj  ) -
     &                      fb(ii2  ,kdim,jj  ) )*rdlamjj
             tmp1 = fac*deli
             tmp2 = abs( tmp1 )
             if( deli*fxl   <= 0.0 ) fxl = 0.
             if( deli*fxr   <= 0.0 ) fxr = 0.
             if( abs( fxl ) > tmp2 ) fxl = tmp1
             if( abs( fxr ) > tmp2 ) fxr = tmp1
!
             fintx(2,3) = fb(ii2  ,kdim,jj  ) * hl (i,k,2)
     &                  + fb(ii2p1,kdim,jj  ) * hr (i,k,2)
     &                  + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)
!
!   latitude 3:  cubic interpolation
!
             fxl = (   - 2.*fb(ii3m1,kdim,jjp1)
     &                 - 3.*fb(ii3  ,kdim,jjp1)
     &                 + 6.*fb(ii3p1,kdim,jjp1)
     &                 -    fb(ii3p2,kdim,jjp1) )*rdlam6jjp1
             fxr = (        fb(ii3m1,kdim,jjp1)
     &                 - 6.*fb(ii3  ,kdim,jjp1)
     &                 + 3.*fb(ii3p1,kdim,jjp1)
     &                 + 2.*fb(ii3p2,kdim,jjp1) )*rdlam6jjp1
!
             deli = (       fb(ii3p1,kdim,jjp1) -
     &                      fb(ii3  ,kdim,jjp1) )*rdlamjjp1
             tmp1 = fac*deli
             tmp2 = abs( tmp1 )
             if( deli*fxl   <= 0.0 ) fxl = 0.
             if( deli*fxr   <= 0.0 ) fxr = 0.
             if( abs( fxl ) > tmp2 ) fxl = tmp1
             if( abs( fxr ) > tmp2 ) fxr = tmp1
!
             fintx(3,3) = fb(ii3  ,kdim,jjp1) * hl(i,k,3)
     &                  + fb(ii3p1,kdim,jjp1) * hr(i,k,3)
     &                  + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
             fintx(4,3) = fb(ii4  ,kdim,jjp2) * xl(i,k,4)
     &                  + fb(ii4p1,kdim,jjp2) * xr(i,k,4)
          endif
!
!-----------------------------------------------------------------------
!
! y derivatives at the inner height levels (kk = 2,3) needed for
! z-interpolation
!
          kk = 2
          fbot(kk) = lbasdy(1,1,jj) * fintx(1,kk) 
     &             + lbasdy(2,1,jj) * fintx(2,kk) 
     &             + lbasdy(3,1,jj) * fintx(3,kk) 
     &             + lbasdy(4,1,jj) * fintx(4,kk)
          ftop(kk) = lbasdy(1,2,jj) * fintx(1,kk) 
     &             + lbasdy(2,2,jj) * fintx(2,kk) 
     &             + lbasdy(3,2,jj) * fintx(3,kk) 
     &             + lbasdy(4,2,jj) * fintx(4,kk)
!
! apply scm0 limiter to derivative estimates.
!
          deli = ( fintx(3,kk) - fintx(2,kk) )*dphii(jj)
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fbot(kk)   <= 0.0 ) fbot(kk) = 0.
          if( deli*ftop(kk)   <= 0.0 ) ftop(kk) = 0.
          if( abs( fbot(kk) ) > tmp2 ) fbot(kk) = tmp1
          if( abs( ftop(kk) ) > tmp2 ) ftop(kk) = tmp1

          kk = 3
          fbot(kk) = lbasdy(1,1,jj) * fintx(1,kk)
     &             + lbasdy(2,1,jj) * fintx(2,kk)
     &             + lbasdy(3,1,jj) * fintx(3,kk)
     &             + lbasdy(4,1,jj) * fintx(4,kk)
          ftop(kk) = lbasdy(1,2,jj) * fintx(1,kk)
     &             + lbasdy(2,2,jj) * fintx(2,kk)
     &             + lbasdy(3,2,jj) * fintx(3,kk)
     &             + lbasdy(4,2,jj) * fintx(4,kk)

! apply scm0 limiter to derivative estimates.
!
          deli = ( fintx(3,kk) - fintx(2,kk) )*dphii(jj)
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fbot(kk)   <= 0.0 ) fbot(kk) = 0.
          if( deli*ftop(kk)   <= 0.0 ) ftop(kk) = 0.
          if( abs( fbot(kk) ) > tmp2 ) fbot(kk) = tmp1
          if( abs( ftop(kk) ) > tmp2 ) ftop(kk) = tmp1
!
! part 3:  y-interpolants
!
          finty(1) = fintx(2,1)*ys (i,k) + fintx(3,1)*yn (i,k)

          finty(2) = fintx(2,2)*hs (i,k) + fbot (2)*dhs(i,k)
     &             + fintx(3,2)*hn (i,k) + ftop (2)*dhn(i,k)

          finty(3) = fintx(2,3)*hs (i,k) + fbot (3)*dhs(i,k)
     &             + fintx(3,3)*hn (i,k) + ftop (3)*dhn(i,k)

          finty(4) = fintx(2,4)*ys (i,k) + fintx(3,4)*yn (i,k)
!
!-----------------------------------------------------------------------
!
          kk = kdp (i,k)
          if(kk == 1) then

            ftop(1) = (finty(3) - finty(2)) / detam(1)
            fbot(1) = lbasdz(1,1,2)*finty(2) + lbasdz(2,1,2)*finty(3)
     &              + lbasdz(3,1,2)*finty(4) + lbasdz(4,1,2)*finty(1)

          elseif(kk == kdimm1) then

            ftop(1) = lbasdz(1,2,kdimm2) * finty(4)
     &              + lbasdz(2,2,kdimm2) * finty(1)
     &              + lbasdz(3,2,kdimm2) * finty(2)
     &              + lbasdz(4,2,kdimm2) * finty(3)
            fbot(1) = 0.0

          else

            ftop(1) = lbasdz(1,1,kk)*finty(1) + lbasdz(2,1,kk)*finty(2)
     &              + lbasdz(3,1,kk)*finty(3) + lbasdz(4,1,kk)*finty(4)
            fbot(1) = lbasdz(1,2,kk)*finty(1) + lbasdz(2,2,kk)*finty(2)
     &              + lbasdz(3,2,kk)*finty(3) + lbasdz(4,2,kk)*finty(4)

          endif
!
          deli = (finty(3) - finty(2)) / detam(kk)
          tmp1 = fac*deli
          tmp2 = abs(tmp1)

          if( deli*fbot(1)   <= 0.0 ) fbot(1) = 0.
          if( deli*ftop(1)   <= 0.0 ) ftop(1) = 0.
          if( abs( fbot(1) ) > tmp2 ) fbot(1) = tmp1
          if( abs( ftop(1) ) > tmp2 ) ftop(1) = tmp1

          fdp(i,plevp-k) = finty(2)*ht(i,k) + ftop(1)*dht(i,k)
     &                   + finty(3)*hb(i,k) + fbot(1)*dhb(i,k)

        end do
      enddo

!-----------------------------------------------------------------------
!     if (lprnt) then
!     print*,' fdp=', fdp(1,plevp-5:plev)
!     print*,'goodby from her_xyz_in_h '
!     endif
!-----------------------------------------------------------------------
      return
      end
      subroutine herxin_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                    idp,jdp,kdp,kkdp,xl,xr,hl,hr,dhl,dhr,fb,fintx)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plond
      use slgshr             , only : rdlam,rdlam6
      implicit none
!
!  input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer,dimension(i_1:i_2,plev,4) :: idp
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp,kkdp

      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: xl, xr
      real(kind=kind_evod),dimension(i_1:i_2,plev,2:3) :: hl,hr,dhl,dhr
      real(kind=kind_evod),dimension(i_1:i_2,plev,4,4) :: fintx
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                                 :: fb

!  local variables
      integer kdim,kdimm1,kdimm2,ii1,ii2,ii3,ii4,jj,kk,i,k,ii1p1,ii2m1
     &,       ii2p1,ii2p2,ii3m1,ii3p1,ii3p2,ii4p1,jjm1,jjp1,jjp2
     &,       kkm1,kkp1,kkp2
      real(kind=kind_evod) :: rdlamjj,rdlamjjp1,rdlam6jj,rdlam6jjp1,
     &                        fxl,fxr,deli,tmp1,tmp2,fac
!
      fac  = 3.*(1. - 10.*epsilon(fac))
!
      kdim   = plev
      kdimm1 = kdim - 1
      kdimm2 = kdim - 2
!
! part 1:  x-interpolation
!
! loop over fields.
! ..x interpolation at each height needed for z interpolation.
! ...x interpolation at each latitude needed for y interpolation.
!
      do k=1,plev
        do i=i_1,i_2
          ii1   = idp(i,k,1)
          ii1p1 = ii1 + 1
          ii2   = idp(i,k,2)
          ii2m1 = ii2 - 1
          ii2p1 = ii2 + 1
          ii2p2 = ii2 + 2
          ii3   = idp(i,k,3)
          ii3m1 = ii3 - 1
          ii3p1 = ii3 + 1
          ii3p2 = ii3 + 2
          ii4   = idp(i,k,4)
          ii4p1 = ii4 + 1

          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjm1  = max(jj-1,s_lat_in_pe)
          jjp1  = min(jj+1,n_lat_in_pe)
          jjp2  = min(jj+2,n_lat_in_pe)

          kk    = kkdp(i,k)
          kkm1  = kk - 1
          kkp1  = kk + 1
          kkp2  = kk + 2

          rdlamjj   = rdlam(jj)
          rdlamjjp1 = rdlam(jjp1)
!
          rdlam6jj   = rdlam6(jj)
          rdlam6jjp1 = rdlam6(jjp1)

!sela     if (ii2-1 .le. 0) then
!sela       print*,' zero index in herxin ii',ii2
!sela       stop221
!sela     endif
!sela     if (jj-1 .le. 0) then
!sela        print*,' zero index in herxin jj',jj
!sela        stop222
!sela     endif
!sela     if (kk-1 .le. 0) then
!sela        print*,' zero index in herxin kk',kk
!sela        stop223
!sela     endif
!
!
! height level 1:  linear interpolation on inner two latitudes only
!
!!!       fintx(i,k,1,1) = not used
          fintx(i,k,2,1) = fb(ii2  ,kkm1,jj  ) * xl(i,k,2) +
     &                     fb(ii2p1,kkm1,jj  ) * xr(i,k,2)
          fintx(i,k,3,1) = fb(ii3  ,kkm1,jjp1) * xl(i,k,3) +
     &                     fb(ii3p1,kkm1,jjp1) * xr(i,k,3)
!!!       fintx(i,k,4,1) = not used
!
! height level 2
!
!   latitude 1:  linear interpolation
!
          fintx(i,k,1,2) = fb(ii1  ,kk,jjm1) * xl(i,k,1) +
     &                     fb(ii1p1,kk,jjm1) * xr(i,k,1)
!
!   latitude 2:  cubic interpolation
!
          fxl = (- 2.*fb(ii2m1,kk,jj) - 3.*fb(ii2  ,kk,jj) 
     &           + 6.*fb(ii2p1,kk,jj) -    fb(ii2p2,kk,jj) )*rdlam6jj
          fxr = (     fb(ii2m1,kk,jj) - 6.*fb(ii2  ,kk,jj) 
     &           + 3.*fb(ii2p1,kk,jj) + 2.*fb(ii2p2,kk,jj) )*rdlam6jj
!
          deli = (    fb (ii2+1,kk,jj) -   fb (ii2  ,kk,jj) )*rdlamjj
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(i,k,2,2) = fb(ii2  ,kk,jj) * hl (i,k,2) 
     &                   + fb(ii2p1,kk,jj) * hr (i,k,2) 
     &                   + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)
!
!   latitude 3:  cubic interpolation
!
          fxl = (- 2.*fb(ii3m1,kk  ,jjp1)
     &           - 3.*fb(ii3  ,kk  ,jjp1) 
     &           + 6.*fb(ii3p1,kk  ,jjp1)
     &           -    fb(ii3p2,kk  ,jjp1) )*rdlam6jjp1
          fxr = (     fb(ii3m1,kk  ,jjp1) 
     &           - 6.*fb(ii3  ,kk  ,jjp1) 
     &           + 3.*fb(ii3p1,kk  ,jjp1) 
     &           + 2.*fb(ii3p2,kk  ,jjp1) )*rdlam6jjp1
!
          deli = (    fb(ii3p1,kk  ,jjp1) - 
     &                fb(ii3  ,kk  ,jjp1) )*rdlamjjp1
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(i,k,3,2) = fb(ii3  ,kk  ,jjp1) * hl(i,k,3) 
     &                   + fb(ii3p1,kk  ,jjp1) * hr(i,k,3) 
     &                   + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
          fintx(i,k,4,2) = fb(ii4  ,kk,jjp2) * xl(i,k,4) 
     &                   + fb(ii4p1,kk,jjp2) * xr(i,k,4)
!
! height level 3
!
!   latitude 1:  linear interpolation
!
          fintx(i,k,1,3) = fb(ii1  ,kkp1,jjm1) * xl(i,k,1) 
     &                   + fb(ii1p1,kkp1,jjm1) * xr(i,k,1)
!
!   latitude 2:  cubic interpolation
!
          fxl = (- 2.*fb(ii2m1,kkp1,jj  ) 
     &           - 3.*fb(ii2  ,kkp1,jj  ) 
     &           + 6.*fb(ii2p1,kkp1,jj  ) 
     &           -    fb(ii2p2,kkp1,jj  ) )*rdlam6jj
          fxr = (     fb(ii2m1,kkp1,jj  ) 
     &           - 6.*fb(ii2  ,kkp1,jj  ) 
     &           + 3.*fb(ii2p1,kkp1,jj  ) 
     &           + 2.*fb(ii2p2,kkp1,jj  ) )*rdlam6jj
!
          deli = (    fb(ii2p1,kkp1,jj  ) - 
     &                fb(ii2  ,kkp1,jj  ) )*rdlamjj
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1
!
          fintx(i,k,2,3) = fb(ii2  ,kkp1,jj  ) * hl (i,k,2) 
     &                   + fb(ii2p1,kkp1,jj  ) * hr (i,k,2) 
     &                   + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)
!
!   latitude 3:  cubic interpolation
!
          fxl = (   - 2.*fb(ii3m1,kkp1,jjp1) 
     &              - 3.*fb(ii3  ,kkp1,jjp1) 
     &              + 6.*fb(ii3p1,kkp1,jjp1) 
     &              -    fb(ii3p2,kkp1,jjp1) )*rdlam6jjp1
          fxr = (        fb(ii3m1,kkp1,jjp1) 
     &              - 6.*fb(ii3  ,kkp1,jjp1) 
     &              + 3.*fb(ii3p1,kkp1,jjp1) 
     &              + 2.*fb(ii3p2,kkp1,jjp1) )*rdlam6jjp1
!
          deli = (       fb (ii3p1,kkp1,jjp1) - 
     &                   fb (ii3  ,kkp1,jjp1) )*rdlamjjp1
          tmp1 = fac*deli
          tmp2 = abs( tmp1 )
          if( deli*fxl   <= 0.0 ) fxl = 0.
          if( deli*fxr   <= 0.0 ) fxr = 0.
          if( abs( fxl ) > tmp2 ) fxl = tmp1
          if( abs( fxr ) > tmp2 ) fxr = tmp1

          fintx(i,k,3,3) = fb(ii3  ,kkp1,jjp1) * hl (i,k,3) 
     &                   + fb(ii3p1,kkp1,jjp1) * hr (i,k,3) 
     &                   + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)

!   latitude 4:  linear interpolation
!
          fintx(i,k,4,3) = fb(ii4  ,kkp1,jjp2) * xl (i,k,4) 
     &                   + fb(ii4p1,kkp1,jjp2) * xr (i,k,4)
!
! height level 4:  linear interpolation on inner two latitudes only
!
!!!       fintx(i,k,1,4) = not used
          fintx(i,k,2,4) = fb(ii2  ,kkp2,jj  ) * xl (i,k,2) 
     &                   + fb(ii2p1,kkp2,jj  ) * xr (i,k,2)
          fintx(i,k,3,4) = fb(ii3  ,kkp2,jjp1) * xl (i,k,3) 
     &                   + fb(ii3p1,kkp2,jjp1) * xr (i,k,3)
!!!       fintx(i,k,4,4) = not used 
!
! the following loop computes x-derivatives for those cases when the
! departure point lies in either the top or bottom interval of the
! model grid.  in this special case, data are shifted up or down to
! keep the departure point in the middle interval of the 4-point
! stencil. therefore, some derivatives that were computed above will
! be over-written.

! top interval
!
          if(kdp (i,k) == 1) then
!
! shift levels 4 and 2 data to levels 1 and 3, respectively
!
            fintx(i,k,2,1) = fintx(i,k,2,4)
            fintx(i,k,3,1) = fintx(i,k,3,4)
!
            fintx(i,k,1,3) = fintx(i,k,1,2)
            fintx(i,k,2,3) = fintx(i,k,2,2)
            fintx(i,k,3,3) = fintx(i,k,3,2)
            fintx(i,k,4,3) = fintx(i,k,4,2)
!
! height level 1 (placed in level 2 of stencil):
!
!   latitude 1:  linear interpolation
!
            fintx(i,k,1,2) = fb (ii1  ,1,jjm1)*xl (i,k,1)
     &                     + fb (ii1p1,1,jjm1)*xr (i,k,1)
!
!   latitude 2:  cubic interpolation
!
            fxl = (   - 2.*fb (ii2m1,1,jj  )
     &                - 3.*fb (ii2  ,1,jj  )
     &                + 6.*fb (ii2+1,1,jj  )
     &                -    fb (ii2+2,1,jj  ) )*rdlam6jj
            fxr = (        fb (ii2-1,1,jj  )
     &                - 6.*fb (ii2  ,1,jj  )
     &                + 3.*fb (ii2+1,1,jj  )
     &                + 2.*fb (ii2+2,1,jj  ) )*rdlam6jj
!
            deli = (       fb (ii2+1,1,jj  ) -
     &                      fb (ii2  ,1,jj  ) )*rdlamjj
            tmp1 = fac*deli
            tmp2 = abs( tmp1 )
            if( deli*fxl   <= 0.0 ) fxl = 0.
            if( deli*fxr   <= 0.0 ) fxr = 0.
            if( abs( fxl ) > tmp2 ) fxl = tmp1
            if( abs( fxr ) > tmp2 ) fxr = tmp1
!
            fintx(i,k,2,2) = fb (ii2  ,1,jj  )*hl (i,k,2)
     &                     + fb (ii2p1,1,jj  )*hr (i,k,2)
     &                     + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)

!   latitude 3:  cubic interpolation
!
            fxl = (   - 2.*fb (ii3m1,1,jjp1)
     &                - 3.*fb (ii3  ,1,jjp1)
     &                + 6.*fb (ii3p1,1,jjp1)
     &                -    fb (ii3p2,1,jjp1) )*rdlam6jjp1
            fxr = (        fb (ii3m1,1,jjp1)
     &                - 6.*fb (ii3  ,1,jjp1)
     &                + 3.*fb (ii3p1,1,jjp1)
     &                + 2.*fb (ii3p2,1,jjp1) )*rdlam6jjp1

            deli = (       fb (ii3p1,1,jjp1) -
     &                     fb (ii3  ,1,jjp1) )*rdlamjjp1
            tmp1 = fac*deli
            tmp2 = abs( tmp1 )
            if( deli*fxl   <= 0.0 ) fxl = 0.
            if( deli*fxr   <= 0.0 ) fxr = 0.
            if( abs( fxl ) > tmp2 ) fxl = tmp1
            if( abs( fxr ) > tmp2 ) fxr = tmp1
!
            fintx(i,k,3,2) = fb (ii3  ,1,jjp1)*hl (i,k,3)
     &                     + fb (ii3p1,1,jjp1)*hr (i,k,3)
     &                     + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
            fintx(i,k,4,2) = fb (ii4  ,1,jjp2)*xl (i,k,4)
     &                     + fb (ii4p1,1,jjp2)*xr (i,k,4)

! height level 3 (placed in level 4 of stencil):
!  linear interpolation on inner two latitudes only
!
!!!         fintx(i,k,1,4) = not used
            fintx(i,k,2,4) = fb (ii2  ,3,jj  )*xl (i,k,2)
     &                     + fb (ii2p1,3,jj  )*xr (i,k,2)
            fintx(i,k,3,4) = fb (ii3  ,3,jjp1)*xl (i,k,3)
     &                     + fb (ii3p1,3,jjp1)*xr (i,k,3)
!!!         fintx(i,k,4,4) = not used

! bot interval
!
          else if(kdp (i,k) .eq. kdimm1) then
!
! shift levels 1 and 3 data to levels 4 and 2, respectively
!
            fintx(i,k,2,4) = fintx(i,k,2,1)
            fintx(i,k,3,4) = fintx(i,k,3,1)
!
            fintx(i,k,1,2) = fintx(i,k,1,3)
            fintx(i,k,2,2) = fintx(i,k,2,3)
            fintx(i,k,3,2) = fintx(i,k,3,3)
            fintx(i,k,4,2) = fintx(i,k,4,3)

! height level 2 (placed in level 1 of stencil):
!  linear interpolation on inner two latitudes only
!
!!!         fintx(i,k,1,1) =  not used
            fintx(i,k,2,1) = fb (ii2  ,kdimm2,jj  )*xl (i,k,2)
     &                     + fb (ii2p1,kdimm2,jj  )*xr (i,k,2)
            fintx(i,k,3,1) = fb (ii3  ,kdimm2,jjp1)*xl (i,k,3)
     &                     + fb (ii3p1,kdimm2,jjp1)*xr (i,k,3)
!!!         fintx(i,k,4,1) =  not used
!
! height level 4 (placed in level 3 of stencil):
!
!   latitude 1:  linear interpolation
!
            fintx(i,k,1,3) = fb (ii1  ,kdim,jjm1)*xl (i,k,1)
     &                     + fb (ii1p1,kdim,jjm1)*xr (i,k,1)
!
!   latitude 2:  cubic interpolation
!
            fxl = (   - 2.*fb (ii2m1,kdim,jj  )
     &                - 3.*fb (ii2  ,kdim,jj  )
     &                + 6.*fb (ii2p1,kdim,jj  )
     &                -    fb (ii2p2,kdim,jj  ) )*rdlam6jj
            fxr = (        fb (ii2m1,kdim,jj  )
     &                - 6.*fb (ii2  ,kdim,jj  )
     &                + 3.*fb (ii2p1,kdim,jj  )
     &                + 2.*fb (ii2p2,kdim,jj  ) )*rdlam6jj
!
            deli = (       fb (ii2p1,kdim,jj  ) -
     &                     fb (ii2  ,kdim,jj  ) )*rdlamjj
            tmp1 = fac*deli
            tmp2 = abs( tmp1 )
            if( deli*fxl   <= 0.0 ) fxl = 0.
            if( deli*fxr   <= 0.0 ) fxr = 0.
            if( abs( fxl ) > tmp2 ) fxl = tmp1
            if( abs( fxr ) > tmp2 ) fxr = tmp1

            fintx(i,k,2,3) = fb (ii2  ,kdim,jj  )*hl (i,k,2)
     &                     + fb (ii2p1,kdim,jj  )*hr (i,k,2)
     &                     + fxl*dhl(i,k,2) + fxr*dhr(i,k,2)

!   latitude 3:  cubic interpolation
!
            fxl = (   - 2.*fb (ii3m1,kdim,jjp1)
     &                - 3.*fb (ii3  ,kdim,jjp1)
     &                + 6.*fb (ii3p1,kdim,jjp1)
     &                -    fb (ii3p2,kdim,jjp1) )*rdlam6jjp1
            fxr = (        fb (ii3m1,kdim,jjp1)
     &                - 6.*fb (ii3  ,kdim,jjp1)
     &                + 3.*fb (ii3p1,kdim,jjp1)
     &                + 2.*fb (ii3p2,kdim,jjp1) )*rdlam6jjp1
!
            deli = (       fb (ii3p1,kdim,jjp1) -
     &                     fb (ii3  ,kdim,jjp1) )*rdlamjjp1
            tmp1 = fac*deli
            tmp2 = abs( tmp1 )
            if( deli*fxl   <= 0.0 ) fxl = 0.
            if( deli*fxr   <= 0.0 ) fxr = 0.
            if( abs( fxl ) > tmp2 ) fxl = tmp1
            if( abs( fxr ) > tmp2 ) fxr = tmp1
!
            fintx(i,k,3,3) = fb (ii3  ,kdim,jjp1)*hl (i,k,3)
     &                     + fb (ii3p1,kdim,jjp1)*hr (i,k,3)
     &                     + fxl*dhl(i,k,3) + fxr*dhr(i,k,3)
!
!   latitude 4:  linear interpolation
!
            fintx(i,k,4,3) = fb (ii4  ,kdim,jjp2)*xl (i,k,4)
     &                     + fb (ii4p1,kdim,jjp2)*xr (i,k,4)
          endif
        end do
      end do
!
      return
      end
      subroutine heryin_h(i_1,i_2,jdp,ys,yn,hs,hn,dhs,dhn,dphii,
     &                    fintx,finty)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev
      use      slgshr        , only : lbasdy
      implicit none
!
!  input variables
      integer i_1,i_2
      integer,dimension(i_1:i_2,plev) :: jdp    ! j-index of departure point coord.
      real(kind=kind_evod),dimension(plev)             :: dphii
      real(kind=kind_evod),dimension(i_1:i_2,plev)     ::
     &                                ys,yn,hs,hn,dhs,dhn
!    &                                ys,yn,hs,hn,dhs,dhn,rdphi
      real(kind=kind_evod),dimension(i_1:i_2,plev,4)   :: finty
      real(kind=kind_evod),dimension(i_1:i_2,plev,4,4) :: fintx

! local variables
      integer i,k,kk,jj
      real(kind=kind_evod)                           :: fac,deli,tmp1,
     &                                                  tmp2
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: ftop,fbot
!
      fac  = 3.*(1. - 10.*epsilon(fac))           

!
! y derivatives at the inner height levels (kk = 2,3) needed for
! z-interpolation
!
      do k=1,plev
        do kk  = 2,3
          do i=i_1,i_2
            jj = jdp(i,k)
            fbot(i,k,kk) = lbasdy(1,1,jj) * fintx(i,k,1,kk) 
     &                   + lbasdy(2,1,jj) * fintx(i,k,2,kk) 
     &                   + lbasdy(3,1,jj) * fintx(i,k,3,kk) 
     &                   + lbasdy(4,1,jj) * fintx(i,k,4,kk)
            ftop(i,k,kk) = lbasdy(1,2,jj) * fintx(i,k,1,kk) 
     &                   + lbasdy(2,2,jj) * fintx(i,k,2,kk) 
     &                   + lbasdy(3,2,jj) * fintx(i,k,3,kk) 
     &                   + lbasdy(4,2,jj) * fintx(i,k,4,kk)
          enddo
        enddo
      enddo
!
! apply scm0 limiter to derivative estimates.
!
      do kk  = 2,3
        do k=1,plev
          do i=i_1,i_2
            jj = jdp(i,k)
            deli = ( fintx(i,k,3,kk) - fintx(i,k,2,kk) )*dphii(jj)
            tmp1 = fac*deli
            tmp2 = abs( tmp1 )
            if( deli*fbot(i,k,kk)   <= 0.0 ) fbot(i,k,kk) = 0.
            if( deli*ftop(i,k,kk)   <= 0.0 ) ftop(i,k,kk) = 0.
            if( abs( fbot(i,k,kk) ) > tmp2 ) fbot(i,k,kk) = tmp1
            if( abs( ftop(i,k,kk) ) > tmp2 ) ftop(i,k,kk) = tmp1
          enddo
        enddo
      enddo
!
! part 3:  y-interpolants
!
      do k=1,plev
        do i=i_1,i_2
          finty(i,k,1) = fintx(i,k,2,1) * ys (i,k) 
     &                 + fintx(i,k,3,1) * yn (i,k)

          finty(i,k,2) = fintx(i,k,2,2) * hs (i,k)
     &                 + fbot (i,k  ,2) * dhs(i,k)
     &                 + fintx(i,k,3,2) * hn (i,k)
     &                 + ftop (i,k  ,2) * dhn(i,k)

          finty(i,k,3) = fintx(i,k,2,3) * hs (i,k)
     &                 + fbot (i,k  ,3) * dhs(i,k)
     &                 + fintx(i,k,3,3) * hn (i,k)
     &                 + ftop (i,k  ,3) * dhn(i,k)

          finty(i,k,4) = fintx(i,k,2,4) * ys (i,k) 
     &                 + fintx(i,k,3,4) * yn (i,k)
        enddo
      enddo

      return
      end
      subroutine herzin_h(i_1,i_2,
     &                    kdim,finty,lbasdz,kdp,hb,ht,dhb,dht,detam,
     &                    term1,term2,term3,term4,fdp)
!
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon
      implicit none
!
!   input variables
      integer i_1,i_2,kdim
      integer,dimension(i_1:i_2,plev) :: kdp
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: finty
      real(kind=kind_evod),dimension(4,2,kdim)       :: lbasdz
      real(kind=kind_evod),dimension(plon,plev)      :: fdp
      real(kind=kind_evod),dimension(i_1:i_2,plev)   ::
     &                  hb,ht,dhb,dht,term1,term2,term3,term4
      real(kind=kind_evod),dimension(plev)           :: detam  ! delta eta at levels

!   local variables
      integer i,k,m,kdimm1,kdimm2,kk
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: ftop,fbot
      real(kind=kind_evod)    deli,tmp1,tmp2,fac
!
      kdimm1 = kdim - 1
      kdimm2 = kdim - 2
!
      do k = 1,plev
        do i = i_1,i_2
          kk = kdp (i,k)
          if(kk  == 1) then

             ftop(i,k,1) = (finty(i,k,3) - finty(i,k,2)) / detam(kk)
             fbot(i,k,1) = lbasdz(1,1,2) * finty(i,k,2)
     &                   + lbasdz(2,1,2) * finty(i,k,3)
     &                   + lbasdz(3,1,2) * finty(i,k,4)
     &                   + lbasdz(4,1,2) * finty(i,k,1)
          elseif(kk == kdimm1) then
             ftop(i,k,1) = lbasdz(1,2,kdimm2) * finty(i,k,4)
     &                   + lbasdz(2,2,kdimm2) * finty(i,k,1)
     &                   + lbasdz(3,2,kdimm2) * finty(i,k,2)
     &                   + lbasdz(4,2,kdimm2) * finty(i,k,3)
             fbot(i,k,1) = 0.0
          else
             ftop(i,k,1) = lbasdz(1,1,kk)*finty(i,k,1)
     &                   + lbasdz(2,1,kk)*finty(i,k,2)
     &                   + lbasdz(3,1,kk)*finty(i,k,3)
     &                   + lbasdz(4,1,kk)*finty(i,k,4)
             fbot(i,k,1) = lbasdz(1,2,kk)*finty(i,k,1)
     &                   + lbasdz(2,2,kk)*finty(i,k,2)
     &                   + lbasdz(3,2,kk)*finty(i,k,3)
     &                   + lbasdz(4,2,kk)*finty(i,k,4)

          endif
        enddo
      enddo

      fac  = 3.*(1. - 10.*epsilon(fac))
!     print*,' herzin_h fac = ',fac
      do k=1,plev
        do i = i_1,i_2
          deli = (finty(i,k,3) - finty(i,k,2)) / detam(kdp(i,k))
          tmp1 = fac*deli
          tmp2 = abs(tmp1)
          if( deli*fbot(i,k,1)   <= 0.0 ) fbot(i,k,1) = 0.
          if( deli*ftop(i,k,1)   <= 0.0 ) ftop(i,k,1) = 0.
          if( abs( fbot(i,k,1) ) > tmp2 ) fbot(i,k,1) = tmp1
          if( abs( ftop(i,k,1) ) > tmp2 ) ftop(i,k,1) = tmp1

          fdp(i,plevp-k) = finty(i,k,2)*ht(i,k) + ftop(i,k,1)*dht(i,k)
     &                   + finty(i,k,3)*hb(i,k) + fbot(i,k,1)*dhb(i,k)
        end do
      enddo

      return
      end
      subroutine linxyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                       fb1,idp,jdp,kdp,xl,xr,ys,yn,zb,zt,
     &                       fdp1)
!reduced from subroutine lin_xyz_in_h
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon,plond
      implicit none

!   input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp

      real(kind=kind_evod),dimension(i_1:i_2,plev)   :: ys,yn,zb,zt
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: xl,xr
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                               :: fb1
      real(kind=kind_evod),dimension(plon,plev)      :: fdp1

!   local variables
      integer i,k,ii2,ii3,jj,kk,kr,jjp1,kkp1
!
      do k=1,plev
        kr = plevp - k
        do i=i_1,i_2
          ii2  = idp(i,k,2)
          ii3  = idp(i,k,3)
          kk   = kdp(i,k)
          kkp1 = kk + 1
          jj   = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjp1 = min(jj+1,n_lat_in_pe)

          fdp1(i,kr) = (( fb1 (ii2  ,kk  ,jj  ) * xl (i,k,2)
     &               +    fb1 (ii2+1,kk  ,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb1 (ii3  ,kk  ,jjp1) * xl (i,k,3)
     &               +    fb1 (ii3+1,kk  ,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zt(i,k)
     &               + (( fb1 (ii2  ,kkp1,jj  ) * xl (i,k,2)
     &               +    fb1 (ii2+1,kkp1,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb1 (ii3  ,kkp1,jjp1) * xl (i,k,3)
     &               +    fb1 (ii3+1,kkp1,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zb(i,k)
        enddo
      enddo
      return
      end
      subroutine lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                        fb1,fb2,fb3,idp,jdp,kdp,xl,xr,ys,yn,zb,zt,
     &                        fdp1,fdp2,fdp3)
!my
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon,plond
      implicit none

!   input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                               :: fb1,fb2,fb3
      real(kind=kind_evod),dimension(plon,plev)      :: fdp1,fdp2,fdp3
      real(kind=kind_evod),dimension(i_1:i_2,plev)   :: ys,yn,zb,zt
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: xl,xr

!  local variables
      integer i,k,ii2,ii3,jj,kk,kr,ii2p1,ii3p1,jjp1,kkp1
!
!my  assumes lagrange or hermite interpolation performed on ug,vg,tg and 
!my  now add in the nonliner terms contribution using trilinear interpolation

      do k=1,plev
        kr = plevp - k
        do i=i_1,i_2
          ii2   = idp(i,k,2)
          ii2p1 = ii2 + 1
          ii3   = idp(i,k,3)
          ii3p1 = ii3 + 1
          kk    = kdp(i,k)
          kkp1  = kk + 1
          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjp1  = min(jj+1,n_lat_in_pe)

          fdp1(i,kr) = fdp1(i,kr)
     &               + (( fb1 (ii2  ,kk  ,jj  ) * xl (i,k,2)
     &               +    fb1 (ii2p1,kk  ,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb1 (ii3  ,kk  ,jjp1) * xl (i,k,3)
     &               +    fb1 (ii3p1,kk  ,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zt(i,k)
     &               + (( fb1 (ii2  ,kkp1,jj  ) * xl (i,k,2)
     &               +    fb1 (ii2p1,kkp1,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb1 (ii3  ,kkp1,jjp1) * xl (i,k,3)
     &               +    fb1 (ii3p1,kkp1,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zb(i,k)
!$$$       print*,
!$$$     . ' in lin_xyz_in_h i,k,i2,i3,kdp,jdp,xl2,xl3,xr,ys,yn,zt,zb = ',
!$$$     . i,k,idp(i,k,2),idp(i,k,3),kdp(i,k),jdp(i,k),xl(i,k,2),xl(i,k,3),
!$$$     . ys(i,k),yn(i,k),zt(i,k),zb(i,k)

          fdp2(i,kr) = fdp2(i,kr)
     &               + (( fb2 (ii2  ,kk  ,jj  )*xl (i,k,2)
     &               +    fb2 (ii2p1,kk  ,jj  )*xr (i,k,2) )*ys(i,k)
     &               +  ( fb2 (ii3  ,kk  ,jjp1)*xl (i,k,3)
     &               +    fb2 (ii3p1,kk  ,jjp1)*xr (i,k,3) )*yn(i,k) )
     &                                                        *zt(i,k)
     &               + (( fb2 (ii2  ,kkp1,jj  ) * xl (i,k,2)
     &               +    fb2 (ii2p1,kkp1,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb2 (ii3  ,kkp1,jjp1) * xl (i,k,3)
     &               +    fb2 (ii3p1,kkp1,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zb(i,k)

          fdp3(i,kr) = fdp3(i,kr)
     &               + (( fb3 (ii2  ,kk  ,jj  ) * xl (i,k,2)
     &               +    fb3 (ii2p1,kk  ,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb3 (ii3  ,kk  ,jjp1) * xl (i,k,3)
     &               +    fb3 (ii3p1,kk  ,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zt(i,k)
     &               + (( fb3 (ii2  ,kkp1,jj  ) * xl (i,k,2)
     &               +    fb3 (ii2p1,kkp1,jj  ) * xr (i,k,2) )*ys(i,k)
     &               +  ( fb3 (ii3  ,kkp1,jjp1) * xl (i,k,3)
     &               +    fb3 (ii3p1,kkp1,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                        *zb(i,k)
        enddo
      enddo

      return
      end
      subroutine lin_xy_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                       kdim,fb1,idp,jdp,x2,x3,yb,yt,fdp1)
!sk   a 2d analog of subroutine lin_xyz_in_h, created from int2d_h_lev
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plon,plond
      implicit none
!
!   input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe,kdim
      integer,dimension(i_1:i_2,plev)   :: jdp
      integer,dimension(i_1:i_2,plev,4) :: idp
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                               :: fb1
      real(kind=kind_evod),dimension(plon,plev)      :: fdp1
      real(kind=kind_evod),dimension(i_1:i_2,plev)   :: yb,yt
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: x2,x3

!   local variables
      integer i,k, kk,ii2,ii3,jj,jjp1
      real (kind=kind_evod) f2,f3
!
      do k=1,plev
        kk = plev+1-k
        do i=i_1,i_2
          ii2  = idp(i,k,2)
          ii3  = idp(i,k,3)
          jj   = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjp1 = min(jj+1,n_lat_in_pe)

          f2 = - fb1 (ii2  ,k,jj  ) * x3(i,k,2)
     &         + fb1 (ii2+1,k,jj  ) * x2(i,k,2)
          f3 = - fb1 (ii3  ,k,jjp1) * x3(i,k,3)
     &         + fb1 (ii3+1,k,jjp1) * x2(i,k,3)
!
          fdp1(i,kk) = fdp1(i,kk) + f2*yb(i,k)+ f3*yt(i,k)
        end do
      end do
      return
      end
      subroutine quasim(f1,f2,fint)
!sk   quasi-monotone correction of fint 
      use machine            , only : kind_evod
      implicit none
!   input variables
      real(kind=kind_evod) :: f1,f2,fint

!   local variables
      real(kind=kind_evod) :: fmin,fmax

!     real(kind=kind_evod),dimension(2) :: f

!     f(1) = f1
!     f(2) = f2
!     fmax = maxval(f)
!     fmin = minval(f)
!     if (fint < fmin) fint = fmin
!     if (fint > fmax) fint = fmax

      fmax = max(f1,f2)
      fmin = min(f1,f2)
      fint = max(fmin,min(fint,fmax))
      return
      end
      subroutine wgt_cub_lin_xyz_in_h(i_1,i_2,s_lat_in_pe,n_lat_in_pe,
     &                                fb1,idp,jdp,kdp,xl,xr,ys,yn,zb,zt,
     &                                fdp1)
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon,plond,wgt
      implicit none

!  input variables
      integer i_1,i_2,s_lat_in_pe,n_lat_in_pe
      integer,dimension(i_1:i_2,plev)   :: jdp,kdp
      integer,dimension(i_1:i_2,plev,4) :: idp
      real(kind=kind_evod),dimension(plond,plev,s_lat_in_pe:n_lat_in_pe)
     &                                               :: fb1
      real(kind=kind_evod),dimension(plon,plev)      :: fdp1
      real(kind=kind_evod),dimension(i_1:i_2,plev)   :: ys,yn,zt,zb
      real(kind=kind_evod),dimension(i_1:i_2,plev,4) :: xl,xr

!   local variables
      integer i,k,ii2,ii3,jj,kk,kr,ii2p1,ii3p1,jjp1,kkp1

!     real wgt(plev)
!     real alphau,alphav,alphat
!     parameter (alphau=0.3,alphav=0.3,alphat=0.3)
!
!my  copy of lin_xyz_in_h for one variable with weights added
!my  
!     do k = 1,34
!       wgt(k) = 0.1
!     enddo
!     do k = 35,43
!       wgt(k) = 0.5
!     enddo
!     do k = 44,plev
!       wgt(k) = 1.0
!     enddo

      do k=1,plev
        kr = plevp - k
        do i=i_1,i_2
          ii2   = idp(i,k,2)
          ii2p1 = ii2 + 1
          ii3   = idp(i,k,3)
          ii3p1 = ii3 + 1
          kk    = kdp(i,k)
          kkp1  = kk + 1
          jj    = max(s_lat_in_pe, min(jdp(i,k),n_lat_in_pe))
          jjp1  = min(jj+1,n_lat_in_pe)

          fdp1(i,kr) = wgt(k)       * fdp1(i,kr)
     &               + (1.0-wgt(k)) * (
     &    (( fb1 (ii2  ,kk  ,jj  ) * xl (i,k,2)
     &  +    fb1 (ii2p1,kk  ,jj  ) * xr (i,k,2) )*ys(i,k)
     &  +  ( fb1 (ii3  ,kk  ,jjp1) * xl (i,k,3)
     &  +    fb1 (ii3p1,kk  ,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                       * zt(i,k)
     &  + (( fb1 (ii2  ,kkp1,jj  ) * xl (i,k,2)
     &  +    fb1 (ii2p1,kkp1,jj  ) * xr (i,k,2) )*ys(i,k)
     &  +  ( fb1 (ii3  ,kkp1,jjp1) * xl (i,k,3)
     &  +    fb1 (ii3p1,kkp1,jjp1) * xr (i,k,3) )*yn(i,k) )
     &                                                       * zb(i,k)
     &                                            )
        enddo
      enddo
      return
      end
      subroutine lagzin_h_m(i_1,i_2,kdim,finty,lbasdz,kdp,hb,ht,
     &                      dhb,dht,detam,term1,term2,term3,term4,fdp,
     &                      quamon)
!cmy renamed with _m
!sk linear interpolation at the top and bottom
!sk 2/25/2012
!sk modified original subroutine lagzin_h for quasi-monotone
!sk quasi-cubic lagrange interpolation
!sk 10/08/2012
!sk added quamon to argument list
      use machine            , only : kind_evod
      use      pmgrid        , only : plev,plevp,plon
      implicit none

!   input variables

      integer i_1,i_2,kdim
      integer,dimension(i_1:i_2,plev)   :: kdp

      real(kind=kind_evod),dimension(i_1:i_2,plev,4) ::finty
      real(kind=kind_evod),dimension(4,2,kdim)       ::lbasdz
      real(kind=kind_evod),dimension(plon,plev)      ::fdp

      real(kind=kind_evod),dimension(plev)         :: detam  ! delta eta at levels
      real(kind=kind_evod),dimension(i_1:i_2,plev) :: ht,hb,dht,dhb,
     &                                                term1,term2,
     &                                                term3,term4
      logical quamon

!   local variables

      integer i,k,m,kdimm1,kk
      real(kind=kind_evod) tem1, tem2
!
      kdimm1 = kdim - 1
      do k = 1,plev
        kk = plevp - k
        do i = i_1,i_2
          if(kdp (i,k) == 1) then
            fdp(i,kk) = finty(i,k,2)*ht(i,k) + finty(i,k,3)*hb(i,k)
          elseif(kdp (i,k) == kdimm1) then
            fdp(i,kk) = finty(i,k,2)*ht(i,k) + finty(i,k,3)*hb(i,k)
          else
            fdp(i,kk) = finty(i,k,1)*term1(i,k)
     &                + finty(i,k,2)*term2(i,k)
     &                + finty(i,k,3)*term3(i,k)
     &                + finty(i,k,4)*term4(i,k)
            if (quamon) then
!             call quasim(finty(i,k,2),finty(i,k,3),fdp(i,plev-k))

              tem1 = min(finty(i,k,2),finty(i,k,3))
              tem2 = max(finty(i,k,2),finty(i,k,3))
              fdp(i,kk) = max(tem1,min(tem2,fdp(i,kk)))
            endif
          endif
        end do
      end do
      return
      end
      subroutine lagzinit_h_n(i_1,i_2,kdim,lbasiz,
     &                        etamid,detam,sigdp,kdp,kkdp,
     &                        hb,ht,dhb,dht,zb,zt,
     &                        term1z,term2z,term3z,term4z)
!sk linear-interpolation weights if dp falls between layers 1 and 2
!sk or layers k-1 and k.
!sk 10/08/2012
!sk renamed lagzinit_h_m as lagzinit_h_n to avoid confusion with _m
!sk that means quasi-monotone version.
      use machine            , only : kind_evod
      use      pmgrid        , only : plev
      implicit none

!  input variables
      integer i_1,i_2,kdim
      integer,dimension(i_1:i_2,plev) :: kdp,kkdp
      real(kind=kind_evod),dimension(4,2,kdim)     :: lbasiz
      real(kind=kind_evod),dimension(kdim)         :: etamid,detam
      real(kind=kind_evod),dimension(i_1:i_2,plev) :: sigdp,
     &     hb,ht,dhb,dht,zb,zt,term1z,term2z,term3z,term4z

!   local variables
      integer i,k,m,ktem
      real(kind=kind_evod) :: dzk,
     &                        zmz1,                 ! |
     &                        zmz2,                 ! |
     &                        zmz3,                 ! |
     &                        zmz4,                 ! |
     &                        coef12,               ! |
     &                        coef34,               ! | -- interpolation weights/coeffs.
     &                        zt1, zb1, zt2, zb2, stem

      do k=1,plev
        do i=i_1,i_2
          ktem        = kdp(i,k)
          dzk         = detam(ktem)
          stem        = sigdp(i,k)
          zt1        = ( etamid(ktem+1) - stem ) / dzk
          zb1        = 1.0 - zt1
          zt(i,k)    = zt1
          zb(i,k)    = zb1
          zt2        = zt1 * zt1
          zb2        = zb1 * zb1

          ht(i,k)    = ( 3.0 - 2.0*zt1 ) * zt2     ! ???????????????
          hb(i,k)    = ( 3.0 - 2.0*zb1 ) * zb2     ! ???????????????

          dht(i,k)   = -dzk * (zt1 - 1.) * zt2
          dhb(i,k)   =  dzk * (zb1 - 1.) * zb2
!skar 12/23/2011
          ht(i,k)    = zt1
          hb(i,k)    = zb1
!skar
          ktem        = kkdp(i,k)
          zmz1        = stem - lbasiz(1,1,ktem)
          zmz2        = stem - lbasiz(2,1,ktem)
          zmz3        = stem - lbasiz(3,1,ktem)
          zmz4        = stem - lbasiz(4,1,ktem)

          coef12      = zmz3 * zmz4
          coef34      = zmz1 * zmz2

          term1z(i,k) = coef12 * zmz2 * lbasiz(1,2,ktem)
          term2z(i,k) = coef12 * zmz1 * lbasiz(2,2,ktem)
          term3z(i,k) = coef34 * zmz4 * lbasiz(3,2,ktem)
          term4z(i,k) = coef34 * zmz3 * lbasiz(4,2,ktem)
        end do
      end do
      return
      end
