      subroutine ndslfv_ppm_advecth1(wind,grid,tend,ddpt,dppt,
     &                    global_lats_a,lonsperlat,deltim,kdt)
!
! a routine to do non-iteration semi-lagrangain advection
! considering advection  with monotonicity in interpolation
! contact: hann-ming henry juang
! program log
! 2011 02 20 : henry juang, created for ndsl advection
!
      use resol_def
      use layout1
      use vert_def
      use coordinate_def
      use physcons
      use mpi_def
      implicit none

      real(kind=kind_evod) wind(lonf,lots,lats_dim_a)
      real(kind=kind_evod) grid(lonf,lota,lats_dim_a)
      real(kind=kind_evod) tend(lonf,lota,lats_dim_a)
      real(kind=kind_evod) ddpt(lonf,levs,lats_dim_a)
      real(kind=kind_evod) dppt(lonf,levs,lats_dim_a)

      real(kind=kind_evod) plev(lonfull,levs+1)
      real(kind=kind_evod) tki (lonfull,levs+1)
      integer,intent(in):: global_lats_a(latg)
      integer,intent(in):: lonsperlat(latg),kdt
      real,   intent(in):: deltim
   
      integer, parameter :: nvars=8	! u v t dp p2 tracers
!     integer, parameter :: nvars=5	! u v t dp p2 tracers

      real	uulon(lonfull,levs,latpart)
      real	vvlon(lonfull,levs,latpart)
      real	qqlon(lonfull,levs*nvars,latpart)

      real	vvlat(latfull,levs,lonpart)
      real	qqlat(latfull,levs*nvars,lonpart)
      real      rma, rm2a, rdt2, rkappa, tkrt0

      logical 	lprint, lpair1, lpair2

      integer mono,mass
      integer nlevs
      integer i,k,lon,lan,lat,lons_lat
      integer ksu,ksv
      integer kau,kav,kat,kar,kap
      integer kp2, kdp, kqq, ktt, kuu, kvv
      integer k2 , kp , kq , kt , ku , kv
      integer kpg, kqg, ktg, kug, kvg
!
      lprint = .false.
      if( mod(kdt,2) .eq. 0 ) then
        lpair1 = .true.
        lpair2 = .false.
      else
        lpair1 = .false.
        lpair2 = .true.
      endif
!
!     print *,' enter ndsl_monoadvh '
!
      mono  = 1
      mass  = 0
! ------- from gloopa
      ksu     =3*levs+0*levh+4
      ksv     =4*levs+0*levh+4
! ------- from gloopa
      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kap     =3*levs+0*levh+1
      kar     =3*levs+0*levh+2

!     print *,' ===== check kap =',kap
!
! ------- local required
      kuu = 1
      kvv = kuu + levs
      ktt = kvv + levs
      kdp = ktt + levs
      kp2 = kdp + levs
      kqq = kp2 + levs

!     print *,' ===== check length for qqlon ',kqq+levh-1

      nlevs = nvars * levs

!     print *,' ===== check nlevs ',nlevs
!
      rkappa = con_cp / con_rd
      rdt2 = 0.5 / deltim      
!
! =================================================================
!   prepare wind and variable in flux form with gaussina weight
! =================================================================
!
!$omp parallel do schedule(dynamic,1) private(lan)
!$omp+private(lat,lons_lat,rma,rm2a,i,k)
!$omp+private(tki,tkrt0,plev)
!$omp+private(kug,kvg,ktg,kpg,kqg)
!$omp+private(ku ,kv ,kt ,kp ,k2 ,kq )

      do lan=1,lats_node_a

        lat = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlat(lat)
        rma  = 1. / cosglat(lat) / con_rerth
        rm2a = rma / cosglat(lat)
!
! wind at time step n (distance counted by angles)
        do k=1,levs
          do i=1,lons_lat
            uulon(i,k,lan) = wind(i,ksu -1+k,lan) * rm2a
            vvlon(i,k,lan) = wind(i,ksv -1+k,lan) * rma
          enddo
        enddo
        if( lprint ) then
          call mymaxmin(uulon(1,1,lan),lons_lat,lonfull,1,' uu in deg')
          call mymaxmin(vvlon(1,1,lan),lons_lat,lonfull,1,' vv in deg')
        endif
!
! u, v, t at n-1 (advect real wind, not virtual wind)
        do k=1,levs
          ku=kuu+k-1
          kv=kvv+k-1
          kt=ktt+k-1
          kug = kau + k - 1
          kvg = kav + k - 1
          ktg = kat + k - 1
          do i=1,lons_lat
            qqlon(i,ku,lan) = grid(i,kug,lan) / cosglat(lat)
            qqlon(i,kv,lan) = grid(i,kvg,lan) / cosglat(lat)
            qqlon(i,kt,lan) = grid(i,ktg,lan)
          enddo
        enddo
! rq at n-1
        do k=1,levh
          kq=kqq+k-1
          kqg = kar + k - 1
          do i=1,lons_lat
            qqlon(i,kq,lan) = grid(i,kqg,lan)
          enddo
        enddo
        if( lprint ) then
          call mymaxmin(qqlon(1,ktt,lan),lons_lat,lonfull,1,' tt4p ')
        endif
!
! compute p at level
        tki(:,1) = 0.0
        tki(:,levs+1) = 0.0
        do k=2,levs
          kt=ktt+k-1
          do i=1,lons_lat
            tkrt0 = (qqlon(i,kt-1,lan)+qqlon(i,kt,lan))
     &             /(thref(k-1)+thref(k))
            tki (i,k)=ck5(k)*tkrt0**rkappa
          enddo
        enddo
        do k=1,levs+1
          do i=1,lons_lat
            plev(i,k)  =ak5(k)+bk5(k)*grid(i,kap,lan)+tki(i,k)
          enddo
        enddo
        plev(:,levs+1) = 0.0
        if( lprint ) then
          call mymaxmin(plev(1,1),lons_lat,lonfull,1,' plev ')
        endif
!
! p and dp at n-1
        do k=1,levs
          kp=kdp+k-1
          k2=kp2+k-1
          kpg = k
          do i=1,lons_lat
            qqlon(i,kp,lan) = plev(i,k)-plev(i,k+1)
            qqlon(i,k2,lan) = plev(i,k)+plev(i,k+1)
            ddpt(i,kpg,lan) = qqlon(i,kp,lan)
            dppt(i,kpg,lan) = qqlon(i,k2,lan)
          enddo
        enddo
!
        if( lpair1 ) then 
!
! first set positive advection in east-west direction 
!
        call cyclic_ppm_massadvx1(lons_lat,lonfull,levs,nvars,deltim,        &
     &                   uulon(1,1,lan),qqlon(1,1,lan),mass)

        if( lprint ) then
        print *,' done cyclic_massadvx with mass= ',mass
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kuu,lan),lons_lat,lonfull,1,' advx u ')
        call mymaxmin(qqlon(1,kvv,lan),lons_lat,lonfull,1,' advx v ')
        call mymaxmin(qqlon(1,ktt,lan),lons_lat,lonfull,1,' advx t ')
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' advx q ')
        call mymaxmin(qqlon(1,kdp,lan),lons_lat,lonfull,1,' advx dp ')
        call mymaxmin(qqlon(1,kp2,lan),lons_lat,lonfull,1,' advx p2 ')
        print *,' done the first x adv for lan=',lan
        print *,' =========================================== '
        endif
 
        endif	! lpair1

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kuu,lan),lons_lat,lonfull,1,' redu u ')
        call mymaxmin(qqlon(1,kvv,lan),lons_lat,lonfull,1,' redu v ')
        call mymaxmin(qqlon(1,ktt,lan),lons_lat,lonfull,1,' redu t ')
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' redu q ')
        call mymaxmin(qqlon(1,kdp,lan),lons_lat,lonfull,1,' redu dp ')
        call mymaxmin(qqlon(1,kp2,lan),lons_lat,lonfull,1,' redu p2 ')
        endif
! change from reduced to full grids.
        call cyclic_ppm_intpx(levs,lons_lat,lonf,vvlon(1,1,lan))
        call cyclic_ppm_intpx(nlevs,lons_lat,lonf,qqlon(1,1,lan))

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kuu,lan),lonfull,lonfull,1,' full u ')
        call mymaxmin(qqlon(1,kvv,lan),lonfull,lonfull,1,' full v ')
        call mymaxmin(qqlon(1,ktt,lan),lonfull,lonfull,1,' full t ')
        call mymaxmin(qqlon(1,kqq,lan),lonfull,lonfull,1,' full q ')
        call mymaxmin(qqlon(1,kdp,lan),lonfull,lonfull,1,' full dp ')
        call mymaxmin(qqlon(1,kp2,lan),lonfull,lonfull,1,' full p2 ')
        endif

      enddo

! ---------------------------------------------------------------------
! mpi para from east-west full grid to north-south full grid
! ---------------------------------------------------------------------
!

!      print *,' ndsl_advect transport from we to ns '
       call para_we2ns(vvlon,vvlat,levs,global_lats_a,latg)
       call para_we2ns(qqlon,qqlat,nlevs,global_lats_a,latg)

       if( lprint ) then
       print *,' ------------ after we2ns ---------------------- '
       do lon=1,mylonlen
        print *,'  lon=',lon
        call mymaxmin(vvlat(1,1  ,lon),latfull,latfull,1,' we2ns v')
        call mymaxmin(qqlat(1,kuu,lon),latfull,latfull,1,' we2ns u')
        call mymaxmin(qqlat(1,ktt,lon),latfull,latfull,1,' we2ns t')
        call mymaxmin(qqlat(1,kqq,lon),latfull,latfull,1,' we2ns q')
       enddo
       endif
!
! ---------------------------------------------------------------------
! -------------- in north-soutn great circle -------------------
! ---------------------------------------------------------------------

!      print *,' ndsl_advect adv loop in y '
!      print *,' mylonlen=',mylonlen

!$omp parallel do schedule(dynamic,1) private(lon)
       do lon=1,mylonlen
! 
!       print *,' lon=',lon

        call cyclic_ppm_massadvy(latfull,levs,nvars,deltim,
     &                   vvlat(1,1,lon),qqlat(1,1,lon),mass)

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlat(1,kuu,lon),latfull,latfull,1,' advqy u ')
        call mymaxmin(qqlat(1,kvv,lon),latfull,latfull,1,' advqy v ')
        call mymaxmin(qqlat(1,ktt,lon),latfull,latfull,1,' advqy t ')
        call mymaxmin(qqlat(1,kqq,lon),latfull,latfull,1,' advqy q ')
        call mymaxmin(qqlat(1,kdp,lon),latfull,latfull,1,' advqy dp ')
        call mymaxmin(qqlat(1,kp2,lon),latfull,latfull,1,' advqy p2 ')
        print *,' done with y at lon=',lon
        endif
 
       enddo
!
! ----------------------------------------------------------------------
! mpi para from north-south direction to east-west direeectory 
! ----------------------------------------------------------------------
!
!      print *,' ndsl_advect transport from ns to we '
       call para_ns2we(qqlat,qqlon,nlevs,global_lats_a,latg)

       if( lprint ) then
       print *,' ------------ after ns2we ---------------------- '
       do lan=1,lats_node_a
        print *,'  lan=',lan
        call mymaxmin(qqlon(1,kqq,lan),lonfull,lonfull,1,' ns2we q')
       enddo
       endif

! ---------------------------------------------------------------
! ---------------- back to east-west direction ------------------
! ---------------------------------------------------------------
!      print *,' ndsl_advect adv loop in x for last '

!$omp parallel do schedule(dynamic,1) private(lan)
!$omp+private(lat,lons_lat,i,k)
!$omp+private(kug,kvg,ktg,kpg,kqg)
!$omp+private(ku ,kv ,kt ,kp ,k2 ,kq )

      do lan=1,lats_node_a

        lat = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlat(lat)

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kuu,lan),lonfull,lonfull,1,' do full u ')
        call mymaxmin(qqlon(1,kvv,lan),lonfull,lonfull,1,' do full v ')
        call mymaxmin(qqlon(1,ktt,lan),lonfull,lonfull,1,' do full t ')
        call mymaxmin(qqlon(1,kqq,lan),lonfull,lonfull,1,' do full q ')
        call mymaxmin(qqlon(1,kdp,lan),lonfull,lonfull,1,' do full dp')
        call mymaxmin(qqlon(1,kp2,lan),lonfull,lonfull,1,' do full p2')
        endif
 
! mass conserving interpolation from full grid to reduced grid
!
        call cyclic_ppm_intpx(nlevs,lonf,lons_lat,qqlon(1,1,lan))

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kuu,lan),lons_lat,lonfull,1,' do redu u ')
        call mymaxmin(qqlon(1,kvv,lan),lons_lat,lonfull,1,' do redu v ')
        call mymaxmin(qqlon(1,ktt,lan),lons_lat,lonfull,1,' do redu t ')
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' do redu q ')
        call mymaxmin(qqlon(1,kdp,lan),lons_lat,lonfull,1,' do redu dp')
        call mymaxmin(qqlon(1,kp2,lan),lons_lat,lonfull,1,' do redu p2')
        endif

!
! second set advection in x for the second of the pair
!
        if( lpair2 ) then

        call cyclic_ppm_massadvx1(lons_lat,lonfull,levs,nvars,deltim,         &
     &                   uulon(1,1,lan),qqlon(1,1,lan),mass)

        if( lprint ) then
        print *,' ------------------------------------------- '
        call mymaxmin(qqlon(1,kqq,lan),lons_lat,lonfull,1,' adv x q ')
        endif

        endif	! lpair2

! u v h dp tendency at n
        do k=1,levs
          ku=kuu+k-1
          kv=kvv+k-1
          kt=ktt+k-1
          kp=kdp+k-1
          k2=kp2+k-1
          kug= kau + k - 1
          kvg= kav + k - 1
          ktg= kat + k - 1
          kpg= k
          do i=1,lons_lat
            qqlon(i,ku,lan) =   qqlon(i,ku,lan) * cosglat(lat)
            qqlon(i,kv,lan) =   qqlon(i,kv,lan) * cosglat(lat)
            tend(i,kug,lan) = ( qqlon(i,ku,lan)-grid(i,kug,lan) )*rdt2
            tend(i,kvg,lan) = ( qqlon(i,kv,lan)-grid(i,kvg,lan) )*rdt2
            tend(i,ktg,lan) = ( qqlon(i,kt,lan)-grid(i,ktg,lan) )*rdt2
            ddpt(i,kpg,lan) = ( qqlon(i,kp,lan)-ddpt(i,kpg,lan) )*rdt2
            dppt(i,kpg,lan) = ( qqlon(i,k2,lan)-dppt(i,kpg,lan) )*rdt2
          enddo
        enddo
! rq update
        do k=1,levh
          kq=kqq+k-1
          kqg=kar + k - 1
          do i=1,lons_lat
            tend(i,kqg,lan) = ( qqlon(i,kq,lan)-grid(i,kqg,lan) )*rdt2
          enddo
        enddo

        if( lprint ) then
        print *,' finish horizonatal advection at lan=',lan
        endif
!
      enddo

! 
! ===============================
!
      return
      end
