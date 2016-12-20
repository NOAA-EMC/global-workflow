      subroutine gloopa
     x    (deltim,trie_ls,trio_ls,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_a,global_lats_a,
     x     lonsperlat,
     x     epse,epso,epsedn,epsodn,
     x     snnp1ev,snnp1od,ndexev,ndexod,
     x     plnev_a,plnod_a,pddev_a,pddod_a,plnew_a,plnow_a,
     x     dyn_f3d,dyn_f2d,global_times_a,kdt)
!
!  march 8, 2011    h. juang    add ndsl option 
!  september 2013   f. yang     add tvd restart capability
c
!
      use machine      , only : kind_evod
      use resol_def    , only : jcap,jcap1,latg,latg2,
     &                          levh,levs,lonf,lota,lotd,lots,
     &                          p_di,p_dlam,p_dphi,p_gz,p_q,
     &                          p_rm,p_rq,p_rt,p_te,p_uln,p_vln,
     &                          p_w,p_x,p_ze,p_zem,
     &                          p_tem,p_y,p_qm,p_zq,p_dim,
     &                          thermodyn_id, lonfx, lonr,ntrac,
     &                          num_a3d,num_a2d
      use layout1      , only : ipt_lats_node_a,lat1s_a,lats_dim_a,
     &                          lats_node_a,len_trie_ls,len_trio_ls,
     &                          lon_dim_a,ls_dim,ls_max_node,me,nodes,
     &                          lats_node_r
      use gg_def       , only : rcs2_a
      use vert_def     , only : ci,del,rdel2,sl,tov
      use date_def     , only : spdmax
      use namelist_def , only : gen_coord_hybrid,hybrid,explicit,ndsl,
     &                          n3dflxtvd,ngptc
      use mpi_def      , only : kind_mpi,mc_comp,mpi_r_mpi,mpi_real8
     &,                         comp_task
      use physcons     , only : rerth => con_rerth
      implicit none
!
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) triemls(len_trie_ls,2, 3*levs+1*levh+1)
      real(kind=kind_evod) triomls(len_trio_ls,2, 3*levs+1*levh+1)
!
      integer              ls_node(ls_dim,3)
!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
!
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_a(nodes)
!
!timers______________________________________________________---
 
!     real*8 rtc ,timer1,timer2
      real(kind=kind_evod) global_times_a(latg,nodes)
 
!timers______________________________________________________---
 
      integer              global_lats_a(latg)
      integer                 lonsperlat(latg)
!
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
!
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
!
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
!
      real(kind=kind_evod)   plnev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnod_a(len_trio_ls,latg2)
      real(kind=kind_evod)   pddev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   pddod_a(len_trio_ls,latg2)
      real(kind=kind_evod)   plnew_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnow_a(len_trio_ls,latg2)
!
c$$$      integer                lots,lotd,lota
c$$$!
c$$$      parameter            ( lots = 5*levs+1*levh+3 )
c$$$      parameter            ( lotd = 6*levs+2*levh+0 )
c$$$      parameter            ( lota = 3*levs+1*levh+1 )
!
      real(kind=kind_evod) for_gr_a_1(lon_dim_a,lots,lats_dim_a)
      real(kind=kind_evod) dyn_gr_a_1(lon_dim_a,lotd,lats_dim_a)
      real(kind=kind_evod) sym_gr_a_1(lon_dim_a,lota,lats_dim_a)
      real(kind=kind_evod) bak_gr_a_1(lon_dim_a,lota,lats_dim_a)
!
!mjr  real(kind=kind_evod) for_gr_a_2(lon_dim_a,lots,lats_dim_a)
!mjr  real(kind=kind_evod) dyn_gr_a_2(lon_dim_a,lotd,lats_dim_a)
!mjr  real(kind=kind_evod) bak_gr_a_2(lon_dim_a,lota,lats_dim_a)
!
      real(kind=kind_evod) for_gr_a_2(lonf,lots,lats_dim_a)
      real(kind=kind_evod) dyn_gr_a_2(lonf,lotd,lats_dim_a)
      real(kind=kind_evod) sym_gr_a_2(lonf,lota,lats_dim_a)
      real(kind=kind_evod) bak_gr_a_2(lonf,lota,lats_dim_a)
!
      real(kind=kind_evod) dpt_gr_a_2(lonf,levs,lats_dim_a)
      real(kind=kind_evod) ppt_gr_a_2(lonf,levs,lats_dim_a)

      real (kind=kind_evod)
     &     dyn_f3d(lonf,levs,num_a3d,lats_node_a),
     &     dyn_f2d(lonf,num_a2d,lats_node_a)

      integer              i,ierr,j,k,kap,kar,kat,kau,kav,kdrlam
      integer              kdrphi,kdtlam,kdtphi,kdulam,kduphi,kdvlam
      integer              kdvphi,ksd,ksplam,kspphi,ksq,ksr,kst
      integer              ksu,ksv,ksz,l,lan,lat,lmax,locl
!mjr  integer              lon_dim,lons_lat,n,node,nvcn,ii
      integer                      lons_lat,n,node,nvcn
!
      integer              ipt_ls
!
      real(kind=kind_evod) deltim
!
      real(kind=kind_evod) xvcn, rcs2loc
!
      integer              iter_max
!
      real(kind=kind_evod) ,allocatable :: spdlat(:,:)
!
      real(kind=kind_evod) spdmax_node (levs)
      real(kind=kind_mpi) spdmax_nodem (levs)
      real(kind=kind_evod) spdmax_nodes(levs,nodes)
      real(kind=kind_mpi) spdmax_nodesm(levs,nodes)
!
      real(kind=kind_evod) reall
      real(kind=kind_evod) rlcs2(jcap1)
!
!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
! ................................................................
!   syn(1, 0*levs+0*levh+1, lan)  ze
!   syn(1, 1*levs+0*levh+1, lan)  di
!   syn(1, 2*levs+0*levh+1, lan)  te
!   syn(1, 3*levs+0*levh+1, lan)  q
!   syn(1, 3*levs+0*levh+2, lan)  dpdlam
!   syn(1, 3*levs+0*levh+3, lan)  dpdphi
!   syn(1, 3*levs+0*levh+4, lan)  uln
!   syn(1, 4*levs+0*levh+4, lan)  vln
!   syn(1, 5*levs+0*levh+4, lan)  rq
! ................................................................
!   dyn(1, 0*levs+0*levh+1, lan)  d(t)/d(phi)
!   dyn(1, 1*levs+0*levh+1, lan)  d(rq)/d(phi)
!   dyn(1, 1*levs+1*levh+1, lan)  d(t)/d(lam)
!   dyn(1, 2*levs+1*levh+1, lan)  d(rq)/d(lam)
!   dyn(1, 2*levs+2*levh+1, lan)  d(u)/d(lam)
!   dyn(1, 3*levs+2*levh+1, lan)  d(v)/d(lam)
!   dyn(1, 4*levs+2*levh+1, lan)  d(u)/d(phi)
!   dyn(1, 5*levs+2*levh+1, lan)  d(v)/d(phi)
! ................................................................
!   anl(1, 0*levs+0*levh+1, lan)  w     dudt
!   anl(1, 1*levs+0*levh+1, lan)  x     dvdt
!   anl(1, 2*levs+0*levh+1, lan)  y     dtdt
!   anl(1, 3*levs+0*levh+1, lan)  z     dqdt
!   anl(1, 3*levs+0*levh+2, lan)  rt    drdt
! ................................................................
!
!
c$$$      parameter(ksz     =0*levs+0*levh+1,
c$$$     x          ksd     =1*levs+0*levh+1,
c$$$     x          kst     =2*levs+0*levh+1,
c$$$     x          ksq     =3*levs+0*levh+1,
c$$$     x          ksplam  =3*levs+0*levh+2,
c$$$     x          kspphi  =3*levs+0*levh+3,
c$$$     x          ksu     =3*levs+0*levh+4,
c$$$     x          ksv     =4*levs+0*levh+4,
c$$$     x          ksr     =5*levs+0*levh+1)
c$$$!
c$$$      parameter(kdtphi  =0*levs+0*levh+1,
c$$$     x          kdrphi  =1*levs+0*levh+1,
c$$$     x          kdtlam  =1*levs+1*levh+1,
c$$$     x          kdrlam  =2*levs+1*levh+1,
c$$$     x          kdulam  =2*levs+2*levh+1,
c$$$     x          kdvlam  =3*levs+2*levh+1,
c$$$     x          kduphi  =4*levs+2*levh+1,
c$$$     x          kdvphi  =5*levs+2*levh+1)
c$$$!
c$$$      parameter(kau     =0*levs+0*levh+1,
c$$$     x          kav     =1*levs+0*levh+1,
c$$$     x          kat     =2*levs+0*levh+1,
c$$$     x          kap     =3*levs+0*levh+1,
c$$$     x          kar     =3*levs+0*levh+2)
!
!
c$$$      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
c$$$      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
c$$$      integer   p_w,p_x,p_y,p_rt,p_zq
c$$$!
c$$$!                                                old common /comfspec/
c$$$      parameter(p_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
c$$$     x          p_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
c$$$     x          p_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
c$$$     x          p_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
c$$$     x          p_qm  = 3*levs+0*levh+2,  !      qme/o(lnte/od,2),
c$$$     x          p_ze  = 3*levs+0*levh+3,  !      zee/o(lnte/od,2,levs),
c$$$     x          p_di  = 4*levs+0*levh+3,  !      die/o(lnte/od,2,levs),
c$$$     x          p_te  = 5*levs+0*levh+3,  !      tee/o(lnte/od,2,levs),
c$$$     x          p_q   = 6*levs+0*levh+3,  !       qe/o(lnte/od,2),
c$$$     x          p_dlam= 6*levs+0*levh+4,  !  dpdlame/o(lnte/od,2),
c$$$     x          p_dphi= 6*levs+0*levh+5,  !  dpdphie/o(lnte/od,2),
c$$$     x          p_uln = 6*levs+0*levh+6,  !     ulne/o(lnte/od,2,levs),
c$$$     x          p_vln = 7*levs+0*levh+6,  !     vlne/o(lnte/od,2,levs),
c$$$     x          p_w   = 8*levs+0*levh+6,  !       we/o(lnte/od,2,levs),
c$$$     x          p_x   = 9*levs+0*levh+6,  !       xe/o(lnte/od,2,levs),
c$$$     x          p_y   =10*levs+0*levh+6,  !       ye/o(lnte/od,2,levs),
c$$$     x          p_zq  =11*levs+0*levh+6,  !      zqe/o(lnte/od,2)
c$$$     x          p_rt  =11*levs+0*levh+7,  !      rte/o(lnte/od,2,levh),
c$$$     x          p_rm  =11*levs+1*levh+7,  !      rme/o(lnte/od,2,levh),
c$$$     x          p_rq  =11*levs+2*levh+7)  !      rqe/o(lnte/od,2,levh),
c$$$!
!
      integer              indlsev,jbasev
      integer kdt
!
      include 'function_indlsev'
!
      integer njeff,lon,iblk,ngptcd
!
!     integer, parameter :: ngptcd = 12
      real(kind=kind_evod), parameter :: cons0=0.0, cons2=2.0
!
      ngptcd  = ngptc
      ksz     =0*levs+0*levh+1
      ksd     =1*levs+0*levh+1
      kst     =2*levs+0*levh+1
      ksq     =3*levs+0*levh+1
      ksplam  =3*levs+0*levh+2
      kspphi  =3*levs+0*levh+3
      ksu     =3*levs+0*levh+4
      ksv     =4*levs+0*levh+4
      ksr     =5*levs+0*levh+4
!
      kdtphi  =0*levs+0*levh+1
      kdrphi  =1*levs+0*levh+1
      kdtlam  =1*levs+1*levh+1
      kdrlam  =2*levs+1*levh+1
      kdulam  =2*levs+2*levh+1
      kdvlam  =3*levs+2*levh+1
      kduphi  =4*levs+2*levh+1
      kdvphi  =5*levs+2*levh+1
!
      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kap     =3*levs+0*levh+1
      kar     =3*levs+0*levh+2
!
      iter_max=0
      do lan=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+lan)
         lons_lat = lonsperlat(lat)
         iter_max = max ( iter_max , (lons_lat+ngptcd-1)/ngptcd )
      enddo
!
      allocate ( spdlat(levs,iter_max ) )
!
! ------- ndsl to prepare (n-1)
!
      if( ndsl ) then
!
!$omp parallel do shared(trie_ls,trio_ls,p_dim,p_zem,p_tem)
!$omp+shared(triemls,triomls,kau,kav,kat)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+private(k)
        do k=1,levs
          call dezouv(trie_ls(1,1,p_dim+k-1), trio_ls(1,1,p_zem+k-1),
     x                triemls(1,1,kau  +k-1), triomls(1,1,kav  +k-1),
     x                epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!
          call dozeuv(trio_ls(1,1,p_dim+k-1), trie_ls(1,1,p_zem+k-1),
     x                triomls(1,1,kau  +k-1), triemls(1,1,kav  +k-1),
     x                epsedn,epsodn,snnp1ev,snnp1od,ls_node)
          triemls(:,:,kat +k-1) = trie_ls(:,:,p_tem+k-1)
          triomls(:,:,kat +k-1) = trio_ls(:,:,p_tem+k-1)
        enddo
        do k=1,levh
          triemls(:,:,kar +k-1) = trie_ls(:,:,p_rm +k-1)
          triomls(:,:,kar +k-1) = trio_ls(:,:,p_rm +k-1)
        enddo
        triemls(:,:,kap) = trie_ls(:,:,p_qm)
        triomls(:,:,kap) = trio_ls(:,:,p_qm)
!
        call sumfln_slg_gg(triemls,triomls,
     x                     lat1s_a,
     x                     plnev_a,plnod_a,
     x                     lota,ls_node,latg2,
     x                     lats_dim_a,lota,
     x                     sym_gr_a_1,
     x                     ls_nodes,max_ls_nodes,
     x                     lats_nodes_a,global_lats_a,
     x                     lats_node_a,ipt_lats_node_a,lon_dim_a,
     x                     lonsperlat,lon_dim_a,latg,0)
!
        do lan=1,lats_node_a   !sela begin lan loop 1
!
!hmhj     lon_dim = lon_dims_a(lan)
          lat = global_lats_a(ipt_lats_node_a-1+lan)
          lons_lat = lonsperlat(lat)

          call four_to_grid(sym_gr_a_1(1,1,lan),sym_gr_a_2(1,1,lan),
     &                      lon_dim_a,lon_dim_a-2,lons_lat,lota)
!
        enddo
!
      endif                             ! end of ndsl for preparing n-1
!
! ................................................................
!
!
      call delnpe(trie_ls(1,1,p_q   ),
     x            trio_ls(1,1,p_dphi),
     x            trie_ls(1,1,p_dlam),
     x            epse,epso,ls_node)
      call delnpo(trio_ls(1,1,p_q   ),
     x            trie_ls(1,1,p_dphi),
     x            trio_ls(1,1,p_dlam),
     x            epse,epso,ls_node)
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+shared(p_di,p_ze,p_uln,p_vln)
!$omp+private(k)
      do k=1,levs
         call dezouv(trie_ls(1,1,p_di +k-1), trio_ls(1,1,p_ze +k-1),
     x               trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!
         call dozeuv(trio_ls(1,1,p_di +k-1), trie_ls(1,1,p_ze +k-1),
     x               trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo
!
      call sumfln_slg_gg(trie_ls(1,1,p_ze),
     x                   trio_ls(1,1,p_ze),
     x            lat1s_a,
     x            plnev_a,plnod_a,
     x            5*levs+3,ls_node,latg2,
     x            lats_dim_a,lots,
     x            for_gr_a_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_a,global_lats_a,
!mjr x            lats_node_a,ipt_lats_node_a,lon_dims_a,
     x            lats_node_a,ipt_lats_node_a,lon_dim_a,
     x            lonsperlat,lon_dim_a,latg,0)
!
      call sumfln_slg_gg(trie_ls(1,1,p_rq),
     x                   trio_ls(1,1,p_rq),
     x            lat1s_a,
     x            plnev_a,plnod_a,
     x            levh,ls_node,latg2,
     x            lats_dim_a,lots,
     x            for_gr_a_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_a,global_lats_a,
!mjr x            lats_node_a,ipt_lats_node_a,lon_dims_a,
     x            lats_node_a,ipt_lats_node_a,lon_dim_a,
     x            lonsperlat,lon_dim_a,latg,5*levs+3)
!
!jfe  do j=1,lats_node_a
!jfe     lat = global_lats_a(ipt_lats_node_a-1+j)
!jfe     ii=0
!jfe  do k=1,lots
!jfe  do i=1,lon_dims_a(j)
!mjr  do i=1,lon_dim_a
!jfe      ii=ii+1
!jfe      if (
!jfe &     i.le.lonsperlat(lat))
!jfe &    write(60+me,*) for_gr_a_1(ii,1,j),i,lat,k
!jfe  enddo
!jfe  enddo
!jfe  enddo
!
      do lan=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+lan)
         lmax = min(jcap,lonsperlat(lat)/2)
         if ( (lmax+1)*2+1 .le. lonsperlat(lat)+2 ) then
            do k=levs+levh+1,4*levs+2*levh
               do i = (lmax+1)*2+1, lonsperlat(lat)+2
                  dyn_gr_a_1(i,k,lan) = cons0    !constant
               enddo
            enddo
         endif
      enddo
!
      call sumder2_slg(trie_ls(1,1,p_te),
     x                 trio_ls(1,1,p_te),
     x             lat1s_a,
     x             pddev_a,pddod_a,
     x             levs,ls_node,latg2,
     x             lats_dim_a,lotd,
     x             dyn_gr_a_1,
     x             ls_nodes,max_ls_nodes,
     x             lats_nodes_a,global_lats_a,
!mjr x             lats_node_a,ipt_lats_node_a,lon_dims_a,
     x             lats_node_a,ipt_lats_node_a,lon_dim_a,
     x             lonsperlat,lon_dim_a,latg,0)
!
      call sumder2_slg(trie_ls(1,1,p_rq),
     x                 trio_ls(1,1,p_rq),
     x             lat1s_a,
     x             pddev_a,pddod_a,
     x             levh,ls_node,latg2,
     x             lats_dim_a,lotd,
     x             dyn_gr_a_1,
     x             ls_nodes,max_ls_nodes,
     x             lats_nodes_a,global_lats_a,
!mjr x             lats_node_a,ipt_lats_node_a,lon_dims_a,
     x             lats_node_a,ipt_lats_node_a,lon_dim_a,
     x             lonsperlat,lon_dim_a,latg,levs)
!
      do k=1,levs
         spdmax_node(k) = cons0     !constant
      enddo
!
!
      do lan=1,lats_node_a   !sela begin lan loop 1
 
!      timer1=rtc()
!
         lat = global_lats_a(ipt_lats_node_a-1+lan)
!
c
!mjr     lon_dim = lon_dims_a(lan)
         lons_lat = lonsperlat(lat)
!!
!!       calculate t rq u v zonal derivs. by multiplication with i*l
!!       note rlcs2=rcs2*l/rerth
!
         lmax = min(jcap,lons_lat/2)
!
         ipt_ls=min(lat,latg-lat+1)
 
         do i=1,lmax+1
            if ( ipt_ls .ge. lat1s_a(i-1) ) then
               reall=i-1
               rlcs2(i)=reall*rcs2_a(ipt_ls)/rerth
            else
               rlcs2(i)=cons0     !constant
            endif
         enddo
!
!$omp parallel do private(k,i)
         do k=1,levs
            do i=1,lmax+1
!
!           d(t)/d(lam)
               dyn_gr_a_1(2*i-1,kdtlam-1+k,lan)=
     x        -for_gr_a_1(2*i  ,kst   -1+k,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  ,kdtlam-1+k,lan)=
     x         for_gr_a_1(2*i-1,kst   -1+k,lan)*rlcs2(i)
!
!           d(u)/d(lam)
               dyn_gr_a_1(2*i-1,kdulam-1+k,lan)=
     x        -for_gr_a_1(2*i  ,ksu   -1+k,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  ,kdulam-1+k,lan)=
     x         for_gr_a_1(2*i-1,ksu   -1+k,lan)*rlcs2(i)
!
!           d(v)/d(lam)
               dyn_gr_a_1(2*i-1,kdvlam-1+k,lan)=
     x        -for_gr_a_1(2*i  ,ksv   -1+k,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  ,kdvlam-1+k,lan)=
     x         for_gr_a_1(2*i-1,ksv   -1+k,lan)*rlcs2(i)
!
            enddo
         end do
!
!$omp parallel do private(k,i)
         do k=1,levh
            do i=1,lmax+1
!
!           d(rq)/d(lam)
               dyn_gr_a_1(2*i-1,kdrlam-1+k,lan)=
     x        -for_gr_a_1(2*i  ,ksr   -1+k,lan)*rlcs2(i)
               dyn_gr_a_1(2*i  ,kdrlam-1+k,lan)=
     x         for_gr_a_1(2*i-1,ksr   -1+k,lan)*rlcs2(i)
!
            enddo
         enddo
!
         call four_to_grid(for_gr_a_1(1,1,lan),for_gr_a_2(1,1,lan),
!mjr &                    lon_dim  ,lon_dim    ,lons_lat,5*levs+levh+3)
     &                    lon_dim_a,lon_dim_a-2,lons_lat,5*levs+levh+3)
!!
         call four_to_grid(dyn_gr_a_1(1,1,lan),dyn_gr_a_2(1,1,lan),
!mjr &                    lon_dim  ,lon_dim    ,lons_lat,4*levs+2*levh)
     &                    lon_dim_a,lon_dim_a-2,lons_lat,4*levs+2*levh)
!
!         timer2=rtc()
!         global_times_a(lat,me+1)=timer2-timer1
!$$$       print*,'timeloopa',me,timer1,timer2,global_times_a(lat,me+1)
 
      enddo !sela fin lan loop 1
!11111111111111111111111111111111111111111111111111111111111111111111
!
! do horizontal advection by using wind at time n, and values at n-1
!
      if( ndsl ) then
!       call ndslfv_ppm_advecth (for_gr_a_2,sym_gr_a_2,bak_gr_a_2,
!    &                         dpt_gr_a_2,ppt_gr_a_2,
!    &                         global_lats_a,lonsperlat,deltim,kdt)
        call ndslfv_ppm_advecth1(for_gr_a_2,sym_gr_a_2,bak_gr_a_2,
     &                           dpt_gr_a_2,ppt_gr_a_2,
     &                           global_lats_a,lonsperlat,deltim,kdt)
!       call ndslfv_mono_advecth1(for_gr_a_2,sym_gr_a_2,bak_gr_a_2,
!    &                           dpt_gr_a_2,ppt_gr_a_2,
!    &                           global_lats_a,lonsperlat,deltim,kdt)
      endif
!
!22222222222222222222222222222222222222222222222222222222222
!22222222222222222222222222222222222222222222222222222222222
      do lan=1,lats_node_a   !sela begin lan loop 2
 
!       timer1=rtc()
!
         lat = global_lats_a(ipt_lats_node_a-1+lan)
!
!mjr     lon_dim  = lon_dims_a(lan)
         lons_lat = lonsperlat(lat)
         rcs2loc  = rcs2_a(min(lat,latg-lat+1))

!!
!!  calculate grid meridional derivatives of u and v.
!!
!!  cos*d(u)/d(theta)= d(v)/d(lam)-a*zeta*cos**2
!!  cos*d(v)/d(theta)=-d(u)/d(lam)+a*divr*cos**2
!!
        if (gen_coord_hybrid) then !-----  generalized hybrid ----------- ! hmhj
!$omp parallel do private(k,j)
          do k=1,levs
            do j=1,lons_lat
!
              dyn_gr_a_2(j,kduphi-1+k,lan)=
     x        dyn_gr_a_2(j,kdvlam-1+k,lan)-
     x        for_gr_a_2(j,ksz   -1+k,lan)
!
              dyn_gr_a_2(j,kdvphi-1+k,lan)=
     x       -dyn_gr_a_2(j,kdulam-1+k,lan)+
     x        for_gr_a_2(j,ksd   -1+k,lan)
            enddo
          enddo
        else
!$omp parallel do private(k,j)
          do k=1,levs
            do j=1,lons_lat
!
              dyn_gr_a_2(j,kduphi-1+k,lan)=
     x        dyn_gr_a_2(j,kdvlam-1+k,lan)-
     x        for_gr_a_2(j,ksz   -1+k,lan)
!
              dyn_gr_a_2(j,kdvphi-1+k,lan)=
     x       -dyn_gr_a_2(j,kdulam-1+k,lan)+
     x        for_gr_a_2(j,ksd   -1+k,lan)
!
              for_gr_a_2(j,kst-1+k,lan)=
     x        for_gr_a_2(j,kst-1+k,lan)-tov(k)
!
            enddo
          enddo
        endif
!
        if (gen_coord_hybrid) then !-----  generalized hybrid ----------- ! hmhj

          if( thermodyn_id == 3 ) then                                    ! hmhj

! beginlon omp loop 3333333333333333333333333333333333333333333333333
!$omp parallel do schedule(dynamic,1) private(lon,njeff,iblk,nvcn,xvcn)
            do lon=1,lons_lat,ngptcd
              njeff = min(ngptcd,lons_lat-lon+1)
              iblk  = (lon-1)/ngptcd + 1
!
!
!             call gfidi_hyb_gc_h(lon_dim_a-2, njeff, lat,
              call gfidi_hyb_gc_h(lonf, njeff, lat,
     x                for_gr_a_2(lon,ksd   ,lan),
     x                for_gr_a_2(lon,kst   ,lan),
     x                for_gr_a_2(lon,ksz   ,lan),
     x                for_gr_a_2(lon,ksu   ,lan),
     x                for_gr_a_2(lon,ksv   ,lan),
     x                for_gr_a_2(lon,ksr   ,lan),
     x                for_gr_a_2(lon,kspphi,lan),
     x                for_gr_a_2(lon,ksplam,lan),
     x                for_gr_a_2(lon,ksq   ,lan),
!    x                rcs2_a(min(lat,latg-lat+1)),
     x                rcs2loc,
     x                spdlat(1,iblk),
     x                deltim,nvcn,xvcn,
     x                dyn_gr_a_2(lon,kdtphi,lan),
     x                dyn_gr_a_2(lon,kdtlam,lan),
     x                dyn_gr_a_2(lon,kdrphi,lan),
     x                dyn_gr_a_2(lon,kdrlam,lan),
     x                dyn_gr_a_2(lon,kdulam,lan),
     x                dyn_gr_a_2(lon,kdvlam,lan),
     x                dyn_gr_a_2(lon,kduphi,lan),
     x                dyn_gr_a_2(lon,kdvphi,lan),
     x                bak_gr_a_2(lon,kap   ,lan),
     x                bak_gr_a_2(lon,kat   ,lan),
     x                bak_gr_a_2(lon,kar   ,lan),
     x                bak_gr_a_2(lon,kau   ,lan),
     x                bak_gr_a_2(lon,kav   ,lan),
     &                dyn_f3d(lon,1,1,lan))
            enddo   !lon
          else
           if( ndsl ) then
!$omp parallel do schedule(dynamic,1) private(lon,njeff,iblk)
            do lon=1,lons_lat,ngptcd
              njeff = min(ngptcd,lons_lat-lon+1)
              iblk  = (lon-1)/ngptcd + 1
!
              call gfidi_hyb_gc_ndsl(lonf, njeff, lat,
     x                for_gr_a_2(lon,ksd   ,lan),
     x                for_gr_a_2(lon,kst   ,lan),
     x                for_gr_a_2(lon,ksz   ,lan),
     x                for_gr_a_2(lon,ksu   ,lan),
     x                for_gr_a_2(lon,ksv   ,lan),
     x                for_gr_a_2(lon,ksr   ,lan),
     x                for_gr_a_2(lon,kspphi,lan),
     x                for_gr_a_2(lon,ksplam,lan),
     x                for_gr_a_2(lon,ksq   ,lan),
!    x                rcs2_a(min(lat,latg-lat+1)),
     x                rcs2loc,
     x                spdlat(1,iblk),deltim,
     x                dyn_gr_a_2(lon,kdtphi,lan),
     x                dyn_gr_a_2(lon,kdtlam,lan),
     x                dyn_gr_a_2(lon,kdrphi,lan),
     x                dyn_gr_a_2(lon,kdrlam,lan),
     x                dyn_gr_a_2(lon,kdulam,lan),
     x                dyn_gr_a_2(lon,kdvlam,lan),
     x                dyn_gr_a_2(lon,kduphi,lan),
     x                dyn_gr_a_2(lon,kdvphi,lan),
     x                sym_gr_a_2(lon,kap   ,lan),
     x                sym_gr_a_2(lon,kat   ,lan),
     x                sym_gr_a_2(lon,kar   ,lan),
     x                sym_gr_a_2(lon,kau   ,lan),
     x                sym_gr_a_2(lon,kav   ,lan),
     x                bak_gr_a_2(lon,kap   ,lan),
     x                bak_gr_a_2(lon,kat   ,lan),
     x                bak_gr_a_2(lon,kar   ,lan),
     x                bak_gr_a_2(lon,kau   ,lan),
     x                bak_gr_a_2(lon,kav   ,lan),
     x                dpt_gr_a_2(lon,1     ,lan),
     x                ppt_gr_a_2(lon,1     ,lan))
            enddo   !lon
           else
!$omp parallel do schedule(dynamic,1) private(lon,njeff,iblk,nvcn,xvcn)
            do lon=1,lons_lat,ngptcd
              njeff = min(ngptcd,lons_lat-lon+1)
              iblk  = (lon-1)/ngptcd + 1
!
!mjr          call gfidi_hyb_gc(lon_dim  -2, njeff, lat,
!             call gfidi_hyb_gc(lon_dim_a-2, njeff, lat,
              call gfidi_hyb_gc(lonf, njeff, lat,
     x                for_gr_a_2(lon,ksd   ,lan),
     x                for_gr_a_2(lon,kst   ,lan),
     x                for_gr_a_2(lon,ksz   ,lan),
     x                for_gr_a_2(lon,ksu   ,lan),
     x                for_gr_a_2(lon,ksv   ,lan),
     x                for_gr_a_2(lon,ksr   ,lan),
     x                for_gr_a_2(lon,kspphi,lan),
     x                for_gr_a_2(lon,ksplam,lan),
     x                for_gr_a_2(lon,ksq   ,lan),
!    x                rcs2_a(min(lat,latg-lat+1)),
     x                rcs2loc,
     x                spdlat(1,iblk),
     x                deltim,nvcn,xvcn,
     x                dyn_gr_a_2(lon,kdtphi,lan),
     x                dyn_gr_a_2(lon,kdtlam,lan),
     x                dyn_gr_a_2(lon,kdrphi,lan),
     x                dyn_gr_a_2(lon,kdrlam,lan),
     x                dyn_gr_a_2(lon,kdulam,lan),
     x                dyn_gr_a_2(lon,kdvlam,lan),
     x                dyn_gr_a_2(lon,kduphi,lan),
     x                dyn_gr_a_2(lon,kdvphi,lan),
     x                bak_gr_a_2(lon,kap   ,lan),
     x                bak_gr_a_2(lon,kat   ,lan),
     x                bak_gr_a_2(lon,kar   ,lan),
     x                bak_gr_a_2(lon,kau   ,lan),
     x                bak_gr_a_2(lon,kav   ,lan),
     &                dyn_f3d(lon,1,1,lan))
            enddo   !lon
           endif

          endif

        elseif (hybrid) then               !-----  hybrid -----------
!$omp parallel do schedule(dynamic,1) private(lon,njeff,iblk,nvcn,xvcn)
          do lon=1,lons_lat,ngptcd
            njeff = min(ngptcd,lons_lat-lon+1)
            iblk  = (lon-1)/ngptcd + 1
!
!mjr        call gfidi_hyb(lon_dim  -2, njeff, lat,
!           call gfidi_hyb(lon_dim_a-2, njeff, lat,
            call gfidi_hyb(lonf, njeff, lat,
     x                   for_gr_a_2(lon,ksd   ,lan),
     x                   for_gr_a_2(lon,kst   ,lan),
     x                   for_gr_a_2(lon,ksz   ,lan),
     x                   for_gr_a_2(lon,ksu   ,lan),
     x                   for_gr_a_2(lon,ksv   ,lan),
     x                   for_gr_a_2(lon,ksr   ,lan),
     x                   for_gr_a_2(lon,kspphi,lan),
     x                   for_gr_a_2(lon,ksplam,lan),
     x                   for_gr_a_2(lon,ksq   ,lan),
!    x                   rcs2_a(min(lat,latg-lat+1)),
     x                   rcs2loc,
     x                   spdlat(1,iblk),
     x                   deltim,nvcn,xvcn,
     x                   dyn_gr_a_2(lon,kdtphi,lan),
     x                   dyn_gr_a_2(lon,kdtlam,lan),
     x                   dyn_gr_a_2(lon,kdrphi,lan),
     x                   dyn_gr_a_2(lon,kdrlam,lan),
     x                   dyn_gr_a_2(lon,kdulam,lan),
     x                   dyn_gr_a_2(lon,kdvlam,lan),
     x                   dyn_gr_a_2(lon,kduphi,lan),
     x                   dyn_gr_a_2(lon,kdvphi,lan),
     x                   bak_gr_a_2(lon,kap   ,lan),
     x                   bak_gr_a_2(lon,kat   ,lan),
     x                   bak_gr_a_2(lon,kar   ,lan),
     x                   bak_gr_a_2(lon,kau   ,lan),
     x                   bak_gr_a_2(lon,kav   ,lan),
     &                   dyn_f3d(lon,1,1,lan))
!
          enddo   !lon
        endif ! -----------------------  generalized hybrid  ------------------
!
        iblk=1
        do lon=1,lons_lat,ngptcd
          do k=1,levs
             spdmax_node(k) = max(spdmax_node(k),spdlat(k,iblk))
          enddo
          iblk = iblk + 1
        enddo
!
        call grid_to_four(bak_gr_a_2(1,1,lan),bak_gr_a_1(1,1,lan),
!mjr &                    lon_dim    ,lon_dim  ,lons_lat,lota)
     &                    lon_dim_a-2,lon_dim_a,lons_lat,lota)
!
!       timer2=rtc()
!       global_times_a(lat,me+1) = 
!    x  global_times_a(lat,me+1)+timer2-timer1
!$$$       print*,'timeloopa',me,timer1,timer2,global_times_a(lat,me+1)
 
      enddo                      !sela fin lan loop 2

!22222222222222222222222222222222222222222222222222222222222
!22222222222222222222222222222222222222222222222222222222222
!
      call four2fln_gg(lats_dim_a,lota,lota,bak_gr_a_1,
     x              ls_nodes,max_ls_nodes,
!mjr x              lats_nodes_a,global_lats_a,lon_dims_a,
     x              lats_nodes_a,global_lats_a,lon_dim_a,
     x              lats_node_a,ipt_lats_node_a,
     x              lat1s_a,lon_dim_a,latg,latg2,
     x              trie_ls(1,1,p_w), trio_ls(1,1,p_w),
     x              plnew_a, plnow_a,
     x              ls_node,0,2*levs+1,lota)
!
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epse,epso,ls_node)
!$omp+shared(p_w,p_x,p_uln,p_vln)
!$omp+private(k)
      do k=1,levs
         call uveodz(trie_ls(1,1,p_w  +k-1), trio_ls(1,1,p_x  +k-1),
     x               trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),
     x               epse,epso,ls_node)
!
         call uvoedz(trio_ls(1,1,p_w  +k-1), trie_ls(1,1,p_x  +k-1),
     x               trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),
     x               epse,epso,ls_node)
      enddo
!
!
!   move div tendency into x and add topog. contrib.
!   integrate vorticity amd moisture in time
!   remember uln is old x
!   remember vln is old w
!
      do k=1,levs
         do i=1,len_trie_ls
            trie_ls(i,1,p_x  +k-1)=
     x      trie_ls(i,1,p_uln+k-1)+             trie_ls(i,1,p_gz)
 
            trie_ls(i,2,p_x  +k-1)=
     x      trie_ls(i,2,p_uln+k-1)+             trie_ls(i,2,p_gz)
 
            trie_ls(i,1,p_w  +k-1)=
     x      trie_ls(i,1,p_zem+k-1)+cons2*deltim*trie_ls(i,1,p_vln+k-1)     !cons
 
            trie_ls(i,2,p_w  +k-1)=
     x      trie_ls(i,2,p_zem+k-1)+cons2*deltim*trie_ls(i,2,p_vln+k-1)     !cons
 
         enddo
         do i=1,len_trio_ls
            trio_ls(i,1,p_x  +k-1)=
     x      trio_ls(i,1,p_uln+k-1)+             trio_ls(i,1,p_gz)
 
            trio_ls(i,2,p_x  +k-1)=
     x      trio_ls(i,2,p_uln+k-1)+             trio_ls(i,2,p_gz)
 
            trio_ls(i,1,p_w  +k-1)=
     x      trio_ls(i,1,p_zem+k-1)+cons2*deltim*trio_ls(i,1,p_vln+k-1)     !cons
 
            trio_ls(i,2,p_w  +k-1)=
     x      trio_ls(i,2,p_zem+k-1)+cons2*deltim*trio_ls(i,2,p_vln+k-1)     !cons
 
         enddo
      enddo
!
      do k=1,levh
         do i=1,len_trie_ls
            trie_ls(i,1,p_rt+k-1)=
     x      trie_ls(i,1,p_rm+k-1)+cons2*deltim* trie_ls(i,1,p_rt+k-1)     !const
            trie_ls(i,2,p_rt+k-1)=
     x      trie_ls(i,2,p_rm+k-1)+cons2*deltim* trie_ls(i,2,p_rt+k-1)     !const
         enddo
         do i=1,len_trio_ls
            trio_ls(i,1,p_rt+k-1)=
     x      trio_ls(i,1,p_rm+k-1)+cons2*deltim* trio_ls(i,1,p_rt+k-1)     !const
            trio_ls(i,2,p_rt+k-1)=
     x      trio_ls(i,2,p_rm+k-1)+cons2*deltim* trio_ls(i,2,p_rt+k-1)     !const
         enddo
      enddo
c$$$      print*,' fin gloopa trie and trio '
!
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         if ( l == 0 ) then
            n = 0
!
            do k=1,levs
               trie_ls(indlsev(n,l),1,p_w+k-1) = cons0     !constant
               trie_ls(indlsev(n,l),2,p_w+k-1) = cons0     !constant
            enddo
!
         endif
      end do
!
! hmhj ----  do explicit scheme --- sicdif should be turned off ---
      if( explicit ) then                                               ! hmhj
      do k=1,levs                                                       ! hmhj
         do i=1,len_trie_ls                                             ! hmhj
            trie_ls(i,1,p_x  +k-1)=                                     ! hmhj
     x      trie_ls(i,1,p_dim+k-1)+cons2*deltim*trie_ls(i,1,p_x+k-1)    ! hmhj
            trie_ls(i,2,p_x  +k-1)=                                     ! hmhj
     x      trie_ls(i,2,p_dim+k-1)+cons2*deltim*trie_ls(i,2,p_x+k-1)    ! hmhj
            trie_ls(i,1,p_y  +k-1)=                                     ! hmhj
     x      trie_ls(i,1,p_tem+k-1)+cons2*deltim*trie_ls(i,1,p_y+k-1)    ! hmhj
            trie_ls(i,2,p_y  +k-1)=                                     ! hmhj
     x      trie_ls(i,2,p_tem+k-1)+cons2*deltim*trie_ls(i,2,p_y+k-1)    ! hmhj
         enddo                                                          ! hmhj
         do i=1,len_trio_ls                                             ! hmhj
            trio_ls(i,1,p_x  +k-1)=                                     ! hmhj
     x      trio_ls(i,1,p_dim+k-1)+cons2*deltim*trio_ls(i,1,p_x+k-1)    ! hmhj
            trio_ls(i,2,p_x  +k-1)=                                     ! hmhj
     x      trio_ls(i,2,p_dim+k-1)+cons2*deltim*trio_ls(i,2,p_x+k-1)    ! hmhj
            trio_ls(i,1,p_y  +k-1)=                                     ! hmhj
     x      trio_ls(i,1,p_tem+k-1)+cons2*deltim*trio_ls(i,1,p_y+k-1)    ! hmhj
            trio_ls(i,2,p_y  +k-1)=                                     ! hmhj
     x      trio_ls(i,2,p_tem+k-1)+cons2*deltim*trio_ls(i,2,p_y+k-1)    ! hmhj
         enddo                                                          ! hmhj
      enddo                                                             ! hmhj
!
         do i=1,len_trie_ls                                             ! hmhj
            trie_ls(i,1,p_zq)=                                          ! hmhj
     x      trie_ls(i,1,p_qm)+cons2*deltim*trie_ls(i,1,p_zq)            ! hmhj
            trie_ls(i,2,p_zq)=                                          ! hmhj
     x      trie_ls(i,2,p_qm)+cons2*deltim*trie_ls(i,2,p_zq)            ! hmhj
         enddo                                                          ! hmhj
         do i=1,len_trio_ls                                             ! hmhj
            trio_ls(i,1,p_zq)=                                          ! hmhj
     x      trio_ls(i,1,p_qm)+cons2*deltim*trio_ls(i,1,p_zq)            ! hmhj
            trio_ls(i,2,p_zq)=                                          ! hmhj
     x      trio_ls(i,2,p_qm)+cons2*deltim*trio_ls(i,2,p_zq)            ! hmhj
         enddo                                                          ! hmhj
      endif     ! explicit                                              ! hmhj
! hmhj -------   end of explicit -------
!
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      spdmax_nodesm = 0.0
      spdmax_nodem = spdmax_node
      call mpi_gather(spdmax_nodem,levs,mpi_r_mpi,
     x                spdmax_nodesm,levs,mpi_r_mpi,
     x                0,mc_comp,ierr)
!     if (comp_task) then
!!!   spdmax_nodes = spdmax_nodesm     ! moorthi
!     endif
!
!sela call mpi_barrier (mpi_comm_world,ierr)
!
      if ( me  ==  0 ) then
!
         spdmax_nodes = spdmax_nodesm
         do k=1,levs
            spdmax(k) = cons0     !constant
            do node=1,nodes
               spdmax(k) = max(spdmax(k),spdmax_nodes(k,node))
            enddo
            spdmax(k) = sqrt(spdmax(k))
         enddo
!
!        print*,'in gloopa for spdmx at kdt=',kdt
!        print 100,(spdmax(k),k=1,levs)
!100     format(' spdmx(01:10)=',10f5.0,:/' spdmx(11:20)=',10f5.0,
!    x        :/' spdmx(21:30)=',10f5.0,:/' spdmx(31:40)=',10f5.0,
!    x        :/' spdmx(41:50)=',10f5.0,:/' spdmx(51:60)=',10f5.0,
!    x        :/' spdmx(61:70)=',10f5.0,:/' spdmx(71:80)=',10f5.0,
!    x        :/' spdmx(81:90)=',10f5.0,:/' spdmx(91:00)=',10f5.0)
!
      endif
!
      call mpi_bcast(spdmax,levs,mpi_real8,
     x               0,mc_comp,ierr)
!
      deallocate ( spdlat )
!
      return
      end
