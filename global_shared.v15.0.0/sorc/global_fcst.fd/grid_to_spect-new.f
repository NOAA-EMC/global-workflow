      subroutine grid_to_spect
     &    (zsg,psg,uug,vvg,teg,rqg,
     x     trie_zs,trio_zs,trie_ps,trio_ps,
     x     trie_di,trio_di,trie_ze,trio_ze,
     x     trie_te,trio_te,trie_rq,trio_rq,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,lonsperlar,
     x     epse,epso,plnew_r,plnow_r)
!!
!! hmhj - this routine do spectral to grid transform in model partial reduced grid
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      implicit none
!!
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
cc
      real(kind=kind_evod) trie_zs(len_trie_ls,2)
      real(kind=kind_evod) trio_zs(len_trio_ls,2)
      real(kind=kind_evod) trie_ps(len_trie_ls,2)
      real(kind=kind_evod) trio_ps(len_trio_ls,2)
      real(kind=kind_evod) trie_di(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_di(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_ze(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_ze(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_te(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_te(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_rq(len_trie_ls,2,levh)
      real(kind=kind_evod) trio_rq(len_trio_ls,2,levh)
!
!!!!  integer, parameter :: lota = 3*levs+1*levh+1 
!
      real(kind=kind_evod) trie_ls(len_trie_ls,2,lota+1)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,lota+1)
!!
      real(kind=kind_evod) for_gr_r_1(lonrx*(lota+1),lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonrx*(lota+1),lats_dim_r)
cc
      integer              ls_node,ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer dimg
cc
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
cc
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
cc
      integer              i,k,kaz,kap,kar,kat,kau,kav
      integer              lan,lat,lotx
      integer              lon_dim,lons_lat
      logical 	lslag
!
cc
cc--------------------------------------------------------------------
cc
      lslag   = .false.
      lotx    = lota + 1

      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kar     =3*levs+0*levh+1
      kap     =3*levs+1*levh+1
      kaz     =3*levs+1*levh+2
cc
cc--------------------------------------------------------------------
      do lan=1,lats_node_r
        lon_dim = lon_dims_r(lan)
        lat = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        do k=1,levs
          do i=1,lons_lat
            for_gr_r_2(i+(kau+k-2)*lon_dim,lan)=uug(i,lan,k)
            for_gr_r_2(i+(kav+k-2)*lon_dim,lan)=vvg(i,lan,k)
            for_gr_r_2(i+(kat+k-2)*lon_dim,lan)=teg(i,lan,k)
          enddo
        enddo
        do k=1,levh
          do i=1,lons_lat
            for_gr_r_2(i+(kar+k-2)*lon_dim,lan)=rqg(i,lan,k)
          enddo
        enddo
        do i=1,lons_lat
          for_gr_r_2(i+(kaz-1)*lon_dim,lan)=zsg(i,lan)
          for_gr_r_2(i+(kap-1)*lon_dim,lan)=psg(i,lan)
        enddo
      enddo
!
c
      do lan=1,lats_node_r
cc
         lon_dim = lon_dims_r(lan)
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)

         call grid2four_thread(for_gr_r_2(1,lan),for_gr_r_1(1,lan),
     &                  lon_dim,lons_lat,lonrx,lotx)
cc
      enddo
!
      dimg=0
      call four2fln(lslag,lats_dim_r,lotx,lotx,for_gr_r_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_r,global_lats_r,lon_dims_r,
     x              lats_node_r,ipt_lats_node_r,dimg,
     x              lat1s_r,lonrx,latr,latr2,
     x              trie_ls(1,1,1), trio_ls(1,1,1),
     x              plnew_r, plnow_r,
     x              ls_node)
cc
cc
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(trie_di,trio_di,trie_ze,trio_ze,trie_te,trio_te)
!$omp+shared(kau,kav,kat,epse,epso,ls_node)
!$omp+private(k)
      do k=1,levs
         call uveodz(trie_ls(1,1,kau+k-1), trio_ls(1,1,kav+k-1),
     x               trie_di(1,1,k),       trio_ze(1,1,k),
     x               epse,epso,ls_node)
cc
         call uvoedz(trio_ls(1,1,kau+k-1), trie_ls(1,1,kav+k-1),
     x               trio_di(1,1,k),       trie_ze(1,1,k),
     x               epse,epso,ls_node)
        trie_te(:,:,k)=trie_ls(:,:,kat+k-1)
        trio_te(:,:,k)=trio_ls(:,:,kat+k-1)
      enddo
      do k=1,levh
        trie_rq(:,:,k)=trie_ls(:,:,kar+k-1)
        trio_rq(:,:,k)=trio_ls(:,:,kar+k-1)
      enddo
      trie_zs(:,:)=trie_ls(:,:,kaz)
      trio_zs(:,:)=trio_ls(:,:,kaz)
      trie_ps(:,:)=trie_ls(:,:,kap)
      trio_ps(:,:)=trio_ls(:,:,kap)
cc
      print *,' exit grid_to_spect '
!!
      return
      end
