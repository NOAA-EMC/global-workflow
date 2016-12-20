      subroutine spect_tv_enthalpy_ps
     &    (direction,
     &     trie_q,trio_q,trie_te,trio_te,trie_rq,trio_rq,
     &     ls_node,ls_nodes,max_ls_nodes,
     &     lats_nodes_r,global_lats_r,lonsperlar,
     &     plnev_r,plnod_r,plnew_r,plnow_r)
!!
!! hmhj - this routine is to convert between virtual temperature and enthalpy
!!        direction=1	convert from virtual temperature to enthalpy
!!	  direction=-1	convert from enthalpy to virtual temperature
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const
      use physcons, fv => con_fvirt, rk => con_rocp, rerth => con_rerth,
     &              grav => con_g
      implicit none
!!
      real(kind=kind_evod) trie_q (len_trie_ls,2)
      real(kind=kind_evod) trio_q (len_trio_ls,2)
      real(kind=kind_evod) trie_te(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_te(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_rq(len_trie_ls,2,levh)
      real(kind=kind_evod) trio_rq(len_trio_ls,2,levh)

!     logical run_enthalpy
!
!     integer, parameter  ::   lotx = levs+levh+1

      real(kind=kind_evod) trie_ls(len_trie_ls,2,levs+levh+1)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,levs+levh+1)
      real(kind=kind_evod) for_gr_r_1(lonrx,(levs+levh+1),lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonr,(levs+levh+1),lats_dim_r)
!
cc
      real(kind=kind_rad) psg(lonr)
      real(kind=kind_rad) teg(lonr,levs)
      real(kind=kind_rad) rqg(lonr,levh)
cc
!
      integer              ls_node(ls_dim,3)
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer dimg
cc
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
!
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!
!
      real(kind=kind_evod)   to_enthalpy(lonr,levs)
      real(kind=kind_evod)   to_virttemp(lonr,levs)
      real(kind=kind_evod)   sumq(lonr,levs)
cc
      integer              i,j,k,kap,kat,kar,nn,nnl
      integer              l,lan,lat,lotx,direction
      integer              lons_lat
!     integer              lon_dim
!
      logical   lslag
!
cc
cc--------------------------------------------------------------------
cc
      lslag   = .false.

      kap     =1
      kat     =kap+1
      kar     =kat+levs
!
      lotx    =1+levs+levh
cc
cc--------------------------------------------------------------------
cc
      trie_ls(:,:,kap) = trie_q (:,:)
      trio_ls(:,:,kap) = trio_q (:,:)
      do k=1,levs
        trie_ls(:,:,kat+k-1) = trie_te(:,:,k)
        trio_ls(:,:,kat+k-1) = trio_te(:,:,k)
      enddo
      do k=1,levh
        trie_ls(:,:,kar+k-1) = trie_rq(:,:,k)
        trio_ls(:,:,kar+k-1) = trio_rq(:,:,k)
      enddo
!
      dimg=0
cc
!     call sumfln_r(trie_ls,trio_ls,

      if (.not. gg_tracers)then
        call sumfln_slg_gg(trie_ls,trio_ls,
     x                     lat1s_r,
     x                     plnev_r,plnod_r,
     x                     lotx,ls_node,latr2,
     x                     lats_dim_r,lotx,for_gr_r_1,
     x                     ls_nodes,max_ls_nodes,
     x                     lats_nodes_r,global_lats_r,
     x                     lats_node_r,ipt_lats_node_r,lon_dim_r,
     x                     lonsperlar,lonrx,latr,0)
      else
         print *,' the gg_tracers option not valid for gen_coord_hybrid'
         call mpi_quit(2222)
      endif ! if(.not.gg_tracers)then
!
      do lan=1,lats_node_r
cc
         lat = global_lats_r(ipt_lats_node_r-1+lan)
!        lon_dim = lon_dims_r(lan)
         lons_lat = lonsperlar(lat)
cc
         call four_to_grid(for_gr_r_1(1,1,lan),for_gr_r_2(1,1,lan),
     &                     lon_dim_r,lon_dim_r-2,lons_lat,lotx)
 
      enddo   !lan
cc
! -------------------------------------------------------------------
! --------------- convert between virttemp and enthalpy -------------
      do lan=1,lats_node_r

!       lon_dim  = lon_dims_r(lan)
        lat      = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
 
        do k=1,levh
          do i=1,lons_lat
            rqg(i,k) = for_gr_r_2(i,kar+k-1,lan)
          enddo
        enddo
 
        do k=1,levs
          do i=1,lons_lat
            to_enthalpy(i,k) = 0.0
            sumq(i,k) = 0.0
          enddo
        enddo
        do nn=1,ntrac
          nnl = (nn-1)*levs
          if (cpi(nn) .ne. 0.0) then
            do k=1,levs
              do i=1,lons_lat
                sumq(i,k) = sumq(i,k) + rqg(i,nnl+k)
                to_enthalpy(i,k) = to_enthalpy(i,k)         
     &                            + cpi(nn)*rqg(i,nnl+k)
              enddo
            enddo
          endif
        enddo
        do k=1,levs
          do i=1,lons_lat
            to_enthalpy(i,k) = (1.0-sumq(i,k))*cpi(0) + to_enthalpy(i,k)
          enddo
        enddo
 
        do k=1,levs
          do i=1,lons_lat
            to_virttemp(i,k) = 1.0 + fv*rqg(i,k)
          enddo
        enddo

        do k=1,levs
          do i=1,lons_lat
            teg(i,k) = for_gr_r_2(i,kat+k-1,lan)
          enddo
        enddo

        do i=1,lons_lat
          psg(i) = for_gr_r_2(i,1,lan)
        enddo

! --------- from ( virttemp lnps ) to ( enthalpy ps )
        if( direction.eq.1 ) then 
          if( run_enthalpy ) then
            do k=1,levs
              do i=1,lons_lat
                teg(i,k) = teg(i,k) / to_virttemp(i,k)
                teg(i,k) = teg(i,k) * to_enthalpy(i,k)
              enddo
            enddo
          endif
          do i=1,lons_lat
            psg(i) = exp( psg(i) )
          enddo
        endif

! --------- from ( enthalpy ps ) to ( virttemp lnps )
        if( direction.eq.-1 ) then 
          if( run_enthalpy ) then
            do k=1,levs
              do i=1,lons_lat
                teg(i,k) = teg(i,k) / to_enthalpy(i,k)
                teg(i,k) = teg(i,k) * to_virttemp(i,k)
              enddo
            enddo
          endif
          do i=1,lons_lat
            psg(i) = log( psg(i) )
          enddo
        endif
! ------------------------------------------------------
        do i=1,lons_lat
          for_gr_r_2(i,1,lan) = psg(i)
        enddo
        do k=1,levs
          do i=1,lons_lat
            for_gr_r_2(i,kat+k-1,lan) = teg(i,k)
          enddo
        enddo
!
      enddo


cc ------------------------ transform back to coefficient ----
      do lan=1,lats_node_r
 
!        lon_dim = lon_dims_r(lan)
 
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)

         call grid_to_four(for_gr_r_2(1,1,lan),for_gr_r_1(1,1,lan),
     &                  lon_dim_r-2,lon_dim_r,lons_lat,levs+1)
 
      enddo
!
      dimg=0
      call four2fln_gg(lats_dim_r,lotx,levs+1,for_gr_r_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_r,global_lats_r,lon_dim_r,
     x              lats_node_r,ipt_lats_node_r,
     x              lat1s_r,lonrx,latr,latr2,
     x              trie_ls(1,1,1), trio_ls(1,1,1),
     x              plnew_r, plnow_r,
     x              ls_node,0,1,levs+1)
!
      do k=1,levs
        trie_te(:,:,k)=trie_ls(:,:,kat+k-1)
        trio_te(:,:,k)=trio_ls(:,:,kat+k-1)
      enddo
      trie_q (:,:)=trie_ls(:,:,kap)
      trio_q (:,:)=trio_ls(:,:,kap)
!!
      return
      end
