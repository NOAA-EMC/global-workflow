      subroutine getcon_spectral ( ls_node,ls_nodes,max_ls_nodes,  &
                                  lats_nodes_a,global_lats_a,      &
                                  lonsperlat,latsmax,              &
                                  lats_nodes_ext,global_lats_ext,  &
                                  epse,epso,epsedn,epsodn,         &
                                  snnp1ev,snnp1od,                 &
                                  plnev_a,plnod_a,pddev_a,pddod_a, &
                                  plnew_a,plnow_a,colat1)
 
! program log:
! 20110220    henry juang update code to fit mass_dp and ndslfv
!
      use stochy_resol_def
      use spectral_layout
      use stochy_gg_def
      use stochy_internal_state_mod

      implicit none
!
      integer              i,j,k,l,lat,lan,lons_lat,n
      integer              ls_node(ls_dim,3),ierr
!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer                      ls_nodes(ls_dim,nodes)
      integer, dimension(nodes) :: max_ls_nodes,  lats_nodes_a
      integer, dimension(latg)  :: global_lats_a, lonsperlat
!
      integer                lats_nodes_ext(nodes)
      integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
!
      real(kind=kind_dbl_prec), dimension(len_trie_ls) :: epse, epsedn, snnp1ev
      real(kind=kind_dbl_prec), dimension(len_trio_ls) :: epso, epsodn, snnp1od 
!
      real(kind=kind_dbl_prec), dimension(len_trie_ls,latg2) :: plnev_a, pddev_a, plnew_a
      real(kind=kind_dbl_prec), dimension(len_trio_ls,latg2) :: plnod_a, pddod_a, plnow_a
!
      real(kind=kind_dbl_prec), allocatable:: colrad_dp(:), wgt_dp(:),&
                      wgtcs_dp(:),  rcs2_dp(:), epse_dp(:),   epso_dp(:),&
                      epsedn_dp(:), epsodn_dp(:),plnev_dp(:), plnod_dp(:),&
                      pddev_dp(:), pddod_dp(:),plnew_dp(:),  plnow_dp(:)
!
      integer       iprint,locl,node,&
                    len_trie_ls_nod, len_trio_ls_nod,&
                    indev, indod, indlsev,jbasev,indlsod,jbasod
!
      integer gl_lats_index, latsmax
      integer global_time_sort_index_a(latg)
!
      real          fd2
!!
      include 'function2'
!
      real(kind=kind_dbl_prec) global_time_a(latg)
!
      real(kind=kind_dbl_prec), parameter :: cons0 = 0.d0, cons0p5  = 0.5d0,&
                                         cons1 = 1.d0, cons0p92 = 0.92d0
      real(kind=kind_dbl_prec) colat1
!
      gl_lats_index = 0
      global_lats_a = -1
      do lat = 1,latg                  !my intialize global_time_a to lonsperlat
          global_time_a(lat) = lonsperlat(lat)
      enddo

      do lat = 1, latg2
         lonsperlat(latg+1-lat) = lonsperlat(lat)
      end do
      do node=1,nodes
          call get_lats_node_a_stochy( node-1, global_lats_a,lats_nodes_a(node),& 
                               gl_lats_index,global_time_sort_index_a, iprint)
      enddo
      call setlats_a_stochy(lats_nodes_a,global_lats_a,iprint, lonsperlat)

      iprint = 0
      do node=1,nodes
         call get_ls_node_stochy( node-1, ls_nodes(1,node),max_ls_nodes(node), iprint )
      enddo
!
      len_trie_ls_max = 0
      len_trio_ls_max = 0
      do node=1,nodes
!
         len_trie_ls_nod = 0
         len_trio_ls_nod = 0
         do locl=1,max_ls_nodes(node)
            l=ls_nodes(locl,node)
            len_trie_ls_nod = len_trie_ls_nod+(jcap+3-l)/2
            len_trio_ls_nod = len_trio_ls_nod+(jcap+2-l)/2
         enddo
         len_trie_ls_max = max(len_trie_ls_max,len_trie_ls_nod)
         len_trio_ls_max = max(len_trio_ls_max,len_trio_ls_nod)
!
      enddo
!
      iprint = 0
!
      lats_dim_a = 0
      do node=1,nodes
         lats_dim_a = max(lats_dim_a,lats_nodes_a(node))
      enddo
      lats_node_a = lats_nodes_a(me+1)

      lats_node_a_max = 0
      do i=1,nodes
        lats_node_a_max = max(lats_node_a_max, lats_nodes_a(i))
      enddo
      latsmax = lats_node_a_max
 
!
      ipt_lats_node_ext = 1
!
      ipt_lats_node_a   = 1
      if ( me > 0 ) then
        do node=1,me
          ipt_lats_node_a = ipt_lats_node_a + lats_nodes_a(node)
        enddo
      endif

!
      iprint = 0
!
      if ( kind_dbl_prec == 8 ) then !------------------------------------
           call glats_stochy(latg2,colrad_a,wgt_a,wgtcs_a,rcs2_a,iprint)
           call epslon_stochy(epse,epso,epsedn,epsodn,ls_node)
           call pln2eo_a_stochy(plnev_a,plnod_a,epse,epso,colrad_a,ls_node,latg2)
           call gozrineo_a_stochy(plnev_a,plnod_a,pddev_a,pddod_a, &
                plnew_a,plnow_a,epse,epso,rcs2_a,wgt_a,ls_node,latg2)
!
      else !------------------------------------------------------------
           allocate  ( colrad_dp(latg2) )
           allocate  (    wgt_dp(latg2) )
           allocate  (  wgtcs_dp(latg2) )
           allocate  (   rcs2_dp(latg2) )
!
           allocate  (   epse_dp(len_trie_ls) )
           allocate  (   epso_dp(len_trio_ls) )
           allocate  ( epsedn_dp(len_trie_ls) )
           allocate  ( epsodn_dp(len_trio_ls) )
!
           allocate  (  plnev_dp(len_trie_ls) )
           allocate  (  plnod_dp(len_trio_ls) )
           allocate  (  pddev_dp(len_trie_ls) )
           allocate  (  pddod_dp(len_trio_ls) )
           allocate  (  plnew_dp(len_trie_ls) )
           allocate  (  plnow_dp(len_trio_ls) )

           call glats_stochy(latg2,colrad_dp,wgt_dp,wgtcs_dp,rcs2_dp,iprint)
!
           do i=1,latg2
              colrad_a(i) = colrad_dp(i)
                 wgt_a(i) =    wgt_dp(i)
               wgtcs_a(i) =  wgtcs_dp(i)
                rcs2_a(i) =   rcs2_dp(i)
           enddo
!
           call epslon_stochy(epse_dp,epso_dp,epsedn_dp,epsodn_dp,ls_node)
!
           do i=1,len_trie_ls
                epse(i) =   epse_dp(i)
              epsedn(i) = epsedn_dp(i)
           enddo
!
           do i=1,len_trio_ls
                epso(i) =   epso_dp(i)
              epsodn(i) = epsodn_dp(i)
           enddo
!
           do lat=1,latg2
!
              call pln2eo_a_stochy(plnev_dp,plnod_dp,epse_dp,epso_dp,colrad_dp(lat),ls_node,1)
!
              call gozrineo_a_stochy(plnev_dp,plnod_dp,pddev_dp,pddod_dp, plnew_dp,plnow_dp,&
                   epse_dp,epso_dp,rcs2_dp(lat),wgt_dp(lat),ls_node,1)
!
              do i=1,len_trie_ls
                 plnev_a(i,lat) = plnev_dp(i)
                 pddev_a(i,lat) = pddev_dp(i)
                 plnew_a(i,lat) = plnew_dp(i)
              enddo
              do i=1,len_trio_ls
                 plnod_a(i,lat) = plnod_dp(i)
                 pddod_a(i,lat) = pddod_dp(i)
                 plnow_a(i,lat) = plnow_dp(i)
              enddo
           enddo
!
           deallocate  ( colrad_dp, wgt_dp,   wgtcs_dp,  rcs2_dp ,  &
                         epse_dp,   epso_dp,  epsedn_dp, epsodn_dp, &
                         plnev_dp,  plnod_dp, pddev_dp,  pddod_dp , &
                         plnew_dp,  plnow_dp )
      endif !-----------------------------------------------------------
!
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         indev  = indlsev(l,l)
         do n = l, jcap, 2
            snnp1ev(indev) = n*(n+1)
              indev        = indev+1
         end do
      end do
!
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasod = ls_node(locl,3)
         if ( l <= jcap-1 ) then
            indod = indlsod(l+1,l)
            do n = l+1, jcap, 2
               snnp1od(indod) = n*(n+1)
                 indod        = indod+1
            end do
         end if
      end do
!
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         jbasod = ls_node(locl,3)
         if (mod(L,2) == mod(jcap+1,2)) then ! set even (n-l) terms of top row to zero
            snnp1ev(indlsev(jcap+1,l)) = cons0
         else                                ! set odd (n-l) terms of top row to zero
            snnp1od(indlsod(jcap+1,l)) = cons0
         endif
      enddo
!
      do j=1,latg
        if( j <= latg2 ) then
          sinlat_a(j) =  cos(colrad_a(j))
        else
          sinlat_a(j) = -cos(colrad_a(latg+1-j))
        endif
        coslat_a(j) = sqrt(1.-sinlat_a(j)*sinlat_a(j))
      enddo
!
      do L=0,jcap
         do lat = 1, latg2
            if ( L <= min(jcap,lonsperlat(lat)/2) ) then
               lat1s_a(L) = lat
               go to 200
            endif
         end do
  200    continue
      end do
!
      
      do j=1,lats_node_a
         lat = global_lats_a(ipt_lats_node_a-1+j)
         if ( lonsperlat(lat) == lonf ) then
            lon_dims_a(j) = lonfx
         else
            lon_dims_a(j) = lonsperlat(lat) + 2
         endif
      enddo
!
      return
      end
