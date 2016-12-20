      subroutine get_topo_grid_grad
     x    (cosf1,
     x     grad_gzlam,grad_gzphi,gzie_ln,gzio_ln,
     x     gz_grid,
     x     phige,phigo,
     x     for_gr_a_1,for_gr_a_2,
     x     trie_ls,trio_ls,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_a,global_lats_a,
     x     lonsperlat,
     x     epse,epso,
     x     plnev_a,plnod_a,
     x     i_write,r_rt)

      use machine   , only : kind_evod
      use resol_def , only : jcap,latg,latg2,levh,levs,lnt2,lonf,
     &                       p_dlam,p_dphi,p_zq
      use layout1   , only : ipt_lats_node_a,lat1s_a,lats_dim_a,
     &                       lats_node_a,len_trie_ls,len_trio_ls,
     &                       lon_dim_a,ls_dim,ls_max_node,me,nodes
      use sig_io    , only : z
      use physcons  , only : rerth => con_rerth,
     &                        grav => con_g,
     &                          rd => con_rd
      use namelist_def , only : ref_temp

      implicit none

      real(kind=kind_evod)      cosf1(          lats_dim_a)
      real(kind=kind_evod) grad_gzlam(lon_dim_a,lats_dim_a)
      real(kind=kind_evod) grad_gzphi(lon_dim_a,lats_dim_a)

      real(kind=kind_evod) gzie_ln(len_trie_ls,2)
      real(kind=kind_evod) gzio_ln(len_trio_ls,2)

      real(kind=kind_evod)    gz_grid(lon_dim_a,lats_dim_a)

      real(kind=kind_evod) phige(len_trie_ls,2)
      real(kind=kind_evod) phigo(len_trio_ls,2)

      real(kind=kind_evod) for_gr_a_1(lon_dim_a,2,lats_dim_a)
!mjr  real(kind=kind_evod) for_gr_a_2(lon_dim_a,2,lats_dim_a)
      real(kind=kind_evod) for_gr_a_2(lonf     ,2,lats_dim_a)

      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)

      integer              ls_node(ls_dim,3)
              
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod

      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_a(nodes)
      integer              global_lats_a(latg)
      integer                 lonsperlat(latg)

      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)

      real(kind=kind_evod)   plnev_a(len_trie_ls,latg2)
      real(kind=kind_evod)   plnod_a(len_trio_ls,latg2)

      integer i_write

      real(kind=kind_evod) trisca(lnt2)
      real(kind=kind_evod) tref,r_rt  

      integer i
      integer l
      integer lan
      integer lat
      integer locl
!mjr  integer lon_dim
      integer lons_lat
      integer n

      integer indlsev,jbasev,indev,indev1,indev2
      integer indlsod,jbasod,indod,indod1,indod2

      include 'function_indlsev'
      include 'function_indlsod'

cc--------------------------------------------------------------------

!    begin  calculation of grad(gz) +++++++++++++++++++++++++

      tref = ref_temp
!sela r_rt = 1./( rd*tref)
      trisca=z*grav ! z is in module sig_io ; set in treadeo

      if (me == 0) print*,' grav in get_topo_grid_grad =',grav,
     &                    ' r_rt=',r_rt

      call triseori(trisca,trie_ls(1,1,p_zq),
     &                     trio_ls(1,1,p_zq),1,ls_node)
       do i=1,len_trie_ls
        gzie_ln(i,1)=trie_ls(i,1,p_zq)*r_rt
        gzie_ln(i,2)=trie_ls(i,2,p_zq)*r_rt
       enddo
       do i=1,len_trio_ls
        gzio_ln(i,1)=trio_ls(i,1,p_zq)*r_rt
        gzio_ln(i,2)=trio_ls(i,2,p_zq)*r_rt
       enddo

      if ( i_write .eq. 1 ) then

      do locl=1,ls_max_node
         write(9000+me,'(" ")')
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         do indev = indev1 , indev2
            write(9000+me,
     &      '("l=",i6," indev=",i6," trie_ls=",2e25.15)')
     &         l, indev, trie_ls(indev,1,p_zq),
     &                   trie_ls(indev,2,p_zq)
         end do
      end do

      do locl=1,ls_max_node
         write(9000+me,'(" ")')
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod1 = indlsod(l+1,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indod2 = indlsod(jcap  ,l)
         else
            indod2 = indlsod(jcap+1,l)
         endif
         do indod = indod1 , indod2
            write(9000+me,
     &      '("l=",i6," indod=",i6," trio_ls=",2e25.15)')
     &         l, indod, trio_ls(indod,1,p_zq),
     &                   trio_ls(indod,2,p_zq)
         end do
      end do
      close(9000+me)

      endif  !  if ( i_write .eq. 1 )

      call delnpe(trie_ls(1,1,p_zq  ),
     x            trio_ls(1,1,p_dphi),
     x            trie_ls(1,1,p_dlam),
     x            epse,epso,ls_node)

      call delnpo(trio_ls(1,1,p_zq  ),
     x            trie_ls(1,1,p_dphi),
     x            trio_ls(1,1,p_dlam),
     x            epse,epso,ls_node)

      call sumfln_slg_gg(trie_ls(1,1,p_dlam),
     x                   trio_ls(1,1,p_dlam),
     x            lat1s_a,
     x            plnev_a,plnod_a,
     x            2     ,ls_node,latg2,
     x            lats_dim_a,2,
     x            for_gr_a_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_a,global_lats_a,
     x            lats_node_a,ipt_lats_node_a,lon_dim_a,
     x            lonsperlat,lon_dim_a,latg,0)

      do lan=1,lats_node_a
       lat = global_lats_a(ipt_lats_node_a-1+lan)
       lons_lat = lonsperlat(lat)

       call four_to_grid(for_gr_a_1(1,1,lan),
     &                   for_gr_a_2(1,1,lan),
     &                   lon_dim_a,lon_dim_a-2,lons_lat,2)

       do i=1,lons_lat
        grad_gzlam(i,lats_node_a+1-lan)=for_gr_a_2(i,1,lan)/cosf1(lan)
        grad_gzphi(i,lats_node_a+1-lan)=for_gr_a_2(i,2,lan)/cosf1(lan)
       enddo

      enddo ! do lan

      if ( i_write .eq. 1 ) then

      do lan=1,lats_node_a
       lat = global_lats_a(ipt_lats_node_a-1+lan)
       lons_lat = lonsperlat(lat)

       write(9400+me,'(" ")')
       do i=1,lons_lat
       write(9400+me,
     &      '("lan=",i6," i=",i6," grad lam phi=",2e25.15)')
     &         lan, i, grad_gzlam(i,lan),
     &                 grad_gzphi(i,lan)
       enddo

      enddo ! do lan

      close(9400+me)

      endif  !  if ( i_write .eq. 1 )

!    fin get_topo_grad_resonan grad(gz)  and gzie_ln, gzio_ln

cc--------------------------------------------------------------------

!mjr  trisca=z*grav ! z is in module sig_io ; set in treadeo
!mjr  print*,' grav in get_topo_grid_grad =',grav

      call triseori(trisca,phige(1,1),
     &                     phigo(1,1),1,ls_node)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if ( i_write .eq. 1 ) then
      do locl=1,ls_max_node
         write(8000+me,'(" ")')
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         do indev = indev1 , indev2
            write(8000+me,
     &      '("l=",i6," indev=",i6," phige=",2e25.15)')
     &         l, indev, phige(indev,1),
     &                   phige(indev,2)
         end do
      end do
      do locl=1,ls_max_node
         write(8000+me,'(" ")')
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod1 = indlsod(l+1,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indod2 = indlsod(jcap  ,l)
         else
            indod2 = indlsod(jcap+1,l)
         endif
         do indod = indod1 , indod2
            write(8000+me,
     &      '("l=",i6," indod=",i6," phigo=",2e25.15)')
     &         l, indod, phigo(indod,1),
     &                   phigo(indod,2)
         end do
      end do
      close(8000+me)
      endif  !  if ( i_write .eq. 1 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call sumfln_slg_gg(phige(1,1),
     x                   phigo(1,1),
     x            lat1s_a,
     x            plnev_a,plnod_a,
     x            1     ,ls_node,latg2,
     x            lats_dim_a,2,
     x            for_gr_a_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_a,global_lats_a,
!mjr x            lats_node_a,ipt_lats_node_a,lon_dims_a,
     x            lats_node_a,ipt_lats_node_a,lon_dim_a,
     x            lonsperlat,lon_dim_a,latg,0)
      do lan=1,lats_node_a
       lat = global_lats_a(ipt_lats_node_a-1+lan)
!mjr   lon_dim = lon_dims_a(lan)
       lons_lat = lonsperlat(lat)
       call four_to_grid(for_gr_a_1(1,1,lan),
     &                   for_gr_a_2(1,1,lan),
!mjr &                   lon_dim,  lon_dim    ,lons_lat,1)
     &                   lon_dim_a,lon_dim_a-2,lons_lat,1)
       do i=1,lons_lat
!mjr    gz_grid(i,lan)=for_gr_a_2(i,1,lan)

        gz_grid(i,lats_node_a+1-lan)=for_gr_a_2(i,1,lan)
       enddo
      enddo ! do lan
      if ( i_write .eq. 1 ) then
      do lan=1,lats_node_a
       lat = global_lats_a(ipt_lats_node_a-1+lan)
       lons_lat = lonsperlat(lat)
       write(8400+me,'(" ")')
       do i=1,lons_lat
       write(8400+me,
     &      '("lan=",i6," i=",i6," gz_grid  =",2e25.15)')
     &         lan, i, gz_grid(i,lan)
       enddo
      enddo ! do lan
      close(8400+me)
      endif  !  if ( i_write .eq. 1 )
      return
      end
