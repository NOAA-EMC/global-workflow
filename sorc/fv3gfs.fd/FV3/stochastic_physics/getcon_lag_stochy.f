      subroutine getcon_lag_stochy(lats_nodes_a,global_lats_a,
     &                      lats_nodes_h,global_lats_h_sn,
     &                      lonsperlat,xhalo,yhalo)
      use stochy_resol_def,     only : jcap,latg,latg2,lonf
      use spectral_layout,     only : me,nodes

      use stochy_gg_def,      only : colrad_a,sinlat_a
      use stochy_layout_lag,  only : ipt_lats_node_h,lat1s_h,lats_dim_h,
     &                          lats_node_h,lats_node_h_max,lon_dim_h
      implicit none
!
      integer     yhalo,xhalo
!
      integer, dimension(nodes) :: lats_nodes_a, lats_nodes_h
      integer, dimension(latg)  :: lonsperlat,   global_lats_a

      integer, dimension(latg+2*yhalo*nodes) :: global_lats_h_sn
!
      integer  i,j,l,n,lat,i1,i2,node,nodesio
      integer, dimension(latg+2*yhalo*nodes) :: global_lats_h_ns
!
      if (me == 0) print 100, jcap, me
100   format ('getcon_h jcap= ',i4,2x,'me=',i3)

      do lat = 1, latg2
         lonsperlat(latg+1-lat) = lonsperlat(lat)
      end do
      nodesio = nodes

!     print*,'con_h me,nodes,nodesio = ',me,nodes,nodesio

      call setlats_lag_stochy(lats_nodes_a,global_lats_a,
     &               lats_nodes_h,global_lats_h_ns,yhalo)

!  reverse order for use in set_halos

      i1 = 1
      i2 = 0
      do n=1,nodes
         j  = 0
         i2 = i2 + lats_nodes_h(n)
         do i=i1,i2
            j = j + 1
            global_lats_h_sn(i) = global_lats_h_ns(i2+1-j)
         enddo
         i1 = i2 + 1
      enddo

 830   format(10(i4,1x))
      lats_dim_h = 0
      do node=1,nodes
         lats_dim_h = max(lats_dim_h, lats_nodes_h(node))
      enddo
      lats_node_h     = lats_nodes_h(me+1)
      lats_node_h_max = 0
      do i=1,nodes
        lats_node_h_max  = max(lats_node_h_max, lats_nodes_h(i))
      enddo
      ipt_lats_node_h = 1
      if ( me > 0 ) then
         do node=1,me
            ipt_lats_node_h = ipt_lats_node_h + lats_nodes_h(node)
         enddo
      endif
      do j=1,latg2
        sinlat_a(j) = cos(colrad_a(j))
      enddo
      do l=0,jcap
         do lat = 1, latg2
            if ( l <= min(jcap,lonsperlat(lat)/2) ) then
               lat1s_h(l) = lat
               go to 200
            endif
         end do
  200    continue
      end do
      lon_dim_h = lonf + 1 + xhalo + xhalo !even/odd
      return
      end
