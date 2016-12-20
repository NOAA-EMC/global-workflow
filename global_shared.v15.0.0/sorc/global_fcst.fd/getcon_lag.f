      subroutine getcon_lag(lats_nodes_a,global_lats_a,
     x                    lats_nodes_h,global_lats_h_sn,
     x                    lonsperlat,xhalo,yhalo)
      use resol_def    , only : jcap,latg,latg2,lonf
      use layout1      , only : me,nodes

      use gg_def       , only : colrad_a,sinlat_a
      use namelist_def , only : shuff_lats_a
      use mpi_def      , only : icolor,liope
      use layout_lag   , only : ipt_lats_node_h,lat1s_h,lats_dim_h,
     &                          lats_node_h,lats_node_h_max,lon_dim_h
      implicit none
!
      integer     yhalo,xhalo
!
      integer     lats_nodes_a(nodes)
      integer     lats_nodes_h(nodes)
      integer    global_lats_a(latg)
      integer    global_lats_h_sn(latg+2*yhalo*nodes)
      integer    global_lats_h_ns(latg+2*yhalo*nodes)
      integer       lonsperlat(latg)
!
      integer  i,j,l,n,lat,i1,i2
      integer  node,nodesio
!     logical  shuffled
!
      if (me == 0) print 100, jcap, me
100   format ('getcon_h jcap= ',i4,2x,'me=',i3)

      do lat = 1, latg2
         lonsperlat(latg+1-lat) = lonsperlat(lat)
      end do
      if (liope) then
         if (icolor.eq.2) then
           nodesio=1
         else
           nodesio=nodes
         endif
      else
         nodesio=nodes
      endif

!     print*,'con_h me,liope,nodes,nodesio = ',me,liope,nodes,nodesio

      shuff_lats_a = .false.

!     print*,' shuff_lats_a = ',shuff_lats_a

!     shuffled = shuff_lats_a

!     print*,'  else in if shuff_lats_a=',shuff_lats_a

      call setlats_lag(lats_nodes_a,global_lats_a,
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

!      write(0,*) ' getcon after setlats_h global_lats_a = '
!sela  write(6,830)     global_lats_a

!      write(0,*) ' getcon after setlats_h global_lats_h_ns = '
!sela  write(6,830)     global_lats_h_ns

 830   format(10(i4,1x))
      lats_dim_h=0
      do node=1,nodes
         lats_dim_h = max(lats_dim_h,lats_nodes_h(node))
      enddo
      lats_node_h = lats_nodes_h(me+1)
      lats_node_h_max=0
      do i=1,nodes
        lats_node_h_max=max(lats_node_h_max,lats_nodes_h(i))
      enddo
      ipt_lats_node_h=1
      if ( me .gt. 0 ) then
         do node=1,me
            ipt_lats_node_h = ipt_lats_node_h + lats_nodes_h(node)
         enddo
      endif
      if (liope .and. icolor .eq. 2) then
            ipt_lats_node_h = 1
      endif
      do j=1,latg2
        sinlat_a(j) = cos(colrad_a(j))
      enddo
      do l=0,jcap
         do lat = 1, latg2
            if ( l .le. min(jcap,lonsperlat(lat)/2) ) then
               lat1s_h(l) = lat
               go to 200
            endif
         end do
  200    continue
      end do
ccmr
!mjr  allocate ( lon_dims_h_coef(lats_node_h) )
!mjr  allocate ( lon_dims_h_grid(lats_node_h) )
ccmr
!mjr  do j=1,lats_node_h
!mjr     lat = global_lats_h_ns(ipt_lats_node_h-1+j)
!mjr     if ( lonsperlat(lat) .eq. lonf ) then
!mjr        lon_dims_h_coef(j) = lonf +2 + 2*xhalo !even
!mjr        lon_dims_h_grid(j) = lonf +1 + 2*xhalo !even/odd
!mjr     else
ccmr        lon_dims_h_coef(j) = lonsperlat(lat) +2 + 2*xhalo !even
ccmr        lon_dims_h_grid(j) = lonsperlat(lat) +1 + 2*xhalo !even/odd
!mjr        lon_dims_h_coef(j) = lonf +2 + 2*xhalo !even
!mjr        lon_dims_h_grid(j) = lonf +1 + 2*xhalo !even/odd
!mjr     endif
!mjr  enddo
!mjr
      lon_dim_h = lonf +1 + 2*xhalo !even/odd
!mjr
      return
      end
