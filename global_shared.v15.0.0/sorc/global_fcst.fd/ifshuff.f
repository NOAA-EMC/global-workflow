       subroutine if_shuff(global_lats_r_old,lats_nodes_r_old,
     .         global_lats_r,lats_nodes_r,timesum_r_b,kdt,ifshuff,
     .         shuffle_overhead)

c
      use resol_def
      use layout1
      use gg_def
      use vert_def
!     use sig_io
      use date_def
      use mpi_def
      implicit none
c
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
c
      integer               lats_nodes_r_old(nodes)
      integer              global_lats_r_old(latr)
      integer kdt
      logical ifshuff
      real(kind=kind_evod) timesum_r_b(latr)
      real timesumold,timesumnew,timeold,timenew
      real shuffle_overhead
      integer node,ilat
      integer lat_old,lat_new
c
      timesumold = 0.0
      timeold = 0.0
      timesumnew = 0.0
      timenew = 0.0
      lat_old = 0
      lat_new = 0
      do node=1,nodes
cmy old
      timesumold = 0.0
      timesumnew = 0.0
         do ilat=1,lats_nodes_r_old(node)
            lat_old = lat_old + 1
       timesumold=timesumold + timesum_r_b(global_lats_r_old(lat_old))
c$$$         if (node .eq. nodes) print*,' old node, lat time = ',
c$$$     .     node,timesum_r_b(global_lats_r_old(lat_old)),
c$$$     .     global_lats_r_old(lat_old),timesumold
         enddo
           print*,' for node  timesumold = ',node,timesumold
 
          timeold = max(timeold,timesumold)
cmy new
         do ilat=1,lats_nodes_r(node)
            lat_new = lat_new + 1
       timesumnew=timesumnew + timesum_r_b(global_lats_r(lat_new))
c$$$         if (node .eq. nodes) print*,' new node, lat time = ',
c$$$     .     node,timesum_r_b(global_lats_r(lat_new)),
c$$$     .     global_lats_r(lat_new),timesumnew
         enddo
         print*,' for node timesumnew = ',node,timesumnew
          timenew = max(timenew,timesumnew)
      enddo
c
      if (timenew+shuffle_overhead .le. timeold) ifshuff = .true.
 
      print*,' from if_shuff kdt,new,old,overhead = ',
     .  kdt,timenew,timeold,shuffle_overhead
 
      return
      end
