      subroutine get_lats_node_a_stochy(me_fake,global_lats_a,
     .          lats_nodes_a_fake,gl_lats_index,
     .          global_time_sort_index,iprint)
cc
      use stochy_resol_def
      use spectral_layout
      implicit none
cc
      integer   gl_lats_index,gl_start
      integer   me_fake
      integer   global_lats_a(latg)
      integer   lats_nodes_a_fake
      integer   iprint
cc
      integer   ijk
      integer   jptlats
      integer   lat
      integer   node,nodesio
      integer global_time_sort_index(latg)
      integer nodes_tmp
cc
c
!jw      if (liope) then
!jw         if (icolor.eq.2) then
!jw           nodesio=1
!jw         else
           nodesio=nodes
!jw         endif
!jw      else
!jw         nodesio=nodes
!jw      endif
!!
cc
      lat = 1
      nodes_tmp = nodes
!jw      if (liope .and. icolor .eq. 2) nodes_tmp = nodes -1
 
      gl_start = gl_lats_index
cc.............................................
      do ijk=1,latg
cc
         do node=1,nodes_tmp
            if (node.eq.me_fake+1) then
               gl_lats_index=gl_lats_index+1
            global_lats_a(gl_lats_index) = global_time_sort_index(lat)
            endif
            lat = lat + 1
            if (lat .gt. latg) go to 200
         enddo
cc
         do node=nodes_tmp,1,-1
            if (node.eq.me_fake+1) then
               gl_lats_index=gl_lats_index+1
            global_lats_a(gl_lats_index) = global_time_sort_index(lat)
            endif
            lat = lat + 1
            if (lat .gt. latg) go to 200
         enddo
cc
      enddo
cc.............................................
cc
  200 continue
cc
cc.............................................
cc
!jw      if (liope .and. icolor .eq. 2) gl_start = 0
         do node=1,nodes_tmp
            if (node.eq.me_fake+1) then
               lats_nodes_a_fake=gl_lats_index-gl_start
c$$$               print*,' setting lats_nodes_a_fake = ',
c$$$     .         lats_nodes_a_fake
            endif
         enddo
 
      if(iprint.eq.1) print 220
  220 format ('completed loop 200 in  get_lats_a  ')
c
      if(iprint.eq.1)
     x   print*,'completed  get_lats_node, lats_nodes_a_fake=',
     x   lats_nodes_a_fake
cc
      return
      end
