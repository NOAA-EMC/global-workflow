      subroutine gather_times_a(lats_nodes_a,global_lats_a,
     .                        global_times_a,global_time_a)

      use resol_def
      use layout1
      use mpi_def
      implicit none
      
      integer lan,ierr,ilat,lat,node,nsend
      integer              global_lats_a(latg)
      integer              lats_nodes_a(nodes)
      integer icount
 
      real(kind=kind_evod) global_times_a(latg,nodes)
      real(kind=kind_evod) global_time_a(latg)
      real(kind=kind_io8) tmps(lats_node_a_max,nodes)
      real(kind=kind_io8) tmpr(lats_node_a_max,nodes)
 
      tmps = -55.55
      if (nodes.ne.1) then
        icount = 0
        do node=1,nodes
          do lan=1,lats_node_a
             icount = icount + 1
           lat=global_lats_a(ipt_lats_node_a-1+lan)
           tmps(lan,node)=global_times_a(lat,me+1)
c$$$           tmps(lan,node) = node * 1000 + lan
          enddo
        enddo
!sela   write(600+me,*) ' icount = ',icount
c
!sela   do node=1,nodes
!sela        write(600+me,*) ' node = ',node
!sela        write(600+me,620) (tmps(lan,node),lan=1,lats_node_a_max)
 620          format(5(e13.5,3x))
!sela   enddo
!!
        nsend=lats_node_a_max
c$$$        print*,' nsend = ',nsend
 
        call mpi_alltoall(tmps,nsend,mpi_a_def,
     x                     tmpr,nsend,mpi_a_def,
     x                     mc_comp,ierr)
!!
 
!sela   do node=1,nodes
!sela     write(700+me,*) ' node = ',node
!sela     write(700+me,620) (tmpr(lan,node),lan=1,lats_node_a_max)
!sela   enddo
 
        ilat=1
        do node=1,nodes
          do lan=1,lats_nodes_a(node)
             lat=global_lats_a(ilat)
             global_time_a(lat)=tmpr(lan,node)
             ilat=ilat+1
          enddo
        enddo
      else
        do lan=1,latg
           global_time_a(lan) = global_times_a(lan,1)
        enddo
      endif
      return
      end
c
      subroutine gather_times_r(lats_nodes_r,global_lats_r,
     .                        global_times_r,global_time_r)

      use resol_def
      use layout1
      use mpi_def
      implicit none 
      integer lan,ierr,ilat,lat,node,nsend
      integer              global_lats_r(latr)
      integer              lats_nodes_r(nodes)
      integer icount
 
      real(kind=kind_evod) global_times_r(latr,nodes)
      real(kind=kind_evod) global_time_r(latr)
      real(kind=kind_io8) tmps(lats_node_r_max,nodes)
      real(kind=kind_io8) tmpr(lats_node_r_max,nodes)
 
      tmps = -55.55
      if (nodes.ne.1) then
        icount = 0
        do node=1,nodes
          do lan=1,lats_node_r
             icount = icount + 1
           lat=global_lats_r(ipt_lats_node_r-1+lan)
           tmps(lan,node)=global_times_r(lat,me+1)
c$$$           tmps(lan,node) = node * 1000 + lan
          enddo
        enddo
!sela   write(600+me,*) ' icount = ',icount
c
!sela   do node=1,nodes
!sela        write(600+me,*) ' node = ',node
!sela        write(600+me,620) (tmps(lan,node),lan=1,lats_node_a_max)
 620          format(5(e13.5,3x))
!sela   enddo
!!
        nsend=lats_node_r_max
c$$$        print*,' nsend = ',nsend
 
        call mpi_alltoall(tmps,nsend,mpi_a_def,
     x                     tmpr,nsend,mpi_a_def,
     x                     mc_comp,ierr)
!!
 
!sela   do node=1,nodes
!sela     write(700+me,*) ' node = ',node
!sela     write(700+me,620) (tmpr(lan,node),lan=1,lats_node_a_max)
!sela   enddo
 
        ilat=1
        do node=1,nodes
          do lan=1,lats_nodes_r(node)
             lat=global_lats_r(ilat)
             global_time_r(lat)=tmpr(lan,node)
             ilat=ilat+1
          enddo
        enddo
      else
        do lan=1,latr
           global_time_r(lan) = global_times_r(lan,1)
        enddo
      endif
      return
      end
