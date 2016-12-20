      subroutine setlats_lag(lats_nodes_a,global_lats_a,
     &                     lats_nodes_h,global_lats_h,yhalo)
!
      use resol_def , only : latg
      use layout1   , only : me,nodes
!
      implicit none
!
      integer              yhalo
!
      integer              lats_nodes_a(nodes)
      integer              lats_nodes_h(nodes)
!
      integer              global_lats_a(latg)
      integer              global_lats_h(latg+2*yhalo*nodes)
!
      integer              iprint
!
      integer              jj,jpt_a,jpt_h,lat_val,nn,nodes_lats
     &,                    j1, j2
!
      lats_nodes_h = 0
!
      nodes_lats = 0
      do nn=1,nodes
         if (lats_nodes_a(nn) > 0) then
             lats_nodes_h(nn) = lats_nodes_a(nn) + 2*yhalo
             nodes_lats       = nodes_lats + 1
         endif
      enddo
!
      global_lats_h = 0
!
!     set non-yhalo latitudes
!
      jpt_a = 0
      jpt_h = yhalo
      do nn=1,nodes
         if (lats_nodes_a(nn) > 0) then
            do jj=1,lats_nodes_a(nn)
               jpt_a = jpt_a + 1
               jpt_h = jpt_h + 1
               global_lats_h(jpt_h) = global_lats_a(jpt_a)
            enddo
            jpt_h = jpt_h + yhalo + yhalo
         endif
      enddo
!
      j1 = latg + 2*yhalo*nodes_lats
      do jj=1,yhalo
        j2 = yhalo - jj
!                                            set north pole yhalo
         global_lats_h(jj)    = global_lats_a(1) + j2
!                                            set south pole yhalo
         global_lats_h(j1-j2) =  global_lats_a(latg)-(jj-1)
      enddo
!
      if (lats_nodes_a(1).ne.latg) then
!
!       set non-polar south yhalos
         jpt_h = 0
         do nn=1,nodes-1
            jpt_h   = jpt_h + lats_nodes_h(nn)
            lat_val = global_lats_h(jpt_h-yhalo)
            do jj=1,yhalo
               global_lats_h(jpt_h-yhalo+jj) = min(lat_val+jj,latg)
            enddo
         enddo
!
!       set non-polar north yhalos
         jpt_h = 0
         do nn=1,nodes-1
            jpt_h   = jpt_h + lats_nodes_h(nn)
            lat_val = global_lats_h(jpt_h+yhalo+1)
            do jj=1,yhalo
               global_lats_h(jpt_h+yhalo-(jj-1)) = max(lat_val-jj,1)
            enddo
         enddo
!
      endif
!

      iprint = 0
!     iprint = 1
      if (iprint == 1 .and. me == 0) then
!
         write(me+6000,'("setlats_h   yhalo=",i3,"   nodes=",i3/)')
     &                 yhalo,nodes
!
         do nn=1,nodes
            write(me+6000,'("lats_nodes_a(",i4,")=",i4,"   ",
     &                   "   lats_nodes_h(",i4,")=",i4)')
     &                   nn, lats_nodes_a(nn),
     &                   nn, lats_nodes_h(nn)
         enddo
!
         jpt_a=0
         do nn=1,nodes
            if (lats_nodes_a(nn).gt.0) then
               write(me+6000,'(" ")')
               do jj=1,lats_nodes_a(nn)
                  jpt_a=jpt_a+1
                  write(me+6000,'(2i4,"   global_lats_a(",i4,")=",i4)')
     &                     nn, jj, jpt_a, global_lats_a(jpt_a)
               enddo
            endif
         enddo
!
         jpt_h=0
         do nn=1,nodes
            if (lats_nodes_h(nn).gt.0) then
               write(me+6000,'(" ")')
               do jj=1,lats_nodes_h(nn)
                  jpt_h=jpt_h+1
                  write(me+6000,'(2i4,"   global_lats_h(",i4,")=",i4)')
     &                     nn, jj, jpt_h, global_lats_h(jpt_h)
               enddo
            endif
         enddo
!
        close(6000+me)
      endif
!
      return
      end
