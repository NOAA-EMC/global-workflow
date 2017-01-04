      subroutine get_ls_node_stochy(me_fake,ls_node,ls_max_node_fake,
     c    iprint)
!
      use stochy_resol_def
      use spectral_layout
      implicit none
!
      integer   me_fake, ls_max_node_fake, iprint
      integer   ls_node(ls_dim)

      integer   ijk, jptls, l, node, nodesio
!
!jw      if (liope) then
!jw         if (icolor.eq.2) then
!jw           nodesio=1
!jw         else

           nodesio = nodes

!jw         endif
!jw      else
!jw         nodesio=nodes
!jw      endif
!!
      ls_node = -1
!
      jptls =  0
      l = 0
!.............................................
      do ijk=1,jcap1
!
         do node=1,nodesio
            if (node == me_fake+1) then
               jptls = jptls + 1
               ls_node(jptls) = l
            endif
            l = l + 1
            if (l > jcap) go to 200
         enddo
!
         do node=nodesio,1,-1
            if (node == me_fake+1) then
               jptls = jptls + 1
               ls_node(jptls) = l
            endif
            l = l + 1
            if (l > jcap) go to 200
         enddo
!
      enddo
!.............................................
!
  200 continue
!
!.............................................
!
      if(iprint == 1) print *, 'completed loop 200 in  get_ls_node'
      ls_max_node_fake = 0
      do ijk=1,ls_dim
         if(ls_node(ijk) >= 0) then
            ls_max_node_fake = ijk
            if(iprint == 1)
     x      print 230, me_fake, ijk, ls_node(ijk)
         endif
  230    format ('me_fake=',i5,'  get_ls_node  ls_node(', i5, ')=',i5)
      enddo
!
      if(iprint == 1)
     &   print*,'completed  get_ls_node, ls_max_node_fake=',
     &   ls_max_node_fake
!
      return
      end
