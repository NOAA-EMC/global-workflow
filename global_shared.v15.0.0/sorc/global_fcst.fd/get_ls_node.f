      subroutine get_ls_node(me_fake,ls_node,ls_max_node_fake,iprint)
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer   me_fake
      integer   ls_node(ls_dim)
      integer   ls_max_node_fake
      integer   iprint
cc
      integer   ijk
      integer   jptls
      integer   l
      integer   node,nodesio
cc
      if (liope) then
         if (icolor.eq.2) then
           nodesio=1
         else
           nodesio=nodes
         endif
      else
         nodesio=nodes
      endif
!!
      ls_node = -1
cc
      jptls =  0
      l = 0
cc.............................................
      do ijk=1,jcap1
cc
         do node=1,nodesio
            if (node.eq.me_fake+1) then
               jptls=jptls+1
               ls_node(jptls) = l
            endif
            l = l + 1
            if (l.gt.jcap) go to 200
         enddo
cc
         do node=nodesio,1,-1
            if (node.eq.me_fake+1) then
               jptls=jptls+1
               ls_node(jptls) = l
            endif
            l = l + 1
            if (l.gt.jcap) go to 200
         enddo
cc
      enddo
cc.............................................
cc
  200 continue
cc
cc.............................................
cc
      if(iprint.eq.1) print 220
  220 format ('completed loop 200 in  get_ls_node  ')
      ls_max_node_fake=0
      do ijk=1,ls_dim
         if(ls_node(ijk).ge.0)then
            ls_max_node_fake=ijk
            if(iprint.eq.1)
     x      print 230, me_fake, ijk, ls_node(ijk)
         endif
  230    format ('me_fake=',i5,'  get_ls_node  ls_node(', i5, ')=',i5)
      enddo
cc
      if(iprint.eq.1)
     x   print*,'completed  get_ls_node, ls_max_node_fake=',
     x   ls_max_node_fake
cc
      return
      end
