      subroutine setlats_a_ext(lats_nodes_a,lats_nodes_ext,
     .                   global_lats_a,
     &                   global_lats_ext,iprint,lonsperlat)
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer              lats_nodes_a(nodes)
cc
      integer              global_lats_a(latg)
      integer              lats_nodes_ext(nodes)
      integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
cc
      integer              iprint,opt,nodesio
cc
      integer              lonsperlat(latg)
cc
      integer              jcount,jpt,lats_sum,node,i
cc
cc
      opt=1
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
      do node=1,nodesio
        if (nodesio.eq.1) then
          lats_nodes_ext(node)=lats_nodes_a(node)+2*jintmx
        else
          if (node.eq.1.or.node.eq.nodesio) then
           lats_nodes_ext(node)=lats_nodes_a(node)+jintmx+nypt
          else
           lats_nodes_ext(node)=lats_nodes_a(node)+2*nypt
          endif
        endif
      enddo
cc........................................................
cc
cc
      jpt=0
      do node=1,nodesio
       if (nodesio.eq.1) then
        do i=1,jintmx
          global_lats_ext(i)=global_lats_a(1)
        enddo
        do i=1,jintmx
          global_lats_ext(jintmx+latg+i)=global_lats_a(latg)
        enddo
        do i=1,latg
          global_lats_ext(i+jintmx)=global_lats_a(i)
        enddo
       else
        do jcount=1,lats_nodes_a(node)
         global_lats_ext(jpt+jintmx+jcount+2*nypt*(node-1))=
     &               global_lats_a(jpt+jcount)
        enddo
        if (node.eq.1) then
         do i=1,jintmx
           global_lats_ext(i)=global_lats_a(1)
         enddo
         do i=1,nypt
           global_lats_ext(jintmx+lats_nodes_a(node)+i)=
     &               global_lats_a(lats_nodes_a(node))+i
         enddo
        elseif (node.eq.nodesio) then
         do i=1,jintmx
           global_lats_ext(latg+jintmx+2*nypt*(nodesio-1)+i)=
     &                    global_lats_a(latg)
         enddo
         do i=nypt,1,-1
           global_lats_ext(jpt+jintmx+2*nypt*(node-1)-i+1)=
     &                    global_lats_a(jpt)-i+1
         enddo
        else
         do i=nypt,1,-1
           global_lats_ext(jpt+jintmx+2*nypt*(node-1)-i+1)=
     &                    global_lats_a(jpt)-i+1
         enddo
         do i=1,nypt
         global_lats_ext(jpt+jintmx+2*nypt*(node-1)+
     &                    lats_nodes_a(node)+i)=
     &              global_lats_a(jpt+lats_nodes_a(node))+i
         enddo
        endif
       endif
        jpt=jpt+lats_nodes_a(node)
      enddo
cc
cc
 
      if ( iprint .ne. 1 ) return
cc
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_a(node) .gt. 0 ) then
            print 600
            lats_sum=0
            do jcount=1,lats_nodes_a(node)
               lats_sum=lats_sum + lonsperlat(global_lats_a(jpt+jcount))
               print 701, node-1,
     x                    node,    lats_nodes_a(node),
     x                    jpt+jcount, global_lats_a(jpt+jcount)
!selax                     lonsperlat(global_lats_a(jpt+jcount)),
!selax                    lats_sum
            enddo
         endif
         jpt=jpt+lats_nodes_a(node)
      enddo
cc
      print 600
cc
  600 format ( ' ' )
cc
  701 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4)
  700 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4,
     x          '  lonsperlat=', i5,
     x          '  lats_sum=',   i6 )
cc
      return
      end
c
      subroutine setlats_a_ext_shuff(lats_nodes_a,lats_nodes_ext,
     .                   global_lats_a,
     &                   global_lats_ext,iprint,lonsperlat)
cc
      use resol_def
      use layout1
      use mpi_def
      implicit none
cc
      integer              lats_nodes_a(nodes)
cc
      integer              global_lats_a(latg)
      integer              lats_nodes_ext(nodes)
      integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
cc
      integer              iprint,opt,nodesio
cc
      integer              lonsperlat(latg)
cc
      integer              jcount,jpt,lats_sum,node
cc
cc
      opt=1
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
      do node=1,nodesio
        if (nodesio.eq.1) then
          lats_nodes_ext(node)=lats_nodes_a(node)+2*jintmx
        else
          if (node.eq.1.or.node.eq.nodesio) then
           lats_nodes_ext(node)=lats_nodes_a(node)+jintmx+nypt
          else
           lats_nodes_ext(node)=lats_nodes_a(node)+2*nypt
          endif
        endif
      enddo
cc........................................................
cc
cc
c$$$      jpt=0
c$$$      do node=1,nodesio
c$$$       if (nodesio.eq.1) then
c$$$        do i=1,jintmx
c$$$          global_lats_ext(i)=global_lats_a(1)
c$$$        enddo
c$$$        do i=1,jintmx
c$$$          global_lats_ext(jintmx+latg+i)=global_lats_a(latg)
c$$$        enddo
c$$$        do i=1,latg
c$$$          global_lats_ext(i+jintmx)=global_lats_a(i)
c$$$        enddo
c$$$       else
c$$$        do jcount=1,lats_nodes_a(node)
c$$$         global_lats_ext(jpt+jintmx+jcount+2*nypt*(node-1))=
c$$$     &               global_lats_a(jpt+jcount)
c$$$        enddo
c$$$        if (node.eq.1) then
c$$$         do i=1,jintmx
c$$$           global_lats_ext(i)=global_lats_a(1)
c$$$         enddo
c$$$         do i=1,nypt
c$$$           global_lats_ext(jintmx+lats_nodes_a(node)+i)=
c$$$     &               global_lats_a(lats_nodes_a(node))+i
c$$$         enddo
c$$$        elseif (node.eq.nodesio) then
c$$$         do i=1,jintmx
c$$$           global_lats_ext(latg+jintmx+2*nypt*(nodesio-1)+i)=
c$$$     &                    global_lats_a(latg)
c$$$         enddo
c$$$         do i=nypt,1,-1
c$$$           global_lats_ext(jpt+jintmx+2*nypt*(node-1)-i+1)=
c$$$     &                    global_lats_a(jpt)-i+1
c$$$         enddo
c$$$        else
c$$$         do i=nypt,1,-1
c$$$           global_lats_ext(jpt+jintmx+2*nypt*(node-1)-i+1)=
c$$$     &                    global_lats_a(jpt)-i+1
c$$$         enddo
c$$$         do i=1,nypt
c$$$         global_lats_ext(jpt+jintmx+2*nypt*(node-1)+
c$$$     &                    lats_nodes_a(node)+i)=
c$$$     &              global_lats_a(jpt+lats_nodes_a(node))+i
c$$$         enddo
c$$$        endif
c$$$       endif
c$$$        jpt=jpt+lats_nodes_a(node)
c$$$      enddo
cc
cc
 
      if ( iprint .ne. 1 ) return
cc
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_a(node) .gt. 0 ) then
            print 600
            lats_sum=0
            do jcount=1,lats_nodes_a(node)
               lats_sum=lats_sum + lonsperlat(global_lats_a(jpt+jcount))
               print 701, node-1,
     x                    node,    lats_nodes_a(node),
     x                    jpt+jcount, global_lats_a(jpt+jcount)
!selax                     lonsperlat(global_lats_a(jpt+jcount)),
!selax                    lats_sum
            enddo
         endif
         jpt=jpt+lats_nodes_a(node)
      enddo
cc
      print 600
cc
  600 format ( ' ' )
cc
  701 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4)
  700 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4,
     x          '  lonsperlat=', i5,
     x          '  lats_sum=',   i6 )
cc
      return
      end
