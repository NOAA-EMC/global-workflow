      subroutine setlats_a_stochy(lats_nodes_a,global_lats_a,
     &                         iprint,lonsperlat)
!
      use stochy_resol_def , only : latg,lonf
      use spectral_layout   , only : nodes,me
!
      implicit none
!
      integer, dimension(latg) :: global_lats_a, lonsperlat
      integer                     lats_nodes_a(nodes)

      integer              iprint,opt,ifin,nodesio
     &,                    jcount,jpt,lat,lats_sum,node,i,ii
     &,                    ngrptg,ngrptl,ipe,irest,idp
     &,                    ngrptgh,nodesioh
!    &,                    ilatpe,ngrptg,ngrptl,ipe,irest,idp
!
      integer,allocatable :: lats_hold(:,:)
!
      allocate ( lats_hold(latg,nodes) )
!
!     iprint = 1
      iprint = 0
      opt    = 1                       ! reduced grid
      if (opt == 2) lonsperlat = lonf  ! full grid
      lats_nodes_a = 0
!     if (liope .and. icolor == 2) then
!       nodesio = 1
!     else
        nodesio = nodes
!     endif
!
      ngrptg = 0
      do lat=1,latg
         do i=1,lonsperlat(lat)
           ngrptg = ngrptg + 1
         enddo
      enddo

!
!   ngrptg contains total number of grid points.
!
!     distribution of the grid
      nodesioh = nodesio / 2

      if (nodesioh*2 /= nodesio) then
!       ilatpe = ngrptg / nodesio
        ngrptl = 0
        ipe    = 0
        irest  = 0
        idp    = 1

        do lat=1,latg
          ifin   = lonsperlat(lat)
          ngrptl = ngrptl + ifin

!        if (me == 0)
!    &write(2000+me,*)'in setlats lat=',lat,' latg=',latg,' ifin=',ifin
!    &,' ngrptl=',ngrptl,' nodesio=',nodesio,' ngrptg=',ngrptg
!    &,' irest=',irest

          if (ngrptl*nodesio <= ngrptg+irest) then
            lats_nodes_a(ipe+1)  = lats_nodes_a(ipe+1) + 1
            lats_hold(idp,ipe+1) = lat
            idp = idp + 1
!         if (me == 0)
!    &    write(2000+me,*)' nodesio1=',nodesio,' idp=',idp,' ipe=',ipe
          else
            ipe = ipe + 1
            if (ipe <= nodesio) lats_hold(1,ipe+1) = lat
            idp    = 2
            irest  = irest + ngrptg - (ngrptl-ifin)*nodesio
            ngrptl = ifin
            lats_nodes_a(ipe+1) = lats_nodes_a(ipe+1) + 1
!          if (me == 0)
!    &     write(2000+me,*)' nodesio1=',nodesio,' idp=',idp,' ipe=',ipe
          endif
!         if (me == 0)
!    &  write(2000+me,*)' lat=',lat,' lats_nodes_a=',lats_nodes_a(ipe+1)
!    &,' ipe+1=',ipe+1
        enddo
      else
        nodesioh = nodesio/2
        ngrptgh  = ngrptg/2
        ngrptl = 0
        ipe    = 0
        irest  = 0
        idp    = 1

        do lat=1,latg/2
          ifin   = lonsperlat(lat)
          ngrptl = ngrptl + ifin

!        if (me == 0)
!    &write(0,*)'in setlats lat=',lat,' latg=',latg,' ifin=',ifin
!    &,' ngrptl=',ngrptl,' nodesio=',nodesio,' ngrptg=',ngrptg
!    &,' irest=',irest,' ngrptgh=',ngrptgh,' nodesioh=',nodesioh

          if (ngrptl*nodesioh <= ngrptgh+irest .or. lat == latg/2) then
            lats_nodes_a(ipe+1)  = lats_nodes_a(ipe+1) + 1
            lats_hold(idp,ipe+1) = lat
!           lats_nodes_a(nodesio-ipe)  = lats_nodes_a(nodesio-ipe) + 1
!           lats_hold(idp,nodesio-ipe) = latg+1-lat
            idp = idp + 1
!           if (me == 0)
!    &      write(0,*)' nodesio1=',nodesioh,' idp=',idp,' ipe=',ipe
          else
            ipe = ipe + 1
            if (ipe <= nodesioh) then
              lats_hold(1,ipe+1) = lat
!             lats_hold(1,nodesio-ipe) = latg+1-lat
            endif
            idp    = 2
            irest  = irest + ngrptgh - (ngrptl-ifin)*nodesioh
            ngrptl = ifin
            lats_nodes_a(ipe+1) = lats_nodes_a(ipe+1) + 1
!           lats_nodes_a(nodesio-ipe)  = lats_nodes_a(nodesio-ipe) + 1
!           if (me == 0)
!    &     write(0,*)' nodesio1h=',nodesioh,'idp=',idp,' ipe=',ipe
          endif
!         if (me == 0)
!    &  write(0,*)' lat=',lat,' lats_nodes_a=',lats_nodes_a(ipe+1)
!    &,' ipe+1=',ipe+1
        enddo
        do node=1, nodesioh
          ii = nodesio-node+1
          jpt = lats_nodes_a(node)
          lats_nodes_a(ii) = jpt
          do i=1,jpt
            lats_hold(jpt+1-i,ii) = latg+1-lats_hold(i,node)
          enddo
        enddo
          

      endif
!!
!!........................................................
!!
      jpt = 0
      do node=1,nodesio
!       write(2000+me,*)'node=',node,' lats_nodes_a=',lats_nodes_a(node)
!    &,           ' jpt=',jpt,' nodesio=',nodesio
        if ( lats_nodes_a(node) > 0 ) then
          do jcount=1,lats_nodes_a(node)
            global_lats_a(jpt+jcount) = lats_hold(jcount,node)
!           write(2000+me,*)' jpt+jcount=',jpt+jcount
!    &,             'global_lats_a=',global_lats_a(jpt+jcount)
          enddo
        endif
        jpt = jpt + lats_nodes_a(node)
      enddo
!!
      deallocate (lats_hold)
      if ( iprint /= 1 ) return
!!
      if (me == 0) then
      jpt=0
      do node=1,nodesio
         if ( lats_nodes_a(node) > 0 ) then
            print 600
            lats_sum=0
            do jcount=1,lats_nodes_a(node)
               lats_sum=lats_sum + lonsperlat(global_lats_a(jpt+jcount))
               print 700, node-1,
     x                    node,    lats_nodes_a(node),
     x                    jpt+jcount, global_lats_a(jpt+jcount),
     x                     lonsperlat(global_lats_a(jpt+jcount)),
     x                    lats_sum
            enddo
         endif
         jpt=jpt+lats_nodes_a(node)
      enddo
!
      print 600
!
  600 format ( ' ' )
!
  700 format (  'setlats  me=', i4,
     x          '  lats_nodes_a(',  i4, ' )=', i4,
     x          '  global_lats_a(', i4, ' )=', i4,
     x          '  lonsperlat=', i5,
     x          '  lats_sum=',   i6 )
!
      endif

      return
      end
