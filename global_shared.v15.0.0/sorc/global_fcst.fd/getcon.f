      subroutine getcon(nges,nradr,nradf,nnmod,
!     subroutine getcon(n1,n2,nges,nradr,nradf,nnmod,
     x                  n3,n4,nflps,nsigi,nsigs,nsfci,
     x                  nznli,nsfcf,nznlf,nsfcs,nznls,
     x                  ndgi,ndgf,ngpken,
     x                  mods,niter,ini,nstep,nfiles,
     x                  ksout,ifges,ibrad,
     x                  ls_node,ls_nodes,max_ls_nodes,
     x                  lats_nodes_a,global_lats_a,
     x                  lonsperlat,
     x                  lats_nodes_r,global_lats_r,
     x                  lonsperlar,
!    x                  lats_nodes_ext,global_lats_ext,
     x                  epse,epso,epsedn,epsodn,
     x                  snnp1ev,snnp1od,ndexev,ndexod,
     x                  plnev_a,plnod_a,pddev_a,pddod_a,
     x                  plnew_a,plnow_a,
     x                  plnev_r,plnod_r,pddev_r,pddod_r,
     x                  plnew_r,plnow_r,colat1)
cc
      use machine      , only : kind_dbl_prec,kind_evod
      use resol_def    , only : jcap,latg,latg2,latr,latr2,lonf,lonrx,
     &                          lonr
      use layout1      , only : ipt_lats_node_a,lat1s_a,lats_dim_a,
     &                          ipt_lats_node_r,lat1s_r,lats_dim_r,
     &                          lats_node_a,lats_node_a_max,lon_dim_a,
     &                          lats_node_r,lats_node_r_max,lon_dim_r,
     &                          len_trie_ls,len_trie_ls_max,
     &                          len_trio_ls,len_trio_ls_max,
     &                          ls_dim,ls_max_node,me,me_l_0,nodes,
     &                          lon_dims_r

      use gg_def       , only : coslat_r,
     &                          colrad_a,rcs2_a,sinlat_a,wgt_a,wgtcs_a,
     &                          colrad_r,rcs2_r,sinlat_r,wgt_r,wgtcs_r
!mjr  use vert_def
!     use sig_io
!mjr  use date_def
      use namelist_def , only : ndsl,shuff_lats_a,shuff_lats_r
      use mpi_def      , only : icolor,liope,comp_task
!
      implicit none
!!
      integer              ibrad,ifges,ini,j,ksout,l,lat,mods
      integer              n,n3,n4,ndgf,ndgi,nfiles,nflps
!     integer              n,n1,n2,n3,n4,ndgf,ndgi,nfiles,nflps
      integer              nges,ngpken,niter,nnmod,nradf,nradr
      integer              nsfcf,nsfci,nsfcs,nsigi,nsigs,nstep
      integer              nznlf,nznli,nznls,i
!
      integer              ls_node(ls_dim,3)
!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
      integer                 max_ls_nodes(nodes)
!
      integer               lats_nodes_a(nodes)
      integer              global_lats_a(latg)
      integer                 lonsperlat(latg)
!
      integer               lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
!
!     integer                lats_nodes_ext(nodes)
!     integer        global_lats_ext(latg+2*jintmx+2*nypt*(nodes-1))
!
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
!
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
!
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
!
      real(kind=kind_evod) plnev_a(len_trie_ls,latg2)
      real(kind=kind_evod) plnod_a(len_trio_ls,latg2)
      real(kind=kind_evod) pddev_a(len_trie_ls,latg2)
      real(kind=kind_evod) pddod_a(len_trio_ls,latg2)
      real(kind=kind_evod) plnew_a(len_trie_ls,latg2)
      real(kind=kind_evod) plnow_a(len_trio_ls,latg2)
!
      real(kind=kind_evod) plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnod_r(len_trio_ls,latr2)
      real(kind=kind_evod) pddev_r(len_trie_ls,latr2)
      real(kind=kind_evod) pddod_r(len_trio_ls,latr2)
      real(kind=kind_evod) plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnow_r(len_trio_ls,latr2)

      real(kind=kind_dbl_prec) ,allocatable:: colrad_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::    wgt_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  wgtcs_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::   rcs2_dp(:)
!
      real(kind=kind_dbl_prec) ,allocatable::   epse_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::   epso_dp(:)
      real(kind=kind_dbl_prec) ,allocatable:: epsedn_dp(:)
      real(kind=kind_dbl_prec) ,allocatable:: epsodn_dp(:)
!
      real(kind=kind_dbl_prec) ,allocatable::  plnev_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  plnod_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  pddev_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  pddod_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  plnew_dp(:)
      real(kind=kind_dbl_prec) ,allocatable::  plnow_dp(:)

      integer              iprint,locl,node,nodesio
      integer              len_trie_ls_nod
      integer              len_trio_ls_nod
!
      integer              indev
      integer              indod
!
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      integer gl_lats_index
      integer global_time_sort_index_a(latg)
      integer global_time_sort_index_r(latr)
      integer nodes_tmp
!
      include 'function_indlsev'
      include 'function_indlsod'
!
      real(kind=kind_evod) global_time_a(latg)
      real(kind=kind_evod) global_time_r(latr)
!
      logical shuffled
!
      real(kind=kind_evod) colat1
!
      real(kind=kind_evod), parameter :: cons0    = 0.d0 
!    &                                  ,cons0p5  = 0.5d0,
!    &                                   cons0p92 = 0.92d0,
!    &                                   cons1    = 1.d0
!
!sela print 100, jcap, levs
100   format (1h0,'getcon ',i3,i3,'created april 2000')
!
      do lat = 1, latg2
         lonsperlat(latg+1-lat) = lonsperlat(lat)
      end do
!
      do lat = 1, latr2
         lonsperlar(latr+1-lat) = lonsperlar(lat)
      end do
!
      if (liope .and. icolor == 2) then
         nodesio = 1
      else
         nodesio = nodes
      endif
!
!sela print*,'me,liope,nodes,nodesio = ',me,liope,nodes,nodesio
      if (nodesio == 1 .and. nodes == 1
     &    .or. (nodes == 2 .and. nodesio == 1) ) then
         shuff_lats_a = .false.
         shuff_lats_r = .false.
 
!sela   print*,' no shuffling with 1 compute task - nodes = ',nodes
      endif
!sela print*,' shuff_lats_a,shuff_lats_r = ',shuff_lats_a,shuff_lats_r

      shuffled = shuff_lats_a .or. shuff_lats_r
 
!!
 
      iprint = 0
!     iprint = 1
      if (shuff_lats_a) then
!!
!my      global_lats_a is latg ;  call again for sh
 
         gl_lats_index = 0
         global_lats_a = -1
!my                   intialize global_time_a to lonsperlat
         do lat = 1,latg
           global_time_a(lat) = lonsperlat(lat)
         enddo
!
!my                   sort the lat times in descending order
!
         if (iprint .eq. 1)
     .      print*,' before sortrx global_time_a = ',
     &            global_time_a

         call sortrx(latg,-global_time_a,global_time_sort_index_a)

         if (iprint .eq. 1)
     .      print*,' after sortrx global_time_sort_index_a = ',
     .      global_time_sort_index_a
 
!my          input lat time index in descending order
!my          output global_lats_a and lats_nodes_a (gl_lats_index temp)
!my
         gl_lats_index = 0
         nodes_tmp = nodes
         if (liope .and. icolor .eq. 2) then
!        if (.not. comp_task) then
           nodes_tmp           = nodes - 1
           lats_nodes_a(nodes) = 0 ! gwvbugfix initialize lats_nodes_a(iotask)
                                   ! which is not set in the loop below
         endif
         do node=1,nodes_tmp
           call get_lats_node_a( node-1, global_lats_a,
     x                          lats_nodes_a(node), gl_lats_index,
     &                          global_time_sort_index_a,iprint)
           if (me+1 .eq. node .and. iprint .eq. 1)
     .       print*,' node, lats_nodes_a = ',node,lats_nodes_a(node)
         enddo
 
!sela print*,' global_lats_a = ',global_lats_a
 
!my the extended part will not work with shuffled lats
!my it assumes sequential ordered lats
!        call setlats_a_ext_shuff(lats_nodes_a,lats_nodes_ext,
!    &              global_lats_a, global_lats_ext,iprint,lonsperlat)
 
      else
 
!        call setlats_a(lats_nodes_a,lats_nodes_ext,global_lats_a,
!    &                  global_lats_ext,iprint,lonsperlat)

         call setlats_a(lats_nodes_a,global_lats_a,iprint,lonsperlat)
 
      endif                       ! shuff_lats_a

!sela  write(6,*) ' getcon after setlats_a global_lats_a = '
!sela  write(6,830)     global_lats_a
 830   format(15(i3,1x))
 
      iprint = 0
      do node=1,nodesio
!
         call get_ls_node( node-1, ls_nodes(1,node),
     x                     max_ls_nodes(node), iprint )
!
      enddo
!
      len_trie_ls_max = 0
      len_trio_ls_max = 0
!
      do node=1,nodesio
         len_trie_ls_nod = 0
         len_trio_ls_nod = 0
         do locl=1,max_ls_nodes(node)
            l = ls_nodes(locl,node)
            len_trie_ls_nod = len_trie_ls_nod + (jcap+3-l)/2
            len_trio_ls_nod = len_trio_ls_nod + (jcap+2-l)/2
            if ( l .eq. 0 ) me_l_0 = node-1
         enddo
         len_trie_ls_max = max(len_trie_ls_max,len_trie_ls_nod)
         len_trio_ls_max = max(len_trio_ls_max,len_trio_ls_nod)
      enddo
!
      if (shuff_lats_r) then !my *********** new code for setlats_r
        gl_lats_index = 0
        global_lats_r = -1
                              !my intialize global_time_r to lonsperlar
        do lat = 1,latr
          global_time_r(lat) = lonsperlar(lat)
        enddo
!
!my                  sort the lat times in descending order
!
        call sortrx(latr,-global_time_r,global_time_sort_index_r)
        if (iprint .eq. 1) print*,' after sortrx for r index = ',
     .      global_time_sort_index_r
 
!my input  lat time index in descending order
!my output global_lats_r and lats_nodes_r (gl_lats_index temp)
!
        gl_lats_index = 0
!
        nodes_tmp = nodes
        if (liope .and. icolor .eq. 2) then
!       if (.not. comp_task) then
          nodes_tmp           = nodes - 1
          lats_nodes_r(nodes) = 0  ! gwvbugfix initialize lats_nodes_r(iotask)
                                   ! which is not set in the loop below
        endif
        do node=1,nodes_tmp
          call get_lats_node_r( node-1, global_lats_r,
     x                         lats_nodes_r(node), gl_lats_index,
     &                         global_time_sort_index_r,iprint)
          if (me+1 .eq. node .and. iprint .eq. 1)
     .        print*,' node, lats_nodes_r = ',node,lats_nodes_r(node)
        enddo
      else
        call setlats_r(lats_nodes_r,global_lats_r,iprint,lonsperlar)
      endif ! shuff_lats_r
!
!sela  write(6,*) ' getcon after setlats_r global_lats_r = '
!sela  write(6,830)     global_lats_r
!
      iprint = 0
!
      lats_dim_a = 0
      lats_dim_r = 0
      do node=1,nodes
         lats_dim_a = max(lats_dim_a,lats_nodes_a(node))
         lats_dim_r = max(lats_dim_r,lats_nodes_r(node))
      enddo
!
!my need to set lats_dim_ext used for declaring variables in main,digifilt,dotstep
!$$$      if (.not. shuffled) then
!     lats_dim_ext=0
!$$$      nodes_tmp = nodes
!$$$      if (liope .and. icolor .eq. 2) nodes_tmp = nodes-1
!$$$      do node=1,nodes_tmp
!     do node=1,nodes
!            lats_dim_ext =
!    .   max(lats_dim_ext, lats_nodes_ext(node), lats_nodes_r(node))
!selaxxx      print*,' node,lats_dim_ext = ',node,lats_dim_ext
!     enddo
!$$$      endif
!
      lats_node_a = lats_nodes_a(me+1)
      lats_node_r = lats_nodes_r(me+1)
 
!$$$      if (.not. shuffled) lats_node_ext = lats_nodes_ext(me+1)
!        lats_node_ext = lats_nodes_ext(me+1)
!
      lats_node_r_max = 0
      do i=1,nodes
        lats_node_r_max = max(lats_node_r_max,lats_nodes_r(i))
      enddo
!
      lats_node_a_max = 0
      do i=1,nodes
        lats_node_a_max = max(lats_node_a_max,lats_nodes_a(i))
      enddo
 
!
      ipt_lats_node_a = 1
      ipt_lats_node_r = 1
!     ipt_lats_node_ext = 1
 
!     if ( .not. shuffled .and. me .gt. 0 ) then
!        do node=1,me
!         ipt_lats_node_ext = ipt_lats_node_ext + lats_nodes_ext(node)
!        enddo
!     endif
!
      if ( me .gt. 0 ) then
         do node=1,me
            ipt_lats_node_a = ipt_lats_node_a + lats_nodes_a(node)
            ipt_lats_node_r = ipt_lats_node_r + lats_nodes_r(node)
!$$$          ipt_lats_node_ext = ipt_lats_node_ext + lats_nodes_ext(node)
!$$$           print*,' node,ipt_lats_node_ext = ',node,ipt_lats_node_ext
         enddo
      endif
      if (liope .and. icolor .eq. 2) then
!     if (.not. comp_task) then
            ipt_lats_node_a = 1
            ipt_lats_node_r = 1
!           ipt_lats_node_ext = 1
      endif
!
!sela print*,' ipt_lats_node_a = ',ipt_lats_node_a
!sela print*,' ipt_lats_node_r = ',ipt_lats_node_r
!sela print*,' ipt_lats_node_ext = ',ipt_lats_node_ext
!
!sela filta= cons0p92                    !constant
!sela filtb =(cons1-filta) * cons0p5     !constant
!
!     n1    = 11
!     n2    = 12
      n3    = 51
      n4    = 52
!
      iprint = 0
!$$$      if ( me .eq. 0 ) iprint = 1
!
      iprint = 0
!$$$      if ( me .eq. 0 ) iprint = 1
      if ( kind_evod == 8 ) then !------------------------------------
           call glats(latg2,colrad_a,wgt_a,wgtcs_a,rcs2_a,iprint)
           call glats(latr2,colrad_r,wgt_r,wgtcs_r,rcs2_r,iprint)
!!
           colat1 = colrad_r(1)
!!
           do i=latr2+1,latr
              colrad_r(i) = colrad_r(latr+1-i)
           enddo
!
           call epslon(epse,epso,epsedn,epsodn,ls_node)
!
           call pln2eo_a(plnev_a,plnod_a,epse,epso,colrad_a,
     &                   ls_node,latg2)
!
           call pln2eo_r(plnev_r,plnod_r,epse,epso,colrad_r,
     &                   ls_node,latr2)
!
           call gozrineo_a(plnev_a,plnod_a,pddev_a,pddod_a,
     &                     plnew_a,plnow_a,epse,epso,
     &                     rcs2_a,wgt_a,ls_node,latg2)
!
           call gozrineo_r(plnev_r,plnod_r,pddev_r,pddod_r,
     &                     plnew_r,plnow_r,epse,epso,
     &                     rcs2_r,wgt_r,ls_node,latr2)
      else !------------------------------------------------------------
           allocate  ( colrad_dp(max(latg2,latr2)) )
           allocate  (    wgt_dp(max(latg2,latr2)) )
           allocate  (  wgtcs_dp(max(latg2,latr2)) )
           allocate  (   rcs2_dp(max(latg2,latr2)) )
!
           allocate  (   epse_dp(len_trie_ls) )
           allocate  (   epso_dp(len_trio_ls) )
           allocate  ( epsedn_dp(len_trie_ls) )
           allocate  ( epsodn_dp(len_trio_ls) )
!
           allocate  (  plnev_dp(len_trie_ls) )
           allocate  (  plnod_dp(len_trio_ls) )
           allocate  (  pddev_dp(len_trie_ls) )
           allocate  (  pddod_dp(len_trio_ls) )
           allocate  (  plnew_dp(len_trie_ls) )
           allocate  (  plnow_dp(len_trio_ls) )
           call glats(latg2,colrad_dp,wgt_dp,wgtcs_dp,rcs2_dp,iprint)
!
           do i=1,latg2
              colrad_a(i) = colrad_dp(i)
                 wgt_a(i) =    wgt_dp(i)
               wgtcs_a(i) =  wgtcs_dp(i)
                rcs2_a(i) =   rcs2_dp(i)
           enddo
!
           call epslon(epse_dp,epso_dp,epsedn_dp,epsodn_dp,ls_node)
!
           do i=1,len_trie_ls
                epse(i) =   epse_dp(i)
              epsedn(i) = epsedn_dp(i)
           enddo
!
           do i=1,len_trio_ls
                epso(i) =   epso_dp(i)
              epsodn(i) = epsodn_dp(i)
           enddo
!
           do lat=1,latg2
!
              call pln2eo_a(plnev_dp,plnod_dp,epse_dp,epso_dp,
     &                      colrad_dp(lat),ls_node,1)
!
              call gozrineo_a(plnev_dp,plnod_dp,pddev_dp,pddod_dp,
     &                        plnew_dp,plnow_dp,epse_dp,epso_dp,
     &                        rcs2_dp(lat),wgt_dp(lat),ls_node,1)
!
              do i=1,len_trie_ls
                 plnev_a(i,lat) = plnev_dp(i)
                 pddev_a(i,lat) = pddev_dp(i)
                 plnew_a(i,lat) = plnew_dp(i)
              enddo
!
              do i=1,len_trio_ls
                 plnod_a(i,lat) = plnod_dp(i)
                 pddod_a(i,lat) = pddod_dp(i)
                 plnow_a(i,lat) = plnow_dp(i)
              enddo
!
           enddo
!
           call glats(latr2,colrad_dp,wgt_dp,wgtcs_dp,rcs2_dp,iprint)
!!
           colat1 = colrad_dp(1)
!!
           do i=1,latr2
              colrad_r(i) = colrad_dp(i)
                 wgt_r(i) =    wgt_dp(i)
               wgtcs_r(i) =  wgtcs_dp(i)
                rcs2_r(i) =   rcs2_dp(i)
           enddo
!
           do i=latr2+1,latr
              colrad_r(i) = colrad_dp(latr+1-i)
           enddo
!
           do lat=1,latr2
!
              call pln2eo_r(plnev_dp,plnod_dp,epse_dp,epso_dp,
     &                      colrad_dp(lat),ls_node,1)
!
              call gozrineo_r(plnev_dp,plnod_dp,pddev_dp,pddod_dp,
     &                        plnew_dp,plnow_dp,epse_dp,epso_dp,
     &                        rcs2_dp(lat),wgt_dp(lat),ls_node,1)
!
              do i=1,len_trie_ls
                 plnev_r(i,lat) = plnev_dp(i)
                 pddev_r(i,lat) = pddev_dp(i)
                 plnew_r(i,lat) = plnew_dp(i)
              enddo
!
              do i=1,len_trio_ls
                 plnod_r(i,lat) = plnod_dp(i)
                 pddod_r(i,lat) = pddod_dp(i)
                 plnow_r(i,lat) = plnow_dp(i)
              enddo
!
           enddo
           deallocate  ( colrad_dp )
           deallocate  (    wgt_dp )
           deallocate  (  wgtcs_dp )
           deallocate  (   rcs2_dp )
!
           deallocate  (   epse_dp )
           deallocate  (   epso_dp )
           deallocate  ( epsedn_dp )
           deallocate  ( epsodn_dp )
!
           deallocate  (  plnev_dp )
           deallocate  (  plnod_dp )
           deallocate  (  pddev_dp )
           deallocate  (  pddod_dp )
           deallocate  (  plnew_dp )
           deallocate  (  plnow_dp )
      endif !-----------------------------------------------------------
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         indev  = indlsev(l,l)
         do n = l, jcap, 2
             ndexev(indev) = n
            snnp1ev(indev) = n*(n+1)
              indev        = indev+1
         end do
      end do
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasod = ls_node(locl,3)
         if ( l .le. jcap-1 ) then
            indod = indlsod(l+1,l)
            do n = l+1, jcap, 2
                ndexod(indod) = n
               snnp1od(indod) = n*(n+1)
                 indod        = indod+1
            end do
         end if
      end do
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         jbasod = ls_node(locl,3)
         if (mod(l,2).eq.mod(jcap+1,2)) then
!          set the even (n-l) terms of the top row to zero
             ndexev(indlsev(jcap+1,l)) = 0
            snnp1ev(indlsev(jcap+1,l)) = cons0     !constant
         else
!          set the  odd (n-l) terms of the top row to zero
             ndexod(indlsod(jcap+1,l)) = 0
            snnp1od(indlsod(jcap+1,l)) = cons0     !constant
         endif
      enddo
!
      do j=1,latg2
        sinlat_a(j) = cos(colrad_a(j))
      enddo
!
      do j=1,latr
        if (j.le.latr2) then
          sinlat_r(j) = cos(colrad_r(j))
        else
          sinlat_r(j) = -cos(colrad_r(j))
        endif
        coslat_r(j) = sqrt(1. e 0 -sinlat_r(j)*sinlat_r(j))
      enddo
!
      do l=0,jcap
         do lat = 1, latg2
            if ( l .le. min(jcap,lonsperlat(lat)/2) ) then
               lat1s_a(l) = lat
               go to 200
            endif
         end do
  200    continue
      end do
!
      do l=0,jcap
         do lat = 1, latr2
            if ( l .le. min(jcap,lonsperlar(lat)/2) ) then
               lat1s_r(l) = lat
               go to 220
            endif
         end do
  220    continue
      end do
!
!
!     do j=1,lats_node_a
!        lat = global_lats_a(ipt_lats_node_a-1+j)
!        if ( lonsperlat(lat) .eq. lonf ) then
!           lon_dims_a(j) = lonfx
!        else
!           lon_dims_a(j) = lonsperlat(lat) + 2
!        endif
!     enddo
!
      lon_dim_a = lonf + 2
!
      do j=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+j)
         if ( lonsperlar(lat) .eq. lonr ) then
            lon_dims_r(j) = lonrx
         else
            lon_dims_r(j) = lonsperlar(lat) + 2
         endif
!???????????????????????????????
!       print*,'in getcon,j,lat,ipt,lonsper,lonf,lonfx,lon_dims=',
!    &     j,lat,ipt_lats_node_r,lonsperlar(lat),lonr,lonrx,
!    &     lon_dims_r(j)

      enddo
!
      lon_dim_r = lonrx
!
!     if (.not. shuffled) then
!     do j=1,lats_node_ext
!        lat = global_lats_ext(ipt_lats_node_ext-1+j)
!selaxxx print*,'ipt_lats_node_ext = ',ipt_lats_node_ext
!selaxxx print*,' j,lat = ',j,lat
!        if ( lonsperlat(lat) .eq. lonf ) then
!           lon_dims_ext(j) = lonfx
!        else
!           lon_dims_ext(j) = lonsperlat(lat) + 1+2*nxpt+1
!        endif
!     enddo
!     endif
!
! hmhj
      if( ndsl ) then
        call ndslfv_init(lonf,latg,
     &                   coslat_r,colrad_a,wgt_a,lats_nodes_a,
     &                   global_lats_a,lonsperlat)
      endif
!
      return
      end
