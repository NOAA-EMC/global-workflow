      subroutine sumfln_slg_gg(flnev,flnod,lat1s,plnev,plnod,
     &                         nvars,ls_node,latl2,
     &                         workdim,nvarsdim,four_gr,
     &                         ls_nodes,max_ls_nodes,
     &                         lats_nodes,global_lats,
     &                         lats_node,ipt_lats_node,lon_dims,
     &                         lons_lat,londi,latl,nvars_0)
!
      use machine   , only : kind_evod
      use resol_def , only : jcap,latgd
      use layout1   , only : len_trie_ls,len_trio_ls,
     &                       ls_dim,ls_max_node,me,nodes
      use mpi_def   , only : kind_mpi,mc_comp,mpi_r_mpi
      implicit none
!
      integer lat1s(0:jcap),latl2
!
      integer              nvars,nvars_0
      real(kind=kind_evod) flnev(len_trie_ls,2*nvars)
      real(kind=kind_evod) flnod(len_trio_ls,2*nvars)
!
      real(kind=kind_evod) plnev(len_trie_ls,latl2)
      real(kind=kind_evod) plnod(len_trio_ls,latl2)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
!    local scalars
!    -------------
!
      integer              j, k, l, lat, lat1, n, kn, n2,indev,indod
!
!    local arrays
!    ------------
!
      real(kind=kind_evod) apev(nvars*2,latl2)
      real(kind=kind_evod) apod(nvars*2,latl2)
      integer              num_threads
      integer              nvar_thread_max
      integer              nvar_1,nvar_2
      integer              thread
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      integer              nvarsdim,latl
      integer              workdim
      integer              londi
      integer lats_node,ipt_lats_node
      integer lon_dims(latgd)
!
!
      real(kind=kind_evod) four_gr(londi,nvarsdim,workdim)
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer                lats_nodes(nodes)
!jfe  integer        global_lats(latg+2*jintmx+2*nypt*(nodes-1))
      integer        global_lats(latl)
      integer               lons_lat(latl)
!
      real(kind=kind_mpi) works(2,nvars,ls_dim*workdim,nodes)
      real(kind=kind_mpi) workr(2,nvars,ls_dim*workdim,nodes)
!
      integer                    kpts(1+jcap)
      integer                    kptr(1+jcap)
      integer              sendcounts(1+jcap)
      integer              recvcounts(1+jcap)
      integer                 sdispls(1+jcap)
!
      integer              ierr,ilat,ipt_ls
      integer              lmax,lval,i,jj
      integer              node,nvar
 
!    for omp buffer copy
      integer ilat_list(nodes)
!
!    statement functions
!    -------------------
!
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
      real(kind=kind_evod), parameter ::  cons0=0.0d0, cons1=1.0d0
      integer num_parthds
!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      num_threads     = min(num_parthds(),nvars)
      nvar_thread_max = (nvars+num_threads-1)/num_threads
      kpts   = 0
!
      do j = 1, ls_max_node   ! start of do j loop #####################
!
              l = ls_node(j,1)
         jbasev = ls_node(j,2)
         jbasod = ls_node(j,3)

         indev  = indlsev(l,l)
         indod  = indlsod(l+1,l)
!
         lat1 = lat1s(l)
      if ( kind_evod == 8 ) then !------------------------------------

!$omp parallel do private(thread,nvar_1,nvar_2,n2)
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1 = (thread-1)*nvar_thread_max+1
            nvar_2 = min(nvar_1+nvar_thread_max-1,nvars)
            n2     = 2*(nvar_2-nvar_1+1)

!           compute the even and odd components of the fourier coefficients
!
!           compute the sum of the even real      terms for each level
!           compute the sum of the even imaginary terms for each level
!
!           call dgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
!    &                 (jcap+3-l)/2,cons1,     !constant
!    &                 plnev(indev,lat1),len_trie_ls,
!    &                 flnev(indev,1,nvar_1),len_trie_ls,cons0,
!    &                 apev(lat1,1,nvar_1), latl2)
            call dgemm(
     &                 't',
     &                 'n',
     &                 n2,
     &                 latl2-lat1+1,
     &                 (jcap+3-l)/2,
     &                 cons1,
     &                 flnev(indev,2*nvar_1-1),
     &                 len_trie_ls,
     &                 plnev(indev,lat1),
     &                 len_trie_ls,
     &                 cons0,
     &                 apev(2*nvar_1-1,lat1),
     &                 2*nvars
     &                 )
!
!           compute the sum of the odd real      terms for each level
!           compute the sum of the odd imaginary terms for each level
!
!           call dgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
!    &                 (jcap+2-l)/2,cons1,     !constant
!    &                 plnod(indod,lat1), len_trio_ls,
!    &                 flnod(indod,1,nvar_1),len_trio_ls,cons0,
!    &                 apod(lat1,1,nvar_1), latl2)
            call dgemm(
     &                 't',
     &                 'n',
     &                 n2,
     &                 latl2-lat1+1,
     &                 (jcap+2-l)/2,
     &                 cons1,
     &                 flnod(indod,2*nvar_1-1),
     &                 len_trio_ls,
     &                 plnod(indod,lat1),
     &                 len_trio_ls,
     &                 cons0,
     &                 apod(2*nvar_1-1,lat1),
     &                 2*nvars
     &                 )
!
         enddo   ! end of thread loop ..................................
      else !------------------------------------------------------------
!$omp parallel do private(thread,nvar_1,nvar_2)
         do thread=1,num_threads   ! start of thread loop ..............
            nvar_1=(thread-1)*nvar_thread_max+1
            nvar_2=min(nvar_1+nvar_thread_max-1,nvars)
         enddo   ! end of thread loop ..................................
      endif !-----------------------------------------------------------
!
ccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
!       compute the fourier coefficients for each level
!       -----------------------------------------------
!
         ilat_list(1) = 0
         do node = 1, nodes - 1
           ilat_list(node+1) = ilat_list(node) + lats_nodes(node)
         end do
 
!$omp parallel do private(node,jj,ilat,lat,ipt_ls,nvar,kn,n2)
         do node=1,nodes
           do jj=1,lats_nodes(node)
             ilat   = ilat_list(node) + jj
             lat    = global_lats(ilat)
             ipt_ls = min(lat,latl-lat+1)
             if ( ipt_ls >= lat1s(ls_nodes(j,me+1)) ) then
               kpts(node) = kpts(node) + 1
               kn = kpts(node)
!
               if ( lat <= latl2 ) then
!                                                northern hemisphere
                 do nvar=1,nvars
                   n2 = nvar + nvar
                   works(1,nvar,kn,node) = apev(n2-1,ipt_ls)
     &                                   + apod(n2-1,ipt_ls)
                   works(2,nvar,kn,node) = apev(n2,  ipt_ls)
     &                                   + apod(n2,  ipt_ls)
                 enddo
               else
!                                                southern hemisphere
                 do nvar=1,nvars
                   n2 = nvar + nvar
                   works(1,nvar,kn,node) = apev(n2-1,ipt_ls)
     &                                   - apod(n2-1,ipt_ls)
                   works(2,nvar,kn,node) = apev(n2,  ipt_ls)
     &                                   - apod(n2,  ipt_ls)
                 enddo
                endif
             endif
           enddo
         enddo
!
      enddo   ! end of do j loop #######################################
!
      kptr = 0
      do node=1,nodes
         do l=1,max_ls_nodes(node)
            lval = ls_nodes(l,node)+1
            do j=1,lats_node
               lat = global_lats(ipt_lats_node-1+j)
               if ( min(lat,latl-lat+1) >= lat1s(lval-1) ) then
                  kptr(node) = kptr(node) + 1
               endif
            enddo
         enddo
      enddo
!
!
      n2 = nvars + nvars
!$omp parallel do private(node)
      do node=1,nodes
         sendcounts(node) = kpts(node) * n2
         recvcounts(node) = kptr(node) * n2
            sdispls(node) = (node-1)   * n2 * ls_dim * workdim
      end do
!
      call mpi_alltoallv(works,sendcounts,sdispls,mpi_r_mpi,
     &                   workr,recvcounts,sdispls,mpi_r_mpi,
     &                   mc_comp,ierr)
!
!$omp parallel do private(j,lat,lmax,nvar,lval,n2)
      do j=1,lats_node
         lat  = global_lats(ipt_lats_node-1+j)
         lmax = min(jcap,lons_lat(lat)/2)
         n2   = lmax + lmax + 3
         if ( n2 <= lons_lat(lat)+2 ) then
           do nvar=1,nvars
             do lval = n2, lons_lat(lat)+2
               four_gr(lval,nvars_0+nvar,j) = cons0
             enddo
           enddo
         endif
      enddo
!
      kptr = 0
!!
!$omp parallel do private(node,l,lval,j,lat,nvar,kn,n2)
      do node=1,nodes
        do l=1,max_ls_nodes(node)
          lval = ls_nodes(l,node)+1
          n2   = lval + lval
          do j=1,lats_node
            lat = global_lats(ipt_lats_node-1+j)
            if ( min(lat,latl-lat+1) >= lat1s(lval-1) ) then
              kptr(node) = kptr(node) + 1
              kn = kptr(node)

              do nvar=1,nvars
                four_gr(n2-1,nvars_0+nvar,j) = workr(1,nvar,kn,node)
                four_gr(n2,  nvars_0+nvar,j) = workr(2,nvar,kn,node)
              enddo
            endif
          enddo
        enddo
      enddo
!
      return
      end
