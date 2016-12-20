      subroutine four2fln_gg(workdim,nvarsdim,nvars,four_gr,
     &                       ls_nodes,max_ls_nodes,
     &                       lats_nodes,global_lats,lon_dims,
     &                       lats_node,ipt_lats_node,
     &                       lat1s,londi,latl,latl2,
     &                       flnev,flnod,
     &                       plnev,plnod,ls_node,nvars_0,
     &                       nvar_zero_top_1,nvar_zero_top_2)
!
!
      use machine   , only : kind_evod
      use resol_def , only : jcap,latgd
      use layout1   , only : len_trie_ls,len_trio_ls,
     &                       ls_dim,ls_max_node,me,nodes
      use mpi_def   , only : kind_mpi,mc_comp,mpi_r_mpi
      implicit none
!
      integer              nvarsdim,latl2,latl
      integer              nvars,nvars_0
      integer              nvar_zero_top_1
      integer              nvar_zero_top_2
      integer              workdim
      integer              londi
      integer              lat1s(0:jcap)
      integer              lats_node,ipt_lats_node
      integer              lon_dims(latgd)
!
      real(kind=kind_evod) four_gr(londi,nvarsdim,workdim)
!
      integer              ls_nodes(ls_dim,nodes)
      integer                 max_ls_nodes(nodes)
      integer                   lats_nodes(nodes)
      integer              global_lats(latl)
!
!$$$      real(kind=kind_mpi) works(2,nvars,ls_dim*workdim,nodes)
!$$$      real(kind=kind_mpi) workr(2,nvars,ls_dim*workdim,nodes)
      real(kind=kind_mpi) ,allocatable ::works(:,:,:,:),workr(:,:,:,:)
      
!
      integer                    kpts(1+jcap)
      integer                    kptr(1+jcap)
      integer              sendcounts(1+jcap)
      integer              recvcounts(1+jcap)
      integer                 sdispls(1+jcap)
!
      integer              ierr,ilat,ipt_ls
      integer              lval,node,nvar,ifin
!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      real(kind=kind_evod) fp(latl2,2,nvars)
      real(kind=kind_evod) fm(latl2,2,nvars)
!
      real(kind=kind_evod) flnev(len_trie_ls,2,nvars)
      real(kind=kind_evod) flnod(len_trio_ls,2,nvars)
!
      real(kind=kind_evod) plnev(len_trie_ls,latl2)
      real(kind=kind_evod) plnod(len_trio_ls,latl2)
!
      integer              ls_node(ls_dim,3)
!
!    local scalars
!    -------------
!
      integer              j,k,l,n
      integer              lat,lat1
      integer              indev1,indev2
      integer              indod1,indod2
      integer              num_threads
      integer              nvar_thread_max
      integer              nvar_1,nvar_2
      integer              thread
      integer              ipt_wr(4,latl2,ls_max_node)
!
!     statement functions
!     -------------------
!
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons1     !constant

      integer num_parthds,ip1,ip2,ip3,ip4

      allocate (works(2,nvars,ls_dim*workdim,nodes))
      allocate (workr(2,nvars,ls_dim*workdim,nodes))

!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
!
!!
      ifin=lats_node
!!
      kpts   = 0
!$omp parallel do private(node,l,lval,j,lat,nvar)
      do node=1,nodes
        do l=1,max_ls_nodes(node)
          lval = ls_nodes(l,node) + 1
          do j=1,ifin
            lat = global_lats(ipt_lats_node-1+j)
            if ( min(lat,latl-lat+1) >= lat1s(lval-1) ) then
              kpts(node) = kpts(node) + 1
              do nvar=1,nvars
!
                 works(1,nvar,kpts(node),node) =
     &           four_gr(2*lval-1,nvars_0+nvar,j)
!
                 works(2,nvar,kpts(node),node) =
     &           four_gr(2*lval,nvars_0+nvar,j)
!
              enddo
            endif
          enddo
        enddo
      enddo
!
!
      kptr   = 0
      do l=1,ls_max_node
        ilat   = 1
        do node=1,nodes
          ifin=lats_nodes(node)
!jfe      do j=1,lats_nodes_ext(node)
          do j=1,ifin
            lat    = global_lats(ilat)
            ipt_ls = min(lat,latl-lat+1)
            if( ipt_ls >= lat1s(ls_nodes(l,me+1)) ) then
              kptr(node) = kptr(node) + 1
              if ( lat <= latl2 ) then
                ipt_wr(1,ipt_ls,l) = kptr(node)
                ipt_wr(2,ipt_ls,l) =      node
              else
                ipt_wr(3,ipt_ls,l) = kptr(node)
                ipt_wr(4,ipt_ls,l) =      node
              endif
            endif
             ilat = ilat + 1
          enddo
        enddo
      enddo
!
!
      do node=1,nodes
         sendcounts(node) = kpts(node) * 2 * nvars
         recvcounts(node) = kptr(node) * 2 * nvars
            sdispls(node) = (node-1) * 2*ls_dim*workdim*nvars
      end do
!
      call mpi_barrier (mc_comp,ierr)
!
      call mpi_alltoallv(works,sendcounts,sdispls,mpi_r_mpi,
     x                   workr,recvcounts,sdispls,mpi_r_mpi,
     x                   mc_comp,ierr)
!
      num_threads     = min(num_parthds(),nvars)
      nvar_thread_max = (nvars+num_threads-1)/num_threads
!
!    -------------------------------------------------
!    compute the coefficients of the expansion
!    in spherical harmonics of the field at each level
!    -------------------------------------------------
!
!
      do j = 1, ls_max_node   ! start of j loop ########################
!
              l=ls_node(j,1)
         jbasev=ls_node(j,2)
         jbasod=ls_node(j,3)
!
         lat1 = lat1s(l)
!
         indev1 = indlsev(l,l)
!
         indod1 = indlsod(l+1,l)
         if (mod(l,2) == mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
            indod2 = indlsod(jcap  ,l)
         else
            indev2 = indlsev(jcap  ,l)
            indod2 = indlsod(jcap+1,l)
         endif
!$omp parallel do shared(fm,fp,workr,ipt_wr)
!$omp+shared(plnev,plnod,flnev,flnod)
!$omp+shared(indev1,indev2,indod1,indod2,jbasev,jbasod)
!$omp+shared(j,l,lat1,nvar_thread_max)
!$omp+private(thread,k,lat,nvar_1,nvar_2,ip1,ip2,ip3,ip4)
!
         do thread=1,num_threads   ! start of thread loop ..............
           nvar_1 = (thread-1)*nvar_thread_max+1
           nvar_2 = min(nvar_1+nvar_thread_max-1,nvars)
!
           do k = nvar_1,nvar_2
             do lat = lat1, latl2
               ip1 = ipt_wr(1,lat,j)
               ip2 = ipt_wr(2,lat,j)
               ip3 = ipt_wr(3,lat,j)
               ip4 = ipt_wr(4,lat,j)
               fp(lat,1,k) = workr(1,k,ip1,ip2) + workr(1,k,ip3,ip4)
               fp(lat,2,k) = workr(2,k,ip1,ip2) + workr(2,k,ip3,ip4)
               fm(lat,1,k) = workr(1,k,ip1,ip2) - workr(1,k,ip3,ip4)
               fm(lat,2,k) = workr(2,k,ip1,ip2) - workr(2,k,ip3,ip4)
               enddo
            enddo
!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
            if ( kind_evod == 8 ) then !------------------------------
!
!          compute even real      expansion coefficients
!          compute even imaginary expansion coefficients
!
            call dgemm ('n', 'n', indev2-indev1+1, 2*(nvar_2-nvar_1+1),
     &                  latl2-lat1+1, cons1,                 !constant
     &                  plnev(indev1,lat1), len_trie_ls,
     &                  fp(lat1,1,nvar_1), latl2, cons0,     !constant
     &                  flnev(indev1,1,nvar_1), len_trie_ls)
!
!          compute odd real      expansion coefficients
!          compute odd imaginary expansion coefficients
!
            call dgemm ('n', 'n', indod2-indod1+1, 2*(nvar_2-nvar_1+1),
     &                  latl2-lat1+1, cons1,                 !constant
     &                  plnod(indod1,lat1), len_trio_ls,
     &                  fm(lat1,1,nvar_1), latl2, cons0,     !constant
     &                  flnod(indod1,1,nvar_1), len_trio_ls)
            else !------------------------------------------------------
!
!          compute even real      expansion coefficients
!          compute even imaginary expansion coefficients
!
            call sgemm ('n', 'n', indev2-indev1+1, 2*(nvar_2-nvar_1+1),
     &                  latl2-lat1+1, cons1,                 !constant
     &                  plnev(indev1,lat1), len_trie_ls,
     &                  fp(lat1,1,nvar_1), latl2, cons0,     !constant
     &                  flnev(indev1,1,nvar_1), len_trie_ls)
!
!          compute odd real      expansion coefficients
!          compute odd imaginary expansion coefficients
!
            call sgemm ('n', 'n', indod2-indod1+1, 2*(nvar_2-nvar_1+1),
     &                  latl2-lat1+1, cons1,                 !constant
     &                  plnod(indod1,lat1), len_trio_ls,
     &                  fm(lat1,1,nvar_1), latl2, cons0,     !constant
     &                  flnod(indod1,1,nvar_1), len_trio_ls)
            endif !-----------------------------------------------------
!
            if (mod(l,2) == mod(jcap+1,2)) then
!             set the even (n-l) terms of the top row to zero
               do k = max(nvar_1,nvar_zero_top_1),
     &                min(nvar_2,nvar_zero_top_2) 
                  flnev(indev2,1,k) = cons0     !constant
                  flnev(indev2,2,k) = cons0     !constant
               end do
            else
!             set the  odd (n-l) terms of the top row to zero
               do k = max(nvar_1,nvar_zero_top_1),
     &                min(nvar_2,nvar_zero_top_2) 
                  flnod(indod2,1,k) = cons0     !constant
                  flnod(indod2,2,k) = cons0     !constant
               end do
            endif
!
         end do   ! end of thread loop .................................
!
      end do   ! end of do j loop ######################################
!
      deallocate(workr,works)
      return
      end
