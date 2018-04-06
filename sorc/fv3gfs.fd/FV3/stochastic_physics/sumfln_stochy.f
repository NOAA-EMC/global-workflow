      subroutine sumfln_stochy(flnev,flnod,lat1s,plnev,plnod,
     &                         nvars,ls_node,latl2,
     &                         workdim,nvarsdim,four_gr,
     &                         ls_nodes,max_ls_nodes,
     &                         lats_nodes,global_lats,
     &                         lats_node,ipt_lats_node,
     &                         lons_lat,londi,latl,nvars_0)
!
      use stochy_resol_def , only : jcap,latgd
      use spectral_layout   , only : len_trie_ls,len_trio_ls,
     &                               ls_dim,ls_max_node,me,nodes
      use machine
      use fv_mp_mod
      use mpp_mod
      use fv_arrays_mod,      only: fv_atmos_type

      implicit none
!
      include 'mpif.h'
      integer lat1s(0:jcap),latl2
!
      integer              nvars,nvars_0
      integer,                allocatable :: pelist(:)
      integer :: npes
      real(kind=kind_dbl_prec) flnev(len_trie_ls,2*nvars)
      real(kind=kind_dbl_prec) flnod(len_trio_ls,2*nvars)
!
      real(kind=kind_dbl_prec) plnev(len_trie_ls,latl2)
      real(kind=kind_dbl_prec) plnod(len_trio_ls,latl2)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
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
      real(kind=kind_dbl_prec), dimension(nvars*2,latl2) ::  apev, apod
      integer               num_threads, nvar_thread_max, nvar_1, nvar_2
     &,                     thread
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      integer              nvarsdim,  latl, workdim, londi
     &,                    lats_node, ipt_lats_node
!
      real(kind=kind_dbl_prec) four_gr(londi,nvarsdim,workdim)
!
      integer              ls_nodes(ls_dim,nodes)
      integer, dimension(nodes) :: max_ls_nodes, lats_nodes
      integer, dimension(latl)  :: global_lats, lons_lat

!jfe  integer              global_lats(latg+2*jintmx+2*nypt*(nodes-1))
!
      real(kind=4),target,dimension(2,nvars,ls_dim*workdim,nodes)::
     &                               workr,works
!      real(kind=4),dimension(2*nvars*ls_dim*workdim*nodes)::
!     &                               work1dr,work1ds
      real(kind=4),pointer:: work1dr(:),work1ds(:)
      integer, dimension(jcap+1) :: kpts, kptr, sendcounts, recvcounts,
     &                              sdispls
!
      integer              ierr,ilat,ipt_ls, lmax,lval,i,jj,lonl,nv
      integer              node,nvar,arrsz
      integer              ilat_list(nodes)              !    for OMP buffer copy
!
!    statement functions
!    -------------------
!
      integer              indlsev, jbasev, indlsod, jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
      real(kind=kind_dbl_prec), parameter ::  cons0=0.0d0, cons1=1.0d0
      integer num_parthds_stochy
!
! xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!
      arrsz=2*nvars*ls_dim*workdim*nodes
      num_threads     = min(NUM_PARTHDS_STOCHY(),nvars)
      nvar_thread_max = (nvars+num_threads-1)/num_threads
      npes = mpp_npes()
      allocate(pelist(0:npes-1))
      call mpp_get_current_pelist(pelist)
      kpts   = 0
!     write(0,*)' londi=',londi,'nvarsdim=',nvarsdim,'workdim=',workdim
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
        if ( kind_dbl_prec == 8 ) then !------------------------------------

!$omp parallel do private(thread,nvar_1,nvar_2,n2)
          do thread=1,num_threads   ! start of thread loop ..............
            nvar_1 = (thread-1)*nvar_thread_max + 1
            nvar_2 = min(nvar_1+nvar_thread_max-1,nvars)

            if (nvar_2 >= nvar_1) then
              n2 = 2*(nvar_2-nvar_1+1)

!           compute the even and odd components of the fourier coefficients
!
!           compute the sum of the even real      terms for each level
!           compute the sum of the even imaginary terms for each level
!
!           call dgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
!     &                 (jcap+2-l)/2,cons1,     !constant
!     &                 plnev(indev,lat1), len_trio_ls,
!     &                 flnev(indev,2*nvar_1-1),len_trio_ls,cons0,
!     &                 apev(2*nvar_1-1,lat1),latl2)
             call esmf_dgemm(
     &                   't',
     &                   'n',
     &                    n2,
     &                   latl2-lat1+1,
     &                   (jcap+3-l)/2,
     &                   cons1,
     &                   flnev(indev,2*nvar_1-1),
     &                   len_trie_ls,
     &                   plnev(indev,lat1),
     &                   len_trie_ls,
     &                   cons0,
     &                   apev(2*nvar_1-1,lat1),
     &                   2*nvars
     &                   )
!
!           compute the sum of the odd real      terms for each level
!           compute the sum of the odd imaginary terms for each level
!
!           call dgemm('t','n',latl2-lat1+1, 2*(nvar_2-nvar_1+1),
!     &                 (jcap+2-l)/2,cons1,     !constant
!     &                 plnod(indod,lat1), len_trio_ls,
!     &                 flnod(indod,2*nvar_1-1),len_trio_ls,cons0,
!     &                 apod(2*nvar_1-1,lat1), latl2)
              call esmf_dgemm(
     &                   't',
     &                   'n',
     &                   n2,
     &                   latl2-lat1+1,
     &                  (jcap+2-l)/2,
     &                   cons1,
     &                   flnod(indod,2*nvar_1-1),
     &                   len_trio_ls,
     &                   plnod(indod,lat1),
     &                   len_trio_ls,
     &                   cons0,
     &                   apod(2*nvar_1-1,lat1),
     &                   2*nvars
     &                   )
!
            endif
          enddo   ! end of thread loop ..................................
        else !------------------------------------------------------------
!$omp parallel do private(thread,nvar_1,nvar_2)
          do thread=1,num_threads   ! start of thread loop ..............
            nvar_1 = (thread-1)*nvar_thread_max + 1
            nvar_2 = min(nvar_1+nvar_thread_max-1,nvars)
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
            ilat  = ilat_list(node) + jj
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
     &                                  + apod(n2-1,ipt_ls)
                  works(2,nvar,kn,node) = apev(n2,  ipt_ls)
     &                                  + apod(n2,  ipt_ls)
                enddo
              else
!                                                southern hemisphere
                do nvar=1,nvars
                  n2 = nvar + nvar
                  works(1,nvar,kn,node) = apev(n2-1,ipt_ls)
     &                                  - apod(n2-1,ipt_ls)
                  works(2,nvar,kn,node) = apev(n2,  ipt_ls)
     &                                  - apod(n2,  ipt_ls)
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
         sdispls(node)    = (node-1)   * n2 * ls_dim * workdim
      end do
      work1dr(1:arrsz)=>workr
      work1ds(1:arrsz)=>works
      call MPP_ALLTOALL(work1ds, sendcounts, sdispls,
     &                  work1dr,recvcounts,sdispls,pelist)
      nullify(work1dr)
      nullify(work1ds)
!$omp parallel do private(j,lat,lmax,nvar,lval,n2,lonl,nv)
      do j=1,lats_node
         lat  = global_lats(ipt_lats_node-1+j)
         lonl = lons_lat(lat)
         lmax = min(jcap,lonl/2)
         n2   = lmax + lmax + 3
!     write(0,*)' j=',j,' lat=',lat,' lmax=',lmax,' n2=',n2
!    &,' nvars=',nvars,' lonl=',lonl
         if ( n2 <= lonl+2 ) then
           do nvar=1,nvars
             nv = nvars_0 + nvar
             do lval = n2, lonl+2
!        write(0,*)' lval=',lval,' nvar=',nvar,nvars_0
!    &,' n2=',n2,' lonl=',lonl,' nv=',nv,' j=',j
!    &,'size=',size(four_gr,1),size(four_gr,2),size(four_gr,3)
               four_gr(lval,nv,j) = cons0
             enddo
           enddo
         endif
      enddo
!
      kptr = 0
!     write(0,*)' kptr=',kptr(1)
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
