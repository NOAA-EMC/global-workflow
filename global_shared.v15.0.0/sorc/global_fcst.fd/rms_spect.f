      subroutine rms_spect(qe_ls,xe_ls,ye_ls,we_ls,re_ls,
     x                     qo_ls,xo_ls,yo_ls,wo_ls,ro_ls,
     x                     ls_nodes,max_ls_nodes)
cc
cc    september 24, 2004
cc    version of rms_spect with ntrac generalized from 3.
cc
cc    may 2, 2003
cc    version of rms_spect with ntrac re_ls ro_ls.
cc
cc    april 10, 2003
cc    version of rms_spect with gather for each level.
cc

cc
      use resol_def
      use layout1
      use namelist_def					! hmhj
      use mpi_def
      implicit none
cc
      real(kind=kind_evod) qe_ls(len_trie_ls,2)
      real(kind=kind_evod) xe_ls(len_trie_ls,2,levs)
      real(kind=kind_evod) ye_ls(len_trie_ls,2,levs)
      real(kind=kind_evod) we_ls(len_trie_ls,2,levs)
      real(kind=kind_evod) re_ls(len_trie_ls,2,levs,ntrac)
cc
      real(kind=kind_evod) qo_ls(len_trio_ls,2)
      real(kind=kind_evod) xo_ls(len_trio_ls,2,levs)
      real(kind=kind_evod) yo_ls(len_trio_ls,2,levs)
      real(kind=kind_evod) wo_ls(len_trio_ls,2,levs)
      real(kind=kind_evod) ro_ls(len_trio_ls,2,levs,ntrac)
cc
!mjr  real(kind=kind_evod) del(levs)
cc
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
cc
      integer              ierr,j,k,l,lenrec,level,locl,n,node
cc
      integer              indev
      integer              indod
cc
      real(kind=kind_evod) fgbar(3+ntrac+1)
cc
      real(kind=kind_evod) fgbar_sav(3,levs)
cc
      real(kind=kind_evod) fgbar_ntrac(ntrac,levs)
cc
!mjr  real(kind=kind_evod) vx
!mjr  real(kind=kind_evod) vy
!mjr  real(kind=kind_evod) vw
!mjr  real(kind=kind_evod) vr
cc
      integer              kmq
      integer              kmx
      integer              kmy
      integer              kmw
      integer              kmr
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
cc
      real(kind=kind_mpi),allocatable :: trieo_ls_node (:,:,:)
      real(kind=kind_mpi),allocatable :: trieo_ls_nodes(:,:,:,:)
cc
      integer       node_l_all(0:jcap)
      integer     jbasev_l_all(0:jcap)
      integer     jbasod_l_all(0:jcap)
cc
      real(kind=kind_evod) cons0,cons0p5     !constant
cc
      cons0   = 0.d0      !constant
      cons0p5 = 0.5d0     !constant
cc
      kmq = 1      !  qe/o_ls
cc
      kmx = 1+kmq  !  xe/o_ls
      kmy = 2+kmq  !  ye/o_ls
      kmw = 3+kmq  !  we/o_ls
      kmr = 4+kmq  !  re/o_ls
cc
         allocate ( trieo_ls_node  ( len_trie_ls_max+len_trio_ls_max,
     x                               2, 3+ntrac+kmq ) )
cc
      if ( me .eq. 0 ) then
         allocate ( trieo_ls_nodes ( len_trie_ls_max+len_trio_ls_max,
     x                               2, 3+ntrac+kmq, nodes ) )
      else
         allocate ( trieo_ls_nodes ( 2, 2, 2, 2 ) )
      endif
cc
      do j=1,len_trie_ls
cc
         trieo_ls_node(j,1,kmq) = qe_ls(j,1)
         trieo_ls_node(j,2,kmq) = qe_ls(j,2)
cc
      enddo
cc
      do j=1,len_trio_ls
cc
         trieo_ls_node(j+len_trie_ls_max,1,kmq) = qo_ls(j,1)
         trieo_ls_node(j+len_trie_ls_max,2,kmq) = qo_ls(j,2)
cc
      enddo
cc
      do 2000 level=1,levs  ! xxxxxxxxxxxxxxxxxxxx begin big level loop
cc
      if ( level .eq. 2 ) then
cc
         kmq = 0      !  qe/o_ls
cc
         kmx = 1+kmq  !  xe/o_ls
         kmy = 2+kmq  !  ye/o_ls
         kmw = 3+kmq  !  we/o_ls
         kmr = 4+kmq  !  re/o_ls
cc
         if ( me .eq. 0 ) then
         deallocate ( trieo_ls_nodes )
           allocate ( trieo_ls_nodes ( len_trie_ls_max+len_trio_ls_max,
     x                                 2, 3+ntrac+kmq, nodes ) )
         endif
cc
      endif
cc
      do j=1,len_trie_ls
cc
         trieo_ls_node(j,1,kmx) = xe_ls(j,1,level)
         trieo_ls_node(j,2,kmx) = xe_ls(j,2,level)
cc
         trieo_ls_node(j,1,kmy) = ye_ls(j,1,level)
         trieo_ls_node(j,2,kmy) = ye_ls(j,2,level)
cc
         trieo_ls_node(j,1,kmw) = we_ls(j,1,level)
         trieo_ls_node(j,2,kmw) = we_ls(j,2,level)
cc
         do k=1,ntrac
         trieo_ls_node(j,1,kmr+k-1) = re_ls(j,1,level,k)
         trieo_ls_node(j,2,kmr+k-1) = re_ls(j,2,level,k)
         enddo
cc
      enddo
cc
      do j=1,len_trio_ls
cc
         trieo_ls_node(j+len_trie_ls_max,1,kmx) = xo_ls(j,1,level)
         trieo_ls_node(j+len_trie_ls_max,2,kmx) = xo_ls(j,2,level)
cc
         trieo_ls_node(j+len_trie_ls_max,1,kmy) = yo_ls(j,1,level)
         trieo_ls_node(j+len_trie_ls_max,2,kmy) = yo_ls(j,2,level)
cc
         trieo_ls_node(j+len_trie_ls_max,1,kmw) = wo_ls(j,1,level)
         trieo_ls_node(j+len_trie_ls_max,2,kmw) = wo_ls(j,2,level)
cc
         do k=1,ntrac
         trieo_ls_node(j+len_trie_ls_max,1,kmr+k-1) = ro_ls(j,1,level,k)
         trieo_ls_node(j+len_trie_ls_max,2,kmr+k-1) = ro_ls(j,2,level,k)
         enddo
cc
      enddo
cc
      lenrec = (len_trie_ls_max+len_trio_ls_max) * 2 * (3+ntrac+kmq)
cc
ccmr  call mpi_barrier(mpi_comm_world,ierr)
cc
      call mpi_gather( trieo_ls_node , lenrec, mpi_r_mpi,
     x                 trieo_ls_nodes, lenrec, mpi_r_mpi,
     x                 0, mc_comp, ierr)
cc
ccmr  call mpi_barrier(mpi_comm_world,ierr)
cc
      if ( me .ne. 0 ) go to 2000
cc
      do node=1,nodes
cc
         jbasev=0
         jbasod=len_trie_ls_max
ccmr     do l = 0, jcap
         do locl=1,max_ls_nodes(node)
            l=ls_nodes(locl,node)
cc
              node_l_all(l) = node
            jbasev_l_all(l) = jbasev
            jbasod_l_all(l) = jbasod
cc
            jbasev=jbasev+(jcap+3-l)/2
            jbasod=jbasod+(jcap+2-l)/2
         end do
cc
      end do
cc
      do k=1,3+ntrac+kmq
cc
ccmr     fgbar(k) = 0.        !constant
         fgbar(k) = cons0     !constant
cc
         l=0
         node   =   node_l_all(l)
         jbasev = jbasev_l_all(l)
         jbasod = jbasod_l_all(l)
cc
         indev=indlsev(0,l)
         indod=indlsod(1,l)
         do n=0, jcap
            if(mod(n+l,2).eq.0) then
               fgbar(k) =
     x         fgbar(k) + trieo_ls_nodes(indev,1,k,node)
     x                  * trieo_ls_nodes(indev,1,k,node)
               indev=indev+1
            else
               fgbar(k) =
     x         fgbar(k) + trieo_ls_nodes(indod,1,k,node)
     x                  * trieo_ls_nodes(indod,1,k,node)
               indod=indod+1
            endif
         end do
cc
         indev=indlsev(0,l)
         indod=indlsod(1,l)
         do n=0, jcap
            if(mod(n+l,2).eq.0) then
               fgbar(k) =
     x         fgbar(k) + trieo_ls_nodes(indev,2,k,node)
     x                  * trieo_ls_nodes(indev,2,k,node)
               indev=indev+1
            else
               fgbar(k) =
     x         fgbar(k) + trieo_ls_nodes(indod,2,k,node)
     x                  * trieo_ls_nodes(indod,2,k,node)
               indod=indod+1
            endif
         end do
cc
ccmr     fgbar(k)=fgbar(k)*0.5         !constant
         fgbar(k)=fgbar(k)*cons0p5     !constant
cc
         do l=1, jcap
            node   =   node_l_all(l)
            jbasev = jbasev_l_all(l)
            jbasod = jbasod_l_all(l)
cc
            indev=indlsev(l  ,l)
            indod=indlsod(l+1,l)
            do n=l, jcap
               if(mod(n+l,2).eq.0) then
                  fgbar(k) =
     x            fgbar(k) + trieo_ls_nodes(indev,1,k,node)
     x                     * trieo_ls_nodes(indev,1,k,node)
                  indev=indev+1
               else
                  fgbar(k) =
     x            fgbar(k) + trieo_ls_nodes(indod,1,k,node)
     x                     * trieo_ls_nodes(indod,1,k,node)
                  indod=indod+1
               endif
            end do
cc
            indev=indlsev(l  ,l)
            indod=indlsod(l+1,l)
            do n=l, jcap
               if(mod(n+l,2).eq.0) then
                  fgbar(k) =
     x            fgbar(k) + trieo_ls_nodes(indev,2,k,node)
     x                     * trieo_ls_nodes(indev,2,k,node)
                  indev=indev+1
               else
                  fgbar(k) =
     x            fgbar(k) + trieo_ls_nodes(indod,2,k,node)
     x                     * trieo_ls_nodes(indod,2,k,node)
                  indod=indod+1
               endif
            end do
cc
         end do
cc
         fgbar(k) = sqrt(fgbar(k))
cc
      end do
cc
ccmr  vx=0.e0      !constant
!mjr  vx=cons0     !constant
ccmr  vy=0.e0      !constant
!mjr  vy=cons0     !constant
ccmr  vw=0.e0      !constant
!mjr  vw=cons0     !constant
ccmr  vr=0.e0      !constant
!mjr  vr=cons0     !constant
cc
!mjr  do k=1,levs
!mjr     vx=vx+fgbar(kmx+k-1)*del(k)
!mjr     vy=vy+fgbar(kmy+k-1)*del(k)
!mjr     vw=vw+fgbar(kmw+k-1)*del(k)
!mjr     vr=vr+fgbar(kmr+k-1)*del(k)
!mjr  end do
cc
      if ( level .eq. 1 ) then
cc
        if ( gen_coord_hybrid ) then					! hmhj
         print 101,fgbar(kmq)						! hmhj
  101    format(/ 1x,3x, ' rms_spect   ps= ',1(es17.10,1x) /)		! hmhj
        else								! hmhj
         print 100,fgbar(kmq)
  100    format(/ 1x,3x, ' rms_spect   rms_ln(ps)= ',1(es17.10,1x) /)
        endif								! hmhj
cc
         print 50
   50    format(1x,3x,
     x          ' rms_spect      div',
     x          '               vort',
     x          '               temp')
cc
      endif
cc
      fgbar_sav(1,level) = fgbar(kmx)
      fgbar_sav(2,level) = fgbar(kmw)
      fgbar_sav(3,level) = fgbar(kmy)
!mjr  fgbar_sav(4,level) = fgbar(kmr)
!mjr  fgbar_sav(5,level) = fgbar(kmr+1)
!mjr  fgbar_sav(6,level) = fgbar(kmr+2)
cc
      do k=1,ntrac
         fgbar_ntrac(k,level) = fgbar(kmr+k-1)
      enddo
cc
      print 200,level,
     x          fgbar(kmx), fgbar(kmw), fgbar(kmy)
ccmr x      , ( fgbar(kmr+k-1), k=1,ntrac )
  200 format(1x,i3,6(2x,es17.10))
cc
 2000 continue  ! xxxxxxxxxxxxxxxxxxxx end big level loop
cc
      if ( me .eq. 0 ) then  ! yyyyyyyyyyyy begin ntrac print
cc
         print 52
   52    format(/ 1x,3x,
     x          ' rms_spect mixratio',
     x          '              ozone',
     x          '           cld_liqo')
cc
         do level=1,levs
            print 200, level, (fgbar_ntrac(k,level),
     x                         k=1,min(3,ntrac))
         enddo
cc
cc----------------------------------------------------------------------
cc
         do j=4,ntrac,4
cc
            print 53, (k, ntrac, k=j,min(j+3,ntrac))
   53       format(/ 1x,3x, 6(2x,i3,' of ntrac =',i3:))
cc
            do level=1,levs
               print 200, level, (fgbar_ntrac(k,level),
     x                            k=j,min(j+3,ntrac))
            enddo
cc
         enddo
cc
cc----------------------------------------------------------------------
cc
         print 54
   54    format(/ ' fin rms_spect ' /)
cc
      endif  ! yyyyyyyyyyyy end ntrac print
cc
      deallocate ( trieo_ls_node  )
      deallocate ( trieo_ls_nodes )
cc
      return
      end
