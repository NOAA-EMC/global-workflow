      subroutine filtr2eo
     x     (teme,tee,ye,dime,die,xe,zeme,zee,we,rme,rqe,rte,
     x      temo,teo,yo,dimo,dio,xo,zemo,zeo,wo,rmo,rqo,rto,filta,
     x      ls_node)
cc
      use resol_def
      use layout1
      implicit none
cc
      real(kind=kind_evod)
     1  tee(len_trie_ls,2,levs),  die(len_trie_ls,2,levs),
     1 teme(len_trie_ls,2,levs), dime(len_trie_ls,2,levs),
     1   ye(len_trie_ls,2,levs),   xe(len_trie_ls,2,levs),
     1  rqe(len_trie_ls,2,levh),  rte(len_trie_ls,2,levh)
cc
      real(kind=kind_evod)
     1  zee(len_trie_ls,2,levs),
     1 zeme(len_trie_ls,2,levs),
     1   we(len_trie_ls,2,levs),
     1  rme(len_trie_ls,2,levh)
cc
      real(kind=kind_evod)
     1  teo(len_trio_ls,2,levs),  dio(len_trio_ls,2,levs),
     1 temo(len_trio_ls,2,levs), dimo(len_trio_ls,2,levs),
     1   yo(len_trio_ls,2,levs),   xo(len_trio_ls,2,levs),
     1  rqo(len_trio_ls,2,levh),  rto(len_trio_ls,2,levh)
cc
      real(kind=kind_evod)
     1  zeo(len_trio_ls,2,levs),
     1 zemo(len_trio_ls,2,levs),
     1   wo(len_trio_ls,2,levs),
     1  rmo(len_trio_ls,2,levh)
cc
      real(kind=kind_evod) filta
cc
      integer              ls_node(ls_dim,3)
cc
cc
      integer              k,l,locl,n
cc
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      real(kind=kind_evod) filtb
cc
      real(kind=kind_evod) cons0p5,cons1     !constant
cc
      integer                      jbasev
      integer                      jbasod
!!
      cons0p5 = 0.5d0                        !constant
      cons1   = 1.d0                         !constant
cc
cc
      filtb = (cons1-filta)*cons0p5          !constant
cc
cc
!$omp parallel do private(k,jbasev,jbasod,locl,l,indev,indod)
!$omp+private(indev1,indev2,indod1,indod2)
      do k=1,levs
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            indev1 = jbasev+(l-l)/2+1
            if (mod(l,2).eq.mod(jcap+1,2)) then
               indev2 = jbasev+(jcap+1-l)/2+1
            else
               indev2 = jbasev+(jcap  -l)/2+1
            endif
            do indev = indev1 , indev2
cc
                tee(indev,1,k) =           ye(indev,1,k)
                tee(indev,2,k) =           ye(indev,2,k)
                die(indev,1,k) =           xe(indev,1,k)
                die(indev,2,k) =           xe(indev,2,k)
                zee(indev,1,k) =           we(indev,1,k)
                zee(indev,2,k) =           we(indev,2,k)
               teme(indev,1,k) =         teme(indev,1,k)
     x                         + filtb * tee (indev,1,k)
               teme(indev,2,k) =         teme(indev,2,k)
     x                         + filtb * tee (indev,2,k)
               dime(indev,1,k) =         dime(indev,1,k)
     x                         + filtb * die (indev,1,k)
               dime(indev,2,k) =         dime(indev,2,k)
     x                         + filtb * die (indev,2,k)
               zeme(indev,1,k) =         zeme(indev,1,k)
     x                         + filtb * zee (indev,1,k)
               zeme(indev,2,k) =         zeme(indev,2,k)
     x                         + filtb * zee (indev,2,k)
cc
            enddo
         enddo
cc
cc......................................................................
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasod=ls_node(locl,3)
            indod1 = jbasod+(l+1-l)/2+1
            if (mod(l,2).eq.mod(jcap+1,2)) then
               indod2 = jbasod+(jcap  -l)/2+1
            else
               indod2 = jbasod+(jcap+1-l)/2+1
            endif
            do indod = indod1 , indod2
cc
                teo(indod,1,k) =           yo(indod,1,k)
                teo(indod,2,k) =           yo(indod,2,k)
                dio(indod,1,k) =           xo(indod,1,k)
                dio(indod,2,k) =           xo(indod,2,k)
                zeo(indod,1,k) =           wo(indod,1,k)
                zeo(indod,2,k) =           wo(indod,2,k)
               temo(indod,1,k) =         temo(indod,1,k)
     x                         + filtb * teo (indod,1,k)
               temo(indod,2,k) =         temo(indod,2,k)
     x                         + filtb * teo (indod,2,k)
               dimo(indod,1,k) =         dimo(indod,1,k)
     x                         + filtb * dio (indod,1,k)
               dimo(indod,2,k) =         dimo(indod,2,k)
     x                         + filtb * dio (indod,2,k)
               zemo(indod,1,k) =         zemo(indod,1,k)
     x                         + filtb * zeo (indod,1,k)
               zemo(indod,2,k) =         zemo(indod,2,k)
     x                         + filtb * zeo (indod,2,k)
cc
            enddo
         enddo
cc
      enddo
cc
cc......................................................................
cc
!$omp parallel do private(k,jbasev,jbasod,locl,l,indev,indod)
!$omp+private(indev1,indev2,indod1,indod2)
      do k=1,levh
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            indev1 = jbasev+(l-l)/2+1
            if (mod(l,2).eq.mod(jcap+1,2)) then
               indev2 = jbasev+(jcap+1-l)/2+1
            else
               indev2 = jbasev+(jcap  -l)/2+1
            endif
            do indev = indev1 , indev2
cc
               rqe(indev,1,k) =         rte(indev,1,k)
               rqe(indev,2,k) =         rte(indev,2,k)
               rme(indev,1,k) =         rme(indev,1,k)
     x                        + filtb * rqe(indev,1,k)
               rme(indev,2,k) =         rme(indev,2,k)
     x                        + filtb * rqe(indev,2,k)
cc
            enddo
         enddo
cc
cc......................................................................
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasod=ls_node(locl,3)
            indod1 = jbasod+(l+1-l)/2+1
            if (mod(l,2).eq.mod(jcap+1,2)) then
               indod2 = jbasod+(jcap  -l)/2+1
            else
               indod2 = jbasod+(jcap+1-l)/2+1
            endif
            do indod = indod1 , indod2
cc
               rqo(indod,1,k) =         rto(indod,1,k)
               rqo(indod,2,k) =         rto(indod,2,k)
               rmo(indod,1,k) =         rmo(indod,1,k)
     x                        + filtb * rqo(indod,1,k)
               rmo(indod,2,k) =         rmo(indod,2,k)
     x                        + filtb * rqo(indod,2,k)
cc
            enddo
         enddo
cc
      enddo
!!
      return
      end
