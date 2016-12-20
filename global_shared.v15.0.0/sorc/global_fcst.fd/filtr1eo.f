      subroutine filtr1eo
     x     (teme,tee,ye,dime,die,xe,zeme,zee,we,rme,rqe,rte,
     x      temo,teo,yo,dimo,dio,xo,zemo,zeo,wo,rmo,rqo,rto,filta,
     x      ls_node)
cc
      use resol_def
      use layout1
      implicit none
c
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
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
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
!
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
               teme(indev,1,k) = filtb * teme(indev,1,k)
     x                         + filta * tee (indev,1,k)
               teme(indev,2,k) = filtb * teme(indev,2,k)
     x                         + filta * tee (indev,2,k)
               dime(indev,1,k) = filtb * dime(indev,1,k)
     x                         + filta * die (indev,1,k)
               dime(indev,2,k) = filtb * dime(indev,2,k)
     x                         + filta * die (indev,2,k)
               zeme(indev,1,k) = filtb * zeme(indev,1,k)
     x                         + filta * zee (indev,1,k)
               zeme(indev,2,k) = filtb * zeme(indev,2,k)
     x                          +filta * zee (indev,2,k)
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
               temo(indod,1,k) = filtb * temo(indod,1,k)
     x                         + filta * teo (indod,1,k)
               temo(indod,2,k) = filtb * temo(indod,2,k)
     x                         + filta * teo (indod,2,k)
               dimo(indod,1,k) = filtb * dimo(indod,1,k)
     x                         + filta * dio (indod,1,k)
               dimo(indod,2,k) = filtb * dimo(indod,2,k)
     x                         + filta * dio (indod,2,k)
               zemo(indod,1,k) = filtb * zemo(indod,1,k)
     x                         + filta * zeo (indod,1,k)
               zemo(indod,2,k) = filtb * zemo(indod,2,k)
     x                         + filta * zeo (indod,2,k)
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
               rme(indev,1,k) = filtb * rme(indev,1,k)
     x                        + filta * rqe(indev,1,k)
               rme(indev,2,k) = filtb * rme(indev,2,k)
     x                        + filta * rqe(indev,2,k)
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
               rmo(indod,1,k) = filtb * rmo(indod,1,k)
     x                        + filta * rqo(indod,1,k)
               rmo(indod,2,k) = filtb * rmo(indod,2,k)
     x                        + filta * rqo(indod,2,k)
cc
            enddo
         enddo
cc
      enddo
!
      return
      end
