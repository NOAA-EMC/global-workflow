      subroutine gefs_dezouv(dev,zod,uev,vod,epsedn,epsodn,
     x                       snnp1ev,snnp1od,ls_node, 
     x                       jcap, len_trie_ls, 
     x                       len_trio_ls, ls_dim, 
     x                       ls_max_node, rerth)

      use machine,  only: kind_evod, kind_phys
cc

cc
      implicit none
cc
      real(kind=kind_evod)     dev(len_trie_ls,2)
      real(kind=kind_evod)     zod(len_trio_ls,2)
      real(kind=kind_evod)     uev(len_trie_ls,2)
      real(kind=kind_evod)     vod(len_trio_ls,2)
cc
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
cc
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      real(kind = kind_phys) rerth
cc
      integer              jcap, len_trie_ls, len_trio_ls
      integer              ls_dim, ls_max_node
      integer              ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer              l,locl,n
cc
      integer              indev,indev1,indev2
      integer              indod,indod1,indod2
      integer              inddif
cc
      real(kind=kind_evod) rl
cc
      real(kind=kind_evod) cons0     !constant
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
cc
cc
cc......................................................................
cc
cc
      cons0 = 0.d0     !constant
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
cc
         uev(indlsev(l,l),1) = cons0     !constant
         uev(indlsev(l,l),2) = cons0     !constant
cc
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(l,l) + 1
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         indod1 = indlsod(l+1,l)
         inddif = indev1 - indod1
cc
         do indev = indev1 , indev2
cc
            uev(indev,1) = -epsedn(indev)
     x                       * zod(indev-inddif,1)
cc
            uev(indev,2) = -epsedn(indev)
     x                       * zod(indev-inddif,2)
cc
         enddo
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap-1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         indod1 = indlsod(l+1,l)
         inddif = indev1 - indod1
cc
         do indev = indev1 , indev2
cc
            vod(indev-inddif,1) = epsodn(indev-inddif)
     x                             * dev(indev,1)
cc
            vod(indev-inddif,2) = epsodn(indev-inddif)
     x                             * dev(indev,2)
cc
         enddo
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap-1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         if ( l .ge. 1 ) then
              rl = l
            do indev = indev1 , indev2
cc             u(l,n)=-i*l*d(l,n)/(n*(n+1))
cc
               uev(indev,1) = uev(indev,1)
     1                 + rl * dev(indev,2)
     2                  / snnp1ev(indev)
cc
               uev(indev,2) = uev(indev,2)
     1                 - rl * dev(indev,1)
     2                  / snnp1ev(indev)
cc
            enddo
         endif
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod1 = indlsod(l+1,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indod2 = indlsod(jcap  ,l)
         else
            indod2 = indlsod(jcap+1,l) - 1
         endif
         if ( l .ge. 1 ) then
              rl = l
            do indod = indod1 , indod2
cc             u(l,n)=-i*l*d(l,n)/(n*(n+1))
cc
               vod(indod,1) = vod(indod,1)
     1                 + rl * zod(indod,2)
     2                  / snnp1od(indod)
cc
               vod(indod,2) = vod(indod,2)
     1                 - rl * zod(indod,1)
     2                  / snnp1od(indod)
cc
            enddo
         endif
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l) - 1
         else
            indev2 = indlsev(jcap  ,l) - 1
         endif
         indod1 = indlsod(l+1,l)
         inddif = indev1 - indod1
cc
         do indev = indev1 , indev2
cc
                 uev(indev,1)      = uev(indev       ,1)
     1      + epsodn(indev-inddif) * zod(indev-inddif,1)
cc
                 uev(indev,2)      = uev(indev       ,2)
     1      + epsodn(indev-inddif) * zod(indev-inddif,2)
cc
         enddo
cc
      enddo
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(l,l) + 1
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap-1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         indod1 = indlsod(l+1,l)
         inddif = indev1 - indod1
cc
         do indev = indev1 , indev2
cc
                 vod(indev-inddif,1) = vod(indev-inddif,1)
     1      - epsedn(indev)          * dev(indev       ,1)
cc
                 vod(indev-inddif,2) = vod(indev-inddif,2)
     1      - epsedn(indev)          * dev(indev       ,2)
cc
         enddo
cc
      enddo
cc
cc......................................................................
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
         indev1 = indlsev(l,l)
         indod1 = indlsod(l+1,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
            indod2 = indlsod(jcap  ,l)
         else
            indev2 = indlsev(jcap  ,l)
            indod2 = indlsod(jcap+1,l)
         endif
         do indev = indev1 , indev2
cc
            uev(indev,1) = uev(indev,1) * rerth
            uev(indev,2) = uev(indev,2) * rerth
cc
         enddo
cc
         do indod = indod1 , indod2
cc
            vod(indod,1) = vod(indod,1) * rerth
            vod(indod,2) = vod(indod,2) * rerth
cc
         enddo
cc
      enddo
cc
      end subroutine gefs_dezouv
