      subroutine gefs_uveodz(ulnev,vlnod,flnev,flnod,epse,epso,
     x                       ls_node, jcap, len_trie_ls,
     x                       len_trio_ls, ls_dim,
     x                       ls_max_node, rerth)
cc
      use machine,  only: kind_evod, kind_phys

      implicit none

      real(kind=kind_evod) ulnev(len_trie_ls,2)
      real(kind=kind_evod) vlnod(len_trio_ls,2)
      real(kind=kind_evod) flnev(len_trie_ls,2)
      real(kind=kind_evod) flnod(len_trio_ls,2)
cc
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
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
      real(kind=kind_evod) rl,rn,rnp1
cc
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons2     !constant
cc
      integer              indlsev,jbasev
      integer              indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
cc
cc......................................................................
cc
      cons0 = 0.d0     !constant
      cons2 = 2.d0     !constant
cc
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
           rl = l
           rn = l
         do indev = indev1 , indev2
                                        flnev(indev       ,1) =
     x                            -rl * ulnev(indev       ,2)
     x      + rn * epso(indev-inddif) * vlnod(indev-inddif,1)
cc
                                        flnev(indev       ,2) =
     x                             rl * ulnev(indev       ,1)
     x      + rn * epso(indev-inddif) * vlnod(indev-inddif,2)
              rn = rn + cons2     !constant
         end do
cc
      end do
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
         rl   = l
         rnp1 = l+2+1
         do indev = indev1 , indev2
                                   flnev(indev       ,1) =
     x                             flnev(indev       ,1)
     x      - rnp1 * epse(indev) * vlnod(indev-inddif,1)
cc
                                   flnev(indev       ,2) =
     x                             flnev(indev       ,2)
     x      - rnp1 * epse(indev) * vlnod(indev-inddif,2)
              rnp1 = rnp1 + cons2     !constant
         end do
cc
      end do
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
         rl   = l
         rn   = l+1
         rnp1 = l+1+1
         do indev = indev1 , indev2
                                          flnod(indev-inddif,1) =
     x                              -rl * vlnod(indev-inddif,2)
     x      - rn   * epse(indev+1     ) * ulnev(indev+1     ,1)
     x      + rnp1 * epso(indev-inddif) * ulnev(indev       ,1)
cc
                                          flnod(indev-inddif,2) =
     x                               rl * vlnod(indev-inddif,1)
     x      - rn   * epse(indev+1     ) * ulnev(indev+1     ,2)
     x      + rnp1 * epso(indev-inddif) * ulnev(indev       ,2)
              rn   = rn   + cons2     !constant
              rnp1 = rnp1 + cons2     !constant
         end do
cc
      end do
cc
cc......................................................................
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         jbasod=ls_node(locl,3)
cc
         if (mod(l,2).eq.mod(jcap+1,2)) then
cc          set the even (n-l) terms of the top row to zero
            flnev(indlsev(jcap+1,l),1) = cons0     !constant
            flnev(indlsev(jcap+1,l),2) = cons0     !constant
         else
cc          set the  odd (n-l) terms of the top row to zero
            flnod(indlsod(jcap+1,l),1) = cons0     !constant
            flnod(indlsod(jcap+1,l),2) = cons0     !constant
         endif
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
            flnev(indev,1)=flnev(indev,1)/rerth
            flnev(indev,2)=flnev(indev,2)/rerth
         enddo
cc
         do indod = indod1 , indod2
            flnod(indod,1)=flnod(indod,1)/rerth
            flnod(indod,2)=flnod(indod,2)/rerth
         enddo
cc
cc
      enddo
cc
      end subroutine gefs_uveodz
