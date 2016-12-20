      subroutine delnpe(qe,odphi,edlam,epse,epso,
     x                  ls_node)
cc

cc
      use resol_def
      use layout1
      use physcons, rerth => con_rerth
      implicit none
cc
cc    input q is in ibm triang. order
cc    output  is in ibm triang. order
cc
      real(kind=kind_evod)    qe(len_trie_ls,2)
      real(kind=kind_evod) odphi(len_trio_ls,2)
      real(kind=kind_evod) edlam(len_trie_ls,2)
cc
      real(kind=kind_evod)  epse(len_trie_ls)
      real(kind=kind_evod)  epso(len_trio_ls)
cc
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
      real(kind=kind_evod) aa,r1mn,rl,rnp2
cc
      real(kind=kind_evod) cons1     !constant
      real(kind=kind_evod) cons2     !constant
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
!!
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev1 = indlsev(l,l)
         if (mod(l,2).eq.mod(jcap+1,2)) then
            indev2 = indlsev(jcap+1,l)
         else
            indev2 = indlsev(jcap  ,l)
         endif
         rl=l
         do indev = indev1 , indev2
cc          dlam(l,n)= i*l*q(l,n)
cc
            edlam(indev,1) = -rl * qe(indev,2)
            edlam(indev,2) =  rl * qe(indev,1)
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
         r1mn = -l
         do indev = indev1 , indev2
cc
                   odphi(indev-inddif,1) =
     1      r1mn *  epso(indev-inddif)   * qe(indev,1)
cc
                   odphi(indev-inddif,2) =
     1      r1mn *  epso(indev-inddif)   * qe(indev,2)
cc
            r1mn = r1mn - cons2     !constant
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
         rnp2 = l+3
         do indev = indev1 , indev2
cc
                   odphi(indev-inddif,1) = odphi(indev-inddif,1) +
     1      rnp2 *  epse(indev)          *    qe(indev       ,1)
cc
                   odphi(indev-inddif,2) = odphi(indev-inddif,2) +
     1      rnp2 *  epse(indev)          *    qe(indev       ,2)
cc
            rnp2 = rnp2 + cons2     !constant
         enddo
cc
      enddo
cc
cc......................................................................
cc
      aa=cons1/rerth     !constant
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
            edlam(indev,1) = edlam(indev,1) * aa
            edlam(indev,2) = edlam(indev,2) * aa
cc
         enddo
cc
         do indod = indod1 , indod2
cc
            odphi(indod,1) = odphi(indod,1) * aa
            odphi(indod,2) = odphi(indod,2) * aa
cc
         enddo
cc
cc
      enddo
!!
      return
      end
