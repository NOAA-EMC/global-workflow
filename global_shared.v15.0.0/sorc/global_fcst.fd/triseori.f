      subroutine triseori(trisca,triev,triod,levels,
     x                    ls_node)
cc
      use resol_def
      use layout1
      implicit none
      integer              levels

      real(kind=kind_evod) trisca(lnt2,levels)
      real(kind=kind_evod)  triev(len_trie_ls,2,levels)
      real(kind=kind_evod)  triod(len_trio_ls,2,levels)
cc
      integer              ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
!cmr    copy real even elements of scalar complex even-odd triangles
!cmr    to   real even triangles.  set top rows to zeros.
!cmr    copy imaginary even elements of scalar complex even-odd triangles
!cmr    to   imaginary even triangles.  set top rows to zeros.
!cmr    copy real odd elements of scalar complex even-odd triangles
!cmr    to   real odd triangles.  set top rows to zeros.
!cmr    copy imaginary odd elements of scalar complex even-odd triangles
!cmr    to   imaginary odd triangles.  set top rows to zeros.
cc
cc    local scalars
cc    -------------
cc
      integer   n
      integer   l
      integer   locl
      integer   k
cc
      integer   indsca
      integer   indev
      integer   indod
      integer   indev1,indev2
      integer   indod1,indod2
cc
cc    statement functions
cc    -------------------
cc
cc    offsca is scalar complex even-odd triangle offset in words.
cc
      integer   offsca
cc
      offsca(n,l) = (jcap+1)*(jcap+2) - (jcap-l+1)*(jcap-l+2) + 2*(n-l)
cc
      integer   indlsev,jbasev
      integer   indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
cc
cc
      real(kind=kind_evod) cons0     !constant
cc
      cons0 = 0.d0     !constant
cc
cc......................................................................
cc
      do k = 1, levels
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
cc          copy the even (n-l) terms for each level
            indsca=offsca(l,l)
            do indev = indev1 , indev2
cc             real part
               triev(indev,1,k) = trisca(indsca+1,k)
cc             imaginary part
               triev(indev,2,k) = trisca(indsca+2,k)
               indsca=indsca+4
            end do
         end do
cc
cc......................................................................
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
          if (l .ne. jcap) then
            jbasod=ls_node(locl,3)
            indod1 = indlsod(l+1,l)
            if (mod(l,2).eq.mod(jcap+1,2)) then
               indod2 = indlsod(jcap  ,l)
            else
               indod2 = indlsod(jcap-1,l)
            endif
cc          copy the odd (n-l) terms for each level
            indsca=offsca(l+1,l)
            do indod = indod1 , indod2
cc             real part
               triod(indod,1,k) = trisca(indsca+1,k)
cc             imaginary part
               triod(indod,2,k) = trisca(indsca+2,k)
               indsca=indsca+4
            end do
 
         endif
         end do
cc
cc......................................................................
cc
         do locl=1,ls_max_node
                    l=ls_node(locl,1)
               jbasev=ls_node(locl,2)
               jbasod=ls_node(locl,3)
            if (mod(l,2).eq.mod(jcap+1,2)) then
cc             set the even (n-l) terms of the top row to zero
cc             real part
               triev(indlsev(jcap+1,l),1,k) = cons0     !constant
cc             imaginary part
               triev(indlsev(jcap+1,l),2,k) = cons0     !constant
            else
cc             set the  odd (n-l) terms of the top row to zero
cc             real part
               triod(indlsod(jcap+1,l),1,k) = cons0     !constant
cc             imaginary part
               triod(indlsod(jcap+1,l),2,k) = cons0     !constant
            endif
         end do
cc
      end do
cc
      return
      end
