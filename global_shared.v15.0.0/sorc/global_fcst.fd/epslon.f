      subroutine epslon(epse,epso,epsedn,epsodn,
     x                  ls_node)
cc
      use resol_def
      use layout1
      implicit none
cc
      real(kind=kind_dbl_prec)   epse(len_trie_ls)
      real(kind=kind_dbl_prec)   epso(len_trio_ls)
cc
      real(kind=kind_dbl_prec) epsedn(len_trie_ls)
      real(kind=kind_dbl_prec) epsodn(len_trio_ls)
cc
      integer                  ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer                  l,locl,n
cc
      integer                  indev
      integer                  indod
cc
      real(kind=kind_dbl_prec) f1,f2,rn,val
cc
      real(kind=kind_dbl_prec) cons0     !constant
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function_indlsev'
      include 'function_indlsod'
cc
cc
      cons0=0.0d0     !constant
cc
cc
cc......................................................................
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasev=ls_node(locl,2)
         indev=indlsev(l,l)
         epse  (indev)=cons0     !constant
         epsedn(indev)=cons0     !constant
          indev=indev+1
cc
         do n=l+2,jcap+1,2
            rn=n
            f1=n*n-l*l
            f2=4*n*n-1
            val=sqrt(f1/f2)
            epse  (indev)=val
            epsedn(indev)=val/rn
             indev=indev+1
         enddo
cc
      enddo
cc
cc
cc......................................................................
cc
cc
      do locl=1,ls_max_node
              l=ls_node(locl,1)
         jbasod=ls_node(locl,3)
         indod=indlsod(l+1,l)
cc
         do n=l+1,jcap+1,2
            rn=n
            f1=n*n-l*l
            f2=4*n*n-1
            val=sqrt(f1/f2)
            epso  (indod)=val
            epsodn(indod)=val/rn
             indod=indod+1
         enddo
cc
      enddo
cc
      return
      end
