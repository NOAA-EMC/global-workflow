      subroutine gozrineo_a_stochy(plnev_a,plnod_a,
     x                      pddev_a,pddod_a,
     x                      plnew_a,plnow_a,
     x                      epse,epso,rcs2_a,wgt_a,ls_node,num_lat)
cc
      use stochy_resol_def
      use spectral_layout
      use machine
      implicit none
cc
      real(kind=kind_dbl_prec) plnev_a(len_trie_ls,latg2)
      real(kind=kind_dbl_prec) plnod_a(len_trio_ls,latg2)
      real(kind=kind_dbl_prec) pddev_a(len_trie_ls,latg2)
      real(kind=kind_dbl_prec) pddod_a(len_trio_ls,latg2)
      real(kind=kind_dbl_prec) plnew_a(len_trie_ls,latg2)
      real(kind=kind_dbl_prec) plnow_a(len_trio_ls,latg2)
cc
      real(kind=kind_dbl_prec)    epse(len_trie_ls)
      real(kind=kind_dbl_prec)    epso(len_trio_ls)
cc
      real(kind=kind_dbl_prec) rcs2_a(latg2)
      real(kind=kind_dbl_prec)  wgt_a(latg2)
cc
      integer                  ls_node(ls_dim,3)
cc
      integer                  num_lat
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
      integer                  l,lat,locl,n
cc
      integer                  indev,indev1,indev2
      integer                  indod,indod1,indod2
      integer                  inddif
cc
      real(kind=kind_dbl_prec) rn,rnp1,wcsa
cc
      real(kind=kind_dbl_prec) cons0     !constant
      real(kind=kind_dbl_prec) cons2     !constant
      real  rerth
cc
      integer                  indlsev,jbasev
      integer                  indlsod,jbasod
cc
      include 'function2'
cc
cc
      cons0 = 0.d0     !constant
      cons2 = 2.d0     !constant
      rerth  =6.3712e+6      ! radius of earth (m)
cc
cc
      do lat=1,num_lat
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            jbasod=ls_node(locl,3)
cc
            rn=l
cc
cc
            pddev_a(indlsev(l,l),lat) = -epso(indlsod(l+1,l))
     x                              * plnod_a(indlsod(l+1,l),lat) * rn
            indev1 = indlsev(L,L) + 1
            if (mod(L,2).eq.mod(jcap+1,2)) then
               indev2 = indlsev(jcap-1,L)
            else
               indev2 = indlsev(jcap  ,L)
            endif
            indod1 = indlsod(l+1,l)
            inddif = indev1 - indod1
cc
            rn  =l+2
            rnp1=l+2+1
            do indev = indev1 , indev2
cc
               pddev_a(indev,lat) = epse(indev)
     x                         * plnod_a(indev-inddif  ,lat) * rnp1
     x                            - epso(indev-inddif+1)
     x                         * plnod_a(indev-inddif+1,lat) * rn
cc
               rn   = rn   + cons2     !constant
               rnp1 = rnp1 + cons2     !constant
            enddo
cc
cc......................................................................
            indev1 = indlsev(L,L)
            if (mod(L,2).eq.mod(jcap+1,2)) then
               indev2 = indlsev(jcap+1,L) - 1
            else
               indev2 = indlsev(jcap  ,L) - 1
            endif
            indod1 = indlsod(l+1,l)
            inddif = indev1 - indod1
cc
            rn  =l+1
            rnp1=l+1+1
            do indev = indev1 , indev2
cc
               pddod_a(indev-inddif,lat) = epso(indev-inddif)
     x                                * plnev_a(indev  ,lat) * rnp1
     x                                   - epse(indev+1)
     x                                * plnev_a(indev+1,lat) * rn
cc
               rn   = rn   + cons2     !constant
               rnp1 = rnp1 + cons2     !constant
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
cc
            if (mod(L,2).eq.mod(jcap+1,2)) then
cc
cc             set the even (n-l) terms of the top row to zero
               pddev_a(indlsev(jcap+1,l),lat) = cons0     !constant
cc
            else
cc
cc             set the  odd (n-l) terms of the top row to zero
               pddod_a(indlsod(jcap+1,l),lat) = cons0     !constant
cc
            endif
cc
         enddo
cc
cc......................................................................
cc
         wcsa=rcs2_a(lat)/rerth
cc
cc
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            jbasod=ls_node(locl,3)
            indev1 = indlsev(L,L)
            indod1 = indlsod(L+1,L)
            if (mod(L,2).eq.mod(jcap+1,2)) then
               indev2 = indlsev(jcap+1,L)
               indod2 = indlsod(jcap  ,L)
            else
               indev2 = indlsev(jcap  ,L)
               indod2 = indlsod(jcap+1,L)
            endif
            do indev = indev1 , indev2
cc
               pddev_a(indev,lat) = pddev_a(indev,lat) * wcsa
               plnew_a(indev,lat) = plnev_a(indev,lat) * wgt_a(lat)
cc
            enddo
cc
            do indod = indod1 , indod2
cc
               pddod_a(indod,lat) = pddod_a(indod,lat) * wcsa
               plnow_a(indod,lat) = plnod_a(indod,lat) * wgt_a(lat)
cc
            enddo
cc
         enddo
cc
      enddo
cc
      return
      end
