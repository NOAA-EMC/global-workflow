      subroutine damp_speed(dive,vore,teme,rte,ndexev,
     x                      divo,voro,temo,rto,ndexod,
     x                      sl,spdmax,deltim,
     x                      ls_node)
!
      use resol_def
      use layout1
      use physcons, rerth => con_rerth
      implicit none
!
      real(kind=kind_evod)   dive(len_trie_ls,2)
      real(kind=kind_evod)   vore(len_trie_ls,2)
      real(kind=kind_evod)   teme(len_trie_ls,2)
      real(kind=kind_evod)    rte(len_trie_ls,2,levs,ntrac)
      integer              ndexev(len_trie_ls)
!
      real(kind=kind_evod)   divo(len_trio_ls,2)
      real(kind=kind_evod)   voro(len_trio_ls,2)
      real(kind=kind_evod)   temo(len_trio_ls,2)
      real(kind=kind_evod)    rto(len_trio_ls,2,levs,ntrac)
      integer              ndexod(len_trio_ls)
!
      real(kind=kind_evod)     sl(levs)
      real(kind=kind_evod) spdmax
      real(kind=kind_evod) deltim
cc
      integer              ls_node(ls_dim,3)
cc
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              it,l,n,locl,n0
!     integer              kd,ku
!
      integer              indev
      integer              indod
      integer              indev1,indev2
      integer              indod1,indod2
      real(kind=kind_evod) alfa,alfadt,beta,coef,factor,rncrit
!     real(kind=kind_evod) sf,tk
!
      real(kind=kind_evod) cons0,cons1,cons1p009     !constant
      real(kind=kind_evod) cons2,cons2p5             !constant
!
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
!
      cons0     = 0.d0        !constant
      cons1     = 1.d0        !constant
      cons1p009 = 1.009d0     !constant
      cons2     = 2.d0        !constant
      cons2p5   = 2.5d0       !constant
!
!
      alfa=cons2p5                    !constant
      beta=rerth*cons1p009/deltim     !constant
      alfadt=alfa*deltim/rerth
!
!
! ......................................................................
!
!
         rncrit=beta/spdmax
         if (rncrit.lt.jcap) then
!
            coef=alfadt*spdmax
!!          kd=max(k-1,1)
!!          ku=min(k+1,levs)
!!!!        sf=sl(k)/(sl(ku)-sl(kd))/sqrt(2.)        !constant
!!          sf=sl(k)/(sl(ku)-sl(kd))/sqrt(cons2)     !constant
!!!!        tk=(teme(1,1,ku)-teme(1,1,kd))*sf
!!!!        tk=(teme1(ku)-teme1(kd))*sf
!!
!!!!        do l = 0, jcap
            do locl=1,ls_max_node
                    l=ls_node(locl,1)
               jbasev=ls_node(locl,2)
               if (l.eq.0) then
                                 n0=2
                           else
                                 n0=l
               endif
               indev1 = indlsev(n0,l)
               if (mod(l,2).eq.mod(jcap+1,2)) then
                  indev2 = indlsev(jcap+1,l)
               else
                  indev2 = indlsev(jcap  ,l)
               endif
!!!!           do n = n0, jcap+1, 2
               do indev = indev1 , indev2
!
                  if    (ndexev(indev).gt.rncrit)       then
                 factor=cons1/(cons1+((ndexev(indev) -  rncrit)*coef))
!
                      dive(indev,1)=dive(indev,1)*factor
                      dive(indev,2)=dive(indev,2)*factor
 
                      vore(indev,1)=vore(indev,1)*factor
                      vore(indev,2)=vore(indev,2)*factor
 
                      teme(indev,1)=teme(indev,1)*factor
                      teme(indev,2)=teme(indev,2)*factor
!
                  endif
               enddo
            enddo
!
! ......................................................................
!
!!!!        do l = 0, jcap
            do locl=1,ls_max_node
                    l=ls_node(locl,1)
               jbasod=ls_node(locl,3)
               indod1 = indlsod(l+1,l)
               if (mod(l,2).eq.mod(jcap+1,2)) then
                  indod2 = indlsod(jcap  ,l)
               else
                  indod2 = indlsod(jcap+1,l)
               endif
!!!!           do n = l+1, jcap+1, 2
               do indod = indod1 , indod2
!
                  if    (ndexod(indod).gt.rncrit)       then
                 factor=cons1/(cons1+((ndexod(indod) -  rncrit)*coef))
!
                      divo(indod,1)=divo(indod,1)*factor
                      divo(indod,2)=divo(indod,2)*factor
 
                      voro(indod,1)=voro(indod,1)*factor
                      voro(indod,2)=voro(indod,2)*factor
 
                      temo(indod,1)=temo(indod,1)*factor
                      temo(indod,2)=temo(indod,2)*factor
!
                  endif
               enddo
            enddo
!
! ......................................................................
!
            do it=1,ntrac
!!!!           do l = 0, jcap
               do locl=1,ls_max_node
                       l=ls_node(locl,1)
                  jbasev=ls_node(locl,2)
                  if (l.eq.0) then
                                    n0=2
                              else
                                    n0=l
                  endif
                  indev1 = indlsev(n0,l)
                  if (mod(l,2).eq.mod(jcap+1,2)) then
                     indev2 = indlsev(jcap+1,l)
                  else
                     indev2 = indlsev(jcap  ,l)
                  endif
!!!!              do n = n0, jcap+1, 2
                  do indev = indev1 , indev2
!
                     if   (ndexev(indev).gt.rncrit)       then
                 factor=cons1/(cons1+((ndexev(indev) -  rncrit)*coef))
!
                        rte(indev,1,1,it)=rte(indev,1,1,it)*factor
                        rte(indev,2,1,it)=rte(indev,2,1,it)*factor
!
                     endif
                  enddo
               enddo
!
! ......................................................................
!
!!!!           do l = 0, jcap
               do locl=1,ls_max_node
                       l=ls_node(locl,1)
                  jbasod=ls_node(locl,3)
                  indod1 = indlsod(l+1,l)
                  if (mod(l,2).eq.mod(jcap+1,2)) then
                     indod2 = indlsod(jcap  ,l)
                  else
                     indod2 = indlsod(jcap+1,l)
                  endif
!
!!!!              do n = l+1, jcap+1, 2
                  do indod = indod1 , indod2
!
                     if   (ndexod(indod).gt.rncrit)       then
                 factor=cons1/(cons1+((ndexod(indod) -  rncrit)*coef))
!
                        rto(indod,1,1,it)=rto(indod,1,1,it)*factor
                        rto(indod,2,1,it)=rto(indod,2,1,it)*factor
!
                     endif
                  enddo
               enddo
!
            enddo
         endif
!
!
      return
      end
