      subroutine impadje_hyb_gc(de,te,qe,xe,ye,ze,
     x                       dti,ue,ve,snnp1ev,ndexev,
     x                       ls_node,locl)

      use resol_def
      use layout1
      use coordinate_def
      implicit none
      real(kind=kind_evod) de(len_trie_ls,2,levs),te(len_trie_ls,2,levs)
      real(kind=kind_evod) xe(len_trie_ls,2,levs),ye(len_trie_ls,2,levs)
      real(kind=kind_evod) ue(len_trie_ls,2,levs),ve(len_trie_ls,2,levs)
      real(kind=kind_evod) qe(len_trie_ls,2),    ze(len_trie_ls,2)
      real(kind=kind_evod) dti,dt
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      integer               ndexev(len_trie_ls)
      integer              j,k
      integer              ls_node(ls_dim,3)
!
      integer              indev,indev1,indev2,l,locl,n
      integer              indlsev,jbasev
!     integer              indlsod,jbasod
!     include 'function_indlsev'
      indlsev(n,l) = jbasev + (n-l)/2 + 1
!
      real(kind=kind_evod) cons0     !constant
 
!     print *,' enter impadje_hyb.locl_gc_fd '			! hmhj

!     eps_si=0.20
      dt=dti*(1.+eps_si)

      cons0 = 0.d0     !constant
           l = ls_node(locl,1)
      jbasev = ls_node(locl,2)
      indev1 = indlsev(l,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indev2 = indlsev(jcap+1,l)
      else
         indev2 = indlsev(jcap  ,l)
      endif
      do k=1,levs
 
          do indev = indev1 , indev2
            ve(indev,1,k)=cons0     !constant
            ve(indev,2,k)=cons0     !constant
          enddo
 
        do j=1,levs
          do indev = indev1 , indev2
            ve(indev,1,k)=ve(indev,1,k)+amhyb(k,j)*ye(indev,1,j)
            ve(indev,2,k)=ve(indev,2,k)+amhyb(k,j)*ye(indev,2,j)
          enddo
        enddo
 
          do indev = indev1 , indev2
            ue(indev,1,k)=xe(indev,1,k)
     &     +dt*snnp1ev(indev)*(ve(indev,1,k)+tor_hyb(k)*ze(indev,1))
 
            ue(indev,2,k)=xe(indev,2,k)
     &     +dt*snnp1ev(indev)*(ve(indev,2,k)+tor_hyb(k)*ze(indev,2))
          enddo
 
      enddo
 
      do k=1,levs
 
          do indev = indev1 , indev2
            ve(indev,1,k)=cons0     !constant
            ve(indev,2,k)=cons0     !constant
          enddo
 
        do j=1,levs
          do indev = indev1 , indev2
            ve(indev,1,k)=
     x      ve(indev,1,k) +dm205_hyb(ndexev(indev)+1,k,j)*ue(indev,1,j)
            ve(indev,2,k)=
     x      ve(indev,2,k) +dm205_hyb(ndexev(indev)+1,k,j)*ue(indev,2,j)
!           ve(indev,1,k)=
!    x      ve(indev,1,k) +d_hyb_m(k,j,ndexev(indev)+1)*ue(indev,1,j)
!           ve(indev,2,k)=
!    x      ve(indev,2,k) +d_hyb_m(k,j,ndexev(indev)+1)*ue(indev,2,j)
          enddo
        enddo
 
      enddo
 
      do j=1,levs
 
        do indev = indev1 , indev2
          qe(indev,1)=qe(indev,1)-dt*svhyb(j)*ve(indev,1,j)
          qe(indev,2)=qe(indev,2)-dt*svhyb(j)*ve(indev,2,j)
        enddo
 
      enddo
 
        do indev = indev1 , indev2
          qe(indev,1)=qe(indev,1)+ze(indev,1)
          qe(indev,2)=qe(indev,2)+ze(indev,2)
        enddo
 
      do k=1,levs
 
        do j=1,levs
          do indev = indev1 , indev2
            te(indev,1,k)=te(indev,1,k)-dt*bmhyb(k,j)*ve(indev,1,j)
            te(indev,2,k)=te(indev,2,k)-dt*bmhyb(k,j)*ve(indev,2,j)
          enddo
        enddo
 
          do indev = indev1 , indev2
            te(indev,1,k)=te(indev,1,k)+ye(indev,1,k)
            te(indev,2,k)=te(indev,2,k)+ye(indev,2,k)
 
            de(indev,1,k)=de(indev,1,k)+ve(indev,1,k)
            de(indev,2,k)=de(indev,2,k)+ve(indev,2,k)
          enddo
 
      enddo

!     print *,' leave impadje_hyb.locl_gc_fd '		! hmhj
 
      return
      end
      subroutine impadjo_hyb_gc(do,to,qo,xo,yo,zo,
     x                       dti,uo,vo,snnp1od,ndexod,
     x                       ls_node,locl)
      use resol_def
      use layout1
      use coordinate_def
      implicit none
      real(kind=kind_evod) do(len_trio_ls,2,levs),to(len_trio_ls,2,levs)
      real(kind=kind_evod) xo(len_trio_ls,2,levs),yo(len_trio_ls,2,levs)
      real(kind=kind_evod) uo(len_trio_ls,2,levs),vo(len_trio_ls,2,levs)
      real(kind=kind_evod) qo(len_trio_ls,2),    zo(len_trio_ls,2)
      real(kind=kind_evod) dti,dt
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer               ndexod(len_trio_ls)
      integer              j,k
      integer              ls_node(ls_dim,3)
!
      integer              indod,indod1,indod2,l,locl,n
!     integer              indlsev,jbasev
      integer              indlsod,jbasod
!     include 'function_indlsod'
      indlsod(n,l) = jbasod + (n-l)/2 + 1
!
      real(kind=kind_evod) cons0     !constant

!     print *,' enter impadjo_hyb.locl_gc_fd '			! hmhj

!     eps_si=0.20
      dt=dti*(1.+eps_si)
 
      cons0 = 0.d0     !constant
           l = ls_node(locl,1)
      jbasod = ls_node(locl,3)
      indod1 = indlsod(l+1,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indod2 = indlsod(jcap  ,l)
      else
         indod2 = indlsod(jcap+1,l)
      endif
      do k=1,levs
 
          do indod = indod1 , indod2
            vo(indod,1,k)=cons0     !constant
            vo(indod,2,k)=cons0     !constant
          enddo
 
        do j=1,levs
          do indod = indod1 , indod2
            vo(indod,1,k)=vo(indod,1,k)+amhyb(k,j)*yo(indod,1,j)
            vo(indod,2,k)=vo(indod,2,k)+amhyb(k,j)*yo(indod,2,j)
          enddo
        enddo
 
          do indod = indod1 , indod2
            uo(indod,1,k)=xo(indod,1,k)
     &     +dt*snnp1od(indod)*(vo(indod,1,k)+tor_hyb(k)*zo(indod,1))
 
            uo(indod,2,k)=xo(indod,2,k)
     &     +dt*snnp1od(indod)*(vo(indod,2,k)+tor_hyb(k)*zo(indod,2))
          enddo
 
      enddo
 
      do k=1,levs
 
          do indod = indod1 , indod2
            vo(indod,1,k)=cons0     !constant
            vo(indod,2,k)=cons0     !constant
          enddo
 
        do j=1,levs
          do indod = indod1 , indod2
            vo(indod,1,k)=
     x      vo(indod,1,k) +dm205_hyb(ndexod(indod)+1,k,j)*uo(indod,1,j)
            vo(indod,2,k)=
     x      vo(indod,2,k) +dm205_hyb(ndexod(indod)+1,k,j)*uo(indod,2,j)
!           vo(indod,1,k)=
!    x      vo(indod,1,k) +d_hyb_m(k,j,ndexod(indod)+1)*uo(indod,1,j)
!           vo(indod,2,k)=
!    x      vo(indod,2,k) +d_hyb_m(k,j,ndexod(indod)+1)*uo(indod,2,j)
          enddo
        enddo
 
      enddo
 
      do j=1,levs
 
        do indod = indod1 , indod2
          qo(indod,1)=qo(indod,1)-dt*svhyb(j)*vo(indod,1,j)
          qo(indod,2)=qo(indod,2)-dt*svhyb(j)*vo(indod,2,j)
        enddo
 
      enddo
 
        do indod = indod1 , indod2
          qo(indod,1)=qo(indod,1)+zo(indod,1)
          qo(indod,2)=qo(indod,2)+zo(indod,2)
        enddo
 
      do k=1,levs
 
        do j=1,levs
          do indod = indod1 , indod2
            to(indod,1,k)=to(indod,1,k)-dt*bmhyb(k,j)*vo(indod,1,j)
            to(indod,2,k)=to(indod,2,k)-dt*bmhyb(k,j)*vo(indod,2,j)
          enddo
        enddo
 
          do indod = indod1 , indod2
            to(indod,1,k)=to(indod,1,k)+yo(indod,1,k)
            to(indod,2,k)=to(indod,2,k)+yo(indod,2,k)
 
            do(indod,1,k)=do(indod,1,k)+vo(indod,1,k)
            do(indod,2,k)=do(indod,2,k)+vo(indod,2,k)
          enddo
 
      enddo
 
!     print *,' end of impadjo_hyb.locl_gc_fd '			! hmhj

      return
      end
