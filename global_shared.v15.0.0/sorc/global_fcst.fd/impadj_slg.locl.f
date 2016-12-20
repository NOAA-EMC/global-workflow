      subroutine impadje_slg(de,te,qe,xe,ye,ze,
     x                       dt,ue,ve,snnp1ev,ndexev,
     x                       ls_node,locl,batah)
      use machine        , only : kind_evod
      use resol_def      , only : jcap,levs
      use layout1        , only : len_trie_ls,ls_dim
      use coordinate_def , only : am_slg,bm_slg,d_slg_m,sv_slg,tor_slg
      implicit none
      real(kind=kind_evod) de(len_trie_ls,2,levs),te(len_trie_ls,2,levs)
      real(kind=kind_evod) xe(len_trie_ls,2,levs),ye(len_trie_ls,2,levs)
      real(kind=kind_evod) ue(len_trie_ls,2,levs),ve(len_trie_ls,2,levs)
      real(kind=kind_evod) qe(len_trie_ls,2),    ze(len_trie_ls,2)
      real(kind=kind_evod) dt,batah,batah_dt2,batah_dt2_minus
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      integer               ndexev(len_trie_ls)
      integer              k
      integer              ls_node(ls_dim,3)
!
      integer              indev,indev1,indev2,l,locl,n
      integer              indlsev,jbasev
!     integer              indlsod,jbasod
!     include 'function_indlsev'
      indlsev(n,l) = jbasev + (n-l)/2 + 1
!
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons1     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      batah_dt2=0.5*batah*dt
      batah_dt2_minus=-batah_dt2
           l = ls_node(locl,1)
      jbasev = ls_node(locl,2)
      indev1 = indlsev(l,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indev2 = indlsev(jcap+1,l)
      else
         indev2 = indlsev(jcap  ,l)
      endif
c$$$  do k=1,levs
c$$$      do indev = indev1 , indev2
c$$$        ve(indev,1,k)=cons0     !constant
c$$$        ve(indev,2,k)=cons0     !constant
c$$$      enddo
c$$$    do j=1,levs
c$$$      do indev = indev1 , indev2
c$$$        ve(indev,1,k)=ve(indev,1,k)+am_slg(k,j)*ye(indev,1,j)
c$$$        ve(indev,2,k)=ve(indev,2,k)+am_slg(k,j)*ye(indev,2,j)
c$$$      enddo
c$$$    enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ye(indev1,1,1), len_trie_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               ve(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ye(indev1,2,1), len_trie_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               ve(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ye(indev1,1,1), len_trie_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               ve(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ye(indev1,2,1), len_trie_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               ve(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
      do k=1,levs
          do indev = indev1 , indev2
            ue(indev,1,k)=xe(indev,1,k) + batah_dt2 *
     &         snnp1ev(indev)*(ve(indev,1,k)+tor_slg(k)*ze(indev,1))

            ue(indev,2,k)=xe(indev,2,k) + batah_dt2 *
     &         snnp1ev(indev)*(ve(indev,2,k)+tor_slg(k)*ze(indev,2))
          enddo
      enddo
c$$$  do k=1,levs
c$$$      do indev = indev1 , indev2
c$$$        ve(indev,1,k)=cons0     !constant
c$$$        ve(indev,2,k)=cons0     !constant
c$$$      enddo
c$$$    do j=1,levs
c$$$      do indev = indev1 , indev2
c$$$        ve(indev,1,k)=
c$$$ x      ve(indev,1,k) +d_slg_m(k,j,ndexev(indev)+1)*ue(indev,1,j)
c$$$        ve(indev,2,k)=
c$$$ x      ve(indev,2,k) +d_slg_m(k,j,ndexev(indev)+1)*ue(indev,2,j)
c$$$      enddo
c$$$    enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indev = indev1 , indev2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,2,1), len_trie_ls*2)
         enddo
      else !------------------------------------------------------------
         do indev = indev1 , indev2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,2,1), len_trie_ls*2)
         enddo
      endif !-----------------------------------------------------------
c$$$  do j=1,levs
c$$$    do indev = indev1 , indev2
c$$$      qe(indev,1)=qe(indev,1) -sv_slg(j)*batah_dt2*ve(indev,1,j)
c$$$      qe(indev,2)=qe(indev,2) -sv_slg(j)*batah_dt2*ve(indev,2,j)
c$$$    enddo
c$$$  enddo
         if ( kind_evod .eq. 8 ) then !------------------------------------
            call dgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, batah_dt2_minus,
     &                  ve(indev1,1,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qe(indev1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, batah_dt2_minus,
     &                  ve(indev1,2,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qe(indev1,2), len_trie_ls*2)
         else !------------------------------------------------------------
            call sgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, batah_dt2_minus,
     &                  ve(indev1,1,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qe(indev1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, batah_dt2_minus,
     &                  ve(indev1,2,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qe(indev1,2), len_trie_ls*2)
         endif !-----------------------------------------------------------
        do indev = indev1 , indev2
          qe(indev,1)=qe(indev,1)+ze(indev,1)
          qe(indev,2)=qe(indev,2)+ze(indev,2)
        enddo
c$$$  do k=1,levs
c$$$    do j=1,levs
c$$$      do indev = indev1 , indev2
c$$$    te(indev,1,k)=te(indev,1,k)-batah_dt2*bm_slg(k,j)*ve(indev,1,j)
c$$$    te(indev,2,k)=te(indev,2,k)-batah_dt2*bm_slg(k,j)*ve(indev,2,j)
c$$$      enddo
c$$$    enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, batah_dt2_minus,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               te(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, batah_dt2_minus,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               te(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, batah_dt2_minus,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               te(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, batah_dt2_minus,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               te(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
      do k=1,levs
          do indev = indev1 , indev2
            te(indev,1,k)=te(indev,1,k)+ye(indev,1,k)
            te(indev,2,k)=te(indev,2,k)+ye(indev,2,k)
            de(indev,1,k)=de(indev,1,k)+ve(indev,1,k)
            de(indev,2,k)=de(indev,2,k)+ve(indev,2,k)
          enddo
      enddo
      return
      end
      subroutine impadjo_slg(do,to,qo,xo,yo,zo,
     x                       dt,uo,vo,snnp1od,ndexod,
     x                       ls_node,locl,batah)
      use machine        , only : kind_evod
      use resol_def      , only : jcap,levs
      use layout1        , only : len_trio_ls,ls_dim
      use coordinate_def , only : am_slg,bm_slg,d_slg_m,sv_slg,tor_slg
      implicit none
      real(kind=kind_evod) do(len_trio_ls,2,levs),to(len_trio_ls,2,levs)
      real(kind=kind_evod) xo(len_trio_ls,2,levs),yo(len_trio_ls,2,levs)
      real(kind=kind_evod) uo(len_trio_ls,2,levs),vo(len_trio_ls,2,levs)
      real(kind=kind_evod) qo(len_trio_ls,2),    zo(len_trio_ls,2)
      real(kind=kind_evod) dt,batah,batah_dt2,batah_dt2_minus
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer               ndexod(len_trio_ls)
      integer              k
      integer              ls_node(ls_dim,3)
!
      integer              indod,indod1,indod2,l,locl,n
!     integer              indlsev,jbasev
      integer              indlsod,jbasod
!     include 'function_indlsod'
      indlsod(n,l) = jbasod + (n-l)/2 + 1
!
      real(kind=kind_evod) cons0     !constant
      real(kind=kind_evod) cons1     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      batah_dt2=0.5*batah*dt
      batah_dt2_minus=-batah_dt2
           l = ls_node(locl,1)
      jbasod = ls_node(locl,3)
      indod1 = indlsod(l+1,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indod2 = indlsod(jcap  ,l)
      else
         indod2 = indlsod(jcap+1,l)
      endif
c$$$  do k=1,levs
c$$$      do indod = indod1 , indod2
c$$$        vo(indod,1,k)=cons0     !constant
c$$$        vo(indod,2,k)=cons0     !constant
c$$$      enddo
c$$$    do j=1,levs
c$$$      do indod = indod1 , indod2
c$$$        vo(indod,1,k)=vo(indod,1,k)+am_slg(k,j)*yo(indod,1,j)
c$$$        vo(indod,2,k)=vo(indod,2,k)+am_slg(k,j)*yo(indod,2,j)
c$$$      enddo
c$$$    enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               yo(indod1,1,1), len_trio_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               vo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               yo(indod1,2,1), len_trio_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               vo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               yo(indod1,1,1), len_trio_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               vo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               yo(indod1,2,1), len_trio_ls*2,
     &               am_slg(1,1), levs, cons0,
     &               vo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
      do k=1,levs
          do indod = indod1 , indod2
            uo(indod,1,k)=xo(indod,1,k) + batah_dt2 *
     &         snnp1od(indod)*(vo(indod,1,k)+tor_slg(k)*zo(indod,1))
            uo(indod,2,k)=xo(indod,2,k) + batah_dt2 *
     &         snnp1od(indod)*(vo(indod,2,k)+tor_slg(k)*zo(indod,2))
          enddo
      enddo
c$$$  do k=1,levs
c$$$      do indod = indod1 , indod2
c$$$        vo(indod,1,k)=cons0     !constant
c$$$        vo(indod,2,k)=cons0     !constant
c$$$      enddo
c$$$    do j=1,levs
c$$$      do indod = indod1 , indod2
c$$$        vo(indod,1,k)=
c$$$ x      vo(indod,1,k) +d_slg_m(k,j,ndexod(indod)+1)*uo(indod,1,j)
c$$$        vo(indod,2,k)=
c$$$ x      vo(indod,2,k) +d_slg_m(k,j,ndexod(indod)+1)*uo(indod,2,j)
c$$$      enddo
c$$$    enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indod = indod1 , indod2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,2,1), len_trio_ls*2)
         enddo
      else !------------------------------------------------------------
         do indod = indod1 , indod2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,2,1), len_trio_ls*2)
         enddo
      endif !-----------------------------------------------------------
c$$$  do j=1,levs
c$$$    do indod = indod1 , indod2
c$$$      qo(indod,1)=qo(indod,1)-sv_slg(j)*batah_dt2*vo(indod,1,j)
c$$$      qo(indod,2)=qo(indod,2)-sv_slg(j)*batah_dt2*vo(indod,2,j)
c$$$    enddo
c$$$  enddo
         if ( kind_evod .eq. 8 ) then !------------------------------------
            call dgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, batah_dt2_minus,
     &                  vo(indod1,1,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qo(indod1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, batah_dt2_minus,
     &                  vo(indod1,2,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qo(indod1,2), len_trio_ls*2)
         else !------------------------------------------------------------
            call sgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, batah_dt2_minus,
     &                  vo(indod1,1,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qo(indod1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, batah_dt2_minus,
     &                  vo(indod1,2,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons1,
     &                  qo(indod1,2), len_trio_ls*2)
         endif !-----------------------------------------------------------
        do indod = indod1 , indod2
          qo(indod,1)=qo(indod,1)+zo(indod,1)
          qo(indod,2)=qo(indod,2)+zo(indod,2)
        enddo
c$$$  do k=1,levs
c$$$    do j=1,levs
c$$$      do indod = indod1 , indod2
c$$$    to(indod,1,k)=to(indod,1,k)-batah_dt2*bm_slg(k,j)*vo(indod,1,j)
c$$$    to(indod,2,k)=to(indod,2,k)-batah_dt2*bm_slg(k,j)*vo(indod,2,j)
c$$$      enddo
c$$$    enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, batah_dt2_minus,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               to(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, batah_dt2_minus,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               to(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, batah_dt2_minus,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               to(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, batah_dt2_minus,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons1,
     &               to(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
      do k=1,levs
          do indod = indod1 , indod2
            to(indod,1,k)=to(indod,1,k)+yo(indod,1,k)
            to(indod,2,k)=to(indod,2,k)+yo(indod,2,k)
            do(indod,1,k)=do(indod,1,k)+vo(indod,1,k)
            do(indod,2,k)=do(indod,2,k)+vo(indod,2,k)
          enddo
      enddo
      return
      end
