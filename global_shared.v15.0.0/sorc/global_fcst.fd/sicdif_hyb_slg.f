      subroutine sicdife_hyb_slg(xe,ye,ze,dt_half,ue,ve,
     x                       ls_node,snnp1ev,ndexev,locl,batah)
      use machine        , only : kind_evod
      use resol_def      , only : jcap,levs
      use layout1        , only : len_trie_ls,ls_dim
!sela use akbk_hyb_def
      use coordinate_def , only : am_slg,bm_slg,d_slg_m,sv_slg,tor_slg
      implicit none
      real(kind=kind_evod) xe(len_trie_ls,2,levs),ye(len_trie_ls,2,levs)
      real(kind=kind_evod) ue(len_trie_ls,2,levs),ve(len_trie_ls,2,levs)
      real(kind=kind_evod) ze(len_trie_ls,2)
      real(kind=kind_evod) dt_half,batah
      integer              ls_node(ls_dim,3)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      integer               ndexev(len_trie_ls)
      integer              i,indev,indev1,indev2,j,k,l,locl,n
      integer              indlsev,jbasev
      include 'function_indlsev'
      real(kind=kind_evod) cons0,cons1,cons2     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant
           l = ls_node(locl,1)
      jbasev = ls_node(locl,2)
      indev1 = indlsev(l,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indev2 = indlsev(jcap+1,l)
      else
         indev2 = indlsev(jcap  ,l)
      endif
c$$$  do j=1,levs
c$$$        do indev = indev1 , indev2
c$$$           ve(indev,1,j) =0.
c$$$           ve(indev,2,j) =0.
c$$$        enddo
c$$$  enddo
c$$$  do j=1,levs
c$$$     do k=1,levs,2
c$$$        do indev = indev1 , indev2
c$$$           ve(indev,1,j) =
c$$$ x         ve(indev,1,j) + ye(indev,1,k  )*am_slg(j,k  )
c$$$ x                       + ye(indev,1,k+1)*am_slg(j,k+1)
c$$$           ve(indev,2,j) =
c$$$ x         ve(indev,2,j) + ye(indev,2,k  )*am_slg(j,k  )
c$$$ x                       + ye(indev,2,k+1)*am_slg(j,k+1)
c$$$        enddo
c$$$     enddo
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
      do 17 j=1,levs
       do indev = indev1 , indev2
         ve(indev,1,j)=ve(indev,1,j)+tor_slg(j)*ze(indev,1)
         ve(indev,1,j)=ve(indev,1,j)*snnp1ev(indev)*dt_half*batah
         ue(indev,1,j)=ve(indev,1,j)+xe(indev,1,j)

         ve(indev,2,j)=ve(indev,2,j)+tor_slg(j)*ze(indev,2)
         ve(indev,2,j)=ve(indev,2,j)*snnp1ev(indev)*dt_half*batah
         ue(indev,2,j)=ve(indev,2,j)+xe(indev,2,j)
            enddo
   17 continue
c$$$  do j=1,levs
c$$$        do indev = indev1 , indev2
c$$$           xe(indev,1,j) =0.
c$$$           xe(indev,2,j) =0.
c$$$        enddo
c$$$  enddo
c$$$  do j=1,levs
c$$$     do k=1,levs,2
c$$$        do indev = indev1 , indev2
c$$$           xe(indev,1,j) =  xe(indev,1,j) +
c$$$ x         ue(indev,1,k  ) * d_slg_m(j,k  ,ndexev(indev)+1)+
c$$$ x         ue(indev,1,k+1) * d_slg_m(j,k+1,ndexev(indev)+1)
c$$$
c$$$           xe(indev,2,j) =  xe(indev,2,j) +
c$$$ x         ue(indev,2,k  ) * d_slg_m(j,k  ,ndexev(indev)+1)+
c$$$ x         ue(indev,2,k+1) * d_slg_m(j,k+1,ndexev(indev)+1)
c$$$        enddo
c$$$     enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indev = indev1 , indev2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  xe(indev,1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  xe(indev,2,1), len_trie_ls*2)
         enddo
      else !------------------------------------------------------------
         do indev = indev1 , indev2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  xe(indev,1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  d_slg_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  xe(indev,2,1), len_trie_ls*2)
         enddo
      endif !-----------------------------------------------------------
c$$$     do indev = indev1 , indev2
c$$$        ue(indev,1,1) = cons0     !constant
c$$$        ue(indev,2,1) = cons0     !constant
c$$$     enddo
c$$$  do k=1,levs
c$$$     do indev = indev1 , indev2
c$$$        ue(indev,1,1) =
c$$$ x      ue(indev,1,1) + dt_half*xe(indev,1,k)*sv_slg(k)
c$$$        ue(indev,2,1) =
c$$$ x      ue(indev,2,1) + dt_half*xe(indev,2,k)*sv_slg(k)
c$$$     enddo
c$$$  enddo
         if ( kind_evod .eq. 8 ) then !------------------------------------
            call dgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt_half,
     &                  xe(indev1,1,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  ue(indev1,1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt_half,
     &                  xe(indev1,2,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  ue(indev1,2,1), len_trie_ls*2)
         else !------------------------------------------------------------
            call sgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt_half,
     &                  xe(indev1,1,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  ue(indev1,1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt_half,
     &                  xe(indev1,2,1), len_trie_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  ue(indev1,2,1), len_trie_ls*2)
         endif !-----------------------------------------------------------
         do indev = indev1 , indev2
            ze(indev,1) = ze(indev,1)-batah*ue(indev,1,1)
            ze(indev,2) = ze(indev,2)-batah*ue(indev,2,1)
         enddo
c$$$  do j=1,levs
c$$$        do indev = indev1 , indev2
c$$$           ue(indev,1,j) =0.
c$$$           ue(indev,2,j) =0.
c$$$        enddo
c$$$  enddo
c$$$  do j=1,levs
c$$$     do k=1,levs,2
c$$$        do indev = indev1 , indev2
c$$$           ue(indev,1,j) =
c$$$ x         ue(indev,1,j) + xe(indev,1,k  )*bm_slg(j,k  )
c$$$ x                       + xe(indev,1,k+1)*bm_slg(j,k+1)
c$$$           ue(indev,2,j) =
c$$$ x         ue(indev,2,j) + xe(indev,2,k  )*bm_slg(j,k  )
c$$$ x                       + xe(indev,2,k+1)*bm_slg(j,k+1)
c$$$        enddo
c$$$     enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               xe(indev1,1,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               xe(indev1,2,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               xe(indev1,1,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               xe(indev1,2,1), len_trie_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
      do j=1,levs
            do indev = indev1 , indev2
               ye(indev,1,j)=ye(indev,1,j)-batah*dt_half*ue(indev,1,j)
               ye(indev,2,j)=ye(indev,2,j)-batah*dt_half*ue(indev,2,j)
            enddo
      enddo
      return
      end
      subroutine sicdifo_hyb_slg(xo,yo,zo,dt_half,uo,vo,
     x                       ls_node,snnp1od,ndexod,locl,batah)
      use machine        , only : kind_evod
      use resol_def      , only : jcap,levs
      use layout1        , only : len_trio_ls,ls_dim
!sela use akbk_hyb_def
      use coordinate_def , only : am_slg,bm_slg,d_slg_m,sv_slg,tor_slg
      implicit none
      real(kind=kind_evod) xo(len_trio_ls,2,levs),yo(len_trio_ls,2,levs)
      real(kind=kind_evod) uo(len_trio_ls,2,levs),vo(len_trio_ls,2,levs)
      real(kind=kind_evod) zo(len_trio_ls,2)
      real(kind=kind_evod) dt_half,batah
      integer              ls_node(ls_dim,3)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer               ndexod(len_trio_ls)
      integer              i,indod,indod1,indod2,j,k,l,locl,n
      integer              indlsod,jbasod
      include 'function_indlsod'
      real(kind=kind_evod) cons0,cons1,cons2     !constant
      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant
           l = ls_node(locl,1)
      jbasod = ls_node(locl,3)
      indod1 = indlsod(l+1,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indod2 = indlsod(jcap  ,l)
      else
         indod2 = indlsod(jcap+1,l)
      endif
c$$$  do j=1,levs
c$$$        do indod = indod1 , indod2
c$$$           vo(indod,1,j) = 0.
c$$$           vo(indod,2,j) = 0.
c$$$        enddo
c$$$  enddo
c$$$  do j=1,levs
c$$$     do k=1,levs,2
c$$$        do indod = indod1 , indod2
c$$$           vo(indod,1,j) =
c$$$ x         vo(indod,1,j) + yo(indod,1,k  )*am_slg(j,k  )
c$$$ x                       + yo(indod,1,k+1)*am_slg(j,k+1)
c$$$           vo(indod,2,j) =
c$$$ x         vo(indod,2,j) + yo(indod,2,k  )*am_slg(j,k  )
c$$$ x                       + yo(indod,2,k+1)*am_slg(j,k+1)
c$$$        enddo
c$$$     enddo
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
      do 17 j=1,levs
       do indod = indod1, indod2
        vo(indod,1,j)=vo(indod,1,j)+tor_slg(j)*zo(indod,1)
        vo(indod,1,j)=vo(indod,1,j)*snnp1od(indod)*dt_half*batah
        uo(indod,1,j)=vo(indod,1,j)+xo(indod,1,j)

        vo(indod,2,j)=vo(indod,2,j)+tor_slg(j)*zo(indod,2)
        vo(indod,2,j)=vo(indod,2,j)*snnp1od(indod)*dt_half*batah
        uo(indod,2,j)=vo(indod,2,j)+xo(indod,2,j)
            enddo
   17 continue
c$$$  do j=1,levs
c$$$        do indod = indod1 , indod2
c$$$           xo(indod,1,j) = 0.
c$$$           xo(indod,2,j) = 0.
c$$$        enddo
c$$$  enddo
c$$$  do j=1,levs
c$$$     do k=1,levs,2
c$$$        do indod = indod1 , indod2
c$$$           xo(indod,1,j) =  xo(indod,1,j) +
c$$$ x         uo(indod,1,k  ) * d_slg_m(j,k  ,ndexod(indod)+1)+
c$$$ x         uo(indod,1,k+1) * d_slg_m(j,k+1,ndexod(indod)+1)
c$$$
c$$$           xo(indod,2,j) =  xo(indod,2,j) +
c$$$ x         uo(indod,2,k  ) * d_slg_m(j,k  ,ndexod(indod)+1)+
c$$$ x         uo(indod,2,k+1) * d_slg_m(j,k+1,ndexod(indod)+1)
c$$$        enddo
c$$$     enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indod = indod1 , indod2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  xo(indod,1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  xo(indod,2,1), len_trio_ls*2)
         enddo
      else !------------------------------------------------------------
         do indod = indod1 , indod2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  xo(indod,1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  d_slg_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  xo(indod,2,1), len_trio_ls*2)
         enddo
      endif !-----------------------------------------------------------
c$$$     do indod = indod1 , indod2
c$$$        uo(indod,1,1) = cons0     !constant
c$$$        uo(indod,2,1) = cons0     !constant
c$$$     enddo
c$$$  do k=1,levs
c$$$     do indod = indod1 , indod2
c$$$        uo(indod,1,1) =
c$$$ x      uo(indod,1,1) + dt_half*xo(indod,1,k)*sv_slg(k)
c$$$        uo(indod,2,1) =
c$$$ x      uo(indod,2,1) + dt_half*xo(indod,2,k)*sv_slg(k)
c$$$     enddo
c$$$  enddo
         if ( kind_evod .eq. 8 ) then !------------------------------------
            call dgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt_half,
     &                  xo(indod1,1,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  uo(indod1,1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt_half,
     &                  xo(indod1,2,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  uo(indod1,2,1), len_trio_ls*2)
         else !------------------------------------------------------------
            call sgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt_half,
     &                  xo(indod1,1,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  uo(indod1,1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt_half,
     &                  xo(indod1,2,1), len_trio_ls*2,
     &                  sv_slg(1), 1, cons0,
     &                  uo(indod1,2,1), len_trio_ls*2)
         endif !-----------------------------------------------------------
         do indod = indod1, indod2
          zo(indod,1) = zo(indod,1)-batah*uo(indod,1,1)
          zo(indod,2) = zo(indod,2)-batah*uo(indod,2,1)
         enddo
c$$$  do j=1,levs
c$$$        do indod = indod1 , indod2
c$$$           uo(indod,1,j) = 0.
c$$$           uo(indod,2,j) = 0.
c$$$        enddo
c$$$  enddo
c$$$  do j=1,levs
c$$$     do k=1,levs,2
c$$$        do indod = indod1 , indod2
c$$$           uo(indod,1,j) =
c$$$ x         uo(indod,1,j) + xo(indod,1,k  )*bm_slg(j,k  )
c$$$ x                       + xo(indod,1,k+1)*bm_slg(j,k+1)
c$$$           uo(indod,2,j) =
c$$$ x         uo(indod,2,j) + xo(indod,2,k  )*bm_slg(j,k  )
c$$$ x                       + xo(indod,2,k+1)*bm_slg(j,k+1)
c$$$        enddo
c$$$     enddo
c$$$  enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               xo(indod1,1,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               xo(indod1,2,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               xo(indod1,1,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               xo(indod1,2,1), len_trio_ls*2,
     &               bm_slg(1,1), levs, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
      do j=1,levs
            do indod = indod1, indod2
               yo(indod,1,j) = yo(indod,1,j)-batah*dt_half*uo(indod,1,j)
               yo(indod,2,j) = yo(indod,2,j)-batah*dt_half*uo(indod,2,j)
            enddo
      enddo
      return
      end
