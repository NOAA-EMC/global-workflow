      subroutine sicdife_hyb_gc(de,te,qe,xe,ye,ze,de_n,te_n,qe_n,
     x                                              dti,ue,ve,
     x                       ls_node,snnp1ev,ndexev,locl)

      use resol_def
      use layout1
      use coordinate_def
      implicit none

      real(kind=kind_evod) de(len_trie_ls,2,levs),te(len_trie_ls,2,levs)
      real(kind=kind_evod) xe(len_trie_ls,2,levs),ye(len_trie_ls,2,levs)
      real(kind=kind_evod) ue(len_trie_ls,2,levs),ve(len_trie_ls,2,levs)
      real(kind=kind_evod) qe(len_trie_ls,2),    ze(len_trie_ls,2)
      real(kind=kind_evod) de_n(len_trie_ls,2,levs)
      real(kind=kind_evod) te_n(len_trie_ls,2,levs)
      real(kind=kind_evod) qe_n(len_trie_ls,2)
      real(kind=kind_evod) dti,dt,repsp,repsm
      integer              ls_node(ls_dim,3)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      integer               ndexev(len_trie_ls)
      integer              i,indev,indev1,indev2,j,k,l,locl,n
      real(kind=kind_evod) qdtze(len_trie_ls,2), 
     . elne(len_trie_ls,2,levs)
      real(kind=kind_evod) u1, u2
      integer              indlsev,jbasev
      include 'function_indlsev'
      real(kind=kind_evod) cons0,cons1,cons2     !constant

!     print *,' enter sicdife_hyb_gc_fd '		! hmhj

      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant

! hmhj forward-weighted semi-implicit
      dt=(cons1+eps_si)*dti
      repsp=cons2/(cons1+eps_si)
      repsm=(cons1-eps_si)/(cons1+eps_si)

           l = ls_node(locl,1)
      jbasev = ls_node(locl,2)
      indev1 = indlsev(l,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indev2 = indlsev(jcap+1,l)
      else
         indev2 = indlsev(jcap  ,l)
      endif
      do j=1,levs
!        do k=1,levs,2
         do k=1,levs
            do indev = indev1 , indev2
               ye(indev,1,j) =
     x         ye(indev,1,j) + de_n(indev,1,k  )*bmhyb(j,k  )
!    x                       + de_n(indev,1,k+1)*bmhyb(j,k+1)
               ye(indev,2,j) =
     x         ye(indev,2,j) + de_n(indev,2,k  )*bmhyb(j,k  )
!    x                       + de_n(indev,2,k+1)*bmhyb(j,k+1)
            enddo
         enddo
      enddo
 
      do k=1,levs
         do indev = indev1 , indev2
            ze(indev,1) =
     x      ze(indev,1) + de_n(indev,1,k)*svhyb(k)
            ze(indev,2) =
     x      ze(indev,2) + de_n(indev,2,k)*svhyb(k)
         enddo
      enddo
 
         do indev = indev1 , indev2
            qdtze(indev,1)   =  qe(indev,1)-qe_n(indev,1)
     x                     + dt*ze(indev,1)
 
            qdtze(indev,2)   =  qe(indev,2)-qe_n(indev,2)
     x                   +   dt*ze(indev,2)
         enddo
 
      do k=1,levs
            do indev = indev1 , indev2
               elne(indev,1,k) = te(indev,1,k)-te_n(indev,1,k)
     x                    + dt*  ye(indev,1,k)
 
               elne(indev,2,k) = te(indev,2,k)-te_n(indev,2,k)
     x                    + dt*  ye(indev,2,k)
            enddo
      enddo
c$$$      do j=1,levs
c$$$            do indev = indev1 , indev2
c$$$               ve(indev,1,j) = cons0     !constant
c$$$               ve(indev,2,j) = cons0     !constant
c$$$            enddo
c$$$         do k=1,levs,2
c$$$            do indev = indev1 , indev2
c$$$               ve(indev,1,j) =
c$$$     x         ve(indev,1,j) + elne(indev,1,k  )*amhyb(j,k  )
c$$$     x                       + elne(indev,1,k+1)*amhyb(j,k+1)
c$$$               ve(indev,2,j) =
c$$$     x         ve(indev,2,j) + elne(indev,2,k  )*amhyb(j,k  )
c$$$     x                       + elne(indev,2,k+1)*amhyb(j,k+1)
c$$$            enddo
c$$$         enddo
c$$$      enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,1,1), len_trie_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               ve(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,2,1), len_trie_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               ve(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,1,1), len_trie_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               ve(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               elne(indev1,2,1), len_trie_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               ve(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
      do 17 j=1,levs
            do indev = indev1 , indev2
               ve(indev,1,j) =
     x         ve(indev,1,j) + tor_hyb(j)*qdtze(indev,1)
 
               ve(indev,1,j) =
     x         ve(indev,1,j) *      snnp1ev(indev)
 
               ve(indev,1,j) =
     x         ve(indev,1,j) +           xe(indev,1,j)
 
               ue(indev,1,j) =           de(indev,1,j)
     x                       +           ve(indev,1,j)*dt
 
               ve(indev,2,j) =
     x         ve(indev,2,j) + tor_hyb(j)*qdtze(indev,2)
 
               ve(indev,2,j) =
     x         ve(indev,2,j) *      snnp1ev(indev)
 
               ve(indev,2,j) =
     x         ve(indev,2,j) +           xe(indev,2,j)
 
               ue(indev,2,j) =           de(indev,2,j)
     x                       +           ve(indev,2,j)*dt
            enddo
   17 continue
c$$$      do j=1,levs
c$$$            do indev = indev1 , indev2
c$$$               ve(indev,1,j) = cons0     !constant
c$$$               ve(indev,2,j) = cons0     !constant
c$$$            enddo
c$$$         do k=1,levs,2
c$$$            do indev = indev1 , indev2
c$$$               ve(indev,1,j) =
c$$$     x         ve(indev,1,j) +               ue(indev,1,k  )
c$$$     x                       * d_hyb_m(j,k  ,ndexev(indev)+1)
c$$$     x                       +               ue(indev,1,k+1)
c$$$     x                       * d_hyb_m(j,k+1,ndexev(indev)+1)
c$$$               ve(indev,2,j) =
c$$$     x         ve(indev,2,j) +               ue(indev,2,k  )
c$$$     x                       * d_hyb_m(j,k  ,ndexev(indev)+1)
c$$$     x                       +               ue(indev,2,k+1)
c$$$     x                       * d_hyb_m(j,k+1,ndexev(indev)+1)
c$$$            enddo
c$$$         enddo
c$$$      enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indev = indev1 , indev2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  d_hyb_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  d_hyb_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,2,1), len_trie_ls*2)
         enddo
      else !------------------------------------------------------------
         do indev = indev1 , indev2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,1,1), len_trie_ls*2,
     &                  d_hyb_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  ue(indev,2,1), len_trie_ls*2,
     &                  d_hyb_m(1,1,ndexev(indev)+1), levs, cons0,
     &                  ve(indev,2,1), len_trie_ls*2)
         enddo
      endif !-----------------------------------------------------------
c$$$         do indev = indev1 , indev2
c$$$            ue(indev,1,1) = cons0     !constant
c$$$            ue(indev,2,1) = cons0     !constant
c$$$         enddo
c$$$      do k=1,levs
c$$$         do indev = indev1 , indev2
c$$$            ue(indev,1,1) =
c$$$     x      ue(indev,1,1) + dt*ve(indev,1,k)*svhyb(k)
c$$$            ue(indev,2,1) =
c$$$     x      ue(indev,2,1) + dt*ve(indev,2,k)*svhyb(k)
c$$$         enddo
c$$$      enddo
         if ( kind_evod .eq. 8 ) then !------------------------------------
            call dgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt,
     &                  ve(indev1,1,1), len_trie_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  ue(indev1,1,1), len_trie_ls*2)
            call dgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt,
     &                  ve(indev1,2,1), len_trie_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  ue(indev1,2,1), len_trie_ls*2)
         else !------------------------------------------------------------
            call sgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt,
     &                  ve(indev1,1,1), len_trie_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  ue(indev1,1,1), len_trie_ls*2)
            call sgemm ('n', 't',
     &                  indev2-indev1+1, 1, levs, dt,
     &                  ve(indev1,2,1), len_trie_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  ue(indev1,2,1), len_trie_ls*2)
         endif !-----------------------------------------------------------
         do indev = indev1 , indev2
            qdtze(indev,1)   = qdtze(indev,1) +
     x       qe_n(indev,1)   -    ue(indev,1,1)
! hmhj 
               ze(indev,1)   = repsp*qdtze(indev,1)  !constant
     x                       - repsm*   qe(indev,1)
 
            qdtze(indev,2)   = qdtze(indev,2) +
     x       qe_n(indev,2)   -    ue(indev,2,1)
! hmhj 
               ze(indev,2)   = repsp*qdtze(indev,2)  !constant
     x                       - repsm*   qe(indev,2)
         enddo
c$$$      do j=1,levs
c$$$            do indev = indev1 , indev2
c$$$               ue(indev,1,j) = cons0     !constant
c$$$               ue(indev,2,j) = cons0     !constant
c$$$            enddo
c$$$         do k=1,levs,2
c$$$            do indev = indev1 , indev2
c$$$               ue(indev,1,j) =
c$$$     x         ue(indev,1,j) + ve(indev,1,k  )*bmhyb(j,k  )
c$$$     x                       + ve(indev,1,k+1)*bmhyb(j,k+1)
c$$$               ue(indev,2,j) =
c$$$     x         ue(indev,2,j) + ve(indev,2,k  )*bmhyb(j,k  )
c$$$     x                       + ve(indev,2,k+1)*bmhyb(j,k+1)
c$$$            enddo
c$$$         enddo
c$$$      enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call dgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,1,1), len_trie_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               ue(indev1,1,1), len_trie_ls*2)
         call sgemm ('n', 't',
     &               indev2-indev1+1, levs, levs, cons1,
     &               ve(indev1,2,1), len_trie_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               ue(indev1,2,1), len_trie_ls*2)
      endif !-----------------------------------------------------------
      do j=1,levs
            do indev = indev1 , indev2
               u1 = elne(indev,1,j) - dt * ue(indev,1,j)
     &           +  te_n(indev,1,j)
! hmhj 
               ye(indev,1,j) = repsp*u1-repsm*te(indev,1,j) !constant
 
               xe(indev,1,j) = repsp*   ve(indev,1,j) !constant
     x                       - repsm*   de(indev,1,j)
 
               u2 = elne(indev,2,j) - dt * ue(indev,2,j)
     &           +  te_n(indev,2,j)
 
               ye(indev,2,j) = repsp*u2-repsm*te(indev,2,j) !constant
 
               xe(indev,2,j) = repsp*   ve(indev,2,j) !constant
     x                       - repsm*   de(indev,2,j)
            enddo
      enddo

!     print *,' leave sicdife_hyb_gc_fd '		! hmhj

      return
      end
      subroutine sicdifo_hyb_gc(do,to,qo,xo,yo,zo,do_n,to_n,qo_n,
     x                                              dti,uo,vo,
     x                       ls_node,snnp1od,ndexod,locl)

      use resol_def
      use layout1
      use coordinate_def
      implicit none
      real(kind=kind_evod) do(len_trio_ls,2,levs),to(len_trio_ls,2,levs)
      real(kind=kind_evod) xo(len_trio_ls,2,levs),yo(len_trio_ls,2,levs)
      real(kind=kind_evod) uo(len_trio_ls,2,levs),vo(len_trio_ls,2,levs)
      real(kind=kind_evod) qo(len_trio_ls,2),    zo(len_trio_ls,2)
      real(kind=kind_evod) do_n(len_trio_ls,2,levs)
      real(kind=kind_evod) to_n(len_trio_ls,2,levs)
      real(kind=kind_evod) qo_n(len_trio_ls,2)
      real(kind=kind_evod) dti,dt,repsp,repsm
      integer              ls_node(ls_dim,3)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      integer               ndexod(len_trio_ls)
      integer              i,indod,indod1,indod2,j,k,l,locl,n
      real(kind=kind_evod) qdtzo(len_trio_ls,2),
     .  elno(len_trio_ls,2,levs)
      real(kind=kind_evod) u1, u2
      integer              indlsod,jbasod
      include 'function_indlsod'
      real(kind=kind_evod) cons0,cons1,cons2     !constant

!     print *,' enter sicdifo_hyb_gc_fd '		! hmhj

      cons0 = 0.d0     !constant
      cons1 = 1.d0     !constant
      cons2 = 2.d0     !constant

! hmhj forward-weighted semi-implicit
!     eps_si=0.05
      dt=(cons1+eps_si)*dti
      repsp=cons2/(cons1+eps_si)
      repsm=(cons1-eps_si)/(cons1+eps_si)

           l = ls_node(locl,1)
      jbasod = ls_node(locl,3)
      indod1 = indlsod(l+1,l)
      if (mod(l,2).eq.mod(jcap+1,2)) then
         indod2 = indlsod(jcap  ,l)
      else
         indod2 = indlsod(jcap+1,l)
      endif
      do j=1,levs
!        do k=1,levs,2
         do k=1,levs
            do indod = indod1 , indod2
               yo(indod,1,j) =
     x         yo(indod,1,j) + do_n(indod,1,k  )*bmhyb(j,k  )
!    x                       + do_n(indod,1,k+1)*bmhyb(j,k+1)
               yo(indod,2,j) =
     x         yo(indod,2,j) + do_n(indod,2,k  )*bmhyb(j,k  )
!    x                       + do_n(indod,2,k+1)*bmhyb(j,k+1)
            enddo
         enddo
      enddo
 
      do k=1,levs
         do indod = indod1 , indod2
            zo(indod,1) =
     x      zo(indod,1) + do_n(indod,1,k)*svhyb(k)
            zo(indod,2) =
     x      zo(indod,2) + do_n(indod,2,k)*svhyb(k)
         enddo
      enddo
 
         do indod = indod1, indod2
            qdtzo(indod,1)   =  qo(indod,1)-qo_n(indod,1)
     x                   +   dt*zo(indod,1)
 
            qdtzo(indod,2)   =  qo(indod,2)-qo_n(indod,2)
     x                   +   dt*zo(indod,2)
         enddo
 
      do k=1,levs
            do indod = indod1, indod2
               elno(indod,1,k) = to(indod,1,k)-to_n(indod,1,k)
     x                    + dt*  yo(indod,1,k)
 
               elno(indod,2,k) = to(indod,2,k)-to_n(indod,2,k)
     x                    + dt*  yo(indod,2,k)
            enddo
      enddo
c$$$      do j=1,levs
c$$$            do indod = indod1 , indod2
c$$$               vo(indod,1,j) = cons0     !constant
c$$$               vo(indod,2,j) = cons0     !constant
c$$$            enddo
c$$$         do k=1,levs,2
c$$$            do indod = indod1 , indod2
c$$$               vo(indod,1,j) =
c$$$     x         vo(indod,1,j) + elno(indod,1,k  )*amhyb(j,k  )
c$$$     x                       + elno(indod,1,k+1)*amhyb(j,k+1)
c$$$               vo(indod,2,j) =
c$$$     x         vo(indod,2,j) + elno(indod,2,k  )*amhyb(j,k  )
c$$$     x                       + elno(indod,2,k+1)*amhyb(j,k+1)
c$$$            enddo
c$$$         enddo
c$$$      enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,1,1), len_trio_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               vo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,2,1), len_trio_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               vo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,1,1), len_trio_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               vo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               elno(indod1,2,1), len_trio_ls*2,
     &               amhyb(1,1), levs, cons0,
     &               vo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
      do 17 j=1,levs
            do indod = indod1, indod2
               vo(indod,1,j) =
     x         vo(indod,1,j) + tor_hyb(j)*qdtzo(indod,1)
 
               vo(indod,1,j) =
     x         vo(indod,1,j) *      snnp1od(indod)
 
               vo(indod,1,j) =
     x         vo(indod,1,j) +           xo(indod,1,j)
 
               uo(indod,1,j) =           do(indod,1,j)
     x                       +           vo(indod,1,j)*dt
 
               vo(indod,2,j) =
     x         vo(indod,2,j) + tor_hyb(j)*qdtzo(indod,2)
 
               vo(indod,2,j) =
     x         vo(indod,2,j) *      snnp1od(indod)
 
               vo(indod,2,j) =
     x         vo(indod,2,j) +           xo(indod,2,j)
 
               uo(indod,2,j) =           do(indod,2,j)
     x                       +           vo(indod,2,j)*dt
            enddo
   17 continue
c$$$      do j=1,levs
c$$$            do indod = indod1 , indod2
c$$$               vo(indod,1,j) = cons0     !constant
c$$$               vo(indod,2,j) = cons0     !constant
c$$$            enddo
c$$$         do k=1,levs,2
c$$$            do indod = indod1 , indod2
c$$$               vo(indod,1,j) =
c$$$     x         vo(indod,1,j) +               uo(indod,1,k  )
c$$$     x                       * d_hyb_m(j,k  ,ndexod(indod)+1)
c$$$     x                       +               uo(indod,1,k+1)
c$$$     x                       * d_hyb_m(j,k+1,ndexod(indod)+1)
c$$$               vo(indod,2,j) =
c$$$     x         vo(indod,2,j) +               uo(indod,2,k  )
c$$$     x                       * d_hyb_m(j,k  ,ndexod(indod)+1)
c$$$     x                       +               uo(indod,2,k+1)
c$$$     x                       * d_hyb_m(j,k+1,ndexod(indod)+1)
c$$$            enddo
c$$$         enddo
c$$$      enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         do indod = indod1 , indod2
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  d_hyb_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  d_hyb_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,2,1), len_trio_ls*2)
         enddo
      else !------------------------------------------------------------
         do indod = indod1 , indod2
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,1,1), len_trio_ls*2,
     &                  d_hyb_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  1, levs, levs, cons1,
     &                  uo(indod,2,1), len_trio_ls*2,
     &                  d_hyb_m(1,1,ndexod(indod)+1), levs, cons0,
     &                  vo(indod,2,1), len_trio_ls*2)
         enddo
      endif !-----------------------------------------------------------
c$$$         do indod = indod1 , indod2
c$$$            uo(indod,1,1) = cons0     !constant
c$$$            uo(indod,2,1) = cons0     !constant
c$$$         enddo
c$$$      do k=1,levs
c$$$         do indod = indod1 , indod2
c$$$            uo(indod,1,1) =
c$$$     x      uo(indod,1,1) + dt*vo(indod,1,k)*svhyb(k)
c$$$            uo(indod,2,1) =
c$$$     x      uo(indod,2,1) + dt*vo(indod,2,k)*svhyb(k)
c$$$         enddo
c$$$      enddo
         if ( kind_evod .eq. 8 ) then !------------------------------------
            call dgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt,
     &                  vo(indod1,1,1), len_trio_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  uo(indod1,1,1), len_trio_ls*2)
            call dgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt,
     &                  vo(indod1,2,1), len_trio_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  uo(indod1,2,1), len_trio_ls*2)
         else !------------------------------------------------------------
            call sgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt,
     &                  vo(indod1,1,1), len_trio_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  uo(indod1,1,1), len_trio_ls*2)
            call sgemm ('n', 't',
     &                  indod2-indod1+1, 1, levs, dt,
     &                  vo(indod1,2,1), len_trio_ls*2,
     &                  svhyb(1), 1, cons0,
     &                  uo(indod1,2,1), len_trio_ls*2)
         endif !-----------------------------------------------------------
         do indod = indod1, indod2
            qdtzo(indod,1)   = qdtzo(indod,1) +
     x       qo_n(indod,1)   -    uo(indod,1,1)
! hmhj 
               zo(indod,1)   = repsp*qdtzo(indod,1)  !constant
     x                       - repsm*   qo(indod,1)
 
            qdtzo(indod,2)   = qdtzo(indod,2) +
     x       qo_n(indod,2)   -    uo(indod,2,1)
 
               zo(indod,2)   = repsp*qdtzo(indod,2)  !constant
     x                       - repsm*   qo(indod,2)
         enddo
c$$$      do j=1,levs
c$$$            do indod = indod1 , indod2
c$$$               uo(indod,1,j) = cons0     !constant
c$$$               uo(indod,2,j) = cons0     !constant
c$$$            enddo
c$$$         do k=1,levs,2
c$$$            do indod = indod1 , indod2
c$$$               uo(indod,1,j) =
c$$$     x         uo(indod,1,j) + vo(indod,1,k  )*bmhyb(j,k  )
c$$$     x                       + vo(indod,1,k+1)*bmhyb(j,k+1)
c$$$               uo(indod,2,j) =
c$$$     x         uo(indod,2,j) + vo(indod,2,k  )*bmhyb(j,k  )
c$$$     x                       + vo(indod,2,k+1)*bmhyb(j,k+1)
c$$$            enddo
c$$$         enddo
c$$$      enddo
      if ( kind_evod .eq. 8 ) then !------------------------------------
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call dgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      else !------------------------------------------------------------
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,1,1), len_trio_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               uo(indod1,1,1), len_trio_ls*2)
         call sgemm ('n', 't',
     &               indod2-indod1+1, levs, levs, cons1,
     &               vo(indod1,2,1), len_trio_ls*2,
     &               bmhyb(1,1), levs, cons0,
     &               uo(indod1,2,1), len_trio_ls*2)
      endif !-----------------------------------------------------------
      do j=1,levs
            do indod = indod1, indod2
               u1 = elno(indod,1,j) - dt * uo(indod,1,j)
     &           +  to_n(indod,1,j)
! hmhj 
               yo(indod,1,j) = repsp*u1-repsm*to(indod,1,j) !constant
 
               xo(indod,1,j) = repsp*   vo(indod,1,j) !constant
     x                       - repsm*   do(indod,1,j)
 
               u2 = elno(indod,2,j) - dt * uo(indod,2,j)
     &           +  to_n(indod,2,j)
 
               yo(indod,2,j) = repsp*u2-repsm*to(indod,2,j) !constant
 
               xo(indod,2,j) = repsp*   vo(indod,2,j) !constant
     x                       - repsm*   do(indod,2,j)
            enddo
      enddo

!     print *,' leave sicdifo_hyb_gc_fd '		! hmhj

      return
      end
