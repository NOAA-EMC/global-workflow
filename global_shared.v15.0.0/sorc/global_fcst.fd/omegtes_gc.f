      subroutine omegtes_gc(njeff,nsize_ar,rcl,
     &                   qg,dphi,dlam,thg,thgx,thgy,dg,ug,vg,vvel)           
 
!
! hmhj : this is modified hybrid by finite difference from henry juang
!       thg can be t or h
!
      use machine , only : kind_phys
 
      use resol_def
      use coordinate_def
      use physcons, rd => con_rd, cp => con_cp
      implicit none
 
      integer njeff,nsize_ar
      integer i,k
 
      real(kind=kind_phys) rcl
      real(kind=kind_phys) dg(nsize_ar,levs), ug(nsize_ar,levs),
     &                     vg(nsize_ar,levs), vvel(nsize_ar,levs),
     &                    thg (nsize_ar,levs),
     &                    thgx(nsize_ar,levs),  thgy(nsize_ar,levs),
     &                     dphi(nsize_ar), dlam(nsize_ar), qg(nsize_ar)
 
       real(kind=kind_phys)
     &      tki(njeff,levp1), tkxy(njeff,levp1),
     &      pp (njeff,levp1),  dpx(njeff,levp1), dpy(njeff,levp1),
     &      dpp(njeff,levs),   dppx(njeff,levs),dppy(njeff,levs),
     &       db(njeff,levp1),  appx(njeff,levs),appy(njeff,levs) 
 
      real(kind=kind_phys) cons0,cons0p5,cons1,cons2,clog2   !constant
      real(kind=kind_phys) rkappa,tkrt0
 
!     print *,' enter  omegtes_gc_fd '				! hmhj
!
!     print *,' in omegtes_gc_fd vertcoord_id =',vertcoord_id

      cons0   = 0.d0      !constant
      cons0p5 = 0.5d0     !constant
      cons1   = 1.d0      !constant
      cons2   = 2.d0      !constant
      clog2=log(cons2)     ! constant
      rkappa = cp / rd

      do k=1,levp1
      do i=1,njeff
        pp (i,k)=ak5(k) + bk5(k)*qg(i)
        dpx(i,k)=bk5(k)*dlam(i)*rcl
        dpy(i,k)=bk5(k)*dphi(i)*rcl
      enddo
      enddo
! -----------------
      if( vertcoord_id.eq.3. ) then
      tki = 0.0
      tkxy= 0.0
      do k=2,levs
        do i=1,njeff
          tkrt0 = (thg(i,k-1)+thg(i,k))/(thref(k-1)+thref(k))
          tki (i,k)=ck5(k)*tkrt0**rkappa
          tkxy(i,k)=tki(i,k)*rkappa/(thg(i,k-1)+thg(i,k))
        enddo
      enddo
      do k=2,levs
        do i=1,njeff
          pp (i,k)=pp (i,k) + tki(i,k)
          dpx(i,k)=dpx(i,k)+tkxy(i,k)*(thgx(i,k-1)+thgx(i,k))
          dpy(i,k)=dpy(i,k)+tkxy(i,k)*(thgy(i,k-1)+thgy(i,k))
        enddo
      enddo
      endif
! -------------------
 
      do k=1,levs
        do i=1,njeff
          dpp (i,k) = pp (i,k) - pp (i,k+1)
          dppx(i,k) = dpx(i,k) - dpx(i,k+1)
          dppy(i,k) = dpy(i,k) - dpy(i,k+1)
          appx(i,k) = dpx(i,k) + dpx(i,k+1)
          appy(i,k) = dpy(i,k) + dpy(i,k+1)
        enddo
      enddo
      do i=1,njeff
        db(i,levs+1) = 0.e0
      enddo
      do k=levs,1,-1
        do i=1,njeff
          db(i,k)=db(i,k+1)+dpp(i,k)*dg(i,k)
     &                     +ug(i,k)*dppx(i,k)+vg(i,k)*dppy(i,k)
        enddo
      enddo
      do k=1,levs
        do i=1,njeff
          vvel(i,k)= ug(i,k)*appx(i,k)+vg(i,k)*appy(i,k)
     &              -db(i,k)-db(i,k+1)
          vvel(i,k)= 0.5 * vvel(i,k)
        enddo
      enddo

!     print *,' leave omegtes_gc_h. '				! hmhj
 
      return
      end
