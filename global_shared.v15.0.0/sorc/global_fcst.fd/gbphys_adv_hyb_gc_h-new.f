      subroutine gbphys_adv_h(im,ix,deltim,hold,uold,vold,rqold,ps,
     &                            hg ,ug ,vg ,rqg ,prsi )
c
! version of ppi=ak+bk*psfc+ck*(h/h0)^(1/kappa)
!
! hmhj : this is modified hybrid by finite difference from henry juang
!
! input
!	im	number of grid point in given latitude to compute
!	ix	dimension of grid point number in a given latitude
!	deltim	time step in second
!	hold	(ix,levs) enthalpy before physics
!	uold	(ix,levs) pseudo u-wind before physics
!	vold	(ix,levs) pseudo v-wind before physics
!	rqold	(ix,levs) tracers before physics
!	prsi	(ix,levs+1) presure at interface
! input and output
!	hg	(ix,levs) enthalpy after physics then update here
!	ug	(ix,levs) pseudo u-wind after physics
!	vg	(ix,levs) pseudo v-wind after physics
!	rqg	(ix,levs) tracers after physics
!
      use machine , only : kind_phys
                                                                                
      use resol_def
      use coordinate_def
      use tracer_const
      use physcons , rd => con_rd , cp => con_cp

      implicit none
      integer ix,im
      integer i,k,n,nvcn
      real kappa,rkappa,deltim,dt2,rdt2
      real hkrt0,wmkm1,wmkp1,xvcn
      real ps(ix),
     1    hg(ix,levs),hold(ix,levs),
     2    ug(ix,levs),uold(ix,levs),
     3    vg(ix,levs),vold(ix,levs),
     4    rqg(ix,levs,ntrac),rqold(ix,levs,ntrac),
     5    prsi(ix,levs+1)
      real  dhdt(ix,levs)
c
      real rdelp(im,levs)
      real hki,hkci(im,levs+1)
      real xr(im,levs),xcp(im,levs),xkappa(im,levs),sumrq(im,levs)
      real dpp(im,levs),rpp(im,levs)
      real wflx(im,levs+1),wf(im,levs+1),wf1(im,levs+1)
      real wml(im,levs),wmm(im,levs),wmu(im,levs)
      real work(im,levs)
      real dup(im,levs),dum(im,levs)
      real alpha(im,levs),betta(im,levs),gamma(im,levs)
      real delta(im,levs),alnpk
      real ppi(im,levs+1),ppl(im,levs)
      real zadv(im,levs,3+ntrac)
c
!     print *,' enter  gbphys_adv_hyb_gc_fd '		! hmhj check
c
      dt2=deltim*2.0
      rdt2=1./dt2
!
      kappa   = rd / cp
      rkappa  = cp / rd

!
! ----- prepare ck/kappa/t(tv/t0)^(1/kappa)
!
      hkci= 0.0
      do k=2,levs
        do i=1,im
          hkrt0 = (hold(i,k-1)+hold(i,k))/(thref(k-1)+thref(k))
          hki   = ck5(k)*hkrt0**rkappa
          hkci(i,k)=hki*rkappa/(hold(i,k-1)+hold(i,k))
        enddo
      enddo
      do k=1,levs+1
        do i=1,im
          ppi(i,k)=prsi(i,k)	! from hyb2press with overturn adjusted
        enddo
      enddo
      do k=1,levs
        do i=1,im
          rpp(i,k)=1./(prsi(i,k)+prsi(i,k+1))
          dpp(i,k)=prsi(i,k)-prsi(i,k+1)
          rdelp(i,k) = 0.5/dpp(i,k)
          ppl(i,k)=0.5*(ppi(i,k)+ppi(i,k+1))
! temperature tendency = temperature change / 2dt
          dhdt(i,k)=(hg(i,k)-hold(i,k))*rdt2
        enddo
      enddo
!
! ----- prepare xr, xcp, xkapa 
!
      xr=0.0
      xcp=0.0
      sumrq=0.0
      do n=1,ntrac
        do k=1,levs
          do i=1,im
            xr   (i,k)=xr   (i,k)+ri (n)*rqg  (i,k,n)
            xcp  (i,k)=xcp  (i,k)+cpi(n)*rqg  (i,k,n)
            sumrq(i,k)=sumrq(i,k)+rqg  (i,k,n)
          enddo
        enddo
      enddo
      do k=1,levs
        do i=1,im
            xr (i,k)=(1.-sumrq(i,k))*rd + xr (i,k)
            xcp(i,k)=(1.-sumrq(i,k))*cp + xcp(i,k)
            xkappa(i,k) = xr(i,k) / xcp(i,k)
        enddo
      enddo
!
! ----- prepare dpxi, dpyi
!
      alpha(:,levs)=0.0
      betta(:,   1)=0.0
      do k=2,levs
        do i=1,im
          alpha(i,k-1)=(prsi(i,k-1)+prsi(i,k))**xkappa(i,k-1)
          alpha(i,k-1)=(prsi(i,k)+prsi(i,k+1))**xkappa(i,k)/
     &                 alpha(i,k-1)
        enddo
      enddo
      do k=1,levs-1
        do i=1,im
          betta(i,k+1)=(prsi(i,k+1)+prsi(i,k+2))**xkappa(i,k+1)
          betta(i,k+1)=(prsi(i,k  )+prsi(i,k+1))**xkappa(i,k)/
     &                 betta(i,k+1)
        enddo
      enddo
      do k=1,levs
        do i=1,im
          alnpk = log( ppl(i,k) / 100. )	! ppl in cb
          gamma(i,k)= 1.0-(xkappa(i,k-1)-xkappa(i,k))*alnpk
     &                   -xkappa(i,k)*dpp(i,k)*rpp(i,k)*2.0
          delta(i,k)= 1.0+(xkappa(i,k)-xkappa(i,k+1))*alnpk
     &                   +xkappa(i,k)*dpp(i,k)*rpp(i,k)*2.0
        enddo
      enddo

! ------ hybrid to solve vertical flux * 2dt ----------

      dup(:,levs)=0.0
      dum(:,   1)=0.0
      do k=1,levs-1
        do i=1,im
          dup(i,k  )=delta(i,k)*hold(i,k)-betta(i,k+1)*hold(i,k+1)
          dum(i,k+1)=alpha(i,k)*hold(i,k)-gamma(i,k+1)*hold(i,k+1)
        enddo
      enddo

      k=2
        do i=1,im
          wmkm1=hkci(i,k)*rdelp(i,k-1)
          wmkp1=hkci(i,k)*rdelp(i,  k)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
          wmu(i,k-1)=wmkp1*dup(i,k)
        enddo
      do k=3,levs-1
        do i=1,im
          wmkm1=hkci(i,k)*rdelp(i,k-1)
          wmkp1=hkci(i,k)*rdelp(i,  k)
          wml(i,k-2)=wmkm1*dum(i,k-1)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
          wmu(i,k-1)=wmkp1*dup(i,k)
        enddo
      enddo
      k=levs
        do i=1,im
          wmkm1=hkci(i,k)*rdelp(i,k-1)
          wmkp1=hkci(i,k)*rdelp(i,  k)
          wml(i,k-2)=wmkm1*dum(i,k-1)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
        enddo
      wf(:,levs:levs+1)=0.0
      do k=2,levs
        do i=1,im
          wf(i,k-1)=hkci(i,k)*(dhdt(i,k-1)+dhdt(i,k))
        enddo
      enddo
      call tridim_hyb(im,im,levs-1,levs+1,1,
     &                wml,wmm,wmu,wf,work,wf1)
      wflx(:,1)=0.0
      wflx(:,levs+1)=0.0
      do k=2,levs
        do i=1,im
          wflx(i,k)=wf1(i,k-1)
        enddo
      enddo

! ------ vertical advection changes for all --------
c
c do vertical advection of tt first, since dup and dum are obtained
c
      do k=1,levs
        do i=1,im
          zadv(i,k,3)=-rdelp (i,k)*
     &             (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
c
c vertical advection of uu 
c
      do k=1,levs-1
        do i=1,im
          dup(i,k  )=uold(i,k)-uold(i,k+1)
          dum(i,k+1)=uold(i,k)-uold(i,k+1)
        enddo
      enddo
      do k=1,levs
        do i=1,im
          zadv(i,k,1)=-rdelp (i,k)*
     &             (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
c
c vertical advection of vv 
c
      do k=1,levs-1
        do i=1,im
          dup(i,k  )=vold(i,k)-vold(i,k+1)
          dum(i,k+1)=vold(i,k)-vold(i,k+1)
        enddo
      enddo
      do k=1,levs
        do i=1,im
          zadv(i,k,2)=-rdelp (i,k)*
     &             (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
c
c vertical advection of qq
c
      do n=1,ntrac
      do k=1,levs-1
        do i=1,im
          dup(i,k  )=rqold(i,k,n)-rqold(i,k+1,n)
          dum(i,k+1)=rqold(i,k,n)-rqold(i,k+1,n)
        enddo
      enddo
      do k=1,levs
        do i=1,im
          zadv(i,k,3+n)=-rdelp (i,k)*
     &               (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
      enddo
! hmhj
! do vertical advection filter
      call vcnhyb(im,levs,3+ntrac,deltim,ppi,ppl,wflx,zadv,nvcn,xvcn)
      if( nvcn.gt.0 ) print *,' in gbphys_adv nvcn=',nvcn,' xvcn=',xvcn 
c
! add vertical filterd advection
      do k=1,levs
      do i=1,im
       ug(i,k)=ug(i,k)+zadv(i,k,1)*dt2
       vg(i,k)=vg(i,k)+zadv(i,k,2)*dt2
       hg(i,k)=hg(i,k)+zadv(i,k,3)*dt2
      enddo
      enddo
      do  n=1,ntrac
       do k=1,levs
       do i=1,im
        rqg(i,k,n)=rqg(i,k,n)+zadv(i,k,3+n)*dt2
       enddo
       enddo
      enddo
!
!     print *,' end of gbphys_adv_hyb_gc_fd. '			! hmhj check
!!
      return
      end

