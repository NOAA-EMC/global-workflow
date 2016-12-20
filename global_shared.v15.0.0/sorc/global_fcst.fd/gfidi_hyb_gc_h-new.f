      subroutine gfidi_hyb_gc_h(lon_dim,lons_lat,lat,
     &  dg,hg,zg,ug,vg,rqg,dphi,dlam,ps,
     &  rcl,spdmax,deltim,nvcn,xvcn,
     &  dhdf,dhdl,drqdf,drqdl,dudl,dvdl,dudf,dvdf,
     &  dpsdt,dhdt,drqdt,dudt,dvdt,szdrqdt)
 
!
! version of ppi=ak+bk*psfc+ck*(h/h0)^(1/kappa)
!
! hmhj : this is modified hybrid by finite difference from henry
! hmhj : use h=cpt instead of tv for thermodynamical equation
! fanglin yang, june 2007: use flux-limited scheme for vertical advection of tracers
!
 
      use machine , only : kind_phys
 
      use resol_def
      use namelist_def
      use coordinate_def
      use tracer_const
!     use physcons, rerth => con_rerth, rd => con_rd, cpd => con_cp
      use physcons, rerth => con_rerth
     &,             omega => con_omega, kappa => con_rocp
      use date_def, only : fhour
      implicit none

      real (kind=kind_phys), parameter :: rkappa=1.0/kappa
      integer lon_dim,lons_lat
      integer i,k,n,nvcn,lat
      real coriol,rcl,sinra,deltim,xvcn
      real hkrt0,wmkm1,wmkp1
      real
     1    dg(lon_dim,levs), hg(lon_dim,levs),  zg(lon_dim,levs),
     2    ug(lon_dim,levs), vg(lon_dim,levs),
     2   rqg(lon_dim,levs,ntrac),
     3  dphi(lon_dim), dlam(lon_dim), ps(lon_dim)
      real
     1  dhdf(lon_dim,levs),       dhdl(lon_dim,levs),
     1  dudf(lon_dim,levs),       dudl(lon_dim,levs),
     1  dvdf(lon_dim,levs),       dvdl(lon_dim,levs),
     1  drqdf(lon_dim,levs,ntrac), drqdl(lon_dim,levs,ntrac)
      real
     1  dudt(lon_dim,levs),       dvdt(lon_dim,levs),
     1  dpsdt(lon_dim),
     1  dhdt(lon_dim,levs),
     1  drqdt(lon_dim,levs,ntrac), spdmax(levs)
      real szdrqdt(lon_dim,levs,ntrac)   !saved vertical advection of tracers from time step n-1
!     real dpdti(lon_dim,levs+1)
!     real dppmin(levs)
c
      real rdelp(lons_lat,levs), rdelp2(lons_lat,levs)
      real xr(lons_lat,levs),xcp(lons_lat,levs)
      real xkappa(lons_lat,levs)
      real drdf (lon_dim,levs), drdl (lon_dim,levs)
      real dcpdf(lon_dim,levs), dcpdl(lon_dim,levs)
      real dkhdf(lon_dim,levs), dkhdl(lon_dim,levs)
      real sumdf(lon_dim,levs), sumdl(lon_dim,levs)
      real sumrq(lon_dim,levs)
      real cg(lons_lat,levs),ek(lons_lat,levs)
      real fb(lons_lat,levs+1),fg(lons_lat,levs)
      real dpxi(lons_lat,levs+1),dpyi(lons_lat,levs+1)
      real hki (lons_lat,levs+1),hkci(lons_lat,levs+1)
      real dpp(lons_lat,levs),rpp(lons_lat,levs)
      real dlnpx(lons_lat,levs),dlnpy(lons_lat,levs)
      real dphix (lons_lat,levs),dphiy (lons_lat,levs)
      real dphixk(lons_lat,levs),dphiyk(lons_lat,levs)
      real wflx(lons_lat,levs+1)
      real wf(lons_lat,levs+1),wf1(lons_lat,levs+1)
      real wml(lons_lat,levs),wmm(lons_lat,levs),wmu(lons_lat,levs)
      real work(lons_lat,levs)
      real dup(lons_lat,levs),dum(lons_lat,levs)
      real ppi(lons_lat,levs+1),ppl(lons_lat,levs)
      real alpha(lons_lat,levs),betta(lons_lat,levs)
      real gamma(lons_lat,levs),delta(lons_lat,levs),alnpk
      real zadv(lons_lat,levs,3+ntrac)
      real rqg_half(lons_lat,0:levs,ntrac), rqg_d(lons_lat,0:levs,ntrac)
      real, parameter :: cons0=0.0,cons1=1.0,cons2=2.0,cons0p5=0.5
      real rdt2
      real rrkp,rrk1m,phkp,phk1m,bb,cc,tmpdrqdt
!     logical adjusted
      integer levsb

c
!     print *,' enter  gfidi_hyb_gc_h '
c
!     call mymaxmin(dg,lons_lat,lon_dim,levs,' dg ')
!     call mymaxmin(hg,lons_lat,lon_dim,levs,' hg ')
!     call mymaxmin(zg,lons_lat,lon_dim,levs,' zg ')
!     call mymaxmin(ug,lons_lat,lon_dim,levs,' ug ')
!     call mymaxmin(vg,lons_lat,lon_dim,levs,' vg ')
!     call mymaxmin(rqg,lons_lat,lon_dim,levs*ntrac,' rqg ')
!     call mymaxmin(dphi,lons_lat,lon_dim,1,' dphi ')
!     call mymaxmin(dlam,lons_lat,lon_dim,1,' dlam ')
!     call mymaxmin(ps,lons_lat,lon_dim,1,' ps ')

!     call mymaxmin(dhdf,lons_lat,lon_dim,levs,' dhdf ')
!     call mymaxmin(dhdl,lons_lat,lon_dim,levs,' dhdl ')
!     call mymaxmin(dudf,lons_lat,lon_dim,levs,' dudf ')
!     call mymaxmin(dudl,lons_lat,lon_dim,levs,' dudl ')
!     call mymaxmin(dvdf,lons_lat,lon_dim,levs,' dvdf ')
!     call mymaxmin(dvdl,lons_lat,lon_dim,levs,' dvdl ')
!     call mymaxmin(drqdf,lons_lat,lon_dim,levs*ntrac,' drqdf ')
!     call mymaxmin(drqdl,lons_lat,lon_dim,levs*ntrac,' drqdl ')
!
! -------- prepare coriolis and gaussian weighting 
!
      rdt2   = 1./(cons2*deltim)

      sinra  = sqrt(cons1-cons1/rcl)
      coriol = cons2*omega*sinra
      sinra  = sinra/rerth
                                                                                
      if(lat.gt.latg2) then
        coriol = -coriol
        sinra  = -sinra
      endif
!
! max wind
!
      spdmax = cons0
      do k=1,levs
       do i=1,lons_lat
        ek(i,k)   = (  ug(i,k)*ug(i,k)+vg(i,k)*vg(i,k) ) * rcl
        spdmax(k) = max( ek(i,k),spdmax(k) )
       enddo
      enddo
!
! ----- prepare xr, xcp, xkapa and their derivatives
!
      xr    = 0.0
      drdl  = 0.0
      drdf  = 0.0
      xcp   = 0.0
      dcpdl = 0.0
      dcpdf = 0.0
      sumdl = 0.0
      sumdf = 0.0
      sumrq = 0.0
!
      do n=1,ntrac
        if( ri(n) .ne. 0.0 .and. cpi(n) .ne. 0.0 ) then
          do k=1,levs
            do i=1,lons_lat
              xr   (i,k) = xr   (i,k) + rqg  (i,k,n)*ri(n)
              drdl (i,k) = drdl (i,k) + drqdl(i,k,n)*ri(n)
              drdf (i,k) = drdf (i,k) + drqdf(i,k,n)*ri(n)
!
              xcp  (i,k) = xcp  (i,k) + rqg  (i,k,n)*cpi(n)
              dcpdl(i,k) = dcpdl(i,k) + drqdl(i,k,n)*cpi(n)
              dcpdf(i,k) = dcpdf(i,k) + drqdf(i,k,n)*cpi(n)
!
              sumrq(i,k) = sumrq(i,k) + rqg  (i,k,n)
              sumdl(i,k) = sumdl(i,k) + drqdl(i,k,n)
              sumdf(i,k) = sumdf(i,k) + drqdf(i,k,n)
            enddo
          enddo
        endif
      enddo
      do k=1,levs
        do i=1,lons_lat
!         drdl (i,k) = drdl (i,k) - rd *sumdl(i,k)
!         drdf (i,k) = drdf (i,k) - rd *sumdf(i,k)
!         xr (i,k)   = (1.-sumrq(i,k))*rd  + xr (i,k)

          drdl (i,k) = drdl (i,k) - ri(0) *sumdl(i,k)
          drdf (i,k) = drdf (i,k) - ri(0) *sumdf(i,k)
          xr (i,k)   = (1.-sumrq(i,k))*ri(0)  + xr (i,k)
!
!         dcpdl(i,k) = dcpdl(i,k) - cpd*sumdl(i,k)
!         dcpdf(i,k) = dcpdf(i,k) - cpd*sumdf(i,k)
!         xcp(i,k)   = (1.-sumrq(i,k))*cpd + xcp(i,k)

          dcpdl(i,k) = dcpdl(i,k) - cpi(0)*sumdl(i,k)
          dcpdf(i,k) = dcpdf(i,k) - cpi(0)*sumdf(i,k)
          xcp(i,k)   = (1.-sumrq(i,k))*cpi(0) + xcp(i,k)
          xkappa(i,k) = xr(i,k) / xcp(i,k)
        enddo
      enddo
!
! ----- prepare dpxi, dpyi
!
      hki(:,1)       = 0.0
      hkci(:,1)      = 0.0
      hki(:,levs+1)  = 0.0
      hkci(:,levs+1) = 0.0
      do k=2,levs
        do i=1,lons_lat
          hkrt0     = (hg(i,k-1)+hg(i,k))/(thref(k-1)+thref(k))
          hki (i,k) = ck5(k)*hkrt0**rkappa
          hkci(i,k) = hki(i,k)*rkappa/(hg(i,k-1)+hg(i,k))
        enddo
      enddo
      do k=1,levs+1
        do i=1,lons_lat
          ppi(i,k)  = ak5(k  ) + bk5(k  )*ps(i) + hki(i,k)
        enddo 
      enddo
!
!!    if( vertcoord_id.eq.3. ) then
!!     call adj_dpp(lons_lat,lon_dim,levs,ppi,dpdti,adjusted)
!      call adj_dpp(lons_lat,lon_dim,levs,ppi,hg,1.0,adjusted)
!      if( adjusted ) then
!      do k=2,levs
!       do i=1,lons_lat
!         hkrt0 = (hg(i,k-1)+hg(i,k))/(thref(k-1)+thref(k))
!         hki (i,k)=ck5(k)*hkrt0**rkappa
!         ppi(i,k)  =ak5(k  )+bk5(k  )*ps(i)+hki(i,k)
!       enddo
!      enddo
!      endif
!!    endif
!
      do k=1,levs
        do i=1,lons_lat
          ppl(i,k)  = cons0p5 * ( ppi(i,k) + ppi(i,k+1) )
          rpp(i,k)=1./(ppi(i,k)+ppi(i,k+1))
          dpp(i,k)=ppi(i,k)-ppi(i,k+1)
          rdelp (i,k) = cons1/dpp(i,k)
          rdelp2(i,k) = cons0p5/dpp(i,k)
          if( dpp(i,k) .lt. 0.0 ) then
            print *,' ----- dpp < 0 in gfidi at i k ',i,k
          endif
        enddo
      enddo
!
! ------------------ debug ---------------
!
! min dpp
!
!     dppmin=100.0
!     do k=1,levs
!      do i=1,lons_lat
!       dppmin(k)=min( dpp(i,k),dppmin(k) )
!      enddo
!     enddo
!
! ----------------------------------------------------------------------

      do k=1,levs+1
        do i=1,lons_lat
          dpxi(i,k)=bk5(k)*dlam(i)*rcl
          dpyi(i,k)=bk5(k)*dphi(i)*rcl
        enddo
      enddo
      do k=2,levs
        do i=1,lons_lat
          dpxi(i,k)=dpxi(i,k)+hkci(i,k)*(dhdl(i,k-1)+dhdl(i,k))
          dpyi(i,k)=dpyi(i,k)+hkci(i,k)*(dhdf(i,k-1)+dhdf(i,k))
        enddo
      enddo
!
      alpha(:,levs)=0.0
      betta(:,1)=0.0
      do k=2,levs
        do i=1,lons_lat
          alpha(i,k-1)=(ppi(i,k-1)+ppi(i,k))**xkappa(i,k-1)
          alpha(i,k-1)=(ppi(i,k)+ppi(i,k+1))**xkappa(i,k)/alpha(i,k-1)
        enddo
      enddo
      do k=1,levs-1
        do i=1,lons_lat
          betta(i,k+1)=(ppi(i,k+1)+ppi(i,k+2))**xkappa(i,k+1)
          betta(i,k+1)=(ppi(i,k  )+ppi(i,k+1))**xkappa(i,k)/betta(i,k+1)
        enddo
      enddo
      do k=1,levs
        do i=1,lons_lat
          alnpk = log( ppl(i,k) / 100. )	! ppl in cb
          gamma(i,k)= 1.0-(xkappa(i,k-1)-xkappa(i,k))*alnpk
     &                   -xkappa(i,k)*dpp(i,k)*rpp(i,k)*2.0
          delta(i,k)= 1.0+(xkappa(i,k)-xkappa(i,k+1))*alnpk
     &                   +xkappa(i,k)*dpp(i,k)*rpp(i,k)*2.0
        enddo
      enddo
c
! ----- prepare cg and fb
c
      do k=1,levs
        do i=1,lons_lat
          fg(i,k)=ug(i,k)*(dpxi(i,k)-dpxi(i,k+1))
     &           +vg(i,k)*(dpyi(i,k)-dpyi(i,k+1))
     &           +dpp(i,k)*dg(i,k)
          cg(i,k)=ug(i,k)*(dpxi(i,k)+dpxi(i,k+1))
     &           +vg(i,k)*(dpyi(i,k)+dpyi(i,k+1))
        enddo
      enddo
c
      do i=1,lons_lat
        fb(i,levs+1)=0.0
      enddo
      do k=levs,1,-1
        do i=1,lons_lat
          fb(i,k)=fb(i,k+1)+fg(i,k)
        enddo
      enddo
c
c local change of surface pressure  d ps dt
c
      do i=1,lons_lat
        dpsdt(i) = - fb(i,1)
      enddo

c
c get dlnpx dlnpy pp
c
      do k=1,levs
        do i=1,lons_lat
          dlnpx(i,k)=rpp(i,k)*(dpxi(i,k)+dpxi(i,k+1))/rcl
          dlnpy(i,k)=rpp(i,k)*(dpyi(i,k)+dpyi(i,k+1))/rcl
        enddo
      enddo
c
c hydrostatic to get geopotential height without horizontal derivative
c
      do k=1,levs
        do i=1,lons_lat
          dkhdl(i,k)=( xr(i,k)*dhdl(i,k)+hg(i,k)*drdl(i,k)
     &                -xkappa(i,k)*hg(i,k)*dcpdl(i,k) )/xcp(i,k)
          dkhdf(i,k)=( xr(i,k)*dhdf(i,k)+hg(i,k)*drdf(i,k)
     &                -xkappa(i,k)*hg(i,k)*dcpdf(i,k) )/xcp(i,k)
        enddo
      enddo
      do k=1,levs
        do i=1,lons_lat
          dphixk(i,k)= rpp(i,k)*( dpp(i,k)*dkhdl(i,k)
     &                  +xkappa(i,k)*hg(i,k)*( (dpxi(i,k)-dpxi(i,k+1))
     &                  -rpp(i,k)*dpp(i,k)*(dpxi(i,k)+dpxi(i,k+1)) ) )
          dphiyk(i,k)= rpp(i,k)*( dpp(i,k)*dkhdf(i,k)
     &                  +xkappa(i,k)*hg(i,k)*( (dpyi(i,k)-dpyi(i,k+1))
     &                  -rpp(i,k)*dpp(i,k)*(dpyi(i,k)+dpyi(i,k+1)) ) )
        enddo
      enddo
      do i=1,lons_lat
        dphix(i,1)= 0.0
        dphiy(i,1)= 0.0
      enddo
      do k=1,levs-1
        do i=1,lons_lat
          dphix(i,k  )= dphix(i,k)+dphixk(i,k)
          dphix(i,k+1)= dphix(i,k)+dphixk(i,k)
          dphiy(i,k  )= dphiy(i,k)+dphiyk(i,k)
          dphiy(i,k+1)= dphiy(i,k)+dphiyk(i,k)
        enddo
      enddo
      do i=1,lons_lat
        dphix(i,levs)= dphix(i,levs)+dphixk(i,levs)
        dphiy(i,levs)= dphiy(i,levs)+dphiyk(i,levs)
      enddo
      do k=1,levs
        do i=1,lons_lat
          dphix(i,k)= dphix(i,k) / rcl
          dphiy(i,k)= dphiy(i,k) / rcl
        enddo
      enddo
c
c total derivative of horizontal wind
c
      do k=1,levs
       do i=1,lons_lat
         dudt(i,k)=
     &                   - xkappa(i,k) *hg(i,k)*dlnpx(i,k)
     &                   - dphix(i,k)
     &                   + vg(i,k)*coriol
         dvdt(i,k)=
     &                   - xkappa(i,k) *hg(i,k)*dlnpy(i,k)
     &                   - dphiy(i,k)
     &                   - ug(i,k)*coriol
     &                   - ek(i,k) * sinra
       enddo
      enddo
c
c total derivative of virtual temperature
c
      do k=1,levs
       do i=1,lons_lat
         dhdt(i,k)=
     &              +xkappa(i,k)*hg(i,k)*rpp(i,k)*
     &                       (cg(i,k)-fb(i,k)-fb(i,k+1))
       enddo
      enddo
c
c
! --------------- horizontal advection first --------
c
c horizontal advection for all
c
      do k=1,levs
        do i=1,lons_lat
          dudt(i,k)=dudt(i,k)
     &               -ug(i,k)*dudl(i,k)-vg(i,k)*dudf(i,k)
          dvdt(i,k)=dvdt(i,k)
     &               -ug(i,k)*dvdl(i,k)-vg(i,k)*dvdf(i,k)
          dhdt(i,k)=dhdt(i,k)
     &               -ug(i,k)*dhdl(i,k)-vg(i,k)*dhdf(i,k)
        enddo
      enddo
      do n=1,ntrac
      do k=1,levs
        do i=1,lons_lat
          drqdt(i,k,n)=
     &               -ug(i,k)*drqdl(i,k,n)-vg(i,k)*drqdf(i,k,n)
        enddo
      enddo
      enddo
!
! ------ hybrid to solve vertical flux ----------

      do i=1,lons_lat
        dup(i,levs)=0.0
        dum(i,1 )=0.0
      enddo
      do k=1,levs-1
        do i=1,lons_lat
          dup(i,k  )=delta(i,k)*hg(i,k)-betta(i,k+1)*hg(i,k+1)
          dum(i,k+1)=alpha(i,k)*hg(i,k)-gamma(i,k+1)*hg(i,k+1)
        enddo
      enddo
!
      levsb=levs
!     do k=levs,2,-1
!       if( bk5(k).eq.0.0 .and. ck5(k).ne.0.0 ) levsb=k-1
!     enddo
      k=2
        do i=1,lons_lat
          wmkm1=hkci(i,k)*rdelp2(i,k-1)
          wmkp1=hkci(i,k)*rdelp2(i,  k)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
          wmu(i,k-1)=wmkp1*dup(i,k)
        enddo
      do k=3,levs-1
        do i=1,lons_lat
          wmkm1=hkci(i,k)*rdelp2(i,k-1)
          wmkp1=hkci(i,k)*rdelp2(i,  k)
          wml(i,k-2)=wmkm1*dum(i,k-1)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
          wmu(i,k-1)=wmkp1*dup(i,k)
        enddo
      enddo
      k=levs
        do i=1,lons_lat
          wmkm1=hkci(i,k)*rdelp2(i,k-1)
          wmkp1=hkci(i,k)*rdelp2(i,  k)
          wml(i,k-2)=wmkm1*dum(i,k-1)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
        enddo
!hmhj wf(:,levs:levs+1)=0.0
!hmhj do k=2,levs
      wf(:,levsb:levs+1)=0.0
      do k=2,levsb
        do i=1,lons_lat
!         wf(i,k-1)=bk5(k)*dpsdt(i)+fb(i,k)-dpdti(i,k)*rdt2
          wf(i,k-1)=bk5(k)*dpsdt(i)+fb(i,k)
     &              +hkci(i,k)*(dhdt(i,k-1)+dhdt(i,k))
        enddo
      enddo
      call tridim_hyb_gc_h(lons_lat,lons_lat,levsb-1,levs+1,1,
     &                wml,wmm,wmu,wf,work,wf1)
      wflx(:,1)=0.0
!hmhj wflx(:,levs+1)=0.0
!hmhj do k=2,levs
      wflx(:,levsb+1:levs+1)=0.0
      do k=2,levsb
        do i=1,lons_lat
          wflx(i,k)=wf1(i,k-1)
        enddo
      enddo
!
! ------ vertical advection for all --------
c
c do vertical advection of hg first, since dup and dum are obtained
c
      do k=1,levs
        do i=1,lons_lat
          zadv(i,k,3)=-rdelp2(i,k)*
     &             (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
c
c
c vertical advection of uu 
c
      do k=1,levs-1
        do i=1,lons_lat
          dup(i,k  )=ug(i,k)-ug(i,k+1)
          dum(i,k+1)=ug(i,k)-ug(i,k+1)
        enddo
      enddo

      do k=1,levs
        do i=1,lons_lat
          zadv(i,k,1)=-rdelp2(i,k)*
     &             (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
c
c vertical advection of vv 
c
      do k=1,levs-1
        do i=1,lons_lat
          dup(i,k  )=vg(i,k)-vg(i,k+1)
          dum(i,k+1)=vg(i,k)-vg(i,k+1)
        enddo
      enddo
      do k=1,levs
        do i=1,lons_lat
          zadv(i,k,2)=-rdelp2(i,k)*
     &             (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
c
c vertical advection of qq
c
! fanglin yang, june 2007
! 1. use total variation diminishing (tvd) flux-limited scheme
!    for vertical advection of tracers ( j. thuburn, qjrms, 1993, 469-487)
!    vertical advection  dq/dt = aa = -w*dq/dp = -[d(q*w)/dp-q*dw/dp]
!    let bb=d(q*w)/dp and cc=-q*dw/dp, aa=-(bb+cc), then q(n+1)=q(n)+aa*dt
! 2. the current scheme is central in space and central in time.  to use
!    the tvd scheme for vertical advection, the time differencing must be
!    forward in time otherwise it is unstable.  however, for horizonatl
!    advection which is center in space, the forward-in-time scheme is
!    always unstable.  to overcome this conflict, the vertical adevtion
!    from time step n-1 is used to get mean advection at current time
!    step.  then, the central-in-time scheme is applied to both the
!    vertical and horizontal advections of tracers.

!--------------------------------------
      if(zflxtvd) then    !flux-limited vertical advection
!--------------------------------------
      do n=1,ntrac
       do i=1,lons_lat
        do k=1,levs-1            !k=1, surface
         rqg_half(i,k,n)=cons0p5*(rqg(i,k,n)+rqg(i,k+1,n))
        enddo
         rqg_half(i,0,n)=rqg(i,1,n)
         rqg_half(i,levs,n)=rqg(i,levs,n)
                                                                                                                             
        do k=1,levs-1            !k=1, surface
         rqg_d(i,k,n)=rqg(i,k,n)-rqg(i,k+1,n)
        enddo
        if(rqg(i,levs,n).ge.cons0) then
         rqg_d(i,levs,n)=rqg(i,levs,n)-
     1     max(cons0,cons2*rqg(i,levs,n)-rqg(i,levs-1,n))
        else
         rqg_d(i,levs,n)=rqg(i,levs,n)-
     1     min(cons0,cons2*rqg(i,levs,n)-rqg(i,levs-1,n))
        endif
        if(rqg(i,1,n).ge.cons0) then
         rqg_d(i,0,n)=max(cons0,cons2*rqg(i,1,n)-rqg(i,2,n))-
     1     rqg(i,1,n)
        else
         rqg_d(i,0,n)=min(cons0,cons2*rqg(i,1,n)-rqg(i,2,n))-
     1     rqg(i,1,n)
        endif
       enddo
! --update tracers at half-integer layers using van leer (1974) limiter
!   (without this update, the scheme is the same as that in loop 340)
        do i=1,lons_lat
        do k=1,levs-1
        if(wflx(i,k+1).gt.cons0) then            !wind blows to down
           rrkp=cons0
           if(rqg_d(i,k,n).ne.cons0) rrkp=rqg_d(i,k+1,n)/rqg_d(i,k,n)
           phkp=(rrkp+abs(rrkp))/(1+abs(rrkp))
           rqg_half(i,k,n)=rqg(i,k+1,n)+
     1                     phkp*(rqg_half(i,k,n)-rqg(i,k+1,n))
        else
           rrk1m=cons0
           if(rqg_d(i,k,n).ne.cons0) rrk1m=rqg_d(i,k-1,n)/rqg_d(i,k,n)
           phk1m=(rrk1m+abs(rrk1m))/(1+abs(rrk1m))
           rqg_half(i,k,n)=rqg(i,k,n)+
     1                     phk1m*(rqg_half(i,k,n)-rqg(i,k,n))
        endif
        enddo
        enddo
            
        do i=1,lons_lat
        do k=1,levs
         bb=rqg_half(i,k-1,n)*wflx(i,k)-rqg_half(i,k,n)*wflx(i,k+1)
         cc=-rqg(i,k,n)*(wflx(i,k)-wflx(i,k+1))
         tmpdrqdt=-rdelp(i,k)*(bb+cc)
         if(fhour.eq.0.0 .or.lsfwd) then
          zadv(i,k,3+n)=tmpdrqdt
         else
          zadv(i,k,3+n)=cons0p5*(tmpdrqdt+szdrqdt(i,k,n))
         endif
         szdrqdt(i,k,n)=tmpdrqdt
        enddo
        enddo
      enddo
!--------------------------------------
      else
!--------------------------------------
      do n=1,ntrac
      do k=1,levs-1
        do i=1,lons_lat
          dup(i,k  )=rqg(i,k,n)-rqg(i,k+1,n)
          dum(i,k+1)=rqg(i,k,n)-rqg(i,k+1,n)
        enddo
      enddo
      do k=1,levs
        do i=1,lons_lat
          zadv(i,k,3+n)=-rdelp2(i,k)*
     &               (wflx(i,k)*dum(i,k)+wflx(i,k+1)*dup(i,k))
        enddo
      enddo
      enddo
!--------------------------------------
      endif
!--------------------------------------

! do vertical advection filter
      call vcnhyb_gc_h(lons_lat,levs,3+ntrac,deltim,
     &            ppi,ppl,wflx,zadv,nvcn,xvcn)
!     if( nvcn.gt.0 ) print *,' ---- nvcn =',nvcn,'    xvcn=',xvcn 
! add vertical filterd advection
      do k=1,levs
      do i=1,lons_lat
       dudt(i,k)=dudt(i,k)+zadv(i,k,1)
       dvdt(i,k)=dvdt(i,k)+zadv(i,k,2)
       dhdt(i,k)=dhdt(i,k)+zadv(i,k,3)
      enddo
      enddo
      do  n=1,ntrac
       do k=1,levs
       do i=1,lons_lat
        drqdt(i,k,n)=drqdt(i,k,n)+zadv(i,k,3+n)
       enddo
       enddo
      enddo

! this multiplication must be on  completed tendencies.
      do k=1,levs
      do i=1,lons_lat
        dudt(i,k)=dudt(i,k)*rcl
        dvdt(i,k)=dvdt(i,k)*rcl
      enddo
      enddo

!     call mymaxmin(dudt,lons_lat,lon_dim,levs,' dudt ')
!     call mymaxmin(dvdt,lons_lat,lon_dim,levs,' dvdt ')
!     call mymaxmin(dhdt,lons_lat,lon_dim,levs,' dhdt ')
!     call mymaxmin(drqdt,lons_lat,lon_dim,levs*ntrac,' drqdt ')
!     call mymaxmin(dpsdt,lons_lat,lon_dim,1,' dpsdt ')
!
!     print *,' end of gfidi_hyb_gc_h. '
!!

      return
      end

      subroutine mymaxmin(a,im,ix,kx,ch)
      real a(ix,kx)
      character*(*) ch
      do k=1,kx
        fmin=a(1,k)
        fmax=a(1,k)
        do i=1,im
          fmin=min(fmin,a(i,k))
          fmax=max(fmax,a(i,k))
        enddo
        print *,' max=',fmax,' min=',fmin,' at k=',k,' for ',ch
      enddo
      return
      end


      subroutine vcnhyb_gc_h(im,km,nm,dt,zint,zmid,zdot,zadv,nvcn,xvcn)
c                .      .    .                                       .
c subprogram:    vcnhyb_gc_h      vertical advection instability filter
c   prgmmr: iredell          org: w/nmc23    date: 91-05-07
c
c abstract: filters vertical advection tendencies
c   in the dynamics tendency equation in order to ensure stability
c   when the vertical velocity exceeds the cfl criterion.
c   the vertical velocity in this case is sigmadot.
c   for simple second-order centered eulerian advection,
c   filtering is needed when vcn=zdot*dt/dz>1.
c   the maximum eigenvalue of the linear advection equation
c   with second-order implicit filtering on the tendencies
c   is less than one for all resolvable wavenumbers (i.e. stable)
c   if the nondimensional filter parameter is nu=(vcn**2-1)/4.
c
c program history log:
c   97-07-30  iredell
c
c usage:    call vcnhyb_gc_h(im,km,nm,dt,zint,zmid,zdot,zadv,nvcn,xvcn)
c
c   input argument list:
c     im       - integer number of gridpoints to filter
c     km       - integer number of vertical levels
c     nm       - integer number of fields
c     dt       - real timestep in seconds
c     zint     - real (im,km+1) interface vertical coordinate values
c     zmid     - real (im,km) midlayer vertical coordinate values
c     zdot     - real (im,km+1) vertical coordinate velocity
c     zadv     - real (im,km,nm) vertical advection tendencies
c
c   output argument list:
c     zadv     - real (im,km,nm) vertical advection tendencies
c     nvcn     - integer number of points requiring filtering
c     xvcn     - real maximum vertical courant number
c
c   subprograms called:
c     tridim_hyb_gc_h   - tridiagonal matrix solver
c
      implicit none
      integer,intent(in):: im,km,nm
      real,intent(in):: dt,zint(im,km+1),zmid(im,km),zdot(im,km+1)
      real,intent(inout):: zadv(im,km,nm)
      integer,intent(out):: nvcn
      real,intent(out):: xvcn
      integer i,j,k,n,ivcn(im)
!     integer kk
      logical lvcn(im)
      real zdm,zda,zdb,vcn(im,km-1)
      real rnu,cm(im,km),cu(im,km-1),cl(im,km-1)
      real rr(im,km,nm)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  compute vertical courant number
c  increase by 10% for safety
      nvcn=0
      xvcn=0.
      lvcn=.false.
      do k=1,km-1
        do i=1,im
          zdm=abs(zint(i,k)-zint(i,k+1))
          zdm=min( zdm, abs(zint(i,k+1)-zint(i,k+2)) )
         vcn(i,k)=abs(zdot(i,k+1)*dt/zdm)*1.1	
          lvcn(i)=lvcn(i).or.vcn(i,k).gt.1.0
          xvcn=max(xvcn,vcn(i,k))

! hmhj debug print
!          if( vcn(i,k).gt.1.0 ) then
!            print *,' vert filter at i k vcn zdot pik pik1 pik2',
!    &       i,k,vcn(i,k),zdot(i,k+1),zint(i,k),zint(i,k+1),zint(i,k+2)
!            do kk=km+1,1,-1
!              print *,' k=',kk,' pi=',zint(i,kk)
!            enddo
!          endif

        enddo
      enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  determine points requiring filtering
      if(xvcn.gt.1.0) then
        do i=1,im
          if(lvcn(i)) then
            ivcn(nvcn+1)=i
            nvcn=nvcn+1
          endif
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  compute tridiagonal matrim
        do j=1,nvcn
          cm(j,1)=1
        enddo
        do k=1,km-1
          do j=1,nvcn
            i=ivcn(j)
            if(vcn(i,k).gt.1.0) then
             zdm=zmid(i,k)-zmid(i,k+1)
             zda=zint(i,k+1)-zint(i,k+2)
             zdb=zint(i,k)-zint(i,k+1)
              rnu=(vcn(i,k)**2-1.0)/4.0
              cu(j,k)=-rnu*zdm/zdb
              cl(j,k)=-rnu*zdm/zda
              cm(j,k)=cm(j,k)-cu(j,k)
              cm(j,k+1)=1-cl(j,k)
            else
              cu(j,k)=0.0
              cl(j,k)=0.0
              cm(j,k+1)=1.0
            endif
          enddo
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  fill fields to be filtered
        do n=1,nm
          do k=1,km
            do j=1,nvcn
              i=ivcn(j)
              rr(j,k,n)=zadv(i,k,n)
            enddo
          enddo
        enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  solve tridiagonal system
        call tridim_hyb_gc_h(nvcn,im,km,km,nm,cl,cm,cu,rr,cu,rr)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  replace filtered fields
        do n=1,nm
          do k=1,km
            do j=1,nvcn
              i=ivcn(j)
              zadv(i,k,n)=rr(j,k,n)
            enddo
          enddo
        enddo
      endif
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
c-----------------------------------------------------------------------
      subroutine tridim_hyb_gc_h(l,lx,n,nx,m,cl,cm,cu,r,au,a)
c                .      .    .                                       .
c subprogram:    tridim_hyb_gc_h      solves tridiagonal matrix problems.
c   prgmmr: iredell          org: w/nmc23    date: 91-05-07
c
c abstract: this routine solves multiple tridiagonal matrix problems
c   with multiple right-hand-side and solution vectors for every matrix.
c   the solutions are found by eliminating off-diagonal coefficients,
c   marching first foreward then backward along the matrix diagonal.
c   the computations are vectorized around the number of matrices.
c   no checks are made for zeroes on the diagonal or singularity.
c
c program history log:
c   97-07-30  iredell
c
c usage:    call tridim_hyb_gc_h(l,lx,n,nx,m,cl,cm,cu,r,au,a)
c
c   input argument list:
c     l        - integer number of tridiagonal matrices
c     lx       - integer first dimension (lx>=l)
c     n        - integer order of the matrices
c     nx       - integer second dimension (nx>=n)
c     m        - integer number of vectors for every matrix
c     cl       - real (lx,2:n) lower diagonal matrix elements
c     cm       - real (lx,n) main diagonal matrix elements
c     cu       - real (lx,n-1) upper diagonal matrix elements
c                (may be equivalent to au if no longer needed)
c     r        - real (lx,nx,m) right-hand-side vector elements
c                (may be equivalent to a if no longer needed)
c
c   output argument list:
c     au       - real (lx,n-1) work array
c     a        - real (lx,nx,m) solution vector elements
c
c attributes:
c   language: fortran 77.
c   machine:  cray.
c
      real cl(lx,2:n),cm(lx,n),cu(lx,n-1),r(lx,nx,m),
     &                         au(lx,n-1),a(lx,nx,m)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  march up
      do i=1,l
        fk=1./cm(i,1)
        au(i,1)=fk*cu(i,1)
      enddo
      do j=1,m
        do i=1,l
          fk=1./cm(i,1)
          a(i,1,j)=fk*r(i,1,j)
        enddo
      enddo
      do k=2,n-1
        do i=1,l
          fk=1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k)=fk*cu(i,k)
        enddo
        do j=1,m
          do i=1,l
            fk=1./(cm(i,k)-cl(i,k)*au(i,k-1))
            a(i,k,j)=fk*(r(i,k,j)-cl(i,k)*a(i,k-1,j))
          enddo
        enddo
      enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  march down
      do j=1,m
        do i=1,l
          fk=1./(cm(i,n)-cl(i,n)*au(i,n-1))
          a(i,n,j)=fk*(r(i,n,j)-cl(i,n)*a(i,n-1,j))
        enddo
      enddo
      do k=n-1,1,-1
        do j=1,m
          do i=1,l
            a(i,k,j)=a(i,k,j)-au(i,k)*a(i,k+1,j)
          enddo
        enddo
      enddo
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
