      subroutine gfidi_hyb_gc_ndsl(lon_dim,lons_lat,lat,
     &  dg,tg,zg,ug,vg,rqg,dphi,dlam,ps,
     &  rcl,spdmax,deltim,
     &  dtdf,dtdl,drdf,drdl,dudl,dvdl,dudf,dvdf,
     &   psm,  ttm, rrm, uum, vvm,
     &  dpsdt,dtdt,drdt,dudt,dvdt,ddpdt,dppdt)
 
!
! version of ppi=ak+bk*psfc+ck*(tv/t0)^(1/kappa)
!
! hmhj : this is modified hybrid by finite difference from henry
! henry juang: add ndsl for semi-lagrangian advection horizontal outside but
!              vertical advection here.
!
 
      use machine , only : kind_phys
 
      use resol_def
      use namelist_def
      use coordinate_def
      use physcons, rerth => con_rerth, rd => con_rd, cp => con_cp
     &,             omega => con_omega, cvap => con_cvap
      implicit none

      integer lon_dim,lons_lat
      integer i,k,n,lat
      real coriol,rcl,kappa,rkappa,cpvocpd1,sinra,deltim
      real tkrt0,wmkm1,wmkp1
      real
     1    uum(lon_dim,levs), vvm(lon_dim,levs),  ttm(lon_dim,levs),
     2    rrm(lon_dim,levs,ntrac), psm(lon_dim)
      real
     1    dg(lon_dim,levs), tg(lon_dim,levs),  zg(lon_dim,levs),
     2    ug(lon_dim,levs), vg(lon_dim,levs),
     2   rqg(lon_dim,levs,ntrac),
     3  dphi(lon_dim), dlam(lon_dim), ps(lon_dim)
      real
     1  dtdf(lon_dim,levs),       dtdl(lon_dim,levs),
     1  dudf(lon_dim,levs),       dudl(lon_dim,levs),
     1  dvdf(lon_dim,levs),       dvdl(lon_dim,levs),
     1  drdf(lon_dim,levs,ntrac), drdl(lon_dim,levs,ntrac)
      real
     1  dudt(lon_dim,levs),       dvdt(lon_dim,levs),
     1  dpsdt(lon_dim),           dppdt(lon_dim,levs),
     1  dtdt(lon_dim,levs),       ddpdt(lon_dim,levs),
     1  drdt(lon_dim,levs,ntrac), spdmax(levs)
!     real dppmin(levs)
c
      real qq(lons_lat,levs,3+ntrac)
      real rdelp(lons_lat,levs),rdelp2(lons_lat,levs)
      real cg(lons_lat,levs),ek(lons_lat,levs)
      real fb(lons_lat,levs+1),fg(lons_lat,levs)
      real dpxi(lons_lat,levs+1),dpyi(lons_lat,levs+1)
      real tki (lons_lat,levs+1),tkci(lons_lat,levs+1)
      real dpp(lons_lat,levs),rpp(lons_lat,levs)
      real dlnpx(lons_lat,levs),dlnpy(lons_lat,levs)
      real dphix (lons_lat,levs),dphiy (lons_lat,levs)
      real dphixk(lons_lat,levs),dphiyk(lons_lat,levs)
      real wflx(lons_lat,levs+1)
      real wf(lons_lat,levs+1),wf1(lons_lat,levs+1)
      real wml(lons_lat,levs),wmm(lons_lat,levs),wmu(lons_lat,levs)
      real work(lons_lat,levs)
      real dup(lons_lat,levs),dum(lons_lat,levs)
      real ppl(lons_lat,levs),ppi(lons_lat,levs+1)
      real pil(lons_lat,levs)
!     real seta(lons_lat,levs)
      real alpha(lons_lat,levs),betta(lons_lat,levs)
      real gamma(lons_lat,levs),delta(lons_lat,levs)
      real cons0,cons0p5, cons1,cons2,dt2,rdt2
      integer nvars
      integer levsb,mass

c
!     print *,' enter  gfidi_hyb_gc_ndsl '
c
!     call mymaxmin(uum,lons_lat,lon_dim,levs,' uum ')
!     call mymaxmin(ug ,lons_lat,lon_dim,levs,' ug  ')
!     call mymaxmin(vvm,lons_lat,lon_dim,levs,' vvm ')
!     call mymaxmin(vg ,lons_lat,lon_dim,levs,' vg  ')
!     call mymaxmin(ttm,lons_lat,lon_dim,levs,' ttm ')
!     call mymaxmin(tg ,lons_lat,lon_dim,levs,' tg  ')
!     call mymaxmin(rrm,lons_lat,lon_dim,levh,' rrm ')
!     call mymaxmin(rqg,lons_lat,lon_dim,levh,' rqg ')
!     call mymaxmin(psm,lons_lat,lon_dim,1   ,' psm ')
!     call mymaxmin(ps ,lons_lat,lon_dim,1   ,' ps  ')

!
! -------- prepare coriolis and gaussian weighting 
!
      cons0   = 0.d0      !constant
      cons0p5 = 0.5d0     !constant
      cons1   = 1.d0      !constant
      cons2   = 2.d0      !constant
       dt2    = cons2*deltim
      rdt2    = 1./dt2
      cpvocpd1=cvap/cp-cons1
      kappa   = rd / cp
      rkappa  = cp / rd

      sinra=sqrt(cons1-cons1/rcl)     !constant
      coriol=cons2*omega*sinra          !constant
      sinra=sinra/rerth
                                                                                
      if(lat.gt.latg2) then
      coriol=-coriol
      sinra=-sinra
      endif
!
! max wind
!
      spdmax=cons0
      do k=1,levs
       do i=1,lons_lat
        ek(i,k)= (  ug(i,k)*ug(i,k)+vg(i,k)*vg(i,k) ) * rcl
        spdmax(k)=max( ek(i,k),spdmax(k) )
       enddo
      enddo
!
! ----- prepare dpxi, dpyi
!
      tki(:,1) = 0.0
      tkci(:,1)= 0.0
      tki(:,levs+1) = 0.0
      tkci(:,levs+1)= 0.0
      do k=2,levs
        do i=1,lons_lat
          tkrt0 = (tg(i,k-1)+tg(i,k))/(thref(k-1)+thref(k))
          tki (i,k)=ck5(k)*tkrt0**rkappa
          tkci(i,k)=tki(i,k)*rkappa/(tg(i,k-1)+tg(i,k))
        enddo
      enddo
      do k=1,levs+1
        do i=1,lons_lat
          ppi(i,k)  =ak5(k  )+bk5(k  )*ps(i)+tki(i,k)
        enddo
      enddo
      do k=1,levs
        do i=1,lons_lat
          ppl(i,k)  = cons0p5 * ( ppi(i,k) + ppi(i,k+1) )
          pil(i,k)  = ( ppl(i,k) * 0.01 ) ** kappa
          rpp(i,k)=cons1/(ppi(i,k)+ppi(i,k+1))
          dpp(i,k)=ppi(i,k)-ppi(i,k+1)
          rdelp (i,k)  = cons1/dpp(i,k)
          rdelp2 (i,k) = cons0p5/dpp(i,k)
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
          dpxi(i,k)=dpxi(i,k)+tkci(i,k)*(dtdl(i,k-1)+dtdl(i,k))
          dpyi(i,k)=dpyi(i,k)+tkci(i,k)*(dtdf(i,k-1)+dtdf(i,k))
        enddo
      enddo
c
! ----- prepare cg and fb
c
      do k=1,levs
        do i=1,lons_lat
!         fg(i,k)=ug(i,k)*(dpxi(i,k)-dpxi(i,k+1))
!    &           +vg(i,k)*(dpyi(i,k)-dpyi(i,k+1))
!    &           +dpp(i,k)*dg(i,k)
!         cg(i,k)=ug(i,k)*(dpxi(i,k)+dpxi(i,k+1))
!    &           +vg(i,k)*(dpyi(i,k)+dpyi(i,k+1))
! ndsl
          fg(i,k)=-ddpdt(i,k)
     &           +dpp(i,k)*dg(i,k)
          cg(i,k)=-dppdt(i,k)
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

!     call mymaxmin(dpsdt,lons_lat,lon_dim,1,' dpsdt ')
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
          dphixk(i,k)= rpp(i,k)*( dpp(i,k)*dtdl(i,k)
     &                  +tg(i,k)*(dpxi(i,k)-dpxi(i,k+1))
     &-rpp(i,k)*dpp(i,k)*tg(i,k)*(dpxi(i,k)+dpxi(i,k+1)) )
          dphiyk(i,k)= rpp(i,k)*( dpp(i,k)*dtdf(i,k)
     &                  +tg(i,k)*(dpyi(i,k)-dpyi(i,k+1))
     &-rpp(i,k)*dpp(i,k)*tg(i,k)*(dpyi(i,k)+dpyi(i,k+1)) )
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
          dphix(i,k)= rd * dphix(i,k) / rcl
          dphiy(i,k)= rd * dphiy(i,k) / rcl
        enddo
      enddo
! if test
c horizontal advection for all
c
!     do k=1,levs
!       do i=1,lons_lat
!         dudt(i,k)=-ug(i,k)*dudl(i,k)-vg(i,k)*dudf(i,k)
!         dvdt(i,k)=-ug(i,k)*dvdl(i,k)-vg(i,k)*dvdf(i,k)
!         dtdt(i,k)=-ug(i,k)*dtdl(i,k)-vg(i,k)*dtdf(i,k)
!       enddo
!     enddo
!     do n=1,ntrac
!     do k=1,levs
!       do i=1,lons_lat
!         drdt(i,k,n)=
!    &               -ug(i,k)*drdl(i,k,n)-vg(i,k)*drdf(i,k,n)
!       enddo
!     enddo
!     enddo
!
c total derivative of horizontal wind
c
      do k=1,levs
       do i=1,lons_lat
         dudt(i,k)=        dudt(i,k) 
     &                   - rd *tg(i,k)*dlnpx(i,k)
     &                   - dphix(i,k)
     &                   + vg(i,k)*coriol
         dvdt(i,k)=        dvdt(i,k) 
     &                   - rd *tg(i,k)*dlnpy(i,k)
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
         dtdt(i,k)=  dtdt(i,k) 
     &              +kappa*tg(i,k)*rpp(i,k)/
     &              (cons1+cpvocpd1*rqg(i,k,1)) *
     &                       (cg(i,k)-fb(i,k)-fb(i,k+1))
       enddo
      enddo
!
! ------ hybrid to solve vertical flux ----------
!
      alpha(:,levs)=0.0
      betta(:,1)=0.0
      do k=2,levs
        do i=1,lons_lat
          alpha(i,k-1)=(ppi(i,k)+ppi(i,k+1))/(ppi(i,k-1)+ppi(i,k))
          alpha(i,k-1)=alpha(i,k-1)**kappa
        enddo
      enddo
      do k=1,levs-1
        do i=1,lons_lat
          betta(i,k+1)=(ppi(i,k)+ppi(i,k+1))/(ppi(i,k+1)+ppi(i,k+2))
          betta(i,k+1)=betta(i,k+1)**kappa
        enddo
      enddo
      do k=1,levs
        do i=1,lons_lat
          gamma(i,k)=1.0-kappa*dpp(i,k)*rpp(i,k)*2.0
          delta(i,k)=1.0+kappa*dpp(i,k)*rpp(i,k)*2.0
        enddo
      enddo

      do i=1,lons_lat
        dup(i,levs)=0.0
        dum(i,1 )=0.0
      enddo
      do k=1,levs-1
        do i=1,lons_lat
          dup(i,k  )=delta(i,k)*tg(i,k)-betta(i,k+1)*tg(i,k+1)
          dum(i,k+1)=alpha(i,k)*tg(i,k)-gamma(i,k+1)*tg(i,k+1)
        enddo
      enddo
!
      levsb=levs
! turn off following in case of sigma-theta-pressure 11/17/2006 hmhj
!hmhj do k=levs,2,-1
!hmhj   if( bk5(k).eq.0.0 .and. ck5(k).ne.0.0 ) levsb=k-1
!hmhj enddo
      k=2
        do i=1,lons_lat
          wmkm1=tkci(i,k)*rdelp2(i,k-1)
          wmkp1=tkci(i,k)*rdelp2(i,  k)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
          wmu(i,k-1)=wmkp1*dup(i,k)
        enddo
      do k=3,levs-1
        do i=1,lons_lat
          wmkm1=tkci(i,k)*rdelp2(i,k-1)
          wmkp1=tkci(i,k)*rdelp2(i,  k)
          wml(i,k-2)=wmkm1*dum(i,k-1)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
          wmu(i,k-1)=wmkp1*dup(i,k)
        enddo
      enddo
      k=levs
        do i=1,lons_lat
          wmkm1=tkci(i,k)*rdelp2(i,k-1)
          wmkp1=tkci(i,k)*rdelp2(i,  k)
          wml(i,k-2)=wmkm1*dum(i,k-1)
          wmm(i,k-1)=wmkm1*dup(i,k-1)+wmkp1*dum(i,k)-1.0
        enddo
!hmhj wf(:,levs:levs+1)=0.0
!hmhj do k=2,levs
      wf(:,levsb:levs+1)=0.0
      do k=2,levsb
        do i=1,lons_lat
          wf(i,k-1)=bk5(k)*dpsdt(i)+fb(i,k)
     &              +tkci(i,k)*(dtdt(i,k-1)+dtdt(i,k))
        enddo
      enddo
      call tridim_hyb_gc_ndsl(lons_lat,lons_lat,levsb-1,levs+1,1,
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
!     call mymaxmin(wflx,lons_lat,lons_lat,levs+1,' wflx ')
!
! ------ vertical advection for all --------
! ====================================================
! for ndsl, update n+1*, then do vertical advection
!
      nvars = 3 + ntrac
!
! update u v and rq 
      do k=1,levs
       do i=1,lons_lat
        qq(i,k,1) = uum(i,k) + dudt(i,k) * dt2
        qq(i,k,2) = vvm(i,k) + dvdt(i,k) * dt2
        qq(i,k,3) = ttm(i,k) + dtdt(i,k) * dt2
! use seta
!       qq(i,k,3) = qq(i,k,3) / pil(i,k)
!       seta(i,k) = qq(i,k,3)
       enddo
      enddo
      do n=1,ntrac
      do k=1,levs
       do i=1,lons_lat
        qq(i,k,3+n) = rrm(i,k,n) + drdt(i,k,n) * dt2
       enddo
      enddo
      enddo
!
! do vertical advection from (n+1)* to (n+1)
      mass=0
      call vertical_ppm_advect (lons_lat,lons_lat,levs,nvars,
     &            deltim,ppi,wflx,qq,mass)

!
! recompute tendency from (n+1) - (n-1)
      do k=1,levs
       do i=1,lons_lat
        dudt(i,k) = (qq(i,k,1) - uum(i,k)) * rdt2
        dvdt(i,k) = (qq(i,k,2) - vvm(i,k)) * rdt2
        dtdt(i,k) = (qq(i,k,3) - ttm(i,k)) * rdt2
! use seta
!       dtdt(i,k) = dtdt(i,k) + pil(i,k)*(qq(i,k,3)-seta(i,k))*rdt2
!    &             - kappa*tg(i,k)*rpp(i,k)*(wflx(i,k)+wflx(i,k+1))
       enddo
      enddo
      do n=1,ntrac
      do k=1,levs
       do i=1,lons_lat
        drdt(i,k,n) = (qq(i,k,3+n) - rrm(i,k,n)) * rdt2
       enddo
      enddo
      enddo

!     call mymaxmin(dudt,lons_lat,lon_dim,levs,' dudt ')
!     call mymaxmin(dvdt,lons_lat,lon_dim,levs,' dvdt ')
!     call mymaxmin(dtdt,lons_lat,lon_dim,levs,' dtdt ')
!     call mymaxmin(drdt,lons_lat,lon_dim,levh,' drdt ')
!     call mymaxmin(dpsdt,lons_lat,lon_dim,1,' dpsdt ')

!! this multiplication must be on  completed tendencies.
      do k=1,levs
      do i=1,lons_lat
        dudt(i,k)=dudt(i,k)*rcl
        dvdt(i,k)=dvdt(i,k)*rcl
      enddo
      enddo

!     print *,' end of gfidi_hyb_gc_ndsl. '
!!

      return
      end


c-----------------------------------------------------------------------
      subroutine tridim_hyb_gc_ndsl(l,lx,n,nx,m,cl,cm,cu,r,au,a)
c                .      .    .                                       .
c subprogram:    tridim_hyb      solves tridiagonal matrix problems.
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
c usage:    call tridim_hyb_gc_ndsl(l,lx,n,nx,m,cl,cm,cu,r,au,a)
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
