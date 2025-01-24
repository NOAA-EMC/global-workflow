subroutine prewgt_reg(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prewgt_reg  setup bkerror
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: setup smoothing and grid transform for bkerror
!
! program history log:
!   2000-03-15  wu
!   2004-08-03  treadon - add only to module use; add intent in/out;
!                         fix bug in which rdgstat_reg inadvertently
!                         recomputed sigl (s/b done in gridmod)
!   2004-10-26  wu - include factors hzscl in the range of RF table
!   2004-11-16  treadon - add longitude dimension to variance array dssv
!   2004-11-20  derber - modify to make horizontal table more reproducable and
!                        move most of table calculations to berror
!   2005-01-22  parrish - split out balance variables to subroutine prebal_reg
!                         contained in module balmod.f90.  change wlat,
!                         lmin, lmax to rllat, llmin, llmax and add
!                         "use balmod" to connect to rllat,llmin,llmax
!   2005-02-23  wu - setup background variance for qoption=2
!   2005-03-28  wu - replace mlath with mlat and modify dim of corz, corp
!   2005-07-15  wu - remove old print out, add max bound to lp
!   2005-11-29  derber - unify ozone variance calculation
!   2006-01-11  kleist - place upper/lower bounds on qoption=2 variance
!   2006-01-31  treadon - invert hzscl
!   2006-04-17  treadon - use rlsig from call rdgstat_reg; replace sigl
!                         with ges_prslavg/ges_psfcavg
!   2007-05-30  h.liu - remove ozmz
!   2008-04-23  safford - rm unused uses and vars
!   2010-03-12  zhu     - move interpolations of dssv and dssvs into this subroutine
!                       - move varq & factoz to berror_read_wgt_reg
!                       - add changes using nrf* for generalized control variables
!   2010-03-15  zhu     - move the calculation of compute_qvar3d here
!   2010-04-10  parrish - remove rhgues, no longer used
!   2010-04-29  wu      - set up background error for oz
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-06-01  todling - rename as,tsfc_sdv to as2d,as3d,atsfc_sdv (alloc now)
!   2010-06-03  todling - protect dssvs w/ mvars check
!   2010-07-31  parrish - replace mpi_allreduce used for getting ozone background error with
!                          mpl_allreduce, and introduce r_quad arithmetic to remove dependency of
!                          results on number of tasks.  This is the same strategy currently used
!                          in dot_product (see control_vectors.f90).
!   2012-12-15  zhu     - add treatment of dssv for cw for all-sky radiance
!   2013-01-22  parrish - initialize kb=0, in case regional_ozone is false.
!                          (fixes WCOSS debug compile error)
!
!   2013-04-17  wu      - use nnnn1o to deside whether to define B related veriables
!                         avoid undefined input when number of tasks is larger than
!                         that of the total levels of control vectors
!   2013-10-19  todling - all guess variables in met-guess
!   2014-02-03  todling - update interface to berror_read_wgt_reg
!   2016-09-xx  CAPS(G. Zhao)  - tuning background error stats for qr/qs/qg to use dbz
!   2017-02-xx  CAPS(G. Zhao)  - add temperature-dependent background error for cloud variables
!   2018-10-23  CAPS(C. Liu)   - add w
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
!   other important variables
!     nsig     - number of sigma levels
!     nx       - number of gaussian lats in one hemisphere
!     ny       - number of longitudes
!     dx       - cos of grid latitudes (radians)
!   agv,wgv,bv - balance correlation matrix for t,p,div
!      sli     - scale info for the 3 subdomain
!     alv,dssv - vertical smoother coef.

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use balmod, only: rllat,rllat1,llmin,llmax
  use berror, only: dssvs,&
       bw,ny,nx,dssv,vs,be,ndeg,&
       init_rftable,hzscl,slw,nhscrf,bkgv_write
  use mpimod, only: nvar_id,levs_id,mpi_sum,mpi_comm_world,mpi_rtype
  use jfunc, only: varq,qoption,varcw,cwoption
  use control_vectors, only: cvars2d,cvars3d
  use control_vectors, only: as2d,as3d,atsfc_sdv
  use control_vectors, only: nrf,nc3d,nc2d,nvars,mvars !_RT ,nrf3_loc,nrf2_loc,nrf_var
  use control_vectors, only: cvars => nrf_var
  use gridmod, only: lon2,lat2,nsig,nnnn1o,regional_ozone,&
       region_dx,region_dy,nlon,nlat,istart,jstart,region_lat
  use constants, only: zero,half,one,two,four,rad2deg,zero_quad
  use guess_grids, only: ges_prslavg,ges_psfcavg
  use m_berror_stats_reg, only: berror_get_dims_reg,berror_read_wgt_reg
  use mpeu_util, only: getindex
  use mpl_allreducemod, only: mpl_allreduce
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use mpeu_util, only: die
  use guess_grids, only: ges_tsen                      ! for t-depn't err vars
  use guess_grids, only: nfldsig
  use directDA_radaruse_mod, only: be_sf,hscl_sf, vscl_sf, be_vp,hscl_vp, vscl_vp, &
                               be_t, hscl_t,  vscl_t,  be_q, hscl_q,  vscl_q,&
                               be_qr, be_qs, be_qg, hscl_qx, vscl_qx,&
                               l_set_be_rw, l_set_be_dbz,&
                               l_use_cvpqx, cvpqx_pval,&
                               l_cvpnr, cvpnr_pval,&       
                               l_plt_be_stats, l_be_T_dep

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
! real(r_kind),parameter:: six          = 6.0_r_kind
! real(r_kind),parameter:: eight        = 8.0_r_kind
  real(r_kind),parameter:: r400000      = 400000.0_r_kind
  real(r_kind),parameter:: r800000      = 800000.0_r_kind
  real(r_kind),parameter:: r015         = 0.15_r_kind


! Declare local variables
  integer(i_kind) k,i,ii
  integer(i_kind) n,nn,nsig180
  integer(i_kind) j,k1,loc,kb,mm1,ix,jl,il
  integer(i_kind) inerr,l,lp,l2
  integer(i_kind) msig,mlat              ! stats dimensions
  integer(i_kind),dimension(nnnn1o):: ks
  integer(i_kind) nrf3_oz,nrf2_sst,nrf3_cw,istatus
  integer(i_kind),allocatable,dimension(:) :: nrf3_loc,nrf2_loc

  integer(i_kind) nrf3_sf,nrf3_vp,nrf3_t,nrf3_q
  integer(i_kind) :: nrf3_ql,nrf3_qi, nrf3_qr,nrf3_qs,nrf3_qg, nrf3_qnr, nrf3_w

  real(r_kind),allocatable,dimension(:,:,:):: vz4plt

  real(r_kind),allocatable,dimension(:):: vz_cld
  real(r_kind),allocatable,dimension(:,:,:):: dsv_cld       ! lon2,nsig,lat2
  real(r_kind),allocatable,dimension(:,:,:,:):: corz_cld    ! lon2,lat2,nsig,3(qr/qs/qg)

  integer(i_kind) :: inerr_out              ! output of berror_var for NCL plotting
  real(r_kind), parameter :: Tbar=290.0_r_kind
  real(r_kind), parameter :: three_eighths=3.0_r_kind/8.0_r_kind
  real(r_kind) :: tsen
  integer(i_kind) :: mid_mlat
  integer(i_kind) :: mid_nsig

  real(r_kind)      :: corz_sf, corz_vp
  real(r_kind)      :: max_sf, min_sf, ave_sf
  real(r_kind)      :: max_vp, min_vp, ave_vp

  external          :: berror_qcld_tdep

  real(r_kind) samp2,dl1,dl2,d
  real(r_kind) samp,hwl,cc
  real(r_kind),dimension(nsig):: rate,rlsig
  real(r_kind),dimension(nsig,nsig):: turn
  real(r_kind),dimension(ny,nx)::sl
  real(r_kind) fact,psfc015

  real(r_kind),dimension(lon2,nsig,llmin:llmax):: dsv
  real(r_kind),dimension(lon2,llmin:llmax):: dsvs

  real(r_kind),allocatable,dimension(:,:):: corp, hwllp
  real(r_kind),allocatable,dimension(:,:,:):: corz, hwll, vz
  real(r_kind),allocatable,dimension(:,:,:,:)::sli
  real(r_quad),dimension(180,nsig):: ozmz,cnt
  real(r_quad),dimension(180*nsig):: ozmz0,cnt0

  real(r_kind),dimension(:,:,:),pointer::ges_oz=>NULL()

! Initialize local variables
!  do j=1,nx
!     do i=1,ny
!        dx(i,j)=region_dx(i,j)
!        dy(i,j)=region_dy(i,j)
!     end do
!  end do

! Setup sea-land mask
  sl=one
!  do j=1,nx
!     do i=1,ny
!        sl(i,j)=min(max(sl(i,j),zero),one)
!     enddo
!  enddo

! Get required indexes from CV var names
  nrf3_oz  = getindex(cvars3d,'oz')
  nrf3_cw  = getindex(cvars3d,'cw')
  nrf2_sst = getindex(cvars2d,'sst')
  nrf3_sf  = getindex(cvars3d,'sf')
  nrf3_vp  = getindex(cvars3d,'vp')
  nrf3_t   = getindex(cvars3d,'t')
  nrf3_q   = getindex(cvars3d,'q')

!   cloud fields
  nrf3_ql  =getindex(cvars3d,'ql')
  nrf3_qi  =getindex(cvars3d,'qi')
  nrf3_qr  =getindex(cvars3d,'qr')
  nrf3_qs  =getindex(cvars3d,'qs')
  nrf3_qg  =getindex(cvars3d,'qg')
  nrf3_qnr =getindex(cvars3d,'qnr')
  nrf3_w   =getindex(cvars3d,'w')

! Read dimension of stats file
  inerr=22
  call berror_get_dims_reg(msig,mlat,inerr)

! Allocate arrays in stats file
  allocate ( corz(1:mlat,1:nsig,1:nc3d) )
  allocate ( corp(1:mlat,nvars-nc3d) )
  allocate ( hwll(0:mlat+1,1:nsig,1:nc3d),hwllp(0:mlat+1,nvars-nc3d) )
  allocate ( vz(1:nsig,0:mlat+1,1:nc3d) )

! Arrays used for temperature-dependent error variance when assimilation radar dbz obs
  if (l_be_T_dep) then
      allocate ( vz_cld(1:nsig) )                      ; vz_cld   = zero;
      allocate ( dsv_cld(1:lon2, 1:nsig, 1:lat2) )     ; dsv_cld  = zero;
      allocate ( corz_cld(1:lon2, 1:lat2, 1:nsig, 4) ) ; corz_cld = zero;
  end if

! Read in background error stats and interpolate in vertical to that specified in namelist
  call berror_read_wgt_reg(msig,mlat,corz,corp,hwll,hwllp,vz,rlsig,varq,qoption,varcw,cwoption,mype,inerr)

! find ozmz for background error variance
  kb=0
  if(regional_ozone) then

     call gsi_bundlegetpointer (gsi_metguess_bundle(1),'oz',ges_oz,istatus)
     if(istatus/=0) call die('prewgt_reg',': missing oz in metguess, aborting ',istatus)

     kb_loop: do k=1,nsig
        if(rlsig(k) <  log(0.35_r_kind))then
           kb=k
           exit kb_loop
        endif
     enddo kb_loop
     mm1=mype+1

     ozmz=zero_quad
     cnt=zero_quad
     do k=1,nsig
        do j=2,lon2-1
           jl=j+jstart(mm1)-2
           jl=min0(max0(1,jl),nlon)
           do i=2,lat2-1
              il=i+istart(mm1)-2
              il=min0(max0(1,il),nlat)
              ix=region_lat(il,jl)*rad2deg+half+90._r_kind
              ozmz(ix,k)=ozmz(ix,k)+ges_oz(i,j,k)*ges_oz(i,j,k)
              cnt(ix,k)=cnt(ix,k)+one
           end do
        end do
     end do
     i=0
     do k=1,nsig
        do ix=1,180
           i=i+1
           ozmz0(i)=ozmz(ix,k)
           cnt0(i)=cnt(ix,k)
        end do
     end do
     nsig180=180*nsig
     call mpl_allreduce(nsig180,qpvals=ozmz0)
     call mpl_allreduce(nsig180,qpvals=cnt0)
     i=0
     do k=1,nsig
        do ix=1,180
           i=i+1
           ozmz(ix,k)=ozmz0(i)
           cnt(ix,k)=cnt0(i)
        end do
     end do
     do k=1,nsig
        do i=1,180
           if(cnt(i,k)>zero) ozmz(i,k)=sqrt(ozmz(i,k)/cnt(i,k))
        enddo
     enddo
  endif ! regional_ozone

! As used in the code, the horizontal length scale
! parameters are used in an inverted form.  Invert
! the parameter values here.
  do i=1,nhscrf
     if (l_set_be_rw .or. l_set_be_dbz ) hzscl(i) = one
     hzscl(i)=one/hzscl(i)
  end do

! apply scaling to vertical length scales.  
! note:  parameter vs needs to be inverted
  if (l_set_be_rw .or. l_set_be_dbz) vs = one
  vs=one/vs
  vz=vz*vs

!------------------------------------------------------------------------------!
! special treatment for hwll(horizontal), vz(vertical scale length)
!         and corz(bkgd err) of u/v/t/q when using radial wind observations of
!         radar
  if ( l_set_be_rw ) then
      do n=1,nc3d
          if (n==nrf3_q) then
              if (mype==0) write(6,*)'PREWGT_REG: tuning BE for q (pe=',mype,')'
              if (    be_q > 0.0_r_kind ) corz(:,:,n) = corz(:,:,n) * be_q ! error stddev for Q
              if (  hscl_q > 0.0_r_kind ) hwll(:,:,n) = hscl_q
              if (  vscl_q > 0.0_r_kind ) vz(  :,:,n) = vscl_q
          end if
          if (n==nrf3_t) then
              if (mype==0) write(6,*)'PREWGT_REG: tuning BE for t (pe=',mype,')'
              if (    be_t > 0.0_r_kind ) corz(:,:,n) = corz(:,:,n) * be_t ! error stddev for t
              if (  hscl_t > 0.0_r_kind ) hwll(:,:,n) = hscl_t
              if (  vscl_t > 0.0_r_kind ) vz(  :,:,n) = vscl_t
          end if
          if (n==nrf3_sf) then
              max_sf  = maxval(corz(:,:,n))
              min_sf  = minval(corz(:,:,n))
              ave_sf  = sum(corz(:,:,n))/(mlat*nsig)
              if (  be_sf > 0.0_r_kind ) then
                  corz_sf = ave_sf * be_sf
                  corz(:,:,n) = corz_sf                ! error stddev for sf (compensate for change in scale)
                  if (mype==0) then
                      write(6,'(1x,A15,I4,A50,4(1x,F15.2),1x,A12)')                         &
                          '(PREWGT_REG:pe=',mype,') stream function err std max min ave:',  &
                          max_sf, min_sf, ave_sf, corz_sf,' (m^2/s^2)'
                      write(6,'(1x,A15,I4,A70,F15.6,A6,I3,1x,A6)')                          &
                          '(PREWGT_REG:pe=',mype,                                           &
                          ') inflate  the pre-fixed err_var of sf (streamfunction) by ',    &
                          be_sf,'   n=', n,cvars3d(n)
                  end if
              end if

              if (mype==0) &
                  write(6,'(1x,A15,I4,A70,F9.1,F9.2,A6,I3,1x,A6)')                          &
                  '(PREWGT_REG:pe=',mype,                                                   &
                  ') re-set the length-scale(hor ver) of sf (stream function): ',           &
                  hscl_sf, vscl_sf,'   n=', n,cvars3d(n)
              if ( hscl_sf .gt. 0.0_r_kind ) hwll(:,:,n) = hscl_sf
              if ( vscl_sf .gt. 0.0_r_kind ) vz(  :,:,n) = vscl_sf
          end if
          if (n==nrf3_vp) then
              max_vp  = maxval(corz(:,:,n))
              min_vp  = minval(corz(:,:,n))
              ave_vp  = sum(corz(:,:,n))/(mlat*nsig)
              if (  be_vp > 0.0_r_kind ) then
                  corz_vp = ave_vp * be_vp
                  corz(:,:,n) = corz_vp                ! error stddev for vp (compensate for change in scale)
                  if (mype == 0) then
                      write(6,'(1x,A15,I4,A50,4(1x,F15.2),1x,A12)')                           &
                          '(PREWGT_REG:pe=',mype,') velocity potential err std max min ave:', &
                          max_vp, min_vp, ave_vp, corz_vp,' (m^2/s^2)'
                      write(6,'(1x,A15,I4,A70,F15.6,A6,I3,1x,A6)')                            &
                          '(PREWGT_REG:pe=',mype,                                             &
                          ') inflate  the pre-fixed err_var of vp (VelPotent) by ',           &
                          be_vp,'   n=', n,cvars3d(n)
                  end if
              end if

              if (mype == 0)                                                                 &
                  write(6,'(1x,A15,I4,A70,F9.1,F9.2,A6,I3,1x,A6)')                           &
                  '(PREWGT_REG:pe=',mype,                                                    &
                  ') re-set the length-scale(hor ver) of vp (VelPotent): ',                  &
                  hscl_vp, vscl_vp,'   n=', n,cvars3d(n)
              if ( hscl_vp > 0.0_r_kind ) hwll(:,:,n) = hscl_vp
              if ( vscl_vp > 0.0_r_kind ) vz(  :,:,n) = vscl_vp

          end if
      end do
  else
      if (mype==0) write(6,'(1x,A15,I4,A80)')                                                &
          '(PREWGT_REG:pe=',mype,                                                            &
          ') DO NOT RE-SET the BACKGROUND ERROR for radar wind assimilation.'
  end if

!------------------------------------------------------------------------------!
! special treatment for hwll(horizontal), vz(vertical scale length)
!         and corz(bkgd err) of cloud hydrometers (qr/qs/qg) when using radar reflectivity observations
  if ( l_set_be_dbz ) then
      do n=1,nc3d
          if (n==nrf3_qr ) then
              if (mype==0) &
                  write(6,'(1x,A60,I4)')'PREWGT_REG: user-defined namelist to tune BE for qr on pe:',mype
              if (    be_qr > 0.0_r_kind ) corz(:,:,n) = be_qr
              if (  hscl_qx > 0.0_r_kind ) hwll(:,:,n) = hscl_qx
              if (  vscl_qx > 0.0_r_kind ) vz(  :,:,n) = vscl_qx
          end if
          if (n==nrf3_qs ) then
              if (mype==0) &
                  write(6,'(1x,A60,I4)')'PREWGT_REG: user-defined namelist to tune BE for qs on pe:',mype
              if (    be_qs > 0.0_r_kind ) corz(:,:,n) = be_qs
              if (  hscl_qx > 0.0_r_kind ) hwll(:,:,n) = hscl_qx
              if (  vscl_qx > 0.0_r_kind ) vz(  :,:,n) = vscl_qx
          end if
          if (n==nrf3_qg ) then
              if (mype==0) &
                  write(6,'(1x,A60,I4)')'PREWGT_REG: user-defined namelist to tune BE for qg on pe:',mype
              if (    be_qg > 0.0_r_kind ) corz(:,:,n) = be_qg
              if (  hscl_qx > 0.0_r_kind ) hwll(:,:,n) = hscl_qx
              if (  vscl_qx > 0.0_r_kind ) vz(  :,:,n) = vscl_qx
          end if
          if (n==nrf3_qnr ) then
              if (mype==0) &
                  write(6,'(1x,A60,I4)')'PREWGT_REG: user-defined namelist to tune BE for qnr on pe:',mype
                                              corz(:,:,n) = 100.0_r_kind
              if (  hscl_qx > 0.0_r_kind ) hwll(:,:,n) = hscl_qx
              if (  vscl_qx > 0.0_r_kind ) vz(  :,:,n) = vscl_qx
          end if

          if (n==nrf3_w ) then
              if (mype==0) &
                  write(6,'(1x,A60,I4)')'PREWGT_REG: user-defined namelist to tune BE for w on pe:',mype
                                              corz(:,:,n) = 3.0_r_kind
              if (  hscl_qx > 0.0_r_kind ) hwll(:,:,n) = hscl_qx
              if (  vscl_qx > 0.0_r_kind ) vz(  :,:,n) = vscl_qx
          end if
      end do
  else
      if (mype==0) write(6,'(1x,A15,I4,A80)')                                  &
          '(PREWGT_REG:pe=',mype,                                              &
          ') DO NOT RE-SET the BACKGROUND ERROR for radar dbz assimilation.'
  end if

!------------------------------------------------------------------------------!

  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

  allocate(nrf3_loc(nc3d),nrf2_loc(nc2d))
  do ii=1,nc3d
     nrf3_loc(ii)=getindex(cvars,cvars3d(ii))
  enddo
  do ii=1,nc2d
     nrf2_loc(ii)=getindex(cvars,cvars2d(ii))
  enddo

  do n=1,nc3d
     if(n==nrf3_oz .and. regional_ozone)then   ! spetial treament for ozone variance
        loc=nrf3_loc(n)
        vz(:,:,n)=1.5_r_kind   ! ozone vertical scale fixed
        do j=llmin,llmax
           call smoothzo(vz(1,j,n),samp,rate,n,j,dsv(1,1,j))
           do k=1,nsig
              do i=1,lon2
                 dsv(i,k,j)=dsv(i,k,j)*as3d(n)
              end do
           end do
        end do
        do j=1,lon2
           jl=j+jstart(mm1)-2
           jl=min0(max0(1,jl),nlon)
           do i=1,lat2
              il=i+istart(mm1)-2
              il=min0(max0(1,il),nlat)
              d=region_lat(il,jl)*rad2deg+90._r_kind
              l=int(d)
              l2=l+1
              dl2=d-real(l,r_kind)
              dl1=one-dl2
              do k=1,nsig
                 dssv(i,j,k,n)=(dl1*ozmz(l,k)+dl2*ozmz(l2,k))*dsv(1,k,llmin)
              end do
           end do
        end do
     else if ( n==nrf3_qr .or. n==nrf3_qs .or. n==nrf3_qg .or. n==nrf3_qnr) then

        if ( l_be_T_dep ) then
            if (mype==0) &
                write(6,*)' prewgt_reg(mype=',mype,')',' Temperature-dependent erro vars for Q_cld and as3d= ', &
                          as3d(n),' for var:',cvars3d(n)
            loc=nrf3_loc(n)

!           re-define vz_cld on model sigma grid for cloud variabels (not on stats grid)
!           to match the re-defined corz_cld, which is also defined on model grid
!           because the error variables of cloud variable is modle/background temperature
!           dependent.
!           only initialization of alv in subroutine smoothzo
            do j=llmin,llmax
                call smoothzo(vz(1,j,n),samp,rate,n,j,dsv(1,1,j))
            end do

            do j=1,lat2
                mid_nsig = int((nsig+1)/2)
                mid_mlat = int((llmin+llmax)/2)
                vz_cld(1:nsig) = vz(1:nsig,mid_mlat,n)    !
                call smoothzo1(vz_cld(1:nsig),samp,rate,dsv_cld(1:lon2,1:nsig,j))
                do i=1,lon2
                    do k=1,nsig
                        tsen=ges_tsen(j,i,k,nfldsig)
                        if (n==nrf3_qr) then
                            if( l_use_cvpqx .and. cvpqx_pval==0.5_r_kind) then
                                 call calc_corz_cld_qr(tsen, cvpqx_pval, corz_cld(i,j,k,1))
                                 dssv(j,i,k,n)=dsv_cld(i,k,j) *0.05_r_kind*corz_cld(i,j,k,1) * as3d(n)       
                            else if( l_use_cvpqx .and. cvpqx_pval==1.0_r_kind) then
                                 call calc_corz_cld_qr(tsen, cvpqx_pval, corz_cld(i,j,k,1))
                                 dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,1) * as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==0.000001_r_kind) then
                                 call calc_corz_cld_qr(tsen, cvpqx_pval, corz_cld(i,j,k,1))
                                 dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,1) * as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==0.4_r_kind) then
                                 call calc_corz_cld_qr(tsen, cvpqx_pval, corz_cld(i,j,k,1))
                                 dssv(j,i,k,n)=dsv_cld(i,k,j) *0.05_r_kind* corz_cld(i,j,k,1) * as3d(n)     
                            else
                              call berror_qcld_tdep(mype,tsen,1,corz_cld(i,j,k,1))
                              dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,1) * as3d(n)
                            end if
                        else if (n==nrf3_qs) then
                            if( l_use_cvpqx .and. cvpqx_pval==0.5_r_kind) then
                                call calc_corz_cld_qs(tsen, cvpqx_pval, corz_cld(i,j,k,2))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * 0.05_r_kind*corz_cld(i,j,k,2) *as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==1.0_r_kind) then
                                call calc_corz_cld_qs(tsen, cvpqx_pval, corz_cld(i,j,k,2))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,2) * as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==0.000001_r_kind) then
                                call calc_corz_cld_qs(tsen, cvpqx_pval, corz_cld(i,j,k,2))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,2) * as3d(n)                                   
                            else if( l_use_cvpqx .and. cvpqx_pval==0.4_r_kind) then
                                call calc_corz_cld_qs(tsen, cvpqx_pval, corz_cld(i,j,k,2))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * 0.05_r_kind*corz_cld(i,j,k,2) * as3d(n)
                            else
                              call berror_qcld_tdep(mype,tsen,2,corz_cld(i,j,k,2))
                              dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,2) * as3d(n)
                            end if
                        else if (n==nrf3_qg) then
                            if( l_use_cvpqx .and. cvpqx_pval==0.5_r_kind) then
                                call calc_corz_cld_qg(tsen, cvpqx_pval, corz_cld(i,j,k,3))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) *0.05_r_kind* corz_cld(i,j,k,3) * as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==1.0_r_kind) then
                                call calc_corz_cld_qg(tsen, cvpqx_pval, corz_cld(i,j,k,3))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,3) * as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==0.000001_r_kind) then
                                call calc_corz_cld_qg(tsen, cvpqx_pval, corz_cld(i,j,k,3))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,3) * as3d(n)
                            else if( l_use_cvpqx .and. cvpqx_pval==0.4_r_kind) then
                                call calc_corz_cld_qg(tsen, cvpqx_pval, corz_cld(i,j,k,3))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) *0.05_r_kind* corz_cld(i,j,k,3) * as3d(n)                       
                            else
                              call berror_qcld_tdep(mype,tsen,3,corz_cld(i,j,k,3))
                              dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,3) *as3d(n)
                            end if

                        else if (n==nrf3_qnr) then
                            if( l_cvpnr .and. cvpnr_pval==0.6_r_kind) then   
                                call calc_corz_cld_qnr(tsen, cvpnr_pval,corz_cld(i,j,k,4))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) *0.05_r_kind*corz_cld(i,j,k,4) * as3d(n)
                            else if( l_cvpnr .and. cvpnr_pval==1.0_r_kind) then
                                call calc_corz_cld_qnr(tsen, cvpnr_pval,corz_cld(i,j,k,4))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,4)* as3d(n)
                            else if( l_cvpnr .and. cvpnr_pval==0.000001_r_kind) then
                                call calc_corz_cld_qnr(tsen, cvpnr_pval,corz_cld(i,j,k,4))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) * corz_cld(i,j,k,4)* as3d(n)
                            else if( l_cvpnr .and. cvpnr_pval==0.4_r_kind) then
                                call calc_corz_cld_qnr(tsen, cvpnr_pval,corz_cld(i,j,k,4))
                                dssv(j,i,k,n)=dsv_cld(i,k,j) *0.05_r_kind*corz_cld(i,j,k,4) * as3d(n)
                            else if( l_cvpnr .and. cvpnr_pval==0.2_r_kind) then
                                call calc_corz_cld_qnr(tsen,cvpnr_pval,corz_cld(i,j,k,4))
                                dssv(j,i,k,n)=dsv_cld(i,k,j)*0.05_r_kind*corz_cld(i,j,k,4) * as3d(n)
                            else if( l_cvpnr .and. cvpnr_pval==0.8_r_kind) then
                                call calc_corz_cld_qnr(tsen,cvpnr_pval,corz_cld(i,j,k,4))
                                dssv(j,i,k,n)=dsv_cld(i,k,j)*0.05_r_kind*corz_cld(i,j,k,4) * as3d(n)
                            end if
                        else
                            write(6,*) &
                                " prewgt_reg: T-dependent error only for rain/snwo/graupel --> wrong for ",n,cvars3d(n)
                            call stop2(999)
                        end if
                    end do

                end do
            end do

        else             ! if NOT T-dependent background error
            loc=nrf3_loc(n)
            do j=llmin,llmax
                call smoothzo(vz(1,j,n),samp,rate,n,j,dsv(1,1,j))
                do k=1,nsig
                    do i=1,lon2
                        dsv(i,k,j)=dsv(i,k,j)*corz(j,k,n)*as3d(n)
                    end do
                end do
            end do

            do j=1,lat2
                do i=1,lon2
                    l=int(rllat1(j,i))
                    l2=min0(l+1,llmax)
                    dl2=rllat1(j,i)-real(l,r_kind)
                    dl1=one-dl2
                    do k=1,nsig
                        dssv(j,i,k,n)=dl1*dsv(i,k,l)+dl2*dsv(i,k,l2)
                    enddo
                end do
            end do
        end if
     else
        loc=nrf3_loc(n)
        do j=llmin,llmax
           call smoothzo(vz(1,j,n),samp,rate,n,j,dsv(1,1,j))
           do k=1,nsig
              do i=1,lon2
                 dsv(i,k,j)=dsv(i,k,j)*corz(j,k,n)*as3d(n)
              end do
           end do
        end do

        do j=1,lat2
           do i=1,lon2
              l=int(rllat1(j,i))
              l2=min0(l+1,llmax)
              dl2=rllat1(j,i)-real(l,r_kind)
              dl1=one-dl2
              do k=1,nsig
                 dssv(j,i,k,n)=dl1*dsv(i,k,l)+dl2*dsv(i,k,l2)
              enddo
           end do
        end do
     endif
  end do

! output berror correlation length scales and variances
  if ( l_plt_be_stats ) then
      inerr_out=2117
      if (mype == 0) then
          do n=1,nc3d
              write(6,*)'---- pregwgt_reg(mype=',mype,'): cvars3d(n)=',n,cvars3d(n),'   output corz/hwll/vz for 3D berror.'
          end do
          open(inerr_out,file='./berror_prewgt_reg_vIntrp.dat',form='unformatted')
          write(inerr_out)mlat,nsig,nc3d
          write(inerr_out)cvars3d(1:nc3d)
          write(inerr_out)(((hwll(j,k,n),j=0,mlat+1),k=1,nsig),n=1,nc3d)
          write(6,*),' ---- prewgt_reg(mype=',mype,'   output vz not normalized !'
          write(inerr_out)(((vz(k,j,n),j=0,mlat+1),k=1,nsig),n=1,nc3d)
          if ( .not. allocated(vz4plt)) allocate (vz4plt(1:nsig,0:mlat+1,1:nc3d) )
          do n=1,nc3d
              do j=0,mlat+1
                  do k=1,nsig
                      vz4plt(k,j,n)=vz(k,j,n)
                  end do
              end do
          end do
          write(inerr_out)(((vz4plt(k,j,n),j=0,mlat+1),k=1,nsig),n=1,nc3d)
          deallocate(vz4plt)
          write(inerr_out)(((corz(j,k,n),j=1,mlat),k=1,nsig),n=1,nc3d)
          if (l_be_T_dep) then
              write(6,*),' ---- prewgt_reg(mype=',mype,':output temperature-dependent error for cloud hydrometers'
              write(inerr_out) ((corz_cld(1,1,k,n),k=1,nsig),n=1,3)
          end if
          close(inerr_out)
      end if
  end if

! Special case of dssv for qoption=2 and cw
  if (qoption==2) call compute_qvar3d

! Background error arrays for sfp, sst, land t, and ice t
  do n=1,nc2d
     loc=nrf2_loc(n)
     do j=llmin,llmax
        do i=1,lon2
           dsvs(i,j)  =corp(j,n)*as2d(n)
        end do
     end do

     do j=1,lat2
        do i=1,lon2
           l=int(rllat1(j,i))
           l2=min0(l+1,llmax)
           dl2=rllat1(j,i)-real(l,r_kind)
           dl1=one-dl2
           dssvs(j,i,n)=dl1*dsvs(i,l)+dl2*dsvs(i,l2)
           if (mvars>=2.and.n==nrf2_sst) then
              dssvs(j,i,nc2d+1)=atsfc_sdv(1)*as2d(n)  
              dssvs(j,i,nc2d+2)=atsfc_sdv(2)*as2d(n)  
           end if
        end do
     end do
  end do

  if (bkgv_write) call write_bkgvars2_grid

! hybrid sigma level structure calculated in rdgstat_reg   
! ks used to load constant horizontal scales for SF/VP
! above sigma level 0.15
! loop l for diff variable of each PE.

  psfc015=r015*ges_psfcavg
  do l=1,nnnn1o
     ks(l)=nsig+1
     if(cvars(nvar_id(l))=='sf' .or. cvars(nvar_id(l))=='SF'.or. &
        cvars(nvar_id(l))=='vp' .or. cvars(nvar_id(l))=='VP')then
        k_loop: do k=1,nsig
           if (ges_prslavg(k) < psfc015) then
              ks(l)=k
              exit k_loop
           end if
        enddo k_loop
     endif
  end do

  if(nnnn1o > 0)then
     allocate(sli(ny,nx,2,nnnn1o))

! sli in scale  unit (can add in sea-land mask)
     samp2=samp*samp
     do i=1,nx
        do j=1,ny
           fact=one/(one+(one-sl(j,i))*bw)
           slw((i-1)*ny+j,1)=region_dx(j,i)*region_dy(j,i)*fact**2*samp2
           sli(j,i,1,1)=region_dy(j,i)*fact
           sli(j,i,2,1)=region_dx(j,i)*fact
        enddo
     enddo
  endif

! Set up scales


! This first loop for nnnn1o will be if we aren't dealing with
! surface pressure, skin temperature, or ozone
  do k=nnnn1o,1,-1
     k1=levs_id(k)
     n=nvar_id(k)

     nn=-1
     do ii=1,nc3d
        if (nrf3_loc(ii)==n) then 
           nn=ii
           if (nn/=nrf3_oz) then
              if (k1 >= ks(k))then
                 l=int(rllat(ny/2,nx/2))
                 fact=one/hwll(l,k1,nn)
                 do i=1,nx
                    do j=1,ny
                       slw((i-1)*ny+j,k)=slw((i-1)*ny+j,1)*fact**2
                       sli(j,i,1,k)=sli(j,i,1,1)*fact
                       sli(j,i,2,k)=sli(j,i,2,1)*fact
                    enddo
                 enddo
              else
                 do i=1,nx
                    do j=1,ny
                       l=int(rllat(j,i))
                       lp=min0(l+1,llmax)
                       dl2=rllat(j,i)-real(l,r_kind)
                       dl1=one-dl2
                       fact=one/(dl1*hwll(l,k1,nn)+dl2*hwll(lp,k1,nn))
                       slw((i-1)*ny+j,k)=slw((i-1)*ny+j,1)*fact**2
                       sli(j,i,1,k)=sli(j,i,1,1)*fact
                       sli(j,i,2,k)=sli(j,i,2,1)*fact
                    enddo
                 enddo
              endif
           else
              if (k1 <= kb )then
                 hwl=r400000
              else
                 hwl=r800000-r400000*(nsig-k1)/(nsig-kb)
              endif
              fact=one/hwl
              do i=1,nx
                 do j=1,ny
                    slw((i-1)*ny+j,k)=slw((i-1)*ny+j,1)*fact**2
                    sli(j,i,1,k)=sli(j,i,1,1)*fact
                    sli(j,i,2,k)=sli(j,i,2,1)*fact
                 enddo
              enddo
           end if 
           exit
        end if
     end do

     if (nn==-1) then 
        do ii=1,nc2d
           if (nrf2_loc(ii)==n .or. n>nrf) then 
              nn=ii
              if (n>nrf) nn=n-nc3d
              cc=one 
              if (nn==nrf2_sst) cc=two
              if (nn==nc2d+1 .or. nn==nc2d+2) cc=four
              do i=1,nx
                 do j=1,ny
                    l=int(rllat(j,i))
                    lp=min0(l+1,llmax)
                    dl2=rllat(j,i)-real(l,r_kind)
                    dl1=one-dl2
                    fact=cc/(dl1*hwllp(l,nn)+dl2*hwllp(lp,nn))
                    slw((i-1)*ny+j,k)=slw((i-1)*ny+j,1)*fact**2
                    sli(j,i,1,k)=sli(j,i,1,1)*fact
                    sli(j,i,2,k)=sli(j,i,2,1)*fact
                 end do
              end do
              exit
           end if
        end do
     end if 

  end do
  deallocate(corz,corp,hwll,hwllp,vz)
  deallocate(nrf3_loc,nrf2_loc)

  if ( allocated(vz_cld  ) ) deallocate ( vz_cld )
  if ( allocated(dsv_cld ) ) deallocate ( dsv_cld )
  if ( allocated(corz_cld) ) deallocate ( corz_cld )

! Load tables used in recursive filters
  if(nnnn1o>0) then
     call init_rftable(mype,rate,nnnn1o,sli)
     deallocate( sli) 
  endif

  return
end subroutine prewgt_reg

subroutine berror_qcld_tdep(mype,tsen,i_cat,q_cld_err)
! temperature dependent error variance
!
! program history log:
! 2016-10-xx CAPS(G. Zhao) - based on Chengsi Liu and Rong Kong's work
!
  use kinds, only: r_kind,i_kind
  use constants, only: pi
  use directDA_radaruse_mod, only: l_use_cvpqx

  implicit none

  integer(i_kind), intent(in  ) :: mype            ! pe number
  integer(i_kind), intent(in  ) :: i_cat           ! cloud hydrometer category
                                                   ! 1: rain water
                                                   ! 2: snow
                                                   ! 3: graupel / hail
  real(r_kind), intent(in   )   :: tsen            ! temperature

  real(r_kind), intent(  out)   :: q_cld_err       ! background error

! define local variables
! qr -- rain
  real(r_kind)            :: Eqrl, Eqrh            ! error @ low-level and high-level
  real(r_kind)            :: Tqrl, Tqrh            ! significant temperature points

! qs -- snow
  real(r_kind)            :: Eqsl, Eqsh            ! error @ low-level and high-level
  real(r_kind)            :: Tqsl, Tqsh            ! significant temperature points

! qg -- graupel
  real(r_kind)            :: Eqgl, Eqgh            ! error @ low-level and high-level
  real(r_kind)            :: Tqgl, Tqgh            ! significant temperature points

!
  real(r_kind) :: Eql, Eqh
  real(r_kind) :: Tql, Tqh

  real(r_kind) :: Delta_T, Delta_E, Mid_E
  real(r_kind) :: dt, de
  real(r_kind) :: a

  logical   :: firstcalled
  save firstcalled
  data firstcalled/.true./

  external stop2


! BOP-------------------------------------------------------------------

! significant temperature point for rain water
  Tqrl=272.65_r_kind
  Tqrh=268.15_r_kind

! significant temperature point for snow
  Tqsl=282.65_r_kind
  Tqsh=280.15_r_kind

! significant temperature point for graupel
  Tqgl=281.15_r_kind
  Tqgh=279.15_r_kind

  if ( l_use_cvpqx ) then
      Eqrl=0.4055_r_kind
      Eqrh=1.0E-6_r_kind

      Eqsl=1.0E-6_r_kind
      Eqsh=0.4055_r_kind

      Eqgl=0.2877_r_kind
      Eqgh=0.2877_r_kind
  else
      Tqrl=272.65_r_kind
      Eqrl=1.2E-3_r_kind
      Tqrh=268.15_r_kind
      Eqrh=1.0E-10_r_kind

      Tqsl=282.65_r_kind
      Eqsl=1.0E-10_r_kind
      Tqsh=280.15_r_kind
      Eqsh=1.2E-3_r_kind

      Tqgl=278.15_r_kind
      Eqgl=6.0E-4_r_kind
      Tqgh=268.15_r_kind
      Eqgh=1.2E-3_r_kind
  end if

  if (i_cat == 1) then
      Tql = Tqrl
      Tqh = Tqrh
      Eql = Eqrl
      Eqh = Eqrh
  else if (i_cat == 2) then
      Tql = Tqsl
      Tqh = Tqsh
      Eql = Eqsl
      Eqh = Eqsh
  else if (i_cat == 3) then
      Tql = Tqgl
      Tqh = Tqgh
      Eql = Eqgl
      Eqh = Eqgh
  else
      write(6,*) 'sub: berror_qcld_tdep:   unknown category id-->',i_cat
      call stop2(999)
  end if

  if (firstcalled) then
      if (mype==0) then
          write(6,*)'berror_qcld_Tdep: use_logqx->',l_use_cvpqx,' i_cat->',i_cat,' mype->',mype
          write(6,*)'be_qcld_Tdep: rain   (Tlow,BElow,Thgh,BEhgh)',Tqrl,Eqrl,Tqrh,Eqrh
          write(6,*)'be_qcld_Tdep: snow   (Tlow,BElow,Thgh,BEhgh)',Tqsl,Eqsl,Tqsh,Eqsh
          write(6,*)'be_qcld_Tdep: graupel(Tlow,BElow,Thgh,BEhgh)',Tqgl,Eqgl,Tqgh,Eqgh
      end if
      firstcalled = .false.
  end if

  if ( tsen > Tql ) then
      q_cld_err = Eql
  else if ( tsen <= Tql .and. tsen >= Tqh ) then
      Delta_T = Tql - Tqh
      Delta_E = Eql - Eqh
      Mid_E = (Eql + Eqh) * 0.5_r_kind
      dt = tsen - Tqh
      a =  pi * dt / Delta_T
      de = -cos(a) * Delta_E * 0.5_r_kind
      q_cld_err =  Mid_E + de
  else
      q_cld_err = Eqh
  end if

  return
! EOP----------

end subroutine berror_qcld_tdep
!
subroutine calc_corz_cld_qr(temp_k, cvpqx_pval, val_corz_cld)
!
! abstract: calculate corz value of qr dependent on temperature
!
! program history log:
! 2019-xx-xx CAPS(L. Chen) - initial version

use kinds, only: r_kind
use constants, only: zero

implicit none

real(r_kind), intent(in)    :: temp_k
real(r_kind), intent(in)    :: cvpqx_pval
real(r_kind), intent(out)   :: val_corz_cld

! local 
real(r_kind) :: t0_kelvin =273.15_r_kind

   if ( cvpqx_pval > 0.45_r_kind .and. cvpqx_pval <= 0.55_r_kind) then
      if ((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
        .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=5.15E-2_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=5.51E-2_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=5.75E-2_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=5.83E-2_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=5.71E-2_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=5.31E-2_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=5.21E-2_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=5.64E-2_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=5.47E-2_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=5.26E-2_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=5.18E-2_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=4.75E-2_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=4.52E-2_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=4.35E-2_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=3.96E-2_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=3.89E-2_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=3.67E-2_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=3.50E-2_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=3.53E-2_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=3.42E-2_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=3.40E-2_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=3.74E-2_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=4.07E-2_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=5.02E-2_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=4.68E-2_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=4.88E-2_r_kind
      end if
  else if ( cvpqx_pval > 0.95_r_kind .and. cvpqx_pval <= 1.05_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <=-54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=9.32E-4_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=1.17E-3_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=1.34E-3_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=1.42E-3_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=1.30E-3_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=1.10E-3_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=9.74E-4_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=1.18E-3_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=1.12E-3_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=1.21E-3_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=1.32E-3_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=1.25E-3_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=1.27E-3_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=1.15E-3_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=1.08E-3_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=1.01E-3_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=9.20E-4_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=8.77E-4_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=8.52E-4_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=8.17E-4_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=8.04E-4_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=7.83E-4_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=7.97E-4_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=1.00E-3_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=8.19E-4_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=8.33E-4_r_kind
      end if
  else if ( cvpqx_pval < 0.01_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=4.22_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=4.25_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=4.29_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=4.31_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=4.26_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=4.17_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=4.22_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=4.26_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=4.14_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=3.74_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=3.50_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=3.13_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=2.81_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=2.79_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=2.44_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=2.46_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=2.31_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=2.20_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=2.20_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=2.09_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=2.06_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=2.54_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=3.01_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=3.70_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=3.67_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=3.98_r_kind
      end if
  else if ( cvpqx_pval > 0.35_r_kind .and. cvpqx_pval <= 0.45_r_kind) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=0.1197_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=0.1265_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=0.1314_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=0.1299_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=0.1296_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=0.1216_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=0.1251_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=0.1281_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=0.1170_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=0.1182_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=0.1099_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=0.1062_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=0.1024_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=0.0917_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=0.0901_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=0.0850_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=0.0789_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=0.0779_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=0.0773_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=0.0758_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=0.0707_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=0.0829_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=0.0913_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=0.1099_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=0.1094_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=0.1125_r_kind
      end if
  end if
end subroutine calc_corz_cld_qr
!
subroutine calc_corz_cld_qs(temp_k, cvpqx_pval, val_corz_cld)
!
! abstract: calculate corz value of qs dependent on temperature
!
! program history log:
! 2019-xx-xx CAPS(L. Chen) - initial version

use kinds, only: r_kind
use constants, only: zero

implicit none

real(r_kind), intent(in)    :: temp_k
real(r_kind), intent(in)    :: cvpqx_pval
real(r_kind), intent(out)   :: val_corz_cld

! local 
real(r_kind) :: t0_kelvin =273.15_r_kind

  if ( cvpqx_pval > 0.45_r_kind .and. cvpqx_pval <= 0.55_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=6.57E-2_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=5.30E-2_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=5.19E-2_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=4.48E-2_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=3.75E-2_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=3.40E-2_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=3.19E-2_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=3.01E-2_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=2.77E-2_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=2.66E-2_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=2.43E-2_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=2.65E-2_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=2.43E-2_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=2.77E-2_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=2.98E-2_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=3.11E-2_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then 
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  else if ( cvpqx_pval > 0.95_r_kind .and. cvpqx_pval <= 1.05_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=2.10E-3_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=1.63E-3_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=1.51E-3_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=1.21E-3_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then 
         val_corz_cld=9.18E-4_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=7.75E-4_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=6.98E-4_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=6.27E-2_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=5.51E-2_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=5.02E-2_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=4.37E-2_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=4.66E-2_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=4.18E-2_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=4.75E-2_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=4.66E-2_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=4.69E-2_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  else if ( cvpqx_pval < 0.01_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=3.44_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=2.82_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=2.98_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=2.72_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=2.47_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=2.36_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=2.28_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=2.20_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=2.06_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=2.01_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=1.82_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=2.06_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=1.84_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=2.08_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=2.50_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=2.66_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  else if ( cvpqx_pval > 0.35 .and. cvpqx_pval <= 0.45_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=0.1303_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=0.1140_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=0.1109_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=0.0964_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=0.0824_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=0.0756_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=0.0720_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=0.0700_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=0.0641_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=0.0602_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=0.0595_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=0.0539_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=0.0610_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=0.0547_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=0.0697_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=0.0775_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  end if
end subroutine calc_corz_cld_qs
!
subroutine calc_corz_cld_qg(temp_k, cvpqx_pval, val_corz_cld)
!
! abstract: calculate corz value of qg dependent on temperature
!
! program history log:
! 2019-xx-xx CAPS(L. Chen) - initial version

use kinds, only: r_kind
use constants, only: zero

implicit none

real(r_kind), intent(in)    :: temp_k
real(r_kind), intent(in)    :: cvpqx_pval
real(r_kind), intent(out)   :: val_corz_cld

! local 
real(r_kind) :: t0_kelvin =273.15_r_kind

  if ( cvpqx_pval > 0.45_r_kind .and. cvpqx_pval <= 0.55_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=4.66E-2_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=4.96E-2_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=4.97E-2_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=4.82E-2_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=4.99E-2_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=4.89E-2_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=4.65E-2_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=4.47E-2_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=4.32E-2_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=4.17E-2_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=4.30E-2_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=3.86E-2_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=4.13E-2_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=3.87E-2_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=3.73E-2_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=3.84E-2_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=3.84E-2_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=4.06E-2_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=4.41E-2_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=4.48E-2_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=4.68E-2_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=4.60E-2_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=4.83E-2_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=4.73E-2_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  else if ( cvpqx_pval > 0.95_r_kind .and. cvpqx_pval <= 1.05_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=8.45E-4_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=1.06E-3_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=1.16E-3_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=1.20E-3_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=1.36E-3_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=1.34E-3_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=1.28E-3_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=1.23E-3_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=1.19E-3_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=1.16E-3_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=1.17E-3_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=1.12E-3_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=1.10E-3_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=1.05E-3_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=9.85E-4_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=9.52E-4_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=8.95E-4_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=8.70E-4_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=9.09E-4_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=8.78E-4_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=8.59E-4_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=7.75E-4_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)& 
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=8.22E-4_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=7.56E-4_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=6.48E-4_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=5.86E-4_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  else if ( cvpqx_pval < 0.01_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=3.73_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=3.57_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=3.38_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=3.14_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=3.08_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=3.00_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=2.86_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=2.73_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=2.64_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=2.53_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=2.70_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=2.25_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=2.69_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=2.35_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=2.43_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=2.49_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=2.62_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
         .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=2.85_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=3.20_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=3.35_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=3.71_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=3.87_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=4.09_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=4.15_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=4.05_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=4.0_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=zero
      end if
  else if ( cvpqx_pval > 0.35_r_kind .and. cvpqx_pval <= 0.45_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=0.1091_r_kind
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=0.1112_r_kind
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=0.1111_r_kind
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=0.1060_r_kind
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=0.1083_r_kind
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=0.1062_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=0.1015_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=0.0971_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=0.0935_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=0.0901_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=0.0945_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=0.0824_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=0.0890_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=0.0867_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=0.0811_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=0.0874_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=0.0860_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=0.0918_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=0.0988_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=0.1041_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=0.1061_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=0.1058_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=0.1142_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=0.1189_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=0.1092_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=zero
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then 
         val_corz_cld=zero
      end if
  end if
end subroutine calc_corz_cld_qg
!
subroutine calc_corz_cld_qnr(temp_k, cvpqx_pval, val_corz_cld)
!
! abstract: calculate corz value of qnr dependent on temperature
!
! program history log:
! 2020-xx-xx CAPS(H. Li) - calculate corz value of qnr dependent on temperature

use kinds, only: r_kind
use constants, only: zero

implicit none

real(r_kind), intent(in)    :: temp_k
real(r_kind), intent(in)    :: cvpqx_pval
real(r_kind), intent(out)   :: val_corz_cld

! local 
real(r_kind) :: t0_kelvin =273.15_r_kind

  if ( cvpqx_pval > 0.15_r_kind .and. cvpqx_pval <= 0.35_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=9.485_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=9.97631_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=10.26889_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=10.44881_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=10.38219_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=9.66746_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=10.11691_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=9.17874_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=9.36543_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=8.92244_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=9.46162_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=8.96573_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=8.16076_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=7.15516_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=6.40303_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=6.06015_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=5.94796_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=6.13248_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=5.73713_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=5.15828_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=4.61217_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=4.11024_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=3.70197_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=3.09048_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=2.80425_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=2.52145_r_kind
      end if
  else if ( cvpqx_pval > 0.95_r_kind .and. cvpqx_pval <= 1.05_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=3495.01948_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=3359.95630_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=3279.37776_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=3259.62172_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=3261.44242_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=2954.79599_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=3253.80263_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=2568.17691_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=3046.86640_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=2554.11253_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=3056.95997_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=2758.72759_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=2715.80791_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=2433.42795_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=2178.34221_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=2055.50023_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=1984.83334_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=2253.66838_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=2054.66838_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=1667.54278_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=1168.21613_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=771.84911_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=536.23827_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=321.23091_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=227.87040_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=166.08732_r_kind
      end if
  else if ( cvpqx_pval < 0.01_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=1.34076_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=1.46285_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=1.59837_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=1.71920_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=1.76760_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=1.73899_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=1.79985_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=1.71070_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=1.70535_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=1.63083_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=1.65737_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=1.53212_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
         .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=1.36688_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=1.02706_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=0.83546_r_kind
     else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=0.72720_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=0.70228_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=0.72675_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=0.70138_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=0.65538_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=0.61329_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=0.57782_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=0.54962_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=0.49000_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=0.46413_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=0.43088_r_kind
      end if
  else if ( cvpqx_pval > 0.35_r_kind .and. cvpqx_pval <= 0.45_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=43.33112_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=46.78213_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=47.86116_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=47.78372_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=46.25078_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=41.49728_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=43.10740_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=37.92371_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=37.73387_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=35.22413_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=38.20840_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=36.05816_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=33.35756_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=30.89302_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=28.43813_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=27.58614_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=27.27685_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=26.12844_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=22.76230_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=19.36064_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=16.41313_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=13.71713_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=11.63848_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=9.01309_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=7.77490_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=6.71456_r_kind
      end if
  else if ( cvpqx_pval > 0.55_r_kind .and. cvpqx_pval <= 0.75_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=248.35315_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=281.41940_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=290.07470_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=286.54191_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=271.70061_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=243.48175_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=242.15677_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=214.39841_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=204.90672_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=184.78424_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=212.82253_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=204.18614_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=191.03116_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=184.32626_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=172.88378_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=173.22089_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=169.51436_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=133.14491_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=101.04275_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=79.26567_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=63.33042_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=49.25802_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=39.22117_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=27.98290_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=22.77527_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=18.73216_r_kind
      end if
  else if ( cvpqx_pval > 0.75_r_kind .and. cvpqx_pval <= 0.95_r_kind ) then
      if((temp_k-t0_kelvin) <= -62.0_r_kind) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -62.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -54.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -54.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -46.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -46.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -38.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -38.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -30.0_r_kind)) then
         val_corz_cld=zero
      else if(((temp_k-t0_kelvin) > -30.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -26.0_r_kind)) then
         val_corz_cld=1683.85896_r_kind
      else if(((temp_k-t0_kelvin) > -26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -22.0_r_kind)) then
         val_corz_cld=2030.55185_r_kind
      else if(((temp_k-t0_kelvin) > -22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -18.0_r_kind)) then
         val_corz_cld=2134.04860_r_kind
      else if(((temp_k-t0_kelvin) > -18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -14.0_r_kind)) then
         val_corz_cld=2107.52824_r_kind
      else if(((temp_k-t0_kelvin) > -14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -10.0_r_kind)) then
         val_corz_cld=1970.77384_r_kind
      else if(((temp_k-t0_kelvin) > -10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -8.0_r_kind)) then
         val_corz_cld=1836.79082_r_kind
      else if(((temp_k-t0_kelvin) > -8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -6.0_r_kind)) then
         val_corz_cld=1655.23752_r_kind
      else if(((temp_k-t0_kelvin) > -6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -4.0_r_kind)) then
         val_corz_cld=1541.02462_r_kind
      else if(((temp_k-t0_kelvin) > -4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= -2.0_r_kind)) then
         val_corz_cld=1401.70998_r_kind
      else if(((temp_k-t0_kelvin) > -2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 0.0_r_kind)) then
         val_corz_cld=1199.47684_r_kind
      else if(((temp_k-t0_kelvin) > 0.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 2.0_r_kind)) then
         val_corz_cld=1552.23711_r_kind
      else if(((temp_k-t0_kelvin) > 2.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 4.0_r_kind)) then
         val_corz_cld=1570.17413_r_kind
      else if(((temp_k-t0_kelvin) > 4.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 6.0_r_kind)) then
         val_corz_cld=1466.94096_r_kind
      else if(((temp_k-t0_kelvin) > 6.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 8.0_r_kind)) then
         val_corz_cld=1520.38137_r_kind
      else if(((temp_k-t0_kelvin) > 8.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 10.0_r_kind)) then
         val_corz_cld=1454.40128_r_kind
      else if(((temp_k-t0_kelvin) > 10.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 12.0_r_kind)) then
         val_corz_cld=1502.99309_r_kind
      else if(((temp_k-t0_kelvin) > 12.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 14.0_r_kind)) then
         val_corz_cld=1412.71476_r_kind
      else if(((temp_k-t0_kelvin) > 14.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 16.0_r_kind)) then
         val_corz_cld=828.60360_r_kind
      else if(((temp_k-t0_kelvin) > 16.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 18.0_r_kind)) then
         val_corz_cld=503.31973_r_kind
      else if(((temp_k-t0_kelvin) > 18.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 20.0_r_kind)) then
         val_corz_cld=351.04665_r_kind
      else if(((temp_k-t0_kelvin) > 20.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 22.0_r_kind)) then
         val_corz_cld=263.12892_r_kind
      else if(((temp_k-t0_kelvin) > 22.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 24.0_r_kind)) then
         val_corz_cld=189.18421_r_kind
      else if(((temp_k-t0_kelvin) > 24.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 26.0_r_kind)) then
         val_corz_cld=140.92174_r_kind
      else if(((temp_k-t0_kelvin) > 26.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 28.0_r_kind)) then
         val_corz_cld=92.23219_r_kind
      else if(((temp_k-t0_kelvin) > 28.0_r_kind)&
          .and. ((temp_k-t0_kelvin) <= 30.0_r_kind)) then
         val_corz_cld=70.31765_r_kind
      else if((temp_k-t0_kelvin) > 30.0_r_kind) then
         val_corz_cld=54.63960_r_kind
      end if
  end if
end subroutine calc_corz_cld_qnr
