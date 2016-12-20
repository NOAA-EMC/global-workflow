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
       init_rftable,hzscl,slw,nhscrf,cwcoveqqcov
  use mpimod, only: nvar_id,levs_id,mpi_sum,mpi_comm_world,ierror,mpi_rtype
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

  real(r_kind) samp2,dl1,dl2,d
  real(r_kind) samp,hwl,cc
  real(r_kind),dimension(nsig):: rate,dlsig,rlsig
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
  real(r_kind),dimension(180,nsig):: ozmzt,cntt

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

! Read dimension of stats file
  inerr=22
  call berror_get_dims_reg(msig,mlat,inerr)

! Allocate arrays in stats file
  allocate ( corz(1:mlat,1:nsig,1:nc3d) )
  allocate ( corp(1:mlat,nc2d) )
  allocate ( hwll(0:mlat+1,1:nsig,1:nc3d),hwllp(0:mlat+1,nvars-nc3d) )
  allocate ( vz(1:nsig,0:mlat+1,1:nc3d) )

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
           ozmzt(ix,k)=ozmz0(i)
           cntt(ix,k)=cnt0(i)
        end do
     end do
     do k=1,nsig
        do i=1,180
           if(cntt(i,k)>zero) ozmzt(i,k)=sqrt(ozmzt(i,k)/cntt(i,k))
        enddo
     enddo
  endif ! regional_ozone
! Normalize vz with del sigmma and convert to vertical grid units!
  dlsig(1)=rlsig(1)-rlsig(2)
  do k=2,nsig-1
     dlsig(k)=half*(rlsig(k-1)-rlsig(k+1))
  enddo
  dlsig(nsig)=rlsig(nsig-1)-rlsig(nsig)

  do n=1,nc3d
     do j=0,mlat+1
        do k=1,nsig
           vz(k,j,n)=vz(k,j,n)*dlsig(k)
        end do
     end do
  end do

! As used in the code, the horizontal length scale
! parameters are used in an inverted form.  Invert
! the parameter values here.
  do i=1,nhscrf
     hzscl(i)=one/hzscl(i)
  end do

! apply scaling to vertical length scales.  
! note:  parameter vs needs to be inverted
  vs=one/vs
  vz=vz*vs

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
              dl2=d-float(l)
              dl1=one-dl2
              do k=1,nsig
                 dssv(i,j,k,n)=(dl1*ozmzt(l,k)+dl2*ozmzt(l2,k))*dsv(1,k,llmin)
              end do
           end do
        end do
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
              dl2=rllat1(j,i)-float(l)
              dl1=one-dl2
              do k=1,nsig
                 dssv(j,i,k,n)=dl1*dsv(i,k,l)+dl2*dsv(i,k,l2)
              enddo
           end do
        end do
     endif
  end do

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
           dl2=rllat1(j,i)-float(l)
           dl1=one-dl2
           dssvs(j,i,n)=dl1*dsvs(i,l)+dl2*dsvs(i,l2)
           if (mvars>=2.and.n==nrf2_sst) then
              dssvs(j,i,nc2d+1)=atsfc_sdv(1)*as2d(n)  
              dssvs(j,i,nc2d+2)=atsfc_sdv(2)*as2d(n)  
           end if
        end do
     end do
  end do


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
                       dl2=rllat(j,i)-float(l)
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
                    dl2=rllat(j,i)-float(l)
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


! Load tables used in recursive filters
  if(nnnn1o>0) then
     call init_rftable(mype,rate,nnnn1o,sli)
     deallocate( sli) 
  endif

  return
end subroutine prewgt_reg
