subroutine prewgt(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prewgt
!   prgmmr: wu               org: np22                date: 2000-03-15
!
! abstract: setup smoothing and grid transform for background error     
!
! program history log:
!   2000-03-15  wu           
!   2004-02-03  kleist, updated to load background stats according
!               to updated mpi distribution on horizontal slabs
!   2004-03-15  derber, kleist, incorporate variances into this routine
!               stats from single file, additional clean up
!   2004-05-17  kleist, documentation and clean up
!   2004-08-03  treadon - add only to module use, add intent in/out
!   2004-10-26  wu - include factors hzscl in the range of RF table
!   2004-11-02  treadon - add horizontal resolution error check on berror_stats
!   2004-11-16  treadon - add longitude dimension to variance array dssv
!   2004-11-20  derber - modify to make horizontal table more reproducable and  
!               move most of table calculations to berror 
!   2005-01-22  parrish - split out balance variables to subroutine prebal--this
!               to make anisotropic filtering option less confusing
!   2005-02-23  wu - setup background variance for qoption=2
!   2005-03-28  wu - change loop index (mlat+1 to mlat) over varq
!   2005-04-14  treadon - add corq2 to global berror_stats read
!   2005-04-22  treadon - change berror file to 4-byte reals
!   2005-05-27  kleist - add setup call for new patch interpolation
!   2005-08-16  guo - add gmao surface interface
!   2005-09-28  guo - set nrr=nlat to support the GMAO grid
!   2005-09-28  guo - fixed nrr to nlat-2 to avoid the subscript of
!		array rlats being out of the range, and to avoid the
!		singularity of rs at rlats(1)=-pi/2.
!   2005-11-16  wgu - set nolp=nr+1+(ny-nlat)/2 to make sure
!               nmix+nrmxb=nr in routine smoothrf no matter what
!               number nlat is.
!   2005-11-29  derber - unify ozone variance calculation
!   2006-01-10  treadon - replace rdsfull with read_gfssfc_full
!   2006-01-11  kleist - place upper/lower bounds on qoption=2 variance
!   2006-01-31  treadon - invert hzscl
!   2006-02-03  derber - fix up sl array stuff
!   2006-04-12  treadon - remove sigl (not used)
!   2006-04-21  kleist  - add capability to perturb background error parameters
!   2006-07-28  derber  - use r1000 from constants
!   2006-09-18  derber  - change minimum moisture variance
!   2007-05-30  h.liu   - use oz stats from berror file
!   2007-07-03  kleist  - add option for flow-dependent background error variances
!   2008-04-23  safford - rm unused uses and vars
!   2008-07-30  guo     - read stats using m_berror_stats
!   2009-01-12  gayno   - rm use of read_gfssfc_full
!   2010-02-25  zhu     - mv varq to m_berror_stats
!                       - make changes for generalizing control variables,
!                         change interface of berror_read_wgt,use nrf*
!   2010-04-01  treadon - move strip to gridmod
!   2010-04-10  parrish - remove rhgues, no longer needed
!   2010-05-28  todling - obtain variable id's on the fly (add getindex)
!   2010-06-01  todling - bypass calculation when pointer not defined
!                         sort of generalize pert_berr
!                         rename as,tsfc_sdv to as3d,as2d,atsfc_sdv (alloc now)
!   2010-06-03  todling - protect motley in dssvs w/ mvars check
!                       - turn nrf2_loc/nrf3_loc into local variables
!   2010-06-18  todling - add call to write_bkgvars_grid and write_bkgvars2_grid
!   2010-07-07  kokron/todling - fix definition of hwllp to do sfc-only
!   2011-07-03  todling - calculation of bl and bl2 must be done in double-prec
!                         or GSI won'd work when running in single precision; go figure!
!   2012-05-14  wargan - add adjustozvar
!   2012-11-26  parrish - move subroutine blend to module blendmod.f90, and add "use blendmod, only: blend"
!   2012-12-15  zhu     - add two cwoption options
!   2013-10-19  todling - all guess variables in met-guess
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-02-01  todling - update interface to berror_read_wgt
!   2014-02-05  mkim/todling - move cw overwrite w/ q to m_berror_stats
!   2014-08-02  zhu     - set up new background error variance and correlation lengths of cw 
!                         for all-sky radiance assimilation
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single,r_double,r_quad
  use berror, only: dssvs,wtaxs,&
       bw,wtxrs,inaxs,inxrs,nr,ny,nx,mr,ndeg,&
       nf,vs,be,dssv,norh,bl2,bl,init_rftable,hzscl,&
       pert_berr,bkgv_flowdep,slw,slw1,slw2,bkgv_write,nhscrf,&
       adjustozvar
  use m_berror_stats,only : berror_read_wgt
  use mpimod, only: nvar_id,levs_id
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  use jfunc, only: varcw,cwoption
  use jfunc, only: varq,qoption
  use control_vectors, only: cvars2d,cvars3d
  use control_vectors, only: cvars => nrf_var
  use control_vectors, only: as2d,as3d,atsfc_sdv
  use control_vectors, only: nrf,nc2d,nc3d,mvars
  use gridmod, only: istart,jstart,lat2,lon2,rlats,nlat,nlon,nsig,&
       nnnn1o,lat1,lon1,itotsub,iglobal,ijn,displs_g,&
       strip
  use general_commvars_mod, only: ltosi,ltosj
  use constants, only: zero,quarter,half,one,two,three,&
       rearth_equator,pi,r1000,r400
  use guess_grids, only: isli2
  use guess_grids, only: ntguessig
  use smooth_polcarf, only: norsp,setup_smooth_polcas
  use mpeu_util, only: getindex
  use blendmod, only: blend
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  integer(i_kind) n,nrr,iii,jjj,nxg,i2,im,jm,j2
  integer(i_kind) i,j,k,ii,nn,nbuf,nmix,nxe,nor,ndx,ndy
  integer(i_kind) nlathh,mm1,nolp,mm,ir,k1
  integer(i_kind) ix,jx,mlat
  integer(i_kind) nf2p,istatus
  integer(i_kind),dimension(0:40):: iblend
  integer(i_kind) nrf3_sf,nrf3_q,nrf3_vp,nrf3_t,nrf3_oz,nrf2_ps,nrf2_sst,nrf3_cw
  integer(i_kind),allocatable,dimension(:) :: nrf3_loc,nrf2_loc

  real(r_kind) wlipi,wlipih,df
  real(r_kind) samp,s2u,df2,pi2
  real(r_quad) y,x,dxx
  real(r_kind),dimension(ndeg):: rate
  real(r_kind),dimension(ndeg,ndeg):: turn
  real(r_kind),dimension(lat2,lon2)::temp
  real(r_kind),dimension(nlat,nlon):: sl,factx
  real(r_kind),dimension(-nf:nf,-nf:nf) :: fact1,fact2
  real(r_kind),dimension(mr:nlat-2):: rs
  real(r_kind),dimension(lat1*lon1)::zsm
  real(r_kind),dimension(itotsub)::work1
  real(r_kind),dimension(ny,nx,3):: scsli
  real(r_kind),dimension(-nf:nf,-nf:nf,3):: scs12
  real(r_single),dimension(nlat,nlon):: corsst
  real(r_kind),dimension(lon2,nsig):: dsv
  real(r_single) hsstmin
  real(r_kind) minhsst
  real(r_kind) my_corz
  real(r_kind),allocatable:: randfct(:)
  real(r_kind),allocatable,dimension(:,:,:,:):: sli,sli1,sli2

  real(r_kind),allocatable,dimension(:,:,:):: vz
  real(r_kind),allocatable,dimension(:,:,:):: hwll
  real(r_kind),allocatable,dimension(:,:)  :: hwllp

  real(r_single),allocatable,dimension(:,:,:):: corz
  real(r_single),allocatable,dimension(:,:,:):: hwllin
  real(r_single),allocatable,dimension(:,:,:):: vscalesin
  real(r_single),allocatable,dimension(:,:)  :: corp
  real(r_single),allocatable,dimension(:,:)  :: hwllinp
  real(r_single),allocatable,dimension(:,:)  :: hsst

  real(r_kind),dimension(lat2,lon2,nsig):: sfvar,vpvar,tvar
  real(r_kind),dimension(lat2,lon2):: psvar
  real(r_kind),dimension(:,:,:),pointer :: ges_oz=>NULL()
! real(r_kind),parameter:: eight_tenths = 0.8_r_kind
! real(r_kind),parameter:: six          = 6.0_r_kind
! real(r_kind),parameter:: r800         = 800.0_r_kind
! real(r_kind),parameter:: r40000       = 40000.0_r_kind
! real(r_kind),parameter:: r25          = one/25.0_r_kind

! Initialize local variables
  pi2=two*pi
  ndy=(nlat-ny)/2
  nxe=nlon/8
  nor=norh*2
  mm1=mype+1
  nlathh=nlat/4
  nf2p=2*nf+1

  if(nc2d>0) then
     allocate(hwllp(0:nlat+1,nc2d))
     allocate(corp(nlat,nc2d))
     allocate(hwllinp(nlat,nc2d))
     hwllp=zero
  endif

  if(nc3d>0)then
     allocate(vz(nsig,0:nlat+1,nc3d))
     allocate(hwll(0:nlat+1,nsig,nc3d))
     allocate(corz(nlat,nsig,nc3d))
     allocate(hwllin(nlat,nsig,nc3d))
     allocate(vscalesin(nsig,nlat,nc3d))
  endif

  allocate(hsst(nlat,nlon))

! Get indexes to required CV variables
  nrf3_oz   = getindex(cvars3d,'oz')
  nrf3_t    = getindex(cvars3d,'t')
  nrf3_sf   = getindex(cvars3d,'sf')
  nrf3_vp   = getindex(cvars3d,'vp')
  nrf3_q    = getindex(cvars3d,'q')
  nrf3_cw   = getindex(cvars3d,'cw')
  nrf2_ps   = getindex(cvars2d,'ps')
  nrf2_sst  = getindex(cvars2d,'sst')
! nrf2_stl  = getindex(cvarsmd,'stl')
! nrf2_sti  = getindex(cvarsmd,'sti')

! Setup blending
  mm=4
  call blend(mm,iblend)

  nolp=nr+1+(ny-nlat)/2
!  nbuf=nolp/4
  nbuf=0
  nmix=nolp-nbuf*2
  dxx=1._r_quad/real((nmix+1),r_quad)
  bl2=0._r_kind
  k=0
  do i=1,nmix
     k=k+1
     x=i*dxx
     y=0._r_quad
     y=real(iblend(mm),r_quad)
     do j=mm-1,0,-1
        y=x*y+real(iblend(j),r_quad)
     enddo
     y=y*x**(mm+1)
     bl2(k)=1._r_quad-y
  enddo
  if(minval(bl2)<zero) then
     write(6,*) 'prewgt: trouble bl2 coeffs negative ', bl2
     call stop2(99)
  endif
  do k=1,nmix    
    bl2(k)=sqrt(bl2(k))
  end do
  
! Modify precision to be consistent with bl2 for higher res grids
! ** NOTE ** bl and bl2 are actually defined to be r_kind
! 
  nmix=(nx-nlon)
  dxx=1._r_quad/real((nmix+1),r_quad)
  ndx=(nx-nlon)
  bl=0._r_kind
  k=ndx-nmix
  do i=1,nmix
     k=k+1
     x=real(i,r_quad)*dxx
     y=0._r_quad
     y=real(iblend(mm),r_quad)
     do j=mm-1,0,-1
        y=x*y+real(iblend(j),r_quad)
     enddo
     y=y*x**real((mm+1),r_quad)
     bl(k)=1._r_quad-y
  enddo

  if(minval(bl)<zero) then
     write(6,*) 'prewgt: trouble bl coeffs negative ', bl
     call stop2(99)
  endif
  do k=1,nmix
     bl(k)=sqrt(bl(k))
  end do

! Setup sea-land mask
  sl=zero
  if(bw /= zero)then
    do j=1,lon1*lat1
       zsm(j)=zero
     end do
     do j=1,lon2
        do i=1,lat2
           temp(i,j)=real(isli2(i,j),r_kind)
        end do
     end do

     call strip(temp,zsm)

     call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
        work1,ijn,displs_g,mpi_rtype,&
        mpi_comm_world,ierror)

     do k=1,iglobal
        i=ltosi(k) ; j=ltosj(k)
        sl(i,j)=work1(k)
     end do



     do j=1,nlon
        do i=1,nlat
           if(sl(i,j) > one)sl(i,j)=zero
        enddo
     enddo
     call smoothww(nlat,nlon,sl,half,2,1)
     do j=1,nlon
        do i=1,nlat
           sl(i,j)=min(max(sl(i,j),zero),one)
        enddo
     enddo
  end if

! Get background error statistics from a file ("berror_stats").
  call berror_read_wgt(corz,corp,hwllin,hwllinp,vscalesin,corsst,hsst,varq,qoption,varcw,cwoption,mype)
  mlat=nlat

! load the horizontal length scales
  hwll=zero
  do j=1,nc3d
     do k=1,nsig
        do i=1,nlat
           hwll(i,k,j)=hwllin(i,k,j)
        end do
     end do
  end do
  if(nrf3_oz>0) hwll(:,:,nrf3_oz)=hwll(:,:,nrf3_oz)*three   !inflate scale

! surface pressure
  if(nrf2_ps>0) then
     do i=1,nlat
        hwllp(i,nrf2_ps)=hwllinp(i,nrf2_ps)
     end do
  endif


! sea surface temperature, convert from km to m
! also calculate a minimum horizontal length scale for
! sst to be used for land skin temp and ice temp
  hsstmin=1.e10_r_single
  minhsst=1.e10_r_kind
  do j=1,nlon
     do i=1,nlat
        hsst(i,j)=r1000*hsst(i,j)
        hsstmin=min(hsstmin,hsst(i,j))
     end do
  end do
  minhsst=hsstmin


! perturb background error
! Things to perturb: as(1-8), hzscl(1-3) and vs(1)
  if (pert_berr) then
     allocate(randfct(nc2d+nc3d+3+1))

     call get_randoms(nc2d+nc3d+3+1,randfct)
     do i=1,nc3d
        as3d(i)=as3d(i)+as3d(i)*randfct(i)
     end do
     ii=nc3d
     do i=1,nc2d
        ii=ii+1
        as2d(i)=as2d(i)+as2d(i)*randfct(ii)
     end do
     do i=1,nhscrf
        hzscl(i)=hzscl(i)+hzscl(i)*randfct(nc2d+nc3d+i)
     end do
     vs=vs+vs*randfct(nc2d+nc3d+3+1)
     if (mype==0) then
        write(6,*) 'PREWGT: REDEFINE AS = ',as3d,as2d
        write(6,*) 'PREWGT: REDEFINE HZSCL = ',hzscl
        write(6,*) 'PREWGT: REDEFINE VS = ',vs
     end if
     deallocate(randfct)
  end if

! As used in the code, the horizontal length scale
! parameters are used in an inverted form.  Invert
! the parameter values here.
  do i=1,nhscrf
     hzscl(i)=one/hzscl(i)
  end do
! apply scaling (deflate/inflate) to vertical length scales
! note: parameter vs needs to be inverted
  vs=one/vs

! Initialize full array to zero before loading part of array below
  vz=zero

! load vertical length scales
  do j=1,nc3d
     do k=1,nsig
        do i=1,nlat
           vz(k,i,j)=vs*vscalesin(k,i,j)
        end do
     end do
  end do

  call rfdpar1(be,rate,ndeg)
  call rfdpar2(be,rate,turn,samp,ndeg)

! Load background error variances onto subdomains
  if(nrf3_sf>0.and.nrf3_vp>0) then
     do k=1,nsig
        do i=1,lat2
           ix=istart(mm1)+i-2
           ix=max(ix,2)
           ix=min(nlat-1,ix)
           do j=1,lon2
              sfvar(i,j,k)=corz(ix,k,nrf3_sf)
              vpvar(i,j,k)=corz(ix,k,nrf3_vp)
           end do
        end do
     end do
  end if
  if(nrf3_t>0) then
     do k=1,nsig
        do i=1,lat2
           ix=istart(mm1)+i-2
           ix=max(ix,2)
           ix=min(nlat-1,ix)
           do j=1,lon2
              tvar(i,j,k)=corz(ix,k,nrf3_t)
           end do
        end do
     end do
  end if

  if(nrf2_ps>0) then
     do i=1,lat2
        ix=istart(mm1)+i-2
        ix=max(ix,2)
        ix=min(nlat-1,ix)
        do j=1,lon2
           psvar(i,j)=corp(ix,nrf2_ps)
        end do
     end do
  end if

! Reweight the variances based on flow dependence if flag set
  if (bkgv_flowdep) then
      call bkgvar_rewgt(sfvar,vpvar,tvar,psvar,mype)
  else
      if (bkgv_write) call write_bkgvars_grid(sfvar,vpvar,tvar,psvar,mype)
  endif

! vertical length scales
!!!$omp parallel do  schedule(dynamic,1) private(i,n,k,j,jx,ix,loc,dsv)
  do n=1,nc3d
     do j=1,lat2         
        jx=istart(mm1)+j-2
        jx=max(jx,2)
        jx=min(nlat-1,jx)
        call smoothzo(vz(1,jx,n),samp,rate,n,j,dsv)

!       load variances onto subdomains
        if (n==nrf3_sf) then
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*sfvar(j,i,k)*as3d(n)   ! streamfunction
              end do
           end do
        else if (n==nrf3_vp) then
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*vpvar(j,i,k)*as3d(n)   ! velocity potential
              end do
           end do
        else if (n==nrf3_t) then
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*tvar(j,i,k)*as3d(n)    ! temperature
              end do
           end do
        else if (n==nrf3_oz.and.adjustozvar) then
           call gsi_bundlegetpointer (gsi_metguess_bundle(ntguessig),'oz',ges_oz,istatus)
           if (istatus==0) then
              do k=1,nsig
                 do i=1,lon2
                    my_corz = max(ges_oz(j,i,k),0.000000002_r_kind)
                    ! Reduce weight in the stratosphere
                    if (my_corz .gt. 0.0000001_r_kind) my_corz = my_corz/4.0 
                    dssv(j,i,k,n)=dsv(i,k)*my_corz*as3d(n)   ! ozone
                 end do
              end do
           endif
        else
           do k=1,nsig
              do i=1,lon2
                 dssv(j,i,k,n)=dsv(i,k)*corz(jx,k,n)*as3d(n)
              end do
           end do
        end if
    enddo
  end do

! Special case of dssv for qoption=2 and cw
  if (qoption==2) call compute_qvar3d

!!!$omp parallel do  schedule(dynamic,1) private(i,n,j,jx,ix,loc)
  do n=1,nc2d
     if (n==nrf2_ps) then
        do j=1,lat2         
           do i=1,lon2
              dssvs(j,i,n)=psvar(j,i)*as2d(n)             ! surface pressure
           end do
        end do
     else if (n==nrf2_sst) then
        do j=1,lat2         
           do i=1,lon2
              if(mvars>=2 .and. isli2(j,i)==1)then
                 dssvs(j,i,nc2d+1)= atsfc_sdv(1)          ! land surface temperature
              else if(mvars>=2 .and. isli2(j,i)==2)then
                 dssvs(j,i,nc2d+2)= atsfc_sdv(2)          ! ice surface temperature
              else
                 jx=istart(mm1)+j-2
                 jx=max(jx,2)
                 jx=min(nlat-1,jx)
                 ix=jstart(mm1)+i-2
                 if (ix==0) ix=nlon
                 ix=max(ix,1)
                 if (ix==nlon+1) ix=1
                 ix=min(nlon,ix)
                 dssvs(j,i,n)=corsst(jx,ix)*as2d(n)        ! sea surface temperature
              end if
           end do
        end do
     end if
  end do

  if (bkgv_write) call write_bkgvars2_grid

! distance of gaussian lat from pole on stereogaphic map
! r=r/(1+z)
  do ir=mr,ubound(rs,1)		! ubound(rs,1) is nlat-2 to skip S.P.
     rs(ir)=cos(rlats(nlat-ir))/(one+sin(rlats(nlat-ir)))
  enddo
  df=tan(pi2/nlon*half)

! set up polcas
  call setwts(wtaxs,wtxrs,inaxs,inxrs,rs,df,nor,nxe,nf,mr,nr)

! set up smooth_polcas if desired (norsp > 0)
  if(norsp>0) call setup_smooth_polcas

! load arrays which are in correct units, to be used to
! define the scales below
  do j=1,nlon
     do i=2,nlat-1
        factx(i,j)=one/(one+(one-sl(i,j))*bw)
     end do
     factx(1,j)=factx(2,j)
     factx(nlat,j)=factx(nlat-1,j)
  end do

  wlipi=nlon/pi2
  wlipih=nlon/pi2*half*samp*samp
  do j=1,nx
     jjj=j-ndx
     if(jjj<1)jjj=nlon+jjj
     if(jjj>nlon)jjj=jjj-nlon
     do i=1,ny
        iii=i+ndy
        scsli(i,j,1)=(rlats(iii+1)-rlats(iii-1))*wlipih*cos(rlats(iii))* &
                     factx(iii,jjj)**2
        scsli(i,j,2)=(rlats(iii     )-rlats(iii-1))*wlipi*factx(iii,jjj)
        scsli(i,j,3)=cos(rlats(iii))*factx(iii,jjj)
     enddo
  enddo

  nxg=nxe+norh
  nrr=ubound(rs,1)	! was nf*3/2
  ndx=(nx-nlon)/2
  call polcasl(factx,fact1,fact2,1,nf,mr,nrr,nor,rs,df,nxe,nxg)
  fact1(0,0)=quarter*(fact1(1,0)+fact1(0,1)+fact1(-1,0)+fact1(0,-1))
  fact2(0,0)=quarter*(fact2(1,0)+fact2(0,1)+fact2(-1,0)+fact2(0,-1))

  df2=df*df
  do j=-nf,nf
     jm=j-1
     j2=j*j
     do i=-nf,nf
        im=i-1
        i2=i*i
        scs12(i,j,1)=(samp/(one+(i2+j2)*df2))**2*fact1(i,j)**2
        scs12(i,j,2)=one/(one+((im*im+i2)*half+j2)*df2)*fact1(i,j)
        scs12(i,j,3)=one/(one+(i2+(j2+jm*jm)*half)*df2)*fact1(i,j)
     enddo
  enddo


! Convert horizontal scales from physical units (m) to grid relative units
! rearth_equator is the equatorial radius from a 1999 IAG report.  The
! horizontal scales are defined at the equator, hence the need for the
! equatorial radius.
  s2u=(two*pi*rearth_equator)/real(nlon,r_kind)


  allocate(sli(ny,nx,2,nnnn1o),sli1(-nf:nf,-nf:nf,2,nnnn1o), &
                               sli2(-nf:nf,-nf:nf,2,nnnn1o))

  allocate(nrf3_loc(nc3d),nrf2_loc(nc2d))
  do ii=1,nc3d
     nrf3_loc(ii)=getindex(cvars,cvars3d(ii))
  enddo
  do ii=1,nc2d
     nrf2_loc(ii)=getindex(cvars,cvars2d(ii))
  enddo

!!!$omp parallel do  schedule(dynamic,1) private(k,k1,j,ii,iii,jjj,i,n,nn,factx,fact1,fact2)
  do k=1,nnnn1o
     k1=levs_id(k)
     if (k1==0) then
        do j=1,nlon
           do i=2,nlat-1
              factx(i,j)=zero
           end do
        end do
     else 
        n=nvar_id(k)
        nn=-1
        do ii=1,nc3d
           if (nrf3_loc(ii)==n) then
              nn=ii
              do j=1,nlon
                 do i=2,nlat-1
                    factx(i,j)=s2u/hwll(i,k1,nn)
                 end do
              end do
              exit
           end if
        end do

        if (nn==-1) then
           do ii=1,nc2d
              if (nrf2_loc(ii)==n .or. n>nrf) then
                 nn=ii
                 if (n>nrf) nn=n-nc3d
                 if (nn==nrf2_sst) then
                    do j=1,nlon
                       do i=2,nlat-1
                          factx(i,j)=s2u/hsst(i,j)
                       end do
                    end do
                 else if (nn>nc2d) then 
                    do j=1,nlon
                       do i=2,nlat-1
                          factx(i,j)=two*s2u/minhsst
                       end do
                    end do
                 else  
                    do j=1,nlon
                       do i=2,nlat-1
                          factx(i,j)=s2u/hwllp(i,nn)
                       end do
                    end do
                 end if
                 exit
              end if
           end do
        end if
     endif    ! end if over nvar_id
     do j=1,nlon
        factx(1,j)=factx(2,j)
        factx(nlat,j)=factx(nlat-1,j)
     end do

     call polcasl(factx,fact1,fact2,1,nf,mr,nrr,nor,rs,df,nxe,nxg)
     fact1(0,0)=quarter*(fact1(1,0)+fact1(0,1)+fact1(-1,0)+fact1(0,-1))
     fact2(0,0)=quarter*(fact2(1,0)+fact2(0,1)+fact2(-1,0)+fact2(0,-1))
! first sli
     do j=1,nx
        jjj=j-ndx
        if(jjj < 1)jjj=jjj+nlon
        if(jjj > nlon)jjj=jjj-nlon
        do i=1,ny
           iii=i+ndy
           slw((j-1)*ny+i,k)=scsli(i,j,1)*factx(iii,jjj)**2
           sli(i,j,1,k)=scsli(i,j,2)*factx(iii,jjj)        
           sli(i,j,2,k)=scsli(i,j,3)*factx(iii,jjj)        
        enddo
     enddo           
! now load sli1/sli2 
     do j=-nf,nf
        do i=-nf,nf
           slw2((j+nf)*nf2p+nf+1+i,k)=scs12(i,j,1)*fact1(i,j)**2
           slw1((j+nf)*nf2p+nf+1+i,k)=scs12(i,j,1)*fact2(i,j)**2
           sli2(i,j,1,k)=scs12(i,j,2)*fact1(i,j)
           sli1(i,j,1,k)=scs12(i,j,2)*fact2(i,j)
           sli2(i,j,2,k)=scs12(i,j,3)*fact1(i,j)
           sli1(i,j,2,k)=scs12(i,j,3)*fact2(i,j)
        enddo
     enddo
  end do ! end do over nsig1o/loadling of sli arrays

  deallocate(nrf3_loc,nrf2_loc)

! Load tables used in recursive filters
  call init_rftable(mype,rate,nnnn1o,sli,sli1,sli2)

! Clean up: first-in, last-out
  deallocate(sli,sli1,sli2)
  deallocate(hsst)

  if(allocated(vscalesin)) deallocate(vscalesin)
  if(allocated(hwllin))    deallocate(hwllin)
  if(allocated(corz))      deallocate(corz)
  if(allocated(hwll))      deallocate(hwll)
  if(allocated(vz))        deallocate(vz)

  if(allocated(hwllinp)) deallocate(hwllinp)
  if(allocated(corp))    deallocate(corp)
  if(allocated(hwllp))   deallocate(hwllp)

  return
end subroutine prewgt

subroutine get_randoms(count,randnums)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_randoms
!   prgmmr: kleist           org: np22              date: 2006-04-24
!
! abstract: get random numbers for perturbing background error parms
!
! program history log:
!   2006-04-21  kleist
!   2008-04-23  safford - rm unused uses
!
!   input argument list:
!     count    - number or random numbers to generate
!
!   output argument list:
!     randnums - array of scaled random numbers
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use obsmod, only: iadate
  use berror, only: pert_berr_fct
  use constants, only: one, two
  implicit none

  integer(i_kind)              ,intent(in   ) :: count
  real(r_kind),dimension(count),intent(  out) :: randnums

  integer(i_kind),allocatable,dimension(:):: numrnds
  real(r_kind),dimension(count+1):: temps
  real(r_kind):: rseed

  integer(i_kind) i,ksize

  call random_seed(size=ksize)
  allocate(numrnds(ksize))

! set seed as a function of analysis date
  rseed = 1e6_r_kind*iadate(1) + 1e4_r_kind*iadate(2) &
       + 1e2_r_kind*iadate(3) + iadate(4)

  do i=1,ksize
     numrnds(i)=rseed
  end do

  call random_seed(put=numrnds)
  deallocate(numrnds)

! this goes from 0-1, but want -1 to 1
  call random_number(temps)

! Set range to be +/- factor
! and don't use first random number generated based on date
  do i=1,count
     randnums(i) = pert_berr_fct*(one - two*temps(i+1))
  end do

  return
end subroutine get_randoms
