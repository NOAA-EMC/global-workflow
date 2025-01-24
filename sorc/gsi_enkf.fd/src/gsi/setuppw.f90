module pw_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setuppw; end interface

contains
subroutine setuppw(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuppw     compute rhs of oi for total column water
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract:  For total column water, this routine
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu - ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist - modify to use delta(pressure) from guess fields
!   2004-06-17  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of pwwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-02-10  treadon - move initialization of dp_pw into routine sprpw
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-14  pondeca - correct error in diagnostic array index
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-02  treadon - rename prsi as ges_prsi
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc
!   2006-08-28      su  - fix a bug in variational qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su  - modify gross check error 
!   2008-12-03  todling - changed handle of tail%time
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-11-19  Hofmann - doing precipitable water (PW) height adjustment
!                                       based on obs vs. model height
!   2013-01-26  parrish -  change tintrp2a to tintrp2a1, tintrp2a11 (so debug compile works on WCOSS)
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-12-09  mccarty - add netcdf_diag capability
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!
!   input argument list:
!     lunin    - unit from which to read observations
!     mype     - mpi task id
!     nele     - number of data elements per observation
!     nobs     - number of observations
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr,getindex
  use kinds, only: r_kind,r_single,r_double,i_kind
  use guess_grids, only: ges_prsi,hrdifsig,nfldsig
  use gridmod, only: lat2,lon2,nsig,get_ij
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,lobsdiag_forenkf,ianldate,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use m_obsNode, only: obsNode
  use m_pwNode, only: pwNode
  use m_pwNode, only: pwNode_appendto
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use constants, only: zero,one,tpwcon,r1000,r10, &
       tiny_r_kind,three,half,two,cg_term,huge_single,&
       wgtlim, rd
  use jfunc, only: jiter,last,miter,jiterstart
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use rapidrefresh_cldsurf_mod, only: l_pw_hgt_adjust, l_limit_pw_innov, max_innov_pct
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use state_vectors, only: svars3d, levels
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare local parameter
  character(len=*),parameter:: myname='setuppw'

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: stop2

! Declare local variables
  real(r_double) rstation_id
  real(r_kind):: pwges,grsmlt,dlat,dlon,dtime,obserror, &
       obserrlm,residual,ratio,dpw
  real(r_kind) error,ddiff, pw_diff
  real(r_kind) ressw2,ress,scale,val2,val,valqc
  real(r_kind) rat_err2,exp_arg,term,ratio_errors,rwgt
  real(r_kind) cg_pw,wgross,wnotgross,wgt,arg
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind),dimension(nobs)::dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(lat2,lon2,nfldsig)::rp2
  real(r_kind),dimension(nsig+1):: prsitmp
  real(r_kind),dimension(nsig):: qges, tvges
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  real(r_kind) zges, prest

  integer(i_kind) ikxx,nn,istat,ibin,ioff,ioff0
  integer(i_kind) i,nchar,nreal,k,j,jj,ii,l,mm1
  integer(i_kind) ier,ilon,ilat,ipw,id,itime,ikx,ipwmax,iqc
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,iobshgt,iobsprs

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed
  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  type(sparr2) :: dhx_dx
  integer(i_kind) :: q_ind, nnz, nind

  logical:: in_curbin, in_anybin, save_jacobian
  type(pwNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  equivalence(rstation_id,station_id)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q
  type(obsLList),pointer,dimension(:):: pwhead
  pwhead => obsLL(:)

  save_jacobian = conv_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

  grsmlt=three  ! multiplier factor for gross check
  mm1=mype+1
  scale=one

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!******************************************************************************
! Read and reformat observations in work arrays.
! Simulate tpw from guess (forward model)
  rp2=zero
  do jj=1,nfldsig
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              rp2(i,j,jj)=rp2(i,j,jj) + ges_q(i,j,k,jj) * &
                   tpwcon*r10*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj))    ! integrate q
           end do
        end do
     end do
  end do

  read(lunin)data,luse,ioid

!        index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipw = 4     ! index of pw observations
  id=5        ! index of station id
  itime=6     ! index of observation time in data array
  ikxx=7      ! index of ob type
  ipwmax=8    ! index of pw max error
  iqc=9       ! index of quality mark
  ier2=10     ! index of original-original obs error ratio
  iuse=11     ! index of use parameter
  ilone=12    ! index of longitude (degrees)
  ilate=13    ! index of latitude (degrees)
  istnelv=14  ! index of station elevation (m)
  iobsprs=15  ! index of observation pressure (hPa)
  iobshgt=16  ! index of observation height (m)

  do i=1,nobs
     muse(i)=nint(data(11,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do

  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l)) then
           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     nchar=1
     ioff0=20
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     if (save_jacobian) then
       nnz   = nsig                   ! number of non-zero elements in dH(x)/dx profile
       nind   = 1
       call new(dhx_dx, nnz, nind)
       nreal = nreal + size(dhx_dx)
     endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     ii=0
     if(netcdf_diag) call init_netcdf_diag_
  end if


! Prepare total precipitable water data
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
 

        dpw=data(ipw,i)
        ikx = nint(data(ikxx,i))
        error=data(ier2,i)

        ratio_errors=error/data(ier,i)
        error=one/error
     endif ! (in_curbin)

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin
  
     if (luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        my_diag => obsdiagLList_nextNode(my_diagLL      ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

        if(.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =',.not.lobsdiag_allocated)
     endif

     if(.not.in_curbin) cycle
 
     ! Interpolate model PW to obs location
     call tintrp2a11(rp2,pwges,dlat,dlon,dtime, &
        hrdifsig,mype,nfldsig)

! Interpolate pressure at interface values to obs location
     call tintrp2a1(ges_prsi,prsitmp,dlat,dlon,dtime, &
         hrdifsig,nsig+1,mype,nfldsig)
     prest=prsitmp(1)*r10   ! model surface pressure(mb) at obs loction

     if (save_jacobian) then
        q_ind =getindex(svars3d,'q')
        if (q_ind < 0) then
           print *, 'Error: no variable q in state vector. Exiting.'
           call stop2(1300)
        endif

        dhx_dx%st_ind(1)  = 1    + sum(levels(1:q_ind-1))
        dhx_dx%end_ind(1) = nsig + sum(levels(1:q_ind-1))

        do k = 1, nsig
           dhx_dx%val(k) = tpwcon*r10*(prsitmp(k)-prsitmp(k+1))
        enddo
     endif


     if(.not.l_pw_hgt_adjust) then
        ! Compute innovation
        ddiff = dpw - pwges
     else

        ! Interpolate model q to obs location
        call tintrp2a1(ges_q,qges,dlat,dlon,dtime, &
             hrdifsig,nsig,mype,nfldsig)
        
        ! Interpolate model T_v to obs location
        call tintrp2a1(ges_tv,tvges,dlat,dlon,dtime, &
             hrdifsig,nsig,mype,nfldsig)
        
        ! Interpolate model z to obs location
        call tintrp2a11(ges_z,zges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
        
        ! Calculate difference in PW from station elevation to model surface elevation
        pw_diff = (zges - data(istnelv,i)) * (prsitmp(1)*r1000*qges(1)) / (rd*tvges(1))
        
        ! Compute innovation
        ddiff = dpw - pw_diff - pwges
     end if

     if (l_limit_pw_innov) then
        ! Limit size of PW innovation to a percent of the background value
        ddiff = sign(min(abs(ddiff),max_innov_pct*pwges),ddiff)
     end if

!    Gross checks using innovation

     residual = abs(ddiff)
     if (residual>grsmlt*data(ipwmax,i)) then
        error = zero
        ratio_errors=zero
        if (luse(i)) awork(7) = awork(7)+one
     end if
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     ratio    = residual/obserrlm
     if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors=zero
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if
     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

     val      = error*ddiff

     if(luse(i))then
!    Compute penalty terms (linear & nonlinear qc).
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_pw=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_pw*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

! Accumulate statistics as a function of observation type.
        ress  = ddiff*scale
        ressw2= ress*ress
        val2  = val*val
        rat_err2 = ratio_errors**2
!       Accumulate statistics for obs belonging to this task
        if (muse(i) ) then
           if(rwgt < one) awork(21) = awork(21)+one
           awork(5) = awork(5)+val2*rat_err2
           awork(4) = awork(4)+one
           awork(22)=awork(22)+valqc
           nn=1
        else
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one             ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress            ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2          ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2   ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc           ! nonlin qc penalty
        
     end if

     if (luse_obsdiag) then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=ddiff)
     endif

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then

        allocate(my_head)
        call pwNode_appendto(my_head,pwhead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

        allocate(my_head%dp(nsig),stat=istat)
        if (istat/=0) write(6,*)'MAKECOBS:  allocate error for pwhead_dp, istat=',istat


!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,my_head%ij,my_head%wij)

        my_head%res    = ddiff
        my_head%err2   = error**2
        my_head%raterr2= ratio_errors**2  
        my_head%time   = dtime
        my_head%b      = cvar_b(ikx)
        my_head%pg     = cvar_pg(ikx)
        my_head%luse   = luse(i)

! Load the delta pressures at the obs location
        do k=1,nsig
           my_head%dp(k)=r10*(prsitmp(k)-prsitmp(k+1))
        end do

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag, my_head%idv,my_head%iob,1, myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()
     endif


!    Save select output for diagnostic file
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        err_input = data(ier2,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input=one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
        if (err_final>tiny_r_kind) errinv_final=one/err_final

        if(binary_diag) call contents_binary_diag_(my_diag)
        if(netcdf_diag) call contents_netcdf_diag_(my_diag)

     end if


  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
        write(7)' pw',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     end if
  end if

! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::q', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get q ...
     varname='q'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_q))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_q(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_q(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_q(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.

     write(string,900) jiter
900  format('conv_pw_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        if (save_jacobian) then
          call nc_diag_header("jac_nnz", nnz)
          call nc_diag_header("jac_nind", nind)
        endif
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = prest              ! use model surface pressure (hPa) so PW
                                             ! can be used in EnKF analysis
        rdiagbuf(7,ii)  = data(iobshgt,i)    ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif


        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error

        rdiagbuf(17,ii) = dpw                ! total precipitable water obs (kg/m**2)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (kg/m**2)
        rdiagbuf(19,ii) = dpw-pwges          ! obs-ges w/o bias correction (kg/m**2) (future slot)
        rdiagbuf(20,ii) = 1.e+10_r_single    ! ensemble ges spread (filled in by EnKF)

        ioff=ioff0
        if (lobsdiagsave) then
           do jj=1,miter 
              ioff=ioff+1 
              if (odiag%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = odiag%obssen(jj)
           enddo
        endif

        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(ioff+1:nreal,ii))
           ioff = ioff + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
! use model surface pressure, so PW can be used in EnKF analysis
  character(7),parameter     :: obsclass = '     pw'
  real(r_single),parameter::     missing = -9.99e9_r_single
  real(r_kind),dimension(miter) :: obsdiag_iuse

           call nc_diag_metadata("Station_ID",                    station_id              )
           call nc_diag_metadata("Observation_Class",             obsclass                )
           call nc_diag_metadata("Observation_Type",              ictype(ikx)             )
           call nc_diag_metadata("Observation_Subtype",           icsubtype(ikx)          )
           call nc_diag_metadata_to_single("Latitude",            data(ilate,i)           )
           call nc_diag_metadata_to_single("Longitude",           data(ilone,i)           )
           call nc_diag_metadata_to_single("Station_Elevation",   data(istnelv,i)         )
           call nc_diag_metadata_to_single("Pressure",            prest                   )
           call nc_diag_metadata_to_single("Height",              data(iobshgt,i)         )
           call nc_diag_metadata_to_single("Time",                dtime,time_offset,'-'   )
           call nc_diag_metadata_to_single("Prep_QC_Mark",        data(iqc,i)             )
           call nc_diag_metadata_to_single("Prep_Use_Flag",       data(iuse,i)            )
           call nc_diag_metadata("Setup_QC_Mark",                 missing                 )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",          1._r_single             )
           else
              call nc_diag_metadata("Analysis_Use_Flag",          -1._r_single            )
           endif
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt                    )
           call nc_diag_metadata_to_single("Errinv_Input",        errinv_input            )
           call nc_diag_metadata_to_single("Errinv_Adjust",       errinv_adjst            )
           call nc_diag_metadata_to_single("Errinv_Final",        errinv_final            )
           call nc_diag_metadata_to_single("Observation",         dpw                     )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff            )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted", dpw,pwges,'-' )
           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                    obsdiag_iuse(jj) =  one
                 else
                    obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )              
           endif

          if (save_jacobian) then
            call nc_diag_data2d("Observation_Operator_Jacobian_stind", dhx_dx%st_ind)
            call nc_diag_data2d("Observation_Operator_Jacobian_endind", dhx_dx%end_ind)
            call nc_diag_data2d("Observation_Operator_Jacobian_val", real(dhx_dx%val,r_single))
          endif

  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_z )) deallocate(ges_z )
  end subroutine final_vars_

end subroutine setuppw
end module pw_setup
