subroutine setuppw(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
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
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,r_double,i_kind
  use guess_grids, only: ges_prsi,hrdifsig,nfldsig
  use gridmod, only: lat2,lon2,nsig,get_ij
  use obsmod, only: pwhead,pwtail,rmiss_single,i_pw_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use obsmod, only: pw_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use constants, only: zero,one,tpwcon,r1000,r10, &
       tiny_r_kind,three,half,two,cg_term,huge_single,&
       wgtlim, rd
  use jfunc, only: jiter,last,miter
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use rapidrefresh_cldsurf_mod, only: l_pw_hgt_adjust, l_limit_pw_innov, max_innov_pct
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none

! Declare passed variables
  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is	! ndat index

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
  real(r_kind) zges

  integer(i_kind) ikxx,nn,istat,ibin,ioff,ioff0
  integer(i_kind) i,nchar,nreal,k,j,jj,ii,l,mm1
  integer(i_kind) ier,ilon,ilat,ipw,id,itime,ikx,ipwmax,iqc
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,iobshgt,iobsprs

  logical,dimension(nobs):: luse,muse
  logical proceed
  
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(pw_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  equivalence(rstation_id,station_id)

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_q

  n_alloc(:)=0
  m_alloc(:)=0

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

  read(lunin)data,luse

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
     muse(i)=nint(data(11,i)) <= jiter
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
     ioff0=19
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     ii=0
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
  
!    Link obs to diagnostics structure
     if(luse_obsdiag)then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_pw_ob_type,ibin)%head)) then
              allocate(obsdiags(i_pw_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuppw: failure to allocate obsdiags',istat
                 call stop2(269)
              end if
              obsdiags(i_pw_ob_type,ibin)%tail => obsdiags(i_pw_ob_type,ibin)%head
           else
              allocate(obsdiags(i_pw_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuppw: failure to allocate obsdiags',istat
                 call stop2(270)
              end if
              obsdiags(i_pw_ob_type,ibin)%tail => obsdiags(i_pw_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_pw_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_pw_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_pw_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_pw_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_pw_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_pw_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_pw_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_pw_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_pw_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_pw_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_pw_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_pw_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_pw_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1

        else
           if (.not.associated(obsdiags(i_pw_ob_type,ibin)%tail)) then
              obsdiags(i_pw_ob_type,ibin)%tail => obsdiags(i_pw_ob_type,ibin)%head
           else
              obsdiags(i_pw_ob_type,ibin)%tail => obsdiags(i_pw_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_pw_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setuppw: index error'
              call stop2(271)
           end if
        endif
     endif

     if(.not.in_curbin) cycle
 
     ! Interpolate model PW to obs location
     call tintrp2a11(rp2,pwges,dlat,dlon,dtime, &
        hrdifsig,mype,nfldsig)

! Interpolate pressure at interface values to obs location
     call tintrp2a1(ges_prsi,prsitmp,dlat,dlon,dtime, &
         hrdifsig,nsig+1,mype,nfldsig)

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
     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_pw_ob_type,ibin)%tail%muse(nobskeep)

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

     if(luse_obsdiag)then
        obsdiags(i_pw_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_pw_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_pw_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_pw_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if ( .not. last .and. muse(i)) then

        if(.not. associated(pwhead(ibin)%head))then
           allocate(pwhead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write pwhead '
           pwtail(ibin)%head => pwhead(ibin)%head
        else
           allocate(pwtail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write pwtail%llpoint '
           pwtail(ibin)%head => pwtail(ibin)%head%llpoint
        end if

        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => pwtail(ibin)%head
        my_head%idv = is
        my_head%iob = i

        allocate(pwtail(ibin)%head%dp(nsig),stat=istat)
        if (istat/=0) write(6,*)'MAKECOBS:  allocate error for pwtail_dp, istat=',istat


!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,pwtail(ibin)%head%ij(1),pwtail(ibin)%head%wij(1))

        pwtail(ibin)%head%res    = ddiff
        pwtail(ibin)%head%err2   = error**2
        pwtail(ibin)%head%raterr2= ratio_errors**2  
        pwtail(ibin)%head%time   = dtime
        pwtail(ibin)%head%b      = cvar_b(ikx)
        pwtail(ibin)%head%pg     = cvar_pg(ikx)
        pwtail(ibin)%head%luse   = luse(i)

! Load the delta pressures at the obs location
        do k=1,nsig
           pwtail(ibin)%head%dp(k)=r10*(prsitmp(k)-prsitmp(k+1))
        end do

        if(luse_obsdiag)then
           pwtail(ibin)%head%diags => obsdiags(i_pw_ob_type,ibin)%tail

           my_head => pwtail(ibin)%head
           my_diag => pwtail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                    (/is,i,ibin/))
              call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif
        endif
     endif


!    Save select output for diagnostic file
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
    
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = data(iobsprs,i)    ! observation pressure (hPa)
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

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error

        rdiagbuf(17,ii) = dpw                ! total precipitable water obs (kg/m**2)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (kg/m**2)
        rdiagbuf(19,ii) = dpw-pwges          ! obs-ges w/o bias correction (kg/m**2) (future slot)

        ioff=ioff0
        if (lobsdiagsave) then
           do jj=1,miter 
              ioff=ioff+1 
              if (obsdiags(i_pw_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_pw_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_pw_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_pw_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

     end if


  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     call dtime_show(myname,'diagsave:pw',i_pw_ob_type)
     write(7)' pw',nchar,nreal,ii,mype,ioff0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
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

  subroutine final_vars_
    if(allocated(ges_q )) deallocate(ges_q )
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_z )) deallocate(ges_z )
  end subroutine final_vars_

end subroutine setuppw
