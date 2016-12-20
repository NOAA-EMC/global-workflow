subroutine setupps(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupps     compute rhs of oi for surface pressure
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: For surface pressure observations, this routine
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
!   2004-06-17  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-06  parrish - increase size of pwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-12  derber  - rewrite and incorporate prep routine   
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-10-21  su   - modified variational quality control and diagnose output
!   2005-11-03  treadon - correct error in ilone,ilate data array indices
!   2005-11-22  wu      - add option to perturb conventional obs
!   2005-11-29  derber - remove psfcg and use ges_lnps instead
!   2006-01-31  todling - storing wgt/wgtlim in diag file instead of wgt only
!   2006-02-02  treadon - rename lnprsl as ges_lnprsl
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-21  treadon - modify optional perturbation to observation
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!   2006-07-31  kleist - change analysis variable to ps (cb) instead of lnps
!   2006-08-28      su - fix a bug in variational qc
!   2007-03-09      su - modify obs perturbation 
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su - modify the error used in gross check
!   2008-03-24      wu - oberror tuning and perturb obs
!   2008-05-23  safford - rm unused vars and uses
!   2008-12-03  todling - changed handle of tail%time
!   2009-02-06  pondeca - for each observation site, add the following to the
!                         diagnostic file: local terrain height, dominate surface
!                         type, station provider name and station subprovider name
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-05-06  Su      - modify the observation gross check error
!   2011-08-09  pondeca - correct bug in qcgross use
!   2013-01-26  parrish - change grdcrd to grdcrd1, intrp2a to intrp2a11,
!                                  tintrp2a to tintrp2a1, tintrp2a11,
!                                  tintrp3 to tintrp31 (so debug compile works on WCOSS)
!   2013-10-19  todling - metguess now holds background 
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
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
  use obsmod, only: rmiss_single,pstail,pshead,perturb_obs,oberror_tune,&
                    i_ps_ob_type,obsdiags,lobsdiagsave,nobskeep,lobsdiag_allocated,&
                    time_offset
  use obsmod, only: ps_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use oneobmod, only: magoberr,maginnov,oneobtest
  use gridmod, only: nsig,get_ij,twodvar_regional
  use constants, only: zero,one_tenth,one,half,pi,g_over_rd, &
             huge_r_kind,tiny_r_kind,two,cg_term,huge_single, &
             r1000,wgtlim,tiny_single,r10,three
  use jfunc, only: jiter,last,jiterstart,miter
  use qcmod, only: dfact,dfact1,npres_print,njqc,vqc
  use guess_grids, only: hrdifsig,ges_lnprsl,nfldsig,ntguessig
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype,icsubtype

  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle

  implicit none

! Declare passed variables
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  integer(i_kind)                                  ,intent(in   ) :: is     ! ndat index
  logical                                          ,intent(in   ) :: conv_diagsave

! Declare local parameters
  character(len=*),parameter:: myname='setupps'
  real(r_kind),parameter:: r0_7=0.7_r_kind

! Declare external calls for code analysis
  external:: intrp2a
  external:: tintrp2a1
  external:: tintrp3
  external:: grdcrd1
  external:: stop2

! Declare local variables
  real(r_double) rstation_id
  real(r_kind) tges,tges2,drbx,pob,pges,psges,psges2,dlat,dlon,dtime,var_jb
  real(r_kind) rdelz,rdp,halfpi,obserror,obserrlm,drdp,residual,ratio
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final,tfact
  real(r_kind) zsges,pgesorig,rwgt
  real(r_kind) r0_005,r0_2,r2_5,tmin,tmax,half_tlapse
  real(r_kind) ratio_errors,error,dhgt,ddiff,dtemp
  real(r_kind) val2,ress,ressw2,val,valqc
  real(r_kind) cg_ps,wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2,qcgross
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nsig):: prsltmp
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  integer(i_kind) ier,ilon,ilat,ipres,ihgt,itemp,id,itime,ikx,iqc,iptrb,ijb
  integer(i_kind) ier2,iuse,ilate,ilone,istnelv,idomsfc,izz,iprvd,isprvd
  integer(i_kind) ikxx,nn,istat,ibin,ioff,ioff0
  integer(i_kind) i,nchar,nreal,ii,jj,k,l,mm1

  logical,dimension(nobs):: luse,muse
  logical proceed
 
  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(ps_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)
  
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  n_alloc(:)=0
  m_alloc(:)=0
!*******************************************************************************
! Read observations in work arrays.

  read(lunin)data,luse

!        index information for data array (see reading routine)
  ier=1       ! index of obs error 
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  ihgt=5      ! index of surface height
  itemp=6     ! index of surface temperature observation
  id=7        ! index of station id
  itime=8     ! index of observation time in data array
  ikxx=9      ! index of ob type
  iqc=10      ! index of quality mark
  ier2=11     ! index of original-original obs error ratio
  iuse=12     ! index of use parameter
  idomsfc=13  ! index of dominant surface type
  ilone=14    ! index of longitude (degrees)
  ilate=15    ! index of latitude (degrees)
  istnelv=16  ! index of station elevation (m)
  izz=17      ! index of surface height
  iprvd=18    ! index of observation provider
  isprvd=19   ! index of observation subprovider
  ijb=20      ! index of non linear qc parameter
  iptrb=21    ! index of ps perturbation

! Declare local constants
  halfpi = half*pi
  r0_005 = 0.005_r_kind
  r0_2=0.2_r_kind
  r2_5=2.5_r_kind
  tmin=150.0_r_kind
  tmax=350.0_r_kind
  half_tlapse=0.00325_r_kind  ! half of 6.5K/1km
  mm1=mype+1
  var_jb=zero

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!  Check to see if observation should be used or monitored
!  muse = true  then used

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  dup=one
!  handle multiple reported data at a station
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and. &
           data(ilon,k) == data(ilon,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l))then
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
     if (twodvar_regional) then; nreal=nreal+2; allocate(cprvstg(nobs),csprvstg(nobs)); endif
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     ii=0
  end if
  call dtime_setup()
  do i = 1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        error=data(ier2,i)
        dlon=data(ilon,i)
        dlat=data(ilat,i)
        pob=data(ipres,i)
 
        dhgt=data(ihgt,i)
        dtemp=data(itemp,i)
        ikx  = nint(data(ikxx,i))
        var_jb=data(ijb,i)
     endif
 
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
           if (.not.associated(obsdiags(i_ps_ob_type,ibin)%head)) then
              allocate(obsdiags(i_ps_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupps: failure to allocate obsdiags',istat
                 call stop2(266)
              end if
              obsdiags(i_ps_ob_type,ibin)%tail => obsdiags(i_ps_ob_type,ibin)%head
           else
              allocate(obsdiags(i_ps_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupps: failure to allocate obsdiags',istat
                 call stop2(267)
              end if
              obsdiags(i_ps_ob_type,ibin)%tail => obsdiags(i_ps_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_ps_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_ps_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_ps_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_ps_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_ps_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_ps_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_ps_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_ps_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_ps_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_ps_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_ps_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_ps_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_ps_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1
        else
           if (.not.associated(obsdiags(i_ps_ob_type,ibin)%tail)) then
              obsdiags(i_ps_ob_type,ibin)%tail => obsdiags(i_ps_ob_type,ibin)%head
           else
              obsdiags(i_ps_ob_type,ibin)%tail => obsdiags(i_ps_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_ps_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setupps: index error'
              call stop2(268)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

!    Load obs error into local variable
     obserror = max(cermin(ikx)*one_tenth,&
          min(cermax(ikx)*one_tenth,data(ier,i)))

! Get guess sfc hght at obs location

     call intrp2a11(ges_z(1,1,ntguessig),zsges,dlat,dlon,mype)

! Interpolate to get log(ps) and log(pres) at mid-layers
! at obs location/time

     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig,&
        mype,nfldsig)
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig,&
        nsig,mype,nfldsig)

! Convert pressure to grid coordinates

     pgesorig = psges

! Take log for vertical interpolation
     psges = log(psges)
     call grdcrd1(psges,prsltmp,nsig,-1)

! Get guess temperature at observation location and surface

     call tintrp31(ges_tv,tges,dlat,dlon,psges,dtime, &
          hrdifsig,mype,nfldsig)

! Adjust observation error and obs value due to differences in surface height

     rdelz=dhgt-zsges
     if(dtemp > tmin .and. dtemp < tmax) then

!  Case of observed surface temperature

        drbx = half*abs(tges-dtemp)+r0_2+r0_005*abs(rdelz)
        tges = half*(tges+dtemp)
     else

!  No observed temperature 
        psges2=log(data(ipres,i))
        call grdcrd1(psges2,prsltmp,nsig,-1)
        call tintrp31(ges_tv,tges2,dlat,dlon,psges2,dtime, &
             hrdifsig,mype,nfldsig)
 
        drbx = half*abs(tges-tges2)+r2_5+r0_005*abs(rdelz)
        tges = half*(tges+tges2)
 
! Extrapolate surface temperature below ground at 6.5 k/km
! note only extrapolating .5dz, if no surface temp available.

        if(rdelz < zero)then
           tges=tges-half_tlapse*rdelz
           drbx=drbx-half_tlapse*rdelz
        end if

     end if

! Adjust guess hydrostatically 

     rdp = g_over_rd*rdelz/tges

! Subtract off dlnp correction, then convert to pressure (cb)
     pges = exp(log(pgesorig) - rdp)


! observational error adjustment 

     drdp=zero
     if (.not.twodvar_regional) then
       drdp = pges*(g_over_rd*abs(rdelz)*drbx/(tges**2))
     endif

!  find adjustment to observational error (in terms of ratio)
     ratio_errors=error/(data(ier,i)+drdp)
     error=one/error

! Compute innovations
     ddiff=pob-pges  ! in cb

! Oberror Tuning and Perturb Obs
     if(muse(i)) then
        if(oberror_tune )then
           if( jiter > jiterstart ) then
              ddiff=ddiff+data(iptrb,i)/error/ratio_errors
           endif
        else if(perturb_obs )then
           ddiff=ddiff+data(iptrb,i)/error/ratio_errors
        endif
     endif

!    Gross check using innovation normalized by error

     obserror = min(r10/max(ratio_errors*error,tiny_r_kind),huge_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(r10*ddiff)
     ratio    = residual/obserrlm

! modify gross check limit for quality mark=3
     if(data(iqc,i) == three ) then
        qcgross=r0_7*cgross(ikx)
     else
        qcgross=cgross(ikx)
     endif

     if (ratio > qcgross .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors = zero
     else
        ratio_errors = ratio_errors/sqrt(dup(i))
     end if

     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.

! If requested, setup for single obs test.

     if (oneobtest) then
        maginnov=one_tenth*maginnov
        magoberr=one_tenth*magoberr
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
        muse(i) = .true.
     endif

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_ps_ob_type,ibin)%tail%muse(nobskeep)

! Compute penalty terms, and accumulate statistics.

     val      = error*ddiff

     if(luse(i))then

!    Compute penalty terms (linear & nonlinear qc).
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if(njqc  .and. var_jb>tiny_r_kind .and. var_jb < 10.0_r_kind .and. error >tiny_r_kind)  then
           if(exp_arg  == zero) then
              wgt=one
           else
              wgt=ddiff*error/sqrt(two*var_jb)
              wgt=tanh(wgt)/wgt
           endif
           term=-two*var_jb*rat_err2*log(cosh((val)/sqrt(two*var_jb)))
           rwgt = wgt/wgtlim
           valqc = -two*term
        else if (vqc  .and. (cvar_pg(ikx)> tiny_r_kind) .and. (error >tiny_r_kind)) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_ps=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_ps*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
           valqc = -two*rat_err2*term
        else
           term = exp_arg
           wgt  = one 
           rwgt = wgt/wgtlim
           valqc = -two*rat_err2*term
        endif
        if (muse(i)) then
!       Accumulate statistics for obs used belonging to this task        
           if(rwgt < one) awork(21) = awork(21)+one
           awork(4)=awork(4)+val2*rat_err2
           awork(5)=awork(5)+one
           awork(22)=awork(22)+valqc
           nn=1
        else
       
!       rejected obs 
           nn=2
!       monitored obs 
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if

!     Accumulate statistics for each ob type

        ress   = ddiff*r10
        ressw2 = ress*ress
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one              ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress             ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2           ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2    ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc            ! nonlin qc penalty
 
     end if

     if(luse_obsdiag)then
        obsdiags(i_ps_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_ps_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_ps_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_ps_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     end if


     if (.not. last .and. muse(i)) then
!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
!    if no minimization (inner loop), do not load arrays


        if(.not. associated(pshead(ibin)%head))then
           allocate(pshead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write pshead '
           pstail(ibin)%head => pshead(ibin)%head
        else
           allocate(pstail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write pstail%llpoint '
           pstail(ibin)%head => pstail(ibin)%head%llpoint
        end if

        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => pstail(ibin)%head
        my_head%idv = is
        my_head%iob = i

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,pstail(ibin)%head%ij(1),pstail(ibin)%head%wij(1))

        pstail(ibin)%head%res      = ddiff
        pstail(ibin)%head%err2     = error**2
        pstail(ibin)%head%raterr2  = ratio_errors**2     
        pstail(ibin)%head%time     = dtime
        pstail(ibin)%head%b        = cvar_b(ikx)
        pstail(ibin)%head%pg       = cvar_pg(ikx)
        pstail(ibin)%head%jb       = var_jb
        pstail(ibin)%head%luse     = luse(i)
        if(oberror_tune) then
           pstail(ibin)%head%kx    = ikx        ! data type for oberror tuning
           pstail(ibin)%head%ppertb= data(iptrb,i)/error/ratio_errors ! obs perturbation
        endif

        if(luse_obsdiag)then
           pstail(ibin)%head%diags => obsdiags(i_ps_ob_type,ibin)%tail

           my_head => pstail(ibin)%head
           my_diag => pstail(ibin)%head%diags
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

! Save obs and simulated surface pressure data for diagnostic output

     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id

        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = data(ipres,i)*r10  ! observation pressure (hPa)
        rdiagbuf(7,ii)  = dhgt               ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = var_jb             ! non linear qc parameter
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one                    
        endif

        pob      = pob*r10
        pges     = pges*r10
        pgesorig = pgesorig*r10

        err_input = data(ier2,i)*r10   ! r10 converts cb to mb
        err_adjst = data(ier,i)*r10
        if (ratio_errors*error/r10>tiny_r_kind) then
           err_final = r10/(ratio_errors*error)
        else
           err_final = huge_single
        endif

        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_single) errinv_input = one/err_input
        if (err_adjst>tiny_single) errinv_adjst = one/err_adjst
        if (err_final>tiny_single) errinv_final = one/err_final

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (hPa**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (hPa**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (hPa**-1)

        rdiagbuf(17,ii) = pob                ! surface pressure observation (hPa)
        rdiagbuf(18,ii) = pob-pges           ! obs-ges used in analysis (coverted to hPa)
        rdiagbuf(19,ii) = pob-pgesorig       ! obs-ges w/o adjustment to guess surface pressure (hPa)

        ioff=ioff0
        if (lobsdiagsave) then
           do jj=1,miter
              ioff=ioff+1
              if (obsdiags(i_ps_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_ps_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_ps_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_ps_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

        if (twodvar_regional) then
           rdiagbuf(ioff+1,ii) = data(idomsfc,i) ! dominate surface type
           rdiagbuf(ioff+2,ii) = data(izz,i)     ! model terrain at ob location
           r_prvstg            = data(iprvd,i)
           cprvstg(ii)         = c_prvstg        ! provider name
           r_sprvstg           = data(isprvd,i)
           csprvstg(ii)        = c_sprvstg       ! subprovider name
        endif

     end if

! End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file

  if(conv_diagsave .and. ii>0)then
     call dtime_show(myname,'diagsave:ps',i_ps_ob_type)
     write(7)' ps',nchar,nreal,ii,mype,ioff0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)

     if (twodvar_regional) then
        write(7)cprvstg(1:ii),csprvstg(1:ii)
        deallocate(cprvstg,csprvstg)
     endif
  end if
  
! End of routine
  return 

  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  call gsi_metguess_get ('var::tv', ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2
  real(r_kind),dimension(:,:,:),pointer:: rank3
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
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
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupps
