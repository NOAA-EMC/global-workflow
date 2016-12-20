subroutine setuptcp(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuptcp                     setup tcpel data
!   prgmmr: kleist            org: np20                date: 2009-02-02
!
! abstract:  Setup routine for TC MSLP data
!
! program history log:
!   2009-02-02  kleist
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2010-05-25  kleist  - output tc_ps observations to conv diag file
!   2010-11-24  todling - add component to write obs sensitiviy to diag file
!   2013-01-26  parrish - change grdcrd to grdcrd1, intrp2a to intrp2a11,
!                          tintrp2a to tintrp2a1, tintrp2a11 (so debug compile works on WCOSS)
!   2013-10-19  todling - metguess now holds background
!   2014-01-28  todling - write sensitivity slot indicator (idia) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use mpeu_util, only: die,perr
  use kinds, only: r_kind,i_kind,r_single,r_double
  use obsmod, only: tcptail,tcphead,obsdiags,i_tcp_ob_type, &
             nobskeep,lobsdiag_allocated,oberror_tune,perturb_obs, &
             time_offset,rmiss_single,lobsdiagsave
  use obsmod, only: tcp_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use qcmod, only: npres_print
  use guess_grids, only: ges_lnprsl,nfldsig,hrdifsig, &
          ntguessig
  use gridmod, only: get_ij,nsig
  use constants, only: zero,half,one,tiny_r_kind,two,cg_term, &
          wgtlim,g_over_rd,huge_r_kind,pi,huge_single,tiny_single,r10
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype,&
          icsubtype
  use jfunc, only: jiter,last,jiterstart,miter
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none

  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  integer(i_kind)                                  ,intent(in   ) :: is	! ndat index

  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork ! obs-ges stats
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork ! data counts and gross checks

  logical                                          ,intent(in)    :: conv_diagsave

! Declare external calls for code analysis
  external:: intrp2a11,tintrp2a1,tintrp2a11
  external:: tintrp3
  external:: grdcrd1
  external:: stop2

! DECLARE LOCAL PARMS HERE
  real(r_double) rstation_id
  character(8) station_id
  equivalence(rstation_id,station_id)

  logical,dimension(nobs):: luse,muse
  logical proceed

  real(r_kind) err_input,err_adjst,err_final,errinv_input,errinv_adjst,errinv_final
  real(r_kind) scale,ratio,obserror,obserrlm
  real(r_kind) residual,ress,ressw2,val,val2
  real(r_kind) valqc,tges,tges2
  real(r_kind) wgross,wnotgross,wgt,arg,exp_arg,term,rat_err2
  real(r_kind) rwgt,cg_ps,drbx
  real(r_kind) error,dtime,dlon,dlat,r0_001,r2_5,r0_2,rsig
  real(r_kind) ratio_errors,psges,zsges,rdp,drdp
  real(r_kind) pob,pges,pgesorig,half_tlapse,ddiff,halfpi,r0_005,rdelz,psges2

  real(r_kind),dimension(nele,nobs):: data
  real(r_kind),dimension(nsig)::prsltmp

  integer(i_kind) i,jj
  integer(i_kind) mm1,idia,idia0
  integer(i_kind) ikxx,nn,istat,iuse,ibin,iptrb,id
  integer(i_kind) ier,ilon,ilat,ipres,itime,ikx,ilate,ilone

  logical:: in_curbin, in_anybin
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(tcp_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  character(len=*),parameter:: myname='setuptcp'

  character(8),allocatable,dimension(:):: cdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) nchar,nreal,ii

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  n_alloc(:)=0
  m_alloc(:)=0

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

!******************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

!    index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ipres=4     ! index of pressure
  itime=5     ! index of time observation
  ikxx=6      ! index of observation type in data array
  ilone=7     ! index of longitude (degrees)
  ilate=8     ! index of latitude (degrees)
  iuse=9      ! index of usage parameter
  id=10       ! index of storm name

  mm1=mype+1
  scale=one
  rsig=nsig
  halfpi = half*pi
  r0_005 = 0.005_r_kind
  r0_2=0.2_r_kind
  r2_5=2.5_r_kind
  half_tlapse=0.00325_r_kind  ! half of 6.5K/1km
  r0_001=0.001_r_kind

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  if(conv_diagsave)then
     nchar=1
     idia0=19
     nreal=idia0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     ii=0
  end if


  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        pob=data(ipres,i)

        error=data(ier,i)
        ikx=nint(data(ikxx,i))
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
           if (.not.associated(obsdiags(i_tcp_ob_type,ibin)%head)) then
              allocate(obsdiags(i_tcp_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuptcp: failure to allocate obsdiags',istat
                 call stop2(301)
              end if
              obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%head
           else
              allocate(obsdiags(i_tcp_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuptcp: failure to allocate obsdiags',istat
                 call stop2(302)
              end if
              obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_tcp_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_tcp_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_tcp_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_tcp_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_tcp_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_tcp_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_tcp_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_tcp_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_tcp_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_tcp_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_tcp_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1
        else
           if (.not.associated(obsdiags(i_tcp_ob_type,ibin)%tail)) then
              obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%head
           else
              obsdiags(i_tcp_ob_type,ibin)%tail => obsdiags(i_tcp_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_tcp_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setuptcp: index error'
              call stop2(303)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

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
     rdelz=-zsges

!  No observed temperature
     psges2=data(ipres,i)
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

! Adjust guess hydrostatically
     rdp = g_over_rd*rdelz/tges

! Subtract off dlnp correction, then convert to pressure (cb)
     pges = exp(log(pgesorig) - rdp)

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

! observational error adjustment
     drdp = pges*(g_over_rd*abs(rdelz)*drbx/(tges**2))

!  find adjustment to observational error (in terms of ratio)
     ratio_errors=error/(error+drdp)
     error=one/error

!    Gross error checks
     obserror = min(r10/max(ratio_errors*error,tiny_r_kind),huge_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(r10*ddiff)
     ratio    = residual/obserrlm
     if (ratio > cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors = zero
     else
! No duplicate check 
     end if

     if (ratio_errors*error <= tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_tcp_ob_type,ibin)%tail%muse(nobskeep)

     val      = error*ddiff
     if(luse(i))then

!       Compute penalty terms (linear & nonlinear qc).
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error >tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_ps=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_ps*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term
 

        if (muse(i)) then
!          Accumulate statistics for obs used belonging to this task
           if(rwgt < one) awork(21) = awork(21)+one
           awork(4)=awork(4)+val2*rat_err2
           awork(5)=awork(5)+one
           awork(22)=awork(22)+valqc
           nn=1
        else

!          rejected obs
           nn=2
!          monitored obs
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if


!       Accumulate statistics for each ob type

        ress   = ddiff*r10
        ressw2 = ress*ress
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one              ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress             ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2           ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2    ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc            ! nonlin qc penalty

     end if

     if(luse_obsdiag)then
        obsdiags(i_tcp_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_tcp_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_tcp_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     end if

     if (.not. last .and. muse(i)) then

        if(.not. associated(tcphead(ibin)%head))then
           allocate(tcphead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write tcphead '
           tcptail(ibin)%head => tcphead(ibin)%head
        else
           allocate(tcptail(ibin)%head%llpoint,stat=istat)
           tcptail(ibin)%head => tcptail(ibin)%head%llpoint
           if(istat /= 0)write(6,*)' failure to write tcptail%llpoint '
        end if

        m_alloc(ibin) = m_alloc(ibin) +1
        my_head => tcptail(ibin)%head
        my_head%idv=is
        my_head%iob=i

        call get_ij(mm1,dlat,dlon,tcptail(ibin)%head%ij(1),tcptail(ibin)%head%wij(1))

        tcptail(ibin)%head%res      = ddiff
        tcptail(ibin)%head%err2     = error**2
        tcptail(ibin)%head%raterr2  = ratio_errors**2
        tcptail(ibin)%head%time     = dtime      
        tcptail(ibin)%head%b        = cvar_b(ikx)
        tcptail(ibin)%head%pg       = cvar_pg(ikx)
        tcptail(ibin)%head%luse     = luse(i)
        if(oberror_tune) then
           tcptail(ibin)%head%kx    = ikx        ! data type for oberror tuning
           tcptail(ibin)%head%ppertb= data(iptrb,i)/error/ratio_errors ! obs perturbation
        endif

        if(luse_obsdiag)then
           tcptail(ibin)%head%diags => obsdiags(i_tcp_ob_type,ibin)%tail

           my_head => tcptail(ibin)%head
           my_diag => tcptail(ibin)%head%diags
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
        rdiagbuf(5,ii)  = 0                  ! station elevation (meters)
        rdiagbuf(6,ii)  = data(ipres,i)*r10  ! observation pressure (hPa)
        rdiagbuf(7,ii)  = 0                  ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = 1                  ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = 1                  ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        pob      = pob*r10
        pges     = pges*r10
        pgesorig = pgesorig*r10
        err_input = data(ier,i)*r10   ! r10 converts cb to mb
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

        idia=idia0
        if (lobsdiagsave) then
           do jj=1,miter
              idia=idia+1
              if (obsdiags(i_tcp_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(idia,ii) = one
              else
                 rdiagbuf(idia,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_tcp_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_tcp_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,ii) = obsdiags(i_tcp_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif

    end if ! conv_diagsave .true. and luse .true.

! End of loop over observations
  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave .and. ii>0)then
     call dtime_show(myname,'diagsave:tcp',i_tcp_ob_type)
     write(7)'tcp',nchar,nreal,ii,mype,idia0
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
  call gsi_metguess_get ('var::ps', ivar, istatus )
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

end subroutine setuptcp
