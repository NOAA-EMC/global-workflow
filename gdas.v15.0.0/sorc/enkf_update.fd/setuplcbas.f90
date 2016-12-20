subroutine setuplcbas(lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setuplcbas    compute rhs for cloud base height of lowest cloud seen
!   prgmmr: derber           org: np23                date: 2004-07-20
!
! abstract: For sea surface temperature observations
!              a) reads obs assigned to given mpi task (geographic region),
!              b) simulates obs from guess,
!              c) apply some quality control to obs,
!              d) load weight and innovation arrays used in minimization
!              e) collects statistics for runtime diagnostic output
!              f) writes additional diagnostic information to output file
!
! program history log:
!   2012-01-29  zhu
!   2014-06-19  carley - update for metguess bundle, change tintrp2a to tintrp2a11
!                        for debug compile on WCOSS, write sensitivity slot indicator
!                        (ioff) to header of diagfile, remove unused vars
!   2015-03-11  pondeca - Modify for possibility of not using obsdiag
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

  use guess_grids, only: hrdifsig,nfldsig,ntguessig,wgt_lcbas     
  use obsmod, only: lcbashead,lcbastail,rmiss_single,i_lcbas_ob_type,obsdiags,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use obsmod, only: lcbas_ob_type
  use obsmod, only: obs_diag,luse_obsdiag
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use oneobmod, only: magoberr,maginnov,oneobtest
  use gridmod, only: nlat,nlon,nsig
  use gridmod, only: get_ij
  use constants, only: zero,tiny_r_kind,one,one_tenth,half,wgtlim,&
            two,cg_term,huge_single,r1000,rad2deg,deg2rad
  use jfunc, only: jiter,last,miter,jiterstart,R_option
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle


  implicit none

! Declare passed variables
  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is	! ndat index

! Declare external calls for code analysis
  external:: tintrp2a11
  external:: stop2

! Declare local variables
  real(r_kind), parameter:: miss_obs=10.e10_r_kind
  
  real(r_double) rstation_id

  real(r_kind) lcbasges,dlat,dlon,ddiff,dtime,error
  real(r_kind) scale,val2,ratio,ressw2,ress,residual
  real(r_kind) obserrlm,obserror,val,valqc,drpx
  real(r_kind) term,rwgt
  real(r_kind) cg_lcbas,wgross,wnotgross,wgt,arg,exp_arg,rat_err2
  real(r_kind) ratio_errors,tfact,zsges
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf


  integer(i_kind) ier,ilon,ilat,izz,ihgt,ilcbas,id,itime,ikx,iqc,iceil
  integer(i_kind) iuse,ilate,ilone,istnelv,iprvd,isprvd
  integer(i_kind) i,nchar,nreal,k,ii,ikxx,nn,isli,ibin,ioff,ioff0,jj
  integer(i_kind) l,mm1
  integer(i_kind) istat
  integer(i_kind) idomsfc,iskint,iff10,isfcr
  integer(i_kind) jlat,jlon
  
  logical,dimension(nobs):: luse,muse

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg

  logical:: in_curbin, in_anybin,proceed
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(lcbas_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  character(len=*),parameter:: myname='setuplcbas'

  real(r_kind),allocatable,dimension(:,:,:) :: ges_lcbas
  real(r_kind),allocatable,dimension(:,:,:) :: ges_z

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)
  
! Check to see if required guess fields are available
  call check_vars_(proceed)
 if(.not.proceed) then
    print *, 'Whoa!  We have some missing metguess variables in setuplcbas.f90....returning to setuprhsall.f90 after advancing through input file'
    read(lunin)data,luse
    return  ! not all vars available, simply return
  end if

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0
!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse

! index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  ilcbas=4    ! index of lcbas observation
  id=5        ! index of station id
  itime=6     ! index of observation time in data array
  ikxx=7      ! index of ob type
  iqc=8       ! index of qulaity mark
  iuse=9      ! index of use parameter
  idomsfc=10  ! index of dominant surface type
  iskint=11   ! index of surface skin temperature
  iff10=12    ! index of 10 meter wind factor
  isfcr=13    ! index of surface roughness
  ilone=14    ! index of longitude (degrees)
  ilate=15    ! index of latitude (degrees)
  istnelv=16  ! index of station elevation (m)
  ihgt=17     ! index of obs height (m)
  izz=18      ! index of model terrain height at ob location
  iceil=19    ! index of cloud ceiling obs
  iprvd=22    ! index of provider
  isprvd=23   ! index of subprovider

  mm1=mype+1
  scale=one

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

! Check for missing data
  if (.not. oneobtest) then
     do i=1,nobs
        if (abs(data(ilcbas,i)-miss_obs)<100.0_r_kind)  then
           muse(i)=.false.
           data(ilcbas,i)=rmiss_single   ! for diag output
        end if
     end do
  end if

! Check for duplicate observations at same location
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
           data(ier,k) < 2000.0_r_kind .and. data(ier,l) < 2000.0_r_kind .and. &
           muse(k) .and. muse(l))then

           tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do



! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     nreal=23
     ioff0=nreal
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     allocate(cprvstg(nobs),csprvstg(nobs))
  end if

  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)
     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)

        ikx  = nint(data(ikxx,i))
        error=data(ier,i)
        isli=data(idomsfc,i)
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
           if (.not.associated(obsdiags(i_lcbas_ob_type,ibin)%head)) then
              allocate(obsdiags(i_lcbas_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuplcbas: failure to allocate obsdiags',istat
                 call stop2(295)
              end if
              obsdiags(i_lcbas_ob_type,ibin)%tail => obsdiags(i_lcbas_ob_type,ibin)%head
           else
              allocate(obsdiags(i_lcbas_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setuplcbas: failure to allocate obsdiags',istat
                 call stop2(295)
              end if
              obsdiags(i_lcbas_ob_type,ibin)%tail => obsdiags(i_lcbas_ob_type,ibin)%tail%next
           end if
           allocate(obsdiags(i_lcbas_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_lcbas_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_lcbas_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_lcbas_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_lcbas_ob_type,ibin)%tail%indxglb=i
           obsdiags(i_lcbas_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_lcbas_ob_type,ibin)%tail%luse=.false.
           obsdiags(i_lcbas_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_lcbas_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_lcbas_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_lcbas_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_lcbas_ob_type,ibin)%tail%obssen(:)=zero

           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_lcbas_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = i
           my_diag%ich = 1
        else
           if (.not.associated(obsdiags(i_lcbas_ob_type,ibin)%tail)) then
              obsdiags(i_lcbas_ob_type,ibin)%tail => obsdiags(i_lcbas_ob_type,ibin)%head
           else
              obsdiags(i_lcbas_ob_type,ibin)%tail => obsdiags(i_lcbas_ob_type,ibin)%tail%next
           end if
           if (obsdiags(i_lcbas_ob_type,ibin)%tail%indxglb/=i) then
              write(6,*)'setuplcbas: index error'
              call stop2(297)
           end if
        end if
     end if

     if(.not.in_curbin) cycle

!    Interpolate to get lcbas at obs location/time (MSL)
     call tintrp2a11(ges_lcbas,lcbasges,dlat,dlon,dtime,hrdifsig,&
            mype,nfldsig)

!    Get guess sfc hght at obs location
     call tintrp2a11(ges_z,zsges,dlat,dlon,dtime,hrdifsig,&
            mype,nfldsig)

     if(luse(i))then
        awork(1) = awork(1) + one
     end if
 
!    Adjust observation error
     drpx=0.05_r_kind*abs(data(istnelv,i)-zsges)
     ratio_errors=error/((data(ier,i)+drpx)*sqrt(dup(i)))
     error=one/error

!    Compute innovations
     ddiff=data(ilcbas,i)-lcbasges

!    If requested, setup for single obs test.
     if (oneobtest) then
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross check using innovation normalized by error
     if (abs(data(ilcbas,i)-rmiss_single) >= tiny_r_kind ) then
        obserror = one/max(ratio_errors*error,tiny_r_kind)
        obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
        residual = abs(ddiff)
        ratio    = residual/obserrlm
        if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
           if (luse(i)) awork(6) = awork(6)+one
           error = zero
           ratio_errors=zero
        end if
     else
        error = zero
        ratio_errors=zero
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.

     if (nobskeep>0 .and. luse_obsdiag) muse(i)=obsdiags(i_lcbas_ob_type,ibin)%tail%muse(nobskeep)

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_lcbas=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_lcbas*wnotgross)
           term = log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           rwgt = wgt/wgtlim
        else
           term = exp_arg
           wgt  = wgtlim
           rwgt = wgt/wgtlim
        endif
        valqc = -two*rat_err2*term

!       Accumulate statistics for obs belonging to this task
        if (muse(i)) then
           if(rwgt < one) awork(21) = awork(21)+one
           awork(4)=awork(4)+val2*rat_err2
           awork(5)=awork(5)+one
           awork(22)=awork(22)+valqc
        end if
        ress   = ddiff*scale
        ressw2 = ress*ress
        val2   = val*val
        rat_err2 = ratio_errors**2
        nn=1
        if (.not. muse(i)) then
           nn=2
           if(ratio_errors*error >=tiny_r_kind)nn=3
        end if
        if (abs(data(ilcbas,i)-rmiss_single) >=tiny_r_kind) then
           bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one           ! count
           bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress          ! (o-g)
           bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2        ! (o-g)**2
           bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2 ! penalty
           bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc         ! nonlin qc penalty
        end if

     endif

     if(luse_obsdiag)then
        obsdiags(i_lcbas_ob_type,ibin)%tail%luse=luse(i)
        obsdiags(i_lcbas_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_lcbas_ob_type,ibin)%tail%nldepart(jiter)=ddiff
        obsdiags(i_lcbas_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        if(.not. associated(lcbashead(ibin)%head))then
           allocate(lcbashead(ibin)%head,stat=istat)
           if(istat /= 0)write(6,*)' failure to write lcbashead '
           lcbastail(ibin)%head => lcbashead(ibin)%head
        else
           allocate(lcbastail(ibin)%head%llpoint,stat=istat)
           if(istat /= 0)write(6,*)' failure to write lcbastail%llpoint '
           lcbastail(ibin)%head => lcbastail(ibin)%head%llpoint
        end if

	m_alloc(ibin) = m_alloc(ibin) + 1
	my_head => lcbastail(ibin)%head
	my_head%idv = is
	my_head%iob = i

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,lcbastail(ibin)%head%ij(1),lcbastail(ibin)%head%wij(1),jlat,jlon)
       
        if (jiter==jiterstart .and. R_option) then
           wgt_lcbas(jlat,jlon)    =wgt_lcbas(jlat,jlon)+lcbastail(ibin)%head%wij(1)
           wgt_lcbas(jlat+1,jlon)  =wgt_lcbas(jlat+1,jlon)+lcbastail(ibin)%head%wij(2)
           wgt_lcbas(jlat,jlon+1)  =wgt_lcbas(jlat,jlon+1)+lcbastail(ibin)%head%wij(3)
           wgt_lcbas(jlat+1,jlon+1)=wgt_lcbas(jlat+1,jlon+1)+lcbastail(ibin)%head%wij(4)
        end if

        lcbastail(ibin)%head%res     = ddiff
        lcbastail(ibin)%head%err2    = error**2
        lcbastail(ibin)%head%raterr2 = ratio_errors**2    
        lcbastail(ibin)%head%time    = dtime
        lcbastail(ibin)%head%b       = cvar_b(ikx)
        lcbastail(ibin)%head%pg      = cvar_pg(ikx)
        lcbastail(ibin)%head%luse    = luse(i)
        if(luse_obsdiag)then
           lcbastail(ibin)%head%diags => obsdiags(i_lcbas_ob_type,ibin)%tail
 
           my_head => lcbastail(ibin)%head
           my_diag => lcbastail(ibin)%head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =', &
                    (/is,i,ibin/))
              call perr(myname,'my_head%(idv,iob) =',(/my_head%idv,my_head%iob/))
              call perr(myname,'my_diag%(idv,iob) =',(/my_diag%idv,my_diag%iob/))
              call die(myname)
           endif
        end if
     endif

!    Save stuff for diagnostic output
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)
        cdiagbuf(ii)    = station_id         ! station id
 
        rdiagbuf(1,ii)  = ictype(ikx)        ! observation type
        rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
 
        rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
        rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
        rdiagbuf(5,ii)  = data(istnelv,i)    ! station elevation (meters)
        rdiagbuf(6,ii)  = rmiss_single       ! observation pressure (hPa)
        rdiagbuf(7,ii)  = data(ihgt,i)       ! observation height (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(iqc,i)        ! input prepbufr qc or event mark
        rdiagbuf(10,ii) = rmiss_single       ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif

        err_input = data(ier,i)
        err_adjst = data(ier,i)
        if (ratio_errors*error>tiny_r_kind) then
           err_final = one/(ratio_errors*error)
        else
           err_final = huge_single
        endif
 
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final

        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)
 
        rdiagbuf(17,ii) = data(ilcbas,i)     ! lcbas observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = data(ilcbas,i)-lcbasges! obs-ges w/o bias correction (K) (future slot)
 
        rdiagbuf(20,ii) = rmiss_single       ! type of measurement

        rdiagbuf(21,ii) = data(idomsfc,i)    ! dominate surface type
        rdiagbuf(22,ii) = data(izz,i)        ! model terrain at observation location
        if (abs(data(iceil,i)-miss_obs)<100.0_r_kind)  then
           rdiagbuf(23,ii) = data(iceil,i)      ! cloud ceiling
        else
           rdiagbuf(23,ii) = rmiss_single
        end if
        r_prvstg        = data(iprvd,i)
        cprvstg(ii)     = c_prvstg           ! provider name
        r_sprvstg       = data(isprvd,i)
        csprvstg(ii)    = c_sprvstg          ! subprovider name

        if (lobsdiagsave) then
           ioff=ioff0
           do jj=1,miter 
              ioff=ioff+1 
              if (obsdiags(i_lcbas_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(ioff,ii) = one
              else
                 rdiagbuf(ioff,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_lcbas_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_lcbas_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              ioff=ioff+1
              rdiagbuf(ioff,ii) = obsdiags(i_lcbas_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
 
     end if

  end do


! Write information to diagnostic file
  if(conv_diagsave)then
     call dtime_show(myname,'diagsave:lcbas',i_lcbas_ob_type)
     write(7)'lcb',nchar,nreal,ii,mype,ioff0
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)

     write(7)cprvstg(1:ii),csprvstg(1:ii)
     deallocate(cprvstg,csprvstg)
  end if
! End of routine

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::lcbas', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get lcbas ...
     varname='lcbas'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_lcbas))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_lcbas(size(rank2,1),size(rank2,2),nfldsig))
         ges_lcbas(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_lcbas(:,:,ifld)=rank2
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
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_z  )) deallocate(ges_z  )
    if(allocated(ges_lcbas)) deallocate(ges_lcbas)
  end subroutine final_vars_
end subroutine setuplcbas

