module sst_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupsst; end interface

contains
subroutine setupsst(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupsst    compute rhs for conventional surface sst
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
!   2004-07-20  derber
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-08-28  derber  - fix some bugs
!   2004-10-06  parrish - increase size of sstwork array for nonlinear qc
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - move logical conv_diagsave from obsmod to argument list
!   2005-03-02  dee - remove garbage from diagnostic file
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2005-11-08  todling - bug fix: lat/lon arrays were inverted to diag file
!   2005-11-14  pondeca - correct error in diagnostic array index
!   2006-01-31  todling/treadon - store wgt/wgtlim in rdiagbuf(6,ii)
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-05-30  su,derber,treadon - modify diagnostic output
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - modify handling of multiple data at same location
!                       - unify NL qc
!   2006-08-28      su - fix a bug in variational qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - add observation diagnostics structure
!   2007-08-28      su  - modify the gross check error 
!   2008-05-21  safford - rm unused vars and uses
!   2009-08-19  guo     - changed for multi-pass setup with dtime_check().
!   2011-04-02  li      - set up Tr analysis and modify to save nst analysis related diagnostic variables
!   2012-04-10  akella  - sstges calculated for nst analysis using NST fields
!   2013-01-26  parrish - change intrp2a to intrp2a11 (so debug compile works on WCOSS)
!   2014-01-28  li      - add ntguessfc to use guess_grids to apply intrp2a11 correctly
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-05-30  li     - Modify to make it work when nst_gsi = 0 and nsstbufr data file exists
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2017-02-06  todling - add netcdf_diag capability; hidden as contained code
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2019-11-12  li      - add 4 nsst variables to netcdf sst diag file
!   2022-04-04  li      - modify to use mixed surface in situ observations
!                         if ( isli == 0 ) then =>  if ( owpct > 0.05_r_kind ) then
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

  use guess_grids, only: dsfct,ntguessfc
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert

  use obsmod, only: rmiss_single,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset
  use m_obsNode, only: obsNode
  use m_sstNode, only: sstNode
  use m_sstNode, only: sstNode_appendto
  use m_obsLList, only: obsLList
  use obsmod, only: luse_obsdiag
  use obsmod, only: netcdf_diag, binary_diag, dirname,ianldate
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d, nc_diag_metadata_to_single
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  use oneobmod, only: magoberr,maginnov,oneobtest
  use gridmod, only: nsig
  use gridmod, only: get_ij
  use constants, only: zero,tiny_r_kind,one,quarter,half,wgtlim, &
            two,cg_term,pi,huge_single,r1000,tfrozen,r_missing
  use jfunc, only: jiter,last,miter
  use qcmod, only: dfact,dfact1,npres_print
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use m_dtime, only: dtime_setup, dtime_check
  implicit none

  integer(i_kind),parameter:: istyp=0,nprep=1
! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare external calls for code analysis
  external:: intrp2a11
  external:: stop2

! Declare local variables
  
  real(r_double) rstation_id

  real(r_kind) sstges,dlat,dlon,ddiff,dtime,error,dsfct_obx,owpct
  real(r_kind) scale,val2,ratio,ressw2,ress,residual
  real(r_kind) obserrlm,obserror,val,valqc
  real(r_kind) term,halfpi,rwgt
  real(r_kind) cg_sst,wgross,wnotgross,wgt,arg,exp_arg,rat_err2
  real(r_kind) ratio_errors,tfact
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf

  real(r_kind) :: tz_tr,zob,tref,dtw,dtc

  integer(i_kind) ier,ilon,ilat,isst,id,itime,ikx,itemp,ipct
  integer(i_kind) ier2,iuse,izob,itref,idtw,idtc,itz_tr,iotype,ilate,ilone,istnelv
  integer(i_kind) i,nchar,nreal,k,ii,ikxx,nn,isli,ibin,ioff,ioff0,jj
  integer(i_kind) l,mm1
  integer(i_kind) id_qc
  integer(i_kind) idomsfc,itz
  integer(i_kind) idatamax,nwsum,nfinal,nobs_qc
  
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf

  logical:: in_curbin, in_anybin
  type(sstNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  integer, parameter:: maxinfo = 20
  character(len=*),parameter:: myname='setupsst'


  equivalence(rstation_id,station_id)
  
  type(obsLList),pointer,dimension(:):: ssthead
  ssthead => obsLL(:)

!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid

!  index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  isst=4      ! index of sst observation
  id=5        ! index of station id
  itime=6     ! index of observation time in data array
  ikxx=7      ! index of ob type
  itemp=8     ! index of open water temperature (background)
  izob=9      ! index of flag indicating depth of observation
  iotype=10   ! index of measurement type
  ipct=11     ! index of open water percentage
  ier2=12     ! index of original obs error
  iuse=13     ! index of use parameter
  idomsfc=14  ! index of dominant surface type
  itz=15      ! index of temperature at depth z (Tz)
  ilone=16    ! index of longitude (degrees)
  ilate=17    ! index of latitude (degrees)
  istnelv=18  ! index of station elevation (m)
  itref=19    ! index of Tr
  idtw=20     ! index of dtw
  idtc=21     ! index of dtc
  itz_tr=22   ! index of tz_tr
  idatamax=22 ! set to largest value in list above

  if(nst_gsi>0) then
     if(nele<idatamax) then
        write(6,*)'setupsst: nele inconsistent with idatamax',nele,idatamax
        call stop2(295)
     endif
  endif

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter
  end do

  nobs_qc=0
  nwsum=0
  nfinal=0

  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
           data(ilon,k) == data(ilon,l) .and.  &
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
     ii=0
     nchar=1
     ioff0=maxinfo+nstinfo
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if

  halfpi = half*pi
  mm1=mype+1
  scale=one

  call dtime_setup()
  do i=1,nobs
    id_qc = 0
    dtime=data(itime,i)
    call dtime_check(dtime, in_curbin, in_anybin)
    if(.not.in_anybin) cycle

    zob   = data(izob,i)
    if(nst_gsi > 0)then
      tref  = data(itref,i)
      dtw   = data(idtw,i)
      dtc   = data(idtc,i)
      tz_tr = data(itz_tr,i)
    else
      tref  = data(itz,i)
      dtw   = zero
      dtc   = zero
      tz_tr = r_missing
    end if

    if (in_curbin) then
       dlat=data(ilat,i)
       dlon=data(ilon,i)

       ikx  = nint(data(ikxx,i))
       error=data(ier2,i)
       isli=data(idomsfc,i)
       owpct=data(ipct,i)
    endif

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
                'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
     endif

     if(.not.in_curbin) cycle

! Interpolate to get sst at obs location/time
     if ( owpct > 0.05_r_kind ) then
       nobs_qc = nobs_qc + 1
       call intrp2a11(dsfct(1,1,ntguessfc),dsfct_obx,dlat,dlon,mype)
     else
       dsfct_obx = zero
     endif

     if(nst_gsi > 1) then
       sstges = max(tref+dtw-dtc+dsfct_obx, tfrozen)
     else
       sstges = max(data(itz,i)+dsfct_obx, tfrozen)
     end if

! Adjust observation error
     ratio_errors=error/data(ier,i)
     error=one/error

     if(owpct == 0 ) error = zero

     ddiff=data(isst,i)-sstges

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross check using innovation normalized by error
     obserror = one/max(ratio_errors*error,tiny_r_kind)
     obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
     residual = abs(ddiff)
     ratio    = residual/obserrlm
     if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
        if (luse(i)) awork(6) = awork(6)+one
        error = zero
        ratio_errors=zero
        if( id_qc == 0 ) id_qc = 1
     else
        ratio_errors=ratio_errors/sqrt(dup(i))
     end if

     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>0.and.luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_sst=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_sst*wnotgross)
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
        bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one           ! count
        bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+ress          ! (o-g)
        bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+ressw2        ! (o-g)**2
        bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2 ! penalty
        bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc         ! nonlin qc penalty

     endif

     if(luse_obsdiag)then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
           jiter=jiter, muse=muse(i), nldepart=ddiff)
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        allocate(my_head)
        call sstNode_appendto(my_head,ssthead(ibin))

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j) indices of guess gridpoint that bound obs location
        call get_ij(mm1,dlat,dlon,my_head%ij,my_head%wij)

        my_head%res     = ddiff
        my_head%err2    = error**2
        my_head%raterr2 = ratio_errors**2    
        my_head%time    = dtime
        my_head%zob     = zob
        my_head%tz_tr   = tz_tr
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%luse    = luse(i)

        if (luse_obsdiag) then
           call obsdiagNode_assert(my_diag, my_head%idv,my_head%iob,1, myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()
     endif


!    Save stuff for diagnostic output
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
        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final
 
        if (binary_diag) call contents_binary_diag_(my_diag)
        if (netcdf_diag) call contents_netcdf_diag_(my_diag)
     end if

  end do                    ! do i=1,nobs

! Write information to diagnostic file
  if(conv_diagsave)then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
        write(7)'sst',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     end if
  end if

! End of routine

contains
  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false. 
     write(string,900) jiter
900  format('conv_sst_',i2.2,'.nc4')
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
        rdiagbuf(6,ii)  = data(itemp,i)      ! background open water temperature (K)
        rdiagbuf(7,ii)  = data(izob,i)       ! observation depth (meters)
        rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)

        rdiagbuf(9,ii)  = data(ipct,i)       ! open water percentage (0 to 1)
        rdiagbuf(10,ii) = id_qc              ! setup qc or event mark
        rdiagbuf(11,ii) = data(iuse,i)       ! read_prepbufr data usage flag
        if(muse(i)) then
           rdiagbuf(12,ii) = one             ! analysis usage flag (1=use, -1=not used)
        else
           rdiagbuf(12,ii) = -one
        endif


        rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (K**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (K**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (K**-1)
 
        rdiagbuf(17,ii) = data(isst,i)       ! SST observation (K)
        rdiagbuf(18,ii) = ddiff              ! obs-ges used in analysis (K)
        rdiagbuf(19,ii) = data(isst,i)-sstges! obs-ges w/o bias correction (K) (future slot)
 
        rdiagbuf(20,ii) = data(iotype,i)     ! type of measurement

        if (nst_gsi>0) then
          rdiagbuf(maxinfo+1,ii) = data(itref,i)    ! Tr
          rdiagbuf(maxinfo+2,ii) = data(idtw,i)     ! dt_warm at zob
          rdiagbuf(maxinfo+3,ii) = data(idtc,i)     ! dt_cool at zob
          rdiagbuf(maxinfo+4,ii) = data(itz_tr,i)   ! d(tz)/d(Tr) at zob
        endif

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
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '    sst'
  real(r_single),parameter::     missing = -9.99e9_r_single
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata_to_single("Latitude",data(ilate,i)      )
           call nc_diag_metadata_to_single("Longitude",data(ilone,i)      )
           call nc_diag_metadata_to_single("Station_Elevation",data(istnelv,i)  )
           call nc_diag_metadata("Pressure",                missing                )
           call nc_diag_metadata_to_single("Height",data(izob,i)       )
           call nc_diag_metadata_to_single("Time",dtime,time_offset,'-')
           call nc_diag_metadata_to_single("Prep_QC_Mark",data(ipct,i)       )
           call nc_diag_metadata_to_single("Prep_Use_Flag",data(iuse,i)       )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb                 )
           call nc_diag_metadata_to_single("Nonlinear_QC_Rel_Wgt",rwgt               )
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    1.0_r_single           )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    -1.0_r_single          )
           endif

           call nc_diag_metadata_to_single("Errinv_Input",errinv_input       )
           call nc_diag_metadata_to_single("Errinv_Adjust",errinv_adjst       )
           call nc_diag_metadata_to_single("Errinv_Final",errinv_final       )

           call nc_diag_metadata_to_single("Observation",data(isst,i) )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_adjusted",ddiff        )
           call nc_diag_metadata_to_single("Obs_Minus_Forecast_unadjusted",data(isst,i),sstges,'-')

           if (nst_gsi>0) then
              call nc_diag_metadata_to_single("FoundationTempBG",data(itref,i) )
              call nc_diag_metadata_to_single("DiurnalWarming_at_zob",data(idtw,i) )
              call nc_diag_metadata_to_single("SkinLayerCooling_at_zob",data(idtc,i) )
              call nc_diag_metadata_to_single("Sensitivity_Tzob_Tr",data(itz_tr,i) )
           endif
 
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
   
  end subroutine contents_netcdf_diag_
end subroutine setupsst
end module sst_setup
