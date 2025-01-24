module cldch_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupcldch; end interface

contains
subroutine setupcldch(obsLL,odiagLL,lunin,mype,bwork,awork,nele,nobs,is,conv_diagsave)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setupcldch    compute rhs for conventional surface cldch
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
!   2015-07-10  pondeca
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-10-07  pondeca - if(.not.proceed) advance through input file first
!                          before retuning to setuprhsall.f90
!   2017-02-06  todling - add netcdf_diag capability; hidden as contained code
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2018-03-01  yang -  use module nltransf to cldch

!   2018-03-22  pondeca/yang -  for code consistency across all analyzed variables,replace
!                      the  original "dup"-based implementation of the option to
!                      assimilate the closest ob to the analysis time only with
!                      Ming Hu's "muse"-based implementationusing.
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

  use m_obsNode  , only: obsNode
  use m_cldchNode, only: cldchNode
  use m_cldchNode, only: cldchNode_appendto
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use m_obsdiagNode, only: obsdiagLList_nextNode
  use m_obsdiagNode, only: obsdiagNode_set
  use m_obsdiagNode, only: obsdiagNode_get
  use m_obsdiagNode, only: obsdiagNode_assert
  use m_obsLList   , only: obsLList

  use guess_grids, only: hrdifsig,nfldsig
  use obsmod, only: rmiss_single,&
                    lobsdiagsave,nobskeep,lobsdiag_allocated,time_offset,bmiss
  use obsmod, only: luse_obsdiag,ianldate
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use gsi_4dvar, only: nobs_bins,hr_obsbin,min_offset
  use oneobmod, only: magoberr,maginnov,oneobtest
  use gridmod, only: nsig
  use gridmod, only: get_ij
  use constants, only: zero,tiny_r_kind,one,half,one_tenth,wgtlim, &
            two,cg_term,huge_single,r1000
  use jfunc, only: jiter,last,miter
  use qcmod, only: dfact,dfact1,npres_print
  use qcmod, only: pcldch,scale_cv
  use convinfo, only: nconvtype,cermin,cermax,cgross,cvar_b,cvar_pg,ictype
  use convinfo, only: icsubtype
  use m_dtime, only: dtime_setup, dtime_check
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  use nltransf, only: nltransf_inverse
  use rapidrefresh_cldsurf_mod, only: l_closeobs
  implicit none

! Declare passed variables
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL
  logical                                          ,intent(in   ) :: conv_diagsave
  integer(i_kind)                                  ,intent(in   ) :: lunin,mype,nele,nobs
  real(r_kind),dimension(100+7*nsig)               ,intent(inout) :: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout) :: bwork
  integer(i_kind)                                  ,intent(in   ) :: is ! ndat index

! Declare external calls for code analysis
  external:: tintrp2a11
  external:: stop2

! Declare local parameters
  real(r_kind),parameter:: r0_1_bmiss=one_tenth*bmiss
  character(len=*),parameter:: myname='setupcldch'

! Declare local variables
  
  real(r_double) rstation_id

  real(r_kind) cldchges,dlat,dlon,ddiff,dtime,error
  real(r_kind) cldchgesout,cldchobout,tempcldch,cldchdiff
  real(r_kind) cldch_errmax
  real(r_kind) scale,val2,ratio,residual
  real(r_kind) obserrlm,obserror,val,valqc
  real(r_kind) term,rwgt
  real(r_kind) cg_cldch,wgross,wnotgross,wgt,arg,exp_arg,rat_err2
  real(r_kind) ratio_errors,tfact
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final
  real(r_kind),dimension(nobs):: dup
  real(r_kind),dimension(nele,nobs):: data
  real(r_single),allocatable,dimension(:,:)::rdiagbuf


  integer(i_kind) ier,ilon,ilat,icldch,id,itime,ikx,imaxerr,iqc
  integer(i_kind) iuse,ilate,ilone,istnelv,iobshgt,izz,iprvd,isprvd
  integer(i_kind) i,nchar,nreal,k,ii,ikxx,nn,ibin,ioff,ioff0,jj
  integer(i_kind) l,mm1
  integer(i_kind) idomsfc
  
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid  ! initial (pre-distribution) obs ID
  logical proceed

  character(8) station_id
  character(8),allocatable,dimension(:):: cdiagbuf
  character(8),allocatable,dimension(:):: cprvstg,csprvstg
  character(8) c_prvstg,c_sprvstg
  real(r_double) r_prvstg,r_sprvstg
  real(r_kind) :: hr_offset

  logical:: in_curbin, in_anybin

  type(cldchNode),pointer:: my_head
  type(obs_diag ),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  equivalence(rstation_id,station_id)
  equivalence(r_prvstg,c_prvstg)
  equivalence(r_sprvstg,c_sprvstg)
  
  real(r_kind),allocatable,dimension(:,:,:) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:) :: ges_cldch
  real(r_kind),allocatable,dimension(:,:,:) :: ges_z

  type(obsLList),pointer,dimension(:):: cldchhead
  cldchhead => obsLL(:)

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) then
     read(lunin)data,luse   !advance through input file
     return  ! not all vars available, simply return
  endif

! If require guess vars available, extract from bundle ...
  call init_vars_

  cldch_errmax=20.0_r_kind
!*********************************************************************************
! Read and reformat observations in work arrays.
  read(lunin)data,luse,ioid
! index information for data array (see reading routine)
  ier=1       ! index of obs error
  ilon=2      ! index of grid relative obs location (x)
  ilat=3      ! index of grid relative obs location (y)
  icldch=4    ! index of cldch observation - background
  id=5        ! index of station id
  itime=6     ! index of observation time in data array
  ikxx=7      ! index of ob type
  imaxerr=8   ! index of cldch max error
  iqc=9       ! index of quality mark
  iuse=10     ! index of use parameter
  idomsfc=11  ! index of dominant surface type
  ilone=12    ! index of longitude (degrees)
  ilate=13    ! index of latitude (degrees)
  istnelv=14  ! index of station elevation (m)
  iobshgt=15  ! index of observation height (m)
  izz=16      ! index of surface height
  iprvd=17    ! index of provider
  isprvd=18   ! index of subprovider

  do i=1,nobs
     muse(i)=nint(data(iuse,i)) <= jiter .and. nint(data(iqc,i)) < 8
  end do

! Check for missing data  !need obs value and error
  do i=1,nobs
    if (data(icldch,i) > r0_1_bmiss)  then
       muse(i)=.false.
       data(icldch,i)=rmiss_single   ! for diag output
       data(iobshgt,i)=rmiss_single  ! for diag output
    end if
  end do

! Check for duplicate observations at same location
  hr_offset=min_offset/60.0_r_kind
  dup=one
  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and. &
           data(ilon,k) == data(ilon,l) .and. &
           data(ier,k) < r1000 .and. data(ier,l) < r1000 .and. &
           muse(k) .and. muse(l))then
           if(l_closeobs) then
              if(abs(data(itime,k)-hr_offset)<abs(data(itime,l)-hr_offset)) then
                  muse(l)=.false.
              else
                  muse(k)=.false.
              endif
           else
              tfact=min(one,abs(data(itime,k)-data(itime,l))/dfact1)
              dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
              dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
           endif
        end if
     end do
  end do

! If requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     nchar=1
     ioff0=22
     nreal=ioff0
     if (lobsdiagsave) nreal=nreal+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nreal,nobs))
     allocate(cprvstg(nobs),csprvstg(nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if

  mm1=mype+1
  scale=one

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
    endif

!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

     if(luse_obsdiag) my_diagLL => odiagLL(ibin)

!    Link obs to diagnostics structure
     if(luse_obsdiag)then
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

!------------------------------------------------------------------------------
! RTMA SO test-part one:check the interpolated fields at the selected station

!     rstation_id     = data(id,i)
!    if (trim(station_id) .ne. 'CYYY') then
! Interpolate to get cldch at obs location/time
!        call tintrp2a11(ges_cldch,cldchges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)
!     endif
!     if (trim(station_id) .eq. 'CYYY') then
!        write (6,*) 'CYYY trim(station_id=',trim(station_id)
! Interpolate to get cldch at obs location/time--print out the interplator's
! grid value
!        call tintrp2so(ges_cldch,cldchges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)
!     endif
! END RTMA SO test-part one
!------------------------------------------------------------------------------

! Interpolate to get cldch at obs location/time
     call tintrp2a11(ges_cldch,cldchges,dlat,dlon,dtime,hrdifsig,mype,nfldsig)

! Adjust observation error
     ratio_errors=error/data(ier,i)
     error=one/error

     ddiff=data(icldch,i)-cldchges

! If requested, setup for single obs test.
     if (oneobtest) then
        ddiff=maginnov
        error=one/magoberr
        ratio_errors=one
     endif

!    Gross check using innovation normalized by error
     if (abs(data(icldch,i)-rmiss_single) >= tiny_r_kind ) then  !MIGHT WANT TO IMPROVE THIS. MPondeca /17Jul2015
        obserror = one/max(ratio_errors*error,tiny_r_kind)
        obserrlm = max(cermin(ikx),min(cermax(ikx),obserror))
        residual = abs(ddiff)
        ratio    = residual/obserrlm
        ratio_errors=ratio_errors/sqrt(dup(i))
        if (ratio> cgross(ikx) .or. ratio_errors < tiny_r_kind) then
           if (luse(i)) awork(6) = awork(6)+one
           error = zero
           ratio_errors=zero
        end if
     else    ! missing data
        error = zero
        ratio_errors=zero
     end if

!-------------------------------------------------------------------
! RTMA SO test-part two: assign obs. error as zero at all stations 
!         except the selected station
!     rstation_id     = data(id,i)
!     if (trim(station_id) .ne. 'CYYY') then
!      write (6,*) 'trim(station_id=',trim(station_id)
!        error = zero
!        ratio_errors=zero
!     else
!        write (6,*) 'CYYY: trim(station_id=)',trim(station_id)
!     endif
! END RTMA SO test part two
!-------------------------------------------------------------------
     if (ratio_errors*error <=tiny_r_kind) muse(i)=.false.
     if (nobskeep>0 .and. luse_obsdiag) call obsdiagNode_get(my_diag, jiter=nobskeep, muse=muse(i))

!    Compute penalty terms (linear & nonlinear qc).
     val      = error*ddiff
     if(luse(i))then
        val2     = val*val
        exp_arg  = -half*val2
        rat_err2 = ratio_errors**2
        if (cvar_pg(ikx) > tiny_r_kind .and. error > tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pg(ikx)
           cg_cldch=cvar_b(ikx)
           wgross = cg_term*cvar_pg(ikx)/(cg_cldch*wnotgross)
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
           nn=1
         else
           nn=2                                     !rejected obs
           if(ratio_errors*error >=tiny_r_kind)nn=3 !monitored obs
        end if
!.........................................................................
!NLTR:  convert cldchges to physical space
        call nltransf_inverse(cldchges,cldchgesout,pcldch,scale_cv)

        if (abs(data(icldch,i)-rmiss_single) >=tiny_r_kind) then
           bwork(1,ikx,1,nn)  = bwork(1,ikx,1,nn)+one                  ! count
!.........................................................................
!NLTR:  convert cldchobs to physical space
           call nltransf_inverse(cldchges,cldchgesout,pcldch,scale_cv)
           tempcldch=data(icldch,i)
           call nltransf_inverse(tempcldch,cldchobout,pcldch,scale_cv)
!values in cldch fits, fort.232, are in physical space
           cldchdiff=(cldchobout-cldchgesout)*scale
           bwork(1,ikx,2,nn)  = bwork(1,ikx,2,nn)+cldchdiff            ! (o-g)
           bwork(1,ikx,3,nn)  = bwork(1,ikx,3,nn)+cldchdiff*cldchdiff  ! (o-g)**2
!END NLTR
           bwork(1,ikx,4,nn)  = bwork(1,ikx,4,nn)+val2*rat_err2        ! penalty
           bwork(1,ikx,5,nn)  = bwork(1,ikx,5,nn)+valqc                ! nonlin qc penalty
        else    ! default value for cldchobout and cldchdiff
           cldchobout=rmiss_single
           cldchdiff=(cldchobout-cldchgesout)*scale
        end if
     endif

     if(luse_obsdiag)then
        call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
                jiter=jiter, muse=muse(i), nldepart=ddiff)
     end if

!    If obs is "acceptable", load array with obs info for use
!    in inner loop minimization (int* and stp* routines)
     if (.not. last .and. muse(i)) then

        allocate(my_head)
        call cldchNode_appendto(my_head,cldchhead(ibin))

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
        my_head%b       = cvar_b(ikx)
        my_head%pg      = cvar_pg(ikx)
        my_head%luse    = luse(i)

        if(luse_obsdiag)then
           call obsdiagNode_assert(my_diag, my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
           my_head%diags => my_diag
        endif

        my_head => null()
     endif


!    Save stuff for diagnostic output
     if(conv_diagsave .and. luse(i))then
        ii=ii+1
        rstation_id     = data(id,i)

        if (ratio_errors*error>tiny_r_kind) then
           err_final = 4000.0_r_kind
        else
           err_final = huge_single
        endif
 
        errinv_input = huge_single
        errinv_adjst = huge_single
        errinv_final = huge_single
!---------------------------------------------------------------------------------
!In diag file, write out cldch error statistics and field in physical space.
!NOTE:  No linear conversion in error stats between physical space and NLTR
!space.
!NOTE:  in RTMA post process only err_final is used
!-------------------------------------------------------------------------
        err_input = 4000.0_r_kind
        err_adjst = 4000.0_r_kind

        if (err_input>tiny_r_kind) errinv_input = one/err_input
        if (err_adjst>tiny_r_kind) errinv_adjst = one/err_adjst
        if (err_final>tiny_r_kind) errinv_final = one/err_final
 
        if (binary_diag) call contents_binary_diag_(my_diag)
        if (netcdf_diag) call contents_netcdf_diag_(my_diag)
 
     end if


  end do

! Release memory of local guess arrays
  call final_vars_

! Write information to diagnostic file
  if(conv_diagsave) then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and. ii>0)then
        write(7)'cei',nchar,nreal,ii,mype,ioff0
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)

        write(7)cprvstg(1:ii),csprvstg(1:ii)
        deallocate(cprvstg,csprvstg)
     end if
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
  call gsi_metguess_get ('var::cldch' , ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
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
!    get cldch ...
     varname='cldch'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_cldch))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_cldch(size(rank2,1),size(rank2,2),nfldsig))
         ges_cldch(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_cldch(:,:,ifld)=rank2
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

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false. 
     write(string,900) jiter
900  format('conv_cldch_',i2.2,'.nc4')
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
        rdiagbuf(6,ii)  = rmiss_single       ! observation pressure (hPa)
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
        rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (m**-1)
        rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (m**-1)
        rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (m**-1)
 
        rdiagbuf(17,ii) = cldchobout         ! CLDCH observation (m)
        rdiagbuf(18,ii) = cldchdiff          ! obs-ges in physical space,for post process
        rdiagbuf(19,ii) = ddiff              ! obs-ges used in analysis in gspace 
        rdiagbuf(20,ii) = rmiss_single       ! type of measurement
        rdiagbuf(21,ii) = data(idomsfc,i)    ! dominate surface type
        rdiagbuf(22,ii) = data(izz,i)        ! model terrain at observation location
        r_prvstg        = data(iprvd,i)
        cprvstg(ii)     = c_prvstg           ! provider name
        r_sprvstg       = data(isprvd,i)
        csprvstg(ii)    = c_sprvstg          ! subprovider name

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
  character(7),parameter     :: obsclass = '  cldch'
  real(r_kind),parameter::     missing = -9.99e9_r_kind
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata("Latitude",                data(ilate,i)          )
           call nc_diag_metadata("Longitude",               data(ilone,i)          )
           call nc_diag_metadata("Station_Elevation",       data(istnelv,i)        )
           call nc_diag_metadata("Pressure",                missing                )
           call nc_diag_metadata("Height",                  data(iobshgt,i)        )
           call nc_diag_metadata("Time",                    dtime-time_offset      )
           call nc_diag_metadata("Prep_QC_Mark",            data(iqc,i)            )
           call nc_diag_metadata("Prep_Use_Flag",           data(iuse,i)           )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb                 )
           call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    rwgt                   )                 
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    one                    )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    -one                   )              
           endif

           call nc_diag_metadata("Errinv_Input",            errinv_input           )
           call nc_diag_metadata("Errinv_Adjust",           errinv_adjst           )
           call nc_diag_metadata("Errinv_Final",            errinv_final           )

           call nc_diag_metadata("Observation",                   data(icldch,i)   )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   ddiff            )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", data(icldch,i)-cldchges )
 
           call nc_diag_metadata("Dominant_Sfc_Type", data(idomsfc,i)              )
           call nc_diag_metadata("Model_Terrain",     data(izz,i)                  )
           r_prvstg            = data(iprvd,i)
           call nc_diag_metadata("Provider_Name",     c_prvstg                     )
           r_sprvstg           = data(isprvd,i)
           call nc_diag_metadata("Subprovider_Name",  c_sprvstg                    )

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

  subroutine final_vars_
    if(allocated(ges_z  )) deallocate(ges_z  )
    if(allocated(ges_cldch)) deallocate(ges_cldch)
    if(allocated(ges_ps )) deallocate(ges_ps )
  end subroutine final_vars_

end subroutine setupcldch
end module cldch_setup
