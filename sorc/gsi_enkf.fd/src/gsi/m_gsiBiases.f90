module m_gsiBiases
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_gsiBiases
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:      2010-03-24
!
! abstract: background bias estimation/correction
!
! program history log:
!   2010-03-24  j guo   - added this document block
!   2011-08-01  lueken  - replaced F90 with f90, replace izero/ione with 0/1, remove _i_kind
!  
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  m_gsiBiases --- Module defining bias prediction scheme
!
! !INTERFACE:
!

! !USES:

  use kinds,only : r_kind
  use kinds,only : i_kind
  use mpimod, only: mype
  use jfunc,  only: biascor, bcoption, diurnalbc
  use gridmod, only: lat2,lon2,nsig
  use gsi_4dvar, only: nhr_assimilation,ibdate

  use GSI_BundleMod, only: gsi_grid
  use GSI_BundleMod, only: gsi_bundle
  use GSI_BundleMod, only: gsi_gridcreate
  use GSI_BundleMod, only: gsi_bundlecreate
  use GSI_BundleMod, only: gsi_bundledestroy
  use GSI_BundleMod, only: gsi_bundlegetpointer
  use GSI_BundleMod, only: gsi_bundlegetvar
  use GSI_BundleMod, only: gsi_bundleputvar
  use GSI_MetGuess_Mod, only: gsi_metguess_bundle
  use GSI_ChemGuess_Mod, only: gsi_chemguess_bundle

  implicit none

! !DESCRIPTION: module defining bias prediction scheme
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-03  todling - add bias_correct interface; add create/destroy
!   2006-12-04  todling - documentation/prologue
!   2007-04-10  todling - bug fix in alloc/dealloc (init/clean)
!   2009-01-20  todling - protect against re-initialization try
!   2013-10-19  todling - metguess now holds background
!
! !REMARS:
!     Acronym BBC stands for background bias correction.
!
! !AUTHOR:
!   guo           org: gmao                date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

  private
  public :: bias_hour
  public :: nbc

  public :: bkg_bias_correction
  public :: bkg_bias_update
  public :: bkg_bias_model
  public :: create_bkgbias_grids
  public :: destroy_bkgbias_grids

  public :: gsi_bkgbias_bundle
  public :: bvars2d, bvars3d    ! TODO: temporarily made public

  integer(i_kind),save :: bias_hour = -1
  integer(i_kind),save :: nbc       = -1

  interface bkg_bias_model; module procedure &
     bias_model2d_,bias_model3d_,bias_model_bundle_; end interface
  interface bkg_bias_update 
     module procedure update2d_,update3d_,updateall_
  end interface
  interface bkg_bias_correction; module procedure correct_; end interface
  interface create_bkgbias_grids ; module procedure init_   ; end interface
  interface destroy_bkgbias_grids; module procedure clean_  ; end interface

  logical,save:: initialized_=.false.
  logical,save:: bkg_bias_set_=.false.

  character(len=32),allocatable,dimension(:):: bvars2d, bvars3d
  character(len=32),allocatable,dimension(:):: bsrcs2d, bsrcs3d
  integer(i_kind),allocatable,dimension(:):: levels
  integer(i_kind),allocatable,dimension(:):: bkg_nymd ! Bkg date: YYYYMMDD
  integer(i_kind),allocatable,dimension(:):: bkg_nhms ! Bkg time: HHMMSS

  type(gsi_bundle),pointer :: gsi_bkgbias_bundle(:)

  character(len=*),parameter::myname='m_gsiBiases'

  integer(i_kind), parameter :: bkg_nstarthr=3  ! GSI always starts 3hrs from syn hour 
contains

subroutine set_ (iamroot,rcname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 define background bias variables
!   prgmmr:	 todling
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.1
!     date:	 2014-10-05
!
! abstract: - set which variables in background is GSI to estimate biases for
!
! program history log:
!   2014-10-05  todling  - initial code
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block
use file_utility, only : get_lun
use mpeu_util, only: gettablesize
use mpeu_util, only: gettable
use mpeu_util, only: getindex
implicit none

logical,optional,intent(in) :: iamroot         ! optional root processor id
character(len=*),optional,intent(in) :: rcname ! optional input filename

character(len=*),parameter::myname_=myname//'*set_'
character(len=*),parameter:: tbname='background_bias_estimator::'
integer(i_kind) luin,ii,nrows,ntot,ipnt,istatus
integer(i_kind) i2d,i3d,n2d,n3d,irank
integer(i_kind),allocatable,dimension(:)::nlevs
character(len=256),allocatable,dimension(:):: utable
character(len=32),allocatable,dimension(:):: vars
character(len=32),allocatable,dimension(:):: sources
logical iamroot_,matched

if(bkg_bias_set_) return

iamroot_=mype==0
if(present(iamroot)) iamroot_=iamroot 

! load file
if (present(rcname)) then
   luin=get_lun()
   open(luin,file=trim(rcname),form='formatted')
else
   luin=5
endif

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nrows)
if(nrows==0) then
   if(luin/=5) close(luin)
   return
endif

! Get contents of table
allocate(utable(nrows))
call gettable(tbname,luin,ntot,nrows,utable)

! release file unit
if(luin/=5) close(luin)

! allocate space for entries from table
allocate(vars(nrows),nlevs(nrows),sources(nrows))

! Retrieve each token of interest from table and define
! variables participating in state vector
n2d=0; n3d=0
do ii=1,nrows
   read(utable(ii),*) vars(ii),&  ! variable name
                      nlevs(ii),& ! number of levels
                      sources(ii) ! source
   if (nlevs(ii)==1) then
      n2d=n2d+1
   else
      n3d=n3d+1
   endif
enddo

deallocate(utable)

allocate(bvars2d(n2d),bvars3d(n3d),&
         bsrcs2d(n2d),bsrcs3d(n3d),levels(n3d))

! loop over variables and identify them by comparison
i2d=0; i3d=0
do ii=1,nrows
   matched=.false.
   if(trim(sources(ii))=='met_guess') then
      if(associated(gsi_metguess_bundle)) then
         call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(vars(ii)),ipnt,istatus,irank=irank);
         if (ipnt>0) then
            if(irank==2) then 
               i2d=i2d+1
               bvars2d(i2d)=trim(vars(ii))
               bsrcs2d(i2d)=trim(sources(ii))
               matched=.true.
            endif
            if(irank==3) then 
               i3d=i3d+1
               bvars3d(i3d)=trim(vars(ii))
               bsrcs3d(i3d)=trim(sources(ii))
               levels(i3d) =abs(nlevs(ii))
               matched=.true.
            endif
         endif
      endif
   endif
   if(trim(sources(ii))=='chem_guess') then
      if(associated(gsi_chemguess_bundle)) then
         call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(vars(ii)),ipnt,istatus,irank=irank);
         if (ipnt>0) then
            if(irank==2) then
               i2d=i2d+1
               bvars2d(i2d)=trim(vars(ii))
               bsrcs2d(i2d)=trim(sources(ii))
               matched=.true.
            endif
            if(irank==3) then 
               i3d=i3d+1
               bvars3d(i3d)=trim(vars(ii))
               bsrcs3d(i3d)=trim(sources(ii))
               levels(i3d) =abs(nlevs(ii))
               matched=.true.
            endif
         endif
      endif
   endif
   ! now care for variables not in guess (eventual use)
   if (.not.matched) then
      if(nlevs(ii)==1) then
         i2d=i2d+1
         bvars2d(i2d)=trim(vars(ii))
         bsrcs2d(i2d)='unmatched'
      else
         i3d=i3d+1
         bvars3d(i3d)=trim(vars(ii))
         bsrcs3d(i3d)='unmatched'
         levels(i3d) =abs(nlevs(ii))
      endif
   endif
enddo

if (iamroot_) then
    write(6,*) myname_,':  BKG BIAS VARIABLES: '
    write(6,*) myname_,':  2D-BKG BIAS VARIABLES: '
    do ii=1,n2d
       write(6,*) trim(bvars2d(ii))
    enddo
    write(6,*) myname_,':  3D-BKG BIAS VARIABLES:'
    do ii=1,n3d
       write(6,*) trim(bvars3d(ii))
    enddo
end if

deallocate(vars,nlevs,sources)
bkg_bias_set_=.true.

end subroutine set_

subroutine init_(hour)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2014-10-04  todling - pass hour of analysis
!                       - bias estimates now held in bundle
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,one
  use guess_grids, only: nfldsig
  implicit none

  integer(i_kind),intent(in) :: hour

  character(len=*), parameter :: myname_=myname//'*init_'
  character(len=32) bname
  type(gsi_grid) :: grid

  integer(i_kind):: istatus
  integer(i_kind):: nt
  integer(i_kind) ida(8),jda(8)
  real(r_kind) fhrstep,fha(5),bkg_nhr

  if (initialized_) return 
  if (bcoption==0 ) return

! set table with fields to bias correct
  call set_(rcname='anavinfo')   ! would be better placed in Observer
  if ( .not. bkg_bias_set_ ) then
     bcoption = 0  ! in case table is absent but user sets to do BBC
     if(mype==0) then
        write(6,*) myname_,': WARNING, BKG bias correction missing driving table'
        write(6,*) myname_,': WARNING, BKG bias correction deactivated ...'
     endif
     return
  endif

  nbc = 1
  if (nint(diurnalbc)==1) nbc=3

! create grid which biases are defined over
  call GSI_GridCreate(grid,lat2,lon2,nsig)

! allocate structures
  allocate(gsi_bkgbias_bundle(nbc))

! loop over slots
  do nt=1,nbc

!    create background bias bundle for this slot
     write(bname,'(a,i3.3)') 'Bkg Bias Vector-',nt
     call GSI_BundleCreate(gsi_bkgbias_bundle(nt),grid,bname,istatus, &
                           names2d=bvars2d,names3d=bvars3d,levels=levels,bundle_kind=r_kind)

  enddo

! store dates/time background available for 
! NOTE: This should not really be done here (it belongs to guess_grids)
  allocate(bkg_nymd(nfldsig))
  allocate(bkg_nhms(nfldsig))
  if(nfldsig>1) then
    bkg_nhr=nhr_assimilation/nfldsig+1.0_r_kind 
  else
    bkg_nhr=6.0_r_kind 
  endif
  do nt=1,nfldsig
     ida(1:3)=ibdate(1:3)
     ida(5:6)=ibdate(4:5)
     jda(:)=0
     fha(:)=0.0_r_kind
     fhrstep = bkg_nstarthr + (nt-1)*bkg_nhr
     fha(2)=fhrstep-3.0_r_kind ! NCEP counts time from previous syn analysis
     call w3movdat(fha,ida,jda)
     bkg_nymd(nt)=jda(1)*10000+jda(2)*100+jda(3)
     bkg_nhms(nt)=jda(5)*10000
     if (mype==0) then
        write(6,'(2a,i8.8,a,i6.6)') myname_, 'Background fields available on ', bkg_nymd(nt), ' ', bkg_nhms(nt)
     endif
  enddo

  bias_hour=hour
  if(mype==0) &
    write(6,*) myname_, ': created ', nbc, ' slot(s) to hold bias estimate'
  initialized_ = .true.

end subroutine init_

subroutine clean_()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    clean_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2014-10-04  todling - bias estimates now held in bundle
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none

   character(len=*), parameter :: myname_=myname//'*clean_'
   integer(i_kind):: nt,istatus

   if (.not.initialized_) return 

   deallocate(bkg_nhms)
   deallocate(bkg_nymd)

!  destroy each instance of bundle
   do nt=1,size(gsi_bkgbias_bundle)

!     destroy nt slot ...
      call GSI_BundleDestroy(gsi_bkgbias_bundle(nt),istatus)
       if(istatus/=0) then
        if(mype==0) &
        write(6,*)myname_,':  Error Dealloc(), istatus=',istatus
      endif

   enddo
   deallocate(gsi_bkgbias_bundle)

end subroutine clean_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: bias_model2d_ --- 2d-interface to bias model
!
! !INTERFACE:
!

subroutine bias_model2d_(bias2d,varname,rc,hour,iflag)

! !USES:

   use constants, only: one
   implicit none

! !INPUT PARAMETERS:

   character(len=*)        ,intent(in   ) :: varname
   integer(i_kind),optional,intent(in   ) :: hour
   integer(i_kind),optional,intent(in   ) :: iflag  ! when -1 routine simple compress biases

! !INPUT/OUTPUT PARAMETERS:

   real   (r_kind),dimension(:,:),intent(inout) :: bias2d

! !OUTPUT PARAMETERS:

   integer(i_kind)               ,intent(  out) :: rc

! !DESCRIPTION: 2d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!   2014-10-04  todling - bug fix: was incorrect for persistent bias case
!                       - bias estimate now kept in bundle
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind) :: istatus
   real(r_kind) :: twopi
   real(r_kind) :: coshr,sinhr,coeff
   real(r_kind),allocatable,dimension(:,:,:):: bias

   rc=0
   allocate(bias(lat2,lon2,nbc))
   call gsi_bundlegetvar(gsi_bkgbias_bundle,varname,bias,istatus)
   if(istatus/=0) then
      deallocate(bias)
      rc=-1
      return
   endif

   coeff = biascor(1)
   if (present(iflag)) then
      if(iflag==-1) coeff=one
   endif

   if (nbc==1) then
      bias2d(:,:) = coeff*bias(:,:,1)
   endif
 
   if (nbc==3) then
      twopi=8._r_kind*atan(one)
      coshr=one*cos(twopi*hour/24._r_kind)*diurnalbc
      sinhr=one*sin(twopi*hour/24._r_kind)*diurnalbc

      bias2d(:,:) = coeff*(bias(:,:,1) + &
                           bias(:,:,2)*coshr + &
                           bias(:,:,3)*sinhr)
   endif
   deallocate(bias)

end subroutine bias_model2d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: bias_model3d_ --- 3d-interface to bias model
!
! !INTERFACE:
!

subroutine bias_model3d_(bias3d,varname,rc,hour,iflag)

! USES:

   use constants, only: one
   implicit none

! !INPUT PARAMETERS:

   character(len=*)        ,intent(in   ) :: varname
   integer(i_kind),optional,intent(in   ) :: hour
   integer(i_kind),optional,intent(in   ) :: iflag  ! when -1 routine simple compress biases

! !INPUT/OUTPUT PARAMETERS:

   real   (r_kind),dimension(:,:,:),intent(inout) :: bias3d

! !OUTPUT PARAMETERS:

   integer(i_kind)                 ,intent(  out) :: rc

! !DESCRIPTION: 3d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!   2014-10-04  todling - bug fix: was incorrect for persistent bias case
!                       - bias estimate now kept in bundle
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind) :: istatus
   real(r_kind) :: twopi
   real(r_kind) :: coshr,sinhr,coeff
   real(r_kind),allocatable,dimension(:,:,:,:):: bias

   rc=0

   allocate(bias(lat2,lon2,nsig,nbc))
   call gsi_bundlegetvar(gsi_bkgbias_bundle,varname,bias,istatus)
   if(istatus/=0) then
      rc=-1
      deallocate(bias)
      return
   endif

   coeff = biascor(1)
   if (present(iflag)) then
      if(iflag==-1) coeff=one
   endif

   if (nbc==1) then
      bias3d(:,:,:) = coeff*bias(:,:,:,1)
   endif

   if (nbc==3) then
      twopi=8._r_kind*atan(one)
      coshr=one*cos(twopi*hour/24._r_kind)*diurnalbc
      sinhr=one*sin(twopi*hour/24._r_kind)*diurnalbc

      bias3d(:,:,:) = coeff*(bias(:,:,:,1) + &
                             bias(:,:,:,2)*coshr + &
                             bias(:,:,:,3)*sinhr)
   endif
   deallocate(bias)

end subroutine bias_model3d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: bias_mode_bundle_ --- bundle-interface to bias model
!
! !INTERFACE:
!
subroutine bias_model_bundle_ (bundle,hour)

! USES:

   implicit none

! !INPUT PARAMETERS:

integer(i_kind),optional,intent(in) :: hour

! !INPUT/OUTPUT PARAMETERS:

type(gsi_bundle) bundle

! !DESCRIPTION: bundle-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2014-10-04  todling - initial code
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   todling      org: gmao     date: 2014-10-04
!
!EOP
!-------------------------------------------------------------------------

integer(i_kind) iv,istatus
real(r_kind), pointer, dimension(:,:)   :: ptr2d
real(r_kind), pointer, dimension(:,:,:) :: ptr3d

do iv=1,size(bvars2d)
   call gsi_bundlegetpointer(bundle, bvars2d(iv), ptr2d, istatus)
   if(istatus==0) call bias_model2d_ (ptr2d,bvars2d(iv),istatus,hour=hour)
enddo

do iv=1,size(bvars3d)
   call gsi_bundlegetpointer(bundle, bvars3d(iv), ptr3d, istatus)
   if(istatus==0) call bias_model3d_ (ptr3d,bvars3d(iv),istatus,hour=hour)
enddo

end subroutine bias_model_bundle_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: update2d_ --- 2d-interface to bias prediction model
!
! !INTERFACE:
!

subroutine update2d_(bias,sval,hour)

! USES:

  use constants, only: half,one,two
  implicit none

! !INPUT PARAMETERS:

  real   (r_kind),dimension(:,:),intent(in   ) :: sval
  integer(i_kind),optional      ,intent(in   ) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:),intent(inout) :: bias

! !DESCRIPTION: 2d-interface to bias prediction model
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!   2014-10-04  todling - revisit selection of persistent/diurnal cases
!                       - multiplicative parameter belongs to bias model
!   2014-10-11  todling - make bias sign consistent with Dee and da Silva (1998)
!                         and Dee and Todling (2000)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

! local variables

  real(r_kind) :: twopi,coshr,sinhr

  if (nbc==1) then
     bias(:,:,1)=bias(:,:,1) - biascor(2)*sval
  endif
  if (nbc==3) then
     twopi=8._r_kind*atan(one)
     coshr=cos(twopi*hour/24._r_kind)*two*biascor(2)
     sinhr=sin(twopi*hour/24._r_kind)*two*biascor(2)

     bias(:,:,1)=bias(:,:,1) - biascor(2)*sval
     bias(:,:,2)=bias(:,:,2) -      coshr*sval
     bias(:,:,3)=bias(:,:,3) -      sinhr*sval
  endif

end subroutine update2d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: update3d_ --- 3d-interface to bias prediction model
!
! !INTERFACE:
!

subroutine update3d_(bias,sval,hour)

! !USES:

  use constants, only: half,one,two
  implicit none

! !INPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:),intent(in   ) :: sval
  integer(i_kind),optional        ,intent(in   ) :: hour

! !INPUT/OUTPUT PARAMETERS:

  real   (r_kind),dimension(:,:,:,:),intent(inout) :: bias

! !DESCRIPTION: 3d-interface to bias prediction model, w/ sval as
!               a single-dimension array.
!
! !REVISION HISTORY:
!
!   2006-10-01  guo     - initial code
!   2006-12-04  todling - documentation/prologue
!   2014-10-04  todling - revisit selection of persistent/diurnal cases
!                       - multiplicative parameter belongs to bias model
!   2014-10-11  todling - make bias sign consistent with Dee and da Silva (1998)
!                         and Dee and Todling (2000)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   guo          org: gmao     date: 2006-10-01
!
!EOP
!-------------------------------------------------------------------------

! local variables

  real(r_kind) :: twopi,coshr,sinhr

  if(nbc==1) then
     bias(:,:,:,1)=bias(:,:,:,1) - biascor(2)*sval
  endif
  if(nbc==3) then
     twopi=8._r_kind*atan(one)
     coshr=cos(twopi*hour/24._r_kind)*two*biascor(2)
     sinhr=sin(twopi*hour/24._r_kind)*two*biascor(2)
 
     bias(:,:,:,1)=bias(:,:,:,1) - biascor(2)*sval
     bias(:,:,:,2)=bias(:,:,:,2) -      coshr*sval
     bias(:,:,:,3)=bias(:,:,:,3) -      sinhr*sval
  endif

end subroutine update3d_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: updateall_ --- update bias in all analysis fields
!
! !INTERFACE:
!

subroutine updateall_ (sval,sval_div,sval_vor,hour)

! !USES:

  use gridmod, only: twodvar_regional

  implicit none

! !INPUT PARAMETERS:

  type(gsi_bundle)             ,intent(in) :: sval
  real(r_kind),dimension(:,:,:),intent(in) :: sval_vor,sval_div
  integer(i_kind),optional     ,intent(in) :: hour

! !OUTPUT PARAMETERS:

! !DESCRIPTION: 3d-interface to update bias of all fields
!
! !REVISION HISTORY:
!
!   2006-12-04  todling - initial code
!   2011-02-11  zhu     - add gust,vis,pblh
!   2014-10-04  todling - bias now kept in bundle
!                       - bug fix: vor/div where fed in reverse (ie, vor to div) 
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   todling          org: gmao     date: 2006-12-04
!
!EOP
!-------------------------------------------------------------------------

  character(len=*), parameter :: myname_=myname//'*updateall_'
  logical upa_update
  integer(i_kind) :: ier,istatus
  real(r_kind),allocatable :: bias2dp1(:,:,:)
  real(r_kind),allocatable :: bias3dp1(:,:,:,:)
  real(r_kind),pointer :: ptr_2d(:,:)
  real(r_kind),pointer :: ptr_ps(:,:), ptr_sst(:,:)
  real(r_kind),pointer :: ptr_tv(:,:,:), ptr_q(:,:,:)
  real(r_kind),pointer :: ptr_u(:,:,:), ptr_v(:,:,:)
  real(r_kind),pointer :: ptr_oz(:,:,:), ptr_cw(:,:,:)
 
! Define pointers for isolated u,v on subdomains work vector


  upa_update=.false.
  allocate(bias2dp1(lat2,lon2,nbc))
  allocate(bias3dp1(lat2,lon2,nsig,nbc))

  call gsi_bundlegetpointer(sval,'u',  ptr_u, istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'u',bias3dp1,istatus);ier=ier+istatus
  if(ier==0) then
     call update3d_ (bias3dp1,ptr_u,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'u',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'v',  ptr_v,  istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'v',bias3dp1,istatus); ier=ier+istatus
  if(ier==0) then
     call update3d_ (bias3dp1,ptr_v,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'v',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'tv', ptr_tv, istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'tv',bias3dp1,istatus); ier=ier+istatus
  if(ier==0) then 
     call update3d_ (bias3dp1,ptr_tv,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'tv',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'q',  ptr_q,  istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'q',bias3dp1,istatus); ier=ier+istatus
  if(ier==0) then 
     call update3d_ (bias3dp1,ptr_q,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'q',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'oz', ptr_oz, istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'oz',bias3dp1,istatus); ier=ier+istatus
  if(ier==0) then 
     call update3d_ (bias3dp1,ptr_oz,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'oz',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'cw', ptr_cw, istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'cw',bias3dp1,istatus); ier=ier+istatus
  if(ier==0) then 
     call update3d_ (bias3dp1,ptr_cw,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'cw',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetvar(gsi_bkgbias_bundle,'div',bias3dp1,istatus)
  if(istatus==0) then 
     call update3d_ (bias3dp1,sval_div,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'div',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetvar(gsi_bkgbias_bundle,'vor',bias3dp1,istatus)
  if(istatus==0) then 
     call update3d_ (bias3dp1,sval_vor,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'vor',bias3dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'ps', ptr_ps, istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'ps',bias2dp1,istatus); ier=ier+istatus
  if(ier==0) then 
     call update2d_ (bias2dp1,ptr_ps,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'ps',bias2dp1,istatus)
     upa_update=.true.
  endif

  call gsi_bundlegetpointer(sval,'sst',ptr_sst,istatus); ier=istatus
  call gsi_bundlegetvar(gsi_bkgbias_bundle,'sst',bias2dp1,istatus); ier=ier+istatus
  if(ier==0) then 
     call update2d_ (bias2dp1,ptr_sst,hour=hour)
     call gsi_bundleputvar(gsi_bkgbias_bundle,'sst',bias2dp1,istatus)
     upa_update=.true.
  endif

  deallocate(bias2dp1)
  deallocate(bias3dp1)

  if (upa_update) then
     if(present(hour)) then
       if(mype==0) & 
       write(6,*) trim(myname_), ': complete bias update for all upper-air vars at hour ',hour
     else
       if(mype==0) & 
       write(6,*) trim(myname_), ': complete bias update for all upper-air variables'
     endif
  endif


  if (twodvar_regional) then
     call gsi_bundlegetpointer (sval,'gust' ,ptr_2d, istatus)
     if (istatus>0) then
        allocate(bias2dp1(size(ptr_2d,1),size(ptr_2d,2),nbc))
        call gsi_bundlegetpointer (sval,'gust' ,ptr_2d, istatus)
        call gsi_bundlegetvar (gsi_bkgbias_bundle,'gust' ,bias2dp1, istatus)
        call update2d_  (bias2dp1,ptr_2d,hour=hour)
        call gsi_bundleputvar (gsi_bkgbias_bundle,'gust' ,bias2dp1, istatus)
        deallocate(bias2dp1)
     end if

     call gsi_bundlegetpointer (sval,'vis' ,ptr_2d, istatus)
     if (istatus>0) then
        allocate(bias2dp1(size(ptr_2d,1),size(ptr_2d,2),nbc))
        call gsi_bundlegetvar (gsi_bkgbias_bundle,'vis' ,bias2dp1, istatus)
        call update2d_  (bias2dp1 ,ptr_2d,hour=hour)
        call gsi_bundleputvar (gsi_bkgbias_bundle,'vis' ,bias2dp1, istatus)
        deallocate(bias2dp1)
     end if

     call gsi_bundlegetpointer (sval,'pblh' ,ptr_2d, istatus)
     if (istatus>0) then
        allocate(bias2dp1(size(ptr_2d,1),size(ptr_2d,2),nbc))
        call gsi_bundlegetvar (gsi_bkgbias_bundle,'pblh' ,bias2dp1, istatus)
        call update2d_  (bias2dp1,ptr_2d,hour=hour)
        call gsi_bundleputvar (gsi_bkgbias_bundle,'pblh' ,bias2dp1, istatus)
        deallocate(bias2dp1)
     end if
     if (present(hour)) then
        if(mype==0) & 
        write(6,*) trim(myname_), ': complete bias update for 2d-Var vars at hour ', hour
     else
        if(mype==0) & 
        write(6,*) trim(myname_), ': complete bias update for 2d-Var variables'
     endif
  end if

end subroutine updateall_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!    &    NASA/GMAO, Global Modeling and Assimilation Office             !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: correct_ --- perform bias correction of background state
!
! !INTERFACE:
!

subroutine correct_()

! !USES:

  use gridmod, only: twodvar_regional
  use guess_grids, only: nfldsig
  use guess_grids, only: sfct
  use constants, only: tiny_r_kind,qmin,qcmin
  use mpeu_util, only: die
  use mpeu_util, only: getindex

  implicit none

! !INPUT PARAMETERS:
! !OUTPUT PARAMETERS:
! !DESCRIPTION: correct background field with bias estimate
!
! !REVISION HISTORY:
!
!   2006-12-04  todling - initial code
!   2011-02-11  zhu     - add gust,vis,pblh
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2014-10-11  todling - make bias sign consistent with Dee and da Silva (1998)
!                         and Dee and Todling (2000)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP; Altix Linux
!
! !AUTHOR:
!   todling          org: gmao     date: 2006-12-04
!
!EOP
!-------------------------------------------------------------------------
! local variables

  character(len=*),parameter::myname_=myname//'*correct_'

  real(r_kind),pointer,dimension(:,:  )  :: ptr2dges   =>NULL()
  real(r_kind),pointer,dimension(:,:  )  :: ges_ps_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_u_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_v_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_div_it =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_vor_it =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_tv_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_q_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_oz_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)  :: ges_cwmr_it=>NULL()

  real(r_kind),allocatable,dimension(:,:)  :: work2d
  real(r_kind),allocatable,dimension(:,:,:):: work3d

  integer(i_kind) :: it,bkg_hour,idummy,ier,istatus

! Get memory for bias-related arrays

  allocate(work2d(lat2,lon2),work3d(lat2,lon2,nsig))

  do it=1,nfldsig
     bkg_hour = bkg_nhms(it)/10000

!    Get pointer to could water mixing ratio
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,istatus)
     if (istatus==0) then
        call bias_model2d_(work2d,'ps',ier,hour=bkg_hour,iflag=-1)
        if(ier==0) then
           ges_ps_it = ges_ps_it - work2d
        endif
     endif

     call bias_model2d_(work2d,'sst',ier,hour=bkg_hour,iflag=-1)
     if(ier==0) then
        sfct(:,:,it) = sfct(:,:,it) - work2d
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u',ges_u_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'u',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_u_it,work3d)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v',ges_v_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'v',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_v_it,work3d)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'tv',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_tv_it,work3d)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'vor',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_vor_it,work3d)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'div',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_div_it,work3d)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'cw' ,ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_cwmr_it,work3d,qcmin)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q',ges_q_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d ,'q',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_q_it,work3d,qmin)
        endif
     endif

     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz_it,istatus)
     if (istatus==0) then
        call bias_model3d_(work3d,'oz',ier,hour=bkg_hour,iflag=-1)
        if (ier==0) then
           call upd_fldr3_(ges_oz_it,work3d,tiny_r_kind)
        endif
     endif

     if (twodvar_regional) then
        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'gust',idummy,istatus)
        if(istatus==0) then
           call bias_model2d_(work2d,'gust',ier,hour=bkg_hour,iflag=-1)
           if (ier==0) then
              call gsi_bundlegetpointer (gsi_metguess_bundle(it),'gust',ptr2dges,ier)
              if (ier==0) then
                 ptr2dges = ptr2dges - work2d
                 ptr2dges =>NULL()
              end if
           end if
        end if

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vis',idummy,istatus)
        if(istatus==0) then
           call bias_model2d_(work2d,'vis',ier,hour=bkg_hour,iflag=-1)
           if (ier==0) then
              call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vis',ptr2dges,ier)
              if (ier==0) then
                 ptr2dges = ptr2dges - work2d
                 ptr2dges =>NULL()
              end if
           end if
        end if

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'pblh',idummy,istatus)
        if(istatus==0) then
           call bias_model2d_(work2d,'pblh',ier,hour=bkg_hour,iflag=-1)
           if (ier==0) then
              call gsi_bundlegetpointer (gsi_metguess_bundle(it),'pblh',ptr2dges,ier)
              if (ier==0) then
                 ptr2dges = ptr2dges - work2d
                 ptr2dges =>NULL()
              end if
           end if
        end if
     end if
  end do

! Clean up bias-related arrays

  deallocate(work2d,work3d)

end subroutine correct_

subroutine upd_fldr3_(ges,xinc,threshold)
  real(r_kind),optional:: threshold
  real(r_kind),pointer :: ges(:,:,:)
  real(r_kind)         :: xinc(:,:,:)
  integer(i_kind) i,j,k
  if(present(threshold))then
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges(i,j,k) = max(ges(i,j,k)-xinc(i,j,k),threshold)
           end do
        end do
     end do
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              ges(i,j,k) = ges(i,j,k)-xinc(i,j,k)
           end do
        end do
     end do
  endif
end subroutine upd_fldr3_

end module m_gsiBiases
