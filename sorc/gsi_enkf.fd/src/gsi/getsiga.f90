subroutine getsiga ()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getsiga
!   prgmmr: todling
!
! abstract:  Calculate analysis errors from Lanczos-CG results
!
! program history log:
!   2010-03-16  todling  - initial code
!   2010-05-14  todling  - update to use gsi_bundle
!   2010-05-27  todling  - gsi_4dcoupler; remove all user-specific TL-related references
!   2010-08-19  lueken   - add only to module use;no machine code, so use .f90
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

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: ibdate,lsqrtb,jsiga
use jfunc, only: jiter
use lanczos, only : congrad_siga
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
implicit none
! declare local variables
character(len=*),parameter:: myname_ = "getsiga"
type(gsi_bundle)     :: siga                      ! vector to analysis errors
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: nvecs
integer(i_kind)      :: ier

! consistency checks
if (jiter/=jsiga) return
if (.not.lsqrtb) then
   write(6,*)trim(myname_),': must set lsqrt=.t. to get analysis errors'
   call stop2(331)
end if

nymd = 10000*ibdate(1)+ibdate(2)*100+ibdate(3)
nhms = 10000*ibdate(4)
if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': starting to calculate analysis errors at ',&
             nymd, nhms

! allocate memory for working arrays
call allocate_state(siga)

! calculate estimate of analysis errors
call congrad_siga(siga,nvecs,ier)

! write out analysis errors
if(ier==0) then
   call gsi_4dcoupler_putpert (siga,nymd,nhms,'tlm','siga')
   if(mype==0) write(6,'(2a,i5,a)')trim(myname_),': complete calculating analysis errors using ',&
                                   nvecs, ' eigenvectors'
else
   if(mype==0) write(6,'(2a,i6)')trim(myname_),': failed to calculate analysis errors, ier= ', ier
endif

! clean up
call deallocate_state(siga)

return
end subroutine getsiga

subroutine view_cv (xhat,mydate,filename,writecv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_cv
!   prgmmr: todling
!
! abstract:  this allow writing CV to file for visualization
!
! program history log:
!   2011-02-23  todling  - initial code
!                          (not sure we'll keep this here)
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

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: nsubwin,lsqrtb
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
use control_vectors, only: control_vector,write_cv
use state_vectors, only: allocate_state,deallocate_state,prt_state_norms
use bias_predictors, only: predictors,allocate_preds,deallocate_preds
use bias_predictors, only: write_preds
implicit none
type(control_vector)        :: xhat
integer(i_kind), intent(in) :: mydate(5) ! as in iadate or ibdate, or similar
character(len=*),intent(in) :: filename
logical,         intent(in) :: writecv   ! when .t., simply write out CV directly
! declare local variables
character(len=*),parameter:: myname_ = "view_cv"
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: ii
type(gsi_bundle) :: mval(nsubwin)
type(predictors) :: sbias

! in case CV not required to be transformed ...
if (writecv) then
   call write_cv(xhat,filename)
   return
else
   if(mype==0) write(6,*) trim(myname_),': not writing CV to disk for now'
   return
endif

! otherwise, transform CV to state-space and write out ...
nymd = 10000*mydate(1)+mydate(2)*100+mydate(3)
nhms = 10000*mydate(4)
if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': start writing state on ',&
             nymd, nhms

! Allocate local variables
do ii=1,nsubwin
   call allocate_state(mval(ii))
end do
call allocate_preds(sbias)

if (lsqrtb) then
   call control2model(xhat,mval,sbias)
else
   call control2state(xhat,mval,sbias)
endif

! write out analysis errors
do ii=1,nsubwin
   call gsi_4dcoupler_putpert (mval(ii),nymd,nhms,'tlm',filename) ! will need to be smart for nsubwin>1
   call prt_state_norms(mval(ii),'output-state')
enddo
call write_preds(sbias,'preds_'//trim(filename),mype)

! Allocate local variables
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do

if(mype==0) write(6,'(3a)')trim(myname_),': complete writing state ', trim(filename)

return
end subroutine view_cv

subroutine view_cv_ad (xhat,mydate,filename,readcv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_cv_ad
!   prgmmr: todling
!
! abstract:  this allows reading state/control vector and transforming to CV
!
! program history log:
!   2012-05-22  todling  - based on view_ad, performs opposite of view_cv
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

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: nsubwin,lsqrtb
use state_vectors, only: allocate_state,deallocate_state
use gsi_4dcouplermod, only: gsi_4dcoupler_getpert
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: assignment(=)
use control_vectors, only: control_vector,read_cv,assignment(=)
use state_vectors, only: allocate_state,deallocate_state,prt_state_norms
use bias_predictors, only: predictors,allocate_preds,deallocate_preds,assignment(=)
use bias_predictors, only: read_preds
use control2state_mod, only: control2state_ad
implicit none
type(control_vector)        :: xhat
integer(i_kind), intent(in) :: mydate(5) ! as in iadate or ibdate, or similar
character(len=*),intent(in) :: filename
logical,         intent(in) :: readcv    ! when .t. simply read in CV
! declare local variables
character(len=*),parameter:: myname_ = "view_cv_ad"
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: ii
type(gsi_bundle) :: mval(nsubwin)
type(predictors) :: sbias

! in case CV not required to be transformed ...
if (readcv) then
   call read_cv(xhat,filename)
   return
else ! for now only
   xhat=zero
   if(mype==0) write(6,*) trim(myname_),': input vector set to zero for now'
   return
endif

! otherwise read state-vector and transform to control-space ...
nymd = 10000*mydate(1)+mydate(2)*100+mydate(3)
nhms = 10000*mydate(4)
if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': start reading state on ',&
             nymd, nhms

! Allocate local variables
do ii=1,nsubwin
   call allocate_state(mval(ii))
   mval(ii)=zero
end do
call allocate_preds(sbias)
sbias=zero
xhat=zero

if (lsqrtb) then
   call control2model(xhat,mval,sbias) ! dirty trick
endif
! read in (model/state) vector
do ii=1,nsubwin
   mval(ii)=zero
   call gsi_4dcoupler_getpert (mval(ii),'tlm',filename) ! will need better for nsubwin>1
   call prt_state_norms(mval(ii),'input-state')
enddo
call read_preds(sbias,'preds_'//trim(filename))

! convert to control vector
if (lsqrtb) then
   call control2model_ad(mval,sbias,xhat)
else
   call control2state_ad(mval,sbias,xhat)
endif

! Allocate local variables
call deallocate_preds(sbias)
do ii=1,nsubwin
   call deallocate_state(mval(ii))
end do

if(mype==0) write(6,'(3a)')trim(myname_),': complete reading state ', trim(filename)
return
end subroutine view_cv_ad

subroutine view_st (sval,filename)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    view_st
!   prgmmr: todling
!
! abstract:  this allow writing CV to file for visualization
!
! program history log:
!   2011-02-23  todling  - initial code
!                          (not sure we'll keep this here)
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

use kinds, only: i_kind, r_kind
use mpimod, only: mype
use constants, only: zero,one
use gsi_4dvar, only: ibdate,nobs_bins,nhr_obsbin
use gsi_4dcouplermod, only: gsi_4dcoupler_putpert
use gsi_bundlemod, only: gsi_bundle
implicit none
type(gsi_bundle)            :: sval(nobs_bins)
character(len=*),intent(in) :: filename
! declare local variables
character(len=*),parameter:: myname_ = "view_st"
integer(i_kind)      :: nymd                      ! date as in YYYYMMDD
integer(i_kind)      :: nhms                      ! time as in HHMMSS
integer(i_kind)      :: ii
integer(i_kind)      :: mydate(5)

integer(i_kind),dimension(8) :: ida,jda
real(r_kind),dimension(5)    :: fha


! write out analysis errors
mydate = ibdate
do ii=1,nobs_bins
   nymd = 10000*mydate(1)+mydate(2)*100+mydate(3)
   nhms = 10000*mydate(4)
   ! iwrtinc ...

   if(mype==0) write(6,'(2a,i8.8,2x,i6.6)')trim(myname_),': start writing state on ', nymd, nhms
   call gsi_4dcoupler_putpert (sval(ii),nymd,nhms,'tlm',filename)
!  call prt_state_norms(sval(ii),'output-state')

   ! increment mydate ...
   fha(:)=0.0; ida=0; jda=0
   fha(2)=nhr_obsbin! relative time interval in hours
   ida(1)=mydate(1) ! year
   ida(2)=mydate(2) ! month
   ida(3)=mydate(3) ! day
   ida(4)=0         ! time zone
   ida(5)=mydate(4) ! hour
   ! Move date-time forward by nhr_assimilation hours
   call w3movdat(fha,ida,jda)
   mydate(1)=jda(1)
   mydate(2)=jda(2)
   mydate(3)=jda(3)
   mydate(4)=jda(5)
enddo

if(mype==0) write(6,'(3a)')trim(myname_),': complete writing state ', trim(filename)

return
end subroutine view_st
