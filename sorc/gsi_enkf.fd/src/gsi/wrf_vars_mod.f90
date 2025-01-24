module wrf_vars_mod
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    wrf_vars_mod
!
!   prgrmmr: Todling
!
! abstract:
!
! program history log:
!   2019-07-11  Todling - add to replace incorrect placement of variables in
!                         control_vectors file. The fact is that a clean up on
!                         what has been done for w (dw) and dbz would not
!                         require this file not the variables defined here.
!      ...>>> the is currently a major mix up (esp. in the WRF side of the code)
!             between what is a control variable and a state variable. The
!             latter is generally the guess field (and controlled by metguess)
!             Therefore the read routines from WRF should NEVER refer to the CV
!             variables as they do ... indeed there should never be a use 
!             statement in those codes related to CV.
!
!             Things like
!               use control_vectors, only: cvars3d
!             should be replaced by
!               use state_vectors, only: svars3d
!
!              checks can done on the fly to avoid definition of variables
!              such as the ones in this module.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
use mpimod, only: mype
use control_vectors, only: nc3d,cvars3d
use kinds, only: i_kind
use gsi_metguess_mod, only: gsi_metguess_get
use constants, only: max_varname_length
implicit none
private
! public methods
public :: init_wrf_vars
! common block variables
public :: w_exist
public :: dbz_exist
public :: fed_exist

logical,save :: w_exist, dbz_exist, fed_exist
contains

subroutine init_wrf_vars
integer(i_kind) ii,istatus
character(max_varname_length),allocatable,dimension(:) :: cloud
integer(i_kind) ncloud
logical :: dbz_cloud_exist,fed_cloud_exist

w_exist=.false.
dbz_exist=.false.
fed_exist=.false.
dbz_cloud_exist=.false.
fed_cloud_exist=.false.

do ii=1,nc3d
  if(mype == 0 ) write(6,*)"anacv cvars3d is ",cvars3d(ii)
  if(trim(cvars3d(ii)) == 'w'.or.trim(cvars3d(ii))=='W') w_exist=.true.
  if(trim(cvars3d(ii))=='dbz'.or.trim(cvars3d(ii))=='DBZ') dbz_exist=.true.
  if(trim(cvars3d(ii))=='fed'.or.trim(cvars3d(ii))=='FED') fed_exist=.true.
enddo

! Inquire about clouds

call gsi_metguess_get('clouds::3d',ncloud,istatus)
if (ncloud>0) then
   allocate(cloud(ncloud))
   call gsi_metguess_get('clouds::3d',cloud,istatus)
endif

do ii=1,ncloud
  if(mype == 0 ) write(6,*)"metguess cloud3d is ",cloud(ii)
  if(trim(cloud(ii))=='fed'.or.trim(cloud(ii))=='FED')fed_cloud_exist=.true.
  if(trim(cloud(ii))=='dbz'.or.trim(cloud(ii))=='DBZ')dbz_cloud_exist=.true.
end do

if(.not.fed_exist .or. .not.fed_cloud_exist )then
  fed_exist=.false.
endif  

if(.not.dbz_exist .or. .not.dbz_cloud_exist )then
  dbz_exist=.false.
endif

if(ncloud>0) deallocate(cloud)

end subroutine init_wrf_vars

end module wrf_vars_mod
