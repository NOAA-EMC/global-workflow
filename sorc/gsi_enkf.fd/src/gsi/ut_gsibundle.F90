!BOI

!  !TITLE: GSI\_Bundle and its Unit Test

!  !AUTHORS: Ricardo Todling

!  !AFFILIATION: Global Modeling and Assimilation Office, NASA/GSFC, Greenbelt,  MD 20771

!  !DATE: 11 May 2010

!  !INTRODUCTION: Overview
#ifdef __PROTEX__

This program provides a serious of illustrations for how to use GSI\_Bundle and
its various functions. The Bundle idea is largely based on a similar ESMF
concept. A Bundle is a collection of fields defined on a given grid. For the time
being, the GSI\_Bundle uses a very simple definition of grid -- a regular grid
-- but it should be relatively simple to generalize it.

See more details about GSI\_Bundle in the DESCRIPTION section of the
main prologue of GSI\_Bundle itself.

#endif
!BOI

#define _BUNDLE_R4_
!-----------------------------------------------------------------------
!BOP
!
! !PROGRAM: ut_gsibunde:  Test driver for GSI_Bundle
!
! !USES:

use kinds, only: i_kind,r_kind
use constants, only: one
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_BundlePutVar
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundleMerge
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleDestroy

! these should eventually not belog to the bundle
use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_GridCreate

implicit none

! !DESCRIPTION: This is a little test program to illustrate how to
!               use GSI\_Bundle. The following test are implemented:
!  \bn
!        \item Creation and Deletion of Bundle
!        \item Retrieval of varibles from the Bunle
!        \item Write something to the Bundle
!        \item Merge of two Bunldes
!  \en
!
! !REMARKS:
!    1. The definition of grid is so far a very simple regular
!       (im,jm,km)-type grid.
!
!    2. By definition, all fields in a Bundle must be on the same
!       grid.
!
! !REVISION HISTORY:
!
!  23Apr2010  Todling  Initial code.
!  28Apr2011  Todling  Add code to test REAL*4 support (please read
!                      comments in routine Ensemble()
!
!EOP
!-----------------------------------------------------------------------
!BOC

! Lets pretend we have a simple (im,jm,km)-like (regular) grid
integer(i_kind), parameter :: lat2=91
integer(i_kind), parameter :: lon2=144
integer(i_kind), parameter :: lev =72

! And lets pretend we have a simple set of 2dr- and 3d-variables
integer(i_kind), parameter :: n2d = 2
character(len=16), parameter :: names2d(n2d) = (/  &
                                'phis            ',    &
                                'ps              '    /)
integer(i_kind), parameter :: n3d = 3
character(len=16), parameter :: names3d(n3d) = (/  &
                                'u               ',    &
                                'v               ',    &
                                'tv              '    /)

integer(i_kind)  ipnt, ierr

type(GSI_Bundle) :: GSI_bundle_mix
type(GSI_Grid)   :: grid

! First create a grid
call GSI_GridCreate ( grid, lat2, lon2, lev )

!---------------------------------------------------------------
! Lets now create a bundle with both 2d- and 3d-variables above,
! defined on our simple grid
!---------------------------------------------------------------
call GSI_BundleCreate ( GSI_bundle_mix, grid, 'Mix Bundle', ierr, &
                        names2d=names2d, names3d=names3d ) 

!---------------------------------------------------------------
! Suppose we now want to find variable "v" and also that we
! want to write something on it ... 
!---------------------------------------------------------------
call GSI_BundleGetPointer ( GSI_bundle_mix, 'v', ipnt, ierr ) 
  if(ierr/=0) then
     print*, 'Pointer not found '
  else
     print*, 'Found field v at location ', ipnt
     GSI_bundle_mix%r3(ipnt)%q = 2._r_kind 
     call GSI_BundlePrint ( GSI_bundle_mix )
  endif

!---------------------------------------------------------------
! Here is another way to write something in a variable, say,
! write a constant to "ps" ...
!---------------------------------------------------------------
call GSI_BundlePutVar ( GSI_bundle_mix, 'ps' , one, ierr )
  if(ierr/=0) then
     print*, 'Trouble using PutVar'
  else
     print*, 'Found field ps at location ', ipnt
     call GSI_BundlePrint ( GSI_bundle_mix )
  endif

! ------
! TEST 1: merge two bundles
! ------
  call merger ( GSI_bundle_mix )

! ------
! TEST 2: create a multiple bundle
! ----
  call multi ( GSI_bundle_mix )

! ------
! TEST 3: append to bundle another bundle w/ mixed vertical layers/levels
! ----
  call edge ( GSI_bundle_mix )

! ------
! TEST 4: test creating a bundle w/ "mix-grids" as in control vector
! ----
  call ensemble ( )

! ------
! TEST 3: print original bundle to show that none of the above changed it
! ------
print*
print*, 'Original bundle should be kept untouched'
call GSI_BundlePrint ( GSI_bundle_mix )

!---------------
! destroy bundle
!---------------
call GSI_BundleDestroy ( GSI_bundle_mix, ierr )

end
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Merger --- Test merging two bundles into a new one
!
! !INTERFACE:
!
subroutine Merger ( GSI_bundle_mix )

! !USES:

use kinds, only: i_kind,r_kind
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_BundlePutVar
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundleMerge
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleDestroy

! these should eventually not belog to the bundle
use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_GridCreate

implicit none

! !INPUT/OUTPUT PARAMETERS:

type(GSI_Bundle) :: GSI_Bundle_mix 

!
! !DESCRIPTION: Test merging two bundles:
!    \bn
!       \item Input known bundle
!       \item Create new bundle w/ 1d-vectors
!       \item Merge known and new bundle into yet another bundle
!       \item Write some fields in the bundle
!       \item Show contents of bundle
!    \en
!
! !REVISION HISTORY:
!
!  22Apr2010 Todling - Initial code
!
!EOP
!-------------------------------------------------------------------------
!BOC

! Pretend we now have 1d-fields
integer(i_kind), parameter :: n1d = 2
character(len=16), parameter :: names1d(n1d) = (/  &
                                'lat             ',    &
                                'lon             '    /)

type(GSI_Bundle) :: GSI_Bundle_new 
type(GSI_Bundle) :: GSI_Bundle_all 
type(GSI_Grid)   :: grid1d
integer(i_kind)  ipnt, lat2, lon2, nlev, ierr
real(r_kind),allocatable,dimension(:)  ::varRank1
real(r_kind),allocatable,dimension(:,:)::varRank2

lat2 = GSI_bundle_mix%grid%im
lon2 = GSI_bundle_mix%grid%jm
nlev = GSI_bundle_mix%grid%km

print*, 'TESTING MERGE ...'
print*, '-----------------'

! create simple grid
call GSI_GridCreate (grid1d, lat2, 0, 0)

!------------------------------------
! create a new 1d bundle and merge it w/ bundle above into yet a 3rd bundle
!------------------------------------
call GSI_BundleCreate ( GSI_bundle_new, grid1d, 'New 1d Bundle', ierr, &
                                    names1d=names1d )
     call GSI_BundleGetPointer ( GSI_bundle_new, 'lon', ipnt, ierr ) 
     call random_number(GSI_bundle_new%r1(ipnt)%q)
     print*, 'this is the new bundle w/ 1d fields'
     call GSI_BundlePrint ( GSI_bundle_new )

!--------------------------------------------
! merge two bundle ... grids must be the same
!--------------------------------------------
call GSI_BundleMerge ( GSI_bundle_all, GSI_bundle_mix, GSI_bundle_new, 'Merged All', ierr )
  if(ierr/=0) then
     print*, 'could not merge bundles '
  else
     call GSI_BundleDestroy ( GSI_bundle_new, ierr ) ! release some mem
     allocate(varRank2(lat2,lon2))
     call random_number(varRank2)
     call GSI_BundlePutVar ( GSI_bundle_all, 'phis', varRank2, ierr ) 
     if (ierr==0) then
        print*
        print*, 'modify phis in new/merged bundle ...'
        call GSI_BundlePrint ( GSI_bundle_all )
     endif
     deallocate(varRank2)

     allocate(varRank1(lat2*lon2*nlev))
     call random_number(varRank1)
     call GSI_BundlePutVar ( GSI_bundle_all, 'u', varRank1, ierr ) 
     if (ierr==0) then
        print*
        print*, 'modify u in new/merged bundle ...'
        call GSI_BundlePrint ( GSI_bundle_all )
     endif
     deallocate(varRank1)
  endif

!-------------------
! destroy all bundle
!-------------------
call GSI_BundleDestroy ( GSI_bundle_all, ierr )

end subroutine merger
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Multi --- Test for handling multi instances of Bundle
!
! !INTERFACE:

subroutine Multi ( GSI_bundle_mix )

! !USES:

use kinds, only: i_kind,r_kind
use constants, only: zero
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundleMerge
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleDestroy
use gsi_bundlemod, only : assignment(=)
use m_rerank, only : rerank

implicit none

! !INPUT/OUTPUT PARAMETERS:

type(GSI_Bundle) :: GSI_Bundle_mix 

!
! !DESCRIPTION: Test handling a multi-instance Bundle (control vector-like).
!    \bn
!       \item Create a new bundle based on pre-existing bundle
!       \item Create new bundle w/ 1d-vectors
!       \item Merge known and new bundle into yet another bundle
!       \item Write some fields in the bundle
!       \item Show contents of bundle
!    \en
!
! !REVISION HISTORY:
!
!  22Apr2010 Todling - Initial code
!
!EOP
!-------------------------------------------------------------------------
!BOC

type(GSI_Bundle),allocatable :: GSI_Bundle_new(:) 
type(GSI_Bundle) :: GSI_Bundle_dup
integer(i_kind) jj, ipnt, ierr
integer(i_kind) nus,nts,nue,nte, ipnts(2), ivals(2)
integer(i_kind),parameter :: nsub=3
character(len=256) :: bname
real(r_kind),dimension(:),pointer :: u
real(r_kind),dimension(:),pointer :: tv

print*, 'TESTING MULTI ...'
print*, '-----------------'

!------------------------------------------------------
! creates new bundle based on pre-existing one
! Notes:
!   a) creation does not fill in contents of bundle
!   b) an explicit copy must be done to copy contents
!   c) for doing (b), notice use of assignment(=) above
!------------------------------------------------------
allocate(GSI_bundle_new(nsub))

call GSI_BundleCreate ( GSI_bundle_dup, GSI_bundle_mix, trim(GSI_bundle_mix%name), ierr )
GSI_bundle_dup = GSI_bundle_mix   ! copies a bundle into another

! Create now multi-instance Bundle
! Notes:
!   a) For GSI purposes, you can think of this as the control vector 
!      in a nsub-long-window 4dvar
do jj=1,nsub
  write(bname,'(a,i2.2)') 'Bundle at ',jj
  call GSI_BundleCreate ( GSI_bundle_new(jj), GSI_bundle_dup, trim(bname), ierr )
  GSI_bundle_new(jj) = GSI_bundle_dup
  call GSI_BundlePrint ( GSI_bundle_new(jj) )
enddo

! Clean up a little
call GSI_BundleDestroy ( GSI_bundle_dup, ierr )

! Now, in each sub-window (instance) do something different ...
do jj=1,nsub
!    ... like ...
     if(jj==1) then
!      ... filling phis with random numbers in 1st window ...
       call GSI_BundleGetPointer ( GSI_bundle_new(jj), 'phis', ipnt, ierr ) 
       if (ierr==0) then
          call random_number(GSI_bundle_new(jj)%r2(ipnt)%q)
       endif
       print*
       print*, 'modify phis in multi-bundle for jj= ',jj
     endif
     if(jj==2) then
!      ... or changing buth u ad tv in 2nd window ...
       call GSI_BundleGetPointer ( GSI_bundle_new(jj), (/'u ','tv'/),  ipnts, ierr, ivals=ivals ) 
       if (ierr==0) then
          nus= ivals(1)
          nts= ivals(2)
          nue= nus+size(GSI_bundle_new(jj)%r3(ipnts(1))%q)
          nte= nts+size(GSI_bundle_new(jj)%r3(ipnts(2))%q)
          u  => GSI_bundle_new(jj)%values(nus:nue)
          tv => GSI_bundle_new(jj)%values(nts:nte)
          call random_number(u (1:size(GSI_bundle_new(jj)%r3(ipnts(1))%q)))
          call random_number(tv(1:size(GSI_bundle_new(jj)%r3(ipnts(2))%q)))
          print*
          print*, 'modify u and tv in multi-bundle for jj= ',jj
       endif
     endif
     if(jj==3) then
!      ... and tv in 3d window ...
       call GSI_BundleGetPointer ( GSI_bundle_new(jj), 'tv', ipnt, ierr ) 
       if (ierr==0) then
          call random_number(GSI_bundle_new(jj)%r3(ipnt)%q)
          print*
          print*, 'modify tv in multi-bundle for jj= ',jj
       endif
     endif
!    Print result of changes for each instance
     call GSI_BundlePrint   ( GSI_bundle_new(jj) )
enddo

!-------------------------------------------------
! enough now ... let's get rid of the multi-bundle
!-------------------------------------------------
do jj=1,nsub
   call GSI_BundleDestroy ( GSI_bundle_new(jj), ierr )
enddo

end subroutine multi
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Edge --- Test layer/edge bundle
!
! !INTERFACE:
!
subroutine Edge ( GSI_bundle_mix )

! !USES:

use kinds, only: i_kind,r_kind
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_BundlePutVar
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundleMerge
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleDestroy

! these should eventually not belog to the bundle
use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_GridCreate

implicit none

! !INPUT/OUTPUT PARAMETERS:

type(GSI_Bundle) :: GSI_Bundle_mix 

!
! !DESCRIPTION: Test bundles w/ mix of layer- and edge-defined fields 
!               (state-vector-like).
!    \bn
!       \item Input known bundle
!       \item Create new bundle w/ 3d-vectors mix of layer- and level-defined
!       \item Merge known and new bundle into yet another bundle
!       \item Write some fields in the bundle
!       \item Show contents of bundle
!    \en
!
! !REVISION HISTORY:
!
!  22Apr2010 Todling - Initial code
!
!EOP
!-------------------------------------------------------------------------
!BOC

! Pretend we now have 1d-fields
integer(i_kind), parameter :: n3d = 3
character(len=16), parameter :: names3d(n3d) = (/  &
                                'sf             ',    &
                                'pe             ',    &
                                'vt             '    /)
integer(i_kind) :: levels(n3d)

type(GSI_Bundle) :: GSI_Bundle_new 
type(GSI_Bundle) :: GSI_Bundle_all 
type(GSI_Grid)   :: grid
integer(i_kind) lat2, lon2, nlev, ierr
! integer(i_kind) ival,ipnt
! real(r_kind),pointer::pe(:,:,:)
real(r_kind),pointer::vt(:,:,:)

print*, 'TESTING EDGE  ...'
print*, '-----------------'

lat2 = GSI_bundle_mix%grid%im
lon2 = GSI_bundle_mix%grid%jm
nlev = GSI_bundle_mix%grid%km

levels = (/ nlev, nlev+1, nlev /)

call GSI_GridCreate (grid, lat2, lon2, nlev)
!------------------------------------
! create a new 1d bundle and merge it w/ bundle above into yet a 3rd bundle
!------------------------------------
call GSI_BundleCreate ( GSI_bundle_new, grid, 'New 3d Bundle', ierr, &
                                    names3d=names3d, levels=levels )
     call GSI_BundlePrint ( GSI_bundle_new )
     call GSI_BundleGetPointer ( GSI_bundle_new, 'vt', vt, ierr ) 
!    call random_number(GSI_bundle_new%r3(ipnt)%q)
     call random_number(vt)
     print*, 'this is the new bundle w/ 3d fields'
     call GSI_BundlePrint ( GSI_bundle_new )

!--------------------------------------------
! merge two bundle ... grids must be the same
!--------------------------------------------
call GSI_BundleMerge ( GSI_bundle_all, GSI_bundle_mix, GSI_bundle_new, 'Merged All', ierr )
  if(ierr/=0) then
     print*, 'could not merge bundles '
  else
       call mimic_int_routines ( GSI_bundle_all )
!      call GSI_BundleGetPointer ( GSI_bundle_all, 'pe', pe, ierr ) 
!      if (ierr==0) then
!         call random_number(pe)
!     endif

     call GSI_BundlePutVar ( GSI_bundle_all, 'sf', vt, ierr ) 
     if (ierr==0) then
        print*
        print*, 'placed vt in sf slot in new/merged bundle ...'
        call GSI_BundlePrint ( GSI_bundle_all )
     endif
  endif

!-------------------
! destroy all bundle
!-------------------
call GSI_BundleDestroy ( GSI_bundle_all, ierr )
call GSI_BundleDestroy ( GSI_bundle_new, ierr )

end subroutine edge
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  mimic_int_routines --- mimic GSI int routines
!
! !INTERFACE:
!
subroutine mimic_int_routines(Bundle)
use m_rerank, only: rerank
use kinds, only: i_kind,r_kind
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
! !USES:
implicit none
!
! !DESCRIPTION: Mimic GSI int routines.
!
! !REVISION HISTORY:
!
!  12Oct2010 Todling - Add prologue for PROTEX consistency
!
!EOP
!-------------------------------------------------------------------------
!BOC
type(gsi_bundle),intent(inout) :: bundle
!real(r_kind),pointer,dimension(:,:,:)::pe
real(r_kind),pointer,dimension(:)::r1pe
integer(i_kind) ierr
!integer(i_kind) npe,nsz,ival
!    call GSI_BundleGetPointer ( bundle, 'pe', ipe, ierr, ival=ival ) 
     call GSI_BundleGetPointer ( bundle, 'pe', r1pe, ierr )
     if (ierr==0) then
!       nsz=size(bundle%r3(ipe)%q)
!       r1pe => bundle%values(ival:ival+nsz)
        call random_number(r1pe)
     else 
        print*, 'mimic_int_routines: failed'
        call stop2(9)
     endif
end subroutine mimic_int_routines

module control_test
use kinds, only : r_single,r_kind
use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_Bundle
implicit none
private
public control
type control
#ifdef _BUNDLE_R4_
  integer :: mykind = r_single
  real(r_single),pointer :: values(:) => null()
#else /* _BUNDLE_R4_ */
  integer :: mykind = r_kind
  real(r_kind),  pointer :: values(:) => null()
#endif /* _BUNDLE_R4_ */
  type(GSI_Grid)   :: grid_main
  type(GSI_Bundle),pointer :: main(:)
  type(GSI_Grid)   :: grid_ensb
  type(GSI_Bundle),pointer :: ensemble(:,:)
end type control
end module control_test
!EOC

!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Ensemble --- Test control-vector-like bundle w/ ensemble component
!
! !INTERFACE:
!
subroutine Ensemble ( )

! !USES:

use kinds, only: i_kind,r_single,r_kind
use gsi_bundlemod, only : GSI_BundleCreate
use gsi_bundlemod, only : GSI_BundleSet
use gsi_bundlemod, only : GSI_BundleGetPointer
use gsi_bundlemod, only : GSI_BundlePutVar
use gsi_bundlemod, only : GSI_Bundle
use gsi_bundlemod, only : GSI_BundleMerge
use gsi_bundlemod, only : GSI_BundlePrint
use gsi_bundlemod, only : GSI_BundleUnSet
use gsi_bundlemod, only : GSI_BundleDestroy

! these should eventually not belog to the bundle
!use gsi_bundlemod, only : GSI_Grid
use gsi_bundlemod, only : GSI_GridCreate

use control_test, only: control
implicit none

! !INPUT/OUTPUT PARAMETERS:

!
! !DESCRIPTION: Test bundles w/ mix of layer- and edge-defined fields 
!               (state-vector-like).
!    \bn
!       \item Input known bundle
!       \item Create new bundle w/ 3d-vectors mix of layer- and level-defined
!       \item Merge known and new bundle into yet another bundle
!       \item Write some fields in the bundle
!       \item Show contents of bundle
!    \en
!
! !REVISION HISTORY:
!
!  22Apr2010 Todling - Initial code
!  28Apr2011 Todling - Now using ensemble as r_single
!
! !REMARKS:
!
! 1) Notice the cpp directive _BUNDLE_R4_ controls whether to use 
!    and REAL*4 or REAL*8 bundle in this example. The cpp directive 
!    is here only for the purposes of this program: GSI should not
!    need it; this is simply due to how I define the control vector
!    in this example (as a combined entity).
!
!EOP
!-------------------------------------------------------------------------
!BOC

character(len=*), parameter :: myname='ensemble'

! These define the regular control vector
integer(i_kind), parameter :: n3d = 4
character(len=4), parameter :: names3d(n3d) = (/  &
                               'sf  ',    &
                               'vp  ',    &
                               't   ',    &
                               'oz  '/)
integer(i_kind), parameter :: n2d = 2
character(len=4), parameter :: names2d(n2d) = (/  &
                               'ps  ',    &
                               'sst '/)

! Lets pretend we have a simple (im,jm,km)-like (regular) grid
integer(i_kind), parameter :: nsubwin=1
integer(i_kind), parameter :: lat2=91
integer(i_kind), parameter :: lon2=144
integer(i_kind), parameter :: nlev=72

! These define the ensemble components of the control vector
integer(i_kind), parameter :: n3d_ens = 2
character(len=4), parameter :: names3d_ens(n3d_ens) = (/  &
                               'sf  ',    &
                               'vp  '/)
integer(i_kind), parameter :: n2d_ens = 1
character(len=4), parameter :: names2d_ens(n2d_ens) = (/  &
                               'ps  '/)

! Lets pretend the ensembe is set on the following grid
integer(i_kind), parameter :: n_ens=2    ! a 2-member ensemble
integer(i_kind), parameter :: lat2_ens=46
integer(i_kind), parameter :: lon2_ens=72
integer(i_kind), parameter :: nlev_ens=64

integer(i_kind)  ierr
integer(i_kind) nn, nval_reg, nval_ens, nval_len, ii
character(len=80) bname

type(control) :: cv

!---------------------------------------------------
! create control vector with added ensemble component
!---------------------------------------------------

! total dimension of control vector
nval_reg = n3d    *lon2    *lat2    *nlev     + n2d    *lon2    *lat2
nval_ens = n3d_ens*lon2_ens*lat2_ens*nlev_ens + n2d_ens*lon2_ens*lat2_ens
nval_len = nval_reg+nval_ens*n_ens
allocate(cv%values(nval_len))
allocate(cv%main(nsubwin))
allocate(cv%ensemble(nsubwin,n_ens))

print*, 'TESTING ENSM  ...'
print*, '-----------------'

! regular part of the control vector ...
ii=0
#ifdef _BUNDLE_R4_
cv%main(1)%valuesR4 => cv%values(ii+1:ii+nval_reg)
#else /* _BUNDLE_R4_ */
cv%main(1)%values   => cv%values(ii+1:ii+nval_reg)
#endif /* _BUNDLE_R4_ */
call GSI_GridCreate ( cv%grid_main, lat2, lon2, nlev )
call GSI_BundleSet  ( cv%main(1), cv%grid_main, 'Regular Part of Control Vector', ierr, &
                                  names2d=names2d,names3d=names3d, bundle_kind=cv%mykind )
if(ierr<0) then
   print*, trim(myname), ': error, cannot define regular part of CV '
   call exit(7) 
endif

! ensemble part of the control vector ...
call GSI_GridCreate ( cv%grid_ensb, lat2_ens, lon2_ens, nlev_ens )
ii=ii+nval_reg
do nn=1,n_ens
#ifdef _BUNDLE_R4_
   cv%ensemble(1,nn)%valuesR4 => cv%values(ii+1:ii+nval_ens)
#else /* _BUNDLE_R4_ */
   cv%ensemble(1,nn)%values   => cv%values(ii+1:ii+nval_ens)
#endif /* _BUNDLE_R4_ */
   write(bname,'(a,i3.3)') 'Ensemble Part of Control Vector Member-',nn
   call GSI_BundleSet  ( cv%ensemble(1,nn), cv%grid_ensb, trim(bname), ierr, &
                                            names2d=names2d_ens,names3d=names3d_ens, &
                                            bundle_kind=cv%mykind )
   if(ierr<0) then
      print*, trim(myname), ': error, cannot member ', nn
      call exit(7) 
   endif
   ii=ii+nval_ens
enddo
! Fill in ensemble-sf w/ something
call mimic_gsi ( cv%ensemble(1,:), n_ens)

! Echo control contents
!call GSI_BundlePrint ( cv%main(1) )
!do nn=1,n_ens
!   call GSI_BundlePrint ( cv%ensemble(1,nn) )
!enddo

!-------------------
! destroy all bundle
!-------------------
do nn=n_ens,1,-1
   call GSI_BundleUnset ( cv%ensemble(1,nn), ierr )
enddo
call GSI_BundleUnset ( cv%main(1), ierr )
deallocate(cv%ensemble)
deallocate(cv%main)
deallocate(cv%values)

end subroutine ensemble
subroutine mimic_gsi ( bundle, n )
use kinds, only: i_kind,r_kind,r_single
use gsi_bundlemod, only: GSI_Bundle
use gsi_bundlemod, only: GSI_BundleGetPointer
use gsi_bundlemod, only: GSI_BundlePrint
implicit none
integer(i_kind),intent(in) :: n
type(gsi_bundle) :: bundle(n)
character(len=*), parameter :: myname='mimic_gsi'
integer(i_kind) nn,ierr
real(r_single),pointer,dimension(:,:,:)::ptr4
real(r_kind),pointer,dimension(:,:,:)::ptr8
if(bundle(1)%AllKinds==r_kind) then
  call GSI_BundleGetPointer ( bundle(1), 'sf', ptr8, ierr ) 
  if(ierr/=0) then
     print*, trim(myname), ': error, cannot find pointer to sf'
     call exit(7)
  endif
  call random_number(ptr8)
else if(bundle(1)%AllKinds==r_single) then
  call GSI_BundleGetPointer ( bundle(1), 'sf', ptr4, ierr ) 
  if(ierr/=0) then
     print*, trim(myname), ': error, cannot find pointer to sf'
     call exit(7)
  endif
  call random_number(ptr4)
else
  print*, trim(myname), ': error, undef kind for bundle'
  call exit(9)
endif
if (n>1) then
   if(bundle(2)%AllKinds==r_kind) then
      call GSI_BundleGetPointer ( bundle(2), 'vp', ptr8, ierr ) 
      call random_number(ptr8)
      if(ierr/=0) then
         print*, trim(myname), ': error, cannot find pointer to vp'
         call exit(7)
      endif
   else if(bundle(2)%AllKinds==r_single) then
      call GSI_BundleGetPointer ( bundle(2), 'vp', ptr4, ierr ) 
      if(ierr/=0) then
         print*, trim(myname), ': error, cannot find pointer to vp'
         call exit(7)
      endif
      call random_number(ptr4)
   else
      print*, trim(myname), ': error, undef kind for bundle'
      call exit(9)
   endif
endif
do nn=1,n
   call GSI_BundlePrint ( bundle(nn) )
enddo
end subroutine mimic_gsi
!EOC
