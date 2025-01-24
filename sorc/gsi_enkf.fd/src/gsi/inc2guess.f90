subroutine inc2guess(sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inc2guess          replaces background with increment
!   prgmmr: todling          org: np22                date: 2007-07-05
!
! abstract:  This routine replaces the background fields with the
!            increments. Its main purpose is to be used in the 4d-var
!            case, though it could be considered for the 3d-var case 
!            as well.
!
!            As it is, this routine assumes a call to update_guess 
!            preceeds this and changes sval appropriately, including
!            change the scales of ozone and calculating vor and div.
!
! program history log:
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-01  todling - only upd when pointer defined
!   2010-06-15  todling - generalize handling of chemistry
!   2010-05-01  todling - add support for generalized guess (use met-guess)
!                       - cwmr now in met-guess
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2013-10-19  todling - all guess variables in met-guess
!
!   input argument list:
!     sval     - analysis increment in grid space
!
!   output argument list:
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: nfldsig,hrdifsig,nfldsfc,sfct
  use state_vectors, only: svars3d,svars2d
  use gsi_4dvar, only: nobs_bins, hr_obsbin
  use xhat_vordivmod, only: xhat_vor,xhat_div
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_metguess_mod, only: gsi_metguess_get
  use gsi_chemguess_mod, only: gsi_chemguess_bundle
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use mpeu_util, only: getindex

  implicit none

! Declare passed variables
  type(gsi_bundle), intent(in   ) :: sval(nobs_bins)

! Declare local variables
  character(len=10),allocatable,dimension(:) :: gases
  character(len=10),allocatable,dimension(:) :: guess
  integer(i_kind) i,j,k,it,ii,ic,id,ngases,nguess,ier,istatus
  real(r_kind) :: zt
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dinc=>NULL()
  real(r_kind),pointer,dimension(:,:  ) :: ptr2dges=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dinc=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ptr3dges=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_div_it=>NULL()
  real(r_kind),pointer,dimension(:,:,:) :: ges_vor_it=>NULL()

! Inquire about guess fields
call gsi_metguess_get('dim',nguess,istatus)
if (nguess>0) then  
    allocate(guess(nguess))
    call gsi_metguess_get('gsinames',guess,istatus)
endif

! Inquire about chemistry fields
call gsi_chemguess_get('dim',ngases,istatus)
if (ngases>0) then  
    allocate(gases(ngases))
    call gsi_chemguess_get('gsinames',gases,istatus)
endif

!*******************************************************************************

! Overwrite guess fields by increments
  do it=1,nfldsig
     if (nobs_bins>1) then
        zt = hrdifsig(it)
        ii = NINT(zt/hr_obsbin)+1
     else
        ii = 1
     endif
     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus)
     ier=ier+istatus
     if (ier==0) then
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 ges_div_it(i,j,k)  = xhat_div(i,j,k,ii)
                 ges_vor_it(i,j,k)  = xhat_vor(i,j,k,ii)
              end do
           end do
        end do
     endif

!    Update met-guess
     do ic=1,nguess
        id=getindex(svars3d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr3dges,istatus)
           if (trim(guess(ic))=='oz'.or.trim(guess(ic))=='q') then
               call copy_positive_fldr3_(ptr3dges,ptr3dinc)
           else
              ptr3dges = ptr3dinc
           endif
        endif
        id=getindex(svars2d,guess(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),               guess(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),guess(ic),ptr2dges,istatus)
           ptr2dges = ptr2dinc
        endif
     enddo

!    Update trace gases
     do ic=1,ngases
        id=getindex(svars3d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr3dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr3dges,istatus)
           ptr3dges=ptr3dinc
!          call copy_positive_fldr3_(ptr3dges,ptr3dinc)
        endif
        id=getindex(svars2d,gases(ic))
        if (id>0) then
           call gsi_bundlegetpointer (sval(ii),                gases(ic),ptr2dinc,istatus)
           call gsi_bundlegetpointer (gsi_chemguess_bundle(it),gases(ic),ptr2dges,istatus)
           ptr2dges=ptr2dinc
!          call copy_positive_fldr2_(ptr2dges,ptr2dinc)
        endif
     enddo
  end do

  if(ngases>0)then
    deallocate(gases)
  endif

  call gsi_bundlegetpointer (sval(ii),'sst',ptr2dinc,istatus)
  if(istatus==0)then
     do k=1,nfldsfc
        do j=1,lon2
           do i=1,lat2
              sfct(i,j,k)= ptr2dinc(i,j)
           end do
        end do
     end do
  end if
  if(mype==0) write(6,*) 'inc2guess: overwriting guess with increment'

  return
  contains
  subroutine copy_positive_fldr2_(ges,xinc)
  real(r_kind),pointer :: ges(:,:)
  real(r_kind),pointer :: xinc(:,:)
  real(r_kind) ana
  do j=1,lon2
     do i=1,lat2
        ana = max(ges(i,j)+ xinc(i,j),1.e-10_r_kind)
        ges(i,j) = ana - ges(i,j)
     end do
  end do
  end subroutine copy_positive_fldr2_
  subroutine copy_positive_fldr3_(ges,xinc)
  real(r_kind),pointer :: ges(:,:,:)
  real(r_kind),pointer :: xinc(:,:,:)
  real(r_kind) ana
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           ana = max(ges(i,j,k)+ xinc(i,j,k),1.e-10_r_kind)
           ges(i,j,k) = ana - ges(i,j,k)
        end do
     end do
  end do
  end subroutine copy_positive_fldr3_
end subroutine inc2guess
