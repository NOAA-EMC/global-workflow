module smooth_mod

use mpisetup
use params, only:  nlons,nlats,smoothparm
use controlvec, only: ncdim
use kinds, only: r_kind
use gridinfo, only: npts

implicit none

private
public :: smooth

contains

subroutine smooth(grids)
real(r_single), intent(inout) :: grids(npts,ncdim) ! there are ndim 2d grids.
! stub - not yet implemented.
if (nproc .eq. 0) print *,'FV3reg inflation smoothing not yet implemented!,stop'
call stop2(544)
end subroutine smooth

end module smooth_mod
