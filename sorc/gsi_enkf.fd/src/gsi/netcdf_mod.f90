module netcdf_mod
!<------------------------------------------------------------------->
!<---- next few lines under version control, D O  N O T  E D I T ---->
! $Date$
! $Revision$
! $Author$
! $Id$
!<------------------------------------------------------------------->
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module netcdf_mod
!   prgmmr:      rmahajan, rahul.mahajan@noaa.gov
!      org:      NCEP/EMC
!     date:      2016-05-31
!
! abstract: a module for netCDF interface 
!
! program history log:
!   2015-05-31  mahajan - initial version
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! module interface:

   use netcdf, only: nf90_noerr
   use netcdf, only: nf90_strerror

   implicit none

   private

   public :: nc_check

  character(len=*) , parameter:: myname='netcdf_mod'

contains

SUBROUTINE nc_check(ierr,subr_name,context,stat)

!  Trap for netcdf errors
!  INPUT: 
!       ierr - netcdf error return code
!  subr_name - subroutine name that made the netcdf call
!    context - what was the context of the call
! OUTPUT: 
!       stat - Return ierr and do not fatally fail, just warn

   use mpeu_util, only: die,perr,warn

   implicit none

   integer,         intent(in ) :: ierr
   character(len=*),intent(in ) :: subr_name, context
   integer,optional,intent(out) :: stat

   if ( ierr /= nf90_noerr ) then
      if ( present(stat) ) then
         call warn(subr_name,'ignored, '//trim(context),trim(nf90_strerror(ierr)))
         stat = ierr
      else
         call perr(subr_name,trim(context),trim(nf90_strerror(ierr)))
         call die(subr_name)
      endif
   endif

   return
END SUBROUTINE nc_check

end module netcdf_mod
