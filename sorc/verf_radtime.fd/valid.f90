!$$$ subprogram documentation block
!               .      .    .
! subprogram:	valid			validate the obs and penalty values
!     prgmmr:   safford			date:  2009-12
!
! abstract:	This module contains code to read a given satellite's 
!               base file and then validate new obs(count) and penalty
!               values by comparing them to the baseline values.  
!
! program history log:
!	2009-12-07 safford - initial coding
!
! contains:
!
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

module valid

  implicit none
  
  private

! --- module routines
  public :: load_base
  public :: validate_count
  public :: validate_penalty 

! --- module parameters
  integer, parameter  :: funit = 17
  real,parameter :: rmiss      = -999.0

! --- module vars
  logical :: base_loaded = .FALSE.
  integer :: nchan, nregion, j, k, dummy

  real,allocatable,dimension(:,:)::  avg_count, sdv_count 
  real,allocatable,dimension(:,:)::  min_count, max_count 
  real,allocatable,dimension(:,:)::  avg_penalty, sdv_penalty 
  real,allocatable,dimension(:,:)::  min_penalty, max_penalty 

  integer,allocatable,dimension(:):: nu_chan
 
  contains

    !-------------------------------------------------------------
    !  load the base file for the given satellite
    !-------------------------------------------------------------

    subroutine load_base( satname, iret )

      !--- interface
      character(20), intent( in )	:: satname
      integer, intent( out )		:: iret     

      !--- variables
      character(20) fname
      character(20) test_satname
      character(10) base_date

      integer fios
      integer chan, region

      logical fexist 


      !--- initialization
      iret   = -1
      fname  = trim(satname) // '.base'
      fexist = .FALSE.

      write(*,*) '--> valid, satname, fname = ', satname, fname

      !--- verify file exists and open the file
      inquire( file = fname, exist = fexist )
      if( fexist .eqv. .FALSE. ) then
         fios = -1 
      else 
         open( UNIT=funit, FILE=fname, IOSTAT=fios )
         write(*,*) ' fios = ', fios
      end if

      if( fios == 0 ) then
         !--- read the file header
         read(funit,*) test_satname, base_date, nchan, nregion
         write(*,*) ' test_satname, base_date, nchan, nregion = ', test_satname, base_date, nchan, nregion
        
         allocate( avg_count(nchan,nregion), sdv_count(nchan,nregion), &
              avg_penalty(nchan,nregion), sdv_penalty(nchan,nregion), &
              min_count(nchan,nregion), max_count(nchan,nregion), &
              min_penalty(nchan,nregion), max_penalty(nchan,nregion) )

         ! --- set all missing
         do k=1,nregion
            do j=1,nchan
               avg_count(j,k) = rmiss
               sdv_count(j,k) = rmiss
               min_count(j,k) = rmiss
               max_count(j,k) = rmiss
               avg_penalty(j,k) = rmiss
               sdv_penalty(j,k) = rmiss
               min_penalty(j,k) = rmiss
               max_penalty(j,k) = rmiss
            end do
         end do

         do k=1,nregion
            do j=1,nchan
               read(funit,*) chan, region, avg_count(j,k), sdv_count(j,k), min_count(j,k), max_count(j,k), &
                             avg_penalty(j,k), sdv_penalty(j,k), min_penalty(j,k), max_penalty(j,k)
            end do
         end do

         iret = 0 
         base_loaded = .TRUE.
      else
         write(*,*) 'unable to load fname for data error checking'
      end if

    end subroutine load_base


    !---------------------------------------------------------------
    !  validate a count
    !     given a count value for a channel and region, determine
    !     if the count is within +/- 2*sdv 
    !
    !     iret         0 = normal
    !                 -1 = invalid channel
    !                 -2 = invalid region
    !                  1 = base file wasn't loaded, unable to validate
    !---------------------------------------------------------------
    subroutine validate_count( channel, region, count, valid, iret )

      !--- interface
      integer, intent( in )		:: channel
      integer, intent( in )		:: region
      real, intent( in )                :: count
      logical, intent( out )            :: valid
      integer, intent( out )		:: iret

      !--- vars
      real cnt, hi, lo, sdv2

      write(*,*) '--> validate_count, channel, region, count ', channel, region, count
      !--- initialize vars
      iret = 0 
      cnt = count
      valid = .FALSE.

      if( base_loaded .eqv. .TRUE. ) then
         if( channel < 1 .OR. channel > nchan ) then
            iret = -1
            write(*,*) 'Warning:  In validate_count attempt to validate channel out of range', channel
            valid = .TRUE.
         else if( region < 1 .OR. region > nregion ) then
            iret = -2
            write(*,*) 'Warnig:  In validate_count attempt to validate region out of range', region
            valid = .TRUE.
         else
            ! 
            !  all unassimilated channels in the base files will have an rmiss
            !  value and are considered valid for verification purposes
            !
            if( avg_count(channel,region) < 0.0 ) then
               valid = .TRUE.
            else
               sdv2 = 2 * sdv_count( channel, region )
               hi = avg_count(channel,region) + sdv2
               lo = avg_count(channel,region) - sdv2

               !
               !  Consider any count valid if:
               !    cnt is 2 sdvs from avg or
               !    cnt is within the established min/max range for chan,region
               !
               if( cnt > 0.0 ) then
                  valid = .TRUE.
               end if 
               !if( cnt <= hi .AND. cnt >= lo ) then
               !   valid = .TRUE.
               !else if( (cnt > 0) .AND. (cnt >= min_count( channel,region )) .AND. &
               !    (cnt <= max_count( channel,region ))  ) then
               !    valid = .TRUE.
               !end if
            end if

         end if

         if ( valid .eqv. .FALSE. ) then
            write(*,*) ' avg_count(channel,region), sdv2, hi, lo = ', avg_count(channel,region), sdv2, hi, lo
         end if
         write (*,*) '<-- valid, iret=', valid, iret
      else 
         !--- base file was not loaded, so return a warning that validation isn't possible
         iret = 1 
      end if 
    end subroutine validate_count


    !-------------------------------------------------------------
    !  validate a penalty value
    !     given a penalty value for a channel and region, determine
    !     if the penalty is within +/- 2*sdv 
    !
    !     iret         0 = normal
    !                 -1 = invalid channel
    !                 -2 = invalid region
    !-------------------------------------------------------------
    subroutine validate_penalty( channel, region, penalty, valid, bound, iret )

      !--- interface
      integer, intent( in )		:: channel
      integer, intent( in )		:: region
      real, intent( in )                :: penalty
      logical, intent( out )            :: valid
      real, intent( out )               :: bound
      integer, intent( out )		:: iret

      !--- vars
      real hi, lo, sdv2

      write(*,*) '--> validate_penalty, channel, region, penalty ', channel, region, penalty

      !--- initialize vars
      iret = 0 
      valid = .FALSE.
      bound = rmiss

      if( base_loaded .eqv. .TRUE. .AND. nchan > 1 ) then
         if( channel < 1 .OR. channel > nchan ) then
            iret = -1
            write(*,*) 'Warning:  In validate_penalty attempt to validate channel out of range', channel
            valid = .TRUE.
         else if( region < 1 .OR. region > nregion ) then
            iret = -2
            write(*,*) 'Warning:  In validate_penalty attempt to validate region out of range', region
            valid = .TRUE.
         else
            !
            !  all unassimilated channels in the base files will have an rmiss
            !  value and are considered valid for verification purposes
            !
            bound = max_penalty( channel,region )

            if( avg_penalty(channel,region) < 0.0 ) then
               valid = .TRUE.
            else
               sdv2 = 2 * sdv_penalty( channel, region )
               hi = avg_penalty(channel,region) + sdv2
               lo = avg_penalty(channel,region) - sdv2

               !
               !  Consider any penalty value valid if:
               !    penalty is 2 sdvs from avg or
               !    penalty is greater than the established max range for 
               !      chan,region by 20% or more --> idea here is to stage
               !      two levels of warning, say a 20% exceedence and then
               !      maybe a 25% or 30% exceedence for a yellow and red
               !      level warning.
               !
               if( (penalty >= 0.0) .AND. penalty <= hi ) then
                  valid = .TRUE.
               else if( (penalty > 0.0) .AND. &
                   (penalty <= (max_penalty( channel,region )*1.2) )  ) then
                  valid = .TRUE.
               end if
  
            end if
         end if

         if ( valid .eqv. .FALSE. ) then
            write(*,*) ' BAD:  penalty, avg_penalty(channel,region), sdv2, hi, lo, bound = ', penalty, avg_penalty(channel,region), sdv2, hi, lo, bound
         end if
         write (*,*) '<-- valid, iret=', valid, iret
      else 
         !--- base file was not loaded, or nchan was 0 so return 
         !--- a warning that validation isn't possible
         write (*,*) 'Warning:  base file not loaded or nchan < 1, nchan= ', nchan
         iret = 1 
      end if 
    end subroutine validate_penalty

    
end module valid
