 module search_util

!--------------------------------------------------------------------------
! Module search
!
! Abstract: Replace undefined values with a valid value.  This can
!   happen for an isolated lake or island that is unresolved by
!   the input grid.
!
! Public Subroutines:
! -------------------
! search                           Performs the search and replace.
!
!--------------------------------------------------------------------------

 private

 public :: search

 contains

 subroutine search (field, mask, idim, jdim, tile, field_num, latitude)

!-----------------------------------------------------------------------
! Replace undefined values on the model grid with a valid value at
! a nearby neighbor.  Undefined values are typically associated
! with isolated islands where there is no source data.
!
! Routine searches a neighborhood with a radius of 100 grid points.
! If no valid value is found, a default value is used.
!
! Note: This routine works for one tile of a cubed sphere grid.  It
! does not consider valid values at adjacent faces.  That is a 
! future upgrade.
!-----------------------------------------------------------------------

 use esmf

 implicit none

 include 'mpif.h'

 integer, intent(in)               :: idim, jdim, tile, field_num
 integer(esmf_kind_i8), intent(in) :: mask(idim,jdim)

 real(esmf_kind_r8), intent(in), optional :: latitude(idim,jdim)

 real(esmf_kind_r8), intent(inout) :: field(idim,jdim)

 integer                           :: i, j, krad, ii, jj
 integer                           :: istart, iend
 integer                           :: jstart, jend
 integer                           :: ierr

 real                              :: default_value
 real(esmf_kind_r8)                :: field_save(idim,jdim)

!-----------------------------------------------------------------------
! Set default value.
!-----------------------------------------------------------------------

 select case (field_num)
   case (0) ! most nst fields
     default_value = 0.0
   case (1) ! ifd
     default_value = 1.0
   case (7) ! terrain height, flag value to turn off terrain adjustment
            ! of soil temperatures.
     default_value = -99999.9
   case (11) ! water temperature will use latitude-dependent value
     default_value = -999.0
   case (21) ! ice temperature
     default_value = 265.0
   case (30) ! xz
     default_value = 30.0
   case (65) ! snow liq equivalent
     default_value = 0.0
   case (66) ! snow depth
     default_value = 0.0
   case (83) ! z0 (cm)
     default_value = 0.01
   case (85) ! soil temp
     default_value = 280.0
   case (86) ! soil moisture (volumetric)
     default_value = 0.18
   case (91) ! sea ice fraction
     default_value = 0.5
   case (92) ! sea ice depth (meters)
     default_value = 1.0
   case (223) ! canopy moist
     default_value = 0.0
   case (224) ! soil type, flag value to turn off soil moisture rescaling.
     default_value = -99999.9
   case default
     print*,'- FATAL ERROR.  UNIDENTIFIED FIELD NUMBER : ', field
     call mpi_abort(mpi_comm_world, 77, ierr)
 end select

!-----------------------------------------------------------------------
! Perform search and replace.
!-----------------------------------------------------------------------

 field_save = field

!$OMP PARALLEL DO DEFAULT(NONE), &
!$OMP SHARED(IDIM,JDIM,MASK,FIELD_SAVE,FIELD,TILE,LATITUDE,DEFAULT_VALUE,FIELD_NUM), &
!$OMP PRIVATE(I,J,KRAD,ISTART,IEND,JSTART,JEND,II,JJ)

 J_LOOP : do j = 1, jdim
   I_LOOP : do i = 1, idim

     if (mask(i,j) == 1 .and. field_save(i,j) < -9999.0) then

       KRAD_LOOP : do krad = 1, 100

         istart = i - krad
         iend   = i + krad
         jstart = j - krad
         jend   = j + krad

         JJ_LOOP : do jj = jstart, jend
         II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!          Search only along outer square.
!-----------------------------------------------------------------------

           if ((jj == jstart) .or. (jj == jend) .or.   &
               (ii == istart) .or. (ii == iend)) then

             if (jj < 1 .or. jj > jdim) cycle JJ_LOOP
             if (ii < 1 .or. ii > idim) cycle II_LOOP

               if (mask(ii,jj) == 1  .and. field_save(ii,jj) > -9999.0) then
                 field(i,j) = field_save(ii,jj)
!                write(6,100) tile,i,j,ii,jj,field(i,j)
                 cycle I_LOOP
               endif

           endif

         enddo II_LOOP
         enddo JJ_LOOP

       enddo KRAD_LOOP

       if (field_num == 11) then
         call sst_guess(latitude(i,j), field(i,j))
       elseif (field_num == 91) then  ! sea ice fract
         if (abs(latitude(i,j)) > 55.0) then
           field(i,j) = default_value
         else
           field(i,j) = 0.0
         endif
       else
         field(i,j) = default_value  ! Search failed.  Use default value.
       endif

       write(6,101) tile,i,j,field(i,j)

     endif
   enddo I_LOOP
 enddo J_LOOP
!$OMP END PARALLEL DO

 100 format(1x,"- MISSING POINT TILE: ",i2," I/J: ",i5,i5," SET TO VALUE AT: ",i5,i5,". NEW VALUE IS: ",f8.3)
 101 format(1x,"- MISSING POINT TILE: ",i2," I/J: ",i5,i5," SET TO DEFAULT VALUE OF: ",f8.3)

 end subroutine search

 subroutine sst_guess(latitude, sst)

 use esmf

 implicit none

 real(esmf_kind_r8), intent(in)  :: latitude

 real(esmf_kind_r8), intent(out) :: sst

 if (latitude >= 60.0) then
   sst = 273.16
 elseif (abs(latitude) <= 30.0) then
   sst = 300.0
 else
   sst = (-0.8947) * abs(latitude) + 326.84
 endif

 end subroutine sst_guess

 end module search_util
