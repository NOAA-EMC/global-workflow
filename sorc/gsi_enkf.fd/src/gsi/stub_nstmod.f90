!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_NSTCouplerMod ---
!
! !DESCRIPTION: This stub provides the default interfaces to the 
!               NST analysis calculations in GSI.
!
! !REVISION HISTORY:
!
!  07Oct2011 Akella/RT - Initial code
!  05Mar2012 Akella    - Create_nst and getnst from satthin are now nst_int_ & nst_set_
!                        Destroy_nst from satthin is nst_final_
!  11Aug2016 Mahajan   - Keep stub as such.
!  31Aug2017 Li        - change nst_set to be nst_read
!
!EOP
!-------------------------------------------------------------------------

subroutine nst_init_()

    use mpimod, only: mype

    implicit none

    if ( mype == 0 ) &
        write(6,*)'Doing nothing in dummy routine NST_INIT_'

    return

end subroutine nst_init_
!*******************************************************************************************

subroutine nst_read_(mype_io)

    use mpimod, only: mype
    use kinds, only: i_kind

    implicit none

    integer(i_kind),intent(in) :: mype_io

    if ( mype == 0 ) &
        write(6,*) 'Doing nothing in dummy routine NST_READ_'

    return

end subroutine nst_read_
!*******************************************************************************************

subroutine nst_final_()

    use mpimod, only: mype

    implicit none

    if ( mype == 0 ) &
        write(6,*) 'Doing nothing in dummy routine NST_FINAL_'

    return

end subroutine nst_final_
!*******************************************************************************************

subroutine deter_nst_(dlat_earth,dlon_earth,obstime,zob,tref,dtw,dtc,tz_tr)

    use mpimod, only: mype
    use kinds,     only: r_kind
    use constants, only: zero

    implicit none

    real(r_kind), intent(in   ) :: dlat_earth,dlon_earth,obstime,zob
    real(r_kind), intent(  out) :: tref,dtw,dtc,tz_tr

    if ( mype == 0 ) &
       write(6,*) 'Doing nothing in dummy routine DETER_NST_'

    tref = zero
    dtw = zero
    dtc = zero
    tz_tr = zero

    return

end subroutine deter_nst_
!*******************************************************************************************

subroutine cal_tztr_(dt_warm,c_0,c_d,w_0,w_d,zc,zw,z,tztr)

    use mpimod, only: mype
    use kinds, only: r_kind
    use constants, only: zero
  
    implicit none
  
    real(kind=r_kind), intent(in   ) :: dt_warm,c_0,c_d,w_0,w_d,zc,zw,z
    real(kind=r_kind), intent(  out) :: tztr
  
    if ( mype == 0 ) &
        write(6,*) 'Doing nothing in dummy routine CAL_TZTR_'
  
    tztr = zero
  
    return

end subroutine cal_tztr_
!*******************************************************************************************

!*******************************************************************************************
subroutine skindepth_(obstype,sd_rad)

    use mpimod, only: mype
    use kinds, only: r_kind
   
    implicit none
   
    character(len=10), intent(in   )  :: obstype
    real(kind=r_kind), intent(  out) :: sd_rad
   
    if ( mype == 0 ) &
        write(6,*) 'Doing nothing in dummy routine SKINDEPTH_'
  
    sd_rad = 0.000015_r_kind
   
    return

end subroutine skindepth_
!*******************************************************************************************
