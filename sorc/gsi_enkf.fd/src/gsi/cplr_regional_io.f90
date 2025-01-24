!$$$   module documentation block
!                .      .    .                                       .
! module:  regional_io
! prgmmr:  treadon           org: np23                date: 2004-12-29
!
! abstract: This module contains routines that handle the input/output
!           of regional gsi guess(analysis) grids
!
! program history log:
!   2004-12-29  treadon
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-07-06  parrish - add variable update_pint
!   2005-10-17  parrish - add ctph0,stph0,tlm0 
!   2010-09-15  pagowski - add cmaq
!   2012-02-16  parrish - if use_gfs_stratosphere true, then broadcast extra parameters to all pes from pe 0.
!   2013-02-25  zhu - add cold_start option
!   2014-12-22  Hu      -  add option i_gsdcldanal_type to control cloud analysis       
!   2016-06-10  mahajan -  moved variables eg. update_pint, etc. to regional_param_mod
!   
! Subroutines Included:
!   sub convert_regional_guess  - convert regional guess to internal format
!   sub write_regional_analysis - write regional analysis
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
module regional_io_mod
use abstract_regional_io_mod
  type, extends(abstract_regional_io_class) :: regional_io_class
  contains
      procedure, pass(this) :: init_regional_io => init_regional_io_wrf
      procedure, pass(this) :: write_regional_analysis => write_regional_analysis_wrf 
      procedure, pass(this) :: convert_regional_guess => convert_regional_guess_wrf
  end type regional_io_class

contains

  subroutine init_regional_io_wrf(this)
     use wrf_params_mod, only: init_wrf_params
     implicit none
     class(regional_io_class), intent(inout) :: this
     associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
     end associate
     call init_wrf_params(.false.,.false.,.true.)
     return
  end subroutine init_regional_io_wrf

  subroutine convert_regional_guess_wrf(this,mype,ctph0,stph0,tlm0)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_regional_guess
  !     prgmmr:    treadon     org: np23                date: 2004-12-29
  !
  ! abstract:  converts wrf input guess file to internal gsi format
  !
  ! program history log:
  !   2004-12-29  treadon
  !   2005-05-24  pondeca - add 2dvar only surface analysis option
  !   2005-07-06  parrish - add variable update_pint
  !   2012-10-11  parrish - add byte_swap, which is set only on pe 0 and must be broadcast to all pes.
  !
  !   input argument list:
  !      mype - mpi task id
  !
  !   output argument list:
  !      ctph0,stph0,tlm0
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$ end documentation block
  
      use kinds, only: i_kind,r_kind
      use mpimod, only: mpi_integer4,mpi_rtype
      use gridmod, only: wrf_mass_regional,wrf_nmm_regional,&
         nems_nmmb_regional,cmaq_regional,&
         twodvar_regional,netcdf
      use hybrid_ensemble_parameters, only: l_hyb_ens,regional_ensemble_option
      use native_endianness, only: byte_swap
      use convert_netcdf_mod, only: convert_netcdf_class
      use get_wrf_binary_interface_mod, only: get_wrf_binary_interface_class
      use get_wrf_nmm_ensperts_mod, only: get_wrf_nmm_ensperts_class
      use mpimod, only: mpi_comm_world,ierror
      use wrf_params_mod, only: update_pint,cold_start
      use gsi_io, only: verbose

      implicit none
  
  !   Declare passed variables
      class(regional_io_class), intent(inout) :: this
      integer(i_kind),intent(in   ) :: mype
      real(r_kind)   ,intent(  out) :: ctph0,stph0,tlm0
  
      type(convert_netcdf_class) :: netcdf_converter
      type(get_wrf_nmm_ensperts_class) :: binary_nmm
      type(get_wrf_binary_interface_class) :: wrf_interface
      logical print_verbose
  
  !   Convert nmm guess file to internal gsi format.  Consider
  !   two possible input formats:  netcdf or binary
      associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
      end associate
  
      print_verbose=.false.
      if(verbose)print_verbose=.true.
      update_pint=.false.
      cold_start=.true.
      if (wrf_nmm_regional) then
         if (mype==0) then
            if (netcdf) then
               if (l_hyb_ens .and. regional_ensemble_option == 2)then
                  call netcdf_converter%convert_netcdf_nmm(update_pint,ctph0,stph0,tlm0,.false.)
               end if
               call netcdf_converter%convert_netcdf_nmm(update_pint,ctph0,stph0,tlm0,.true.)
            else
               call wrf_interface%convert_binary_nmm(update_pint,ctph0,stph0,tlm0)
               if (l_hyb_ens .and. regional_ensemble_option == 2)then
                  call binary_nmm%convert_binary_nmm_ens
               end if
            end if
         end if
         call mpi_barrier(mpi_comm_world,ierror)
         call mpi_bcast(update_pint,1,mpi_integer4,0,mpi_comm_world,ierror)
         call mpi_bcast(ctph0,1,mpi_rtype,0,mpi_comm_world,ierror)
         call mpi_bcast(stph0,1,mpi_rtype,0,mpi_comm_world,ierror)
         call mpi_bcast(tlm0,1,mpi_rtype,0,mpi_comm_world,ierror)
         call mpi_bcast(byte_swap,1,mpi_integer4,0,mpi_comm_world,ierror)
         if(mype == 0 .and. print_verbose)write(6,*)' in convert_regional_guess, for wrf nmm binary input, byte_swap=',byte_swap
  
  !   Convert mass guess file to internal gsi format.  Consider
  !   two possible input formats:  netcdf or binary
  
      elseif (wrf_mass_regional) then
         if (mype==0) then
            if (netcdf) then
               call netcdf_converter%convert_netcdf_mass
            else
               call wrf_interface%convert_binary_mass
            end if
         end if
         call mpi_barrier(mpi_comm_world,ierror)
         call mpi_bcast(byte_swap,1,mpi_integer4,0,mpi_comm_world,ierror)
         if(print_verbose)write(6,*)' in convert_regional_guess, for wrf arw binary input, byte_swap=',byte_swap
  
      elseif (cmaq_regional) then
         if (mype==0) then
  !cmaq binary is read in directly, only need to link file to sigf
            call make_sigf
         end if
         
         call mpi_barrier(mpi_comm_world,ierror)
         
  !   Convert nems nmmb guess file to internal gsi format.
  
      elseif (nems_nmmb_regional) then
         if (mype==0) then
            call wrf_interface%convert_nems_nmmb(update_pint,ctph0,stph0,tlm0)
         end if
         call mpi_barrier(mpi_comm_world,ierror)
         call mpi_bcast(update_pint,1,mpi_integer4,0,mpi_comm_world,ierror)
         call mpi_bcast(ctph0,1,mpi_rtype,0,mpi_comm_world,ierror)
         call mpi_bcast(stph0,1,mpi_rtype,0,mpi_comm_world,ierror)
         call mpi_bcast(tlm0,1,mpi_rtype,0,mpi_comm_world,ierror)
  
  !   Convert binary twodvar guess file to internal gsi format.
  
      elseif (twodvar_regional) then
         if (mype==0) then
            call convert_binary_2d
         end if
         call mpi_barrier(mpi_comm_world,ierror)
      end if
  
      return
  end subroutine convert_regional_guess_wrf


  subroutine write_regional_analysis_wrf(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    write_regional_analysis
  !     prgmmr:    treadon     org:  np23               date: 2004-12-29
  !
  ! abstract:  write regional analysis grid to output file
  !
  ! program history log:
  !   2004-12-29  treadon
  !   2005-05-24  pondeca - add 2dvar only surface analysis option
  !
  !   input argument list:
  !      mype - mpi task id
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$ end documentation block
  
      use kinds, only: i_kind
      use wrwrfmassa_mod, only: wrwrfmassa_class
      use wrwrfnmma_mod, only: wrwrfnmma_class
      use convert_netcdf_mod, only: convert_netcdf_class
      use gridmod, only: wrf_mass_regional,wrf_nmm_regional,&
         nems_nmmb_regional,cmaq_regional,&
         twodvar_regional,netcdf
      use mpimod, only: mpi_comm_world,ierror
      use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type
      implicit none
  
  !   Declare passed variables
      class(regional_io_class), intent(inout) :: this
      integer(i_kind),intent(in):: mype

  !   Declare local variables
      type(wrwrfmassa_class) :: wrwrfmassa
      type(wrwrfnmma_class) :: wrwrfnmma
      type(convert_netcdf_class) :: netcdf_converter
  
      associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
      end associate
  !   Write nmm analysis file.  Consider two possible
  !   output formats:  netcdf or binary
      if (wrf_nmm_regional) then
         if (netcdf) then
            call wrwrfnmma%wrwrfnmma_netcdf(mype)
            if (mype==0) then
               call netcdf_converter%update_netcdf_nmm
            end if
            call mpi_barrier(mpi_comm_world,ierror)
         else
            call wrwrfnmma%wrwrfnmma_binary(mype)
         end if
      end if
  
  !   Write mass analysis file.  Consider two possible
  !   output formats:  netcdf or binary
      if (wrf_mass_regional) then
         if(netcdf) then
            call wrwrfmassa%wrwrfmassa_netcdf(mype)
            if (mype==0 .and. i_gsdcldanal_type /=5) then
               call netcdf_converter%update_netcdf_mass
            endif
            call mpi_barrier(mpi_comm_world,ierror)
         else
            call wrwrfmassa%wrwrfmassa_binary(mype)
         end if
      end if
  
  !write cmaq analysis
  
      if (cmaq_regional) call write_cmaq(mype)
  
  !   Write nems nmmb analysis file.
  
      if (nems_nmmb_regional) call wrwrfnmma%wrnemsnmma_binary(mype)
  
  !   Write 2d analysis file
  !   output format: binary
      if (twodvar_regional) call wr2d_binary(mype)
  
      return
  end subroutine write_regional_analysis_wrf
end module regional_io_mod
