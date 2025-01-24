module lag_fields
!$$$ module documentation block
!           .      .    .                                       .
! module:   lag_fields
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added module doc block
!   2010-07-14  todling - use die to abort
!   2010-08-19  lueken - add only to module use;no machine code, so use .f90
!   2011-04-20  todling - de-active read and write until further notice
!   2013-10-25  todling - reposition ltosi and others to commvars
!
! subroutines included:
!   sub lag_modini
!   sub lag_guessini
!   sub lag_alloc_uv
!   sub lag_destroy_uv
!   sub lag_destroy_state
!   sub lag_gather_gesuv
!   sub lag_gather_stateuv
!   sub lag_ADscatter_stateuv
!   sub lag_presetup
!   sub lag_state_write
!   sub lag_state_read
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nsig,ak5,bk5
  use gridmod, only: lat1,lon1,latlon1n
  use gridmod, only: ijn_s,ijn,displs_g,ird_s
  use gridmod, only: iglobal,itotsub
  use general_commvars_mod, only: ltosi,ltosj,ltosi_s,ltosj_s
  use guess_grids, only: nfldsig,hrdifsig
  use constants, only: zero,two,pi,deg2rad
  use gsi_4dvar, only: nobs_bins,hr_obsbin,l4dvar

  use lag_traj, only: lag_initlparam,lag_iteduration
  use lag_traj, only: lag_rk2itenpara_r,lag_rk2itenpara_i
  ! use lag_traj, only: lag_rk4itenpara_r,lag_rk4itenpara_i
  use lag_traj, only: lag_rk2iter_nl
  ! use lag_traj, only: lag_rk4iter_nl

  use lag_interp, only: lag_inipgrid,lag_delpgrid,lag_logcte_p
  use lag_interp, only: lag_index_h
  use mpeu_util, only: die

  implicit none

  private

  ! Constants and variables
  public:: infile_lag,lag_nmax_bal,lag_vorcore_stderr_a,lag_vorcore_stderr_b
  public:: lag_kfirst,lag_klast,lag_kcount

  public:: lag_u_full,lag_v_full,lag_uv_fill,lag_uv_alloc
  public:: ntotal_orig_lag,nlocal_orig_lag
  public:: orig_lag_data,orig_lag_num

  public:: lag_nl_vec,lag_tl_vec,lag_ad_vec
  public:: lag_tl_spec_r,lag_tl_spec_i

  ! Routines
  public:: lag_modini,lag_guessini,lag_presetup
  public:: lag_alloc_uv
  public:: lag_destroy_uv,lag_destroy_state

  public:: lag_gather_gesuv,lag_gather_stateuv,lag_ADscatter_stateuv

  public:: lag_state_write,lag_state_read

  ! File(s) containing the initial positions of balloons
  character(255) ::infile_lag='inistate_lag.dat'
  ! Maximum number of balloon in reference state
  integer(i_kind)::lag_nmax_bal=1000
  ! Observation error for vorcore balloon
  !   error = b + a * timestep(in hours)
  real(r_kind)::lag_vorcore_stderr_a=2.0e3_r_kind
  real(r_kind)::lag_vorcore_stderr_b=zero

  integer(i_kind)::lag_kfirst,lag_klast ! First and last level use
  integer(i_kind)::lag_kcount           ! Number of levels use

  integer(i_kind),parameter::lag_unit_in  =11 ! Unit to read in the ini file
  integer(i_kind),parameter::lag_unit_save=11 ! Unit to read/write the state

  integer(i_kind),parameter::iv_debug=1       ! printing level
  integer(i_kind),parameter::idonothing = 1   ! deactive this code for now

  character(*),parameter::lag_filesave='lagstate.save'  ! File for save/read
                                                        ! the state of the
                                                        ! lag. module

  ! Allocatable arrays to store the whole U and V fields
  !   1st dim: horiz index number
  !   2nd dim: verical level (from kfirst to klast)
  !   3rd dim: obs bin
  real(r_kind),dimension(:,:,:),allocatable::lag_u_full,lag_v_full
  
  logical,dimension(:),allocatable::lag_uv_fill   ! fields fetched ?
  logical::lag_uv_alloc                           ! allocated ?

  integer(i_kind)::ntotal_orig_lag  ! Total number of balloons present at 
                                    ! initial time
  integer(i_kind)::nlocal_orig_lag  ! Total number of balloons affected
                                    ! at this given processor

  ! Location of balloons at the begining of the first obsbin
  real(r_kind)   ,dimension(:,:),allocatable::orig_lag_data
  ! Data associated with balloons (number, processor, internal number)
  integer(i_kind),dimension(:,:),allocatable::orig_lag_num

  ! Vectors to contain NL, TL and AD calculations for each balloon
  ! affected to the processor (not all balloons)
  real(r_kind),dimension(:,:,:),allocatable::lag_nl_vec
  real(r_kind),dimension(:,:,:),allocatable::lag_tl_vec
  real(r_kind),dimension(:,:,:),allocatable::lag_ad_vec

  ! Specifications of TL and AD models
  real(r_kind)   ,dimension(:,:,:),allocatable::lag_tl_spec_r
  integer(i_kind),dimension(:,:,:),allocatable::lag_tl_spec_i

  contains


  ! ------------------------------------------------------------------------
  ! Initialise constants
  subroutine lag_modini
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_modini
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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
    use constants, only: r3600
    implicit none

    ! Initialisation of the model module (lag_traj)
    lag_iteduration=hr_obsbin*r3600
    call lag_initlparam()
    
    ! Not initialised at first
    lag_uv_alloc=.false.
    
  end subroutine lag_modini
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Allocate the pseudo state vector for lagrangian assimilation by reading
  ! the guess fields
  ! - in 4D var, read only the position at the begining of the assimilation
  !   window (positions for the other obsbin are calculated by lag_presetup)
  ! - in 3D var, read the positions at the begining of the assimilation window, 
  !   but also try to read the guess at the times of the atmos. model guess.
  !   If these additional guess are missing, values will be calculated by
  !   lag_presetup (but the result of the assimilation won't be optimal)
  subroutine lag_guessini
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_guessini
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
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
    use mpimod, only: mype,npe
    implicit none   

    real(r_kind)::lon,lat,p
    integer(i_kind)::num
    real(r_kind),dimension(nsig+1)::pcalc
    real(r_kind),dimension(nsig+1)::tmp_press
    integer(i_kind)::i,j,iferror,nread
    real(r_kind)   ,dimension(:,:),allocatable::tmp_inibal
    integer(i_kind),dimension(:,:),allocatable::tmp_ininum
    real(r_kind),dimension(:),allocatable  ::tmp_inisig
    logical::lerror,leof,lmax,lok
    character(len=255)::tmp_fname

    ! Table of pressure for all levels (standard pressure)
    do i=1,nsig+1
       pcalc(i)=10_r_kind*ak5(i)+1e3_r_kind*bk5(i)
    end do
    do i=1,nsig
       tmp_press(i)=log(sqrt(pcalc(i)*pcalc(i+1)))
    end do
      
    !Allocate the array to read data
    allocate(tmp_inibal(lag_nmax_bal,3))  ! balloon initial position
    allocate(tmp_ininum(lag_nmax_bal,3))  ! numbers of balloons
    allocate(tmp_inisig(lag_nmax_bal))    ! to store vertical level
    ntotal_orig_lag=0
    nlocal_orig_lag=0

    !open the initial position file
    write(unit=tmp_fname,fmt="(A,'.',I3.3)") trim(infile_lag),1
    open(lag_unit_in,file=trim(tmp_fname),form='formatted',iostat=iferror)
    lerror = (iferror/=0)

    !read the first line
    if (.not.lerror) then
       read(lag_unit_in,fmt=*,iostat=iferror) num,lon,lat,p
       lerror=(iferror>0)
       leof  =(iferror<0)
       lmax  =.false.
    end if

    !loop on the file
    nread=0
    do while (.not.(lerror .or. leof .or. lmax))

       nread=nread+1
       lon=deg2rad*lon
       lat=deg2rad*lat

       lok=(lon>=-pi     .and. lon<=two*pi .and. &
           &lat>=-pi/two .and. lat<=pi/two .and. &
           &p>=zero)

       if (lok) then

          if (lon<zero) lon=lon+two*pi
          ntotal_orig_lag=ntotal_orig_lag+1
          tmp_ininum(ntotal_orig_lag,1)=num ! balloon id number
          tmp_ininum(ntotal_orig_lag,2)=mod(ntotal_orig_lag-1,npe)  ! Processor of
                                                                    ! affectation
          tmp_ininum(ntotal_orig_lag,3)=(ntotal_orig_lag-1)/npe + 1 ! Nr. on processor

          ! Number of balloons affected to this processor
          if (mype==tmp_ininum(ntotal_orig_lag,2)) nlocal_orig_lag=nlocal_orig_lag+1

          tmp_inibal(ntotal_orig_lag,1)=lon ! Balloon data
          tmp_inibal(ntotal_orig_lag,2)=lat
          tmp_inibal(ntotal_orig_lag,3)=p

          ! Calculate the level relative coordinate
          tmp_inisig(ntotal_orig_lag)=log(p)
          call grdcrd1(tmp_inisig(ntotal_orig_lag),tmp_press,nsig,-1)
       end if

       ! read the next one
       read(lag_unit_in,fmt=*,iostat=iferror) num,lon,lat,p
       lerror=(iferror>0)
       leof  =(iferror<0)
       lmax  =nread>=lag_nmax_bal

    end do

    close(lag_unit_in)

    if (mype==0) &
       write(6,'(A,I4,A)') 'LAG_GUESSINI: first guess read. ',&
         &ntotal_orig_lag,' balloons availlable.'

    ! Is there observations ?
    if (ntotal_orig_lag>0) then

       !Allocate the array containing the original positions
       allocate(orig_lag_data(ntotal_orig_lag,3),stat=i)
       if (i /= 0) stop "Allocation failled"
       allocate(orig_lag_num (ntotal_orig_lag,3),stat=i)
       if (i /= 0) stop "Allocation failled"
       orig_lag_data(:,:)=tmp_inibal(1:ntotal_orig_lag,:)
       orig_lag_num(:,:) =tmp_ininum(1:ntotal_orig_lag,:)

       ! Setup the min and max level
       lag_kfirst=floor(  minval(tmp_inisig(1:ntotal_orig_lag)))-1
       lag_klast =ceiling(maxval(tmp_inisig(1:ntotal_orig_lag)))+1
       lag_kcount=lag_klast-lag_kfirst+1
 
       !Setup the pressure subgrid
       call lag_inipgrid(tmp_press(lag_kfirst:lag_klast))
 
       !Allocate the distributed state vectors
       if (nlocal_orig_lag/=0) then
          if (l4dvar) then
             allocate(lag_nl_vec(nlocal_orig_lag,nobs_bins,3))
             allocate(lag_tl_vec(nlocal_orig_lag,nobs_bins,3))
             allocate(lag_ad_vec(nlocal_orig_lag,nobs_bins,3))
          else
             allocate(lag_nl_vec(nlocal_orig_lag,nfldsig,3))
             allocate(lag_tl_vec(nlocal_orig_lag,nfldsig,3))
             allocate(lag_ad_vec(nlocal_orig_lag,nfldsig,3))
          end if
          lag_nl_vec=zero;lag_tl_vec=zero; lag_ad_vec=zero
          do i=1,ntotal_orig_lag 
             if (mype==orig_lag_num(i,2)) then
                lag_nl_vec(orig_lag_num(i,3),1,:)=orig_lag_data(i,:)
             end if
          end do
       end if

       !Allocate the distributed TL model definition
       if (nlocal_orig_lag/=0 .and. nobs_bins>1) then
          allocate(lag_tl_spec_r(nlocal_orig_lag,nobs_bins-1,lag_rk2itenpara_r))
          allocate(lag_tl_spec_i(nlocal_orig_lag,nobs_bins-1,lag_rk2itenpara_i))
       end if

       !If in 3Dvar, try to read the other guess fields
       if (.not.l4dvar) then

          ! If there is several obsbin in 3Dvar it will really blow up, so...
          if (nobs_bins/=1) &
            &call die('LAG_GUESSINI: only 1 obsbin can be handle in 3Dvar')

          do i=2,nfldsig

             if (mype==0) &
                write(6,'(A,I2,A)') 'LAG_GUESSINI: 3Dvar try to read guess ',i,'.'

             !open the initial position file
             write(unit=tmp_fname,fmt="(A,'.',I3.3)") trim(infile_lag),i
             open(lag_unit_in,file=trim(tmp_fname),form='formatted',iostat=iferror)
             lerror = (iferror/=0)
 
             !read the first line
             if (.not.lerror) then
                read(lag_unit_in,fmt=*,iostat=iferror) num,lon,lat,p
                lerror=(iferror>0)
                leof  =(iferror<0)
                lmax  =.false.
             end if

             !loop on the file
             do while (.not.(lerror .or. leof .or. lmax))
 
                lon=deg2rad*lon
                lat=deg2rad*lat

                lok=(lon>=-pi     .and. lon<=two*pi .and. &
                    &lat>=-pi/two .and. lat<=pi/two .and. &
                    &p>=zero)

                ! If good, store in the appropriate array
                if (lok) then
                   if (lon<zero) lon=lon+two*pi
 
                   do j=1,ntotal_orig_lag
                      if (orig_lag_num(j,1)==num .and. orig_lag_num(j,2)==mype) then
                         lag_nl_vec(orig_lag_num(j,3),i,1)=lon
                         lag_nl_vec(orig_lag_num(j,3),i,2)=lat
                         lag_nl_vec(orig_lag_num(j,3),i,3)=p
                      end if
                   end do    
            
                end if

                ! read the next one
                read(lag_unit_in,fmt=*,iostat=iferror) num,lon,lat,p
                lerror=(iferror>0)
                leof  =(iferror<0)
                lmax  =nread>=lag_nmax_bal
 
             end do

             close(lag_unit_in)

          end do ! nfldsig

       end if ! 3D var read next guesses

    end if ! ntotal_orig_lag>0

    !cleaning
    deallocate(tmp_inibal,tmp_ininum,tmp_inisig)

    !debug printing
    if (iv_debug>=1) then
       if (mype==0) then
          print *,'GUESS NUM',orig_lag_num
          print *,'GUESS kfirst, klast',lag_kfirst,lag_klast
       end if
       do i=1,size(lag_nl_vec,2)
          do j=1,size(lag_nl_vec,1)
             print '(A,I2.2,A,I2.2,A,I2.2,A,F12.6,F12.6,F12.6)',&
               &'GUESS ',i,' MYPE ',mype,' LOC# ',j,' POS ',lag_nl_vec(j,i,:)
          end do
       end do
    end if

  end subroutine lag_guessini
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Allocate the u and v fields
  subroutine lag_alloc_uv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_alloc_uv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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
    implicit none
    
    if (.not.lag_uv_alloc) then
       allocate(lag_u_full(iglobal,lag_kcount,max(nobs_bins,nfldsig)))
       allocate(lag_v_full(iglobal,lag_kcount,max(nobs_bins,nfldsig)))
       allocate(lag_uv_fill(max(nobs_bins,nfldsig)))
       lag_u_full=zero; lag_v_full=zero
       lag_uv_fill(:)=.false.
       lag_uv_alloc  =.true.
    end if

  end subroutine lag_alloc_uv
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Deallocate the u and v fields
  subroutine lag_destroy_uv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_destroy_uv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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
    implicit none

    if (lag_uv_alloc) then
       deallocate(lag_u_full)
       deallocate(lag_v_full)
       deallocate(lag_uv_fill)
       lag_uv_alloc=.false.
    end if

  end subroutine lag_destroy_uv
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Deallocate the state parameters for the lagrangian assimilation
  subroutine lag_destroy_state
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_destroy_state
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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
    implicit none
    
    if (allocated(lag_nl_vec)) deallocate(lag_nl_vec)
    if (allocated(lag_tl_vec)) deallocate(lag_tl_vec)
    if (allocated(lag_ad_vec)) deallocate(lag_ad_vec)

    if (allocated(lag_tl_spec_r)) deallocate(lag_tl_spec_r)
    if (allocated(lag_tl_spec_i)) deallocate(lag_tl_spec_i)

    if (allocated(orig_lag_data)) deallocate(orig_lag_data)
    if (allocated(orig_lag_num))  deallocate(orig_lag_num)

  end subroutine lag_destroy_state
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Gather the the guess fields for u and v, for obsbin time hrdifsig(t)
  ! (using global variables ges_u and ges_v).
  subroutine lag_gather_gesuv(itt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_gather_gesuv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2010-04-01  treadon - move strip and reorder to gridmod
!   2013-10-19  todling - metguess now holds background
!
!   input argument list:
!    itt
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
 
    use mpimod, only: mype,mpi_comm_world,mpi_rtype
    use gridmod, only: strip,reorder
    use gsi_bundlemod, only : gsi_bundlegetpointer
    use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
    
    implicit none

    integer(i_kind),intent(in   ) :: itt

    integer(i_kind):: k,i
    integer(i_kind):: ierror,istatus
    real(r_kind),dimension(lat1*lon1,nsig):: ustrip,vstrip
    real(r_kind),dimension(:),allocatable::  worku,workv
    real(r_kind),dimension(:,:,:),pointer::  ges_u_itt=>NULL()
    real(r_kind),dimension(:,:,:),pointer::  ges_v_itt=>NULL()

    ! Security
    if (ntotal_orig_lag==0) return

    ierror=0
    call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'u',ges_u_itt,istatus)
    ierror=ierror+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'v',ges_v_itt,istatus)
    ierror=ierror+istatus
    if(ierror/=0) return

    ! allocations
    call lag_alloc_uv()
    allocate(worku(max(iglobal,itotsub)))
    allocate(workv(max(iglobal,itotsub)))

    ! strip buffer parts of ges_u and ges_v
    call strip(ges_u_itt,ustrip,nsig)
    call strip(ges_v_itt,vstrip,nsig)

    ! gather the fields only for the predefined level range
    do k=lag_kfirst,lag_klast
       call mpi_allgatherv(ustrip(1,k),ijn(mype+1),mpi_rtype,&
         worku,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       call mpi_allgatherv(vstrip(1,k),ijn(mype+1),mpi_rtype,&
         workv,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       call reorder(worku,1,1)
       call reorder(workv,1,1)
       do i=1,iglobal
          lag_u_full(lag_index_h(ltosj(i),ltosi(i)),k-lag_kfirst+1,itt)=worku(i)
          lag_v_full(lag_index_h(ltosj(i),ltosi(i)),k-lag_kfirst+1,itt)=workv(i)
       end do
    end do

    lag_uv_fill(itt)=.true.

    deallocate(worku)
    deallocate(workv)

  end subroutine lag_gather_gesuv
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Gather the increments fields for u and v, taking increments in the 
  ! subdomains state vectors svalu and svalv. for obsbin t.
  subroutine lag_gather_stateuv(svalu,svalv,itt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_gather_stateuv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2010-04-01  treadon - move strip and reorder to gridmod
!
!   input argument list:
!    svalu,svalv
!    itt
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mype,mpi_comm_world,mpi_rtype
    use gridmod, only: strip,reorder,lat2,lon2
    implicit none

    real(r_kind),dimension(latlon1n),intent(in   ) :: svalu,svalv
    integer(i_kind)                 ,intent(in   ) :: itt

    integer(i_kind):: k,i
    integer(i_kind):: ierror
    real(r_kind),dimension(lat1*lon1,nsig):: ustrip,vstrip
    real(r_kind),dimension(:),allocatable::  worku,workv

    ! Security
    if (ntotal_orig_lag==0) return

    ! allocations
    call lag_alloc_uv()
    allocate(worku(max(iglobal,itotsub)))
    allocate(workv(max(iglobal,itotsub)))

    ! strip buffer parts of ges_u and ges_v
    call strip(reshape(svalu,(/lat2,lon2,nsig/)),ustrip,nsig)
    call strip(reshape(svalv,(/lat2,lon2,nsig/)),vstrip,nsig)

    ! gather the fields only for the predefined level range
    do k=lag_kfirst,lag_klast
       call mpi_allgatherv(ustrip(1,k),ijn(mype+1),mpi_rtype,&
         worku,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       call mpi_allgatherv(vstrip(1,k),ijn(mype+1),mpi_rtype,&
         workv,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
       call reorder(worku,1,1)
       call reorder(workv,1,1)
       do i=1,iglobal
          lag_u_full(lag_index_h(ltosj(i),ltosi(i)),k-lag_kfirst+1,itt)=worku(i)
          lag_v_full(lag_index_h(ltosj(i),ltosi(i)),k-lag_kfirst+1,itt)=workv(i)
       end do
    end do

    lag_uv_fill(itt)=.true.

    deallocate(worku)
    deallocate(workv)

  end subroutine lag_gather_stateuv
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Scatter the adjoint increments back in subdomain fields for u and v.
  !  - Adjoint increments are added in subdomain state vectors svalu & svalv
  !  - Adjoint increments are read in global domain arrays lag_u_full and 
  !    lag_v_full at obsbin t
  subroutine lag_ADscatter_stateuv(svalu,svalv,itt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_ADscatter_stateuv
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2010-04-01  treadon - move reorder2 to gridmod
!
!   input argument list:
!    svalu,svalv
!    itt
!
!   output argument list:
!    svalu,svalv
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only: mype,mpi_comm_world,mpi_rtype,mpi_sum
    use gridmod, only: reorder2
    implicit none

    real(r_kind),dimension(latlon1n),intent(inout) :: svalu, svalv
    integer(i_kind)                 ,intent(in   ) :: itt

    integer(i_kind):: k,i
    integer(i_kind):: ierror
    real(r_kind),dimension(:,:),allocatable::  worku ,workv
    real(r_kind),dimension(:),allocatable::  worku2,workv2

    integer(i_kind)::hdisp_glo1,hdisp_glo2,hdisp_sub1,hdisp_sub2

    ! lag_u_full & lag_v_full must be there
    if (.not.lag_uv_alloc .or. ntotal_orig_lag==0) return

    allocate(worku(iglobal,lag_kcount))
    allocate(workv(iglobal,lag_kcount))
    allocate(worku2(itotsub))
    allocate(workv2(itotsub))

    ! Sum the adjoint increments calculated on the whole grid by each
    ! processors
    call mpi_allreduce(lag_u_full(1,1,itt),worku,iglobal*lag_kcount,&
      mpi_rtype,mpi_sum,mpi_comm_world,ierror)
    call mpi_allreduce(lag_v_full(1,1,itt),workv,iglobal*lag_kcount,&
      mpi_rtype,mpi_sum,mpi_comm_world,ierror)
    ! print *,'Mype',mype,'Contribution total worku',sum(worku)

    ! Extract the part of the previously reduced grid for the given subdomain
    do k=lag_kfirst,lag_klast

       ! reorder all the elements to obtain the same shape than svalu & svalv
       do i=1,itotsub
          worku2(i)=worku(lag_index_h(ltosj_s(i),ltosi_s(i)),k-lag_kfirst+1)
          workv2(i)=workv(lag_index_h(ltosj_s(i),ltosi_s(i)),k-lag_kfirst+1)
       end do
       call reorder2(worku2,1,1)
       call reorder2(workv2,1,1)
       ! if (k==lag_kfirst+1) print *,'Mype',mype,'Contribution total worku2',sum(worku2)

       ! indexes in the arrays
       hdisp_glo1=ird_s(mype+1)+1
       hdisp_glo2=ird_s(mype+1)+ijn_s(mype+1)-1
       hdisp_sub1=1+ijn_s(mype+1)*(k-1)
       hdisp_sub2=hdisp_sub1+ijn_s(mype+1)-1
 
       svalu(hdisp_sub1:hdisp_sub2)=&
         svalu(hdisp_sub1:hdisp_sub2) + worku2(hdisp_glo1:hdisp_glo2)
       ! if (k==lag_kfirst+1) print *,'Mype',mype,'Svalu',sum(svalu)
 
       svalv(hdisp_sub1:hdisp_sub2)=&
         svalv(hdisp_sub1:hdisp_sub2) + workv2(hdisp_glo1:hdisp_glo2)

    end do

    deallocate(worku)
    deallocate(workv)
    deallocate(worku2)
    deallocate(workv2)

  end subroutine lag_ADscatter_stateuv
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! 1. Load the entire wind guess in memory.
  ! 2. In 4Dvar, perfom the non-linear trajectory integration for all balloons
  ! affected to this processor. Subsequently save the parameters for the TL
  ! model.
  ! 3. In 3Dvar, perfom the non-linear trajectory integration for all balloons
  ! affected to this processor only if the postion as not been read previously
  ! in guess files. (for an optimal 3Dvar, values should be read in guess files)
  subroutine lag_presetup
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_presetup
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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

    use constants, only: r3600
    use mpimod, only: mype
    implicit none

    integer(i_kind)::i,j,k,itime
    real(r_kind)::fieldtime

    ! If no balloons do nothing
    if (ntotal_orig_lag>0) then

       ! Load the wind field guess in memory
       do i=1,nfldsig
          call lag_gather_gesuv(i)
          if (iv_debug>=1) print '(A,I2,A,I2)','LAG_PRESETUP: Mype',mype,&
            &'. Gather u,v guess',i
       end do

       ! 4Dvar, Perform the NL integration given this guess fo each obsbin
       if (l4dvar .and. nobs_bins>1) then
          do i=2,nobs_bins
 
             fieldtime=(i-2)*hr_obsbin
             if(fieldtime>=hrdifsig(1) .and. fieldtime<=hrdifsig(nfldsig)) then
 
                ! Which guess field to use
                do k=1,nfldsig-1
                   if(fieldtime >= hrdifsig(k) .and. fieldtime <= hrdifsig(k+1)) then
                      itime=k
                   end if
                end do

                do j=1,nlocal_orig_lag
                   lag_nl_vec(j,i,:)=lag_nl_vec(j,i-1,:)
                   call lag_rk2iter_nl(&
                     &lag_nl_vec(j,i,1),lag_nl_vec(j,i,2),lag_nl_vec(j,i,3),&
                     &lag_u_full(:,:,itime),lag_v_full(:,:,itime),&
                     &lag_iteduration,&
                     &lag_tl_spec_i(j,i-1,:),lag_tl_spec_r(j,i-1,:))
                end do

             else
                call die('lag_presetup: Inapropriate velocity guess fields to &
                  & compute trajectories')
             end if

          end do
       end if ! l4dvar

       ! 3Dvar, Perform the NL integration given this guess fo each guess time 
       ! (if needed only)
       if (.not.l4dvar .and. nfldsig>1) then

          do i=2,nfldsig
             do j=1,nlocal_orig_lag
                ! If the guess add already been read do nothing
                if (lag_nl_vec(j,i,3)==zero) then
                   lag_nl_vec(j,i,:)=lag_nl_vec(j,i-1,:)
                   call lag_rk2iter_nl(&
                     &lag_nl_vec(j,i,1),lag_nl_vec(j,i,2),lag_nl_vec(j,i,3),&
                     &lag_u_full(:,:,i-1),lag_v_full(:,:,i-1),&
                     &(hrdifsig(i)-hrdifsig(i-1))*r3600)
                end if
             end do
          end do

       end if ! not l4dvar

       !debug printing
       if (iv_debug>=1) then
          do i=1,size(lag_nl_vec,2)
             do j=1,size(lag_nl_vec,1)
                print '(A,I2.2,A,I2.2,A,I2.2,A,F12.6,F12.6,F12.6)',&
                  &'PRESETUP ',i,' MYPE ',mype,' LOC# ',j,' POS ',lag_nl_vec(j,i,:)
             end do
          end do
       end if

    end if ! ntotal_orig_lag>0

  end subroutine lag_presetup
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Save all the lagrangian parameters in a file (1 file per task)
  subroutine lag_state_write()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_state_write
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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

    use mpimod, only: mype
    implicit none

    character(len=255)::loc_filename
    integer(i_kind)::istat

    ! RTodling: Return for now - this code is innactive
    if(idonothing/=0) return

    ! Create the filename with the mype number    
    write(loc_filename,fmt="(A,'.',I4.4)") lag_filesave,mype
    ! Open result file
    open(unit=lag_unit_save,file=trim(loc_filename),iostat=istat,&
      form='unformatted',action='write')
    if (istat/=0) call die('lag_state_write: Unable to open file');

    write(lag_unit_save) ntotal_orig_lag, nlocal_orig_lag

    if (ntotal_orig_lag>0) then
  
       write(lag_unit_save) lag_kfirst,lag_klast
       write(lag_unit_save) lag_logcte_p

       write(lag_unit_save) orig_lag_data, orig_lag_num

       if (nlocal_orig_lag>0) then
          write(lag_unit_save) lag_nl_vec
 
          if (nobs_bins>1) write(lag_unit_save) lag_tl_spec_r, lag_tl_spec_i
       end if

    end if

    close(lag_unit_save,iostat=istat)
    if (istat/=0) call die('lag_state_write: Unable to close file');

  end subroutine lag_state_write
  ! ------------------------------------------------------------------------

  ! ------------------------------------------------------------------------
  ! Read all the lagrangian parameters in a file (1 file per task)
  subroutine lag_state_read()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_state_read
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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

    use mpimod, only: mype
    implicit none

    character(len=255)::loc_filename
    integer(i_kind)::istat
    real(r_kind),dimension(:),allocatable::tmp_pgrid
 
    ! RTodling: Return for now - this code is innactive
    if(idonothing/=0) return

    ! Create the filename with the mype number    
    write(unit=loc_filename,fmt="(A,'.',I4.4)") lag_filesave,mype
    ! Open result file
    open(unit=lag_unit_save,file=trim(loc_filename),iostat=istat,&
      form='unformatted',action='read')
    if (istat/=0) call die('lag_state_read: Unable to open file');

    ! First read the number of balloons
    read(lag_unit_save,iostat=istat) ntotal_orig_lag, nlocal_orig_lag
    if (istat/=0) call die('lag_state_read: Unable to read # of balloons')

    ! Start from scratch
    call lag_destroy_state()

    if (ntotal_orig_lag>0) then

       read(lag_unit_save,iostat=istat)  lag_kfirst,lag_klast
       if (istat/=0) call die('lag_state_read: Unable to read pgrid par.')
       lag_kcount=lag_klast-lag_kfirst+1

       allocate(tmp_pgrid(lag_kcount),stat=istat)
       if (istat/=0) call die('lag_state_read: err. alloc. tmp_pgrid')

       allocate(orig_lag_data(ntotal_orig_lag,3),stat=istat)
       if (istat/=0) call die('lag_state_read: err. alloc. orig_lag_data')
       allocate(orig_lag_num (ntotal_orig_lag,3),stat=istat)
       if (istat/=0) call die('lag_state_read: err. alloc. orig_lag_num')

       read(lag_unit_save,iostat=istat) tmp_pgrid
       if (istat/=0) call die('lag_state_read: Unable to read pgrid')
       call lag_inipgrid(tmp_pgrid)

       read(lag_unit_save,iostat=istat) orig_lag_data, orig_lag_num
       if (istat/=0) call die('lag_state_read: Unable to read orig_lag*')
 
       if (nlocal_orig_lag/=0) then
 
          allocate(lag_nl_vec(nlocal_orig_lag,nobs_bins,3),stat=istat)
          if (istat/=0) call die('lag_state_read: err. alloc. lag_nl_vec')
          allocate(lag_tl_vec(nlocal_orig_lag,nobs_bins,3),stat=istat)
          if (istat/=0) call die('lag_state_read: err. alloc. lag_tl_vec')
          allocate(lag_ad_vec(nlocal_orig_lag,nobs_bins,3),stat=istat)
          if (istat/=0) call die('lag_state_read: err. alloc. lag_ad_vec')
          lag_tl_vec=zero; lag_ad_vec=zero; lag_nl_vec=zero;

          read(lag_unit_save,iostat=istat) lag_nl_vec
          if (istat/=0) call die('lag_state_read: Unable to read lag_nl_vec')

          if (nobs_bins>1) then
 
             allocate(lag_tl_spec_r(nlocal_orig_lag,nobs_bins-1,lag_rk2itenpara_r),&
               stat=istat)
             if (istat/=0) call die('lag_state_read: err. alloc. spec_r')
             allocate(lag_tl_spec_i(nlocal_orig_lag,nobs_bins-1,lag_rk2itenpara_i),&
               stat=istat)
             if (istat/=0) call die('lag_state_read: err. alloc. spec_i')
 
             read(lag_unit_save,iostat=istat) lag_tl_spec_r, lag_tl_spec_i
             if (istat/=0) call die('lag_state_read: Unable to read spec_*')

          end if

       end if

    end if

    close(lag_unit_save,iostat=istat)
    if (istat/=0) call die('lag_state_read: Unable to close file');

  end subroutine lag_state_read
  ! ------------------------------------------------------------------------


end module lag_fields
