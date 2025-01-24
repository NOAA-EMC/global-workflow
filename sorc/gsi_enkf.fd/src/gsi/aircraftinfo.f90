module aircraftinfo
!$$$   module documentation block
!                .      .    .                                       .
! module:    aircraftinfo
!
! abstract:  This module contains variables and routines related
!            to information for the use of aircraft temperature data.
!
! program history log:
!   2013-05-17 Yanqiu Zhu
!
! subroutines included:
!   sub init_aircraft            - set aircraft related variables to defaults
!   sub aircraftinfo_read        - read in aircraft info and biases
!   sub aircraftinfo_write       - write out aircraft biases
!
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block


! !USES:

  use kinds, only: r_kind,i_kind,r_quad,r_double
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_aircraft
  public :: aircraftinfo_read
  public :: aircraftinfo_write

! set passed variables to public
  public :: aircraft_t_bc
  public :: aircraft_t_bc_pof
  public :: aircraft_t_bc_ext
  public :: cleanup_tail
  public :: biaspredt
  public :: max_tail
  public :: ntail, ntail_update, idx_tail, taillist, timelist, npredt, predt
  public :: ostats_t,rstats_t,varA_t
  public :: mype_airobst
  public :: upd_pred_t
  public :: upd_aircraft
  public :: nsort,itail_sort,idx_sort
  public :: hdist_aircraft
  
  logical :: aircraft_t_bc ! logical to turn off or on the aircraft temperature bias correction
  logical :: aircraft_t_bc_pof ! logical to turn off or on the aircraft temperature bias correction with pof
  logical :: aircraft_t_bc_ext ! logical to turn off or on the externally supplied aircraft bias correction
  logical :: cleanup_tail ! logical to remove tail number no longer used
  logical :: upd_aircraft ! indicator if update bias at 06Z & 18Z
  
  integer(i_kind), parameter :: max_tail=100000  ! max tail numbers
  integer(i_kind) npredt          ! predictor number
  integer(i_kind) ntail           ! total tail number
  integer(i_kind) ntail_update    ! new total tail number
  integer(i_kind) mype_airobst    ! processor reading in aircraft profile data 
  integer(i_kind) nsort           ! used in sorting tail number
  
  character(len=10),dimension(max_tail):: taillist  ! tail number
  character(len=1),dimension(max_tail):: itail_sort ! used in sorting tail number
  integer(i_kind),dimension(max_tail):: idx_tail    ! index of tail
  integer(i_kind),dimension(max_tail):: idx_sort    ! used in sorting tail number
  integer(i_kind),dimension(max_tail):: timelist    ! time stamp
  real(r_kind):: biaspredt                          ! berror var for temperature bias correction coefficients
  real(r_kind):: upd_pred_t                         ! =1 update bias; =0 no update
  real(r_kind):: hdist_aircraft                     ! horizontal distance threshold for errormod_aircraft
  real(r_kind),allocatable,dimension(:,:):: predt        ! coefficients for predictor part of bias correction

  real(r_kind),allocatable,dimension(:,:):: varA_t
  real(r_quad),allocatable,dimension(:,:):: ostats_t
  real(r_quad),allocatable,dimension(:,:):: rstats_t
  
contains


  subroutine init_aircraft
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    init_aircraft
!
!   prgrmmr:     zhu      org: np23                date: 2013-05-17
!
! abstract:  This routine sets default values for variables used in
!            the aircraft temperature bias correction routines.
!
! program history log:
!   2013-05-17  Zhu
!   2014-03-04  Sienkiewicz - added aircraft_t_bc_ext option
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use constants, only: one
    implicit none

    ntail  = 0       ! total number of tail numbers
    ntail_update = 0 ! total number of tail numbers
    npredt = 3       ! number of bias correction predictors
    biaspredt = one
    aircraft_t_bc = .false.   ! .true.=turn on bias correction
    aircraft_t_bc_pof = .false.   ! .true.=turn on bias correction
    aircraft_t_bc_ext = .false.   ! .true.=turn on bias correction
    cleanup_tail = .false.    ! no removal of tail number 
    mype_airobst = 0

    upd_aircraft=.true.
    upd_pred_t=one

    hdist_aircraft=60000.0_r_kind
    
  end subroutine init_aircraft


  subroutine aircraftinfo_read
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    aircraftinfo_read
!
!   prgrmmr:     zhu        org: np20                date: 2013-05-17
!
! abstract:  This routine reads the tail number and bias correction info
!
! program history log:
!   2013-05-17  Yanqiu Zhu
!   2014-03-04  Sienkiewicz - changes for external table aircraft_t_bc_ext option
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    use constants, only: zero,zero_quad
    use mpimod, only: mype
    use obsmod, only: iadate
    use gsi_io, only: verbose
    implicit none

    integer(i_kind) j,k,lunin,nlines,ip,istat
    integer(i_kind) anal_time
    integer(i_kind) isort
    real(r_kind),dimension(npredt):: ostatsx
    real(r_kind),dimension(npredt)::varx
    real(r_kind),dimension(npredt):: predr
    character(len=1):: cflg
    character(len=1):: cb,cb0
    character(len=10):: tailwk
    character(len=150):: crecord
    logical pcexist,print_verbose

    data lunin / 49 /


    print_verbose = .false. .and. mype == 0
    if(verbose .and. mype == 0)print_verbose=.true.
!   Determine number of entries in aircraft bias file
    inquire(file='aircftbias_in',exist=pcexist)
    if (.not. pcexist) then 
       write(6,*)'AIRCRAFTINFO_READ:  ***ERROR*** aircftbias_in not found'
       call stop2(340)
    end if

    open(lunin,file='aircftbias_in',form='formatted')
    j=0
    nlines=0
    read1:  do
       read(lunin,100,iostat=istat) cflg,crecord
       if (istat /= 0) exit
       nlines=nlines+1
       if (cflg == '!') cycle
       j=j+1
    end do read1
    if (istat>0) then
       close(lunin)
       write(6,*)'AIRCRAFTINFO_READ:  ***ERROR*** error reading aircftbias_in, istat=',istat
       write(6,*)'AIRCRAFTINFO_READ:  stop program execution'
       call stop2(340)
    endif
    ntail = j
    ntail_update = j

    if (print_verbose) then
       write(6,120) ntail
120    format('AIRCRAFTINFO_READ:  ntail=',1x,i6)
    endif
    if (ntail > max_tail) then 
       write(6,*)'AIRCRAFTINFO_READ:  ***ERROR*** ntail exceeds max_tail'
       write(6,*)'AIRCRAFTINFO_READ:  stop program execution'
       call stop2(340)
    end if
    rewind(lunin)

    allocate(predt(npredt,max_tail))
    idx_tail = 0
    timelist = 111111
    predt = zero

    allocate(ostats_t(npredt,max_tail), rstats_t(npredt,max_tail),varA_t(npredt,max_tail))
    varA_t = zero
    ostats_t = zero_quad
    rstats_t = zero_quad

    j=0
    do k=1,nlines
       read(lunin,100) cflg,crecord
       if (cflg == '!') cycle
       j=j+1
       read(crecord,*) taillist(j),idx_tail(j),(predr(ip),ip=1,npredt),(ostatsx(ip),ip=1,npredt), &
                       (varx(ip),ip=1,npredt),timelist(j)
       if (print_verbose) write(6,110) taillist(j),idx_tail(j),(predr(ip),ip=1,npredt), &
                    (ostatsx(ip),ip=1,npredt),(varx(ip),ip=1,npredt),timelist(j)
       do ip=1,npredt
          ostats_t(ip,j)=ostatsx(ip)
          predt(ip,j)=predr(ip)
          varA_t(ip,j)=varx(ip)
       end do
    end do
    close(lunin)
100 format(a1,a150)
110 format(a10,1x,i5,9(1x,f12.6),1x,i8)

!   Do not update aircraft temperature bias at 6Z and 18Z
    if (.not. upd_aircraft) then 
       anal_time = iadate(4)
       if (anal_time==6 .or. anal_time==18) upd_pred_t = zero
       if (mype==0) print*, 'aircraft_info anal_time upd_pred_t=', anal_time, upd_pred_t
    end if

!   Sort the tail number based on the first char
    cb0 = ' '
    isort = 0
    do k=1,ntail
       tailwk = trim(taillist(k))
       cb = tailwk(1:1)
       if (cb /= cb0) then
          isort = isort+1
          itail_sort(isort) = cb
          idx_sort(isort) = k
          cb0 = cb
       end if 
    end do
    nsort = isort
    if (mype==0) print*, 'nsort = ', nsort
!   do k=1,nsort
!      print*, itail_sort(k),idx_sort(k)
!   end do
  end subroutine aircraftinfo_read


  subroutine aircraftinfo_write
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    aircraftinfo_write
!
!   prgrmmr:     zhu        org: np20                date: 2013-05-17
!
! abstract:  This routine writes the tail number and bias correction info
!
! program history log:
!   2013-05-17  Yanqiu Zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

    use constants, only: zero
    use obsmod, only: iadate
    implicit none

    character(40),allocatable,dimension(:) :: csort
    integer,allocatable,dimension(:) :: idx_csort

    integer(i_kind) i,j,jj,lunout
    integer(i_kind) iyyyymm,obsolete
!   real(r_kind),dimension(npredt):: varx

    data lunout / 51 /

    open(lunout,file='aircftbias_out',form='formatted')
    rewind lunout

!   append new tail numbers at the end of the original list
!   do j=1,ntail_update
!      do i=1,npredt
!         varx(i)=varA_t(i,j)
!      end do
!      write(lunout,'(1x,a10,1x,i5,10(1x,f10.4))') &
!           taillist(j),idx_tail(j),(predt(i,j),i=1,npredt),(ostats_t(i,j),i=1,npredt),(varx(i),i=1,npredt)
!   end do

    print*, 'ntail=', ntail, ' ntail_update=',ntail_update
    allocate(csort(ntail_update),idx_csort(ntail_update))

!   sorting in alphabetic order with new tail numbers
    do i=1,ntail_update
       csort(i) = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
       idx_csort(i) = i
    end do
    do i=1,ntail_update
       csort(i) = taillist(i)
    end do 
!   cleanup tailnumber in the aircraft bias file
    obsolete = 0
    if (cleanup_tail) then
       iyyyymm = iadate(1)*100+iadate(2)
       do i=1,ntail_update
          if (abs(iyyyymm-timelist(i))>=100) then 
             csort(i) = 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
             obsolete = obsolete+1
          end if
       end do
    end if
    call indexc40(ntail_update,csort,idx_csort)

    do jj=1,ntail_update-obsolete
       j = idx_csort(jj)
       write(lunout,'(1x,a10,1x,i5,9(1x,f12.6),1x,i7)') &
            taillist(j),jj,(predt(i,j),i=1,npredt), &
            (ostats_t(i,j),i=1,npredt),(varA_t(i,j),i=1,npredt),timelist(j)
    end do

    close(lunout)
    deallocate(predt)
    deallocate(ostats_t,rstats_t,varA_t)

    deallocate(csort,idx_csort)
  end subroutine aircraftinfo_write


!$$$  subprogram documentation block
!
! subprogram: indexc40
!   Programmer: D. Keyser       Org: NP22       Date: 2012-05-08
!
! Abstract: Uses efficient sort algorithm to produce index sort list for a 40-character
!   array.  Does not rearrange the file.
!
! Program History Log:
! 1993-06-05  R  Kistler -- FORTRAN version of C-program
! 1993-07-15  P. Julian  -- Modified to sort 12-character array
! 1994-08-25  D. Keyser  -- Modified to sort 16-character array
! 1995-05-30  D. Keyser  -- Tests for < 2 elements in sort list, if so returns without
!                            sorting (but fills indx array)
! ????-??-??  P. M. Pauley (NRL) -- Size of carrin changed to character*24
! 2010-11-15  S. Bender  -- Size of carrin changed to character*40
! 2012-05-08  D. Keyser  -- Prepared for operational implementation
!
! Usage:    call indexc40(n,carrin,indx)
!
!   Input argument list:
!     n        - Size of array to be sorted
!     carrin   - 40-character array to be sorted
!
!   Output argument list:
!     indx     - Array of pointers giving sort order of carrin in ascending order {e.g.,
!                carrin(indx(i)) is sorted in ascending order for original i = 1, ... ,n}
!
! Remarks: Called by main program.
!
! Attributes:
!   Language: FORTRAN 90
!   Machine:  NCEP WCOSS
!
!$$$
      subroutine indexc40(n,carrin,indx)

      implicit none

      integer      n, &              ! dimension of array to be sorted
                   j, &              ! do loop index, sort variable
                   i, &              ! sort variable
                   l, &              ! variable used to decide if sort is finished
                   ir, &             !           "                 "
                   indx(n), &        ! pointer array
                   indxt             ! pointer used in sort

      character*40 carrin(n), &      ! input array to be sorted
                   cc                ! character variable used in sort

! # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

      do j = 1,n
        indx(j) = j
      enddo

! Must be > 1 element in sort list, else return
! ---------------------------------------------

      if(n.le.1)  return

      l = n/2 + 1
      ir = n

      do 
         if(l.gt.1) then
            l = l - 1
            indxt = indx(l)
            cc = carrin(indxt)
         else
            indxt = indx(ir)
            cc = carrin(indxt)
            indx(ir) = indx(1)
            ir = ir - 1
            if(ir.eq.1) then
               indx(1) = indxt
               return
            endif
         endif

         i = l
         j = l * 2

         do 
            if(j.le.ir)  then
              if(j.lt.ir)  then
                if(carrin(indx(j)).lt.carrin(indx(j+1)))  j = j + 1
              endif
              if(cc.lt.carrin(indx(j))) then
                indx(i) = indx(j)
                i = j
                j = j + i
              else
                j = ir + 1
              endif
            endif

            if(j > ir) exit
         end do
         indx(i) = indxt
      end do

      end subroutine indexc40

end module aircraftinfo
