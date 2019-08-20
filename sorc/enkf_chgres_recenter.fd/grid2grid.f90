module grid2grid

  use nemsio_module

  implicit none

  private

  integer, public                              :: idvc, idsl, idvm, nvcoord
  integer, public                              :: ntrac, ncldt,icldamt
  integer, public                              :: ij_input, kgds_input(200)
  integer(nemsio_intkind), public              :: i_input, j_input, lev
  integer(nemsio_intkind), public              :: idate(7)

  logical, public                              :: gfdl_mp

  ! ----------------------------------------------------------------------

  ! Fields on target grid before atmospheric readjustment

  real, pointer, public      :: sfcp_b4_adj_output(:)
  real, pointer, public      :: hgt_b4_adj_output(:)

  real, pointer, public      :: ugrd_b4_adj_output(:,:)
  real, pointer, public      :: vgrd_b4_adj_output(:,:)

  real, pointer, public      :: tmp_b4_adj_output(:,:)
  real, pointer, public      :: dzdt_b4_adj_output(:,:)

  real, pointer, public      :: q_b4_adj_output(:,:,:)

  ! Pointers to subarrays of q_b4_adj_output:
  real, pointer, public      :: spfh_b4_adj_output(:,:)
  real, pointer, public      :: o3mr_b4_adj_output(:,:)
  real, pointer, public      :: clwmr_b4_adj_output(:,:)
  real, pointer, public      :: rwmr_b4_adj_output(:,:)
  real, pointer, public      :: icmr_b4_adj_output(:,:)
  real, pointer, public      :: snmr_b4_adj_output(:,:)
  real, pointer, public      :: grle_b4_adj_output(:,:)
  real, pointer, public      :: cldamt_b4_adj_output(:,:)


  ! ----------------------------------------------------------------------
  ! Fields on target grid after atmospheric readjustment

  real, pointer, public         :: hgt_output(:)  ! interpolated from input grid
  real, pointer, public         :: hgt_external_output(:)

  real, pointer, public      :: sfcp_output(:)

  real, pointer, public      :: ugrd_output(:,:)
  real, pointer, public      :: vgrd_output(:,:)

  real, pointer, public      :: tmp_output(:,:)
  real, pointer, public         :: delz_output(:,:)
  real, pointer, public         :: dpres_output(:,:)
  real, pointer, public         :: dzdt_output(:,:)
  
  real, pointer, public      :: q_output(:,:,:)

  ! Pointers to subarrays of q_output:
  real, pointer, public      :: spfh_output(:,:)
  real, pointer, public      :: o3mr_output(:,:)
  real, pointer, public      :: clwmr_output(:,:)
  real, pointer, public      :: rwmr_output(:,:)
  real, pointer, public      :: icmr_output(:,:)
  real, pointer, public      :: snmr_output(:,:)
  real, pointer, public      :: grle_output(:,:)
  real, pointer, public      :: cldamt_output(:,:)

  real, pointer, public         :: rlat_output(:)
  real, pointer, public         :: rlon_output(:)

  ! ----------------------------------------------------------------------

  real, pointer, public                    :: vcoord(:,:)
  integer, public                   :: kgds_output(200)

  public                                       :: read_vcoord_info
  public                                       :: hinterp_grid2grid
  public                                       :: adjust_for_terrain
  public                                       :: set_output_grid

  ! Module-local variables, to reduce code complexity.


  integer :: ip, ipopt(20)
  integer :: ibi(1), ibo(1)
  logical*1, allocatable :: bitmap_input(:,:),bitmap_output(:,:)
  real, allocatable       :: crot(:), srot(:)

  !----------------------------------------------------------------------

contains

  !----------------------------------------------------------------------

  subroutine adjust_for_terrain

    ! Adjust fields based on differences between the interpolated and
    ! external terrain.

    use utils
    use setup

    implicit none

    integer               :: k, t

    real, allocatable     :: pres_b4_adj_output(:,:)
    real, allocatable     :: pres_output(:,:)

    ! First, compute the mid-layer pressure using the interpolated
    ! surface pressure.

    allocate(pres_b4_adj_output(ij_output,lev))
    pres_b4_adj_output = 0.0

    print*
    print*,"COMPUTE MID-LAYER PRESSURE FROM INTERPOLATED SURFACE PRESSURE."
    call newpr1(ij_output, lev, idvc, idsl, nvcoord, vcoord, &
         sfcp_b4_adj_output, pres_b4_adj_output)

    ! Adjust surface pressure based on differences between
    ! interpolated and grid terrain.

    allocate(sfcp_output(ij_output))
    sfcp_output = 0.0


20  format('TARGET GRID: MAX/MIN ',A,' AT LEVEL ',I0,' IS: ',F17.10,' ',F17.10)
30  format('TARGET GRID: MAX/MIN ',A,': ',F17.10,' ',F17.10)

40  format('OUTPUT: MAX/MIN ',A,' AT LEVEL ',I0,': ',F17.10,' ',F17.10)
50  format('OUTPUT: MAX/MIN TRACER #',I0,' AT LEVEL ',I0,': ',F17.10,' ',F17.10)

    call printrusage
    print*,"ADJUST SURFACE PRESSURE BASED ON TERRAIN DIFFERENCES"
    call newps(hgt_output, sfcp_b4_adj_output, ij_output, &
         lev, pres_b4_adj_output, tmp_b4_adj_output, &
         spfh_b4_adj_output, hgt_external_output, sfcp_output)
    ! print 30,'SURFACE PRESSURE B4 ADJ',maxval(sfcp_b4_adj_output),minval(sfcp_b4_adj_output)
    ! print 30,'SURFACE PRESSURE OUTPUT',maxval(sfcp_output),minval(sfcp_output)
    ! do k=1,lev
    !    print 20,'TEMPERATURE B4 ADJ',k,maxval(tmp_b4_adj_output(:,k)),minval(tmp_b4_adj_output(:,k))
    ! enddo
    ! do k=1,lev
    !    print 20,'SPFH B4 ADJ',k,maxval(spfh_b4_adj_output(:,k)),minval(spfh_b4_adj_output(:,k))
    ! enddo

    deallocate(sfcp_b4_adj_output)

    ! Recompute mid-layer pressure based on the adjusted surface
    ! pressure.

    allocate(pres_output(ij_output, lev))
    pres_output = 0.0

    allocate(dpres_output(ij_output, lev))
    dpres_output = 0.0

    call printrusage
    print*,"RECOMPUTE MID-LAYER PRESSURE."
    call newpr1(ij_output, lev, idvc, idsl, nvcoord, vcoord, &
         sfcp_output, pres_output, dpres_output)
    ! do k=1,lev
    !    print 20,'DPRES MID-LAYER PRESSURE OUTPUT',k,maxval(dpres_output(:,k)),minval(dpres_output(:,k))
    ! enddo

    ! Vertically interpolate from the pre-adjusted to the adjusted
    ! mid-layer pressures.

    allocate(q_output(ij_output,lev,ntrac))
    q_output = 0.0

    allocate(dzdt_output(ij_output,lev))
    dzdt_output = 0.0

    allocate(ugrd_output(ij_output,lev))
    ugrd_output=0.0

    allocate(vgrd_output(ij_output,lev))
    vgrd_output=0.0

    allocate(tmp_output(ij_output,lev))
    tmp_output=0.0

    call printrusage
    print*,"VERTICALLY INTERPOLATE TO NEW PRESSURE LEVELS"
    call vintg(i_output, j_output, lev, lev, ntrac, pres_b4_adj_output,  &
         ugrd_b4_adj_output, vgrd_b4_adj_output, tmp_b4_adj_output, q_b4_adj_output,  &
         dzdt_b4_adj_output, pres_output, ugrd_output, vgrd_output, tmp_output, &
         q_output, dzdt_output)

    ! do k=1,lev
    !    print *,'TEMPERATURE ',k,' ',maxval(tmp_output(:,k)),' ',minval(tmp_output(:,k))
    !    print *,'U WIND ',k,' ',maxval(ugrd_output(:,k)),' ',minval(ugrd_output(:,k))
    !    print *,'V WIND ',k,' ',maxval(vgrd_output(:,k)),' ',minval(vgrd_output(:,k))
    !    print *,'DZDT ',k,' ',maxval(dzdt_output(:,k)),' ',minval(dzdt_output(:,k))
    !    do t=1,ntrac
    !       print *,'TRACER ',t,' ',k,' ',maxval(q_output(:,k,t)),' ',minval(q_output(:,k,t))
    !    enddo
    ! enddo

    deallocate (dzdt_b4_adj_output,  q_b4_adj_output)
    deallocate (pres_b4_adj_output, pres_output)

    spfh_output => q_output(:,:,1)
    o3mr_output => q_output(:,:,2)
    clwmr_output => q_output(:,:,3)
    if (gfdl_mp) then
       rwmr_output => q_output(:,:,4)
       icmr_output => q_output(:,:,5)
       snmr_output => q_output(:,:,6)
       grle_output => q_output(:,:,7)
       if (icldamt == 1) then
          cldamt_output => q_output(:,:,8)
       endif
    endif

    deallocate(hgt_b4_adj_output)
    deallocate(ugrd_b4_adj_output)
    deallocate(vgrd_b4_adj_output)
    deallocate(tmp_b4_adj_output)

    nullify(sfcp_b4_adj_output,hgt_b4_adj_output,ugrd_b4_adj_output)
    nullify(vgrd_b4_adj_output,tmp_b4_adj_output,dzdt_b4_adj_output)
    nullify(q_b4_adj_output,spfh_b4_adj_output,o3mr_b4_adj_output)
    nullify(clwmr_b4_adj_output,rwmr_b4_adj_output,icmr_b4_adj_output)
    nullify(snmr_b4_adj_output,grle_b4_adj_output,cldamt_b4_adj_output)

    allocate(delz_output(ij_output, lev))
    delz_output = 0.0

    call printrusage
    print *,'COMPUTE DELZ'
    call compute_delz(ij_output, lev, vcoord(:,1), vcoord(:,2), sfcp_output, hgt_output, &
         tmp_output, spfh_output, delz_output)

    deallocate(hgt_output)

  end subroutine adjust_for_terrain

  ! ----------------------------------------------------------------------

  subroutine set_output_grid

    !-------------------------------------------------------------------
    ! Set grid specs on the output grid.
    !-------------------------------------------------------------------

    use setup
    use utils

    implicit none

    character(len=20)                    :: vlevtyp, vname

    integer(nemsio_intkind)              :: vlev
    integer                              :: iret

    real(nemsio_realkind), allocatable   :: dummy(:)

    type(nemsio_gfile)                   :: gfile

    print*
    print*,"OUTPUT GRID I/J DIMENSIONS: ", i_output, j_output

    !-------------------------------------------------------------------
    ! Set the grib 1 grid description section, which is needed
    ! by the IPOLATES library.
    !-------------------------------------------------------------------

    kgds_output = 0

    call calc_kgds(i_output, j_output, kgds_output)

    !-------------------------------------------------------------------
    ! Read the terrain on the output grid.  To ensure exact match,
    ! read it from an existing enkf nemsio restart file.
    !-------------------------------------------------------------------

    call nemsio_init(iret)

    print*
    print*,"OPEN OUTPUT GRID TERRAIN FILE: ", trim(terrain_file)
    call nemsio_open(gfile, terrain_file, "read", iret=iret)
    if (iret /= 0) then
       print*,"FATAL ERROR OPENING FILE: ",trim(terrain_file)
       print*,"IRET IS: ", iret
       call errexit(50)
    endif

    allocate(dummy(ij_output))
    allocate(hgt_external_output(ij_output))

    print*
    print*,"READ SURFACE HEIGHT"
    vlev    = 1
    vlevtyp = "sfc"
    vname   = "hgt"
    call nemsio_readrecv(gfile, vname, vlevtyp, vlev, dummy, 0, iret)
    if (iret /= 0) then
       print*,"FATAL ERROR READING FILE: ",trim(terrain_file)
       print*,"IRET IS: ", iret
       call errexit(51)
    endif

    hgt_external_output = dummy
    !print *,'MAX/MIN HGT_EXTERNAL_OUTPUT IS ',maxval(hgt_external_output),minval(hgt_external_output)

    deallocate(dummy)

    call nemsio_close(gfile, iret=iret)

    call nemsio_finalize()

  end subroutine set_output_grid

  ! ------------------------------------------------------------------------

  subroutine read_vcoord_info

    ! Read vertical coordinate information.
    use setup
    implicit none

    integer                    :: istat, levs_vcoord, n, k

    print*
    print*,"OPEN VERTICAL COORD FILE: ", trim(vcoord_file)
    open(14, file=trim(vcoord_file), form='formatted', iostat=istat)
    if (istat /= 0) then
       print*,"FATAL ERROR OPENING FILE. ISTAT IS: ", istat
       call errexit(4)
    endif

    read(14, *, iostat=istat) nvcoord, levs_vcoord
    if (istat /= 0) then
       print*,"FATAL ERROR READING FILE HEADER. ISTAT IS: ",istat
       call errexit(5)
    endif

    ! The last value in the file is not used for the fv3 core.  Only
    ! read the first (lev + 1) values.

    allocate(vcoord(lev+1, nvcoord))
    read(14, *, iostat=istat) ((vcoord(n,k), k=1,nvcoord), n=1,lev+1)
    if (istat /= 0) then
       print*,"FATAL ERROR READING FILE. ISTAT IS: ",istat
       call errexit(6)
    endif

    print*
    do k = 1, (lev+1)
       print*,'VCOORD FOR LEV ', k, 'IS: ', vcoord(k,:)
    enddo

    close(14)

  end subroutine read_vcoord_info

  !----------------------------------------------------------------------

  subroutine hinterp_grid2grid
    use setup
    use utils
    implicit none

    ! ----------------------------------------------------------------------
    ! Locals copied from read_input_data
    character(len=20)                    :: vlevtyp, vname
    character(len=50), allocatable       :: recname(:)

    integer(nemsio_intkind)              :: vlev, iret, idum, nrec
    integer                              :: n

    real, allocatable   :: dummy(:,:)
    real(nemsio_realkind), allocatable   :: readbuf(:)

    type(nemsio_gfile)                   :: gfile

    ! ----------------------------------------------------------------------
    ! Locals copied from gaus_to_gaus

    integer                 :: numpts

    ! ----------------------------------------------------------------------

    call nemsio_init(iret)

    print*
    print*,"OPEN INPUT FILE: ",trim(input_file)
    call nemsio_open(gfile, input_file, "read", iret=iret)
    if (iret /= 0) then
       print*,"FATAL ERROR OPENING FILE: ",trim(input_file)
       print*,"IRET IS: ", iret
       call errexit(2)
    endif


    print*,"GET INPUT FILE HEADER"
    call nemsio_getfilehead(gfile, iret=iret, nrec=nrec, idate=idate, & 
         dimx=i_input, dimy=j_input, dimz=lev)
    if (iret /= 0) goto 67

    print*,'DIMENSIONS OF DATA ARE: ', i_input, j_input, lev
    print*,'DATE OF DATA IS:        ', idate

    ij_input = i_input * j_input

    allocate(recname(nrec))

    call nemsio_getfilehead(gfile, iret=iret, recname=recname)
    if (iret /= 0) goto 67

    gfdl_mp = .false.   ! Zhao-Carr MP
    do n = 1, nrec
       if (trim(recname(n)) == "icmr") then
          gfdl_mp = .true.    ! GFDL MP
          exit
       endif
    enddo

    icldamt = 0
    do n = 1, nrec
       if (trim(recname(n)) == "cld_amt") then
          icldamt = 1        ! 3D cloud amount present
          exit
       endif
    enddo

    ! Read header info

    call nemsio_getfilehead(gfile, iret=iret, idvc=idum)
    if (iret /= 0) goto 67
    idvc = idum
    print*,'IDVC IS: ', idvc

    call nemsio_getfilehead(gfile, iret=iret, idsl=idum)
    if (iret /= 0) goto 67
    idsl = idum
    print*,'IDSL IS: ', idsl

    call nemsio_getfilehead(gfile, iret=iret, idvm=idum)
    if (iret /= 0) goto 67
    idvm = idum
    print*,'IDVM IS: ', idvm

    if (gfdl_mp) then
       ntrac = 7 + icldamt
       ncldt = 5
    else
       ntrac = 3
       ncldt = 1
    endif

    ! ----------------------------------------------------------------------
    ! Set up for reading and interpolating.

    allocate(dummy(ij_input,2))
    allocate(readbuf(ij_input))

    call printrusage
    print*
    print*,'READ AND INTERPOLATE DATA TO OUTPUT GRID'

    call calc_kgds(i_input, j_input, kgds_input)

    ip    = 0   ! bilinear
    ipopt = 0
    ibi = 0 ! no bitmap
    ibo = 0 ! no bitmap

    allocate(bitmap_input(ij_input,2))
    bitmap_input = .true.
    allocate(bitmap_output(ij_output,2))
    bitmap_output = .true.

    allocate(rlat_output(ij_output))
    rlat_output = 0.0
    allocate(rlon_output(ij_output))
    rlon_output = 0.0

    call read_vcoord_info

    ! ----------------------------------------------------------------------
    ! Read and interpolate level-by-level, field-by-field.

    ! Interpolate surface fields

    call printrusage

    allocate(sfcp_b4_adj_output(ij_output))
    call grid2grid_surface('SURFACE PRESSURE',gfile,readbuf,'pres','sfc',dummy(:,1),&
         sfcp_b4_adj_output,iret)
    if(iret/=0) goto 67

    allocate(hgt_b4_adj_output(ij_output))
    call grid2grid_surface('SURFACE HEIGHT',gfile,readbuf,'hgt','sfc',dummy(:,1),&
         hgt_b4_adj_output,iret)
    if(iret/=0) goto 67
    allocate(hgt_output(ij_output))
    hgt_output=hgt_b4_adj_output

    ! Interpolate velocity fields

    allocate(crot(ij_output), srot(ij_output))
    crot = 0.
    srot = 0.
    allocate(ugrd_b4_adj_output(ij_output,lev))
    allocate(vgrd_b4_adj_output(ij_output,lev))
    call grid2grid_vector('U WIND','V WIND',gfile,readbuf,'ugrd','vgrd','mid layer',&
         dummy(:,1),dummy(:,2),ugrd_b4_adj_output,vgrd_b4_adj_output,iret)
    if(iret/=0) goto 67

    ! Interpolate non-species 3D scalars

    call printrusage
    allocate(tmp_b4_adj_output(ij_output,lev))
    call grid2grid_scalar('TEMPERATURE',gfile,readbuf,'tmp','mid layer',dummy(:,1),&
         tmp_b4_adj_output,iret)
    if(iret/=0) goto 67

    allocate(dzdt_b4_adj_output(ij_output,lev))
    call grid2grid_scalar('DZDT',gfile,readbuf,'dzdt','mid layer',dummy(:,1),&
         dzdt_b4_adj_output,iret)
    if(iret/=0) goto 67

    call printrusage
    ! Interpolate tracers
    allocate(q_b4_adj_output(ij_output,lev,ntrac))

    spfh_b4_adj_output=>q_b4_adj_output(:,:,1)
    call grid2grid_scalar('SPECIFIC HUMIDITY',gfile,readbuf,'spfh','mid layer',&
         dummy(:,1),spfh_b4_adj_output,iret)
    if(iret/=0) goto 67

    o3mr_b4_adj_output=>q_b4_adj_output(:,:,2)
    call grid2grid_scalar('OZONE',gfile,readbuf,'o3mr','mid layer',&
         dummy(:,1),o3mr_b4_adj_output,iret)
    if(iret/=0) goto 67

    clwmr_b4_adj_output=>q_b4_adj_output(:,:,3)
    call grid2grid_scalar('CLOUD LIQUID WATER',gfile,readbuf,'clwmr','mid layer',&
         dummy(:,1),clwmr_b4_adj_output,iret)
    if(iret/=0) goto 67

    if(gfdl_mp) then
       rwmr_b4_adj_output=>q_b4_adj_output(:,:,4)
       call grid2grid_scalar('RAIN MIXING RATIO',gfile,readbuf,'rwmr','mid layer',&
            dummy(:,1),rwmr_b4_adj_output,iret)
       if(iret/=0) goto 67

       icmr_b4_adj_output=>q_b4_adj_output(:,:,5)
       call grid2grid_scalar('ICE MIXING RATIO',gfile,readbuf,'icmr','mid layer',&
            dummy(:,1),icmr_b4_adj_output,iret)
       if(iret/=0) goto 67

       snmr_b4_adj_output=>q_b4_adj_output(:,:,6)
       call grid2grid_scalar('SNOW MIXING RATIO',gfile,readbuf,'snmr','mid layer',&
            dummy(:,1),snmr_b4_adj_output,iret)
       if(iret/=0) goto 67

       grle_b4_adj_output=>q_b4_adj_output(:,:,7)
       call grid2grid_scalar('GRAUPEL MIXING RATIO',gfile,readbuf,'grle','mid layer',&
            dummy(:,1),grle_b4_adj_output,iret)
       if(iret/=0) goto 67

       if(icldamt==1) then
          cldamt_b4_adj_output=>q_b4_adj_output(:,:,8)
          call grid2grid_scalar('3D CLOUD AMOUNT',gfile,readbuf,'cld_amt','mid layer',&
               dummy(:,1),cldamt_b4_adj_output,iret)
          if(iret/=0) goto 67
       endif
       ! print *,'bottom of hinterp_grid2grid'
       call printrusage
    endif

    deallocate(dummy,readbuf,recname,bitmap_input,bitmap_output)

    return

67  continue
    
    print*,"FATAL ERROR PROCESSING INPUT FILE: ", trim(input_file)
    print*,"IRET IS: ", iret
    call errexit(3)

  end subroutine hinterp_grid2grid

  ! ----------------------------------------------------------------------

  subroutine grid2grid_vector(&
       u_human_name,v_human_name,&
       gfile,readbuf,uname,vname,vlevtyp,&
       u_dummy,v_dummy,&
       u_b4_adj_output,v_b4_adj_output,iret)
    use setup
    implicit none
    character(len=*) :: u_human_name,v_human_name
    type(nemsio_gfile) :: gfile
    real(nemsio_realkind) :: readbuf(:)
    character(len=*) :: uname, vname, vlevtyp
    real :: u_dummy(:), v_dummy(:)
    real :: u_b4_adj_output(:,:), v_b4_adj_output(:,:)
    integer :: iret
    !locals
    integer :: vlev,num_fields,numpts
    character(len=20) :: uname_x, vname_x, vlevtyp_x

    uname_x=uname
    vname_x=vname
    vlevtyp_x=vlevtyp

    num_fields=1

10  format(A,': READ ',A,' AND ',A)
20  format(A,': SOURCE GRID: MAX/MIN ',A,' AT LEVEL ',I0,' IS: ',F17.10,' ',F17.10)
30  format(A,': TARGET GRID: MAX/MIN ',A,' AT LEVEL ',I0,' IS: ',F17.10,' ',F17.10)

    print 10,trim(input_file),u_human_name,v_human_name

    do vlev=1,lev
       readbuf=-999.
       call nemsio_readrecv(gfile, uname_x, vlevtyp_x, vlev, readbuf, 0, iret)
       if(iret/=0) return
       u_dummy=readbuf
       
       readbuf=-999.
       call nemsio_readrecv(gfile, vname_x, vlevtyp_x, vlev, readbuf, 0, iret)
       if(iret/=0) return
       v_dummy=readbuf

       print 20,trim(input_file),u_human_name,vlev,maxval(u_dummy),minval(u_dummy)
       print 20,trim(input_file),v_human_name,vlev,maxval(v_dummy),minval(v_dummy)

       u_b4_adj_output(:,vlev)=-999.
       v_b4_adj_output(:,vlev)=-999.
       call ipolatev(&
            ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
            num_fields, ibi, bitmap_input, u_dummy, v_dummy,  &
            numpts, rlat_output, rlon_output, crot, srot, ibo, bitmap_output, &
            u_b4_adj_output(:,vlev), v_b4_adj_output(:,vlev), iret)
       if(iret/=0) return
       !print 30,trim(input_file),u_human_name,vlev,maxval(u_b4_adj_output(:,vlev)),minval(u_b4_adj_output(:,vlev))
       !print 30,trim(input_file),v_human_name,vlev,maxval(v_b4_adj_output(:,vlev)),minval(v_b4_adj_output(:,vlev))
    end do
  end subroutine grid2grid_vector

  ! ----------------------------------------------------------------------

  subroutine grid2grid_scalar(&
       human_name,gfile,readbuf,vname,vlevtyp,dummy,b4_adj_output,iret)
    use setup
    implicit none
    character(len=*) :: human_name
    type(nemsio_gfile) :: gfile
    real(nemsio_realkind) :: readbuf(:)
    character(len=*) :: vname, vlevtyp
    real :: dummy(:), b4_adj_output(:,:)
    integer :: iret
    !locals
    integer :: vlev,num_fields,numpts
    character(len=20) :: vname_x, vlevtyp_x

    vname_x=vname
    vlevtyp_x=vlevtyp

    num_fields=1

10  format(A,': READ ',A)
20  format(A,': SOURCE GRID: MAX/MIN ',A,' AT LEVEL ',I0,' IS: ',F17.10,' ',F17.10)
30  format(A,': TARGET GRID: MAX/MIN ',A,' AT LEVEL ',I0,' IS: ',F17.10,' ',F17.10)

    print 10,trim(input_file),human_name

    do vlev=1,lev

       readbuf=-999.
       call nemsio_readrecv(gfile, vname, vlevtyp, vlev, readbuf, 0, iret)
       if(iret/=0) return
       dummy=readbuf

       print 20,trim(input_file),human_name,vlev,maxval(dummy),minval(dummy)
       call ipolates(&
            ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
            num_fields, ibi, bitmap_input, dummy,  &
            numpts, rlat_output, rlon_output, ibo, bitmap_output, &
            b4_adj_output(:,vlev), iret)
       !print 30,trim(input_file),human_name,vlev,maxval(b4_adj_output(:,vlev)),minval(b4_adj_output(:,vlev))
       if(iret/=0) return
    end do
  end subroutine grid2grid_scalar

  ! ----------------------------------------------------------------------

  subroutine grid2grid_surface(&
       human_name,gfile,readbuf,vname,vlevtyp,dummy,b4_adj_output,iret)
    use setup
    implicit none
    character(len=*) :: human_name
    type(nemsio_gfile) :: gfile
    real(nemsio_realkind) :: readbuf(:)
    character(len=*) :: vname, vlevtyp
    real :: dummy(:), b4_adj_output(:)
    integer :: iret
    !locals
    integer :: vlev,num_fields,numpts
    character(len=20) :: vname_x, vlevtyp_x

    vname_x=vname
    vlevtyp_x=vlevtyp

    num_fields=1

10  format(A,': READ ',A)
20  format(A,': SOURCE GRID: MAX/MIN ',A,': ',F17.10,' ',F17.10)
30  format(A,': TARGET GRID: MAX/MIN ',A,': ',F17.10,' ',F17.10)

    print 10,trim(input_file),human_name

    vlev=1

    readbuf=-999.
    call nemsio_readrecv(gfile, vname, vlevtyp, vlev, readbuf, 0, iret)
    if(iret/=0) return
    dummy=readbuf

    print 20,trim(input_file),human_name,maxval(dummy),minval(dummy)
    call ipolates(&
         ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
         num_fields, ibi, bitmap_input, dummy,  &
         numpts, rlat_output, rlon_output, ibo, bitmap_output, &
         b4_adj_output, iret)
    if(iret/=0) return
    !print 30,trim(input_file),human_name,maxval(b4_adj_output),minval(b4_adj_output)
  end subroutine grid2grid_surface

end module grid2grid
