module get_wrf_binary_interface_mod
use abstract_get_wrf_binary_interface_mod
  type, extends(abstract_get_wrf_binary_interface_class) :: get_wrf_binary_interface_class
  contains
    procedure, pass(this) :: convert_binary_mass => convert_binary_mass_wrf
    procedure, pass(this) :: convert_binary_nmm => convert_binary_nmm_wrf
    procedure, pass(this) :: convert_nems_nmmb => convert_nems_nmmb_wrf
    procedure, nopass :: latlon2radians
    procedure, pass(this) :: count_recs_wrf_binary_file
    procedure, pass(this) :: inventory_wrf_binary_file
    procedure, pass(this) :: initialize_byte_swap_wrf_binary_file
    procedure, pass(this) :: int_get_ti_header_char
    procedure, pass(this) :: int_get_write_field_header 
    procedure, nopass :: retrieve_index
    procedure, nopass :: retrieve_field_i1
    procedure, nopass :: retrieve_field_r1
    procedure, nopass :: retrieve_field_rn1
    procedure, nopass :: retrieve_field_rn1n2
    procedure, nopass :: next_buf 
  end type get_wrf_binary_interface_class
contains
  subroutine convert_binary_mass_wrf(this)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_binary_mass   read wrf mass binary restart 
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: using wrf library routines, read a wrf mass core binary
  !             format restart file.  write the result to temporary binary
  !             file expected by read_wrf_mass_guess.
  !
  ! program history log:
  !   2004-09-10  parrish
  !   2004-10-26  wu - date info from the field (T) instead of START_DATE, 
  !                    current time is necessary for cycling.
  !   2004-11-05  wu - add check on input wrf guess file, stop code if problem
  !   2004-11-07  parrish - change so byte offset information is written, instead 
  !                            of a whole new binary file--this done so mpi-io
  !                            can be used later to read/write directly from
  !                            the wrf mass core binary file.  also, do inventory
  !                            of whole file, so offsets are general--not dependent
  !                            on type of file.
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-07-06  parrish - add read of pint byte address
  !   2005-11-29  parrish - add changes to allow earlier reading of surface fields needed
  !                         for intelligent thinning of satellite data.
  !   2006-04-06  middlecoff - changed out_unit from 55 to lendian_out
  !   2006-09-15  treadon - use nhr_assimilation to build local guess filename
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2010-06-24  Hu  - bug fix: replace XICE with SEAICE
  !   2010-06-24  Hu  - add code to read 5 cloud/hydrometeor variables for cloud analysis
  !   2010-11-16  tong - - add loop to read upto 7 wrf mass binary restart file and
  !                        write to temporary binary files (extend FGAT capability for
  !                        wrf mass binary format)
  !   2012-10-11  parrish - move line "write(filename,'("sigf",i2.2)')n+nhr_assimilation-1" so input names
  !                           sigfxx are properly defined for all values of n, not just n=1.
  !   2012-10-11  parrish - add call to initialize_byte_swap_wrf_binary_file routine, and also add this
  !                           subroutine to this file.
  !   2012-11-26  Hu  - add code to read surface variables for GSD soil nudging
  !   2013-01-29  parrish - replace retrieve_field calls with retrieve_field_r1, retrieve_field_rn1,
  !                           retrieve_field_rn1n2 (so debug compile works on WCOSS)
  !   2013-04-23  parrish - add internal check for types of GLAT/GLON
  !   2013-05-14  guo     - added #ifdef WRF arround "call initialize_byte_swap_wrf_binary_file()".
  !   2017-03-23  Hu  - add code to read hybrid vertical coodinate in WRF MASS
  !                         core
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  ! COMMENTS ON MPI I-O AS USED TO READ AND WRITE FROM WRF BINARY FORMAT FILES:
  
  !   The wrf binary format model files are sequential unformatted files.  There is a sequence
  !   of 2048 byte informational records, which are sometimes followed by a record with a 
  !   variable specified by an identifier in the preceding informational record.
  
  !   The first step, carried out in this routine, convert_binary_mass (or convert_binary_nmm),
  !   is to do an inventory, by reading the file opened for direct access.  Sequential files
  !   have a 4 leading and trailing bytes for each record, containing the length of the record
  !   in bytes.  These are first counted by subroutine count_recs_wrf_binary_file to get 
  !   the total number of records on the file, and then an inventory is created with 
  !   subroutine inventory_wrf_binary_file.  This creates a list of field identifiers for
  !   everything contained on the file, and byte offsets to the beginning of each field
  !   relative to the start of the file (taking into account the 4-byte leading and trailing
  !   record markers also).  This inventory information is then scanned to select the 
  !   variables required by the analysis, and the corresponding byte offsets are written to
  !   a temporary file for later access by subroutines read_wrf_mass_binary_guess, 
  !   wrwrf_mass_binary, etc., all of which read and write to the wrf binary file using
  !   mpi i-o routines.
  !   This initial inventory is very fast because of the direct access reads, with record length 
  !   set to the optimal value for the disks.  Also, not all direct access records are read, but 
  !   only those which contain sequential record marks.
  !
  !   The mpi i-o routines as used here are quite simple.  Taking the wrf file as a contiguous
  !   string of bytes, any piece of the file, characterized by a byte offset from the start of
  !   the file, and a length, can be read or written to/from any processor, as long as one is
  !   careful not to write into the same area from different processors at the same time.
  !
  !   The routines used here are
  
  !   mpi_file_read_at and mpi_file_write_at
  
  !   which are blocking commands, ie, after the call, the buffer used to transfer bytes to/from
  !   memory is free to be used.  See subroutines read_wrf_mass_binary_guess, read_wrf_nmm_binary_guess,
  !   wrwrfnmma_binary, and wrwrfmassa_binary.
  
  !   These routines have been made a bit more complicated due to the ordering of 3-d variables
  !   on the wrf files.  They are currently stored in ikj format, but the analysis requires reading
  !   and writing in ijk format.  To get around this in an efficient manner, 3-d variables are 
  !   read in in ik groups for a range of j-index on each processor, where the j-index is evenly
  !   distributed across all processors.  Once the read is complete, an mpi_alltoallv command is
  !   used to redistribute so the k-index is distributed evenly across processors, and each processor
  !   has a subset of full ij fields over its designated k-index range.
  
    use kinds, only: r_single,i_llong,i_kind
    use gsi_4dvar, only: nhr_assimilation
    use gsi_io, only: lendian_out, verbose
    use rapidrefresh_cldsurf_mod, only: l_hydrometeor_bkio,l_gsd_soilTQ_nudge
    use gsi_metguess_mod, only: gsi_metguess_get
    use gridmod, only: wrf_mass_hybridcord
    implicit none
    class(get_wrf_binary_interface_class), intent(inout) :: this
  
  ! Declare local parameters
    integer(i_kind),parameter:: in_unit = 15
    real(r_single),parameter:: one_single = 1.0_r_single
    real(r_single),parameter:: r45 = 45.0_r_single
    
    character(6) filename
    character(9) wrfges
    integer(i_kind),allocatable:: start_block(:),end_block(:)
    integer(i_kind),allocatable:: start_byte(:),end_byte(:)
    integer(i_llong),allocatable:: file_offset(:)
    integer(i_llong) n_position
    character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
    integer(i_kind),allocatable:: domainend_all(:,:)
    integer(i_kind) nrecs
    integer(i_kind) status_hdr
    integer(i_kind) hdrbuf(512)
  
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nsig_soil_regional
    real(r_single) pt_regional
    integer(i_kind) k,n
    integer(i_kind) n_actual_clouds,istatus
    real(r_single),allocatable::field1(:),field1p(:),field2(:,:),field2b(:,:),field2c(:,:)
    real(r_single),allocatable::field1a(:),field1pa(:)
    real(r_single) rad2deg_single
    real(r_single)rdx,rdy
    integer(i_kind) ksize
    integer(i_kind) index
    logical print_verbose

    print_verbose=.false.
    if(verbose)print_verbose=.true.
  
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
  
    n_loop: do n=1,9  ! loop over forecast hours in assim interval
  
       if(n==nhr_assimilation)then
          wrfges = 'wrf_inout'
       else
          write(wrfges,'("wrf_inou",i1.1)')n
       endif
  
       write(filename,'("sigf",i2.2)')n
  
       open(in_unit,file=wrfges,form='unformatted')
       if(print_verbose)then
          write(6,*)' convert_binary_mass: in_unit,lendian_out=',in_unit,lendian_out
          write(6,*)' convert_binary_mass: in_unit,out_unit=',wrfges,',',filename
       end if
     
  ! Check for valid input file
       read(in_unit,iostat=status_hdr)hdrbuf
       if(n==nhr_assimilation)then
          if(status_hdr /= 0) then
             write(6,*)'CONVERT_BINARY_MASS:  problem with wrfges = ',&
                  trim(wrfges),', Status = ',status_hdr
             call stop2(74)
          endif
       else
          if(status_hdr /= 0) then
             write(6,*)'CONVERT_BINARY_MASS:  no off hour guess = ', filename
             close(in_unit)
             cycle n_loop
          endif
       end if
  
       open(lendian_out,file=filename,form='unformatted')
       rewind lendian_out
  
  !   reopen for direct access reading of sequential file
  
       close(in_unit)
  
  !    first determine if endian mismatch between machine and file, and set logical byte_swap accordingly.
       call this%initialize_byte_swap_wrf_binary_file(in_unit,wrfges)
       call count_recs_wrf_binary_file(this,in_unit,wrfges,nrecs)
       if(print_verbose)write(6,*) '  after count_recs_wrf_binary_file, nrecs=',nrecs
     
       allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
       allocate(memoryorder_all(nrecs))
       allocate(start_block(nrecs),end_block(nrecs),start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))
     
       call this%inventory_wrf_binary_file(in_unit,wrfges,nrecs, &
                                      datestr_all,varname_all,memoryorder_all,domainend_all, &
                                      start_block,end_block,start_byte,end_byte,file_offset)
            
  !   start with date record for date forecast was started
  
  !                   y,m,d,h,m,s
       call this%retrieve_index(index,'START_DATE',varname_all,nrecs)
       if(index<0) stop
       read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
            iyear,imonth,iday,ihour,iminute,isecond
       if(print_verbose)then
          write(6,*)' convert_binary_mass: START_DATE =',&
            iyear,imonth,iday,ihour,iminute,isecond
       end if
     
  !                  nsig_soil_regional
       call this%retrieve_index(index,'SMOIS',varname_all,nrecs)
       if(index<0) stop
  
       if(trim(memoryorder_all(index))=='XZY') then
          nsig_soil_regional=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          nsig_soil_regional=domainend_all(3,index)
       end if
       if(print_verbose)then
          write(6,*)' convert_binary_mass: sig_soil_regional=',&
            nsig_soil_regional
       end if
  
  !                  nlon_regional, nlat_regional, nsig_regional
       call this%retrieve_index(index,'T',varname_all,nrecs)
       if(index<0) stop
  
       if(trim(memoryorder_all(index))=='XZY') then
          nlon_regional=domainend_all(1,index)
          nlat_regional=domainend_all(3,index)
          nsig_regional=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          nlon_regional=domainend_all(1,index)
          nlat_regional=domainend_all(2,index)
          nsig_regional=domainend_all(3,index)
       end if
       read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
            iyear,imonth,iday,ihour,iminute,isecond
       if(print_verbose)then
          write(6,*)' convert_binary_mass: iy,m,d,h,m,s=',&
            iyear,imonth,iday,ihour,iminute,isecond
          write(6,*)' convert_binary_mass: nlon,lat,sig_regional=',&
            nlon_regional,nlat_regional,nsig_regional
       end if
       
  !                  pt_regional
       call this%retrieve_index(index,'P_TOP',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,pt_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
     
       if(print_verbose)write(6,*)' convert_binary_mass: pt_regional=',pt_regional
     
       write(lendian_out) iyear,imonth,iday,ihour,iminute,isecond, &
            nlon_regional,nlat_regional,nsig_regional,pt_regional,nsig_soil_regional
       
       allocate(field1(nsig_regional),field1p(nsig_regional+1))
       allocate(field1a(nsig_regional),field1pa(nsig_regional+1))
   
       if(wrf_mass_hybridcord) then
!                    c3h
          call retrieve_index(index,'C3H',varname_all,nrecs)
          if(index<0) stop
          call retrieve_field_rn1(in_unit,wrfges,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
          if(print_verbose)then
             do k=1,nsig_regional
                write(6,*)' convert_binary_mass: k,c3h(k)=',k,field1(k)
             end do
          end if
!                    c4h
          call retrieve_index(index,'C4H',varname_all,nrecs)
          if(index<0) stop
          call retrieve_field_rn1(in_unit,wrfges,field1a,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
          if(print_verbose)then
             do k=1,nsig_regional
                write(6,*)' convert_binary_mass: k,c4h(k)=',k,field1a(k)
             end do
          end if
          write(lendian_out)field1,field1a             !  C3H, C4H
   
!                    c3f
          call retrieve_index(index,'C3F',varname_all,nrecs)
          if(index<0) stop
          call retrieve_field_rn1(in_unit,wrfges,field1p,nsig_regional+1, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
          if(print_verbose)then
             do k=1,nsig_regional+1
                write(6,*)' convert_binary_mass: k,c3f(k)=',k,field1p(k)
             end do
          end if
!                    c4f
          call retrieve_index(index,'C4F',varname_all,nrecs)
          if(index<0) stop
          call retrieve_field_rn1(in_unit,wrfges,field1pa,nsig_regional+1, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
          if(print_verbose)then
             do k=1,nsig_regional+1
                write(6,*)' convert_binary_mass: k,c4f(k)=',k,field1pa(k)
             end do
          end if
          write(lendian_out)field1p,field1pa            !  c4f,c4f
       else
!                    znu
          call this%retrieve_index(index,'ZNU',varname_all,nrecs)
          if(index<0) stop
          call this%retrieve_field_rn1(in_unit,wrfges,field1,nsig_regional, &
                                         start_block(index+1),end_block(index+1), &
                                         start_byte(index+1),end_byte(index+1))
          if(print_verbose)then
             do k=1,nsig_regional
                write(6,*)' convert_binary_mass: k,znu(k)=',k,field1(k)
             end do
          end if
          field1a=0.0_r_single
          write(lendian_out)field1,field1a             !  ZNU
     
  !                  znw
          call this%retrieve_index(index,'ZNW',varname_all,nrecs)
          if(index<0) stop
          call this%retrieve_field_rn1(in_unit,wrfges,field1p,nsig_regional+1, &
                                         start_block(index+1),end_block(index+1), &
                                         start_byte(index+1),end_byte(index+1))
          if(print_verbose)then
             do k=1,nsig_regional+1
                write(6,*)' convert_binary_mass: k,znw(k)=',k,field1p(k)
             end do
          end if
          field1pa=0.0_r_single
          write(lendian_out)field1p,field1pa            !  ZNW
       endif   ! no hybrid vertical coordinate
   
       deallocate(field1,field1p)
       deallocate(field1a,field1pa)
   
!                  rdx
       call this%retrieve_index(index,'RDX',varname_all,nrecs)
     if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,rdx,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
     
       if(print_verbose)write(6,*)' convert_binary_mass: 1/rdx=',&
            one_single/rdx   ! 1._4 necessary only to get bit reproducibility
  
  !                  rdy
       call this%retrieve_index(index,'RDY',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,rdy,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
     
       if(print_verbose)write(6,*)' convert_binary_mass: 1/rdy=',&
            one_single/rdy  ! 1._4 necessary only to get bit reproducibility
       
       allocate(field2(nlon_regional,nlat_regional))
       allocate(field2b(nlon_regional,nlat_regional))
       allocate(field2c(nlon_regional,nlat_regional))
     
  !                  mapfac_m
       call this%retrieve_index(index,'MAPFAC_M',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
     
       if(print_verbose)then
          write(6,*)' convert_binary_mass: max,min mapfac_m=',maxval(field2),minval(field2)
          write(6,*)' convert_binary_mass: max,min MAPFAC_M(:,1)=', &
            maxval(field2(:,1)),minval(field2(:,1))
          write(6,*)' convert_binary_mass: max,min MAPFAC_M(1,:)=', &
            maxval(field2(1,:)),minval(field2(1,:))
          write(6,*)' convert_binary_mass: mapfac_m(1,1),mapfac_m(nlon,1)=', &
            field2(1,1),field2(nlon_regional,1)
          write(6,*)' convert_binary_mass: mapfac_m(1,nlat),mapfac_m(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       field2b=one_single/(field2*rdx)  !DX_MC
       field2c=one_single/(field2*rdy)  !DY_MC
     
  !                  XLAT
       rad2deg_single=r45/atan(one_single)
       call this%retrieve_index(index,'XLAT',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)then
          write(6,*)' convert_binary_mass: max,min XLAT(:,1)=',&
            maxval(field2(:,1)),minval(field2(:,1))
          write(6,*)' convert_binary_mass: max,min XLAT(1,:)=',&
            maxval(field2(1,:)),minval(field2(1,:))
          write(6,*)' convert_binary_mass: xlat(1,1),xlat(nlon,1)=',&
            field2(1,1),field2(nlon_regional,1)
          write(6,*)' convert_binary_mass: xlat(1,nlat),xlat(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       field2=field2/rad2deg_single
       write(lendian_out)field2,field2b,n_position     !  XLAT,DX_MC
     
  !                  XLONG
       call this%retrieve_index(index,'XLONG',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)then
          write(6,*)' convert_binary_mass: max,min XLONG(:,1)=',&
            maxval(field2(:,1)),minval(field2(:,1))
          write(6,*)' convert_binary_mass: max,min XLONG(1,:)=',&
            maxval(field2(1,:)),minval(field2(1,:))
          write(6,*)' convert_binary_mass: xlong(1,1),xlong(nlon,1)=',&
            field2(1,1),field2(nlon_regional,1)
          write(6,*)' convert_binary_mass: xlong(1,nlat),xlong(nlon,nlat)=', &
            field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
       end if
       field2=field2/rad2deg_single
     
       write(lendian_out)field2,field2c,n_position     !  XLONG,DY_MC
     
       write(lendian_out) wrfges
     
  !      index for START_DATE record
       call this%retrieve_index(index,'START_DATE',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index)
       write(lendian_out)n_position    ! offset for START_DATE record
     
  !                  MUB
       call this%retrieve_index(index,'MUB',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
     
       if(print_verbose)write(6,*)'  byte offset for MUB = ',n_position
  
       write(lendian_out)n_position    ! offset for mub
  
  !                  MU
       call this%retrieve_index(index,'MU',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
     
       if(print_verbose)write(6,*)'  byte offset for MU = ',n_position
  
       write(lendian_out)n_position    ! offset for mu
    
  !                   PHB                  
       k=nsig_regional
       call this%retrieve_index(index,'PHB',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for PHB(',k+1,') = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for PHB 
                                                           !     (zsfc*g is 1st level of this 3d field)
                                 !  but more efficient to read in whole 3-d field because of ikj order
  
  !                   T                  
       call this%retrieve_index(index,'T',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for T(',k,') = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)  ! offset for T(k)    ! POT TEMP (sensible)
  
  
  !                   QVAPOR
       call this%retrieve_index(index,'QVAPOR',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for QVAPOR(',k,' = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for QVAPOR(k)
       
  !                   U                  
       call this%retrieve_index(index,'U',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for U(',k,' = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for U(k)
  
  !                   V                  
       call this%retrieve_index(index,'V',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for V(',k,' = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for V(k)
  
  !                   LANDMASK          
       call this%retrieve_index(index,'LANDMASK',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for LANDMASK = ',n_position
  
       write(lendian_out)n_position     !  LANDMASK  (1=land, 0=water)
  
  !                   XICE                
       call this%retrieve_index(index,'SEAICE',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
     
       if(print_verbose)write(6,*)'  byte offset for XICE = ',n_position
  
       write(lendian_out)n_position     !  XICE
  
  !                   SST                
       call this%retrieve_index(index,'SST',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for SST = ',n_position
  
       write(lendian_out)n_position     !  SST
  
  !                   IVGTYP                
       call this%retrieve_index(index,'IVGTYP',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for IVGTYP = ',n_position
  
       write(lendian_out)n_position     !  IVGTYP
  
  !                   ISLTYP                
       call this%retrieve_index(index,'ISLTYP',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
     
       if(print_verbose)write(6,*)'  byte offset for ISLTYP = ',n_position
  
       write(lendian_out)n_position     !  ISLTYP
    
  !                   VEGFRA                
       call this%retrieve_index(index,'VEGFRA',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for VEGFRA = ',n_position
  
       write(lendian_out)n_position     !  VEGFRA
  
  !                   SNOW
       call this%retrieve_index(index,'SNOW',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for SNOW = ',n_position
  
       write(lendian_out)n_position     !  SNOW
  
  !                   U10                
       call this%retrieve_index(index,'U10',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for U10 = ',n_position
  
       write(lendian_out)n_position     !  U10
    
  !                   V10                
       call this%retrieve_index(index,'V10',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
     
       if(print_verbose)write(6,*)'  byte offset for V10 = ',n_position
  
       write(lendian_out)n_position     !  V10
  
  !                   SMOIS              
       call this%retrieve_index(index,'SMOIS',varname_all,nrecs)
       if(index<0) stop
       if(trim(memoryorder_all(index))=='XZY') then
          ksize=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          ksize=domainend_all(3,index)
       end if
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, ksize, memoryorder for SMOIS(',ksize,') = ', &
                                             n_position,ksize,memoryorder_all(index)
       write(lendian_out)n_position,ksize,memoryorder_all(index)     !  SMOIS
  
  !                   TSLB               
       call this%retrieve_index(index,'TSLB',varname_all,nrecs)
       if(index<0) stop
       if(trim(memoryorder_all(index))=='XZY') then
          ksize=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          ksize=domainend_all(3,index)
       end if
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, ksize, memoryorder for TSLB(',ksize,') = ', &
                                                n_position,ksize,memoryorder_all(index)
       write(lendian_out)n_position,ksize,memoryorder_all(index)     !  TSLB
  
  !                   TSK                
       call this%retrieve_index(index,'TSK',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for TSK = ',n_position
  
       write(lendian_out)n_position     !  TSK
  
  !                   Q2                
       call this%retrieve_index(index,'Q2',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for Q2 = ',n_position
       write(lendian_out)n_position     !  Q2
  
  
       if(l_gsd_soilTQ_nudge) then
  !                      SOIL1              
          call this%retrieve_index(index,'SOILT1',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset for SOILT1= ',n_position
          write(lendian_out)n_position     ! SOILT1 
  !                   TH2                
          call this%retrieve_index(index,'TH2',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset for TH2 = ',n_position
          write(lendian_out)n_position     !  TH2
       endif
  
       if(l_hydrometeor_bkio .and. n_actual_clouds>0) then
  !      QCLOUD
          call this%retrieve_index(index,'QCLOUD',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QCLOUD(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QCLOUD(k)
  
  !      QRAIN
          call this%retrieve_index(index,'QRAIN',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QRAIN(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QRAIN(k)
  
  !      QICE
          call this%retrieve_index(index,'QICE',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QICE(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QICE(k)
  
  !      QSNOW
          call this%retrieve_index(index,'QSNOW',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QSNOW(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QSNOW(k)
  
  !      QGRAUP
          call this%retrieve_index(index,'QGRAUP',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QGRAUP(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QGRAUP(k)
  
  !      QNRAIN
          call this%retrieve_index(index,'QNRAIN',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QNRAIN(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QNRAIN(k)
  
  !      QNRIC
          call this%retrieve_index(index,'QNICE',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QNICE(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QNICE(k)
  
  !      QNCLOUD
          call this%retrieve_index(index,'QNCLOUD',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for QNCLOUD(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for QNCLOUD(k)
  
  !      RAD_TTEN_DFI
          call this%retrieve_index(index,'RAD_TTEN_DFI',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for RAD_TTEN_DFI(',k,' = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for RAD_TTEN_DFI(k)
  
       endif     ! l_hydrometeor_bkio
  
  !??????????????????/later put in z0 here, but for now just fill with something
       call this%retrieve_index(index,'TSK',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_mass: max,min Z0=', &
            maxval(field2),minval(field2)
       write(lendian_out)field2        !  Z0
       call this%retrieve_index(index,'SST',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_mass: max,min SST=', &
            maxval(field2),minval(field2)
       write(lendian_out)field2        !  SST
       call this%retrieve_index(index,'TSK',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_mass: max,min TSK=', &
            maxval(field2),minval(field2)
       write(lendian_out)field2        !  TSK
       call this%retrieve_index(index,'LANDMASK',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_mass: max,min LANDMASK=', &
            maxval(field2),minval(field2)
       write(lendian_out)field2        !  LANDMASK
       call this%retrieve_index(index,'SEAICE',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_mass: max,min XICE=', &
            maxval(field2),minval(field2)
       write(lendian_out)field2        !  XICE
       call this%retrieve_index(index,'SNOW',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_mass: max,min SNOW=', &
            maxval(field2),minval(field2)
       write(lendian_out)field2        !  SNOW
     
       deallocate(field2,field2b,field2c)
       deallocate(datestr_all,varname_all,domainend_all,memoryorder_all)
       deallocate(start_block,end_block,start_byte,end_byte,file_offset)
       
       close(in_unit)
       close(lendian_out)
     
    enddo n_loop
    
  end subroutine convert_binary_mass_wrf
  subroutine convert_binary_nmm_wrf(this,update_pint,ctph0,stph0,tlm0)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_binary_nmm    read wrf nmm binary restart 
  !   prgmmr: parrish          org: np22                date: 2003-09-05
  !
  ! abstract: using wrf library routines, read a wrf nmm binary
  !             format restart file.  write the result to temporary binary
  !             file expected by read_wrf_nmm_guess.
  !
  ! program history log:
  !   2004-09-10  parrish
  !   2004-11-05  wu - add check on input wrf guess file, stop code if problem
  !   2004-11-11  parrish - change so byte offset information is written, instead
  !                            of a whole new binary file--this done so mpi-io
  !                            can be used later to read/write directly from
  !                            the wrf mass core binary file.  also, do inventory
  !                            of whole file, so offsets are general--not dependent
  !                            on type of file.
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-07-06  parrish - add read of pint byte address
  !   2005-10-17  parrish - add ctph0,stph0,tlm0
  !   2006-04-06  middlecoff - changed out_unit from 55 to lendian_out
  !   2006-06-19  wu - changes to allow nfldsig=3 (multiple first guess)
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2011-11-16  tong - increase number of multiple first guess upto 7
  !   2012-01-12  zhu     - add cloud hydrometoers
  !   2012-10-11  parrish - move line "write(filename,'("sigf",i2.2)')n+nhr_assimilation-1" so input names
  !                           sigfxx are properly defined for all values of n, not just n=1.
  !   2012-10-11  parrish - add call to initialize_byte_swap_wrf_binary_file routine, and also add this
  !                           subroutine to this file.
  !   2012-12-10  eliu    - modify to add the use of use_gfs_stratosphere
  !   2013-01-29  parrish - replace retrieve_field calls with retrieve_field_r1, retrieve_field_rn1,
  !                           retrieve_field_rn1n2 (so debug compile works on WCOSS)
  !   2013-02-15  parrish - change dimension of eta1_new,eta2_new from nsig_max to nsig_max+1.
  !
  !   input argument list:
  !     update_pint:   false on input
  !
  !   output argument list:
  !     update_pint:   true on output if field pint (non-hydrostatic pressure in nmm model)
  !                     is available, in which case pint gets updated by analysis increment of pd,
  !                      the nmm hydrostatic pressure thickness variable.
  !     ctph0,stph0:   cos and sin thp0, earth lat of center of nmm grid 
  !                    (0 deg lat in rotated nmm coordinate)
  !                      (used by calctends routines)
  !     tlm0
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: r_single,i_llong,r_kind,i_kind
    use constants, only: zero,half,rad2deg
    use gsi_4dvar, only: nhr_assimilation
    use gsi_io, only: lendian_out, verbose
    use gsi_metguess_mod, only: gsi_metguess_get
    use gfs_stratosphere, only: mix_gfs_nmmb_vcoords,use_gfs_stratosphere,nsig_max,nsig_save                                                                            
    implicit none
    class(get_wrf_binary_interface_class), intent(inout) :: this
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  
    integer(i_kind),parameter:: in_unit = 15
  
  
    character(9) wrfges,fileout
    integer(i_kind),allocatable:: start_block(:),end_block(:)
    integer(i_kind),allocatable:: start_byte(:),end_byte(:)
    integer(i_llong),allocatable:: file_offset(:)
    integer(i_llong) n_position
    character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
    integer(i_kind),allocatable:: domainend_all(:,:)
    integer(i_kind) nrecs
    integer(i_kind) status_hdr
    integer(i_kind) hdrbuf(512)
    
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional
    integer(i_kind) nsig_regional_new,nsig_read               
    integer(i_kind) n_actual_clouds,istatus
    integer(i_kind) nstart_hour
    real(r_single) dlmd_regional,dphd_regional,pt_regional,pdtop_regional
    real(r_single) dy_nmm
    integer(i_kind) k,n
    real(r_single),allocatable::field1(:),field1p(:),field2(:,:),field2b(:,:)
    real(r_single),allocatable::aeta1(:),deta1(:),eta1(:)            
    real(r_single),allocatable::aeta2(:),deta2(:),eta2(:)             
    real(r_single),allocatable::aeta1_new(:),deta1_new(:),eta1_new(:) 
    real(r_single),allocatable::aeta2_new(:),deta2_new(:),eta2_new(:)
    integer(i_kind) ksize
    integer(i_kind) index
    logical :: print_verbose

    print_verbose=.false.
    if(verbose)print_verbose=.true.
     
  ! Inquire about cloud guess fields
    call gsi_metguess_get('clouds::3d',n_actual_clouds,istatus)
  
    n_loop: do n=1,9
  
       if(n==nhr_assimilation)then
          wrfges = 'wrf_inout'
       else
          write(wrfges,'("wrf_inou",i1.1)')n
       endif
  
       write(fileout,'("sigf",i2.2)')n
  
       open(in_unit,file=trim(wrfges),form='unformatted')
       if(print_verbose)then
          write(6,*)' convert_binary_nmm: in_unit,lendian_out=',in_unit,lendian_out
          write(6,*)' convert_binary_nmm: in_unit,out_unit=',wrfges,',',fileout
       end if
  
  !    Check for valid input file
       read(in_unit,iostat=status_hdr)hdrbuf
       if(n==nhr_assimilation)then
          if(status_hdr /= 0) then
             write(6,*)'CONVERT_BINARY_NMM:  problem with wrfges = ',&
                  trim(wrfges),', Status = ',status_hdr
             call stop2(74)
          endif
       else
          if(status_hdr /= 0) then
             write(6,*)'CONVERT_BINARY_NMM:  no off hour guess   = ', fileout
             close(in_unit)
             cycle n_loop
          endif
       endif
  
       open(lendian_out,file=fileout,form='unformatted')
       rewind lendian_out
  
  !    reopen for direct access reading of sequential file
  
       close(in_unit)
  
  !    first determine if endian mismatch between machine and file, and set logical byte_swap accordingly.
       call this%initialize_byte_swap_wrf_binary_file(in_unit,wrfges)
       call count_recs_wrf_binary_file(this,in_unit,wrfges,nrecs)
       if(print_verbose)write(6,*) '  after count_recs_wrf_binary_file, nrecs=',nrecs
  
       allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
       allocate(memoryorder_all(nrecs))
       allocate(start_block(nrecs),end_block(nrecs),start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))
  
       call this%inventory_wrf_binary_file(in_unit,wrfges,nrecs, &
                                      datestr_all,varname_all,memoryorder_all,domainend_all, &
                                      start_block,end_block,start_byte,end_byte,file_offset)
   
  !    start with date record for date forecast was started
  
  !                   y,m,d,h,m,s
       call this%retrieve_index(index,'START_DATE',varname_all,nrecs)
       if(index<0) stop
       read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
                  iyear,imonth,iday,ihour,iminute,isecond
       if(print_verbose)write(6,*)' convert_binary_nmm: START_DATE =',&
            iyear,imonth,iday,ihour,iminute,isecond
    
  !                  nlon_regional, nlat_regional, nsig_regional
       call this%retrieve_index(index,'T',varname_all,nrecs)
       if(index<0) stop
  
       if(trim(memoryorder_all(index))=='XZY') then
          nlon_regional=domainend_all(1,index)
          nlat_regional=domainend_all(3,index)
          nsig_regional=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          nlon_regional=domainend_all(1,index)
          nlat_regional=domainend_all(2,index)
          nsig_regional=domainend_all(3,index)
       end if
  !    These will hold original vertical structure for regional
       allocate(deta1(nsig_regional),aeta1(nsig_regional),eta1(nsig_regional+1))
       allocate(deta2(nsig_regional),aeta2(nsig_regional),eta2(nsig_regional+1))
  
       read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
            iyear,imonth,iday,ihour,iminute,isecond
       if(print_verbose)then
          write(6,*)' convert_binary_nmm: iy,m,d,h,m,s=',&
            iyear,imonth,iday,ihour,iminute,isecond
          write(6,*)' convert_binary_nmm: nlon,lat,sig_regional=',&
            nlon_regional,nlat_regional,nsig_regional
       end if
  
  !                  NSTART_HOUR
       call this%retrieve_index(index,'NSTART_HOUR',varname_all,nrecs)
       if(index<0)then
          write(6,*)' ***WARNING*** NSTART_HOUR is not found, only need to be updated for WRF restart file'
       else
          call this%retrieve_field_i1(in_unit,wrfges,nstart_hour,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
          if(print_verbose)write(6,*)' convert_binary_nmm: nstart_hour=',nstart_hour
       end if 
  
  
  !                  dlmd_regional
       call this%retrieve_index(index,'DLMD',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,dlmd_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)write(6,*)' convert_binary_nmm: dlmd_regional=',dlmd_regional
    
  !                  dphd_regional
       call this%retrieve_index(index,'DPHD',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,dphd_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)write(6,*)' convert_binary_nmm: dphd_regional=',dphd_regional
  
  !                  pt_regional
       call this%retrieve_index(index,'PT',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,pt_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: pt_regional=',pt_regional
  
  !                  pdtop_regional
       call this%retrieve_index(index,'PDTOP',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,pdtop_regional,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: pdtop_regional=',pdtop_regional
  
  !     write(lendian_out) iyear,imonth,iday,ihour,iminute,isecond, &
  !          nlon_regional,nlat_regional,nsig_regional, &
  !          dlmd_regional,dphd_regional,pt_regional,pdtop_regional
   
       allocate(field1(nsig_max),field1p(nsig_max+1))           
    
  !                  deta1
       call this%retrieve_index(index,'DETA1',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1(in_unit,wrfges,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       do k=1,nsig_regional
          if(print_verbose)write(6,*)' convert_binary_nmm: k,deta1(k)=',k,field1(k)
          deta1(k)=field1(k)   
       end do
  
  !    write(lendian_out)field1             !  DETA1 
  
  !                  aeta1
       call this%retrieve_index(index,'AETA1',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1(in_unit,wrfges,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       do k=1,nsig_regional
          if(print_verbose)write(6,*)' convert_binary_nmm: k,aeta1(k)=',k,field1(k)
          aeta1(k)=field1(k) 
       end do
  
  !    write(lendian_out)field1             !  AETA1 
    
  !                  eta1
       call this%retrieve_index(index,'ETA1',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1(in_unit,wrfges,field1p,nsig_regional+1, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       do k=1,nsig_regional+1
          if(print_verbose)write(6,*)' convert_binary_nmm: k,eta1(k)=',k,field1p(k)
          eta1(k)=field1p(k)   
       end do
  
  !    write(lendian_out)field1p            !  ETA1 
  
  !                  deta2
       call this%retrieve_index(index,'DETA2',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1(in_unit,wrfges,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       do k=1,nsig_regional
          if(print_verbose)write(6,*)' convert_binary_nmm: k,deta2(k)=',k,field1(k)
          deta2(k)=field1(k)     
       end do
  
  !    write(lendian_out)field1             !  DETA2 
  
  !                  aeta2
       call this%retrieve_index(index,'AETA2',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1(in_unit,wrfges,field1,nsig_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       do k=1,nsig_regional
          if(print_verbose)write(6,*)' convert_binary_nmm: k,aeta2(k)=',k,field1(k)
          aeta2(k)=field1(k)    
       end do
  
  !    write(lendian_out)field1             !  AETA2 
  
  !                  eta2
       call this%retrieve_index(index,'ETA2',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1(in_unit,wrfges,field1p,nsig_regional+1, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       do k=1,nsig_regional+1
          if(print_verbose)write(6,*)' convert_binary_nmm: k,eta2(k)=',k,field1p(k)
          eta2(k)=field1p(k)    
       end do
  !    write(lendian_out)field1p            !  ETA2
  
       deallocate(field1,field1p)
  
  !    Get global-regional blended vertical coordinate
       nsig_read=nsig_regional
       if(use_gfs_stratosphere) then  !get new vertical coordinate
          allocate(deta1_new(nsig_max),aeta1_new(nsig_max),eta1_new(nsig_max+1))
          allocate(deta2_new(nsig_max),aeta2_new(nsig_max),eta2_new(nsig_max+1))
          call mix_gfs_nmmb_vcoords(deta1,aeta1,eta1,deta2,aeta2,eta2, &
                                    pdtop_regional,pt_regional,nsig_regional, &
                                    deta1_new,aeta1_new,eta1_new,deta2_new,aeta2_new,eta2_new,nsig_regional_new)
          nsig_read=nsig_save
          if(print_verbose)then
             write(6,*)' in convert_netcdf_nmm, compute new vertical coordinate which is merged with gfs'   
             write(6,*)' previous nsig_regional=',nsig_regional
          end if
          nsig_regional=nsig_regional_new    !new nsig
          if(print_verbose)then
             write(6,*)'      new nsig_regional=',nsig_regional
             write(6,*)'              nsig_read=',nsig_read 
          end if
          deallocate(deta1,aeta1,eta1)
          deallocate(deta2,aeta2,eta2)
          allocate(deta1(nsig_regional),aeta1(nsig_regional),eta1(nsig_regional+1))
          allocate(deta2(nsig_regional),aeta2(nsig_regional),eta2(nsig_regional+1))
          do k=1,nsig_regional
             deta1(k)=deta1_new(k)
             aeta1(k)=aeta1_new(k)
             deta2(k)=deta2_new(k)
             aeta2(k)=aeta2_new(k)
          end do
          do k=1,nsig_regional+1
             eta1(k)=eta1_new(k)
             eta2(k)=eta2_new(k)
          end do
          deallocate(deta1_new,aeta1_new,eta1_new)
          deallocate(deta2_new,aeta2_new,eta2_new)
       end if ! use_gfs_stratosphere
       write(lendian_out) iyear,imonth,iday,ihour,iminute,isecond, &
            nlon_regional,nlat_regional,nsig_regional, &
            dlmd_regional,dphd_regional,pt_regional,pdtop_regional
       write(lendian_out)deta1(1:nsig_regional)    ! DETA1
       write(lendian_out)aeta1(1:nsig_regional)    ! AETA1
       write(lendian_out) eta1(1:nsig_regional+1)  !  ETA1
       write(lendian_out)deta2(1:nsig_regional)    ! DETA2
       write(lendian_out)aeta2(1:nsig_regional)    ! AETA2
       write(lendian_out) eta2(1:nsig_regional+1)  !  ETA2
       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2)
  
       allocate(field2(nlon_regional,nlat_regional))
       allocate(field2b(nlon_regional,nlat_regional))
  
  !                  GLAT
       call this%retrieve_index(index,'GLAT',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)then
          write(6,*)' convert_binary_nmm: max,min GLAT=', &
            rad2deg*maxval(field2),rad2deg*minval(field2)
          write(6,*)' convert_binary_nmm: glat(1,1),glat(nlon,1)=', &
            rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
          write(6,*)' convert_binary_nmm: glat(1,nlat),glat(nlon,nlat)=', &
            rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
          write(6,*)' convert_binary_nmm: my guess at tph0d = ', &
            rad2deg*field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
       end if
       ctph0=cos(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
       stph0=sin(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  
  !                  DX_NMM
       call this%retrieve_index(index,'DX_NMM',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)then
          write(6,*)' convert_binary_nmm: max,min DX_NMM=', &
            maxval(field2b),minval(field2b)
          write(6,*)' convert_binary_nmm: dx_nmm(1,1),dx_nmm(nlon,1)=', &
            field2b(1,1),field2b(nlon_regional,1)
          write(6,*)' convert_binary_nmm: dx_nmm(1,nlat),dx_nmm(nlon,nlat)=', &
            field2b(1,nlat_regional),field2b(nlon_regional,nlat_regional)
       end if
  
       write(lendian_out)field2,field2b     !  GLAT,DX_NMM
  
  !                  GLON
       call this%retrieve_index(index,'GLON',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
  
       if(print_verbose)then
          write(6,*)' convert_binary_nmm: max,min GLON=', &
            rad2deg*maxval(field2),rad2deg*minval(field2)
          write(6,*)' convert_binary_nmm: glon(1,1),glon(nlon,1)=', &
            rad2deg*field2(1,1),rad2deg*field2(nlon_regional,1)
          write(6,*)' convert_binary_nmm: glon(1,nlat),glon(nlon,nlat)=', &
            rad2deg*field2(1,nlat_regional),rad2deg*field2(nlon_regional,nlat_regional)
          write(6,*)' convert_binary_nmm: my guess at tlm0d = ', &
            half*rad2deg*(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)+ &
                        field2(2+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
       end if
       tlm0=half*(field2(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)+ &
                field2(2+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  
  !                  DY_NMM
       call this%retrieve_index(index,'DY_NMM',varname_all,nrecs)
       if(index<0) stop
       call this%retrieve_field_r1(in_unit,wrfges,dy_nmm,start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: DY_NMM=',dy_nmm
       field2b=dy_nmm
  
       write(lendian_out)field2,field2b     !  GLON,DY_NMM
  
       write(lendian_out) wrfges
  
  !      index for START_DATE record
       call this%retrieve_index(index,'START_DATE',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index)
       write(lendian_out)n_position    ! offset for START_DATE record
  
  !      index for NSTART_HOUR record 
       call this%retrieve_index(index,'NSTART_HOUR',varname_all,nrecs)
       if(index<0)then
          n_position=-99
       else
          n_position=file_offset(index+1)
       end if
       write(lendian_out)n_position    ! offset for NSTART_HOUR record
  
  !                  PD
       call this%retrieve_index(index,'PD',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       write(lendian_out)n_position   !  offset for PD
  
  !                   FIS                  
       call this%retrieve_index(index,'FIS',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
  
       if(print_verbose)write(6,*)'  byte offset for FIS = ',n_position
  
       write(lendian_out)n_position  !  offset for FIS
  
  !                   PINT               
       call this%retrieve_index(index,'PINT',varname_all,nrecs)
       update_pint=.false.
       if(index>0) then
          update_pint=.true.
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for PINT = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for PINT !
       end if
  
  !                   T                  
       call this%retrieve_index(index,'T',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for T = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for T    !
    
  !                   Q                  
       call this%retrieve_index(index,'Q',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for Q = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for Q    !
  
  !                   U                  
       call this%retrieve_index(index,'U',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for U = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for U    !
  
  !                   V                  
       call this%retrieve_index(index,'V',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset, memoryorder for V = ',n_position,memoryorder_all(index)
       write(lendian_out)n_position,memoryorder_all(index)    ! offset for V    !
  
  !                   SM                
       call this%retrieve_index(index,'SM',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for SM = ',n_position
       write(lendian_out)n_position    ! offset for SM    !
  
  !                   SICE                
       call this%retrieve_index(index,'SICE',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for SICE = ',n_position
       write(lendian_out)n_position    ! offset for SICE  !
  
  !                   SST                
       call this%retrieve_index(index,'SST',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for SST = ',n_position
       write(lendian_out)n_position    ! offset for SST   !
  
  !                   IVGTYP                
       call this%retrieve_index(index,'IVGTYP',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for IVGTYP = ',n_position
       write(lendian_out)n_position    ! offset for IVGTYP    !
  
  !                   ISLTYP                
       call this%retrieve_index(index,'ISLTYP',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for ISLTYP = ',n_position
       write(lendian_out)n_position    ! offset for ISLTYP    !
  
  !                   VEGFRC                
       call this%retrieve_index(index,'VEGFRC',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for VEGFRC = ',n_position
       write(lendian_out)n_position    ! offset for VEGFRC    !
  
  !                   SNO                
       call this%retrieve_index(index,'SNO',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for SNO = ',n_position
       write(lendian_out)n_position    ! offset for SNO   !
  
  !                   U10                
       call this%retrieve_index(index,'U10',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for U10 = ',n_position
       write(lendian_out)n_position    ! offset for U10   !
  
  !                   V10                
       call this%retrieve_index(index,'V10',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for V10 = ',n_position
       write(lendian_out)n_position    ! offset for V10   !
  
  !                   SMC                
       call this%retrieve_index(index,'SMC',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(trim(memoryorder_all(index))=='XZY') then
          ksize=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          ksize=domainend_all(3,index)
       end if
       if(print_verbose)write(6,*)'  byte offset, ksize, memoryorder for SMC = ',n_position,ksize,memoryorder_all(index)
       write(lendian_out)n_position,ksize,memoryorder_all(index)    ! offset for SMC   !
  
  !                   STC                
       call this%retrieve_index(index,'STC',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(trim(memoryorder_all(index))=='XZY') then
          ksize=domainend_all(2,index)
       end if
       if(trim(memoryorder_all(index))=='XYZ') then
          ksize=domainend_all(3,index)
       end if
       if(print_verbose)write(6,*)'  byte offset, ksize, memoryorder for STC = ',n_position,ksize,memoryorder_all(index)
       write(lendian_out)n_position,ksize,memoryorder_all(index)    ! offset for STC   !
    
  !                   TSK                
       call this%retrieve_index(index,'TSK',varname_all,nrecs)
       if(index<0) stop
       n_position=file_offset(index+1)
       if(print_verbose)write(6,*)'  byte offset for TSK = ',n_position
       write(lendian_out)n_position    ! offset for TSK   !
  
       if (n_actual_clouds>0) then
  !                   CWM
          call this%retrieve_index(index,'CWM',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for CWM = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for CWM    !
  
  !                   F_ICE
          call this%retrieve_index(index,'F_ICE',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for F_ICE = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for F_ICE    !
  
  !                   F_RAIN
          call this%retrieve_index(index,'F_RAIN',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for F_RAIN = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for F_RAIN    !
  
  !                   F_RIMEF
          call this%retrieve_index(index,'F_RIMEF',varname_all,nrecs)
          if(index<0) stop
          n_position=file_offset(index+1)
          if(print_verbose)write(6,*)'  byte offset, memoryorder for F_RIMEF = ',n_position,memoryorder_all(index)
          write(lendian_out)n_position,memoryorder_all(index)    ! offset for F_RIMEF    !
       end if  ! end of n_actual_clouds>0
  
  !????????????????????????????????????????????????????????????????read z0 here to see what it looks like
       call this%retrieve_index(index,'Z0',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: max,min Z0=', &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  Z0
  !?????????????????????????????????????????????????????????????????
       call this%retrieve_index(index,'SST',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: max,min SST=', &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SST
       call this%retrieve_index(index,'TSK',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: max,min TSK=', &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  TSK
       call this%retrieve_index(index,'SM',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: max,min SM=', &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SM
       call this%retrieve_index(index,'SICE',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: max,min SICE=', &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SICE
       call this%retrieve_index(index,'SNO',varname_all,nrecs)
       call this%retrieve_field_rn1n2(in_unit,wrfges,field2b,nlon_regional,nlat_regional, &
                                    start_block(index+1),end_block(index+1), &
                                    start_byte(index+1),end_byte(index+1))
       if(print_verbose)write(6,*)' convert_binary_nmm: max,min SNO=', &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SNO
       deallocate(field2,field2b)
       deallocate(datestr_all,varname_all,domainend_all,memoryorder_all)
       deallocate(start_block,end_block,start_byte,end_byte,file_offset)
    
       close(in_unit)
       close(lendian_out)
    enddo n_loop
  
  end subroutine convert_binary_nmm_wrf
  subroutine convert_nems_nmmb_wrf(this,update_pint,ctph0,stph0,tlm0)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_nems_nmmb     read nems nmmb restart 
  !   prgmmr: parrish          org: np22                date: 2009-03-17
  !
  ! abstract: using nemsio library routines, read nems nmmb restart file
  !             write the result to temporary binary
  !             file expected by read_wrf_nmm_guess.
  !
  ! program history log:
  !   2004-09-10  parrish
  !   2004-11-05  wu - add check on input wrf guess file, stop code if problem
  !   2004-11-11  parrish - change so byte offset information is written, instead
  !                            of a whole new binary file--this done so mpi-io
  !                            can be used later to read/write directly from
  !                            the wrf mass core binary file.  also, do inventory
  !                            of whole file, so offsets are general--not dependent
  !                            on type of file.
  !   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
  !   2005-07-06  parrish - add read of pint byte address
  !   2005-10-17  parrish - add ctph0,stph0,tlm0
  !   2006-04-06  middlecoff - changed out_unit from 55 to lendian_out
  !   2006-06-19  wu - changes to allow nfldsig=3 (multiple first guess)
  !   2007-04-12  parrish - add modifications to allow any combination of ikj or ijk
  !                          grid ordering for input 3D fields
  !   2012-02-08  parrish - 1. modify subroutine convert_nems_nmmb to add use of use_gfs_stratosphere.
  !                         2. move conversion of aeta1, eta1 from init_reg_glob_ll (in gridmod.F90) to here.
  !   2013-02-15  parrish - change dimension of eta1_new,eta2_new from nsig_max to nsig_max+1.
  !   2013-04-17  parrish - option to accept input lat/lon in both degrees and radians
  !   2015-05-12  wu      - changes for FGAT
  !
  !   input argument list:
  !     update_pint:   false on input
  !
  !   output argument list:
  !     update_pint:   true on output if field pint (non-hydrostatic pressure in nmm model)
  !                     is available, in which case pint gets updated by analysis increment of pd,
  !                      the nmm hydrostatic pressure thickness variable.
  !     ctph0,stph0:   cos and sin thp0, earth lat of center of nmm grid 
  !                    (0 deg lat in rotated nmm coordinate)
  !                      (used by calctends routines)
  !     tlm0
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: r_single,r_kind,i_kind
    use constants, only: one_tenth,half,rad2deg,r100,r0_01
    use gsi_4dvar, only: nhr_assimilation
    use gsi_io, only: lendian_out,verbose
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_getheadvar,nemsio_readrecv
    use gfs_stratosphere, only: mix_gfs_nmmb_vcoords,use_gfs_stratosphere,nsig_max
    implicit none
    class(get_wrf_binary_interface_class), intent(inout) :: this
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  
  ! integer(i_kind),parameter:: in_unit = 15
    real(r_kind),parameter:: rd_over_cp = 0.285725661955006982_r_kind
  
    type(nemsio_gfile) :: gfile
    character(255) wrfges,fileout
    
    integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
    integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nsig_regional_new
    real(r_single) dlmd_regional,dphd_regional,pt_regional,pdtop_regional
    integer(i_kind) i,j,ii,k,n
    real(r_single),allocatable::field2(:),field2b(:),field2c(:)
    real(r_single),allocatable::aeta1(:),deta1(:),eta1(:)
    real(r_single),allocatable::aeta2(:),deta2(:),eta2(:)
    real(r_single),allocatable::aeta1_new(:),deta1_new(:),eta1_new(:)
    real(r_single),allocatable::aeta2_new(:),deta2_new(:),eta2_new(:)
    integer(i_kind) idate(7),nrec,iret
    character(4) gdatatype,modelname
    character(32) gtype
    integer(i_kind) nfhour,nfminute,nfsecondn,nfsecondd,nframe,ntrac,nsoil,nmeta
    logical extrameta
    real(r_single),allocatable,dimension(:):: dsg1,sgml1,sg1,dsg2,sgml2,sg2
    real(r_single),allocatable,dimension(:,:):: glat,glon,dx,dy
    real(r_single),allocatable,dimension(:):: glata,glona,dxa,dya
    character(8),allocatable:: recname(:)
    character(16),allocatable  :: reclevtyp(:)
    integer(i_kind),allocatable:: reclev(:)
    real(r_kind) date6,date7,second,fhour,fminute
    character(3) nmmb_verttype   !   'OLD' for old vertical coordinate definition
                                 !                old def: p = eta1*pdtop+eta2*(psfc-pdtop-ptop)+ptop
                                 !   'NEW' for new vertical coordinate definition
                                 !                new def: p = eta1*pdtop+eta2*(psfc-ptop)+ptop
    real(r_kind) pd,pd_to_ps,psfc_this
    real(r_kind) factor,ratio
    logical print_verbose
  
    print_verbose=.false.
    if(verbose)print_verbose=.true.
    call nemsio_init(iret=iret)
    if(iret/=0) then
       write(6,*)'CONVERT_NEMS_NMMB: problem with nemsio_init, Status = ',iret
       call stop2(74)
    end if
    
    n_loop: do n=1,9
  
          write(wrfges,'("wrf_inout",i2.2)')n
       call nemsio_open(gfile,wrfges,'READ',iret=iret)
       write(6,*)' convert_nems_nmmb: nemsio_open, file name, iret=',trim(wrfges),iret
       if(n==nhr_assimilation) then
          if(iret/=0) then
             write(6,*)'CONVERT_NEMS_NMMB:  problem with wrfges = ',&
                   trim(wrfges),', Status = ',iret
             call stop2(74)
          end if
       else
          if(iret/=0) then
             write(6,*)'CONVERT_NEMS_NMMB:  no off hour guess = ',trim(wrfges)
             call nemsio_close(gfile,iret=iret)
             write(6,*)' close nemsio file, iret=',iret
             cycle n_loop
          end if
       end if
       write(fileout,'("sigf",i2.2)')n
       write(6,*)' convert_nems_nmmb: in_unit,out_unit=',trim(wrfges),',',trim(fileout)
       open(lendian_out,file=trim(fileout),form='unformatted')
       rewind lendian_out
  
  !     obtain model info (date, grid dims, etc.)
  
  !     idate(1) = year
  !     idate(2) = month
  !     idate(3) = day
  !     idate(4) = hour
  !     idate(5) = minute
  !     idate(6) = seconds*idate(7)
  !     idate(7) = scale factor for seconds
       call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=nlon_regional,dimy=nlat_regional, &
         dimz=nsig_regional,idate=idate,gdatatype=gdatatype,gtype=gtype,modelname=modelname, &
         nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
         nframe=nframe,ntrac=ntrac,nsoil=nsoil,extrameta=extrameta,nmeta=nmeta)
       if(print_verbose)then
          write(6,*)' nemsio_getfilehead, iret=',iret,'nrec=',nrec
          write(6,*)' nlon_regional=',nlon_regional,' nlat_regional=',nlat_regional,' nsig_regional=',nsig_regional
          write(6,*)' idate=',idate
          write(6,*)' gdatatype=',gdatatype,' gtype=',trim(gtype)
          write(6,*)' nfhour=',nfhour,' nfminute=',nfminute,' nfsecondn=',nfsecondn,' nfsecondd=',nfsecondd
          write(6,*)' modelname=',modelname,' extrameta=',extrameta,' nframe=',nframe,' nmeta=',nmeta
       end if
  
  !   start with date record for date forecast was started
  
  !                   y,m,d,h,m,s
       iyear=  idate(1)
       imonth= idate(2)
       iday=   idate(3)
       ihour=  idate(4)
       iminute=idate(5)
       date6=idate(6) ; date7=idate(7)
       second=date6/date7
       isecond=nint(second)
       if(print_verbose)write(6,*)' convert_nems_nmmb: START_DATE =',&
            iyear,imonth,iday,ihour,iminute,isecond
    
    
  !                  dlmd_regional
  
       call nemsio_getheadvar(gfile,'DLMD',dlmd_regional,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: dlmd_regional,iret=',dlmd_regional,iret
    
  !                  dphd_regional
  
       call nemsio_getheadvar(gfile,'DPHD',dphd_regional,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: dphd_regional,iret=',dphd_regional,iret
  
  !                  pt_regional
       call nemsio_getheadvar(gfile,'PT',pt_regional,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: pt_regional,iret=',pt_regional,iret
  
  !                  pdtop_regional
       call nemsio_getheadvar(gfile,'PDTOP',pdtop_regional,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: pdtop_regional,iret=',pdtop_regional,iret
  
       fhour=nfhour
       fminute=nfminute
    
    
  !                  dsg1 (used to be deta1) 
  
       allocate(dsg1(nsig_regional),deta1(nsig_regional))
       call nemsio_getheadvar(gfile,'DSG1',dsg1,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: retrieve dsg1,iret=',iret
  
       do k=1,nsig_regional
          deta1(k)=dsg1(nsig_regional+1-k)
       end do
       if(print_verbose)then
          do k=1,nsig_regional
             write(6,*)' convert_nems_nmmb: k,dsg1 (deta1)(k)=',k,deta1(k)
          end do
       end if
  
  
  !                  sgml1    (used to be aeta1)
  
       allocate(sgml1(nsig_regional),aeta1(nsig_regional))
       call nemsio_getheadvar(gfile,'SGML1',sgml1,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: retrieve sgml1,iret=',iret
  
       do k=1,nsig_regional
          aeta1(k)=sgml1(nsig_regional+1-k)
       end do
       if(print_verbose)then
          do k=1,nsig_regional
             write(6,*)' convert_nems_nmmb: k,sgml1 (aeta1)(k)=',k,aeta1(k)
          end do
       end if
       nmmb_verttype='OLD'
       if(aeta1(1)<.6_r_single) then
          nmmb_verttype='NEW'
       end if
  
    
  !                  sg1       (used to be eta1)
  
       allocate(sg1(nsig_regional+1),eta1(nsig_regional+1))
       call nemsio_getheadvar(gfile,'SG1',sg1,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: retrieve sg1,iret=',iret
  
       do k=1,nsig_regional+1
          eta1(k)=sg1(nsig_regional+2-k)
       end do
       if(print_verbose)then
          do k=1,nsig_regional+1
             write(6,*)' convert_nems_nmmb: k,sg1 (eta1)(k)=',k,eta1(k)
          end do
       end if
  
  
  !                  dsg2 (used to be deta2) 
  
       allocate(dsg2(nsig_regional),deta2(nsig_regional))
       call nemsio_getheadvar(gfile,'DSG2',dsg2,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: retrieve dsg2,iret=',iret
  
       do k=1,nsig_regional
          deta2(k)=dsg2(nsig_regional+1-k)
       end do
       if(print_verbose)then
          do k=1,nsig_regional
             write(6,*)' convert_nems_nmmb: k,dsg2 (deta2)(k)=',k,deta2(k)
          end do
       end if
  
  
  !                  sgml2    (used to be aeta2)
  
       allocate(sgml2(nsig_regional),aeta2(nsig_regional+1))
       call nemsio_getheadvar(gfile,'SGML2',sgml2,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: retrieve sgml2,iret=',iret
  
       do k=1,nsig_regional
          aeta2(k)=sgml2(nsig_regional+1-k)
       end do
       if(print_verbose)then
          do k=1,nsig_regional
             write(6,*)' convert_nems_nmmb: k,sgml2 (aeta2)(k)=',k,aeta2(k)
          end do
       end if
  
  
  !                  sg2       (used to be eta2)
  
       allocate(sg2(nsig_regional+1),eta2(nsig_regional+1))
       call nemsio_getheadvar(gfile,'SG2',sg2,iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: retrieve sg2,iret=',iret
  
       do k=1,nsig_regional+1
          eta2(k)=sg2(nsig_regional+2-k)
       end do
       if(print_verbose)then
          do k=1,nsig_regional+1
             write(6,*)' convert_nems_nmmb: k,sg2 (eta2)(k)=',k,eta2(k)
          end do
       end if
  
  !----------------------------------------detect if new nmmb coordinate:
         nmmb_verttype='OLD'
         if(aeta1(1)<.6_r_single) then
            write(6,*)' in convert_nems_nmmb, detect new nmmb vert coordinate'
            do k=1,nsig_regional
               deta1(k)=deta1(k)+deta2(k)      !  even though deta1 not used, probably needed to do this--will see
               aeta1(k)=aeta1(k)+aeta2(k)
            end do
            do k=1,nsig_regional+1
               eta1(k)=eta1(k)+eta2(k)
            end do
            nmmb_verttype='NEW'
         end if
  
  !  check to see if merging with gfs stratosphere
       if(use_gfs_stratosphere) then
          allocate(deta1_new(nsig_max),aeta1_new(nsig_max),eta1_new(nsig_max+1))
          allocate(deta2_new(nsig_max),aeta2_new(nsig_max),eta2_new(nsig_max+1))
  
  !  generate new mixed vertical coordinate info:
          call mix_gfs_nmmb_vcoords(deta1,aeta1,eta1,deta2,aeta2,eta2, &
                                    pdtop_regional,pt_regional,nsig_regional, &
                             deta1_new,aeta1_new,eta1_new,deta2_new,aeta2_new,eta2_new,nsig_regional_new)
          write(6,*)' in convert_nems_nmmb, compute new vertical coordinate which is merged with gfs'
          write(6,*)' previous nsig_regional=',nsig_regional
  
  !   replace nsig_regional and deta1, aeta1, eta1, deta2, aeta2,eta2 with new mixed nmmb-gfs version:
          nsig_regional=nsig_regional_new
          write(6,*)'      new nsig_regional=',nsig_regional
          deallocate(deta1,aeta1,eta1)
          deallocate(deta2,aeta2,eta2)
          allocate(deta1(nsig_regional),aeta1(nsig_regional),eta1(nsig_regional+1))
          allocate(deta2(nsig_regional),aeta2(nsig_regional),eta2(nsig_regional+1))
          do k=1,nsig_regional
             deta1(k)=deta1_new(k)
             aeta1(k)=aeta1_new(k)
             deta2(k)=deta2_new(k)
             aeta2(k)=aeta2_new(k)
          end do
          do k=1,nsig_regional+1
             eta1(k)=eta1_new(k)
             eta2(k)=eta2_new(k)
          end do
          deallocate(deta1_new,aeta1_new,eta1_new)
          deallocate(deta2_new,aeta2_new,eta2_new)
  
       end if
  
       write(lendian_out)iyear,imonth,iday,ihour,iminute,isecond,fhour,fminute, &
            nlon_regional,nlat_regional,nsig_regional, &
            dlmd_regional,dphd_regional,pt_regional,pdtop_regional,nmmb_verttype
       write(lendian_out)deta1              !  DETA1
       write(lendian_out)aeta1              !  AETA1
       write(lendian_out)eta1               !  ETA1
       write(lendian_out)deta2              !  DETA2
       write(lendian_out)aeta2              !  AETA2
       write(lendian_out)eta2               !  ETA2
       deallocate(deta1,aeta1,eta1,deta2,aeta2,eta2)
  
       deallocate(sg1,sg2,sgml1,sgml2,dsg1,dsg2)
       allocate(field2(nlon_regional*nlat_regional))
       allocate(field2b(nlon_regional*nlat_regional))
       allocate(field2c(nlon_regional*nlat_regional))
       allocate(recname(nrec),reclevtyp(nrec),reclev(nrec))
  
       allocate(glat(nlon_regional,nlat_regional),glon(nlon_regional,nlat_regional))
       allocate(  dx(nlon_regional,nlat_regional),  dy(nlon_regional,nlat_regional))
       allocate(glata(nlon_regional*nlat_regional),glona(nlon_regional*nlat_regional))
       allocate(  dxa(nlon_regional*nlat_regional),  dya(nlon_regional*nlat_regional))
       call nemsio_getfilehead(gfile,iret=iret,recname=recname,reclevtyp=reclevtyp, &
             reclev=reclev,lat=glata(:),lon=glona(:),dx=dxa(:),dy=dya(:))
       ii=0
       do j=1,nlat_regional
          do i=1,nlon_regional
             ii=ii+1
             glat(i,j)=glata(ii)
             glon(i,j)=glona(ii)
             dx  (i,j)=    dxa  (ii)
             dy  (i,j)=    dya  (ii)
          end do
       end do
  
  !     following detects if glat, glon are in degree units.  If so,
  !      they are converted to radians.
  
       call this%latlon2radians(glat,glon,dx,dy,nlon_regional,nlat_regional)
  
  !                  GLAT
  
       if(print_verbose)then
          write(6,*)' convert_nems_nmmb: max,min GLAT=', &
            rad2deg*maxval(glat),rad2deg*minval(glat)
          write(6,*)' convert_nems_nmmb: glat(1,1),glat(nlon,1)=', &
            rad2deg*glat(1,1),rad2deg*glat(nlon_regional,1)
          write(6,*)' convert_nems_nmmb: glat(1,nlat),glat(nlon,nlat)=', &
            rad2deg*glat(1,nlat_regional),rad2deg*glat(nlon_regional,nlat_regional)
          write(6,*)' convert_nems_nmmb: my guess at tph0d = ', &
            rad2deg*glat(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)
       end if
       ctph0=cos(glat(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
       stph0=sin(glat(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  
  !                  DX_NMM
  
       if(print_verbose)then
          write(6,*)' convert_nems_nmmb: max,min DX_NMM=', &
            maxval(dx),minval(dx)
          write(6,*)' convert_nems_nmmb: dx_nmm(1,1),dx_nmm(nlon,1)=', &
            dx(1,1),     dx(nlon_regional,1)
          write(6,*)' convert_nems_nmmb: dx_nmm(1,nlat),dx_nmm(nlon,nlat)=', &
            dx(1,nlat_regional),     dx(nlon_regional,nlat_regional)
       end if
  
       write(lendian_out)glat,dx            !  GLAT,DX_NMM  !?????????????check to see if dx, dy backwards
                                                            !?????????????? in existing wrf nmm ????????
  
  !                  GLON
  
       if(print_verbose)then
          write(6,*)' convert_nems_nmmb: max,min GLON=',rad2deg*maxval(  glon),rad2deg*minval(  glon)
          write(6,*)' convert_nems_nmmb: glon(1,1),glon(nlon,1)=',rad2deg*glon(1,1),rad2deg*glon(nlon_regional,1)
          write(6,*)' convert_nems_nmmb: glon(1,nlat),glon(nlon,nlat)=', &
            rad2deg*glon(1,nlat_regional),rad2deg*glon(nlon_regional,nlat_regional)
          write(6,*)' convert_nems_nmmb: my guess at tlm0d = ', &
            half*rad2deg*(  glon(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)+ &
                            glon(2+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
       end if
       tlm0=half*(  glon(1+(nlon_regional-1)/2,1+(nlat_regional-1)/2)+ &
                    glon(2+(nlon_regional-1)/2,1+(nlat_regional-1)/2))
  
  !                  DY_NMM
  
       if(print_verbose)then
          write(6,*)' convert_nems_nmmb: max,min DY_NMM=', &
            maxval(dy),minval(dy)
          write(6,*)' convert_nems_nmmb: dy_nmm(1,1),dy_nmm(nlon,1)=', &
            dy(1,1),     dy(nlon_regional,1)
          write(6,*)' convert_nems_nmmb: dy_nmm(1,nlat),dy_nmm(nlon,nlat)=', &
            dy(1,nlat_regional),     dy(nlon_regional,nlat_regional)
       end if
  
       write(lendian_out)glon,dy            !  GLON,DY_NMM
  
       write(lendian_out) wrfges
  
  !                   PINT               
  
       call nemsio_readrecv(gfile,'pres','layer',1,field2(:),iret=iret)
       update_pint=.false.
       if(iret==0) update_pint=.true.
       if(print_verbose)write(6,*)' convert_nems_nmmb: pint, iret,update_pint=',iret,update_pint
  
  !????????????????????????????????????????????????????????????????read z0 here to see what it looks like
       call nemsio_readrecv(gfile,'zorl','sfc',1,field2b(:),iret=iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: iret,max,min Z0=',iret, &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  Z0 (?)  ask if zorl is same as z0
  !?????????????????????????????????????????????????????????????????
       call nemsio_readrecv(gfile,'tsea','sfc',1,field2b(:),iret=iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: iret,max,min SST=',iret, &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SST
  !????????????????????????????????????????????
       call nemsio_readrecv(gfile,'tg','sfc',1,field2b(:),iret=iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: iret,max,min TG=',iret, &
            maxval(field2b),minval(field2b)
       call nemsio_readrecv(gfile,'ths','sfc',1,field2b(:),iret=iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: iret,max,min THS=',iret, &
            maxval(field2b),minval(field2b)
       call nemsio_readrecv(gfile,'dpres','hybrid sig lev',1,field2c(:),iret=iret)
       if(print_verbose)write(6,*)' convert_nems_nmmb: iret,max,min PD=',iret, &
            maxval(field2c),minval(field2c)
       if(nmmb_verttype=='OLD') then
          pd_to_ps=r0_01*(pdtop_regional+pt_regional)
       else
          pd_to_ps=r0_01*pt_regional
       end if
       if(print_verbose)write(6,*)' pdtop_regional,pt_regional,pd_to_ps=',pdtop_regional,pt_regional,pd_to_ps
       do i=1,nlon_regional*nlat_regional
          pd=r0_01*field2c(i)
          psfc_this=pd+pd_to_ps
          ratio=(one_tenth*psfc_this/r100)
          factor=ratio**rd_over_cp
          field2c(i)=field2b(i)*factor
       end do
       if(print_verbose)then
          write(6,*)' nmmb_verttype=',nmmb_verttype
          write(6,*)' max diff ths-ts=',maxval(field2b-field2c)
          write(6,*)' min diff ths-ts=',minval(field2b-field2c)
          write(6,*)' convert_nems_nmmb: iret,max,min TS=',iret, &
               maxval(field2c),minval(field2c)
       end if
  
       write(lendian_out)field2c     !  TSK   (ths converted to ts)
  !????????????????????????????????????????sm
       call nemsio_readrecv(gfile,'sm','sfc',1,field2b(:),iret=iret)
       write(6,*)' convert_nems_nmmb: iret,max,min SM=',iret, &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SM
       call nemsio_readrecv(gfile,'sice','sfc',1,field2b(:),iret=iret)
       write(6,*)' convert_nems_nmmb: iret,max,min SICE=',iret, &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SICE
       call nemsio_readrecv(gfile,'sno','sfc',1,field2b(:),iret=iret)
       write(6,*)' convert_nems_nmmb: iret,max,min SNO=',iret, &
            maxval(field2b),minval(field2b)
       write(lendian_out)field2b     !  SNO
       deallocate(field2,field2b,field2c,recname,reclevtyp,reclev,glat,glon,dx,dy)
       deallocate(glata,glona,dxa,dya)
    
       call nemsio_close(gfile,iret=iret)
       write(6,*)' close nemsio file, iret=',iret
       close(lendian_out)
    enddo n_loop
  
  end subroutine convert_nems_nmmb_wrf
  subroutine latlon2radians(glat,glon,dx,dy,nx,ny)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    latlon2radians  check for degrees and convert to radians
  !   prgmmr: parrish          org: np22                date: 2013-04-18
  !
  ! abstract: Find 2x2 box with max range of glat, then compute ratio of 
  !             max_range_glat*rearth to max dx, dy over the 4 corners.
  !             This ratio should be between approximately 1 and 1.4 
  !             If the units of glat are degrees, then the ratio will be 
  !             between about 57 and 80.  So a unique determination
  !             can be made about the units of glat, glon.
  !             If it is determined that the units are degrees, then glat, glon
  !             will be converted to radians.
  !
  ! program history log:
  !   2013-04-18  parrish - initial documentation
  !
  !   input argument list:
  !     glat             - latitudes at model gridpoints (degrees or radians)
  !     glon             - longitudes at model gridpoints (degrees or radians)
  !     dx               - grid increment at model gridpoints in x direction (meters)
  !     dy               - grid increment at model gridpoints in y direction (meters)
  !     nx               - number of points in x direction
  !     ny               - number of points in y direction
  !
  !   output argument list:
  !     glat             - latitudes at model gridpoints (radians)
  !     glon             - longitudes at model gridpoints (radians)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: r_single,i_kind
    use constants, only: zero,deg2rad,rearth
    implicit none
  
    integer(i_kind),intent(in   ) :: nx,ny
    real(r_single) ,intent(inout) :: glat(nx,ny),glon(nx,ny)
    real(r_single) ,intent(in   ) :: dx(nx,ny),dy(nx,ny)
  
    integer(i_kind) i,ip,j,jp,iskip,jskip
    real(r_single) boxdlatmax,boxdxymax,ratiomax,srearth
   
    srearth=rearth
    ratiomax=zero
  !  don't need to check all 2x2 boxes.  Do every tenth in each direction
    jskip=max(1,ny/10)
    iskip=max(1,nx/10)
    do j=1,ny-1,jskip
       jp=j+1
       do i=1,nx-1,iskip
          ip=i+1
          boxdlatmax=max(glat(i,j),glat(ip,j),glat(i,jp),glat(ip,jp)) &
                    -min(glat(i,j),glat(ip,j),glat(i,jp),glat(ip,jp))
          boxdxymax=max(dx(i,j),dx(ip,j),dx(i,jp),dx(ip,jp), &
                        dy(i,j),dy(ip,j),dy(i,jp),dy(ip,jp))
          ratiomax=max(srearth*boxdlatmax/boxdxymax,ratiomax)
       end do
    end do
    write(6,'(" maximum ratio of boxdlatmax*rearth/boxdxymax =",f12.2)') ratiomax
    write(6,'("   If 1 < ratiomax < 1.4, then glat,glon units are radians")')
    write(6,'("   If 57 < ratiomax < 80, then glat,glon units are degrees")')
    if(ratiomax > 20._r_single ) then
       do j=1,ny
          do i=1,nx
             glat(i,j)=deg2rad*glat(i,j)
             glon(i,j)=deg2rad*glon(i,j)
          end do
       end do
    end if
  
  end subroutine latlon2radians
  subroutine count_recs_wrf_binary_file(this,in_unit,wrfges,nrecs)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    count_recs_binary_file  count # recs on wrf binary file
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: count number of sequential records contained in wrf binary
  !             file.  this is done by opening the file in direct access
  !             mode with block length of 2**20, the size of the physical
  !             blocks on ibm "blue" and "white" machines.  for optimal
  !             performance, change block length to correspond to the
  !             physical block length of host machine disk space. 
  !             records are counted by looking for the 4 byte starting
  !             and ending sequential record markers, which contain the
  !             record size in bytes.  only blocks are read which are known
  !             by simple calculation to contain these record markers.
  !             even though this is done on one processor, it is still
  !             very fast, and the time will always scale by the number of
  !             sequential records, not their size.  this step and the
  !             following inventory step consistently take less than 0.1 seconds
  !             to complete.
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2005-02-17  todling, ifdef'ed wrf code out
  !   2006-04-06  middlecoff - replace fortran open,close with openfileread,closefile
  !   2012-10-11  parrish - add calls to to_native_endianness_i4 (when byte_swap=.true.) after all
  !                           direct access reads from wrf binary file (through subroutine next_buf)
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges - filename of input wrf binary restart file
  !
  !   output argument list:
  !     nrecs            - number of sequential records found on input wrf binary restart file.
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
  !   do an initial read through of a wrf binary file, and get total number of sequential file records
  
    use kinds, only: i_byte,i_long,i_llong,i_kind
    use native_endianness, only: byte_swap
    implicit none
  
    class(get_wrf_binary_interface_class), intent(inout) :: this
    integer(i_kind),intent(in   ) :: in_unit
    character(9)   ,intent(in   ) :: wrfges
    integer(i_kind),intent(  out) :: nrecs
  
    character(10) cwrfges
    integer(i_llong) nextbyte,locbyte,thisblock
    integer(i_byte) lenrec4(4)
    integer(i_long) lenrec(1),lensave
    equivalence (lenrec4(1),lenrec(1))
    integer(i_byte) missing4(4)
    integer(i_long) missing
    equivalence (missing,missing4(1))
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_llong) num_swap
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,loc_count,nreads
    logical lastbuf
    integer(i_kind) ierr
  
    cwrfges = wrfges
    cwrfges(10:10) = char(0)
    call openfileread (in_unit, ierr, cwrfges)
  ! open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
    nrecs=0
    missing=-9999_i_long
    nextbyte=0_i_llong
    locbyte=lrecl
    nreads=0
    lastbuf=.false.
    do
  
  !    get length of next record
  
       do i=1,4
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) then
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
          lenrec4(i)=buf(locbyte)
       end do
       if(byte_swap) then
          num_swap=1
          call to_native_endianness_i4(lenrec,num_swap)
       end if
       if(lenrec(1) <= 0_i_long .and. lastbuf) then
          call closefile(in_unit,ierr)
          return
       end if
       if(lenrec(1) <= 0_i_long .and. .not.lastbuf) then
          write(6,*)' problem in count_recs_wrf_binary_file, lenrec has bad value before end of file'
          write(6,*)'     lenrec =',lenrec(1)
          call closefile(in_unit,ierr)
          return
       end if
       nextbyte=nextbyte+1_i_llong
       locbyte=locbyte+1_i_llong
       if(locbyte > lrecl .and. lastbuf) then
          call closefile(in_unit,ierr)
          return
       end if
       if(locbyte > lrecl) then
          call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
       end if
  
       nrecs=nrecs+1
      
       loc_count=1
       do i=2,4
          if(loc_count>=lenrec(1)) exit
          loc_count=loc_count+1
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf)then
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
       end do
       do i=1,4
          if(loc_count>=lenrec(1)) exit
          loc_count=loc_count+1
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) then
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
       end do
       nextbyte=nextbyte-loc_count+lenrec(1)
       locbyte=locbyte-loc_count+lenrec(1)
       if(locbyte > lrecl .and. lastbuf) then
          call closefile(in_unit,ierr)
          return
       end if
       if(locbyte > lrecl) then
          call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
       end if
       lensave=lenrec(1)
       do i=1,4
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) then
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
          lenrec4(i)=buf(locbyte)
       end do
       if(byte_swap) then
          num_swap=1
          call to_native_endianness_i4(lenrec,num_swap)
       end if
       if(lenrec(1) /= lensave) then
          write(6,*)' problem in count_recs_wrf_binary_file, beginning and ending rec len words unequal'
          write(6,*)'     begining reclen =',lensave
          write(6,*)'       ending reclen =',lenrec(1)
          write(6,*)'             in_unit =',in_unit
          call closefile(in_unit,ierr)
          return
       end if
  
      
    end do
  
    write(6,*)' reached impossible place in count_recs_wrf_binary_file'
    call closefile(in_unit,ierr)
    return
  
  end subroutine count_recs_wrf_binary_file
  
  subroutine initialize_byte_swap_wrf_binary_file(this,in_unit,wrfges)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    initialize_byte_swap_wrf_binary_file  set byte_swap
  !   prgmmr: parrish          org: np22                date: 2012-10-11
  !
  ! abstract:  compare endian format of binary file wrfges and set variable byte_swap (a public variable in
  !              module native_endianness) true if file endian format is different from machine endian format,
  !              otherwise set byte_swap=false.
  !
  ! program history log:
  !   2012-10-11  parrish
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges           - binary input file name.
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_long,i_llong,i_kind
    use native_endianness, only: byte_swap
    implicit none
  
    class(get_wrf_binary_interface_class), intent(inout) :: this
    integer(i_kind) ,intent(in   ) :: in_unit
    character(9)    ,intent(in   ) :: wrfges
  
    character(10) cwrfges
    integer(i_llong) nextbyte,locbyte,thisblock
    integer(i_byte) lenrec4(4)
    integer(i_byte) lenrec4_swap(4)
    integer(i_long) lenrec(1)
    integer(i_long) lenrec_swap
    equivalence (lenrec4(1),lenrec(1))
    equivalence (lenrec4_swap(1),lenrec_swap)
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,nreads
    logical lastbuf
    integer(i_kind) ierr
  
  
    cwrfges = wrfges
    cwrfges(10:10) = char(0)
    call openfileread (in_unit, ierr, cwrfges)
  ! open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
    nextbyte=0_i_llong
    locbyte=lrecl
    nreads=0
    lastbuf=.false.
  
  ! get length of 1st record, then use to set byte_swap.
  
    do i=1,4
       nextbyte=nextbyte+1_i_llong
       locbyte=locbyte+1_i_llong
       if(locbyte > lrecl) call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
       lenrec4(i)=buf(locbyte)
       lenrec4_swap(5-i)=buf(locbyte)
    end do
    byte_swap = lenrec(1) <= 0 .or. lenrec(1) > 4096
       
!   write(6,*)' byte_swap,lenrec4,lenrec4_swap=',byte_swap,lenrec4,lenrec4_swap
!   write(6,*)' byte_swap,lenrec,lenrec_swap=',byte_swap,lenrec(1),lenrec_swap
  
    call closefile(in_unit,ierr)
  
  end subroutine initialize_byte_swap_wrf_binary_file

  subroutine inventory_wrf_binary_file(this,in_unit,wrfges,nrecs, &
                                       datestr_all,varname_all,memoryorder_all,domainend_all, &
                                       start_block,end_block,start_byte,end_byte,file_offset)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    inventory_wrf_binary_file  get contents of wrf binary file
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: generate list of contents and map of wrf binary file which can be
  !             used for reading and writing with mpi io routines.
  !             same basic routine as count_recs_wrf_binary_file, except 
  !             now wrf unpacking routines are used to decode wrf header
  !             records, and send back lists of variable mnemonics, dates,
  !             grid dimensions, and byte addresses relative to start of 
  !             file for each field (this is used by mpi io routines).
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2006-04-06  middlecoff - replace fortran open,close with openfileread,closefile
  !   2007-04-12  parrish - add output variable memoryorder_all to be used with modifications
  !                          which allow any combination of ikj/ijk grid ordering for 3D fields.
  !   2012-10-11  parrish - add calls to to_native_endianness_i4 (when byte_swap=.true.) after all
  !                           direct access reads from wrf binary file (through subroutine next_buf)
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges - filename of input wrf binary restart file
  !     nrecs            - number of sequential records found on input wrf binary restart file.
  !                          (obtained by a previous call to count_recs_wrf_binary_file)
  !
  !   output argument list:  (all following dimensioned nrecs)
  !     datestr_all      - date character string for each field, where applicable (or else blanks)
  !     varname_all      - wrf mnemonic for each variable, where applicable (or blank)
  !     memoryorder_all
  !     domainend_all    - dimensions of each field, where applicable (up to 3 dimensions)
  !     start_block      - direct access block number containing 1st byte of record
  !                            (after 4 byte record mark)
  !     end_block        - direct access block number containing last byte of record
  !                            (before 4 byte record mark)
  !     start_byte       - relative byte address in direct access block of 1st byte of record
  !     end_byte         - relative byte address in direct access block of last byte of record
  !     file_offset      - absolute address of byte before 1st byte of record (used by mpi io)
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_long,i_llong,i_kind
  ! use module_internal_header_util, only: int_get_ti_header_char,int_get_write_field_header
    use native_endianness, only: byte_swap
    implicit none
  
    class(get_wrf_binary_interface_class), intent(inout) :: this
    integer(i_kind) ,intent(in   ) :: in_unit,nrecs
    character(9)    ,intent(in   ) :: wrfges
    character(132)  ,intent(  out) :: datestr_all(nrecs),varname_all(nrecs),memoryorder_all(nrecs)
    integer(i_kind) ,intent(  out) :: domainend_all(3,nrecs)
    integer(i_kind) ,intent(  out) :: start_block(nrecs),end_block(nrecs)
    integer(i_kind) ,intent(  out) :: start_byte(nrecs),end_byte(nrecs)
    integer(i_llong),intent(  out) :: file_offset(nrecs)
  
    character(10) cwrfges
    integer(i_kind) irecs
    integer(i_llong) nextbyte,locbyte,thisblock
    integer(i_byte) lenrec4(4)
    integer(i_long) lenrec(1),lensave
    equivalence (lenrec4(1),lenrec(1))
    integer(i_byte) missing4(4)
    integer(i_long) missing
    equivalence (missing,missing4(1))
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_llong) num_swap
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,loc_count,nreads
    logical lastbuf
    integer(i_byte) hdrbuf4(2048)
    integer(i_long) hdrbuf(512)
    equivalence(hdrbuf(1),hdrbuf4(1))
    integer(i_kind),parameter:: int_field       =       530
    integer(i_kind),parameter:: int_dom_ti_char =       220
    integer(i_kind) hdrbufsize
    integer(i_kind) inttypesize
    integer(i_kind) datahandle
    character(128) element,dumstr,strdata
    integer(i_kind) loccode
    character(132) blanks
    integer(i_kind) typesize
    integer(i_kind) fieldtype
    integer(i_kind) domaindesc
    character(132) memoryorder,stagger,dimnames(3)
    integer(i_kind) domainstart(3),domainend(3)
    integer(i_kind) patchstart(3),patchend(3)
    character(132) datestr,varname
    integer(i_kind) itypesize
    integer(i_kind) ierr
  
    call wrf_sizeof_integer(itypesize)
    inttypesize=itypesize
  
  
    blanks=trim(' ')
  
    cwrfges = wrfges
    cwrfges(10:10) = char(0)
    call openfileread (in_unit, ierr, cwrfges)
  ! open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
    irecs=0
    missing=-9999_i_long
    nextbyte=0_i_llong
    locbyte=lrecl
    nreads=0
    lastbuf=.false.
    do
  
  !    get length of next record
  
       do i=1,4
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) then
             write(6,*)' normal end of file reached in inventory_wrf_binary_file'
             write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
          lenrec4(i)=buf(locbyte)
       end do
       if(byte_swap) then
          num_swap=1
          call to_native_endianness_i4(lenrec,num_swap)
       end if
       if(lenrec(1) <= 0_i_long .and. lastbuf) then
          write(6,*)' normal end of file reached in inventory_wrf_binary_file'
          write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
          call closefile(in_unit,ierr)
          return
       end if
       if(lenrec(1) <= 0_i_long .and. .not. lastbuf) then
          write(6,*)' problem in inventory_wrf_binary_file, lenrec has bad value before end of file'
          write(6,*)'     lenrec =',lenrec(1)
          call closefile(in_unit,ierr)
          return
       end if
  
       if(mod(lenrec(1),4)/=0) then
          write(6,*)' problem in inventory_wrf_binary_file, lenrec not a multiple of 4'
          write(6,*)'     lenrec =',lenrec(1)
          call closefile(in_unit,ierr)
          return
       end if
       nextbyte=nextbyte+1_i_llong
       locbyte=locbyte+1_i_llong
       if(locbyte > lrecl .and. lastbuf) then
          write(6,*)' normal end of file reached in inventory_wrf_binary_file'
          write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
          call closefile(in_unit,ierr)
          return
       end if
       if(locbyte > lrecl) then
          call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
       end if
  
       irecs=irecs+1
       start_block(irecs)=thisblock
       start_byte(irecs)=locbyte
       file_offset(irecs)=nextbyte-1_i_llong
       hdrbuf4(1)=buf(locbyte)
       hdrbuf4(2:4)=missing4(2:4)
       hdrbuf4(5:8)=missing4(1:4)
       datestr_all(irecs)=blanks
       varname_all(irecs)=blanks
       memoryorder_all(irecs)=blanks
       domainend_all(1:3,irecs)=0
  
       loc_count=1
       do i=2,8
          if(loc_count>=lenrec(1)) exit
          loc_count=loc_count+1
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) then
             write(6,*)' normal end of file reached in inventory_wrf_binary_file'
             write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
          hdrbuf4(i)=buf(locbyte)
       end do
       if(byte_swap) then
          num_swap=2
          call to_native_endianness_i4(hdrbuf,num_swap)
       end if
  
  !     if(lenrec(1)==2048_i_long) write(6,*)' irecs,hdrbuf(2),int_dom_ti_char,int_field=', &
  !                                       irecs,hdrbuf(2),int_dom_ti_char,int_field
       if(lenrec(1)==2048_i_long.and.(hdrbuf(2) == int_dom_ti_char .or. hdrbuf(2) == int_field)) then
  
  !    bring in next full record, so we can unpack datestr, varname, and domainend
          do i=9,lenrec(1)
             loc_count=loc_count+1
             nextbyte=nextbyte+1_i_llong
             locbyte=locbyte+1_i_llong
             if(locbyte > lrecl .and. lastbuf) then
                write(6,*)' normal end of file reached in inventory_wrf_binary_file'
                write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
                call closefile(in_unit,ierr)
                return
             end if
             if(locbyte > lrecl) then
                call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
             end if
             hdrbuf4(i)=buf(locbyte)
          end do
          if(byte_swap) then
             num_swap=(lenrec(1)/4)-2
             call to_native_endianness_i4(hdrbuf(3),num_swap)
          end if
  
          if(hdrbuf(2) == int_dom_ti_char) then
  
             call this%int_get_ti_header_char(hdrbuf,hdrbufsize,inttypesize, &
                      datahandle,element,dumstr,strdata,loccode)
             varname_all(irecs)=trim(element)
             datestr_all(irecs)=trim(strdata)
  !           write(6,*)' irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),trim(datestr_all(irecs))
  
          else
  
  
             call this%int_get_write_field_header(hdrbuf,hdrbufsize,typesize, &
                datahandle,datestr,varname,fieldtype, &
                domaindesc,memoryorder,stagger,dimnames, &
                domainstart,domainend,patchstart,patchend)
             varname_all(irecs)=trim(varname)
             datestr_all(irecs)=trim(datestr)
             memoryorder_all(irecs)=trim(memoryorder)
             domainend_all(1:3,irecs)=domainend(1:3)
  !           write(6,*)' irecs,datestr,domend,varname = ', &
  !                 irecs,trim(datestr_all(irecs)),domainend_all(1:3,irecs),trim(varname_all(irecs))
  
          end if
  
       end if
  
       nextbyte=nextbyte-loc_count+lenrec(1)
       locbyte=locbyte-loc_count+lenrec(1)
       if(locbyte > lrecl .and. lastbuf) then
          write(6,*)' normal end of file reached in inventory_wrf_binary_file'
          write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
          call closefile(in_unit,ierr)
          return
       end if
       if(locbyte > lrecl) then
          call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
       end if
       end_block(irecs)=thisblock
       end_byte(irecs)=locbyte
       lensave=lenrec(1)
       do i=1,4
          nextbyte=nextbyte+1_i_llong
          locbyte=locbyte+1_i_llong
          if(locbyte > lrecl .and. lastbuf) then
             write(6,*)' normal end of file reached in inventory_wrf_binary_file'
             write(6,*)'  nblocks=',thisblock,' irecs,nrecs=',irecs,nrecs,' nreads=',nreads
             call closefile(in_unit,ierr)
             return
          end if
          if(locbyte > lrecl) then
             call this%next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
          end if
          lenrec4(i)=buf(locbyte)
       end do
       if(byte_swap) then
          num_swap=1
          call to_native_endianness_i4(lenrec,num_swap)
       end if
       if(lenrec(1) /= lensave) then
          write(6,*)' problem in inventory_wrf_binary_file, beginning and ending rec len words unequal'
          write(6,*)'     begining reclen =',lensave
          write(6,*)'       ending reclen =',lenrec(1)
          write(6,*)'               irecs =',irecs
          write(6,*)'               nrecs =',nrecs
          call closefile(in_unit,ierr)
          return
       end if
      
    end do
  
    write(6,*)' reached impossible place in inventory_wrf_binary_file'
    call closefile(in_unit,ierr)
    return
  
  
  end subroutine inventory_wrf_binary_file
  subroutine next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    next_buf    bring in next direct access block
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: bring in next direct access block when needed, as the file is scanned
  !             from beginning to end during counting and inventory of records.
  !             (subroutines count_recs_wrf_binary_file and inventory_wrf_binary_file)
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2006-04-06  middlecoff - replace direct access read with getbytes
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     nextbyte         - byte number from beginning of file that is desired 
  !     locbyte          - byte number from beginning of last block read for desired byte
  !     lrecl            - direct access block length
  !     nreads           - number of blocks read before now (for diagnostic information only)
  !     lastbuf          - logical, if true, then no more blocks, so return 
  !
  !   output argument list:
  !     buf              - output array containing contents of next block
  !     locbyte          - byte number from beginning of new block read for desired byte
  !     thisblock        - number of new block being read by this routine
  !     nreads           - number of blocks read now (for diagnostic information only)
  !     lastbuf          - logical, if true, then at end of file.
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_llong,i_kind
    implicit none
  
    integer(i_llong),intent(in   ) :: lrecl
    integer(i_kind) ,intent(in   ) :: in_unit
    integer(i_llong),intent(in   ) :: nextbyte
    integer(i_byte) ,intent(  out) :: buf(lrecl)
    integer(i_llong),intent(  out) :: thisblock
    integer(i_kind) ,intent(inout) :: nreads
    integer(i_llong),intent(inout) :: locbyte
    logical         ,intent(inout) :: lastbuf
  
    integer(i_kind) ierr
  
    if(lastbuf) return
  
    ierr=0
    nreads=nreads+1
  
  !  compute thisblock:
  
    thisblock = 1_i_llong + (nextbyte-1_i_llong)/lrecl
  
    locbyte = 1_i_llong+mod(locbyte-1_i_llong,lrecl)
  
  ! The Fortran standard does not 
  !  - specify what iostat should be for a DA read past the EOF
  !  - provide a way to detect end-of-file for a DA file
  !  - apply the concept end-of-file to direct-access files
  ! Consequently,the standard does not specify what the contents
  ! of the buffer will be for locations past the EOF.
  
  ! Hence the replacement of the DA read below with the call
  ! to getbytes
  
  ! read(in_unit,rec=thisblock,iostat=ierr)buf
  ! lastbuf = ierr /= 0
  
    call getbytes(in_unit, buf, thisblock, lrecl, ierr)
    lastbuf = ierr == 1
  
  end subroutine next_buf
  subroutine retrieve_index(index,string,varname_all,nrecs)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    retrieve_index  get record number of desired variable
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: by examining previously generated inventory of wrf binary restart file,
  !             find record number that contains the header record for variable
  !             identified by input character variable "string".
  !
  ! program history log:
  !   2004-11-29  parrish
  !
  !   input argument list:
  !     string           - mnemonic for variable desired
  !     varname_all      - list of all mnemonics obtained from inventory of file
  !     nrecs            - total number of sequential records counted in wrf
  !                        binary restart file
  !
  !   output argument list:
  !     index            - desired record number
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: i_kind
    implicit none
  
    integer(i_kind),intent(in   ) :: nrecs
    integer(i_kind),intent(  out) :: index
    character(*)   ,intent(in   ) :: string
    character(132) ,intent(in   ) :: varname_all(nrecs)
  
    integer(i_kind) i
  
    do i=1,nrecs
       if(trim(string) == trim(varname_all(i))) then
          index=i
          return
       end if
    end do
  
    write(6,*)'RETRIEVE_INDEX:  ***PROBLEM*** reading wrf nmm binary file, ',&
         'rec id "',trim(string),'" not found'
    index=-1
    return
  
  end subroutine retrieve_index
  
  subroutine retrieve_field_i1(in_unit,wrfges,outi1,start_block,end_block,start_byte,end_byte)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    retrieve_field_i1 retrieve single integer(4) variable from binary restart file
  !   prgmmr: parrish          org: np22                date: 2013-01-29
  !
  ! abstract: still using direct access, retrieve a field from the wrf binary restart file.
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2012-10-11  parrish - add calls to to_native_endianness_i4 (when byte_swap=.true.) after all
  !                           direct access reads from wrf binary file
  !   2013-01-24  parrish - specialized version of original subroutine retrieve_field for getting
  !                          single integer(4) variable from wrf binary file
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges - filename of input wrf binary restart file
  !     start_block      - direct access block number containing 1st byte of record
  !                            (after 4 byte record mark)
  !     end_block        - direct access block number containing last byte of record
  !                            (before 4 byte record mark)
  !     start_byte       - relative byte address in direct access block of 1st byte of record
  !     end_byte         - relative byte address in direct access block of last byte of record
  !
  !   output argument list:
  !     out              - output buffer where desired field is deposited
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_kind,i_llong,i_long
    use native_endianness, only: byte_swap
    use gsi_io, only: verbose
    implicit none
  
    integer(i_kind),intent(in   ) :: in_unit
    character(9)   ,intent(in   ) :: wrfges
    integer(i_kind),intent(in   ) :: start_block,end_block,start_byte,end_byte
    integer(i_kind),intent(  out) :: outi1
  
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_llong) num_swap
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    integer(i_byte) out(4)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,ii,k,ibegin,iend,ierr
    logical print_verbose
  
    print_verbose=.false.
    if(verbose)print_verbose=.true.
    open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  
    if(print_verbose)write(6,*)'RETRIEVE_FIELD:  start_block,end_block,s_,e_byte=',&
         start_block,end_block,start_byte,end_byte
    if(mod(start_byte-1,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(start_byte-1,4) /= 0'
    if(mod(end_byte,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(end_byte,4) /= 0'
    ii=0
    do k=start_block,end_block
       read(in_unit,rec=k,iostat=ierr)buf
       if(byte_swap) then
          ibegin=1 ; iend=lword
          if(k == start_block) ibegin=1+(start_byte-1)/4
          if(k == end_block) iend=end_byte/4
          num_swap=iend-ibegin+1
          call to_native_endianness_i4(buf4(ibegin),num_swap)
       end if
       ibegin=1 ; iend=lrecl
       if(k == start_block) ibegin=start_byte
       if(k == end_block) iend=end_byte
       do i=ibegin,iend
          ii=ii+1
          out(ii)=buf(i)
       end do
    end do
    close(in_unit)
  
    outi1=transfer(out,outi1)
    
  end subroutine retrieve_field_i1
  
  subroutine retrieve_field_r1(in_unit,wrfges,outr1,start_block,end_block,start_byte,end_byte)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    retrieve_field_r1 retrieve single real(4) variable from binary restart file
  !   prgmmr: parrish          org: np22                date: 2013-01-29
  !
  ! abstract: still using direct access, retrieve a field from the wrf binary restart file.
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2012-10-11  parrish - add calls to to_native_endianness_i4 (when byte_swap=.true.) after all
  !                           direct access reads from wrf binary file
  !   2013-01-24  parrish - specialized version of original subroutine retrieve_field for getting
  !                          single real(4) variable from wrf binary file
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges - filename of input wrf binary restart file
  !     start_block      - direct access block number containing 1st byte of record
  !                            (after 4 byte record mark)
  !     end_block        - direct access block number containing last byte of record
  !                            (before 4 byte record mark)
  !     start_byte       - relative byte address in direct access block of 1st byte of record
  !     end_byte         - relative byte address in direct access block of last byte of record
  !
  !   output argument list:
  !     out              - output buffer where desired field is deposited
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_kind,i_llong,i_long,r_single
    use native_endianness, only: byte_swap
    implicit none
  
    integer(i_kind),intent(in   ) :: in_unit
    character(9)   ,intent(in   ) :: wrfges
    integer(i_kind),intent(in   ) :: start_block,end_block,start_byte,end_byte
    real(r_single),intent(  out) :: outr1
  
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_llong) num_swap
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    integer(i_byte) out(4)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,ii,k,ibegin,iend,ierr
  
    open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  
    write(6,*)'RETRIEVE_FIELD:  start_block,end_block,s_,e_byte=',&
         start_block,end_block,start_byte,end_byte
    if(mod(start_byte-1,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(start_byte-1,4) /= 0'
    if(mod(end_byte,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(end_byte,4) /= 0'
    ii=0
    do k=start_block,end_block
       read(in_unit,rec=k,iostat=ierr)buf
       if(byte_swap) then
          ibegin=1 ; iend=lword
          if(k == start_block) ibegin=1+(start_byte-1)/4
          if(k == end_block) iend=end_byte/4
          num_swap=iend-ibegin+1
          call to_native_endianness_i4(buf4(ibegin),num_swap)
       end if
       ibegin=1 ; iend=lrecl
       if(k == start_block) ibegin=start_byte
       if(k == end_block) iend=end_byte
       do i=ibegin,iend
          ii=ii+1
          out(ii)=buf(i)
       end do
    end do
    close(in_unit)
  
    outr1=transfer(out,outr1)
    
  end subroutine retrieve_field_r1
  
  subroutine retrieve_field_rn1(in_unit,wrfges,outrn1,n1,start_block,end_block,start_byte,end_byte)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    retrieve_field_rn1 retrieve real(4) outrn1(n1) from wrf binary file
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: still using direct access, retrieve a field from the wrf binary restart file.
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2012-10-11  parrish - add calls to to_native_endianness_i4 (when byte_swap=.true.) after all
  !                           direct access reads from wrf binary file
  !   2013-01-24  parrish - specialized version of original subroutine retrieve_field for getting
  !                          real(4) outrn1(n1) from wrf binary file
  !   2013-01-26  parrish - change out(4) to out(4*n1)
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges - filename of input wrf binary restart file
  !     start_block      - direct access block number containing 1st byte of record
  !                            (after 4 byte record mark)
  !     end_block        - direct access block number containing last byte of record
  !                            (before 4 byte record mark)
  !     start_byte       - relative byte address in direct access block of 1st byte of record
  !     end_byte         - relative byte address in direct access block of last byte of record
  !
  !   output argument list:
  !     out              - output buffer where desired field is deposited
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_kind,i_llong,i_long,r_single
    use native_endianness, only: byte_swap
    use constants, only: zero
    implicit none
  
    integer(i_kind),intent(in   ) :: in_unit,n1
    character(9)   ,intent(in   ) :: wrfges
    integer(i_kind),intent(in   ) :: start_block,end_block,start_byte,end_byte
    real(r_single),intent(  out) :: outrn1(n1)
  
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_llong) num_swap
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    integer(i_byte) out(4*n1)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,ii,k,ibegin,iend,ierr,nretrieved
  
    open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  
    write(6,*)'RETRIEVE_FIELD:  start_block,end_block,s_,e_byte=',&
         start_block,end_block,start_byte,end_byte
    if(mod(start_byte-1,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(start_byte-1,4) /= 0'
    if(mod(end_byte,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(end_byte,4) /= 0'
    ii=0
    do k=start_block,end_block
       read(in_unit,rec=k,iostat=ierr)buf
       if(byte_swap) then
          ibegin=1 ; iend=lword
          if(k == start_block) ibegin=1+(start_byte-1)/4
          if(k == end_block) iend=end_byte/4
          num_swap=iend-ibegin+1
          call to_native_endianness_i4(buf4(ibegin),num_swap)
       end if
       ibegin=1 ; iend=lrecl
       if(k == start_block) ibegin=start_byte
       if(k == end_block) iend=end_byte
       do i=ibegin,iend
          ii=ii+1
          out(ii)=buf(i)
       end do
    end do
    close(in_unit)
  
    nretrieved=ii/4
    ii=1
    do i=1,min(nretrieved,n1)
       outrn1(i)=transfer(out(ii:ii+3),outrn1(i))
       ii=ii+4
    end do
    do i=min(nretrieved,n1)+1,n1
       outrn1(i)=zero
    end do
!   write(6,*)' in retrieve_field_rn1, num expected=',n1, ' num retrieved=',nretrieved
    
  end subroutine retrieve_field_rn1
  
  subroutine retrieve_field_rn1n2(in_unit,wrfges,outrn1n2,n1,n2,start_block,end_block,start_byte,end_byte)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    retrieve_field_rn1n2 retrieve real(4) outrn1n2(n1,n2) from wrf binary file
  !   prgmmr: parrish          org: np22                date: 2004-11-29
  !
  ! abstract: still using direct access, retrieve a field from the wrf binary restart file.
  !
  ! program history log:
  !   2004-11-29  parrish
  !   2012-10-11  parrish - add calls to to_native_endianness_i4 (when byte_swap=.true.) after all
  !                           direct access reads from wrf binary file
  !   2013-01-24  parrish - specialized version of original subroutine retrieve_field for getting
  !                          real(4) outrn1n2(n1,n2) from wrf binary file
  !   2013-01-26  parrish - change out(4) to out(4*n1*n2)
  !
  !   input argument list:
  !     in_unit          - fortran unit number where input file is opened through.
  !     wrfges - filename of input wrf binary restart file
  !     start_block      - direct access block number containing 1st byte of record
  !                            (after 4 byte record mark)
  !     end_block        - direct access block number containing last byte of record
  !                            (before 4 byte record mark)
  !     start_byte       - relative byte address in direct access block of 1st byte of record
  !     end_byte         - relative byte address in direct access block of last byte of record
  !
  !   output argument list:
  !     out              - output buffer where desired field is deposited
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_byte,i_kind,i_llong,i_long,r_single
    use native_endianness, only: byte_swap
    implicit none
  
    integer(i_kind),intent(in   ) :: in_unit,n1,n2
    character(9)   ,intent(in   ) :: wrfges
    integer(i_kind),intent(in   ) :: start_block,end_block,start_byte,end_byte
    real(r_single),intent(  out) :: outrn1n2(n1,n2)
  
    integer(i_llong),parameter:: lrecl=2**20_i_llong
    integer(i_llong),parameter:: lword=2**18_i_llong
    integer(i_llong) num_swap
    integer(i_long) buf4(lword)
    integer(i_byte) buf(lrecl)
    integer(i_byte) out(4*n1*n2)
    equivalence(buf4(1),buf(1))
    integer(i_kind) i,ii,j,k,ibegin,iend,ierr,nretrieved
  
    open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  
    write(6,*)'RETRIEVE_FIELD:  start_block,end_block,s_,e_byte=',&
         start_block,end_block,start_byte,end_byte
    if(mod(start_byte-1,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(start_byte-1,4) /= 0'
    if(mod(end_byte,4)/=0) write(6,*)' PROBLEM WITH RETRIEVE_FIELD, mod(end_byte,4) /= 0'
    ii=0
    do k=start_block,end_block
       read(in_unit,rec=k,iostat=ierr)buf
       if(byte_swap) then
          ibegin=1 ; iend=lword
          if(k == start_block) ibegin=1+(start_byte-1)/4
          if(k == end_block) iend=end_byte/4
          num_swap=iend-ibegin+1
          call to_native_endianness_i4(buf4(ibegin),num_swap)
       end if
       ibegin=1 ; iend=lrecl
       if(k == start_block) ibegin=start_byte
       if(k == end_block) iend=end_byte
       do i=ibegin,iend
          ii=ii+1
          out(ii)=buf(i)
       end do
    end do
    close(in_unit)
  
    nretrieved=ii/4
    ii=1
    do j=1,n2
       do i=1,n1
          outrn1n2(i,j)=transfer(out(ii:ii+3),outrn1n2(i,j))
          ii=ii+4
       end do
    end do
    write(6,*)' in retrieve_field_rn1n2, num expected=',n1*n2, ' num retrieved=',nretrieved
    
  end subroutine retrieve_field_rn1n2

  subroutine int_get_ti_header_char(this, hdrbuf, hdrbufsize, itypesize, &
                                DataHandle, Element, VarName, Data, code )
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    int_get_ti_header_char
  !   prgmmr: 
  !
  ! abstract: Same as int_gen_ti_header_char except that Data is read from the file.
  !
  ! program history log:
  !     2008-03-31  safford - add subroutine doc block
  !     2009-01-03  todling - wrapped unavailable routine int_get_ti_header_c within ifdef
  !     2009-09-28  guo     - flagged uninitialized variable to signal possible conflict.
  !
  !   input argument list:
  !     hdrbuf     - 
  !     itypesize  - 
  !     Element    - 
  !     Data       - 
  !     VarName    - 
  !
  !   output argument list:
  !     hdrbuf     - 
  !     hdrbufsize - 
  !     Element    - 
  !     Data       - 
  !     VarName    - 
  !     DataHandle - 
  !     code       - 
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: i_kind
    use gsi_io, only: verbose
    IMPLICIT NONE
  
  ! INCLUDE 'intio_tags.h'
    class(get_wrf_binary_interface_class), intent(inout) :: this
    INTEGER(i_kind), INTENT(INOUT) ::  hdrbuf(*)
    INTEGER(i_kind), INTENT(  OUT) ::  hdrbufsize
    INTEGER(i_kind), INTENT(IN   ) ::  itypesize
    CHARACTER*(*)  , INTENT(INOUT) ::  Element, Data, VarName
    INTEGER(i_kind), INTENT(  OUT) ::  DataHandle, code
  !Local
    INTEGER(i_kind) i, n, DummyCount, typesize
    CHARACTER * 132  dummyData
  !  logical, external :: debug_foo
  !
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    call int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                             DataHandle, dummyData, DummyCount, code )
    i = n/itypesize+1 ;
    call int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n
    call int_unpack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
    call int_unpack_string ( VarName  , hdrbuf( i ), n ) ; i = i + n
    hdrbufsize = hdrbuf(1)
    if(verbose)write(6,*)' in int_get_ti_header_char, hdrbufsize,itypesize,typesize=',&
                  hdrbufsize,itypesize,typesize
  
    RETURN
  END SUBROUTINE int_get_ti_header_char

  SUBROUTINE int_get_write_field_header ( this, hdrbuf, hdrbufsize, ftypesize, &
                                          DataHandle , DateStr , VarName , FieldType ,                 &
                                          DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                          DomainStart , DomainEnd ,                                    &
                                          PatchStart , PatchEnd )
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    int_get_write_field_header
  !   prgmmr: 
  !
  ! abstract:  See documentation block in int_gen_write_field_header() for 
  !            a description of a "write field" header.  
  !
  ! program history log:
  !     2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     hdrbuf     - 
  !     ftypesize  - 
  !     DateStr    -
  !     VarName    - 
  !     MemoryOrder
  !     Stagger
  !     DimNames
  !
  !   output argument list:
  !     hdrbuf     - 
  !     hdrbufsize - 
  !     ftypesize  - 
  !     DataHandle - 
  !     DateStr    -
  !     VarName    - 
  !     FieldType
  !     DomainDesc
  !     MemoryOrder
  !     Stagger
  !     DimNames
  !     DomainStart,DomainEnd
  !     PatchStart,PatchEnd
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
    use kinds, only: i_kind
    IMPLICIT NONE
  
  ! INCLUDE 'intio_tags.h'
    class(get_wrf_binary_interface_class), intent(inout) :: this
    INTEGER(i_kind)              , INTENT(INOUT) ::  hdrbuf(*)
    INTEGER(i_kind)              , INTENT(  OUT) ::  hdrbufsize
    INTEGER(i_kind)              , INTENT(INOUT) ::  ftypesize
    INTEGER(i_kind)              , INTENT(  OUT) :: DataHandle
    CHARACTER*(*)                , INTENT(INOUT) :: DateStr
    CHARACTER*(*)                , INTENT(INOUT) :: VarName
    INTEGER(i_kind)              , INTENT(  OUT) :: FieldType
    INTEGER(i_kind)              , INTENT(  OUT) :: DomainDesc
    CHARACTER*(*)                , INTENT(INOUT) :: MemoryOrder
    CHARACTER*(*)                , INTENT(INOUT) :: Stagger
    CHARACTER*(*)   ,dimension(*), INTENT(INOUT) :: DimNames
    INTEGER(i_kind) ,dimension(*), INTENT(  OUT) :: DomainStart, DomainEnd
    INTEGER(i_kind) ,dimension(*), INTENT(  OUT) :: PatchStart,  PatchEnd
  !Local
    integer(i_kind),parameter:: int_field       =       530
    CHARACTER*132 mess
    INTEGER(i_kind) i, n
  
    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
    end associate
    hdrbufsize = hdrbuf(1)
    IF ( hdrbuf(2) /= int_field ) THEN
       write(mess,*)'int_get_write_field_header: hdrbuf(2) ne int_field ',hdrbuf(2),int_field
       CALL wrf_error_fatal3 ( "module_internal_header_util.b" , 220 ,  mess )
    ENDIF
    ftypesize = hdrbuf(3)
  
    i = 4
    DataHandle = hdrbuf(i)     ; i = i+1
    call int_unpack_string( DateStr, hdrbuf(i), n )     ; i = i+n
    call int_unpack_string( VarName, hdrbuf(i), n )     ; i = i+n
    FieldType = hdrbuf(i)      ; i = i+1
    call int_unpack_string( MemoryOrder, hdrbuf(i), n ) ; i = i+n
    call int_unpack_string( Stagger, hdrbuf(i), n )     ; i = i+n
    call int_unpack_string( DimNames(1), hdrbuf(i), n ) ; i = i+n
    call int_unpack_string( DimNames(2), hdrbuf(i), n ) ; i = i+n
    call int_unpack_string( DimNames(3), hdrbuf(i), n ) ; i = i+n
    DomainStart(1) = hdrbuf(i)    ; i = i+1
    DomainStart(2) = hdrbuf(i)    ; i = i+1
    DomainStart(3) = hdrbuf(i)    ; i = i+1
    DomainEnd(1) = hdrbuf(i)       ; i = i+1
    DomainEnd(2) = hdrbuf(i)       ; i = i+1
    DomainEnd(3) = hdrbuf(i)       ; i = i+1
    PatchStart(1) = hdrbuf(i)     ; i = i+1
    PatchStart(2) = hdrbuf(i)     ; i = i+1
    PatchStart(3) = hdrbuf(i)     ; i = i+1
    PatchEnd(1) = hdrbuf(i)       ; i = i+1
    PatchEnd(2) = hdrbuf(i)       ; i = i+1
    PatchEnd(3) = hdrbuf(i)       ; i = i+1
    DomainDesc = hdrbuf(i)       ; i = i+1
  
    RETURN
  END SUBROUTINE int_get_write_field_header

  SUBROUTINE int_unpack_string ( str, buf, n )
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    int_unpack_string
  !   prgmmr: 
  !
  ! abstract:  This routine is used to extract a string from a sequence of integers.  
  !            The first integer is the string length.  
  !
  ! program history log:
  !     2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     str        -
  !     buf        -
  !
  !   output argument list:
  !     str        -
  !     n          -
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$
  
    use kinds, only: i_kind
    IMPLICIT NONE
  
    CHARACTER*(*)                , INTENT(  OUT) :: str
    INTEGER(i_kind)              , INTENT(  OUT) :: n       ! on return, N is the number of ints copied from buf
    INTEGER(i_kind), DIMENSION(*), INTENT(IN   ) :: buf
  !Local
    INTEGER(i_kind) i
    INTEGER(i_kind) strlen
  
    strlen = buf(1)
    str = ""
    DO i = 1, strlen
       str(i:i) = char(buf(i+1))
    ENDDO
    n = strlen + 1
  END SUBROUTINE int_unpack_string
end module get_wrf_binary_interface_mod
  !WRF:DRIVER_LAYER:UTIL
  !
  MODULE module_wrf_error
  !$$$   module documentation block
  !
  ! module:  module_wrf_error
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add module and subroutine doc blocks
  !
  ! subroutines included:
  !   wrf_at_debug_level           ---
  !   init_module_wrf_error        ---
  !
  ! variable definitions:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    use kinds, only: i_kind
    implicit none
  
  ! set default to private
    private
  ! set subroutines to public
    public :: wrf_at_debug_level
    public :: init_module_wrf_error
  ! set passed variables to public
    public :: wrf_debug_level
  
    INTEGER(i_kind) :: wrf_debug_level = 0
  
  CONTAINS
  
    LOGICAL FUNCTION wrf_at_debug_level ( level )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_at_debug_level
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     level    - debug level
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
      use kinds, only: i_kind
      IMPLICIT NONE
  
      INTEGER(i_kind) , INTENT(IN   ) :: level
  
      wrf_at_debug_level = ( level <= wrf_debug_level )
      RETURN
    END FUNCTION wrf_at_debug_level
  
    SUBROUTINE init_module_wrf_error
  !$$$   subprogram documentation block
  !
  ! subprogram:  init_module_wrf_error
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
      IMPLICIT NONE
    END SUBROUTINE init_module_wrf_error
  
  END MODULE module_wrf_error
  
    SUBROUTINE set_wrf_debug_level ( level )
  !$$$   subprogram documentation block
  !
  ! subprogram:  set_wrf_debug_level
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     level    - debug level
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
      USE module_wrf_error
      use kinds, only: i_kind
      IMPLICIT NONE
  
      INTEGER(i_kind) , INTENT(IN   ) :: level
  
      wrf_debug_level = level
      RETURN
    END SUBROUTINE set_wrf_debug_level
  
    SUBROUTINE get_wrf_debug_level ( level )
  !$$$   subprogram documentation block
  !
  ! subprogram:  get_wrf_debug_level
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !
  !   output argument list:
  !     level    - debug level
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
      USE module_wrf_error
      use kinds, only: i_kind
      IMPLICIT NONE
  
      INTEGER(i_kind) , INTENT(  OUT) :: level
  
      level = wrf_debug_level
      RETURN
    END SUBROUTINE get_wrf_debug_level
  
  
  SUBROUTINE wrf_debug( level , str )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_debug
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     level    - debug level
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    USE module_wrf_error
    use kinds, only: i_kind
    IMPLICIT NONE
  
    CHARACTER*(*) str
    INTEGER(i_kind) , INTENT (IN   ) :: level
    INTEGER(i_kind)                  :: debug_level
  
    CALL get_wrf_debug_level( debug_level )
    IF ( level <= debug_level ) THEN
      ! old behavior
       CALL wrf_message( str )
    ENDIF
    RETURN
  END SUBROUTINE wrf_debug
  
  SUBROUTINE wrf_message( str )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_message
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !    str
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    USE module_wrf_error
    IMPLICIT NONE
  
    CHARACTER*(*), intent(in   ) :: str
  
    write(6,*) TRIM(str)
    print*, TRIM(str)
  !TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
  !TBH:  Restore this call once this ESMF bug is fixed.
  !TBH#ifdef USE_LOGERR
  !TBH  IF ( WRFU_IsInitialized() ) THEN
  !TBH    CALL WRFU_LogWrite( TRIM(str), WRFU_LOG_INFO )
  !TBH  ENDIF
  !TBH#endif
  END SUBROUTINE wrf_message
  
  ! intentionally write to stderr only
  SUBROUTINE wrf_message2( str )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_message2
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !    str
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    USE module_wrf_error
    IMPLICIT NONE
  
    CHARACTER*(*), intent(in   ) :: str
  
    write(6,*) str
  !TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
  !TBH:  Restore this call once this ESMF bug is fixed.
  !TBH#ifdef USE_LOGERR
  !TBH  IF ( WRFU_IsInitialized() ) THEN
  !TBH    CALL WRFU_LogWrite( str, WRFU_LOG_INFO )
  !TBH  ENDIF
  !TBH#endif
  END SUBROUTINE wrf_message2
  
  SUBROUTINE wrf_error_fatal3( file_str, line, str )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_error_fatal3
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     file_str -
  !     line     -
  !     str      -
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    USE module_wrf_error
    use kinds, only: i_kind
    IMPLICIT NONE
  
    CHARACTER*(*)   , intent (in   ) :: file_str
    INTEGER(i_kind) , INTENT (IN   ) :: line  ! only print file and line if line > 0
    CHARACTER*(*)   , intent (in   ) :: str
    CHARACTER*256 :: line_str
  
    write(line_str,'(i6)') line
    CALL wrf_message( '-------------- FATAL CALLED ---------------' )
    ! only print file and line if line is positive
    IF ( line > 0 ) THEN
       CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
    ENDIF
    CALL wrf_message( str )
    CALL wrf_message( '-------------------------------------------' )
  ! CALL wrf_abort
    call stop2(199)
  END SUBROUTINE wrf_error_fatal3
  
  SUBROUTINE wrf_error_fatal( str )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_error_fatal
  !
  ! abstract:  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     str      -
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    USE module_wrf_error
    IMPLICIT NONE
  
    CHARACTER*(*),intent(in   ) :: str
  
    CALL wrf_error_fatal3 ( ' ', 0, str )
  END SUBROUTINE wrf_error_fatal
  
  SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
  !$$$   subprogram documentation block
  !
  ! subprogram:  wrf_check_error
  !
  ! abstract:   Check to see if expected value == actual value
  !             If not, print message and exit.  
  !
  ! program history log:
  !   2008-03-31  safford - add subroutine doc block
  !
  !   input argument list:
  !     expected -
  !     actual   -
  !     line     -
  !     str
  !     file_str
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    USE module_wrf_error
    use kinds, only: i_kind
    IMPLICIT NONE
  
    INTEGER(i_kind) , INTENT (IN   ) :: expected
    INTEGER(i_kind) , INTENT (IN   ) :: actual
    CHARACTER*(*)   , intent (in   ) :: str
    CHARACTER*(*)   , intent (in   ) :: file_str
    INTEGER(i_kind) , INTENT (IN   ) :: line
    CHARACTER (LEN=512)   :: rc_str
    CHARACTER (LEN=512)   :: str_with_rc
  
    IF ( expected /= actual ) THEN
       WRITE (rc_str,*) '  Routine returned error code = ',actual
       str_with_rc = TRIM(str // rc_str)
       CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
    ENDIF
  END SUBROUTINE wrf_check_error
