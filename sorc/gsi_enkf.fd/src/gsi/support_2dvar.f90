subroutine convert_binary_2d
!$$$  subprogram documentation block
!
! Adapted from convert_binary_mass
!   prgmmr: pondeca           org: np20                date: 2004-12-13
!
! abstract:
! Read in from restart file of 2dvar-only surface analysis and write
! the result to temporary binary file expected by read_2d_guess.
!
! program history log:
!   2004-12-13  pondeca
!   2006-04-06  middlecoff - change in_unit from 15 to 11 (big endian)
!                            and out_unit 55 to lendian_out
!   2006-09-15  treadon - use nhr_assimilation to build local guess filename
!   2007-03-13  derber - remove unused qsinv2 from jfunc use list
!   2008-04-03  safford - remove unused vars
!   2008-11-04  pondeca - add utility routines for 2dvar applications 
!                         on ndfd grid, ie., rtma applications
!   2008-11-04  pondeca - add routines for hilbert-curve based cross-validation
!   2008-11-04  pondeca - add routines for dew-point computation at
!                         station location. used for qc purposes in 2dvar.
!   2009-02-27  pondeca - add fgat to 2dvar
!   2011-02-09  zhu     - add gust,vis and pblh
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-16  carley/zhu - add tcamt and ceiling
!   2015-07-10  pondeca - add cloud ceiling height (cldch)
!   2016-05-03  pondeca - add uwnd10m, vwnd10m
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

  use kinds, only: r_single,i_kind
  use gsi_4dvar, only: nhr_assimilation
  use gsi_io, only: lendian_out,verbose
  use mpeu_util, only: die
  use qcmod, only: vis_thres,cldch_thres
  use obsmod, only: use_similarity_2dvar
  implicit none

! Declare local parameters
  real(r_single),parameter:: one_single = 1.0_r_single
  real(r_single),parameter:: r45 = 45.0_r_single

  character(6) filename
  character(9) wrfges

  integer(i_kind) in_unit,status_hdr
  integer(i_kind) hdrbuf(512)
  integer(i_kind) n
  integer(i_kind) i,j

  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field2(:,:),field2b(:,:)
  real(r_single),allocatable::field2c(:,:)
  integer(i_kind),allocatable::ifield2(:,:)
  real(r_single) rad2deg_single
  logical print_verbose

  data in_unit / 11 /

  print_verbose=.true.
  if(verbose)print_verbose=.true.
  n_loop: do n=1,9

     if(n==1)then
        wrfges = 'wrf_inout'
     else
        write(wrfges,'("wrf_inou",i1.1)')n
     endif
     open(in_unit,file=trim(wrfges),form='unformatted')
     if(print_verbose)write(6,*)' convert_binary_2d: in_unit,lendian_out=',in_unit,lendian_out
 
! Check for valid input file
     read(in_unit,iostat=status_hdr)hdrbuf
     if(n==1)then
        if(status_hdr /= 0) then
           write(6,*)'CONVERT_BINARY_2D:  problem with wrfges = ',&
                trim(wrfges),', Status = ',status_hdr
           call stop2(74)
        endif
     else
        if(status_hdr /= 0) then
           write(6,*)'CONVERT_BINARY_2D:  no off hour guess  ', trim(wrfges)
           close(in_unit)
           cycle n_loop
        endif
     endif

     write(filename,'("sigf",i2.2)')n+nhr_assimilation-1
     if(print_verbose)write(6,*)' CONVERT_BINARY_2D: in_unit,out_unit=',wrfges,',',filename
     open(lendian_out,file=filename,form='unformatted')
     rewind lendian_out
 
     read(in_unit) iyear,imonth,iday,ihour,iminute,isecond, &
                   nlon_regional,nlat_regional,nsig_regional
     if(print_verbose)then
        write(6,*)' convert_binary_2d: iy,m,d,h,m,s=',&
                 iyear,imonth,iday,ihour,iminute,isecond
        write(6,*)' convert_binary_2d: nlon,lat,sig_regional=',&
                 nlon_regional,nlat_regional,nsig_regional
     end if
     write(lendian_out) iyear,imonth,iday,ihour,iminute,isecond, &
                 nlon_regional,nlat_regional,nsig_regional
 

     allocate(field2(nlon_regional,nlat_regional))
     allocate(field2b(nlon_regional,nlat_regional))
     allocate(field2c(nlon_regional,nlat_regional))
     allocate(ifield2(nlon_regional,nlat_regional))

     read(in_unit) field2b,field2c !DX_MC,DY_MC

!                  XLAT
     rad2deg_single=r45/atan(one_single)
     read(in_unit)field2
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min XLAT(:,1)=',&
                 maxval(field2(:,1)),minval(field2(:,1))
        write(6,*)' convert_binary_2d: max,min XLAT(1,:)=',&
                 maxval(field2(1,:)),minval(field2(1,:))
        write(6,*)' convert_binary_2d: xlat(1,1),xlat(nlon,1)=',&
                 field2(1,1),field2(nlon_regional,1)
        write(6,*)' convert_binary_2d: xlat(1,nlat),xlat(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     end if
     field2=field2/rad2deg_single
     write(lendian_out)field2,field2b    !XLAT,DX_MC
 

!                  XLONG
     read(in_unit)field2
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min XLONG(:,1)=',&
                 maxval(field2(:,1)),minval(field2(:,1))
        write(6,*)' convert_binary_2d: max,min XLONG(1,:)=',&
                 maxval(field2(1,:)),minval(field2(1,:))
        write(6,*)' convert_binary_2d: xlong(1,1),xlong(nlon,1)=',&
                 field2(1,1),field2(nlon_regional,1)
        write(6,*)' convert_binary_2d: xlong(1,nlat),xlong(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     end if
     field2=field2/rad2deg_single
     write(lendian_out)field2,field2c   !  XLONG,DY_MC


     read(in_unit)field2             !  psfc0
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min psfc0=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid psfc0=', &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  PHB (zsfc*g)
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min,mid PHB=', &
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2
 

     read(in_unit)field2             !  T  (sensible)
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min,mid T=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  Q
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min,mid Q=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  U
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min,mid U=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  V
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min,mid V=',&
                 maxval(field2),minval(field2), &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  LANDMASK  (1=land, 0=water)
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min landmask=', &
                 maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid landmask=', &
                 field2(nlon_regional/2,nlat_regional/2)
        write(6,*)' convert_binary_2d: landmask(1,1),landmask(nlon,1)=', &
                 field2(1,1),field2(nlon_regional,1)
        write(6,*)' convert_binary_2d: landmask(1,nlat),landmask(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  XICE
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min XICE=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid XICE=', &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  SST
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min SST=',&
                 maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid SST=', &
                 field2(nlon_regional/2,nlat_regional/2)
        write(6,*)' convert_binary_2d: sst(1,1),sst(nlon,1)=',&
                 field2(1,1),field2(nlon_regional,1)
        write(6,*)' convert_binary_2d: sst(1,nlat),sst(nlon,nlat)=', &
                 field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
     end if
     write(lendian_out)field2


     read(in_unit)ifield2            !  IVGTYP
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min IVGTYP=', &
                 maxval(ifield2),minval(ifield2)
        write(6,*)' convert_binary_2d: mid IVGTYP=', &
                 ifield2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)ifield2


     read(in_unit)ifield2            !  ISLTYP
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min ISLTYP=', &
                 maxval(ifield2),minval(ifield2)
        write(6,*)' convert_binary_2d: mid ISLTYP=', &
                 ifield2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)ifield2


     read(in_unit)field2             !  VEGFRA
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min VEGFRA=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid VEGFRA=', &
                 field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  SNOW
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min SNO=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid SNO=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2
 

     read(in_unit)field2             !  SMOIS
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min SMOIS=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid SMOIS=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2


     read(in_unit)field2             !  TSLB
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min TSLB=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid TSLB=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2
 

     read(in_unit)field2             !  TSK
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min TSK=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid TSK=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  SFCR
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min SFCR=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid SFCR=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2
 
     read(in_unit)field2             !  GUST
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min GUST=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid GUST=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

!
     read(in_unit)field2             !  VIS
!...........................................................
!NLTR: apply threshold vis_thres to visibility first guess
!...........................................................

     do j=1,nlon_regional
        do i=1,nlat_regional   
           if (field2(j,i) <= 0.0_r_single) field2(j,i)=one_single
           if (field2(j,i) >= vis_thres) field2(j,i)=vis_thres
         enddo
     enddo
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min VIS=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid VIS=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  PBLH
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min PBLH=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid PBLH=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  CLDCH
!...........................................................
!NLTR: apply threshold cldch_thres to cldch first guess
!...........................................................
     do j=1,nlon_regional
        do i=1,nlat_regional
           if (field2(j,i) <= 0.0_r_single) field2(j,i)=one_single
           if (field2(j,i) > cldch_thres) field2(j,i)=cldch_thres
        enddo
     enddo
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min CLDCH=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid CLDCH=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  WSPD10M
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min WSPD10M=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid WSPD10M=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  TD2M
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min TD2M=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid TD2M=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  MXTM
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min MXTM=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid MXTM=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  MITM
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min MITM=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid MITM=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  PMSL
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min PMSL=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid PMSL=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  HOWV
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min HOWV=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid HOWV=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  TCAMT
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min TCAMT=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid TCAMT=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  LCBAS
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min LCBAS=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid LCBAS=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  UWND10M
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min UWND10M=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid UWND10M=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2

     read(in_unit)field2             !  VWND10M
     if(print_verbose)then
        write(6,*)' convert_binary_2d: max,min VWND10M=',maxval(field2),minval(field2)
        write(6,*)' convert_binary_2d: mid VWND10M=',field2(nlon_regional/2,nlat_regional/2)
     end if
     write(lendian_out)field2
    
     !  variables for assimilating VIIRS Similarity
     if(use_similarity_2dvar) then
       read(in_unit)field2             ! PRESGRID1
       write(6,*)' convert_binary_2d: max,min PRESGRID1=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid PRESGRID1=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             ! PRESGRID2
       write(6,*)' convert_binary_2d: max,min PRESGRID2=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid PRESGRID2=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             ! TMPGRID1
       write(6,*)' convert_binary_2d: max,min TMPGRID1=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid TMPGRID1=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             ! TMPGRID2
       write(6,*)' convert_binary_2d: max,min TMPGRID2=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid TMPGRID2=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             ! QGRID1
       write(6,*)' convert_binary_2d: max,min QGRID1=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid QGRID1=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             ! QGRID2
       write(6,*)' convert_binary_2d: max,min QGRID2=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid QGRID2=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2


       read(in_unit)field2             ! UGRID1
       write(6,*)' convert_binary_2d: max,min UGRID1=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid UGRID1=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             ! VGRID1
       write(6,*)' convert_binary_2d: max,min VGRID1=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid VGRID1=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             !HGTGRID1
       write(6,*)' convert_binary_2d: max,min HGTGRID1=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid HGTGRID1=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2

       read(in_unit)field2             !PRESGSFC
       write(6,*)' convert_binary_2d: max,min PRESGSFC=',maxval(field2),minval(field2)
       write(6,*)' convert_binary_2d: mid PRESGSFC=',field2(nlon_regional/2,nlat_regional/2)
       write(lendian_out)field2
     end if


     close(in_unit)
     close(lendian_out)

     deallocate(field2)
     deallocate(field2b)
     deallocate(field2c)
     deallocate(ifield2)
  enddo n_loop
end subroutine convert_binary_2d

!----------------------------------------------------------------------------------
subroutine read_2d_files(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_2d_files   same as read_files, but for files used in 2dvar
!   Adapted from read_wrf_nmm_files
!   prgmmr: pondeca           org: np20                date: 2004-12-27
!
! abstract: figure out available time levels of background fields for
!             later input.
!
! program history log:
!   2004-12-27  pondeca
!   2006-04-06  middlecoff - remove mpi_request_null since not used
!   2008-04-03  safford    - remove uses mpi_status_size, zero_single (not used)
!   2009-10-09  pondeca - with the adding of 4dvar to the gsi, the time reference for the
!                         obs changed. Adjust guess times accordingly by using time_offset
!                         and thus ensure that fgat works properly
!   2010-04-20  jing    - set hrdifsig_all and hrdifsfc_all for non-ESMF cases.
!   2016-06-07  yang    - add iadatemn
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype,npe
  use guess_grids, only: nfldsig,nfldsfc,ntguessig,ntguessfc,&
       ifilesig,ifilesfc,hrdifsig,hrdifsfc,create_gesfinfo
  use guess_grids, only: hrdifsig_all,hrdifsfc_all
  use gsi_4dvar, only: nhr_assimilation
  use constants, only: zero,one,r60inv
  use obsmod, only: time_offset,iadatemn
  use gsi_io, only: verbose
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  real(r_kind),parameter:: r0_001=0.001_r_kind

! Declare local variables
  logical(4) fexist
  character(6) filename
  integer(i_kind) in_unit
  integer(i_kind) i,j,iwan,npem1
  integer(i_kind) nhr_half
  integer(i_kind) nminanl,nmings,nming2,ndiff
  integer(i_kind),dimension(5):: idate5
  real(r_kind) temp
  real(r_kind),dimension(202):: time_ges
  logical print_verbose

!-----------------------------------------------------------------------------
! Start read_2d_files here.

! Turning on print_verbose will result in additional prints from this routine only.
  print_verbose = .false.
  if(verbose)print_verbose=.true.
  nhr_half=nhr_assimilation/2
  if(nhr_half*2<nhr_assimilation) nhr_half=nhr_half+1
  npem1=npe-1

  if(mype == 0 .and. print_verbose)print*,'in read_2d_files: nhr_assimilation,nhr_half,time_offset=', &
                                         nhr_assimilation,nhr_half,time_offset

  do i=1,202
     time_ges(i) = 999._r_kind
  end do

! Let a single task query the guess files.
  if(mype==npem1) then

!    Convert analysis time to minutes relative to fixed date
!    use iadatemn to get nminal--confirm with Manuel: for hourly RTMA, the
!    minutes value is alway hardcoded as 0, right?
     call w3fs21(iadatemn,nminanl)
     write(6,*)'READ_2d_FILES:  analysis date,minutes ',iadatemn,nminanl

!    Check for consistency of times from sigma guess files.
     in_unit=15
     iwan=0
     do i=0,99
        write(filename,100)i
100     format('sigf',i2.2)
        inquire(file=filename,exist=fexist)
        if(fexist)then
           open(in_unit,file=filename,form='unformatted')
           read(in_unit) idate5
           write(6,*) 'READ_2D_FILES: input file name and idate(1-5)',filename,idate5
           close(in_unit)
           call w3fs21(idate5,nmings)
           nming2=nmings
           ndiff=nming2-nminanl
           write(6,*)'READ_2d_FILES: sigma guess file time in minutes',nming2
           if(abs(ndiff) > 60*nhr_half ) cycle
           iwan=iwan+1
           time_ges(iwan) = (nming2-nminanl)*r60inv + time_offset
           time_ges(iwan+100)=i+r0_001
        end if
     end do
     write(6,*)'READ_2d_FILES:iwan=',iwan,(time_ges(i),i=1,iwan)
     time_ges(201)=one
     time_ges(202)=one
     if(iwan > 1)then
        do i=1,iwan
           do j=i+1,iwan
              if(time_ges(j) < time_ges(i))then
                 temp=time_ges(i+100)
                 time_ges(i+100)=time_ges(j+100)
                 time_ges(j+100)=temp
                 temp=time_ges(i)
                 time_ges(i)=time_ges(j)
                 time_ges(j)=temp
              end if
           end do
           if(abs(time_ges(i)-time_offset) < r0_001)time_ges(202) = i
        end do
     end if
     time_ges(201) = iwan+r0_001
  end if

! Broadcast guess file information to all tasks
  call mpi_bcast(time_ges,202,mpi_rtype,npem1,mpi_comm_world,ierror)

  nfldsig   = nint(time_ges(201))
  nfldsfc   = nfldsig

! Allocate space for guess information files
  call create_gesfinfo

  do i=1,nfldsig
     ifilesig(i) = -100
     hrdifsig(i) = zero
  end do

  do i=1,nfldsfc
     ifilesfc(i) = -100
     hrdifsfc(i) = zero
  end do

! Load time information for sigma guess field sinfo into output arrays
  ntguessig = nint(time_ges(202))
  do i=1,nfldsig
     hrdifsig(i) = time_ges(i)
     ifilesig(i) = nint(time_ges(i+100))
     hrdifsig_all(i) = hrdifsig(i)
  end do
  if(mype == 0) write(6,*)' READ_2d_FILES:  sigma fcst files used in analysis  :  ',&
       (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig


! Think of guess sfcf files as coinciding with guess sigf files
  ntguessfc = ntguessig
  do i=1,nfldsig
     hrdifsfc(i) = hrdifsig(i)
     ifilesfc(i) = ifilesig(i)
     hrdifsfc_all(i) = hrdifsfc(i)
  end do
  if(mype == 0) write(6,*)' READ_2d_FILES:  surface fcst files used in analysis:  ',&
       (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc

!
! End of routine
  return
  end subroutine read_2d_files

!----------------------------------------------------------------------------------
subroutine read_2d_guess(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_2d_guess      read 2d interface file
!
!   Adapted from read_wrf_mass_guess
!   prgmmr: pondeca           org: np20                date: 2005-01-06
!
! abstract:   read guess from a binary file created in a previous step
!             that interfaces with the restart file which may be
!             written in a different format. The a-grid is assumed.
!             The guess is read in by complete horizontal fields, one field
!             per processor, in parallel.
!
! program history log:
!   2005-01-06  pondeca
!   2005-11-29  derber - remove external iteration dependent calculations
!   2006-02-02  treadon - remove unused quanities from use guess_grids
!   2006-04-06  middlecoff - changed nfcst from 11 to 15 so nfcst could be used as little endian
!   2006-07-30  kleist - make change to ges_ps from ln(ps)
!   2006-07-28  derber  - include sensible temperature
!   2008-04-02  safford - rm unused vars and uses
!   2010-12-05  pondeca - change definition of land point from (landmask value)/=0.
!                         to (landmask value)>=0.5
!   2011-02-09  zhu     - add gust,vis and pblh
!   2011-05-01  todling - introduce met-guess (cwmr no longer in guess-grids)
!   2013-10-19  todling - metguess now holds background
!                         remove reference to tv and q derivatives
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-16  carley/zhu - add tcamt and ceiling
!   2015-07-10  pondeca - add cloud ceiling height
!   2016-05-03  pondeca - add uwnd10m, vwnd10m
!   2018-01-xx  yang    - test the method of nonlinear transform
!   2018-06-10  zhang   - add 10 extra model variables for similarity theory application
!   2019-07-26  pondeca - read in surface roughness length from GSI-2DVAR input file
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use mpimod, only: mpi_sum,mpi_integer,mpi_real4,mpi_comm_world,npe,ierror
  use guess_grids, only: fact10,soil_type,veg_frac,veg_type,sfct,sno,soil_temp,soil_moi,&
       isli,nfldsig,ifilesig,ges_tsen,sfc_rough
  use gridmod, only: lon1,lat1,nlat_regional,nlon_regional,&
       nsig,ijn_s,displs_s,itotsub
  use obsmod, only: use_similarity_2dvar
  use constants, only: zero,one,grav,fv,zero_single,one_tenth
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpeu_util, only: die
  use gsi_io, only: verbose
  use qcmod, only: pvis,pcldch,scale_cv
  use nltransf, only: nltransf_forward
  use aux2dvarflds, only: init_aux2dvarflds
  
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  real(r_kind),parameter:: r0_01=0.01_r_kind

! Declare local variables
  integer(i_kind) kt,kq,ku,kv
  real(r_kind) dummy,dummyout

! 2D variable names stuck in here
  integer(i_kind) nfcst

! other internal variables
  character(len=*),parameter::myname='read_2d_guess'
  real(r_single) tempa(itotsub)
  real(r_single),allocatable::temp1(:,:)
  real(r_single),allocatable::all_loc(:,:,:)
  integer(i_kind),allocatable::itemp1(:,:)
  integer(i_kind),allocatable::igtype(:),jsig_skip(:)
  character(60),allocatable::identity(:)
  character(6) filename
  integer(i_kind) irc_s_reg(npe),ird_s_reg(npe)
  integer(i_kind) ifld,im,jm,lm,num_2d_fields
  integer(i_kind) num_all_fields,num_loc_groups,num_all_pad
  integer(i_kind) i,icount,icount_prev,it,j,k
  integer(i_kind) i_0,i_psfc,i_fis,i_t,i_q,i_u,i_v,i_sno,i_smois,i_tslb
  integer(i_kind) i_sm,i_xice,i_sst,i_tsk,i_ivgtyp,i_isltyp,i_vegfrac,i_gust,i_vis,i_pblh
  integer(i_kind) i_wspd10m,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv,i_tcamt,i_lcbas,i_cldch
  integer(i_kind) i_uwnd10m,i_vwnd10m
  integer(i_kind) i_sfcr
  !Similarity
  integer(i_kind) i_presgrid1,i_presgrid2,i_tmpgrid1,i_tmpgrid2,i_qgrid1,i_qgrid2
  integer(i_kind) i_ugrid1,i_vgrid1,i_hgtgrid1,i_presgsfc

  integer(i_kind) isli_this
  real(r_kind) psfc_this,sm_this,xice_this
  integer(i_kind) icwmr,ier,istatus
  logical ihave_gust,ihave_pblh,ihave_vis,ihave_tcamt,ihave_lcbas
  logical ihave_wspd10m,ihave_td2m,ihave_mxtm,ihave_mitm,ihave_pmsl,ihave_howv
  logical ihave_cldch,ihave_uwnd10m,ihave_vwnd10m
  !Similarity
  logical ihave_presgrid1,ihave_presgrid2,ihave_tmpgrid1,ihave_tmpgrid2,ihave_qgrid1,ihave_qgrid2
  logical ihave_ugrid1,ihave_vgrid1,ihave_hgtgrid1,ihave_presgsfc

  real(r_kind),pointer,dimension(:,:  )::ges_gust   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_vis    =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_pblh   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_wspd10m=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_td2m   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_mxtm   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_mitm   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_pmsl   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_howv   =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_tcamt  =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_lcbas  =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_cldch  =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_uwnd10m=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_vwnd10m=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_ps_it  =>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_z_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_u_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_v_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_div_it =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_vor_it =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_tv_it  =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_q_it   =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ges_cwmr_it=>NULL()

  !Similarity
  real(r_kind),pointer,dimension(:,:  )::ges_presgrid1=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_presgrid2=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_tmpgrid1=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_tmpgrid2=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_qgrid1=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_qgrid2=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_ugrid1=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_vgrid1=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_hgtgrid1=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ges_presgsfc=>NULL()
  logical print_verbose

!  RESTART FILE input grid dimensions in module gridmod
!      These are the following:
!          im -- number of x-points on A-grid
!          jm -- number of y-points on A-grid
!          lm -- number of vertical levels ( = nsig for now)


! Turning on print_verbose will result in additional prints from this routine only.
  print_verbose = .false.
  if(verbose)print_verbose=.true.
  if(print_verbose .and. mype == 0)then
     write(6,*)' at 0 in read_2d_guess'
  end if


! Big section of operations done only on first outer iteration


  im=nlon_regional
  jm=nlat_regional
  lm=nsig

! Following is for convenient 2D input
  if (use_similarity_2dvar) then !Similarity
     num_2d_fields=41
  else
     num_2d_fields=31! Adjust according to content of RTMA restart file ---- should this number be in a namelist or anavinfo file at some point?
  end if
  num_all_fields=num_2d_fields*nfldsig
  num_loc_groups=num_all_fields/npe
  if(print_verbose .and. mype == 0)then
     write(6,'(" at 1 in read_2d_guess, lm            =",i6)')lm
     write(6,'(" at 1 in read_2d_guess, num_2d_fields=",i6)')num_2d_fields
     write(6,'(" at 1 in read_2d_guess, nfldsig       =",i6)')nfldsig
     write(6,'(" at 1 in read_2d_guess, num_all_fields=",i6)')num_all_fields
     write(6,'(" at 1 in read_2d_guess, npe           =",i6)')npe
     write(6,'(" at 1 in read_2d_guess, num_loc_groups=",i6)')num_loc_groups
  end if
  do
     num_all_pad=num_loc_groups*npe
     if(num_all_pad >= num_all_fields) exit
     num_loc_groups=num_loc_groups+1
  end do
  if(print_verbose .and. mype == 0)then
    write(6,'(" at 1 in read_2d_guess, num_all_pad   =",i6)')num_all_pad
    write(6,'(" at 1 in read_2d_guess, num_loc_groups=",i6)')num_loc_groups
  end if

  allocate(all_loc(lat1+2,lon1+2,num_all_pad))
  allocate(jsig_skip(num_2d_fields))
  allocate(igtype(num_2d_fields))
  allocate(identity(num_2d_fields))

! igtype is a flag indicating whether each input field is integer or real
! igtype > 0 for real field
! igtype < 0 for integer field

  i=0
  i=i+1 ; i_psfc=i                                            ! psfc
  write(identity(i),'("record ",i3,"--psfc")')i
  jsig_skip(i)=3     ! number of records to skip before getting to psfc
  igtype(i)=1

  i=i+1 ; i_fis=i                                             ! sfc geopotential
  write(identity(i),'("record ",i3,"--fis")')i
  jsig_skip(i)=0
  igtype(i)=1

  i_t=i+1
  do k=1,lm
     i=i+1                                                    ! t(k)  (sensible temp)
     write(identity(i),'("record ",i3,"--t(",i2,")")')i,k
     jsig_skip(i)=0
     igtype(i)=1
  end do

  i_q=i+1
  do k=1,lm
     i=i+1                                                    ! q(k)
     write(identity(i),'("record ",i3,"--q(",i2,")")')i,k
     jsig_skip(i)=0 ; igtype(i)=1
  end do

  i_u=i+1
  do k=1,lm
     i=i+1                                                    ! u(k)
     write(identity(i),'("record ",i3,"--u(",i2,")")')i,k
     jsig_skip(i)=0 ; igtype(i)=2
  end do

  i_v=i+1
  do k=1,lm
     i=i+1                                                    ! v(k)
     write(identity(i),'("record ",i3,"--v(",i2,")")')i,k
     jsig_skip(i)=0 ; igtype(i)=3
  end do

  i=i+1   ; i_sm=i                                            ! landmask
  write(identity(i),'("record ",i3,"--sm")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_xice=i                                            ! xice
  write(identity(i),'("record ",i3,"--xice")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_sst=i                                             ! sst
  write(identity(i),'("record ",i3,"--sst")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_ivgtyp=i                                          ! ivgtyp
  write(identity(i),'("record ",i3,"--ivgtyp")')i
  jsig_skip(i)=0 ; igtype(i)=-1

  i=i+1 ; i_isltyp=i                                          ! isltyp
  write(identity(i),'("record ",i3,"--isltyp")')i
  jsig_skip(i)=0 ; igtype(i)=-1

  i=i+1 ; i_vegfrac=i                                         ! vegfrac
  write(identity(i),'("record ",i3,"--vegfrac")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_sno=i                                             ! sno
  write(identity(i),'("record ",i3,"--sno")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_smois=i                                           ! smois
  write(identity(i),'("record ",i3,"--smois(",i2,")")')i,k
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_tslb=i                                            ! tslb
  write(identity(i),'("record ",i3,"--tslb(",i2,")")')i,k
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_tsk=i                                             ! tsk
  write(identity(i),'("record ",i3,"--sst")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_sfcr=i                                            ! sfcr
  write(identity(i),'("record ",i3,"--sfcr")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_gust=i                                            ! gust
  write(identity(i),'("record ",i3,"--gust")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_vis=i                                             ! vis
  write(identity(i),'("record ",i3,"--vis")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_pblh=i                                            ! pblh
  write(identity(i),'("record ",i3,"--pblh")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_cldch=i                                           ! cldch
  write(identity(i),'("record ",i3,"--cldch")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_wspd10m=i                                         ! wspd10m
  write(identity(i),'("record ",i3,"--wspd10m")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_td2m=i                                            ! td2m
  write(identity(i),'("record ",i3,"--td2m")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_mxtm=i                                            ! mxtm
  write(identity(i),'("record ",i3,"--mxtm")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_mitm=i                                            ! mitm
  write(identity(i),'("record ",i3,"--mitm")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_pmsl=i                                            ! pmsl
  write(identity(i),'("record ",i3,"--pmsl")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_howv=i                                            ! howv
  write(identity(i),'("record ",i3,"--howv")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_tcamt=i                                           ! tcamt
  write(identity(i),'("record ",i3,"--tcamt")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_lcbas=i                                           ! lcbas
  write(identity(i),'("record ",i3,"--lcbas")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_uwnd10m=i                                         ! uwnd10m
  write(identity(i),'("record ",i3,"--uwnd10m")')i
  jsig_skip(i)=0 ; igtype(i)=1

  i=i+1 ; i_vwnd10m=i                                         ! vwnd10m
  write(identity(i),'("record ",i3,"--vwnd10m")')i
  jsig_skip(i)=0 ; igtype(i)=1

  !x Similarity
  if (use_similarity_2dvar) then
     i=i+1 ; i_presgrid1=i                                       ! presgrid1
     write(identity(i),'("record ",i3,"--presgrid1")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_presgrid2=i                                       ! presgrid2
     write(identity(i),'("record ",i3,"--presgrid2")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_tmpgrid1=i                                        ! tmpgrid1
     write(identity(i),'("record ",i3,"--tmpgrid1")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_tmpgrid2=i                                        ! tmpgrid2
     write(identity(i),'("record ",i3,"--tmpgrid2")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_qgrid1=i                                          ! qgrid1
     write(identity(i),'("record ",i3,"--qgrid1")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_qgrid2=i                                          ! qgrid2
     write(identity(i),'("record ",i3,"--qgrid2")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_ugrid1=i                                          ! ugrid1
     write(identity(i),'("record ",i3,"--ugrid1")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_vgrid1=i                                          ! vgrid1
     write(identity(i),'("record ",i3,"--vgrid1")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_hgtgrid1=i                                        ! hgtgrid1
     write(identity(i),'("record ",i3,"--hgtgrid1")')i
     jsig_skip(i)=0 ; igtype(i)=1

     i=i+1 ; i_presgsfc=i                                        ! presgsfc
     write(identity(i),'("record ",i3,"--presgsfc")')i
     jsig_skip(i)=0 ; igtype(i)=1
  end if

  if (print_verbose .and. mype==0) then
     print*
     do i=1,num_2d_fields
        print'(a)',trim(identity(i))
     enddo
     print*
  endif

! End of stuff from 2D restart file

  allocate(temp1(im,jm),itemp1(im,jm))

  do i=1,npe
     irc_s_reg(i)=ijn_s(mype+1)
  end do
  ird_s_reg(1)=0
  do i=1,npe
     if(i /= 1) ird_s_reg(i)=ird_s_reg(i-1)+irc_s_reg(i-1)
  end do

! Read fixed format input file created from external interface
! This is done by reading in parallel from every pe, and redistributing
! to local domains once for every npe fields read in, using
! mpi_all_to_allv

  nfcst=15
  icount=0
  icount_prev=1
  do it=1,nfldsig
     write(filename,'("sigf",i2.2)')ifilesig(it)
     open(nfcst,file=filename,form='unformatted') ; rewind nfcst
     if(mype == 0)write(6,*)'READ_2d_GUESS:  open nfcst=',nfcst,' to file=',filename

!    Read, interpolate, and distribute 2D restart fields
     do ifld=1,num_2d_fields
        icount=icount+1
        if(jsig_skip(ifld) > 0) then
           do i=1,jsig_skip(ifld)
              read(nfcst)
           end do
        end if
        if(mype==mod(icount-1,npe)) then
           if(igtype(ifld) > 0) then
              read(nfcst)((temp1(i,j),i=1,im),j=1,jm)
              if(print_verbose .and. mype == 0)write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
              call fill_mass_grid2t(temp1,im,jm,tempa,1)
           end if
           if(igtype(ifld) < 0) then
              read(nfcst)((itemp1(i,j),i=1,im),j=1,jm)
              do j=1,jm
                 do i=1,im
                    temp1(i,j)=itemp1(i,j)
                 end do
              end do
              if(print_verbose .and. mype == 0)write(6,'(" ifld, temp1(im/2,jm/2)=",i6,e15.5)')ifld,temp1(im/2,jm/2)
              call fill_mass_grid2t(temp1,im,jm,tempa,1)
           end if
        else
           read(nfcst)
        end if

!       Distribute to local domains everytime we have npe fields
        if(mod(icount,npe) == 0.or.icount==num_all_fields) then
           call mpi_alltoallv(tempa,ijn_s,displs_s,mpi_real4, &
                all_loc(1,1,icount_prev),irc_s_reg,ird_s_reg,mpi_real4,mpi_comm_world,ierror)
           icount_prev=icount+1
        end if
     end do
     close(nfcst)
  end do

! Next do conversion of units as necessary and
! reorganize into WeiYu's format--

  do it=1,nfldsig
     ier=0
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it,   istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it,   istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it,   istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it,  istatus)
     ier=ier+istatus
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q', ges_q_it,   istatus)
     ier=ier+istatus
     if(ier/=0) call die(myname,'missing fields, ier= ', ier)

     i_0=(it-1)*num_2d_fields
     kt=i_0+i_t-1
     kq=i_0+i_q-1
     ku=i_0+i_u-1
     kv=i_0+i_v-1

     do k=1,nsig
        kt=kt+1
        kq=kq+1
        ku=ku+1
        kv=kv+1
        do i=1,lon1+2
           do j=1,lat1+2
              ges_u_it(j,i,k) = real(all_loc(j,i,ku),r_kind)
              ges_v_it(j,i,k) = real(all_loc(j,i,kv),r_kind)
              ges_vor_it(j,i,k) = zero
              ges_q_it(j,i,k)   = real(all_loc(j,i,kq),r_kind)
              ges_tsen(j,i,k,it)  = real(all_loc(j,i,kt),r_kind)
           end do
        end do
     end do
     do i=1,lon1+2
        do j=1,lat1+2
           ges_z_it(j,i)    = real(all_loc(j,i,i_0+i_fis),r_kind)/grav ! surface elevation multiplied by g

!          convert input psfc to psfc in mb, and then to cb

           psfc_this=r0_01*real(all_loc(j,i,i_0+i_psfc),r_kind)
           ges_ps_it(j,i)=one_tenth*psfc_this   ! convert from mb to cb
           sno(j,i,it)=real(all_loc(j,i,i_0+i_sno),r_kind)
           soil_moi(j,i,it)=real(all_loc(j,i,i_0+i_smois),r_kind)
           soil_temp(j,i,it)=real(all_loc(j,i,i_0+i_tslb),r_kind)
        end do
     end do

!    Convert sensible temp to virtual temp
     do k=1,nsig
        do i=1,lon1+2
           do j=1,lat1+2
              ges_tv_it(j,i,k) = ges_tsen(j,i,k,it) * (one+fv*ges_q_it(j,i,k))
           end do
        end do
     end do

     ges_div_it=zero
  end do ! <it>


!    Transfer surface fields
     do it=1,nfldsig

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,icwmr)
        if(icwmr==0) ges_cwmr_it=zero

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'gust',ges_gust,ier)
        ihave_gust=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vis',ges_vis,ier)
        ihave_vis =ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'pblh',ges_pblh,ier)
        ihave_pblh=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cldch',ges_cldch,ier)
        ihave_cldch=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'wspd10m',ges_wspd10m,ier)
        ihave_wspd10m=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'td2m',ges_td2m,ier)
        ihave_td2m=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'mxtm',ges_mxtm,ier)
        ihave_mxtm=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'mitm',ges_mitm,ier)
        ihave_mitm=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'pmsl',ges_pmsl,ier)
        ihave_pmsl=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'howv',ges_howv,ier)
        ihave_howv=ier==0

        call gsi_bundlegetpointer(gsi_metguess_bundle(it),'tcamt',ges_tcamt,ier)
        ihave_tcamt =ier==0

        call gsi_bundlegetpointer(gsi_metguess_bundle(it),'lcbas',ges_lcbas,ier)
        ihave_lcbas =ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'uwnd10m',ges_uwnd10m,ier)
        ihave_uwnd10m=ier==0

        call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vwnd10m',ges_vwnd10m,ier)
        ihave_vwnd10m=ier==0
     
         !x Similarity
        if (use_similarity_2dvar) then
           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'presgrid1',ges_presgrid1,ier)
           ihave_presgrid1=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'presgrid2',ges_presgrid2,ier)
           ihave_presgrid2=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tmpgrid1',ges_tmpgrid1,ier)
           ihave_tmpgrid1=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tmpgrid2',ges_tmpgrid2,ier)
           ihave_tmpgrid2=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qgrid1',ges_qgrid1,ier)
           ihave_qgrid1=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qgrid2',ges_qgrid2,ier)
           ihave_qgrid2=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ugrid1',ges_ugrid1,ier)
           ihave_ugrid1=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vgrid1',ges_vgrid1,ier)
           ihave_vgrid1=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'hgtgrid1',ges_hgtgrid1,ier)
           ihave_hgtgrid1=ier==0

           call gsi_bundlegetpointer (gsi_metguess_bundle(it),'presgsfc',ges_presgsfc,ier)
           ihave_presgsfc=ier==0
        end if

        i_0=(it-1)*num_2d_fields
        do i=1,lon1+2
           do j=1,lat1+2
              fact10(j,i,it)=one    !  later fix this by using correct w10/w(1)
              veg_type(j,i,it)=real(all_loc(j,i,i_0+i_ivgtyp),r_kind)
              veg_frac(j,i,it)=r0_01*real(all_loc(j,i,i_0+i_vegfrac),r_kind)
              soil_type(j,i,it)=real(all_loc(j,i,i_0+i_isltyp),r_kind)
              sm_this=zero
              if(all_loc(j,i,i_0+i_sm) >= 0.5_r_single) sm_this=one
              xice_this=zero
              if(all_loc(j,i,i_0+i_xice) /= zero_single) xice_this=one

              isli_this=0
              if(xice_this==one) isli_this=2
              if(xice_this==zero.and.sm_this==one) isli_this=1
              isli(j,i,it)=isli_this

              sfct(j,i,it)=real(all_loc(j,i,i_0+i_sst),r_kind)
              if(isli(j,i,it) /= 0) sfct(j,i,it)=real(all_loc(j,i,i_0+i_tsk),r_kind)

              sfc_rough(j,i,it)=real(all_loc(j,i,i_0+i_sfcr),r_kind)

              if(ihave_gust) &
                 ges_gust(j,i)=real(all_loc(j,i,i_0+i_gust),r_kind)

              if (ihave_vis) then
!..............................................................
!NOTE:  input data come from sigf06, already using vis_thres, 
!       min=1.0 m and max=vis_thres
!..............................................................
                 dummy=real(all_loc(j,i,i_0+i_vis),r_kind)
                 call nltransf_forward(dummy,dummyout,pvis,scale_cv)
                 ges_vis(j,i)=dummyout
              endif

              if(ihave_pblh) &
                 ges_pblh(j,i)=real(all_loc(j,i,i_0+i_pblh),r_kind)

              if(ihave_cldch) then 
!..............................................................
!NOTE:  input data come from sigf06, already using cldch_thres, 
!       min=1.0 m and max=cldch_thres
!..............................................................
                 dummy=real(all_loc(j,i,i_0+i_cldch),r_kind)
                 call nltransf_forward(dummy,dummyout,pcldch,scale_cv)
                 ges_cldch(j,i)=dummyout
              endif

              if (ihave_wspd10m) & 
                 ges_wspd10m(j,i)=real(all_loc(j,i,i_0+i_wspd10m),r_kind)

              if(ihave_td2m) &
                 ges_td2m(j,i)=real(all_loc(j,i,i_0+i_td2m),r_kind)

              if(ihave_mxtm) &
                 ges_mxtm(j,i)=real(all_loc(j,i,i_0+i_mxtm),r_kind)

              if(ihave_mitm) &
                 ges_mitm(j,i)=real(all_loc(j,i,i_0+i_mitm),r_kind)

              if(ihave_pmsl) &
                 ges_pmsl(j,i)=one_tenth*r0_01*real(all_loc(j,i,i_0+i_pmsl),r_kind)    !  convert from Pa to cb

              if(ihave_howv) &
                 ges_howv(j,i)=real(all_loc(j,i,i_0+i_howv),r_kind)

              if (ihave_tcamt) &
                 ges_tcamt(j,i)=real(all_loc(j,i,i_0+i_tcamt),r_kind)

              if (ihave_lcbas) & 
                 ges_lcbas(j,i)=max(min(real(all_loc(j,i,i_0+i_lcbas),r_kind),20000.0_r_kind),one_tenth) !Enforce upper and lower bounds on lowest cloud base

              if (ihave_uwnd10m) & 
                 ges_uwnd10m(j,i)=real(all_loc(j,i,i_0+i_uwnd10m),r_kind)

              if (ihave_vwnd10m) & 
                 ges_vwnd10m(j,i)=real(all_loc(j,i,i_0+i_vwnd10m),r_kind)

               !x Similarity
              if (use_similarity_2dvar) then
                 if (ihave_presgrid1) &
                    ges_presgrid1(j,i)=real(all_loc(j,i,i_0+i_presgrid1),r_kind)

                 if (ihave_presgrid2) &
                    ges_presgrid2(j,i)=real(all_loc(j,i,i_0+i_presgrid2),r_kind)

                 if (ihave_tmpgrid1) &
                    ges_tmpgrid1(j,i)=real(all_loc(j,i,i_0+i_tmpgrid1),r_kind)

                 if (ihave_tmpgrid2) &
                    ges_tmpgrid2(j,i)=real(all_loc(j,i,i_0+i_tmpgrid2),r_kind)

                 if (ihave_qgrid1) &
                    ges_qgrid1(j,i)=real(all_loc(j,i,i_0+i_qgrid1),r_kind)

                 if (ihave_qgrid2) &
                    ges_qgrid2(j,i)=real(all_loc(j,i,i_0+i_qgrid2),r_kind)

                 if (ihave_ugrid1) &
                    ges_ugrid1(j,i)=real(all_loc(j,i,i_0+i_ugrid1),r_kind)

                 if (ihave_vgrid1) &
                    ges_vgrid1(j,i)=real(all_loc(j,i,i_0+i_vgrid1),r_kind)

                 if (ihave_hgtgrid1) &
                    ges_hgtgrid1(j,i)=real(all_loc(j,i,i_0+i_hgtgrid1),r_kind)

                 if (ihave_presgsfc) &
                    ges_presgsfc(j,i)=real(all_loc(j,i,i_0+i_presgsfc),r_kind)
              end if

           end do
        end do
     end do

     call mpi_barrier(mpi_comm_world,ierror)
     if(use_similarity_2dvar) then
       call init_aux2dvarflds(mype)
     endif

     deallocate(all_loc,jsig_skip,igtype,identity,temp1)
     return
end subroutine read_2d_guess

!----------------------------------------------------------------------------------
subroutine wr2d_binary(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wr2d_binary              write out 2D restart file
! Adpated from wrwrfmassa.
!   prgmmr: pondeca           org: np20                date: 2005-2-7
!
!   abstract: read 2D guess restart interface file, add analysis
!             increment, and write out 2D analysis restart
!             interface file.
!
! program history log:
!   2005-02-07  pondeca
!   2006-04-06  middlecoff - Changed iog from 11 to 15 so iog could be little endian
!                          Changed ioan from 51 to 66 so ioan could be little endian
!   2006-07-28 derber - include sensible temperature
!   2006-07-31  kleist - make change to ges_ps instead of ln(ps)
!   2008-04-03  safford - rm unused vars and uses
!   2010-04-01  treadon - move strip_single to gridmod
!   2011-02-09  zhu     - add gust,vis,pblh
!   2013-10-19  todling - metguess now holds background
!                         correct reference to svars rather than cvars
!   2013-10-24  todling - general interface to strip
!   2014-03-19  pondeca - add wspd10m
!   2014-04-10  pondeca - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-16  carley/zhu - add tcamt and ceiling
!   2014-06-27  carley - slight change to specification of num_2d_fields
!   2015-07-10  pondeca - add cloud ceiling height
!   2016-05-03  pondeca - add uwnd10m, vwnd10m
!
!   input argument list:
!     mype     - pe number
!
!   output argument list:
!     no output arguments
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind
  use constants, only: one_tenth,one

  use guess_grids, only: ntguessig,ifilesig,&
       ges_tsen
  use mpimod, only: mpi_comm_world,ierror,mpi_real4
  use gridmod, only: lat2,iglobal,itotsub,strip,&
       lon2,nsig,lon1,lat1,nlon_regional,nlat_regional,ijn,displs_g
  use obsmod, only: use_similarity_2dvar
  use mpeu_util, only: getindex
  use constants, only: zero_single,r10,r100
  use derivsmod, only: qsatg
  use jfunc, only: jiter,miter
  use gsi_metguess_mod, only: gsi_metguess_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use qcmod, only: pvis,pcldch,scale_cv,vis_thres,cldch_thres
  use nltransf, only: nltransf_inverse 
  use mpeu_util, only: die
  use gsi_io, only: verbose
  use aux2dvarflds, only: destroy_aux2dvarflds
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  real(r_kind),parameter:: r225=225.0_r_kind

! Declare local variables

  character(len=*),parameter::myname='wr2d_binary'
  integer(i_kind) im,jm,lm
  integer(i_kind),allocatable::itemp1(:)
  real(r_single),allocatable::temp1(:),tempa(:),tempb(:)
  real(r_single),allocatable::all_loc(:,:,:)
  real(r_single),allocatable::strp(:)
  character(6) filename
  character(2) ch2
  integer(i_kind) iog,ioan,i,j,k,kt,kq,ku,kv,it,i_psfc,i_t,i_q,i_u,i_v
  integer(i_kind) i_sst,i_skt,i_gust,i_vis,i_pblh,ier,istatus,i_tcamt,i_lcbas,i_cldch
  integer(i_kind) i_wspd10m,i_td2m,i_mxtm,i_mitm,i_pmsl,i_howv,i_uwnd10m,i_vwnd10m
  !x Similarity
  integer(i_kind) i_presgrid1,i_presgrid2,i_tmpgrid1,i_tmpgrid2,i_qgrid1,i_qgrid2,&
                  i_ugrid1,i_vgrid1,i_hgtgrid1,i_presgsfc

  integer(i_kind) num_2d_fields,num_all_fields,num_all_pad
  integer(i_kind) regional_time0(6),nlon_regional0,nlat_regional0,nsig0
  real(r_kind) psfc_this
  real(r_single) glon0(nlon_regional,nlat_regional),glat0(nlon_regional,nlat_regional)
  real(r_single) dx_mc0(nlon_regional,nlat_regional),dy_mc0(nlon_regional,nlat_regional)
  real(r_single),allocatable::all_loc_qsatg(:,:,:),all_loc_prh(:,:,:),temp1_prh(:)
  real(r_kind) tempvis,visout,tempcldch,cldchout
  
  integer(i_kind) iaux(100),kaux
  character(15) caux(100)
  logical print_verbose
  
  real(r_kind),dimension(:,:  ),pointer:: ptr2d    =>NULL()
  real(r_kind),dimension(:,:  ),pointer:: ges_ps_it=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_u_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_v_it =>NULL()
  real(r_kind),dimension(:,:,:),pointer:: ges_q_it =>NULL()

  print_verbose = .false.
  if(verbose)print_verbose=.true.
  im=nlon_regional
  jm=nlat_regional
  lm=nsig

  i_psfc=1
  i_t=2
  i_q=i_t+lm
  i_u=i_q+lm
  i_v=i_u+lm
  i_sst=i_v+lm
  i_skt=i_sst+1
  i_gust=i_skt+1
  i_vis=i_gust+1
  i_pblh=i_vis+1
  i_cldch=i_pblh+1
  i_wspd10m=i_cldch+1
  i_td2m=i_wspd10m+1
  i_mxtm= i_td2m+1
  i_mitm= i_mxtm+1
  i_pmsl= i_mitm+1
  i_howv= i_pmsl+1
  i_tcamt=i_howv+1
  i_lcbas=i_tcamt+1
  i_uwnd10m=i_lcbas+1
  i_vwnd10m=i_uwnd10m+1
  
  !x Similarity
  if (use_similarity_2dvar) then
     i_presgrid1=i_vwnd10m+1
     i_presgrid2=i_presgrid1+1
     i_tmpgrid1=i_presgrid2+1
     i_tmpgrid2=i_tmpgrid1+1
     i_qgrid1=i_tmpgrid2+1
     i_qgrid2=i_qgrid1+1
     i_ugrid1=i_qgrid2+1
     i_vgrid1=i_ugrid1+1
     i_hgtgrid1=i_vgrid1+1
     i_presgsfc=i_hgtgrid1+1
     num_2d_fields=i_presgsfc     ! - should always equal the last integer from the
                                ! -   preceding list
  else
     num_2d_fields=i_vwnd10m      ! - should always equal the last integer from the
                               ! -   preceding list
  end if

  num_all_fields=num_2d_fields
  num_all_pad=num_all_fields

  allocate(all_loc(lat1+2,lon1+2,num_all_pad))
  allocate(strp(lat1*lon1))
  allocate(all_loc_qsatg(lat1+2,lon1+2,nsig),all_loc_prh(lat1+2,lon1+2,nsig))



  allocate(itemp1(im*jm))
  allocate(temp1(im*jm))
  allocate(temp1_prh(im*jm))

! if(mype == 0) write(6,*)' at 2 in wr2d_binary'

  iog=15
  ioan=66
  if(mype == 0) then
     write(filename,'("sigf",i2.2)')ifilesig(ntguessig)
     open (iog,file=filename,form='unformatted')
     write(ch2,'(i2.2)') jiter+1
     if (jiter <  miter) open (ioan,file='sigfupdate'//ch2,form='unformatted')
     if (jiter == miter) open (ioan,file='siganl',form='unformatted')
     rewind iog ; rewind ioan
  end if

! Convert analysis variables to 2D variables
  it=ntguessig

  ier=0
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it,  istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it,   istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q', ges_q_it,   istatus)
  ier=ier+istatus
  if(ier/=0) call die(myname,'missing fields, ier= ', ier)

! Create all_loc from ges_*
  all_loc=zero_single
  kt=i_t-1
  kq=i_q-1
  ku=i_u-1
  kv=i_v-1
  do k=1,nsig
     kt=kt+1
     kq=kq+1
     ku=ku+1
     kv=kv+1
     do i=1,lon2
        do j=1,lat2
           all_loc(j,i,ku)=ges_u_it(j,i,k)
           all_loc(j,i,kv)=ges_v_it(j,i,k)
           all_loc(j,i,kq)=ges_q_it(j,i,k)
           all_loc(j,i,kt)=ges_tsen(j,i,k,it)   ! sensible temperature
           all_loc_qsatg(j,i,k)=qsatg(j,i,k)
           all_loc_prh(j,i,k)=ges_q_it(j,i,k)/qsatg(j,i,k)
        end do
     end do
  end do
  do i=1,lon2
     do j=1,lat2
        psfc_this=r10*ges_ps_it(j,i)   ! convert from cb to mb
        all_loc(j,i,i_psfc)=r100*psfc_this
     end do
  end do

  if(mype == 0) then
     read(iog) regional_time0,nlon_regional0,nlat_regional0,nsig0
     write(ioan) regional_time0,nlon_regional0,nlat_regional0,nsig0
     read(iog) glat0,dx_mc0
     write(ioan) glat0,dx_mc0
     read(iog) glon0,dy_mc0
     write(ioan) glon0,dy_mc0
  end if

! Update psfc
  if( print_verbose .and. mype == 0) write(6,*)' at 6 in wr2d_binary'

  allocate(tempa(itotsub),tempb(itotsub))
  if(mype == 0) then
     read(iog)temp1
  endif
  call strip(all_loc(:,:,i_psfc),strp)
  call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
       tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
  if(mype == 0) then
     call fill_mass_grid2t(temp1,im,jm,tempb,2)
     do i=1,iglobal
        tempa(i)=tempa(i)-tempb(i)
     end do
     call unfill_mass_grid2t(tempa,im,jm,temp1)
     write(ioan)temp1
  end if

!  FIS read/write
  if(mype == 0) then
     read(iog)temp1
     write(ioan)temp1
  end if

! Update t
  kt=i_t-1
  do k=1,nsig
     kt=kt+1
     if(mype == 0) read(iog)temp1
     call strip(all_loc(:,:,kt),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  end do

! Update q
  kq=i_q-1
  do k=1,nsig
     kq=kq+1
     if(mype == 0) then
        read(iog)temp1
        temp1_prh=temp1
     endif
     call strip(all_loc(:,:,kq),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if

     call strip(all_loc_qsatg(:,:,k),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1_prh,im,jm,tempb,2)
        do i=1,iglobal
           tempb(i)=tempb(i)/tempa(i)
         end do
     end if
     call strip(all_loc_prh(:,:,k),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        temp1_prh=zero_single
        call unfill_mass_grid2t(tempa,im,jm,temp1_prh)
     end if
  end do

! Update u
  ku=i_u-1
  do k=1,nsig
     ku=ku+1
     if(mype == 0) read(iog)temp1
     call strip(all_loc(:,:,ku),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  end do

! Update v
  kv=i_v-1
  do k=1,nsig
     kv=kv+1
     if(mype == 0) read(iog)temp1
     call strip(all_loc(:,:,kv),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        call fill_mass_grid2t(temp1,im,jm,tempb,2)
        do i=1,iglobal
           tempa(i)=tempa(i)-tempb(i)
        end do
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        write(ioan)temp1
     end if
  end do

  if (mype==0) then
     write(ioan)temp1_prh  !increment of pseudo RH
  endif

  if (mype == 0) then
     do k=7,17 ! SM,SICE,SST,IVGTYP,ISLTYP,VEGFRA,SNOW,SMOIS,TSLB,TSK,SFCR
        if (k==10 .or. k==11) then
           read(iog)itemp1
           write(ioan)itemp1
         else
           read(iog)temp1
           write(ioan)temp1
        endif
     end do
  end if

  caux(1)='gust'    ; iaux(1)=i_gust
  caux(2)='vis'     ; iaux(2)=i_vis
  caux(3)='pblh'    ; iaux(3)=i_pblh
  caux(4)='cldch'   ; iaux(4)=i_cldch
  caux(5)='wspd10m' ; iaux(5)=i_wspd10m
  caux(6)='td2m'    ; iaux(6)=i_td2m
  caux(7)='mxtm'    ; iaux(7)=i_mxtm
  caux(8)='mitm'    ; iaux(8)=i_mitm
  caux(9)='pmsl'    ; iaux(9)=i_pmsl
  caux(10)='howv'   ; iaux(10)=i_howv
  caux(11)='tcamt'  ; iaux(11)=i_tcamt
  caux(12)='lcbas'  ; iaux(12)=i_lcbas
  caux(13)='uwnd10m'; iaux(13)=i_uwnd10m
  caux(14)='vwnd10m'; iaux(14)=i_vwnd10m

  if (use_similarity_2dvar) then
     caux(15)='presgrid1'  ; iaux(15)=i_presgrid1
     caux(16)='presgrid2'  ; iaux(16)=i_presgrid2
     caux(17)='tmpgrid1'   ; iaux(17)=i_tmpgrid1
     caux(18)='tmpgrid2'   ; iaux(18)=i_tmpgrid2
     caux(19)='qgrid1'     ; iaux(19)=i_qgrid1
     caux(20)='qgrid2'     ; iaux(20)=i_qgrid2
     caux(21)='ugrid1'     ; iaux(21)=i_ugrid1
     caux(22)='vgrid1'     ; iaux(22)=i_vgrid1
     caux(23)='hgtgrid1'   ; iaux(23)=i_hgtgrid1
     caux(24)='presgsfc'   ; iaux(24)=i_presgsfc
     kaux=24
  else
     kaux=14  !Adjust as you add variables
  end if

  do k=1,kaux
     call gsi_bundlegetpointer (gsi_metguess_bundle(it),trim(caux(k)),ptr2d, ier)
     if (ier==0) then
        do i=1,lon2
           do j=1,lat2
              if (trim(caux(k))=='pmsl') then 
                 all_loc(j,i,iaux(k))=r100*r10*ptr2d(j,i)
              elseif(trim(caux(k))=='vis') then
                 tempvis=ptr2d(j,i)
                 call nltransf_inverse(tempvis,visout,pvis,scale_cv) 
                 all_loc(j,i,iaux(k))=max(min(visout,vis_thres),one)
              elseif(trim(caux(k))=='cldch') then
                 tempcldch=ptr2d(j,i)
                 call nltransf_inverse(tempcldch,cldchout,pcldch,scale_cv)
                 all_loc(j,i,iaux(k))=max(min(cldchout,cldch_thres),one)
              else
                 all_loc(j,i,iaux(k))=ptr2d(j,i)
              endif
           end do
        end do
        if(mype==0) read(iog)temp1
        call strip(all_loc(:,:,iaux(k)),strp)
        call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
             tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
        if(mype == 0) then
           call fill_mass_grid2t(temp1,im,jm,tempb,2)
           do i=1,iglobal
              tempa(i)=tempa(i)-tempb(i)
           end do
           call unfill_mass_grid2t(tempa,im,jm,temp1)
           write(ioan)temp1
        end if
     else
        if(mype==0) then
           read(iog)temp1
           write(ioan)temp1
        end if
     endif
  enddo
  if (mype==0) then
     close(iog)
     close(ioan)
  endif

! Write out qsatg for gsi-2dvar post-processing purposes
  do k=1,nsig
     call strip(all_loc_qsatg(:,:,k),strp)
     call mpi_gatherv(strp,ijn(mype+1),mpi_real4, &
          tempa,ijn,displs_g,mpi_real4,0,mpi_comm_world,ierror)
     if(mype == 0) then
        temp1=zero_single
        call unfill_mass_grid2t(tempa,im,jm,temp1)
        open (94,file='bckg_qsat.dat',form='unformatted')
        write(94) temp1
        close(94)
     end if
  end do

  deallocate(all_loc)
  deallocate(itemp1)
  deallocate(temp1)
  deallocate(tempa)
  deallocate(tempb)
  deallocate(temp1_prh)
  deallocate(all_loc_qsatg)
  deallocate(all_loc_prh)
  deallocate(strp)

end subroutine wr2d_binary
!----------------------------------------------------------------------------------
module ndfdgrids
!$$$ module documentation block
!           .      .    .                                       .
! module:   ndfdgrids
!   prgmmr: pondeca          org: np23                date: 2008-11-04
!
! abstract: get navigational information pertaining to ndfd grid and
!           initialize other variables used with the rtma applications
!           of the gsi.
!
! program history log:
!   2008-11-04  pondeca - consolidate scattered routines into current
!                         module
!   2010-12-05  pondeca - add the capability to shift slightly the obs
!               which are close to shorelines so that land (water) obs are not
!               mistaken for water(land) obs by the landmask.
!   2010-12-05  pondeca - add the capability to randomly pick one dataset
!               for cross-validation in the hilbert-curve procedure
!
!
! subroutines included:
!   sub init_ndfdgrid
!   sub ndfdgrid_info
!   sub latlon_to_grid0
!   sub grid_to_latlon0
!   sub relocsfcob
!   sub adjust_error
!   sub mkvalley_file
!   sub valley_adjustment
!   sub destroy_ndfdgrid
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_single,r_kind
  use mpimod, only: mype

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_ndfdgrid
  public :: ndfdgrid_info
  public :: latlon_to_grid0
  public :: grid_to_latlon0
  public :: adjust_error
  public :: relocsfcob
  public :: mkvalley_file 
  public :: valley_adjustment
  public :: destroy_ndfdgrid

  integer(i_kind),parameter::lunreloc=77

  character(60) cgrid
  integer(i_kind) nx,ny
  real(r_single),allocatable::slmask(:,:)
  real(r_single),allocatable::terrain(:,:)
  real(r_single),allocatable::valleys(:,:)
  real(r_kind) da8,alat18,elon18,elonv8,alatan8


  real(r_kind) oberrinflfact,slmland
  integer(i_kind) ineighbour,jneighbour
  logical ladjusterr
  logical fexist
  logical lshoreline
  logical,save :: relocfile_opnd=.false.
  logical valleygcheck

contains

subroutine init_ndfdgrid
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: get navigational information pertaining to ndfd grid and
!           initialize other variables used with the rtma applications
!           of the gsi.
!
! program history log:
!   2008-11-04  pondeca
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: five,zero_single
  use mpimod, only: mype
  use gsi_io, only: verbose
  implicit none

  character(3) clun33
  logical print_verbose

  namelist/parmcardreadprepb/cgrid,ladjusterr,oberrinflfact,valleygcheck, & 
                             ineighbour,jneighbour,slmland,lshoreline

  print_verbose = .false.
  if(verbose)print_verbose=.true.
  cgrid='conus'
  ladjusterr=.false.
  lshoreline=.false.
  oberrinflfact=five
  ineighbour=3
  jneighbour=3
  slmland=0.51_r_kind
  valleygcheck=.false.

  inquire(file='parmcard_input',exist=fexist)
  if (fexist) then
     open(55,file='parmcard_input',form='formatted')
     read(55,parmcardreadprepb)
     close(55)
  else
     print*,'init_ndfdgrid: WARNING - MISSING RTMA NAMELIST FILE: parmcard_input.  RUNNING WITH DEFAULT SETTINGS'
  endif

  if(print_verbose)then
     print*,'in init_ndfdgrid: cgrid=',cgrid
     print*,'in init_ndfdgrid: ladjusterr =',ladjusterr
     print*,'in init_ndfdgrid: oberrinflfact=',oberrinflfact
     print*,'in init_ndfdgrid: ineighbour=',ineighbour
     print*,'in init_ndfdgrid: jneighbour=',jneighbour
     print*,'in init_ndfdgrid: lshoreline=',lshoreline
     print*,'in init_ndfdgrid: slmland=',slmland
     print*,'in init_ndfdgrid: valleygcheck=',valleygcheck
  end if

  call ndfdgrid_info

  if(print_verbose)then
     print*,'in init_ndfdgrid: nx,ny,gridspacing=',nx,ny,da8
     print*,'in init_ndfdgrid: alat18,elon18,elonv8,alatan=',& 
                            alat18,elon18,elonv8,alatan8
  end if

  allocate(slmask(nx,ny))
  open (55,file='rtma_slmask.dat',form='unformatted')
  read(55) slmask
  close(55)

  allocate(terrain(nx,ny))
  open (55,file='rtma_terrain.dat',form='unformatted')
  read(55) terrain
  close(55)

  terrain(:,:)=max(zero_single,terrain(:,:))
  print*,'in init_ndfdgrid: terrain,min,max=',minval(terrain),maxval(terrain)

  allocate(valleys(nx,ny))

  valleys=1._r_single
  if (valleygcheck) call mkvalley_file

  if (.not.relocfile_opnd) then
     write(clun33,'(i3.3)') mype
     open (lunreloc,file='shoreline_obrelocation.dat_'//clun33, &
          form='formatted')
      write(lunreloc,*) '***********************************************************************'
      write(lunreloc,*) 'stn      itype     rlatin     rlonin       rlatout     rlonout  vartype'
      write(lunreloc,*) '***********************************************************************'
      relocfile_opnd=.true.
   endif


end subroutine init_ndfdgrid

subroutine ndfdgrid_info
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: contains navigational information pertaining to ndfd grid
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!
! output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none

  namelist/navigationinfo/nx,ny,da8,alat18,elon18,elonv8,alatan8

  if (trim(cgrid) == 'conus') then
     nx=1073
     ny=689
     alat18=20.192_r_kind
     elon18=238.446_r_kind
     da8=5079.406_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'alaska') then 
     nx=825
     ny=553
     alat18=40.530101_r_kind
     elon18=181.429000_r_kind
     da8=5953.125_r_kind
     elonv8=210.000000_r_kind
     alatan8=60.000000_r_kind
 
  elseif (trim(cgrid) == 'hawaii') then 
     nx=321
     ny=225

!    alat18=18.066780_r_kind      !before domain shift
!    elon18=198.374755_r_kind     !before domain shift

     alat18=18.072699_r_kind
     elon18=198.474999_r_kind
     da8=2500.000_r_kind
     elonv8=9999._r_kind
     alatan8=20.000000_r_kind

  elseif (trim(cgrid) == 'prico') then 
     nx=353                            !1p25 grid
     ny=257                            !1p25 grid
     alat18=16.828700_r_kind           !1p25 grid
     elon18=291.804700_r_kind          !1p25 grid
     da8=1250.000_r_kind               !1p25 grid
     elonv8=9999._r_kind               !1p25 grid
     alatan8=20.000000_r_kind          !1p25 grid


  elseif (trim(cgrid) == 'guam') then 
     nx=193
     ny=193
     alat18=12.349884_r_kind
     elon18=143.686538_r_kind
     da8=2500.000_r_kind
     elonv8=9999._r_kind
     alatan8=20.000000_r_kind

  elseif (trim(cgrid) == 'cohres') then
     nx=2145
     ny=1377
     alat18=20.192_r_kind
     elon18=238.446_r_kind
     da8=2539.703_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'akhres') then
     nx=1649
     ny=1105
     alat18=40.530101_r_kind
     elon18=181.429000_r_kind
     da8=2976.563_r_kind
     elonv8=210.000000_r_kind
     alatan8=60.000000_r_kind

  elseif (trim(cgrid) == 'hrrr') then
     nx=1799
     ny=1059
     alat18=21.138_r_kind
     elon18=237.280_r_kind
     da8=3000.000_r_kind
     elonv8=262.500_r_kind
     alatan8=38.500_r_kind

  elseif (trim(cgrid) == 'cohresext') then
     nx=2145
     ny=1597
     alat18=20.192_r_kind
     elon18=238.446_r_kind
     da8=2539.703_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'cohreswexp') then
     nx=2345
     ny=1597
     alat18=19.228976_r_kind
     elon18=233.723448_r_kind
     da8=2539.703_r_kind
     elonv8=265.000_r_kind
     alatan8=25.000_r_kind

  elseif (trim(cgrid) == 'juneau') then
     nx=655
     ny=855
     alat18=51.500000_r_kind
     elon18=217.500000_r_kind
     da8=1448.281_r_kind
     elonv8=225.000000_r_kind
     alatan8=60.000000_r_kind

  else
     print*,'in ndfdgrid_info: unknown grid ',cgrid,'...aborting'
     call abort
  endif

  inquire(file='navigationinfo_input',exist=fexist)
  if (fexist) then
     open(55,file='navigationinfo_input',form='formatted')
     read(55,navigationinfo)
     close(55)
  endif

end subroutine ndfdgrid_info
!****************************************************************
subroutine latlon_to_grid0(rlat8,rlon8,xx8,yy8)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: given the earth (lat,lon) for a selected point on the ndfd grid,
! use w3 subroutines to compute the (x,y) coordinates on the
! projected cartesian grid
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!   rlon8  - east longitude in degrees
!   rlat8  - latitude in degrees
!
! output argument list:
!    xx8 - x coordinate on the plane projected grid 
!    yy8 - y coordinate on the plane projected grid
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

  real(r_kind),intent(in   ) :: rlat8,rlon8
  real(r_kind),intent(  out) :: xx8,yy8

  logical lambconform
  logical polarstereo
  logical lmercator


  lambconform=trim(cgrid)=='conus'.or.trim(cgrid)=='cohres'.or.trim(cgrid)=='hrrr'.or. &
              trim(cgrid)=='cohresext'.or.trim(cgrid)=='cohreswexp'
  polarstereo=trim(cgrid)=='alaska'.or.trim(cgrid)=='akhres'.or.trim(cgrid)=='juneau'
  lmercator=trim(cgrid)=='hawaii'.or.trim(cgrid)=='guam'.or.trim(cgrid)=='prico'
  
  if (lambconform) call w3fb11(rlat8,rlon8,alat18,elon18,da8,elonv8,alatan8,xx8,yy8)
  if (polarstereo) call w3fb06(rlat8,rlon8,alat18,elon18,da8,elonv8,xx8,yy8) 
  if (lmercator)   call w3fb08(rlat8,rlon8,alat18,elon18,alatan8,da8,xx8,yy8)

end subroutine latlon_to_grid0
!****************************************************************
subroutine grid_to_latlon0(xx8,yy8,rlat8,rlon8)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: given the (x,y) coordinates on the projected cartesian grid
! for a selected point of the ndfd grid, use w3 subroutines to
! compute the earth (lat,lon)
!
! program history log:
!   2010-03-18  pondeca
!
! input argument list:
!   xx8 - x coordinate on the plane projected grid
!   yy8 - y coordinate on the plane projected grid
!
! output argument list:
!   rlon8  - east longitude in degrees
!   rlat8  - latitude in degrees
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

  real(r_kind),intent(in   ) :: xx8,yy8
  real(r_kind),intent(  out) :: rlat8,rlon8

  integer(i_kind) ierr

  logical lambconform
  logical polarstereo
  logical lmercator


  lambconform=trim(cgrid)=='conus'.or.trim(cgrid)=='cohres'.or.trim(cgrid)=='hrrr'.or. &
              trim(cgrid)=='cohresext'.or.trim(cgrid)=='cohreswexp'
  polarstereo=trim(cgrid)=='alaska'.or.trim(cgrid)=='akhres'.or.trim(cgrid)=='juneau'
  lmercator=trim(cgrid)=='hawaii'.or.trim(cgrid)=='guam'.or.trim(cgrid)=='prico'

  if (lambconform) then 
     call w3fb12(xx8,yy8,alat18,elon18,da8,elonv8,alatan8,rlat8,rlon8,ierr)
     if (ierr > 0) then
       print*,'in grid_to_latlon0: trouble,xx8,yy8,rlat8,rlon8,ierr=',&
                                           xx8,yy8,rlat8,rlon8,ierr
     endif

  endif

  if (polarstereo) call w3fb07(xx8,yy8,alat18,elon18,da8,elonv8,rlat8,rlon8)
  if (lmercator)   call w3fb09(xx8,yy8,alat18,elon18,alatan8,da8,rlat8,rlon8)

end subroutine grid_to_latlon0
!****************************************************************
subroutine adjust_error(alon,alat,oberr,oberr2)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2008-11-04
!
! abstract: Inflate the observation error is the ob is near a land-water
! boundary
!
! program history log:
!   2008-11-04  pondeca  
!
! input argument list:
!   alon  - observation east longitude in radians 
!   alat  - observation latitude in radians
!   oberr  - observation error
!   oberr2  - observation error
!
! output argument list:
!   oberr  - observation error
!   oberr2  - observation error
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: zero,zero_single

  implicit none

! Declare passed variables
  real (r_kind), intent(in   ) :: alon,alat
  real (r_kind), intent(inout) :: oberr,oberr2

! Declare local variables
  integer(i_kind) i,j,istart,jstart,is,ie,js,je
  real (r_single) rsign1,rsign2
  real(r_kind) rlat8,rlon8,xx8,yy8
  logical lcase1 !handle exception for Islands off of Southern California
 
  if (.not.ladjusterr) return

  rlon8=real(alon,r_kind)
  rlat8=real(alat,r_kind)

  if (rlon8>180._r_kind) rlon8=rlon8-360._r_kind

  lcase1=(rlon8>=-122._r_kind .and. rlon8<=-117._r_kind & 
            .and. rlat8>=32._r_kind .and. rlat8<=35._r_kind)

  if (trim(cgrid)=='conus' .or. trim(cgrid)=='cohres' .or. &
      trim(cgrid)=='cohresext' .or. trim(cgrid)=='cohreswexp') then
     if(lcase1) return
  endif

  if (rlon8<zero) rlon8=rlon8+360._r_kind

  call latlon_to_grid0(rlat8,rlon8,xx8,yy8)

  istart=floor(xx8)
  jstart=floor(yy8)

! print*,'in adjust_error: alon,rlon8,alat,rlat8,istart,jstart=', & 
!                          alon,rlon8,alat,rlat8,istart,jstart
! print*,'in adjust_error: slmask,min,max=',minval(slmask),maxval(slmask)
! print*,'in adjust_error: before, oberr,oberr2=',oberr,oberr2
  
  is=max(1,(istart-ineighbour))
  ie=min((istart+ineighbour),nx)
  js=max(1,(jstart-jneighbour))
  je=min((jstart+jneighbour),ny)

  if (slmask(is,js)<=0.5_r_single) rsign1=-1._r_single
  if (slmask(is,js)>0.5_r_single)  rsign1=+1._r_single

  do j=js,je
     do i=is,ie
        if (slmask(i,j)<=0.5_r_single) rsign2=-1._r_single
        if (slmask(i,j)>0.5_r_single)  rsign2=+1._r_single
        if (rsign1*rsign2<zero_single) then
           oberr=oberr*oberrinflfact
           oberr2=oberr2*oberrinflfact
           return 
        endif
     enddo 
  enddo 
  return
end subroutine adjust_error
!****************************************************************
subroutine relocsfcob(rlon8,rlat8,cobtypein,cstationin,kxin)
!$$$  subprogram documentation block
!
!   prgmmr: pondeca           org: np20                date: 2010-03-18
!
! abstract: relocate ob that land-sea mask says it's on water to 
! nearby land grid point
!
! program history log:
!   2010-03-18  pondeca  
!
! input argument list:
!   rlon8  - observation east longitude in radians
!   rlat8  - observation latitude in radians
!
! output argument list:
!   rlon8  - relocated observation east longitude in radians
!   rlat8  - relocated observation latitude in radians
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use constants, only: zero,zero_single,half,rad2deg,deg2rad

  implicit none

! Declare passed variables
  real (r_kind), intent(inout   ) :: rlon8,rlat8
  character(len=*) ,intent(in   ) :: cobtypein
  character(len=8) ,intent(in   ) :: cstationin
  integer(i_kind)  ,intent(in   ) :: kxin

! Declare local parameters
  integer(i_kind),parameter::npts=300
  real(r_single),parameter::dx=0.03125_r_single
  real(r_single),parameter::dy=0.03125_r_single
  real(r_single),parameter::del=0.125_r_single

! Declare local variables
  character(20) cvarname
  integer(i_kind) i,j,istart,jstart,is,ie,js,je
  real(r_kind) rlonin8,rlatin8,xxin8,yyin8,xx8,yy8
  real(r_single) xxin,yyin
  real(r_single) dist,distmin,ri,rj,delx,dely
  real(r_single) ris,rie,rjs,rje,rimin,rjmin
  real(r_single) rlonin,rlatin,rlon,rlat
  real(r_single) slmin,slmout,slmask0
  logical lfound


  if (.not.lshoreline) return

  rlonin8=rlon8*rad2deg
  rlatin8=rlat8*rad2deg
  rlatin=real(rlatin8,r_single)
  rlonin=real(rlonin8,r_single)

  call latlon_to_grid0(rlatin8,rlonin8,xxin8,yyin8)

  xxin=real(xxin8,kind=r_single)
  yyin=real(yyin8,kind=r_single)

  istart=floor(xxin)
  jstart=floor(yyin)

  call bilinear_2d0(slmask,nx,ny,slmin,yyin,xxin)!Note the reverse order "yyin,xxin"

! print*,'in relocsfcob: cstationin,slmin,rlonin8,rlatin8=',trim(cstationin),slmin,rlonin8,rlatin8

  slmout=-9999.

  if (slmin < 0.75_r_single) then
  
     is=max(1,(istart-ineighbour))
     ie=min((istart+ineighbour),nx)
     js=max(1,(jstart-jneighbour))
     je=min((jstart+jneighbour),ny)

     ris=real(is,r_single)
     rie=real(ie,r_single)
     rjs=real(js,r_single)
     rje=real(je,r_single)

     distmin=1.e+20_r_single
     lfound=.false.

     do j=1,npts
      rj=rjs+real(j-1,r_single)*dy  
      if (rj > rje) cycle
      do i=1,npts
         ri=ris+real(i-1,r_single)*dx  
         if (ri > rie) cycle

         call bilinear_2d0(slmask,nx,ny,slmask0,rj,ri)

         if (slmask0>=slmland) then
            lfound=.true.
            dist=(ri-xxin)*(ri-xxin)+(rj-yyin)*(rj-yyin)                        
            if (dist > zero_single) dist=sqrt(dist)
            if (dist < distmin) then 
               rimin=ri
               rjmin=rj
               distmin=dist
               slmout=slmask0
            endif
         endif
      enddo 
     enddo 

     delx=zero_single
     dely=zero_single

     if (lfound) then
        if ((rimin-xxin) < zero_single) delx=-del
        if ((rimin-xxin) > zero_single) delx=+del
        if ((rjmin-yyin) < zero_single) dely=-del
        if ((rjmin-yyin) > zero_single) dely=+del

        call bilinear_2d0(slmask,nx,ny,slmask0,rjmin+dely,rimin+delx)

        if (slmask0>=slmland) then
           xx8=real(rimin+delx,r_kind)
           yy8=real(rjmin+dely,r_kind)
           slmout=slmask0
         else
           xx8=real(rimin,r_kind)
           yy8=real(rjmin,r_kind)
        endif

        call grid_to_latlon0(xx8,yy8,rlat8,rlon8)

        rlat=real(rlat8,r_single)
        rlon=real(rlon8,r_single)

        if (trim(cobtypein)=='t') cvarname='temperatureob'
        if (trim(cobtypein)=='q') cvarname='moistureob'
        if (trim(cobtypein)=='ps') cvarname='psfcob'
        if (trim(cobtypein)=='uv') cvarname='windob'
        if (trim(cobtypein)=='spd') cvarname='wspdob'
        write(lunreloc,'(a8,i6,4(f12.4),3x,a20)') cstationin,kxin,rlatin,rlonin,rlat,rlon,cvarname
     endif

!    print*,'in relocsfcob: cstationin,kxin,rlatin,rlonin,rlat,rlon,cvarname=', & 
!              trim(cstationin),kxin,rlatin,rlonin,rlat,rlon,trim(cvarname) 
!    print*,'in relocsfcob: rlatin8,rlonin8,xxin8,yyin8=',rlatin8,rlonin8,xxin8,yyin8
!    print*,'in relocsfcob: rlat8,rlon8,xx8,yy8=',rlat8,rlon8,xx8,yy8
!    print*,'in relocsfcob: istart,jstart,imin,jmin,distmin=',istart,jstart,imin,jmin,distmin
!    print*,'in relocsfcob: slmin,slmout=',slmin,slmout
!    print*,'in relocsfcob: ************************************************'

    rlat8=rlat8*deg2rad
    rlon8=rlon8*deg2rad

  endif

end subroutine relocsfcob
!****************************************************************
!****************************************************************
subroutine mkvalley_file

  use anisofilter, only: smther_one
  use gsi_io, only: verbose

  implicit none

  real(r_single) radius0,hdiff0
  real(r_single) radius2,hstdmin
  real(r_single) radius3
  real(r_single) hmean,hmin,hmax
  real(r_single) stdmax
  real(r_single) rminsq0,rminsq
  integer(i_kind) i,j,ijdel,ii,jj,ncount
  integer(i_kind) npassvalley
  integer(i_kind) npassstd
  integer(i_kind) npasscomposite
  real(r_single),allocatable::fldstd(:,:,:)
  real(r_single),allocatable::auxfld(:,:)
  logical truevalleymap
  logical std_based_valleymap
  logical composite_valleymap
  logical smooth_composite
  logical print_verbose

  namelist/valleymapsettings/radius0,hdiff0,npassvalley,radius2,hstdmin, & 
                             npassstd,radius3,npasscomposite, &
                             truevalleymap,std_based_valleymap, & 
                             composite_valleymap,smooth_composite

  data radius0/100000./  !meters
  data hdiff0/2000./     !meters
  data npassvalley/0/    !# of smoothing passes  /3/
  data radius2/40000./   !meters
  data hstdmin/1.5/      !minimum value of the std that is kept in the std-based 'valleymap'
  data radius3/100000./  !meters
  data npassstd/0/       !# of smoothing passes
  data npasscomposite/2/ !# of smoothing passes
  data truevalleymap/.false./
  data std_based_valleymap/.false./
  data composite_valleymap/.true./
  data smooth_composite/.false./

  print_verbose=.false.
  if(verbose)print_verbose=.true.

  inquire(file='valley_map.dat',exist=fexist)
  if (fexist) then
     if (print_verbose .and. mype==0) print*,'in mkvalley_file: read in valley map from external file'
     open(55,file='valley_map.dat',form='unformatted')
     read(55) valleys
     close(55)
     if (print_verbose .and. mype==0) print*,'in mkvalley_file: valleys,min,max=',minval(valleys),maxval(valleys)
     return
  endif



  inquire(file='parmcard_input',exist=fexist)
  if(fexist) then
    open (55,file='parmcard_input',form='formatted')
    read (55,valleymapsettings)
    close(55)
  endif



  if (composite_valleymap) then
     if (print_verbose .and. mype==0) print*,'in mkvalley_file: since composite_valleymap is .true.,&
                         &force truevalleymap and std_based_valleymap to be .true. as well'
     truevalleymap=.true.
     std_based_valleymap=.true.
  endif



  if (print_verbose .and. mype==0) then
     print*,' -----in  mkvalley_file -----'
     print*,'radius0,hdiff0,npassvalley=',radius0,hdiff0,npassvalley
     print*,'radius2,hstdmin,npassstd=',radius2,hstdmin,npassstd
     print*,'radius3,npasscomposite=',radius3,npasscomposite
     print*,'truevalleymap=',truevalleymap
     print*,'std_based_valleymap=',std_based_valleymap
     print*,'composite_valleymap=',composite_valleymap
     print*,'smooth_composite=',smooth_composite
     print*,' ---------------------- -----'
  endif
  if (print_verbose .and. mype==0) print*,'in mkvalley_file: terrain,min,max=',minval(terrain),maxval(terrain)

 

  if (truevalleymap) then
     ijdel=nint(radius0/real(da8,kind=r_single))
     if (print_verbose .and. mype==0) print*,'in mkvalley_file: for true valley map: radius0,hdiff0,ijdel=',radius0,hdiff0,ijdel

     valleys(:,:)=1._r_single
     do j=1,ny
        do i=1,nx
           hmean=0._r_single; ncount=0 
           hmin=+huge(hmin) ; hmax=-huge(hmax)
           do jj=max(1,j-ijdel),min(ny,j+ijdel)
              do ii=max(1,i-ijdel),min(nx,i+ijdel)
                 if ( ((i-ii)*(i-ii) + (j-jj)*(j-jj)) <= (ijdel*ijdel) )  then
                    hmean=hmean+terrain(ii,jj)
                    ncount=ncount+1
                    hmin=min(hmin,terrain(ii,jj))
                    hmax=max(hmax,terrain(ii,jj))
                 endif
              enddo
           enddo
           hmean=hmean/max(1._r_single,real(ncount,r_single))

           if ((hmax-hmin)>=hdiff0 .and. terrain(i,j)<hmean) & 
           valleys(i,j)=terrain(i,j)/hmean   
        enddo
     enddo

     if (.not.composite_valleymap) then
        if (mype==0) then 
          open (55,file='valley_map_unsmoothed.dat',form='unformatted')
          write(55) valleys
          close(55)
        endif

        call smther_one(valleys,1,nx,1,ny,npassvalley)
        if (print_verbose .and. mype==0)print*,'in mkvalley_file: valleys,min,max=',minval(valleys),maxval(valleys)

        if (mype==0) then
          open (55,file='valley_map.dat',form='unformatted')
          write(55) valleys
          close(55)
        endif
     endif
  endif



  if (std_based_valleymap) then
     allocate(fldstd(nx,ny,1))
     fldstd(1:nx,1:ny,1)=terrain(1:nx,1:ny)

     call get_fldstd(fldstd,1,nx,1,ny,1,1,real(radius2,kind=r_kind),npassstd,mype)

     do j=1,ny
        do i=1,nx
           if (fldstd(i,j,1) < hstdmin) fldstd(i,j,1)=0._r_single
        enddo
     enddo
     fldstd=-(fldstd-maxval(fldstd)) !sort of convert to valleys of stds

     ijdel=nint(radius3/real(da8,kind=r_single))
     if (print_verbose .and. mype==0) print*,'in mkvalley_file: search area to renormalize std of terrain: radius3,ijdel=',radius3,ijdel

     allocate(auxfld(nx,ny))
     auxfld(:,:)=1._r_single
     do j=1,ny
        do i=1,nx
           stdmax=-huge(stdmax)
           do jj=max(1,j-ijdel),min(ny,j+ijdel)
              do ii=max(1,i-ijdel),min(nx,i+ijdel)
                 if ( ((i-ii)*(i-ii) + (j-jj)*(j-jj)) <= (ijdel*ijdel) ) stdmax=max(stdmax,fldstd(ii,jj,1))
              enddo
           enddo
           if (stdmax > 0._r_single) auxfld(i,j)=fldstd(i,j,1)/stdmax
        enddo
     enddo

     if (.not.composite_valleymap) then
        valleys(:,:)=auxfld(:,:)
        if (mype==0) then 
          open (55,file='valley_map_unsmoothed.dat',form='unformatted')
          write(55) valleys
          close(55)
        endif

        if (mype==0) then
          open (55,file='valley_map.dat',form='unformatted')
          write(55) valleys
          close(55)
        endif

        if (print_verbose) print*,'in mkvalley_file: valleys,min,max=',minval(valleys),maxval(valleys)
     endif
  endif



  if (composite_valleymap) then
     if (smooth_composite) then
        fldstd(1:nx,1:ny,1)=auxfld(1:nx,1:ny)
        ijdel=nint(radius3/real(da8,kind=r_single))
        do j=1,ny
           do i=1,nx
              if (fldstd(i,j,1) >=1._r_single .and. valleys(i,j) < 1._r_single) then
                 rminsq=+huge(rminsq)
                 do jj=max(1,j-ijdel),min(ny,j+ijdel)
                    do ii=max(1,i-ijdel),min(nx,i+ijdel)
                       if (ii==i .and. jj==j) cycle
                       if (fldstd(ii,jj,1) < 1._r_single) then
                          rminsq0=real((i-ii)*(i-ii) + (j-jj)*(j-jj),r_single)
                          if (rminsq0 < rminsq .and. rminsq0 <= real(ijdel*ijdel,r_single)) then 
                             rminsq=rminsq0
                             auxfld(i,j)=fldstd(ii,jj,1)  !Note that you are changing auxfld, not fldstd
                          endif
                       endif
                    enddo
                 enddo
              endif
           enddo
        enddo
        valleys(:,:)=auxfld(:,:)
     elseif (.not.smooth_composite) then
        valleys(:,:)=min(valleys(:,:),auxfld(:,:))
     endif

     if (mype==0) then 
       open (55,file='valley_map_unsmoothed.dat',form='unformatted')
       write(55) valleys
       close(55)
     endif

     call smther_one(valleys,1,nx,1,ny,npasscomposite)
     if (print_verbose)print*,'in mkvalley_file: valleys,min,max=',minval(valleys),maxval(valleys)

     if (mype==0) then
       open (55,file='valley_map.dat',form='unformatted')
       write(55) valleys
       close(55)
     endif
  endif



  if (std_based_valleymap) then
     deallocate(fldstd)
     deallocate(auxfld)
  endif
end subroutine mkvalley_file
!****************************************************************
!****************************************************************
subroutine valley_adjustment(xob,yob,usage_rj)
!================================================================     
!           (im,jp)      (i,jp)       ip,jp)
!              --------------------------
!              |           |            |
!              |           |            |
!              |           |            |
!              |           |            |
!        (im,j)----------(i,j)----------- (ip.j)
!              |           |            |
!              |           |            |
!              |           |            |
!              |           |            |
!              --------------------------
!           (im,jm)      (i,jm)       ip,jm)
!================================================================     
  implicit none

!Declare passed variables
  real(r_kind),intent(in):: xob,yob
  real(r_kind),intent(inout):: usage_rj

!Declare local parameters
  real(r_single),parameter:: sone=1._r_single
  real(r_kind),parameter:: dusage=0.25_r_kind

!Declare local variables
  integer(i_kind) i,j,im,jm,ip,jp

  if (valleygcheck) then
     i=int(xob) ; im=max(1,i-1) ; ip=min(nx,i+1) 
     j=int(yob) ; jm=max(1,j-1) ; jp=min(ny,j+1)

     if (valleys(i,j )<sone .or. valleys(im,j) < sone .or. valleys(ip,j)<sone .or. & 
         valleys(i,jm)<sone .or. valleys(i,jp)<sone .or. & 
         valleys(im,jm)<sone .or. valleys(ip,jm)<sone .or. & 
         valleys(im,jp)<sone .or. valleys(ip,jp)<sone ) usage_rj=usage_rj+dusage
  endif
  
end subroutine valley_adjustment
!****************************************************************
!****************************************************************
subroutine  destroy_ndfdgrid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_ndfdgrid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
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

  deallocate(slmask)
  deallocate(terrain)
  deallocate(valleys)
  if (relocfile_opnd) close (lunreloc)
end subroutine destroy_ndfdgrid

end module ndfdgrids
!************************************************************
!************************************************************
module hilbertcurve
!$$$ module documentation block
!           .      .    .                                       .
! module:   hilbertcurve
!   prgmmr: park             org: kma                 date: 2006-09-29
!
! abstract: contains subroutines to compute cross-validation datasets
!           using jim purser's hilbert curve approach
!
! program history log:
!   2006-09-29  park 
!   2008-11-04  pondeca - consolidate code by putting subroutines
!                         in a module. note: code is designed to handle
!                         each ob-type (eg. T, uv, q, etc) separately.
!                         The following restrictions apply: 
!                         (i) in convinfo, the number of cross-validating
!                         groups should be the same for all ob-subtypes
!                         of a given ob type such as T. In other words, it must
!                         be the same for subtypes 180, 181, etc. of the T-obs.
!                         If they are not the same, then the number of
!                         cross-validating subsets used by purser's hilbert curve 
!                         routine will be equal to the last number
!                         of groups that the code reads in for that ob type in
!                         read_prepbufr.f90; 
!                         (ii) the code will not work properly if there is more 
!                         than one processor handling the same ob type or if the
!                         same processor handles more than one ob-type. This
!                         poses no threat to the RTMA so far, but nonetheless it is
!                         something that will have to be dealt with in the future.
!   2014-06-16  carley - add tcamt and lcbas
!   2014-03-19  pondeca - add cross-validation for wspd10m, replace
!                         tobs,qobs,vobs,etc., wih obstype
!   2014-04-10  pondeca - add cross-validation for td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add cross-validation for howv
!   2016-05-03  pondeca - add cross-validtaion for uwnd10m, vwnd10m
!                        
!
! subroutines included:
!   sub init_hilbertcurve
!   sub accum_hilbertcurve
!   sub apply_hilbertcurve
!   sub destroy_hilbertcurve
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind,r_single

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_hilbertcurve
  public :: accum_hilbertcurve
  public :: apply_hilbertcurve
  public :: destroy_hilbertcurve

  integer(i_kind) ncross

  character(8),allocatable,dimension(:):: hil_cstation
  character(8),allocatable,dimension(:):: hil_cprovider
  character(8),allocatable,dimension(:):: hil_csubprovider
  real(r_kind),allocatable,dimension(:):: hil_dlon
  real(r_kind),allocatable,dimension(:):: hil_dlat
  real(r_kind),allocatable,dimension(:):: hil_alon
  real(r_kind),allocatable,dimension(:):: hil_alat
  real(r_kind),allocatable,dimension(:):: hil_time
  integer(i_kind),allocatable,dimension(:):: hil_ikx
  integer(i_kind),allocatable,dimension(:):: hil_kx
  integer(i_kind),allocatable,dimension(:):: hil_i
  integer(i_kind),allocatable,dimension(:):: test_set
  
  integer(i_kind) ngrps_tob
  integer(i_kind) ngrps_uvob
  integer(i_kind) ngrps_spdob
  integer(i_kind) ngrps_psob
  integer(i_kind) ngrps_qob
  integer(i_kind) ngrps_pwob
  integer(i_kind) ngrps_sstob
  integer(i_kind) ngrps_gustob
  integer(i_kind) ngrps_visob
  integer(i_kind) ngrps_wspd10mob
  integer(i_kind) ngrps_td2mob
  integer(i_kind) ngrps_mxtmob
  integer(i_kind) ngrps_mitmob
  integer(i_kind) ngrps_pmslob
  integer(i_kind) ngrps_howvob
  integer(i_kind) ngrps_tcamtob
  integer(i_kind) ngrps_lcbasob
  integer(i_kind) ngrps_cldchob
  integer(i_kind) ngrps_uwnd10mob
  integer(i_kind) ngrps_vwnd10mob

  integer(i_kind) ngrpsfact0

  logical random_cvgrp
  real(r_kind) usagecv,usage_dup

contains

subroutine init_hilbertcurve(maxobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!   2014-06-16  carley - add tcamt and lcbas
!
!   input argument list:
!    maxobs
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use mpimod, only: mype
  use gsi_io, only: verbose

  implicit none

  integer(i_kind),intent(in   ) :: maxobs

  character(1),parameter::blank=' '
  integer(i_kind) i,k
  logical fexist, print_verbose

  namelist/parmcardhcurve/random_cvgrp,usagecv,usage_dup,ngrps_tob,ngrps_uvob, & 
                    ngrps_spdob,ngrps_psob,ngrps_qob, & 
                    ngrps_pwob,ngrps_sstob,ngrps_gustob,ngrps_visob, &
                    ngrps_td2mob, ngrps_mxtmob,ngrps_mitmob, &
                    ngrps_pmslob, ngrps_howvob, &
                    ngrps_tcamtob,ngrps_lcbasob,ngrps_cldchob, & 
                    ngrps_uwnd10mob,ngrps_vwnd10mob, &
                    ngrpsfact0

  random_cvgrp=.false.
  usagecv=3._r_kind
  usage_dup=3._r_kind
  ngrps_tob=5
  ngrps_uvob=8
  ngrps_spdob=0
  ngrps_psob=5
  ngrps_qob=5
  ngrps_pwob=0
  ngrps_sstob=0
  ngrps_gustob=8
  ngrps_visob=8
  ngrps_td2mob=5
  ngrps_mxtmob=8
  ngrps_mitmob=8
  ngrps_pmslob=8
  ngrps_howvob=8
  ngrps_tcamtob=8
  ngrps_lcbasob=8
  ngrps_cldchob=8
  ngrpsfact0=10
  print_verbose = .false.
  if(verbose)print_verbose=.true.


  inquire(file='parmcard_input',exist=fexist)
  if (fexist) then
     open(55,file='parmcard_input',form='formatted')
     read(55,parmcardhcurve)
     close(55)
  else
     print*,'init_hilbertcurve: WARNING - MISSING RTMA NAMELIST FILE: parmcard_input.  RUNNING WITH DEFAULT SETTINGS'
  endif

  ngrps_wspd10mob=ngrps_uvob
  ngrps_uwnd10mob=ngrps_uvob
  ngrps_vwnd10mob=ngrps_uvob

  if(print_verbose .and. mype == 0)then
    print*,'in init_hilbertcurve: random_cvgrp=',random_cvgrp
    print*,'in init_hilbertcurve: usagecv=',usagecv
    print*,'in init_hilbertcurve: usage_dup=',usage_dup
    print*,'in init_hilbertcurve: ngrps_tob=',ngrps_tob
    print*,'in init_hilbertcurve: ngrps_uvob=',ngrps_uvob
    print*,'in init_hilbertcurve: ngrps_spdob=',ngrps_spdob
    print*,'in init_hilbertcurve: ngrps_psob=',ngrps_psob
    print*,'in init_hilbertcurve: ngrps_qob=',ngrps_qob
    print*,'in init_hilbertcurve: ngrps_pwob=',ngrps_pwob
    print*,'in init_hilbertcurve: ngrps_sstob=',ngrps_sstob
    print*,'in init_hilbertcurve: ngrps_gustob=',ngrps_gustob
    print*,'in init_hilbertcurve: ngrps_visob=',ngrps_visob
    print*,'in init_hilbertcurve: ngrps_wspd10mob=',ngrps_wspd10mob
    print*,'in init_hilbertcurve: ngrps_td2mob=',ngrps_td2mob
    print*,'in init_hilbertcurve: ngrps_mxtmob=',ngrps_mxtmob
    print*,'in init_hilbertcurve: ngrps_mitmob=',ngrps_mitmob
    print*,'in init_hilbertcurve: ngrps_pmslob=',ngrps_pmslob
    print*,'in init_hilbertcurve: ngrps_howvob=',ngrps_howvob
    print*,'in init_hilbertcurve: ngrps_tcamtob=',ngrps_tcamtob
    print*,'in init_hilbertcurve: ngrps_lcbasob=',ngrps_lcbasob
    print*,'in init_hilbertcurve: ngrps_cldchob=',ngrps_cldchob
    print*,'in init_hilbertcurve: ngrps_uwnd10mob=',ngrps_uwnd10mob
    print*,'in init_hilbertcurve: ngrps_vwnd10mob=',ngrps_vwnd10mob
    print*,'in init_hilbertcurve: ngrpsfact0=',ngrpsfact0
  end if


  ncross=0

  allocate(hil_cstation(maxobs))
  allocate(hil_cprovider(maxobs))
  allocate(hil_csubprovider(maxobs))
  allocate(hil_dlon(maxobs))
  allocate(hil_dlat(maxobs))
  allocate(hil_alon(maxobs))
  allocate(hil_alat(maxobs))
  allocate(hil_time(maxobs))
  allocate(hil_ikx(maxobs))
  allocate(hil_kx(maxobs))
  allocate(hil_i(maxobs))

  do i=1,maxobs
     write(hil_cstation(i),'(8a1)') (blank,k=1,8)
     write(hil_cprovider(i),'(8a1)') (blank,k=1,8)
     write(hil_csubprovider(i),'(8a1)') (blank,k=1,8)
  enddo

end subroutine init_hilbertcurve

subroutine accum_hilbertcurve(usage,cstation,cprovider,csubprovider, &
                              alat,alon,dlat,dlon,time,toff,ikx,kx,ndata)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    accum_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!    usage,time,toff
!    dlat,dlon       !grid-relative lat and lon
!    alat,alon       !earth lat and lon in radians on entry
!    ikx,kx,ndata
!    cstation,cprovider,csubprovider
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use constants, only: rad2deg

  implicit none

  real(r_kind),intent(in)::usage,time,toff
  real(r_kind),intent(in)::dlat,dlon       !grid-relative lat and lon
  real(r_kind),intent(in)::alat,alon       !earth lat and lon in radians on entry
  integer(i_kind),intent(in)::ikx,kx,ndata
  character(8),intent(in)::cstation,cprovider,csubprovider
 
  logical goodkx
 
  goodkx= ( (kx>=180.and.kx<=188).or.(kx>=280.and.kx<=288).or.&
            (kx>=192.and.kx<=195).or.(kx>=292.and.kx<=295) ) .and.&
            usage<6._r_kind
    
!!!      if(ncnumgrp(ikx) > 0 .and. usage < 6._r_kind) then
!!!      if(usage < 6._r_kind) then
      if(goodkx) then
         ncross=ncross+1
         hil_cstation(ncross)=cstation
         hil_cprovider(ncross)=cprovider
         hil_csubprovider(ncross)=csubprovider
         hil_dlat(ncross)=dlat
         hil_dlon(ncross)=dlon
         hil_alat(ncross)=alat*rad2deg
         hil_alon(ncross)=alon*rad2deg
         hil_time(ncross)=time-toff
         hil_ikx(ncross)=ikx
         hil_kx(ncross)=kx
         hil_i(ncross)=ndata

!     write(6,*) 'CHECK0:',ndata,ncross,hil_ikx(ncross),hil_kx(ncross), & 
!                ncnumgrp(hil_ikx(ncross)),ncgroup(hil_ikx(ncross))
  endif

end subroutine accum_hilbertcurve


subroutine apply_hilbertcurve(maxobs,obstype,cdata_usage)

!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    apply_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!   2014-06-16  carley - add tcamt and lcbas
!   2014-06-26  carley - reorganized use of cdata_usage array
! 
!
!   input argument list:
!    maxobs
!    obstype   
!    cdata_usage
!
!   output argument list:
!    cdata_usage
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use constants, only: zero
  use convinfo, only:  ncmiter,ncgroup,ncnumgrp
  use  gridmod, only: nlon,nlat
  use gsi_io, only: verbose

  implicit none


!Declare passed variables

  character(len=*),intent(in  ) :: obstype
  integer(i_kind),intent(in   ) :: maxobs
  real(r_kind)   ,intent(inout) :: cdata_usage(maxobs)

!Declare local variables
  real(r_kind),parameter:: epsilon=1.e-03_r_kind
  integer(i_kind) i,j,n,nt,ncnumgrp0,ncgroup0
  integer(i_kind),allocatable,dimension(:)::hilflag,hilikx,hili,ipoint
  real(r_kind),allocatable::hildlon(:),hildlat(:)
  real(r_kind) usage
  integer(i_kind) ncvcount(100)
  character(60) outfile
  character(30) cstring
  character(2) clun
  logical ldup
  logical print_verbose


! Turning on print_verbose will result in additional prints from this routine
! only.
  print_verbose = .false.
  if(verbose)print_verbose=.true.

  allocate(hilflag(maxobs))
  allocate(hilikx(maxobs))
  allocate(hili(maxobs))
  allocate(ipoint(maxobs))
  allocate(hildlon(maxobs))
  allocate(hildlat(maxobs))
      


      !initialize hildlon and hildlat (prevents floating point failure when run in debug mode)
      hildlon=zero
      hildlat=zero

      if(print_verbose)print*,'in apply_hilbertcurve: obstype=',obstype


      nt=ncross
      if(nt.gt.0) then

         !--deal with duplicate obs. use only the ob that is nearest
         !  to the valid assimilation time

         if(print_verbose)print*,'in apply_hilbertcurve: before duplicate removal: obstype, ncross=',trim(obstype),nt

         hilflag=+1
         do j=1,nt
            do i=j+1,nt
!              ldup=hil_cstation(i)(1:8)==hil_cstation(j)(1:8)        .and. & 
!                   hil_cprovider(i)(1:8)==hil_cprovider(j)(1:8)      .and. &
!                   hil_csubprovider(i)(1:8)==hil_csubprovider(j)(1:8).and. &
!                   hil_kx(i)==hil_kx(j)                              .and. &
!                   abs(hil_dlon(i)-hil_dlon(j))<epsilon              .and. &
!                   abs(hil_dlat(i)-hil_dlat(j))<epsilon  
       
               ldup=abs(hil_dlon(i)-hil_dlon(j))<epsilon .and. &
                    abs(hil_dlat(i)-hil_dlat(j))<epsilon  

                if ( ldup ) then 
                  if ( abs(hil_time(i)) >= abs(hil_time(j)) ) then 
                       hilflag(i)=-1
                     else
                       hilflag(j)=-1
                  endif
                endif
            enddo
         enddo

         ncross=0
         do i=1,nt
            if (hilflag(i) > 0) then 
                ncross=ncross+1
                hildlon(ncross)=hil_dlon(i)
                hildlat(ncross)=hil_dlat(i)
                hilikx(ncross)=hil_ikx(i)
                hili(ncross)=hil_i(i)
                ipoint(ncross)=i
            endif
         enddo

         if(print_verbose)print*,'in apply_hilbertcurve: after duplicate removal: obstype,ncross=',trim(obstype),ncross

        !--evoke the main code for the hilbert curve

        allocate(test_set(ncross))

        if (random_cvgrp) then 
            if(obstype =='t'       ) ncnumgrp0=ngrps_tob
            if(obstype == 'uv'     ) ncnumgrp0=ngrps_uvob
            if(obstype == 'spd'    ) ncnumgrp0=ngrps_spdob
            if(obstype == 'ps'     ) ncnumgrp0=ngrps_psob
            if(obstype == 'q'      ) ncnumgrp0=ngrps_qob
            if(obstype == 'pw'     ) ncnumgrp0=ngrps_pwob
            if(obstype == 'sst'    ) ncnumgrp0=ngrps_sstob
            if(obstype == 'gust'   ) ncnumgrp0=ngrps_gustob
            if(obstype == 'vis'    ) ncnumgrp0=ngrps_visob
            if(obstype == 'wspd10m') ncnumgrp0=ngrps_wspd10mob
            if(obstype == 'td2m'   ) ncnumgrp0=ngrps_td2mob
            if(obstype == 'mxtm'   ) ncnumgrp0=ngrps_mxtmob
            if(obstype == 'mitm'   ) ncnumgrp0=ngrps_mitmob
            if(obstype == 'pmsl'   ) ncnumgrp0=ngrps_pmslob
            if(obstype == 'howv'   ) ncnumgrp0=ngrps_howvob
            if(obstype == 'tcamt'  ) ncnumgrp0=ngrps_tcamtob
            if(obstype == 'lcbas'  ) ncnumgrp0=ngrps_lcbasob
            if(obstype == 'cldch'  ) ncnumgrp0=ngrps_cldchob
            if(obstype == 'uwnd10m') ncnumgrp0=ngrps_uwnd10mob
            if(obstype == 'vwnd10m') ncnumgrp0=ngrps_vwnd10mob
          else
            ncnumgrp0=ncnumgrp(hilikx(ncross)) ! number of cross-validating datasets is
                                               ! chosen to be the last "number of groups" 
                                               ! specified in convinfo for that ob type. 
                                               ! there is no particular reason to do so.
        endif

        hildlon=hildlon/nlon
        hildlat=hildlat/nlat

        if (ncross > ngrpsfact0*ncnumgrp0) then
           call hilbert(ncnumgrp0,ncross,hildlon(1:ncross),hildlat(1:ncross),test_set)
         else
           print*,' ------- in apply_hilbertcurve ----------:'
           print*,'not enough obs for cross validation.  obstype,ncross=',trim(obstype),ncross
           print*,'resetting ncross to 0'
           ncross=0
           print*,' ----------------------------------------:'
        endif

        if (random_cvgrp) call shuffle(ncnumgrp0,ncgroup0)

        do i=1,ncross

           if (.not. random_cvgrp)  ncgroup0=ncgroup(hilikx(i)) !note: convinfo must be set up so that this 
                                                                !is the same group element for all ob subtypes 
                                                                !of a given ob type 

           if (i==1 .and. print_verbose) print*,'in apply_hilbertcurve: obstype,ncnumgrp0,ncgroup0=',trim(obstype),ncnumgrp0,ncgroup0 

           if (test_set(i).eq.ncgroup0) then
              if (     random_cvgrp) usage=usagecv
              if (.not.random_cvgrp) usage=ncmiter(hilikx(i))

              cdata_usage(hili(i))=usage + ( cdata_usage(hili(i))-real(int(cdata_usage(hili(i))),kind=r_kind) )

              j=ipoint(i)
              do n=1,nt
                 if (n==j) cycle
                 ldup=abs(hil_dlon(j)-hil_dlon(n))<epsilon .and. &
                      abs(hil_dlat(j)-hil_dlat(n))<epsilon  
                 if ( ldup ) then 
                    cdata_usage(hil_i(n))=usage_dup + ( cdata_usage(hil_i(n))-real(int(cdata_usage(hil_i(n))),kind=r_kind) )
                 endif
              enddo
           endif

!         write(6,*) 'CHECK2:',i,test_set(i),ncgroup(hilikx(i)),ncross
        end do
      endif

      if(ncross.gt.0) then
        ! count number of obs in each cross-validation dataset
        if(print_verbose)print*,'obstype,ncnumgrp0=',obstype,ncnumgrp0
        ncvcount=0
        do n=1,ncnumgrp0
           do i=1,ncross
              if (test_set(i).eq.n) then
                 ncvcount(n)=ncvcount(n)+1
              endif
           enddo
        if(print_verbose)print*,'n,ncvcount(n)=',n,ncvcount(n)
        enddo

        ! write all groups to a file
        if(obstype=='t')       outfile='tobs_allcv_groups'
        if(obstype=='uv')      outfile='uvobs_allcv_groups'
        if(obstype=='spd')     outfile='spdobs_allcv_groups'
        if(obstype=='ps')      outfile='psobs_allcv_groups'
        if(obstype=='q')       outfile='qobs_allcv_groups'
        if(obstype=='gust')    outfile='gustobs_allcv_groups'
        if(obstype=='vis')     outfile='visobs_allcv_groups'
        if(obstype=='wspd10m') outfile='wspd10mobs_allcv_groups'
        if(obstype=='td2m')    outfile='td2mobs_allcv_groups'
        if(obstype=='mxtm')    outfile='mxtmobs_allcv_groups'
        if(obstype=='mitm')    outfile='mitmobs_allcv_groups'
        if(obstype=='pmsl')    outfile='pmslobs_allcv_groups'
        if(obstype=='howv')    outfile='howvobs_allcv_groups'
        if(obstype=='tcamt')   outfile='tcamtobs_allcv_groups'
        if(obstype=='lcbas')   outfile='lcbasobs_allcv_groups'
        if(obstype=='cldch')   outfile='cldchobs_allcv_groups'
        if(obstype=='uwnd10m') outfile='uwnd10mobs_allcv_groups'
        if(obstype=='vwnd10m') outfile='vwnd10mobs_allcv_groups'
        open (92,file=trim(outfile),form='unformatted')

        write(92) ncross,ncnumgrp0,ncgroup0
        write(92) ncvcount

        do n=1,ncnumgrp0

           write(clun,'(i2.2)') n
           cstring='start group number '//clun
           write(92) cstring

           do i=1,ncross
              if (test_set(i).eq.n) then
                  j=ipoint(i)
                  write(92) hil_cstation(j)
                  write(92) hil_cprovider(j)
                  write(92) hil_csubprovider(j)
                  write(92) hil_kx(j), &
                            hil_alon(j),hil_alat(j), &
                            hil_dlon(j),hil_dlat(j)
              endif
           enddo
        enddo
        close(92)
      endif

  deallocate(hilflag)
  deallocate(hilikx)
  deallocate(hili)
  deallocate(ipoint)
  deallocate(hildlon)
  deallocate(hildlat)

end subroutine apply_hilbertcurve

subroutine destroy_hilbertcurve
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_hilbertcurve
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
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

  deallocate(hil_cstation)
  deallocate(hil_cprovider)
  deallocate(hil_csubprovider)
  deallocate(hil_dlon)
  deallocate(hil_dlat)
  deallocate(hil_alon)
  deallocate(hil_alat)
  deallocate(hil_time)
  deallocate(hil_ikx)
  deallocate(hil_kx)
  deallocate(hil_i)
  if (ncross>0) deallocate(test_set)

end subroutine destroy_hilbertcurve
end module hilbertcurve
!------------------------------------------------------
      subroutine bilinear_2d0(rffcst,ix,jx,rfobs,xx,yy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bilinear_2d0
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-10-02  lueken - added subpogram doc block
!
!   input argument list:
!    rffcst               - model grid value
!    ix,jx
!    xx,yy                - define coordinates in grid units
!                         of point for which interpolation is
!                         performed
!
!   output argument list:
!    rfobs                - interpolated value
!
! notes:
!
!     i+1,j |          | i+1,j+1
!         --+----------+---
!           |          | dym
!           |    *     + -
!           |   x,y    | dy
!           |          |
!         --+----+-----+---
!        i,j|<dx>|<dxm>| i,j+1
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
      use kinds, only: r_single,i_kind
      implicit none

!declare passed variables
      integer(i_kind),intent(in   ) :: ix,jx
      real(r_single) ,intent(in   ) :: rffcst(ix,jx)
      real(r_single) ,intent(in   ) :: xx,yy
      real(r_single) ,intent(  out) :: rfobs

!declare local variables
      integer(i_kind) i,j,ip,jp
      real(r_single) dx,dy,dxm,dym

      i  = ifix(yy)
      j  = ifix(xx)
      
      dx = xx - real(j,r_single)
      dy = yy - real(i,r_single)
      dxm= 1.0_r_single-dx
      dym= 1.0_r_single-dy
 
      i=min(max(1,i),ix) ; j=min(max(1,j),jx)
      ip=min(ix,i+1)     ; jp=min(jx,j+1) 

      rfobs=dxm*(dym*rffcst(i,j)+dy*rffcst(ip,j)) &
               + dx *(dym*rffcst(i,jp)+dy*rffcst(ip,jp))

      return
end subroutine bilinear_2d0
!------------------------------------------------------
!------------------------------------------------------
!------------------------------------------------------
subroutine getwdir(ue,ve,wdir)
!                .      .    .                                       .
! subprogram:   getwdir
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!    ue-
!    ve-
!
!   output argument list:
!    wdir
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind
  use constants, only: zero,deg2rad
  implicit none

! Declare passed variables
  real(r_kind),intent(in   ) :: ue,ve
  real(r_kind),intent(  out) :: wdir

! Declare local parameters
  real(r_kind),parameter::r90=90._r_kind
  real(r_kind),parameter::r180=180._r_kind
  real(r_kind),parameter::r270=270._r_kind
  real(r_kind),parameter::r360=360._r_kind

! Declare local variables
  real(r_kind) wspd2,angle

  wspd2=ue*ue+ve*ve
  if (wspd2.eq.zero) then
       wdir=zero 
       return
  endif

  if (ve.eq.zero) then
     if (ue.gt.zero) wdir = r270
     if (ue.lt.zero) wdir = r90 
    else 
     angle = atan(ue/ve)/deg2rad
     if (ue.le.zero .and. ve.le.zero ) wdir = angle
     if (ue.le.zero .and. ve.ge.zero ) wdir = angle + r180
     if (ue.ge.zero .and. ve.ge.zero ) wdir = angle + r180
     if (ue.ge.zero .and. ve.le.zero ) wdir = angle + r360
  endif

  return
end subroutine getwdir
!************************************************************
!------------------------------------------------------
!------------------------------------------------------
subroutine windfactor(p0,wfact0)
! subprogram:   windfactor
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!    p0 
!
!   output argument list:
!    wfact
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: one
  implicit none

! Declare passed variables
  real(r_kind),intent(in   ) :: p0
  real(r_kind),intent(  out) :: wfact0

! Declare local parameters
  integer(i_kind),parameter::nlevs=14

! Declare local variables
  real(r_kind) plevs(nlevs)
  real(r_kind) wfacts(nlevs)
  real(r_kind) alpha
  integer(i_kind) n

  !note:  wfacts represents the ratio 
  !       wind(n)/wind10m

  plevs(1)=1000._r_kind ; wfacts(1)=1.16
  plevs(2)=975._r_kind  ; wfacts(2)=1.23
  plevs(3)=950._r_kind  ; wfacts(3)=1.27
  plevs(4)=925._r_kind  ; wfacts(4)=1.31
  plevs(5)=900._r_kind  ; wfacts(5)=1.34
  plevs(6)=875._r_kind  ; wfacts(6)=1.33
  plevs(7)=850._r_kind  ; wfacts(7)=1.29
  plevs(8)=825._r_kind  ; wfacts(8)=1.25
  plevs(9)=800._r_kind  ; wfacts(9)=1.19
  plevs(10)=750._r_kind ; wfacts(10)=1.12
  plevs(11)=725._r_kind ; wfacts(11)=1.16
  plevs(12)=700._r_kind ; wfacts(12)=1.20
  plevs(13)=650._r_kind ; wfacts(13)=1.23
  plevs(14)=600._r_kind ; wfacts(14)=1.14

  do n=1,nlevs-1
     if (p0.ge.plevs(n+1) .and. p0.le.plevs(n)) then
        alpha=(wfacts(n+1)-wfacts(n))/(plevs(n+1)-plevs(n))
        wfact0=wfacts(n)+alpha*(p0-plevs(n))
     endif
  enddo

  if (p0 < plevs(nlevs))  wfact0=wfacts(nlevs)
  if (p0 > plevs(1))      wfact0=wfacts(1)

  wfact0=one/wfact0
end subroutine windfactor
!============================================================
subroutine shuffle(ngrps,ngrp0)

  use kinds, only: r_kind,i_kind
  use obsmod, only: iadate
  use gsi_io, only: verbose
  implicit none

!Declare passed variables
  integer(i_kind),intent(in)::  ngrps
  integer(i_kind),intent(out):: ngrp0

!Declare local variables
  integer(i_kind) iseed,nt
  logical print_verbose
! real(r_kind) randx
  print_verbose=.false.
  if(verbose)print_verbose=.true.

  call w3fs21(iadate,iseed) !use # of minutes since 0000, 1 jan 1978
                            !as the seed

  nt=sum(iadate)            !arbitrarily select number of times to call
                            !random number generator
 
  if(print_verbose)print*,'in shuffle: iadate,iseed,nt=',iadate,iseed,nt


! ngrp0=nint(real((ngrps-1),r_kind)*randx+0.001_r_kind)+1

! print*,'in shuffle: ngrps,randx,ngrp0=',ngrps,randx,ngrp0

! use this instead:  /20Jan2011
  ngrp0=mod(nt,ngrps)
  if (ngrp0==0) ngrp0=ngrps
  if(print_verbose)print*,'in shuffle:ngrps,ngrp0=',ngrps,ngrp0

end subroutine shuffle
!===========================================================================
subroutine landlake_uvmerge_original(u,v,uland,vland,uwter,vwter,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       
! subprogram:    landlake_uvmerge merge
!   prgmmr: pondeca          org: np22                date: 2013-07-15
!
! abstract: merge land and lake u,v increments together (iflg=1) or perform
!           the adjoint operation (iflg=0)
!
! program history log:
!   2013-07-15 pondeca
!
!   input argument list:
!     u         - u grid values
!     v         - v grid values
!     uland     - land u grid values
!     vland     - land v grid values
!     uwter     - lake u grid values
!     vwter     - lake v grid values
!     iflg      - flag for u,v manipulation
!               0: u,v --> uland,vland,uwter,vwter
!               1: uland,vland,uwter,vwter --> u,v
!
!   output argument list:
!     u         - u grid values
!     v         - v grid values
!     uland     - land u grid values
!     vland     - land v grid values
!     uwter     - lake u grid values
!     vwter     - lake v grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig
  use guess_grids, only: isli2
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,uland,vland,uwter,vwter

! Declare local variables
  integer(i_kind) i,j,k

  if (iflg == 0) then 
        do j=1,lon2
           do i=1,lat2
              if(isli2(i,j) == 1) then
                 do k=1,nsig                           !note: this subroutine is always called with nsig=1
                    uland(i,j,k)=+u(i,j,k)
                    vland(i,j,k)=+v(i,j,k)
                 enddo
               else
                 do k=1,nsig
                    uwter(i,j,k)=+u(i,j,k)
                    vwter(i,j,k)=+v(i,j,k)
                 enddo
              end if
           enddo
        enddo

     else
   
        do j=1,lon2
           do i=1,lat2
              if(isli2(i,j) == 1) then
                 do k=1,nsig
                    u(i,j,k)=uland(i,j,k)
                    v(i,j,k)=vland(i,j,k)
                 enddo
               else
                 do k=1,nsig
                    u(i,j,k)=uwter(i,j,k)
                    v(i,j,k)=vwter(i,j,k)
                 enddo
              end if
           enddo
        enddo
  endif
end subroutine landlake_uvmerge_original
!===========================================================================
subroutine landlake_uvmerge(u,v,uland,vland,uwter,vwter,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       
! subprogram:    landlake_uvmerge merge
!   prgmmr: pondeca          org: np22                date: 2013-07-15
!
! abstract: merge land and lake u,v increments together (iflg=1) or perform
!           the adjoint operation (iflg=0)
!
! program history log:
!   2013-07-15 pondeca
!
!   input argument list:
!     u         - u grid values
!     v         - v grid values
!     uland     - land u grid values
!     vland     - land v grid values
!     uwter     - lake u grid values
!     vwter     - lake v grid values
!     iflg      - flag for u,v manipulation
!               0: u,v --> uland,vland,uwter,vwter
!               1: uland,vland,uwter,vwter --> u,v
!
!   output argument list:
!     u         - u grid values
!     v         - v grid values
!     uland     - land u grid values
!     vland     - land v grid values
!     uwter     - lake u grid values
!     vwter     - lake v grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,region_lat,region_lon, & 
                     nlon_regional,nlat_regional,istart,jstart
  use guess_grids, only: isli2
  use mpimod, only: mype
  use constants, only: rad2deg
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: iflg
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: u,v,uland,vland,uwter,vwter

! Declare local variables
  integer(i_kind) i,j,k
  integer(i_kind) iglob,jglob
  integer(i_kind) mm1
  real(r_kind) flon,flat
  logical glerlarea

! Declare local parameters
!    Great Lakes
  real(r_kind),parameter::flon1=-93._r_kind
  real(r_kind),parameter::flon2=-75._r_kind
  real(r_kind),parameter::flat1=40.5_r_kind
  real(r_kind),parameter::flat2=49.5_r_kind

!    Great Salt Lake
  real(r_kind),parameter::slon1=-113._r_kind
  real(r_kind),parameter::slon2=-112._r_kind
  real(r_kind),parameter::slat1=40.6_r_kind
  real(r_kind),parameter::slat2=41.7_r_kind

  mm1=mype+1

  if (iflg == 0) then 
        do j=1,lon2
           jglob=max(1,min(j+jstart(mm1)-2,nlon_regional))
           do i=1,lat2
              iglob=max(1,min(i+istart(mm1)-2,nlat_regional))
              flat=region_lat(iglob,jglob)*rad2deg
              flon=region_lon(iglob,jglob)*rad2deg
              glerlarea=(flat>=flat1.and.flat<=flat2).and.(flon>=flon1.and.flon<=flon2)
              !glerlarea=glerlarea.or.((flat>=slat1.and.flat<=slat2).and.(flon>=slon1.and.flon<=slon2))

              if(isli2(i,j) == 1 .or. .not.glerlarea) then
                 do k=1,nsig                           !note: this subroutine is always called with nsig=1
                    uland(i,j,k)=+u(i,j,k)
                    vland(i,j,k)=+v(i,j,k)
                 enddo
               else
                 do k=1,nsig
                    uwter(i,j,k)=+u(i,j,k)
                    vwter(i,j,k)=+v(i,j,k)
                 enddo
              end if
           enddo
        enddo

     else
   
        do j=1,lon2
           jglob=max(1,min(j+jstart(mm1)-2,nlon_regional))
           do i=1,lat2
              iglob=max(1,min(i+istart(mm1)-2,nlat_regional))
              flat=region_lat(iglob,jglob)*rad2deg
              flon=region_lon(iglob,jglob)*rad2deg
              glerlarea=(flat>=flat1.and.flat<=flat2).and.(flon>=flon1.and.flon<=flon2)
              !glerlarea=glerlarea.or.((flat>=slat1.and.flat<=slat2).and.(flon>=slon1.and.flon<=slon2))

              if(isli2(i,j) == 1 .or. .not.glerlarea) then
                 do k=1,nsig
                    u(i,j,k)=uland(i,j,k)
                    v(i,j,k)=vland(i,j,k)
                 enddo
               else
                 do k=1,nsig
                    u(i,j,k)=uwter(i,j,k)
                    v(i,j,k)=vwter(i,j,k)
                 enddo
              end if
           enddo
        enddo
  endif
end subroutine landlake_uvmerge
!===========================================================================
!===========================================================================
subroutine get_fldstd(field,i1,i2,j1,j2,k1,k2,radius,npass,mype)
!$$$  subprogram documentation block
!                .      .    .                                       
! subprogram:    get_fldstd
!   prgmmr: pondeca          org: np22                date: 2017-04-17
!
! abstract:
! 
!
! program history log:
!   2017-04-17 pondeca
!
!   input argument list:
!     field     - input field
!     i1,i2     - i-dimensions of field (south->north direction)
!     j1,j2     - j-dimensions of field (west->east direction)
!     k1,k2     - k-dimensions of field
!     radius    - radius in meters for variance estimation
!
!   output argument list:
!     field     - output variance
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_single,r_kind,r_double,i_kind
  use gridmod, only: region_dx,region_dy
  use anisofilter, only: smther_one
  implicit none

! Declare passed variables
  integer(i_kind) ,intent(in) :: i1,i2,j1,j2,k1,k2,npass
  integer(i_kind) ,intent(in) :: mype
  real(r_kind)  ,intent(in) :: radius
  real(r_single),dimension(i1:i2,j1:j2,k1:k2),intent(inout) :: field

! Declare local variables
  integer(i_kind) i,j,k
  integer(i_kind) ijdel,m,n,ncount
  real(r_double) f0,var,stdmean
  real(r_kind) ds
  real(r_single), allocatable, dimension(:,:,:):: fieldaux

  allocate (fieldaux(i1:i2,j1:j2,k1:k2))

  ds=max(maxval(region_dx),maxval(region_dy)) !min(minval(region_dx),minval(region_dy))
  ijdel=nint(radius/ds)
  if (mype==0) print*,'in get_fldstd: radius,ds,ijdel=',radius,ds,ijdel

  do k=k1,k2
     do j=j1,j2
        do i=i1,i2
           fieldaux(i,j,k)=field(i,j,k)
        enddo
     enddo
  enddo
  do k=k1,k2
     do j=j1,j2
        do i=i1,i2
           f0=real(fieldaux(i,j,k),kind=r_double)
           var=0._r_double
           ncount=0
           do n=max(j1,j-ijdel),min(j2,j+ijdel)
              do m=max(i1,i-ijdel),min(i2,i+ijdel)
                 if ( ((i-m)*(i-m) + (j-n)*(j-n)) <= (ijdel*ijdel) )  then
                    ncount=ncount+1
                    var=var+(real(fieldaux(m,n,k),kind=r_double)-f0)* &
                      (real(fieldaux(m,n,k),kind=r_double)-f0)
                 endif
              enddo
           enddo
           field(i,j,k)=sqrt(var/real(ncount,kind=r_double))
        enddo
     enddo
     call smther_one(field(:,:,k),i1,i2,j1,j2,npass)
  enddo

!==> for convenience, renormalize by mean std and take log (1+std)

  ncount=(j2-j1+1)*(i2-i1+1)

  do k=k1,k2
     stdmean=0._r_double
     do j=j1,j2
        do i=i1,i2
           stdmean=stdmean+real(field(i,j,k),kind=r_double)
        enddo
     enddo
     stdmean=stdmean/real(ncount,kind=r_double)
     if (stdmean > 0._r_double) field(:,:,k)=field(:,:,k)/stdmean
     field(:,:,k)=log(1._r_single+field(:,:,k))
  enddo
  
  deallocate (fieldaux)
end subroutine get_fldstd
!===========================================================================
!===========================================================================
