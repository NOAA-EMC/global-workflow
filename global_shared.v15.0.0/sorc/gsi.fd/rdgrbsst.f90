subroutine rdgrbsst(file_sst,mlat_sst,mlon_sst,&
     sst_an,rlats_sst,rlons_sst,nlat_sst,nlon_sst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rdgrbsst                   read SST analysis
!   prgmmr: xu li            org: np23                date: 2003-04-04
!
! abstract: read SST analysis (GRIB format) and save it as expanded and transposed array
!
!     Subroutine rdgrbsst must be compiled with the NCEP W3 library
!     and the BACIO library.
!
!
! program history log:
!   2003-04-04  xu li, bert katz
!   2005-04-18  treadon - fill southern and northern rows of sst grid
!                         with mean of adjacent row.  This treatment is
!                         consistent with rdgesfc.f90 and rdgesig.f90
!
!   input argument list:
!     file_sst - file name of GRIB SST file
!     mlat_sst,mlon_sst
!
!   argument list defined by this reading:
!     sst_an - SST field (for 0.5 x 0.5 resolution)
!             sst_an(1,1)     is at 90.00 deg. S,  0.25 deg. W (-90.00,-0.25)
!             sst_an(1,2)     is at 90.00 deg. S,  0.25 deg. E (-90.00,+0.25)
!             sst_an(2,1)     is at 89.75 deg. S,  0.25 deg. W (-89.75,-0.25)
!             sst_an(2,2)     is at 89.75 deg. S,  0.25 deg. E (-89.75,+0.25)
!             sst_an(361,721) is at 89.75 deg. N,  0.25 deg. W (+89.75,359.75)
!             sst_an(361,722) is at 89.75 deg. N,  0.25 deg. E (+89.75,360.25)
!             sst_an(362,721) is at 90.00 deg. N,  0.25 deg. W (+90.00,359.75)
!             sst_an(362,722) is at 90.00 deg. N,  0.25 deg. E (+90.00,360.25)
!   Note: (1) The data is stored from north to south originally in GRIB format,
!             but is stored from south to north with this reading routine
!         (2) Two poles are added and their values are extrapolated in meridional direction
!         (3) Two ends are added and their values (periodic) in zonal direction
!         (4) The output (sst_an) dimension is changed as opposite order, e.g. (for 0.5 x 0.5)
!             (722,362) ==> (362, 722)
!     nlat_sst  - latitudinal dimension of SST
!     nlon_sst  - longitudinal dimension of SST
!     xsst0 - latitude of origin
!     ysst0 - longitude of origin
!     dres  - lat/lon increment
!
!     call subs: getgbh, getgb
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: h1000,half,deg2rad,zero
  implicit none

! Declare passed variables and arrays
  character(6)                             ,intent(in   ) :: file_sst
  integer(i_kind)                          ,intent(in   ) :: mlat_sst,mlon_sst
  integer(i_kind)                          ,intent(  out) :: nlat_sst,nlon_sst
  real(r_kind),dimension(mlat_sst)         ,intent(  out) :: rlats_sst
  real(r_kind),dimension(mlon_sst)         ,intent(  out) :: rlons_sst
  real(r_kind),dimension(mlat_sst,mlon_sst),intent(  out) :: sst_an

! Declare local parameters
  integer(i_kind),parameter:: lu_sst = 21   ! FORTRAN unit number of GRIB SST file

! Declare local variables and arrays
  logical(1), allocatable, dimension(:) ::  lb

  integer(i_kind) iret,ni,nj
  integer(i_kind) iyrstmp,imstmp,idstmp,mscan,kb1
  integer(i_kind) jincdir,i,iincdir,kb2,kb3,kf,kg,k,j,jf
  integer(i_kind),dimension(22):: jgds,kgds
  integer(i_kind),dimension(25):: jpds,kpds

  real(r_kind) xsst0,ysst0,dres,sums,sumn
  real(r_kind), allocatable, dimension(:) :: f
  real(r_kind), allocatable,dimension(:,:) :: sst   ! SST analysis (RTG or others), for read
  
!******************************************************************************************
!
! Open SST analysis file (GRIB)
  call baopenr(lu_sst,trim(file_sst),iret)
  if (iret /= 0 ) then
     write(6,*)'RDGRBSST:  ***ERROR*** opening SST file'
     call stop2(59)
  endif


! Define SST variables for read
  j=-1
  jpds=-1
  jgds=-1
  jpds(5)=11        ! SST variable
  jpds(6)=1      ! surface
  call getgbh(lu_sst,0,j,jpds,jgds,kg,kf,k,kpds,kgds,iret)

  nlat_sst = kgds(3)   ! number points on longitude circle (360)
  nlon_sst = kgds(2)   ! number points on latitude circle (720)


! Allocate arrays
  allocate(lb(nlat_sst*nlon_sst))
  allocate(f(nlat_sst*nlon_sst))
  jf=nlat_sst*nlon_sst

! Read in the analysis
  call getgb(lu_sst,0,jf,j,jpds,jgds,kf,k,kpds,kgds,lb,f,iret)
  if (iret /= 0) then
     write(6,*)'RDGRBSST:  ***ERROR*** reading sst analysis data record'
     deallocate(lb,f)
     call stop2(59)
  endif

! Allocate SST analysis and the arrays for its dimensions
  nlat_sst = nlat_sst + 2          ! Add two poles (90S & 90N)
  nlon_sst = nlon_sst + 2          ! Add two buffer ends in zonal direction (-0.25 & 0.25)

  if ( (nlat_sst>mlat_sst) .or. (nlon_sst>mlon_sst) ) then
     write(6,*)'RDGRBSST:  inconsistent dimensions.  mlat_sst,mlon_sst=',&
          mlat_sst,mlon_sst,' -versus- nlat_sst,nlon_sst=',nlat_sst,nlon_sst
     deallocate(lb,f)
     call stop2(60)
  endif

  allocate(sst(nlon_sst,nlat_sst))

  xsst0 = -real(kgds(4))/h1000        ! latitude of origin
  ysst0 =  real(kgds(5))/h1000        ! longitude of origin
  dres  =  real(kgds(9))/h1000        ! increment of latitude/longitude (the same here)

! Get lat_sst & lon_sst
  do i = 2, nlat_sst - 1
     rlats_sst(i) = (xsst0 + float(i-2)*dres)*deg2rad
  enddo

  rlats_sst(1)        = -90.0_r_kind*deg2rad
  rlats_sst(nlat_sst) =  90.0_r_kind*deg2rad
  
  do j = 2, nlon_sst - 1
     rlons_sst(j) = (ysst0 + float(j-2)*dres)*deg2rad
  enddo

  rlons_sst(1)        = -half*dres*deg2rad                              ! 1
  rlons_sst(nlon_sst) =  (360._r_kind+half*dres)*deg2rad                ! 722

! Load dimensions and grid specs.  Check for unusual values
  ni=kgds(2)                ! 720 for 0.5 x 0.5
  nj=kgds(3)                ! 360 for 0.5 x 0.5 resolution

  mscan=kgds(11)
  kb1=ibits(mscan,7,1)   ! i scan direction
  kb2=ibits(mscan,6,1)   ! j scan direction
  kb3=ibits(mscan,5,1)   ! (i,j) or (j,i)

! Get i and j scanning directions from kb1 and kb2.
! 0 yields +1, 1 yields -1. +1 is west to east, -1 is east to west.
  iincdir = 1-kb1*2

! 0 yields -1, 1 yields +1. +1 is south to north, -1 is north to south.
  jincdir = kb2*2 - 1
  do k=1,kf

!    kb3 from scan mode indicates if i points are consecutive
!    or if j points are consecutive
     if(kb3==0)then     !  (i,j)
        i=(ni+1)*kb1+(mod(k-1,ni)+1)*iincdir
        j=(nj+1)*(1-kb2)+jincdir*((k-1)/ni+1)
     else                !  (j,i)
        j=(nj+1)*(1-kb2)+(mod(k-1,nj)+1)*jincdir
        i=(ni+1)*kb1+iincdir*((k-1)/nj+1)
     endif
     sst(i+1,j+1)=f(k)
  end do

! Set sst value at 90 S (1) & 90 N (362 = nlon_sst) to be mean of 
! adjacent row
  i = 0
  sums=zero
  sumn=zero
  do j = 2, nlon_sst - 1
     i    = i + 1
     sums = sums + sst(j,2)
     sumn = sumn + sst(j,nlat_sst-1)
  end do
  sums = sums / float(i)
  sumn = sumn / float(i)
  do j = 2,nlon_sst-1
     sst(j,1)        = sums
     sst(j,nlat_sst) = sumn
  enddo
     
! Get sst value for added two ends in zonal direction
  do i = 1, nlat_sst
     sst(1,i) = sst(nlon_sst-1,i)
     sst(nlon_sst,i) = sst(2,i)
  enddo
     
! Transpose sst to sst_an (output)
  do j = 1, nlon_sst
     do i = 1, nlat_sst
        sst_an(i,j) = sst(j,i)
     enddo
  enddo
     
  iyrstmp=kpds(8) + (kpds(21)-1)*100
  imstmp=kpds(9)
  idstmp=kpds(10)
  deallocate(lb,f)
  return
  
end subroutine rdgrbsst
