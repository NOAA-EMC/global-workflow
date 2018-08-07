!------------------------------------------------------------------
! 
! Read in surface and nst data on the cubed-sphere grid,
! interpolate it to the gaussian grid, and output the result
! to a nemsio file.  The output file mimics those produced by
! the legacy spectral GFS system.  To not process nst
! data, set flag 'donst' to 'no'.  To process nst, set to 'yes'.
!
! Input files:
! ------------
! weights.nc              Interpolation weights.  netcdf format
! anal.tile[1-6].nc       fv3 surface restart files
! orog.tile[1-6].nc       fv3 orography files
! fort.41 namelist        Configuration namelist
! vcoord.txt              Vertical coordinate definition file
!                         (ascii)
!
! Output files:
! -------------
! sfc.gaussian.nemsio     surface data on gaussian grid - nemsio
!
! Namelist variables:
! -------------------
! yy/mm/dd/hh             year/month/day/hour of data.
! i/jgaus                 i/j dimension of gaussian grid.
! donst                   When 'no' do not process nst data.
!                         When 'yes' process nst data.
!
! 2018-Jan-30 Gayno       Initial version
!
!------------------------------------------------------------------

 module io

 use nemsio_module

 implicit none

 character(len=3)   :: donst

 integer, parameter :: num_tiles = 6

 integer :: itile, jtile, igaus, jgaus

 integer(nemsio_intkind) :: idate(8)

 type :: sfc_data
! surface variables
   real, allocatable :: alvsf(:)
   real, allocatable :: alvwf(:)
   real, allocatable :: alnsf(:)
   real, allocatable :: alnwf(:)
   real, allocatable :: canopy(:)
   real, allocatable :: facsf(:)
   real, allocatable :: facwf(:)
   real, allocatable :: ffhh(:)
   real, allocatable :: ffmm(:)
   real, allocatable :: fice(:)
   real, allocatable :: f10m(:)
   real, allocatable :: hice(:)
   real, allocatable :: q2m(:)
   real, allocatable :: orog(:)
   real, allocatable :: sheleg(:)
   real, allocatable :: slmask(:)
   real, allocatable :: shdmax(:)
   real, allocatable :: shdmin(:)
   real, allocatable :: slope(:)
   real, allocatable :: srflag(:)
   real, allocatable :: snoalb(:)
   real, allocatable :: snwdph(:)
   real, allocatable :: stype(:)
   real, allocatable :: t2m(:)
   real, allocatable :: tprcp(:)
   real, allocatable :: tisfc(:)
   real, allocatable :: tsea(:)
   real, allocatable :: tg3(:)
   real, allocatable :: uustar(:)
   real, allocatable :: vfrac(:)
   real, allocatable :: vtype(:)
   real, allocatable :: zorl(:)
   real, allocatable :: slc(:,:)
   real, allocatable :: smc(:,:)
   real, allocatable :: stc(:,:)
! nst variables
   real, allocatable :: c0(:)
   real, allocatable :: cd(:)
   real, allocatable :: dconv(:)
   real, allocatable :: dtcool(:)
   real, allocatable :: land(:)
   real, allocatable :: qrain(:)
   real, allocatable :: tref(:)
   real, allocatable :: w0(:)
   real, allocatable :: wd(:)
   real, allocatable :: xs(:)
   real, allocatable :: xt(:)
   real, allocatable :: xtts(:)
   real, allocatable :: xu(:)
   real, allocatable :: xv(:)
   real, allocatable :: xz(:)
   real, allocatable :: xzts(:)
   real, allocatable :: zc(:)
 end type sfc_data
 
 type(sfc_data) :: tile_data, gaussian_data

 end module io

 program main

 use netcdf
 use io

 implicit none

 character(len=12)       :: weightfile

 integer                 :: i, error, ncid, id_ns, n_s
 integer                 :: id_col, id_row, id_s, n
 integer                 :: yy, mm, dd, hh
 integer, allocatable    :: col(:), row(:)

 real(kind=8), allocatable :: s(:)

 namelist /setup/ yy, mm, dd, hh, igaus, jgaus, donst

 call w3tagb('GAUSSIAN_SFCANL',2018,0179,0055,'NP20')

 print*,"- BEGIN EXECUTION"

 donst = 'no'

 print*
 print*,"- READ SETUP NAMELIST"
 open(41, file="./fort.41")
 read(41, nml=setup, iostat=error)
 if (error /= 0) then
   print*,"** FATAL ERROR READING NAMELIST. ISTAT IS: ", error
   call errexit(56)
 endif
 close (41)

 idate = 0
 idate(1) = yy
 idate(2) = mm
 idate(3) = dd
 idate(4) = hh

!------------------------------------------------------------------------------
! Read interpolation weight file.
!------------------------------------------------------------------------------

 print*
 print*,"- READ INTERPOLATION WEIGHT FILE"

 weightfile = "./weights.nc"

 error=nf90_open(trim(weightfile),nf90_nowrite,ncid)
 call netcdf_err(error, 'OPENING weights.nc' )

 error=nf90_inq_dimid(ncid, 'n_s', id_ns)
 call netcdf_err(error, 'READING n_s id' )
 error=nf90_inquire_dimension(ncid,id_ns,len=n_s)
 call netcdf_err(error, 'READING n_s' )

 allocate(col(n_s))
 error=nf90_inq_varid(ncid, 'col', id_col)
 call netcdf_err(error, 'READING col id' )
 error=nf90_get_var(ncid, id_col, col)
 call netcdf_err(error, 'READING col' )

 allocate(row(n_s))
 error=nf90_inq_varid(ncid, 'row', id_row)
 call netcdf_err(error, 'READING row id' )
 error=nf90_get_var(ncid, id_row, row)
 call netcdf_err(error, 'READING row' )

 allocate(s(n_s))
 error=nf90_inq_varid(ncid, 'S', id_s)
 call netcdf_err(error, 'READING s id' )
 error=nf90_get_var(ncid, id_s, s)
 call netcdf_err(error, 'READING s' )

 error = nf90_close(ncid)

!------------------------------------------------------------------------------
! Read the tiled analysis data.
!------------------------------------------------------------------------------

 call read_data_anl

!------------------------------------------------------------------------------
! Interpolate tiled data to gaussian grid.
!------------------------------------------------------------------------------

 allocate(gaussian_data%orog(igaus*jgaus))    ! sfc
 allocate(gaussian_data%t2m(igaus*jgaus))
 allocate(gaussian_data%tisfc(igaus*jgaus))
 allocate(gaussian_data%q2m(igaus*jgaus))
 allocate(gaussian_data%stype(igaus*jgaus))
 allocate(gaussian_data%snwdph(igaus*jgaus))
 allocate(gaussian_data%slope(igaus*jgaus))
 allocate(gaussian_data%shdmax(igaus*jgaus))
 allocate(gaussian_data%shdmin(igaus*jgaus))
 allocate(gaussian_data%snoalb(igaus*jgaus))
 allocate(gaussian_data%slmask(igaus*jgaus))
 allocate(gaussian_data%tg3(igaus*jgaus))
 allocate(gaussian_data%alvsf(igaus*jgaus))
 allocate(gaussian_data%alvwf(igaus*jgaus))
 allocate(gaussian_data%alnsf(igaus*jgaus))
 allocate(gaussian_data%alnwf(igaus*jgaus))
 allocate(gaussian_data%facsf(igaus*jgaus))
 allocate(gaussian_data%facwf(igaus*jgaus))
 allocate(gaussian_data%ffhh(igaus*jgaus))
 allocate(gaussian_data%ffmm(igaus*jgaus))
 allocate(gaussian_data%sheleg(igaus*jgaus))
 allocate(gaussian_data%canopy(igaus*jgaus))
 allocate(gaussian_data%vfrac(igaus*jgaus))
 allocate(gaussian_data%vtype(igaus*jgaus))
 allocate(gaussian_data%zorl(igaus*jgaus))
 allocate(gaussian_data%tsea(igaus*jgaus))
 allocate(gaussian_data%f10m(igaus*jgaus))
 allocate(gaussian_data%tprcp(igaus*jgaus))
 allocate(gaussian_data%uustar(igaus*jgaus))
 allocate(gaussian_data%fice(igaus*jgaus))
 allocate(gaussian_data%hice(igaus*jgaus))
 allocate(gaussian_data%srflag(igaus*jgaus))
 allocate(gaussian_data%slc(igaus*jgaus,4))
 allocate(gaussian_data%smc(igaus*jgaus,4))
 allocate(gaussian_data%stc(igaus*jgaus,4))

 if (trim(donst) == "yes" .or. trim(donst) == "YES") then
   allocate(gaussian_data%c0(igaus*jgaus))  ! nst
   allocate(gaussian_data%cd(igaus*jgaus))  
   allocate(gaussian_data%dconv(igaus*jgaus))  
   allocate(gaussian_data%dtcool(igaus*jgaus)) 
   allocate(gaussian_data%land(igaus*jgaus)) 
   allocate(gaussian_data%qrain(igaus*jgaus)) 
   allocate(gaussian_data%tref(igaus*jgaus)) 
   allocate(gaussian_data%w0(igaus*jgaus)) 
   allocate(gaussian_data%wd(igaus*jgaus)) 
   allocate(gaussian_data%xs(igaus*jgaus)) 
   allocate(gaussian_data%xt(igaus*jgaus)) 
   allocate(gaussian_data%xtts(igaus*jgaus)) 
   allocate(gaussian_data%xu(igaus*jgaus)) 
   allocate(gaussian_data%xv(igaus*jgaus)) 
   allocate(gaussian_data%xz(igaus*jgaus)) 
   allocate(gaussian_data%xzts(igaus*jgaus)) 
   allocate(gaussian_data%zc(igaus*jgaus)) 
 endif

 do i = 1, n_s
   gaussian_data%orog(row(i))   = gaussian_data%orog(row(i)) + s(i)*tile_data%orog(col(i))
   gaussian_data%t2m(row(i))    = gaussian_data%t2m(row(i)) + s(i)*tile_data%t2m(col(i))
   gaussian_data%tisfc(row(i))  = gaussian_data%tisfc(row(i)) + s(i)*tile_data%tisfc(col(i))
   gaussian_data%q2m(row(i))    = gaussian_data%q2m(row(i)) + s(i)*tile_data%q2m(col(i))
   gaussian_data%stype(row(i))  = gaussian_data%stype(row(i)) + s(i)*tile_data%stype(col(i))
   gaussian_data%snwdph(row(i)) = gaussian_data%snwdph(row(i)) + s(i)*tile_data%snwdph(col(i))
   gaussian_data%slope(row(i))  = gaussian_data%slope(row(i)) + s(i)*tile_data%slope(col(i))
   gaussian_data%shdmax(row(i)) = gaussian_data%shdmax(row(i)) + s(i)*tile_data%shdmax(col(i))
   gaussian_data%shdmin(row(i)) = gaussian_data%shdmin(row(i)) + s(i)*tile_data%shdmin(col(i))
   gaussian_data%slmask(row(i)) = gaussian_data%slmask(row(i)) + s(i)*tile_data%slmask(col(i))
   gaussian_data%tg3(row(i))    = gaussian_data%tg3(row(i)) + s(i)*tile_data%tg3(col(i))
   gaussian_data%alvsf(row(i))  = gaussian_data%alvsf(row(i)) + s(i)*tile_data%alvsf(col(i))
   gaussian_data%alvwf(row(i))  = gaussian_data%alvwf(row(i)) + s(i)*tile_data%alvwf(col(i))
   gaussian_data%alnsf(row(i))  = gaussian_data%alnsf(row(i)) + s(i)*tile_data%alnsf(col(i))
   gaussian_data%alnwf(row(i))  = gaussian_data%alnwf(row(i)) + s(i)*tile_data%alnwf(col(i))
   gaussian_data%sheleg(row(i)) = gaussian_data%sheleg(row(i)) + s(i)*tile_data%sheleg(col(i))
   gaussian_data%canopy(row(i)) = gaussian_data%canopy(row(i)) + s(i)*tile_data%canopy(col(i))
   gaussian_data%vfrac(row(i))  = gaussian_data%vfrac(row(i)) + s(i)*tile_data%vfrac(col(i))
   gaussian_data%zorl(row(i))   = gaussian_data%zorl(row(i)) + s(i)*tile_data%zorl(col(i))
   gaussian_data%tsea(row(i))   = gaussian_data%tsea(row(i)) + s(i)*tile_data%tsea(col(i))
   gaussian_data%f10m(row(i))   = gaussian_data%f10m(row(i)) + s(i)*tile_data%f10m(col(i))
   gaussian_data%vtype(row(i))  = gaussian_data%vtype(row(i)) + s(i)*tile_data%vtype(col(i))
   gaussian_data%tprcp(row(i))  = gaussian_data%tprcp(row(i)) + s(i)*tile_data%tprcp(col(i))
   gaussian_data%facsf(row(i))  = gaussian_data%facsf(row(i)) + s(i)*tile_data%facsf(col(i))
   gaussian_data%facwf(row(i))  = gaussian_data%facwf(row(i)) + s(i)*tile_data%facwf(col(i))
   gaussian_data%ffhh(row(i))   = gaussian_data%ffhh(row(i)) + s(i)*tile_data%ffhh(col(i))
   gaussian_data%ffmm(row(i))   = gaussian_data%ffmm(row(i)) + s(i)*tile_data%ffmm(col(i))
   gaussian_data%uustar(row(i)) = gaussian_data%uustar(row(i)) + s(i)*tile_data%uustar(col(i))
   gaussian_data%fice(row(i))   = gaussian_data%fice(row(i)) + s(i)*tile_data%fice(col(i))
   gaussian_data%hice(row(i))   = gaussian_data%hice(row(i)) + s(i)*tile_data%hice(col(i))
   gaussian_data%snoalb(row(i)) = gaussian_data%snoalb(row(i)) + s(i)*tile_data%snoalb(col(i))
   gaussian_data%srflag(row(i)) = gaussian_data%srflag(row(i)) + s(i)*tile_data%srflag(col(i))
   if (trim(donst) == "yes" .or. trim(donst) == "YES") then
     gaussian_data%c0(row(i))     = gaussian_data%c0(row(i)) + s(i)*tile_data%c0(col(i))
     gaussian_data%cd(row(i))     = gaussian_data%cd(row(i)) + s(i)*tile_data%cd(col(i))
     gaussian_data%dconv(row(i))  = gaussian_data%dconv(row(i)) + s(i)*tile_data%dconv(col(i))
     gaussian_data%dtcool(row(i)) = gaussian_data%dtcool(row(i)) + s(i)*tile_data%dtcool(col(i))
     gaussian_data%qrain(row(i))  = gaussian_data%qrain(row(i)) + s(i)*tile_data%qrain(col(i))
     gaussian_data%tref(row(i))   = gaussian_data%tref(row(i)) + s(i)*tile_data%tref(col(i))
     gaussian_data%w0(row(i))     = gaussian_data%w0(row(i)) + s(i)*tile_data%w0(col(i))
     gaussian_data%wd(row(i))     = gaussian_data%wd(row(i)) + s(i)*tile_data%wd(col(i))
     gaussian_data%xs(row(i))     = gaussian_data%xs(row(i)) + s(i)*tile_data%xs(col(i))
     gaussian_data%xt(row(i))     = gaussian_data%xt(row(i)) + s(i)*tile_data%xt(col(i))
     gaussian_data%xtts(row(i))   = gaussian_data%xtts(row(i)) + s(i)*tile_data%xtts(col(i))
     gaussian_data%xu(row(i))     = gaussian_data%xu(row(i)) + s(i)*tile_data%xu(col(i))
     gaussian_data%xv(row(i))     = gaussian_data%xv(row(i)) + s(i)*tile_data%xv(col(i))
     gaussian_data%xz(row(i))     = gaussian_data%xz(row(i)) + s(i)*tile_data%xz(col(i))
     gaussian_data%xzts(row(i))   = gaussian_data%xzts(row(i)) + s(i)*tile_data%xzts(col(i))
     gaussian_data%zc(row(i))     = gaussian_data%zc(row(i)) + s(i)*tile_data%zc(col(i))
   endif
   do n = 1, 4
     gaussian_data%slc(row(i),n) = gaussian_data%slc(row(i),n) + s(i)*tile_data%slc(col(i),n)
     gaussian_data%smc(row(i),n) = gaussian_data%smc(row(i),n) + s(i)*tile_data%smc(col(i),n)
     gaussian_data%stc(row(i),n) = gaussian_data%stc(row(i),n) + s(i)*tile_data%stc(col(i),n)
   enddo
 enddo

 deallocate(col, row, s)

 deallocate(tile_data%orog)
 deallocate(tile_data%t2m)
 deallocate(tile_data%tisfc)
 deallocate(tile_data%q2m)
 deallocate(tile_data%stype)
 deallocate(tile_data%snwdph)
 deallocate(tile_data%slope)
 deallocate(tile_data%shdmax)
 deallocate(tile_data%shdmin)
 deallocate(tile_data%snoalb)
 deallocate(tile_data%slmask)
 deallocate(tile_data%tg3)
 deallocate(tile_data%alvsf)
 deallocate(tile_data%alvwf)
 deallocate(tile_data%alnsf)
 deallocate(tile_data%alnwf)
 deallocate(tile_data%facsf)
 deallocate(tile_data%facwf)
 deallocate(tile_data%ffhh)
 deallocate(tile_data%ffmm)
 deallocate(tile_data%sheleg)
 deallocate(tile_data%canopy)
 deallocate(tile_data%vfrac)
 deallocate(tile_data%vtype)
 deallocate(tile_data%zorl)
 deallocate(tile_data%tsea)
 deallocate(tile_data%f10m)
 deallocate(tile_data%tprcp)
 deallocate(tile_data%uustar)
 deallocate(tile_data%fice)
 deallocate(tile_data%hice)
 deallocate(tile_data%srflag)
 deallocate(tile_data%slc)
 deallocate(tile_data%smc)
 deallocate(tile_data%stc)

 if (trim(donst) == "yes" .or. trim(donst) == "YES") then
   deallocate(tile_data%c0)
   deallocate(tile_data%cd)
   deallocate(tile_data%dconv)
   deallocate(tile_data%dtcool)
   deallocate(tile_data%qrain)
   deallocate(tile_data%tref)
   deallocate(tile_data%w0)
   deallocate(tile_data%wd)
   deallocate(tile_data%xs)
   deallocate(tile_data%xt)
   deallocate(tile_data%xtts)
   deallocate(tile_data%xu)
   deallocate(tile_data%xv)
   deallocate(tile_data%xz)
   deallocate(tile_data%xzts)
   deallocate(tile_data%zc)
 endif

!------------------------------------------------------------------------------
! Write gaussian data to nemsio file.
!------------------------------------------------------------------------------

 call write_sfc_data

 deallocate(gaussian_data%orog)
 deallocate(gaussian_data%t2m)
 deallocate(gaussian_data%tisfc)
 deallocate(gaussian_data%q2m)
 deallocate(gaussian_data%stype)
 deallocate(gaussian_data%snwdph)
 deallocate(gaussian_data%slope)
 deallocate(gaussian_data%shdmax)
 deallocate(gaussian_data%shdmin)
 deallocate(gaussian_data%snoalb)
 deallocate(gaussian_data%slmask)
 deallocate(gaussian_data%tg3)
 deallocate(gaussian_data%alvsf)
 deallocate(gaussian_data%alvwf)
 deallocate(gaussian_data%alnsf)
 deallocate(gaussian_data%alnwf)
 deallocate(gaussian_data%facsf)
 deallocate(gaussian_data%facwf)
 deallocate(gaussian_data%ffhh)
 deallocate(gaussian_data%ffmm)
 deallocate(gaussian_data%sheleg)
 deallocate(gaussian_data%canopy)
 deallocate(gaussian_data%vfrac)
 deallocate(gaussian_data%vtype)
 deallocate(gaussian_data%zorl)
 deallocate(gaussian_data%tsea)
 deallocate(gaussian_data%f10m)
 deallocate(gaussian_data%tprcp)
 deallocate(gaussian_data%uustar)
 deallocate(gaussian_data%fice)
 deallocate(gaussian_data%hice)
 deallocate(gaussian_data%srflag)
 deallocate(gaussian_data%slc)
 deallocate(gaussian_data%smc)
 deallocate(gaussian_data%stc)

 if (trim(donst) == "yes" .or. trim(donst) == "YES") then
   deallocate(gaussian_data%c0)
   deallocate(gaussian_data%cd)
   deallocate(gaussian_data%dconv)
   deallocate(gaussian_data%dtcool)
   deallocate(gaussian_data%land)
   deallocate(gaussian_data%qrain)
   deallocate(gaussian_data%tref)
   deallocate(gaussian_data%w0)
   deallocate(gaussian_data%wd)
   deallocate(gaussian_data%xs)
   deallocate(gaussian_data%xt)
   deallocate(gaussian_data%xtts)
   deallocate(gaussian_data%xu)
   deallocate(gaussian_data%xv)
   deallocate(gaussian_data%xz)
   deallocate(gaussian_data%xzts)
   deallocate(gaussian_data%zc)
 endif

 print*
 print*,'- NORMAL INTERPOLATION'

 call w3tage('GAUSSIAN_SFCANL')

 end program main

!-------------------------------------------------------------------------------------------
! Write gaussian surface data to nemsio file.
!-------------------------------------------------------------------------------------------

 subroutine write_sfc_data

 use nemsio_module
 use io

 implicit none

 integer(nemsio_intkind), parameter :: nrec_all=60
 integer(nemsio_intkind), parameter :: nmetaaryi=1
 integer(nemsio_intkind), parameter :: nmetavari=4
 integer(nemsio_intkind), parameter :: nmetavarr=1
 integer(nemsio_intkind), parameter :: nmetavarc=2

 character(nemsio_charkind)         :: recname_all(nrec_all)
 character(nemsio_charkind)         :: reclevtyp_all(nrec_all)
 character(nemsio_charkind)         :: aryiname(nmetaaryi)
 character(nemsio_charkind)         :: variname(nmetavari)
 character(nemsio_charkind)         :: varrname(nmetavarr)
 character(nemsio_charkind)         :: varcname(nmetavarc)
 character(nemsio_charkind)         :: varcval(nmetavarc)
 character(nemsio_charkind), allocatable :: recname(:)
 character(nemsio_charkind), allocatable :: reclevtyp(:)

 integer(nemsio_intkind)            :: iret, version, nrec
 integer(nemsio_intkind)            :: reclev_all(nrec_all)
 integer(nemsio_intkind)            :: aryival(jgaus,nmetaaryi)
 integer(nemsio_intkind)            :: aryilen(nmetaaryi)
 integer(nemsio_intkind)            :: varival(nmetavari)
 integer                            :: i, k, n, nvcoord, levs_vcoord
 integer(nemsio_intkind), allocatable  :: reclev(:)
 
 real(nemsio_realkind), allocatable :: the_data(:)
 real(nemsio_realkind)              :: varrval(nmetavarr)
 real(nemsio_realkind), allocatable :: lat(:), lon(:)
 real(kind=4), allocatable          :: dummy(:,:), slat(:), wlat(:)
 real(nemsio_realkind), allocatable :: vcoord(:,:,:)

 type(nemsio_gfile)                 :: gfileo

 data recname_all /'alnsf', 'alnwf', 'alvsf', 'alvwf', &
               'cnwat', 'crain', 'f10m',  'facsf', &
               'facwf', 'ffhh',  'ffmm',  'fricv', &
               'icec',  'icetk', 'land',  'orog', &
               'snoalb', 'sfcr',  'shdmax', 'shdmin', &
               'soill',   'soill',   'soill',    'soill',  &
               'sltyp', 'soilw',   'soilw',    'soilw', &
               'soilw',   'snod',  'sotyp',  'spfh', &
               'tmp',   'tmp',   'tmp',  'tmp',  &
               'tg3',   'ti', 'tmp',    'tmp', &
               'tprcp', 'veg',   'vtype',  'weasd', &
               'c0', 'cd', 'dconv', 'dtcool', &
               'qrain',  'tref', &
               'w0', 'wd',  'xs',  'xt', &
               'xtts',  'xu', 'xv',  'xz', &
               'xzts', 'zc'/

 data reclevtyp_all /'sfc',   'sfc',   'sfc',   'sfc', &
                 'sfc',   'sfc', '10 m above gnd',   'sfc', &
                 'sfc',   'sfc',   'sfc',   'sfc', &
                 'sfc',   'sfc',   'sfc',   'sfc', &
                 'sfc',   'sfc',   'sfc',   'sfc', &
                 '0-10 cm down', '10-40 cm down', '40-100 cm down', '100-200 cm down', &
                 'sfc', '0-10 cm down', '10-40 cm down', '40-100 cm down', &
                 '100-200 cm down', 'sfc', 'sfc',  '2 m above gnd', &
                 '0-10 cm down', '10-40 cm down', '40-100 cm down', '100-200 cm down', &
                 'sfc',   'sfc',   '2 m above gnd',   'sfc', &
                 'sfc',   'sfc',   'sfc',   'sfc', & 
                 'sfc',   'sfc',   'sfc',   'sfc', & 
                 'sfc',   'sfc',   'sfc', & 
                 'sfc',   'sfc',   'sfc',   'sfc', & 
                 'sfc',   'sfc',   'sfc',   'sfc', & 
                 'sfc'/

 data reclev_all /1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, &
              1, 1, 1, 1, 1, 1, 1/
        
 data aryiname /'lpl'/

 data variname /'fhzero', 'ncld', 'nsoil', 'imp_physics'/

 data varival /6, 5, 4, 11/

 data varrname /'dtp'/

 data varrval /225.0/

 data varcname /"y-direction", "z-direction"/

 data varcval /"north2south", "bottom2top"/

 version  = 200809

 aryival = igaus    ! reduced grid definition
 aryilen = jgaus

 allocate(dummy(igaus,jgaus))
 do i = 1, igaus
   dummy(i,:) = float(i-1) * 360.0 / float(igaus)
 enddo

 allocate(lon(igaus*jgaus))
 lon = reshape (dummy, (/igaus*jgaus/) )

! Call 4-byte version of splib to match latitudes in history files.

 allocate(slat(jgaus))
 allocate(wlat(jgaus))
 call splat(4, jgaus, slat, wlat)

 do i = (jgaus/2+1), jgaus
   dummy(:,i) = 90.0 - (acos(slat(i)) * 180.0 / (4.0*atan(1.0)))
 enddo

 do i = 1, (jgaus/2)
   dummy(:,i) = -(dummy(:,(jgaus-i+1)))
 enddo

 deallocate(slat, wlat)

 allocate(lat(igaus*jgaus))
 lat = reshape (dummy, (/igaus*jgaus/) )

 deallocate(dummy)

 print*
 print*, "- OPEN VCOORD FILE."
 open(14, file="vcoord.txt", form='formatted', iostat=iret)
 if (iret /= 0) goto 43

 print*, "- READ VCOORD FILE."
 read(14, *, iostat=iret) nvcoord, levs_vcoord
 if (iret /= 0) goto 43

 allocate(vcoord(levs_vcoord,3,2))
 vcoord = 0.0
 read(14, *, iostat=iret) ((vcoord(n,k,1), k=1,nvcoord), n=1,levs_vcoord)
 if (iret /= 0) goto 43

 close (14)

 if (trim(donst) == "yes" .or. trim(donst) == "YES") then
   nrec = nrec_all
   allocate(recname(nrec))
   recname = recname_all
   allocate(reclevtyp(nrec))
   reclevtyp = reclevtyp_all
   allocate(reclev(nrec))
   reclev = reclev_all
 else
   nrec = 44
   allocate(recname(nrec))
   recname = recname_all(1:nrec)
   allocate(reclevtyp(nrec))
   reclevtyp = reclevtyp_all(1:nrec)
   allocate(reclev(nrec))
   reclev = reclev_all(1:nrec)
 endif

 call nemsio_init(iret=iret)

 print*
 print*,"- OPEN GAUSSIAN NEMSIO SURFACE FILE"

 call nemsio_open(gfileo, "sfc.gaussian.nemsio", 'write',   &
                  modelname="FV3GFS", gdatatype="bin4", version=version,  &
                  nmeta=8, nrec=nrec, dimx=igaus, dimy=jgaus, dimz=(levs_vcoord-1),     &
                  nframe=0, nsoil=4, ntrac=8, jcap=-9999,  &
                  ncldt=5, idvc=-9999, idsl=-9999, idvm=-9999, &
                  idrt=4, lat=lat, lon=lon, vcoord=vcoord, &
                  nfhour=0, nfminute=0, nfsecondn=0,  &
                  nfsecondd=1, nfday=0, idate=idate, &
                  recname=recname, reclevtyp=reclevtyp, &
                  reclev=reclev, extrameta=.true., &
                  nmetavari=nmetavari, variname=variname, varival=varival, &
                  nmetavarr=nmetavarr, varrname=varrname, varrval=varrval, &
                  nmetavarc=nmetavarc, varcname=varcname, varcval=varcval, &
                  nmetaaryi=nmetaaryi, aryiname=aryiname, &
                  aryival=aryival, aryilen=aryilen, iret=iret)
 if (iret /= 0) goto 44

 deallocate (lat, lon, vcoord, recname, reclevtyp, reclev)

 allocate(the_data(igaus*jgaus))

 print*,"- WRITE GAUSSIAN NEMSIO SURFACE FILE"

 print*,"- WRITE ALNSF"
 the_data = gaussian_data%alnsf
 call nemsio_writerec(gfileo,  1, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE ALNWF"
 the_data = gaussian_data%alnwf
 call nemsio_writerec(gfileo,  2, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE ALVSF"
 the_data = gaussian_data%alvsf
 call nemsio_writerec(gfileo,  3, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE ALVWF"
 the_data = gaussian_data%alvwf
 call nemsio_writerec(gfileo,  4, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE CANOPY"
 the_data = gaussian_data%canopy
 call nemsio_writerec(gfileo,  5, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE CRAIN (SRFLAG)"
 the_data = gaussian_data%srflag
 call nemsio_writerec(gfileo,  6, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE F10M"
 the_data = gaussian_data%f10m
 call nemsio_writerec(gfileo, 7, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE FACSF"
 the_data = gaussian_data%facsf
 call nemsio_writerec(gfileo, 8, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE FACWF"
 the_data = gaussian_data%facwf
 call nemsio_writerec(gfileo, 9, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE FFHH"
 the_data = gaussian_data%ffhh
 call nemsio_writerec(gfileo, 10, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE FFMM"
 the_data = gaussian_data%ffmm
 call nemsio_writerec(gfileo, 11, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE UUSTAR"
 the_data = gaussian_data%uustar
 call nemsio_writerec(gfileo, 12, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE FICE"
 the_data = gaussian_data%fice
 call nemsio_writerec(gfileo, 13, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE HICE"
 the_data = gaussian_data%hice
 call nemsio_writerec(gfileo, 14, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SLMSK"
 the_data = gaussian_data%slmask
 call nemsio_writerec(gfileo, 15, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE OROG"
 the_data = gaussian_data%orog
 call nemsio_writerec(gfileo, 16, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SNOALB"
 the_data = gaussian_data%snoalb
 call nemsio_writerec(gfileo, 17, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE ZORL"
 the_data = gaussian_data%zorl * 0.01 ! meters
 call nemsio_writerec(gfileo, 18, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SHDMAX"
 the_data = gaussian_data%shdmax
 call nemsio_writerec(gfileo, 19, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SHDMIN"
 the_data = gaussian_data%shdmin
 call nemsio_writerec(gfileo, 20, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SLC"
 the_data = gaussian_data%slc(:,1)
 call nemsio_writerec(gfileo, 21, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%slc(:,2)
 call nemsio_writerec(gfileo, 22, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%slc(:,3)
 call nemsio_writerec(gfileo, 23, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%slc(:,4)
 call nemsio_writerec(gfileo, 24, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SLOPE"
 the_data = gaussian_data%slope
 call nemsio_writerec(gfileo, 25, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SMC"
 the_data = gaussian_data%smc(:,1)
 call nemsio_writerec(gfileo, 26, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%smc(:,2)
 call nemsio_writerec(gfileo, 27, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%smc(:,3)
 call nemsio_writerec(gfileo, 28, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%smc(:,4)
 call nemsio_writerec(gfileo, 29, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SNWDPH"
 the_data = gaussian_data%snwdph * 0.001 ! meters
 call nemsio_writerec(gfileo, 30, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE STYPE"
 the_data = gaussian_data%stype
 call nemsio_writerec(gfileo, 31, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE Q2M"
 the_data = gaussian_data%q2m
 call nemsio_writerec(gfileo, 32, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE STC"
 the_data = gaussian_data%stc(:,1)
 call nemsio_writerec(gfileo, 33, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%stc(:,2)
 call nemsio_writerec(gfileo, 34, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%stc(:,3)
 call nemsio_writerec(gfileo, 35, the_data, iret=iret)
 if (iret /= 0) goto 44

 the_data = gaussian_data%stc(:,4)
 call nemsio_writerec(gfileo, 36, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE TG3"
 the_data = gaussian_data%tg3
 call nemsio_writerec(gfileo, 37, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE TISFC"
 the_data = gaussian_data%tisfc
 call nemsio_writerec(gfileo, 38, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE T2M"
 the_data = gaussian_data%t2m
 call nemsio_writerec(gfileo, 39, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE TSEA"
 the_data = gaussian_data%tsea
 call nemsio_writerec(gfileo, 40, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE TPRCP"
 the_data = gaussian_data%tprcp
 call nemsio_writerec(gfileo, 41, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE VFRAC"
 the_data = gaussian_data%vfrac * 100.0 ! whole percent
 call nemsio_writerec(gfileo, 42, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE VTYPE"
 the_data = gaussian_data%vtype
 call nemsio_writerec(gfileo, 43, the_data, iret=iret)
 if (iret /= 0) goto 44

 print*,"- WRITE SHELEG"
 the_data = gaussian_data%sheleg
 call nemsio_writerec(gfileo, 44, the_data, iret=iret)
 if (iret /= 0) goto 44

 if (trim(donst) == "yes" .or. trim(donst) == "YES") then

   print*,"- WRITE C0"
   the_data = gaussian_data%c0
   call nemsio_writerec(gfileo, 45, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE CD"
   the_data = gaussian_data%cd
   call nemsio_writerec(gfileo, 46, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE DCONV"
   the_data = gaussian_data%dconv
   call nemsio_writerec(gfileo, 47, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE DTCOOL"
   the_data = gaussian_data%dtcool
   call nemsio_writerec(gfileo, 48, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE QRAIN"
   the_data = gaussian_data%qrain
   call nemsio_writerec(gfileo, 49, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE TREF"
   the_data = gaussian_data%tref
   call nemsio_writerec(gfileo, 50, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE W0"
   the_data = gaussian_data%w0
   call nemsio_writerec(gfileo, 51, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE WD"
   the_data = gaussian_data%wd
   call nemsio_writerec(gfileo, 52, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XS"
   the_data = gaussian_data%xs
   call nemsio_writerec(gfileo, 53, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XT"
   the_data = gaussian_data%xt
   call nemsio_writerec(gfileo, 54, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XTTS"
   the_data = gaussian_data%xtts
   call nemsio_writerec(gfileo, 55, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XU"
   the_data = gaussian_data%xu
   call nemsio_writerec(gfileo, 56, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XV"
   the_data = gaussian_data%xv
   call nemsio_writerec(gfileo, 57, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XZ"
   the_data = gaussian_data%xz
   call nemsio_writerec(gfileo, 58, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE XZTS"
   the_data = gaussian_data%xzts
   call nemsio_writerec(gfileo, 59, the_data, iret=iret)
   if (iret /= 0) goto 44
 
   print*,"- WRITE ZC"
   the_data = gaussian_data%zc
   call nemsio_writerec(gfileo, 60, the_data, iret=iret)
   if (iret /= 0) goto 44
 
 endif

 call nemsio_close(gfileo,iret=iret)

 call nemsio_finalize()

 deallocate(the_data)

 return

 43 continue
 print*,"- ** FATAL ERROR OPENING/READING VCOORD FILE."
 print*,"- IRET IS: ", iret
 call errexit(17)
 stop

 44 continue
 print*,"- ** FATAL ERROR WRITING GAUSSIAN NEMSIO FILE."
 print*,"- IRET IS: ", iret
 call errexit(15)
 stop

 end subroutine write_sfc_data

!-------------------------------------------------------------------------------------------
! Read tile data.
!-------------------------------------------------------------------------------------------

 subroutine read_data_anl

 use netcdf
 use io

 implicit none

 integer             :: ijtile, id_dim, id_var
 integer             :: error, tile, ncid
 integer             :: istart, iend

 real(kind=8), allocatable        :: dummy(:,:), dummy3d(:,:,:)

!-------------------------------------------------------------------------------------------
! Get tile dimensions from the first analysis file.
!-------------------------------------------------------------------------------------------

 error=nf90_open("./anal.tile1.nc",nf90_nowrite,ncid)
 error=nf90_inq_dimid(ncid, 'xaxis_1', id_dim)
 call netcdf_err(error, 'READING xaxis_1' )
 error=nf90_inquire_dimension(ncid,id_dim,len=itile)
 call netcdf_err(error, 'READING xaxis_1' )

 error=nf90_inq_dimid(ncid, 'yaxis_1', id_dim)
 call netcdf_err(error, 'READING yaxis_1' )
 error=nf90_inquire_dimension(ncid,id_dim,len=jtile)
 call netcdf_err(error, 'READING yaxis_1' )

 error = nf90_close(ncid)

 ijtile = itile*jtile

 allocate(dummy(itile,jtile))
 allocate(dummy3d(itile,jtile,4))

 allocate(tile_data%orog(ijtile*num_tiles))
 allocate(tile_data%canopy(ijtile*num_tiles))
 allocate(tile_data%slmask(ijtile*num_tiles))
 allocate(tile_data%tg3(ijtile*num_tiles))
 allocate(tile_data%alvsf(ijtile*num_tiles))
 allocate(tile_data%alvwf(ijtile*num_tiles))
 allocate(tile_data%alnsf(ijtile*num_tiles))
 allocate(tile_data%alnwf(ijtile*num_tiles))
 allocate(tile_data%facsf(ijtile*num_tiles))
 allocate(tile_data%facwf(ijtile*num_tiles))
 allocate(tile_data%ffhh(ijtile*num_tiles))
 allocate(tile_data%ffmm(ijtile*num_tiles))
 allocate(tile_data%fice(ijtile*num_tiles))
 allocate(tile_data%hice(ijtile*num_tiles))
 allocate(tile_data%sheleg(ijtile*num_tiles))
 allocate(tile_data%stype(ijtile*num_tiles))
 allocate(tile_data%vfrac(ijtile*num_tiles))
 allocate(tile_data%vtype(ijtile*num_tiles))
 allocate(tile_data%zorl(ijtile*num_tiles))
 allocate(tile_data%tsea(ijtile*num_tiles))
 allocate(tile_data%f10m(ijtile*num_tiles))
 allocate(tile_data%q2m(ijtile*num_tiles))
 allocate(tile_data%shdmax(ijtile*num_tiles))
 allocate(tile_data%shdmin(ijtile*num_tiles))
 allocate(tile_data%slope(ijtile*num_tiles))
 allocate(tile_data%snoalb(ijtile*num_tiles))
 allocate(tile_data%srflag(ijtile*num_tiles))
 allocate(tile_data%snwdph(ijtile*num_tiles))
 allocate(tile_data%t2m(ijtile*num_tiles))
 allocate(tile_data%tisfc(ijtile*num_tiles))
 allocate(tile_data%tprcp(ijtile*num_tiles))
 allocate(tile_data%uustar(ijtile*num_tiles))
 allocate(tile_data%slc(ijtile*num_tiles,4))
 allocate(tile_data%smc(ijtile*num_tiles,4))
 allocate(tile_data%stc(ijtile*num_tiles,4))
! nst
 if (trim(donst) == "yes" .or. trim(donst) == "YES") then
   allocate(tile_data%c0(ijtile*num_tiles))
   allocate(tile_data%cd(ijtile*num_tiles))
   allocate(tile_data%dconv(ijtile*num_tiles))
   allocate(tile_data%dtcool(ijtile*num_tiles))
   allocate(tile_data%land(ijtile*num_tiles))
   allocate(tile_data%qrain(ijtile*num_tiles))
   allocate(tile_data%tref(ijtile*num_tiles))
   allocate(tile_data%w0(ijtile*num_tiles))
   allocate(tile_data%wd(ijtile*num_tiles))
   allocate(tile_data%xs(ijtile*num_tiles))
   allocate(tile_data%xt(ijtile*num_tiles))
   allocate(tile_data%xtts(ijtile*num_tiles))
   allocate(tile_data%xu(ijtile*num_tiles))
   allocate(tile_data%xv(ijtile*num_tiles))
   allocate(tile_data%xz(ijtile*num_tiles))
   allocate(tile_data%xzts(ijtile*num_tiles))
   allocate(tile_data%zc(ijtile*num_tiles))
 endif

 do tile = 1, 6

   print*
   print*, "- READ INPUT SFC DATA FOR TILE: ", tile

   istart = (ijtile) * (tile-1) + 1
   iend   = istart + ijtile - 1

   if (tile==1) error=nf90_open("./anal.tile1.nc",nf90_nowrite,ncid)
   if (tile==2) error=nf90_open("./anal.tile2.nc",nf90_nowrite,ncid)
   if (tile==3) error=nf90_open("./anal.tile3.nc",nf90_nowrite,ncid)
   if (tile==4) error=nf90_open("./anal.tile4.nc",nf90_nowrite,ncid)
   if (tile==5) error=nf90_open("./anal.tile5.nc",nf90_nowrite,ncid)
   if (tile==6) error=nf90_open("./anal.tile6.nc",nf90_nowrite,ncid)

   call netcdf_err(error, 'OPENING FILE' )

   error=nf90_inq_varid(ncid, "slmsk", id_var)
   call netcdf_err(error, 'READING slmsk ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING slmsk' )
   print*,'- SLMSK: ',maxval(dummy),minval(dummy)
   tile_data%slmask(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tsea", id_var)
   call netcdf_err(error, 'READING tsea ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tsea' )
   print*,'- TSEA:  ',maxval(dummy),minval(dummy)
   tile_data%tsea(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "sheleg", id_var)
   call netcdf_err(error, 'READING sheleg ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING sheleg' )
   print*,'- SHELEG: ',maxval(dummy),minval(dummy)
   tile_data%sheleg(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tg3", id_var)
   call netcdf_err(error, 'READING tg3 ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tg3' )
   print*,'- TG3: ',maxval(dummy),minval(dummy)
   tile_data%tg3(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "zorl", id_var)
   call netcdf_err(error, 'READING zorl ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING zorl' )
   print*,'- ZORL: ',maxval(dummy),minval(dummy)
   tile_data%zorl(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alvsf", id_var)
   call netcdf_err(error, 'READING alvsf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alvsf' )
   print*,'- ALVSF: ',maxval(dummy),minval(dummy)
   tile_data%alvsf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alvwf", id_var)
   call netcdf_err(error, 'READING alvwf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alvwf' )
   print*,'- ALVWF: ',maxval(dummy),minval(dummy)
   tile_data%alvwf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alnsf", id_var)
   call netcdf_err(error, 'READING alnsf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alnsf' )
   print*,'- ALNSF: ',maxval(dummy),minval(dummy)
   tile_data%alnsf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "alnwf", id_var)
   call netcdf_err(error, 'READING alnwf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING alnwf' )
   print*,'- ALNWF: ',maxval(dummy),minval(dummy)
   tile_data%alnwf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "facsf", id_var)
   call netcdf_err(error, 'READING facsf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING facsf' )
   print*,'- FACSF: ',maxval(dummy),minval(dummy)
   tile_data%facsf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "facwf", id_var)
   call netcdf_err(error, 'READING facwf ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING facwf' )
   print*,'- FACWF: ',maxval(dummy),minval(dummy)
   tile_data%facwf(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "vfrac", id_var)
   call netcdf_err(error, 'READING vfrac ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING vfrac' )
   print*,'- VFRAC: ',maxval(dummy),minval(dummy)
   tile_data%vfrac(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "canopy", id_var)
   call netcdf_err(error, 'READING canopy ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING canopy' )
   print*,'- CANOPY: ',maxval(dummy),minval(dummy)
   tile_data%canopy(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "f10m", id_var)
   call netcdf_err(error, 'READING f10m ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING f10m' )
   print*,'- F10M: ',maxval(dummy),minval(dummy)
   tile_data%f10m(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "t2m", id_var)
   call netcdf_err(error, 'READING t2m ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING t2m' )
   print*,'- T2M: ',maxval(dummy),minval(dummy)
   tile_data%t2m(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "q2m", id_var)
   call netcdf_err(error, 'READING q2m ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING q2m' )
   print*,'- Q2M: ',maxval(dummy),minval(dummy)
   tile_data%q2m(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "vtype", id_var)
   call netcdf_err(error, 'READING vtype ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING vtype' )
   print*,'- VTYPE: ',maxval(dummy),minval(dummy)
   tile_data%vtype(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "stype", id_var)
   call netcdf_err(error, 'READING stype ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING stype' )
   print*,'- STYPE: ',maxval(dummy),minval(dummy)
   tile_data%stype(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "uustar", id_var)
   call netcdf_err(error, 'READING uustar ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING uustar' )
   print*,'- UUSTAR: ',maxval(dummy),minval(dummy)
   tile_data%uustar(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "ffmm", id_var)
   call netcdf_err(error, 'READING ffmm ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING ffmm' )
   print*,'- FFMM: ',maxval(dummy),minval(dummy)
   tile_data%ffmm(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "ffhh", id_var)
   call netcdf_err(error, 'READING ffhh ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING ffhh' )
   print*,'- FFHH: ',maxval(dummy),minval(dummy)
   tile_data%ffhh(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "hice", id_var)
   call netcdf_err(error, 'READING hice ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING hice' )
   print*,'- HICE: ',maxval(dummy),minval(dummy)
   tile_data%hice(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "fice", id_var)
   call netcdf_err(error, 'READING fice ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING fice' )
   print*,'- FICE: ',maxval(dummy),minval(dummy)
   tile_data%fice(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tisfc", id_var)
   call netcdf_err(error, 'READING tisfc ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tisfc' )
   print*,'- TISFC: ',maxval(dummy),minval(dummy)
   tile_data%tisfc(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "tprcp", id_var)
   call netcdf_err(error, 'READING tprcp ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING tprcp' )
   print*,'- TPRCP: ',maxval(dummy),minval(dummy)
   tile_data%tprcp(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "srflag", id_var)
   call netcdf_err(error, 'READING srflag ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING srfalg' )
   print*,'- SRFLAG: ',maxval(dummy),minval(dummy)
   tile_data%srflag(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "snwdph", id_var)
   call netcdf_err(error, 'READING snwdph ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING snwdph' )
   print*,'- SNWDPH: ',maxval(dummy),minval(dummy)
   tile_data%snwdph(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "shdmin", id_var)
   call netcdf_err(error, 'READING shdmin ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING shdmin' )
   print*,'- SHDMIN: ',maxval(dummy),minval(dummy)
   tile_data%shdmin(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "shdmax", id_var)
   call netcdf_err(error, 'READING shdmax ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING shdmax' )
   print*,'- SHDMAX: ',maxval(dummy),minval(dummy)
   tile_data%shdmax(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "slope", id_var)
   call netcdf_err(error, 'READING slope ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING slope' )
   print*,'- SLOPE: ',maxval(dummy),minval(dummy)
   tile_data%slope(istart:iend) = reshape(dummy, (/ijtile/))

   error=nf90_inq_varid(ncid, "snoalb", id_var)
   call netcdf_err(error, 'READING snoalb ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING snoalb' )
   print*,'- SNOALB: ',maxval(dummy),minval(dummy)
   tile_data%snoalb(istart:iend) = reshape(dummy, (/ijtile/))

   if (trim(donst) == "yes" .or. trim(donst) == "YES") then

     error=nf90_inq_varid(ncid, "c_0", id_var)
     call netcdf_err(error, 'READING c_0 ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING c_0' )
     print*,'- C_0: ',maxval(dummy),minval(dummy)
     tile_data%c0(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "c_d", id_var)
     call netcdf_err(error, 'READING c_d ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING c_d' )
     print*,'- C_D: ',maxval(dummy),minval(dummy)
     tile_data%cd(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "d_conv", id_var)
     call netcdf_err(error, 'READING d_conv ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING d_conv' )
     print*,'- D_CONV: ',maxval(dummy),minval(dummy)
     tile_data%dconv(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "dt_cool", id_var)
     call netcdf_err(error, 'READING dt_cool ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING dt_cool' )
     print*,'- DT_COOL: ',maxval(dummy),minval(dummy)
     tile_data%dtcool(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "qrain", id_var)
     call netcdf_err(error, 'READING qrain ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING qrain' )
     print*,'- QRAIN: ',maxval(dummy),minval(dummy)
     tile_data%qrain(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "tref", id_var)
     call netcdf_err(error, 'READING tref ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING tref' )
     print*,'- TREF: ',maxval(dummy),minval(dummy)
     tile_data%tref(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "w_0", id_var)
     call netcdf_err(error, 'READING w_0 ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING w_0' )
     print*,'- W_0: ',maxval(dummy),minval(dummy)
     tile_data%w0(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "w_d", id_var)
     call netcdf_err(error, 'READING w_d ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING w_d' )
     print*,'- W_D: ',maxval(dummy),minval(dummy)
     tile_data%wd(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xs", id_var)
     call netcdf_err(error, 'READING xs ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xs' )
     print*,'- XS: ',maxval(dummy),minval(dummy)
     tile_data%xs(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xt", id_var)
     call netcdf_err(error, 'READING xt ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xt' )
     print*,'- XT: ',maxval(dummy),minval(dummy)
     tile_data%xt(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xtts", id_var)
     call netcdf_err(error, 'READING xtts ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xtts' )
     print*,'- XTTS: ',maxval(dummy),minval(dummy)
     tile_data%xtts(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xzts", id_var)
     call netcdf_err(error, 'READING xzts ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xzts' )
     print*,'- XZTS: ',maxval(dummy),minval(dummy)
     tile_data%xzts(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xu", id_var)
     call netcdf_err(error, 'READING xu ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xu' )
     print*,'- XU: ',maxval(dummy),minval(dummy)
     tile_data%xu(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xv", id_var)
     call netcdf_err(error, 'READING xv ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xv' )
     print*,'- XV: ',maxval(dummy),minval(dummy)
     tile_data%xv(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "xz", id_var)
     call netcdf_err(error, 'READING xz ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING xz' )
     print*,'- XZ: ',maxval(dummy),minval(dummy)
     tile_data%xz(istart:iend) = reshape(dummy, (/ijtile/))

     error=nf90_inq_varid(ncid, "z_c", id_var)
     call netcdf_err(error, 'READING z_c ID' )
     error=nf90_get_var(ncid, id_var, dummy)
     call netcdf_err(error, 'READING z_c' )
     print*,'- Z_C: ',maxval(dummy),minval(dummy)
     tile_data%zc(istart:iend) = reshape(dummy, (/ijtile/))

   endif  ! nst fields

   error=nf90_inq_varid(ncid, "smc", id_var)
   call netcdf_err(error, 'READING smc ID' )
   error=nf90_get_var(ncid, id_var, dummy3d)
   call netcdf_err(error, 'READING smc' )
   print*,'- SMC: ',maxval(dummy3d),minval(dummy3d)
   tile_data%smc(istart:iend,1:4) = reshape(dummy3d, (/ijtile,4/))

   error=nf90_inq_varid(ncid, "stc", id_var)
   call netcdf_err(error, 'READING stc ID' )
   error=nf90_get_var(ncid, id_var, dummy3d)
   call netcdf_err(error, 'READING stc' )
   print*,'- STC: ',maxval(dummy3d),minval(dummy3d)
   tile_data%stc(istart:iend,1:4) = reshape(dummy3d, (/ijtile,4/))

   error=nf90_inq_varid(ncid, "slc", id_var)
   call netcdf_err(error, 'READING slc ID' )
   error=nf90_get_var(ncid, id_var, dummy3d)
   call netcdf_err(error, 'READING slc' )
   print*,'- SLC: ',maxval(dummy3d),minval(dummy3d)
   tile_data%slc(istart:iend,1:4) = reshape(dummy3d, (/ijtile,4/))

   error = nf90_close(ncid)

   print*
   print*, "- READ INPUT OROG DATA FOR TILE: ",tile

   if (tile==1) error=nf90_open("./orog.tile1.nc",nf90_nowrite,ncid)
   if (tile==2) error=nf90_open("./orog.tile2.nc",nf90_nowrite,ncid)
   if (tile==3) error=nf90_open("./orog.tile3.nc",nf90_nowrite,ncid)
   if (tile==4) error=nf90_open("./orog.tile4.nc",nf90_nowrite,ncid)
   if (tile==5) error=nf90_open("./orog.tile5.nc",nf90_nowrite,ncid)
   if (tile==6) error=nf90_open("./orog.tile6.nc",nf90_nowrite,ncid)

   call netcdf_err(error, 'OPENING FILE' )

   error=nf90_inq_varid(ncid, "orog_raw", id_var)
   call netcdf_err(error, 'READING orog_raw ID' )
   error=nf90_get_var(ncid, id_var, dummy)
   call netcdf_err(error, 'READING orog_raw' )
   print*,'- OROG: ',maxval(dummy),minval(dummy)
   tile_data%orog(istart:iend) = reshape(dummy, (/ijtile/))

   error = nf90_close(ncid)

 enddo

 deallocate (dummy, dummy3d)

 end subroutine read_data_anl

!-------------------------------------------------------------------------------------------
! Netcdf error routine.
!-------------------------------------------------------------------------------------------

 subroutine netcdf_err(err, string)

 use netcdf

 implicit none

 character(len=*), intent(in) :: string
 integer, intent(in)          :: err

 character(len=256)           :: errmsg

 if( err.eq.nf90_noerr )return

 errmsg = nf90_strerror(err)
 print*,''
 print*,'** FATAL ERROR: ', trim(string), ': ', trim(errmsg)
 print*,'STOP.'
 call errexit(22)

 return
 end subroutine netcdf_err
