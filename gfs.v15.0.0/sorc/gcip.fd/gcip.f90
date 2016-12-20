
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!
! Program:	GCIP
!
! Author:	Yali Mao
!
! Date:	        January 2011, modified Dec 2011
!
! Notes:        file name of model is given, date time string
!               is determined by the model file.
!
!*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

program GCIP

  use Grib_MOD

  use Kinds ! if it's not declared, some other strange compiling messages will occur.
  use Config
  use Model
  use Metar
  use Pirep
  use Satellite
  use Lightning
  use Radar

  use Algo

  use Pressure2Flight

  IMPLICIT NONE


!**********************************************************************
  type(config_t) :: cfg

  integer, parameter :: URLsize = 256

  ! YYYY MM DD ZZ HH MM SS MS to call /nwprod/lib/sorc/w3nco/w3movdat.f
  integer :: iruntime(8) 
  character(len=URLsize) :: configFile, output_file, modelFile
  character(len=URLsize) :: metarFile, shipFile, satFiles(4)
  character(len=URLsize) :: pirepFile, lightningFile, radarFile

  type(metar_data_t), allocatable :: ships(:,:) ! optional

  type(input_t) :: inputs      ! includes all 6 datasets
  type(icing_output_t) :: outdat     ! pressure level
  type(icing_output_t) :: outdat_FL  ! flight level

  integer :: igrib
  ! for grib 2
  type(gribfield) :: gfld 
  ! for grib 1, gds is for grib 2 as well
  integer :: kpds(200), kgds(200)

  integer :: nx, ny, nz, nfiner, iret

!real, allocatable :: realData2D(:,:) ! used for output data into binary file
!real, allocatable :: realData3D(:,:, :) ! used for output data into binary file

  call parse_args(iruntime, configFile, modelFile, satFiles, &
                metarFile, shipFile, radarFile, pirepFile, &
                lightningFile, output_file, iret)
  if(iret /= 0)  stop "Error: parsing commands"

  call setConfig(configFile, cfg, iret)
  if(iret /= 0)  stop "Error: configuration wrong"

  igrib = cfg% model% grib

  call printConfig(cfg) 

  !================================================!
  ! model
  !================================================!
  write(*,*) LF, "*****************************************"
  write(*,*) "Ingesting model data ..."

  ! kpds for Grib1
  ! kgds for Grib1 and Grib2
  ! gfld for Grib2
  call runModel(trim(modelFile), cfg, kpds, kgds, gfld, inputs%model, iret)
  ! change date and forecast for GCIP writing
  if (igrib == 1) then
     kpds(8) = iruntime(1)-2000
     kpds(9) = iruntime(2)
     kpds(10) = iruntime(3)
     kpds(11) = iruntime(5)
     kpds(12) = 0
     kpds(14) = 0  ! TIME RANGE 1
     kpds(15) = 0  ! TIME RANGE 2
     kpds(16) = 0  ! TIME RANGE FLAG ,must set to 0!
  elseif(igrib == 2) then
     gfld%idsect(6)  = iruntime(1) ! Year ( 4 digits )
     gfld%idsect(7)  = iruntime(2) ! Month
     gfld%idsect(8)  = iruntime(3) ! Day
     gfld%idsect(9)  = iruntime(5) ! Hour
     gfld%idsect(13) = 0 ! Type of processed data ( see Code Table 1.4 )
     gfld%ipdtmpl(9) = 0
  end if

  nx = kgds(2)
  ny = kgds(3)
  nz = cfg% levels% num_pressure


  if(iret /= 0) stop "Error: not able to read model data sucessfully"

!call writeBIN3D(nx, ny, nz, "rh.sample", inputs%model%rh)
!call writeBIN3D(nx, ny, nz, "t.sample", inputs%model%t)
!call writeBIN3D(nx, ny, nz, "pvv.sample", inputs%model%pvv)
!call writeBIN3D(nx, ny, nz, "lqc.sample", inputs%model%lqc)
!call writeBIN3D(nx, ny, nz, "icc.sample", inputs%model%icc)
!call writeBIN3D(nx, ny, nz, "ept.sample", inputs%model%ept)
!call writeBIN3D(nx, ny, nz, "slw.sample", inputs%model%slw)
!call writeBIN3D(nx, ny, nz, "twp.sample", inputs%model%twp)


  !================================================!
  ! metar
  !================================================!
  !!! Global::metar_data_t
  write(*,*) LF, "*****************************************"
  write(*,*) "Ingesting METAR data ..."
  call runMetar(trim(metarFile), iruntime, cfg, kgds, inputs%metar, iret)
  if (iret == 0 .and. trim(shipFile) /= "") then
     call runMetar(trim(shipFile), iruntime, cfg, kgds, ships, iret)
     if(iret == 0) then
       where(.not. inputs%metar%affected) inputs%metar = ships
       deallocate(ships)
     endif
  end if

  if(iret /= 0) stop "Error: not able to read Metar/Ship file sucessfully"

!allocate(realData2D(nx, ny))
!realData2D = inputs%metar%cloudCoverage
!call writeBIN2D(nx, ny, "metarCoverage.sample", realData2D)
!realData2D = inputs%metar%cloudBaseHeight
!call writeBIN2D(nx, ny, "metarCldBase.sample", realData2D)
!deallocate(realData2D)


  !================================================!
  ! satellite
  !================================================!
  write(*,*) LF, "*****************************************"
  write(*,*) "Ingesting satellite data ..."
  call runSat(satFiles, iruntime, cfg, kgds, inputs%sat, iret)

  nfiner = cfg%sat%nfiner

!if(iret == 0) then
!call writeBIN2D(nx*nfiner, ny*nfiner, "ch2.sample", inputs%sat%ch2)
!call writeBIN2D(nx*nfiner, ny*nfiner, "vis.sample", inputs%sat%vis)
!call writeBIN2D(nx*nfiner, ny*nfiner, "ch4.sample", inputs%sat%ch4)
!call writeBIN2D(nx*nfiner, ny*nfiner, "ch2mch4.sample", inputs%sat%ch2mch4)
!call writeBIN2D(nx*nfiner, ny*nfiner, "sunz.sample", inputs%sat%sunz)
!call writeBIN2D(nx*nfiner, ny*nfiner, "ch2_ref.sample", inputs%sat%ch2_ref)
!call writeBIN2D(nx*nfiner, ny*nfiner, "satice.sample", inputs%sat%satice)
!endif

  !================================================!
  ! pirep            default: MISSING
  !================================================!
  !!! Global::pirep_data_t
  write(*,*) LF, "*****************************************"
  write(*,*) "Ingesting PIREPs data ..."
  call runPirep(trim(pirepFile), iruntime, cfg, kgds, inputs%model%h, inputs%pirep, iret)


!  allocate(realData3D(nx, ny, nz))
!  realData3D =  inputs%pirep%interest
!  if(iret==0) call writeBIN3D(nx, ny, nz, "pirepInterest.sample", realData3D)
!  realData3D =  inputs%pirep%weight
!  if(iret==0) call writeBIN3D(nx, ny, nz, "pirepWeight.sample", realData3D)
!  deallocate(realData3D)


  !================================================!
  ! lightning            default: dist- MISSING rate-0.0
  !================================================!
  !!! Global::lightning_data_t
  write(*,*) LF, "*****************************************"
  write(*,*) "Ingesting lightning data ..."
  call runLightning(trim(lightningFile), iruntime, cfg, kgds, inputs%lightning, iret)

!  if(iret == 0) then
!     allocate(realData2D(nx, ny))
!     realData2D = inputs%lightning%dist
!     if(iret==0) call writeBIN2D(nx, ny, "lightningDist.sample", realData2D)
!     realData2D = inputs%lightning%rate
!     if(iret==0) call writeBIN2D(nx, ny, "lightningRate.sample", realData2D)
!     deallocate(realData2D)
!  end if

  !================================================!
  ! radar
  !================================================!
  write(*,*) LF, "*****************************************"
  write(*,*) "Ingesting radar data ..."
  call runRadar(trim(radarFile), cfg, inputs%model%top, kgds, inputs%radar, iret)

!  if(iret==0) then
!     allocate(realData3D(nx, ny, 7))
!     realData3D = 0
!     realData3D = inputs%radar%vipPct
!     call writeBIN3D(nx, ny,7, "radarvip.sample", realData3D)
!     deallocate(realData3D)
!
!     allocate(realData3D(nx, ny, 2))
!     realData3D = 0
!     realData3D = inputs%radar%dbzPct
!     call writeBIN3D(nx, ny,2, "radardbz.sample", realData3D)
!     deallocate(realData3D)
!  end if

  !================================================!
  ! run algo
  !================================================!

  ! adjust some inputs
  write(*,*) LF, "*****************************************"
  write(*,*) "Running algorithm ..."
  call runAlgo(nz, kgds, nfiner, inputs, outdat, iret) ! outdat includes height information

  if(iret /= 0) stop "Algorithm"


  !================================================!
  ! do clean-up of inputs after runAlgo()
  !================================================!
  ! Model
  ! deallocate(inputs%model%h) ! will be deallocated after pressue to height level
  deallocate(inputs%model%pvv)
  deallocate(inputs%model%t)
  deallocate(inputs%model%rh)
  deallocate(inputs%model%ept)
  deallocate(inputs%model%icc)
  deallocate(inputs%model%lqc)
  deallocate(inputs%model%slw)
  deallocate(inputs%model%twp)

  ! Satellite
  if(allocated(inputs%sat%vis)) deallocate(inputs%sat%vis)
  if(allocated(inputs%sat%ch2)) deallocate(inputs%sat%ch2)
  if(allocated(inputs%sat%ch4)) deallocate(inputs%sat%ch4)
  if(allocated(inputs%sat%ch2mch4)) deallocate(inputs%sat%ch2mch4)
  if(allocated(inputs%sat%sunz)) deallocate(inputs%sat%sunz)
  if(allocated(inputs%sat%ch2_ref)) deallocate(inputs%sat%ch2_ref)
  if(allocated(inputs%sat%satice)) deallocate(inputs%sat%satice)

  ! metar, radar, pirep and lightning
  if(allocated(inputs%metar)) deallocate(inputs%metar)
  if(allocated(inputs%radar%vipPct)) deallocate(inputs%radar%vipPct)
  if(allocated(inputs%radar%dbzPct))  deallocate(inputs%radar%dbzPct)
  if(allocated(inputs%pirep))  deallocate(inputs%pirep)
  if(allocated(inputs%lightning))  deallocate(inputs%lightning)


  outdat%ctype="PRS"
  if(igrib == 1) then
     call writeIcing(output_file, outdat, iret, kpds, kgds)
  elseif(igrib == 2) then
     call writeIcing(output_file, outdat, iret, gfld=gfld)
  end if
  write(*,*) "writing iret=", iret

  !================================================!
  ! pressure2flight, category embedded
  !================================================!
  write(*,*) LF, "*****************************************"
  write(*,*) "Running pressure2flight ..."
  ! category embedded
  call runPressure2Flight(kgds, inputs%model%h, outdat, outdat_FL, iret)

  !================================================!
  ! do clean-up of pressure-level outdat
  !================================================!
  call cleanupOutput(outdat)
  deallocate(inputs%model%h)
  deallocate(inputs%model%top)
  deallocate(inputs%model%p)


  !================================================!
  ! write cip outputs to GRIB file
  !================================================!
  write(*,*) LF, "*****************************************"
  outdat_FL%ctype="FLT"
  if(igrib == 1) then
     call writeIcing(trim(output_file) // ".FLT", outdat_FL, iret, kpds, kgds)
  elseif(igrib == 2) then
     call writeIcing(trim(output_file) // ".FLT", outdat_FL, iret, gfld=gfld)
  end if

  !================================================!
  ! do the final clean-up
  !================================================!
  if (igrib == 2)  call gf_free(gfld)

  call doneConfig(cfg)
  call cleanupOutput(outdat_FL)


  write(*,*) LF, "****************DONE*************************"

  stop
end program GCIP

!**********************************************************************
! * subroutine: usage() -- prints proper usage
! *
subroutine usage(runtime, configFile, modelFile, satFiles, &
                  metarFile, shipFile, radarFile, pirepFile, &
                  lightningFile, output_file, iret)
  character(len=*), intent(in) :: runtime   ! YYYYMMDDHH
  character(len=*), intent(in) :: configFile, modelFile, satFiles(:)
  character(len=*), intent(in) :: metarFile, shipFile, radarFile
  character(len=*), intent(in) :: pirepFile, lightningFile
  character(len=*), intent(in) :: output_file ! output GRIB file

  character(len=256) :: progname

  integer :: i

  call GET_COMMAND_ARGUMENT(0, progname)

  print *, 'Usage:',trim(progname), ' -t YYYYMMDDHH -c config_file ', &
            '-model model_file -metar metar_file [-ship ship_file] ', &
            '-sat visible shortwave infared satellite_source ', &
            '[-radar radar_file] [-pirep pirep_file] ', &
            '[-lightning lightning_file -o output_file'
  print *, 'Currently they are:'
  print *, '  YYYYMMDDHH:  ', runtime
  print *, '  config_file: ', trim(configFile)
  print *, '  model-file: ', trim(modelFile)
  print *, '  METAR file: ', trim(metarFile)
  print *, '  ships file: ', trim(shipFile)
  print *, '  satellite files: ', (trim(satFiles(i)), i = 1, size(satFiles))
  print *, '  radar file: ', trim(radarFile)
  print *, '  PIREP file: ', trim(pirepFile)
  print *, '  lightning file: ', trim(lightningFile)
  print *, '  output_file: ', trim(output_file)
end subroutine usage

!**********************************************************************
! * subroutine: parse_args()
! *

subroutine parse_args(iruntime, configFile, modelFile, satFiles, &
                metarFile, shipFile, radarFile, pirepFile, &
                lightningFile, output_file, iret)

  integer, intent(out) :: iruntime(8) 
  character(len=*), intent(out) :: configFile, modelFile, satFiles(4)
  character(len=*), intent(out) :: metarFile, shipFile, radarFile
  character(len=*), intent(out) :: pirepFile, lightningFile
  character(len=*), intent(out) :: output_file
  integer, intent(out) :: iret

  integer :: i, N
  character(len=256) :: arg
  character(len=12) :: runtime    ! YYYYMMDDHH

  iret = 0
  runtime	= "" ! without this assignment, len_trim() does not work properly
  configFile	= ""
  output_file	= ""
  modelFile	= ""
  satFiles(:)	= ""
  metarFile	= ""
  shipFile	= ""
  radarFile	= ""
  pirepFile	= ""
  lightningFile = ""

  N = COMMAND_ARGUMENT_COUNT()

  i = 1
  do while (i <= N)

     call GET_COMMAND_ARGUMENT(i, arg )

     select case(trim(arg))
     case('-t')
        call GET_COMMAND_ARGUMENT(i+1, runtime)
        i = i + 1
     case('-c')
        call GET_COMMAND_ARGUMENT(i+1, configFile)
        i = i + 1
     case("-model")
        call GET_COMMAND_ARGUMENT(i+1, modelFile)
        i = i + 1
     case("-sat")
        call GET_COMMAND_ARGUMENT(i+1, satFiles(1))
        call GET_COMMAND_ARGUMENT(i+2, satFiles(2))
        call GET_COMMAND_ARGUMENT(i+3, satFiles(3))
        call GET_COMMAND_ARGUMENT(i+4, satFiles(4))
        i = i + 4
     case("-metar")
        call GET_COMMAND_ARGUMENT(i+1, metarFile)
        i = i + 1
     case("-ship")
        call GET_COMMAND_ARGUMENT(i+1, shipFile)
        i = i + 1
     case("-radar")
        call GET_COMMAND_ARGUMENT(i+1, radarFile)
        i = i + 1
     case("-pirep")
        call GET_COMMAND_ARGUMENT(i+1, pirepFile)
        i = i + 1
     case("-lightning")
        call GET_COMMAND_ARGUMENT(i+1, lightningFile)
        i = i + 1
     case('-o')
        call GET_COMMAND_ARGUMENT(i+1, output_file)
        i = i + 1
     case default
        i = i + 1
     end select
  end do

  if(len_trim(runtime)  == 0 .or. len_trim(configFile) == 0 .or. &
       len_trim(output_file) == 0) then
     iret = -1
     call usage(runtime, configFile, output_file)
  end if

  iruntime(:) = 0
  ! iruntime(4) is time zone
  read(runtime, "(I4,4I2)")iruntime(1),iruntime(2),iruntime(3),iruntime(5)

  return
end subroutine parse_args


!**********************************************************************
! * subroutine: writeBIN2D() -- writing 2D pure data to binary file
! *
subroutine writeBIN2D(nx, ny, binfile, dataset)
  implicit none

  integer :: nx, ny
  character(len=*), intent(in) :: binfile
  real, intent(in) :: dataset(nx,ny)

  !  integer :: nx, ny
  integer :: unit, iret

  unit = 10

  write(*,*) "Writing ", trim(binfile), " data. nx=", nx, "ny=", ny
  open(unit, file=trim(binfile), form="unformatted", access="direct", recl=nx*ny*4)
  write(unit, rec=1, iostat=iret) dataset(:, :)
  if(iret /= 0) write(*,*) iret
  close(unit)

  return
end subroutine writeBIN2D

!**********************************************************************
! * subroutine: writeBIN3D() -- writing 3D pure data to binary file
! *
subroutine writeBIN3D(nx, ny, nz, binfile, dataset)
  implicit none

  integer :: nx, ny, nz
  character(len=*), intent(in) :: binfile
  real, intent(in) :: dataset(nx,ny, nz)
 !  integer :: nx, ny
  integer :: unit, iret

  unit = 10

  write(*,*) "Writing ", trim(binfile), " data. nx=", nx, "ny=", ny, "nz=", nz
  open(unit, file=trim(binfile), form="unformatted", access="direct", recl=nx*ny*nz*4)
  write(unit, rec=1, iostat=iret) dataset
  if(iret /= 0) write(*,*) iret
  close(unit)

  return
end subroutine writeBIN3D

!**********************************************************************
! * subroutine: cleanupOutput()
! *
! * Description: release memories of all allocated output data
! * 
! * Returns:
! *
! * Notes: 
! *
subroutine cleanupOutput(outdat)
  use Kinds
  type(icing_output_t), intent(inout):: outdat

  ! height
  deallocate(outdat%levels)
  ! severity
  deallocate(outdat%severity)
  ! probability
  deallocate(outdat%probability)
  ! SLD
  deallocate(outdat%sld)

  return
end subroutine cleanupOutput

