module gsi_nemsio_mod
!$$$   module documentation block
!             .      .    .                                       .
! module:     gsi_nemsio_mod
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added module doc block
!   2014-06-30  wu     - remove debugging printout
!   2015_05_13  wu     - output error flag of nemsio_open
!   2015-06-10  s.liu  - add gsi_nemsio_read_fraction to handle NMMB f_rain and f_ice
!   2015-06-10  s.liu  - add gsi_nemsio_write_fraction to handle NMMB f_rain and f_ice
!   2016-02-05  s.liu  - add fraction2variable and variable2fraction to handle NMMB f_rain and f_ice
!
! subroutines included:
!   sub gsi_nemsio_open
!   sub gsi_nemsio_update
!   sub gsi_nemsio_close
!   sub gsi_nemsio_read
!   sub gsi_nemsio_read_fraction
!   sub gsi_nemsio_write
!   sub gsi_nemsio_write_fraction
!   sub fraction2variable
!   sub variable2fraction
!
! variable definitions:
!
! attributes:
!   langauge: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use nemsio_module, only: nemsio_gfile
  use gridmod, only: nlon_regional,nlat_regional
  implicit none

  type(nemsio_gfile) :: gfile
  save gfile

  real(r_single),allocatable::work_saved(:)

! set default to private
  private
! set subroutines to public
  public :: gsi_nemsio_open
  public :: gsi_nemsio_update
  public :: gsi_nemsio_close
  public :: gsi_nemsio_read
  public :: gsi_nemsio_read_fraction
  public :: gsi_nemsio_read_fractionnew
  public :: gsi_nemsio_write
  public :: gsi_nemsio_write_fraction
  public :: gsi_nemsio_write_fractionnew

contains

  subroutine gsi_nemsio_open(file_name,iostatus,message,mype,mype_io,ierr)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_open
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    file_name
!    iostatus
!    message
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use nemsio_module, only: nemsio_init,nemsio_open
    implicit none

    character(*)   ,intent(in   ) :: file_name        !  input file name
    character(*)   ,intent(in   ) :: iostatus         !  'READ' for read only, 'rdwr' for read/write
    character(*)   ,intent(in   ) :: message          !  info to appear in write statement on status of file open
    integer(i_kind),intent(in   ) :: mype,mype_io
    integer(i_kind),intent(out  ) :: ierr

    integer(i_kind) iret

    if(mype==mype_io) then
       call nemsio_init(iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem with nemsio_init, Status = ',iret
          call stop2(74)
       end if
       ierr=0
       call nemsio_open(gfile,file_name,trim(iostatus),iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem opening file',trim(file_name),', Status = ',iret
          ierr=1
          return
       end if
    end if
    allocate(work_saved(nlon_regional*nlat_regional))

  end subroutine gsi_nemsio_open

  subroutine gsi_nemsio_update(file_name,message,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_update
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    file_name
!    message
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_getfilehead,nemsio_close,nemsio_setheadvar
    use nemsio_module, only: nemsio_getheadvar
    use constants, only: zero
    implicit none

    character(*)   ,intent(in   ) :: file_name        !  input file name
    character(*)   ,intent(in   ) :: message          !  info to appear in write statement on status of file open
    integer(i_kind),intent(in   ) :: mype,mype_io

    integer(i_kind) iret,nrec
    integer(i_kind) idate(7),jdate(7),nfhour,nfminute,nfsecondn,nfday,ihrst,idat(3)
    integer(i_kind),dimension(8):: ida,jda
    real(r_kind),dimension(5):: fha
    integer(i_kind) im,jm,lm,nfsecondd,nframe,ntrac,nsoil,nmeta,ntimestep
    logical extrameta
    character(4) gdatatype,modelname
    character(32) gtype

    if(mype==mype_io) then
       call nemsio_init(iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem with nemsio_init, Status = ',iret
          call stop2(74)
       end if
       call nemsio_open(gfile,file_name,'RDWR',iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem opening file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
       call nemsio_getheadvar(gfile,'idat',idat,iret)
       write(6,*)' check old idat after getheadvar, idat,iret=',idat,iret
       call nemsio_getheadvar(gfile,'ihrst',ihrst,iret)
       write(6,*)' check old ihrst after getheadvar, ihrst,iret=',ihrst,iret
       call nemsio_getheadvar(gfile,'ntimestep',ntimestep,iret)
       write(6,*)' check old ntimestep after getheadvar, ntimestep,iret=',ntimestep,iret
       call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=im,dimy=jm, &
         dimz=lm,idate=idate,gdatatype=gdatatype,gtype=gtype,modelname=modelname, &
         nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
         nfday=nfday, &
         nframe=nframe,ntrac=ntrac,nsoil=nsoil,extrameta=extrameta,nmeta=nmeta)
 
       write(6,*)' in gsi_nemsio_update, guess yr,mn,dy,hr,fhr=',idate(1:4),nfhour
       fha=zero ; ida=0 ; jda=0
       fha(2)=nfhour
       fha(3)=nfminute
       ida(1)=idate(1)    !  year
       ida(2)=idate(2)    !  month
       ida(3)=idate(3)    !  day
       ida(4)=0       !  time zone
       ida(5)=idate(4)    !  hour
       ida(6)=idate(5)    ! minute
       call w3movdat(fha,ida,jda)
       jdate(1)=jda(1)    !  new year
       jdate(2)=jda(2)    !  new month
       jdate(3)=jda(3)    !  new day
       jdate(4)=jda(5)    !  new hour
       jdate(5)=jda(6)     !  new minute
       jdate(6)=0     !  new scaled seconds
       jdate(7)=idate(7)  !  new seconds multiplier
       nfhour=0       !  new forecast hour
       nfminute=0
       nfsecondn=0
       ntimestep=0


          call nemsio_setheadvar(gfile,'idate',jdate,iret)
          write(6,*)' after setheadvar, jdate,iret=',jdate,iret
          call nemsio_setheadvar(gfile,'nfhour',nfhour,iret)
          write(6,*)' after setheadvar, nfhour,iret=',nfhour,iret
          call nemsio_setheadvar(gfile,'nfminute',nfminute,iret)
          write(6,*)' after setheadvar, nfminute,iret=',nfminute,iret
          call nemsio_setheadvar(gfile,'nfsecondn',nfsecondn,iret)
          write(6,*)' after setheadvar, nfsecondn,iret=',nfsecondn,iret

!                  
          idat(3)=jdate(1)       !  forecast starting year
          idat(2)=jdate(2)       !  forecast starting month
          idat(1)=jdate(3)       !  forecast starting day  
          ihrst=jdate(4)         !  forecast starting hour (0-23)
          call nemsio_setheadvar(gfile,'idat',idat,iret)
          write(6,*)' after setheadvar, idat,iret=',idat,iret
          call nemsio_setheadvar(gfile,'ihrst',ihrst,iret)
          write(6,*)' after setheadvar, ihrst,iret=',ihrst,iret
          call nemsio_setheadvar(gfile,'ntimestep',ntimestep,iret)
          write(6,*)' after setheadvar, ntimestep,iret=',ntimestep,iret
 
    

!                        Following is diagnostic to check if date updated:

       call nemsio_getfilehead(gfile,iret=iret,nrec=nrec,dimx=im,dimy=jm, &
         dimz=lm,idate=idate,gdatatype=gdatatype,gtype=gtype,modelname=modelname, &
         nfhour=nfhour,nfminute=nfminute,nfsecondn=nfsecondn,nfsecondd=nfsecondd, &
         nfday=nfday, &
         nframe=nframe,ntrac=ntrac,nsoil=nsoil,extrameta=extrameta,nmeta=nmeta)
       write(6,*)' in gsi_nemsio_update, analysis yr,mn,dy,hr,fhr=',idate(1:4),nfhour
       call nemsio_getheadvar(gfile,'idat',idat,iret)
       write(6,*)' check new idat after getheadvar, idat,iret=',idat,iret
       call nemsio_getheadvar(gfile,'ihrst',ihrst,iret)
       write(6,*)' check new ihrst after getheadvar, ihrst,iret=',ihrst,iret
       call nemsio_getheadvar(gfile,'ntimestep',ntimestep,iret)
       write(6,*)' check new ntimestep after getheadvar, ntimestep,iret=',ntimestep,iret
       call nemsio_close(gfile,iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem closing file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
      
    end if

  end subroutine gsi_nemsio_update

  subroutine gsi_nemsio_close(file_name,message,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_close
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    file_name
!    message
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use nemsio_module, only: nemsio_close
    implicit none

    character(*)   ,intent(in   ) :: file_name        !  input file name
    character(*)   ,intent(in   ) :: message          !  info to appear in write statement on status of file open
    integer(i_kind),intent(in   ) :: mype,mype_io

    integer(i_kind) iret

    if(mype==mype_io) then
       call nemsio_close(gfile,iret=iret)
       if(iret/=0) then
          write(6,*)trim(message),'  problem closing file',trim(file_name),', Status = ',iret
          call stop2(74)
       end if
    end if
    deallocate(work_saved)

  end subroutine gsi_nemsio_close

  subroutine gsi_nemsio_read(varname,vartype,gridtype,lev,var,mype,mype_io,good_var)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_read
!   pgrmmr: parrish
!
! abstract:  intermediate level routine to read nmmb model fields using nems_io. 
!             the desired field is retrieved from the previously opened file as a
!             full 2d horizontal field, then interpolated to the analysis grid
!             from the nmmb model grid.  finally, the 2d field is scattered from
!             processor mype_io to subdomains in output array var. 
!             a copy of the original field on the nmmb grid is saved internally in array
!             work_saved in case this field is to be updated by the analysis
!             increment in a call to gsi_nemsio_write immediately after the call to
!             gsi_nemsio_read.
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2010-01-22  parrish - added optional variable good_var to detect read errors in calling program
!                            and have option to avoid program stop.
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!    varname,vartype,gridtype - descriptors for variable to be retrieved from nmmb file
!    lev                      - vertical level number
!    mype     - mpi task id
!    mype_io  - mpi task where field is read from disk
!    good_var - optional, on input, set to .false.  if present(good_var) then error stop is
!                bypassed and good_var is returned .true. for successful read, .false. otherwise.
!
!   output argument list:
!    var      - for successful read, contains desired variable on subdomains.
!    good_var - see above
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror,mpi_integer4
    use gridmod, only:       lat2,lon2,nlon,nlat
    use gridmod, only:       ijn_s,displs_s,itotsub
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use nemsio_module, only: nemsio_readrecv
    use mod_nmmb_to_a, only: nmmb_h_to_a,nmmb_v_to_a
    implicit none

    character(*)   ,intent(in   ) :: varname,vartype,gridtype      ! gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable
    real(r_kind)   ,intent(  out) :: var(lat2*lon2)
    integer(i_kind),intent(in   ) :: mype,mype_io
    logical,optional,intent(inout):: good_var

    integer(i_kind) i,iret,j,mm1,n
    real(r_kind) work(itotsub)
    real(r_kind) work_a(nlat,nlon)
    real(r_single) work_b(nlon_regional*nlat_regional)
    logical good_var_loc

    mm1=mype+1

    if(mype==mype_io) then

!            read field from file with nemsio

       call nemsio_readrecv(gfile,trim(varname),trim(vartype),lev,work_b,iret=iret)
       if(iret==0) then
          work_saved=work_b

!         interpolate to analysis grid

          if(trim(gridtype)=='H') call nmmb_h_to_a(work_b,work_a)
          if(trim(gridtype)=='V') call nmmb_v_to_a(work_b,work_a)


!        scatter to subdomains

          do n=1,itotsub
             i=ltosi_s(n)
             j=ltosj_s(n)
             work(n)=work_a(i,j)
          end do
       end if
    end if
    call mpi_bcast(iret,1,mpi_integer4,mype_io,mpi_comm_world,ierror)
    good_var_loc=.true.
    if(iret/=0) then
       good_var_loc=.false.
       if(mype==0) then
          write(6,*)'  problem reading varname=',trim(varname),', vartype=',trim(vartype),', Status = ',iret
          if(.not.present(good_var)) call stop2(74)
       end if
    end if
    if(present(good_var)) good_var=good_var_loc

    if(good_var_loc) &
      call mpi_scatterv(work,ijn_s,displs_s,mpi_rtype, &
                   var,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)

  end subroutine gsi_nemsio_read

  subroutine gsi_nemsio_read_fraction(varname_frain,varname_fice,varname_clwmr,varname_t, &
                 vartype,lev,var_qi,var_qs,var_qr,var_qw,mype,mype_io,good_var)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_read_fraction
!   pgrmmr: Shun Liu
!
! abstract:  copy from gsi_nemsio_read. To read in NMMB f_rain, f_ice, f_rime and
!            T together and then convert to rain water mixing ratio and snow
!            mixing ratio
!
! program history log:

!   2015-06-5  S.Liu - read in f_rain, f_ice, f_rimef and T
!   2016-02-10 S.Liu - remove gridtype if-test since all variables are in mass point
!
!   input argument list:
!    varname,vartype,gridtype - descriptors for variable to be retrieved from
!    nmmb file
!    lev                      - vertical level number
!    mype     - mpi task id
!    mype_io  - mpi task where field is read from disk
!    good_var - optional, on input, set to .false.  if present(good_var) then
!    error stop is
!                bypassed and good_var is returned .true. for successful read,
!                .false. otherwise.
!
!   output argument list:
!    var      - for successful read, contains desired variable on subdomains.
!    good_var - see above
!
! attributes:
!   language: f90
!   machine:
!

!$$$ end documentation block
    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror,mpi_integer4
    use gridmod, only:       lat2,lon2,nlon,nlat
    use gridmod, only:       ijn_s,displs_s,itotsub
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use nemsio_module, only: nemsio_readrecv
    use mod_nmmb_to_a, only: nmmb_h_to_a,nmmb_v_to_a
    implicit none

    character(*)   ,intent(in   ) :: vartype      !  gridtype='H' or 'V'
    character(*)   ,intent(in   ) :: varname_frain, varname_fice, varname_clwmr, varname_t     !  gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable

    real(r_kind)   ,intent(  out) :: var_qi(lat2*lon2)
    real(r_kind)   ,intent(  out) :: var_qs(lat2*lon2)
    real(r_kind)   ,intent(  out) :: var_qr(lat2*lon2)
    real(r_kind)   ,intent(  out) :: var_qw(lat2*lon2)

    integer(i_kind),intent(in   ) :: mype,mype_io
    logical,optional,intent(inout):: good_var

    integer(i_kind) i,iret,j,mm1,n

    real(r_kind) work_qi(itotsub)
    real(r_kind) work_qs(itotsub)
    real(r_kind) work_qr(itotsub)
    real(r_kind) work_qw(itotsub)

    real(r_kind) work_a_qi(nlat,nlon)
    real(r_kind) work_a_qs(nlat,nlon)
    real(r_kind) work_a_qr(nlat,nlon)
    real(r_kind) work_a_qw(nlat,nlon)

    real(r_single) work_b_frain(nlon_regional*nlat_regional)
    real(r_single) work_b_fice(nlon_regional*nlat_regional)
    real(r_single) work_b_clwmr(nlon_regional*nlat_regional)
    real(r_single) work_b_t(nlon_regional*nlat_regional)

    real(r_single) work_b_qi(nlon_regional*nlat_regional)
    real(r_single) work_b_qs(nlon_regional*nlat_regional)
    real(r_single) work_b_qr(nlon_regional*nlat_regional)
    real(r_single) work_b_qw(nlon_regional*nlat_regional)

    real(r_single) :: t, f_ice, f_rain, wc, qi, qs, qr, qw
    logical good_var_loc

    mm1=mype+1

    if(mype==mype_io) then

!            read field from file with nemsio

       call nemsio_readrecv(gfile,trim(varname_frain),trim(vartype),lev,work_b_frain,iret=iret)
       call nemsio_readrecv(gfile,trim(varname_fice),trim(vartype),lev,work_b_fice,iret=iret)
       call nemsio_readrecv(gfile,trim(varname_clwmr),trim(vartype),lev,work_b_clwmr,iret=iret)
       call nemsio_readrecv(gfile,trim(varname_t),trim(vartype),lev,work_b_t,iret=iret)

       do n=1,nlon_regional*nlat_regional
         t=work_b_t(n)
         f_rain=work_b_frain(n)
         f_ice=work_b_fice(n)
         wc=work_b_clwmr(n)
         call fraction2variable(t,f_ice,f_rain,wc,qi,qs,qr,qw)
         work_b_qi(n)=qi
         work_b_qs(n)=qs
         work_b_qr(n)=qr
         work_b_qw(n)=qw
       end do

       if(iret==0) then
!         work_saved=work_b

!         interpolate to analysis grid

          call nmmb_h_to_a(work_b_qi,work_a_qi)
          call nmmb_h_to_a(work_b_qs,work_a_qs)
          call nmmb_h_to_a(work_b_qr,work_a_qr)
          call nmmb_h_to_a(work_b_qw,work_a_qw)


!        scatter to subdomains

          do n=1,itotsub
             i=ltosi_s(n)
             j=ltosj_s(n)
             work_qi(n)=work_a_qi(i,j)
             work_qs(n)=work_a_qs(i,j)
             work_qr(n)=work_a_qr(i,j)
             work_qw(n)=work_a_qw(i,j)
          end do
       end if
    end if

    call mpi_bcast(iret,1,mpi_integer4,mype_io,mpi_comm_world,ierror)
    good_var_loc=.true.
    if(iret/=0) then
       good_var_loc=.false.
       if(mype==0) then
          write(6,*)'  problem reading varname=',trim(varname_frain),', vartype=',trim(vartype),', Status = ',iret
          if(.not.present(good_var)) call stop2(74)
       end if
    end if
    if(present(good_var)) good_var=good_var_loc

    if(good_var_loc) then
      call mpi_scatterv(work_qi,ijn_s,displs_s,mpi_rtype, &
                   var_qi,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
      call mpi_scatterv(work_qs,ijn_s,displs_s,mpi_rtype, &
                   var_qs,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
      call mpi_scatterv(work_qr,ijn_s,displs_s,mpi_rtype, &
                   var_qr,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
      call mpi_scatterv(work_qw,ijn_s,displs_s,mpi_rtype, &
                   var_qw,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    end if

  end subroutine gsi_nemsio_read_fraction
  subroutine gsi_nemsio_write(varname,vartype,gridtype,lev,var,mype,mype_io,add_saved)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write
!   pgrmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!    varname,vartype,gridtype
!    lev
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!    var
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror
    use gridmod, only:       lat2,lon2,nlon,nlat,lat1,lon1
    use gridmod, only:       ijn,displs_g,itotsub,iglobal
    use general_commvars_mod, only: ltosi,ltosj
    use nemsio_module, only: nemsio_writerecv
    use mod_nmmb_to_a, only: nmmb_a_to_h,nmmb_a_to_v
    implicit none

    character(*)   ,intent(in   ) :: varname,vartype,gridtype      ! gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable
    real(r_kind)   ,intent(in   ) :: var(lat2,lon2)
    integer(i_kind),intent(in   ) :: mype,mype_io
    logical        ,intent(in   ) :: add_saved

    integer(i_kind) i,iret,j,mm1,n
    real(r_kind) work(itotsub),work_sub(lat1,lon1)
    real(r_kind) work_a(nlat,nlon)
    real(r_single) work_b(nlon_regional*nlat_regional)

    mm1=mype+1

    do i=1,lon1
       do j=1,lat1
          work_sub(j,i)=var(j+1,i+1)
       end do
    end do
    call mpi_gatherv(work_sub,ijn(mm1),mpi_rtype, &
                           work,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    if(mype==mype_io) then
       do n=1,iglobal
          i=ltosi(n)
          j=ltosj(n)
          work_a(i,j)=work(n)
       end do
       if(trim(gridtype)=='H') call nmmb_a_to_h(work_a,work_b)
       if(trim(gridtype)=='V') call nmmb_a_to_v(work_a,work_b)
       if(add_saved) work_b=work_b+work_saved
       call nemsio_writerecv(gfile,trim(varname),trim(vartype),lev,work_b,iret=iret)
       if(iret/=0) then
          write(6,*)'  problem writing varname=',trim(varname),', vartype=',trim(vartype),', Status = ',iret
          call stop2(74)
       end if
    end if

  end subroutine gsi_nemsio_write

  subroutine gsi_nemsio_write_fraction(varname_frain,varname_fice,vartype,lev,var_t,var_i,var_r,var_l,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write_fraction
!   pgrmmr:    Shun Liu
!
! abstract:
!
! program history log:
!   2015-05-12  S.Liu - copy from gsi_nemsio_write and modify to handle NMMB hydrometor fraction variable
!
!   input argument list:
!    varname,vartype,gridtype
!    lev
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!    var_frain, var_fice
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror
    use gridmod, only:       lat2,lon2,nlon,nlat,lat1,lon1
    use gridmod, only:       ijn,displs_g,itotsub,iglobal
    use general_commvars_mod, only: ltosi,ltosj
    use nemsio_module, only: nemsio_writerecv
    use mod_nmmb_to_a, only: nmmb_a_to_h,nmmb_a_to_v
    implicit none

    character(*)   ,intent(in   ) :: varname_frain,varname_fice,vartype      ! gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable
    real(r_kind)   ,intent(in   ) :: var_i(lat2,lon2), var_r(lat2,lon2), var_l(lat2,lon2), var_t(lat2,lon2)
    integer(i_kind),intent(in   ) :: mype,mype_io
!   logical        ,intent(in   ) :: add_saved

    integer(i_kind) i,iret,j,mm1,n
    real(r_kind) work_t(itotsub),work_sub_t(lat1,lon1)
    real(r_kind) work_a_t(nlat,nlon)
    real(r_single) work_b_t(nlon_regional*nlat_regional)

    real(r_kind) work_i(itotsub),work_sub_i(lat1,lon1)
    real(r_kind) work_a_i(nlat,nlon)
    real(r_single) work_b_i(nlon_regional*nlat_regional)

    real(r_kind) work_r(itotsub),work_sub_r(lat1,lon1)
    real(r_kind) work_a_r(nlat,nlon)
    real(r_single) work_b_r(nlon_regional*nlat_regional)

    real(r_kind) work_l(itotsub),work_sub_l(lat1,lon1)
    real(r_kind) work_a_l(nlat,nlon)
    real(r_single) work_b_l(nlon_regional*nlat_regional)

    real(r_single) work_b_frain(nlon_regional*nlat_regional)
    real(r_single) work_b_fice(nlon_regional*nlat_regional)
    real(r_single) t,qfi,qfr,qfw,f_rain,f_ice

    mm1=mype+1

    do i=1,lon1
       do j=1,lat1
          work_sub_t(j,i)=var_t(j+1,i+1)
          work_sub_i(j,i)=var_i(j+1,i+1)
          work_sub_r(j,i)=var_r(j+1,i+1)
          work_sub_l(j,i)=var_l(j+1,i+1)
       end do
    end do
!    write(6,*)'writeout1', maxval(work_sub_t),maxval(work_sub_i),maxval(work_sub_r)
    call mpi_gatherv(work_sub_t,ijn(mm1),mpi_rtype, &
                           work_t,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    call mpi_gatherv(work_sub_i,ijn(mm1),mpi_rtype, &
                           work_i,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    call mpi_gatherv(work_sub_r,ijn(mm1),mpi_rtype, &
                           work_r,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    call mpi_gatherv(work_sub_l,ijn(mm1),mpi_rtype, &
                           work_l,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
!    write(6,*)'writeout2', maxval(work_t),maxval(work_i),maxval(work_r)
    if(mype==mype_io) then
       do n=1,iglobal
          i=ltosi(n)
          j=ltosj(n)
          work_a_t(i,j)=work_t(n)
          work_a_i(i,j)=work_i(n)
          work_a_r(i,j)=work_r(n)
          work_a_l(i,j)=work_l(n)
       end do
!      write(6,*)'writeout3', maxval(work_a_r),maxval(work_a_l)

       call nmmb_a_to_h(work_a_t,work_b_t)
       call nmmb_a_to_h(work_a_i,work_b_i)
       call nmmb_a_to_h(work_a_r,work_b_r)
       call nmmb_a_to_h(work_a_l,work_b_l)

!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_t,work_b_i)
!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_i,work_b_i)
!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_r,work_b_r)
!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_l,work_b_l)

!      if(add_saved) work_b_t=work_b_t+work_saved_t
!      if(add_saved) work_b_i=work_b_i+work_saved_i
!      if(add_saved) work_b_r=work_b_r+work_saved_r
!      if(add_saved) work_b_l=work_b_l+work_saved_l
!      write(6,*)'writeout4', maxval(work_b_r),maxval(work_b_l)
!      write(6,*)'writeout44',nlon_regional,nlat_regional,nlon,nlat
       do n=1,nlon_regional*nlat_regional
             t=work_b_t(n)
           qfi=work_b_i(n)
           qfr=work_b_r(n)
           qfw=work_b_l(n)
           call variable2fraction(t, qfi, qfr, qfw, f_ice, f_rain)
           work_b_frain(n)=f_rain
           work_b_fice(n)=f_ice
!          work_b_frain(n)=qfr
!          work_b_fice(n)=qfw
       end do

       call nemsio_writerecv(gfile,trim(varname_frain),trim(vartype),lev,work_b_frain,iret=iret)
       call nemsio_writerecv(gfile,trim(varname_fice),trim(vartype),lev,work_b_fice,iret=iret)
!      write(6,*)'writeout5', maxval(work_b_frain),maxval(work_b_fice)

       if(iret/=0) then
          write(6,*)'  problem writing varname=',trim(varname_frain),', vartype=',trim(vartype),', Status = ',iret
          call stop2(74)
       end if
    end if

  end subroutine gsi_nemsio_write_fraction

  Subroutine fraction2variable(t,f_ice,f_rain, wc, qi,qs,qr,qw)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor
! analysis
!
!   PRGMMR: Shun Liu          ORG: EMC/NCEP        DATE: 2015-05-28
!
! ABSTRACT:
!  This subroutine fraction to qi, qs, qr, qw
!
! PROGRAM HISTORY LOG:
!    2015-05-28  Shun Liu Add NCO document block
!    2016-06-21  Shun Liu give number precisio and remove f_rimef
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INTPUT:
!     t         -  sensible temperature
!     f_ice     -  fraction of condensate in form of ice
!     f_rain    -  fraction of liquid water in form of rain
!     f_rimef   -  ratio of total ice growth to deposition groth
!   OUTPUT
!     qi    -  cloud ice mixing ratio
!     qs    -  large ice mixing ratio
!     qr    -  rain mixing ratio
!     qw    -  cloud water mixing ratio
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  WCOSS at NOAA/ESRL - college park, DC
!
!$$$

 use kinds, only: r_kind,r_single

   real(r_single) t, qi,qs, qr, qw, wc
   real(r_single) f_ice, f_rain
   real(r_single),parameter:: epsq=1.e-12_r_single
   real(r_single),parameter:: tice=233.15_r_single,ticek=273.15_r_single
   real(r_single),parameter:: tice_mix=243.15_r_single
   real(r_single) ::t1,t2, coef1, coef2, coef


   qi=0.0_r_single; qs=0.0_r_single; qr=0.0_r_single; qw=0.0_r_single
   if(wc > 0.0_r_single) then

     if(f_ice>1.0_r_single) f_ice=1.0_r_single
     if(f_ice<0.0_r_single) f_ice=0.0_r_single
     if(f_rain>1.0_r_single) f_rain=1.0_r_single
     if(f_rain<0.0_r_single) f_rain=0.0_r_single

     qi=0.05_r_single*wc*f_ice
     qs=0.95_r_single*wc*f_ice

     if(t<=tice_mix)then
       t1=tice_mix
       t2=tice
       coef1=0.05_r_single
       coef2=0.10_r_single
       coef=(t-t2)/(t1-t2)*coef1+(t-t1)/(t2-t1)*coef2
       qi=coef*wc*f_ice
       qs=(1.0_r_single-coef)*wc*f_ice
     end if

!* do not consider frime at the moment
     qr=wc*(1.0_r_single-f_ice)*f_rain
     qw=wc*(1.0_r_single-f_ice)*(1.0_r_single-f_rain)
   end if

  end subroutine fraction2variable


  subroutine variable2fraction(t, qi, qr, qw, f_ice, f_rain)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor analysis
!
!   PRGMMR: Shun Liu          ORG: EMC/NCEP        DATE: 2012-10-24
!
! ABSTRACT:
!  This subroutine qi qr qw to fraction
!
! PROGRAM HISTORY LOG:
!    2013-10-18  Shun Liu Add NCO document block
!    2015-11-16  Shun Liu move from gsdcldanalysis4nmmb.F90 to this module
!    2016-06-21  Shun Liu give number precisio
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INPUT
!     qi    -  cloud ice mixing ratio
!     qr    -  rain mixing ratio
!     qw    -  cloud water mixing ratio
!   OUTPUT:
!     f_ice     -  fraction of condensate in form of ice
!     f_rain    -  fraction of liquid water in form of rain
!     f_rimef   -  ratio of total ice growth to deposition groth
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  WCOSS at NOAA/ESRL - college park, DC
!
!$$$

 use kinds, only: r_kind,r_single

   real(r_single) t, qi, qr, qw, wc, dum
   real(r_single) f_ice, f_rain
   real(r_single),parameter:: epsq=1.e-12_r_single
   real(r_single),parameter:: tice=233.15_r_single,ticek=273.15_r_single

   wc=qi+qr+qw
   if(wc > 0.0_r_single) then
     if(qi<epsq)then 
           f_ice=0.0_r_single
           if(t<tice) f_ice=1.0_r_single
     else 
           f_ice=0.0_r_single
           dum=qi/wc
           if(dum<1.0_r_single) then
             f_ice=dum
           else
             f_ice=1.0_r_single
           end if
     end if

     if(qr < epsq) then
           f_rain=0.0_r_single
     else
           f_rain=qr/(qr+qw)
     end if
   else
           f_rain=0.0_r_single
           f_ice=0.0_r_single
   end if

  end subroutine variable2fraction
  subroutine gsi_nemsio_read_fractionnew(varname_frain,varname_fice,varname_clwmr,varname_frimef, &
                 vartype,lev,var_qi,var_qs,var_qr,var_qw,mype,mype_io,good_var)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_read_fraction
!   pgrmmr: Shun Liu
!
! abstract:  copy from gsi_nemsio_read. To read in NMMB f_rain, f_ice, f_rime and
!            T together and then convert to rain water mixing ratio and snow
!            mixing ratio
!
! program history log:

!   2015-06-5  S.Liu - read in f_rain, f_ice, f_rimef and T
!   2016-02-10 S.Liu - remove gridtype if-test since all variables are in mass point
!   2016-      T. Lei - to add the frimef to be read out 
!
!   input argument list:
!    varname,vartype,gridtype - descriptors for variable to be retrieved from
!    nmmb file
!    lev                      - vertical level number
!    mype     - mpi task id
!    mype_io  - mpi task where field is read from disk
!    good_var - optional, on input, set to .false.  if present(good_var) then
!    error stop is
!                bypassed and good_var is returned .true. for successful read,
!                .false. otherwise.
!
!   output argument list:
!    var      - for successful read, contains desired variable on subdomains.
!    good_var - see above
!
! attributes:
!   language: f90
!   machine:
!

!$$$ end documentation block
    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror,mpi_integer4
    use gridmod, only:       lat2,lon2,nlon,nlat
    use gridmod, only:       ijn_s,displs_s,itotsub
    use general_commvars_mod, only: ltosi_s,ltosj_s
    use nemsio_module, only: nemsio_readrecv
    use mod_nmmb_to_a, only: nmmb_h_to_a,nmmb_v_to_a
    implicit none

    character(*)   ,intent(in   ) :: vartype      !  gridtype='H' or 'V'
    character(*)   ,intent(in   ) :: varname_frain, varname_fice, varname_clwmr, varname_frimef     !  gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable

    real(r_kind)   ,intent(  out) :: var_qi(lat2*lon2)
    real(r_kind)   ,intent(  out) :: var_qs(lat2*lon2)
    real(r_kind)   ,intent(  out) :: var_qr(lat2*lon2)
    real(r_kind)   ,intent(  out) :: var_qw(lat2*lon2)

    integer(i_kind),intent(in   ) :: mype,mype_io
    logical,optional,intent(inout):: good_var

    integer(i_kind) i,iret,j,mm1,n

    real(r_kind) work_qi(itotsub)
    real(r_kind) work_qs(itotsub)
    real(r_kind) work_qr(itotsub)
    real(r_kind) work_qw(itotsub)

    real(r_kind) work_a_qi(nlat,nlon)
    real(r_kind) work_a_qs(nlat,nlon)
    real(r_kind) work_a_qr(nlat,nlon)
    real(r_kind) work_a_qw(nlat,nlon)

    real(r_single) work_b_frain(nlon_regional*nlat_regional)
    real(r_single) work_b_fice(nlon_regional*nlat_regional)
    real(r_single) work_b_clwmr(nlon_regional*nlat_regional)
    real(r_single) work_b_frimef(nlon_regional*nlat_regional)

    real(r_single) work_b_qi(nlon_regional*nlat_regional)
    real(r_single) work_b_qs(nlon_regional*nlat_regional)
    real(r_single) work_b_qr(nlon_regional*nlat_regional)
    real(r_single) work_b_qw(nlon_regional*nlat_regional)

    real(r_single) :: f_rimef, f_ice, f_rain, wc, qi, qs, qr, qw
    logical good_var_loc

    mm1=mype+1

    if(mype==mype_io) then

!            read field from file with nemsio

       call nemsio_readrecv(gfile,trim(varname_frain),trim(vartype),lev,work_b_frain,iret=iret)
       call nemsio_readrecv(gfile,trim(varname_fice),trim(vartype),lev,work_b_fice,iret=iret)
       call nemsio_readrecv(gfile,trim(varname_clwmr),trim(vartype),lev,work_b_clwmr,iret=iret)
       call nemsio_readrecv(gfile,trim(varname_frimef),trim(vartype),lev,work_b_frimef,iret=iret)
       if(iret /= 0) then
         write(6,*)'reading frimef or other variables with errors, stop'
         stop
       endif


       do n=1,nlon_regional*nlat_regional
         f_rimef=work_b_frimef(n)
         f_rain=work_b_frain(n)
         f_ice=work_b_fice(n)
         wc=work_b_clwmr(n)
         call fraction2variablenew(f_ice,f_rain,f_rimef,wc,qi,qs,qr,qw)
         work_b_qi(n)=qi
         work_b_qs(n)=qs
         work_b_qr(n)=qr
         work_b_qw(n)=qw
       end do

       if(iret==0) then
!         work_saved=work_b

!         interpolate to analysis grid

          call nmmb_h_to_a(work_b_qi,work_a_qi)
          call nmmb_h_to_a(work_b_qs,work_a_qs)
          call nmmb_h_to_a(work_b_qr,work_a_qr)
          call nmmb_h_to_a(work_b_qw,work_a_qw)


!        scatter to subdomains

          do n=1,itotsub
             i=ltosi_s(n)
             j=ltosj_s(n)
             work_qi(n)=work_a_qi(i,j)
             work_qs(n)=work_a_qs(i,j)
             work_qr(n)=work_a_qr(i,j)
             work_qw(n)=work_a_qw(i,j)
          end do
       end if
    end if

    call mpi_bcast(iret,1,mpi_integer4,mype_io,mpi_comm_world,ierror)
    good_var_loc=.true.
    if(iret/=0) then
       good_var_loc=.false.
       if(mype==0) then
          write(6,*)'  problem reading varname=',trim(varname_frain),', vartype=',trim(vartype),', Status = ',iret
          if(.not.present(good_var)) call stop2(74)
       end if
    end if
    if(present(good_var)) good_var=good_var_loc

    if(good_var_loc) then
      call mpi_scatterv(work_qi,ijn_s,displs_s,mpi_rtype, &
                   var_qi,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
      call mpi_scatterv(work_qs,ijn_s,displs_s,mpi_rtype, &
                   var_qs,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
      call mpi_scatterv(work_qr,ijn_s,displs_s,mpi_rtype, &
                   var_qr,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
      call mpi_scatterv(work_qw,ijn_s,displs_s,mpi_rtype, &
                   var_qw,ijn_s(mm1),mpi_rtype,mype_io,mpi_comm_world,ierror)
    end if

  end subroutine gsi_nemsio_read_fractionnew
  subroutine gsi_nemsio_write_fractionnew(varname_frain,varname_fice,varname_frimef,vartype,lev,var_s,var_i,var_r,var_l,mype,mype_io)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    gsi_nemsio_write_fraction
!   pgrmmr:    Shun Liu
!
! abstract:
!
! program history log:
!   2015-05-12  S.Liu - copy from gsi_nemsio_write and modify to handle NMMB hydrometor fraction variable
!   2016        T. Lei- to add the frimef to be written out
!   input argument list:
!    varname,vartype,gridtype
!    lev
!    add_saved
!    mype     - mpi task id
!    mype_io
!
!   output argument list:
!    var_frain, var_fice
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use mpimod, only:        mpi_rtype,mpi_comm_world,ierror
    use gridmod, only:       lat2,lon2,nlon,nlat,lat1,lon1
    use gridmod, only:       ijn,displs_g,itotsub,iglobal
    use general_commvars_mod, only: ltosi,ltosj
    use nemsio_module, only: nemsio_writerecv
    use mod_nmmb_to_a, only: nmmb_a_to_h,nmmb_a_to_v
    implicit none

    character(*)   ,intent(in   ) :: varname_frain,varname_fice,varname_frimef,vartype      ! gridtype='H' or 'V'
    integer(i_kind),intent(in   ) :: lev              !   vertical level of desired variable
    real(r_kind)   ,intent(in   ) :: var_i(lat2,lon2), var_r(lat2,lon2), var_l(lat2,lon2), var_s(lat2,lon2)
    integer(i_kind),intent(in   ) :: mype,mype_io
!   logical        ,intent(in   ) :: add_saved

    integer(i_kind) i,iret,j,mm1,n
    real(r_kind) work_s(itotsub),work_sub_s(lat1,lon1)
    real(r_kind) work_a_s(nlat,nlon)
    real(r_single) work_b_s(nlon_regional*nlat_regional)

    real(r_kind) work_i(itotsub),work_sub_i(lat1,lon1)
    real(r_kind) work_a_i(nlat,nlon)
    real(r_single) work_b_i(nlon_regional*nlat_regional)

    real(r_kind) work_r(itotsub),work_sub_r(lat1,lon1)
    real(r_kind) work_a_r(nlat,nlon)
    real(r_single) work_b_r(nlon_regional*nlat_regional)

    real(r_kind) work_l(itotsub),work_sub_l(lat1,lon1)
    real(r_kind) work_a_l(nlat,nlon)
    real(r_single) work_b_l(nlon_regional*nlat_regional)

    real(r_single) work_b_frain(nlon_regional*nlat_regional)
    real(r_single) work_b_fice(nlon_regional*nlat_regional)
    real(r_single) work_b_frimef(nlon_regional*nlat_regional)
    real(r_single) qfs,qfi,qfr,qfw,f_rain,f_ice,f_rimef

    mm1=mype+1

    do i=1,lon1
       do j=1,lat1
          work_sub_s(j,i)=var_s(j+1,i+1)
          work_sub_i(j,i)=var_i(j+1,i+1)
          work_sub_r(j,i)=var_r(j+1,i+1)
          work_sub_l(j,i)=var_l(j+1,i+1)
       end do
    end do
    call mpi_gatherv(work_sub_s,ijn(mm1),mpi_rtype, &
                           work_s,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    call mpi_gatherv(work_sub_i,ijn(mm1),mpi_rtype, &
                           work_i,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    call mpi_gatherv(work_sub_r,ijn(mm1),mpi_rtype, &
                           work_r,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    call mpi_gatherv(work_sub_l,ijn(mm1),mpi_rtype, &
                           work_l,ijn,displs_g,mpi_rtype,mype_io,mpi_comm_world,ierror)
    if(mype==mype_io) then
       do n=1,iglobal
          i=ltosi(n)
          j=ltosj(n)
          work_a_s(i,j)=work_s(n)
          work_a_i(i,j)=work_i(n)
          work_a_r(i,j)=work_r(n)
          work_a_l(i,j)=work_l(n)
       end do

       call nmmb_a_to_h(work_a_s,work_b_s)
       call nmmb_a_to_h(work_a_i,work_b_i)
       call nmmb_a_to_h(work_a_r,work_b_r)
       call nmmb_a_to_h(work_a_l,work_b_l)

!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_t,work_b_i)
!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_i,work_b_i)
!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_r,work_b_r)
!      if(trim(gridtype)=='V') call nmmb_a_to_v(work_a_l,work_b_l)

!      if(add_saved) work_b_t=work_b_t+work_saved_t
!      if(add_saved) work_b_i=work_b_i+work_saved_i
!      if(add_saved) work_b_r=work_b_r+work_saved_r
!      if(add_saved) work_b_l=work_b_l+work_saved_l
       do n=1,nlon_regional*nlat_regional
             qfs=work_b_s(n)
           qfi=work_b_i(n)
           qfr=work_b_r(n)
           qfw=work_b_l(n)
           call variable2fractionnew(qfs, qfi, qfr, qfw, f_ice, f_rain,f_rimef)
           work_b_frain(n)=f_rain
           work_b_fice(n)=f_ice
           work_b_frimef(n)=f_rimef
!          work_b_frain(n)=qfr
!          work_b_fice(n)=qfw
       end do

       call nemsio_writerecv(gfile,trim(varname_frain),trim(vartype),lev,work_b_frain,iret=iret)
       call nemsio_writerecv(gfile,trim(varname_fice),trim(vartype),lev,work_b_fice,iret=iret)
       call nemsio_writerecv(gfile,trim(varname_frimef),trim(vartype),lev,work_b_frimef,iret=iret)

       if(iret/=0) then
          write(6,*)'  problem writing varname=',trim(varname_frain),', vartype=',trim(vartype),', Status = ',iret
          call stop2(74)
       end if
    end if

  end subroutine gsi_nemsio_write_fractionnew
  Subroutine fraction2variablenew(f_ice,f_rain,f_rimef, wc, qi,qs,qr,qw)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor
! analysis
!
!   PRGMMR: Ting Lei          ORG: EMC/NCEP        DATE: 2016
!
! ABSTRACT:
!  This subroutine convert fraction to qi, qs, qr, qw exactly
!  following their theorectical formula in NMMB ferrier-Algo scheme 
!  and, the exact physical meaning of qi, qs, qr, qw are not considerred
!  and are only used as the intermidiate variables
!
! PROGRAM HISTORY LOG:
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INTPUT:
!     wc: summation of qi, qr and qw 
!     f_ice     -  fraction of condensate in form of ice
!     f_rain    -  fraction of liquid water in form of rain
!     f_rimef   -  ratio of total ice growth to deposition groth
!   OUTPUT
!     qi    -  
!     qs    -  
!     qr    -  
!     qw    -  
!clt CW=QC+QR+QS
!    QS=F_ICE*CW
!   QR=F_RAIN*(1-F_ICE)*CW
!   QC=(1-F_RAIN)*(1-F_ICE)*CW
!   QG(qi in the above)=QS*F_RIMEF
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  WCOSS at NOAA/ESRL - college park, DC
!
!$$$

 use kinds, only: r_kind,r_single

   real(r_single)  qi,qs, qr, qw, wc
   real(r_single) f_ice, f_rain,f_rimef
   real(r_single) onemf_ice, onemf_rain
    
   onemf_ice=1.0_r_single-f_ice
   onemf_rain=1.0_r_single-f_rain
   
   if(wc > 0.0_r_single) then

     if(f_ice>1.0_r_single) f_ice=1.0_r_single
     if(f_ice<0.0_r_single) f_ice=0.0_r_single
     if(f_rain>1.0_r_single) f_rain=1.0_r_single
     if(f_rain<0.0_r_single) f_rain=0.0_r_single
     qs=f_ice*wc
     qr=f_rain*onemf_ice*wc
     qw=onemf_rain*onemf_ice*wc
     qi=qs*f_rimef
else
   qi=0.0_r_single; qs=0.0_r_single; qr=0.0_r_single; qw=0.0_r_single
   end if

  end subroutine fraction2variablenew
  subroutine variable2fractionnew( qs,qi, qr, qw, f_ice, f_rain,f_rimef)
!clt modified from variable2fraction, see explanation in fration2variablenew

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  gsdcloudanalysis      driver for generalized cloud/hydrometeor analysis
!
!   PRGMMR: Ting Lei          ORG: EMC/NCEP        DATE: 2016
!
! ABSTRACT:
!  This subroutine qi qr qw and qs  to fraction
!  following their theorectical formula in NMMB ferrier-Algo scheme 
!  and, the exact physical meaning of qi, qs, qr, qw are not considerred
!  and are only used as the intermidiate variables
!
! PROGRAM HISTORY LOG:
!
!
!   input argument list:
!     mype     - processor ID that does this IO
!
!   output argument list:
!
! USAGE:
!   INPUT
!     qi    -  
!     qi    -  
!     qr    -
!     qw    -  
!   OUTPUT:
!     f_ice     -  fraction of condensate in form of ice
!     f_rain    -  fraction of liquid water in form of rain
!     f_rimef   -  ratio of total ice growth to deposition groth
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  WCOSS at NOAA/ESRL - college park, DC
!
!$$$

 use kinds, only: r_kind,r_single

   real(r_single)  qi, qr, qw, wc, dum
   real(r_single)  qs 
   real(r_single) f_ice, f_rain,f_rimef
   real(r_single),parameter:: epsq=1.e-12_r_single
   real(r_single) onemf_ice

   wc=qs+qr+qw
   if(wc > 0.0_r_single) then
     if(qs<epsq)then 
           f_ice=0.0_r_single
     else 
           dum=qs/wc
           if(dum<1.0_r_single) then
             f_ice=dum
           else
             f_ice=1.0_r_single
           end if
     end if
     onemf_ice=1.0_r_single-f_ice
     if(qr < epsq) then
           f_rain=0.0_r_single
     else
            dum=qr/(onemf_ice*wc)
           if(dum<1.0_r_single) then
             f_rain=dum
           else
             f_rain=1.0_r_single
            endif
     end if
     if(qi< epsq) then
           f_rimef=1.0_r_single
      else
           if(qs>epsq) then
           f_rimef=min(qi/qs,50.0_r_single) 
           else
           f_rimef=1.0_r_single !cltthinkdeb
          endif
       
    endif
        


   else
           f_rain=0.0_r_single
           f_ice=0.0_r_single
           f_rimef=1.0_r_single
   end if

  end subroutine variable2fractionnew


end module gsi_nemsio_mod
