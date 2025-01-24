subroutine read_files(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_files       get info about atm & sfc guess files
!   prgmmr: derber           org: np23                date: 2002-11-14
!
! abstract:  This routine determines how many global atmospheric and
!            surface guess files are present.  The valid time for each
!            guess file is determine.  The time are then sorted in
!            ascending order.  This information is broadcast to all
!            mpi tasks.
!
! program history log:
!   2002-11-14  derber
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-12-02  treadon - replace mpe_ibcast (IBM extension) with
!                         standard mpi_bcast
!   2005-01-27  treadon - make use of sfcio module
!   2005-02-18  todling - no need to read entire sfc file; only head needed
!   2005-03-30  treadon - clean up formatting of write statements
!   2006-01-09  treadon - use sigio to read gfs spectral coefficient file header
!   2007-03-01  tremolet - measure time from beginning of assimilation window
!   2007-04-17  todling  - getting nhr_assimilation from gsi_4dvar
!   2008-05-27  safford - rm unused vars
!   2009-01-07  todling - considerable revamp (no pre-assigned dims)
!   2009-08-26  li      - add variables to handle nst guess files
!   2010-04-20  jing    - set hrdifsig_all and hrdifsfc_all for non-ESMF cases.
!   2010-12-06  Huang   - make use of nemsio_module to check whether atm and sfc files
!                         are in NEMSIO format and get header informaion 'lpl'
!   2010-12-16  li      - (1) set nfldnst, ntguesnst, ifilenst, hrdifnst, hrdifnst_all for nst files. 
!                         (2) add zero initialization of nfldsfc
!   2011-04-04  Huang   - change looping over 0,99 to find existed sigf and sfcf
!                         twice. Use fcst_hr_sig and fcst_hr_sfc to store info of
!                         files found in first loop of 0,99. Use nfldsig and nfldsfc
!                         to access needed sigf and sfcf w/ fcst_hr_sig and *_sfc.
!   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!   2017-09-08  li      - add sfcnst_comb to get nfldnst and control when sfc & nst combined 
!   2019-03-21  Wei/Martin - add capability to read in aerosol guess from NEMS
!   2019-09-24  martin  - add support for use_gfs_ncio
!   2020-07-08  Wei     - fix the capability to count external aerosol files
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!
!   comments:
!     The difference of time Info between operational GFS IO (gfshead%, sfc_head%),
!      analysis time (iadate), and NEMSIO (idate=)
!
!       gfshead & sfc_head            NEMSIO Header           Analysis time (obsmod)
!       ===================   ============================  ==========================
!         %idate(1)  Hour     idate(1)  Year                iadate(1)  Year
!         %idate(2)  Month    idate(2)  Month               iadate(2)  Month
!         %idate(3)  Day      idate(3)  Day                 iadate(3)  Day
!         %idate(4)  Year     idate(4)  Hour                iadate(4)  Hour
!                             idate(5)  Minute              iadate(5)  Minute
!                             idate(6)  Scaled seconds
!                             idate(7)  Seconds multiplier
!
!     The difference of header forecasting hour Info bewteen operational GFS IO
!      (gfshead%, sfc_head%) and NEMSIO
!
!           gfshead & sfc_head                NEMSIO Header
!       ==========================     ============================
!       %fhour  FCST Hour (r_kind)     nfhour     FCST Hour (i_kind)
!                                      nfminute   FCST Mins (i_kind)
!                                      nfsecondn  FCST Secs (i_kind) numerator
!                                      nfsecondd  FCST Secs (i_kind) denominator
!
!       %fhour = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
!                real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_single,i_kind,i_llong
  use mpimod, only: mpi_rtype,mpi_comm_world,ierror,npe,mpi_itype
  use guess_grids, only: nfldsig,nfldsfc,nfldnst,ntguessig,ntguessfc,ntguesnst,&
       ifilesig,ifilesfc,ifilenst,hrdifsig,hrdifsfc,hrdifnst,create_gesfinfo
  use guess_grids, only: hrdifsig_all,hrdifsfc_all,hrdifnst_all
  use guess_grids, only: nfldaer, ntguesaer, ifileaer, hrdifaer, hrdifaer_all !for aerosol
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen,nhr_assimilation,nhr_obsbin
  use hybrid_ensemble_parameters, only: ntlevs_ens
  use gridmod, only: nlat_sfc,nlon_sfc,lpl_gfs,dx_gfs,use_gfs_nemsio,sfcnst_comb,use_gfs_ncio
  use constants, only: zero,r60inv,r60,r3600,i_missing
  use obsmod, only: iadate
  use gsi_nstcouplermod, only: nst_gsi
  use sfcio_module, only: sfcio_head,sfcio_sropen,&
       sfcio_sclose,sfcio_srhead
  use nstio_module, only: nstio_head,nstio_sropen,&
       nstio_srclose,nstio_srhead
  use sigio_module, only: sigio_head,sigio_sropen,&
       sigio_sclose,sigio_srhead
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_getheadvar
  use read_obsmod, only: gsi_inquire
  use gsi_io, only: verbose
  use module_ncio, only: Dataset, Dimension, open_dataset, get_dim, &
                                read_vardata, get_idate_from_time_units, &
                                close_dataset
  use chemmod, only: lread_ext_aerosol
  
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local parameters
  integer(i_kind),parameter:: lunsfc=11
  integer(i_kind),parameter:: lunatm=12
  integer(i_kind),parameter:: lunnst=13
  integer(i_kind),parameter:: num_lpl=2000
  integer(i_kind),parameter:: max_file = 100
  real(r_kind),parameter:: r0_001=0.001_r_kind

! Declare local variables
  logical(4) fexist
  logical:: present
  character(6) filename
  integer(i_kind) i,j,iwan,npem1,iret
  integer(i_kind) nhr_half,ihr
  integer(i_kind) iamana(4) ! changed to 4 from 3 for aer files
  integer(i_kind) nminanl,nmings,nming2,ndiff
  integer(i_kind),dimension(4):: idateg
  integer(i_kind),dimension(2):: i_ges
  integer(i_kind),allocatable,dimension(:):: nst_ges
  integer(i_kind),dimension(5):: idate5
  integer(i_kind),dimension(6):: idate6
  integer(i_kind),dimension(num_lpl):: lpl_dum
  integer(i_kind),dimension(7):: idate
  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  integer(i_kind),dimension(:,:),allocatable:: irec
  integer(i_llong) :: lenbytes
  real(r_single) hourg4
  real(r_kind), allocatable, dimension(:) :: fhour
  real(r_kind) hourg,t4dv
  real(r_kind),allocatable,dimension(:,:):: time_atm
  real(r_kind),allocatable,dimension(:,:):: time_sfc
  real(r_kind),allocatable,dimension(:,:):: time_nst
  real(r_kind),allocatable,dimension(:,:):: time_aer

  type(sfcio_head):: sfc_head
  type(sigio_head):: sigatm_head
  type(nstio_head):: nst_head
  type(nemsio_gfile) :: gfile_atm,gfile_sfc,gfile_nst,gfile_aer
  logical :: print_verbose
  type(Dataset) :: atmges, sfcges, nstges
  type(Dimension) :: ncdim


  print_verbose=.false.
  if(verbose)print_verbose=.true.
!-----------------------------------------------------------------------------
! Initialize variables
  nhr_half=nhr_assimilation/2
  if(nhr_half*2 < nhr_assimilation) nhr_half=nhr_half+1
  npem1=npe-1

  fexist=.true.
  nfldsig=0
  nfldsfc=0
  nfldnst=0
  nfldaer=0
  iamana=0

! Check for non-zero length atm, sfc, aer, and nst files on single task
  if(mype==npem1)then
     allocate( irec(max_file,4) )
     irec=i_missing

! Check for atm files with non-zero length
     do i=0,max_file-1
        write(filename,'(''sigf'',i2.2)')i
        call gsi_inquire(lenbytes,fexist,filename,mype)
        if(fexist .and. lenbytes>0) then
           nfldsig=nfldsig+1
           irec(nfldsig,1) = i
        end if
     enddo
     if(nfldsig==0) then
        write(6,*)'READ_FILES: ***ERROR*** NO atm fields; aborting'
        call stop2(169)
     end if

! Check for sfc files with non-zero length
     do i=0,max_file-1
        write(filename,'(''sfcf'',i2.2)')i
        call gsi_inquire(lenbytes,fexist,filename,mype)
        if(fexist .and. lenbytes>0) then
           nfldsfc=nfldsfc+1
           irec(nfldsfc,2) = i
        end if
     enddo
     if(nfldsfc==0) then
        write(6,*)'READ_FILES: ***ERROR* NO sfc fields; aborting'
        call stop2(170)
     end if

     allocate(time_atm(nfldsig,2),time_sfc(nfldsfc,2))

     if(nst_gsi > 0 ) then  ! nst application is an option
!    Check for nsf files with non-zero length
        if ( sfcnst_comb ) then
           nfldnst = nfldsfc
        else
           do i=0,max_file-1
              write(filename,'(''nstf'',i2.2)')i
              call gsi_inquire(lenbytes,fexist,filename,mype)
              if(fexist .and. lenbytes>0) then
                 nfldnst=nfldnst+1
                 irec(nfldnst,3) = i
              end if
           enddo
           if(nfldnst==0) then
              write(6,*)'READ_FILES: ***ERROR*** NO nst fields; aborting'
              call stop2(170)
           end if
        end if

        allocate(time_nst(nfldnst,2))
     end if

     if(lread_ext_aerosol) then
!    Check for aer files with non-zero length
        do i=0,max_file-1
           write(filename,'(''aerf'',i2.2)')i
           call gsi_inquire(lenbytes,fexist,filename,mype)
           if(fexist .and. lenbytes>0) then
              nfldaer=nfldaer+1
              irec(nfldaer,4) = i
           end if
        enddo
        if(nfldaer==0) then
           write(6,*)'READ_FILES: ***ERROR*** NO aer fields; aborting'
           call stop2(170)
        end if

        allocate(time_aer(nfldaer,2))
     end if

! Let a single task query the guess files.

!    Convert analysis time to minutes relative to fixed date
     call w3fs21(iadate,nminanl)
     write(6,*)'READ_FILES:  analysis date,minutes ',iadate,nminanl

!    Check for consistency of times from atmospheric guess files.
     iwan=0
     do i=1,nfldsig
        write(filename,'(''sigf'',i2.2)')irec(i,1)
        if(print_verbose)write(6,*)'READ_FILES:  process ',trim(filename)
        if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then
           call sigio_sropen(lunatm,filename,iret)
           call sigio_srhead(lunatm,sigatm_head,iret)
           hourg4=sigatm_head%fhour
           idateg=sigatm_head%idate
           call sigio_sclose(lunatm,iret)
        else if (use_gfs_ncio) then
           atmges = open_dataset(filename,errcode=iret)
           if (iret /=0 .and. mype==0) &
                write(6,*)'READ_FILES: ***WARNING*** problem reading atm file ',trim(filename),iret 
           idate6 = get_idate_from_time_units(atmges) 
           call read_vardata(atmges, 'time', fhour)
           hourg4 = real(nint(fhour(1)),r_kind) ! going to make this nearest integer for now
           idateg(1) = idate6(4)
           idateg(2) = idate6(2)
           idateg(3) = idate6(3)
           idateg(4) = idate6(1)
           call close_dataset(atmges)
        else
           call nemsio_init(iret=iret)
           call nemsio_open(gfile_atm,filename,'READ',iret=iret)
           idate         = i_missing
           nfhour        = i_missing; nfminute      = i_missing
           nfsecondn     = i_missing; nfsecondd     = i_missing
           call nemsio_getfilehead(gfile_atm, nfhour=nfhour, nfminute=nfminute, &
              nfsecondn=nfsecondn, nfsecondd=nfsecondd, idate=idate, iret=iret)
           call nemsio_close(gfile_atm,iret=iret)
           if ( nfhour == i_missing .or. nfminute == i_missing .or. &
                nfsecondn == i_missing .or. nfsecondd == i_missing ) then
              write(6,*)'READ_FILES: ***ERROR*** some forecast hour info ', &
                 'are not defined in ', trim(filename)
              write(6,*)'READ_FILES: nfhour, nfminute, nfsecondn, and nfsecondd = ', &
                 nfhour, nfminute, nfsecondn, nfsecondd
              call stop2(80)
           endif

           hourg4 = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
                    real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
           idateg(1) = idate(4)  !hour
           idateg(2) = idate(2)  !month
           idateg(3) = idate(3)  !day
           idateg(4) = idate(1)  !year
        endif

        hourg = hourg4
        idate5(1)=idateg(4); idate5(2)=idateg(2)
        idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=0
        call w3fs21(idate5,nmings)
        nming2=nmings+60*hourg
        write(6,*)'READ_FILES:  atm guess file ',filename,hourg,idateg,nming2
        t4dv=real((nming2-iwinbgn),r_kind)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle
        else
           ndiff=nming2-nminanl
           if(abs(ndiff) > 60*nhr_half ) cycle
        endif
        iwan=iwan+1
        if(nminanl==nming2) iamana(1)=iwan
        time_atm(iwan,1) = t4dv
        time_atm(iwan,2) = irec(i,1)+r0_001
     end do

!    Check for consistency of times from surface guess files.
     iwan=0
     do i=1,nfldsfc
        write(filename,'(''sfcf'',i2.2)')irec(i,2)
        if(print_verbose)write(6,*)'READ_FILES:  process ',trim(filename)        
        if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then
           call sfcio_sropen(lunsfc,filename,iret)
           call sfcio_srhead(lunsfc,sfc_head,iret)
           hourg4=sfc_head%fhour
           idateg=sfc_head%idate
           i_ges(1)=sfc_head%lonb
           i_ges(2)=sfc_head%latb+2
           if(sfc_head%latb/2>num_lpl)then
              write(6,*)'READ_FILES: increase dimension of variable lpl_dum'
              call stop2(80)
           endif
           lpl_dum=0
           lpl_dum(1:sfc_head%latb/2)=sfc_head%lpl
           call sfcio_sclose(lunsfc,iret)
           if(i == 1 .and. print_verbose)write(6,*)' READ_FILES: in sfcio sfc_head%lpl = ', sfc_head%lpl
        else if (use_gfs_ncio) then
           sfcges = open_dataset(filename,errcode=iret)
           if (iret /=0 .and. mype==0) &
                write(6,*)'READ_FILES: ***WARNING*** problem reading sfc file ',trim(filename),iret
           ncdim = get_dim(sfcges, 'grid_xt'); sfc_head%lonb = ncdim%len
           ncdim = get_dim(sfcges, 'grid_yt'); sfc_head%latb = ncdim%len
           idate6 = get_idate_from_time_units(sfcges) 
           call read_vardata(sfcges, 'time', fhour)
           hourg4 = real(nint(fhour(1)),r_kind) ! going to make this nearest integer for now
           idateg(1) = idate6(4)
           idateg(2) = idate6(2)
           idateg(3) = idate6(3)
           idateg(4) = idate6(1)
           i_ges(1)=sfc_head%lonb
           i_ges(2)=sfc_head%latb+2
           if((sfc_head%latb+1)/2>num_lpl)then
              write(6,*)'READ_FILES: increase dimension of variable lpl_dum'
              call stop2(80)
           endif
           if ( (sfc_head%latb+1)/2 /= sfc_head%latb/2 ) then
              write(6,*) 'READ_FILES: ****WARNING**** (sfc_head%latb+1)/2 = ', &
                 (sfc_head%latb+1)/2, 'sfc_head%latb/2 = ', sfc_head%latb/2
           end if
           if (allocated(sfc_head%lpl)) deallocate(sfc_head%lpl)
           allocate(sfc_head%lpl((sfc_head%latb+1)/2))
           sfc_head%lpl=sfc_head%lonb
           call close_dataset(sfcges)
           lpl_dum=0
           lpl_dum(1:sfc_head%latb/2)=sfc_head%lpl
           deallocate(sfc_head%lpl)
        else
           call nemsio_init(iret=iret)
           call nemsio_open(gfile_sfc,filename,'READ',iret=iret)
           idate         = i_missing
           nfhour        = i_missing; nfminute      = i_missing
           nfsecondn     = i_missing; nfsecondd     = i_missing
           call nemsio_getfilehead(gfile_sfc, nfhour=nfhour, nfminute=nfminute,  &
              nfsecondn=nfsecondn, nfsecondd=nfsecondd, idate=idate, &
              dimx=sfc_head%lonb, dimy=sfc_head%latb, iret=iret)
           if ( nfhour == i_missing .or. nfminute == i_missing .or. &
                nfsecondn == i_missing .or. nfsecondd == i_missing ) then
              write(6,*)'READ_FILES: ***ERROR*** some forecast hour info ', &
                 'are not defined in ', trim(filename)
              write(6,*)'READ_FILES: nfhour, nfminute, nfsecondn, and nfsecondd = ', &
                 nfhour, nfminute, nfsecondn, nfsecondd
              call stop2(80)
           endif
           hourg4   = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
                      real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
           idateg(1) = idate(4)  !hour
           idateg(2) = idate(2)  !month
           idateg(3) = idate(3)  !day
           idateg(4) = idate(1)  !year
           i_ges(1)=sfc_head%lonb
           i_ges(2)=sfc_head%latb+2
           if((sfc_head%latb+1)/2>num_lpl)then
              write(6,*)'READ_FILES: increase dimension of variable lpl_dum'
              call stop2(80)
           endif
           if ( (sfc_head%latb+1)/2 /= sfc_head%latb/2 ) then
              write(6,*) 'READ_FILES: ****WARNING**** (sfc_head%latb+1)/2 = ', &
                 (sfc_head%latb+1)/2, 'sfc_head%latb/2 = ', sfc_head%latb/2
           end if
           if (allocated(sfc_head%lpl)) deallocate(sfc_head%lpl)
           allocate(sfc_head%lpl((sfc_head%latb+1)/2))
           sfc_head%lpl=i_missing
           call nemsio_getheadvar(gfile_sfc,'lpl',sfc_head%lpl,iret=iret)
           if ( iret /= 0 ) then
              write(6,*)' READ_FILES: ****ERROR**** reading sfc_head%lpl from ', &
                 trim(filename),', iret = ', iret
              write(6,*)' sfc_head%lpl = ', sfc_head%lpl(1:20)
              call nemsio_close(gfile_sfc,iret=iret)
              call stop2(80)
           end if
           call nemsio_close(gfile_sfc,iret=iret)
           lpl_dum=0
           lpl_dum(1:sfc_head%latb/2)=sfc_head%lpl
           deallocate(sfc_head%lpl)
        endif
        hourg = hourg4
        idate5(1)=idateg(4); idate5(2)=idateg(2)
        idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=0
        call w3fs21(idate5,nmings)
        nming2=nmings+60*hourg
        write(6,*)'READ_FILES:  sfc guess file ',filename,hourg,idateg,nming2
        t4dv=real((nming2-iwinbgn),r_kind)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) cycle
        else
           ndiff=nming2-nminanl
           if(abs(ndiff) > 60*nhr_half ) cycle
        endif
        iwan=iwan+1
        if(nminanl==nming2) iamana(2)=iwan
        time_sfc(iwan,1) = t4dv
        time_sfc(iwan,2) = irec(i,2)+r0_001
     end do

!    Check for consistency of times from nst guess files.
     if ( nst_gsi > 0 ) then
        allocate(nst_ges(2))
        if ( sfcnst_comb ) then
           time_nst = time_sfc
           iamana(3) = iamana(2)
        else
           iwan=0
           do i=1,nfldnst
              write(filename,'(''nstf'',i2.2)')irec(i,3)
              write(6,*)'READ_FILES:  process ',trim(filename)
              if ( (.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio) ) then
                 call nstio_sropen(lunnst,filename,iret)
                 call nstio_srhead(lunnst,nst_head,iret)
                 hourg4=nst_head%fhour
                 idateg=nst_head%idate
                 nst_ges(1)=nst_head%lonb
                 nst_ges(2)=nst_head%latb+2
              else if (use_gfs_ncio) then
                 nstges = open_dataset(filename)
                 ncdim = get_dim(nstges, 'grid_xt'); nst_head%lonb = ncdim%len
                 ncdim = get_dim(nstges, 'grid_yt'); nst_head%latb = ncdim%len
                 idate6 = get_idate_from_time_units(nstges) 
                 call read_vardata(nstges, 'time', fhour)
                 hourg4 = fhour(1)
                 idateg(1) = idate6(4)
                 idateg(2) = idate6(2)
                 idateg(3) = idate6(3)
                 idateg(4) = idate6(1)
              else
                 call nemsio_init(iret=iret)
                 call nemsio_open(gfile_nst,filename,'READ',iret=iret)
                 idate         = i_missing
                 nfhour        = i_missing; nfminute      = i_missing
                 nfsecondn     = i_missing; nfsecondd     = i_missing
                 call nemsio_getfilehead(gfile_nst, nfhour=nfhour, nfminute=nfminute,  &
                    nfsecondn=nfsecondn, nfsecondd=nfsecondd, idate=idate, &
                    dimx=nst_head%lonb, dimy=nst_head%latb, iret=iret)
                 call nemsio_close(gfile_nst,iret=iret)
                 if ( nfhour == i_missing .or. nfminute == i_missing .or. &
                      nfsecondn == i_missing .or. nfsecondd == i_missing ) then
                    write(6,*)'READ_FILES: ***ERROR*** some forecast hour info ', &
                         'are not defined in ', trim(filename)
                    write(6,*)'READ_FILES: nfhour, nfminute, nfsecondn, and nfsecondd = ', &
                         nfhour, nfminute, nfsecondn, nfsecondd
                    call stop2(80)
                 endif
                 hourg4   = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
                            real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
                 idateg(1) = idate(4)  !hour
                 idateg(2) = idate(2)  !month
                 idateg(3) = idate(3)  !day
                 idateg(4) = idate(1)  !year
                 nst_ges(1)=nst_head%lonb
                 nst_ges(2)=nst_head%latb+2
              endif
              if ( i_ges(1) /= nst_ges(1) .or. i_ges(2) /= nst_ges(2) ) then
                 write(6,'(''READ_FILES: sfc file lat,lon '',2i5,'' do not match with nst file lat,lon '',2i5)') &
                    i_ges(2)-2,i_ges(1),nst_ges(2)-2,nst_ges(1)
              call stop2(80)
              end if
              hourg = hourg4
              idate5(1)=idateg(4); idate5(2)=idateg(2)
              idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=0
              call w3fs21(idate5,nmings)
              nming2=nmings+60*hourg
              write(6,*)'READ_FILES:  nst guess file',filename,hourg,idateg,nming2
              t4dv=real((nming2-iwinbgn),r_kind)*r60inv
              if (l4dvar.or.l4densvar) then
                 if (t4dv<zero .OR. t4dv>winlen) cycle
              else
                 ndiff=nming2-nminanl
                 if(abs(ndiff) > 60*nhr_half ) cycle
              endif
              iwan=iwan+1
              if(nminanl==nming2) iamana(3)=iwan
              time_nst(iwan,1) = t4dv
              time_nst(iwan,2) = irec(i,3)+r0_001
           end do
           deallocate(nst_ges)
        endif                       ! if ( sfcnst_comb ) then
     endif                          ! if ( nst_gsi > 0 ) then

!    for external aerosol files only
!    Check for consistency of times from aer guess files.
     if ( lread_ext_aerosol ) then
        write(6,*) 'READ_FILES: nfldaer ', nfldaer
        iwan=0
        do i=1,nfldaer
           write(filename,'(''aerf'',i2.2)')irec(i,4)
           write(6,*)'READ_FILES:  process ',trim(filename)
           if ( .not. use_gfs_nemsio ) then
              write(6,*)'READ_FILES: ***ERROR*** aerosol files only work with nemsio'
           else
              call nemsio_init(iret=iret)
              call nemsio_open(gfile_aer,filename,'READ',iret=iret)
              idate         = i_missing
              nfhour        = i_missing; nfminute      = i_missing
              nfsecondn     = i_missing; nfsecondd     = i_missing
              call nemsio_getfilehead(gfile_aer, nfhour=nfhour, nfminute=nfminute,  &
                 nfsecondn=nfsecondn, nfsecondd=nfsecondd, idate=idate )
              call nemsio_close(gfile_aer,iret=iret)
              if ( nfhour == i_missing .or. nfminute == i_missing .or. &
                   nfsecondn == i_missing .or. nfsecondd == i_missing ) then
                 write(6,*)'READ_FILES: ***ERROR*** some forecast hour info ', &
                      'are not defined in ', trim(filename)
                 write(6,*)'READ_FILES: nfhour, nfminute, nfsecondn, and nfsecondd = ', &
                      nfhour, nfminute, nfsecondn, nfsecondd
                 call stop2(80)
              endif
              hourg4   = real(nfhour,r_kind) + real(nfminute,r_kind)/r60 + &
                         real(nfsecondn,r_kind)/real(nfsecondd,r_kind)/r3600
              idateg(1) = idate(4)  !hour
              idateg(2) = idate(2)  !month
              idateg(3) = idate(3)  !day
              idateg(4) = idate(1)  !year
           endif
           hourg = hourg4
           idate5(1)=idateg(4); idate5(2)=idateg(2)
           idate5(3)=idateg(3); idate5(4)=idateg(1); idate5(5)=0
           call w3fs21(idate5,nmings)
           nming2=nmings+60*hourg
           write(6,*)'READ_FILES:  aer guess file',filename,hourg,idateg,nming2
           t4dv=real((nming2-iwinbgn),r_kind)*r60inv
           if (l4dvar.or.l4densvar) then
              if (t4dv<zero .OR. t4dv>winlen) cycle
           else
              ndiff=nming2-nminanl
              if(abs(ndiff) > 60*nhr_half ) cycle
           endif
           iwan=iwan+1
           if(nminanl==nming2) iamana(4)=iwan
           time_aer(iwan,1) = t4dv
           time_aer(iwan,2) = irec(i,4)+r0_001
        end do
     endif ! if ( lread_ext_aerosol ) then
     deallocate( irec )
  end if

! Broadcast guess file information to all tasks
  call mpi_bcast(nfldsig,1,mpi_itype,npem1,mpi_comm_world,ierror)
  call mpi_bcast(nfldsfc,1,mpi_itype,npem1,mpi_comm_world,ierror)
  print_verbose=.false.
  if(verbose)print_verbose=.true.
  if (nst_gsi > 0) call mpi_bcast(nfldnst,1,mpi_itype,npem1,mpi_comm_world,ierror)
  if (lread_ext_aerosol) call mpi_bcast(nfldaer,1,mpi_itype,npem1,mpi_comm_world,ierror)! for external aerosol files

  if(.not.allocated(time_atm)) allocate(time_atm(nfldsig,2))
  if(.not.allocated(time_sfc)) allocate(time_sfc(nfldsfc,2))

  call mpi_bcast(time_atm,2*nfldsig,mpi_rtype,npem1,mpi_comm_world,ierror)
  call mpi_bcast(time_sfc,2*nfldsfc,mpi_rtype,npem1,mpi_comm_world,ierror)
  if(.not.allocated(time_nst)) allocate(time_nst(nfldnst,2))
  if (nst_gsi > 0 ) call mpi_bcast(time_nst,2*nfldnst,mpi_rtype,npem1,mpi_comm_world,ierror)

! for external aerosol files
  if(lread_ext_aerosol .and. (.not.allocated(time_aer))) allocate(time_aer(nfldaer,2))
  if (lread_ext_aerosol) call mpi_bcast(time_aer,2*nfldaer,mpi_rtype,npem1,mpi_comm_world,ierror)

  call mpi_bcast(iamana,3,mpi_rtype,npem1,mpi_comm_world,ierror)
  call mpi_bcast(i_ges,2,mpi_itype,npem1,mpi_comm_world,ierror)
  nlon_sfc=i_ges(1)
  nlat_sfc=i_ges(2)
  call mpi_bcast(lpl_dum,num_lpl,mpi_itype,npem1,mpi_comm_world,ierror)
  allocate(lpl_gfs(nlat_sfc/2))
  allocate(dx_gfs(nlat_sfc/2))
  lpl_gfs(1)=1  ! singularity at pole
  dx_gfs(1) = 360._r_kind / lpl_gfs(1)
  do j=2,nlat_sfc/2
     lpl_gfs(j)=lpl_dum(j-1)
     dx_gfs(j) = 360._r_kind / lpl_gfs(j)
  enddo

! Allocate space for guess information files
  call create_gesfinfo

! Load time information for atm guess field sinfo into output arrays
  ntguessig = iamana(1)
  do i=1,nfldsig
     hrdifsig(i) = time_atm(i,1)
     ifilesig(i) = nint(time_atm(i,2))
     hrdifsig_all(i) = hrdifsig(i)
  end do
  if(mype == 0) write(6,*)'READ_FILES:  atm fcst files used in analysis  :  ',&
       (ifilesig(i),i=1,nfldsig),(hrdifsig(i),i=1,nfldsig),ntguessig
  if (ntguessig==0) then
     write(6,*)'READ_FILES: ***ERROR*** center atm fcst NOT AVAILABLE: PROGRAM STOPS'
     call stop2(99)
  endif
  if (l4densvar .and. nfldsig/=ntlevs_ens) then
     if (mype==0) then
        write(6,*)'READ_FILES: ***FATAL ERROR*** insufficient atm fcst for 4densvar:  PROGRAM STOPS'
        do i=1,ntlevs_ens
           ihr=nhr_obsbin*(i-1)+nhr_half
           present=.false.
           do j=1,nfldsig
              if (ihr == ifilesig(j)) present=.true.
           end do
           if (.not.present) then
              write(filename,'(''sigf'',i2.2)')ihr
              write(6,*)'READ_FILES: ***FATAL ERROR*** file ',trim(filename),' missing:  PROGRAM STOPS'
           endif
        end do
     endif
     call mpi_barrier(mpi_comm_world,ierror)
     call stop2(99)
  endif

! Load time information for surface guess field info into output arrays
  ntguessfc = iamana(2)
  do i=1,nfldsfc
     hrdifsfc(i) = time_sfc(i,1)
     ifilesfc(i) = nint(time_sfc(i,2))
     hrdifsfc_all(i) = hrdifsfc(i)
  end do
  if(mype == 0) write(6,*)'READ_FILES:  sfc fcst files used in analysis:  ',&
       (ifilesfc(i),i=1,nfldsfc),(hrdifsfc(i),i=1,nfldsfc),ntguessfc
  if (ntguessfc==0) then
     write(6,*)'READ_FILES: ***ERROR*** center sfc fcst NOT AVAILABLE: PROGRAM STOPS'
     call stop2(99)
  endif
  if (l4densvar .and. nfldsfc/=ntlevs_ens) then
     if (mype==0) then
        write(6,*)'READ_FILES: ***FATAL ERROR*** insufficient sfc fcst for 4densvar:  PROGRAM STOPS'
        do i=1,ntlevs_ens
           ihr=nhr_obsbin*(i-1)+nhr_half
           present=.false.
           do j=1,nfldsfc
              if (ihr == ifilesfc(j)) present=.true.
           end do
           if (.not.present) then
              write(filename,'(''sfcf'',i2.2)')ihr
              write(6,*)'READ_FILES: ***FATAL ERROR*** file ',trim(filename),' missing:  PROGRAM STOPS'
           endif
        end do
     endif
     call mpi_barrier(mpi_comm_world,ierror)
     call stop2(99)
  endif
  
  deallocate(time_atm,time_sfc)
  
! Load time information for nst guess field info into output arrays
  ntguesnst = iamana(3)
  if ( nst_gsi > 0 ) then
     if ( sfcnst_comb ) then
        hrdifnst = hrdifsfc
        hrdifnst_all = hrdifnst
        if(mype == 0) write(6,*)'READ_FILES: hrdifnst = hrdifsfc'
     else
        do i=1,nfldnst
           hrdifnst(i) = time_nst(i,1)
           ifilenst(i) = nint(time_nst(i,2))
           hrdifnst_all(i) = hrdifnst(i)
        end do
        if(mype == 0) write(6,*)'READ_FILES:  nst fcst files used in analysis:  ',&
             (ifilenst(i),i=1,nfldnst),(hrdifnst(i),i=1,nfldnst),ntguesnst
        if (ntguesnst==0) then
           write(6,*)'READ_FILES: ***ERROR*** center nst fcst NOT AVAILABLE: PROGRAM STOPS'
           call stop2(99)
        endif
        if (l4densvar .and. nfldnst/=ntlevs_ens) then
           write(6,*)'READ_FILES: ***ERROR*** insufficient nst fcst for 4densvar:  PROGRAM STOPS'
           call stop2(99)
        endif
    endif
    deallocate(time_nst)
  endif

! for external aerosol files
! Load time information for aer guess field info into output arrays
  ntguesaer = iamana(4)
  if ( lread_ext_aerosol ) then
    do i=1,nfldaer
       hrdifaer(i) = time_aer(i,1)
       ifileaer(i) = nint(time_aer(i,2))
       hrdifaer_all(i) = hrdifaer(i)
    end do
    if(mype == 0) write(6,*)'READ_FILES:  aer fcst files used in analysis:  ',&
         (ifileaer(i),i=1,nfldaer),(hrdifaer(i),i=1,nfldaer),ntguesaer
    if (ntguesaer==0) then
       write(6,*)'READ_FILES: ***ERROR*** center aer fcst NOT AVAILABLE: PROGRAM STOPS'
       call stop2(99)
    endif
    if (l4densvar .and. nfldaer/=ntlevs_ens) then
       write(6,*)'READ_FILES: ***ERROR*** insufficient aer fcst for 4densvar: PROGRAM STOPS'
       call stop2(99)
    endif
    deallocate(time_aer)
  endif
! End of routine
  return
end subroutine read_files
