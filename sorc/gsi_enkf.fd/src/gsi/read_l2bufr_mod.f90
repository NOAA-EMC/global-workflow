module read_l2bufr_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    read_l2bufr_mod module for processing level2 radar bufr files
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract: Process level 2 radar bufr files, converting radial wind observations
!             to superobs, which are read by subroutine read_superwinds.
!             Because these files can be very big (5-10 Gb), some additions were
!             made to bufrlib to allow use of mpi-io routines when reading the
!             bufr file.  This reduced processing time from 5-8 minutes to
!             60-90 seconds for a 5Gb test file.
!
! program history log:
!   2005-08-01  parrish
!   2006-04-21  parrish, complete rewrite.
!   2006-05-22  parrish, fix bug which causes infinite loop when no data pass initial checks
!   2007-10-24  parrish  add l2superob_only option
!   2009-04-18  woollen  improve mpi_io interface with bufrlib routines
!   2009-11-24  parrish  change time variable from regional_time (passed from gridmod) to
!                          iadate (passed from obsmod), to prevent all radar data being tossed.
!   2011-07-04  todling  - fixes to run either single or double precision
!   2020-10-23  S.Liu    - fix reading l2rw_bufr issue
!   2020-10-14  xu/sippel - Add options to superob the GRO
!   2021-12-08  s.liu    - change max_num_radars from 150 to 250
!   2021-12-10  s.liu    - fixes to find the index of radar station
!
! subroutines included:
!   sub initialize_superob_radar - initialize superob parameters to defaults
!   sub radar_bufr_read_all      - process input level 2 bufr file and write out superobs
!
! Variable Definitions:
!   def del_azimuth     - azimuth range for superob box  (default 5 degrees)
!   def del_elev        - elevation angle range for superob box  (default .05 degrees)
!   def del_range       - radial range for superob box  (default 5 km)
!   def del_time        - 1/2 time range for superob box  (default .5 hours)
!   def elev_angle_max  - max elevation angle (default of 5 deg recommended by S. Liu)
!   def minnum                  - minimum number of samples needed to make a superob
!   def range_max       - max radial range to use in constructing superobs  (default 100km)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: initialize_superob_radar
  public :: radar_bufr_read_all
! set passed variables to public
  public :: range_max,del_time,l2superob_only,elev_angle_max,del_azimuth
  public :: minnum,del_range,del_elev

  public :: invtllv,radar_sites,radar_box,radar_rmesh,radar_zmesh,radar_pmot

  integer(i_kind) minnum,radar_pmot
  real(r_kind) del_azimuth,del_elev,del_range,del_time,elev_angle_max,range_max,radar_rmesh,radar_zmesh 
  logical l2superob_only,radar_sites,radar_box 

contains

  subroutine initialize_superob_radar
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    initialize_superob_radar
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
    use constants, only: quarter,half,five
    implicit none

    del_azimuth=five            !   (5 degrees)
    del_elev=quarter            !   (.25 degrees for elevation angle bin)
    del_range=5000._r_kind      !  (5 km)
    del_time=half               !   (hours)
    elev_angle_max=five         !  recommended by S. Liu to avoid heavy convection problems
    minnum=50
    range_max=100000._r_kind    !  (100km)
    l2superob_only=.false.
    radar_sites=.false. 
    radar_box=.false. 
    radar_rmesh=10._r_kind 
    radar_zmesh=500._r_kind 

!  radar_pmot of 0,1,2,3 will save different sets of obs output
!  radar_pmot - all obs - thin obs
!  radar_pmot - all obs
!  radar_pmot - use obs
!  radar_pmot - use obs + thin obs

    radar_pmot = 2
  end subroutine initialize_superob_radar

  subroutine radar_bufr_read_all(npe,mype)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radar_bufr_read_all
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!   2010-08-30  treadon - changes for reproducibile results
!   2016-08-12  lippi   - Logic for running single radar test.
!   2016-12-14  lippi   - Read 'l2rwbufr' from the OBS_INPUT table rather than
!                         hardcoding that name into the code. Logic to exit code
!                         if the infile (l2rwbufr) is not present. Also, added
!                         new namelist parameter vadwnd_l2rw_qc.
!
!   input argument list:
!     mype     - mpi task id
!     npe
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$  end documentation block

    use kinds, only: r_double,r_quad
    use constants, only: zero,half,one,two,rearth,deg2rad,rad2deg,zero_quad,one_quad
    use mpimod, only: mpi_comm_world,mpi_min,mpi_sum,mpi_real8,ierror,mpi_real16
    use mpimod, only: mpi_max,mpi_integer4
    use obsmod,only: iadate,ndat,dsis,dfile,dtype
    use qcmod, only: vadwnd_l2rw_qc
    use oneobmod, only: lsingleradar,singleradar
    use mpeu_util, only: IndexSet, IndexSort
    use file_utility, only : get_lun  
    use constants, only: pi,rearth_equator 
    use mpeu_util, only: gettablesize,gettable 
    use gridmod, only: txy2ll 
    use gsi_io, only: verbose
    implicit none

    integer(i_kind),intent(in):: npe,mype

    integer(i_kind),parameter:: max_num_radars=250
    integer(i_kind),parameter:: maxobs=2e8 
    integer(i_kind),parameter:: n_gates_max=4000
    real(r_kind),parameter:: four_thirds = 4.0_r_kind / 3.0_r_kind
    real(r_kind),parameter:: r8     = 8.0_r_kind
    real(r_kind),parameter:: r89_5  = 89.5_r_kind
    real(r_kind),parameter:: r90    = 90.0_r_kind
    real(r_kind),parameter:: r125   = 125.0_r_kind
    real(r_kind),parameter:: r250   = 250.0_r_kind
    real(r_kind),parameter:: r360   = 360.0_r_kind
    real(r_kind),parameter:: r720   = 720.0_r_kind
    real(r_kind),parameter:: r99999 = 99999._r_kind
    real(r_kind),parameter:: rinv60 = 1.0_r_kind/60.0_r_kind
    real(r_double),parameter:: r1e5_double = 1.0e5_r_double


    integer(i_kind) nazbin,nrbin,nelbin
    integer(i_kind) i,ibyte,idate,inbufr,iret,isubset,krad,levs,lundx,n_gates
    integer(i_kind) k,iii,j,jj,jjall,jjj,numzzzz,num_radars_0,iloc
    integer(i_kind) iyref,imref,idref,ihref
    integer(i_kind) iazbin,irbin,ielbin
    integer(i_kind) nminref,nminthis
    integer(i_kind) num_radars,ireadsb,ireadmg,next
    integer(i_kind) nsuper,nsuperall
    integer(i_kind) nthisrad,nthisbins
    integer(i_kind) idups,idups0
    integer(i_kind) radar_count,radar_true,ntot,nlevz 
    real(r_kind) dx,dy 
    real(r_kind) halfpi,twopi,rkm2dg,delat,dgv,rlat_min,rlon_min,dlat_grid,dlon_grid 
    integer(i_kind) mlat,mlonx,ilev,ilat,ilon 
    integer(i_kind) nradials_in,nradials_fail_angmax,nradials_fail_time,nradials_fail_elb
    integer(i_kind) nradials_in1,nradials_fail_angmax1,nradials_fail_time1,nradials_fail_elb1
    integer(i_kind) nobs_in,nobs_badvr,nobs_badsr,nobs_lrbin,nobs_hrbin,nrange_max,irad
    integer(i_kind) nobs_in1,nobs_badvr1,nobs_badsr1,nobs_lrbin1,nobs_hrbin1,nrange_max1
    integer(i_kind) num_radars_max,num_radars_min
    integer(i_kind) idate5(5)
    integer(i_kind),allocatable,dimension(:) :: indx,icount
    integer(i_kind),allocatable,dimension(:,:) :: ibins,ibins2

    real(r_double) hdr(10),hdr2(12),rwnd(3,n_gates_max),rdisttest(n_gates_max)

    real(r_kind) delaz,delel,delr,t
    real(r_kind) ddiffmin,ddiffmin0,distfact,range
    real(r_kind) stn_lat,stn_lon,stn_hgt,stn_az,stn_el
    real(r_kind) timemax,timemin
    real(r_kind) timemax1,timemin1
    real(r_kind) this_stalat,this_stalon,this_stahgt
    real(r_kind) rlon0,clat0,slat0,rlonglob,rlatglob,clat1,caz0,saz0,cdlon,sdlon,caz1,saz1
    real(r_kind) this_stalatr,thisazimuthr,thistiltr
    real(r_kind) thisrange,thisazimuth,thistilt,thisvr,thisvr2
    real(r_kind) corrected_tilt
    real(r_kind) thiserr,thistime,thislat,thislon,corrected_azimuth
    real(r_kind) rad_per_meter,thishgt
    real(r_kind) rlonloc,rlatloc
    real(r_kind) a43,aactual,b,c,selev0,celev0,epsh,erad,h,ha
    real(r_kind) celev,selev,gamma
    real(r_kind) vrmax,vrmin,errmax,errmin
    real(r_kind) vrmaxall,vrminall,errmaxall,errminall
    real(r_kind) delazmmax,rdelaz,rdelr,rdelel
    real(r_kind) delazmmaxall
    real(r_kind) deltiltmaxall,deltiltmax
    real(r_kind) deltiltminall,deltiltmin
    real(r_kind) deldistmaxall,deldistmax
    real(r_kind) deldistminall,deldistmin
    real(r_kind),dimension(max_num_radars) :: stn_lat_table,stn_lon_table, &
                 stn_hgt_table,master_lat_table,master_lon_table,master_hgt_table
    real(r_kind),dimension(max_num_radars*npe) :: stn_lat_table_all,stn_lon_table_all, &
                 stn_hgt_table_all

    real(r_quad) thiscount
    real(r_quad),dimension(6) :: binsx
    real(r_quad),allocatable,dimension(:,:,:) :: bins,bins_work
!            bins(1,...) radial distance
!            bins(2,...) azimuth
!            bins(3,...) elev angle
!            bins(4,...) vr
!            bins(5,...) vr**2
!            bins(6,...) relative time
!            ibins(...) count

!       bins(  :,   :,   :)
!            var  rbin*azbin*elbin rad#
    character(8) chdr,chdr2,subset
    character(10) date
    character(20) infile
    character(4),dimension(max_num_radars):: stn_id_table,master_stn_table
    character(4) stn_id_table_all(max_num_radars*npe)
    character(4) stn_id,this_staid
    character(4*max_num_radars) cstn_id_table,cmaster_stn_table
    equivalence(stn_id_table(1),cstn_id_table)
    equivalence(master_stn_table(1),cmaster_stn_table)
    equivalence (chdr,hdr(1))
    equivalence (chdr2,hdr2(1))
    character(len=*),parameter:: tbname='SUPEROB_RADAR::' 

    logical rite,print_verbose
    logical lradar 
    character(len=256),allocatable,dimension(:):: rtable 
    character(4),allocatable,dimension(:):: rsite 
    integer(i_kind),allocatable,dimension(:):: ruse 

    print_verbose=.false.
    if(verbose) print_verbose=.true.
    if (radar_sites) then 
       open(666,file=trim('gsiparm.anl'),form='formatted')
       call gettablesize(tbname,666,ntot,radar_count)
       allocate(rtable(radar_count),rsite(radar_count),ruse(radar_count)) 
       call gettable(tbname,666,ntot,radar_count,rtable) 
       do i=1,radar_count 
         read(rtable(i),*) rsite(i),ruse(i) 
         if (mype==0) write(*,'(A20,X,A4,X,I3)'),"Radar sites usage: ",rsite(i),ruse(i)
       end do 
    end if 
    ! define infile if using either option for radial winds.
    do i=1,ndat
       if(trim(dtype(i))=='rw'.and.trim(dsis(i))=='l2rw'.and.vadwnd_l2rw_qc)then
          infile=trim(dfile(i)) !l2rwbufr
          exit
       else if(trim(dtype(i))=='rw'.and.trim(dsis(i))=='l2rw'.and..not.vadwnd_l2rw_qc)then
          infile=trim(dfile(i)) !l2rwbufr with no VAD QC
          exit
       else
          infile=''
       end if
    end do
    if(mype==0) write(6,*)'RADAR_BUFR_READ_ALL:  radial wind infile for superobbing = ',infile
    if (infile=='') then
       if(mype==0) write(6,*) '***WARNING*** NOT USING ANY RADIAL WIND OBSERVATIONS!!!' 
       return ! Go back to gsisub if nothing to do.    
    end if
 
    rad_per_meter= one/rearth
    erad = rearth
    nazbin=nint(r360/del_azimuth)
    nrbin=nint(range_max/del_range)
    nelbin=nint(elev_angle_max/del_elev)
    delaz=r360/nazbin
    delr=range_max/nrbin
    delel=elev_angle_max/nelbin
    rdelaz=one/delaz
    rdelr =one/delr
    rdelel=one/delel
    num_radars=0
    do i=1,max_num_radars
       stn_id_table(i)='ZZZZ'
    end do
    stn_lat_table=r99999
    stn_lon_table=r99999
    stn_hgt_table=r99999

    rite = .false.
    if (mype==0) rite=.true.
    
!   Open bufr file with openbf to initialize bufr table, etc in bufrlib
    inbufr=10
    open(inbufr,file=infile,form='unformatted')
    rewind inbufr
    lundx=inbufr
    call openbf(inbufr,'IN',lundx)
    call datelen(10)
    if(l2superob_only) then
       write(date,'( i10)') idate
       read (date,'(i4,3i2)') iyref,imref,idref,ihref
       if(rite) write(6,*)' create superobs only, radar file date = ',iyref,imref,idref,ihref
    else
       iyref=iadate(1)              !????  add mods so can be used in global mode
       imref=iadate(2)
       idref=iadate(3)
       ihref=iadate(4)
       if(rite) write(6,*)' using restart file date = ',iyref,imref,idref,ihref
    end if
    if(rite) write(6,*)'RADAR_BUFR_READ_ALL: analysis time is ',iyref,imref,idref,ihref
    idate5(1)=iyref
    idate5(2)=imref
    idate5(3)=idref
    idate5(4)=ihref
    idate5(5)=0          ! minutes
    call w3fs21(idate5,nminref)

!    Do an initial read of a bit of data to infer what multiplying factor is for 
!    radial distance.  There is a possible ambiguity, where the scaling is either 
!    125 or 250. If the minimum difference between gate distances is 2, then factor 
!    is 125, if = 1 then factor is 250.

    idups=0
    nobs_in=0
    ddiffmin=huge(ddiffmin)
    next=0
    do while(ireadmg(inbufr,subset,idate)>=0)
       next=next+1
       if(next == npe)next=zero
       if(next /= mype)cycle
       read(subset,'(2x,i6)')isubset
       if(isubset>6033) then
          iret=6034
          exit
       end if
       do while (ireadsb(inbufr)==0)
          call ufbint(inbufr,rdisttest,1,n_gates_max,n_gates,'DIST125M')
          if(n_gates>1) then
             do i=1,n_gates-1
                if(nint(abs(rdisttest(i+1)-rdisttest(i)))==0) then
                   idups=idups+1
                else
                   ddiffmin=min(abs(rdisttest(i+1)-rdisttest(i)),ddiffmin)
                end if
             end do
          end if
          call ufbint(inbufr,hdr2,12,1,levs,'SSTN CLAT CLON HSMSL HSALG ANEL YEAR MNTH DAYS HOUR MINU SECO')
          if(hdr2(6)>elev_angle_max) cycle
          idate5(1)=nint(hdr2(7)) ; idate5(2)=nint(hdr2(8)) ; idate5(3)=nint(hdr2(9))
          idate5(4)=nint(hdr2(10)) ; idate5(5)=nint(hdr2(11))
          call w3fs21(idate5,nminthis)
          t=(real(nminthis-nminref,r_kind)+real(nint(hdr2(12)),r_kind)*rinv60)*rinv60
          if(abs(t)>del_time) cycle
          nobs_in=nobs_in+n_gates
          stn_id=chdr2 
          radar_true=0 
          if (radar_sites) then 
             do i=1,radar_count 
                if (trim(stn_id) .eq. trim(rsite(i)) .and. ruse(i) == 1 ) radar_true=1 
             end do 
             if (radar_true == 0) cycle 
          end if 
          ibyte=locindex(stn_id_table,max_num_radars,stn_id)
          if(ibyte==0) then
             num_radars=num_radars+1
             if(num_radars>max_num_radars) then
                write(6,*)'RADAR_BUFR_READ_ALL:  stop processing level 2 radar ',&
                     'bufr file--increase parameter max_num_radars'
                call stop2(99)
             end if
             stn_lat=hdr2(2)
             stn_lon=hdr2(3)
             stn_hgt=hdr2(4)+hdr2(5)
             stn_id_table(num_radars)=stn_id
             stn_lon_table(num_radars)=stn_lon
             stn_lat_table(num_radars)=stn_lat
             stn_hgt_table(num_radars)=stn_hgt
          end if
       end do
    end do

    call mpi_allreduce(num_radars,num_radars_max,1,mpi_integer4,mpi_max,mpi_comm_world,ierror)
    if(num_radars_max<=0) then
       if(rite) write(6,*)'RADAR_BUFR_READ_ALL:  NO RADARS KEPT IN radar_bufr_read_all, ',&
            'continue without level 2 data'
       call closbf(inbufr)
       close(inbufr)
       return
    end if
    call mpi_reduce(num_radars,num_radars_min,1,mpi_integer4,mpi_min,0,mpi_comm_world,ierror)
    if(rite) write(6,*)' min,max num_radars=',num_radars_min,num_radars_max

!  Create master station list

!  First gather all stn id and lat,lon,hgt lists
    call mpi_allgather(stn_id_table,max_num_radars,mpi_integer4, &
         stn_id_table_all,max_num_radars,mpi_integer4,mpi_comm_world,ierror)
    call mpi_allgather(stn_lat_table,max_num_radars,mpi_real8, &
         stn_lat_table_all,max_num_radars,mpi_real8,mpi_comm_world,ierror)
    call mpi_allgather(stn_lon_table,max_num_radars,mpi_real8, &
         stn_lon_table_all,max_num_radars,mpi_real8,mpi_comm_world,ierror)
    call mpi_allgather(stn_hgt_table,max_num_radars,mpi_real8, &
         stn_hgt_table_all,max_num_radars,mpi_real8,mpi_comm_world,ierror)
!   Create unique master list of all radar names,lats,lons
    jj=0
    do j=1,max_num_radars*npe
       if(stn_id_table_all(j) /= 'ZZZZ')then
          jj=jj+1
          stn_id_table_all(jj)=stn_id_table_all(j)
          stn_lat_table_all(jj)=stn_lat_table_all(j)
          stn_lon_table_all(jj)=stn_lon_table_all(j)
          stn_hgt_table_all(jj)=stn_hgt_table_all(j)
       end if
    end do
    jjall=jj
    num_radars_0=0
    jj=1
    outer: do j=1,jjall
        num_radars_0=num_radars_0+1
        if(num_radars_0 > max_num_radars) then
	  write(6,*)'RADAR_BUFR_READ_ALL:  stop processing level 2 radar ',&
	     'bufr file--increase parameter max_num_radars'
          call stop2(99)
        end if
        master_stn_table(num_radars_0)=stn_id_table_all(jj)
        master_lat_table(num_radars_0)=stn_lat_table_all(jj)
        master_lon_table(num_radars_0)=stn_lon_table_all(jj)
        master_hgt_table(num_radars_0)=stn_hgt_table_all(jj)
        numzzzz=0
        do jjj=jj+1,jjall
          if(stn_id_table_all(jjj) /= 'ZZZZ')then
	     if(stn_id_table_all(jjj)==master_stn_table(num_radars_0)) then
	        stn_id_table_all(jjj)='ZZZZ'
	     else
	        if(numzzzz == 0)numzzzz=jjj
	     end if
	  end if
        end do
        if(numzzzz==0) exit outer
        jj=numzzzz
    end do outer
    allocate(indx(num_radars_0))
    call indexset(indx)
    call indexsort(indx,master_stn_table)

    call mpi_allreduce(ddiffmin,ddiffmin0,1,mpi_real8,mpi_min,mpi_comm_world,ierror)
    call mpi_allreduce(idups,idups0,1,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
    distfact=zero
    if(nint(ddiffmin0)==1)     distfact=r250
    if(nint(ddiffmin0)==2)     distfact=r125
    if(distfact==zero) then
       write(6,*)'RADAR_BUFR_READ_ALL:  problem with level 2 bufr file, ',&
	    'gate distance scale factor undetermined, going with 125'
       distfact=r125
    end if

    timemax=-huge(timemax)
    timemin=huge(timemin)
    nradials_in=0
    nradials_fail_angmax=0
    nradials_fail_time=0
    nradials_fail_elb=0
    nobs_badvr=0
    nobs_badsr=0
    nobs_lrbin=0
    nobs_hrbin=0
    nrange_max=0
    nthisrad=nrbin*nazbin*nelbin
    nthisbins=6*nthisrad
    if (radar_box) then 
       twopi  = two*pi
       rkm2dg = 360.0_r_kind/(twopi*rearth_equator)*1.e3_r_kind
       dlat_grid = 5.0_r_kind  
       dlon_grid = 5.0_r_kind  
       halfpi = half*pi
       dx=radar_rmesh*rkm2dg
       dy=dx
       mlat=dlat_grid/dy+half
       mlonx=dlon_grid/dx+half
       delat=dlat_grid/mlat
       dgv=delat*half
       mlat=max(2,mlat);mlonx=max(2,mlonx)
       nlevz=nint(15000.0_r_kind/radar_zmesh)
       nthisrad=nlevz*mlat*mlonx
       nthisbins=6*nthisrad
    end if
! reopen and reread the file for data this time
    call closbf(inbufr)
    close(inbufr)
    open(inbufr,file=infile,form='unformatted')
    call openbf(inbufr,'IN',inbufr)
    allocate(bins(6,nthisrad,num_radars_0),ibins(nthisrad,num_radars_0))
    bins=zero_quad
    ibins=0

    next=0
    do while(ireadmg(inbufr,subset,idate)>=0)
       next=next+1
       if(next == npe)next=zero
       if(next /= mype)cycle
       read(subset,'(2x,i6)')isubset
       if(isubset>6033) then
	  iret=6034
	  exit
       end if
       do while (ireadsb(inbufr)==0)
	  call ufbint(inbufr,hdr,10,1,levs, &
	       'SSTN YEAR MNTH DAYS HOUR MINU SECO ANAZ ANEL QCRW')
	  nradials_in=nradials_in+1
	  if(hdr(9)>elev_angle_max) then
	     nradials_fail_angmax=nradials_fail_angmax+1
	     cycle
	  end if
	  idate5(1)=nint(hdr(2)) ; idate5(2)=nint(hdr(3)) ; idate5(3)=nint(hdr(4))
	  idate5(4)=nint(hdr(5)) ; idate5(5)=nint(hdr(6))
	  call w3fs21(idate5,nminthis)
	  t=(real(nminthis-nminref,r_kind)+real(nint(hdr(7)),r_kind)*rinv60)*rinv60
	  timemax=max(t,timemax)
	  timemin=min(t,timemin)
	  if(abs(t)>del_time) then
	     nradials_fail_time=nradials_fail_time+1
	     cycle
	  end if
	  stn_az=r90-hdr(8)
	  stn_el=hdr(9)
	  iazbin=stn_az*rdelaz
	  iazbin=mod(iazbin,nazbin)
	  if(iazbin<0) iazbin=iazbin+nazbin
	  iazbin=iazbin+1
	  if(iazbin<=0.or.iazbin>nazbin) then
	     write(6,*)'RADAR_BUFR_READ_ALL:  error in getting iazbin, program stops'
	     call stop2(99)
	  end if
	  ielbin=ceiling(stn_el*rdelel)
	  if(ielbin<1.or.ielbin>nelbin) then
	     nradials_fail_elb=nradials_fail_elb+1
	     cycle
	  end if
	  stn_id=chdr 
	  ibyte=locindex(master_stn_table,max_num_radars,stn_id)
          if (radar_sites) then 
             radar_true=0 
             do i=1,radar_count 
               if (trim(stn_id) == trim(rsite(i)) .and. ruse(i) == 1) radar_true=1 
             end do 
             if (radar_true == 0) then 
               cycle 
             end if 
          end if 
	  if(ibyte==0) then
	     write(6,*) ' index error in radar_bufr_read_all -- program stops -- ',ibyte,stn_id
	     call stop2(99)
	  else
	     krad=ibyte
	  end if

	  call ufbint(inbufr,rwnd,3,n_gates_max,n_gates,'DIST125M DMVR DVSW')
	  do i=1,n_gates
	     range=distfact*rwnd(1,i)
	     if(range>range_max) then
		nrange_max=nrange_max+1
		cycle
	     end if
	     if(rwnd(2,i)>r1e5_double) then
		nobs_badvr=nobs_badvr+1
		cycle
	     end if
	     if(rwnd(3,i)>r1e5_double) then
		nobs_badsr=nobs_badsr+1
		cycle
	     end if
             if (.not.radar_box) then
	        irbin=ceiling(range*rdelr)
	        if(irbin<1) then
		   nobs_lrbin=nobs_lrbin+1
		   cycle
	        end if
	        if(irbin>nrbin) then
		   nobs_hrbin=nobs_hrbin+1
		   cycle
	        end if

	        iloc=nrbin*(nazbin*(ielbin-1)+(iazbin-1))+irbin
	        bins(1,iloc,krad)=bins(1,iloc,krad)+range
	        bins(2,iloc,krad)=bins(2,iloc,krad)+stn_az
	        bins(3,iloc,krad)=bins(3,iloc,krad)+stn_el
	        bins(4,iloc,krad)=bins(4,iloc,krad)+rwnd(2,i)
	        bins(5,iloc,krad)=bins(5,iloc,krad)+rwnd(2,i)**2
	        bins(6,iloc,krad)=bins(6,iloc,krad)+t
	        ibins(iloc,krad)=ibins(iloc,krad)+1
             else 
                this_stalat=master_lat_table(krad)
                if(abs(this_stalat)>r89_5) cycle
                this_stalon=master_lon_table(krad)
                rlon0=deg2rad*this_stalon
                this_stalatr=this_stalat*deg2rad
                clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
                this_staid=master_stn_table(krad)
                this_stahgt=master_hgt_table(krad)
                thisrange=  range
                thisazimuth=stn_az
                thistilt=stn_el
                thisvr=rwnd(2,i)


                vrmax = tiny(1.0_r_kind)
                vrmin = huge(1.0_r_kind)

                vrmax=max(vrmax,thisvr)
                vrmin=min(vrmin,thisvr)
                thisvr2=rwnd(2,i)**2
                thiserr=sqrt(abs(thisvr2-thisvr**2))


                errmax = tiny(1.0_r_kind)
                errmin = huge(1.0_r_kind)

                errmax=max(errmax,thiserr)
                errmin=min(errmin,thiserr)
                thistime=t
                aactual=erad+this_stahgt
                a43=four_thirds*aactual
                thistiltr=thistilt*deg2rad
                selev0=sin(thistiltr)
                celev0=cos(thistiltr)
                b=thisrange*(thisrange+two*aactual*selev0)
                c=sqrt(aactual*aactual+b)
                ha=b/(aactual+c)
                epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
                h=ha-epsh
                thishgt=this_stahgt+h
                celev=celev0
                selev=selev0
 
                if(thisrange>=one) then
                   celev=a43*celev0/(a43+h)
                   selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
                end if

                corrected_tilt=atan2(selev,celev)*rad2deg
                gamma=half*thisrange*(celev0+celev)
!               Get earth lat lon of superob
                thisazimuthr=thisazimuth*deg2rad
                rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
                rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
                call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
                thislat=rlatglob*rad2deg
                thislon=rlonglob*rad2deg

                if(abs(thislat)>r89_5) cycle
                clat1=cos(rlatglob)
                caz0=cos(thisazimuthr)
                saz0=sin(thisazimuthr)
                cdlon=cos(rlonglob-rlon0)
                sdlon=sin(rlonglob-rlon0)
                caz1=clat0*caz0/clat1
                saz1=saz0*cdlon-caz0*sdlon*slat0
                corrected_azimuth=atan2(saz1,caz1)*rad2deg
                rlat_min=this_stalat-2.5_r_kind 
                rlon_min=this_stalon-2.5_r_kind 
                ilev=ceiling(thishgt/radar_zmesh)
                ilat=ceiling((thislat-rlat_min)/delat)
                ilon=ceiling((thislon-rlon_min)/(dlon_grid/mlonx)) 
                if(ilev<1 .or. ilat<1 .or. ilon<1) then
                   nobs_lrbin=nobs_lrbin+1
                   cycle
                end if
                if(ilev>nlevz .or. ilat>mlat .or. ilon>mlonx) then
                   nobs_hrbin=nobs_hrbin+1
                   cycle
                end if
                iloc=mlat*(mlonx*(ilev-1)+(ilon-1))+ilat
                bins(1,iloc,krad)=bins(1,iloc,krad)+range
                bins(2,iloc,krad)=bins(2,iloc,krad)+stn_az
                bins(3,iloc,krad)=bins(3,iloc,krad)+stn_el
                bins(4,iloc,krad)=bins(4,iloc,krad)+rwnd(2,i)
                bins(5,iloc,krad)=bins(5,iloc,krad)+rwnd(2,i)**2
                bins(6,iloc,krad)=bins(6,iloc,krad)+t
                ibins(iloc,krad)=ibins(iloc,krad)+1

             end if !radar_box end
	  end do
       end do          !  end do while
    end do             !  loop over blocks
    call closbf(inbufr)
    close(inbufr)
    if (.not. allocated(ibins2)) allocate(ibins2(nthisrad,num_radars_0))

    ibins2=0
    call mpi_allreduce(ibins,ibins2,nthisrad*num_radars_0,mpi_integer4,mpi_sum,mpi_comm_world,ierror)
    deallocate(ibins)

    call mpi_reduce(nradials_in,nradials_in1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_angmax,nradials_fail_angmax1,1,&
	 mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_time,nradials_fail_time1,1,&
	 mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nradials_fail_elb,nradials_fail_elb1,1,&
	 mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_in,nobs_in1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_badvr,nobs_badvr1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_badsr,nobs_badsr1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_lrbin,nobs_lrbin1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nobs_hrbin,nobs_hrbin1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(nrange_max,nrange_max1,1,mpi_integer4,mpi_sum,0,mpi_comm_world,ierror)
    call mpi_reduce(timemax,timemax1,1,mpi_real8,mpi_max,0,mpi_comm_world,ierror)
    call mpi_reduce(timemin,timemin1,1,mpi_real8,mpi_min,0,mpi_comm_world,ierror)

    if(mype==0 ) then
    
       allocate(icount(num_radars_0))
       if(rite)write(6,*)'RADAR_BUFR_READ_ALL:  num_radars_0 = ',num_radars_0
       do irad=1,num_radars_0
          krad=indx(irad)
          icount(krad)=0
          do i=1,nthisrad
            if(ibins2(i,krad) >= minnum)icount(krad)=icount(krad)+1
          end do
	  if(rite)write(6,'(" master list radar ",i3," stn id,lat,lon,hgt,num = ",a4,2f10.2,f8.1,i10)') &
	       irad,master_stn_table(krad),master_lat_table(krad),master_lon_table(krad), &
               master_hgt_table(krad),icount(krad)
       end do
       if(rite)then
          write(6,*)'RADAR_BUFR_READ_ALL:  ddiffmin,distfact,idups=',ddiffmin0,distfact,idups0
          write(6,*)' nthisrad=',nthisrad
          write(6,*)' nthisbins=',nthisbins
          write(6,*)' timemin,max=',timemin1,timemax1
          write(6,*)' nradials_in=',nradials_in1
          write(6,*)' nradials_fail_angmax=',nradials_fail_angmax1
          write(6,*)' nradials_fail_time=',nradials_fail_time1
          write(6,*)' nradials_fail_elb=',nradials_fail_elb1
          write(6,*)' nobs_in=',nobs_in1
          write(6,*)' nobs_badvr=',nobs_badvr1
          write(6,*)' nobs_badsr=',nobs_badsr1
          write(6,*)' nobs_lrbin=',nobs_lrbin1
          write(6,*)' nobs_hrbin=',nobs_hrbin1
          write(6,*)' nrange_max=',nrange_max1
       end if
       deallocate(icount)

!   Prepare to create superobs and write out.
       open(inbufr,file='radar_supobs_from_level2',form='unformatted',iostat=iret)
       rewind inbufr
       nsuperall=0
       vrmaxall=-huge(vrmaxall)
       vrminall=huge(vrminall)
       errmaxall=-huge(errmaxall)
       errminall=huge(errminall)
       delazmmaxall=-huge(delazmmaxall)
       deltiltmaxall=-huge(deltiltmaxall)
       deldistmaxall=-huge(deldistmaxall)
       deltiltminall=huge(deltiltminall)
       deldistminall=huge(deldistminall)
    end if
    allocate(bins_work(6,nthisrad,npe))
    do irad=1,num_radars_0
       krad=indx(irad)
       if(r_double==r_quad) then
          call mpi_gather(bins(1,1,krad),nthisbins,mpi_real8 ,bins_work,nthisbins, &
                       mpi_real8 ,0,mpi_comm_world,ierror)
       else
          call mpi_gather(bins(1,1,krad),nthisbins,mpi_real16,bins_work,nthisbins, &
                       mpi_real16,0,mpi_comm_world,ierror)
       endif
       if(mype == 0)then
    
!   Create superobs and write out.
	  nsuper=0
	  vrmax=-huge(vrmax)
	  vrmin=huge(vrmin)
	  errmax=-huge(errmax)
	  errmin=huge(errmin)
	  delazmmax=-huge(delazmmax)
	  deltiltmax=-huge(deltiltmax)
	  deldistmax=-huge(deldistmax)
	  deltiltmin=huge(deltiltmin)
	  deldistmin=huge(deldistmin)
	  this_stalat=master_lat_table(krad)
	  if(abs(this_stalat)>r89_5) cycle
	  this_stalon=master_lon_table(krad)
	  rlon0=deg2rad*this_stalon
	  this_stalatr=this_stalat*deg2rad
	  clat0=cos(this_stalatr) ; slat0=sin(this_stalatr)
	  this_staid=master_stn_table(krad)
	  this_stahgt=master_hgt_table(krad)
	  do iii=1,nthisrad
! If single radar exp, set lradar false if not the specified radar.
           lradar=.true.
           if(lsingleradar) then
             if(this_staid /= singleradar) lradar=.false.
           end if
           if(lradar) then ! Logical for when running single radar exp.
	     if(ibins2(iii,krad) < minnum) cycle

	     thiscount=one_quad/real(ibins2(iii,krad),r_quad)
       if (radar_box) then 
          do i=1,6
             binsx(i)=bins_work(i,iii,1)
          end do
          do k=2,npe
             do i=1,6
                binsx(i)=binsx(i)+bins_work(i,iii,k)
             end do
          end do
          do i=1,6
             binsx(i)=binsx(i)*thiscount
          end do

          thisrange=  binsx(1)
          thisazimuth=binsx(2)
          thistilt=binsx(3)
          thisvr=binsx(4)
          vrmax=max(vrmax,thisvr)
          vrmin=min(vrmin,thisvr)
          thisvr2=binsx(5)
          thiserr=sqrt(abs(thisvr2-thisvr**2))
          errmax=max(errmax,thiserr)
          errmin=min(errmin,thiserr)
          thistime=binsx(6)

       else 
          do i=1,6
             binsx(i)=bins_work(i,iii,1)
          end do
          do k=2,npe
             do i=1,6
	              binsx(i)=binsx(i)+bins_work(i,iii,k)
             end do
          end do
          do i=1,6
             binsx(i)=binsx(i)*thiscount
          end do

	        thisrange=  binsx(1)
	        thisazimuth=binsx(2)
	        thistilt=binsx(3)
	        thisvr=binsx(4)
	        vrmax=max(vrmax,thisvr)
	        vrmin=min(vrmin,thisvr)
	        thisvr2=binsx(5)
	        thiserr=sqrt(abs(thisvr2-thisvr**2))
	        errmax=max(errmax,thiserr)
	        errmin=min(errmin,thiserr)
	        thistime=binsx(6)
      end if 

!            Compute obs height here
!            Use 4/3rds rule to get elevation of radar beam
!            (if local temperature, moisture available, then vertical position 
!             might be estimated with greater accuracy by ray tracing )

	     aactual=erad+this_stahgt
	     a43=four_thirds*aactual
	     thistiltr=thistilt*deg2rad
	     selev0=sin(thistiltr)
	     celev0=cos(thistiltr)
	     b=thisrange*(thisrange+two*aactual*selev0)
	     c=sqrt(aactual*aactual+b)
	     ha=b/(aactual+c)
	     epsh=(thisrange*thisrange-ha*ha)/(r8*aactual)
	     h=ha-epsh
	     thishgt=this_stahgt+h
	     
!            Get corrected tilt angle
	     celev=celev0
	     selev=selev0
	     if(thisrange>=one) then
		celev=a43*celev0/(a43+h)
		selev=(thisrange*thisrange+h*h+two*a43*h)/(two*thisrange*(a43+h))
	     end if
	     corrected_tilt=atan2(selev,celev)*rad2deg
	     deltiltmax=max(corrected_tilt-thistilt,deltiltmax)
	     deltiltmin=min(corrected_tilt-thistilt,deltiltmin)
	     gamma=half*thisrange*(celev0+celev)
	     deldistmax=max(gamma-thisrange,deldistmax)
	     deldistmin=min(gamma-thisrange,deldistmin)

!            Get earth lat lon of superob
	     thisazimuthr=thisazimuth*deg2rad
	     rlonloc=rad_per_meter*gamma*cos(thisazimuthr)
             rlatloc=rad_per_meter*gamma*sin(thisazimuthr)
             call invtllv(rlonloc,rlatloc,rlon0,clat0,slat0,rlonglob,rlatglob)
             thislat=rlatglob*rad2deg
             thislon=rlonglob*rad2deg
!            Keep away from poles, rather than properly deal with polar singularity
	     if(abs(thislat)>r89_5) cycle

!            Get corrected azimuth
             clat1=cos(rlatglob)
             caz0=cos(thisazimuthr)
             saz0=sin(thisazimuthr)
             cdlon=cos(rlonglob-rlon0)
             sdlon=sin(rlonglob-rlon0)
             caz1=clat0*caz0/clat1
             saz1=saz0*cdlon-caz0*sdlon*slat0
             corrected_azimuth=atan2(saz1,caz1)*rad2deg
             delazmmax=max(min(abs(corrected_azimuth-thisazimuth-r720),&
                  abs(corrected_azimuth-thisazimuth-r360),&
                  abs(corrected_azimuth-thisazimuth     ),&
                  abs(corrected_azimuth-thisazimuth+r360),&
                  abs(corrected_azimuth-thisazimuth+r720)),delazmmax)
             write(inbufr) this_staid,this_stalat,this_stalon,this_stahgt, &
                  thistime,thislat,thislon,thishgt,thisvr,corrected_azimuth,&
                  thiserr,corrected_tilt,gamma
             nsuper=nsuper+1
           end if
          end do
          if(nsuper > 0)then
            write(6,*)' for radar ',this_staid,' nsuper=',nsuper,' delazmmax=',delazmmax,lradar
            write(6,*)' vrmin,max=',vrmin,vrmax,' errmin,max=',errmin,errmax
            write(6,*)' deltiltmin,max=',deltiltmin,deltiltmax,' deldistmin,max=',deldistmin,deldistmax
            vrminall=min(vrminall,vrmin)
            vrmaxall=max(vrmaxall,vrmax)
            errminall=min(errminall,errmin)
            errmaxall=max(errmaxall,errmax)
            delazmmaxall=max(delazmmaxall,delazmmax)
            deltiltmaxall=max(deltiltmaxall,deltiltmax)
            deldistmaxall=max(deldistmaxall,deldistmax)
            deltiltminall=min(deltiltminall,deltiltmin)
            deldistminall=min(deldistminall,deldistmin)
            nsuperall=nsuperall+nsuper
          end if
       end if
    end do
    if(mype == 0)then
       if(rite)then
          write(6,*)' total number of superobs written=',nsuperall
          write(6,*)'  vrmin,maxall=',vrminall,vrmaxall
          write(6,*)' errmin,maxall=',errminall,errmaxall
          write(6,*)' delazmmaxall=',delazmmaxall
          write(6,*)' deltiltmin,maxall=',deltiltminall,deltiltmaxall
          write(6,*)' deldistmin,maxall=',deldistminall,deldistmaxall
       end if
       close(inbufr)
       close(inbufr)
    end if
    deallocate(indx)
    deallocate(bins_work,bins,ibins2)
    if(l2superob_only) then
       call mpi_finalize(ierror)
       stop
    end if
    if (radar_sites) deallocate(rtable,rsite,ruse) 

end subroutine radar_bufr_read_all


SUBROUTINE tllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    tllv             
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-21  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  use kinds, only:  r_kind
  implicit none

  real(r_kind),intent(in   ) :: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(  out) :: tlm,tph

  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM-TLMO
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC+STPH0*SPH
  TLM=ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH-STPH0*CC)

END SUBROUTINE tllv

SUBROUTINE invtllv(ALM,APH,TLMO,CTPH0,STPH0,TLM,TPH)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invtllv
!
!   prgrmmr:
!
! abstract:  inverse of tllv:  input ALM,APH is rotated lon,lat
!                   output is earth lon,lat, TLM,TPH
!
! program history log:
!   2008-03-25  safford -- add subprogram doc block, rm unused uses
!
!   input argument list:
!     alm   -- input earth longitude
!     aph   -- input earth latitude
!     tlmo  -- input earth longitude of rotated grid origin (radrees)
!     ctph0 -- cos(earth lat of rotated grid origin)
!     stph0 -- sin(earth lat of rotated grid origin)
!
!   output argument list:
!     tlm   -- rotated grid longitude
!     tph   -- rotated grid latitude
!
! attributes:
!   language:  f90
!   machine:
!
!$$$ end documentation block

  use kinds, only:  r_kind
  implicit none

  real(r_kind),intent(in   ) :: alm,aph,tlmo,ctph0,stph0
  real(r_kind),intent(  out) :: tlm,tph

  real(r_kind):: relm,srlm,crlm,sph,cph,cc,anum,denom

  RELM=ALM
  SRLM=SIN(RELM)
  CRLM=COS(RELM)
  SPH=SIN(APH)
  CPH=COS(APH)
  CC=CPH*CRLM
  ANUM=CPH*SRLM
  DENOM=CTPH0*CC-STPH0*SPH
  TLM=tlmo+ATAN2(ANUM,DENOM)
  TPH=ASIN(CTPH0*SPH+STPH0*CC)
  return
END SUBROUTINE invtllv

FUNCTION locindex(stnall,nstr,stn)
  use kinds, only:  i_kind
  implicit none
  
  integer(i_kind),intent(in):: nstr
  character(4),intent(in):: stnall(nstr)
  character(4),intent(in):: stn
  integer(i_kind):: i,locindex
  do i=1,nstr
     locindex=index(stnall(i),stn)
     if(locindex>0) then
       locindex=i
       exit
     end if
  end do
END FUNCTION locindex
end module read_l2bufr_mod
