  module grib2_module
!------------------------------------------------------------------------
!
! This module generates grib2 messages and writes out the messages in 
!   parallel.
!
! program log:
!   March, 2010    Jun Wang   Initial code
!   Jan,   2012    Jun Wang   post available fields with grib2 description
!                              are defined in xml file
!   March, 2015    Lin Gan    Replace XML file with flat file implementation
!                              with parameter marshalling
!------------------------------------------------------------------------
  use xml_perl_data, only: param_t,paramset_t
!
  implicit none
  private
! ------------------------------------------------------------------------
!
!--- general grib2 info provided by post control file
!  type param_t
!     integer                                         :: post_avblfldidx=-9999
!     character(len=80)                                :: shortname=''
!     character(len=300)                                :: longname=''
!     character(len=30)                                :: pdstmpl=''
!     integer                                         :: mass_windpoint=1
!     character(len=30)                                :: pname=''
!     character(len=10)                                :: table_info=''
!     character(len=20)                                :: stats_proc=''
!     character(len=80)                                :: fixed_sfc1_type=''
!     integer, dimension(:), pointer                  :: scale_fact_fixed_sfc1 => null()
!     real, dimension(:), pointer                     :: level => null()
!     character(len=80)                                :: fixed_sfc2_type=''
!     integer, dimension(:), pointer                  :: scale_fact_fixed_sfc2 => null()
!     real, dimension(:), pointer                     :: level2 => null()
!     character(len=80)                                :: aerosol_type=''
!     character(len=80)                                :: typ_intvl_size=''
!     integer                                         :: scale_fact_1st_size=0
!     real                                            :: scale_val_1st_size=0.
!     integer                                         :: scale_fact_2nd_size=0
!     real                                            :: scale_val_2nd_size=0.
!     character(len=80)                                :: typ_intvl_wvlen=''
!     integer                                         :: scale_fact_1st_wvlen=0
!     real                                            :: scale_val_1st_wvlen=0.
!     integer                                         :: scale_fact_2nd_wvlen=0
!     real                                            :: scale_val_2nd_wvlen=0.
!     real, dimension(:), pointer                     :: scale => null()
!     integer                                         :: stat_miss_val=0
!     integer                                         :: leng_time_range_prev=0
!     integer                                         :: time_inc_betwn_succ_fld=0
!     character(len=80)                                :: type_of_time_inc=''
!     character(len=20)                                :: stat_unit_time_key_succ=''
!     character(len=20)                                :: bit_map_flag=''
!     integer                                          :: perturb_num=0
!     integer                                          :: num_ens_fcst=10
!  end type param_t
!
!  type paramset_t
!     character(len=6)                                :: datset=''
!     integer                                         :: grid_num=255
!     character(len=20)                                :: sub_center=''
!     character(len=20)                                :: version_no=''
!     character(len=20)                                :: local_table_vers_no=''
!     character(len=20)                                :: sigreftime=''
!     character(len=20)                                :: prod_status=''
!     character(len=20)                                :: data_type=''
!     character(len=20)                                :: gen_proc_type=''
!     character(len=30)                                :: time_range_unit=''
!     character(len=50)                                :: orig_center=''
!     character(len=30)                                :: gen_proc=''
!     character(len=20)                                :: packing_method=''
!     character(len=20)                                :: field_datatype=''
!     character(len=20)                                :: comprs_type=''
!     character(len=50)                                :: type_ens_fcst=''
!     character(len=50)                                :: type_derived_fcst=''
!     type(param_t), dimension(:), pointer            :: param => null()
!  end type paramset_t
   type(paramset_t),save :: pset
!
!--- grib2 info related to a specific data file
  integer nrecout
  integer num_pset
  integer isec,hrs_obs_cutoff,min_obs_cutoff
  integer sec_intvl,stat_miss_val,time_inc_betwn_succ_fld
  integer perturb_num,num_ens_fcst
  character*80 type_of_time_inc,stat_unit_time_key_succ
  logical*1,allocatable :: bmap(:)
  integer ibm
  integer,allocatable   :: mg(:)
!
  integer,parameter :: max_bytes=1000*1300000
  integer,parameter :: MAX_NUMBIT=16
  integer,parameter :: lugi=650
  character*255 fl_nametbl,fl_gdss3
  real(8) :: stime,stime1,stime2,etime,etime1
  logical :: first_grbtbl
!
  public num_pset,pset,nrecout,gribit2,grib_info_init,first_grbtbl,grib_info_finalize
  real(8), EXTERNAL :: timef
!-------------------------------------------------------------------------------------
!
  contains
!
!-------------------------------------------------------------------------------------
  subroutine grib_info_init()
!
!--- initialize general grib2 information and 
!
    implicit none
!
!    logical,intent(in) :: first_grbtbl
!
!-- local variables
    integer ierr
    character(len=80) outfile
    character(len=10) envvar
!
!-- set up pset
!
!-- 1. pset is set up at READCNTRL_xml.f
!--    initialize items of pset that are not set in xml file
!
        if(pset%grid_num==0)                             &
           pset%grid_num=218
        if(trim(pset%sub_center)=='')                    &
           pset%sub_center="ncep_emc"
        if(trim(pset%version_no)=='')                    &
           pset%version_no="v2003"
        if(trim(pset%local_table_vers_no)=='')           &
           pset%local_table_vers_no="local_table_no"
        if(trim(pset%sigreftime)=='')                    &
           pset%sigreftime="fcst"
        if(trim(pset%prod_status)=='')                    &
           pset%prod_status="oper_test"
        if(trim(pset%data_type)=='')                     &
           pset%data_type="fcst"
        if(trim(pset%orig_center)=='')                   &
           pset%orig_center="nws_ncep"
        if(trim(pset%time_range_unit)=='')               &
           pset%time_range_unit="hour"
        if(trim(pset%gen_proc_type)=='')                 &
           pset%gen_proc_type="fcst"
        if(trim(pset%gen_proc)=='')                      &
           pset%gen_proc="gfs_avn"
        if(trim(pset%packing_method)=='')                &
           pset%packing_method="jpeg"
        if(trim(pset%field_datatype)=='')                &
           pset%field_datatype="flting_pnt"
        if(trim(pset%comprs_type)=='')                   &
           pset%comprs_type="lossless"
       if(trim(pset%type_ens_fcst)=='')                  &
           pset%type_ens_fcst="pos_pert_fcst"
       if(trim(pset%type_derived_fcst)=='')              &
           pset%type_derived_fcst="unweighted_mean_all_mem"
!
!-- set up other grib2_info
!
    isec=0
    hrs_obs_cutoff=0 ! applies to only obs
    min_obs_cutoff=0 ! applies to only obs
    sec_intvl=0
    stat_miss_val=0
    type_of_time_inc='same_start_time_fcst_fcst_time_inc'
    stat_unit_time_key_succ='missing'
    time_inc_betwn_succ_fld=0
!
!-- open fld name tble 
!
    if(first_grbtbl) then
      fl_nametbl='params_grib2_tbl_new'
      call open_and_read_4dot2( fl_nametbl, ierr )
      if ( ierr .ne. 0 ) then
        print*, 'Couldnt open table file - return code was ',ierr
        call mpi_abort()
      endif
      first_grbtbl=.false.
    endif
!
!--
!
  end subroutine grib_info_init
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
  subroutine grib_info_finalize
!
!--- finalize grib2 information and  close file
!
    implicit none
!
!---
    integer ierr
    call close_4dot2(ierr)
!   
  end subroutine grib_info_finalize
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
  subroutine gribit2(post_fname)
!
!-------
    use ctlblk_mod, only : im,jm,im_jm,num_procs,me,jsta,jend,ifhr,sdat,ihrst,imin,    &
                           mpi_comm_comp,ntlfld,fld_info,datapd,icnt,idsp
    implicit none
!
    include 'mpif.h'
!
!    real,intent(in)      :: data(im,1:jend-jsta+1,ntlfld)
    character(255),intent(in) :: post_fname
!
!------- local variables
    integer i,j,k,n,nm,nprm,nlvl,fldlvl1,fldlvl2,cstart,cgrblen,ierr
    integer nf,nfpe,nmod
    integer fh, clength,lunout
    integer idisc,icatg,iparm,itblinfo,ntrange,leng_time_range_stat
    integer,allocatable :: nfld_pe(:),snfld_pe(:),enfld_pe(:)
    integer(4),allocatable :: isdsp(:),iscnt(:),ircnt(:),irdsp(:)
    integer status(MPI_STATUS_SIZE)
    integer(kind=MPI_OFFSET_KIND) idisp
    integer,allocatable :: jsta_pe(:),jend_pe(:)
    integer,allocatable :: grbmsglen(:)
    real,allocatable    :: datafld(:,:)
    real,allocatable    :: datafldtmp(:)
!
    character(1) cgrib(max_bytes)
!
!
!---------------- code starts here --------------------------
!
!
!******* part 1 resitribute data ********
!
!--- calculate # of fields on each processor
!
    nf=ntlfld/num_procs
    nfpe=nf+1
    nmod=mod(ntlfld,num_procs)
!    print *,'ntlfld=',ntlfld,'nf=',nf,'nmod=',nmod
    allocate(snfld_pe(num_procs),enfld_pe(num_procs),nfld_pe(num_procs))
    do n=1,num_procs
      if(n-1<nmod ) then
        snfld_pe(n)=nfpe*(n-1)+1
        enfld_pe(n)=snfld_pe(n)+nfpe-1
        nfld_pe(n)=nfpe
      else
        snfld_pe(n)=nfpe*nmod+nf*(n-1-nmod)+1
        enfld_pe(n)=snfld_pe(n)+nf-1
        nfld_pe(n)=nf
      endif
    enddo
!      print *,'in gribit2,ntlfld=',ntlfld,'nf=',nf,'myfld=',snfld_pe(me+1),enfld_pe(me+1)
!
!--- reditribute data from partial domain data with all fields 
!---   to whole domain data but partial fields
!
    allocate(jsta_pe(num_procs),jend_pe(num_procs))
    call mpi_allgather(jsta,1,MPI_INTEGER,jsta_pe,1,          &
      MPI_INTEGER,MPI_COMM_COMP,ierr)
    call mpi_allgather(jend,1,MPI_INTEGER,jend_pe,1,          &
      MPI_INTEGER,MPI_COMM_COMP,ierr)
!      print *,'in gribit2,jsta_pe=',jsta_pe,'jend_pe=',jend_pe
!
!---  end part1
!
!********************* generate grib2 message and write out data ****
!
    allocate(bmap(im_jm))
    allocate(mg(im_jm))
!
!--- sequatial write if the number of fields to write is small
!
    if(minval(nfld_pe)<1.or.num_procs==1) then
!
!-- collect data to pe 0
      allocate(datafld(im_jm,ntlfld) )
      if(num_procs==1) then
        datafld=reshape(datapd,(/im_jm,ntlfld/))
      else
        do i=1,ntlfld 
          call mpi_gatherv(datapd(:,:,i),icnt(me),MPI_REAL,          &
             datafld(:,i),icnt,idsp,MPI_REAL,0,MPI_COMM_COMP,ierr)
        enddo
      endif
!
!-- pe 0 create grib2 message and write to the file
      if(me==0) then
!
         lunout=601
         call baopenw(lunout,trim(post_fname),ierr)
         print*,'write_grib2:  opened ',lunout, &
             'for grib2 data  ',trim(post_fname), &
             'return code is ',ierr
!
         do i=1,ntlfld 
           nprm=fld_info(i)%ifld
           nlvl=fld_info(i)%lvl
           fldlvl1=fld_info(i+snfld_pe(me+1)-1)%lvl1
           fldlvl2=fld_info(i+snfld_pe(me+1)-1)%lvl2
           if(trim(pset%param(nprm)%table_info).eq.'NCEP') then
             itblinfo=1
           else
             itblinfo=0
           endif
!           print *,'i=',i,'nprm=',fld_info(i)%ifld,'pname=',trim(pset%param(nprm)%pname), &
!            'lev_type=',trim(pset%param(nprm)%fixed_sfc1_type),'itblinfo=',itblinfo,      &
!            'nlvl=',nlvl,'lvl1=',fldlvl1,'lvl2=',fldlvl2, &
!            'shortname=',trim(pset%param(nprm)%shortname)
           call search_for_4dot2_entry(                                &
                pset%param(nprm)%pname,                 &
                itblinfo,                               &
                idisc, icatg, iparm, ierr)
           if(ierr==0) then
             write(6,'(3(A,I4),A,A)') '  discipline ',idisc,           &
                                      '  category ',icatg,             &
                                      '  parameter ',iparm,            &
                                      ' for var ',trim(pset%param(nprm)%pname)

            call gengrb2msg(idisc,icatg, iparm,nprm,nlvl,fldlvl1,fldlvl2,     &
                fld_info(i)%ntrange,fld_info(i)%tinvstat,datafld(:,i),       &
                cgrib,clength)
!            print *,'finished gengrb2msg field=',i,'ntlfld=',ntlfld,'clength=',clength
            call wryte(lunout, clength, cgrib)
           else
            print *,'WRONG, could not find ',trim(pset%param(nprm)%pname), &
                 " in WMO and NCEP table!, ierr=", ierr
            call mpi_abort()
           endif
         enddo
!
         call baclose(lunout,ierr)
         print *,'finish one grib file'
      endif
!
!for more fields, use pararrle i/o
    else
!
!      print *,'in grib2,num_procs=',num_procs
      allocate(iscnt(num_procs),isdsp(num_procs))
      allocate(ircnt(num_procs),irdsp(num_procs))
      isdsp(1)=0
      do n=1,num_procs
       iscnt(n)=(jend_pe(me+1)-jsta_pe(me+1)+1)*im*nfld_pe(n)
       if(n<num_procs)isdsp(n+1)=isdsp(n)+iscnt(n)
      enddo
!
      irdsp(1)=0
      do n=1,num_procs
        ircnt(n)=(jend_pe(n)-jsta_pe(n)+1)*im*nfld_pe(me+1)
        if(n<num_procs)irdsp(n+1)=irdsp(n)+ircnt(n)
      enddo
!      print *,'in grib2,iscnt=',iscnt(1:num_procs),'ircnt=',ircnt(1:num_procs), &
!       'nfld_pe=',nfld_pe(me+1)
      allocate(datafldtmp(im_jm*nfld_pe(me+1)) )
      allocate(datafld(im_jm,nfld_pe(me+1)) )
!
      call mpi_alltoallv(datapd,iscnt,isdsp,MPI_REAL,                  &
        datafldtmp,ircnt,irdsp,MPI_REAL,MPI_COMM_COMP,ierr)
!
!--- re-arrange the data
      datafld=0.
      nm=0
      do n=1,num_procs
      do k=1,nfld_pe(me+1)
      do j=jsta_pe(n),jend_pe(n)
      do i=1,im
        nm=nm+1
        datafld((j-1)*im+i,k)=datafldtmp(nm)
      enddo
      enddo
      enddo
      enddo
      deallocate(datafldtmp)
!
!-- now each process has several full domain fields, start to create grib2 message.      
!
!      print *,'nfld',nfld_pe(me+1),'snfld=',snfld_pe(me+1)
!      print *,'nprm=',   &
!         fld_info(snfld_pe(me+1):snfld_pe(me+1)+nfld_pe(me+1)-1)%ifld
!      print *,'pname=',pset%param(5)%pname
      cstart=1
      do i=1,nfld_pe(me+1)
        nprm=fld_info(i+snfld_pe(me+1)-1)%ifld
        nlvl=fld_info(i+snfld_pe(me+1)-1)%lvl
        fldlvl1=fld_info(i+snfld_pe(me+1)-1)%lvl1
        fldlvl2=fld_info(i+snfld_pe(me+1)-1)%lvl2
        ntrange=fld_info(i+snfld_pe(me+1)-1)%ntrange
        leng_time_range_stat=fld_info(i+snfld_pe(me+1)-1)%tinvstat
        if(trim(pset%param(nprm)%table_info).eq.'NCEP') then
          itblinfo=1
        else
          itblinfo=0
        endif
!        print *,'i=',i,'nprm=',nprm,'pname=',trim(pset%param(nprm)%pname), &
!            'lev_type=',trim(pset%param(nprm)%fixed_sfc1_type),'itblinfo=',itblinfo, &
!            'nlvl=',nlvl,'ntrange=',ntrange,'leng_time_range_stat=',  &
!             leng_time_range_stat,'fldlvl1=',fldlvl1,'fldlvl2=',fldlvl2,'cfld=',i+snfld_pe(me+1)-1
        call search_for_4dot2_entry(                                &
                pset%param(nprm)%pname,                 &
                itblinfo,                               &
                idisc, icatg, iparm, ierr)
       if(ierr==0) then
         write(6,'(3(A,I4),A,A)') '  discipline ',idisc,           &
                                  '  category ',icatg,             &
                                  '  parameter ',iparm,            &
                                  ' for var ',trim(pset%param(nprm)%pname)
!
!--- generate grib2 message ---
!
         call gengrb2msg(idisc,icatg, iparm,nprm,nlvl,fldlvl1,fldlvl2,ntrange,  &
                       leng_time_range_stat,datafld(:,i),cgrib(cstart),clength)
         cstart=cstart+clength
!
       else
         print *,'WRONG, could not find ',trim(pset%param(nprm)%pname), &
                 " in WMO and NCEP table!"
!!!         call mpi_abort()
       endif
!
     enddo
     cgrblen=cstart-1
!     print *,'after collect all data,cgrblen=',cgrblen
!
!******* write out grib2 message using MPI I/O *******************
!
!--- open file that will store grib2 messages
!
     call mpi_barrier(mpi_comm_comp,ierr)
!
!     print *,'bf mpi_file_open,fname=',trim(post_fname)
     call mpi_file_open(mpi_comm_comp,trim(post_fname),                       &
          mpi_mode_create+MPI_MODE_WRONLY,MPI_INFO_NULL,fh,ierr)
!     print *,'af mpi_file_open,ierr=',ierr
!
!--- broadcast message size
     allocate(grbmsglen(num_procs))
     call mpi_allgather(cgrblen,1,MPI_INTEGER,grbmsglen,1,MPI_INTEGER,         &
          mpi_comm_comp,ierr)
!     print *,'after gather gribmsg length=',grbmsglen(1:num_procs)
!
!--- setup start point
     idisp=0
     do n=1,me
      idisp=idisp+grbmsglen(n)
     enddo
!
     call mpi_file_write_at(fh,idisp,cgrib,cgrblen,MPI_CHARACTER,status,ierr)
!
     call mpi_file_close(fh,ierr)
!    etime=timef()
!    print *,'the totsl time to write 578 records is : ',etime-stime,             &
!       ' mpi_all2all time=',etime1-stime,' grib mpi write time=',stime2-stime1,   &
!       ' mpiwrt=',etime-stime2
!
!--- deallocate arrays
!
     deallocate(grbmsglen)
     deallocate(iscnt,isdsp,ircnt,irdsp)
!
   endif
!
   deallocate(datafld,bmap,mg)
   deallocate(nfld_pe,snfld_pe,enfld_pe,jsta_pe,jend_pe)
!
  end subroutine gribit2
!
!----------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------
!
  subroutine gengrb2msg(idisc,icatg, iparm,nprm,nlvl,fldlvl1,fldlvl2,ntrange,tinvstat,  &
     datafld1,cgrib,lengrib)
!
!----------------------------------------------------------------------------------------
!
    use ctlblk_mod, only : im,jm,im_jm,ifhr,idat,sdat,ihrst,ifmin,imin,fld_info,SPVAL, &
                           vtimeunits,modelname
    use gridspec_mod, only: maptype
    use grib2_all_tables_module, only: g2sec0,g2sec1,                                    &
                           g2sec4_temp0,g2sec4_temp8,g2sec4_temp44,g2sec4_temp48,        &
                           g2sec5_temp0,g2sec5_temp2,g2sec5_temp3,g2sec5_temp40,         &
                           get_g2_sec5packingmethod       
    !use gdtsec3, only: getgdtnum
    implicit none
!
    integer,intent(in) :: idisc,icatg, iparm,nprm,fldlvl1,fldlvl2,ntrange,tinvstat
    integer,intent(inout) :: nlvl
    real,dimension(:),intent(in) :: datafld1
    character(1),intent(inout) :: cgrib(max_bytes)
    integer, intent(inout) :: lengrib
!
    integer, parameter :: igdsmaxlen=200
!
    integer, parameter :: ipdstmplenmax=100
    integer, parameter :: ipdstmp4_0len=15
    integer, parameter :: ipdstmp4_1len=18
    integer, parameter :: ipdstmp4_8len=29
    integer, parameter :: ipdstmp4_11len=32
    integer, parameter :: ipdstmp4_12len=31
    integer, parameter :: ipdstmp4_44len=21
    integer, parameter :: ipdstmp4_48len=26
!
    integer, parameter :: idrstmplenmax=50
    integer, parameter :: idrstmp5_0len=5
    integer, parameter :: idrstmp5_2len=16
    integer, parameter :: idrstmp5_3len=18
    integer, parameter :: idrstmp5_40len=7
!
    integer listsec0(2)              ! Length of section 0 octets 7 & 8
    integer listsec1(13)             ! Length of section 1 from octets 6-21
    integer ipdstmpllen                   ! Length of general Section 4 PDS Template
    integer ipdstmpl(ipdstmplenmax)       ! Length of Section 4 PDS Template 4.48
    integer idrstmplen
    integer idrstmpl(idrstmplenmax)       ! Length of Section 5 PDS Template 5.40
    integer igds(5)                       ! Length of section 3 GDS Octet 6-14
    integer igdstmplen
    integer igdtlen,igdtn
    integer idefnum
    integer ideflist(100)
    integer idrsnum,numcoord,ipdsnum
    integer scaled_val_fixed_sfc2,scale_fct_fixed_sfc1
    integer scaled_val_fixed_sfc1,scale_fct_fixed_sfc2
    character(80) fixed_sfc2_type
    integer idec_scl,ibin_scl,ibmap,inumbits
    real    fldscl
    integer igdstmpl(igdsmaxlen)
    integer lat1,lon1,lat2,lon2,lad,ds1
    real(4) coordlist(1)
    logical ldfgrd
!
    integer ierr,ifhrorig,ihr_start
    integer gefs1,gefs2,gefs3,gefs_status
    character(len=4) cdum
    integer perturb_num,num_ens_fcst,e1_type
!
!----------------------------------------------------------------------------------------
! Find out if the Post is being run for the GEFS model
! Check if gen_proc is gefs
    gefs_status=0
    if(trim(pset%gen_proc)=='gefs') then
      call getenv('e1',cdum)
      read(cdum,'(I4)',iostat=gefs_status)gefs1
      e1_type=gefs1

      if(gefs_status /= 0) print *, &
      "GEFS Run: Could not read e1 envir. var, User needs to set in script"

      call getenv('e2',cdum)
      read(cdum,'(I4)',iostat=gefs_status)gefs2
      perturb_num=gefs2

      if(gefs_status /= 0) print *, &
      "GEFS Run: Could not read e2 envir. var, User needs to set in script"

      !set default number of ens forecasts to 10 for GEFS
      !num_ens_fcst=10
      call getenv('e3',cdum)
      read(cdum,'(I4)',iostat=gefs_status)gefs3
      num_ens_fcst=gefs3

      if(gefs_status /= 0) print *, &
      "GEFS Run: Could not read e3 envir. var, User needs to set in script"

      print*,'GEFS env var ',e1_type,perturb_num,num_ens_fcst

      ! Set pdstmpl to tmpl4_1 or tmpl4_11
      print *, "Processing for GEFS and default setting is tmpl4_1 and tmpl4_11"
      if (trim(pset%param(nprm)%pdstmpl)=='tmpl4_0') then
        pset%param(nprm)%pdstmpl='tmpl4_1'
      elseif (trim(pset%param(nprm)%pdstmpl)=='tmpl4_8') then
        pset%param(nprm)%pdstmpl='tmpl4_11'
      endif
    endif
!
!----------------------------------------------------------------------------------------
! Feed input keys for GRIB2 Section 0 and 1 and get outputs from arrays listsec0 and listsec1
!
       call g2sec0(idisc,listsec0)
!
!----------------------------------------------------------------------------------------
!GRIB2 - SECTION 1
!  IDENTIFICATION SECTION
!Octet No.      Content
!1-4 Length of the section in octets (21 or N)
!5   Number of the section (1)
!6-7 Identification of originating/generating center (See Table 0 {GRIB1})              ! keys_sec1(1)
!8-9 Identification of originating/generating subcenter (See Table C)                   ! keys_sec1(2)
!10  GRIB master tables version number (currently 2) (See Table 1.0) (See note 1 below) ! keys_sec1(3)
!11 Version number of GRIB local tables used to augment Master Tables (see Table 1.1)   ! keys_sec1(4)
!12 Significance of reference time (See Table 1.2)                                      ! keys_sec1(5)
!13-14 Year (4 digits)                                                                  ! keys_sec1(6)
!15 Month                                                                               ! keys_sec1(7)
!16 Day                                                                                 ! keys_sec1(8)
!17 Hour                                                                                ! keys_sec1(9)
!18 Minute                                                                              ! keys_sec1(10)
!19 Second                                                                              ! keys_sec1(11)
!20 Production Status of Processed data in the GRIB message (See Table 1.3)             ! keys_sec1(12)
!21 Type of processed data in this GRIB message (See Table 1.4)                         ! keys_sec1(13)
!22-N Reserved
!
       call g2sec1(pset%orig_center,pset%sub_center,  &
               pset%version_no,pset%local_table_vers_no,&
               pset%sigreftime,nint(sdat(3)),nint(sdat(1)),nint(sdat(2)),ihrst,imin, &
               isec,pset%prod_status,pset%data_type,listsec1)
!jw : set sect1(2) to 0 to compare with cnvgrb grib file
! For GEFS runs we need to set the section 1 values for Grib2
       if(trim(pset%gen_proc)=='gefs') then
         listsec1(2)=2
! Settings below for control (1 or 2) vs perturbed (3 or 4) ensemble forecast
         if(e1_type.eq.1.or.e1_type.eq.2) then
           listsec1(13)=3
         elseif(e1_type.eq.3.or.e1_type.eq.4) then
           listsec1(13)=4
         endif
         print *, "After g2sec1 call we need to set listsec1(2) = ",listsec1(2)
         print *, "After g2sec1 call we need to set listsec1(13) = ",listsec1(13)         
       else
         listsec1(2)=0
       endif
!
       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)
!
!----------------------------------------------------------------------------------------
! Packup Grid Definition Section (section 3) and add to GRIB2 message
!
! Define all the above derived data types and the input values will be available
! through fortran modules
!
       igdtlen=19
       ldfgrd=(MAPTYPE==203.and.(trim(pset%param(nprm)%pname)=='ugrd'.or.  &
         trim(pset%param(nprm)%pname)=='vgrd'))
       call getgds(ldfgrd,igdsmaxlen,igdtlen,igds,igdstmpl)
       idefnum=1
       ideflist=0     !Used if igds(3) .ne. 0. Dummy array otherwise
!
       call addgrid(cgrib,max_bytes,igds,igdstmpl,igdtlen,ideflist,idefnum,ierr)
!
!----------------------------------------------------------------------------------------
! Packup sections 4 through 7 for a given field and add them to a GRIB2 message which are
! Product Defintion Section, Data Representation Section, Bit-Map Section and Data Section
! respectively
!
!GRIB2 - TEMPLATE 4.0
!Product definition template analysis or forecast at a horizontal level or in a
!horizontal layer at a point in time
!Revised 09/21/2007
!Octet  Contents
!10 Parameter category (see Code table 4.1)
!11 Parameter number (see Code table 4.2)
!12 Type of generating process (see Code table 4.3)
!13 Background generating process identifier (defined by originating centre)
!14 Analysis or forecast generating process identified (see Code ON388 Table A)
!15-16 Hours of observational data cutoff after reference time (see Note)
!17 Minutes of observational data cutoff after reference time (see Note)
!18 Indicator of unit of time range (see Code table 4.4)
!19-22 Forecast time in units defined by octet 18
!23 Type of first fixed surface (see Code table 4.5)
!24 Scale factor of first fixed surface
!25-28 Scaled value of first fixed surface
!29 Type of second fixed surfaced (see Code table 4.5)
!30 Scale factor of second fixed surface
!31-34 Scaled value of second fixed surfaces
!Notes:  Hours greater than 65534 will be coded as 65534
!
!PRODUCT TEMPLATE 4. 0 :  3 5 2 0 96 0 0 1 12 100 0 100 255 0 0
!  TEXT: HGT      1 mb valid at  12 hr after 2009110500:00:00
!
       coordlist=0
       numcoord=0
!       print *,'size(level)=',size(pset%param(nprm)%level),'nlvl=',nlvl, &
!       'lev_type=',trim(pset%param(nprm)%fixed_sfc1_type),'fldlvl1=', &
!        fldlvl1,'fldlvl2=',fldlvl2
!lvl is shown in ctl file
       if(fldlvl1==0.and.fldlvl2==0) then
   
         if(size(pset%param(nprm)%level)>1.and.size(pset%param(nprm)%level)>=nlvl) then
           scaled_val_fixed_sfc1=nint(pset%param(nprm)%level(nlvl))
         else if(size(pset%param(nprm)%level)==1) then
           scaled_val_fixed_sfc1=nint(pset%param(nprm)%level(1))
         else
           scaled_val_fixed_sfc1=0
         endif
         if(size(pset%param(nprm)%scale_fact_fixed_sfc1)>1.and. &
           size(pset%param(nprm)%scale_fact_fixed_sfc1)>=nlvl) then
           scale_fct_fixed_sfc1=pset%param(nprm)%scale_fact_fixed_sfc1(nlvl)
         else if(size(pset%param(nprm)%scale_fact_fixed_sfc1)==1) then
           scale_fct_fixed_sfc1=pset%param(nprm)%scale_fact_fixed_sfc1(1)
         else
           scale_fct_fixed_sfc1=0
         endif
!
!for hygrid dpes level is decided in the code, not from ctl file
       else
         scaled_val_fixed_sfc1=fldlvl1
         scale_fct_fixed_sfc1=0
         scaled_val_fixed_sfc2=fldlvl2
         scale_fct_fixed_sfc2=0
       endif

       fixed_sfc2_type=pset%param(nprm)%fixed_sfc2_type
       if(size(pset%param(nprm)%level2)>1.and.size(pset%param(nprm)%level2)<nlvl) then
         fixed_sfc2_type=''
       endif
       if(size(pset%param(nprm)%level2)>1.and.size(pset%param(nprm)%level2)>=nlvl) then
         scaled_val_fixed_sfc2=nint(pset%param(nprm)%level2(nlvl))
       else if(size(pset%param(nprm)%level2)==1) then
         scaled_val_fixed_sfc2=nint(pset%param(nprm)%level2(1))
       else
         scaled_val_fixed_sfc2=0
       endif
       if(size(pset%param(nprm)%scale_fact_fixed_sfc2)>1 .and. &
          size(pset%param(nprm)%scale_fact_fixed_sfc2)>=nlvl) then
         scale_fct_fixed_sfc2=pset%param(nprm)%scale_fact_fixed_sfc2(nlvl)
       else if(size(pset%param(nprm)%scale_fact_fixed_sfc2)==1) then
         scale_fct_fixed_sfc2=pset%param(nprm)%scale_fact_fixed_sfc2(1)
       else
         scale_fct_fixed_sfc2=0
       endif

       ihr_start = ifhr-tinvstat 
       if(modelname=='RAPR'.and.vtimeunits=='FMIN') then
         ifhrorig = ifhr
         ifhr = ifhr*60 + ifmin
       else
         if(ifmin > 0.)then  ! change time range unit to minute
            pset%time_range_unit="minute"
            ifhrorig = ifhr
            ifhr = ifhr*60 + ifmin
            ihr_start = max(0,ifhr-tinvstat*60)
         end if
       end if
!        print *,'bf g2sec4_temp0,ipdstmpl=',trim(pset%param(nprm)%pdstmpl),'fixed_sfc_type=',   &
!        pset%param(nprm)%fixed_sfc1_type,'scale_fct_fixed_sfc1=',      &
!        scale_fct_fixed_sfc1,'scaled_val_fixed_sfc1=',scaled_val_fixed_sfc1, &
!        'sfc2_type=',trim(pset%param(nprm)%fixed_sfc2_type),scale_fct_fixed_sfc2, &
!        scaled_val_fixed_sfc2
     
       if(trim(pset%param(nprm)%pdstmpl)=='tmpl4_0') then
         ipdsnum=0
         ipdstmpllen=ipdstmp4_0len
         call g2sec4_temp0(icatg,iparm,pset%gen_proc_type,       &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ifhr,                       &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              fixed_sfc2_type,                                 &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp0,ipdstmpl0=',ipdstmpl(1:ipdstmp4_0len)
       elseif(trim(pset%param(nprm)%pdstmpl)=='tmpl4_1') then
         ipdsnum=1
         ipdstmpllen=ipdstmp4_1len
         call g2sec4_temp1(icatg,iparm,pset%gen_proc_type,     &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ifhr,                       &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              fixed_sfc2_type,                                 &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              pset%type_ens_fcst,perturb_num,num_ens_fcst,     &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp1,ipdstmpl1=',ipdstmpl(1:ipdstmp4_1len)
!
       elseif(trim(pset%param(nprm)%pdstmpl)=='tmpl4_8') then
!
         ipdsnum=8
         ipdstmpllen=ipdstmp4_8len
         call g2sec4_temp8(icatg,iparm,pset%gen_proc_type,       &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ihr_start,              &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              pset%param(nprm)%fixed_sfc2_type,                &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              idat(3),idat(1),idat(2),idat(4),idat(5),         &
              sec_intvl,ntrange,stat_miss_val,                 &
              pset%param(nprm)%stats_proc,type_of_time_inc,    &
              pset%time_range_unit, tinvstat,                  &
              stat_unit_time_key_succ,time_inc_betwn_succ_fld, &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp8,ipdstmpl8=',ipdstmpl(1:ipdstmp4_8len)

       elseif(trim(pset%param(nprm)%pdstmpl)=='tmpl4_11') then
         ipdsnum=11
         ipdstmpllen=ipdstmp4_11len
         call g2sec4_temp11(icatg,iparm,pset%gen_proc_type,    &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ifhr-tinvstat,              &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              pset%param(nprm)%fixed_sfc2_type,                &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              pset%type_ens_fcst,perturb_num,num_ens_fcst,     &
              idat(3),idat(1),idat(2),idat(4),idat(5),         &
              sec_intvl,ntrange,stat_miss_val,                 &
              pset%param(nprm)%stats_proc,type_of_time_inc,    &
              pset%time_range_unit, tinvstat,                  &
              stat_unit_time_key_succ,time_inc_betwn_succ_fld, &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp11,ipdstmpl11=',ipdstmpl(1:ipdstmp4_11len)

       elseif(trim(pset%param(nprm)%pdstmpl)=='tmpl4_12') then
         ipdsnum=12
         ipdstmpllen=ipdstmp4_12len
         call g2sec4_temp12(icatg,iparm,pset%gen_proc_type,    &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ifhr-tinvstat,              &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              fixed_sfc2_type,                                 &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              pset%type_derived_fcst,num_ens_fcst,     &
              idat(3),idat(1),idat(2),idat(4),idat(5),         &
              sec_intvl,ntrange,stat_miss_val,                 &
              pset%param(nprm)%stats_proc,type_of_time_inc,    &
              pset%time_range_unit, tinvstat,                  &
              stat_unit_time_key_succ,time_inc_betwn_succ_fld, &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp12,ipdstmpl12=',ipdstmpl(1:ipdstmp4_12len)

       elseif(trim(pset%param(nprm)%pdstmpl)=='tmpl4_44') then
!
         ipdsnum=44
         ipdstmpllen=ipdstmp4_44len
         call g2sec4_temp44(icatg,iparm,pset%param(nprm)%aerosol_type, &
              pset%param(nprm)%typ_intvl_size,                 &
              pset%param(nprm)%scale_fact_1st_size,            &
              pset%param(nprm)%scale_val_1st_size,             &
              pset%param(nprm)%scale_fact_2nd_size,            &
              pset%param(nprm)%scale_val_2nd_size,             &
              pset%gen_proc_type,                              &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ifhr,                       &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              pset%param(nprm)%fixed_sfc2_type,                &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp44,ipdstmpl44=',ipdstmpl(1:ipdstmp4_44len),'ipdsnum=',ipdsnum

       elseif(trim(pset%param(nprm)%pdstmpl)=='tmpl4_48') then
!
         ipdsnum=48
         ipdstmpllen=ipdstmp4_48len
         call g2sec4_temp48(icatg,iparm,pset%param(nprm)%aerosol_type, &
              pset%param(nprm)%typ_intvl_size,                 &
              pset%param(nprm)%scale_fact_1st_size,            &
              pset%param(nprm)%scale_val_1st_size,             &
              pset%param(nprm)%scale_fact_2nd_size,            &
              pset%param(nprm)%scale_val_2nd_size,             &
              pset%param(nprm)%typ_intvl_wvlen,                &
              pset%param(nprm)%scale_fact_1st_wvlen,           &
              pset%param(nprm)%scale_val_1st_wvlen,            &
              pset%param(nprm)%scale_fact_2nd_wvlen,           &
              pset%param(nprm)%scale_val_2nd_wvlen,            &
              pset%gen_proc_type,                              &
              pset%gen_proc,hrs_obs_cutoff,min_obs_cutoff,     &
              pset%time_range_unit,ifhr,                       &
              pset%param(nprm)%fixed_sfc1_type,                &
              scale_fct_fixed_sfc1,                            &
              scaled_val_fixed_sfc1,                           &
              pset%param(nprm)%fixed_sfc2_type,                &
              scale_fct_fixed_sfc2,                            &
              scaled_val_fixed_sfc2,                           &
              ipdstmpl(1:ipdstmpllen))
!       print *,'aft g2sec4_temp48,name=',trim(pset%param(nprm)%shortname),&
!          'ipdstmpl48=',ipdstmpl(1:ipdstmp4_48len)

      endif

      if(ifmin>0.)then
       ifhr = ifhrorig
      end if


!
!----------
! idrstmpl array is the output from g2sec5
!
       call get_g2_sec5packingmethod(pset%packing_method,idrsnum,ierr)
       if(maxval(datafld1)==minval(datafld1))then
        idrsnum=0
        print*,' changing to simple packing for constant fields'
       end if 
!       print *,'aft g2sec5,packingmethod=',pset%packing_method,'idrsnum=',idrsnum, &
!         'data=',maxval(datafld1),minval(datafld1)
!
!*** set number of bits, and binary scale
!
       ibmap=255
       bmap=.true.
       if(any(datafld1>=SPVAL))then
         ibmap=0
         where(abs(datafld1)>=SPVAL)bmap=.false.
       endif
!
       if(size(pset%param(nprm)%level)==size(pset%param(nprm)%scale)) then
         if(size(pset%param(nprm)%level)==1) nlvl=1
         if(nlvl/=0) then
           fldscl=nint(pset%param(nprm)%scale(nlvl))
         else
           fldscl=0
         endif
       else if (size(pset%param(nprm)%scale)==1) then
         fldscl=nint(pset%param(nprm)%scale(1))
       endif
!
       call g2getbits(ibmap,fldscl,size(datafld1),bmap,datafld1,ibin_scl,idec_scl,inumbits)
!       print *,'idec_scl=',idec_scl,'ibin_scl=',ibin_scl,'number_bits=',inumbits
       if( idrsnum==40 ) then
         idrstmplen=idrstmp5_40len
         call g2sec5_temp40(idec_scl,ibin_scl,inumbits,pset%comprs_type,idrstmpl(1:idrstmplen))
       elseif( idrsnum==2 ) then
         idrstmplen=idrstmp5_2len
         call g2sec5_temp2(idec_scl,ibin_scl,idrstmpl(1:idrstmplen))
       elseif( idrsnum==3 ) then
         idrstmplen=idrstmp5_3len
         call g2sec5_temp3(idec_scl,ibin_scl,pset%order_of_sptdiff,idrstmpl(1:idrstmplen))
       elseif( idrsnum==0 ) then
         idrstmplen=idrstmp5_0len
         call g2sec5_temp0(idec_scl,ibin_scl,inumbits,idrstmpl(1:idrstmplen))
       endif
!
!----------------------------------------------------------------------------------------
! Define all required inputs like ibmap, numcoord, coordlist etc externally in the module
! prior to calling the addfield routine. Again hide the addfield routine from the user
!
!         print *,'before addfield, data=',maxval(datafld1),minval(datafld1),'ibmap=',ibmap, &
!        'max_bytes=',max_bytes,'ipdsnum=',ipdsnum,'ipdstmpllen=',ipdstmpllen,'ipdstmpl=',ipdstmpl(1:ipdstmpllen), &
!        'coordlist=',coordlist,'numcoord=',numcoord,'idrsnum=',idrsnum,'idrstmpl=',idrstmpl,  &
!        'idrstmplen=',idrstmplen,'im_jm=',im_jm

       call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl(1:ipdstmpllen),         &
                     ipdstmpllen,coordlist,numcoord,idrsnum,idrstmpl,         &
                     idrstmplen ,datafld1,im_jm,ibmap,bmap,ierr)
!
!---------------------------------------------------------------------------------------
! Finalize GRIB message after all grids and fields have been added by adding the end
! section "7777"
! Again hide the gribend routine from the user
!
       call gribend(cgrib,max_bytes,lengrib,ierr)
!
!-------
  end subroutine gengrb2msg
!
!--------------------------------------------------------------------------------------
!
  subroutine g2sec3tmpl40(nx,nY,lat1,lon1,lat2,lon2,lad,ds1,len3,igds,ifield3)
   implicit none
!
       integer(4),intent(inout) :: nx,ny,lat1,lon1,lat2,lon2,lad,ds1,len3
       integer(4),intent(inout) :: ifield3(len3),igds(5)
!
       nx=1152
       ny=576
       lat1=89761000   !lat of 1st grd pt in micro-deg
       lon1=0          !east-long of 1st grd pt in micro-deg
       lat2=-89761000  !lat of last grd pt in micro-deg
       lon2=359687000  !east-long of last grd pt in micro-deg
       lad=313000      !lat at which projection intersects earth
       ds1=288         !grid spacing in x and y
!
       igds=0
       igds(2)=nx*ny
       igds(5)=40
!
       ifield3(1) = 6 !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(2) = 0
       ifield3(3) = 0
       ifield3(4) = 0
       ifield3(5) = 0
       ifield3(6) = 0
       ifield3(7) = 0
       ifield3(8) = nx
       ifield3(9) = ny
       ifield3(10) = 0
       ifield3(11) = 0
       ifield3(12) = lat1
       ifield3(13) = lon1
       ifield3(14) = 48
       ifield3(15) = lat2
       ifield3(16) = lon2
       ifield3(17) = lad
       ifield3(18) = ds1
       ifield3(19) = 0

       return
       end subroutine g2sec3tmpl40
!
!-------------------------------------------------------------------------------------
!
       subroutine g2getbits(ibm,scl,len,bmap,g,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total number of bits,
!   The argument list is modified to have ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false. skip)
!   fld: real(len), datafield 
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo,le,irange
      REAL    :: GROUND,GMIN,GMAX,s,rmin,rmax,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
      ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF

!     write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range
!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
!
      IF ( scl > 0.0 ) THEN 
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
 !        I1=1
 !        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
 !          I1=I1+1
 !        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
!     write(0,*)'in g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',& 
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits
!
!-------------------------------------------------------------------------------------
!
      subroutine getgds(ldfgrd,len3,ifield3len,igds,ifield3)
!     
!***** set up gds kpds to call Boi's code
!
      use CTLBLK_mod,  only : im,jm
      use gridspec_mod, only: DXVAL,DYVAL,CENLAT,CENLON,LATSTART,LONSTART,LATLAST,     &
     &                        LONLAST,MAPTYPE,STANDLON,latstartv,cenlatv,lonstartv,    &
                              cenlonv,TRUELAT1,TRUELAT2
!   
      implicit none
!
      logical,   intent(in)    :: ldfgrd
      integer(4),intent(in)    :: len3
      integer(4),intent(out)   :: ifield3len
      integer(4),intent(inout) :: ifield3(len3),igds(5)
    
!      print *,'in getgds, im=',im,'jm=',jm,'latstart=',latstart,'lonsstart=',lonstart,'maptyp=',maptype
!
!** set up igds 
      igds(1) = 0      !Source of grid definition (see Code Table 3.0)
      igds(2) = im*jm  !Number of grid points in the defined grid.
      igds(3) = 0      !Number of octets needed for each additional grid points definition
      igds(4) = 0      !Interpretation of list for optional points definition (Code Table 3.11)
!
!** define grid template 3
      IF(MAPTYPE == 1) THEN     !LAmbert Conformal
       igds(5)     = 30         !Lambert conformal
       ifield3len  = 22
       ifield3     = 0
!
       ifield3(1)  = 6          !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8)  = im         !number of points along the x-axis
       ifield3(9)  = jm         !number of points along the y-axis
       ifield3(10) = latstart   !latitude of first grid point
       ifield3(11) = lonstart   !longitude of first grid point
       ifield3(12) = 8          !Resolution and component flags
       ifield3(13) = TRUELAT1
       ifield3(14) = STANDLON   !longitude of meridian parallel to y-axis along which latitude increases
       ifield3(15) = DXVAL
       ifield3(16) = DYVAL
       IF(TRUELAT1>0)then
        ifield3(17) = 0
       else
        ifield3(17) =128        !for southern hemisphere
       end if
       ifield3(18) = 64
       ifield3(19) = TRUELAT1   !first latitude from the pole at which the secant cone cuts the sphere
       ifield3(20) = TRUELAT2   !second latitude from the pole at which the secant cone cuts the sphere

!** Polar stereographic
     ELSE IF(MAPTYPE == 2)THEN  !Polar stereographic
       igds(5)     = 20
       ifield3len  = 22
       ifield3     = 0
!
       ifield3(1)  = 6           !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8)  = im         !number of points along the x-axis
       ifield3(9)  = jm         !number of points along the y-axis
       ifield3(10) = latstart   !latitude of first grid point
       ifield3(11) = lonstart   !longitude of first grid point
       ifield3(12) = 8          !Resolution and component flags
       ifield3(13) = TRUELAT1
       ifield3(14) = STANDLON   !longitude of meridian parallel to y-axis along which latitude increases
       ifield3(15) = DXVAL
       ifield3(16) = DYVAL
       ifield3(17) = 0 
       ifield3(18) = 64
!
!** Mercator
      ELSE IF(MAPTYPE.EQ.3)THEN !Mercator
       igds(5)=10
       ifield3len=22
       ifield3=0
!
       ifield3(1) = 6           !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8) = im          !number of points along the x-axis
       ifield3(9) = jm          !number of points along the y-axis
       ifield3(10) = latstart   !latitude of first grid point
       ifield3(11) = lonstart   !longitude of first grid point
       ifield3(12) = 8          !Resolution and component flags
       ifield3(13) = TRUELAT1   !latitude(s) at which the Mercator projection intersects the Earth
       ifield3(14) = latlast    !latitude of last grid point
       ifield3(15) = lonlast    !longitude of last grid point 
       ifield3(16) = 64         !Scanning mode
       ifield3(17) = 0          !Orientation of the grid, angle between i direction on the map and Equator
       ifield3(18) = DXVAL
       ifield3(19) = DYVAL
!
!** ARAKAWA STAGGERED E-GRID
      ELSE IF(MAPTYPE == 203)THEN  !ARAKAWA STAGGERED E-GRID`
       igds(5)     = 32768
       ifield3len  = 22
       ifield3     = 0
!
       ifield3(1)  = 6          !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8)  = im         !number of points along the x-axis
       ifield3(9)  = jm         !number of points along the y-axis
       ifield3(10) = 0          !Basic angle of the initial production domain 
       if(.not.ldfgrd) then
         ifield3(11) = 0          !Subdivisions of basic angle used to define extreme lons & lats:missing 
       else
         ifield3(11) = 45000000   !Subdivisions of basic angle used to define extreme lons & lats
       endif
       ifield3(12) = latstart   !latitude of first grid point
       ifield3(13) = lonstart   !longitude of first grid point
       ifield3(14) = 0          !Resolution and component flags
       ifield3(15) = CENLAT     !center latitude of grid point
       ifield3(16) = CENLON     !Center longitude of grid point
       ifield3(17) = DXVAL
       ifield3(18) = DYVAL
       ifield3(19) = 64         !Scanning mode
!
!** ARAKAWA STAGGERED non-E-GRID
      ELSE IF(MAPTYPE == 205)THEN  !ARAKAWA STAGGERED E-GRID`
       igds(5)     = 32769
       ifield3len  = 21
       ifield3     = 0
!
       ifield3(1)  = 6          !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8)  = im         !number of points along the x-axis
       ifield3(9)  = jm         !number of points along the y-axis
       ifield3(10) = 0          !Basic angle of the initial production domain
       if(.not.ldfgrd) then
         ifield3(11) = 0        !Subdivisions of basic angle used to define extreme lons & lats:missing
       else
         ifield3(11) = 45000000 !Subdivisions of basic angle used to define extreme lons & lats
       endif
       ifield3(12) = latstart   !latitude of first grid point
       ifield3(13) = lonstart   !longitude of first grid point
       ifield3(14) = 56         !Resolution and component flags
       ifield3(15) = CENLAT     !center latitude of grid point
       ifield3(16) = CENLON     !Center longitude of grid point
       ifield3(17) = DXVAL
       ifield3(18) = DYVAL
       ifield3(19) = 64         !Scanning mode
       ifield3(20) = latlast    !Scanning mode
       ifield3(21) = lonlast    !Scanning mode



!
!** Gaussian grid
      ELSE IF(MAPTYPE == 4 ) THEN  !Gaussian grid 
       igds(5)     = 40
       ifield3len  = 19
       ifield3     = 0
!
       ifield3(1)  = 6 !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8)  = im
       ifield3(9)  = jm
       ifield3(10) = 0   
       ifield3(11) = 0   
       ifield3(12) = latstart
       ifield3(13) = lonstart
       ifield3(14) = 48     
       ifield3(15) = latlast 
       ifield3(16) = lonlast 
       ifield3(17) = NINT(360./(IM)*1000000.)
       ifield3(18) = NINT(JM/2.0)
!
!** Latlon grid
      ELSE IF(MAPTYPE == 0 ) THEN
       igds(5)     = 0
       ifield3len  = 19
       ifield3     = 0
!
       ifield3(1)  = 6 !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(8)  = im
       ifield3(9)  = jm
       ifield3(10) = 0
       ifield3(11) = 0
       ifield3(12) = latstart
       ifield3(13) = lonstart
       ifield3(14) = 48     
       ifield3(15) = latlast
       ifield3(16) = lonlast
       ifield3(17) = NINT(180./(JM-1)*1.0E6) 
       ifield3(18) = NINT(360./(IM)*1.0E6) 
       ifield3(19) = 0 

     ENDIF

!    write(0,*)'igds=',igds,'igdstempl=',ifield3(1:ifield3len)
     end subroutine getgds
!
!-------------------------------------------------------------------------------------
!
  end module grib2_module
