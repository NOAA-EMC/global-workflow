module sfcobsqc
!$$$ module documentation block
!           .      .    .                                       .
! module:   sfcobsqc
!   prgmmr: pondeca
!
! abstract: contains subroutines for the qc of surface obs based
!           on (i) the provider uselist for mesonet winds, (ii) 
!           station uselist for mesonet winds, and (iii) rejectlists 
!           for any ob type (u and v-wind, wind speed, temperatature, 
!           moisture, surface pressure). the code inquires for the 
!           existence of thsese lists in the gsi working directorty 
!           and applies them if present.
!           
!
! program history log:
!   2007-10-19  pondeca
!   2011-02-15  zhu - add get_gustqm
!   2014-04-10  pondeca - add reject lists for td, mxtm, mitm, pmsl
!   2014-05-07  pondeca - add reject list for howv
!   2014-07-11  carley - add reject list for lcbas and tcamt
!   2014-10-01  Xue - add GSD surface data uselist
!   2015-07-10  pondeca - add reject list for cldch
!   2018-03-14  pondeca - add station accept list for mesonet visibility
!
! subroutines included:
!   sub init_rjlists
!   sub get_usagerj
!   sub get_gustqm
!   readin_rjlists
!   sub destroy_rjlists
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind,r_single,r_double

  implicit none

  private

  character(16),allocatable,dimension(:)::cprovider
  character(5),allocatable,dimension(:)::csta_winduse
  character(80),allocatable,dimension(:)::w_rjlist,t_rjlist,p_rjlist,q_rjlist
  character(80),allocatable,dimension(:)::td_rjlist,mxtm_rjlist,mitm_rjlist,pmsl_rjlist,howv_rjlist, &
                                          lcbas_rjlist,tcamt_rjlist,cldch_rjlist
  character(80),allocatable,dimension(:)::t_day_rjlist,t_night_rjlist
  character(80),allocatable,dimension(:)::q_day_rjlist,q_night_rjlist
  character(8),allocatable,dimension(:,:)::csta_windbin
  character(80),allocatable,dimension(:)::csta_visuse

  integer(i_kind) sfcuselist_nt_use
  character(8),allocatable,dimension(:)::sfcuselist_use_id
  character(1),allocatable,dimension(:)::w_use_sfcuselist
  character(1),allocatable,dimension(:)::t_use_sfcuselist
  character(1),allocatable,dimension(:)::td_use_sfcuselist

  integer(i_kind) nprov,nwrjs,ntrjs,nprjs,nqrjs,nsta_mesowind_use,nsta_mesovis_use
  integer(i_kind) ntdrjs,nmxtmrjs,nmitmrjs,npmslrjs,nhowvrjs,&
                  nlcbasrjs,ntcamtrjs,ncldchrjs
  integer(i_kind) ntrjs_day,ntrjs_night
  integer(i_kind) nqrjs_day,nqrjs_night
  integer(i_kind) nbins
  integer(i_kind),allocatable,dimension(:)::nwbaccpts

  logical gsdsfclistexist
  logical gsdsfcproviderlistexist
  logical listexist
  logical wlistexist
  logical tlistexist
  logical plistexist
  logical qlistexist
  logical tdlistexist
  logical mxtmlistexist
  logical mitmlistexist
  logical pmsllistexist
  logical howvlistexist
  logical lcbaslistexist
  logical tcamtlistexist
  logical cldchlistexist
  logical listexist2
  logical t_day_listexist
  logical t_night_listexist
  logical q_day_listexist
  logical q_night_listexist
  logical wbinlistexist
  logical vis_uselistexist

  public init_rjlists
  public get_usagerj
  public get_gustqm
  public readin_rjlists
  public get_sunangle
  public get_wbinid
  public destroy_rjlists

  public init_gsd_sfcuselist
  public apply_gsd_sfcuselist
  public destroy_gsd_sfcuselist

  logical :: verbose = .false.
contains

subroutine init_gsd_sfcuselist
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_gsd_sfcuselist
!   prgmmr:
!
! abstract: read in GSD surface observation uselist 
!
! program history log:
!   2014-10-01  Xue - initial code
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

  use mpimod, only: mype

  implicit none

  integer(i_kind) use_unit
  character(150) cstring
  character(80) clistname

  integer(i_kind), parameter::nmax=60000_i_kind

  data use_unit / 777_i_kind /
!**************************************************************************
  if (mype==0) verbose =.false.
  sfcuselist_nt_use= 0
  allocate(sfcuselist_use_id(nmax))
  allocate(w_use_sfcuselist(nmax))
  allocate(t_use_sfcuselist(nmax))
  allocate(td_use_sfcuselist(nmax))

  gsdsfclistexist=.false.

  inquire(file='gsd_sfcobs_uselist.txt',exist=gsdsfclistexist)
  if(gsdsfclistexist) then
    open (use_unit,file='gsd_sfcobs_uselist.txt',form='formatted')

    read_use: do
      read(use_unit,'(a150)',end=7745) cstring
      if(cstring(1:1) == ';') cycle read_use    ! skip comments marked as ;

      sfcuselist_nt_use=sfcuselist_nt_use+1
      sfcuselist_use_id(sfcuselist_nt_use)= adjustl(cstring(1:10))
      w_use_sfcuselist(sfcuselist_nt_use)= adjustl(cstring(11:12))
      t_use_sfcuselist(sfcuselist_nt_use)= adjustl(cstring(13:14))
      td_use_sfcuselist(sfcuselist_nt_use)= adjustl(cstring(15:16))
      if(verbose) print*,'sfcuselist_use_id=',sfcuselist_nt_use,&
                                    sfcuselist_use_id(sfcuselist_nt_use),&
                                ",",w_use_sfcuselist(sfcuselist_nt_use),&
                                ",",t_use_sfcuselist(sfcuselist_nt_use),&
                                ",",t_use_sfcuselist(sfcuselist_nt_use)

    end do read_use
7745 continue
  endif
  close(use_unit)

!==> Read mesonet provider names from the uselist
  clistname='gsd_sfcobs_provider.txt'
  call readin_rjlists(clistname,gsdsfcproviderlistexist,cprovider,500,nprov)
  if(verbose)&
    print*,'mesonetproviderlist: provider,nprov=',gsdsfcproviderlistexist,nprov


end subroutine init_gsd_sfcuselist

subroutine destroy_gsd_sfcuselist
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_gsd_sfcuselist
!   prgmmr:
!
! abstract:
!
! program history log:
!   2015-02-05  Hu - added subprogram doc block
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

  deallocate(sfcuselist_use_id)
  deallocate(w_use_sfcuselist)
  deallocate(t_use_sfcuselist)
  deallocate(td_use_sfcuselist)

end subroutine destroy_gsd_sfcuselist

subroutine apply_gsd_sfcuselist(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                       usage_rj)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    apply_gsd_sfcuselist
!   prgmmr:
!
! abstract: use GSD surface observation uselist  to decide
!           which surface observation should be used in the analysis
!
! program history log:
!   2014-10-01  Xue - initial code
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



  use constants, only: zero_single
  implicit none

  integer(i_kind),intent(in   ) :: kx
  character(10)  ,intent(in   ) :: obstype
  character(8)   ,intent(in   ) :: c_station_id
  character(8)   ,intent(in   ) :: c_prvstg,c_sprvstg
  real(r_kind)   ,intent(inout) :: usage_rj

! Declare local variables
  integer(i_kind) m,nlen
  character(8)  ch8
  real(r_kind) usage_rj0

! Declare local parameters
  real(r_kind),parameter:: r6    = 6.0_r_kind
  real(r_kind),parameter:: r5000 = 5000._r_kind
  real(r_kind),parameter:: r5100 = 5100._r_kind
  real(r_kind),parameter:: r6000 = 6000._r_kind
  real(r_kind),parameter:: r6100 = 6100._r_kind
  real(r_kind),parameter:: r6200 = 6200._r_kind

  if (usage_rj >= r6) return  

  usage_rj0=usage_rj
  usage_rj=r6000

  if(gsdsfcproviderlistexist) then
     do m=1,nprov
        if (c_prvstg(1:8) == cprovider(m)(1:8) .and. &
           (c_sprvstg(1:8) == cprovider(m)(9:16) .or. &
            cprovider(m)(9:16) == 'allsprvs') ) then
              usage_rj=usage_rj0
              exit
        endif
     enddo
  endif

  if(.not.gsdsfclistexist) return 
  if (usage_rj==usage_rj0) return  ! station is in provider list, use it
                                   ! if not (usage_rj=r6000), check uselist

  if (kx<200 .and. (obstype=='t' .or. obstype=='q')) then  !<==mass obs
! assume everything is reject first, get back those obs exist in uselist 
    usage_rj= r5000                                           
    if(obstype=='t') then
        do m=1,sfcuselist_nt_use
           ch8(1:8)=sfcuselist_use_id(m)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
             if (t_use_sfcuselist(m)=='0') then  ! put it to reject list
                usage_rj=r5000
             elseif(t_use_sfcuselist(m)=='1') then ! use it original usage value
                usage_rj= usage_rj0                                          
             endif
             exit
           endif
        enddo
     elseif(obstype=='q') then
        do m=1,sfcuselist_nt_use
           ch8(1:8)=sfcuselist_use_id(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
              if (td_use_sfcuselist(m)=='0') then  ! put it to reject list
                 usage_rj=r5000
             elseif(td_use_sfcuselist(m)=='1') then ! use it original usage value
                usage_rj= usage_rj0                                          
             endif
             exit
           endif
        enddo
    endif
   endif ! kx < 200

   if (kx>=200) then  ! wind vector obs
     if(obstype=='uv') then
        usage_rj= r5000                                           
        do m=1,sfcuselist_nt_use
           ch8(1:8)=sfcuselist_use_id(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==288.or.kx==295).and. c_station_id(1:nlen)==ch8(1:nlen))) then
              if (w_use_sfcuselist(m)=='0') then  ! put it to reject list
                 usage_rj=r5000
             elseif(w_use_sfcuselist(m)=='1') then ! use it original usage value
                usage_rj= usage_rj0                                          
             endif
             exit
           endif
        enddo
     endif
   end if

end subroutine apply_gsd_sfcuselist

subroutine init_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_rjlists
!   prgmmr:
!
! abstract: initialize qc lists 
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

  use mpimod, only: mype

  implicit none

! Declare passed variables

! Declare local variables
  integer(i_kind) meso_unit,ncount
  integer(i_kind) ibin
  real(r_single) aa1,aa2
  character(80) cstring
  character(80) clistname
  character(8) ach8,bch8
  character(16) ch16

  integer(i_kind), parameter::nmax=100000
  integer(i_kind) nmax2

  data meso_unit / 20 /
!**************************************************************************
  if (mype==0) verbose =.true.

  allocate(cprovider(500))
  allocate(w_rjlist(nmax))
  allocate(t_rjlist(nmax))
  allocate(p_rjlist(nmax))
  allocate(q_rjlist(nmax))
  allocate(td_rjlist(nmax))
  allocate(mxtm_rjlist(nmax))
  allocate(mitm_rjlist(nmax))
  allocate(pmsl_rjlist(nmax))
  allocate(howv_rjlist(nmax))
  allocate(lcbas_rjlist(nmax))
  allocate(tcamt_rjlist(nmax))
  allocate(cldch_rjlist(nmax))
  allocate(csta_winduse(nmax))
  allocate(t_day_rjlist(nmax))
  allocate(t_night_rjlist(nmax))
  allocate(q_day_rjlist(nmax))
  allocate(q_night_rjlist(nmax))
  allocate(csta_visuse(nmax))

!==> Read mesonet provider names from the uselist
 clistname='mesonetuselist'
 call readin_rjlists(clistname,listexist,cprovider,500,nprov)
 if(verbose)&
 print*,'mesonetuselist: listexist,nprov=',listexist,nprov


 
!==> Read in station names from the reject list for wind
 clistname='w_rejectlist'
 call readin_rjlists(clistname,wlistexist,w_rjlist,nmax,nwrjs)
 if(verbose)&
 print*,'w_rejectlist: wlistexist,nwrjs=',wlistexist,nwrjs


 
!==> Read in station names from the reject lists for temperature
 clistname='t_rejectlist'
 call readin_rjlists(clistname,tlistexist,t_rjlist,nmax,ntrjs)
 if(verbose)&
 print*,'t_rejectlist: tlistexist,ntrjs=',tlistexist,ntrjs


 
 clistname='t_day_rejectlist'
 call readin_rjlists(clistname,t_day_listexist,t_day_rjlist,nmax,ntrjs_day)
 if(verbose)&
 print*,'t_day_rejectlist: t_day_listexist,ntrjs_day=',t_day_listexist,ntrjs_day


 
 clistname='t_night_rejectlist'
 call readin_rjlists(clistname,t_night_listexist,t_night_rjlist,nmax,ntrjs_night)
 if(verbose)&
 print*,'t_night_rejectlist: t_night_listexist,ntrjs_night=',t_night_listexist,ntrjs_night


 
!==> Read in station names from the reject list for surface pressure
 clistname='p_rejectlist'
 call readin_rjlists(clistname,plistexist,p_rjlist,nmax,nprjs)
 if(verbose)&
 print*,'p_rejectlist: plistexist,nprjs=',plistexist,nprjs


 
!==> Read in station names from the reject lists for specific humidity
 clistname='q_rejectlist'
 call readin_rjlists(clistname,qlistexist,q_rjlist,nmax,nqrjs)
 if(verbose)&
 print*,'q_rejectlist: qlistexist,nqrjs=',qlistexist,nqrjs


 
 clistname='q_day_rejectlist'
 call readin_rjlists(clistname,q_day_listexist,q_day_rjlist,nmax,nqrjs_day)
 if(verbose)&
 print*,'q_day_rejectlist: q_day_listexist,nqrjs_day=',q_day_listexist,nqrjs_day

  

 clistname='q_night_rejectlist'
 call readin_rjlists(clistname,q_night_listexist,q_night_rjlist,nmax,nqrjs_night)
 if(verbose)&
 print*,'q_night_rejectlist: q_night_listexist,nqrjs_night=',q_night_listexist,nqrjs_night



 clistname='td_rejectlist'
 call readin_rjlists(clistname,tdlistexist,td_rjlist,nmax,ntdrjs)
 if(verbose)&
 print*,'td_rejectlist: tdlistexist,ntdrjs=',tdlistexist,ntdrjs

 clistname='mxtm_rejectlist'
 call readin_rjlists(clistname,mxtmlistexist,mxtm_rjlist,nmax,nmxtmrjs)
 if(verbose)&
 print*,'mxtm_rejectlist: mxtmlistexist,nmxtmrjs=',mxtmlistexist,nmxtmrjs

 clistname='mitm_rejectlist'
 call readin_rjlists(clistname,mitmlistexist,mitm_rjlist,nmax,nmitmrjs)
 if(verbose)&
 print*,'mitm_rejectlist: mitmlistexist,nmitmrjs=',mitmlistexist,nmitmrjs


 clistname='pmsl_rejectlist'
 call readin_rjlists(clistname,pmsllistexist,pmsl_rjlist,nmax,npmslrjs)
 if(verbose)&
 print*,'pmsl_rejectlist: pmsllistexist,npmslrjs=',pmsllistexist,npmslrjs


 clistname='howv_rejectlist'
 call readin_rjlists(clistname,howvlistexist,howv_rjlist,nmax,nhowvrjs)
 if(verbose)&
 print*,'howv_rejectlist: howvlistexist,nhowvrjs=',howvlistexist,nhowvrjs
 

 clistname='lcbas_rejectlist'
 call readin_rjlists(clistname,lcbaslistexist,lcbas_rjlist,nmax,nlcbasrjs)
 if(verbose)&
 print*,'lcbas_rejectlist: lcbaslistexist,nlcbasrjs=',lcbaslistexist,nlcbasrjs

 clistname='tcamt_rejectlist'
 call readin_rjlists(clistname,tcamtlistexist,tcamt_rjlist,nmax,ntcamtrjs)
 if(verbose)&
 print*,'tcamt_rejectlist: tcamtlistexist,ntcamtrjs=',tcamtlistexist,ntcamtrjs

 clistname='cldch_rejectlist'
 call readin_rjlists(clistname,cldchlistexist,cldch_rjlist,nmax,ncldchrjs)
 if(verbose)&
 print*,'cldch_rejectlist: cldchlistexist,ncldchrjs=',cldchlistexist,ncldchrjs

!==> Read in 'good' mesonet station names from the station uselist
 inquire(file='mesonet_stnuselist',exist=listexist2)
 if(listexist2) then
    open (meso_unit,file='mesonet_stnuselist',form='formatted')
    ncount=0
    do
       ncount=ncount+1
       read(meso_unit,'(a5,a80)',end=181) csta_winduse(ncount),cstring
    end do
181 continue
    nsta_mesowind_use=ncount-1
    if(verbose)&
    print*,'mesonet_stnuselist: nsta_mesowind_use=',nsta_mesowind_use
 endif
 close(meso_unit)

!==> Read in wind direction stratified wind accept lists
 inquire(file='wbinuselist',exist=wbinlistexist)
 if(verbose)&
 print*,'wdirbinuselist: wbinuselist=',wbinlistexist

 nbins=0
 if(wbinlistexist) then
    open (meso_unit,file='wbinuselist',form='formatted')
    read(meso_unit,'(a16,i2)',end=191) ch16,nbins
    allocate(nwbaccpts(max(1,nbins)))
    nwbaccpts(:)=0
    do
       read(meso_unit,'(a8,3x,a8,3x,f7.4,2x,f9.4,3x,i2)',end=191) ach8,bch8,aa1,aa2,ibin
       nwbaccpts(ibin)=nwbaccpts(ibin)+1
    end do
191 continue
    if(verbose)then
       print*,'wdirbinuselist: number of bins=',nbins
       print*,'wdirbinuselist: (nwbaccpts(ibin),ibin=1,nbins)=',(nwbaccpts(ibin),ibin=1,nbins)
    endif

    nmax2=maxval(nwbaccpts(:))
    if(verbose)&
    print*,'wdirbinuselist: maximum number of obs in a single bin=',nmax2

    allocate(csta_windbin(max(1,nmax2),max(1,nbins)))

    rewind(meso_unit)
    read(meso_unit,'(a16,i2)',end=193) ch16,nbins
    nwbaccpts(:)=0
    do
       read(meso_unit,'(a8,3x,a8,3x,f7.4,2x,f9.4,3x,i2)',end=193) ach8,bch8,aa1,aa2,ibin
       nwbaccpts(ibin)=nwbaccpts(ibin)+1
       csta_windbin(nwbaccpts(ibin),ibin)=ach8
    end do
193 continue
 endif
 close(meso_unit)

!==> Read in 'good' mesonet station names from the station uselist for visibility
 inquire(file='mesonet_stnuselist_for_vis',exist=vis_uselistexist)
 if(vis_uselistexist) then
    open (meso_unit,file='mesonet_stnuselist_for_vis',form='formatted')
    ncount=0
    do
      ncount=ncount+1
      read(meso_unit,'(a5,a80)',end=194) csta_visuse(ncount),cstring
    end do
194 continue
    nsta_mesovis_use=ncount-1
!    if(verbose)&
    print*,'mesonet_stnuselist_for_vis: nsta_mesovis_use=',nsta_mesovis_use
 endif
 close(meso_unit)

end subroutine init_rjlists

subroutine get_usagerj(kx,obstype,c_station_id,c_prvstg,c_sprvstg, &
                       dlon,dlat,idate,dtime,udbl,vdbl,usage_rj)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_usagerg
!   prgmmr:
!
! abstract: determine the usage value of read_prepbufr for surface obs. the following
!           is done: (i) if incoming usage value is >=100. then do nothing, since
!           read_prepbufr has already flagged this ob and assigned a specific usage 
!           value to it. (ii) use usage=500. for temperature, moisture, or surface pressure
!           obs which are found in the rejectlist. (iii) 
!
! program history log:
!   2009-10-01  lueken - added subprogram doc block
!
!   input argument list:
!    kx
!    obstype
!    c_station_id
!    c_prvstg,c_sprvstg
!    dlon
!    dlat
!    idate
!    udbl
!    vdbl
!
!   output argument list:
!    usage_rj
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero_single
  use gridmod, only: twodvar_regional,tll2xy
  use ndfdgrids, only: valley_adjustment

  implicit none

  integer(i_kind),intent(in   ) :: kx
  integer(i_kind),intent(in   ) :: idate
  character(10)  ,intent(in   ) :: obstype
  character(8)   ,intent(in   ) :: c_station_id
  character(8)   ,intent(in   ) :: c_prvstg,c_sprvstg
  real(r_kind)   ,intent(in   ) :: dlon
  real(r_kind)   ,intent(in   ) :: dlat
  real(r_kind)   ,intent(in   ) :: dtime
  real(r_double) ,intent(in   ) :: udbl,vdbl
  real(r_kind)   ,intent(inout) :: usage_rj

! Declare local variables
  integer(i_kind) m,nlen
  integer(i_kind) ibin
  character(8)  ch8
  real(r_kind) usage_rj0
  real(r_single) sunangle
  real(r_kind) xob,yob
  logical outside

! Declare local parameters
  real(r_kind),parameter:: r6    = 6.0_r_kind
  real(r_kind),parameter:: r5000 = 5000._r_kind
  real(r_kind),parameter:: r5100 = 5100._r_kind
  real(r_kind),parameter:: r6000 = 6000._r_kind
  real(r_kind),parameter:: r6100 = 6100._r_kind
  real(r_kind),parameter:: r6200 = 6200._r_kind

  if (usage_rj >= r6) return

  usage_rj0=usage_rj

  if (kx<200) then  !<==mass obs

    if(obstype=='t') then

      if(tlistexist ) then
        do m=1,ntrjs
           ch8(1:8)=t_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo
      endif

      if((t_day_listexist .or. t_night_listexist) .and. usage_rj==usage_rj0) then
           call get_sunangle(idate,dtime,dlon,dlat,sunangle)
           if (sunangle > zero_single) then
              do m=1,ntrjs_day
                 ch8(1:8)=t_day_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
             else 
              do m=1,ntrjs_night
                 ch8(1:8)=t_night_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
           endif 
      endif

     elseif(obstype=='q') then

      if(qlistexist ) then
        do m=1,nqrjs
           ch8(1:8)=q_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
              usage_rj=r5000
              exit
           endif
        enddo
      endif

      if((q_day_listexist .or. q_night_listexist) .and. usage_rj==usage_rj0) then
           call get_sunangle(idate,dtime,dlon,dlat,sunangle)
           if (sunangle > zero_single) then
              do m=1,nqrjs_day
                 ch8(1:8)=q_day_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
             else 
              do m=1,nqrjs_night
                 ch8(1:8)=q_night_rjlist(m)(1:8)
                 nlen=len_trim(ch8)
                 if ((trim(c_station_id) == trim(ch8)) .or. &
                     ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
                    usage_rj=r5100
                    exit
                 endif
              enddo
           endif 
      endif

     elseif(obstype=='ps' .and. plistexist ) then
        do m=1,nprjs
           ch8(1:8)=p_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
              usage_rj=r5000
              exit
           endif
        enddo

     elseif(obstype=='td2m' .and. tdlistexist ) then
        do m=1,ntdrjs
           ch8(1:8)=td_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then
              usage_rj=r5000
              exit
           endif
        enddo

     elseif(obstype=='mxtm' .and. mxtmlistexist) then
        do m=1,nmxtmrjs
           ch8(1:8)=mxtm_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     elseif(obstype=='mitm' .and. mitmlistexist) then
        do m=1,nmitmrjs
           ch8(1:8)=mitm_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     elseif(obstype=='pmsl' .and. pmsllistexist) then
        do m=1,npmslrjs
           ch8(1:8)=pmsl_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     elseif(obstype=='howv' .and. howvlistexist) then
        do m=1,nhowvrjs
           ch8(1:8)=howv_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     elseif(obstype=='lcbas' .and. lcbaslistexist) then
        do m=1,nlcbasrjs
           ch8(1:8)=lcbas_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     elseif(obstype=='tcamt' .and. tcamtlistexist) then
        do m=1,ntcamtrjs
           ch8(1:8)=tcamt_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     elseif(obstype=='cldch' .and. cldchlistexist) then
        do m=1,ncldchrjs
           ch8(1:8)=cldch_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
               ((kx==188.or.kx==195).and.c_station_id(1:nlen)==ch8(1:nlen))) then !handle wfo's mesonets which never end with
              usage_rj=r5000                                           !an "a" or "x" in the eight position following blanks
              exit
           endif
        enddo

     end if
  endif

  if (kx>=200) then !<==wind obs

     if (kx==288.or.kx==295) then
        usage_rj=r6000
        if (listexist) then !note that uselists must precede the rejectlist
           do m=1,nprov
              if (cprovider(m)(1:7)=='allprvs' .or. & 
                 (c_prvstg(1:8) == cprovider(m)(1:8) .and. (c_sprvstg(1:8) == cprovider(m)(9:16)  &
                                                      .or. cprovider(m)(9:16) == 'allsprvs')) ) then
                 usage_rj=usage_rj0
                 exit
              endif
           enddo
        endif
        if (listexist2 .and. usage_rj/=usage_rj0) then
           do m=1,nsta_mesowind_use
              if (c_station_id(1:5) == csta_winduse(m)(1:5)) then
                 usage_rj=usage_rj0
                 exit
              endif
           enddo
        endif
        if(wbinlistexist .and. usage_rj/=usage_rj0) then
          call get_wbinid(udbl,vdbl,nbins,ibin)
          if(ibin <= nbins ) then
            do m=1,nwbaccpts(ibin)
              ch8(1:8)=csta_windbin(m,ibin)(1:8)
              nlen=len_trim(ch8)
              if (c_station_id(1:nlen)==ch8(1:nlen)) then
                usage_rj=usage_rj0
                exit
              endif
            enddo
          endif
        endif
     endif

     if( (obstype=='uv' .or. obstype=='wspd10m' .or. obstype=='uwnd10m' .or.  obstype=='vwnd10m') .and. wlistexist ) then
        do m=1,nwrjs
           ch8(1:8)=w_rjlist(m)(1:8)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)) .or. &
              ((kx==288.or.kx==295).and. c_station_id(1:nlen)==ch8(1:nlen))) then
              if (kx/=288.and.kx/=295) then
                 usage_rj=r5000
              else
                 if (usage_rj==usage_rj0)    usage_rj=r6100 !ob is in at least one of the above three uselists
                 if (usage_rj==r6000)        usage_rj=r6200 !ob is in none of the above three uselists
              endif
              exit
           endif
        enddo
     endif

  end if

!==> station uselist for mesonet visibility

  if (obstype=='vis' .and.( kx==188.or.kx==288.or.kx==195.or.kx==295) )then
     usage_rj=r6000
     if (vis_uselistexist .and. usage_rj/=usage_rj0) then !note that usage_rj could differ from usage_rj0 after the rejectlist application
        do m=1,nsta_mesovis_use                          !which happens to be currently unavailable for vis
           nlen=len_trim(csta_visuse(m))
           if (c_station_id(1:nlen) == csta_visuse(m)(1:nlen)) then
              usage_rj=usage_rj0
              exit
           endif
        enddo
     endif
  end if

  if (twodvar_regional) then
     call tll2xy(dlon,dlat,xob,yob,outside)
     if ((obstype=='t' .or. obstype=='q') .and. .not.outside) call valley_adjustment(xob,yob,usage_rj)
  endif
end subroutine get_usagerj


subroutine get_gustqm(kx,c_station_id,c_prvstg,c_sprvstg,gustqm)
!$$$  module documentation block
! abstract: determine the qm for gust 1) for gust that is not on the uselist qm=9
!                                     2) for gust that is on the rejectlist qm=3 or 15
! program history log:
!   2009-01-29  zhu
!
  implicit none

  integer(i_kind) kx
  integer(i_kind) nlen
  integer(i_kind) gustqm
  character(8),intent(in)::  c_station_id
  character(8),intent(in)::  c_prvstg,c_sprvstg

! Declare local variables
  integer(i_kind) m
  character(8)  ch8

  gustqm=9
  if (listexist) then
     do m=1,nprov
        if (cprovider(m)(1:7)=='allprvs' .or. &
           (c_prvstg(1:8) == cprovider(m)(1:8) .and. (c_sprvstg(1:8) == cprovider(m)(9:16)  &
                                               .or. cprovider(m)(9:16) == 'allsprvs')) ) then
           gustqm=0
           exit
         endif
     enddo
  endif
  if (listexist2) then
     do m=1,nsta_mesowind_use
        if (c_station_id(1:5) == csta_winduse(m)(1:5)) then
           gustqm=0
           exit
         endif
     enddo
  endif

  if(wlistexist ) then
     do m=1,nwrjs
        ch8(1:8)=w_rjlist(m)(1:8)
        nlen=len_trim(ch8)
        if ((trim(c_station_id) == trim(ch8)) .or. &
            ((kx==288.or.kx==295).and.c_station_id(1:nlen)==ch8(1:nlen))) then
            if (gustqm==0) then
               gustqm=3
            else
               gustqm=15
            end if
            exit
        endif
     enddo
  endif
end subroutine get_gustqm


subroutine readin_rjlists(clistname,fexist,clist,ndim,ncount)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    readin_rjlists
!   prgmmr:
!
! abstract: read in accept or reject list
!
! program history log:
!   2012-17-02  pondeca
!
!   input argument list:
!   clistname
!   ndim
!
!   output argument list:
!   clist
!   ncount
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

! Declare passed variables
  integer(i_kind),intent(in):: ndim
  character(80),intent(in):: clistname

  integer(i_kind),intent(out):: ncount
  character(*),intent(out):: clist(ndim)
  logical,intent(out)::fexist

! Declare local variables
  integer(i_kind) meso_unit,n,m
  character(80) cstring

  data meso_unit / 20 /
!**************************************************************************
 ncount=0

 inquire(file=trim(clistname),exist=fexist)
 if(fexist) then
    open (meso_unit,file=trim(clistname),form='formatted')
    do m=1,3
       read(meso_unit,*,end=131) cstring
    enddo
    n=0
    do 
       n=n+1
       read(meso_unit,*,end=131) clist(n)
    end do
131 continue
    ncount=n-1

    close(meso_unit)
 endif
end subroutine readin_rjlists


subroutine destroy_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_rjlists
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

  deallocate(cprovider)
  deallocate(w_rjlist)
  deallocate(t_rjlist)
  deallocate(p_rjlist)
  deallocate(q_rjlist)
  deallocate(csta_winduse)
  deallocate(t_day_rjlist)
  deallocate(t_night_rjlist)
  deallocate(q_day_rjlist)
  deallocate(q_night_rjlist)
  deallocate(td_rjlist)
  deallocate(mxtm_rjlist)
  deallocate(mitm_rjlist)
  deallocate(pmsl_rjlist)
  deallocate(howv_rjlist)
  deallocate(lcbas_rjlist)
  deallocate(tcamt_rjlist)
  deallocate(cldch_rjlist)
  if(wbinlistexist) deallocate(nwbaccpts)
  if(wbinlistexist) deallocate(csta_windbin)
  deallocate(csta_visuse)
end subroutine destroy_rjlists


subroutine get_sunangle(idate,dtime,dlon,dlat,sunangle) 

  use constants, only: deg2rad
  implicit none

  integer(i_kind),intent(in)::idate
  real(r_kind),intent(in)::dtime,dlon,dlat
  real(r_single),intent(out)::sunangle

  real(r_single),parameter:: s24 = 24._r_single
  real(r_single),parameter:: s180 = 180._r_single
  real(r_single),parameter:: s360 = 360._r_single

  integer(i_kind) iyear,imonth,iday,ihour
  integer(i_kind) iw3jdn
  integer(i_kind) jday2,iyear3,imonth3,iday3,idaywk3,idayyr3
  integer(i_kind) ndays
  real(r_single) rlat,rlon
  real(r_single) time
  character(10) date

  write(date,'( i10)') idate
  read (date,'(i4,3i2)') iyear,imonth,iday,ihour
  jday2=iw3jdn(iyear,imonth,iday)

  time=real(ihour,kind=r_single)+real(dtime,kind=r_single)

  if (time.gt.s24) then
     time = time-s24
     jday2=jday2+1
   elseif (time.lt.0) then
     time=time+s24
     jday2=jday2-1
  endif

  call w3fs26(jday2,iyear3,imonth3,iday3,idaywk3,idayyr3)
  !account for leap year
  if(mod(iyear3,4).eq.0) then
     ndays=366
  else
     ndays=365
  endif

  rlon=real(dlon/deg2rad,kind=r_single) ; if (rlon > s180) rlon=rlon-s360
  rlat=real(dlat/deg2rad,kind=r_single)

  call calcsun(idayyr3,ndays,time,rlat,rlon,sunangle)

end subroutine get_sunangle

subroutine get_wbinid(udbl,vdbl,nbins,ibin)

  use constants, only: zero, tiny_r_kind

  implicit none

  integer(i_kind),intent(in   ):: nbins
  real(r_double) ,intent(in   ):: udbl,vdbl
  integer(i_kind),intent(out  ):: ibin

! Declare local variables
  integer(i_kind) n
  real(r_kind) binwidth
  real(r_kind) ue,ve,wdir

  real(r_kind),parameter::r360=360._r_kind

  ue=real(udbl,kind=r_kind)
  ve=real(vdbl,kind=r_kind)

  binwidth=r360/real(max(1,nbins),kind=r_kind)

  call getwdir(ue,ve,wdir)

  if (abs(wdir)<=tiny_r_kind .or. abs(wdir-r360)<=tiny_r_kind) then  
     if (abs(ue)<=tiny_r_kind .and. abs(ve)<=tiny_r_kind) then
        ibin=nbins+1 !don't use if wind ob is calm
     else
        ibin=nbins
     endif
   else
     do n=1,nbins
        if ( wdir >= real(n-1,r_kind)*binwidth .and. wdir < real(n,r_kind)*binwidth ) then 
            ibin=n
            exit
         endif
      enddo
   endif

end subroutine get_wbinid

end module sfcobsqc

subroutine calcsun (idayr,idaysy,baltm,xlon,ylat,solar)

  implicit none

  integer(4),intent(in)::idayr,idaysy
  real(4),intent(in)::baltm,xlon,ylat
  real(4),intent(out)::solar
  real(4)::dangl,sdgl,cdgl,tsnoon,afy,safy,cafy,ssod,alon,sha,rlat

  dangl = 6.2831853 * (real(idayr) - 79.)/real(idaysy)
  sdgl = sin(dangl)
  cdgl = cos(dangl)
  !solar noon in utc
  tsnoon = -.030*sdgl-.120*cdgl+.330*sdgl*cdgl+.0016*sdgl**2-.0008
  !afy = angular fraction of year
  afy=6.2831853*(real(idayr)-1. + (baltm/24.))/real(idaysy)
  safy=sin(afy)
  cafy=cos(afy)
  !ssod = sin of dolar declination
  ssod = 0.3978492*sin(4.88578+afy+(.033420*safy)-(0.001388*cafy)+(.000696*safy*cafy)+(.000028*(safy**2-cafy**2)))
  alon=mod(720.+360.-xlon,360.)
  !sha = solar hour angle
  sha=0.0174532925*((15.*(tsnoon+baltm+36.))-alon)
  rlat=0.0174532925*ylat
  solar=57.29578*asin((ssod*sin(rlat))+sqrt(1.0-ssod**2)*cos(rlat)*cos(sha))
  return

end subroutine calcsun
