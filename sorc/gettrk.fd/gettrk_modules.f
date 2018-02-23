      module def_vitals
        type tcvcard         ! Define a new type for a TC Vitals card
          character*4   tcv_center      ! Hurricane Center Acronym
          character*4   tcv_storm_id    ! Storm Identifier (03L, etc)
          character*9   tcv_storm_name  ! Storm name
          integer       tcv_ymd         ! Date of observation (yyyymmdd)
          integer       tcv_hhmm        ! Time of observation (UTC)
          integer       tcv_lat         ! Storm Lat (*10), always >0
          character*1   tcv_latns       ! 'N' or 'S'
          integer       tcv_lon         ! Storm Lon (*10), always >0
          character*1   tcv_lonew       ! 'E' or 'W'
          integer       tcv_stdir       ! Storm motion vector (in degr)
          integer       tcv_stspd       ! Spd of storm movement (m/s*10)
          integer       tcv_pcen        ! Min central pressure (mb)
          integer       tcv_penv        ! val outrmost closed isobar(mb)
          integer       tcv_penvrad     ! rad outrmost closed isobar(km)
          integer       tcv_vmax        ! max sfc wind speed (m/s)
          integer       tcv_vmaxrad     ! rad of max sfc wind spd (km)
          integer       tcv_r15ne       ! NE rad of 15 m/s winds (km)
          integer       tcv_r15se       ! SE rad of 15 m/s winds (km)
          integer       tcv_r15sw       ! SW rad of 15 m/s winds (km)
          integer       tcv_r15nw       ! NW rad of 15 m/s winds (km)
          character*1   tcv_depth       ! Storm depth (S,M,D) X=missing
        end type tcvcard
        type (tcvcard), save, allocatable :: storm(:)
        integer, save, allocatable :: stormswitch(:)
        real, save, allocatable    :: slonfg(:,:),slatfg(:,:)
        character*3, save, allocatable :: stcvtype(:) ! FOF or TCV
      end module def_vitals
c
      module gen_vitals
        type gencard     ! Define a new type for a genesis vitals card
          integer       gv_gen_date    ! genesis date in yyyymmddhh
          integer       gv_gen_fhr     ! genesis fcst hour (usually 0)
          integer       gv_gen_lat     ! genesis lat (*10), always >0
          character*1   gv_gen_latns   ! 'N' or 'S'
          integer       gv_gen_lon     ! genesis lon (*10), always >0
          character*1   gv_gen_lonew   ! 'W' or 'E'
          character*3   gv_gen_type    ! 'FOF'; or TC Vitals ATCF ID
          integer       gv_obs_ymd     ! Date of observation (yyyymmdd)
          integer       gv_obs_hhmm    ! Time of observation (UTC)
          integer       gv_obs_lat     ! Storm Lat (*10), always >0
          character*1   gv_obs_latns   ! 'N' or 'S'
          integer       gv_obs_lon     ! Storm Lon (*10), always >0
          character*1   gv_obs_lonew   ! 'E' or 'W'
          integer       gv_stdir       ! Storm motion vector (in degr)
          integer       gv_stspd       ! Spd of storm movement (m/s*10)
          integer       gv_pcen        ! Min central pressure (mb)
          integer       gv_penv        ! val outrmost closed isobar(mb)
          integer       gv_penvrad     ! rad outrmost closed isobar(km)
          integer       gv_vmax        ! max sfc wind speed (m/s)
          integer       gv_vmaxrad     ! rad of max sfc wind spd (km)
          integer       gv_r15ne       ! NE rad of 15 m/s winds (km)
          integer       gv_r15se       ! SE rad of 15 m/s winds (km)
          integer       gv_r15sw       ! SW rad of 15 m/s winds (km)
          integer       gv_r15nw       ! NW rad of 15 m/s winds (km)
          character*1   gv_depth       ! Storm depth (S,M,D) X=missing
        end type gencard
        type (gencard), save, allocatable :: gstorm(:)
      end module gen_vitals
c
      module inparms
        type datecard  ! Define a new type for the input namelist parms
          integer       bcc    ! First 2 chars of yy of date (century)
          integer       byy    ! Beginning yy of date to search for 
          integer       bmm    ! Beginning mm of date to search for 
          integer       bdd    ! Beginning dd of date to search for 
          integer       bhh    ! Beginning hh of date to search for 
          integer       model  ! integer identifier for model data used
          character*8   modtyp ! 'global' or 'regional'
          character*7   lt_units ! 'hours' or 'minutes' to indicate the
                                 ! units of lead times in grib files
          character*6   file_seq ! 'onebig' or 'multi' tells if grib
                                 ! data will be input as one big file or
                                 ! as individual files for each tau.
          character*8   nesttyp ! Either "moveable" or "fixed"
        end type datecard
      end module inparms
c
      module trkrparms
        type trackstuff  ! Define a new type for various tracker parms
          real          westbd  ! Western boundary of search area
          real          eastbd  ! Eastern boundary of search area
          real          northbd ! Northern boundary of search area
          real          southbd ! Southern boundary of search area
          character*7   type    ! 'tracker', 'midlat' or 'tcgen'
          real          mslpthresh ! min mslp gradient to be maintained
          real          v850thresh ! min avg 850 Vt to be maintained
          character*8   gridtype ! 'global' or 'regional'
          real          contint  ! Contour interval to be used for 
                                 ! "midlat" or "tcgen" cases.     
          logical       want_oci ! Flag for whether to compute & write
                                 ! out roci for a trkrtype=tracker run
          character*1   out_vit  ! Flag for whether to write out vitals
          integer       gribver  ! Indicates whether input data is 
                                 ! GRIBv1 (1) or GRIBv2 (2)
          integer       g2_jpdtn ! Indicates GRIB2 template to use when
                                 ! reading (0 = deterministic fcst, 
                                 ! 1 = ens fcst)
        end type trackstuff
      end module trkrparms
c
      module contours
        integer, parameter :: maxconts=100 ! max # of cont. intervals
        type cint_stuff  ! Define a new type for contour interval info
          real    :: xmaxcont ! max contour level in a field
          real    :: xmincont ! min contour level in a field
          real    :: contvals(maxconts) ! contour values in the field
          integer :: numcont  ! # of contour levels in a field
        end type cint_stuff
      end module contours
c     
      module atcf
        integer         atcfnum   ! ATCF ID of model (63 for GFDL...)
        character*4     atcfname  ! ATCF Name of model (GFSO for GFS...)
        integer         atcfymdh  ! YMDH to be used as initial date for
                                  ! output ATCF forecast records.  Will
                                  ! be equal to initial run date for all
                                  ! models except SREF; SREF will have a
                                  ! start date artificially modified to
                                  ! be 3 hours earlier in order to not
                                  ! cause the NHC interpolator to burp.
        integer         atcffreq  ! frequency (in centahours) of output
                                  ! for atcfunix and certain other
                                  ! files.  Default: 600 (six-hourly)
      end module atcf
c
      module gfilename_info
        character*4 ,save :: gmodname  ! Model ID for first part of GRIB
                                  ! file name ("gfdl","hwrf","hrs", etc)
        character*40 ,save :: rundescr ! This is descriptive and up to
                                  ! the developer (e.g., "6thdeg",
                                  ! "9km_run","1.6km_run"         
                                  ! "15km_ens_run_member_n13", etc)
        character*40 ,save :: atcfdescr ! This is optional.  If used, it
                                  ! should be something that identifies
                                  ! the particular storm, preferably
                                  ! using the atcf ID.  For example, the
                                  ! GFDL model standard is to use
                                  ! something like "ike09l", or  
                                  ! "two02e", etc.             
      end module gfilename_info
c
      module phase
        character*1 , save ::   phaseflag   ! Will phase be determined
                                            ! (y/n)                   
        character*4 , save ::   phasescheme ! What scheme to use:
                                    ! cps  = Hart's cyclone phase space
                                    ! vtt  = Vitart                    
                                    ! both = Both cps and vtt are used
        real, save  :: wcore_depth  ! The contour interval (in deg K)
                             ! used in determining if a closed contour 
                             ! exists in the 300-500 mb T data, for
                             ! use with the vtt scheme.
      end module phase
c
      module structure
        character*1 , save ::   structflag  ! Will structure be
                                            ! analyzed (y/n)?
        character*1 , save ::   ikeflag     ! Will IKE & SDP be
                                            ! computed (y/n)?
      end module structure
c     
      module tracked_parms
          real, save, allocatable  ::  zeta(:,:,:)
          real, save, allocatable  ::  u(:,:,:)
          real, save, allocatable  ::  v(:,:,:)
          real, save, allocatable  ::  hgt(:,:,:)
          real, save, allocatable  ::  slp(:,:)
          real, save, allocatable  ::  tmean(:,:)
          real, save, allocatable  ::  cpshgt(:,:,:)
          integer, save, allocatable :: ifhours(:)  
          integer, save, allocatable :: iftotalmins(:)
          integer, save, allocatable :: ifclockmins(:)
          integer, save, allocatable :: ltix(:)
          real, save, allocatable    :: fhreal(:)
      end module tracked_parms
c     
      module radii
c       For Barnes smoothing of parameters for tracking, e-folding
c       radius = retrk (km), influence radius = ritrk (km). Max radius
c       for searching for the max vorticity = rads (km).  There is an
c       important distinction between rads and ritrk.  rads is used to
c       determine the maximum distance from the guess position that
c       we'll allow points to be in order to be considered as a
c       candidate location for the updated fix position.  On the other
c       hand, ritrk is used once you're doing the barnes analysis on
c       data for that candidate location, so that if data is not within
c       distance ritrk of the candidate location, we ignore it.  Also, 
c       for use in find_maxmin, nhalf = the number of times to halve the
c       spacing of the search grid.  Note that different values are used
c       for the magnitude of the wind than for other parameters; this is
c       so that the search area is restricted in order to avoid a 
c       problem of the program finding one wind minimum near the center
c       and another one out near the storm's edge.
c
c       redlm and ridlm = the e-folding radius (km) and influence radius
c       (km) for Barnes analysis of u,v for updating first guess lat,lon
c       for search.  dlm = deep layer mean.
c
c        real, parameter :: retrk_most=60.0, retrk_vmag=60.0
c        real, parameter :: ritrk_most=120.0, ritrk_vmag=120.0
c        real, parameter :: rads_most=120.0, rads_vmag=120.0
c        real, parameter :: retrk_most=150.0, retrk_vmag=60.0
c        real, parameter :: ritrk_most=300.0, ritrk_vmag=120.0
c        real, parameter :: rads_most=300.0, rads_vmag=120.0
        real, parameter :: retrk_most=75.0, retrk_vmag=60.0
        real, parameter :: ritrk_most=150.0, ritrk_vmag=120.0
        real, parameter :: rads_most=300.0, rads_vmag=120.0
        real, parameter :: retrk_coarse=150.0, retrk_hres=60.0
        real, parameter :: rads_fine=200.0, rads_hres=150.0
        real, parameter :: ritrk_coarse=300.0, rads_coarse=350.0
        real, parameter :: redlm=500.0, ridlm=2000.0
c        real, parameter :: redlm=500.0, ridlm=1700.0
ctpm6/14        real, parameter :: redlm=500.0, ridlm=1000.0
      end module radii
c
      module grid_bounds
        real, save ::  glatmin, glatmax, glonmin, glonmax  ! These 
                       ! define the boundaries of the input data grid
        real, save, allocatable ::  glat(:), glon(:)  ! Will be filled
                       ! with lat/lon values for each pt on input grid
      end module grid_bounds
c
      module error_parms
        real, parameter :: err_gfs_init=275.0 ! init errmax for gfs,mrf
                                              ! and gdas
        real, parameter :: err_reg_init=300.0 ! init errmax for others
        real, parameter :: err_ecm_max=330.0  ! errmax for ecmwf
        real, parameter :: err_reg_max=225.0  ! errmax for others for
                                              ! remaining fcst times
        real, parameter :: maxspeed_tc=60  ! max speed of storm movement
                                           ! for tracker or tcgen cases
        real, parameter :: maxspeed_ml=80  ! max speed of storm movement
                                           ! for midlat cases
C        real, parameter :: errpgro=1.25, errpmax=600.0
        real, parameter :: errpgro=1.25, errpmax=485.0
        real, parameter :: stermn = 0.1   ! Min Std dev for trk errors
        real, parameter :: uverrmax = 225.0  ! For use in get_uv_guess
      end module error_parms
c
      module set_max_parms
        integer, parameter :: maxstorm_tc=15  ! max # of storms pgm can
                                           ! handle, for tc tracker case
        integer, parameter :: maxstorm_mg=2000  ! max # of storms pgm can
                                      ! handle, for midlat or tcgen case
        integer, parameter :: maxtime=500  ! max # of fcst times pgm 
        integer, parameter :: maxtp=11     ! max # of tracked parms 
        integer, parameter :: maxmodel=20  ! max # of models currently 
                                           ! available
        integer, parameter :: max_ike_cats=6 ! max # of IKE categories
        integer, save :: interval_fhr      ! # of hrs between fcst times
        integer, parameter :: maxcenters=150 ! max # of max/min centers
                                             ! to be tracked.
      end module set_max_parms
c
      module level_parms
        integer, parameter :: nlevs=4   ! max # of vert levs to be read
        integer, parameter :: nlevg=3   ! # of vert levs to be used for
                                        ! figuring next guess position
        integer, parameter :: nlevm1=2  ! # tracked levs for hgt
        integer, parameter :: nlevzeta=3 ! # tracked levs for zeta
        integer, parameter :: levsfc=4  ! array position of sfc winds.
        integer, parameter :: nlev850=1 ! array position in u and v
        integer, parameter :: nlev700=2 ! arrays for 850, 700 & 500
        integer, parameter :: nlev500=3 ! winds.  Used in get_uv_center
        integer, parameter :: nlevs_cps=13 ! # levs for Hart's CPS
        real, save      :: wgts(nlevg)  ! Wghts for use in get_next_ges
        data wgts /0.25, 0.50, 0.25/    ! 850, 700 & 500 mb wgts
      end module level_parms
c
      module trig_vals
        real, save :: pi, dtr
        real, save :: dtk = 111.1949     ! Dist (km) over 1 deg lat
                                         ! using erad=6371.0e+3
        real, save :: erad = 6371.0e+3   ! Earth's radius (m)
        real, save :: ecircum = 40030.2  ! Earth's circumference
                                         ! (km) using erad=6371.e3
        real, save :: omega = 7.292e-5
      end module trig_vals
c
      module verbose_output
        integer, save :: verb            ! Level of detail printed to terminal
                                         ! 0 = No output
                                         ! 1 = Error messages only
                                         ! 2 = 
                                         ! 3 = All      
      end module verbose_output
c
      module waitfor_parms
        character*1 :: use_waitfor ! y or n, for waiting for input files
        integer :: wait_min_age    ! min age (in seconds)... time since
                                   ! last file modification
        integer :: wait_min_size   ! minimum file size in bytes
        integer :: wait_max_wait   ! max total wait time in seconds 
        integer :: wait_sleeptime  ! number of seconds to wait between 
                                   ! checks
        integer, parameter :: pfc_cmd_len = 800
        character*1 :: use_per_fcst_command ! enable per_fcst_command
        character(pfc_cmd_len) :: per_fcst_command ! command to run every forecast time
      end module waitfor_parms
c---------------------------------------------------------------------c
c
