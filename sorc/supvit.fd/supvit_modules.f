      module def_vitals
        type tcvcard         ! Define a new type for a TC Vitals card
          character*4   tcv_center      ! Hurricane Center Acronym
          character*3   tcv_storm_id    ! Storm Identifier (03L, etc)
          character*9   tcv_storm_name  ! Storm name
          integer       tcv_century     ! 2-digit century id (19 or 20)
          integer       tcv_yymmdd      ! Date of observation
          integer       tcv_hhmm        ! Time of observation (UTC)
          integer       tcv_lat         ! Storm Lat (*10), always >0
          character*1   tcv_latns       ! 'N' or 'S'
          integer       tcv_lon         ! Storm Lon (*10), always >0
          character*1   tcv_lonew       ! 'E' or 'W'
          integer       tcv_stdir       ! Storm motion vector (in degr)
          integer       tcv_stspd       ! Spd of storm movement (m/s*10)
          character*85  tcv_chunk       ! Remainder of vitals record;
                                        ! will just be read & written
        end type tcvcard
      end module def_vitals
c
      module inparms
        type datecard  ! Define a new type for the input namelist parms
          sequence
          integer       yy    ! Beginning yy of date to search for
          integer       mm    ! Beginning mm of date to search for
          integer       dd    ! Beginning dd of date to search for
          integer       hh    ! Beginning hh of date to search for
        end type datecard
      end module inparms
c
      module date_checks
        integer, save  :: ymd_now,hhmm_now,ymd_old,hhmm_old
     &                   ,ymd_future,hhmm_future
      end module date_checks
c
      module set_max_parms
        integer, parameter :: maxstorm=400  ! max # of storms pgm can
                                            ! handle
      end module set_max_parms
c
      module trig_vals
        real, save :: pi, dtr, rtd
        real, save :: dtk = 111194.9     ! Dist (m) over 1 deg lat
                                         ! using erad=6371.0e+3
        real, save :: erad = 6371.0e+3   ! Earth's radius (m)
        real, save :: ecircum = 40030200  ! Earth's circumference
                                          ! (m) using erad=6371.e3
        real, save :: omega = 7.292e-5
        real, save :: secphr = 3600.
      end module trig_vals
c
c------------------------------------------------------
c
