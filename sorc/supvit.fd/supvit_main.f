      program sort_and_update_vitals
c
c$$$  MAIN PROGRAM DOCUMENTATION BLOCK
c
c Main Program: SUPVIT       Sort and Update Vitals File
C   PRGMMR: MARCHOK          ORG: NP22        DATE: 1999-04-14
c
c ABSTRACT: This program searches through the TC Vitals file and reads 
c   the records for a particular dtg.  It contains logic to eliminate
c   duplicate records and only keep the most recent one (see further
c   documentation below).  It also searches to see if a storm was
c   included in the Vitals file 6 hours earlier (or 3 hours earlier 
c   if we're tracking with the off-synoptic-time SREF) but is missing 
c   from the current Vitals records.  In this case, the program assumes
c   that the regional forecasting center was late in reporting the
c   current position, and it includes the old Vitals record with
c   the current Vitals records.  This program will also take the
c   position and heading from that old vitals record and extrapolate the
c   information to get a current first guess estimate of the storm's
c   position.  By the way, if a storm was found 3 or 6 hours earlier, 
c   logic is also included to eliminate any duplicate records of that 
c   storm in those old records.  Finally, if it turns out that the 
c   reason an old vitals is no longer on the current records is that 
c   the storm has dissipated, don't worry about including it to be 
c   passed into the tracking program; the tracking program will not be 
c   able to track it and that'll be the end of it.
c
c Program history log:
c   98-03-26  Marchok - Original operational version.
c   99-04-01  Marchok - Modified code to be able to read the year off
c                       of the TC Vitals card as a 4-digit integer, 
c                       instead of as a 2-digit integer.
c   00-06-13  Marchok - Modified code to be able to read vitals from 6h
c                       ahead (this is for use in the GDAS tropical 
c                       cyclone relocation system).
c   04-05-27  Marchok - Modified code to be able to read vitals from 3h
c                       ago.  This is for tracking with the 09z and 21z
c                       SREF ensemble.  Since there are no vitals at
c                       these offtimes, we need to update vitals from
c                       the synoptic times 3h earlier.
c
c Input files:
c   unit   31    Text file containing all vitals (including duplicates)
c                for current time and time from 3 or 6 hours ago and 
c                3 or 6 hours ahead.
c Output files:
c   unit   51    Text file containing sorted, updated vitals (without
c                any duplicates) valid at the current time only.
c
c Subprograms called:
c   read_nlists  Read input namelists for input dates
c   read_tcv_file Read TC vitals file to get initial storm positions
c   delete_dups  Delete duplicate TC vitals records from current time
c   delete_old  Delete records from 6h ago if current record exists
c   delete_old_dups  Delete duplicate records from 6h ago time
c   update_old_vits  Update position of storms from 6h ago positions
c   output       Output 1 record for each updated vitals record
c
c Attributes:
c   Language: Fortran_90
c
c$$$
c
c-------
c 
c
      USE def_vitals; USE set_max_parms; USE inparms; USE date_checks
      USE trig_vals
c
      type (tcvcard) storm(maxstorm)
      type (datecard) dnow, dold, dfuture

      logical  okstorm(maxstorm)
      integer  vit_hr_incr
c
      call w3tagb('SUPVIT  ',1999,0104,0058,'NP22   ')
c
      okstorm = .FALSE.
c
      pi = 4. * atan(1.)   ! pi, dtr and rtd were declared in module
      dtr = pi/180.0       ! trig_vals, but were not yet defined.
      rtd = 180.0/pi 
c
c     -----------------------------------------
c     Read namelists to get date information
c
      call read_nlists (dnow,dold,dfuture,vit_hr_incr)
c
c     -----------------------------------------------------------
c     Read in storm cards for current time and delete duplicates
c

      inowct = 0
      call read_tcv_file (storm,ymd_now,hhmm_now,inowct,okstorm)
 
      if (inowct > 0) then
        call delete_dups (storm,inowct,okstorm)
      else
        print *,' '
        print *,'!!! No storms on tcv card for current time.'
        print *,'!!! A check will be made for old tcv storm cards,'
        print *,'!!! and if any exist, the positions will be updated'
        print *,'!!! (extrapolated) to get a first guess position for'
        print *,'!!! the current time.'
        print *,'!!! Current forecast time = ',ymd_now,hhmm_now
        print *,'!!! Old forecast time = ',ymd_old,hhmm_old
      endif
c
c     -----------------------------------------------------------
c     Read in storm cards for 3h or 6h ago and delete duplicates
c
      rewind (31)
      itempct = inowct
      call read_tcv_file (storm,ymd_old,hhmm_old,itempct,okstorm)
      ioldct = itempct - inowct
 
      if (ioldct > 0) then
        if (inowct > 0) then
          call delete_old (storm,inowct,ioldct,okstorm)
        endif
        call delete_old_dups (storm,inowct,ioldct,okstorm)
      endif
 
c     ----------------------------------------------------------------
c     Now update any vitals records left from 3h or 6h ago by 
c     extrapolating their positions ahead to the current time.
 
      if (ioldct > 0) then
        call update_old_vits (storm,inowct,ioldct,okstorm,vit_hr_incr)
      endif


c     --------------------------------------------------------------
c     Read in storm cards for 3h or 6h ahead and delete duplicates.  
c     This is used for Qingfu's vortex relocation purposes.  If he is
c     doing the analysis/relocation for, say, 12z, he looks at the
c     first guess files from the 06z cycle and tracks from there.
c     But suppose there is a storm whose first tcvitals card is 
c     issued at 12z; then we would have no tcvitals card at 06z for
c     the tracker to use.  So this next part reads the vitals from
c     the cycle 6h ahead and, if it finds any vitals that were not
c     included with the current time's vitals, then it extrapolates
c     those vitals from the next cycle *backwards* to the current
c     time.  By the way, itempct is input/output for the  read 
c     routine.  Going in, it contains the count of the number of 
c     records read in so far.  In that read routine, itempct is 
c     incremented for every valid record read for the input time.

      rewind (31)
      iprevct = inowct + ioldct
      call read_tcv_file (storm,ymd_future,hhmm_future,itempct,okstorm)
      ifuturect = itempct - iprevct

      print *,'before d6a if, ifuturect = ',ifuturect,' iprevct= '
     &       ,iprevct
      print *,'before d6a if, inowct = ',inowct,' ioldct= ',ioldct

      if (ifuturect > 0) then
        if (iprevct > 0) then
          call delete_future (storm,iprevct,ifuturect,okstorm)
        endif
        call delete_future_dups (storm,iprevct,ifuturect,okstorm)
      endif

c     ----------------------------------------------------------------
c     Now update any vitals records not filtered out from 3h or 6h
c     ahead by extrapolating their future positions *backwards* to 
c     the current time.
 
      if (ifuturect > 0) then
        call update_future_vits (storm,iprevct,ifuturect,okstorm
     &                          ,vit_hr_incr)
      endif

 
c     ---------------------------------------------------------
c     Now output all of the sorted, updated TC Vitals records
 
      itotalct = inowct + ioldct + ifuturect
      call output (storm,itotalct,okstorm)
c
      call w3tage('SUPVIT  ')
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_tcv_file (storm,ymd,hhmm,ict,okstorm)
c
c     ABSTRACT: This routine  reads in the TC Vitals file, and stores
c     into an array those records that match the input ymd and hhmm.
c
c     INPUT:
c
c     ict     Tells at what index in the storm array to begin reading
c             the input records into.  This is important because this
c             subroutine is called twice; the first time the data are
c             for the current time and are just started at ict = 0,
c             but the second time it's called we're getting the 6h ago
c             data, and they have to be added onto the end of the
c             array, so we need to know where the current time's data
c             ends so we know what index to start the 6h ago data.
c
      USE def_vitals; USE set_max_parms
c
      type (tcvcard) storm(maxstorm), ts
c
      integer  ymd,hhmm
      logical  okstorm(maxstorm)
c
      lucard = 31

      print *,' '
      print '(a26,i6.6,a8,i4.4)',' IN READ_TCV_FILE: , ymd= ',ymd
     &      ,'  hhmm= ',hhmm
      print *,' '


      do while (.true.)
        read (lucard,21,END=801,ERR=891) ts
        if (ts%tcv_yymmdd == ymd .and. ts%tcv_hhmm == hhmm) then
          ict = ict + 1
          storm(ict) = ts
          okstorm(ict) = .TRUE.
          write (6,23) ' !!! MATCH, ict= ',ict,storm(ict)
        endif
      enddo
  801 continue

   21 format (a4,1x,a3,1x,a9,1x,i2,i6,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x
     &       ,i3,a85)
   23 format (a18,i3,2x,a4,1x,a3,1x,a9,1x,i2,i6.6,1x,i4.4,1x,i3,a1,1x,i4
     &       ,a1,1x,i3,1x,i3,a85)

      iret = 0
      return

  891 print *,'!!! ERROR in program sort_and_update_vitals.  Error '
      print *,'!!! occurred in read_tcv_file while reading unit ',lucard
      iret = 98

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine output (storm,itotalct,okstorm)
c
      USE def_vitals; USE set_max_parms; USE inparms
c
      type (tcvcard) storm(maxstorm)
      type (datecard) dnow, dold, dfuture

      logical  okstorm(maxstorm)
c
      lunvit = 51

      ist = 1
      do while (ist <= itotalct)

        if (okstorm(ist)) then
          if (storm(ist)%tcv_stdir == -99 .or.
     &        storm(ist)%tcv_stspd == -99) then
            write (lunvit,23,ERR=891) storm(ist)
          else
            write (lunvit,21,ERR=891) storm(ist)
          endif
        endif

        ist = ist + 1

      enddo

   21 format (a4,1x,a3,1x,a9,1x,i2.2,i6.6,1x,i4.4,1x,i3.3,a1,1x,i4.4
     &       ,a1,1x,i3.3,1x,i3.3,a85)
   23 format (a4,1x,a3,1x,a9,1x,i2.2,i6.6,1x,i4.4,1x,i3.3,a1,1x,i4.4
     &       ,a1,1x,i3,1x,i3,a85)

      iret = 0
      return

  891 print *,'!!! ERROR in program sort_and_update_vitals.  Error '
      print *,'!!! occurred in output while writing new vitals file '
      print *,'!!! to unit number',lunvit
      iret = 98

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine update_old_vits (storm,inowct,ioldct,okstorm
     &                           ,vit_hr_incr)
c
c     ABSTRACT: This subroutine updates the vitals from 3h or 6h ago.
c     It uses the heading and direction values listed in the vitals
c     record (see Module def_vitals for specfics on where to find 
c     heading & direction in the vitals record) to get a new
c     position for the current time by extrapolating out 3h or 6h.
c
      USE def_vitals; USE set_max_parms; USE inparms; USE date_checks
      USE trig_vals
c
      type (tcvcard) storm(maxstorm)
      type (datecard) dnow, dold

      logical  okstorm(maxstorm)
      integer  vit_hr_incr
c
      ist  = inowct + 1
      iend = inowct + ioldct
      do while (ist <= iend)

        if (okstorm(ist) .and. storm(ist)%tcv_yymmdd == ymd_old .and.
     &                         storm(ist)%tcv_hhmm == hhmm_old) then

          rlat = float(storm(ist)%tcv_lat) / 10.
          rlon = float(storm(ist)%tcv_lon) / 10.
          rhdg = float(storm(ist)%tcv_stdir)
          rspd = float(storm(ist)%tcv_stspd) / 10.
 
c         ------------------------------------------
c         This first part updates the positions by simply
c         extrapolating the current motion along the current
c         heading at the current speed for 3h or 6h.  Be
c         careful with adding and subtracting these distances
c         in the different hemispheres (see the if statements).
c         Remember: In the storm message file, there are NO
c         negative signs to distinguish between hemispheres,
c         so a southern hemisphere latitude will be POSITIVE,
c         but will be distinguished by the 'S'.
          
          strmucomp = rspd * sin(dtr*rhdg)
          strmvcomp = rspd * cos(dtr*rhdg)
c
          vdistdeg = (strmvcomp * secphr * vit_hr_incr) / dtk
          if (storm(ist)%tcv_latns == 'N') then
            rnewlat = rlat + vdistdeg
          else
            rnewlat = rlat - vdistdeg
          endif
c
          avglat = 0.5 * (rlat + rnewlat)
          cosfac = cos(dtr * avglat)
          udistdeg = (strmucomp * secphr * vit_hr_incr) / (dtk * cosfac)
          if (storm(ist)%tcv_lonew == 'W') then
            rnewlon = rlon - udistdeg
          else
            rnewlon = rlon + udistdeg
          endif
 
c         ------------------------------------------
c         This part updates the E/W and N/S characters
c         in the event that a storm changes hemisphere.
c         (N to S and S to N is not really possible, but
c         we'll include the code anyway).  If a storm
c         does change hemisphere, say from W to E at 180,
c         we need to also adjust the new longitude value
c         from say 186W to 174E.  Have to include this
c         code since storm messages contain longitudes on
c         a 0-180 basis (E&W), NOT 0-360.
 
          if (storm(ist)%tcv_latns == 'N') then
            if (rnewlat < 0.) then
              storm(ist)%tcv_latns = 'S'
              rnewlat   = -1. * rnewlat
            endif
          else
            if (rnewlat < 0.) then
              storm(ist)%tcv_latns = 'N'
              rnewlat   = -1. * rnewlat
            endif
          endif
c
          if (storm(ist)%tcv_lonew == 'W') then
            if (rnewlon > 180.) then
              storm(ist)%tcv_lonew = 'E'
              rnewlon = 180. -  abs(rnewlon - 180.)
            endif
          else
            if (rnewlon > 180.) then
              storm(ist)%tcv_lonew = 'W'
              rnewlon = 180. - abs(rnewlon - 180.)
            endif
          endif

          storm(ist)%tcv_lat = int ((rnewlat + 0.05) * 10.)
          storm(ist)%tcv_lon = int ((rnewlon + 0.05) * 10.)
          storm(ist)%tcv_yymmdd = ymd_now
          storm(ist)%tcv_hhmm   = hhmm_now

        endif

        ist = ist + 1
   
      enddo
c
      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine update_future_vits (storm,iprevct,ifuturect,okstorm
     &                              ,vit_hr_incr)
c
c     ABSTRACT: This subroutine updates the vitals from 3h or 6h ahead.
c     It uses the heading and direction values listed in the vitals
c     record (see Module def_vitals for specfics on where to find 
c     heading & direction in the vitals record) to get a new
c     position for the current time by extrapolating *BACKWARDS*
c     3h or 6h to the current time.
c
      USE def_vitals; USE set_max_parms; USE inparms; USE date_checks
      USE trig_vals
c
      type (tcvcard) storm(maxstorm)
      type (datecard) dnow, dold, dfuture

      logical  okstorm(maxstorm)
      integer  vit_hr_incr
c
      ist  = iprevct + 1
      iend = iprevct + ifuturect
      do while (ist <= iend)

        if (okstorm(ist) .and. storm(ist)%tcv_yymmdd == ymd_future .and.
     &                         storm(ist)%tcv_hhmm == hhmm_future) then

          rlat = float(storm(ist)%tcv_lat) / 10.
          rlon = float(storm(ist)%tcv_lon) / 10.
          rhdg = float(storm(ist)%tcv_stdir)
          rspd = float(storm(ist)%tcv_stspd) / 10.

c         IMPORTANT NOTE: Since we are extrapolating *BACKWARDS* in 
c         time in this routine, we have to take that value of the 
c         storm heading in rhdg and switch it by 180 degrees so that
c         we will be pointing back in the direction the storm came
c         from....

          if (rhdg >= 0. .and. rhdg <= 180.) then
            rhdg = rhdg + 180.
          else 
            rhdg = rhdg - 180.
          endif
 
c         ------------------------------------------
c         This first part updates the positions by simply
c         extrapolating the current motion along the REVERSE of
c         the current heading at the current speed for 6 hours. 
c         Be careful with adding and subtracting these distances
c         in the different hemispheres (see the if statements).
c         Remember: In the storm message file, there are NO
c         negative signs to distinguish between hemispheres,
c         so a southern hemisphere latitude will be POSITIVE,
c         but will be distinguished by the 'S'.
          
          strmucomp = rspd * sin(dtr*rhdg)
          strmvcomp = rspd * cos(dtr*rhdg)
c
          vdistdeg = (strmvcomp * secphr * vit_hr_incr) / dtk
          if (storm(ist)%tcv_latns == 'N') then
            rnewlat = rlat + vdistdeg
          else
            rnewlat = rlat - vdistdeg
          endif
c
          avglat = 0.5 * (rlat + rnewlat)
          cosfac = cos(dtr * avglat)
          udistdeg = (strmucomp * secphr * vit_hr_incr) / (dtk * cosfac)
          if (storm(ist)%tcv_lonew == 'W') then
            rnewlon = rlon - udistdeg
          else
            rnewlon = rlon + udistdeg
          endif
 
c         ------------------------------------------
c         This part updates the E/W and N/S characters
c         in the event that a storm changes hemisphere.
c         (N to S and S to N is not really possible, but
c         we'll include the code anyway).  If a storm
c         does change hemisphere, say from W to E at 180,
c         we need to also adjust the new longitude value
c         from say 186W to 174E.  Have to include this
c         code since storm messages contain longitudes on
c         a 0-180 basis (E&W), NOT 0-360.
 
          if (storm(ist)%tcv_latns == 'N') then
            if (rnewlat < 0.) then
              storm(ist)%tcv_latns = 'S'
              rnewlat   = -1. * rnewlat
            endif
          else
            if (rnewlat < 0.) then
              storm(ist)%tcv_latns = 'N'
              rnewlat   = -1. * rnewlat
            endif
          endif
c
          if (storm(ist)%tcv_lonew == 'W') then
            if (rnewlon > 180.) then
              storm(ist)%tcv_lonew = 'E'
              rnewlon = 180. -  abs(rnewlon - 180.)
            endif
          else
            if (rnewlon > 180.) then
              storm(ist)%tcv_lonew = 'W'
              rnewlon = 180. - abs(rnewlon - 180.)
            endif
          endif

          storm(ist)%tcv_lat = int ((rnewlat + 0.05) * 10.)
          storm(ist)%tcv_lon = int ((rnewlon + 0.05) * 10.)
          storm(ist)%tcv_yymmdd = ymd_now
          storm(ist)%tcv_hhmm   = hhmm_now

        endif

        ist = ist + 1
   
      enddo
c
      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_old_dups (storm,inowct,ioldct,okstorm)
c
c     ABSTRACT: The purpose of this subroutine is to loop through the
c     list of storms for the dtg from 3h or 6h ago to eliminate any 
c     duplicates.  Be sure to sort based on storm identifier (e.g.,
c     13L) instead of storm name, since the name may change (e.g., 
c     from "THIRTEEN" to "IRIS") for an upgrade in intensity, but the
c     storm number identifier will remain the same.
c
c     ict     Total number of storm card entries for this dtg
c
      USE def_vitals; USE set_max_parms
c 
      type (tcvcard) storm(maxstorm)
      logical okstorm(maxstorm)
      character found_dup*1
c
      ist  = inowct + 1
      iend = inowct + ioldct
      do while (ist < iend)

        isortnum  = ist + 1
        found_dup = 'n'
        if (okstorm(ist)) then

          do while (isortnum <= iend .and. found_dup == 'n')

            if (storm(ist)%tcv_storm_id == storm(isortnum)%tcv_storm_id)
     &      then
              found_dup = 'y'
            endif
            isortnum = isortnum + 1

          enddo

        endif

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

c     NOTE: The last member of the array to be checked is okay,
c     since all potential duplicates for this record were eliminated
c     in the previous sort while loop just completed, and, further,
c     the last member of this array is either already FALSE (from 
c     being checked off in delete_old), or it's TRUE because it
c     didn't get checked off in delete_old, so keep it.

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_old (storm,inowct,ioldct,okstorm)
c
c     ABSTRACT: This subroutine compares the list of storm card entries
c     from 3h or 6h ago to those from the current time to eliminate
c     any matching storms (i.e., if we've got a current record for a 
c     storm, we obviously don't need the old one).
c
      USE def_vitals; USE set_max_parms
c
      type (tcvcard) storm(maxstorm)
c
      logical   okstorm(maxstorm)
      character found_dup*1 
c
      ist  = inowct + 1
      iend = inowct + ioldct
      do while (ist <= iend)

        isortnum = 1
        found_dup = 'n'
        do while (isortnum <= inowct .and. found_dup == 'n')

          if (storm(ist)%tcv_storm_id == storm(isortnum)%tcv_storm_id)
     &    then
            found_dup = 'y'
          endif
          isortnum = isortnum + 1

        enddo      

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_future (storm,iprevct,ifuturect,okstorm)
c
c     ABSTRACT: This subroutine compares the list of storm card entries
c     from 3h or 6h ahead to those from the current time and from 3h or
c     6h ago to eliminate any matching storms (i.e., we only need the
c     record for the future time if we don't have either a current time
c     record or an old record that we've updated).
c
      USE def_vitals; USE set_max_parms
c
      type (tcvcard) storm(maxstorm)
c
      logical   okstorm(maxstorm)
      character found_dup*1
c
      ist  = iprevct + 1
      iend = iprevct + ifuturect
      do while (ist <= iend)

        isortnum = 1
        found_dup = 'n'
        do while (isortnum <= iprevct .and. found_dup == 'n')

          if (storm(ist)%tcv_storm_id == storm(isortnum)%tcv_storm_id)
     &    then
            found_dup = 'y'
          endif
          isortnum = isortnum + 1

        enddo

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_future_dups (storm,iprevct,ifuturect,okstorm)
c
c     ABSTRACT: The purpose of this subroutine is to loop through the
c     list of storms for the dtg from 3h or 6h ahead to eliminate any
c     duplicates.  Be sure to sort based on storm identifier (e.g.,
c     13L) instead of storm name, since the name may change (e.g.,
c     from "THIRTEEN" to "IRIS") for an upgrade in intensity, but the
c     storm number identifier will remain the same.
c
c     ict     Total number of storm card entries for this dtg
c
      USE def_vitals; USE set_max_parms
c
      type (tcvcard) storm(maxstorm)
      logical okstorm(maxstorm)
      character found_dup*1
c
      ist  = iprevct + 1
      iend = iprevct + ifuturect
      do while (ist < iend)

        isortnum  = ist + 1
        found_dup = 'n'
        if (okstorm(ist)) then

          do while (isortnum <= iend .and. found_dup == 'n')

            if (storm(ist)%tcv_storm_id == storm(isortnum)%tcv_storm_id)
     &      then
              found_dup = 'y'
            endif
            isortnum = isortnum + 1

          enddo

        endif

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

c     NOTE: The last member of the array to be checked is okay,
c     since all potential duplicates for this record were eliminated
c     in the previous sort while loop just completed, and, further,
c     the last member of this array is either already FALSE (from
c     being checked off in delete_future), or it's TRUE because it
c     didn't get checked off in delete_future, so keep it.

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine delete_dups (storm,ict,okstorm)
c
c     ABSTRACT: The purpose of this subroutine is to loop through the
c     list of storms for the current dtg to eliminate any duplicates.
c     Be sure to sort based on storm identifier (e.g.,13L) instead of
c     storm name, since the name may change (e.g., from "THIRTEEN" to
c     "IRIS") for an upgrade in intensity, but the storm number 
c     identifier will remain the same.
c
c     ict     Total number of storm card entries for this dtg
c
      USE def_vitals; USE set_max_parms
c  
      type (tcvcard) storm(maxstorm)
      logical okstorm(maxstorm)
      character found_dup*1
c
      ist = 1
      do while (ist < ict)

        isortnum  = ist + 1
        found_dup = 'n'
        do while (isortnum <= ict .and. found_dup == 'n')

          if (storm(ist)%tcv_storm_id == storm(isortnum)%tcv_storm_id)
     &    then
            found_dup = 'y'
          endif
          isortnum = isortnum + 1

        enddo

        if (found_dup == 'y') then
          okstorm(ist) = .FALSE.
        endif

        ist = ist + 1

      enddo

c     Now set the last member of the array to be checked as okay, 
c     since all potential duplicates for this record were eliminated 
c     in the previous sort while loop just completed.

      okstorm(ict) = .TRUE.
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_nlists (dnow,dold,dfuture,vit_hr_incr)
c
c     ABSTRACT:  Read in the namelists that contain the date for the
c     current time, the time from 3h or 6h ago, and the time from 3h
c     or 6h ahead .  It also converts the input dates for the current 
c     time, the old time and the future time into a format that can 
c     be easily compared against the dates in the TC Vitals file.
c
      USE inparms; USE date_checks
c
      type (datecard) dnow,dold,dfuture
c
      integer   vit_hr_incr
c
      namelist/datenowin/dnow
      namelist/dateoldin/dold
      namelist/datefuturein/dfuture
      namelist/hourinfo/vit_hr_incr
c
      read (5,NML=datenowin,END=801)
  801 continue
      read (5,NML=dateoldin,END=803)
  803 continue
      read (5,NML=datefuturein,END=805)
  805 continue
      read (5,NML=hourinfo,END=807)
  807 continue
c
      ymd_now     = dnow%yy * 10000 + dnow%mm * 100 + dnow%dd
      hhmm_now    = dnow%hh * 100
      ymd_old     = dold%yy * 10000 + dold%mm * 100 + dold%dd
      hhmm_old    = dold%hh * 100
      ymd_future  = dfuture%yy * 10000 + dfuture%mm * 100 + dfuture%dd
      hhmm_future = dfuture%hh * 100
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      integer function char2int (charnum)
c
c     This function takes as input a character numeral and
c     returns the integer equivalent
c
      character*1 charnum,cx(10)
      data cx/'0','1','2','3','4','5','6','7','8','9'/
c
      do i=1,10
        if (charnum.eq.cx(i)) char2int = i-1
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      character function int2char (inum)
c
c     This function takes as input an integer and
c     returns the character numeral equivalent
c
      character*1 cx(10)
      data cx/'0','1','2','3','4','5','6','7','8','9'/
c
      do i=1,10
        ihold=i-1
        if (ihold.eq.inum) int2char = cx(i)
      enddo
c
      return
      end
