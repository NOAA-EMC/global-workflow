      subroutine prlevel(ipdtn,ipdtmpl,labbrev)
      
      integer,intent(in) :: ipdtn
      integer,intent(in) :: ipdtmpl(*)
      character(len=40),intent(out) :: labbrev

      character(len=10) :: tmpval1,tmpval2
      
      labbrev(1:40)=" "

      selectcase (ipdtn)
         case(0:15)
           ipos=10
         case(40:43)
           ipos=11
         case(44:47)
           ipos=16
         case(48)
           ipos=21
         case(50:51)
           ipos=10
         case(52)
           ipos=13
         case(91)
           ipos=10
         case default
           ipos=10
      end select

      if ( ipdtmpl(ipos) .eq. 100 .and.           ! Pressure Level
     &     ipdtmpl(ipos+3) .eq. 255 ) then
         !write(tmpval1,*) ipdtmpl(ipos+2)/100
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1)+2)
         labbrev=trim(tmpval1)//" mb"

      elseif ( ipdtmpl(ipos) .eq. 100 .and.           ! Pressure Layer
     &         ipdtmpl(ipos+3) .eq. 100 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1)+2)
         call frmt(tmpval2,ipdtmpl(ipos+5),ipdtmpl(ipos+4)+2)
         labbrev=trim(tmpval1)//" - "//trim(tmpval2)//" mb"

      elseif ( ipdtmpl(ipos) .eq. 101 ) then       ! Mean Sea Level
         labbrev(1:30)=" Mean Sea Level "

      elseif ( ipdtmpl(ipos) .eq. 102 .and.        ! Altitude above MSL
     &     ipdtmpl(ipos+3) .eq. 255 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         labbrev=trim(tmpval1)//" m above MSL"

      elseif ( ipdtmpl(ipos) .eq. 103 .and.        ! Height above Ground
     &     ipdtmpl(ipos+3) .eq. 255 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         labbrev=trim(tmpval1)//" m above ground"

      elseif ( ipdtmpl(ipos) .eq. 103 .and.        ! Height above Ground
     &     ipdtmpl(ipos+3) .eq. 103 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         call frmt(tmpval2,ipdtmpl(ipos+5),ipdtmpl(ipos+4))
         labbrev=trim(tmpval1)//" - "//trim(tmpval2)//" m AGL"

      elseif ( ipdtmpl(ipos) .eq. 104 .and.        ! Sigma Level
     &     ipdtmpl(ipos+3) .eq. 255 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         labbrev=trim(tmpval1)//" sigma"

      elseif ( ipdtmpl(ipos) .eq. 104 .and.        ! Sigma Layer
     &     ipdtmpl(ipos+3) .eq. 104 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         call frmt(tmpval2,ipdtmpl(ipos+5),ipdtmpl(ipos+4))
         labbrev=trim(tmpval1)//" - "//trim(tmpval2)//" sigma"

      elseif ( ipdtmpl(ipos) .eq. 105 .and.        ! Hybrid Level
     &     ipdtmpl(ipos+3) .eq. 255 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         labbrev=trim(tmpval1)//" hybrid lvl"

      elseif ( ipdtmpl(ipos).eq.105 .and.          ! Hybrid Level
     &         ipdtmpl(ipos+3).eq.105) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         call frmt(tmpval2,ipdtmpl(ipos+5),ipdtmpl(ipos+4))
         labbrev=trim(tmpval1)//" - "//trim(tmpval2)//" hybrid lvl"


      elseif ( ipdtmpl(ipos) .eq. 106 .and.        ! Depth Below Land Sfc
     &     ipdtmpl(ipos+3) .eq. 255 ) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         labbrev=trim(tmpval1)//" m below land"

      elseif ( ipdtmpl(ipos).eq.106 .and.          ! Depth Below Land Sfc Layer
     &         ipdtmpl(ipos+3).eq.106) then
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1))
         call frmt(tmpval2,ipdtmpl(ipos+5),ipdtmpl(ipos+4))
         labbrev=trim(tmpval1)//" - "//trim(tmpval2)//" m DBLY"

      elseif ( ipdtmpl(ipos) .eq. 107 ) then       ! Isentrophic (theta) level (THEL)
         labbrev(1:30)=" Isentropic level"

      elseif ( ipdtmpl(ipos).eq.108 .and.          ! Press Diff from Ground Layer
     &         ipdtmpl(ipos+3).eq.108) then
         !write(tmpval1,*) ipdtmpl(ipos+2)/100
         !write(tmpval2,*) ipdtmpl(ipos+5)/100
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1)+2)
         call frmt(tmpval2,ipdtmpl(ipos+5),ipdtmpl(ipos+4)+2)
         labbrev=trim(tmpval1)//" - "//trim(tmpval2)//" mb SPDY"

      elseif ( ipdtmpl(ipos) .eq. 110 ) then       ! Layer between two hybrid levels (HYBY)
         labbrev(1:30)=" Layer bet 2-hyb lvl"

      elseif ( ipdtmpl(ipos).eq.109 .and.          ! Potential Vorticity Sfc
     &         ipdtmpl(ipos+3).eq.255) then
         !write(tmpval1,*) ipdtmpl(ipos+2)
         call frmt(tmpval1,ipdtmpl(ipos+2),ipdtmpl(ipos+1)-6)
         labbrev=trim(tmpval1)//" pv surface"

      elseif ( ipdtmpl(ipos) .eq. 111 ) then       ! Eta Level (EtaL)
         labbrev(1:30)=" Eta level"
      elseif ( ipdtmpl(ipos) .eq. 114 ) then       ! Layer between two isentropic levels (THEY)
         labbrev(1:30)=" Layer bet. 2-isent."
      elseif ( ipdtmpl(ipos) .eq. 117 ) then       ! Mixed layer depth
         labbrev(1:30)=" Mixed layer depth"
      elseif ( ipdtmpl(ipos) .eq. 120 ) then       ! Layer between two Eta levels (EtaY)
         labbrev(1:30)=" Layer bet. 2-Eta lvl"
      elseif ( ipdtmpl(ipos) .eq. 121 ) then       ! Layer between two isobaric surface (IBYH)
         labbrev(1:30)=" Layer bet. 2-isob." 
      elseif ( ipdtmpl(ipos) .eq. 125 ) then       ! Specified height level above ground  (HGLH)
         labbrev(1:30)=" Specified height lvl"
      elseif ( ipdtmpl(ipos) .eq. 126 ) then       ! Isobaric Level (ISBP)
         labbrev(1:30)=" Isobaric level"

      elseif ( ipdtmpl(ipos) .eq. 160 ) then       ! Depth below sea level
         labbrev(1:30)=" Depth below sea lvl"

      elseif ( ipdtmpl(ipos) .eq. 170 ) then       ! Ionospheric D-region
         labbrev(1:30)=" Ionospheric D-region lvl"

      elseif ( ipdtmpl(ipos) .eq. 1 ) then         ! Ground/Water Surface
         labbrev(1:30)=" Surface "

      elseif ( ipdtmpl(ipos) .eq. 2 ) then         ! Cloud base level (CBL)
         labbrev(1:30)=" Cloud base lvl"
      elseif ( ipdtmpl(ipos) .eq. 3 ) then         ! Cloud top level (CTL)
         labbrev(1:30)=" Cloud top lvl"

      elseif ( ipdtmpl(ipos) .eq. 4 ) then         ! Freezing Level
         labbrev(1:30)=" 0 Deg Isotherm"

      elseif ( ipdtmpl(ipos) .eq. 5 ) then         ! Level of adiabatic condensation lifted
         labbrev(1:30)=" Level of adiabatic"     ! from the surface

      elseif ( ipdtmpl(ipos) .eq. 6 ) then         ! Max Wind Level
         labbrev(1:30)=" Max wind lvl"

      elseif ( ipdtmpl(ipos) .eq. 7 ) then         ! Tropopause
         labbrev(1:30)=" Tropopause"

      elseif ( ipdtmpl(ipos) .eq. 8 ) then         ! Nominal top of Atmosphere
         labbrev(1:30)=" Nom. top"

      elseif ( ipdtmpl(ipos) .eq. 9 ) then         !  Sea bottom
         labbrev(1:30)=" Sea Bottom"

      elseif ( ipdtmpl(ipos) .eq. 10 ) then        ! Entire Atmosphere (EATM)
         labbrev(1:30)=" Entire Atmosphere"

      elseif ( ipdtmpl(ipos) .eq. 11 ) then        ! Cumulonimbus Base
         labbrev(1:30)=" Cumulonimbus Base"

      elseif ( ipdtmpl(ipos) .eq. 12 ) then        ! Cumulonimbus Top
         labbrev(1:30)=" Cumulonimbus Top"

      elseif ( ipdtmpl(ipos) .eq. 20 ) then        ! Isothermal level
         labbrev(1:30)=" Isothermal level"

      elseif ( ipdtmpl(ipos) .eq. 200 ) then       ! Entire Atmosphere (EATM)
         labbrev(1:30)=" Entire Atmosphere"
      elseif ( ipdtmpl(ipos) .eq. 201 ) then       ! Entire ocean (EOCN)
         labbrev(1:30)=" Entire ocean"
      elseif ( ipdtmpl(ipos) .eq. 204 ) then       ! Highest tropospheric freezing level (HTFL)
         labbrev(1:30)=" Highest Frz. lvl"
      elseif ( ipdtmpl(ipos) .eq. 206 ) then       ! Grid scale cloud bottom level (GCBL)
         labbrev(1:30)=" Grid scale cloud bl" 
      elseif ( ipdtmpl(ipos) .eq. 207 ) then       ! Grid scale cloud top level (GCTL)
         labbrev(1:30)=" Grid scale cloud tl"
      elseif ( ipdtmpl(ipos) .eq. 209 ) then       ! Boundary layer cloud bottom level (BCBL)
         labbrev(1:30)=" Boundary layer cbl"
      elseif ( ipdtmpl(ipos) .eq. 210 ) then       ! Boundary layer cloud top level (BCTL)
         labbrev(1:30)=" Boundary layer ctl"
      elseif ( ipdtmpl(ipos) .eq. 211 ) then       ! Boundary layer cloud layer (BCY)
         labbrev(1:30)=" Boundary layer cl"
      elseif ( ipdtmpl(ipos) .eq. 212 ) then       ! Low cloud bottom level (LCBL)
         labbrev(1:30)=" Low cloud bot. lvl"
      elseif ( ipdtmpl(ipos) .eq. 213 ) then       ! Low cloud top level (LCTL)
         labbrev(1:30)=" Low cloud top lvl"
      elseif ( ipdtmpl(ipos) .eq. 214 ) then       ! Low cloud layer (LCY)
         labbrev(1:30)=" Low cloud layer"
      elseif ( ipdtmpl(ipos) .eq. 215 ) then       ! Cloud ceiling (CEIL)
         labbrev(1:30)=" Cloud ceiling"
      elseif ( ipdtmpl(ipos) .eq. 220 ) then       ! Planetary Boundary Layer (PBLRI)
         labbrev(1:30)=" Planetary boundary"
      elseif ( ipdtmpl(ipos) .eq. 221 ) then       ! Layer Between Two Hybrid Levels (HYBY)
         labbrev(1:30)=" Layer 2 Hybrid lvl "
      elseif ( ipdtmpl(ipos) .eq. 222 ) then       ! Middle cloud bottom level (MCBL)
         labbrev(1:30)=" Mid. cloud bot. lvl"
      elseif ( ipdtmpl(ipos) .eq. 223 ) then       ! Middle cloud top level (MCTL)
         labbrev(1:30)=" Mid. cloud top lvl"
      elseif ( ipdtmpl(ipos) .eq. 224 ) then       ! Middle cloud layer (MCY)
         labbrev(1:30)=" Middle cloud layer"
      elseif ( ipdtmpl(ipos) .eq. 232 ) then       ! High cloud bottom level (HCBL)
         labbrev(1:30)=" High cloud bot. lvl"
      elseif ( ipdtmpl(ipos) .eq. 233 ) then       ! High cloud top level (HCTL)
         labbrev(1:30)=" High cloud top lvl"
      elseif ( ipdtmpl(ipos) .eq. 234 ) then       ! High cloud layer (HCY)
         labbrev(1:30)=" High cloud layer"
      elseif ( ipdtmpl(ipos) .eq. 235 ) then       ! Ocean isotherm level (OITL)
         labbrev(1:30)=" Ocean Isotherm lvl"
      elseif ( ipdtmpl(ipos) .eq. 236 ) then       ! Layer between two depth below ocean sfc (OLYR)
         labbrev(1:30)=" Layer 2-depth below"
      elseif ( ipdtmpl(ipos) .eq. 237 ) then       ! Bottom of Ocean mixed layer (OBML)
         labbrev(1:30)=" Bot. Ocean mix. lyr"
      elseif ( ipdtmpl(ipos) .eq. 238 ) then       ! Bottom of Ocean iisothermal layer (OBIL)
         labbrev(1:30)=" Bot. Ocean iso. lyr"
      elseif ( ipdtmpl(ipos) .eq. 239 ) then       ! Layer ocean surface and 26C ocean
         labbrev(1:30)=" layer ocean sfc 26C"    ! isothermal level (S26CY)
      elseif ( ipdtmpl(ipos) .eq. 240 ) then       ! Ocean Mixed Layer
         labbrev(1:30)=" Ocean Mixed Layer"    
      elseif ( ipdtmpl(ipos) .eq. 241 ) then       ! Ordered Sequence of Data 
         labbrev(1:30)=" Order Seq. Of Data"    
      elseif ( ipdtmpl(ipos) .eq. 242 ) then       ! Convective cloud bottom level (CCBL)
         labbrev(1:30)=" Con. cloud bot. lvl"
      elseif ( ipdtmpl(ipos) .eq. 243 ) then       ! Convective cloud top level (CCTL)
         labbrev(1:30)=" Con. cloud top lvl"
      elseif ( ipdtmpl(ipos) .eq. 244 ) then       ! Convective cloud layer (CCY)
         labbrev(1:30)=" Conv. cloud layer"
      elseif ( ipdtmpl(ipos) .eq. 245 ) then       ! Lowest level of the wet bulb zero (LLTW)
         labbrev(1:30)=" Lowest lvl wet bulb"
      elseif ( ipdtmpl(ipos) .eq. 246 ) then       ! Maximum equiv. potential temp. level (MTHE) 
         labbrev(1:30)=" Max. equi. potential"
      elseif ( ipdtmpl(ipos) .eq. 247 ) then       ! Equilibrium level (EHLT)
         labbrev(1:30)=" Equilibrium level"
      elseif ( ipdtmpl(ipos) .eq. 248 ) then       ! Shallow convective cloud bottom level (SCBL)
         labbrev(1:30)=" Shallow con. cld bl"
      elseif ( ipdtmpl(ipos) .eq. 249 ) then       ! Shallow convective cloud top level (SCTL)
         labbrev(1:30)=" Shallow con. cld tl"
      elseif ( ipdtmpl(ipos) .eq. 251 ) then       ! Deep convective cloud bottom level (DCBL)
         labbrev(1:30)=" Deep conv. cld bl"
      elseif ( ipdtmpl(ipos) .eq. 252 ) then       ! Deep convective cloud top level (DCTL)
         labbrev(1:30)=" Deep conv. cld tl"
      elseif ( ipdtmpl(ipos) .eq. 253 ) then       ! Lowest bottom level of supercooled
         labbrev(1:30)=" Lowest bot. lvl sup"    ! liquid water layer (LBLSW)
      elseif ( ipdtmpl(ipos) .eq. 254 ) then       ! Highest top level of supercooled 
         labbrev(1:30)=" highest top lvl sup"    ! liquid water layer (HBLSW)

      else
         write(labbrev,fmt='(1x,I4," (Unknown Lvl)")') 
     &         ipdtmpl(ipos)
      endif

      return
      end


      subroutine frmt(cval,ival,iscal)
     
      character(len=10),intent(out) :: cval
      integer,intent(in) :: ival,iscal

      real :: rval
      integer :: newscal
      character(len=7) :: cformat

      if  ( iscal .eq. 0 ) then
         write(cval,'(I0)') ival
      else
         newscal=-1*iscal
         rval=real(ival)*(10.0**newscal)
         if ( rval .eq. real(nint(rval)) ) then
            write(cval,'(1X,I0)') nint(rval)
         else
            write(cformat,fmt='("(f0.",I1,")")') iabs(iscal)
            !write(6,'(A)') "DEGRIB2:",cformat
            write(cval,fmt=cformat) rval
         endif
      endif

      return
      end
