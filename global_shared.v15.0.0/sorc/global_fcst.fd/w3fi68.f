      subroutine w3fi68 (id, pds)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    w3fi68      convert 25 word array to grib pds
c   prgmmr: r.e.jones        org: w/nmc42    date: 91-05-14
c
c abstract: converts an array of 25, or 27 integer words into a
c   grib product definition section (pds) of 28 bytes , or 30 bytes.
c   if pds bytes > 30, they are set to zero.
c
c program history log:
c   91-05-08  r.e.jones
c   92-09-25  r.e.jones   change to 25 words of input, level
c                         can be in two words. (10,11)
c   93-01-08  r.e.jones   change for time range indicator if 10,
c                         store time p1 in pds bytes 19-20.
c   93-01-26  r.e.jones   correction for fixed height above
c                         ground level
c   93-03-29  r.e.jones   add save statement
c   93-06-24  cavanough   modified program to allow for generation
c                         of pds greater than 28 bytes (the desired
c                         pds size is in id(1).
c   93-09-30  farley      change to allow for subcenter id; put
c                         id(24) into pds(26).
c   93-10-12  r.e.jones   changes for on388 rev. oct 9,1993, new
c                         levels 125, 200, 201.
c   94-02-23  r.e.jones   take out sbytes, replace with do loop
c   94-04-14  r.e.jones   changes for on388 rev. mar 24,1994, new
c                         levels 115,116.
c   94-12-04  r.e.jones   change to add id words 26, 27 for pds
c                         bytes 29 and 30.
c   95-09-07  r.e.jones   change for new level 117, 119.
c   95-10-31  iredell     removed saves and prints
c 2003-02-25  iredell     recognize level type 126
c 2005-05-06  d.c.stokes  recognize level types 235, 237, 238
c
c usage:    call w3fi68 (id, pds)
c   input argument list:
c     id       - 25, 27 word integer array
c   output argument list:
c     pds      - 28 30,  or greater character pds for edition 1
c
c remarks: layout of 'id' array:
c     id(1)  = number of bytes in product definition section (pds)
c     id(2)  = parameter table version number
c     id(3)  = identification of originating center
c     id(4)  = model identification (allocated by originating center)
c     id(5)  = grid identification
c     id(6)  = 0 if no gds section, 1 if gds section is included
c     id(7)  = 0 if no bms section, 1 if bms section is included
c     id(8)  = indicator of parameter and units (table 2)
c     id(9)  = indicator of type of level       (table 3)
c     id(10) = value 1 of level  (0 for 1-100,102,103,105,107
c              109,111,113,115,117,119,125,126,160,200,201,
c              235,237,238 
c              level is in id word 11)
c     id(11) = value 2 of level
c     id(12) = year of century
c     id(13) = month of year
c     id(14) = day of month
c     id(15) = hour of day
c     id(16) = minute of hour   (in most cases set to 0)
c     id(17) = fcst time unit
c     id(18) = p1 period of time
c     id(19) = p2 period of time
c     id(20) = time range indicator
c     id(21) = number included in average
c     id(22) = number missing from averages
c     id(23) = century  (20, change to 21 on jan. 1, 2001)
c     id(24) = subcenter identification
c     id(25) = scaling power of 10
c     id(26) = flag byte, 8 on/off flags
c              bit number  value  id(26)   definition
c              1           0      0      full fcst field
c                          1      128    fcst error field
c              2           0      0      original fcst field
c                          1      64     bias corrected fcst field
c              3           0      0      original resolution retained
c                          1      32     smoothed field
c              note: id(26) can be the sum of bits 1, 2, 3.
c              bits 4-8 not used, set to zero
c              if id(1) is 28, you do not need id(26) and id(27).
c     id(27) = unused, set to 0 so pds byte 30 is set to zero.
c
c   subprogram can be called from a multiprocessing environment.
c
c attributes:
c   language: silicongraphics 3.5 fortran 77
c   machine:  silicongraphics iris-4d/25, 35, indigo, indy
c   language: cray cft77 fortran
c   machine:  cray c916/256, j916/2048
c
c$$$
c
      integer        id(*)
c
      character * 1  pds(*)
c
        pds(1)  = char(mod(id(1)/65536,256))
        pds(2)  = char(mod(id(1)/256,256))
        pds(3)  = char(mod(id(1),256))
        pds(4)  = char(id(2))
        pds(5)  = char(id(3))
        pds(6)  = char(id(4))
        pds(7)  = char(id(5))
        pds(8)  = char(ior(ishft(id(6),7),
     &                      ishft(id(7),6)))
        pds(9)  = char(id(8))
        pds(10) = char(id(9))
        i9      = id(9)
c
c       test type of level to see if level is in two
c       words or one
c
        if ((i9.ge.1.and.i9.le.100).or.i9.eq.102.or.
     &       i9.eq.103.or.i9.eq.105.or.i9.eq.107.or.
     &       i9.eq.109.or.i9.eq.111.or.i9.eq.113.or.
     &       i9.eq.115.or.i9.eq.117.or.i9.eq.119.or.
     &       i9.eq.125.or.i9.eq.126.or.i9.eq.160.or.
     &       i9.eq.200.or.i9.eq.201.or.i9.eq.235.or.
     &       i9.eq.237.or.i9.eq.238) then
          level   = id(11)
          if (level.lt.0) then
            level = - level
            level = ior(level,32768)
          end if
          pds(11) = char(mod(level/256,256))
          pds(12) = char(mod(level,256))
        else
          pds(11) = char(id(10))
          pds(12) = char(id(11))
        end if
        pds(13) = char(id(12))
        pds(14) = char(id(13))
        pds(15) = char(id(14))
        pds(16) = char(id(15))
        pds(17) = char(id(16))
        pds(18) = char(id(17))
c
c       test time range indicator (pds byte 21) for 10
c       if so put time p1 in pds bytes 19-20.
c
        if (id(20).eq.10) then
          pds(19) = char(mod(id(18)/256,256))
          pds(20) = char(mod(id(18),256))
        else
          pds(19) = char(id(18))
          pds(20) = char(id(19))
        end if
      print *,' id20=',id(20),' id18=',id(18),' id19=',id(19)
     &,' pds=',pds(19),pds(20)
        pds(21) = char(id(20))
        pds(22) = char(mod(id(21)/256,256))
        pds(23) = char(mod(id(21),256))
        pds(24) = char(id(22))
        pds(25) = char(id(23))
        pds(26) = char(id(24))
        iscale  = id(25)
        if (iscale.lt.0) then
          iscale = -iscale
          iscale =  ior(iscale,32768)
        end if
        pds(27) = char(mod(iscale/256,256))
        pds(28) = char(mod(iscale    ,256))
        if (id(1).gt.28) then
          pds(29) = char(id(26))
          pds(30) = char(id(27))
        end if
c
c       set pds 31-?? to zero
c
        if (id(1).gt.30) then
          k = id(1)
          do i = 31,k
            pds(i) = char(00)
          end do
        end if
c
      return
      end
