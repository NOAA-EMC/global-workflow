      subroutine kengrd(ib,ibp,jb,jbp,di,dj,
     &           blat,blon,n,idim,jdim,colat)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PROGRAM NAME (up to 20 characters)
C   PRGMMR: YOUR NAME        ORG: W/NMCXX    DATE: YY-MM-DD
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL PROGRAMMER'S NAME HERE
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$

      real di(n),dj(n),blat(n),blon(n),colat(jdim)
      integer ib(n),ibp(n),jb(n),jbp(n),n,idim,jdim
      integer jb1, jb2
      real xlon, dlon, b, xlat, dlat, ylat
      k = 0
c
c  for all points 1 to n
c
      outer: do
      k = k + 1
      if(k.gt.n) exit outer
c
c  find the longitude grid for blon
c
      xlon = blon(k)
      if(xlon.lt.0.) xlon = 360. + xlon
      if(xlon.ge.360.) xlon = xlon - 360.
      dlon = 360. / float(idim)
      b = xlon / dlon + 1.
      ib(k) = ifix(b)
      ibp(k) = ib(k) + 1
      if(ibp(k).gt.idim) ibp(k) = ibp(k) - idim
      di(k) = (b - float(ib(k)))
c
c  find the latitude. We allow for uneven latitude grid system
c
      xlat = blat(k) + 90.
      dlat = 180. / float(jdim)
      b = xlat / dlat + 1.
      jb(k) = ifix(b)
      jb(k) = min(jb(k),jdim)
      jb(k) = max(jb(k),1)
      ylat = colat(jb(k))
      if((jb(k).eq.1.and.xlat.lt.ylat).or.
     &   (jb(k).eq.jdim.and.xlat.ge.ylat)) then
        jbp(k) = jb(k)
        dj(k) = 1.
        cycle outer
      endif
      do
        if(xlat.ge.ylat) then
          if(colat(jb(k)+1).gt.xlat) then
            jbp(k) = jb(k) + 1
            dj(k) = (xlat - ylat) / (colat(jbp(k)) - ylat)
            cycle outer
          else
            jb(k) = jb(k) + 1
            ylat = colat(jb(k))
          endif
        else
          if(colat(jb(k)-1).lt.ylat) then
            jb(k) = jb(k) - 1
            ylat = colat(jb(k))
          else
            jbp(k) = jb(k)
            jb(k) = jb(k) - 1
            ylat = colat(jb(k))
            dj(k) = (xlat - ylat) / (colat(jbp(k)) - ylat)
            cycle outer
          endif
        endif
      enddo
      enddo outer
c
c  The MRF model grid goes from north to south so I need to flip them
c
      do k = 1, n
        jb1 = jdim - jb(k) + 1
        jb2 = jdim - jbp(k) + 1
        jb(k) = jb2
        jbp(k) = jb1
        dj(k) = 1. - dj(k)
      enddo
      return
      end
