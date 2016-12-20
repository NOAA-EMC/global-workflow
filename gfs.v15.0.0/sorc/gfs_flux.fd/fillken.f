      subroutine fillken(sfc,k,kdim,kpoint,ib,ibp,jb,jbp,di,dj,
     &           data,idim,jdim,imask,lsmask)
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
      real sfc(kdim,kpoint),di(kpoint),dj(kpoint),data(idim,jdim)
      integer ib(kpoint),ibp(kpoint),jb(kpoint),jbp(kpoint)
      integer i,j,kp,k,kdim,kpoint,idim,jdim, imask(kpoint)
      integer nn(4), dd(4), ii(4), jj(4)
      real lsmask(idim,jdim)
      logical found
c
c  this routine performs the extraction of a set of ken points
c  from a grid point data
c  we use the nearest neighbor right now
c
      if(k.eq.11.or.k.eq.12) then
      do kp = 1, kpoint
        ip = ibp(kp)
        i = ib(kp)
        jp = jbp(kp)
        j = jb(kp)
        pi = ib(kp) + di(kp)
        pj = jb(kp) + dj(kp)
        call qpfint(data,idim,jdim,pi,pj,sfc(k,kp))
c       if(di(kp).gt..5) then
c         i = ibp(kp)
c       else
c         i = ib(kp)
c       endif
c       if(dj(kp).gt..5) then
c         j = jbp(kp)
c       else
c         j = jb(kp)
c       endif
c       sfc(k,kp) = data(i,j)
      enddo
c     elseif(k.eq.3) then
c     do kp = 1, kpoint
c       if(di(kp).gt..5) then
c         i = ibp(kp)
c       else
c         i = ib(kp)
c       endif
c       if(dj(kp).gt..5) then
c         j = jbp(kp)
c       else
c         j = jb(kp)
c       endif
c       sfc(k,kp) = data(i,j)
c     enddo
      else
      do kp = 1, kpoint
        if(di(kp).gt..5) then
          i = ibp(kp)
        else
          i = ib(kp)
        endif
        if(dj(kp).gt..5) then
          j = jbp(kp)
        else
          j = jb(kp)
        endif
        if(nint(lsmask(i,j)).eq.imask(kp)) then
          sfc(k,kp) = data(i,j)
c         print *, ' for station ',kp,', nearest station mask matches'
        else
          dd(1) = di(kp) ** 2 + dj(kp) ** 2
          dd(2) = (1.-di(kp)) ** 2 + dj(kp) ** 2
          dd(3) = di(kp) ** 2 + (1. - dj(kp)) ** 2
          dd(4) = (1.-di(kp)) ** 2 + (1. - dj(kp)) ** 2
          ii(1) = ib(kp)
          ii(2) = ibp(kp)
          ii(3) = ib(kp)
          ii(4) = ibp(kp)
          jj(1) = jb(kp)
          jj(2) = jb(kp)
          jj(3) = jbp(kp)
          jj(4) = jbp(kp)
          nn(1) = 1
          nn(2) = 2
          nn(3) = 3
          nn(4) = 4
c
c   order the four points in relation to the station distance
c   the nearest station gets nn(1)
c
          do i1 = 1, 3
            do i2 = i1 + 1, 4
              if(dd(i2).lt.dd(i1)) then
                nk = nn(i1)
                nn(i1) = nn(i2)
                nn(i2) = nk
              endif
            enddo
          enddo
          found = .false.
          do in = 1, 4
            ni = nn(in)
            i = ii(ni)
            j = jj(ni)
            if(nint(lsmask(i,j)).eq.imask(kp).and.
     &         .not.found) then
              sfc(k,kp) = data (i,j)
              found = .true.
            endif
c           print *, ' lsmask, imask, found =', nint(lsmask(i,j)),
c    &       imask(kp), found
          enddo
          if(.not.found) then
            ip = ibp(kp)
            i = ib(kp)
            jp = jbp(kp)
            j = jb(kp)
            pi = ib(kp) + di(kp)
            pj = jb(kp) + dj(kp)
            sfc(k,kp) = di(kp)*dj(kp)*data(ip,jp)
     &            + (1.-di(kp))*dj(kp)*data(i,jp)
     &            + (1.-dj(kp))*di(kp)*data(ip,j)
     &            + (1.-dj(kp))*(1.-di(kp))*data(i,j)
          else
c           print *, ' for station ',kp,' found one matching mask'
c           print *, ' kp, imask =', kp, imask(kp)
c           print *, ' lsmask at the four points'
c           i1 = ib(kp)
c           i2 = ibp(kp)
c           j1 = jb(kp)
c           j2 = jbp(kp)
c           print *, nint(lsmask(i1,j1)), nint(lsmask(i2,j1)),
c    &        nint(lsmask(i1,j2)), nint(lsmask(i2,j2))
          endif
        endif
c       For snow depth, the minimum depth is zero
        if(k.eq.10) sfc(k,kp) = max(sfc(k,kp),0.)
      enddo
      endif
      return
      end
