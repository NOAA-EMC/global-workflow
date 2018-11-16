        subroutine GULFTL()
c   This routine searches fortran input unit 13 for
c   formatted lines beginning with PUTLA or GULFT.
c   PUTLA lines contain all of the arguments for
c   calls to PUTLAB and enable titles (or any other
c   characters) to be specified at execution time.
c   The GULFT lines were implemented first, these
c   allow short titles to be plotted in a fixed
c   font with only coordinate information
c   and are thus easier for the user who does
c   not know much about putlab, to specify .
c   Using GULFT lines is somewhat analagous to
c   using the old GULFTL subroutine found in many
c   MVS FAX plotting codes.  Using PUTLA is a much
c   more general way  to plot character
c   data at execution time. 
c
c   It is common and expected in many cases  for 
c   PUTLA and GULFT lines to be absent from
c   fortran unit 13, in that case the subroutine
c   does nothing.  If fortran unit 13 is not connected, 
c   the subroutine also does nothing.
c
       common/pshift/ishiftx(50),ishifty(50),ishiftxx,ishiftyy
        dimension kp(2)
        character*80  cline
        character*160 cline2
c     SEARCH UNIT 13 FOR ANY TITLES DESIRED
c     generic putlab input search 
c
c        open(13,file='fort.13')
c
ckumar
ckumar Changed the Unit # to 13 from 4
ckumar
        do 5,k=1,999999
        read(13,104,end=599)cline2
 104    format(a160)
        if(cline2(1:5) .eq. 'PUTLA') then
         read(cline2,103)
     1 ix,jx,font,angle,nchar,kp(1),kp(2),irt
 103    format(6x,2i5,2f5.1,1x,i3,i2,i2,i2)
        ix=ix+ishiftxx
        jx=jx+ishiftyy
        call putlab
     1 (ix,jx,font,cline2(39:39+nchar),angle,nchar,kp,irt)
        endif
 5      continue
 599     continue
ckumar
        rewind(13)
ckumar
c      end generic putlab search      
c  now search for specific short titles (easier to
c  specify in input lines)
        kp(1)=1
        kp(2)=0
        do 10,k=1,9999999
ckumar
         read(13,101,end=699)cline
ckumar
 101   format(a80)
           if(cline(1:5) .eq. 'GULFT') then
         read(cline,102)ix,jx
 102    format(6x,i5,i5)
        ix=ix+ishiftxx
        jx=jx+ishiftyy
        call putlab(ix,jx,01.,cline(18:47),90.,30,kp,0)
        print *,' GULFTL PLOT',ix,jx,cline(17:47)
        endif
 10     continue
 699    continue
ckumar
       rewind(13)
ckumar
        do 15,k=1,9999999
ckumar
        read(13,101,end=799) cline
ckumar
        if(cline(1:3).eq. 'BOX' )then
        read(cline,107)ix,iy,il,jl
 107    format(4x,4i5)
        ix=ix+ishiftxx
        iy=iy+ishiftyy
        call box(ix,iy,il,jl)
          endif
 15     continue
 799    continue
ckumar
        rewind (13)
ckumar
         return 
        end
