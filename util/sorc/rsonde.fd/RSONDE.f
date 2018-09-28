       program RSONDE 
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: RSONDE
C   PRGMMR: SAGER            ORG: NP12        DATE: 1999-09-27
C
C ABSTRACT:      Generate formatted radiosonde                  
c                report.   Used downstream by WNDANLFV and 
c                also easily human readable for data debugging
c                purposes.
C
C PROGRAM HISTORY LOG:
C   97-01-17  VandenBerghe  first written
C   98-07-09  Sager         Y2K testing and conversion to f90 compiler
C   99-09-10  Sager         Conversion to IBM SP
C
C USAGE:
C   INPUT FILES:
c     fort.11   - bufr "prepbufr" file containing radiosonde
c                 reports 
c                  assigned -Fcos fort.10
c     fort.5 (stdin)  single word of text specifying "P" or "Z"
c                     directs program to extract pressure or geopotentia
c                     from fort.10.  BUFRLIB bug prevents concurrent ext
c                     of both (grumble grumble!!) 
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     
c       fort.51       formatted sounding report 
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - redupa
C     LIBRARY:   - UFBINT OPENBF READMG READSB
C       COMMON   - none
C       W3LIB    - W3TAG
c       libraries are loaded with -l /nwprod/w3lib -l /nwprod/bufrlib/bu
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - system library return codes .. no user
c                - return codes
C
C REMARKS:                                               
C
C ATTRIBUTES:
C   MACHINE:  IBM SP                                                  
C
C$$$
        Character*8 sre,ere
        character*1 cflag
        common/stuff/pad(1000),hdr,pa2(1000),arr
        common/dat/idate
        dimension hdr(10) 
        character*80 chead
         equivalence (chead,hdr)
        dimension arr(10,255)
      CALL W3TAGB('RSONDE',1999,0270,0060,'NP12')                   

        read(5,901) cflag
 901    format(a1)
C       print *,' cflag is ',cflag
        sre='      70'
        ere='      78'
        inum=1
        do 10,k=1,300000
         do l=1,10
         do m=1,255
         arr(l,m)=0
         end do
         end do
         do l=1,10
         hdr(l)=0
         end do
         if(cflag .eq. 'P' )call redupap(sre,ere,hdr,arr,is,is2)
C        PRINT *,'  returning from redupap'
         if(cflag .eq. 'Z') call redupaz(sre,ere,hdr,arr,is,is2)
          if(cflag .ne. 'Z' .and.  cflag .ne. 'P') then
             print *,' ERROR IN SPECIFYING PRESSURE/Z CHOICE'
             call  errexit (240) 
             endif
c         call redupa(sre,ere,hdr,arr,is,is2)
C        print *,' is2 is ',is2
C
C       Test for end of file.  If found, stop normally
C
        if(is2 .ne. 0)  THEN
            stop 'redupa '
            CALL W3TAGE('RSONDE') 
        END IF
         do 40,l=1,39
         do 40,j=1,10
         if(arr(j,l) .gt. 999999 .or. arr(j,l) .lt. -999999)
     1    arr(j,l)=-999999
 40       continue
          do 12,kl=1,39
          if(arr(9,kl) .gt. 2.0)  then
               arr(5,kl)=-999999
               arr(6,kl)=-999999
           endif
          if(arr(8,kl) .gt. 2.0) arr(4,kl)=-999999    
          if(arr(8,kl) .gt. 2.0) arr(3,kl)=-999999    
          if(arr(1,kl) .le. 1200. .and. arr(1,kl) .ge. 0.) 
cyy     1    print 102,chead(1:8),hdr(2),hdr(3),(arr(n,kl),n=1,10),
     1    Write(51,102)chead(1:8),hdr(2),hdr(3),(arr(n,kl),n=1,10),
     1    idate
          if(arr(1,kl) .gt. 1200. .or.  arr(1,kl) .lt. 0.) 
cj     1    print 192,chead(1:8),hdr(2),hdr(3),(arr(n,kl),n=1,10),
     1    Write(51,192)chead(1:8),hdr(2),hdr(3),(arr(n,kl),n=1,10),
     1    idate
 12       continue
 102      format(a8,1x,f6.2,1x,f6.2,1x,10(f10.2,1x),i10,' SNDP')
 192      format(a8,1x,f6.2,1x,f6.2,1x,10(f10.2,1x),i10,' SNDZ')
 10    continue
 101    format(10f8.2)
C
      CALL W3TAGE('RSONDE') 
        stop
        end
