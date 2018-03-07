      subroutine mkfldsep(csep,iopt,lenin,lenbull,lenout)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    mkfldsep    Makes TOC Flag Field Separator Block
C   PRGMMR: Gilbert           ORG: W/NP11     DATE: 2002-09-16
C
C ABSTRACT: Generates a TOC Flag Field Separator Block used to separate
C   WMO Bulletins within a transmission file to be ingested in TOC's
C   FTP Input Service, which can be used to disseminate WMO buletins.  
C   ( see http://weather.gov/tg/ftpingest.html )
C
C   This routine can generate different flag field separator blocks
C   depending on the value of variable iopt.
C
C   Bulletin "Flag Field Separator" block - OPTION 1 (old)
C      bytes  1 - 4          marker string (####)
C             5 - 7          block length [018 fixed value]
C             8 - 13         total length of bulletin in bytes [octets]
C                            (not including the flag field block)
C             14 - 17        marker string (####)
C             18             line Feed (ASCII "0A")
C   
C   Bulletin "Flag Field Separator" block - OPTION 1a (new)
C      bytes  1 - 4          marker string (####)
C             5 - 7          block length (nnn) - value always greater than 018
C             8 - 18         total length of bulletin in bytes [octets]
C                            (not including the flag field block)
C             19 - nnn-5     reserved for future use
C             nnn-4 - nnn-1  marker string (####)
C             nnn            line Feed (ASCII "0A")
C
C   Bulletin "Flag Field Separator" block - OPTION 2 (limited)
C      bytes  1 - 4          marker string (****)
C             5 - 14         total length of bulletin in bytes [octets]
C                            (not including the flag field block)
C             15 - 18        marker string (****)
C             19             line Feed (ASCII "0A")
C
C
C PROGRAM HISTORY LOG:
C 2002-09-16  Gilbert      ORIGINAL AUTHOR
C
C USAGE:    call mkfldsep(csep,iopt,lenin,lenbull,lenout)
C   INPUT ARGUMENT LIST:
C     iopt        Flag Field Separator block option:
C                 = 1: Separator block for use with alphanumeric bulletins.
C                      if lenin <= 18 and lenbull <= 999999, 
C                          OPTION 1 block will be generated.
C                      if lenin > 18 or lenbull > 999999, 
C                          OPTION 1a block will be generated.
C                 = 2: Separator block for use with GRIB/BUFR bulletins.
C     lenin       Desired length of the flag field separator block.
C                 ignored, if iopt=2.
C     lenbull     Integer length of the bulletin (in bytes) that will follow
C                 this separator block.
C
C   OUTPUT ARGUMENT LIST: 
C     csep*(*)    Character array containing the flag field separator.
C     lenout      Integer length of the flag field separator block.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM/SP
C
C$$$
C
      character*(*),intent(out) :: csep
      integer,intent(in) :: iopt,lenin,lenbull
      integer,intent(out) :: lenout
C
      character(len=4),parameter :: cstar='****',clb='####'
C
      if (iopt.eq.1) then
         if ( lenin .le. 18 .and. lenbull .le. 999999 ) then 
                                      ! Create OPTION 1 separator block
            csep(1:4)=clb
            csep(5:7)='018'
            write(csep(8:13),fmt='(I6.6)') lenbull
            csep(14:17)=clb
            csep(18:18)=char(10)
            lenout=18
         else                         ! Create OPTION 1a separator block
            nnn=lenin
            if ( nnn.lt.23 ) nnn=23
            csep(1:4)=clb
            write(csep(5:7),fmt='(I3.3)') nnn
            write(csep(8:18),fmt='(I11.11)') lenbull
            csep(19:nnn-5)='0'
            csep(nnn-4:nnn-1)=clb
            csep(nnn:nnn)=char(10)
            lenout=nnn
         endif
      elseif (iopt.eq.2) then         !  Create OPTION 2 separator block
         csep(1:4)=cstar
         write(csep(5:14),fmt='(I10.10)') lenbull
         csep(15:18)=cstar
         csep(19:19)=char(10)
         lenout=19
      else
         print *,"mkfldsep: Option ",iopt," not recognized."
         csep(1:lenin)=' '
      endif
C
      return
      end
