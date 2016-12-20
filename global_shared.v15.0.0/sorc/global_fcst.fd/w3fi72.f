      subroutine w3fi72(itype,fld,ifld,ibitl,
     &                  ipflag,id,pds,
     &                  igflag,igrid,igds,icomp,
     &                  ibflag,ibmap,iblen,ibdsfl,
     &                  npts,kbuf,itot,jerr)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:  w3fi72        make a complete grib message
c   prgmmr: farley           org: nmc421      date:94-11-22
c
c abstract: makes a complete grib message from a user supplied
c   array of floating point or integer data.  the user has the
c   option of supplying the pds or an integer array that will be
c   used to create a pds (with w3fi68).  the user must also
c   supply other necessary info; see usage section below.
c
c program history log:
c   91-05-08  r.e.jones
c   92-07-01  m. farley    added gds and bms logic.  placed existing
c                          logic for bds in a routine.
c   92-10-02  r.e.jones    add error exit for w3fi73
c   93-04-30  r.e.jones    replace do loops to move character data
c                          with xmovex, use xstore to zero character
c                          array. make change so flat field will pack.
c   93-08-06  cavanaugh    modified call to w3fi75
c   93-10-26  cavanaugh    added code to restore input field to original
c                          values if d-scale not 0
c   94-01-27  cavanaugh    added igds array in call to w3fi75 to provide
c                          information for boustrophedonic processing
c   94-03-03  cavanaugh    increased size of gds array for thin grids
c   94-05-16  farley       cleaned up documentation
c   94-11-10  farley       increased size of pfld/ifld arrarys from
c                          100k to 260k for .5 degree sst anal fields
c   94-12-04  r.e.jones    change document for ipflag.
c   95-10-31  iredell      removed saves and prints
c   98-05-19  gilbert      increased array dimensions to handle grids
c                          of up to 500,000 grid points.
c   95-10-31  iredell      generalized word size
c   98-12-21  gilbert      replaced function ichar with mova2i.
c   99-02-01  gilbert      changed the method of zeroing out array kbuf.
c                          the old method, using w3fi01 and xstore was
c                          incorrect with 4-byte integers and 8-byte reals.
c
c usage:  call w3fi72(itype,fld,ifld,ibitl,
c        &            ipflag,id,pds,
c        &            igflag,igrid,igds,icomp,
c        &            ibflag,ibmap,iblen,ibdsfl,
c        &            ibdsfl,
c        &            npts,kbuf,itot,jerr)
c
c   input argument list:
c     itype    - 0 = floating point data supplied in array 'fld'
c                1 = integer data supplied in array 'ifld'
c     fld      - real array of data (at proper gridpoints) to be
c                converted to grib format if itype=0.
c                see remarks #1 & 2.
c     ifld     - integer array of data (at proper gridpoints) to be
c                converted to grib format if itype=1.
c                see remarks #1 & 2.
c     ibitl    - 0 = computer computes length for packing data from
c                    power of 2 (number of bits) best fit of data
c                    using 'variable' bit packer w3fi58.
c                8, 12, etc. computer rescales data to fit into that
c                    'fixed' number of bits using w3fi59.
c                see remarks #3.
c
c     ipflag   - 0 = make pds from user supplied array (id)
c                1 = user supplying pds
c                note: if pds is greater than 30, use iplfag=1.
c                the user could call w3fi68 before he calls
c                w3fi72. this would make the first 30 bytes of
c                the pds, user then would make bytes after 30.
c     id       - integer array of  values that w3fi68 will use
c                to make an edition 1 pds if ipflag=0.  (see the
c                docblock for w3fi68 for layout of array)
c     pds      - character array of values (valid pds supplied
c                by user) if ipflag=1. length may exceed 28 bytes
c                (contents of bytes beyond 28 are passed
c                through unchanged).
c
c     igflag   - 0 = make gds based on 'igrid' value.
c                1 = make gds from user supplied info in 'igds'
c                    and 'igrid' value.
c                see remarks #4.
c     igrid    - #   = grid identification (table b)
c                255 = if user defined grid; igds must be supplied
c                      and igflag must =1.
c     igds     - integer array containing user gds info (same
c                format as supplied by w3fi71 - see dockblock for
c                layout) if igflag=1.
c     icomp    - resolution and component flag for bit 5 of gds(17)
c                0 = earth oriented winds
c                1 = grid oriented winds
c
c     ibflag   - 0 = make bit map from user supplied data
c                # = bit map predefined by center
c                see remarks #5.
c     ibmap    - integer array containing bit map
c     iblen    - length of bit map will be used to verify length
c                of field (error if it doesn't match).
c
c     ibdsfl   - integer array containing table 11 flag info
c                bds octet 4:
c                (1) 0 = grid point data
c                    1 = spherical harmonic coefficients
c                (2) 0 = simple packing
c                    1 = second order packing
c                (3) ... same value as 'itype'
c                    0 = original data were floating point values
c                    1 = original data were integer values
c                (4) 0 = no additional flags at octet 14
c                    1 = octet 14 contains flag bits 5-12
c                (5) 0 = reserved - always set to 0
c         byte 6 option 1 not available (as of 5-16-93)
c                (6) 0 = single datum at each grid point
c                    1 = matrix of values at each grid point
c         byte 7 option 0 with second order packing n/a (as of 5-16-93)
c                (7) 0 = no secondary bit maps
c                    1 = secondary bit maps present
c                (8) 0 = second order values have constant width
c                    1 = second order values have different widths
c
c   output argument list:
c     npts     - number of gridpoints in array fld or ifld
c     kbuf     - entire grib message ('grib' to '7777')
c                equivalence to integer array to make sure it
c                is on word bounary.
c     itot     - total length of grib message in bytes
c     jerr     - = 0, completed making grib field without error
c                  1, ipflag not 0 or 1
c                  2, igflag not 0 or 1
c                  3, error converting ieee f.p. number to ibm370 f.p.
c                  4, w3fi71 error/igrid not defined
c                  5, w3fk74 error/grid representation type not valid
c                  6, grid too large for packer dimension arrays
c                     see automation division for revision!
c                  7, length of bit map not equal to size of fld/ifld
c                  8, w3fi73 error, all values in ibmap are zero
c
c   output files:
c     ft06f001 - standard fortran output print file
c
c   subprograms called:
c     library:
c       w3lib    - w3fi58, w3fi59, w3fi68, w3fi71, w3fi73, w3fi74
c                  w3fi75, w3fi76
c       fortran 90 intrinsic - bit_size
c
c remarks:
c   1)  if bit map to be included in message, null data should
c       be included in fld or ifld.  this routine will take care
c       of 'discarding' any null data based on the bit map.
c   2)  units must be those in grib documentation:  nmc o.n. 388
c       or wmo publication 306.
c   3)  in either case, input numbers will be multiplied by
c       '10 to the nth' power found in id(25) or pds(27-28),
c       the d-scaling factor, prior to binary packing.
c   4)  all nmc produced grib fields will have a grid definition
c       section included in the grib message.  id(6) will be
c       set to '1'.
c       - gds will be built based on grid number (igrid), unless
c         igflag=1 (user supplying igds).  user must still supply
c         igrid even if igds provided.
c   5)  if bit map used then id(7) or pds(8) must indicate the
c       presence of a bit map.
c   6)  array kbuf should be equivalenced to an integer value or
c       array to make sure it is on a word boundary.
c   7)  subprogram can be called from a multiprocessing environment.
c
c attributes:
c   language: fortran 90
c
c$$$
c
      real            fld(*)
c
      integer         ibdsfl(*)
      integer         ibmap(*)
      integer         id(*)
      integer         ifld(*)
      integer         igds(*)
      integer,allocatable:: ipfld(:)
      integer         ib(4)
c
      character * 1   bds11(11)
      character * 1   kbuf(*)
      character * 1   pds(*)
      character * 1   gds(200)
      character(1),allocatable:: bms(:)
      character(1),allocatable:: pfld(:)
      character * 1   seven
      character * 1   zero
c
c
c   ascii rep of  /'G', 'R', 'I', 'B'/
c
      data  ib    / 71,  82,  73,  66/
c
      ier    = 0
      iberr  = 0
      jerr   = 0
      igribl = 8
      ipdsl  = 0
      lengds = 0
      lenbms = 0
      lenbds = 0
      itoss  = 0
c
c$           1.0   product definition section(pds).
c
c   set id(6) to 1 ...or... modify pds(8) ...
c      regardless of user specification...
c   nmc grib fields will always have a gds
c
      if (ipflag .eq.0) then
        id(6) = 1
        call w3fi68(id,pds)
      else if (ipflag .eq. 1) then
        if (iand(mova2i(pds(8)),64) .eq. 64) then
c         both gds and bms
          pds(8) = char(192)
        else if (mova2i(pds(8)) .eq. 0) then
c         gds only
          pds(8) = char(128)
        end if
        continue
      else
c       print *,' w3fi72 error, ipflag is not 0 or 1 ipflag = ',ipflag
        jerr = 1
        go to 900
      end if
c
c     get length of pds
c
      ipdsl = mova2i(pds(1)) * 65536 + mova2i(pds(2)) * 256 +
     &        mova2i(pds(3))
c
c$           2.0   grid definition section (gds).
c
c     if igflag=1 then user is supplying the igds information
c
      if (igflag .eq. 0) then
        call w3fi71(igrid,igds,igerr)
        if (igerr .eq. 1) then
c         print *,' w3fi71 error, grid type not defined...',igrid
          jerr = 4
          go to 900
        end if
      end if
      if (igflag .eq. 0  .or.  igflag .eq.1) then
        call w3fi74(igds,icomp,gds,lengds,npts,igerr)
        if (igerr .eq. 1) then
c         print *,' w3fi74 error, grid rep type not valid...',igds(3)
          jerr = 5
          go to 900
        else
        end if
      else
c       print *,' w3fi72 error, igflag is not 0 or 1 igflag = ',igflag
        jerr = 2
        go to 900
      end if
c
c$           3.0   bit map section (bms).
c
c     set itoss=1 if bitmap being used.  w3fi75 will toss data
c     prior to packing.  later coding will be needed when the
c     'predefined' grids are finally 'defined'.
c
      if (mova2i(pds(8)) .eq. 64 .or.
     &    mova2i(pds(8)) .eq. 192)   then
        itoss = 1
        if (ibflag .eq. 0) then
          if (iblen .ne. npts) then
c           print *,' w3fi72 error, iblen .ne. npts = ',iblen,npts
            jerr = 7
            go to 900
          end if
          allocate(bms(npts/8+6))
          call w3fi73(ibflag,ibmap,iblen,bms,lenbms,ier)
          if (ier .ne. 0) then
c           print *,' w3fi73 error, ibmap values are all zero'
            jerr = 8
            go to 900
          end if
        else
c         print *,'   bit map predefined by center, ibflag = ',ibflag
        end if
      end if
c
c$           4.0   binary data section (bds).
c
c$           4.1   scale the data with d-scale from pds(27-28)
c
      jscale = mova2i(pds(27)) * 256 + mova2i(pds(28))
      if (iand(jscale,32768).ne.0) then
        jscale = - iand(jscale,32767)
      end if
      scale  = 10.0 ** jscale
      if (itype .eq. 0) then
        do 410 i = 1,npts
          fld(i) = fld(i) * scale
  410   continue
      else
        do 411 i = 1,npts
          ifld(i) = nint(float(ifld(i)) * scale)
  411   continue
      end if
c
c$           4.2   call w3fi75 to pack data and make bds.
c
      allocate(pfld(npts*4))
c
      if(ibdsfl(2).ne.0) then
        allocate(ipfld(npts*32/bit_size(1)+1))
        ipfld=0
      endif
c
      call w3fi75(ibitl,itype,itoss,fld,ifld,ibmap,ibdsfl,
     &         npts,bds11,ipfld,pfld,len,lenbds,iberr,pds,igds)
c
      if(ibdsfl(2).ne.0) then
        call xmovex(pfld,ipfld,npts*4)
        deallocate(ipfld)
      endif
c
        if (iberr .eq. 1) then
          jerr = 3
          go to 900
        end if
c            4.3   if d-scale not 0, rescale input field to
c                   original value
c
      if (jscale.ne.0) then
          dscale = 1.0 / scale
          if (itype.eq.0) then
              do 412 i = 1, npts
                  fld(i)  = fld(i) * dscale
  412         continue
          else
              do 413 i = 1, npts
                  fld(i)  = nint(float(ifld(i)) * dscale)
  413         continue
          end if
      end if
c
c$           5.0   output section.
c
c$           5.1   zero out the output array kbuf.
c
      zero    = char(00)
      itot    = igribl + ipdsl + lengds + lenbms + lenbds + 4
c     print *,'igribl  =',igribl
c     print *,'ipdsl   =',ipdsl
c     print *,'lengds  =',lengds
c     print *,'lenbms  =',lenbms
c     print *,'lenbds  =',lenbds
c     print *,'itot    =',itot
      kbuf(1:itot)=zero
c
c$           5.2   move section 0 - 'is' into kbuf (8 bytes).
c
      istart  = 0
      do 520 i = 1,4
        kbuf(i) = char(ib(i))
  520 continue
c
      kbuf(5) = char(mod(itot / 65536,256))
      kbuf(6) = char(mod(itot /   256,256))
      kbuf(7) = char(mod(itot        ,256))
      kbuf(8) = char(1)
c
c$           5.3   move section 1 - 'pds' into kbuf (28 bytes).
c
      istart  = istart + igribl
      if (ipdsl.gt.0) then
        call xmovex(kbuf(istart+1),pds,ipdsl)
      else
c       print *,'length of pds less or equal 0, ipdsl = ',ipdsl
      end if
c
c$           5.4   move section 2 - 'gds' into kbuf.
c
      istart  = istart + ipdsl
      if (lengds .gt. 0) then
        call xmovex(kbuf(istart+1),gds,lengds)
      end if
c
c$           5.5   move section 3 - 'bms' into kbuf.
c
      istart  = istart + lengds
      if (lenbms .gt. 0) then
        call xmovex(kbuf(istart+1),bms,lenbms)
      end if
c
c$           5.6   move section 4 - 'bds' into kbuf.
c
c$                 move the first 11 octets of the bds into kbuf.
c
      istart  = istart + lenbms
      call xmovex(kbuf(istart+1),bds11,11)
c
c$                 move the packed data into the kbuf
c
      istart  = istart + 11
      if (len.gt.0) then
        call xmovex(kbuf(istart+1),pfld,len)
      end if
c
c$                 add '7777' to end off kbuf
c   note that these 4 octets not included in actual size of bds.
c
      seven  = char(55)
      istart = itot - 4
      do 562 i = 1,4
        kbuf(istart+i) = seven
 562  continue
c
 900  continue
      if(allocated(bms)) deallocate(bms)
      if(allocated(pfld)) deallocate(pfld)
      return
      end
