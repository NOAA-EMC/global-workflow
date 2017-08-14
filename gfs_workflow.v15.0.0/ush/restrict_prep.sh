#!/bin/ksh

set -ax


PRPI=${PRPI:-$DATA/prepqc.$CDUMP.$CDATE}

export EXECobsproc_shared_bufr_remorest=${EXECobsproc_shared_bufr_remorest:-/gpfs/hps/emc/global/noscrub/emc.glopara/svn/obsproc/branches/obsproc_shared/bufr_remorest.tkt-367.crayport/exec}
REMX=${REMX:-$EXECobsproc_shared_bufr_remorest/bufr_remorest}
REMC=${REMC:-bufr_remorest.prepbufr.parm}

cat <<\EOFparm > bufr_remorest.prepbufr.parm
=========================================================================

  Cards for PREPBUFR Version of BUFR_REMOREST -- Version 15 March 2013

  -->   GPSIPW can be moved from MSG_RESTR to MSG_MIXED oncw dump interface to
         PREPDATA can recognize U.S.-provider (ENI) reports which are now
         restricted

 &SWITCHES
   MSG_RESTR = 'AIRCAR  ',   ! These are the Table A Entries for
               'MSONET  ',   !  BUFR messages for which ALL reports
               'GPSIPW  ',   !  are RESTRICTED and will be REMOVED.
               '        ',   !  (up to 20)
   MSG_MIXED = 'ADPSFC  ',   ! These are the Table A Entries for
               'AIRCFT  ',   !  BUFR messages which contain a MIXTURE
               '        ',   !  of restricted and unrestricted
               '        ',   !  reports (based on mnemonic "RSRD").  All
               '        ',   !  restricted reports will be REMOVED.
               '        ',   !  (up to 20)
   MSG_MASKA = 'SFCSHP  ',   ! These are the Table A Entries for
               '        ',   !  BUFR messages for which ALL reports
               '        ',   !  are RESTRICTED if their dump report type is
               '        ',   !  one of up to 10 possible listed in switch
               '        ',   !  IMASK_T29 below (each line in IMASK_T29 applies
               '        ',   !  to the Table A entry in the same line number
               '        ',   !  here). Restricted reports will not be removed,
               '        ',   !  but their report ids will be unilaterally
               '        ',   !  changed to "MASKSTID"
               '        ',   !  (up to 20)
               '        ' 
   IMASK_T29 = 522,523,8*99999, ! Dump report types restricted in MSG_MASKA(1)
               10*99999,        ! Dump report types restricted in MSG_MASKA(2)
               10*99999         ! etc., {up to 20 for MSG_MASKA(20)}

 /

    Note 1: A particular Table A entry should NEVER appear in more than one
            of MSG_RESTR, MSG_MIXED or MSG_MASKA.
    Note 2: Any Table A entry not in either MSG_RESTR, MSG_MIXED or MSG_MASKA
            is assumed to be a Table A entry for BUFR messages for which
            ALL reports are UNRESTRICTED (these messages are copied
            intact, no reports are unpacked).
    Note 3: Always fill in the arrays MSG_RESTR, MSG_MIXED and MSG_MASKA
            beginning with word 1.  If there are less than 20 words filled in
            an array, either set the extra words to "        " (8 blank
            characters) or do not specify them here (they default to
            "        ").
    Note 4: In array IMASK_T29, a value of "99999" means not applicable whereas
            a value of "000" means reports in all dump report types in the
            corresponding Table A entry in MSG_MASKA should be restricted
            (masked) {in this case IMASK_T29(1,x) would be set to 000 and
            IMASK_T29(2:10,x) would be set to 99999 for all reports in Table A
            entry MSG_MASKA(x) since they would all be ignored - this is the
            default for all Table A entries MSG_MASKA(1:20) if this is not set
            (i.e., for data dump files)}

=========================================================================
EOFparm

echo $PRPI > filename
export FORT11=filename
export FORT21=$PRPI
export FORT51=$PRPI.unrestricted
time -p $REMX< $REMC > outout 2> errfile
err=$?
cat errfile >> outout
cat outout >> remorest.out
