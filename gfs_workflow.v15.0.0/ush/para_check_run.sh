#!/bin/ksh
set -x

export TMPDIR=$DATAMAIL
rm -rf $TMPDIR
mkdir -p $TMPDIR
cd $TMPDIR

export PARA_CHECKSH=${PARA_CHECKSH:-$USHDIR/para_check_status.sh}
export PARA_CHECK_MAIL=${PARA_CHECK_MAIL:-$LOGNAME}
export machine=${machine:-WCOSS}

$PARA_CHECKSH $PSLOT $CDATE $CDUMP > temp.msg

# If mail command below is executed on WCOSS_C, a blank email 
# is sent.   WCOSS HelpDesk ticket never found reason for this 
# behavior.   Full body email sent on WCOSS_C only when mail 
# command executed from job running this script.  The mail 
# command below works when  executed on other machines.  Hence 
# the logic below to only execute mail commnd in this script
# on non WCOSS_C machines.

if [ $machine != WCOSS_C ] ; then
 mail -s "$PSLOT $CDATE $CDUMP status" $PARA_CHECK_MAIL < temp.msg
fi

exit




