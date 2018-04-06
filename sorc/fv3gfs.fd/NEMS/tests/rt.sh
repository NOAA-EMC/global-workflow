#! /bin/bash

for loc in . ./tests ./NEMS/tests ../tests ../NEMS/tests ; do
    if [[ -x "$loc/rtgen" ]] ; then
        cd $loc
        # Get the fully qualified path:
        real_loc=$( pwd -P )

        # Ensure our produtil is first in $PYTHONPATH
        export PYTHONPATH=$real_loc/produtil/ush${PYTHONPATH:+:$PYTHONPATH}

        # Run rtgen as "rt.sh" 
        exec $real_loc/rtgen --NEMSCompsetRun "$@"
    fi
done

echo "Cannot find rtgen!  Try running from NEMS/tests directory."
exit -1