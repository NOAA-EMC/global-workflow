   #!/bin/sh
   #
   # usage $0 output (list of gfs grib2 forecast files in order)
   #
   C=":APCP:surface:"
   D=":ACPCP:surface:"
   F=":NCPCP:surface:"
   output=$1
   shift 1
   cat $* | wgrib2 - -match "($C|$D|$F)" -set_grib_type c3 \
      -if "$C" -ncep_norm $output \
      -if "$D" -ncep_norm $output \
      -if "$F" -ncep_norm $output
