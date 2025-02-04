# create global_satinfo file for a given date
date=$1
# header
echo '!sensor/instr/sat      chan iuse  error  error_cld  ermax   var_b    var_pg  icld_det icloud iaerosol'
# loop over satellites
cd ${FIXgfs}/gsi/build_gsinfo/satinfo
for sat in $(cat satellites); do
    # find matching date
    usedate=""
    for f in $sat/*; do
        if [ $f != "$sat/readme" ]; then # skip readme file
           datex=`basename $f`
	   if [ $date -ge $datex ]; then
              usedate=$datex
           fi
        fi
    done
    # cat matching date file, or quit if date not found
    if [ $usedate != "" ]; then
        cat $sat/$usedate
    else
        echo "date not found for $sat"
	exit 1
    fi
done
