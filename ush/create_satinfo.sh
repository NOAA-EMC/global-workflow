#! /usr/bin/env bash
# create global_satinfo file for a given date
date_in=$1
# header
echo '!sensor/instr/sat      chan iuse  error  error_cld  ermax   var_b    var_pg  icld_det icloud iaerosol'
# loop over satellites
cd "${BUILD_GSINFO_DIR}/satinfo" || exit 1
while IFS= read -r sat 
do
    # find matching date
    usedate=""
    for f in "${sat}"/*; do
        if [[ ${f} != "${sat}/readme" ]]; then # skip readme file
           datex=$(basename "${f}")
           if [[ ${date_in} -ge ${datex} ]]; then
              usedate=${datex}
           fi
        fi
    done
    # cat matching date file, or quit if date not found
    if [[ ${usedate} != "" ]]; then
        cat "${sat}/${usedate}" || exit 1
    else
        echo "date not found for ${sat}"
        exit 1
    fi
done < <(grep -v '^ *#' satellites || true)
