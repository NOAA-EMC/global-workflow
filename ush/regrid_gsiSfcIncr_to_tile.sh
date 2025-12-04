#! /usr/bin/env bash

source "${HOMEgfs}/ush/atparse.bash"

#-------------------------------------------------------------------------------------------------
# Script to regrid surface increment from GSI grid
# to fv3 tiles.
# Clara Draper, Dec 2024
# David New, Nov 2025 (parallelization updates)
#-------------------------------------------------------------------------------------------------

NMEM_REGRID=${NMEM_REGRID:-1}
CASE_IN=${CASE_IN:-${CASE_ENS}}
LFHR=${LFHR:-6}

# get resolutions
LONB_CASE_IN=$((4 * ${CASE_IN:1}))
LATB_CASE_IN=$((2 * ${CASE_IN:1}))

ntiles=6

APREFIX_ENS="enkfgdas.t${cyc}z."

LSOIL_INCR=${LSOIL_INCR:-2}

export n_vars=$((LSOIL_INCR * 2))

soilt_incr_vars=$(seq -s ',' -f '"soilt%g_inc"' 1 "${LSOIL_INCR}")
slc_incr_vars=$(seq -s ',' -f '"slc%g_inc"' 1 "${LSOIL_INCR}")
export soil_incr_vars="${soilt_incr_vars},${slc_incr_vars}"

if [[ "${DO_LAND_IAU}" = ".true." ]]; then
    IFS=',' read -ra landifhrs <<< "${IAUFHRS}"
fi
export in_fname="'sfci'"
export out_fname="'sfci'"
export in_dir=""
export dir_mask_in="'./'"
export fname_mask_in="'NULL'"
export ires=${LONB_CASE_IN}
export jres=${LATB_CASE_IN}
export ireso=${CASE_OUT:1}
export jreso=${CASE_OUT:1}

regrid_nml_tmpl="${PARMgfs}/regrid_sfc/regrid.nml_tmpl"

if [[ "${LFHR}" -ge 0 ]]; then
    soilinc_fhrs=("${LFHR}")
else                                     # construct restart times for deterministic member
    soilinc_fhrs=("${assim_freq}")       # increment file at middle of window
    if [[ "${DOIAU:-}" == "YES" ]]; then # Update surface restarts at beginning of window
        half_window=$((assim_freq / 2))
        soilinc_fhrs+=("${half_window}")
    fi
fi

# Stage input files

cd "${DATA}" || exit 1

# Create MDMD command file for fixed files
rm -f cmdfile_in.0
touch cmdfile_in.0
chmod 755 cmdfile_in.0

# Append fixed files command file to master command file
{
    echo "#!/bin/bash"

    # input, fixed files
    echo "cpreq ${FIXorog}/${CASE_IN}/gaussian.${LONB_CASE_IN}.${LATB_CASE_IN}.nc \
            ${DATA}/gaussian_scrip.nc"

    # output, fixed files
    echo "cpreq ${FIXorog}/${CASE_OUT}/${CASE_OUT}_mosaic.nc \
            ${DATA}/${CASE_OUT}_mosaic.nc"

    for n in $(seq 1 "${ntiles}"); do
        echo "cpreq ${FIXorog}/${CASE_OUT}/sfc/${CASE_OUT}.mx${OCNRES_OUT}.vegetation_type.tile${n}.nc \
                ${DATA}/vegetation_type.tile${n}.nc"
        echo "cpreq ${FIXorog}/${CASE_OUT}/${CASE_OUT}_grid.tile${n}.nc \
                ${DATA}/${CASE_OUT}_grid.tile${n}.nc"
    done
} >> cmdfile_in.0

if [[ "${NMEM_REGRID}" -gt 1 ]]; then

    echo "INFO: Preparing to regrid surface increments for ${NMEM_REGRID} ensemble members."
    for imem in $(seq 1 "${NMEM_REGRID}"); do

        memdir=$(printf "mem%03i" "${imem}")

        MEMDIR=${memdir} YMD=${PDY} HH=${cyc} declare_from_tmpl \
            COMIN_SOIL_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL \
            COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

        # Create MPMD command file for this member
        rm -f "cmdfile_in.${imem}" "cmdfile_out.${imem}"
        touch "cmdfile_in.${imem}" "cmdfile_out.${imem}"
        chmod 755 "cmdfile_in.${imem}" "cmdfile_out.${imem}"

        # Create commands to stage input files
        {
            echo "#!/bin/bash"

            echo "mkdir -p ${DATA}/${memdir}"

            for FHR in "${soilinc_fhrs[@]}"; do
                echo "cpreq ${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}increment.sfc.i00${FHR}.nc \
                            ${DATA}/${memdir}/sfci00${FHR}.nc"
            done

            if [[ "${DO_LAND_IAU}" = ".true." ]]; then
                for FHI in "${landifhrs[@]}"; do
                    echo "cpreq ${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}increment.sfc.i00${FHI}.nc \
                                ${DATA}/${memdir}/sfci00${FHI}.nc"
                done
            fi
        } >> "cmdfile_in.${imem}"

        # Create commands to copy output files
        {
            echo "#!/bin/bash"

            if [[ "${DO_LAND_IAU}" = ".false." || "${RUN}" == "gdas" || "${RUN}" == "gfs" ]]; then
                for FHR in "${soilinc_fhrs[@]}"; do
                    for n in $(seq 1 "${ntiles}"); do
                        echo "cpfs ${DATA}/${memdir}/sfci00${FHR}.mem${imem}.tile${n}.nc \
                              ${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.i00${FHR}.tile${n}.nc"
                    done
                done
            fi

            if [[ "${DO_LAND_IAU}" = ".true." ]]; then
                for n in $(seq 1 "${ntiles}"); do
                    echo "cpfs ${DATA}/${memdir}/sfci.mem${imem}.tile${n}.nc \
                          ${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.tile${n}.nc"
                done
            fi
        } >> "cmdfile_out.${imem}"

    done # for imem in $(seq 1 "${NMEM_REGRID}"); do
    in_dir=$(seq -s ", " -f "'./mem%03g/'" 1 "${NMEM_REGRID}")

else # deterministic member only (NMEM_REGRID=1)

    echo "INFO: Preparing to regrid surface increments for deterministic member."

    # Create commands to stage input files and append to the cmdfile.0
    {
        for FHR in "${soilinc_fhrs[@]}"; do
            echo "cpreq ${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}ensmean_increment.sfc.i00${FHR}.nc \
                        ${DATA}/sfci00${FHR}.nc"
        done

        if [[ "${DO_LAND_IAU}" = ".true." ]]; then
            for FHI in "${landifhrs[@]}"; do
                echo "cpreq ${COMIN_SOIL_ANALYSIS_MEM}/${APREFIX_ENS}ensmean_increment.sfc.i00${FHI}.nc \
                            ${DATA}/sfci00${FHI}.nc"
            done
        fi
    } >> "cmdfile_in.0"
    in_dir="'./'"

    # Create commands to copy output files
    rm -f cmdfile_out.0
    touch cmdfile_out.0
    chmod 755 cmdfile_out.0
    {
        echo "#!/bin/bash"

        if [[ "${DO_LAND_IAU}" = ".false." || "${RUN}" == "gdas" || "${RUN}" == "gfs" ]]; then
            for FHR in "${soilinc_fhrs[@]}"; do
                for n in $(seq 1 "${ntiles}"); do
                    echo "cpfs ${DATA}/sfci00${FHR}.mem1.tile${n}.nc \
                          ${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.i00${FHR}.tile${n}.nc"
                done
            done
        fi

        if [[ "${DO_LAND_IAU}" = ".true." ]]; then
            for n in $(seq 1 "${ntiles}"); do
                echo "cpfs ${DATA}/sfci.mem1.tile${n}.nc \
                      ${COMOUT_ATMOS_ANALYSIS_MEM}/increment.sfc.tile${n}.nc"
            done
        fi
    } >> "cmdfile_out.0"

fi

# Create master MPMD command files for input and output
rm -f cmdfile_in cmdfile_out
touch cmdfile_in cmdfile_out

# Append all members' command files to master command file
if [[ -f cmdfile_in.0 ]]; then
    echo "${DATA}/cmdfile_in.0" >> cmdfile_in
fi
if [[ -f cmdfile_out.0 ]]; then
    echo "${DATA}/cmdfile_out.0" >> cmdfile_out
fi
if [[ "${NMEM_REGRID}" -gt 1 ]]; then
    for imem in $(seq 1 "${NMEM_REGRID}"); do
        if [[ -f "cmdfile_in.${imem}" ]]; then
            echo "${DATA}/cmdfile_in.${imem}" >> cmdfile_in
        fi
        if [[ -f "cmdfile_out.${imem}" ]]; then
            echo "${DATA}/cmdfile_out.${imem}" >> cmdfile_out
        fi
    done
fi

# Run MPMD to stage input files
"${USHgfs}/run_mpmd.sh" "cmdfile_in" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "run_mpmd.sh failed to copy input and fix data!"
fi
mv mpmd.out mpmd_in.out

# Finish defining input/output directory list
export out_dir="${in_dir}"

# Regrid soil increments

# Increments for offline analysis
# If land IAU --> deterministic only. If no land IAU --> both deterministic and ensemble
if [[ "${DO_LAND_IAU}" = ".false." || "${RUN}" == "gdas" || "${RUN}" == "gfs" ]]; then
    for FHR in "${soilinc_fhrs[@]}"; do
        # Set namelist variables
        export add_time_dim=".false."
        export time_list="${FHR}"
        export out_fname="'sfci00${FHR}'"

        # Create regrid namelist
        rm -f "regrid.nml"
        atparse < "${regrid_nml_tmpl}" >> "regrid.nml"

        # Run regrid executable
        export pgm="${REGRID_EXEC}"
        ${APRUN_REGRID} "${REGRID_EXEC}"
        export err=$?
        if [[ ${err} -ne 0 ]]; then
            err_exit "${REGRID_EXEC} failed to regrid soil increments (without LANDIAU), ABORT!"
        fi
    done
fi

# Increments for forecast job with land IAU
# If land IAU --> deterministic and ensemble
if [[ "${DO_LAND_IAU}" = ".true." ]]; then
    # Set namelist variables
    export add_time_dim=".true."
    export time_list="${IAUFHRS}"
    export out_fname="'sfci'"

    # Create regrid namelist
    rm -f "regrid.nml"
    atparse < "${regrid_nml_tmpl}" >> "regrid.nml"

    # Run regrid executable
    export pgm="${REGRID_EXEC}"
    ${APRUN_REGRID} "${REGRID_EXEC}"
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "${pgm} failed to regrid soil increments (with LANDIAU), ABORT!"
    fi
fi

# Run MPMD to save output files
"${USHgfs}/run_mpmd.sh" "cmdfile_out" && true
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "run_mpmd.sh failed to copy output files to COMOUT, ABORT!"
fi
mv mpmd.out mpmd_out.out

exit 0
