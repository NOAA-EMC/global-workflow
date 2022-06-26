#!/bin/bash

DATM_namelists(){
MESH_ATM=${datm_src}"_"mesh.nc

cat > datm_in <<eof
&datm_nml
  datamode = "${atm_datamode}"
  factorfn_data = "null"
  factorfn_mesh = "null"
  flds_co2 = .false.
  flds_presaero = .false.
  flds_wiso = .false.
  iradsw = 1
  model_maskfile = "DATM_INPUT/${MESH_ATM}"
  model_meshfile = "DATM_INPUT/${MESH_ATM}"
  nx_global = ${ATM_NX_GLB}
  ny_global = ${ATM_NY_GLB}
  restfilm = "null"
/
eof

cat > datm.streams <<eof
stream_info:               ${datm_src}01
taxmode01:                 limit
mapalgo01:                 bilinear
tInterpAlgo01:             linear
readMode01:                single
dtlimit01:                 1.0
stream_offset01:           0
yearFirst01:               $SYEAR
yearLast01:                $SYEAR
yearAlign01:               $SYEAR
stream_vectors01:          "u:v"
stream_mesh_file01:        DATM_INPUT/${MESH_ATM}
stream_lev_dimname01:      null
stream_data_files01:       DATM_INPUT/${datm_src}.${SYEAR}${SMONTH}.nc
stream_data_variables01:  "slmsksfc Sa_mask" "DSWRF Faxa_swdn" "DLWRF Faxa_lwdn" "vbdsf_ave Faxa_swvdr" "vddsf_ave Faxa_swvdf" "nbdsf_ave Faxa_swndr" "nddsf_ave Faxa_swndf" "u10m Sa_u10m" "v10m Sa_v10m" "hgt_hyblev1 Sa_z" "psurf Sa_pslv" "tmp_hyblev1 Sa_tbot" "spfh_hyblev1 Sa_shum" "ugrd_hyblev1 Sa_u" "vgrd_hyblev1 Sa_v" "q2m Sa_q2m" "t2m Sa_t2m" "pres_hyblev1 Sa_pbot" "precp Faxa_rain" "fprecp Faxa_snow"
eof

#diag_table
DIAG_TABLE=$PARM_FV3DIAG/diag_table_cpl
cat > diag_table << EOF
Datm forecast
${SYEAR} ${SMONTH} ${SDAY} ${SHOUR} 0 0
EOF
cat $DIAG_TABLE >> diag_table
cat > input.nml <<EOF
&fms_nml
            clock_grain='ROUTINE'
            clock_flags='NONE'
            domains_stack_size = 5000000
            stack_size =0
/
EOF
}
