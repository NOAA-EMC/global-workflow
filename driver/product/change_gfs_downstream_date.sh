set -x

# export cyc=12
# export cyc=18
export fhr=012
export dir=` pwd `
export PDY=`date -u +%Y%m%d`
export PDY1=`expr $PDY - 1`

export olddate=20200712
export newdate=20200922

export gdas=/gpfs/dell1/nco/ops/com/gfs/prod/gdas.${PDY}/${cyc}
export gdasgp=/gpfs/dell1/nco/ops/com/gfs/prod/gdas.${PDY}/${cyc}/gempak
export gdasmeta=/gpfs/dell1/nco/ops/com/gfs/prod/gdas.${PDY}/${cyc}/gempak/meta

export gdastest=/gpfs/dell2/ptmp/Boi.Vuong/com/gfs/prod/gdas.${PDY}/${cyc}
export gdastestgp=/gpfs/dell2/ptmp/Boi.Vuong/com/gfs/prod/gdas.${PDY}/${cyc}/gempak
export gdastestmeta=/gpfs/dell2/ptmp/Boi.Vuong/com/gfs/prod/gdas.${PDY}/${cyc}/gempak/meta

for cyc in 00 06 12 18
# for cyc in 00
do
sed -i "s/${olddate}/${newdate}/g"  run_JGDAS_ATMOS_GEMPAK_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGDAS_ATMOS_GEMPAK_META_NCDC_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_GEMPAK_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_AWIPS_20KM_1P0DEG_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_AWIPS_G2_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_FBWIND_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_GEMPAK_META_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_PGRB2_SPEC_NPOESS_dell.sh_${cyc}
sed -i "s/${olddate}/${newdate}/g"  run_JGFS_ATMOS_GEMPAK_PGRB2_SPEC_dell.sh_${cyc}

sed -i s/envir=prod/envir=para/g  run_JGDAS_ATMOS_GEMPAK_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGDAS_ATMOS_GEMPAK_META_NCDC_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_GEMPAK_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_AWIPS_20KM_1P0DEG_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_AWIPS_G2_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_FBWIND_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_GEMPAK_META_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_PGRB2_SPEC_NPOESS_dell.sh_${cyc}
sed -i s/envir=prod/envir=para/g  run_JGFS_ATMOS_GEMPAK_PGRB2_SPEC_dell.sh_${cyc}

done
