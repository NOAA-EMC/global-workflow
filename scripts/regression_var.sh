# It is now possible to run all regression tests (except RTMA) using the hybrid ensemble option with
#  internally generated random ensemble perturbations.  No script changes are required.
#  To run with hybrid ensemble option on, change HYBENS_GLOBAL and/or HYBENS_REGIONAL from "false" to "true".
#  These are located at the end of this script.

# Specify machine that is being used (Zeus or WCOSS)

platform=$REMOTEHOST
if [ -d /da ]; then
#For WCOSS
   export machine="WCOSS"
elif [ -d /scratch1/portfolios/NCEPDEV/da ]; then
   if [ `expr substr $platform 1 4` = "zeus" ]; then
#For Zeus
      export machine="Zeus"
   elif [ `expr substr $platform 1 5` = "theia" ]; then
#For Theia
      export machine="Theia"
   fi
fi

# Variables with the same values are defined below.

export global_T62_adate=2014080400
export global_4dvar_T62_adate=2014080400
export global_hybrid_T126_adate=2014092912
export global_enkf_T62_adate=2014092912
export global_lanczos_T62_adate=2014080400
export global_nemsio_T62_adate=2013011400
export nmmb_nems_adate=2009031600
export arw_binary_adate=2010072412
export arw_netcdf_adate=2008051112
export nmm_binary_adate=2010021600
export nmm_netcdf_adate=2007122000
export rtma_adate=2015030712
export hwrf_nmm_adate=2012102812
export JCAP=62

# Set predefined paths and variables here.
# Note that experiment name is same as that entered in -j option below.

if [ "$machine" = "Zeus" -o "$machine" = "Theia" ]; then

#  First, experiment names.

   export global_T62_updat_exp1=global_T${JCAP}_loproc_updat
   export global_T62_updat_exp2=global_T${JCAP}_hiproc_updat
   export global_T62_contrl_exp1=global_T${JCAP}_loproc_contrl
   export global_T62_contrl_exp2=global_T${JCAP}_hiproc_contrl
   export global_T62_ozonly_updat_exp1=global_T${JCAP}_loproc_ozonly_updat
   export global_T62_ozonly_updat_exp2=global_T${JCAP}_hiproc_ozonly_updat
   export global_T62_ozonly_contrl_exp1=global_T${JCAP}_loproc_ozonly_contrl
   export global_T62_ozonly_contrl_exp2=global_T${JCAP}_hiproc_ozonly_contrl
   export global_4dvar_T62_updat_exp1=global_4dvar_T${JCAP}_loproc_updat
   export global_4dvar_T62_updat_exp2=global_4dvar_T${JCAP}_hiproc_updat
   export global_4dvar_T62_contrl_exp1=global_4dvar_T${JCAP}_loproc_contrl
   export global_4dvar_T62_contrl_exp2=global_4dvar_T${JCAP}_hiproc_contrl
   export global_hybrid_T126_updat_exp1=global_hybrid_loproc_updat
   export global_hybrid_T126_updat_exp2=global_hybrid_hiproc_updat
   export global_hybrid_T126_contrl_exp1=global_hybrid_loproc_contrl
   export global_hybrid_T126_contrl_exp2=global_hybrid_hiproc_contrl
   export global_enkf_T62_updat_exp1=global_enkf_loproc_updat
   export global_enkf_T62_updat_exp2=global_enkf_hiproc_updat
   export global_enkf_T62_contrl_exp1=global_enkf_loproc_contrl
   export global_enkf_T62_contrl_exp2=global_enkf_hiproc_contrl
   export global_lanczos_T62_updat_exp1=global_lanczos_T${JCAP}_loproc_updat
   export global_lanczos_T62_updat_exp2=global_lanczos_T${JCAP}_hiproc_updat
   export global_lanczos_T62_contrl_exp1=global_lanczos_T${JCAP}_loproc_contrl
   export global_lanczos_T62_contrl_exp2=global_lanczos_T${JCAP}_hiproc_contrl
   export global_nemsio_T62_updat_exp1=global_nemsio_T${JCAP}_loproc_updat
   export global_nemsio_T62_updat_exp2=global_nemsio_T${JCAP}_hiproc_updat
   export global_nemsio_T62_contrl_exp1=global_nemsio_T${JCAP}_loproc_contrl
   export global_nemsio_T62_contrl_exp2=global_nemsio_T${JCAP}_hiproc_contrl
   export nmmb_nems_updat_exp1=nmmb_nems_loproc_updat
   export nmmb_nems_updat_exp2=nmmb_nems_hiproc_updat
   export nmmb_nems_contrl_exp1=nmmb_nems_loproc_contrl
   export nmmb_nems_contrl_exp2=nmmb_nems_hiproc_contrl
   export arw_binary_updat_exp1=arw_binary_loproc_updat
   export arw_binary_updat_exp2=arw_binary_hiproc_updat
   export arw_binary_contrl_exp1=arw_binary_loproc_contrl
   export arw_binary_contrl_exp2=arw_binary_hiproc_contrl
   export arw_netcdf_updat_exp1=arw_netcdf_loproc_updat
   export arw_netcdf_updat_exp2=arw_netcdf_hiproc_updat
   export arw_netcdf_contrl_exp1=arw_netcdf_loproc_contrl
   export arw_netcdf_contrl_exp2=arw_netcdf_hiproc_contrl
   export nmm_binary_updat_exp1=nmm_binary_loproc_updat
   export nmm_binary_updat_exp2=nmm_binary_hiproc_updat
   export nmm_binary_contrl_exp1=nmm_binary_loproc_contrl
   export nmm_binary_contrl_exp2=nmm_binary_hiproc_contrl
   export nmm_netcdf_updat_exp1=nmm_netcdf_loproc_updat
   export nmm_netcdf_updat_exp2=nmm_netcdf_hiproc_updat
   export nmm_netcdf_contrl_exp1=nmm_netcdf_loproc_contrl
   export nmm_netcdf_contrl_exp2=nmm_netcdf_hiproc_contrl
   export rtma_updat_exp1=rtma_loproc_updat
   export rtma_updat_exp2=rtma_hiproc_updat
   export rtma_contrl_exp1=rtma_loproc_contrl
   export rtma_contrl_exp2=rtma_hiproc_contrl
   export hwrf_nmm_d2_updat_exp1=hwrf_nmm_d2_loproc_updat
   export hwrf_nmm_d2_updat_exp2=hwrf_nmm_d2_hiproc_updat
   export hwrf_nmm_d2_contrl_exp1=hwrf_nmm_d2_loproc_contrl
   export hwrf_nmm_d2_contrl_exp2=hwrf_nmm_d2_hiproc_contrl
   export hwrf_nmm_d3_updat_exp1=hwrf_nmm_d3_loproc_updat
   export hwrf_nmm_d3_updat_exp2=hwrf_nmm_d3_hiproc_updat
   export hwrf_nmm_d3_contrl_exp1=hwrf_nmm_d3_loproc_contrl
   export hwrf_nmm_d3_contrl_exp2=hwrf_nmm_d3_hiproc_contrl

#  Next, paths for experiment and control executables,
#  fix, ptmp, and CRTM coefficient files.

   export group=global
   export queue=batch
   if [[ "$machine" = "Zeus" ]]; then
      export basedir=/scratch1/portfolios/NCEPDEV/da/save/$LOGNAME
      export gsisrc=$basedir/EXP-testCRTM_R2.2/src
      export gsiexec_updat=$basedir/EXP-testCRTM_R2.2/src/global_gsi
      export gsiexec_contrl=$basedir/svn1/src/global_gsi
      export enkfexec_updat=$basedir/EXP-testCRTM_R2.2/src/enkf/global_enkf
      export enkfexec_contrl=$basedir/svn1/src/enkf/global_enkf
      export fixgsi=$basedir/EXP-testCRTM_R2.2/fix
      export scripts=$basedir/EXP-testCRTM_R2.2/scripts
      export fixcrtm=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/CRTM_REL-2.2.1/crtm_v2.2.1/fix
   elif [[ "$machine" = "Theia" ]]; then
      export basedir=/scratch4/NCEPDEV/da/save/$LOGNAME
      export gsisrc=$basedir/EXP-testCRTM_R2.2/src
      export gsiexec_updat=$basedir/EXP-testCRTM_R2.2/src/global_gsi
      export gsiexec_contrl=$basedir/svn1/src/global_gsi
      export enkfexec_updat=$basedir/EXP-testCRTM_R2.2/src/enkf/global_enkf
      export enkfexec_contrl=$basedir/svn1/src/enkf/global_enkf
      export fixgsi=$basedir/EXP-testCRTM_R2.2/fix
      export scripts=$basedir/EXP-testCRTM_R2.2/scripts
      export fixcrtm=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/lib/crtm/2.2.1/fix
   fi
   export tmpdir=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME
   export savdir=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME

#  Next, paths for canned case data.

   export global_T62_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_T62_adate}_update
   export global_T62_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_T62_adate}_update
   export global_4dvar_T62_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_4dvar_T62_adate}_update
   export global_4dvar_T62_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_4dvar_T62_adate}_update
   export global_hybrid_T126_datobs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_hybrid_T126_adate}_update/obs
   export global_hybrid_T126_datges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_hybrid_T126_adate}_update/ges
   export global_enkf_T62_datobs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_enkf_T62_adate}_update/obs
   export global_enkf_T62_datges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_enkf_T62_adate}_update/ges
   export global_lanczos_T62_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_lanczos_T62_adate}_update
   export global_lanczos_T62_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_lanczos_T62_adate}_update
   export global_nemsio_T62_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap/${global_nemsio_T62_adate}_update
   export global_nemsio_T62_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/global/sigmap_nemsio/$global_nemsio_T62_adate
   export nmmb_nems_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/nmmb_nems/$nmmb_nems_adate
   export nmmb_nems_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/nmmb_nems/$nmmb_nems_adate
   export arw_binary_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/arw_binary/$arw_binary_adate
   export arw_binary_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/arw_binary/$arw_binary_adate
   export arw_netcdf_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/arw_netcdf/$arw_netcdf_adate
   export arw_netcdf_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/arw_netcdf/$arw_netcdf_adate
   export nmm_binary_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/nmm_binary/$nmm_binary_adate
   export nmm_binary_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/nmm_binary/$nmm_binary_adate
   export nmm_netcdf_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/nmm_netcdf/$nmm_netcdf_adate
   export nmm_netcdf_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/nmm_netcdf/$nmm_netcdf_adate
   export rtma_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/rtma/$rtma_adate
   export rtma_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/rtma/$rtma_adate
   export hwrf_nmm_obs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/hwrf_nmm/$hwrf_nmm_adate
   export hwrf_nmm_ges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/regional/hwrf_nmm/$hwrf_nmm_adate

#  Location of ndate utility, noscrub directory, and account name (accnt = ada by default).

   export ndate=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
   if [[ "$machine" = "Zeus" ]]; then
      export noscrub=/scratch1/portfolios/NCEPDEV/da/noscrub/$LOGNAME
   elif [[ "$machine" = "Theia" ]]; then
      export noscrub=/scratch4/NCEPDEV/da/noscrub/$LOGNAME
   fi
   export endianness=Big_Endian
#  export endianness=Little_Endian - to be used once convert big_endian is removed from Makefile.conf
   export accnt=hybrid
   if [[ "$machine" = "Zeus" ]]; then
      export launcher=mpiexec_mpt
   elif [[ "$machine" = "Theia" ]]; then
      export launcher=mpirun
   fi

elif [[ "$machine" = "WCOSS" ]]; then

#  First, experiment names.

   export global_T62_updat_exp1=global_T${JCAP}_loproc_updat
   export global_T62_updat_exp2=global_T${JCAP}_hiproc_updat
   export global_T62_contrl_exp1=global_T${JCAP}_loproc_contrl
   export global_T62_contrl_exp2=global_T${JCAP}_hiproc_contrl
   export global_T62_ozonly_updat_exp1=global_T${JCAP}_loproc_ozonly_updat
   export global_T62_ozonly_updat_exp2=global_T${JCAP}_hiproc_ozonly_updat
   export global_T62_ozonly_contrl_exp1=global_T${JCAP}_loproc_ozonly_contrl
   export global_T62_ozonly_contrl_exp2=global_T${JCAP}_hiproc_ozonly_contrl
   export global_4dvar_T62_updat_exp1=global_4dvar_T${JCAP}_loproc_updat
   export global_4dvar_T62_updat_exp2=global_4dvar_T${JCAP}_hiproc_updat
   export global_4dvar_T62_contrl_exp1=global_4dvar_T${JCAP}_loproc_contrl
   export global_4dvar_T62_contrl_exp2=global_4dvar_T${JCAP}_hiproc_contrl
   export global_hybrid_T126_updat_exp1=global_hybrid_loproc_updat
   export global_hybrid_T126_updat_exp2=global_hybrid_hiproc_updat
   export global_hybrid_T126_contrl_exp1=global_hybrid_loproc_contrl
   export global_hybrid_T126_contrl_exp2=global_hybrid_hiproc_contrl
   export global_enkf_T62_updat_exp1=global_enkf_loproc_updat
   export global_enkf_T62_updat_exp2=global_enkf_hiproc_updat
   export global_enkf_T62_contrl_exp1=global_enkf_loproc_contrl
   export global_enkf_T62_contrl_exp2=global_enkf_hiproc_contrl
   export global_lanczos_T62_updat_exp1=global_lanczos_T${JCAP}_loproc_updat
   export global_lanczos_T62_updat_exp2=global_lanczos_T${JCAP}_hiproc_updat
   export global_lanczos_T62_contrl_exp1=global_lanczos_T${JCAP}_loproc_contrl
   export global_lanczos_T62_contrl_exp2=global_lanczos_T${JCAP}_hiproc_contrl
   export global_nemsio_T62_updat_exp1=global_nemsio_T${JCAP}_loproc_updat
   export global_nemsio_T62_updat_exp2=global_nemsio_T${JCAP}_hiproc_updat
   export global_nemsio_T62_contrl_exp1=global_nemsio_T${JCAP}_loproc_contrl
   export global_nemsio_T62_contrl_exp2=global_nemsio_T${JCAP}_hiproc_contrl
   export nmmb_nems_updat_exp1=nmmb_nems_loproc_updat
   export nmmb_nems_updat_exp2=nmmb_nems_hiproc_updat
   export nmmb_nems_contrl_exp1=nmmb_nems_loproc_contrl
   export nmmb_nems_contrl_exp2=nmmb_nems_hiproc_contrl
   export arw_binary_updat_exp1=arw_binary_loproc_updat
   export arw_binary_updat_exp2=arw_binary_hiproc_updat
   export arw_binary_contrl_exp1=arw_binary_loproc_contrl
   export arw_binary_contrl_exp2=arw_binary_hiproc_contrl
   export arw_netcdf_updat_exp1=arw_netcdf_loproc_updat
   export arw_netcdf_updat_exp2=arw_netcdf_hiproc_updat
   export arw_netcdf_contrl_exp1=arw_netcdf_loproc_contrl
   export arw_netcdf_contrl_exp2=arw_netcdf_hiproc_contrl
   export nmm_binary_updat_exp1=nmm_binary_loproc_updat
   export nmm_binary_updat_exp2=nmm_binary_hiproc_updat
   export nmm_binary_contrl_exp1=nmm_binary_loproc_contrl
   export nmm_binary_contrl_exp2=nmm_binary_hiproc_contrl
   export nmm_netcdf_updat_exp1=nmm_netcdf_loproc_updat
   export nmm_netcdf_updat_exp2=nmm_netcdf_hiproc_updat
   export nmm_netcdf_contrl_exp1=nmm_netcdf_loproc_contrl
   export nmm_netcdf_contrl_exp2=nmm_netcdf_hiproc_contrl
   export rtma_updat_exp1=rtma_loproc_updat
   export rtma_updat_exp2=rtma_hiproc_updat
   export rtma_contrl_exp1=rtma_loproc_contrl
   export rtma_contrl_exp2=rtma_hiproc_contrl
   export hwrf_nmm_d2_updat_exp1=hwrf_nmm_d2_loproc_updat
   export hwrf_nmm_d2_updat_exp2=hwrf_nmm_d2_hiproc_updat
   export hwrf_nmm_d2_contrl_exp1=hwrf_nmm_d2_loproc_contrl
   export hwrf_nmm_d2_contrl_exp2=hwrf_nmm_d2_hiproc_contrl
   export hwrf_nmm_d3_updat_exp1=hwrf_nmm_d3_loproc_updat
   export hwrf_nmm_d3_updat_exp2=hwrf_nmm_d3_hiproc_updat
   export hwrf_nmm_d3_contrl_exp1=hwrf_nmm_d3_loproc_contrl
   export hwrf_nmm_d3_contrl_exp2=hwrf_nmm_d3_hiproc_contrl

#  Next, paths for experiment and control executables,
#  fix, ptmp, and CRTM coefficient files.

   export group=dev
   export queue=dev
   export basedir=/da/save/$LOGNAME
   export gsisrc=$basedir/trunk/src
   export gsiexec_updat=$basedir/trunk/src/global_gsi
   export gsiexec_contrl=$basedir/svn1/src/global_gsi
   export enkfexec_updat=$basedir/trunk/src/enkf/global_enkf
   export enkfexec_contrl=$basedir/svn1/src/enkf/global_enkf
   export fixgsi=$basedir/trunk/fix
   export scripts=$basedir/trunk/scripts
   export fixcrtm=/da/save/Michael.Lueken/CRTM_REL-2.2.1/crtm_v2.2.1/fix
   export tmpdir=/ptmpp1/$LOGNAME
   export savdir=/ptmpp1/$LOGNAME

#  Next, paths for canned case data.

   export global_T62_obs=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_T62_adate
   export global_T62_ges=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_T62_adate
   export global_4dvar_T62_obs=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_4dvar_T62_adate
   export global_4dvar_T62_ges=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_4dvar_T62_adate
   export global_hybrid_T126_datobs=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_hybrid_T126_adate/obs
   export global_hybrid_T126_datges=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_hybrid_T126_adate/ges
   export global_enkf_T62_datobs=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_enkf_T62_adate/obs
   export global_enkf_T62_datges=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_enkf_T62_adate/ges
   export global_lanczos_T62_obs=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_lanczos_T62_adate
   export global_lanczos_T62_ges=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_lanczos_T62_adate
   export global_nemsio_T62_obs=/da/noscrub/Michael.Lueken/CASES/global/sigmap/$global_nemsio_T62_adate
   export global_nemsio_T62_ges=/da/noscrub/Michael.Lueken/CASES/global/sigmap_nemsio/$global_nemsio_T62_adate
   export nmmb_nems_obs=/da/noscrub/Michael.Lueken/CASES/regional/nmmb_nems/$nmmb_nems_adate
   export nmmb_nems_ges=/da/noscrub/Michael.Lueken/CASES/regional/nmmb_nems/$nmmb_nems_adate
   export arw_binary_obs=/da/noscrub/Michael.Lueken/CASES/regional/arw_binary/$arw_binary_adate
   export arw_binary_ges=/da/noscrub/Michael.Lueken/CASES/regional/arw_binary/$arw_binary_adate
   export arw_netcdf_obs=/da/noscrub/Michael.Lueken/CASES/regional/arw_netcdf/$arw_netcdf_adate
   export arw_netcdf_ges=/da/noscrub/Michael.Lueken/CASES/regional/arw_netcdf/$arw_netcdf_adate
   export nmm_binary_obs=/da/noscrub/Michael.Lueken/CASES/regional/ndas_binary/$nmm_binary_adate
   export nmm_binary_ges=/da/noscrub/Michael.Lueken/CASES/regional/ndas_binary/$nmm_binary_adate
   export nmm_netcdf_obs=/da/noscrub/Michael.Lueken/CASES/regional/ndas_binary/$nmm_netcdf_adate
   export nmm_netcdf_ges=/da/noscrub/Michael.Lueken/CASES/regional/nmm_netcdf/$nmm_netcdf_adate
   export rtma_obs=/da/noscrub/Michael.Lueken/CASES/regional/rtma_binary/$rtma_adate
   export rtma_ges=/da/noscrub/Michael.Lueken/CASES/regional/rtma_binary/$rtma_adate
   export hwrf_nmm_obs=/da/noscrub/Michael.Lueken/CASES/regional/hwrf_nmm/$hwrf_nmm_adate
   export hwrf_nmm_ges=/da/noscrub/Michael.Lueken/CASES/regional/hwrf_nmm/$hwrf_nmm_adate

#  Location of ndate utility and noscrub directory.

   export ndate=/nwprod/util/exec/ndate
   export noscrub=/da/noscrub/$USER
   export endianness=Big_Endian

fi

# Define type of GPSRO data to be assimilated (refractivity or bending angle)
#default will be refractivity for now

export gps_dtype="gps_bnd"

# Define ptmp location

export ptmp_loc="/ptmpp1/$USER"

# Regression output filename

export global_regression="global_regression_results.$gps_dtype.txt"
export global_ozonly_regression="global_ozonly_regression_results.$gps_dtype.txt"
export global_lanczos_regression="global_lanczos_regression_results.$gps_dtype.txt"
export global_3d4dvar_regression="global_3d4dvar_regression_results.$gps_dtype.txt"
export global_4dvar_regression="global_4dvar_regression_results.$gps_dtype.txt"
export global_nemsio_regression="global_nemsio_regression_results.$gps_dtype.txt"
export global_hybrid_regression="global_hybrid_regression_results.$gps_dtype.txt"
export global_enkf_regression="global_enkf_regression_results.$gps_dtype.txt"
export rtma_regression="rtma_regression_results.$gps_dtype.txt"
export nmm_binary_regression="nmm_binary_regression_results.$gps_dtype.txt"
export nmm_netcdf_regression="nmm_netcdf_regression_results.$gps_dtype.txt"
export arw_binary_regression="arw_binary_regression_results.$gps_dtype.txt"
export arw_netcdf_regression="arw_netcdf_regression_results.$gps_dtype.txt"
export nems_nmmb_regression="nems_nmmb_regression_results.$gps_dtype.txt"
export hwrf_nmm_d2_regression="hwrf_nmm_d2_regression_results.$gps_dtype.txt"
export hwrf_nmm_d3_regression="hwrf_nmm_d3_regression_results.$gps_dtype.txt"

# Regression vfydir

export regression_vfydir="$noscrub/regression"

# Define location for copying control run data to

export control_RTMA="$noscrub/tmpreg_${rtma}/$exp1_rtma_cntrl"
export control_RTMA2="$noscrub/tmpreg_${rtma}/$exp2_rtma_cntrl"
export control_global_T62="$noscrub/tmp${global}/$exp1_global_cntrl"
export control_global_T622="$noscrub/tmp${global}/$exp2_global_cntrl"
export control_global_lanczos_T62="$noscrub/tmp${global_lanczos}/$exp1_global_lanczos_cntrl"
export control_global_lanczos_T622="$noscrub/tmp${global_lanczos}/$exp2_global_lanczos_cntrl"
export control_global_3d4dvar_T62="$noscrub/tmp${global_3d4dvar}/$exp1_global_3d4dvar_cntrl"
export control_global_3d4dvar_T622="$noscrub/tmp${global_3d4dvar}/$exp2_global_3d4dvar_cntrl"
export control_global_4dvar_T62="$noscrub/tmp${global_4dvar}/$exp1_global_4dvar_cntrl"
export control_global_4dvar_T622="$noscrub/tmp${global_4dvar}/$exp2_global_4dvar_cntrl"
export control_global_nemsio_T62="$noscrub/tmp${global_nemsio}/$exp1_global_nemsio_cntrl"
export control_global_nemsio_T622="$noscrub/tmp${global_nemsio}/$exp2_global_nemsio_cntrl"
export control_nmm_binary="$noscrub/tmpreg_${nmm_binary}/$exp1_nmm_binary_cntrl"
export control_nmm_binary2="$noscrub/tmpreg_${nmm_binary}/$exp2_nmm_binary_cntrl"
export control_nmm_netcdf="$noscrub/tmpreg_${nmm_netcdf}/$exp1_nmm_netcdf_cntrl"
export control_nmm_netcdf2="$noscrub/tmpreg_${nmm_netcdf}/$exp2_nmm_netcdf_cntrl"
export control_arw_binary="$noscrub/tmpreg_${arw_binary}/$exp1_arw_binary_cntrl"
export control_arw_binary2="$noscrub/tmpreg_${arw_binary}/$exp2_arw_binary_cntrl"
export control_arw_netcdf="$noscrub/tmpreg_${arw_netcdf}/$exp1_arw_netcdf_cntrl"
export control_arw_netcdf2="$noscrub/tmpreg_${arw_netcdf}/$exp2_arw_netcdf_cntrl"
export control_nems_nmmb="$noscrub/tmpreg_${nems_nmmb}/$exp1_nems_nmmb_cntrl"
export control_nems_nmmb2="$noscrub/tmpreg_${nems_nmmb}/$exp2_nems_nmmb_cntrl"
export control_hwrf_nmm_d2="$noscrub/tmpreg_${hwrf_nmm_d2}/$exp1_hwrf_nmm_d2_cntrl"
export control_hwrf_nmm_d22="$noscrub/tmpreg_${hwrf_nmm_d2}/$exp2_hwrf_nmm_d2_cntrl"
export control_hwrf_nmm_d3="$noscrub/tmpreg_${hwrf_nmm_d3}/$exp1_hwrf_nmm_d3_cntrl"
export control_hwrf_nmm_d32="$noscrub/tmpreg_${hwrf_nmm_d3}/$exp2_hwrf_nmm_d3_cntrl"

# Define debug variable - If you want to run the debug tests, set this variable to .true.  Default is .false.
export debug=".false."   # Set debug to .true. to run the debug tests.  Otherwise, keep this set to .false.

# Define parameters for global_T62_3d4dvar and global_T62_4dvar
export minimization="lanczos"  # If "lanczos", use sqrtb lanczos minimization algorithm.  Otherwise use "pcgsoi".
export nhr_obsbin="6"          # Time window for observation binning.  Use "6" for 3d4dvar test.  Otherwise use "1"

# Define parameters for hybrid ensemble option test.
#   (default is set to false, so no hybrid ensemble option test.)

export HYBENS_GLOBAL=".false."
export ENSEMBLE_SIZE_GLOBAL="10"
export HYBENS_UV_GLOBAL=".true."
export BETA1_INV_GLOBAL="0.5"
export HYBENS_HOR_SCALE_GLOBAL="1500"
export HYBENS_VER_SCALE_GLOBAL="20"
export GENERATE_ENS_GLOBAL=".true."
export HYBENS_ANISO_GLOBAL=".false."

export HYBENS_REGIONAL=".false."
export ENSEMBLE_SIZE_REGIONAL="10"
export HYBENS_UV_REGIONAL=".true."
export BETA1_INV_REGIONAL="0.5"
export HYBENS_HOR_SCALE_REGIONAL="1500"
export HYBENS_VER_SCALE_REGIONAL="20"
export GENERATE_ENS_REGIONAL=".true."
export HYBENS_ANISO_REGIONAL=".false."
export NLON_ENS_REGIONAL="0"
export NLAT_ENS_REGIONAL="0"
export JCAP_ENS_REGIONAL="0"
export JCAP_ENS_TEST_REGIONAL="0"

# Toggle EnKF update code bias correction flag
#  lupd_satbiasc = TRUE  = compute and update radiance bias correction
#  lupd_satbiasc = FALSE = do NOT compute or update radiance bias correction
#  default is FALSE (as done in NCEP operations)

export lupd_satbiasc=".false."
