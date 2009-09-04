#!/bin/sh

#@ job_name=regression_driver
#@ step_name=driver
#@ error=regression_driver.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class=dev
#@ group=dev
#@ wall_clock_limit = 01:05:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ queue

#@ step_name=table_creation
#@ error=table_creation.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class=dev
#@ group=dev
#@ wall_clock_limit = 00:10:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ dependency = (driver == 0)
#@ queue

#@ step_name=debug_tests
#@ error=debug_tests.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class=dev
#@ group=dev
#@ wall_clock_limit = 00:10:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ dependency = (table_creation == 0)
#@ queue

. regression_var.sh

case $LOADL_STEP_NAME in
  driver)

rm -rf $regression_vfydir

if [[ $control == true ]]; then

   rm -rf $noscrub/tmpreg_${rtma}
   rm -rf $noscrub/tmp${global}
   rm -rf $noscrub/tmpreg_${arw_binary}
   rm -rf $noscrub/tmpreg_${arw_netcdf}
   rm -rf $noscrub/tmpreg_${nmm_binary}
   rm -rf $noscrub/tmpreg_${nmm_netcdf}
   rm -rf $noscrub/tmpreg_${nems_nmmb}

   list="global_T62 RTMA nmm_binary nmm_netcdf arw_binary arw_netcdf nems_nmmb"
   for configuration in $list; do
       llsubmit regression_$configuration.sh
   done

sleep 3600 ## sleep for an hour to allow for jobs to finish running before attempting to generate table

elif [[ $control == false ]]; then
     list="global_T62_nc RTMA_nc nmm_binary_nc nmm_netcdf_nc arw_binary_nc arw_netcdf_nc nems_nmmb_nc"
     for configuration in $list; do
         llsubmit regression_$configuration.sh
     done

sleep 1800 ## sleep for half an hour to allow for jobs to finish running before attmepting to generate table

fi

rm -f regression_driver.e*

exit ;;

  table_creation)

set -ax

# pool all grepped data from single regression tests to
# create a table of values for runtime and memory

# Name and create location for creation of regression test table
tmpdir_table=$ptmp_loc/$compare/table_creation/
rm -rf $tmpdir_table
mkdir -p $tmpdir_table
cd $tmpdir_table

# Define output name for table
output=regression_test_table.out

ncp=/bin/cp

# Name temporary directories holding regression test files
tmpdir_RTMA=$ptmp_loc/$compare/tmpreg_${rtma}/${exp1_rtma_sub_1node}_vs_${exp1_rtma_bench_1node}
tmpdir_nmm_binary=$ptmp_loc/$compare/tmpreg_${nmm_binary}/${exp1_nmm_binary_sub_2node}_vs_${exp1_nmm_binary_bench_2node}
tmpdir_nmm_netcdf=$ptmp_loc/$compare/tmpreg_${nmm_netcdf}/${exp1_nmm_netcdf_sub_1node}_vs_${exp1_nmm_netcdf_bench_1node}
tmpdir_arw_netcdf=$ptmp_loc/$compare/tmpreg_${arw_netcdf}/${exp1_arw_netcdf_sub_1node}_vs_${exp1_arw_netcdf_bench_1node}
tmpdir_arw_binary=$ptmp_loc/$compare/tmpreg_${arw_binary}/${exp1_arw_binary_sub_1node}_vs_${exp1_arw_binary_bench_1node}
tmpdir_nems_nmmb=$ptmp_loc/$compare/tmpreg_${nems_nmmb}/${exp1_nems_nmmb_sub_2node}_vs_${exp1_nems_nmmb_bench_2node}
tmpdir_global=$ptmp_loc/$compare/tmp${global}/${exp1_global_sub_1node}_vs_${exp1_global_bench_1node}

# Copy grepped out files
list="$tmpdir_RTMA $tmpdir_nmm_binary $tmpdir_nmm_netcdf $tmpdir_arw_netcdf $tmpdir_arw_binary $tmpdir_nems_nmmb $tmpdir_global"
for dir in $list; do
   $ncp $dir/runtime* ./
   $ncp $dir/memory* ./
done

# At this time, creating table one experiment at a time.
# Beginning with global GSI results
glob_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_global_sub_1node.txt)
glob_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_global_sub_2node.txt)
glob_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_global_bench_1node.txt)
glob_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_global_bench_2node.txt)

glob_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_global_sub_1node.txt)
glob_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_global_sub_2node.txt)
glob_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_global_bench_1node.txt)
glob_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_global_bench_2node.txt)

# Now, RTMA
rtma_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_rtma_sub_1node.txt)
rtma_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_rtma_sub_2node.txt)
rtma_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_rtma_bench_1node.txt)
rtma_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_rtma_bench_2node.txt)

rtma_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_rtma_sub_1node.txt)
rtma_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_rtma_sub_2node.txt)
rtma_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_rtma_bench_1node.txt)
rtma_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_rtma_bench_2node.txt)

# Now, nmm_binary
nmm_binary_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_nmm_binary_sub_2node.txt)
nmm_binary_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_nmm_binary_sub_3node.txt)
nmm_binary_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_nmm_binary_bench_2node.txt)
nmm_binary_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_nmm_binary_bench_3node.txt)

nmm_binary_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_nmm_binary_sub_2node.txt)
nmm_binary_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_nmm_binary_sub_3node.txt)
nmm_binary_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_nmm_binary_bench_2node.txt)
nmm_binary_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_nmm_binary_bench_3node.txt)

# Now nems_nmmb
nems_nmmb_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_nems_nmmb_sub_2node.txt)
nems_nmmb_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_nems_nmmb_sub_3node.txt)
nems_nmmb_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_nems_nmmb_bench_2node.txt)
nems_nmmb_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_nems_nmmb_bench_3node.txt)

nems_nmmb_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_nems_nmmb_sub_2node.txt)
nems_nmmb_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_nems_nmmb_sub_3node.txt)
nems_nmmb_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_nems_nmmb_bench_2node.txt)
nems_nmmb_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_nems_nmmb_bench_3node.txt)

# Now nmm_netcdf
nmm_netcdf_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_nmm_netcdf_sub_1node.txt)
nmm_netcdf_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_nmm_netcdf_sub_2node.txt)
nmm_netcdf_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_nmm_netcdf_bench_1node.txt)
nmm_netcdf_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_nmm_netcdf_bench_2node.txt)

nmm_netcdf_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_nmm_netcdf_sub_1node.txt)
nmm_netcdf_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_nmm_netcdf_sub_2node.txt)
nmm_netcdf_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_nmm_netcdf_bench_1node.txt)
nmm_netcdf_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_nmm_netcdf_bench_2node.txt)

# Now arw_netcdf
arw_netcdf_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_arw_netcdf_sub_1node.txt)
arw_netcdf_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_arw_netcdf_sub_2node.txt)
arw_netcdf_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_arw_netcdf_bench_1node.txt)
arw_netcdf_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_arw_netcdf_bench_2node.txt)

arw_netcdf_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_arw_netcdf_sub_1node.txt)
arw_netcdf_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_arw_netcdf_sub_2node.txt)
arw_netcdf_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_arw_netcdf_bench_1node.txt)
arw_netcdf_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_arw_netcdf_bench_2node.txt)

# Finally arw_binary
arw_binary_sub_1_time=$(awk '{ print $8 }' runtime.$exp1_arw_binary_sub_1node.txt)
arw_binary_sub_2_time=$(awk '{ print $8 }' runtime.$exp2_arw_binary_sub_2node.txt)
arw_binary_bench_1_time=$(awk '{ print $8 }' runtime.$exp1_arw_binary_bench_1node.txt)
arw_binary_bench_2_time=$(awk '{ print $8 }' runtime.$exp2_arw_binary_bench_2node.txt)

arw_binary_sub_1_mem=$(awk '{ print $8 }' memory.$exp1_arw_binary_sub_1node.txt)
arw_binary_sub_2_mem=$(awk '{ print $8 }' memory.$exp2_arw_binary_sub_2node.txt)
arw_binary_bench_1_mem=$(awk '{ print $8 }' memory.$exp1_arw_binary_bench_1node.txt)
arw_binary_bench_2_mem=$(awk '{ print $8 }' memory.$exp2_arw_binary_bench_2node.txt)

# Now create table layout

{

echo "Experimental Case      low task time(s)     high task time(s)   low task time(s)     high task time(s)   low task mem      high task mem  low task mem     high task mem"
echo "                                    subversion                                benchmark                            subversion                       benchmark           "
echo
echo "T"$global"             "$glob_sub_1_time"           "$glob_sub_2_time"          "$glob_bench_1_time"           "$glob_bench_2_time"          "$glob_sub_1_mem"            "$glob_sub_2_mem"         "$glob_bench_1_mem"           "$glob_bench_2_mem""
echo ""$rtma"                   "$rtma_sub_1_time"           "$rtma_sub_2_time"          "$rtma_bench_1_time"           "$rtma_bench_2_time"          "$rtma_sub_1_mem"            "$rtma_sub_2_mem"         "$rtma_bench_1_mem"           "$rtma_bench_2_mem""
echo ""$nmm_binary"            "$nmm_binary_sub_1_time"           "$nmm_binary_sub_2_time"          "$nmm_binary_bench_1_time"           "$nmm_binary_bench_2_time"          "$nmm_binary_sub_1_mem"            "$nmm_binary_sub_2_mem"         "$nmm_binary_bench_1_mem"           "$nmm_binary_bench_2_mem""
echo ""$nems_nmmb"              "$nems_nmmb_sub_1_time"           "$nems_nmmb_sub_2_time"          "$nems_nmmb_bench_1_time"           "$nems_nmmb_bench_2_time"          "$nems_nmmb_sub_1_mem"            "$nems_nmmb_sub_2_mem"         "$nems_nmmb_bench_1_mem"           "$nems_nmmb_bench_2_mem""
echo ""$nmm_netcdf"            "$nmm_netcdf_sub_1_time"            "$nmm_netcdf_sub_2_time"           "$nmm_netcdf_bench_1_time"            "$nmm_netcdf_bench_2_time"           "$nmm_netcdf_sub_1_mem"            "$nmm_netcdf_sub_2_mem"         "$nmm_netcdf_bench_1_mem"           "$nmm_netcdf_bench_2_mem""
echo ""$arw_netcdf"             "$arw_netcdf_sub_1_time"            "$arw_netcdf_sub_2_time"           "$arw_netcdf_bench_1_time"            "$arw_netcdf_bench_2_time"           "$arw_netcdf_sub_1_mem"            "$arw_netcdf_sub_2_mem"         "$arw_netcdf_bench_1_mem"           "$arw_netcdf_bench_2_mem""
echo ""$arw_binary"             "$arw_binary_sub_1_time"            "$arw_binary_sub_2_time"           "$arw_binary_bench_1_time"            "$arw_binary_bench_2_time"           "$arw_binary_sub_1_mem"            "$arw_binary_sub_2_mem"         "$arw_binary_bench_1_mem"           "$arw_binary_bench_2_mem""

} >> $output

mkdir -p $regression_vfydir

$ncp $output                  $regression_vfydir/

cd $scripts
rm -f table_creation.e*

exit ;;

  debug_tests)

set -ax

if [[ $debug = "true" ]]; then

cd $src
make debug

# Now, return back to script directory to submit scripts for debug mode testing
cd $scripts

   list="global_T62_db RTMA_db nmm_binary_db nmm_netcdf_db arw_binary_db arw_netcdf_db nems_nmmb_db"
   for configuration in $list; do
       llsubmit regression_$configuration.sh
   done

elif [[ $debug = "false" ]]; then
      echo 'No debug runs made'
fi

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit
