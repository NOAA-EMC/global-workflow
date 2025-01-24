set -ax

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the control run

exp1=$1
exp2=$3
exp3=$2

input=$5

#input=tmp62

# Name output file
output=$6

#output=$global_regression

# Give location of analysis results, and choose location for regression output
savdir=$savdir/$input
vfydir=$regression_vfydir
failed_test=0
ncp=/bin/cp

# Name and create temporary directory
# pc: (1) Where is "compare" defined?
#     (2) $savdir already has $input in it, why add another one?
tmpdir=$savdir/$compare/$input/${exp1}_vs_${exp2}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Other required constants for regression testing
maxtime=1200

# Copy stdout and fort.220 files 
# from $savdir to $tmpdir
list="$exp1 $exp2 $exp3"
for exp in $list; do
   $ncp $savdir/$exp/stdout ./stdout.$exp
   $ncp $savdir/$exp/fort.220 ./fort.220.$exp
   $ncp $savdir/$exp/siganl ./siganl.$exp
   $ncp $savdir/$exp/siginc ./siginc.$exp
   $ncp $savdir/$exp/wrf_inout ./wrf_inout.$exp
   $ncp $savdir/$exp/wrf_inout06 ./wrf_inout06.$exp
   $ncp $savdir/$exp/siginc.nc ./siginc.nc.$exp
   $ncp $savdir/$exp/fv3_dynvars ./fv3_dynvars.$exp
   $ncp $savdir/$exp/fv3_sfcdata ./fv3_sfcdata.$exp
   $ncp $savdir/$exp/fv3_tracer ./fv3_tracer.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep -c 'cost,grad,step' stdout.$exp > minimizer_use.$exp
   if [ $(awk '{ print $1 }' minimizer_use.$exp) -gt 0 ]; then
      grep 'cost,grad,step' stdout.$exp > penalty.$exp.txt
   else
      grep 'congrad::evaljgrad: grepcost' stdout.$exp > penalty.$exp.txt
   fi
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.exp1.txt with penalty.exp2.txt)
diff penalty.$exp1.txt penalty.$exp2.txt > penalty.${exp1}-${exp2}.txt
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt

# Give location of additional output files for scalability testing
exp1_scale=$2
exp2_scale=$4

# Copy stdout for additional scalability testing
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   $ncp $savdir/$exp_scale/stdout ./stdout.$exp_scale
done

# Grep out run time from stdout file
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   grep 'The total amount of wall time' stdout.$exp_scale > runtime.$exp_scale.txt
   grep 'The maximum resident set size' stdout.$exp_scale > memory.$exp_scale.txt
done

# Important values used to calculate timethresh and memthresh below
# Values below can be fine tuned to make the regression more or less aggressive
# Currently using a value of 10%

timedif=$7
memdiff=$8
scaledif=$9

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from control as avg values here

time2=$(awk '{ print $8 }' runtime.$exp2.txt)
time1=$(awk '{ print $8 }' runtime.$exp1.txt)
mem=$(awk '{ print $8 }' memory.$exp2.txt)

timethresh=$( echo "scale=6;$time2 / $timedif + $time2" | bc -l )
memthresh=$( echo "scale=0;$mem / $memdiff + $mem" | bc -l )

# Fill time variables with scalability data

time_scale1=$(awk '{ print $8 }' runtime.$exp1_scale.txt)
time_scale2=$(awk '{ print $8 }' runtime.$exp2_scale.txt)

timethresh2=$( echo "scale=6;$time_scale2 / $timedif + $time_scale2" | bc -l )

# Now, figure out difference in time between two runs

scale1=$( echo "scale=6;$time1 - $time_scale1" | bc -l )
scale2=$( echo "scale=6;$time2 - $time_scale2" | bc -l )

# Calculate maximum allowable deviation for scalability

scalelogic=$( echo "0 < $scale1" | bc )
if [[ "$scalelogic" = 1 ]]; then
   scale1thresh=$( echo "scale=6;$scale1 / $scaledif + $scale1" | bc -l )
else
   scale1thresh=$( echo "scale=6;$scale1 / $scaledif - $scale1" | bc -l )
fi

# Begin applying threshold tests
# First, wall time (both maximum allowable time and max/min allowable deviation)

   {

   # This part is for the maximum allowable time (operationally)

     timelogic=$( echo "$time1 > $maxtime" | bc )
     if [[ "$timelogic" = 1 ]]; then
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable operational time of '$maxtime' seconds,'
       echo 'resulting in Failure of max-time in the regression test.'
       echo  
       failed_test=1
     else
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the maximum allowable operational time of '$maxtime' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # This part is for deviation of wall time for timethresh

   {

     timelogic=$( echo "$time1 > $timethresh" | bc )
     if [[ "$timelogic" = 1 ]]; then
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable threshold time of '$timethresh' seconds,'
       echo 'resulting in Failure time-thresh of the regression test.'
       echo
       failed_test=1
     else
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the allowable threshold time of '$timethresh' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # This part is for deviation of wall time for timethresh2

   {

     timelogic=$( echo "$time_scale1 > $timethresh2" | bc )
     if [[ "$timelogic" = 1 ]]; then
       echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds.  This has exceeded maximum allowable threshold time of '$timethresh2' seconds,'
       echo 'resulting in Failure of timethresh2 the regression test.'
       echo
       failed_test=1
     else
       echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds and is within the allowable threshold time of '$timethresh2' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # Next, maximum residence set size

   {

     if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $memthresh ]]; then
       echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable memory of '$memthresh' KBs,'
       echo 'resulting in Failure memthresh of the regression test.'
       echo
       failed_test=1
     else
       echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable memory of '$memthresh' KBs,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

# Next, reproducibility between exp1 and exp2

{

if [[ $(grep -c 'cost,grad,step' penalty.${exp1}-${exp2}.txt) = 0 ]]; then
   if [[ $(grep -c 'congrad::evaljgrad: grepcost' penalty.${exp1}-${exp2}.txt) = 0 ]]; then
      echo 'The results (penalty) between the two runs ('${exp1}' and '${exp2}') are reproducible.'
#      echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp2}.txt)' lines different.'
      echo
   else
      echo 'The results (penalty) between the two runs are nonreproducible,'
      echo 'thus the regression test has Failed on cost for '${exp1}' and '${exp2}' analyses.'
#     echo 'thus the regression test has failed for '${exp1}' and '${exp2}' analyses with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp2}.txt)' lines different.'
      echo
      failed_test=1
   fi
else
   echo 'The results (penalty) between the two runs are nonreproducible,'
   echo 'thus the regression test has Failed on cost for '${exp1}' and '${exp2}' analyses.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp2

if [[ `expr substr $exp1 1 4` = "rtma" ]]; then

{

if cmp -s siganl.${exp1} siganl.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
   echo 'Thus, the case has Failed siganl the regression tests.'
   echo
   failed_test=1
fi

} >> $output

elif [[ -f wrf_inout.${exp1} ]]; then

{

if cmp -s wrf_inout.${exp1} wrf_inout.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
   echo 'Thus, the case has Failed wrf_inout the regression tests.'
   echo
   failed_test=1
fi

} >> $output

elif [[ -f wrf_inout06.${exp1} ]]; then

{

if cmp -s wrf_inout06.${exp1} wrf_inout06.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
   echo 'Thus, the case has Failed wrf_inout06 of the regression tests.'
   echo
   failed_test=1
fi

} >> $output

elif [[ `expr substr $exp1 1 6` = "global" ]]; then
   if [[ -f siginc.${exp1} ]]; then
{

if cmp -s siginc.${exp1} siginc.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
   echo 'Thus, the case has Failed siginc of the regression tests.'
   echo
   failed_test=1
fi

} >> $output
   elif [[ -f siganl.${exp1} ]]; then
{

if cmp -s siganl.${exp1} siganl.${exp2} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
   echo 'Thus, the case has Failed siganl of the regression tests.'
   echo
   failed_test=1
fi

} >> $output
   elif [[ -f siginc.nc.${exp1} ]] ; then
{
ncdump siginc.nc.${exp1} > siginc.nc.${exp1}.out
ncdump siginc.nc.${exp2} > siginc.nc.${exp2}.out
if diff -s siginc.nc.${exp1}.out siginc.nc.${exp2}.out
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
   echo 'Thus, the case has Failed siganl of the regression tests.'
   echo
   failed_test=1
fi
} >> $output
   fi

   elif [[ `expr substr $exp1 1 4` = "rrfs" ]] || [[ `expr substr $exp1 1 4` = "hafs" ]]; then
{
     fv3_failed_test=0
     if cmp -s fv3_dynvars.${exp1} fv3_dynvars.${exp2}
     then
       echo 'The fv3_dynvars are reproducible'
     else
       fv3_failed_test=1
     fi
     if cmp -s fv3_sfcdata.${exp1} fv3_sfcdata.${exp2}
     then
       echo 'The fv3_sfcdata are reproducible'
     else
       fv3_failed_test=1
     fi
     if cmp -s fv3_tracer.${exp1} fv3_tracer.${exp2}
     then
       echo 'The fv3_tracer are reproducible'
     else
       fv3_failed_test=1
     fi
     if [[ $fv3_failed_test -eq 0 ]]
     then
        echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
        echo 'since the corresponding results are identical.'
        echo
     else
        echo 'The results between the two runs ('${exp1}' and '${exp2}') are not reproducible'
        echo 'Thus, the case has Failed siganl of the regression tests.'
        echo
        failed_test=1
     fi
} >> $output
fi

# Next, reproducibility between exp1 and exp3

{

if [[ $(grep -c 'cost,grad,step' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
   if [[ $(grep -c 'congrad::evaljgrad: grepcost' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
      echo 'The results (penalty) between the two runs ('${exp1}' and '${exp3}') are reproducible'
#     echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp3}.txt)' lines different.'
      echo
   else
      echo 'The results (penalty) between the two runs are nonreproducible,'
      echo 'thus the regression test has Failed cost for '${exp1}' and '${exp3}' analyses.'
#     echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp3}.txt)' lines different.'
      echo
      failed_test=1
   fi
else
   echo 'The results (penalty) between the two runs are nonreproducible,'
   echo 'thus the regression test has Failed cost for '${exp1}' and '${exp3}' analyses.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp3

if [[ `expr substr $exp1 1 4` = "rtma" ]]; then

{

   if cmp -s wrf_inout.${exp1} wrf_inout.${exp3}
   then
      echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
      echo 'since the corresponding results are identical.'
      echo
   else
      echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
      echo 'Thus, the case has Failed wrf_inout of the regression tests.'
      echo
      failed_test=1
   fi

} >> $output

elif [[ -f wrf_inout.${exp1} ]]; then

{

   if cmp -s wrf_inout.${exp1} wrf_inout.${exp3}
   then
      echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
      echo 'since the corresponding results are identical.'
      echo
   else
      echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
      echo 'Thus, the case has Failed wrf_inout of the regression tests.'
      echo
      failed_test=1
   fi

} >> $output

elif [[ -f wrf_inout06.${exp1} ]]; then

{

   if cmp -s wrf_inout06.${exp1} wrf_inout06.${exp3}
   then
      echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
      echo 'since the corresponding results are identical.'
      echo
   else
      echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
      echo 'Thus, the case has Failed wrf_inout06 of the regression tests.'
      echo
      failed_test=1
   fi

} >> $output

elif [[ `expr substr $exp1 1 6` = "global" ]]; then
   if [[ -f siginc.${exp1} ]]; then
{

      if cmp -s siginc.${exp1} siginc.${exp3}
      then
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
         echo 'since the corresponding results are identical.'
         echo
      else
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
         echo 'Thus, the case has Failed siginc of the regression tests.'
         echo
         failed_test=1
      fi

} >> $output

   elif [[ -f siganl.${exp1} ]]; then

{

      if cmp -s siganl.${exp1} siganl.${exp3} 
      then
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
         echo 'since the corresponding results are identical.'
         echo
      else
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
         echo 'Thus, the case has Failed siganl of the regression tests.'
         echo
         failed_test=1
      fi

} >> $output

   elif [[ -f siginc.nc.${exp1} ]]; then

{
      ncdump siginc.nc.${exp1} > siginc.nc.${exp1}.out
      ncdump siginc.nc.${exp3} > siginc.nc.${exp3}.out

      if diff -s siginc.nc.${exp1}.out siginc.nc.${exp3}.out
      then
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
         echo 'since the corresponding results are identical.'
         echo
      else
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
         echo 'Thus, the case has Failed siganl of the regression tests.'
         echo
         failed_test=1
      fi
} >> $output

   fi

elif [[ `expr substr $exp1 1 4` = "rrfs" ]] || [[ `expr substr $exp1 1 4` = "hafs" ]]; then
{
     fv3_failed_test=0
     if cmp -s fv3_dynvars.${exp1} fv3_dynvars.${exp3}
     then
       echo 'The fv3_dynvars are reproducible'
     else
       fv3_failed_test=1
     fi
     if cmp -s fv3_sfcdata.${exp1} fv3_sfcdata.${exp3}
     then
       echo 'The fv3_sfcdata are reproducible'
     else
       fv3_failed_test=1
     fi
     if cmp -s fv3_tracer.${exp1} fv3_tracer.${exp3}
     then
       echo 'The fv3_tracer are reproducible'
     else
       fv3_failed_test=1
     fi
     if [[ $fv3_failed_test -eq 0 ]]
     then
        echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
        echo 'since the corresponding results are identical.'
        echo
     else
        echo 'The results between the two runs ('${exp1}' and '${exp3}') are not reproducible'
        echo 'Thus, the case has Failed siganl of the regression tests.'
        echo
        failed_test=1
     fi
} >> $output

fi

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

# Final check for any failed tests
count=$(grep -i "fail" $output |wc -l)
if [ $count -gt 0 ]; then
    (( failed_test = $failed_test + $count ))
fi

# Remove job log files is no failures detected
cd $scripts
if [ $count -eq 0 ]; then
    rm -f ${exp1}.out
    rm -f ${exp2}.out
    rm -f ${exp3}.out
    rm -f ${exp2_scale}.out
fi

if [[ "$clean" = ".true." ]]; then
   rm -rf $savdir
fi

exit $failed_test
