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

ncp=/bin/cp

# Name and create temporary directory
tmpdir=$savdir/$compare/$input/${exp1}_vs_${exp2}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Other required constants for regression testing
maxtime=1200

# Copy stdout and incr files 
# from $savdir to $tmpdir
list="$exp1 $exp2 $exp3"
if [[ $(expr substr $exp1 1 4) = "rrfs" ]]; then
   for exp in $list; do
      $ncp $savdir/$exp/stdout ./stdout.$exp
      nmem=5
      imem=1
      while [[ $imem -le $nmem ]]; do
         member="_mem"`printf %03i $imem`
         $ncp $savdir/$exp/fv3sar_tile1_mem${member}_dynvars $tmpdir/dynvars$member.$exp
         $ncp $savdir/$exp/fv3sar_tile1_mem${member}_tracer $tmpdir/tracer$member.$exp
         (( imem = $imem + 1 ))
      done
   done
else
   for exp in $list; do
      $ncp $savdir/$exp/stdout ./stdout.$exp
      nmem=10
      imem=1
      while [[ $imem -le $nmem ]]; do
         member="_mem"`printf %03i $imem`
         $ncp $savdir/$exp/incr_${global_adate}_fhr06$member $tmpdir/incr$member.$exp
         (( imem = $imem + 1 ))
      done
   done
fi

# Grep out ensemble mean increment information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep 'ens. mean anal. increment' stdout.$exp > increment.$exp.txt
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., increment.exp1.txt with increment.exp2.txt)
diff increment.$exp1.txt increment.$exp2.txt > increment.${exp1}-${exp2}.txt
diff increment.$exp1.txt increment.$exp3.txt > increment.${exp1}-${exp3}.txt

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
       echo 'resulting in Failure maxtime of the regression test.'
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
       echo 'resulting in Failure timethresh of the regression test.'
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
       echo 'resulting in Failure timethresh2 of the regression test.'
       echo
       failed_test=1
     else
       echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds and is within the allowable threshold time of '$timethresh2' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # Next, maximum residence set size (both harware limitation and percent difference)
   # First, hardware limitation


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

if [[ $(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp2}.txt) = 0 ]]; then
   if [[ $(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp2}.txt) = 0 ]]; then
      echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible.'
#      echo 'since the corresponding ens. mean anal. increments are identical with '$(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp2}.txt)' lines different.'
      echo
   else
      echo 'The results between the two runs are nonreproducible,'
#      echo 'thus the regression test has failed for '${exp1}' and '${exp2}' analyses.'
      echo 'thus the regression test has Failed mean anal for '${exp1}' and '${exp2}' analyses with '$(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp2}.txt)' lines different.'
      echo
      failed_test=1
#      exit 1
   fi
else
   echo 'The results between the two runs are nonreproducible,'
#   echo 'thus the regression test has failed for '${exp1}' and '${exp2}' analyses.'
   echo 'thus the regression test has Failed mean anal for '${exp1}' and '${exp2}' analyses with '$(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp2}.txt)' lines different.'
   echo
   failed_test=1
#   exit 1
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp2

if [[ $(expr substr $exp1 1 4) = "rrfs" ]]; then

{

nmem=5
imem=1
while [[ $imem -le $nmem ]]; do
   member="_mem"`printf %03i $imem`
   ncdump dynvars$member.${exp1} > dynvars$member.${exp1}.out
   ncdump dynvars$member.${exp2} > dynvars$member.${exp2}.out
   if [ ! diff dynvars$member.${exp1}.out dynvars$member.${exp2}.out ]; then
       echo 'dynvars'$member'.'${exp1}' dynvars'$member'.'${exp2}' are NOT identical'
       failed_test=1
   else
       rm -f dynvars$member.${exp1}.out dynvars$member.${exp2}.out
       echo 'dynvars'$member'.'${exp1}' dynvars'$member'.'${exp2}' are identical'
   fi
   ncdump tracer$member.${exp1} > tracers$member.${exp1}.out
   ncdump tracer$member.${exp2} > tracers$member.${exp2}.out
   if [ ! diff tracers$member.${exp1}.out tracers$member.${exp2}.out ]; then
       echo 'tracer'$member'.'${exp1}'  tracer'$member'.'${exp2}' are NOT identical'
       failed_test=1
   else
       rm -f tracers$member.${exp1}.out tracers$member.${exp2}.out
       echo 'tracer'$member'.'${exp1}'  tracer'$member'.'${exp2}' are identical'
       q
   fi
   (( imem = $imem + 1 ))
done
echo

} >> $output

elif [[ -f wrf_inout.${exp1} ]]; then

{

if cmp -s wrf_inout.${exp1} wrf_inout.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
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
fi

} >> $output
   else
{
nmem=10
imem=1
while [[ $imem -le $nmem ]]; do
   member="_mem"`printf %03i $imem`
   ncdump incr$member.${exp1} > incr$member.${exp1}.out
   ncdump incr$member.${exp2} > incr$member.${exp2}.out
   if [ ! diff incr$member.${exp1}.out incr$member.${exp2}.out ]; then
       echo 'incr'$member'.'${exp1}' incr'$member'.'${exp2}' are NOT identical'
   else
       rm -f incr$member.${exp1}.out incr$member.${exp2}.out
   fi
   (( imem = $imem + 1 ))
done
echo
} >> $output
   fi
fi

# Next, reproducibility between exp1 and exp3

if [[ `expr substr $exp1 1 4` = "hwrf" ]]; then

{

   echo  'Results are not currently reproducible for different number of processors.  Skipping test.'
   echo

} >> $output

else

{

   if [[ $(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp3}.txt) = 0 ]]; then
      if [[ $(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp3}.txt) = 0 ]]; then
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
#        echo 'since the corresponding ens. mean anal. increments are identical with '$(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp3}.txt)' lines different.'
         echo
      else
         echo 'The results between the two runs are nonreproducible,'
#         echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses.'
         echo 'thus the regression test has Failed mean anal for '${exp1}' and '${exp3}' analyses with '$(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp3}.txt)' lines different.'
         echo
         failed_test=1
#         exit 1
      fi
   else
      echo 'The results between the two runs are nonreproducible,'
#      echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses.'
         echo 'thus the regression test has Failed mean anal for '${exp1}' and '${exp3}' analyses with '$(grep -c 'ens. mean anal. increment' increment.${exp1}-${exp3}.txt)' lines different.'
      echo
      failed_test=1
#      exit 1
   fi

} >> $output

# Next, check reproducibility of results between exp1 and exp3

   if [[ $(expr substr $exp1 1 4) = "rrfs" ]]; then

{

	nmem=5
        imem=1
        while [[ $imem -le $nmem ]]; do
           member="_mem"`printf %03i $imem`
           ncdump dynvars$member.${exp1} > dynvars$member.${exp1}.out
           ncdump dynvars$member.${exp3} > dynvars$member.${exp3}.out
           if [ ! diff dynvars$member.${exp1}.out dynvars$member.${exp3}.out ]; then
               echo 'dynvars'$member'.'${exp1}' dynvars'$member'.'${exp3}' are NOT identical'
               failed_test=1
           else
               rm -f dynvars$member.${exp1}.out dynvars$member.${exp3}.out
               echo 'dynvars'$member'.'${exp1}' dynvars'$member'.'${exp3}' are identical'
           fi
           ncdump tracer$member.${exp1} > tracers$member.${exp1}.out
           ncdump tracer$member.${exp3} > tracers$member.${exp3}.out
           if [ ! diff tracers$member.${exp1}.out tracers$member.${exp3}.out ]; then
               echo 'tracer'$member'.'${exp1}'  tracer'$member'.'${exp3}' are NOT identical'
               failed_test=1
           else
               rm -f tracers$member.${exp1}.out tracers$member.${exp3}.out
               echo 'tracer'$member'.'${exp1}'  tracer'$member'.'${exp3}' are identical'
           fi
           (( imem = $imem + 1 ))
        done
        echo

} >> $output

   elif [[ -f wrf_inout.${exp1} ]]; then

{

      if cmp -s wrf_inout.${exp1} wrf_inout.${exp3}
      then
         echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
         echo 'since the corresponding results are identical.'
         echo
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
         fi

} >> $output
      else

{
   nmem=10
   imem=1
   while [[ $imem -le $nmem ]]; do
      member="_mem"`printf %03i $imem`
      ncdump incr$member.${exp1} > incr$member.${exp1}.out
      ncdump incr$member.${exp3} > incr$member.${exp3}.out
      if [ ! diff incr$member.${exp1}.out incr$member.${exp3}.out ]; then
          echo 'incr'$member'.'${exp1}' incr'$member'.'${exp3}' are NOT identical'
      else
          rm -f incr$member.${exp1}.out incr$member.${exp3}.out
      fi
      (( imem = $imem + 1 ))
   done
   echo
} >> $output
      fi
   fi
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
