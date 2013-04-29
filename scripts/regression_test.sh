set -ax

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the control run

exp1=$1
exp2=$3
exp3=$2

#exp1=$global_T62_updat_exp1
#exp2=$global_T62_contrl_exp1
#exp3=$global_T62_updat_exp2

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
# Dew/Mist=26 GB/16 tasks per node
##maxmem=$((1500000*1))
# Vapor=110 GB/48 tasks per node
##maxmem=$((2300000*1))
# Cirrus=110 GB/32 tasks per node
maxmem=$((3400000*1))

# Copy stdout and fort.220 files 
# from $savdir to $tmpdir
list="$exp1 $exp2 $exp3"
for exp in $list; do
   $ncp $savdir/$exp/stdout ./stdout.$exp
   $ncp $savdir/$exp/fort.220 ./fort.220.$exp
   $ncp $savdir/$exp/siganl ./siganl.$exp
   $ncp $savdir/$exp/wrf_inout ./wrf_inout.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep 'cost,grad,step' fort.220.$exp > penalty.$exp.txt
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.exp1.txt with penalty.exp2.txt)
diff penalty.$exp1.txt penalty.$exp2.txt > penalty.${exp1}-${exp2}.txt
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt

# Give location of additional output files for scalability testing
exp1_scale=$2
exp2_scale=$4

#exp1_scale=$global_T62_updat_exp2
#exp2_scale=$global_T62_contrl_exp2

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

timedif=10
memdiff=8
scaledif=4

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from control as avg values here

time2=$(awk '{ print $8 }' runtime.$exp2.txt)
time1=$(awk '{ print $8 }' runtime.$exp1.txt)
mem=$(awk '{ print $8 }' memory.$exp2.txt)

timethresh=$((time2 / timedif + time2))
memthresh=$((mem / memdiff + mem))

# Fill time variables with scalability data

time_scale1=$(awk '{ print $8 }' runtime.$exp1_scale.txt)
time_scale2=$(awk '{ print $8 }' runtime.$exp2_scale.txt)

timethresh2=$((time_scale2 / timedif + time_scale2))

# Now, figure out difference in time between two runs

scale1=$((time1 - time_scale1))
scale2=$((time2 - time_scale2))

# Calculate maximum allowable deviation for scalability

scale1thresh=$((scale1 / scaledif + scale1))

# Begin applying threshold tests
# First, wall time (both maximum allowable time and max/min allowable deviation)

if [[ "$arch" = AIX ]]; then

   {

   # This part is for the maximum allowable time (operationally)

     if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $maxtime ]]; then
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable operational time of '$maxtime' seconds,'
       echo 'resulting in failure of the regression test.'
       echo
     else
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the maximum allowable operational time of '$maxtime' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # This part is for deviation of wall time for timethresh

   {

     if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $timethresh ]]; then
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable threshold time of '$timethresh' seconds,'
       echo 'resulting in failure of the regression test.'
       echo
     else
       echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the allowable threshold time of '$timethresh' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # This part is for deviation of wall time for timethresh2

   {

     if [[ $(awk '{ print $8 }' runtime.$exp1_scale.txt) -gt $timethresh2 ]]; then
       echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds.  This has exceeded maximum allowable threshold time of '$timethresh2' seconds,'
       echo 'resulting in failure of the regression test.'
       echo
     else
       echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds and is within the allowable threshold time of '$timethresh2' seconds,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # Next, maximum residence set size (both harware limitation and percent difference)
   # First, hardware limitation

   {

     if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $maxmem ]]; then
       echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable hardware memory limit of '$maxmem' KBs,'
       echo 'resulting in failure of the regression test.'
       echo
     else
       echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable hardware memory limit of '$maxmem' KBs,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

   # Next, maximum residence set size

   {

     if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $memthresh ]]; then
       echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable memory of '$memthresh' KBs,'
       echo 'resulting in failure of the regression test.'
       echo
     else
       echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable memory of '$memthresh' KBs,'
       echo 'continuing with regression test.'
       echo
     fi

   } >> $output

fi

# Next, reproducibility between exp1 and exp2

{

if [[ $(grep -c 'cost,grad,step' penalty.${exp1}-${exp2}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp2}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are nonreproducible,'
   echo 'thus the regression test has failed for '${exp1}' and '${exp2}' analyses with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp2}.txt)' lines different.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp2

if [[ -f wrf_inout.${exp1} ]]; then

{

if cmp -s wrf_inout.${exp1} wrf_inout.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output
else
{

if cmp -s siganl.${exp1} siganl.${exp2} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

fi

# Next, reproducibility between exp1 and exp3

{

if [[ $(grep -c 'cost,grad,step' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are nonreproducible,'
   echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses with '$(grep -c 'cost,grad,step' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp3

if [[ -f wrf_inout.${exp1} ]]; then

{

if cmp -s wrf_inout.${exp1} wrf_inout.${exp3}
then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

else

{

if cmp -s siganl.${exp1} siganl.${exp3} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

fi

if [[ "$arch" = AIX ]]; then

   # Finally, scalability

   {

   if [[ $scale1thresh -ge $scale2 ]]; then
      echo 'The case has passed the scalability regression test.'
      echo 'The slope for the update ('$scale1thresh' seconds per node) is greater than or equal to that for the control ('$scale2' seconds per node).'
   else
      echo 'The case has failed the scalability test.'
      echo 'The slope for the update ('$scale1thresh' seconds per node) is less than that for the control ('$scale2' seconds per node).'
   fi

   } >> $output

fi

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

cd $scripts
rm -f ${exp1}.out
rm -f ${exp2}.out
rm -f ${exp3}.out
rm -f ${exp2_scale}.out

exit
