#!/bin/bash

#----------------------------------------------------------------
#  Check the contents of the radstat file and compare to
#  the ${run}_radmon_satype.txt file.  Report any missing 
#  or zero sized diag files.
#    

   function usage {
     echo "Usage:  radmon_diag_ck.sh -rad radstat --sat satype --out output "
     echo ""
     echo "            -r,--rad radstat file (required)"
     echo "              File name or path to radstat file."
     echo ""
     echo "            -s,--sat satype file (required)"
     echo "              File name or path to satype file."
     echo ""
     echo "            -o,--out output file name (required)"
     echo "              File name for missing diag file report."
   }


echo "--> radmon_diag_ck.sh"


#--------------------------
#  Process input arguments
#
   nargs=$#
   if [[ $nargs -ne 6 ]]; then
      usage
      exit 1
   fi

   while [[ $# -ge 1 ]]
   do
      key="$1"
      echo $key

      case $key in
         -r|--rad)
            radstat_file="$2"
            shift # past argument
         ;;
         -s|--sat)
            satype_file="$2"
            shift # past argument
         ;;
         -o|--out)
            output_file="$2"
            shift # past argument
         ;;
         *)
            #unspecified key 
            echo " unsupported key = $key"
         ;;
      esac

      shift
   done

#   set -ax

   echo " radstat_file = ${radstat_file}"
   echo " satype_file  = ${satype_file}"
   echo " output_file  = ${output_file}"

   missing_diag=""
   zero_len_diag=""

   #---------------------------------------------
   #  get list of diag files in the radstat file 
   #
   radstat_contents=`tar -tf ${radstat_file} | grep '_ges' |  
		gawk -F"diag_" '{print $2}' | 
		gawk -F"_ges" '{print $1}'`

  
   #---------------------------------------------
   #  load contents of satype_file into an array
   #
   satype_contents=`cat ${satype_file}`

 
   #-------------------------------------------------
   #  compare $satype_contents and $radstat_contents
   #    report anything missing 
   #
   for sat in $satype_contents; do
     test=`echo $radstat_contents | grep $sat`
     
     if [[ ${#test} -le 0 ]]; then
        missing_diag="${missing_diag} ${sat}"
     fi

   done

   echo ""  
   echo "missing_diag = ${missing_diag}" 
   echo ""  


   #---------------------------------------------------------
   #  Check for zero sized diag files.  The diag files in
   #  the radstat file (which is a tar file) are gzipped.  
   #  I find that 0 sized, gzipped file has a size of ~52 
   #  (I assume that's for header and block size).
   #  
   #  So for this check we'll assume anything in the radstat
   #  file with a size of > 1000 bytes is suspect.  (That's 
   #  overkill, 100 is probably sufficient, but I'm the 
   #  nervous type.) So we'll extract, uncompress, and check 
   #  the actual file size of those.  Anything with an 
   #  uncompressed size of 0 goes on the zero_len_diag list.
   #
   verbose_contents=`tar -tvf ${radstat_file} | grep '_ges'`


   #-------------------------------------------------------
   # note:  need to reset the IFS to line breaks otherwise
   #        the $vc value in the for loop below will break 
   #        on all white space, not the line break. 
   SAVEIFS=$IFS
   IFS=$(echo -en "\n\b")


   for vc in ${verbose_contents}; do

      gzip_len=`echo ${vc} | gawk '{print $3}'`

      if [[ ${gzip_len} -le 1000 ]]; then
         test_file=`echo ${vc} | gawk '{print $6}'`
         tar -xf ${radstat_file} ${test_file} 

         gunzip ${test_file}
         unzipped_file=`echo ${test_file%.*}`
         
         uz_file_size=`ls -la ${unzipped_file} | gawk '{print $5}'`

         if [[ ${uz_file_size} -le 0 ]]; then
            sat=`echo ${unzipped_file} | gawk -F"diag_" '{print $2}' | 
		gawk -F"_ges" '{print $1}'`

            zero_len_diag="${zero_len_diag} ${sat}"
         fi

         rm -f ${unzipped_file}
      fi
   done

   IFS=${SAVEIFS}		# reset IFS to default (white space)

   echo ""  
   echo "zero_len_diag = ${zero_len_diag}" 
   echo ""  


   #-----------------------------------------
   #  Write results to $output_file
   #
   if [[ ${#zero_len_diag} -gt 0 ]]; then
      for zld in ${zero_len_diag}; do
         echo "  Zero Length diagnostic file:    $zld" >> $output_file
      done
   fi

   if [[ ${#missing_diag} -gt 0 ]]; then
      for md in ${missing_diag}; do
         echo "  Missing diagnostic file    :    $md" >> $output_file
      done
   fi


echo "<-- radmon_diag_ck.sh"
exit
