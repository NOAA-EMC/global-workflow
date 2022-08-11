#!/usr/bin/env perl

use strict;
use warnings;
use List::MoreUtils 'true';
use List::MoreUtils 'first_index';
use List::MoreUtils 'last_index';

#---------------------------------------------------------------------------
#  minmon_xtrct_gnorms.pl
#
#  Update the gnorm_data.txt file with data from a new cycle.  Add 
#  this new data to the last line of the gnorm_data.txt file.
#
#  Note:  If the gnorm_data.txt file does not exist, it will be created.
#
#  The gnorm_data.txt file is used plotted directly by the javascript on
#  the GSI stats page.
#---------------------------------------------------------------------------
sub updateGnormData {
   my $cycle     = $_[0];
   my $igrad     = $_[1];
   my $fgnorm    = $_[2];
   my $avg_gnorm = $_[3];
   my $min_gnorm = $_[4];
   my $max_gnorm = $_[5];
   my $suffix    = $_[6];

   my $rc        = 0;
   my @filearray;
 
   my $gdfile  = "gnorm_data.txt";  

   my $outfile = "new_gnorm_data.txt";
   my $yr      = substr( $cycle, 0, 4);
   my $mon     = substr( $cycle, 4, 2);
   my $day     = substr( $cycle, 6, 2);
   my $hr      = substr( $cycle, 8, 2);
 
   my $newln = sprintf ' %04d,%02d,%02d,%02d,%e,%e,%e,%e,%e%s', 
                    $yr, $mon, $day, $hr, $igrad, $fgnorm,   
                    $avg_gnorm, $min_gnorm, $max_gnorm, "\n";

   #-------------------------------------------------------------
   #  attempt to locate the latest $gdfile and copy it locally
   #
   if( -e $gdfile ) {
      open( INFILE, "<${gdfile}" ) or die "Can't open ${gdfile}: $!\n";

      @filearray = <INFILE>;

#   This is the mechanism that limits the data to 30 days worth.  Should I 
#   keep it or let the transfer script(s) truncate?  6/12/16 -- I'm going to keep
#   it.  I can add this as a later change once I add a user mechanism to vary the 
#   amount of data plotted (on the fly).

      my $cyc_interval = $ENV{'CYCLE_INTERVAL'}; 
      if( $cyc_interval eq "" ) {
         $cyc_interval = 6;
      }

      my $max_cyc = 119;	#	default 30 days worth of data = 120 cycles
				#	If CYCLE_INTERVAL is other than "" or 6
				#	then set the $max_cyc using that interval
      if( $cyc_interval != 6 && $cyc_interval != 0 ) {
         my $cyc_per_day = 24 / $cyc_interval;
         $max_cyc = (30 * $cyc_per_day) - 1;
      } 
  
      while( $#filearray > $max_cyc ) { 
         shift( @filearray );
      }
      close( INFILE );
   }

   #  Here is the problem Russ encountered after re-running the MinMon:  
   #     If the cycle time in $newln is the same as an existing record in 
   #       *.gnorm_data.txt then we end up with 2+ rows for the same cycle time.
   #       In that case $newln should replace the first existing line
   #       in @filearray and all other lines that might match should be deleted.
   #     Else when the cycle time doesn't already exist (the expected condition) 
   #       it should be pushed into @filearray.
 
   # algorithm:  
   # =========
   #   Establish $count of matches on "$yr,$mon,$day,$hr"
   #     if $count > 0 
   #       while $count > 1
   #          get last_index and remove with splice
   #       replace first_index with $newln
   #     else
   #       push $newln
   # 
   my $srch_strng = "$yr,$mon,$day,$hr";
   my $count = true { /$srch_strng/ } @filearray;
 
   if( $count > 0 ) {
      while( $count > 1 ) {
         my $l_index = last_index { /$srch_strng/ } @filearray;
         splice @filearray, $l_index, 1;
         $count = true { /$srch_strng/ } @filearray;
      }
      my $f_index = first_index { /$srch_strng/ } @filearray;
      splice @filearray, $f_index, 1, $newln;
   }
   else {
      push( @filearray, $newln );
   }
   
   open( OUTFILE, ">$outfile" ) or die "Can't open ${$outfile}: $!\n";
   print OUTFILE @filearray;
   close( OUTFILE );

   system("cp -f $outfile $gdfile"); 

}

#---------------------------------------------------------------------------
#  makeErrMsg
#
#  Apply a gross check on the final value of the gnorm for a specific 
#  cycle.  If the final_gnorm value is greater than the gross_check value
#  then put that in the error message file.  Also check for resets or a
#  premature halt, and journal those events to the error message file too.
#
#  Note to self:   reset_iter array is passed by reference
#---------------------------------------------------------------------------
sub  makeErrMsg {
   my $suffix      = $_[0];
   my $cycle       = $_[1];
   my $final_gnorm = $_[2];
   my $stop_flag   = $_[3];
   my $stop_iter   = $_[4];
   my $reset_flag  = $_[5];
   my $reset_iter  = $_[6];  #reset iteration array
   my $infile      = $_[7];
   my $gross_check = $_[8];  

   my $mail_msg    ="";
   my $out_file = "${cycle}.errmsg.txt";


   if( $stop_flag > 0 ) {
      my $stop_msg = " Gnorm check detected premature iteration stop:  suffix = $suffix, cycle = $cycle, iteration = $stop_iter";
      $mail_msg .= $stop_msg;
   }

   if( $reset_flag > 0 ) {
      my $ctr=0; 
      my $reset_msg = "\n Gnorm check detected $reset_flag reset(s):  suffix = $suffix, cycle = $cycle";
      $mail_msg .= $reset_msg;
      $mail_msg .= "\n";
      $mail_msg .= "   Reset(s) detected in iteration(s):  @{$reset_iter}[$ctr] \n";

      my $arr_size = @{$reset_iter};
      for( $ctr=1; $ctr < $arr_size; $ctr++ ) {
         $mail_msg .= "                                       @{$reset_iter}[$ctr]\n";
      }
   }

   if( $final_gnorm >= $gross_check ){
      my $gnorm_msg  = " Final gnorm gross check failure:  suffix = $suffix,  cycle = $cycle, final gnorm = $final_gnorm ";

      $mail_msg .= $gnorm_msg;
   }

   if( length $mail_msg > 0 ){
      my $file_msg  = "  File source for report is:  $infile";
      $mail_msg .= $file_msg;
   }

   if( length $mail_msg > 0 ){
      my $mail_link   = "http://www.emc.ncep.noaa.gov/gmb/gdas/gsi_stat/index.html?src=$suffix&typ=gnorm&cyc=$cycle";
      open( OUTFILE, ">$out_file" ) or die "Can't open ${$out_file}: $!\n";
      print OUTFILE $mail_msg;
      print OUTFILE "\n\n $mail_link";
      close( OUTFILE );
   } 
}


#---------------------------------------------------------------------------
#
#  Main routine begins here
#
#---------------------------------------------------------------------------

if ($#ARGV != 4 ) {
   print "usage: minmon_xtrct_gnorms.pl SUFFIX pdy cyc infile jlogfile\n";
   exit;
}


my $suffix   = $ARGV[0];
my $pdy      = $ARGV[1];
my $cyc      = $ARGV[2];
my $infile   = $ARGV[3];
my $jlogfile = $ARGV[4];


my $scr = "minmon_xtrct_gnorms.pl";
print "$scr Has Started\n";

#
# This needs to be redesigned to get the gnorm value from the gsistat file
# using the line that starts "cost,grad,step,b,step?:".  The line formerly 
# used for the gnorm and reduction values may not be available if the the
# verbose output flag is set to FALSE.
#
# So, using the grad value on that line:
#   gnorm[i]  = (grad[i]**)/(grad[0]**)
#   reduct[i] = sqrt(gnorm)

my $igrad_target;
my $igrad_number;
my $expected_gnorms;
my $gross_check_val;

my $rc    = 0;
my $cdate = sprintf '%s%s', $pdy, $cyc;

my $gnormfile = $ENV{"mm_gnormfile"};


if( (-e $gnormfile) ) {
   open( GNORMFILE, "<${gnormfile}" ) or die "Can't open ${gnormfile}: $!\n";
   my $line;

   while( $line = <GNORMFILE> ) {
      if( $line =~ /igrad_target/ ) {
         my @termsline = split( /:/, $line );
         $igrad_target = $termsline[1];
      } elsif( $line =~ /igrad_number/ ) {
         my @termsline = split( /:/, $line );
         $igrad_number = $termsline[1];
      } elsif( $line =~ /expected_gnorms/ ){
         my @termsline = split( /:/, $line );
         $expected_gnorms = $termsline[1];
      } elsif( $line =~ /gross_check_val/ ){
         my @termsline = split( /:/, $line );
         $gross_check_val = $termsline[1];
      }
   }
   close( GNORMFILE );
} else {
   $rc = 4;
}

if( $rc == 0 ) { 
   if( (-e $infile) ) {
      open( INFILE, "<${infile}" ) or die "Can't open ${infile}: $!\n";

      my $found_igrad = 0;
      my $final_gnorm = 0.0;
      my $igrad       = 0.0;
      my $header      = 4;
      my $header2     = 0;
      my @gnorm_array;
      my @last_10_gnorm;

      my $reset_flag = 0;
      my $stop_flag = 0;
      my $warn_str  = "WARNING";
      my $stop_str  = "Stopping";
      my $stop_iter = "";
      my $reset_str = "Reset";
      my @reset_iter;		# reset iteration array

      my $stop_iter_flag  = 0;
      my $reset_iter_flag = 0;
      my $line;
      while( $line = <INFILE> ) {

         ##############################################
         #  if the reset_iter_flag is 1 then record the 
         #  current outer & inner iteration number
         ##############################################
         if( $reset_iter_flag == 1 ) {
            if( $line =~ /${igrad_target}/ ) {
               my @iterline  = split( / +/, $line ); 
               my $iter_str = $iterline[2] . "," . $iterline[3];
               push( @reset_iter, $iter_str);
               $reset_iter_flag = 0;  
            }
         }


         if( $line =~ /${igrad_target}/ ) {
            my @gradline  = split( / +/, $line ); 

            my $grad = $gradline[$igrad_number];

	    if( $found_igrad == 0 ){
               $igrad = $grad;
               $found_igrad = 1;
            }

            my $igrad_sqr = $igrad**2;
            my $grad_sqr  = $grad**2;
            my $gnorm     = $grad_sqr/$igrad_sqr;
          		
	    push( @gnorm_array, $gnorm );
         }


         if( $line =~ /${warn_str}/ ) {
            if( $line =~ /${stop_str}/ ) {
               $stop_flag++;
               $stop_iter_flag=1;
            }
            elsif( $line =~ /${reset_str}/ ){
               $reset_flag++;
               $reset_iter_flag = 1;
            }
         }

      }
      close( INFILE );

      ########################################################################
      #  If the stop_flag is >0 then record the last outer & inner
      #  iteration number.  The trick is that it's the last iteration in the 
      #  log file and we just passed it when we hit the stop warning message,
      #  so we have to reopen the file and get the last iteration number.
      ########################################################################
      if( $stop_flag > 0 ) {
         open( INFILE, "<${infile}" ) or die "Can't open ${infile}: $!\n";

         my @lines = reverse <INFILE>;
         foreach $line (@lines) {
            if( $line =~ /${igrad_target}/ ){
               my @iterline  = split( / +/, $line ); 
               $stop_iter = $iterline[2] . "," . $iterline[3];
               last;
            }
         }
         close( INFILE );
      }


      my @all_gnorm = @gnorm_array;
   
      ##############################################################################  
      ##
      ##  If the iterations were halted due to error then the @all_gnorm array won't
      ##  be the expected size.  In that case we need to pad the array out with 
      ##  RMISS values so GrADS won't choke when it tries to read the data file.
      ##
      ##  Note that we're padding @all_gnorm.  The @gnorm_array is examined below
      ##  and we don't want to pad that and mess up the min/max calculation.
      ## 
      ###############################################################################  
      my $arr_size = @all_gnorm;

      if( $arr_size < $expected_gnorms ) {
         for( my $ctr = $arr_size; $ctr < $expected_gnorms; $ctr++ ) {
            push( @all_gnorm, -999.0 );
         }
      }

      my $sum_10_gnorm = 0.0;
      my $min_gnorm    = 9999999.0;
      my $max_gnorm    = -9999999.0;
      my $avg_gnorm    = 0.0;

      for( my $ctr = 9; $ctr >= 0; $ctr-- ) {
         my $new_gnorm = pop( @gnorm_array );
         $sum_10_gnorm = $sum_10_gnorm + $new_gnorm;
         if( $new_gnorm > $max_gnorm ) {
            $max_gnorm = $new_gnorm;
         }
         if( $new_gnorm < $min_gnorm ) {
            $min_gnorm = $new_gnorm;
         }
         if( $ctr == 9 ) {
            $final_gnorm = $new_gnorm;
         }
      }

      $avg_gnorm = $sum_10_gnorm / 10;

   
      #####################################################################
      #  Update the gnorm_data.txt file with information on the 
      #  initial gradient, final gnorm, and avg/min/max for the last 10 
      #  iterations.
      #####################################################################
      updateGnormData( $cdate,$igrad,$final_gnorm,$avg_gnorm,$min_gnorm,$max_gnorm,$suffix );


      #####################################################################
      #  Call makeErrMsg to build the error message file to record any     
      #  abnormalities in the minimization.  This file can be mailed by
      #  a calling script.            
      #####################################################################
      makeErrMsg( $suffix, $cdate, $final_gnorm, $stop_flag, $stop_iter, $reset_flag, \@reset_iter, $infile, $gross_check_val );


      #########################################################
      # write to GrADS ready output data file
      #
      #   Note:  this uses pack to achieve the same results as 
      #          an unformatted binary Fortran file.
      #########################################################
      my $filename2 = "${cdate}.gnorms.ieee_d";

      open( OUTFILE, ">$filename2" ) or die "Can't open ${filename2}: $!\n";
      binmode OUTFILE;

      print OUTFILE pack( 'f*', @all_gnorm);

      close( OUTFILE );

      #--------------------------
      #  move files to $M_TANKverf
      #--------------------------
      my $tankdir = $ENV{"M_TANKverf"};
      print "M_TANKverf = $tankdir \n";
      if(! -d $tankdir) {
         print "making $tankdir\n";
         system( "mkdir -p $tankdir" );
      }
   
      if( -e $filename2 ) {
         system("cp -f $filename2 ${tankdir}/.");
      }

      my $gdfile  = "gnorm_data.txt";  
      if( -e $gdfile ) {
         system("cp -f $gdfile ${tankdir}/.");
      }

      my $errmsg = "${cdate}.errmsg.txt";
      if( -e $errmsg ) {
         system("cp -f $errmsg ${tankdir}/.");
      }
   
   }				# $rc still == 0 after reading gmon_gnorm.txt

}else {				# $infile does not exist
   $rc = 3;
}

print "$scr has ended, return code = $rc \n"
