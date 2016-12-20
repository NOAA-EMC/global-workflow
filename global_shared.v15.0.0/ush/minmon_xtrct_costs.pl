#! /usr/bin/perl

#---------------------------------------------------------------------------
#  minmon_xtrct_costs.pl
#
#  Extract cost data from gsistat file and load into cost 
#  and cost term files.
#---------------------------------------------------------------------------

use strict;
use warnings;

#----------------------------------------------
#  subroutine to trim white space from strings
#----------------------------------------------
sub  trim { my $s = shift; $s =~ s/^\s+|\s+$//g; return $s };


#---------------------------
#
#  Main routine begins here
#
#---------------------------

if ($#ARGV != 4 ) {
	print "usage: minmon_xtrct_costs.pl SUFFIX PDY cyc infile jlogfile\n";
	exit;
}
my $suffix = $ARGV[0];

my $pdy      = $ARGV[1];
my $cyc      = $ARGV[2];
my $infile   = $ARGV[3];
my $jlogfile = $ARGV[4];

#--------------------------------------------------
my $scr = "minmon_xtrct_costs.pl";
my $msg = $scr . " HAS STARTED";

my @msgcmd = ("postmsg", $jlogfile, $msg);
system( @msgcmd ) == 0
   or die "system @msgcmd failed: $?";
#--------------------------------------------------

my $rc    = 0;
my $cdate = sprintf '%s%s', $pdy, $cyc;

if( (-e $infile) ) {

   my $found_cost      = 0;
   my $found_costterms = 0;
   my @cost_array;
   my @jb_array;
   my @jo_array;
   my @jc_array;
   my @jl_array;
   my @term_array;
   my @all_cost_terms;

   my $cost_target;
   my $cost_number;
   my $costterms_target;
   my $jb_number = 5;
   my $jo_number = 6;
   my $jc_number = 7;
   my $jl_number = 8;

#   my $FIXminmon = $ENV{"FIXminmon"};
   my $costfile = $ENV{"mm_costfile"};
#   my $costfile = sprintf '%s', "./minmon_cost.txt";
   
   if( (-e $costfile) ) {
      open( COSTFILE, "<${costfile}" ) or die "Can't open ${costfile}: $!\n";
      my $line;

      while( $line = <COSTFILE> ) {
         if( $line =~ /cost_target/ ) {
            my @termsline = split( /:/, $line );
            $cost_target = $termsline[1];
         } elsif( $line =~ /cost_number/ ) {
            my @termsline = split( /:/, $line );
            $cost_number = $termsline[1];
         } elsif( $line =~ /costterms_target/ ){
            my @termsline = split( /:/, $line );
            $costterms_target = $termsline[1];
         }
      }
      close( COSTFILE );
   } else {
      $rc = 2;
   }

   #------------------------------------------------------------------------
   #  Open the infile and search for the $costterms_target and $cost_target
   #  strings.  If found, parse out the cost information and push into 
   #  holding arrays.
   #------------------------------------------------------------------------
   if( $rc == 0 ) {
      open( INFILE, "<${infile}" ) or die "Can't open ${infile}: $!\n";

      my $line;
      my $term_ctr=0;

      while( $line = <INFILE> ) {
         if( $line =~ /$costterms_target/ ) {
            my @termsline = split( / +/, $line );
            push( @jb_array, $termsline[$jb_number] );
            push( @jo_array, $termsline[$jo_number] );
            push( @jc_array, $termsline[$jc_number] );
            push( @jl_array, $termsline[$jl_number] );
         }

         if( $line =~ /$cost_target/ ) {   
            my @costline = split( / +/, $line );
            push( @cost_array, $costline[$cost_number] );
         }

         if( $term_ctr > 0 ) {
            my @termline = split( / +/, $line );
         
            if ( $term_ctr < 10 ) {
               push( @term_array, trim($termline[1]) );
               push( @term_array, trim($termline[2]) );
               push( @term_array, trim($termline[3]) );
               $term_ctr++;
            } else {
               push( @term_array, trim($termline[1]) );
               push( @term_array, trim($termline[2]) );
               $term_ctr = 0;
            }

         }elsif ( $line =~ "J=" && $line !~ "EJ=" ) {
            my @termline = split( / +/, $line );
            push( @term_array, trim($termline[2]) );
            push( @term_array, trim($termline[3]) );
            push( @term_array, trim($termline[4]) );
            $term_ctr = 1;
         } 
      }

      close( INFILE );


      #----------------------------------------------
      #  move cost_array into all_costs by iteration
      #----------------------------------------------
      my @all_costs;
      for my $i (0 .. $#cost_array) {
         my $iterline = sprintf ' %d,%e,%e,%e,%e,%e%s', 
                       $i, $cost_array[$i], $jb_array[$i], $jo_array[$i], 
                       $jc_array[$i], $jl_array[$i], "\n";

         push( @all_costs, $iterline );
      }

      #---------------------------------------------------
      #  move term_array into all_cost_terms by iteration
      #---------------------------------------------------
      if( @term_array > 0 ) {   
         my $nterms = 32;
         my $max_iter = ($#term_array+1)/$nterms;
         my $niter = $max_iter -1;

         for my $iter (0 .. $niter ) {
            my $step = $iter * $nterms;
            my $iterline = sprintf '%d, %e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e,%e%s',
              $iter, $term_array[$step], $term_array[$step+1], $term_array[$step+2],
                     $term_array[$step+3], $term_array[$step+4], $term_array[$step+5],      
                     $term_array[$step+6], $term_array[$step+7], $term_array[$step+8],    
                     $term_array[$step+9], $term_array[$step+10], $term_array[$step+11],    
                     $term_array[$step+12], $term_array[$step+13], $term_array[$step+14],  
                     $term_array[$step+15], $term_array[$step+16], $term_array[$step+17],  
                     $term_array[$step+18], $term_array[$step+19], $term_array[$step+20],  
                     $term_array[$step+21], $term_array[$step+22], $term_array[$step+23],  
                     $term_array[$step+24], $term_array[$step+25], $term_array[$step+26],  
                     $term_array[$step+27], $term_array[$step+28], $term_array[$step+29],  
                     $term_array[$step+30], $term_array[$step+31], "\n";
            push( @all_cost_terms, $iterline );
         } 
      }

      #------------------------------------------
      #  write all_costs array to costs.txt file
      #------------------------------------------
      my $filename2 = "${cdate}.costs.txt";
      if( @all_costs > 0 ) {
         open( OUTFILE, ">$filename2" ) or die "Can't open ${filename2}: $!\n";
         print OUTFILE @all_costs;
         close( OUTFILE );
      }

      #-----------------------------------------------------
      #  write all_cost_terms array to costs_terms.txt file
      #-----------------------------------------------------
      my $filename3 = "${cdate}.cost_terms.txt";
      if( @all_cost_terms > 0 ) {
         open( OUTFILE, ">$filename3" ) or die "Can't open ${filename3}: $!\n";
         print OUTFILE @all_cost_terms;
         close( OUTFILE );
      }

      #--------------------------
      #  move files to $M_TANKverf
      #--------------------------
      my $tankdir = $ENV{"M_TANKverf"};
      if(! -d $tankdir) {
         system( "mkdir -p $tankdir" );
      } 

      if( -e $filename2 ) {
         my $newfile2 = "${tankdir}/${filename2}";
         system("cp -f $filename2 $newfile2"); 
      }
      if( -e $filename3 ) {
         my $newfile3 = "${tankdir}/${filename3}";
         system("cp -f $filename3 $newfile3"); 
      }

   }				# $rc still == 0 after reading gmon_cost.txt
}
else {				# $infile does not exist
   $rc = 1;
}

#--------------------------------------------------
$msg = $scr . " HAS ENDED";
@msgcmd = ("postmsg", $jlogfile, $msg);
system( @msgcmd ) == 0
   or die "system @msgcmd failed: $?";
#--------------------------------------------------

print "$rc \n"

