#!/usr/bin/env perl

use strict;

#---------------------------------------------------------------------------
#  minmon_xtrct_reduct.pl
#
#  Extract the reduction stats for a GSI minimization run and store in
#  reduction.ieee_d files ready for GrADS use.
#---------------------------------------------------------------------------

if ($#ARGV != 3 ) {
	print "usage: minmon_xtrct_reduct.pl SUFFIX pdy cyc infile\n";
        print " suffix is data source identifier\n";
        print " pdy is YYYYMMDD of the cycle to be processed\n";
        print " cyc is HH of the cycle to be processed\n";
        print " infile is the data file containing the reduction stats\n";
	exit;
}
my $suffix   = $ARGV[0];
my $pdy      = $ARGV[1];
my $cyc      = $ARGV[2];
my $infile   = $ARGV[3];

my $scr = "minmon_xtrct_reduct.pl";
print "$scr has started\n";

my $rc    = 0;
my $cdate = sprintf '%s%s', $pdy, $cyc;
my $initial_gradient = -999.0;
my $iter_gradient;

if( (-e $infile) ) {

   my $reduct_target = "cost,grad,step,b,step?";
   my $gradient_num = 5;
   my $reduct;

   open( INFILE, "<${infile}" ) or die "Can't open ${infile}: $!\n";

   my @reduct_array;

   while( my $line = <INFILE> ) {
      if( $line =~ /$reduct_target/ ) {
         my @reduct_ln  = split( / +/, $line ); 
         $iter_gradient = $reduct_ln[$gradient_num];
         if( $initial_gradient == -999.0 ){
            $initial_gradient = $iter_gradient;
         }
        
         $reduct = $iter_gradient / $initial_gradient;          

         push( @reduct_array, $reduct );
      }
   }

   close( INFILE );


   #################################
   #  write reduct_array to outfile
   #################################
   my $outfile = "${cdate}.reduction.ieee_d";
   open( OUTFILE, ">$outfile" ) or die "Can't open ${outfile}: $!\n";
   binmode OUTFILE;

   print OUTFILE pack( 'f*', @reduct_array);
   close( OUTFILE );

   #----------------------------
   #  copy outfile to $M_TANKverf
   #----------------------------
   my $tankdir = $ENV{"M_TANKverf"};
   if(! -d $tankdir) {
      system( "mkdir -p $tankdir" );
   }

   if( -e $outfile ) {
      my $newfile = "${tankdir}/${outfile}";
      system("cp -f $outfile $newfile");
   }

} else {				# $infile does not exist
   $rc = 5;
}

print "$scr has ended, return code = $rc \n"
