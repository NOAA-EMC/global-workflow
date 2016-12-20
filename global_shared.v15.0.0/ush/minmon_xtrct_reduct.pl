#! /usr/bin/perl
use strict;

#---------------------------------------------------------------------------
#  minmon_xtrct_reduct.pl
#
#  Extract the reduction stats for a GSI minimization run and store in
#  reduction.ieee_d files ready for GrADS use.
#---------------------------------------------------------------------------

if ($#ARGV != 4 ) {
	print "usage: minmon_xtrct_reduct.pl SUFFIX pdy cyc infile jlogfile\n";
        print " suffix is data source identifier\n";
        print " pdy is YYYYMMDD of the cycle to be processed\n";
        print " cyc is HH of the cycle to be processed\n";
        print " infile is the data file containing the reduction stats\n";
        print " jlogfile is the job log file\n";
	exit;
}
my $suffix   = $ARGV[0];
my $pdy      = $ARGV[1];
my $cyc      = $ARGV[2];
my $infile   = $ARGV[3];
my $jlogfile = $ARGV[4];

#--------------------------------------------------
my $scr = "minmon_xtrct_reduct.pl";
my $msg = $scr . " HAS STARTED";

my @msgcmd = ("postmsg", $jlogfile, $msg);
system( @msgcmd ) == 0
   or die "system @msgcmd failed: $?";
#--------------------------------------------------

my $rc    = 0;
my $cdate = sprintf '%s%s', $pdy, $cyc;

if( (-e $infile) ) {

   my $reduct_target = "penalty and grad reduction";
   my $reduct_num = 12;

   open( INFILE, "<${infile}" ) or die "Can't open ${infile}: $!\n";

   my @reduct_array;

   while( my $line = <INFILE> ) {
      if( $line =~ /$reduct_target/ ) {
         my @reduct_ln  = split( / +/, $line ); 
         push( @reduct_array, $reduct_ln[$reduct_num] );
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

#--------------------------------------------------
$msg = $scr . " HAS ENDED";
@msgcmd = ("postmsg", $jlogfile, $msg);
system( @msgcmd ) == 0
   or die "system @msgcmd failed: $?";
#--------------------------------------------------

print "$rc \n"

