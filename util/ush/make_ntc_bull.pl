#!/usr/bin/perl
#
#------------------------------------------------------
#
# This is make_ntc_bull.pl 
# It attaches the appropriate headers to the input file
# and copies it to a unique name for input to NTC.
#
# A Bulletin Flag Field Separator is prepended to the 
# text bulletin.  This TOC header contains the total
# number of bytes in the product not counting the 
# bulletin flag field separator.
#
# Input: 
#    File identifier -  Output name identier.
#    subheader - "NONE" if none.
#    Originator - Not used currently               
#    datetime  - Not used currently                         
#    filename - input file name
#    output_path - name of output file
#
#   Author: Larry Sager based on a script by Paula Freeman
#
#  31 Oct  05 -- new script                           
#
#------------------------------------------------------

if ($ENV{job}) { $job=$ENV{job}; }
if ($ENV{SENDCOM}) { $SENDCOM=$ENV{SENDCOM}; }
if ($ENV{SENDDBN}) { $SENDDBN=$ENV{SENDDBN}; }
$NArgs = @ARGV;

if ($NArgs < 6)   {
   usage ();
   exit;
}

#
#  Get input
#

$NAME=shift; 
$WMOname=shift; 
$ORIGname=shift; 
$DATEname=shift; 
$Filename=shift; 
$OutputFilename=shift;
print " Input :  $Filename";
print " Output:  $OutputFilename";


if ( ($Filename eq "") || ($OutputFilename eq "")  ) {
   usage ();
   exit;
}

#
#  Create the file for TOC
#
      if ( $NAME eq "plot" ) {
         make_tocplot ();
      }
      elsif ($NAME eq "redb" ) {
         make_tocredb ();
      }
      else {
         make_tocbull ();
      }
#
#

   
sub usage () {
   print "Usage: $0  <wmoheader> <Originator> <YYYYMMDDHH> <subheader|NONE> <inpath> <outpath>\n";
}

sub make_tocbull {

#
#  Attach WMO header
#  Get the bytecount of file to insert into the Bulletin Flag Field Seperator.
#

   $ix = 0;
   $under = "_";
   open (INFILE, $Filename) or die "Cannot open $Filename";

   while ($cho=<INFILE>) {
      $rec = $rec . $cho;
   }
   $cho = $rec;
   $cho =~ s/\n//g;
   $cho =~ s/<<@@/\r\r\n/g;
   $cho =~ s/<<@/\r\r\n/g;
   $cho =~ s/<<//g;
   $cho =~ s/>//g;
   $cho =~ s/\^//g;
   $cho =~ s/\$//g;
   $cho =~ s/\|/+/g;
   $value = 40;
   $Outp="$OutputFilename";
   open(OUTFILE, ">$Outp") or die "Cannot open $OutputFilename for output.";
   while ($ix == 0)  {
      $cho = substr($cho,$value);
      $value = 38;
      $cho =~ s/'1/\&\&/;
      $cho =~ s/'0/\&\&/;
#      print  "cho is $cho";
      ($cho2,$cho) = split(/\&\&/,$cho);
      ($cho2,$cho3) = split(/\%/,$cho2);
#      print  "cho2 is $cho2";
      $ByteCount = length($cho2);
      print " length is $ByteCount ";
      $BulletinFlagFieldSep = sprintf( "****%10.10d****", $ByteCount);
      if ($SENDCOM eq "YES") {
        if ($ByteCount  >  50  ) {
          print OUTFILE "$BulletinFlagFieldSep\n";
          print OUTFILE $cho2;
        }
        else {   
          $ix = 1;
        }
      }
   }
   close OUTFILE;
   if ($SENDDBN eq "YES" ) {
# Modified 20051205 by wx11rp to ensure the current production machine is used.
#      $dbn_alert="/gpfs/w/nco/dbnet/bin/dbn_alert";
      $dbn_alert=$ENV{'DBNROOT'} . "/bin/dbn_alert";
      $type="GRIB_LOW";
      $job2=$job;
      $subtype=$ORIGname;
      $file_path=$Outp;
      @command = ($dbn_alert, $type, $subtype, $job2, $file_path);
      if (system (@command) != 0) {
          print "Error alerting: @command \n";
      }
   }

   close INFILE;
   close OUTFILE;

   print "$Filename -> $OutputFilename\n";
}

sub make_tocplot {

#
#  Attach WMO header
#  Get the bytecount of file to insert into the Bulletin Flag Field Seperator.
#

   $ix = 0;
   $under = "_";
   open (INFILE, $Filename) or die "Cannot open $Filename";

   while ($cho=<INFILE>) {
      $rec = $rec . $cho;
   }
   $cho = $rec;
#   $Outp="$OutputFilename$under$job";
   $Outp="$OutputFilename";
   open(OUTFILE, ">$Outp") or die "Cannot open $OutputFilename for output.";
   while ($ix == 0)  {
      $cho =~ s/\$\$/\&\&/;
      ($cho2,$cho) = split(/\&\&/,$cho);
#      $cho2 =~ s/@/ /g;
#      $cho2 = $cho2 . "  ";
      $ByteCount = length($cho2);
      print " length is $ByteCount ";
      $BulletinFlagFieldSep = sprintf( "****%10.10d****", $ByteCount);
      if ($SENDCOM eq "YES") {
        if ($ByteCount  >  50  ) {
          print OUTFILE "$BulletinFlagFieldSep\n";
          print OUTFILE $cho2;
        }
        else {
          $ix = 1;
        }
      }
   }
   close OUTFILE;
   if ($SENDDBN eq "YES" ) {
# 20051205 Modified by wx11rp to allow the script to run on any manchine labeled as the production machine
#      $dbn_alert="/gpfs/w/nco/dbnet/bin/dbn_alert";
      $dbn_alert=$ENV{'DBNROOT'} . "/bin/dbn_alert";
      $type="GRIB_LOW";
      $subtype=$DATEname;
      $job2=$job;
      $file_path=$Outp;
      @command = ($dbn_alert, $type, $subtype, $job2, $file_path);
      if (system (@command) != 0) {
              print "Error alerting: @command \n";
      }
   }

   close INFILE;
   close OUTFILE;

   print "$Filename -> $OutputFilename\n";
}
sub make_tocredb {

#
#  Prepare the Redbook graphic for transmission to TOC by removing the AWIPS
#  header and creating an NTC header.  Get the Bytecount of the file to   
#  insert into the Bulletin Flag Field Seperator.
#

   $ix = 0;
   $under = "_";
   open (INFILE, $Filename) or die "Cannot open $Filename";

   while ($cho=<INFILE>) {
      $rec = $rec . $cho;
   }
   $cho = $rec;
   $Outp="$OutputFilename";
   open(OUTFILE, ">$Outp") or die "Cannot open $OutputFilename for output.";
   $cho = substr($cho,24); 
   $ByteCount = length($cho);
   print " length is $ByteCount ";
   $BulletinFlagFieldSep = sprintf( "****%10.10d****", $ByteCount);
   if ($SENDCOM eq "YES") {
      if ($ByteCount  >  50  ) {
          print OUTFILE "$BulletinFlagFieldSep\n";
          print OUTFILE $cho;
          
      }
   }
   close OUTFILE;
   if ($SENDDBN eq "YES" ) {
# 20051205 Modified by wx11rp to allow the script to run on any manchine labeled as the production machine
#      $dbn_alert="/gpfs/w/nco/dbnet/bin/dbn_alert";
      $dbn_alert=$ENV{'DBNROOT'} . "/bin/dbn_alert";
      $type="GRIB_LOW";
      $subtype=$DATEname;
      $job2=$job;
      $file_path=$Outp;
     @command = ($dbn_alert, $type, $subtype, $job2, $file_path);
      if (system (@command) != 0) {
          print "Error alerting: @command \n";
      }
   }

   close INFILE;
   close OUTFILE;

   print "$Filename -> $OutputFilename\n";
}
