#!/usr/bin/perl
#
#------------------------------------------------------
#
# This is make_NTC_file.pl 
# It attaches the appropriate headers to the input file
# and copies it to a unique name for input to NTC.
#
# The following lines are prepended to the file:
#   1. A Bulletin Flag Field Seperator
#   2. A WMO header line
#   3. An optional subheader, e.g. DIFAX1064
#
# Input wmoheader Originator datetime path
# where:
#    wmoheader - WMO id to use in WMO header.
#    subheader - "NONE" if none.
#    Originator - Originator to use in WMO header
#    datetime  - date/time to use in WMO header, yyyymmddhh
#    path - name input file
#    output_path - name of output file
#
#   Author: Paula Freeman based on script by Larry Sager
#
#------------------------------------------------------

$NArgs = @ARGV;

if ($NArgs < 6)   {
   usage ();
   exit;
}

#
#  Get input
#

$WMOHeader=shift; 
$Origin=shift; 
$YYYYMMDDHH=shift; 
$SubHeader=shift;
$Filename=shift; 
$OutputFilename=shift;

print "Filename is $Filename\n";
print "Output Filename is $OutputFilename\n";
$YYYYMMDDHH =~ /\d{4}(\d{2})(\d{4})/;
$MMDDHH = $1 . $2;
$DDHHMM = $2 . "00";
print "WMOHeader = $WMOHeader\n";
print "SubHeader = $SubHeader\n";
print "Origin = $Origin\n";


if ( ($WMOHeader eq "") || ($Origin eq "") || ($YYYYMMDDHH eq "") || ($Filename eq "") || ($OutputFilename eq "")  || ($SubHeader eq "") ) {
   usage ();
   exit;
}

#
#  Create the file for TOC
#

   make_toc ();
#
#

   
sub usage () {
   print "Usage: $0  <wmoheader> <Originator> <YYYYMMDDHH> <subheader|NONE> <inpath> <outpath>\n";
}

sub make_toc  {

#
#  Attach WMO header and subheader (if not "NONE").
#  Get the bytecount of file to insert into the Bulletin Flag Field Seperator.
#  Add in length of WMO header, plus two carriage returns and line feed.
#  If Subheader specified, count that in also,  plus line a  feed.
#

   $Header = "$WMOHeader $Origin $DDHHMM";
   $ByteCount = `wc -c $Filename | cut -c1-8`;
   $ByteCount= $ByteCount + length($Header) + 3;
   if ($SubHeader =~ /NONE/) {
      print "No Subheader\n";
   } else {
      if ($SubHeader =~ /IMAG/){
         $ByteCount = $ByteCount + length($SubHeader);
      } else {
         $ByteCount = $ByteCount + length($SubHeader) + 3;
      }
   }
   $BulletinFlagFieldSep = sprintf( "****%10.10d****", $ByteCount);
   
   open(OUTFILE, ">$OutputFilename") or die "Cannot open $OutputFilename for output.";
   print OUTFILE "$BulletinFlagFieldSep\n";
   print OUTFILE "$Header\r\r\n";
   if ($SubHeader =~ /NONE/) {
      print "No Subheader\n";
   } else {
      if ($SubHeader =~ /IMAG/){
         print OUTFILE "$SubHeader";
      } else {
         print OUTFILE "$SubHeader\r\r\n";
      }
   }
   open (INFILE, $Filename) or die "Cannot open $Filename";

   while ($rec=<INFILE>) {
      print OUTFILE $rec;
   }

   close INFILE;
   close OUTFILE;

   print "$Filename -> $OutputFilename\n";
}

