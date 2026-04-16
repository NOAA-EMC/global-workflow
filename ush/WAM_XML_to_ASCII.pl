#!/usr/bin/env perl

#===============================================================================
#
#  FILE: WAM_XML_to_ASCII.pl
#
#  DESCRIPTION: Parses space weather data (F10.7 and Kp indices) from an XML
#               input file (wam_input2.xsd) and converts it into a formatted
#               ASCII text file (wam_input.asc) suitable for downstream processing.
#
#  INPUT: wam_input2.xsd (Expected to contain 'issue-date',
#        'f10-81-avg-currentday', and an array of 'data-item' nodes).
#  OUTPUT: wam_input.asc (Formatted ASCII table containing 56 records of
#          Date_Time, F10, Kp, F10Flag, and KpFlag).
use strict;
use warnings;

# use module
use XML::Simple;
use Data::Dumper;

# create object
my $xml = new XML::Simple;

# read XML file
my $data = $xml->XMLin("wam_input2.xsd");

# write XML Data to file

open (MYFILE, ">", "wam_input.asc")
 or die "Can't Open MYFILE: $!";

print MYFILE "Issue Date          ", $data->{'issue-date'}, "\n";
print MYFILE "F10 81 Day Avg      ", $data->{'f10-81-avg-currentday'}, " \n";
print MYFILE "Flags:  0=Forecast, 1=Estimated, 2=Observed \n\n";

printf MYFILE " Date_Time                   F10           Kp       F10Flag      KpFlag   \n";
printf MYFILE "-----------------------------------------------------------------------   \n";

 my $counter = 0;

while ($counter < 56) {
 printf MYFILE "%s%12g%12g%12g%12g \n",
       $data->{'data-item'}->[$counter]->{'time-tag'},
       $data->{'data-item'}->[$counter]->{'f10'},
       $data->{'data-item'}->[$counter]->{'kp'},
       $data->{'data-item'}->[$counter]->{'f10-flag'},
       $data->{'data-item'}->[$counter]->{'kp-flag'};
       $counter +=1;
       }

close (MYFILE);

