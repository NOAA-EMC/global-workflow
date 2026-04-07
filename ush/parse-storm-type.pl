#! /usr/bin/env perl

# DESCRIPTION: Parses lines of tropical cyclone data found in the tcvitals file.
#              It specifically looks for records from NHC (National Hurricane
#              Center) or JTWC (Joint Typhoon Warning Center).
#
# FUNCTIONALITY:
# 1. Validation: Matches lines against a specific pattern to ensure they contain
#    the required identifiers, storm name, and date/time group. Invalid lines
#    are ignored with a warning.
# 2. Reformatting: Truncates the storm name/identifier to 9 characters and
#    reformats the output line, left-justifying the name.
# 3. Filtering: Checks the 2-character tropical cyclone type field (expected at
#    character 150) and ignores any records classified as non-tropical/unclassified:
#    DB (Disturbance), EX (Extra-tropical), LO (Low), WV (Wave), or XX (Unknown).
# 4. Output: Prints/returns only the reformatted lines that represent a classified
#    tropical cyclone.


use warnings;
use strict;

while(defined($_=<>)) {
    chomp;
    /^((?:NHC |JTWC) [0-589]\d[A-Z]) (\S+)\s+(.*\d\d\d\d\d\d\d\d \d\d\d\d.{34} .. .*)/ or do {
        warn "Ignoring invalid line: \"$_\"\n";
        next;
    };
    my ($a1,$b1,$c1)=($1,$2,$3);
    if(length($b1)>9) {
        $b1=substr($b1,0,9);
    }

    my $line=sprintf("%s %-9s %s",$a1,$b1,$c1);
    my $tctype = 'ZZ';
    if(length($line)>=152) {
	$tctype=substr($_,150,2);
    } else {
	# warn "MISSING STORM TYPE (ASSUME XX): $_\n";
	warn "NO STORM TYPE: $_\n";
    }
    if($tctype =~ /DB|EX|LO|WV|XX/i) {
        warn "Ignoring line due to TC type $tctype: $_";
        next;
    }
    print "$line\n"
}

