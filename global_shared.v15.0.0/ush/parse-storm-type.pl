#! /usr/bin/env perl

use warnings;
use strict;

while(defined($_=<>)) {
    chomp;
    /^((?:NHC |JTWC) [0-59]\d[A-Z]) (\S+)\s+(.*\d\d\d\d\d\d\d\d \d\d\d\d.{34} .. .*)/ or do {
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
	warn "MISSING STORM TYPE (ASSUME XX): $_\n";
    }
    if($tctype =~ /DB|EX|LO|WV|XX/i) {
        warn "Ignoring line due to TC type $tctype: $_";
        next;
    }
    print "$line\n"
}

