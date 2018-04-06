#! /usr/bin/env perl

use File::Basename;
use File::Spec;
use English qw{ RS OS_ERROR };
use warnings;
use strict;

my $tmdir=File::Spec->canonpath(File::Spec->rel2abs(dirname(__FILE__)));
my $confdir=dirname(dirname($tmdir))."/src/conf";

my $salt=rand(1000000);

my $cshinc="$confdir/module-setup.csh.inc";
my $shinc="$confdir/module-setup.sh.inc";
my $crontab_bak="crontab-$salt.bak";
my $crontab_new="crontab-$salt.new";

########################################################################

# Ensure needed files are present

my $bad=0;
if(!-s "$cshinc") {
    $bad=1;
    warn "$cshinc: empty or missing\n"
}
if(!-s "$shinc") {
    $bad=1;
    warn "$shinc: empty or missing\n"
}
if($bad) {
    die "Cannot run script.  See earlier messages for details.\n"
}

########################################################################

# Read and store the old crontab

open(CRONTAB,"crontab -l |") or die "crontab -l: cannot execute program: $OS_ERROR";
my $crontab=do {
    local $RS=undef;
    <CRONTAB>
};
close(CRONTAB) or die "crontab: error reading from crontab: $OS_ERROR";

open(OLDTAB,">$crontab_bak") or die "$crontab_bak: cannot open file: $OS_ERROR";
print(OLDTAB $crontab) or die "$crontab_bak: error writing: $OS_ERROR";
close(OLDTAB) or die "$crontab_bak: error closing: $OS_ERROR";
print STDERR "$crontab_bak: backup of crontab before this script modifies it\n";

########################################################################

# Scan for available shells and generate in-memory test scripts for each

print STDERR "Scan for available shells...\n";

my @shells=qw{ /bin/bash /bin/ksh /bin/sh /bin/csh /bin/tcsh };
my %tests;

foreach my $shell ( @shells ) {
    next unless -x "$shell";
    my $shellbn=basename($shell);
    my $modfile=$shinc;
    $modfile=$cshinc if $shell=~/csh/;
    $tests{$shell} = {
        resultfile=>"$tmdir/$shellbn-$salt.result",
        shellfile=>"$tmdir/test-$salt.$shellbn",
        modfile=>"$modfile",
        status=>"unknown",
        script=>"#! $shell
source $modfile
module help
",
                     };
}

########################################################################

# Create on-disk test scripts for each shell

print STDERR "Delete old scripts and results...\n";
foreach my $shell (keys %tests) {
  foreach my $file (qw{ resultfile shellfile }) {
    if(-e $tests{$shell}{$file} || -l $tests{$shell}{$file} ) {
      unlink($tests{$shell}{$file})
        or die "$tests{$shell}{$file}: cannot delete: $OS_ERROR";
    }
  }
}

# Create new scripts:
print STDERR "Create new scripts...\n";
foreach my $shell (keys %tests) {
  open(SHELLTEST,">$tests{$shell}{shellfile}")
    or die "$tests{$shell}{shellfile}: cannot open for write: $OS_ERROR";
  print(SHELLTEST "$tests{$shell}{script}")
    or die "$tests{$shell}{shellfile}: cannot write: $OS_ERROR";
  close(SHELLTEST)
    or die "$tests{$shell}{shellfile}: error closing output file: $OS_ERROR";
  chmod(0700,$tests{$shell}{shellfile})
    or die "$tests{$shell}{shellfile}: cannot chmod 0700: $!";
}

# Create the new crontab:
print STDERR "Generate new crontab...\n";
my $in5minutes=time() + 120;
my $give_up=$in5minutes+120;
my ($lsec,$lmin,$lhour,$lmday,$lmon,@other)=localtime($in5minutes);
$lmon++;
foreach my $shell (keys %tests) {
  $crontab.="
# Test module-setup for $shell:
$lmin $lhour $lmday $lmon * $tests{$shell}{shellfile} > $tests{$shell}{resultfile} 2>&1
";
}

# Write the crontab to a temporary file:
print STDERR "$crontab_new: write crontab to this file\n";
open(CRONNEW,">$crontab_new") or die "$crontab_new: cannot open for writing: $!";
print(CRONNEW $crontab) or die "$crontab_new: cannot write: $!";
close(CRONNEW) or die "$crontab_new: error closing output file: $!";

# Update the crontab:
print STDERR "crontab $crontab_new: update the crontab...\n";
system("crontab $crontab_new < /dev/null");
if($?) {
  die "crontab $crontab_new: exit $? ($!)";
}

# Wait for results:
print STDERR "Wait up to four minutes for results...";
my $processed=0;
while(time()<$give_up && $processed<scalar keys %tests) {
  foreach my $shell (keys %tests) {
    next if $tests{$shell}{status} ne "unknown";
    if(! -s $tests{$shell}{resultfile}) {
      warn "$tests{$shell}{resultfile}: empty or missing\n";
      next;
    }
    my @stat=stat $tests{$shell}{resultfile};
    if(time()-$stat[9]<20) {
      warn "$tests{$shell}{resultfile}: not 20 seconds old yet\n";
      next;
    }

    $tests{$shell}{status}="FAIL";
    $processed++;

    open(RESULTFILE,"<$tests{$shell}{resultfile}") or do {
      warn "$tests{$shell}{resultfile}: cannot open for read: $!";
      next;
    };

    while(defined($_=<RESULTFILE>)) {
      if(/Usage.*odule/) {
        $tests{$shell}{status}="PASS";
        last;
      }
    }

    close(RESULTFILE) or
      warn "$tests{$shell}{resultfile}: error closing input file: $!";

    print "$shell: $tests{$shell}{status}\n";
  }
  print "tick... processed $processed ... tick ...\n";
  sleep 3;
}

#Restore crontab:
system("crontab $crontab_bak < /dev/null");
if($?) {
  die "crontab $crontab_bak: exit $? ($!)";
}

my $passfail="PASS";
foreach my $shell (keys %tests) {
  print "$shell: $tests{$shell}{status}\n";
  $passfail="FAIL" unless $tests{$shell}{status} eq 'PASS';
}
print "RESULT: $passfail\n";
