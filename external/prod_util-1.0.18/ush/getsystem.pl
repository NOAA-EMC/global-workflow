#!/usr/bin/perl
use strict;
use feature qw(switch);
use Getopt::Std;

# This script returns the name of the system of the current node.
# It first tries to detect the system and phase using the hostname and the $SITE
# variable.  If unsuccessful, it tries to determine the system using the external
# ip address using the 'ip' command, and the phase using the number of cores on
# the current node.  NOTE: This script is only guaranteed to work on service nodes.

# Options:
#     -o Get OTHER system of the same type
#     -p Append phase of the system (p1, p2, or XC40)
#     -t Get the system type/vendor (IBM or Cray)

#die "uh oh";
my %options=();
getopts("opt", \%options);

sub system_name {
    if (defined $options{'t'}) {
        given ($_[0]) {
            when(['t','g']) { return 'IBM'; }
            when(['l','s']) { return 'Cray'; }
            when(['Theia']) { return 'Theia'; }
            when(['Jet'])   { return 'Jet'; }
            when(['GAEA'])  { return 'GAEA'; }
        }
    } else {
        given ($_[0]) {
            when('t') { return defined $options{'o'} ? 'Gyre' : 'Tide'; }
            when('g') { return defined $options{'o'} ? 'Tide' : 'Gyre'; }
            when('l') { return defined $options{'o'} ? 'Surge' : 'Luna'; }
            when('s') { return defined $options{'o'} ? 'Luna' : 'Surge'; }
            when('Theia') { return 'Theia' }
            when('Jet') { return 'Jet' }
            when('GAEA') { return 'GAEA' }
        }
    }
}

sub get_ibm_phase {
    if ( defined $options{'p'} ) {
        open CPUINFO, "<", "/proc/cpuinfo" or die $!;
        while (<CPUINFO>) {
            if (/cpu cores\s+:\s*(\d+)$/) {
                given ($1) {
                    # Assumes all phase 1 nodes have 8 cores
                    when(8) { close CPUINFO; return '-p1'; }
                    # Assumes all phase 2 nodes have 12 cores
                    when(12) { close CPUINFO; return '-p2'; }
                }
            }
        }
        die "Unable to identify the phase of the IBM iDataPlex system.";
    } else {
        return '';
    }
}

sub get_cray_phase {
    return ( defined $options{'p'} ? '-XC40' : '' );
}


if ( -d '/scratch3' && -d '/scratch4' ) { # Theia or Selene
    print system_name('Theia');
} elsif ( -d '/lfs3/' ) {
    print system_name('Jet');
} elsif ( -d '/lustre' && -d '/ncrc' ) {
    print system_name('GAEA');
} else {
    # WCOSS
    my $hostname=`hostname`;
    if ( $hostname =~ /^(t|g)(\d{1,2})\w\d/ ) {  # IBM iDataPlex nodes
        print system_name($1);
        print ($2 < 20 ? '-p1' : '-p2') if defined $options{'p'};
    } elsif ( $hostname =~ /^nid\d{5}$/ && exists $ENV{'SITE'} ) {  # Cray XC40 nodes
        given ($ENV{'SITE'}) {
            when('LUNA') { print system_name('l') . get_cray_phase(); }
            when('SURGE') { print system_name('s') . get_cray_phase(); }
            default { die "Unable to identify the system. You may not be running on a service node." }
        }
    } else {
        my $external_ip;
        foreach (`/sbin/ip address`) {
            if (/inet ((?:192\.58|140\.90)\.\d{1,3})\.\d{1,3}\//) {
                $external_ip=$1;
                last;
            }
        }
        given ($external_ip) {
            when('192.58.1') { print system_name('t') . get_ibm_phase(); }
        when('192.58.2') { print system_name('g') . get_ibm_phase(); }
            when('192.58.3') { print system_name('l') . get_cray_phase(); }
            when('140.90.226') { print system_name('s') . get_cray_phase(); }
            default { die "Unable to identify the system. You may not be running on a service node." }
        }
    }
}
