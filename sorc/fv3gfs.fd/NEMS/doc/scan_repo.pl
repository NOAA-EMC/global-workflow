#! /usr/bin/env perl

my $nems_rev='unknown';
my $app_rev='unknown';
my $nems_loc='nems';
my $app_name='app';
my $app_loc='app';

if( open(FH,'svn info .. |') ) {
    while(defined($_=<FH>)) {
        chomp;
        /^Revision: (\d+)/ and $nems_rev=$1;
        # URL: https://svnemc.ncep.noaa.gov/projects/nems/branches/update-docs
        m(^URL: .*?(branches/\S+|tags/\S+|[^/]+)$) and $nems_loc=$1;
    }
}

if( open(FH,'svn info ../.. |') ) {
    while(defined($_=<FH>)) {
        chomp;
        /^Revision: (\d+)/ and $app_rev=$1;
        #URL: https://svnemc.ncep.noaa.gov/projects/nems/apps/NEMSGSM/trunk
        m(^URL: https://.*?/apps/([^/]+)) and $app_name=$1;
        m(^URL: .*?(branches/\S+|tags/\S+|[^/]+)$) and $app_loc=$1;
    }
}

print(
"nems_rev=\"$nems_rev\"
app_rev=\"$app_rev\"
nems_loc=\"$nems_loc\"
app_loc=\"$app_loc\"
app_name=\"$app_name\"
")
