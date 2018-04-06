#!/usr/bin/perl
#
# PostXMLPreprocessor.pl
#
# This script will process the post control and available table file 
#   to generate a ACSII output file as the input for Fortran.
#
# Usage: perl PostXMLPreprocessor.pl
#
# Optional arguments:
#    -d debug in verbose mode     - Use this mode to generate flat file with tag name
#                                   For prod, Post task only accept the file w/o tag name
#    -l logging_level             - overrides $Config::default_logging_level
#     0 - debug                     
#     1 - info
#     2 - warn
#     3 - fatal
#
# Required file:
#
#   Both XML file as defined in sub.
#   POST-XML-Library.pl    - XML library provide the interface to the XML file
#                            generate txt file with name tag before data.
#                            For debug purpose only.
#          or
#   POST-XML-Library-NT.pl - XML library provide the interface to the XML file
#                            generate txt file with data only. Intended to use in prod      
#
#   Lin Gan  - 02/23/2015  - New version that run on local Linux box
#
#   Lin Gan  - 03/18/2015  - Apply PMB header for WCOSS job submit
#
#   Lin Gan  - 04/28/2015  - Implement as makefile solution
#
#
#

  
  use strict;
  use warnings;
  use File::Basename;
  use FileHandle;
  use Getopt::Std;
  use XML::LibXML;	                                        # use XML LibXML package
  use POSIX qw(strftime);                                       # for strftime

  sub usage;

#-------------------------------------------------------------- 
# POST XML library
#-------------------------------------------------------------- 
  require 'POST-XML-Library-NT.pl';
 
  sub logmsg($$);
  sub silence_used_once_warnings();
  
#--------------------------------------------------------------  
# Package global variables accessed by functions
#--------------------------------------------------------------

  our $logname="";                                              # Base name of the log file (without the date at the end),
  our $logfile="";                                              # Filename of current log file $log_dir/$logname.<date>
  our $logging_level=1;                                         # level of logging, default or overridden on command lie
  our $log=new FileHandle;        				# Log file handle
  our $make_log_file=0;                                         # When running in batch, standard output and error are captured
  our $XML_dir;							# XML processing dir contain ctrl, available, and flat file
  our $log_dir;							# log file dest dir
  our $ctrl_xml_file;						# postcntrl.xml
  our $avil_xml_file;						# post_avblflds.xml
  our $generate_tag_name;					# If XML library should generate tag name for debug purpose
  our $control_xml_name;					# Post control XMl file name
  our $available_xml_name;					# Post available table XMl file name
  our $flat_file_name;						# Output Post flat file name
  my $filename;							# Output file name
#--------------------------------------------------------------
# Get options
#--------------------------------------------------------------

#  my %options;
#  getopts('l:d:c:a:f', \%options);
#  $control_xml_name = defined $options{c} ? $options{c} : "";
#  $available_xml_name = defined $options{a} ? $options{a} : "";
#  $flat_file_name = defined $options{f} ? $options{f} : "";

my $nargs = @ARGV;
if ( $nargs != 3 )   {
   usage();
   print "Number of input argument $nargs is not match required 3\n";
   exit 1;
}

$control_xml_name = $ARGV[0];
$available_xml_name = $ARGV[1];
$flat_file_name = $ARGV[2];
 
#--------------------------------------------------------------
# check input paramaters
#-------------------------------------------------------------- 
if ( (! $control_xml_name) || (! $available_xml_name) || (! $flat_file_name) )   {
  usage();
  exit 1;
}

#--------------------------------------------------------------  
# Set directory for POST XML flat file default to the ptmp directory
#--------------------------------------------------------------

$XML_dir=$ENV{'PWD'};
(defined($XML_dir) || !$XML_dir) or die  "Environment variable PWD not set.\n";
$log_dir= $ENV{'PWD'};
(defined($log_dir) || !$log_dir) or die  "Environment variable PWD not set.\n";

#--------------------------------------------------------------
# set target file name
#--------------------------------------------------------------

  $ctrl_xml_file = "${XML_dir}/${control_xml_name}";
  $avil_xml_file = "${XML_dir}/${available_xml_name}";

#--------------------------------------------------------------
# Set logging level to default if not set in command switch
#--------------------------------------------------------------

  $logging_level = $Config::default_logging;

#--------------------------------------------------------------
# Set tag name generation option
#--------------------------------------------------------------

  $generate_tag_name = "";

#--------------------------------------------------------------
# Set log file name
#--------------------------------------------------------------

  $logname="POST_XML_Flat_File_processor_POST_FlatFile.log";
 
  logmsg($Config::info, "Begin PostXMLPreprocessor \n");

  logmsg($Config::info, "XML Configuration file read successfully. \n");

#--------------------------------------------------------------
# Generate flat file
  if ($generate_tag_name eq "1") {
# for debug (with tag name) 
    logmsg($Config::info, "Generate debug ONLY flat file as postxconfig.txt \n");
    $filename = "${XML_dir}/postxconfig.txt";
  }
  else {
# for production (without tag name)
    $filename = "${XML_dir}/${flat_file_name}";
  }
#--------------------------------------------------------------

  open(my $fh, '>', $filename) or die "Could not open file '$filename' $!";

#--------------------------------------------------------------  
# calling xml library
#--------------------------------------------------------------

  my @source_array=constract_ctrl_elements($generate_tag_name, $ctrl_xml_file, $avil_xml_file);
  logmsg($Config::info, "Successfully process input XML files. \n");
#--------------------------------------------------------------  
# Loop through array and write out
#--------------------------------------------------------------

  foreach (@source_array) {
	  print $fh "$_\n";
  }
	    
  close($fh);
  
  logmsg($Config::info, "Flat file is new generated. \n");

#------------------------------------------------------------------
#  Print to a dated log file. Only print if logging level of msg
#  is greater than or equal to logging level.
#
#  $logging_level - global level of logging enabled
#                   0=debug, 1=info, 2=warn, 3=fatal
#  $log_msg_level - log level of this msg
#
#  Accesses globals:
#  $log -  file handle
#  $log_dir -  log directory
#  $logfile - name of current log file.
#------------------------------------------------------------------
#

sub logmsg($$) {
   my ($log_msg_level, $msg) = @_;
   my ($foo, $line);

   my $date=strftime("%Y%m%d", gmtime(time));
   my $time=strftime("%H%M%S", gmtime(time));
   my $this_script =  basename ( (caller(0) )[1]);
   ($foo, $foo, $line) = caller;
   my $called_from_line_num = sprintf("%05d", $line);
   my ($new_log, $new_msg);

#--------------------------------------------------------------
# Open the logfile, if we are making one
#--------------------------------------------------------------


#--------------------------------------------------------------
# $new_log="${Config::log_dir}/${main::logname}.${date}";
#--------------------------------------------------------------

      $new_log="${main::logname}.${date}";
      if ( $main::logfile ne $new_log ) {
         $main::log->close if ($main::logfile ne "");
         
#--------------------------------------------------------------
# Set log file name
#--------------------------------------------------------------

         $main::logfile=$new_log;

#--------------------------------------------------------------         
# set log contain
# $main::log=$msg;
#--------------------------------------------------------------         

         my $testt = $main::log;
         
         $testt = $main::logfile;
         $main::log->open (">> $main::logfile") or die "Error opening log file $main::logfile: $?";
      }


   $new_msg = "${time}: [$called_from_line_num]  ${msg}\n";

#--------------------------------------------------------------
# $msg = "${time}: $Config::severity{$log_msg_level} [$called_from_line_num]  ${msg}\n";
#--------------------------------------------------------------

   print $new_msg;

#--------------------------------------------------------------
# print $msg;
#--------------------------------------------------------------

      print $main::log $new_msg or print "Could not write to $main::log: $?\n";

#--------------------------------------------------------------
# print $main::log $msg or print "Could not write to $main::log: $?\n";
#--------------------------------------------------------------

} 
  
#------------------------------------------------------------
# usage
#------------------------------------------------------------

sub usage{
   print "usage: perl PostXMLPreprocessor.pl control_xml_file available_xml_file flat_file\n";
   print "where:\n";
   print "  control_xml_file      : Post control XML file for the model \n";
   print "  available_xml_file    : Post available table file for the model  \n";
   print "  flat_file             : Post flat file name \n";
   print "Please do not attempt to run this utility manually. Using makefile or contact EMC for assistance \n";
}

sub  trim { my $s = shift; $s =~ s/^\s+|\s+$//g; return $s };

#------------------------------------------------------------------
# silence_used_once_warnings
#
# With use strict, if something
# is only used once it produces a warning, thinking it's a typo.
# After verifying that the once-use is intention, add another use
# here to silence the warning.
#
#------------------------------------------------------------------
sub silence_used_once_warnings() {

   $Config::default_logging = $Config::default_logging;
#   $Config::logging_level = $Config::logging_level;
#   $Config::version = $Config::version;
#   $Config::error=$Config::error;
#   $Config::warn = $Config::warn;
#   $Config::severity = $Config::severity;
#   $Config::log_dir = $Config::log_dir;
}

