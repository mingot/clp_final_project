#!/usr/bin/perl -w
#
# validator.pl
# Validator script for the 2006 KDD Cup results submission data
# files.  Teams can run this script on their results files to ensure
# that they obey the submission format requirements.  Similar code
# will be used in the final evaluation and scoring scripts.
#
# Usage:
#   validator.pl resultsfile.dat
# or (on a UNIX-derived system)
#   validator.pl < resultsfile.dat
#
# Output:
# The program will output one line for each error it finds in your
# input, plus a short summary at the end.  The output looks like:
#
#    [error report lines]
#    ----
#    Total errors=0
#    Expected number of labels=1391
#    Total labels read=3 (ERROR)
#    Label sum=0
#
# "Total errors" reports the number of errors encountered during
#   parsing.
# "Expected number of labels" reports the number of labels that we
#   expect to find (i.e., the total number of candidates in the test set
#   -- currently 1391, but that may change)
# "Total labels read" reports the total number of (valid) label lines
#   that we found in your input file.  This will report ERROR if it
#   doesn't match the expected number of labels or OK if it does.
# "Label sum" is the sum of all label values that we find (as a
#   checksum for your sake)
#
# The "#!/usr/bin/perl" line at the top of this file may need to be
# changed to conform to local paths.
#
# Copyright Terran Lane, 2006

use strict;

my $err_count=0;
my $label_sum=0;
my $labels_read=0;
my $expected=1391;
while (<>) {
    chomp;
    if ($_!~/^[01]$/) {
	print "ERR[line " . $. . "]: got '" . $_ .
	    "'; expected '0' or '1'\n";
	++$err_count;
	next;
    }
    $label_sum+=$_;
    ++$labels_read;
}
print "----\nTotal errors=" . $err_count . "\n";
print "Expected number of labels=" . $expected . "\n";
print "Total labels read=" . $labels_read . 
    (($labels_read==$expected) ? " (OK)" : " (ERROR)") . "\n";
print "Label sum=" . $label_sum . "\n";
