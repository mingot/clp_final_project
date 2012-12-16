#!/usr/bin/perl -w
#
# Script to select sets of bootstrap vectors.  Generates a file of
# bootstrap vectors, where each row is a single vector and the i'th
# entry of the j'th row indicates the i'th element to include in the
# j'th bootstrap experiment.
#
# Usage:
#   gen_bootstrap_matrix.pl [options]
# See "gen_bootstrap_matrix.pl -h" for details on options
#
# Copyright Terran Lane, 2006

use strict;
use Getopt::Long;
$Getopt::Long::ignorecase=0;

# This is heavy-handed, but I don't see a good way to do this in a
# nicely user/directory structure-independent way.
BEGIN {
    if (exists($ENV{SCORELIBDIR})) {
	push(@INC,$ENV{SCORELIBDIR});
    }
}

use scorelib;

########## subroutines ##########

# Print a usage message and exit
#
# Inputs:
#  ecode (int) -- exit code for program
sub usage($) {
    my $ecode=shift;
    print <<EOF;
Usage: gen_bootstrap_matix.pl [options] > bootstrap_file.dat
  Options:
    A|answers|answer_file (string) -- name of test set (ground truth)
      data file
    M|mask|mask_file (string) -- name of mask data vector file
    n|nsamples (int) -- number of bootstrap samples to generate
    m|multilevel (flag) -- multilevel bootstrapping (as opposed to flat)
    x|null (flag) -- produce "null" vectors -- just 0..(#elements-1)
    s|seed (int) -- specify the PRNG seed number (default: 69)
    d|debug (flag) -- enable debug mode
    h|help (flag) -- print this message and exit
EOF
    exit($ecode);
}

# Pick a bootstrap sample by sampling first from patients and then
# from candidates, given patients.
sub pick_multilev($) {
    my $truth=shift;
    my $result=[];
    # First, compile list of unique patient IDs and corresponding list
    # of candidate indices.
    my %patient_candidates;
    for (my $i=0;$i<=$#$truth;++$i) {
	my $this_pat=$truth->[$i]->{patient};
	if (!exists($patient_candidates{$this_pat})) {
	    $patient_candidates{$this_pat}=[];
	}
	push(@{$patient_candidates{$this_pat}},$i);
    }
    my @patients=sort { $a <=> $b } keys(%patient_candidates);
    # now we're ready to pick the actual samples
    for (my $i=0;$i<=$#$truth;++$i) {
	# first, pick a patient
	my $pat=$patients[int(rand($#patients+1))];
	# now pick a candidate index, given that patient ID
	my $count=$#{$patient_candidates{$pat}}+1;
	my $idx=$patient_candidates{$pat}->[int(rand($count))];
	push(@$result,$idx);
    }
    return $result;
}

# Generate a "flat" bootstrap sample.  Basically just resamples the
# data as-is, without regard to multiple-instance selection.
#
# Inputs:
#	$truth -- ground truth file structure, as returned by scan_truth
# Outputs:
#	sampleref -- reference to array of data element indices
sub pick_flat($) {
    my $truth=shift;
    my $sampleref=[];
    for (my $i=0;$i<=$#$truth;++$i) {
	push(@$sampleref,int(rand($#$truth+1)));
    }
    return $sampleref;
}

########## main program ##########

#### option processing ####

# variable initialization and defaults

my $answers_fname="";
my $mask_fname="";
my $nrows=1;
my $seed=69;
my $debug=0;
my $multilev=0;
my $nullrand=0;

my %allopts = ( "A|answers|answer_file=s"	=> \$answers_fname,
		"M|mask|mask_file=s"		=> \$mask_fname,
		"n|nsamples=i"			=> \$nrows,
		"m|mulilevel"			=> \$multilev,
		"x|null"			=> \$nullrand,
		"s|seed=i"			=> \$seed,
		"d|debug"			=> \$debug,
		"h|help"	       		=> sub { usage(0); }
		);
if (!GetOptions(%allopts)) {
    exit(1);
}
if ($debug) {
    print <<EOF;
answers_fname=$answers_fname
mask_fname=$mask_fname
nrows=$nrows
multilevel=$multilev
rand seed=$seed
EOF
}

# first, load the ground truth file
my $truth=scan_truth($answers_fname);

# now filter it, if necessary
my $mask_vec=scan_mask($mask_fname,$#$truth+1);
$truth=filter($truth,$mask_vec);
if ($debug) {
    foreach my $k (@$truth) {
	print "[DBG:truth]: p=" . $k->{patient} . "\tl=" .
	    $k->{label} . "\n";
    }
}

# start of main generation loop

# Could make this cleaner and more object-oriented-y by using
# subroutine refs and stripping out the case statement.  Oh well.
for (my $n=0;$n<$nrows;++$n) {
    if ($nullrand) {
	print join("\t",0..($#$truth)) . "\n";
    }
    elsif ($multilev) {
	print join("\t",@{pick_multilev($truth)}) . "\n";
    }
    else {
	print join("\t",@{pick_flat($truth)}) . "\n";
    }
}
