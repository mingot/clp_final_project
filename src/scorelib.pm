# support code library for the KDD Cup 2006 scoring function suites
#
# Usage:
#   In a perl script, insert "use scorelib;" near the top of the
#   file.  The directory in which this library is located must be
#   included on the @INC array.
#
# Copyright Terran Lane, 2006

package scorelib;

use strict;

########## package init code ##########

BEGIN {
    use Exporter   ();
    our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

    # set the version for version checking
    $VERSION     = 1.00;

    @ISA         = qw(Exporter);
    @EXPORT      = qw(&scan_truth &scan_data &scan_mask &scan_bootvecs
		      &filter
		      &sum &mean &std &deciles &range &all &any &lg
		      &init_rptfile &rpt &get_rpthandle &close_rptfile);
    %EXPORT_TAGS = ( );     # eg: TAG => [ qw!name1 name2! ],

    # your exported package globals go here,
    # as well as any optionally exported functions
    @EXPORT_OK   = qw();
}
our @EXPORT_OK;

########## file loader code ##########

# Scan columns from a ground-truth test set data file (not from a
# submission file -- use scan_data for that).  Returns a list of
# structs (i.e., hashrefs) -- one for each data point scanned.  Each
# element of the list contains a "patient" field, giving the ID of the
# patient for this data point, and a "label" field, giving the ID of
# the PE for this item.  If $result->[$i]->{label}==0, then this
# candidate does not correspond to a PE; if $result->[$i]->{label}==X,
# then it corresponds to PE number X.
#
# Inputs:
#	fname (string) -- name of data file to scan in
#
# Outputs:
#	data (array ref) -- ref to list of hashrefs.
#		data->[$i]->{label}: PE label for ith data point
#		data->[$i]->{patient}: Patient ID for ith data point
#
sub scan_truth($) {
    my $data=[];
    my $fname=shift;
    open(INDAT,"< $fname") || die("Couldn't open data file
      '${fname}'");
    while (<INDAT>) {
	chomp;
	my @cols=split;
	push(@{$data},{ "patient" => $cols[0],
		        "label" => $cols[1] });
    }
    return $data;
}

# Scan data from a results submission file (not a ground truth test
# set file -- use scan_truth for that)
#
# Inputs:
#    fname -- file name of competitor's submitted data file
#
# Outputs:
#    data -- array reference to array of booleans
sub scan_data($) {
    my $fname=shift;
    my $data=[];
    open(INDAT,"< $fname") || die("Couldn't open data file
      '${fname}'");
    while (<INDAT>) {
	chomp;
	if ($_!~/^[01]$/) {
	    die("Input error on line " . $. . ": '" . $_ . "'");
	}
	push(@{$data},$_);
    }
    close(INDAT);
    return $data;
}

# Load the contents of a mask file.  If no mask file is specified,
# then generate a mask vector of all ones.
#
# Inputs:
#	mask_fname (string) -- file name of mask data file.  Assumes a
#		single boolean (0/1) per line, with one line for each
#		candidate in the data file.
#	nitems (int) -- number of data items to synthesize, if no mask
#		fname is specified
#
# Outputs:
#	mask_vec -- vector (array) of bits, where "1" indicates that
#		the corresponding candidate should be preserved, and
#		"0" indicates that it should be discarded.
#
# Throws:
#	dies if mask_fname is specified but unreadable or if the
#		number of items in the mask_fname doesn't match nitems
sub scan_mask($$) {
    my $mask_fname=shift;
    my $nitems=shift;
    my $mask_vec;
    if ($mask_fname) {
	open(MASKIN,"< $mask_fname") || die("Couldn't open mask file");
	while (<MASKIN>) {
	    chomp;
	    push(@$mask_vec,$_);
	}
	close(MASKIN);
	if (($#$mask_vec+1)!=$nitems) {
	    die("Wrong number of elements in mask file");
	}
    }
    else {
	$mask_vec=[];
	for (my $i=0;$i<$nitems;++$i) { push(@$mask_vec,1); }
    }
    return $mask_vec;
}

# Load the contents of a bootstrap vector file.  If no file name is
# specified, then generate a "null" bootstrap vector -- just a list of
# the candidate indices in order, with no repeats and no randomization.
#
# Inputs:
#	boot_fname (string) -- file name of bootstrap data file.  Assumes a
#		single vector per line, where each vector is a
#		whitespace-separated list of indices into the
#		(post-masked) ground truth set.
#	nitems (int) -- number of data items to synthesize, if no
#		bootstrap fname is specified
#
# Outputs:
#	boot_vecs -- vector (array) of vectors, where boot_vecs[i] is
#		the ith bootstrap vector and boot_vecs[i][j] is the
#		jth data index to use during the ith iteration of
#		bootstrapping.
#
# Throws:
#	dies if boot_fname is specified but unreadable or if the
#		number of items in the boot_fname vectors doesn't
#		match nitems or if any line of the boot_fname contains
#		the wrong number of items.
sub scan_bootvecs($$) {
    my $boot_fname=shift;
    my $nitems=shift;
    $nitems--;
    my $result=[];
    if ($boot_fname) {
	open(BOOTIN,"< $boot_fname") ||
	    die("Couldn't open bootstrap matrix file");
	while (<BOOTIN>) {
	    chomp;
	    my @scratch=split;
	    if ($#scratch!=$nitems) {
		die("Wrong number of indices on line $. of bootstrap " .
		    "file '$boot_fname': expected $nitems, got " .
		    $#scratch);
	    }
	    push(@$result,\@scratch);
	}
	close(BOOTIN);
    }
    else {
	# we want a single "null" bootstrap vector -- it should just
	# identify every test vector once
	my @scratch=0..$nitems;
	push(@$result,\@scratch);
    }
    return $result;
}

########## dataset manipulation routines ##########

# Filter out a subset of the elements of an array according to the
# values in a mask vector.  When mask(i)!=0, the i'th element of the
# input array is copied.  Otherwise, the i'th element of the input
# array is dropped.
#
# Inputs:
#	arr -- array to mask
#	mask -- bit array of mask values: 1=>copy; 0=>drop
#
# Outputs:
#	result -- filtered copy of arr
sub filter($$) {
    my $arr=shift;
    my $mask=shift;
    my $result=[];
    for (my $i=0;$i<=$#$mask;++$i) {
	if ($mask->[$i]) { push(@$result,$arr->[$i]); }
    }
    return $result;
}

########## arithmatic and statistical routines ##########

# Calculate the sum of elements in a numeric vector
sub sum($) {
    my $aref=shift;
    my $sum=0;
    foreach my $v (@{$aref}) {
	$sum+=$v;
    }
    return $sum;
}

# Calculate the mean of elements in a numeric vector
#
# Inputs:
#	aref: reference to list of data
#	norm: normalization factor (optional; default: count of data
#		in aref)
sub mean($;$) {
    my $aref=shift;
    my $norm=(($#_>-1) ? shift : ($#$aref+1));
    return sum($aref)/$norm;
}

# Calculate the standard deviation of a numeric vector
#
# Inputs:
#	aref: reference to list of data
#	norm: normalization factor (optional; default: count of data
#		in aref)
sub std($;$) {
    my $aref=shift;
    my $norm=(($#_>-1) ? shift : ($#$aref+1));
    my $xbar=mean($aref,$norm);
    my @squared=map { ($_-$xbar)*($_-$xbar) } @$aref;
    my $ssq=sum(\@squared);
    return sqrt($ssq/$norm);
}

# Calculate the deciles of a numeric vector
sub deciles($) {
    my $aref=shift;
    $aref=[sort { $a <=> $b } @$aref];
    my $sz=$#$aref;
    my @idx=1..10;
    @idx=map { int($sz*$_/10) } @idx;
    return [ @{$aref}[@idx] ];
}

# calculate the range of a numeric vector
sub range($) {
    my $aref=shift;
    $aref=[sort { $a <=> $b } @$aref];
    return [ $aref->[0], $aref->[$#$aref] ];
}

# log base 2
sub lg($)    {
    my $v=shift;
    return log($v)/log(2);
}

########## boolean operators ##########

# Utility command: test boolean "all" of all args
sub all {
    my $result=1;
    foreach my $k (@_) { $result&&=$k; }
    return $result;
}

# Utility command: test boolean "any" of all args
sub any {
    my $result=0;
    foreach my $k (@_) { $result||=$k; }
    return $result;
}

########## printing utilities ##########

# This uses a lexical closure gensym as a filehandle for a reporting
# mechanism.  The file handle is initialized when init_rptfile() is
# called and is closed when close_rptfile() is called.  rpt($) prints
# a single string to the file, followed by a newline.  get_rpthandle()
# allows the caller to get a reference to the filehandle so that they
# can send messages there as well.
{
    my $href;

sub init_rptfile($) {
    my $fname=shift;
    open($href,"> $fname") || die("Couldn't open file '${fname}'");
}

sub rpt($) {
    $href && print $href shift() . "\n";
}

sub get_rpthandle() {
    return $href;
}

sub close_rptfile() {
    $href && close($href);
}
}

########## EOF ##########
# module must return 1 (true) to indicate success
1;
