#!/usr/bin/perl -w
#
# score.pl
# This script implements the scoring rules for the 2006 KDD Cup
# competition.
#
# Usage:
#   score.pl [options]
# see "score.pl -h" for details on options.
#
# If this script is being run from a directory other than the one in
# which the scorelib.pm file is located, you must ensure that the
# scorelib.pm directory is on the @INC directory for your perl
# interpreter.  You can either set the @INC directory directly, or set
# the environment variable $SCORELIBDIR to point to the directory in
# which scorelib.pm is located.  Example (csh syntax):
#    setenv SCORELIBDIR /path/to/scorelib/dir/
#    score.pl [options]
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
    print <<'EOF';
Usage: score.pl [options]
  Options:
    A|answers|answer_file (string) -- name of test set (ground truth)
      data file
    B|bootstrap|bootstrap_file (string) -- name of file containing
      bootstrap resampling vectors
    M|mask|mask_file (string) -- name of file containing candidate
      mask data
    t1a (fname) -- load classifier data from filename and run Task1a
                   test on it
    t1b (fname) -- load classifier data from filename and run Task1b
                   test on it
    t1c (fname) -- load classifier data from filename and run Task1c
                   test on it
    t2a (fname) -- load classifier data from filename and run Task2a
                   test on it
    t2b (fname) -- load classifier data from filename and run Task2b
                   test on it
    t2c (fname) -- load classifier data from filename and run Task2c
                   test on it
    t3 (fname) -- load classifier data from filename and run Task3
                   test on it
    R (fname) -- dump raw results to fname
    d|debug (flag) -- enable debug mode
    h|help (flag) -- print this message and exit
NOTE: -A and at least one task must be specified.  All sub-tasks of a
task must be specified (e.g., if you give t1a, you must also give t1b
and t1c).
EOF
    exit($ecode);
}

# The core function that tallies up the results and compiles the
# aggregate score report.  The resulting report is 'raw' -- it has not
# been assessed for disqualification w.r.t. the TP thresholds given in
# the task document.
#
# Inputs:
#    truth -- ground truth data set, as returned via scan_truth.
#    submit -- vector of submitted classifications, as returned via
#      scan_data
#    bvec -- bootstrap instance selection vector
#    debug -- boolean debug flag -- enables debugging output
#
# Returns:
#    result -- ref to hash containing all tallied results
sub score($$$;$) {
    my $truth=shift;
    my $submit=shift;
    my $bvec=shift;
    my $debug=(($#_>-1) ? shift : 0);

    # start with a validation check
    if ($#$truth!=$#$submit) {
	die("Wrong number of labels in submission file (" .
	    $#$submit . " found, " . $#$truth . " expected)");
    }
    # Now remap truth and submit vectors according to the bootstrap
    # index vector.  This avoids needing to remap each element
    # individually as we go, which would be a royal pain and highly
    # bug-prone.  (This icky bit of syntax uses the perl array slicing
    # mechanism, array references for both truth/submit and bvec, and
    # the array->reference construction mechanism.  Ugh.)
    $truth=[@{$truth}[@$bvec]];
    $submit=[@{$submit}[@$bvec]];

    # This is a bit of a hack -- it assumes that PE IDs are unique
    # across patients (the same PE ID does not occur in two patients)
    # -- but that seems to be the case in the training data.  Be sure
    # to run test_PEs.pl to verify this on the final training data.
    my %tp_per_patient;		# did they find >=1 true PE in this patient?
    my %raw_pe_tp;		# did they find this PE at least once?
    my %fp_per_patient;		# how many FPs in this patient?
    my %pe_per_patient;		# count of PEs in each patient
    my %tp_id_by_patient;	# which PEs did they find in each patient?
    my %patient_ids;		# set of all unique patients
    my $patient_count=0;	# count of unique patient IDs
    my %pe_ids;			# set of all unique PE ids
    my %true_neg_patients;	# set of all patients that have no PEs
    my %class_neg_patients;	# set of all patients classified as neg
    my $pe_count=0;		# count of unique PE IDs
    my $result={
	"TPcount" => 0,
	"TPrate" => 0,
	"TNcount" => 0,
	"TNrate" => 0,
	"FPcount" => 0,
	"FPrate" => 0,
	"FNcount" => 0,
	"FNrate" => 0,
	"meanFP" => 0,
	"rawPEsensitivity" => 0,
	"meanPEsensitivity" => 0,
	"patientSensCount" => 0,
	"patientSensRate" => 0,
	"TNpatients" => 0,
	"FNpatients" => 0,
	"NPV" => 0,
	};
    for (my $i=0;$i<=$#$truth;++$i) {
	my $this_patient=$truth->[$i]->{patient};
	my $this_pe=$truth->[$i]->{label};
	if ($debug) {
	    print "DBG: t[${i}]=" . $this_pe . " s[${i}]=" .
		$submit->[$i] . " ";
	}
	$patient_ids{$this_patient}=1;
	$pe_ids{$this_pe}=1;
	if (!exists($true_neg_patients{$this_patient})) {
	    $true_neg_patients{$this_patient}=1;
	}
	if (!exists($class_neg_patients{$this_patient})) {
	    $class_neg_patients{$this_patient}=1;
	}
	$true_neg_patients{$this_patient}&&=!($this_pe);
	$class_neg_patients{$this_patient}&&=!($submit->[$i]);
	if ($this_pe==0) {		# not a PE
	    if ($submit->[$i]==0) {		# got it right
		$result->{TNcount}++;
		if ($debug) { print "TN\n"; }
	    }
	    else {				# whups!  FP!
		$result->{FPcount}++;
		$fp_per_patient{$this_patient}++;
		if ($debug) { print "FP\n"; }
	    }
	}
	else {				# truth->[$i]>0 ==> really a PE
	    $pe_per_patient{$this_patient}->{$this_pe}=1;
	    if ($submit->[$i]==1) {	# they found it!
		$result->{TPcount}++;
		$tp_per_patient{$this_patient}=1;
		$raw_pe_tp{$this_pe}=1;
		$tp_id_by_patient{$this_patient}->{$this_pe}=1;
		if ($debug) { print "TP\n"; }
	    }
	    else {			# they missed it!
		$result->{FNcount}++;
		if ($debug) { print "FN\n"; }
	    }
	}
    }
    $patient_count=scalar keys(%patient_ids);
    $pe_count=scalar keys(%pe_ids);
    $result->{FPrate}=$result->{FPcount}/($result->{FPcount}+$result->{TNcount});
    $result->{FNrate}=$result->{FNcount}/($result->{FNcount}+$result->{TPcount});
    $result->{TPrate}=$result->{TPcount}/($result->{FNcount}+$result->{TPcount});
    $result->{TNrate}=$result->{TNcount}/($result->{FPcount}+$result->{TNcount});
    my $sum=0;
    foreach my $k (keys(%fp_per_patient)) {
	$sum+=$fp_per_patient{$k};
    }
    $result->{meanFP}=$sum/$patient_count;
    $result->{rawPEsensitivity}=scalar keys(%raw_pe_tp);
    $sum=0;
    # PE sensitivity is defined as the _number_ (not fraction) of PEs
    # correctly identfied in a single patient.  Thus, mean PE
    # sensistivity is sum(PE sens for each patient)/number patients.
    # Note that a patient that doesn't contain any real PEs counts
    # zero on this scale, but affects all classifiers equally.
    foreach my $k (keys(%pe_per_patient)) {
	$sum+=scalar keys(%{$tp_id_by_patient{$k}});
    }
    $result->{meanPEsensitivity}=$sum/$patient_count;
    # number of patients correctly identified
    $result->{patientSensCount}=scalar keys(%tp_per_patient);
    # patient sensitivity rate=
    #    (# pos patients identified)/(total # pos patients)
    $result->{patientSensRate}=(scalar keys(%tp_per_patient))/
	(scalar keys(%pe_per_patient));
    # Finally, calculate the negative prediction value:
    # TN=# of correctly identified negative patients
    # FN=# of patients incorrectly marked as negative
    # NPV=TN/(TN+FN)
    my $TN_pats=0;
    my $FN_pats=0;
    foreach my $k (keys(%class_neg_patients)) {
	if ($debug) {
	    print "patient($k): truth=" . $true_neg_patients{$k} .
		"  classified=" . $class_neg_patients{$k} . "\n";
	}
	# 1=>they did label this patient as negative; keep going
	# 0=>they ended up labeling this patient as positive; skip
	if (!$class_neg_patients{$k}) { next; }
	if ($true_neg_patients{$k}) {
	    $TN_pats++;
	}
	else {
	    $FN_pats++;
	}
    }
    if ($debug) {
	print ">>> Found ${TN_pats} true neg and ${FN_pats} false neg\n";
    }
    $result->{TNpatients}=$TN_pats;
    $result->{FNpatients}=$FN_pats;
    # handle: what if they didn't label _anybody_ negative?
    if ($TN_pats+$FN_pats==0) {
	$result->{NPV}=0;
    }
    else {
	$result->{NPV}=($TN_pats)/($TN_pats+$FN_pats);
    }
    return $result;
}

# determine whether a result set is disqualified for violation of
# Task1 threshold standards
#
# Inputs:
#   res1a -- results of 1a submission, as returned by score()
#   res1b -- results of 1b submission, as returned by score()
#   res1c -- results of 1c submission, as returned by score()
#
# Returns:
#   qualified -- boolean: 1 if result set qualifies, else 0
sub qualified1($$$) {
    my $res1a=shift;
    my $res1b=shift;
    my $res1c=shift;
    my $qualified=1;
    $qualified&&=($res1a->{meanFP}<=2);
    $qualified&&=($res1b->{meanFP}<=4);
    $qualified&&=($res1c->{meanFP}<=10);
    return $qualified;
}

# determine whether a result set is disqualified for violation of
# Task2 threshold standards
#
# Inputs:
#   res1a -- results of 1a submission, as returned by score()
#   res1b -- results of 1b submission, as returned by score()
#   res1c -- results of 1c submission, as returned by score()
#
# Returns:
#   qualified -- boolean: 1 if result set qualifies, else 0
sub qualified2($$$) {
    # The qualification rules for task 2 are actually the same as for
    # task 1.
    return qualified1(shift,shift,shift);
}

# determine whether a result set is disqualified for violation of
# Task3 threshold standards
#
# Inputs:
#   res -- result data structure, as returned by score()
#
# Returns:
#   qualified -- boolean: 1 if result set qualifies, else 0
sub qualified3($) {
    my $res=shift;
    my $qualified=(abs($res->{NPV}-1)<1e-4);
}

# Run all scoring for Task 1 (for each of the three sub-parts) and
# return a summary of the score and whether this entry qualified.
#
# Inputs:
#   truth -- the ground truth data, as returned by scan_truth
#   datA -- classifier data for sub-task A, as returned by scan_data
#   datB -- classifier data for sub-task B, as returned by scan_data
#   datC -- classifier data for sub-task C, as returned by scan_data
#   bvec -- bootstrap instance selection vector, as returned by scan_bootvecs
#
# Outputs:
#   result -- data structure (hashref) with one "results" entry for each
#     sub-task (as emitted by score()) and a "qualified" boolean
sub run_task1($$$$$) {
    my $truth=shift;
    my $datA=shift;
    my $datB=shift;
    my $datC=shift;
    my $bvec=shift;
    my $result;
    $result->{subAres}=score($truth,$datA,$bvec);
    $result->{subBres}=score($truth,$datB,$bvec);
    $result->{subCres}=score($truth,$datC,$bvec);
    $result->{qualified}=qualified1($result->{subAres},
				    $result->{subBres},
				    $result->{subCres});
    return $result;
}

# Run all scoring for Task 2 (for each of the three sub-parts) and
# return a summary of the score and whether this entry qualified.
#
# Inputs:
#   truth -- the ground truth data, as returned by scan_truth
#   datA -- classifier data for sub-task A, as returned by scan_data
#   datB -- classifier data for sub-task B, as returned by scan_data
#   datC -- classifier data for sub-task C, as returned by scan_data
#   bvec -- bootstrap instance selection vector, as returned by scan_bootvecs
#
# Outputs:
#   result -- data structure (hash) with one "results" entry for each
#     sub-task (as emitted by score()) and a "qualified" boolean
sub run_task2($$$$$) {
    my $truth=shift;
    my $datA=shift;
    my $datB=shift;
    my $datC=shift;
    my $bvec=shift;
    my $result;
    $result->{subAres}=score($truth,$datA,$bvec);
    $result->{subBres}=score($truth,$datB,$bvec);
    $result->{subCres}=score($truth,$datC,$bvec);
    $result->{qualified}=qualified2($result->{subAres},
				    $result->{subBres},
				    $result->{subCres});
    return $result;
}

# Run all scoring for Task 3 (which only has one sub-task) and
# return a summary of the score and whether this entry qualified.
#
# Inputs:
#   truth -- the ground truth data, as returned by scan_truth
#   data -- classifier data for Task 3, as returned by scan_data
#   bvec -- bootstrap instance selection vector, as returned by scan_bootvecs
#
# Outputs:
#   result -- data structure (hash) with a single "t3res" field giving
#	   the results for this task (as emitted by score()) and a
#	   "qualified" boolean
sub run_task3($$$) {
    my $truth=shift;
    my $data=shift;
    my $bvec=shift;
    my $result;
    $result->{t3res}=score($truth,$data,$bvec);
    $result->{qualified}=qualified3($result->{t3res});
    return $result;
}

# Compute a summary of the bootstrap test results.
#
# Inputs:
#	res_list: list of individual subtask results, as returned by
#		score().  Should contain one entry for each
#		iteration of the bootstrap test.
#	qual_list: bit list of which entries are "qualified" -- which
#		passed validation for this sub-task.
#
# Outputs:
#	result: single structure (hash) containing the summaries of
#		each category from the score() result.  Most of these
#		come with both a mean across the bootstrap, a standard
#		deviation, and a set of deciles.
sub summarize_bootstrap($$) {
    my $res_list=shift;
    my $qual_list=shift;
    my $meanFP=[map { $_->{meanFP} } @$res_list];
    # the sensitivity vectors need to be filtered so as to drop the
    # disqualified bootstrap samples
    my $rawPEsens=[map { $_->{rawPEsensitivity} } @$res_list];
    $rawPEsens=filter($rawPEsens,$qual_list);
    # Handle the possibility that _nothing_ qualifies.
    if ($#$rawPEsens<0) { $rawPEsens=[0]; }
    my $meanPEsens=[map { $_->{meanPEsensitivity} } @$res_list];
    $meanPEsens=filter($meanPEsens,$qual_list);
    if ($#$meanPEsens<0) { $meanPEsens=[0]; }
    my $patSensCount=[map { $_->{patientSensCount} } @$res_list];
    $patSensCount=filter($patSensCount,$qual_list);
    if ($#$patSensCount<0) { $patSensCount=[0]; }
    my $NPV=[map { $_->{NPV} } @$res_list];
    $NPV=filter($NPV,$qual_list);
    if ($#$NPV<0) { $NPV=[0]; }
    my $patTN=[map { $_->{TNpatients} } @$res_list];
    $patTN=filter($patTN,$qual_list);
    if ($#$patTN<0) { $patTN=[0]; }
    my $total_count=$#$res_list+1;
    my $result={
	"totalQualified" => sum($qual_list),
	"TPcount" => mean([map { $_->{TPcount} } @$res_list]),
	"FPcount" => mean([map { $_->{FPcount} } @$res_list]),
	"TNcount" => mean([map { $_->{TNcount} } @$res_list]),
	"FNcount" => mean([map { $_->{FNcount} } @$res_list]),
	"meanFP" => mean($meanFP),
	"stdFP" => std($meanFP),
	"decFP" => deciles($meanFP),
	"rangeFP" => range($meanFP),
	"rawPEsensitivity" => mean($rawPEsens,$total_count),
	"stdRawPEsens" => std($rawPEsens,$total_count),
	"decRawPEsens" => deciles($rawPEsens),
	"rangeRawPEsens" => range($rawPEsens),
	"meanPEsensitivity" => mean($meanPEsens,$total_count),
	"stdMeanPEsens" => std($meanPEsens,$total_count),
	"decMeanPEsens" => deciles($meanPEsens),
	"rangeMeanPEsens" => range($meanPEsens),
	"patientSensCount" => mean($patSensCount,$total_count),
	"stdPatSensCount" => std($patSensCount,$total_count),
	"decPatSensCount" => deciles($patSensCount),
	"rangePatSensCount" => range($patSensCount),
	"patientSensRate" => mean([map { $_->{patientSensRate} }
				        @$res_list]),
	"NPV" => mean($NPV,$total_count),
	"stdNPV" => std($NPV,$total_count),
	"decNPV" => deciles($NPV),
	"rangeNPV" => range($NPV),
	"TNpatients" => mean($patTN,$total_count),
	"stdTNPat" => std($patTN,$total_count),
	"decTNPat" => deciles($patTN),
	"rangeTNPat" => range($patTN),
    };
    return $result;
}

# Print a human-readable and (hopefully) machine-parseable report of
# a score result for a single sub-task.
#
# Inputs:
#  result -- result data structure, as output by score()
#  leader -- string used to lead each line
#  fhandle (optional) -- file handle to dump to
#
# Outputs:
#  None; just prints to STDOUT
sub report($$;$) {
    my $result=shift;
    my $leader=shift;
    my $fref;
    if ($#_>=0) {
	$fref=shift;
    }
    else {
	$fref=*STDOUT{IO};
    }
    my $decFPStr=join(" ",@{$result->{decFP}});
    my $rangeFPStr=join(" ",@{$result->{rangeFP}});
    my $decRawPESensStr=join(" ",@{$result->{decRawPEsens}});
    my $decMeanPESensStr=join(" ",@{$result->{decMeanPEsens}});
    my $rangeRawPESensStr=join(" ",@{$result->{rangeRawPEsens}});
    my $rangeMeanPESensStr=join(" ",@{$result->{rangeMeanPEsens}});
    my $decPatSensCountStr=join(" ",@{$result->{decPatSensCount}});
    my $rangePatSensCountStr=join(" ",@{$result->{rangePatSensCount}});
    my $decNPVStr=join(" ",@{$result->{decNPV}});
    my $rangeNPVStr=join(" ",@{$result->{rangeNPV}});
    my $decTNPatStr=join(" ",@{$result->{decTNPat}});
    my $rangeTNPatStr=join(" ",@{$result->{rangeTNPat}});
    print $fref <<EOF;
[$leader] TP count=$result->{TPcount}
[$leader] FP count=$result->{FPcount}
[$leader] TN count=$result->{TNcount}
[$leader] FN count=$result->{FNcount}
[$leader] mean per-patient FP=$result->{meanFP}
[$leader] std per-patient FP=$result->{stdFP}
[$leader] decile per-patient FP=[ $decFPStr ]
[$leader] range per-patient FP=[ $rangeFPStr ]
[$leader] raw sensitivity=$result->{rawPEsensitivity}
[$leader] std raw sensitivity=$result->{stdRawPEsens}
[$leader] decile raw sensitivity=[ $decRawPESensStr ]
[$leader] range raw sensitivity=[ $rangeRawPESensStr ]
[$leader] mean sensitivity=$result->{meanPEsensitivity}
[$leader] std mean sensitivity=$result->{stdMeanPEsens}
[$leader] decile mean sensitivity=[ $decMeanPESensStr ]
[$leader] range mean sensitivity=[ $rangeMeanPESensStr ]
[$leader] patient sensitivity count=$result->{patientSensCount}
[$leader] std patient sensitivity count=$result->{stdPatSensCount}
[$leader] decile patient sensitivity count=[ $decPatSensCountStr ]
[$leader] range patient sensitivity count=[ $rangePatSensCountStr ]
[$leader] patient sensitivity rate=$result->{patientSensRate}
[$leader] NPV=$result->{NPV}
[$leader] std NPV=$result->{stdNPV}
[$leader] decNPV=[ $decNPVStr ]
[$leader] rangeNPV=[ $rangeNPVStr ]
[$leader] patient-level TN rate=$result->{TNpatients}
[$leader] std patient-level TN rate=$result->{stdTNPat}
[$leader] deciles patient-level TN rate=[ $decTNPatStr ]
[$leader] range patient-level TN rate=[ $rangeTNPatStr ]
EOF
}

########## main program ##########

#### option processing ####

# variable initialization and defaults
my $answers_fname="";
my $mask_fname="";
my $boot_fname="";
my $task_1a=0;
my $task_1b=0;
my $task_1c=0;
my $task_2a=0;
my $task_2b=0;
my $task_2c=0;
my $task_3=0;
my $raw_fname="";
my $debug=0;

my %allopts = ("A|answers|answer_file=s"	=>  \$answers_fname,
	       "M|mask|mask_file=s"		=>  \$mask_fname,
	       "B|bootstrap|bootstrap_file=s"	=>  \$boot_fname,
	       "t1a=s"				=>  \$task_1a,
	       "t1b=s"				=>  \$task_1b,
	       "t1c=s"				=>  \$task_1c,
	       "t2a=s"				=>  \$task_2a,
	       "t2b=s"				=>  \$task_2b,
	       "t2c=s"				=>  \$task_2c,
	       "t3=s"				=>  \$task_3,
               "R=s"                            =>  \$raw_fname,
	       "d|debug"			=> \$debug,
	       "h|help"				=> sub { usage(0); }
	       );
if (!GetOptions(%allopts)) {
    exit(1);
}
if ($debug) {
    print <<EOF;
answers_fname=$answers_fname
mask_fname=$mask_fname
boot_fname=$boot_fname
t1a=$task_1a
t1b=$task_1b
t1c=$task_1c
t2a=$task_2a
t2b=$task_2b
t2c=$task_2c
t3=$task_3
R=$raw_fname
debug=$debug
EOF
}

# open the raw reporting file, if requested
if ($raw_fname) {
    init_rptfile($raw_fname);
}

##### load and validate data #####
# Get the answer set
if (!$answers_fname) {
    usage(1);
}
my $truth=scan_truth($answers_fname);
if ($debug) {
    print "#truth=" . ($#$truth+1) . "\n";
}

# get the mask set
my $mask_vec=scan_mask($mask_fname,$#$truth+1);
# filter the truth file
$truth=filter($truth,$mask_vec);
if ($debug) {
    print "#filter(truth)=" . ($#$truth+1) . "\n";
}

# Get the bootstrap vectors.  Note: we assume that the bootstrap
# matrix has been pre-filtered, so that the IDs in the bootstrap
# vectors correspond to the row indices of our matrices.
my $boot_vecs=scan_bootvecs($boot_fname,$#$truth+1);
if ($debug) {
    print "Bootstrap matrix:\n";
    print "#cols=" . ($#$boot_vecs+1) . "\n";
    for (my $i=0;$i<=$#$boot_vecs;++$i) {
	print "\t[" . $i . "] #rows=" . ($#{$boot_vecs->[$i]}+1) . "\n";
    }
}

# Did they specifiy at least one task?
if (!any($task_1a,$task_1b,$task_1c,$task_2a,$task_2b,$task_2c,$task_3)) {
    print STDERR "Error: Didn't request any tasks\n";
    usage(1);
}

# Did they ask for task 1?  If so, run it.
if (any($task_1a,$task_1b,$task_1c)) {
    if (!all($task_1a,$task_1b,$task_1c)) {
	print STDERR "Error: Requested some, but not all, task1 sub-tasks\n";
	usage(1);
    }
    print "=" x 20 . " TASK 1 " . "=" x 20 . "\n";
    my $subAdata=filter(scan_data($task_1a),$mask_vec);
    my $subBdata=filter(scan_data($task_1b),$mask_vec);
    my $subCdata=filter(scan_data($task_1c),$mask_vec);
    my $all_results=[];
    foreach my $bvec (@$boot_vecs) {
        push(@$all_results,
	     run_task1($truth,$subAdata,$subBdata,$subCdata,$bvec));
    }
    print "[T1] " . ($#$all_results+1) . " bootstrap scores\n";
    my $qualvec=[map { $_->{qualified} } @$all_results];
    my $subAsum=summarize_bootstrap([map { $_->{subAres} } @$all_results],
				    $qualvec);
    my $subBsum=summarize_bootstrap([map { $_->{subBres} } @$all_results],
				    $qualvec);
    my $subCsum=summarize_bootstrap([map { $_->{subCres} } @$all_results],
				    $qualvec);
    report($subAsum,"T1A");
    report($subBsum,"T1B");
    report($subCsum,"T1C");
    printf("[T1summary] total result :" . "%0.3f\t" x 6 . "\n",
	   $subAsum->{meanPEsensitivity},
	   $subBsum->{meanPEsensitivity},
	   $subCsum->{meanPEsensitivity},
	   $subAsum->{stdMeanPEsens},
	   $subBsum->{stdMeanPEsens},
	   $subCsum->{stdMeanPEsens}
	   );
    print "[T1summary] " . sum($qualvec) .
          "/" . ($#$all_results+1) . " qualified\n";
    print "=" x 48 . "\n";
    # finally, if we need to see the full, raw results, dump them here
    if ($raw_fname) {
        rpt("===== Raw results =====");
	my $count=0;
	foreach my $resAll (@$all_results) {
	    rpt("== Sample ${count} ==");
	    # report expects the fields decFP, rangeFP, etc., which
	    # are added by summarize_bootstrap.  But, for verification
	    # purposes, we want to avoid using summarize_bootstrap, so
	    # we add these fields here manually
	    my $resApart=$resAll->{subAres};
	    my $resBpart=$resAll->{subBres};
	    my $resCpart=$resAll->{subCres};
	    foreach my $extra_field ( "stdFP",
				      "stdRawPEsens",
				      "stdMeanPEsens",
				      "stdPatSensCount",
				      "stdNPV",
				      "stdTNPat" ) {
		$resApart->{$extra_field}="infty";
		$resBpart->{$extra_field}="infty";
		$resCpart->{$extra_field}="infty";
	    }
	    foreach my $extra_field ( "decFP", "rangeFP",
				      "decRawPEsens", "rangeRawPEsens",
				      "decMeanPEsens", "rangeMeanPEsens",
				      "decPatSensCount", "rangePatSensCount",
				      "decNPV", "rangeNPV",
				      "decTNPat", "rangeTNPat" ) {
		$resApart->{$extra_field}=[];
		$resBpart->{$extra_field}=[];
		$resCpart->{$extra_field}=[];
	    }
	    report($resApart,"T1ARaw[${count}]",get_rpthandle());
	    report($resBpart,"T1BRaw[${count}]",get_rpthandle());
	    report($resCpart,"T1CRaw[${count}]",get_rpthandle());
	    my $qualA=( ($resApart->{meanFP}<=2) ? 1 : 0 );
	    my $qualB=( ($resBpart->{meanFP}<=4) ? 1 : 0 );
	    my $qualC=( ($resCpart->{meanFP}<=10) ? 1 : 0 );
	    rpt("T1meanFPall[${count}]: [ " .
		$resApart->{meanFP} . " " .
		$resBpart->{meanFP} . " " .
		$resCpart->{meanFP} . " " . "]");
	    rpt("T1Qualified[${count}]: [ " .
		$qualA . " " .
		$qualB . " " .
		$qualC . " " .
		"] => " . ($resAll->{qualified} ? 1 : 0 ));
	    $count++;
	}
    }
}

# Did they ask for task 2?  If so, run it.
if (any($task_2a,$task_2b,$task_2c)) {
    if (!all($task_2a,$task_2b,$task_2c)) {
	print STDERR "Error: Requested some, but not all, task2 sub-tasks\n";
	usage(1);
    }
    print "=" x 20 . " TASK 2 " . "=" x 20 . "\n";
    my $subAdata=filter(scan_data($task_2a),$mask_vec);
    my $subBdata=filter(scan_data($task_2b),$mask_vec);
    my $subCdata=filter(scan_data($task_2c),$mask_vec);
    my $all_results=[];
    foreach my $bvec (@$boot_vecs) {
        push(@$all_results,
    	 run_task2($truth,$subAdata,$subBdata,$subCdata,$bvec));
    }
    print "[T2] " . ($#$all_results+1) . " bootstrap scores\n";
    my $qualvec=[map { $_->{qualified} } @$all_results];
    my $subAsum=summarize_bootstrap([map { $_->{subAres} } @$all_results],
				    $qualvec);
    my $subBsum=summarize_bootstrap([map { $_->{subBres} } @$all_results],
				    $qualvec);
    my $subCsum=summarize_bootstrap([map { $_->{subCres} } @$all_results],
				    $qualvec);
    report($subAsum,"T2A");
    report($subBsum,"T2B");
    report($subCsum,"T2C");
    printf("[T2summary] total result :" . "%0.3f\t" x 6 . "\n",
	   $subAsum->{patientSensCount},
	   $subBsum->{patientSensCount},
	   $subCsum->{patientSensCount},
	   $subAsum->{stdPatSensCount},
	   $subBsum->{stdPatSensCount},
	   $subCsum->{stdPatSensCount}
	   );
    print "[T2summary] " . sum($qualvec) .
          "/" . ($#$all_results+1) . " qualified\n";
    print "=" x 48 . "\n";
}

if ($task_3) {
    print "=" x 20 . " TASK 3 " . "=" x 20 . "\n";
    my $t3data=filter(scan_data($task_3),$mask_vec);
    my $all_results=[];
    foreach my $bvec (@$boot_vecs) {
        push(@$all_results,
    	 run_task3($truth,$t3data,$bvec));
    }
    print "[T3] " . ($#$all_results+1) . " bootstrap scores\n";
    my $qualvec=[map { $_->{qualified} } @$all_results];
    my $t3sum=summarize_bootstrap([map { $_->{t3res} } @$all_results],
       		      		  $qualvec);
    report($t3sum,"T3");
    printf("[T3summary] total result :" . "%0.3f\t" x 8 . "\n",
           $t3sum->{NPV},$t3sum->{stdNPV},
	   $t3sum->{TNpatients},$t3sum->{stdTNPat},
	   $t3sum->{meanPEsensitivity},$t3sum->{stdMeanPEsens},
	   $t3sum->{meanFP},$t3sum->{stdFP});
    print "[T3summary] " . sum($qualvec) . "/" . ($#$all_results+1) .
          " qualified\n";
    print "=" x 48 . "\n";
}

if ($raw_fname) {
    close_rptfile();
}
