This archive contains materials from the 2006 KDD Cup competition.
The materials here should be sufficient to build new classifiers for
this data and to replicate the scoring conditions of the competition.

This package includes:

README.TXT				This file
kdd-cup-2006-task-spec-final.pdf	Description of data and rules
data/
  KDDPETest.txt				Test data vectors
  KDDPETestLabel.txt			Test data labels
  KDDPETrain.txt			Training data vectors+labels
  KDDPEfeatureNames.txt			List of feature names
  bootstrap_vectors.dat			Bootstrap vectors used in evaluation
  drop_duplicate_patients_mask.dat	Mask for removing duplicated patients from evaluation
src/
  gen_bootstrap_matrix.pl		Program for generating bootstrap vectors
  score.pl				Scoring program
  scorelib.pm				Library code used by the above
  validator.pl				File format validator for input files for score.pl
results/
  README.TXT				Info on the results files
  Task1_results.txt			Results for Task 1
  Task2_results.txt			Results for Task 2
  Task3_results.txt			Results for Task 3

Run "score.pl -h" or "gen_bootstrap_matrix.pl -h" for brief usage
directions.

Note that the programs are all written in Perl 5.  They require Perl 5
to be installed and available in /usr/bin/perl and that the
scorelib.pm be accessible in some directory on the @INC search path.
You can also set the SCORELIBDIR environment directory to the
directory in which your copy of scorelib.pm is located and the scripts
will prepend that directory to @INC when they run.
