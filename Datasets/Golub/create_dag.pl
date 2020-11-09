
$jobfname=">condor_job.submit";

open(fh,$jobfname);


	print fh "universe = vanilla";
	print fh "\n";
	print fh "executable = /usr/lib64/R/bin/R";
	print fh "\n";
	print fh "input = /home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/performCMAmultiagentPSOnominal_tenfold_backelim_cvbestparticle_dec812_golub.R";
	print fh "\n";
	print fh "arguments= --no-save --vanilla";
	print fh "\n";
	print fh "error = /home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/itr7.err";
	print fh "\n";
	print fh "log = /home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/itr7.log";
	print fh "\n";
	print fh "should_transfer_files = YES";
	print fh "\n";
	print fh "when_to_transfer_output = ON_EXIT";
	print fh "\n";

	for($i=7;$i<8;$i=$i+1)
	{
	print fh "\n";
	print fh "output =  /home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/Routitr",$i,".out";
	print fh "\n";
	print fh "queue";
	print fh "\n";
	print fh "\n";
	}
