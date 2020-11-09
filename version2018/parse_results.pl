#!/usr/bin/perl
#    use strict;
    use warnings;

    #my $directory = '/home/kuppal2/Documents/Projects/xmsPANDA/Other/scripts/version2015/';

$directory=$ARGV[0];
    opendir (DIR, $directory) or die $!;

$pattern1=$ARGV[1]; #"v5C_2.out";
$pattern2=$ARGV[2];
@methods=("limma","lasso","rfe","elasticnet","f.test","union","rankAggreg-monte carlo","rankAggreg-GA","optselect");
$m=0;
while (my $file = readdir(DIR)) {

	if(($file=~/$pattern1/) && ($file=~/$pattern2/)){
	
	open $inf,'<',$file;
	print $file,"\n";
	chomp(my @rows = <$inf>);
	#while (my $row=<$inf>)
	for($i=0;$i<scalar(@rows);$i++)
	{

		$row=$rows[$i];
		chomp $row;
		#if($row=~/Test AUC:/){
		if($row=~/(test AUC acc)/){

			print $methods[$m], " : ",$rows[($i)],"\n";
			$m++;	
		}else{

			if($row=~/Test AUC:/){
				print $methods[$m], " : ",$rows[($i+1)],"\n";
			}
		}
	}

	}
    }


closedir(DIR);
    exit 0;

