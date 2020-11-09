#!/usr/bin/perl
#    use strict;
    use warnings;

    my $directory = '/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/fromHPZ/version2015/'; #'/home/kuppal2/Documents/Projects/xmsPANDA/Other/scripts/version2015/';

    opendir (DIR, $directory) or die $!;

$pattern=$ARGV[0]; #"v5C_2.out";

while (my $file = readdir(DIR)) {

	if($file=~/$pattern/){
	
	open $inf,'<',$file;

	chomp(my @rows = <$inf>);
	#while (my $row=<$inf>)
	for($i=0;$i<scalar(@rows);$i++)
	{

		$row=$rows[$i];
		chomp $row;

		if($row=~/Test AUC:/){

			print $file, " : ",$rows[($i+1)],"\n";
			
		}
	}

	}
    }


closedir(DIR);
    exit 0;

