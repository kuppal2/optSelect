$dir="/home/kuppal2/Documents/Projects/xmsPANDA/Other/scripts/version2015/";


@weightA_1=(0.1,0.3,0.5,0.7,0.9);
@weightB_2=(0.1,0.3,0.5,0.7,0.9);

@weightC_3=(0.1,0.3,0.5,0.7,0.9);

@weightD_4=(0.1,0.3,0.5,0.7,0.9);

@max_k=(3,5,10,15); #,20,25,50,75,100);

@neighb_1=(0.25,0.45,0.65,0.9);
@global_1=(0.25,0.45,0.65,0.9);
@conf_1=(0.25,0.45,0.65,0.9);

@weightcomb=(0.7,0.0,0.05,0.25);
@files=("maqcIIbreastPCR"); #("arcene"); #,"golub","prostate","srbct","maqcIIbreastPCR"); #,"maqcIIbreastPCR"); #,"14cancer");
@files=("golub","prostate","srbct","maqcIIbreastPCR","maqcIIbreastER","iris");

#open inf1, ">runjobs_OCFS_1.sh";
for (my $f=0;$f<scalar(@files); $f++){

#$itr_n="vmay2415_v32_manuscript_analysis";

$itr_n="vmay2415_v32_FINAL";
open inf, ">jobs".$files[$f]."itr".$itr_n."max".$max_k[$k]."0831$files[$f].cmd";
for(my $k=0;$k<scalar(@max_k);$k++){
	



#23
$outf="nohup R --no-save --args $weightcomb[0] $weightcomb[1] $weightcomb[2] $weightcomb[3] global 0.45 0.2 0.25 $itr_n $max_k[$k] < run_ocfs_$files[$f]_vjune262015_sensitivity.R > n$files[$f]$max_k[$k]globalnochange5itrreset5nod1pct083116.out &";
print inf $outf."\n";

#print inf1 "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd\n";
}
close inf;
}
#close inf1;



