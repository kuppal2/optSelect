$dir="/home/kuppal3/Research/Feature_selection/Rcode/version2015/";

@weightA_1=(0.1,0.3,0.5,0.7,0.9);
@weightB_2=(0.1,0.3,0.5,0.7,0.9);

@weightC_3=(0.1,0.3,0.5,0.7,0.9);

@weightD_4=(0.1,0.3,0.5,0.7,0.9);

@max_k=(3,5,10,15,20);

@neighb_1=(0.25,0.45,0.65,0.9);
@global_1=(0.25,0.45,0.65,0.9);
@conf_1=(0.25,0.45,0.65,0.9);

#@weightcomb=(0.5,0.05,0.05,0.4);

@weightcomb=(0.7,0.0,0.05,0.25);
@files=("golub","prostate","srbct","maqcIIbreastER","maqcIIbreastPCR","iris"); #,"14cancer");

#@files=("maqcIIbreastER","maqcIIbreastPCR");
@inertia_method=("global"); #("rankbased","random","global");
open inf1, ">runjobs_OCFS_feb2817.sh";
for (my $f=0;$f<scalar(@files); $f++){

#$itr_n="vmay2415_v32_feb2717";
$itr_n="vmay2415_v32_FINAL";
for(my $k=0;$k<scalar(@max_k);$k++){
for(my $i=0;$i<scalar(@inertia_method);$i++){
	open inf, ">jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd";

print inf "universe = vanilla\n";
print inf "executable = /usr/lib64/R/bin/R\n";
print inf "input = ".$dir."run_ocfs_$files[$f]_vjune262015_sensitivity.R\n";
print inf "arguments= --no-save --args $weightcomb[0] $weightcomb[1] $weightcomb[2] $weightcomb[3] $inertia_method[$i] 0.45 0.2 0.25 $itr_n $max_k[$k]\n";
print inf "output= /home/kuppal3/Research/Feature_selection/Rcode/version2015/out/$files[$f]_".$itr_n."_max$max_k[$k]_$weightcomb[0]$weightcomb[1]$weightcomb[2]$weightcomb[3]_$inertia_method[$i]l0.450.20.25.out\n";
print inf "error= /home/kuppal3/Research/Feature_selection/Rcode/version2015/error/$files[$f]_".$itr_n."_max$max_k[$k]_$weightcomb[0]$weightcomb[1]$weightcomb[2]$weightcomb[3]_$inertia_method[$i]l0.450.20.25.err\n";
print inf "should_transfer_files = YES\n";
print inf "when_to_transfer_output = ON_EXIT\n";
print inf "queue\n";

close inf;

print inf1 "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd\n";
}
}
}



close inf1;



