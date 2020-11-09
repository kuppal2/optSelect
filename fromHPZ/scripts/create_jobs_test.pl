$dir="/home/stu/kuppal3/Research/Feature_selection/Rcode/version2016/";

@weightA_1=(0.1,0.3,0.5,0.7,0.9);
@weightB_2=(0.1,0.3,0.5,0.7,0.9);

@weightC_3=(0.1,0.3,0.5,0.7,0.9);

@weightD_4=(0.1,0.3,0.5,0.7,0.9);

@max_k=(5,10); #,25,50,100);

@neighb_1=(0.25,0.45,0.65,0.9);
@global_1=(0.25,0.45,0.65,0.9);
@conf_1=(0.25,0.45,0.65,0.9);

@weightcomb=(0.7,0.0,0.05,0.25);
@files=("srbct"); #("prostate","srbct","maqcIIbreastER","maqcIIbreastPCR"); #,"14cancer");
open inf1, ">runjobs_OCFS_1.sh";
for (my $f=0;$f<scalar(@files); $f++){

$itr_n=26;
for(my $k=0;$k<scalar(@max_k);$k++){
	open inf, ">jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd";

print inf "universe = vanilla\n";
print inf "executable = /usr/lib64/R/bin/R\n";
print inf "input = ".$dir."run_ocfs_$files[$f]_vjune262015_sensitivity.R\n";
print inf "arguments= --no-save --args $weightcomb[0] $weightcomb[1] $weightcomb[2] $weightcomb[3] global 0.65 0.1 0.2 $itr_n $max_k[$k]\n";
print inf "output= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/out/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0].out\n";
print inf "error= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/error/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0].err\n";
print inf "should_transfer_files = YES\n";
print inf "when_to_transfer_output = ON_EXIT\n";
print inf "queue\n";

close inf;

print inf1 "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd\n";
}
}
for (my $f=0;$f<scalar(@files); $f++){

$itr_n=23;
for(my $k=0;$k<scalar(@max_k);$k++){
        open inf, ">jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd";

print inf "universe = vanilla\n";
print inf "executable = /usr/lib64/R/bin/R\n";
print inf "input = ".$dir."run_ocfs_$files[$f]_vjune262015_sensitivity.R\n";
print inf "arguments= --no-save --args $weightcomb[0] $weightcomb[1] $weightcomb[2] $weightcomb[3] global 0.45 0.25 0.25 $itr_n $max_k[$k]\n";
print inf "output= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/out/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0].out\n";
print inf "error= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/error/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0].err\n";
print inf "should_transfer_files = YES\n";
print inf "when_to_transfer_output = ON_EXIT\n";
print inf "queue\n";
close inf;
print inf1 "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd\n";
}

}

for (my $f=0;$f<scalar(@files); $f++){

$itr_n=27;
for(my $k=0;$k<scalar(@max_k);$k++){
        open inf, ">jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd";

print inf "universe = vanilla\n";
print inf "executable = /usr/lib64/R/bin/R\n";
print inf "input = ".$dir."run_ocfs_$files[$f]_vjune262015_sensitivity.R\n";
print inf "arguments= --no-save --args $weightcomb[0] $weightcomb[1] $weightcomb[2] $weightcomb[3] global 0.2 0.1 0.65 $itr_n $max_k[$k]\n";
print inf "output= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/out/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0].out\n";
print inf "error= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/error/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0].err\n";
print inf "should_transfer_files = YES\n";
print inf "when_to_transfer_output = ON_EXIT\n";
print inf "queue\n";
close inf;
print inf1 "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k].".cmd\n";
}

}
close inf1;



