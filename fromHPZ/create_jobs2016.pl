$dir="/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/";

@weightA_1=(0.1,0.3,0.5,0.7,0.9);
@weightB_2=(0.1,0.3,0.5,0.7,0.9);

@weightC_3=(0.1,0.3,0.5,0.7,0.9);

@weightD_4=(0.1,0.3,0.5,0.7,0.9);

@max_k=(3,5,10,15,20,25,50,75,100);

@neighb_1=(0.25,0.45,0.65,0.9);
@global_1=(0.25,0.45,0.65,0.9);
@conf_1=(0.25,0.45,0.65,0.9);

#@weightcombA=(0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1);

@weightcombA=(0,0.2,0.3,0.4,0.5,0.6,0.7,0.75,0.8,0.9,1);
@weightcombB=(0,0.05,0.25,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1); #0.75,0.8,0.85,0.9,0.95,1);
@weightcombC=(0.01,0.05,0.1,0.2,0.25); #,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1);
@weightcombD=(0.01,0.05,0.1,0.2,0.25); #,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1);

@files=("prostate","srbct"); #,"maqcIIbreastER","maqcIIbreastPCR","golub");

@followerprob=(0.5,0.6,0.7,0.8);#(0.1,0.2,0.3,0.4,0.5,0.6,0.65,0.7,0.8,0.9);
@leaderprob=(0.4,0.3,0.2,0.1); #(0.8,0.7,0.6,0.5,0.4,0.3,0.35,0.2,0.1,0);

@inertiamethod=("global","random"); #("rankbased","random","global","dec");
@itrn_vec=(34,35);

open inf1, ">runjobs_OCFS_1.sh";

for (my $f=0;$f<scalar(@files); $f++){

for(my $g=0;$g<scalar(@itrn_vec); $g++){
$itr_n=$itrn_vec[$g];
for(my $k=0;$k<scalar(@max_k);$k++){

for(my $j=0;$j<scalar(@followerprob);$j++){
for(my $l=0;$l<scalar(@inertiamethod);$l++){

for(my $wA=0;$wA<scalar(@weightcombA);$wA++){
 for(my $wB=0;$wB<scalar(@weightcombB);$wB++){
  for(my $wC=0;$wC<scalar(@weightcombC);$wC++){ 
   for(my $wD=0;$wD<scalar(@weightcombD);$wD++){
      open inf, ">jobs".$files[$f]."itr".$itr_n."max".$max_k[$k]."follower".$followerprob[$j]."leader".$leaderprob[$j]."wA".$weightcombA[$wA]."wB".$weightcombB[$wB]."wC".$weightcombC[$wC]."wD".$weightcombA[$wD]."iner".$inertiamethod[$l].".cmd";

print inf "universe = vanilla\n";
print inf "executable = /usr/lib64/R/bin/R\n";
print inf "input = ".$dir."run_ocfs_$files[$f]_vjune262015_sensitivity.R\n";
print inf "arguments= --no-save --args $weightcombA[$wA] $weightcombB[$wB] $weightcombC[$wC] $weightcombD[$wD] $inertiamethod[$l] $followerprob[$j] 0.1 $leaderprob[$j] $itr_n $max_k[$k]\n";
print inf "output= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/out/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0]_$followerprob[$j]_$leaderprob[$j]_$inertiamethod[$l]_wA$weightcombA[$wA]_wB$weightcombB[$wB]_wC$weightcombC[$wC]_wD$weightcombA[$wD].out\n";
print inf "error= /home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/error/$files[$f]_may2415_v31_itr".$itr_n."_max$max_k[$k]_$weightcomb[0]_$followerprob[$j]_$leaderprob[$j]_$inertiamethod[$l]_wA$weightcombA[$wA]_wB$weightcombB[$wB]_wC$weightcombC[$wC]_wD$weightcombA[$wD].err\n";
print inf "should_transfer_files = YES\n";
print inf "when_to_transfer_output = ON_EXIT\n";
print inf "queue\n";

close inf;

print inf1 "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k]."follower".$followerprob[$j]."leader".$leaderprob[$j]."wA".$weightcombA[$wA]."wB".$weightcombB[$wB]."wC".$weightcombC[$wC]."wD".$weightcombA[$wD]."iner".$inertiamethod[$l].".cmd\n";

print "condor_submit jobs".$files[$f]."itr".$itr_n."max".$max_k[$k]."follower".$followerprob[$j]."leader".$leaderprob[$j]."wA".$weightcombA[$wA]."wB".$weightcombB[$wB]."wC".$weightcombC[$wC]."wD".$weightcombA[$wD]."iner".$inertiamethod[$l].".cmd\n";

}
}
}
}
}
}
}
}
}

close inf1;



