#!usr/bin/perl

$cmd="R --quiet --vanilla --no-restore --no-save </home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/performCMAmultiagentPSOnominal_tenfold_backelim_cvbestparticle_dec812_golub.R 2>&1";

exec($cmd. "> Routcmapsoitr11znorm10000itrminpres1_golub.txt &");
