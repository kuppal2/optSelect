#!usr/bin/perl

$cmd="R64 --quiet --vanilla --no-restore --no-save <performCMAmultiagentPSOnominal_tenfold_backelim_cvbestparticle_070113_arcene.R 2>&1";

exec($cmd. "> Routpso_070113_1minpres_10itrs_50reps_itr1kruskaljuly1CMA_ttestfiltbackwardFalse_cvfold1.txt");
