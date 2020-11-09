universe = vanilla
executable = /usr/lib64/R/bin/R
#input = /home/stu/kuppal3/karan_libs/myR2/lib/R/bin/p_spect_1.R
#input = /home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/pso_cpg_valid_test_final_inertia_kfold_multiobj_fixed_data_win_novalid_top100eval_naivebayes.R
input = /home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/performCMAmultiagentPSOnominal_tenfold_backelim_cvbestparticle_090213_arcene.R

arguments= --no-save
output =  /home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/condor_jobs/itr2cmapsoarcenewrand.out
error = /home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/condor_jobs/itr2cmapsoarcenewrand.err
log = /home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/condor_jobs/itr2cmapsoarcenewrand.log
should_transfer_files = YES
when_to_transfer_output = ON_EXIT
queue
