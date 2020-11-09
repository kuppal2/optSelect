universe = vanilla
executable = /usr/lib64/R/bin/R
#input = /home/stu/kuppal3/karan_libs/myR2/lib/R/bin/p_spect_1.R
#input = /home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/pso_cpg_valid_test_final_inertia_kfold_multiobj_fixed_data_win_novalid_top100eval_naivebayes.R
input = /home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/pso_cpg_novalid_nominal_naivebayes_mutinfo.R

arguments= --no-save
output =  /home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/result_novalid_wadapt_naivebayes_mutinfo/cpg_Rpso_naivebayes_1000itr_10and0_70train_wadapt_BER.out
error = /home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/result_novalid_wadapt_naivebayes_mutinfo/vt_cpg_pso_knn.err
log = /home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/result_novalid_wadapt_naivebayes_mutinfo/vt_cpg_pso_knn.log
should_transfer_files = YES
when_to_transfer_output = ON_EXIT
queue
