i=100
nohup R --no-save --args 0.8 0.05 0.05 0.1 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_arcene_vaug72018_sensitivity.R > narcene.$i._vfeb817_v5C_2018_v11_0.80.050.050.1_0.30.10.5_none.out &

i=1000
nohup R --no-save --args 0.8 0.05 0.05 0.1 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_arcene_vaug72018_sensitivity.R > narcene.$i._vfeb817_v5C_2018_v11_0.80.050.050.1_0.30.10.5_none.out &
