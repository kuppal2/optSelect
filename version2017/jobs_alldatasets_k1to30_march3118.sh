

for i in $(seq 5 10 30)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done


for i in $(seq 10 10 30)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch312018_v1 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar3118Bv1_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done

