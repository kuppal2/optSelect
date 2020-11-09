

for i in $(seq 5 5 20)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v2 $i none < run_ocfs_iris_vjune132018_sensitivity.R > niris.$i._vfeb817_v5C2018v2_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v3 $i none < run_ocfs_iris_vaug72018_sensitivity.R > niris.$i._vfeb817_v5C2018v3_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v2 $i backward < run_ocfs_iris_vjune132018_sensitivity.R > niris.$i._vfeb817_v5C2018v2_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pctback.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v3 $i none < run_ocfs_iris_vaug72018_sensitivity.R > niris.$i._vfeb817_v5C2018v3_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out


for i in $(seq 10 10 30)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune112018_v1 $i none < run_ocfs_iris_vmarch62018_sensitivity.R > niris.$i._vjune112018_v1_0.70.20.050.05w0.01_none.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 rankbased 0.45 0.05 0.45 vjune112018_v1 $i none < run_ocfs_iris_vjune132018_sensitivity.R > niris.$i._vjune112018v1_0.70.20.050.05w1rank0.450.050.45.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 rankbased 0.45 0.2 0.25 vfeb817_v5C $i none < run_ocfs_iris_vjune132018_sensitivity.R > niris.$i._vfeb817_v5C_0.70.20.050.05w1rank0.450.050.45.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 rankbased 0.45 0.2 0.25 vfeb817_v5C_2018_v1 $i none < run_ocfs_iris_vjune132018_sensitivity.R > niris.$i._vfeb817_v5C2018_0.70.20.050.05w1rank0.450.050.45.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 rankbased 0.45 0.2 0.25 vfeb817_v5C_2018_v2 $i none < run_ocfs_iris_vjune132018_sensitivity.R > niris.$i._vfeb817_v5C2018v2_0.70.20.050.05w1rank0.450.050.45.out &


for i in $(seq 5 5 20)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjune102018_v1 $i none < run_ocfs_iris_vmarch62018_sensitivity.R > niris.$i._vjune1018v1_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done

done

