for i in $(seq 3 2 5)
do 
nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_golub_vaug72018_sensitivity.R > ngolub.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &

nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_srbct_vaug72018_sensitivity.R > nsrbct.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &


nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_maqcIIbreastER_vaug72018_sensitivity.R > nmaqcIIbreastER.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &

nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_maqcIIbreastPCR_vaug72018_sensitivity.R > nmaqcIIbreastPCR.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &

nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_prostate_vaug72018_sensitivity.R > nprostate.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"
done

nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_iris_vaug82018_sensitivity.R > niris.$i._vfeb817_v5C2018v10_0.750.050.050.15w1global0.450.050.45globalbesttrain0.8pct.out &

for i in $(seq 10 5 20) 
do 
nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_golub_vaug72018_sensitivity.R > ngolub.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &
nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_srbct_vaug72018_sensitivity.R > nsrbct.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &
nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_maqcIIbreastER_vaug72018_sensitivity.R > nmaqcIIbreastER.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &
nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_maqcIIbreastPCR_vaug72018_sensitivity.R > nmaqcIIbreastPCR.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &
nohup R --no-save --args 0.75 0.05 0.05 0.15 global 0.3 0.1 0.5 vfeb817_v5C_2018_v11 $i none < run_ocfs_prostate_vaug72018_sensitivity.R > nprostate.$i._vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done

