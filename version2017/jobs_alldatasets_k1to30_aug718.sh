#perl parse_results.pl . vfeb817_v5C_2018_v11_0.750.050.050.15_0.30.10.5_none srbct.3.

for i in $(seq 5 5 5)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v7 $i none < run_ocfs_prostate_vaug72018_sensitivity.R > nprostate.$i._vfeb817_v5C_2018_v7_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v7 $i none < run_ocfs_golub_vaug72018_sensitivity.R > ngolub.$i._vfeb817_v5C_2018_v7_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v7 $i none < run_ocfs_srbct_vaug72018_sensitivity.R > nsrbct.$i._vfeb817_v5C_2018_v7_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v7 $i none < run_ocfs_maqcIIbreastER_vaug72018_sensitivity.R > nmaqcIIbreastER.$i._vfeb817_v5C_2018_v7_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v7 $i none < run_ocfs_maqcIIbreastPCR_vaug72018_sensitivity.R > nmaqcIIbreastPCR.$i._vfeb817_v5C_2018_v7_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done

#
for i in $(seq 3 2 5)
do

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v10 $i none < run_ocfs_golub_vaug72018_sensitivity.R > ngolub.$i._vfeb817_v5C_2018_v10_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v10 $i none < run_ocfs_srbct_vaug72018_sensitivity.R > nsrbct.$i._vfeb817_v5C_2018_v10_0.70.20.050.05w0.01_none.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v10 $i none < run_ocfs_maqcIIbreastER_vaug72018_sensitivity.R > nmaqcIIbreastER.$i._vfeb817_v5C_2018_v10_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v10 $i none < run_ocfs_maqcIIbreastPCR_vaug72018_sensitivity.R > nmaqcIIbreastPCR.$i._vfeb817_v5C_2018_v10_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v10 $i none < run_ocfs_prostate_vaug72018_sensitivity.R > nprostate.$i._vfeb817_v5C_2018_v10_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

done

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v3 $i none < run_ocfs_iris_vaug72018_sensitivity.R > niris.$i._vfeb817_v5C2018v3_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pctB.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v4 $i none < run_ocfs_iris_vaug72018_sensitivity.R > niris.$i._vfeb817_v5C2018v3_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v4 $i none < run_ocfs_iris_vaug72018_sensitivity.R > niris.$i._vfeb817_v5C2018v4_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v5 $i none < run_ocfs_iris_vaug72018_sensitivity.R > niris.$i._vfeb817_v5C2018v5_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out &



nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vfeb817_v5C_2018_v7 $i none < run_ocfs_iris_vaug82018_sensitivity.R > niris.$i._vfeb817_v5C2018v7_0.70.20.050.05w1global0.450.050.45globalbesttrain0.8pct.out &
