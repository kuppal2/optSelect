
i=3

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_iris_vmarch62018_sensitivity.R > niris.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &



for i in $(seq 5 5 30)

for i in $(seq 25 5 30)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

echo "This will wait until $i forward jobs are done"
date
wait
date
echo "$i forward jobs are Done"

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

echo "This will wait until $i maxnum jobs are done"
date
wait
date
echo "$i maxnum jobs are Done"

done


for i in $(seq 3 5 3)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i backward < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_back.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i backward < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_back.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i backward < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_back.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i backward < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_back.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i backward < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_back.out &
echo "This will wait until $i backward jobs are done"
date
wait
date
echo "$i backward jobs are Done"

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i forward < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_fwd.out &

echo "This will wait until $i forward jobs are done"
date
wait
date
echo "$i forward jobs are Done"

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

echo "This will wait until $i none jobs are done"
date
wait
date
echo "$i none jobs are Done"

for i in $(seq 3 5 3)
do
nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &

echo "This will wait until $i maxnum jobs are done"
date
wait
date
echo "$i maxnum jobs are Done"
done


i=3

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_iris_vmarch62018_sensitivity.R > niris.$i._vmar618v3_0.70.20.050.05w0.01_none.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER.$i._vmar618v3_0.70.20.050.05w0.01_none.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i none < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR.$i._vmar618v3_0.70.20.050.05w0.01_none.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v3 $i maxnum < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub.$i._vmar618v3_0.70.20.050.05w0.01_maxnum.out &


perl parse_results.pl . ngolub3g vjan9v2B_
perl parse_results.pl . nsrbct3g vjan9v2B_
perl parse_results.pl . nmaqcIIbreastER3g vjan9v2B_
perl parse_results.pl . nmaqcIIbreastPCR3g vjan9v2B_
perl parse_results.pl . nprostate3g vjan9v2B_

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 5 < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate5globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 5 < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub5globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 5 < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct5globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 5 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER5globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 5 < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR5globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 3 < run_ocfs_iris_vmarch62018_sensitivity.R > nirisglobalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate10globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub10globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct10globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER10globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR10globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 15 < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate15globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 15 < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub15globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 15 < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct15globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 15 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER15globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 15 < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR15globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 30 < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate30globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 30 < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub30globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 30 < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct30globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 30 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER30globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 30 < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR30globalnochange5itrreset5nod1pctvjan9v2B_0.70.20.050.05weight0.01.out &



nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate10globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub10globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct10globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER10globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01B.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjan92018_v1 10 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER10globalnochange5itrreset5nod1pctvjan9v1_0.70.20.050.05weight0.01B.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 10 < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR10globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01.out &


nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 3 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER3globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01B.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER3globalnochange5itrreset5nod1pctvjan9v1_0.70.20.050.05weight0.01B.out &



nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vmarch62018_v1 5 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER5globalnochange5itrreset5nod1pctvjan9v2_0.70.20.050.05weight0.01B.out &

nohup R --no-save --args 0.7 0.2 0.05 0.05 global 0.45 0.2 0.25 vjan92018_v1 5 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER5globalnochange5itrreset5nod1pctvjan9v1_0.70.20.050.05weight0.01B.out &









nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vfeb122018_v1 3 < run_ocfs_iris_vmarch62018_sensitivity.R > nirisglobalnochange5itrreset5nod1pctvfeb12_0.60.20.050.15weight0.09.out &



nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_iris_vmarch62018_sensitivity.R > nirisglobalnochange5itrreset5nod1pctvjan9_0.60.20.050.15weight0.09.out &


nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_prostate_vmarch62018_sensitivity.R > nprostate3globalnochange5itrreset5nod1pctvjan9_0.60.20.050.15.out &

nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_golub_vmarch62018_sensitivity.R > ngolub3globalnochange5itrreset5nod1pctvjan9_0.60.20.050.15.out &

nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_srbct_vmarch62018_sensitivity.R > nsrbct3globalnochange5itrreset5nod1pctvjan9_0.60.20.050.15.out &

nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_maqcIIbreastER_vmarch62018_sensitivity.R > nmaqcIIbreastER3globalnochange5itrreset5nod1pctvjan9_0.60.20.050.15.out &

nohup R --no-save --args 0.6 0.2 0.05 0.15 global 0.45 0.2 0.25 vjan92018_v1 3 < run_ocfs_maqcIIbreastPCR_vmarch62018_sensitivity.R > nmaqcIIbreastPCR3globalnochange5itrreset5nod1pctvjan9_0.60.20.050.15.out &


