#options(echo=FALSE)
library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(bioDist, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
source("/home/stu/kuppal3/Research/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")

#source("/home/stu/kuppal3/Research/Feature_selection/performCMA.R")
source("/home/stu/kuppal3/Research/Feature_selection/performPSO.R")
source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensus.R")

#datafile<-read.csv("/home/stu/kuppal3/Research/Huira/Output/cpg_dist_gccont_obsexp_seq2to5exactpatterns.csv", na.strings="", sep=",", header=TRUE)
#outloc<-"/home/stu/kuppal3/Research/Huira/Output/CMA_PSO_results/allfeats1to50step_5radial_fitfunc0.05_itr2/"

datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/MEDIPS_analysis/pattern_search/count_medips_hesc_ams_rand1000chr1to3_len2to8_patterns_normbylengthmult1000.csv", na.strings="", sep=",", header=TRUE)
outloc<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/medips_analysis/CMA_PSO_results/"

datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/MEDIPS_analysis/sep6analysis/pattern_search/count_medips_yuh_ams_rand1000chrall_len2to8_patterns_normbylengthmult1000.csv", na.strings="", sep=",", header=TRUE)
outloc<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/medips_analysis/yuhong_seqs/CMA_PSO_results/"


maxgenes=150
mingenes=1
step=50
#methods=c("rf", "kruskal.test", "f.test")
methods=c("f.test")
percentTest=0.40
kname="radial"
maxitrs=2
#CMAres<-performCMA(datafile, outloc, maxgenes, mingenes, step, methods, percentTest, 0.05,100, kname)
CMAres<-performCMA(datafile, outloc, maxgenes, mingenes, step, methods, percentTest, 0.05,100, kname, maxitrs)


c1=2.05
c2=2.05
numitrs=3
reinititrs=200
numpart=3
kname="radial"

print(dim(CMAres$modtraindata))
print(length(CMAres$modtrainclass))

if(CMAres$numfeat>1)
{
print("Start PSO")
performPSO(CMAres$modtraindata, CMAres$modtestdata, CMAres$modtrainclass, CMAres$modtestclass,outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.06, 100)
}
else
{
	model_train_valid<-svm(CMAres$modtraindata, CMAres$modtrainclass,   kernel=kname, type="C")


        pred_test<-predict(model_train_valid, CMAres$modtestdata)

        test.table<-table(pred_test, CMAres$modtestclass)

        print("test predicted table")
        print(test.table)

        testacc<-sum(diag(test.table))/(dim(CMAres$modtestdata)[1])
        print("Test accuracy is ")
        print(testacc)


}
print("Complete")

