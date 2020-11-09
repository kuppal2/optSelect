.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
#options(echo=FALSE)
library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(bioDist, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
source("/home/stu/kuppal3/Research/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")
source("performCMA.R")
source("performPSO.R")

#Huira
#datafile<-read.csv("/home/stu/kuppal3/Research/Huira/Output/cpg_dist_gccont_obsexp_seq2to5exactpatterns.csv", na.strings="", sep=",", header=TRUE)
#outloc<-"/home/stu/kuppal3/Research/Huira/Output/CMA_PSO_results/allfeats1to50step_5radial_fitfunc0.05_itr1/"

datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/MEDIPS_analysis/pattern_search/count_medips_hesc_ams_rand1000chr1to3_len2to8_patterns_normbylengthmult1000.csv", na.strings="", sep=",", header=TRUE)
outloc<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/medips_analysis/CMA_PSO_results/"

datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/MEDIPS_analysis/sep6analysis/pattern_search/count_medips_yuh_ams_rand1000chrall_len2to8_patterns_normbylengthmult1000.csv", na.strings="", sep=",", header=TRUE)
outloc<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/medips_analysis/yuhong_seqs/CMA_PSO_results/"

maxgenes=150
mingenes=100
step=5
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")
#methods=c("t.test")
methods=c("rfe", "rf", "kruskal.test", "f.test")
percentTest=0.40
kname="radial"
CMAres<-performCMA(datafile, outloc, maxgenes, mingenes, step, methods, percentTest, 0.07,100, kname)

c1=2.05
c2=2.05
numitrs=500
reinititrs=100
numpart=20
kname="radial"

print("Start PSO")
performPSO(CMAres$modtraindata, CMAres$modtestdata, outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.08, 100)
print("Complete")

