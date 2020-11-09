#library(snow)
#library(e1071)
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/svm_cv_func.R")

#cl<-makeCluster(8)
#cl<-makeSOCKcluster(c(rep("godel2",2),rep("godel3",3),rep("lebniz2",3),rep("leibniz3",2)))
#cl<-makeSOCKcluster("godel2",2)

.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)

cl<-makeCluster(20)
#cl<-makeSOCKcluster(c(rep("godel2",20),rep("godel3",20))) #,rep("lebniz2",10),rep("leibniz3",10)))



#options(echo=FALSE)
library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
library(bioDist, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#source("/home/stu/kuppal3/Research/Genomic_imprinting/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_func.R")

#options(echo=FALSE)
#library(e1071, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(CMA) #, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(bioDist) #, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#source("/home/stu/kuppal3/Research/Genomic_imprinting/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")

#source("/home/stu/kuppal3/Research/Feature_selection/performPSOnominalheart_regression.R")
#source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensusheart_regression.R")

#source("/home/stu/kuppal3/Research/Heart_data/regression/performPSOnominalheart_regression.R")
#source("performPSOnominallung_regression_factors_oct242010.R")
#source("/home/stu/kuppal3/Research/Heart_data/regression/performCMA_consensusheart_regression.R")

#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_oct242010.R")
#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_nov192010.R")
#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_nov192010.R")

#source("/home/stu/kuppal3/Research/Feature_selection/regression/perform_PSO_nov192010.R")
#source("/home/stu/kuppal3/Research/Feature_selection/performPSOnominal_tenfold_regression.R")
#source("/home/stu/kuppal3/Research/Feature_selection/preprocessdata_regression.R")


#source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensus_tenfold_ber_nominal_lungheart_feb242011.R")
#source("/home/stu/kuppal3/Research/Feature_selection/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_feb282011.R")

#source("/home/stu/kuppal3/Research/Feature_selection/

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_sep52013_backward.R")

#source("performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_sep192012_backward.R")
#datafile<-read.csv("/home/stu/kuppal3/Research/Heart_data/data_nominal.csv", na.strings="", sep=",", header=TRUE)

#datafile<-read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/orderednominal/luca_no_missing_1056feats_nominal_ordered_events.csv", na.strings="", sep=",", header=TRUE)
#datafile<-read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/count_lung_cancer_len1to8_patterns_normbylengthmult1000_withevents.csv", na.strings="", sep=",", header=TRUE)

#outloc<-"/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/regression/PSO_reg_results/itr1/"

#datafile=read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/LuCa_RLGS_Features_modified_051409_update_withevents.csv", sep=",", header=TRUE)

#outloc<-"/home/stu/kuppal3/Research/Heart_data/Output/CMA_PSO_results/itr3_ms_parallel_IMT/"

#traind<-read.csv("/home/stu/kuppal3/Research/Feature_selection/Datasets/ARCENE/arcene_train.data")
#testd<-read.csv("/home/stu/kuppal3/Research/Feature_selection/Datasets/ARCENE/arcene_valid.data")
if(FALSE){
trainy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.labels", header=FALSE)
testy<-read.csv("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.labels", header=FALSE)

traind<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_train.data", sep=" ",header=FALSE)
testd<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/arcene_valid.data", sep=" ",header=FALSE)
}

#/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest
trainy<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.ytrain", header=FALSE)
testy<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.ytest", header=FALSE)

trainy<-t(trainy)
testy<-t(testy)

#traind<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtrain", sep=" ",header=FALSE)
#testd<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest", sep=" ",header=FALSE)


traind<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtrain")

testd<-read.table("/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/khan.xtest")

traind<-t(traind) #[,-c(10001)]
testd<-t(testd) #[,-c(10001)]
traind<-cbind(trainy,traind)
testd<-cbind(testy,testd)

traind<-na.omit(traind)
testd<-na.omit(testd)

#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/minpres2union_pso1000f0.25c0.25_backwardfalse_cvfoldthres0.7itr3wlininc/"

outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/itr1cmapso5000f0.1c0.5_minpres1union_backwardtrue_cvfoldthres0.7_wrand/"
#traind<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.train", header=FALSE)
                  


#outloc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/ARCENE/itr6_allmethods_minmethods1_ttestfilt_backwardsel_pso10/"
#traind<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.train", header=FALSE)
#testd<-read.csv("/home/stu/kuppal3/Research/SpectF_data/SPECTF.test", header=FALSE)
#outloc<-"/home/stu/kuppal3/Research/SpectF_data/Results/itr2_ms_parallel/"
dir.create(outloc)
temp2=t(traind)
temp2=apply(temp2, 2, function(x){which(x=="MD")})
temp2=unlist(temp2)
temp2=unique(temp2)
if(length(temp2)>1)
{
	traind=traind[,-c(temp2)]

	rm(temp2)
}

minfeats=2
maxgenes=2000
mingenes=1
stepitr=1
methods=c("rf","rfe","limma","lasso","elasticnet","kruskal.test") #, "f.test", "elasticnet", "wilcox.test", "welch.test")
#methods=c("t.test")
percentTest=0.40
kname="radial"
maxitrs=100
removeindex=c(0)
norm_method="znorm"
evalmethod="10 fold"
classindex=1
maxfacts=0
percentTest=0.40
kname="radial"
upclasssep=1
dclasssep=-1
tolerance=0.1
accWeight=1
featWeight=0.06
minpresent=1 #round(1*length(methods))
maxfeats=1000 #dim(datafile)[2]-length(removeindex)-1
maxfeatspercent=0.1
evalmethod="CV"
numfolds=5
CVfoldthresh=0.7
backward.sel=FALSE
scheme_val="one-vs-all"
iter_learn=5
######PSO parameters#########
c1=2.05
c2=2.05
itr=5000
maxitr=(0.1)*itr
num_part=30
kname="radial"
accuracyweight=1
featweight=0.06
k<-numfolds
followerprob=0.1
confusionprob=0.5
wmax<-1
wmin<-0.4
behavior_reset_itr<-(1)*itr

##################

#id<-sample(1:data_dim[1],size=percentTest*data_dim[1],replace=F)

#testm<-datafile[id,]

#ncid<-datafile[-id,]


