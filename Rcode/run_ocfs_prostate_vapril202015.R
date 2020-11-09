
#.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)
library(yaImpute)
library(pROC)
library(bioDist)
#library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")

library(CMA)
library(expm)

cl<-makeCluster(1)

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.1.R")
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/OCFS_vnov252014.R")

#same results
#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov26.22014_v11.R")

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vapr282015.R")

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vapr282015_v2.R")

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vapr282015_v10_1.R")
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/svm_cv_v1.1.R")

#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_nov172014_backward.R")

load("/home/stu/kuppal3/Research/Feature_selection/Datasets/Prostate/Prostate.Rda")

trainm<-Prostate$X
testm<-Prostate$Xt
trainclass<-Prostate$Y
testclass<-Prostate$Yt

cnames<-paste("var",seq(1,dim(trainm)[2]),sep="")
colnames(trainm)<-cnames
colnames(testm)<-cnames

trainm<-t(trainm)
testm<-t(testm)


if(FALSE){
trainm[trainm < 100] <- 100
trainm[trainm > 16000] <- 16000

testm[testm < 100] <- 100
testm[testm > 16000] <- 16000

mmfilt <- function(x,r = 5, d = 500, na.rm = TRUE) {
 minval <- min(x, na.rm = na.rm)
 maxval <- max(x, na.rm = na.rm)
 (maxval/minval > r) && (maxval - minval > d)
}

#mmfun <- mmfilt()
#ffun <- filterfun(mmfun)
#good <- genefilter(X, ffun)

good<-apply(trainm,1,mmfilt)


trainm<-trainm[good,]

print(dim(trainm))
testm<-testm[good,]

}

trainm<-t(trainm)
testm<-t(testm)

trainm<-cbind(trainclass,trainm)
testm<-cbind(testclass,testm)

trainm<-na.omit(trainm)
testm<-na.omit(testm)

#outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/OCFSv21itr1_LIMMApres1novarsel_l0.45f0.45c0.05minsel0.3randbehavfeatw0.01_local/"
outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Prostate/OCFSvapr2815v10_1.1reg_itr1_LassoRFELIMMAELpres1backwsel_l0.25f0.45c0.25_top10pctmaxitrs100minselmedianrankbehavfeatw0.01_CV2accA100B1wrand6methodsmax100/"

dir.create(outloc)
setwd(outloc)

trainm<-as.matrix(trainm)
testm<-as.matrix(testm)
trainclass<-trainm[,1] #CMAres$modtrainclass
testclass<-testm[,1] #CMAres$modtestclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata
testm<-testm[,-c(1)] #CMAres$modtestmata

#a: Confusions
#b: Neighbors
#c: Global
#d: Death

a<-c(0.25,0.25,0.25,0.25)
b<-c(0.3,0.1,0.4,0.1)
c<-c(0.25,0.25,0.5,0)
d<-c(0.9,0.1,0,0.1)

a<-c(0,0.4,0.1,0.5)
b<-c(0.3,0.1,0.4,0.1)
c<-c(0,0.5,0.5,0)
d<-c(0.9,0.1,0,0)

a<-c(0,0.4,0.1,0.5)
b<-c(0.2,0.3,0.4,0.1)
c<-c(0,0.4,0.4,0.2)
d<-c(0.9,0.1,0,0)

transition_matrix<-rbind(a,b,c,d)


dir.create(outloc)
setwd(outloc)
temp2=t(trainm)
temp2=apply(temp2, 2, function(x){which(x=="MD")})
temp2=unlist(temp2)
temp2=unique(temp2)
if(length(temp2)>1)
{
	trainm=trainm[,-c(temp2)]

	rm(temp2)
}

boostweight=rep(0,dim(trainm)[2])

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("limma"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))


CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("lasso"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("rfe"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("elasticnet"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

if(FALSE){
CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("rf"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))
}

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("f.test"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

#1
CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=0.005,
minnum=3,
stepitr=1,
gsmethods=c("limma","lasso","rfe","elasticnet", "f.test"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
percentTest=0.40,
accuracyweight=1,
featweight=0.06,
minpresent=1,
kname="radial",
norm_method="none",
tolerance=0.1,
maxitrs=5,
classindex=1,
numfacts=0,
evalmethod="CV",
numfolds=10,
CVfoldthresh=0.7,
varselmethod="backward",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

cma_feat_list<-colnames(trainm)

save(CMAres,file="CMAres.Rda")
write.table(cma_feat_list,file="selected_cma_feat_list.txt",sep="t",row.names=FALSE)

# modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y
#if(FALSE)
{
trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
learningsets<-CMAres$learningsets
}

if(FALSE)
{
trainclass<-trainm[,1] #CMAres$modtrainclass
testclass<-testm[,1] #CMAres$modtestclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata
testm<-testm[,-c(1)] #CMAres$modtestmata

}

d_dim<-dim(trainm)

print("Original dimension")
print(d_dim)

#2 call run_pso function()
system.time(psores<-run_pso(outloc=outloc,trainm,trainclass,testm,testclass,transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=10,
global_max_itr=30,
num_part=20,
kname="radial",
errortype="BER",
accuracyweightA=100,
accuracyweightB=1,
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=0.45,
confusionprob=0.25,
leaderprob=0.25,
wmax=1,
wmin=1,
behavior_reset_itr=5,
maxitrreset=3,
num_neighbors=3,
minselect.pct=0.5,
evalMode="CV2",
minfitnessthresh=50,
maxnum=3,minnum=3,inertia_method="random",particlebehav_method="rankbased",constriction_factor=1,
select.global.best=TRUE,numnodes=30,evalFunc=eval_fit_kfold_diff,itr.terminate=FALSE))





feat_ind<-psores$bestfeatlist
feat_names<-psores$bestfeatnames

scoringmatrix<-as.data.frame(psores$scoringmatrix)
print(scoringmatrix)
print(feat_names[feat_ind])

save(psores,file="psores.Rda")
print("Complete")
