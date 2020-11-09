
#.libPaths("/home/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)
library(yaImpute)
library(pROC)
library(bioDist)
#library(CMA, lib="/home/kuppal3/karan_libs/Rlibs/")
library(RankAggreg)
library(CMA)
library(expm)

cl<-makeCluster(1)

#source("/home/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.1.R")
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/version2015/OCFS_vnov282014.R")

#same results
#source("/home/kuppal3/Research/Feature_selection/Rcode/version2015/OCFS_vnov28.22014_v11.R")

#source("/home/kuppal3/Research/Feature_selection/Rcode/version2015/OCFS_vapr282015.R")

#source("/home/kuppal3/Research/Feature_selection/Rcode/version2015/OCFS_vapr282015_v5_2.R")

#source("/home/kuppal3/Research/Feature_selection/Rcode/version2015/OCFS_vapr282015_v11.R")

args<-commandArgs(trailingOnly=TRUE)
sname<-paste("/home/kuppal2/Documents/Projects/xmsPANDA/Other/scripts/OCFS_",args[9],".R",sep="")
source(sname)
print(sname)
data_loc<-"/home/kuppal2/Documents/Projects/xmsPANDA/Other/Datasets/MAQCII_BreastCancer/"
setwd(data_loc)
load("MaqcIIbr.Rda")
outloc<-paste(data_loc,"OCFS_ER",args[9],"_",args[10],"_",args[11],"/",sep="")

trainm<-MaqcIIbr$trainx
testm<-MaqcIIbr$testx
trainclass<-MaqcIIbr$trainER #PCRvsRD
testclass<-MaqcIIbr$testER #PCRvsRD

#trainclass<-MaqcIIbr$trainPCRvsRD
#testclass<-MaqcIIbr$testPCRvsRD

trainm<-trainm[,-c(22284)]
testm<-testm[,-c(22284)]
trainm<-apply(trainm,2,as.numeric)
testm<-apply(testm,2,as.numeric)



#outloc<-"/home/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/OCFSv21itr1_LIMMApres1novarsel_l0.45f0.45c0.05minsel0.3randbehavfeatw0.01_local/"

trainm<-cbind(trainclass,trainm)
testm<-cbind(testclass,testm)

trainm<-na.omit(trainm)
testm<-na.omit(testm)

#outloc<-"/home/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/OCFSv21itr1_LIMMApres1novarsel_l0.45f0.45c0.05minsel0.3randbehavfeatw0.01_local/"
#outloc<-"/home/kuppal3/Research/Feature_selection/Datasets/MAQCII_BreastCancer/OCFSvmay2415v25_itr3PCRvsRD_LassoRFELIMMAELpres1nonesel_l0.25f0.45c0.25_top5maxitrs100minselmedianrankbehavfeatw0.01_CV2accA100B1wrand6methodsmax100reptop5/"






#OCFSvmay2415v2reg_itr1_LassoRFELIMMAELpres1backwsel_l0.25f0.45c0.25_top10pctmaxitrs100minselmedianrandbehavfeatw0.01_CV2accA100B1wrand6methodsmax100wrand/"

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
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("limma"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))


CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("lasso"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("rfe"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("elasticnet"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

if(FALSE){
CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("rf"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))
}

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("f.test"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
scheme_val="one-vs-all",
iter_learn=1,boostweight=rep(0,dim(trainm)[2]))

#1
CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=as.numeric(args[10]),
minnum=3,
stepitr=1,
gsmethods=c("limma","lasso","rfe","elasticnet", "f.test"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
pct_train=0.40,
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
varselmethod="none",
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
weightA<-as.numeric(args[1]),
weightB<-as.numeric(args[2]),
weightC<-as.numeric(args[3]),
weightD<-as.numeric(args[4]),
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=as.numeric(args[6]),
confusionprob=as.numeric(args[7]),
leaderprob=as.numeric(args[8]),
wmax=1,
wmin=1,
behavior_reset_itr=5,
maxitrreset=3,
num_neighbors=3,
minselect.pct=0.5,
evalMode="CV2",
minfitnessthresh=50,
maxnum=as.numeric(args[10]),minnum=3,inertia_method=args[5],particlebehav_method="randbased",constriction_factor=1,
select.global.best=TRUE,numnodes=4,evalFunc=eval_fit_kfold_diff,itr.terminate=FALSE,train.pct=0.8))

feat_ind<-psores$bestfeatlist
feat_names<-psores$bestfeatnames

scoringmatrix<-as.data.frame(psores$scoringmatrix)
print(scoringmatrix)
print(feat_names[feat_ind])

save(psores,file="psores.Rda")
print("Complete")
