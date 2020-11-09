
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


args<-commandArgs(trailingOnly=TRUE)
#source("/home/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vmay2415_v31.R")

#sname<-paste("/home/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_",args[9],".R",sep="")
sname<-paste("/home/kuppal2/Documents/Projects/xmsPANDA/Other/scripts/OCFS_",args[9],".R",sep="")
source(sname)
print(sname)

#data_loc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/" 
#data_loc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/Golub/"
data_loc<-"/home/kuppal2/Documents/Projects/xmsPANDA/Other/Datasets/IRIS/"
setwd(data_loc)

load("IRIS.Rda")

trainm<-Iris$X
testm<-Iris$Xt
trainclass<-Iris$Y
testclass<-Iris$Yt

cnames<-paste("var",seq(1,dim(trainm)[2]),sep="")
colnames(trainm)<-cnames
colnames(testm)<-cnames

args<-commandArgs(trailingOnly=TRUE)
trainm<-na.omit(trainm)
testm<-na.omit(testm)

#outloc<-paste("/home/kuppal3/Research/Feature_selection/Datasets/IRIS_Probe/OCFSv062016_v32_sensitivity_itr",args[9],"/",sep="")

weightA<-as.numeric(args[1])
weightB<-as.numeric(args[2])
weightC<-as.numeric(args[3])
weightD<-as.numeric(args[4])

#outloc<-paste(data_loc,"OCFSv062016_v32_sensitivity_itr",args[9],"/",sep="")
outloc<-paste(data_loc,"OCFS_iris",args[9],"_",args[10],"_",weightA,"_",args[11],"_",args[5],"/",sep="")

dir.create(outloc)
setwd(outloc)

trainm<-as.matrix(trainm)
testm<-as.matrix(testm)

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


# modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y
if(FALSE)
{
trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass
learningsets<-CMAres$learningsets
}


print(apply(trainm,2,mean))
d_dim<-dim(trainm)

print("Original dimension")
print(d_dim)

#2 call run_pso function()
system.time(psores<-run_pso(outloc=outloc,trainm,trainclass,testm,testclass,transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=10,
global_max_itr=3,
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
select.global.best=TRUE,numnodes=2,evalFunc=eval_fit_kfold_diff,itr.terminate=FALSE,train.pct=as.numeric(args[11])))


feat_ind<-psores$bestfeatlist
feat_names<-psores$bestfeatnames

scoringmatrix<-as.data.frame(psores$scoringmatrix)
print(scoringmatrix)
print(feat_names[feat_ind])

save(psores,file="psores.Rda")
print("Complete")
