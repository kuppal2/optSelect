
#.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
library(snow)
library(e1071)
library(yaImpute)
library(pROC)
library(bioDist)
#library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")

library(CMA)
library(expm)

cl<-makeCluster(20)

#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.1.R")
#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/OCFS_vnov252014.R")

#same results
#source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov26.22014_v11.R")

source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov302014_v26.2.R")

#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/svm_cv_v1.1.R")

#source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_nov172014_backward.R")


datafile=read.csv("/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/LuCa_RLGS_Features_modified_051409_update_withevents.csv", sep=",", header=TRUE)
#outloc<-"/home/stu/kuppal3/Research/Lung_cancer_sequences/Data_may172009/CMA_PSO_Results/itr9_12ormore_ber/"

datafile<-as.data.frame(datafile)
datafile<-datafile[,-c(1)]


temp2=t(datafile)
temp2=apply(temp2, 2, function(x){which(x=="MD")})
temp2=unlist(temp2)
temp2=unique(temp2)
datafile=datafile[,-c(temp2)]

rm(temp2)

data_dim=dim(datafile)[2]


#*****Remove the last 15 categorical variables******
datafile=datafile[,-c((data_dim-15):(data_dim-1))]

data_dim=dim(datafile)[2]

classA=datafile[which(datafile[,data_dim]==0),]
classB=datafile[which(datafile[,data_dim]>=12),]

classA[,data_dim]="Zero"
classB[,data_dim]="TwelveorMore"

data_dimA=dim(classA)
data_dimB=dim(classB)

#id<-sample(1:data_dimA[1],size=data_dimB[1],replace=F)
#classA=classA[id,]

datafile=rbind(classA,classB)

id<-sample(1:dim(datafile)[1],size=0.6*(dim(datafile)[1]),replace=F)
traind<-datafile[id,]
testd<-datafile[-id,]
trainy<-traind[,data_dim]
testy<-testd[,data_dim]
traind<-cbind(trainy,traind[,-c(data_dim)])
testd<-cbind(testy,testd[,-c(data_dim)])

trainm<-traind
testm<-testd
trainm<-na.omit(trainm)
testm<-na.omit(testm)

outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/LungCancer/outputOCFSv26.2_CMAall_minsel0.5_CV2_leaderprob0.3_featw0.01_6methods_12ormore/"





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


CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
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
iter_learn=1)


CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
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
iter_learn=1)

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
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
iter_learn=1)

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
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
iter_learn=1)


CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
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
iter_learn=1)

CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
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
iter_learn=1)

#1
CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
maxnum=100,
minnum=3,
stepitr=1,
gsmethods=c("limma","lasso","rfe","elasticnet", "f.test", "rf"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
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
iter_learn=1)

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
accuracyweightA=10,
accuracyweightB=20,
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=0.45,
confusionprob=0.20,
leaderprob=0.3,
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
