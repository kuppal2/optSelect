#options(echo=FALSE)
library(e1071)
library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]
#datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/count_macs_gal_20ormore_lessthan10_len2to5_patterns_normbylength.csv", na.strings="", sep=",", header=TRUE)
#datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/Rawdata/count_macs_gal_20ormore_lessthan10_len2to5_patterns_normbylength_2000samps.csv", na.strings="", header=TRUE)

#datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/Rawdata/count_macs_gal_25ormore_lessthan5.41_len2to6_patterns_normbylength.csv", na.strings="", sep=",", header=TRUE)
#data_dim<-dim(datafile)

#id<-sample(1:data_dim[1],size=0.40*data_dim[1],replace=F)

#datafile<-datafile[id,]



#try(system("mkdir /home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/Output/0.002to0.03_0.001step_ber_radial_fitfunc0.08_itr1/", intern = TRUE, ignore.stderr = TRUE))

#outloc<-"/home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/Output/5to80feats_10fold_radial_fitfunc0.05_itr3/"

#/home/stu/kuppal3/Research/Huira/Output/CMA_results/0.005to0.03_0.001step_ber_radial_fitfunc0.08/

#datafile<-read.csv("/home/stu/kuppal3/Research/Huira/Output/norm_vs_notnorm/count_dmrs_en2to5_patterns_norm.csv", na.strings="", sep=",", header=TRUE)

datafile<-read.csv("/home/stu/kuppal3/Research/Huira/Output/cpg_dist_gccont_obsexp_seq2to5exactpatterns.csv", na.strings="", sep=",", header=TRUE)

outloc<-"/home/stu/kuppal3/Research/Huira/Output/CMA_results/allfeats1to50step_5radial_fitfunc0.05_itr2/"
data_dim<-dim(datafile)
classA=levels(datafile[,data_dim[2]])[1]
classB=levels(datafile[,data_dim[2]])[2]

#print(paste(classA,":", length(classA), sep=""))
#print(paste(classB,":", length(classB), sep=""))

id<-sample(1:data_dim[1],size=0.40*data_dim[1],replace=F)

#84 test samples
testm<-datafile[id,]

ncid<-datafile[-id,]

write.csv(testm[,1], paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(ncid[,1], paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

testm<-testm[,-c(1)]
ncid<-ncid[,-c(1)]

#write.csv(testm, paste(outloc,"orig30%_40%test.csv",sep=""), row.names=FALSE)
#write.csv(ncid, paste(outloc,"orig30%_60%train.csv",sep=""), row.names=FALSE)

data_dim<-dim(ncid)
print(data_dim)


nci_x<-ncid[,-c(data_dim[2])]
nci_y<-ncid[,data_dim[2]]
nci_xm<-as.matrix(nci_x)

#num_g<-0.02*data_dim[2]
#print("num of genes")
#print(num_g)

kname="radial"
numfolds<-10
bestmod<-0

#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

train<-GenerateLearningsets(y=nci_y, method="CV", fold=10, strat=TRUE)
print("Initial misclassification:")


#icla<-classification(nci_xm, nci_y, learningsets=train, classifier=knnCMA, nbgene=data_dim[2]-1)

#ieval<-evaluation(icla, measure="misclassification")

#show(ieval)

#for(ng in seq(0.001,0.01,0.001))
#for(num_g in seq(5,80,5))
#{
#num_g=ng*data_dim[2]
#print("num genes")
#print(num_g)

maxnum=0.3*data_dim[2]

for(m in 1:length(methods))
{
varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=methods[m])
genelist={}
for(i in 1:numfolds)
{
        genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

}
genelist<-unique(genelist)

for(num_g in seq(1,30,5))
{

trainset<-nci_xm[,genelist[1:num_g]]

model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

foldacc<-model$tot.accuracy

fitfunc<-100*(foldacc)+0.05*(1-num_g)

if(fitfunc>bestmod)
{
        bestmod<-fitfunc
        bestmethod<-methods[m]
        bestgenelist<-genelist[1:num_g]
}

print(paste(methods[m],":",foldacc,sep=""))
}
}
print("Complete")
print(paste("numgenes selected:",length(bestgenelist),sep=""))
print(paste("best acc:", bestmod, sep=""))
print(paste("best method:", bestmethod, sep=""))

modtrain<-ncid[,c(bestgenelist,data_dim[2])]
modtest<-testm[,c(bestgenelist, data_dim[2])]

model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")



        pred_train<-predict(model_train_valid, testm[,bestgenelist])

        test.table<-table(pred_train, testm[,data_dim[2]])
print("test predicted table")
print(test.table)

error<-1-sum(diag(test.table))/(dim(modtest)[1])
print("Test accuracy is ")
print(1-error)

print(paste(classA,":", length(which(testm[,data_dim]==classA)), sep=""))
print(paste(classB,":", length(which(testm[,data_dim]==classB)), sep=""))


printbest<-paste(bestmethod, ":", bestmod, sep="")

write.csv(printbest, paste(outloc,"bestmodel.csv",sep=""))
bestgenelist<-as.matrix(bestgenelist)
write.csv(modtrain, paste(outloc,"modtrain.csv",sep=""), row.names=FALSE)
write.csv(modtest, paste(outloc,"modtest.csv",sep=""), row.names=FALSE)
write.csv(bestgenelist, paste(outloc,"bestgenelist.csv",sep=""), row.names=FALSE)
