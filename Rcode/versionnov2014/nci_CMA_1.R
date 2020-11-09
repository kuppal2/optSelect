options(echo=FALSE)

library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(CMA)
ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_final.csv", sep=",", header=T)
data_dim<-dim(ncid)
data_dim

nci_x<-ncid[,-c(1)]
nci_y<-ncid[,1]
nci_xm<-as.matrix(nci_x)

train<-GenerateLearningsets(y=nci_y, method="LOOCV", strat=TRUE)
print("t.test 1 vs all")

gcla<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="t.test", scheme="one-vs-all"), classifier=knnCMA)

eval<-evaluation(gcla, measure="misclassification")

show(eval)

print("elastic net 1 vs all")
elastc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="elasticnet", scheme="one-vs-all"), classifier=knnCMA)

elasteval<-evaluation(elastc, measure="misclassification")

show(elasteval)

print("rfe SVM 1 vs all")
rfesvmt<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="rfe", scheme="one-vs-all"), classifier=knnCMA)

rfeeval<-evaluation(rfesvmt, measure="misclassification")

show(rfeeval)

print("random forest 1 vs all")
rfc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="rf", scheme="one-vs-all"), classifier=knnCMA)

rfeval<-evaluation(rfc, measure="misclassification")

show(rfeval)

print("lasso 1 vs all")
lassc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="lasso", scheme="one-vs-all"), classifier=knnCMA)

lassoeval<-evaluation(lassc, measure="misclassification")

show(lassoeval)

print("rf multiclass")
rfmult<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="rf", scheme="multiclass"), classifier=knnCMA)

rfmeval<-evaluation(rfmult, measure="misclassification")

show(rfmeval)

print("f.test 1 vs all")
ftc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="f.test", scheme="multiclass"), classifier=knnCMA)

ftceval<-evaluation(ftc, measure="misclassification")

show(ftceval)


