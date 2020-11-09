options(echo=FALSE)

library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
#library(CMA)
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/khan_R/khan_d.csv", sep="\t", header=T)

ncid<-read.delim("SPECTF.train")
#ncid<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#ncid<-ncid[,-c(1)]
data_dim<-dim(ncid)
data_dim

nci_x<-ncid[,-c(1)]
nci_y<-ncid[,1]
nci_xm<-as.matrix(nci_x)

num_g<-100
print("num of genes")
print(num_g)
train<-GenerateLearningsets(y=nci_y, method="LOOCV", strat=TRUE)
print("Initial misclassification:")


icla<-classification(nci_xm, nci_y, learningsets=train, classifier=knnCMA, nbgene=data_dim[2]-1)

ieval<-evaluation(icla, measure="misclassification")

show(ieval)


tcla<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="t.test", scheme="one-vs-all"), classifier=knnCMA, nbgene=num_g)

teval<-evaluation(tcla, measure="misclassification")

show(teval)

print("elastic net 1 vs all")
elastc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="elasticnet", scheme="one-vs-all"), classifier=knnCMA, nbgene=num_g)

elasteval<-evaluation(elastc, measure="misclassification")

show(elasteval)

print("rfe SVM 1 vs all")
rfesvmt<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="rfe", scheme="one-vs-all"), classifier=knnCMA, nbgene=num_g )

rfeeval<-evaluation(rfesvmt, measure="misclassification")

show(rfeeval)

print("random forest 1 vs all")
rfc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="rf", scheme="one-vs-all"), classifier=knnCMA, nbgene=num_g)

rfeval<-evaluation(rfc, measure="misclassification")

show(rfeval)

print("lasso 1 vs all")
lassc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="lasso", scheme="one-vs-all"), classifier=knnCMA, nbgene=num_g)

lassoeval<-evaluation(lassc, measure="misclassification")

show(lassoeval)

print("rf multiclass")
rfmult<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="rf", scheme="multiclass"), classifier=knnCMA, nbgene=num_g)

rfmeval<-evaluation(rfmult, measure="misclassification")

show(rfmeval)

print("f.test multiclass")
ftc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="f.test", scheme="multiclass"), classifier=knnCMA, nbgene=num_g)
ftceval<-evaluation(ftc, measure="misclassification")
show(ftceval)

print("kruskal.test multiclass")
ktc<-classification(nci_xm, nci_y, learningsets=train, genesellist=list(method="kruskal.test", scheme="multiclass"), classifier=knnCMA, nbgene=num_g)

ktceval<-evaluation(ktc, measure="misclassification")

show(ktceval)



#dalike<-list(tcla, elastc, rfesvmt, rfc, lassc, rfmult, ftc, ktc)

#comparison<-compare(dalike, plot=TRUE, measure=c("misclassification", "brier score", "average probability"))
#pdf("/home/stu/kuppal3/karan_libs/R_karan/golub_R/golub_classplot.pdf")

#write(comparison, file="/home/stu/kuppal3/karan_libs/R_karan/golub_R/golub_class.txt")
#save(comparison, file="golub_classplot.RData")


#print(comparison)

#dev.off()
