classlabels<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/GSE19711/Classlabels_all.txt",sep="\t",header=TRUE)

d1<-read.table("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/GSE19711/GSE19711_series_matrix.txt",sep="\t",header=TRUE)


rownames(d1)<-as.character(d1$ID_REF)

d1<-d1[,-c(1)]

d1<-as.data.frame(d1)

d1<-d1[-c(27579),]

d2<-apply(d1,1,function(x){x[which(is.na(x)==TRUE)]<-mean(x,na.rm=TRUE);return(x)})


d2<-round(d2,3)

set.seed(555)
train_ind<-sample(size=0.7*540,x=1:540)

table(classlabels$Class[train_ind])
table(classlabels$Class[-train_ind])

#rows samples; genes columns

X<-d2[train_ind,]
Xt<-d2[-train_ind,]

Y<-classlabels$Class[train_ind]
Yt<-classlabels$Class[-train_ind]

OvarianCancer<-new("list")
OvarianCancer$X<-X
OvarianCancer$Xt<-Xt
OvarianCancer$Y<-Y
OvarianCancer$Yt<-Yt

save(OvarianCancer,file="/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/GSE19711/OvarianCancer.Rda")

