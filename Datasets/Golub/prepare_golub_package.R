data_loc<-"/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Datasets/Golub/"
#data_loc<-"/home/kuppal2/Documents/Projects/xmsPANDA/Other/Datasets/Golub/"
setwd(data_loc)
#load("/home/stu/kuppal3/Research/Feature_selection/Datasets/MAQCII_BreastCancer/MaqcIIbr.Rda")


outloc<-paste(data_loc,"OCFSv062016_v32_Golub_sensitivity_itr",args[9],"/",sep="")

load("Golub.rda")

dir.create(outloc)
setwd(outloc)

trainm<-Golub$X
testm<-Golub$Xt
trainclass<-Golub$y
testclass<-Golub$yt


######Golub data 
cnames<-paste("var",seq(1,dim(trainm)[2]),sep="")
colnames(trainm)<-cnames
colnames(testm)<-cnames
trainm<-t(trainm)
testm<-t(testm)
trainm[trainm < 100] <- 100
trainm[trainm > 16000] <- 16000
testm[testm < 100] <- 100
testm[testm > 16000] <- 16000

mmfilt <- function(x,r = 5, d = 500, na.rm = TRUE) {
 minval <- min(x, na.rm = na.rm)
 maxval <- max(x, na.rm = na.rm)
 (maxval/minval > r) && (maxval - minval > d)
}

good<-apply(trainm,1,mmfilt)

trainm<-trainm[good,]
testm<-testm[good,]
trainm<-log10(trainm+1)
testm<-log10(testm+1)
trainm<-t(trainm)
testm<-t(testm)
trainm<-cbind(trainclass,trainm)
testm<-cbind(testclass,testm)
trainm<-na.omit(trainm)
testm<-na.omit(testm)

dir.create(outloc)
setwd(outloc)

trainm<-as.matrix(trainm)
testm<-as.matrix(testm)
trainclass<-trainm[,1] #CMAres$modtrainclass
testclass<-testm[,1] #CMAres$modtestclass
trainm<-trainm[,-c(1)] #CMAres$modtrainmata
testm<-testm[,-c(1)] #CMAres$modtestmata

dir.create(outloc)
setwd(outloc)

Golub=new("list")
Golub<-list("trainm"=trainm,"testm"=testm,"trainclass"=trainclass,"testclass"=testclass)

save(Golub,file="Golub_package.Rda")
