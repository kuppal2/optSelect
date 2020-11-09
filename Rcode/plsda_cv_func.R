library(class)
library(plsgenomics)
library(pls)

plsda_cv<-function(v,x,y,ncomp,nruncv,errortype="total",conflevel=99){

num_samp=dim(x)[1]

num_datasets= floor(num_samp)
n1<-floor(num_samp/v)
n2<-num_samp-n1*v
n3<-v-n2

ind<-rep(c(n1,n1+1),c(n3,n2))
ind<-diffinv(ind)
min_err=1
best_k=1

group<-sample(1:num_samp,num_samp, replace=FALSE)


itr=0
#plsda_error <- matrix(0,v)  # we set K=30 before, it can be changed to any number<100.
plsda_error<-rep(0,v)
for ( i in 1:v)
{
g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g]
testclass<-y[g]

#temptest<-as.data.frame(temptest)
#temptrain<-as.matrix(temptrain)

print(g)

#print(dim(temptrain))
#print(dim(temptest))

#plsda_cv <- plsda(x=temptrain,y=tempclass, type="C",kernel=kname) 

#opt_comp<-pls.lda.cv(Xtrain=temptrain, Ytrain=tempclass,  ncomp=c(1:10), nruncv=10, alpha=2/3, priors=NULL)
plsda_pred<-pls.lda(Xtrain=temptrain,Ytrain=tempclass,ncomp=ncomp,nruncv=nruncv,Xtest=temptest)
#predfit<-predict(plsda_pred,temptest)

#print(length(which(plsda_pred$predclass==testclass)))

print(testclass)
print(plsda_pred$predclass)

plsda_totacc<-length(which(plsda_pred$predclass==testclass))/length(testclass)

if(FALSE){

plsda_table<-table(plsda_pred$predclass,testclass)
print(plsda_table)

plsda_totacc<-sum(diag(plsda_table))/length(testclass)
if(errortype=="BER"){
plsda_bererror<-apply(plsda_table,2,function(x){x/sum(x)})
plsda_bererror<-replace(plsda_bererror,which(is.na(plsda_bererror)==TRUE),1)
plsda_bacc<-mean(diag(plsda_bererror))

plsda_totacc<-plsda_bacc
}
}
plsda_error[i]<-(plsda_totacc*100)

}
avgacc <-mean(plsda_error)
sdacc<-sd(plsda_error)

probval<-(1-(conflevel*0.01))/2
probval<-1-probval
print(probval)
error <- qnorm(probval)*sdacc/sqrt(length(y))

leftconfint<-avgacc-error
rightconfint<-avgacc+error

#limit<-avg.error-(sd.error*(avg.error) # 1 sd criterion
#print(avgacc)
#print(sdacc)

return(list(mean_acc=avgacc,sd_acc=sdacc, acc_each_fold=plsda_error,confint=c(leftconfint,rightconfint)))

#return(list(num=best_k,error=min_err, avg=avg.error))
}
