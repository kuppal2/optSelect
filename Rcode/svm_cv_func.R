library(class)
svm_cv<-function(v,x,y,kname="radial"){

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
#svm_error <- matrix(0,v)  # we set K=30 before, it can be changed to any number<100.
svm_error<-rep(0,v)
for ( i in 1:v)
{
g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g]
testclass<-y[g]

svm_cv <- svm(x=temptrain,y=tempclass, type="C",kernel=kname) 
predfit<-predict(svm_cv,temptest)
svm_table<-table(predfit,testclass)


svm_toterror<-(1-sum(diag(svm_table))/length(testclass))
svm_bererror<-apply(svm_table,2,function(x){x/sum(x)})
svm_bererror<-replace(svm_bererror,which(is.na(svm_bererror)==TRUE),1)
svm_bererror<-1-mean(diag(svm_bererror))
svm_error[i]<-100-(svm_bererror*100)

}
avg.error <-mean(svm_error)
sd.error<-sd(svm_error)

#limit<-avg.error-(sd.error*(avg.error) # 1 sd criterion
print(avg.error)
print(sd.error)

return(list(error=avg.error,sderror=sd.error))

#return(list(num=best_k,error=min_err, avg=avg.error))
}
