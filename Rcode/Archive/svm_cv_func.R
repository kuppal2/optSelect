library(class)
svm_cv=function(v,x,y){

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
svm_error <- matrix(0,v)  # we set K=30 before, it can be changed to any number<100.

for ( i in 1:v)
{


g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g]
testclass<-y[g]

svm_cv <- svm(x=temptrain,y=tempclass, type="C") 

predfit<-predict(svm_cv,temptest)

#knn_cv <- knn(as.matrix(temptrain), as.matrix(temptest),cl=y, k=2*j-1) 
#knn_table<-table(knn_cv,cv_class[group[i:k],1])

svm_table<-table(predfit,testclass)

svm_toterror<-(1-sum(diag(svm_table))/length(testclass))
svm_error<-apply(svm_table,2,function(x){x/sum(x)})
svm_error<-1-mean(diag(svm_error))



}
avg.error <- apply(svm_error, 2, mean)
#avg.error<-apply(svm_error,2,mean)
sd.error<-apply(svm_error,2,sd)
#limit<-avg.error-(sd.error*(avg.error) # 1 sd criterion

return(list(error=avg.error,sderror=sd.error))

#return(list(num=best_k,error=min_err, avg=avg.error))
}
