library(class)
knn_cv=function(v,x,y,k){

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
knn_error <- matrix(0,v, k)  # we set K=30 before, it can be changed to any number<100.

for ( i in 1:v)
{


g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g,]

for ( j in 1:k)
{
knn_cv <- knn(temptrain,temptest,tempclass, k=j) 

#knn_cv <- knn(as.matrix(temptrain), as.matrix(temptest),cl=y, k=2*j-1) 
#knn_table<-table(knn_cv,cv_class[group[i:k],1])

knn_table<-table(knn_cv,y[g,])


knn_error[i,j]<-(1-sum(diag(knn_table))/length(g))

}

}
avg.error <- apply(knn_error, 2, mean)
#avg.error<-apply(knn_error,2,mean)
sd.error<-apply(knn_error,2,sd)/sqrt(n1)
limit<-min(avg.error)+sd.error[which.min(avg.error)] # 1 sd criterion
cv_knn<-max(which(avg.error<limit))
best_k<-which (avg.error==min(avg.error))

return(list(num=best_k,error=avg.error[best_k]))

#return(list(num=best_k,error=min_err, avg=avg.error))
}
