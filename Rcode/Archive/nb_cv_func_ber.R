library(e1071)
naivebayes_cv=function(v,x,y){

num_samp=dim(x)[1]
#y<-as.matrix(y)
num_datasets= floor(num_samp)
n1<-floor(num_samp/v)
n2<-num_samp-n1*v
n3<-v-n2

ind<-rep(c(n1,n1+1),c(n3,n2))
ind<-diffinv(ind)
min_err=1
best_k=1

#tune_degree<-degree
#tune_cost<-cost
#tune_gamma<-gamma

group<-sample(1:num_samp,num_samp, replace=FALSE)


itr=0
nb_error <- matrix(0,v)  # we set K=30 before, it can be changed to any number<100.
#i<-v
for ( i in 1:v)
{


g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g]

nb_cv <- naiveBayes(temptrain,tempclass) 

test_pred<-predict(nb_cv, temptest)


#knn_cv <- knn(as.matrix(temptrain), as.matrix(temptest),cl=y, k=2*j-1) 
#knn_table<-table(svm_cv,cv_class[group[i:k],1])

nb_table<-table(test_pred,y[g])
#ind_class_A<-which(y[g]=="'\\'(11.5-inf)\\''")
ind_class_A<-which(y[g]=="1")
num_A<-length(ind_class_A)
if(num_A>0)
{
ber_a<-nb_table[2]/num_A
}
else
{
ber_a<-0
}
#ind_class_B<-which(y[g]=="'\\'(-inf-11.5]\\''")
ind_class_B<-which(y[g]=="2")

num_B<-length(ind_class_B)
if(num_B>0)
{
ber_b<-nb_table[3]/num_B
}
else
{
ber_b<-0
}
nb_error[i]<-(ber_a+ber_b)/2
#svm_error[i,j]<-(1-sum(diag(svm_table))/length(g))



}
#print(svm_error)
avg.error <- apply(nb_error, 2, mean)
#avg.error<-apply(knn_error,2,mean)
sd.error<-apply(nb_error,2,sd)/sqrt(n1)
#limit<-min(avg.error)+sd.error[which.min(avg.error)] # 1 sd criterion
#cv_knn<-max(which(avg.error<limit))
#best_k<-which (avg.error==min(avg.error))

#return(list(num=best_k,error=avg.error[best_k]))

return(list(num=best_k,error=min(avg.error), avg=avg.error))
}
