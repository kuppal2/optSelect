svm_cv <-
function(v,x,y,kname="radial",errortype="CV",conflevel=95,setseed=321){

num_samp=dim(x)[1]

num_datasets= floor(num_samp)
n1<-floor(num_samp/v)
n2<-num_samp-n1*v
n3<-v-n2

ind<-rep(c(n1,n1+1),c(n3,n2))
ind<-diffinv(ind)
min_err=1
best_k=1

###Do not change###
if(is.na(setseed)==FALSE){
set.seed(setseed)
}
group<-sample(1:num_samp,num_samp, replace=FALSE)


itr=0
#svm_acc <- matrix(0,v)  # we set K=30 before, it can be changed to any number<100.
svm_acc<-rep(0,v)
for ( i in 1:v)
{
g<-group[(ind[i]+1):ind[i+1]]
temptest<-x[g,]
temptrain <-x[-g,]
tempclass <-y[-g]
testclass<-y[g]

mod_cv <- svm(x=temptrain,y=tempclass, type="C",kernel=kname) 
predfit<-predict(mod_cv,temptest)
svm_table<-table(predfit,testclass)

class_names<-rownames(svm_table)
beracc<-{}
totacc<-length(which(predfit==testclass))/length(testclass)
for(c in 1:dim(svm_table)[1]){
testclass_ind<-which(testclass==class_names[c])
beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
}

beracc<-mean(beracc,na.rm=TRUE)

if(errortype=="CV"){
	svm_acc[i]<-(totacc*100)	
}else{
if(errortype=="AUC"){
	pred_acc<-multiclass.roc(testclass,as.numeric(predfit))
	pred_acc_orig<-pred_acc$auc[1]
	auc_acc<-pred_acc_orig
	

	svm_acc[i]<-(auc_acc*100)
}else{
svm_acc[i]<-(beracc*100)
}
}

}
avg_acc <-mean(svm_acc,na.rm=TRUE)
sd_acc<-sd(svm_acc,na.rm=TRUE)

#limit<-avg_acc-(sd.error*(avg_acc) # 1 sd criterion


#return(list(error=avg_acc,sderror=sd.error))
probval<-(1-(conflevel*0.01))/2
probval<-1-probval

error <- qnorm(probval)*sd_acc/sqrt(length(y))

leftconfint<-avg_acc-error
rightconfint<-avg_acc+error


return(list(mean_acc=avg_acc,sd_acc=sd_acc, acc_each_fold=svm_acc,confint=c(leftconfint,rightconfint)))
#return(list(num=best_k,error=min_err, avg=avg_acc))
}
