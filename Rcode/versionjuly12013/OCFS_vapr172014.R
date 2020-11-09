
#1)
###################################################################
ocfs.run<-function(train)
{

print("Stage 1: Consensus based")
print("Stage 1: Consensus based")
#CMAres<-preProcess(datafile, outloc, percentTest,norm_method,classindex, upclasssep, dclasssep, removeindex, maxfacts)
CMAres<-performCMA(traind,testd, outloc, maxfeatspercent, minfeats, stepitr, methods, percentTest, featWeight,accWeight, kname, max.nochange.itrs, minpresent, norm_method, tolerance, classindex, upclasssep, dclasssep, removeindex, maxfacts,numfolds=k,evalmethod,CVfoldthresh,backward.sel,scheme_val,iter_learn)

#if(FALSE)
{
print("Stage 2: Selecting optimal subset using PSO")
#system.time(

#psores<-performPSO(trainx, testx, trainy, testy, outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.04, fitfunc)

psores<-performPSO(trainm=CMAres$modtraindata, CMAres$modtestdata, CMAres$modtrainclass, CMAres$modtestclass,outloc, numitrs, reinititrs, numpart, c1, c2, kname, percentTest, 0.0, 1, fitfunc)

trainm<-CMAres$modtraindata
testm<-CMAres$modtestdata
trainclass<-CMAres$modtrainclass
testclass<-CMAres$modtestclass


d_dim<-dim(trainm)
}
}
###################################################################
	

#2)
###################################################################
#function to find nearest neighbor of a particle
nn_search<-function(particle_ind, part_group)
{
	partm<-as.matrix(part_group[particle_ind,])
	
	#nn_arr<-array(0,2)
	dist_mat<-dist(as.matrix(part_group))
	dist_mat<-as.matrix(dist_mat)
	nn_order<-order(dist_mat[particle_ind,])
	nn_order<-nn_order[-c(1)]
	return(nn_order)
}
###################################################################


#3)
	#function to evaluate k-fold CV
	###################################################################
	eval_fit_kfold_diff<-function(particle,trainm,trainclass,accuracyweight=1,featweight=0.06, kfold=10, classifier="svm")
	{
		num_feat<-0
		
		#select the columns with selected features 
		ind<-which(particle==1)
		
		col_sel<-ind
		num_feat<-length(col_sel)
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			folderror<-{}
			 for(f in 1:1)
                        {
                        	
									if(classifier=="svm"){
									folderror_cur<-svm_cv(x=trainset[,-c(1)],y=trainset[,1],v=kfold,kname="radial")
									}else{
										if(classifier=="plsda"){
									folderror_cur<-plsda_cv(x=trainset[,-c(1)],y=trainset[,1],v=kfold,kname="radial")
									}
										}
									print(folderror_cur)	
									folderror_cur<-folderror_cur$error #-(1.96*folderror_cur$sderror)
									folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			
                        }
                        folderror<-mean(folderror)-(1.96*sd(folderror)/(sqrt(length(folderror))))
						#folderror<-model$tot.accuracy
						#folderror<-(1-ber)*100
						rm(trainset)
		}
		else
		{
		folderror<-1
		}
		fitfunc<-(accuracyweight*(folderror))-(featweight*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		return(fitfunc)

	}
	###################################################################
	
	#4)
	###################################################################
	#Function to run binary PSO:
	#stochastic behavioral optimization
	#PSBBO: particle swarm behavior based optimization
	#final output is a vector that includes the optimum set of features
	
	performPSO<-function(trainm,trainclass,num_part=30,c1=2.05,c2=2.05,wmax=1,wmin=0.4,itr=5000,max_nochange_itr=50,svm_kernel="radial",accuracy_weight=1,feat_weight=0.06,
	num_folds=10,followerprob=0.4,confusionprob=0.1,behavior_reset_itr=50000,scale=TRUE, degree=NA, gamma=NA, coef0=0, cost=1, nu=NA,tolerance=NA,epsilon=NA,
shrinking=TRUE, fitted=TRUE, probability=TRUE,outloc,numnodes=4)
	{
	 
	 d<-dim(trainm)[2]
	 d_dim<-dim(trainm)

	c1<-2.05

	 c2<-2.05
	 
	 ll<-0
	 
	 ul<-1
	 fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-10000000000000000
	 prev_gbest<-fitness_gbest+1 
	 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
	 feat_names<-colnames(trainm)
	 feat_list<-feat_names

	no_change<-0
		
	global_best_index<-10000000000000000000
		
	min_fit_x<-1000000000000000

	min_fit_index<-100000000000000000
	
	#position
	x<-array(0, dim=c(num_part,d_dim[2]))

	#velocity
	v<-array(0, dim=c(num_part,d_dim[2]))
	
	x_gbest<-array(0, dim=c(d_dim[2]))

	p_gbest<-array(0, dim=c(d_dim[2]))
	   x_lbest<-array(0, dim=c(num_part, d_dim[2]))
	   count_feat<-array(0,dim=c(num_part))
	 
	for (row in 1:num_part)
	 {
		num_feat<-0
		ran<-runif(d_dim[2])
		
		for (col in 1:d_dim[2])		
		{
			
			if (ran[col]<0)
			{
				x[row,col]<-0
				x_lbest[row,col]<-0
				#num_feat<-num_feat+1

			}
			else
			
			{
				x[row,col]<-1
				x_lbest[row,col]<-1
				num_feat<-num_feat+1
				
				#col_sel[row,num_feat]<-col
				
			}
			
		}
		 count_feat[row]<-num_feat
	 
	 }
	 
	 num_featl<-d 
	 num_featg<-d

	cl<-makeCluster(numnodes)
	#clusterExport(cl, "x")

	clusterExport(cl, "eval_fit_kfold_diff")
	#clusterExport(cl, "trainm")
	#clusterExport(cl, "trainclass")
	#clusterExport(cl, "accuracyweight")
	#clusterExport(cl, "featweight")
	#clusterExport(cl, "k")
        #clusterExport(cl, "kname")	
	clusterExport(cl, "svm_cv")
	clusterExport(cl, "plsda_cv")
	clusterEvalQ<-function(cl, expr)
	clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

	#load a library on all clusters
	clusterEvalQ(cl, library(e1071))

 	agent_behavior<-runif(num_part,0,1)	
	itr_data={}
	for (k in 1:itr)
		{
			feat_sel<-0
		
			itr_val={}	
			print("iteration number: ") 
			print(k)

			min_fit_x<-1000000000000000
			 
			min_fit_index<-100000000000000000 
			part_list={}
			for (i in 1:num_part)
			{
				#each element of the list represents a particle
				part_list=c(part_list, c=list(sapply(x[i,],head)))
				
				if(k%%behavior_reset_itr==0){
					agent_behavior=runif(num_part,0,1)
				}	
			}
			
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,trainm,trainclass, kfold=numfolds)
				fitness_x<-sapply(res1,head)
				fitness_x<-(-1)*fitness_x
				
				
				min_fit_x<-min(fitness_x)
				min_fit_index<-which(fitness_x==min_fit_x)
				
				bestind<-runif(1,1,length(which(fitness_x==min_fit_x)))
				bestind<-min_fit_index[bestind]
				#print(bestind)
				numfeatl<-length(which(x[bestind[1],]==1))
				
				for (i in 1:num_part)
				{
					if(fitness_x[i]<fitness_lbest[i])
					{
					
						fitness_lbest[i]<-fitness_x[i]
						for (j in 1:d_dim[2])
						{
							
							x_lbest[i,j] <- x[i,j]
						
					}
					
				}
				
			
			 rank_vec<-rank(fitness_x)
			 
			 #update the global best and its fitness
			if(min_fit_x < fitness_gbest)
			{
				no_change<-0

				
						fitness_gbest<-min_fit_x
					
						global_best_index<-bestind
						num_featg<-num_featl
						print ("global fitness updated to :")
						print(fitness_gbest)
						print(min(fitness_x))
						print("min fit index")
						print(min_fit_index)
						print(global_best_index)
						print ("golbal best num features updated to")
						
						print(length(which(x[global_best_index,]==1)))
						
						#global_best_index<-round(runif(1,1,length(global_best_index)))
						
						for (j in 1:d_dim[2])
						{
							
							x_gbest[j]<-x[global_best_index,j]
							
						}
						print(length(which(x_gbest==1)))
			}
			else
			{
				no_change<-no_change+1

				if(no_change>max.nochange.itr)
				{
					#print("RE-INITIALIZING...")
				 	#stop(paste("No change for ",max.nochange.itr," iterations",sep=""))	
					 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					print(paste("No change for ",max.nochange.itr," iterations. Exiting PSO.",sep=""))
					break;
					print("exited")
					min_fit_x<-1000000000000000000000
					 for (row in 1:num_part)
					{
						num_feat<-0
					
						 #ran<-runif(1,0,1)

						for (col in 1:d_dim[2])

						{
							x[row,col]<-x_gbest[col]
							num_feat<-num_feat+1
						}

								#col_sel[row,num_feat]<-col

							#}

						}
						count_feat[row]<-num_feat

					}				


					no_change<-0
				}
			}
			 ###update the particle velocity and position
			for (i in 1:num_part)
			{
					feat_sel<-0
			      
				
				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*d_dim[2]))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				#w<-((exp(rank_vec[i]/num_part))/1.359)
				#constant; global search
				#w<-1				
				
				#random inertia;
				#w<-0.5+(runif(1,0,1)/2)
				
				#linearly increasing with rank
				w<-wmin+(((wmax-wmin)/num_part)*rank_vec[i])

				#w<-wmax-(((wmax-wmin)/num_part)*rank_vec[i])
				
				#w<-(wmax-wmin)*((itr-k)/itr)+wmin
				#print("x_lbest")
				#print(x_lbest[i,])
				#print(x[i,])		
			       x_curbest<-x_gbest
                                        if(agent_behavior[i]<confusionprob)
                                        {
                                             	ran<-runif(d_dim[2])
                                             
                				for (col in 1:d_dim[2]) 
                				{                               
                                                        
                        if (ran[col]<0)         
                        {
                                x_curbest[col]<-0

                        }else{

                                x_curbest[col]<-1

                        }

                }
                
                                        }else{
                                                if(agent_behavior[i]<(confusionprob+followerprob)){
                                                        x_curbest_ind<- nn_search(i,x) #getnearestneighbor
                                			#print("current best is ")
							#print(x_curbest_ind)
				                	x_curbest<-x[x_curbest_ind[1],]
						}else{
							x_curbest<-x_gbest
						}

                                        }

				
	
			       for (j in 1:d_dim[2])
				{
					r1<-runif(1,0,1)

					r2<-runif(1,0,1)

					r3<-runif(1,0,1)
							
					v[i,j]<-((w*v[i,j])+(c1*r1*(x_lbest[i,j]-x[i,j]))+(c2*r2*(x_curbest[j]-x[i,j])))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< (-6))
					{
						v[i,j]=-6
					}
					
					S<-1/(1+exp(-v[i,j]))
					
					if(S>r3)
					{
							x[i,j]<-1

							feat_sel<-feat_sel+1
							
							count_feat[i]<-feat_sel						

					}			
					else
					{
						x[i,j]<-0
						
					}
				}
					
			}		
			itr_val<-cbind(itr_val, k)
			itr_val<-cbind(itr_val, w)
			itr_val<-cbind(itr_val, num_featg)
			itr_val<-cbind(itr_val, fitness_gbest)
			
			itr_val<-cbind(itr_val, min_fit_x)
			itr_val<-cbind(itr_val, count_feat[min_fit_index])
			itr_data<-rbind(itr_data, itr_val)
			
		}
		

		

	return(x_gbest)
}

	
	###################################################################
	
	#5)	
	###################################################################
	#Function to evaluate results using 10-fold and/or test -dataset
	#1) permutation tests; bootstrap; k-fold, AUC
	#2) options for classifiers: svm, random forest, naive bayes
	#3) 
	evalresults_svm<-function(trainm, trainclass, testm, testclass, outloc,kfold=10,kname="radial")
	{
	  
	    model_train_valid<-svm(traindata,  trainclass,   kernel=kname, type="C", k=kfold)
		pred_train<-predict(model_train_valid, modtest)
		
		
		if(is.na(testm)==FALSE){
		pred_test<-predict(model_train_valid, testm)
		test_table<-table(pred_test, testclass)
		
		}
		return(list("kfoldacc"=model_train_valid$tot.accuracy,"testacc"=))
	}

###################################################################


#6)
###################################################################

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
###################################################################

#7)
####################################################################

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


return(list(mean_acc=avgacc,sd_acc=sdacc, acc_each_fold=plsda_error,confint=c(leftconfint,rightconfint)))

}





####################################################################



##8)
###################################################################

performCMA<-function(ncid, testm, outloc, maxnum, minnum, stepitr, gsmethods, percentTest,featweight, accuracyweight, kname, maxitrs, minpresent, norm_method, tolerance=1, classindex, upclasssep, dclasssep, removeindex, numfacts,numfolds=10,evalmethod="CV",CVfoldthresh=0.3,backward.sel=FALSE, scheme_val="one-vs-all",iter_learn=5)
{

data_dim<-dim(ncid)
write.csv(testm[,1], paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(ncid[,1], paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

filestr3<-paste(outloc, "original_traindata.csv", sep="")
write.table(ncid, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "original_testdata.csv", sep="")
write.table(testm, file=filestr3, sep=",", row.names=FALSE)


print("classindex is ")
print(classindex)


upsep<-upclasssep
dsep<-dclasssep

#ncid<-rbind(ncidA,ncidB)
print("dim of ncid is ")
print(dim(ncid))
print(levels(ncid[,classindex]))
#print(ncidB[1:4,1:10])
#testm[which(testm[,classindex]>=upsep),classindex]="A"
#testm[which(testm[,classindex]<=dsep),classindex]="B"
#testmA<-testm[which(testm[,classindex]=="A"),]
#testmB<-testm[which(testm[,classindex]=="B"),]
#testm<-rbind(testmA,testmB)

data_dim<-dim(ncid)
print(data_dim)


nci_x<-ncid[,-c(classindex)]
nci_y<-as.factor(ncid[,classindex])
rm(ncid)

nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm[,-c(classindex)])

test_y<-as.factor(testm[,classindex])
rm(testm)

#datafile=datafile[,-c(classindex)]


factcols<-lapply(1:dim(nci_xm)[2],function(i){
 factvec<-as.factor(nci_xm[,i])
 getlevels=levels(factvec)
 }
 )

factcols<-sapply(factcols,head,n=100000000000000000)

factnums<-lapply(1:dim(nci_xm)[2],function(i){
 factvec<-length(factcols[[i]])

 }
 )

factcols=which(factnums<=numfacts)
factnames=colnames(nci_xm)

print("length of factcols")
print(length(factcols))


trainfact=nci_xm[,c(factcols)]
testfact=test_xm[,c(factcols)]

trainfactnames=colnames(trainfact)

trainfact=makefactors(trainfact)
testfact=makefactors(testfact)
trainfact=as.data.frame(trainfact)
testfact=as.data.frame(testfact)

if(length(factcols)>0)
{
nci_xm=nci_xm[,-c(factcols)]
test_xm=test_xm[,-c(factcols)]

trainfact_check=apply(trainfact,2,as.numeric)
testfact_check=apply(testfact,2,as.numeric)

trainfact_num={}

{
for(i in 1:dim(trainfact_check)[2])
{
	trainfact_num=c(trainfact_num,is.na(trainfact_check[1,i]))
}

#print(trainfact_num[1:10])

trainfact_num_index=which(trainfact_num==TRUE)
print(trainfact_num_index)
}
#trainfactnames=colnames(trainfact[,trainfact_num_index])



#trainfact_num=which(is.na(trainfact_check))
if(length(trainfact_num_index)>0)
{
nci_xm=cbind(apply(nci_xm,2,as.numeric),trainfact[,-trainfact_num_index])
test_xm=cbind(apply(test_xm,2,as.numeric),testfact[,-trainfact_num_index])

trainfactnames=trainfactnames[trainfact_num_index]
trainfact=trainfact[,trainfact_num_index]
testfact=testfact[,trainfact_num_index]
print("trainfact samp ")
print(trainfact[1:10,1:3])
}
}
nci_xm=apply(nci_xm,2,as.numeric)
test_xm=apply(test_xm,2,as.numeric)
print("test class")
print(test_y[1:4])

print("orig train matrix")
print(nci_xm[1:5,1:10])
#nci_xm<-as.matrix(nci_xm)
#test_xm<-as.matrix(test_xm)
print("orig train matrix")
print(nci_xm[1:5,1:10])
print(as.numeric(nci_xm[1:3,3]))


bestmod<-0

train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=TRUE, niter=iter_learn)

scoringmatrix=matrix(0,data_dim[2]-1,length(gsmethods))

maxnum=round(maxnum*dim(nci_xm)[2])
print("maxnum is ")
print(maxnum)

if(FALSE)
{
varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=c("t.test"),scheme=scheme_val)
	genelist={}
	for(i in 1:numfolds)
	{
		genelist<-c(genelist,toplist(varsel,iter=i,maxnum, show=FALSE)$index)

	}
	print(head(genelist))

	print(length(genelist))
	genelist<-unlist(genelist)
	genelist<-genelist[order(genelist)]
	genelist<-unique(genelist)
origgenelist<-genelist	

nci_xm<-nci_xm[,genelist]
test_xm<-test_xm[,genelist]
}
print("# of genes left after filtering:")
print(dim(nci_xm))
mod_dim=dim(nci_xm)[2]
scoringmatrix=matrix(0,mod_dim,length(gsmethods))

class_labels<-levels(as.factor(nci_y))
num_classes<-length(class_labels)

#tolerance=0.5
for(m in 1:length(gsmethods))
{
	#varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme="pairwise")
	varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme=scheme_val)
	genelist={}
	
	
	for(i in 1:(numfolds*iter_learn))
	{
		#for(c in 1:num_classes){
		for(c in 1:num_classes){
		t1<-toplist(varsel,iter=i,k=maxnum, show=FALSE)

		if(num_classes>2){
		#genelist<-c(genelist,toplist(varsel,iter=i,dim(nci_xm)[2], show=FALSE)$index)

		genelist<-c(genelist,t1[[c]]$index)
		}else{
			genelist<-c(genelist,t1$index)
		}
		}

		#r1<-varsel@rankings[[1]][1,]
		#imp1<-varsel@importance[[1]][1,]
	}

	genelist_seltable<-sort(table(genelist), dec = TRUE)

	genelist_seltable<-genelist_seltable[which(genelist_seltable>=(CVfoldthresh*numfolds*iter_learn))]
	
	#if(length(genelist_good)>0)
	{
	good_genes<-names(genelist_seltable)
	
	good_genes<-as.numeric(good_genes)
	genelist<-unique(good_genes)

	#for(num_g in seq(minnum, maxnum, step))
	num_g=maxnum
	bestmod=0
	prevacc=0
	noimprovement=0
	bestgenelist={}
	#while((noimprovement<=totitrs) || num_g<maxnum)
	
	if(backward.sel==TRUE){
	
	while(num_g>minnum)
	{
		trainset<-nci_xm[,genelist[1:num_g]]
		model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)

		#fitval<-fitted(model)
		#fitval<-predict(model,trainset)
         #       tab1=table(fitval, nci_y)
          #      beracc=(tab1[1,1]/sum(tab1[,1]))+(tab1[2,2]/sum(tab1[,2]))
           #     beracc=0.5*beracc*100
	
		tenfoldacc<-model$tot.accuracy
		foldacc<-tenfoldacc	
		#foldacc<-0.7*(model$tot.accuracy)+0.3*(beracc)

		fitfunc<-accuracyweight*(foldacc)+featweight*(1-num_g/dim(nci_xm)[2])
		
		if(fitfunc>bestmod)
                {
                        bestmod<-fitfunc
                        bestmethod<-gsmethods[m]
                        bestgenelist<-genelist[1:num_g]
                        noimprovement=0
                }
                else
                {
                        #if(abs(prevacc-fitfunc)>tolerance)
                        if(abs(bestmod-fitfunc)<=tolerance)
                        {
                                noimprovement=0
				#noimprovement+1
                        	bestgenelist<-genelist[1:num_g]
			}
			else{
                        noimprovement=noimprovement+1
			}	

		}

		prevacc=fitfunc
		#prevacc=foldacc
		print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
		if(noimprovement<=maxitrs)
		{
			num_g=num_g-stepitr
		}
		else
		{
			num_g=minnum
		}
		
	print(paste(gsmethods[m],":",length(bestgenelist), ":", bestmod,sep=""))	
	}
	}else{
		bestgenelist=genelist
		}
	
	scoringmatrix[bestgenelist,m]=1
	}
}
summat=apply(scoringmatrix,1,sum)
print("dim of scoring matrix is ")
print(dim(scoringmatrix))

bestgenelist=which(summat>=minpresent)

print(length(summat))
if(length(bestgenelist)<1)
{
	minpresent=minpresent-1
	bestgenelist=which(summat>=minpresent)
	{
		if(length(bestgenelist)<1)
		{
			minpresent=minpresent-1
			bestgenelist=which(summat>=minpresent)
		}
	#	else
	#	{
	#		bestgenelist=origgenelist
	#	}
	}
	
}

print("bestgenelist")

print(bestgenelist)
if(length(bestgenelist)>0)
{
	modtrain<-as.matrix(nci_xm[,c(bestgenelist)])
	modtest<-as.matrix(test_xm[,c(bestgenelist)])
}
else
{
	modtrain<-as.matrix(nci_xm)
	modtest<-as.matrix(test_xm)
}

#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=dim(modtrain)[2], testacc=testacc))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtrainclass=nci_y, numfeat=dim(modtrain)[2], bestacc=testacc))

print("Complete")

}
##########################################################################################



eval_cma_results<-function()
{

	
traindim=dim(modtrain)
testdim=dim(modtest)
print(modtrain[1:3,])
print(modtest[1:3,])

#modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
#modtest=cbind(apply(modtest,2,as.numeric),testfact)

print(paste("numgenes selected:",length(bestgenelist),sep=""))


model_train<-svm(modtrain,  nci_y,   kernel=kname, type="C", cross=10)
testacc<-sum(diag(test.table))/(dim(modtest)[1])
print(paste("test acc:", testacc, sep=""))

print(paste("10 fold train", model_train$tot.accuracy, sep=""))
pred10fold<-fitted(model_train)
tenfold.table<-table(pred10fold,nci_y)
print("confusion matrix train 10 fold")
print(tenfold.table)
                    
 filestr3<-paste(outloc, "modified_cmatest.csv", sep="")
        write.table(modtest, file=filestr3, sep=",", row.names=FALSE)
 filestr3<-paste(outloc, "modified_cmatrain.csv", sep="")
        write.table(modtrain, file=filestr3, sep=",", row.names=FALSE)
 filestr3<-paste(outloc, "modified_cmatest_class.csv", sep="")
        write.table(test_y, file=filestr3, sep=",", row.names=FALSE)
 
 filestr3<-paste(outloc, "modified_cmatrain_class.csv", sep="")
        write.table(nci_y, file=filestr3, sep=",", row.names=FALSE)

 filestr3<-paste(outloc, "modified_cma_testacc.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)

print("confusion matrix test")
print(test.table)

pred_train<-predict(model_train_valid, modtrain)
train.table<-table(pred_train, nci_y)
trainacc<-sum(diag(train.table))/(dim(modtrain)[1])
print(paste("train acc:", trainacc, sep=""))
print("confusion matrix train")
print(train.table)
 filestr3<-paste(outloc, "modified_cma_trainacc.csv", sep="")
        write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "modified_cma_10foldacc.csv", sep="")
        write.table(model_train$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)

#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#estgenelist,dim(ncid)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

colnames(trainfact)=trainfactnames
colnames(testfact)=trainfactnames
modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
modtest=cbind(apply(modtest,2,as.numeric),testfact)

modtrain=as.data.frame(modtrain)
modtest=as.data.frame(modtest)


}
