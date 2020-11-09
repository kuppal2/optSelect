###################################################################
#A new fitness function is used. It takes into account both the 
#classification accuracy and the number of features. Uses the group 1 >=10 and group 2 otherwise. Only, the top 100 ranked features
#from after performing SVM Attribute Select in WEKA are used.
#This version runs for 100 iterations and uses global best neighbor.
#This version runs for 100 iterations and uses global best neighbor.
library(e1071)
library(bioDist)
#source("/home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/group_0or10/svm_cv_func_10foldblindtest.R")
source("/home/stu/kuppal3/Research/Huira/Source/PSO/svm_cv_func_10foldblindtest.R")
#library(class)
#source("/home/stu/kuppal3/Research/PSO/r_python/R_code/PSO_CPG_R/knn_cv_func3.R")



datafile<-read.csv("/home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/Rawdata/count_macs_gal_25ormore_lessthan5.41_len2to6_patterns_normbylength.csv", na.strings="", sep=",", header=TRUE)


#datafile<-read.csv("/home/stu/kuppal3/Research/Huira/Output/PSO_results/res_100itrs_w0.9to0.4_pcor0.3_polynomial_60%train/orig_train.csv", na.strings="", sep=",", header=TRUE)

#print(datafile[1:10,1:10])
data_dim<-dim(datafile)

#classA=levels(datafile[,data_dim[2]])[1]
#classB=levels(datafile[,data_dim[2]])[2]

id<-sample(1:data_dim[1],size=0.40*data_dim[1],replace=F)

#84 test samples
testm<-datafile[id,]

trainm<-datafile[-id,]

outloc<-"/home/stu/kuppal3/Research/Yuhong_project/GSM529875_liver_AG/Output/PSO_all_results/res_1000itrs_ber_radial_w1_itr1/"


filestrtrain<-paste(outloc, "orig_train.csv", sep="")

filestrtest<-paste(outloc, "orig_test.csv", sep="")

#write.table(trainm, file=filestrtrain, sep=",", row.names=FALSE)
#write.table(testm, file=filestrtest, sep=",", row.names=FALSE)

#remove the sequence column
trainm<-trainm[,-c(1)]
testm<-testm[,-c(1)]


#trainfile<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/peakbin_rand100_seqlen300_0and5to11/peakbind_train.csv"

#trainm<-read.csv(trainfile, na.strings="", sep = ",", header=TRUE)

#testfile<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/peakbin_rand100_seqlen300_0and5to11/peakbind_test.csv"

#testm<-read.csv(testfile, na.strings="", sep = ",", header=TRUE)

#outloc<-"/home/stu/kuppal3/Research/Yuhong_project/Pattern_search/macs_analysis/PSO_results/"

#trainm<-read.csv("/home/stu/kuppal3/Research/Huira/Output/PSO_results/res_100itrs_w0.9to0.4_pcor0.3_polynomial_60%train/modified_train.csv", na.strings="", sep=",", header=TRUE)

#testm<-read.csv("/home/stu/kuppal3/Research/Huira/Output/PSO_results/res_100itrs_w0.9to0.4_pcor0.3_polynomial_60%train/modified_test.csv", na.strings="", sep=",", header=TRUE)

data_dim<-dim(trainm)[2]

#data_dim<-dim(datafile)

classA=levels(trainm[,data_dim])[1]
classB=levels(trainm[,data_dim])[2]

#print(paste(classA,":", length(which(trainm[,data_dim]==classA)), sep=""))
#print(paste(classB,":", length(which(trainm[,data_dim]==classB)), sep=""))


data_dim<-data_dim-1

traindata<-trainm[,1:data_dim]*10000+1
trainclass<-trainm[,(data_dim+1)]

test_dim<-dim(testm)[2]

test_dim<-test_dim-1

testdata<-testm[,1:test_dim]*10000+1
testclass<-testm[,(test_dim+1)]

#trainset<-cbind(trainm[,data_dim], trainm[,1:(data_dim-1)])

#testset<-cbind(testm[,test_dim], testm[,1:(test_dim-1)])

#id<-sample(1:data_dim[1],size=0.20*data_dim[1],replace=F)

#84 test samples
#testm<-trainm[id,]

#trainm<-trainm[-id,]

#data_dim<-dim(trainm[,-c(1)])

#indB<-which(trainm[,1]=="B")

#indA<-which(trainm[,1]=="A")

#trainmA<-trainm[indA,]

#id<-sample(indB,size=length(indA)+1,replace=F)
#ignore<-trainm[-append(indA, id),]
#ignore<-trainmB[-id,]
#trainmB<-trainm[id,]

#trainm<-rbind(trainmA, trainmB)



#testm_dim<-dim(testm[,-c(1)])

#valid 28 samples
#vid<-sample(1:data_dim[1],size=0.30*data_dim[1],replace=F)

#validm<-trainm[vid,]

#137 train samples
#trainm<-trainm[-vid,]

#print(dim(testm))
#print(dim(validm))
print(dim(trainm))

#itestm<-testm[-vid,]

kname="radial"

print(trainclass[1:2])
all_names<-names(traindata)
feat_names<-names(traindata)
data_dim<-dim(traindata)
 
kcross<-10

traincl<-as.character(trainclass)
traincl<-replace(traincl,which(traincl==classA),0)
trainclassnum<-replace(traincl,which(traincl==classB),1)

trainclassnum<-as.numeric(trainclassnum)
#trainmmutinfo<-cbind(traindata, trainclassnum)


trainA<-traindata[which(trainclassnum==0),]
trainclassA<-trainclassnum[which(trainclassnum==0)]

dim(trainA)

trainB<-traindata[which(trainclassnum==1),]
trainclassB<-trainclassnum[which(trainclassnum==1)]
dim(trainB)

pcorlimit<-0.1

 #pcorA<-cor(trainA,trainclassA, method="spearman")

#pcorA[ is.na(pcorA) ] <- 0
#abspcorA<-abs(pcorA)

#indA<-which(abspcorA>pcorlimit)

#pcorB<-cor(trainB,trainclassB, method="spearman")

#pcorB[ is.na(pcorB) ] <- 0
#abspcorB<-abs(pcorB)

#indB<-which(abspcorB>pcorlimit)

#ind<-c(indA,indB)

#ind<-order(ind)
#ind<-unique(ind)

pcor<-cor(traindata,trainclassnum, method="spearman")

pcor[ is.na(pcor) ] <- 0
abspcor<-abs(pcor)

ind<-which(abspcor>=pcorlimit)
print("length ind is ")
print(length(ind))

traindata<-traindata[,ind]
testdata<-testdata[,ind]

print("mod dim of train is ")
print(dim(traindata))

print("mod dim of test is")
print(dim(testdata))

all_names<-names(traindata)
feat_names<-names(traindata)
data_dim<-dim(traindata)

#tune_m<-tune(svm, traindata, trainclass, type="C", kernel=kname, ranges=list(gamma=0.5^(-1:3), cost=10^(-2:4), degree=1))

#tune_m<-tune(svm, traindata[,ind], trainclass, type="C", kernel=kname, ranges=list(gamma=0.5^(-1:3), cost=1000, degree=1))
 #       tune_gamma<-tune_m$best.model$gamma
  #     tune_degree<-tune_m$best.model$degree
   #     tune_cost<-tune_m$best.model$cost
#tune_degree<-1
#tune_gamma<-0.0005
#tune_cost<-1000

# traincl<-as.character(trainclass)
#traincl<-replace(traincl,which(traincl=="High"),1)
#trainclassnum<-replace(traincl,which(traincl=="Low"),0)

#trainclassnum<-as.numeric(trainclassnum)
#trainmmutinfo<-cbind(traindata, trainclassnum)

 #pcor<-cor(traindata,trainclassnum, method="spearman")

#pcor[ is.na(pcor) ] <- 0 
#muttrm<-mutualInfo(trainmmutinfo)
#muttrm<-as.matrix(muttrm)

#write.csv(muttrm, file="train_mutualinfo.csv", row.names=FALSE)

#chimat<-chisq.test(traindata)

eval_fit<-function(particle)
{
	
	num_feat<-0
	w1<-100
        w2<-0.06

	#select the columns with selected features 
	ind<-which(particle==1)

	#need to add 1 to take into account the index of the feature in the original dataset
	col_sel<-ind
	num_feat<-length(col_sel)
	
if(num_feat>1)
{
	trainset<-trainm[,c(1,col_sel)]

	validset<-validm[,c(col_sel)]

	#model<-svm(trainset, trainm[,1], type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost)
	
#model<-svm(Class ~ ., data=trainset, type="C", kernel=kname, cross=10)
	
		model<-svm(trainset, trainclass, type="C", kernel=kname)
	pred<-predict(model, validset)
	
	svm.table<-table(pred, validm[,1])

	
ind_class_A<-which(validm[,1]==classA)
num_A<-length(ind_class_A)
if(num_A>0)
{
ber_a<-svm.table[2]/num_A
}
else
{
ber_a<-0
}

ind_class_B<-which(validm[,1]==classB)

num_B<-length(ind_class_B)
if(num_B>0)
{
ber_b<-svm.table[3]/num_B
}
else
{
ber_b<-0
}
ber<-(ber_a+ber_b)/2

	#error<-1-sum(diag(svm.table))/(dim(validset)[1])
	error<-ber

	fitfunc<-(100*(1-error))+(0.06*(1-num_feat))
	rm(svm.table)
	rm(trainset)
	rm(validset)
}
else
{
#error<-1
fitfunc<-0
}
rm(col_sel)
rm(num_feat)
return(fitfunc)

}


#function to evaluate k-fold CV
eval_fit_kfold<-function(particle, k)
{
	
	num_feat<-0
	#select the columns with selected features 
	ind<-which(particle==1)

	#need to add 1 to take into account the index of the feature in the original dataset
	col_sel<-ind
	num_feat<-length(col_sel)
	#print("itr num feat:")
	#print(length(ind))	
	if(num_feat>1)
	{
		#trainset<-trainm[,c(1,col_sel)]
		
		trainset<-traindata[,c(col_sel)]
		
		model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

		#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
		
		 svm.table<-table(model$fitted, trainclass)

		ind_class_A<-which(trainclass==classA)
num_A<-length(ind_class_A)
if(num_A>0)
{
ber_a<-svm.table[2]/num_A
}
else
{
ber_a<-0
}

ind_class_B<-which(trainclass==classB)

num_B<-length(ind_class_B)
if(num_B>0)
{
ber_b<-svm.table[3]/num_B
}
else
{
ber_b<-0
}
ber<-(ber_a+ber_b)/2

		folderror<-model$tot.accuracy
		#folderror<-(1-ber)*100
		rm(trainset)

	}
	else
	{
	folderror<-1
	}

	fitfunc<-(100*(folderror))+(0.09*(1-num_feat))
	rm(col_sel)
	rm(num_feat)
	return(fitfunc)

}


data_cols<-data_dim[2]
class_factor<-factor(as.matrix(trainm[,1]))

class_labels <-levels(class_factor)
#fitness<-eval_fit()
#print(fitness)



run_pso<-function(d_dim)
{
 itr<-1000
 maxitr<-100
 d<-d_dim[2]
 
 c1<-2.05

 c2<-2.05
 
 ll<-0

 ul<-1
 
 num_part<-10
count_feat<-array(0,dim=c(num_part))
 

#position
 x<-array(0, dim=c(num_part,d_dim[2]))

#velocity
v<-array(0, dim=c(num_part,d_dim[2]))
 
#group<-sample(1:1709,50, replace=FALSE)

 x_gbest<-array(0, dim=c(d_dim[2]))

p_gbest<-array(0, dim=c(d_dim[2]))
   x_lbest<-array(0, dim=c(num_part, d_dim[2]))
   
   r_arr1<-array(0, dim=c(d_dim[1]))
    r_arr2<-array(0, dim=c(d_dim[1]))

 #random initialization of the particle 2D array
#where 0: feature not selected, 1 otherwise
 for (row in 1:num_part)
 {
	num_feat<-0
	r_arr1[row]<-runif(1,0,1)
	r_arr2[row]<-runif(1,0,1)
	
	
	
	for (col in 1:d_dim[2])		
	{
		ran<-runif(1,ll,ul)
		if (ran<0.5)
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

print(count_feat)

wmax<-0.9
wmin<-0.4

itr_data<-""

 fitness_gbest<-10000000000000000
prev_gbest<-fitness_gbest+1 
 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))

best_kfold<-50
        fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 

        feat_list<-feat_names

	no_change<-0
	
	global_best_index<-10000000000000000000
	
	min_fit_x<-1000000000000000

                 #w<-wmin+((wmax-wmin)*k)/itr

        min_fit_index<-100000000000000000
	k<-0
	#while(fitness_gbest>0.25) 
         # || best_kfold>=70))
	for (k in 1:itr)
	
	#while(num_featg>25 && k<itr)
	{
		feat_sel<-0
		#k<-k+1
		print("iteration number: ") 
		print(k)
		  #x_lbest<-array(0, dim=c(num_part, d_dim[2]))
		  # fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
		
		itr_val<-{}
      
		#w<-wmax-((wmax-wmin)*k)/itr
		min_fit_x<-1000000000000000
		 

		min_fit_index<-100000000000000000 
		
		for (i in 1:num_part)
		{
			#fitness_x[i]<-(-1)*eval_fit(x[i,])
			fitness_x[i]<-(-1)*eval_fit_kfold(x[i,], 10)
					
	
			#print("fitness")
			
			#print(fitness_x[i])
			if(fitness_x[i]<min_fit_x)
			{
				#if(count_feat[i]<=num_featl)
				#{
				#print("count part")
				#print(count_feat[i])
				min_fit_x<-fitness_x[i]

				min_fit_index<-i
  				num_featl<-count_feat[i]
				#}

			}
			
			 if(fitness_x[i]<fitness_lbest[i])
			 {
				#if(count_feat[i]<=num_featl)
				#{
				fitness_lbest[i]<-fitness_x[i]
				
				for (j in 1:d_dim[2])
				{
				

					x_lbest[i,j] <- x[i,j]
				}
				#}
			}
			
			
		
		}
		

		 #update the global best and its fitness
		 
                if(min_fit_x < fitness_gbest)
		{
			no_change<-0

			
					fitness_gbest<-min_fit_x
				
					#knn.table<-table(validset[,1],knn(trainset[,-c(1)],validset[,-c(1)],trainset[,1],k=best_k[1],l=0,prob=FALSE, use.all=TRUE))

                        		global_best_index<-min_fit_index
					num_featg<-num_featl
                       	 		print ("global fitness updated to :")
					print(fitness_gbest)
					print ("golbal best num features updated to")

					print (num_featg)
					
				

					#print ("best kfold accuracy")
					#print(best_kfold)
	
					#kfoldgbest<-eval_fit_kfold(x[min_fit_index,], 10)

					#x_gbest<-x[global_best_index,]
					#print(global_best_index)

					for (j in 1:d_dim[2])
					{
						#x_gbest<-x[global_best_index,]

						x_gbest[j]<-x[global_best_index,j]
						#print(x_gbest[1])
						#print(x[global_best_index,1])
					}

					
					#print(x_gbest[1:10])				
			
		      
		}
		else
		{
			no_change<-no_change+1

			if(no_change>maxitr)
			{
				print("RE-INITIALIZING...")
				
				# fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
				min_fit_x<-1000000000000000000000
				 for (row in 1:num_part)
 				{
        				num_feat<-0
        				#min_fit_lbest[row]<-1000000000000000000000
					 ran<-runif(1,0,1)

        				for (col in 1:d_dim[2])

        				{
                				#ran<-runif(1,0,1)
                				if (ran<0.9)
                				{
							ran2<-runif(1,0,1)
							if(ran2<0.9){	
                        				x[row,col]<-0}
							else
							{x[row,col]<-1
							num_feat<-num_feat+1	
							}
                        				#x_lbest[row,col]<-0
						#	if(ran==1)
						#	{
                        			#		num_feat<-num_feat+1
						#	}
                				}
                				else

                				{
                        				x[row,col]<-x_gbest[col]
                        				#x_lbest[row,col]<-1
                        				if(x_gbest[col]==1){
							num_feat<-num_feat+1}

                        				#col_sel[row,num_feat]<-col

                				}

        				}
         				count_feat[row]<-num_feat

 				}				


				no_change<-0
			}
		}
		rank_vec<-rank(fitness_x)

		 ###update the particle velocity and position

                for (i in 1:num_part)
		{
				feat_sel<-0
                        #nbest,min_dist=nsearch(x,i,n,d_dim[2],3)

                        #print "neareset neighbor is ", nbest

                        #print "min dist is", min_dist
			
		#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*d_dim[2]))^2)
		#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
		#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
		w<-1				
 		#w<-(wmax-wmin)*((itr-k)/itr)+wmin
                       for (j in 1:d_dim[2])
			{
                                r1<-runif(1,0,1)

                                r2<-runif(1,0,1)

                                r3<-runif(1,0,1)

				#print(x_gbest[j])
				#v[i,j]<-w*v[i,j]+c1*r_arr1[i]*(x_lbest[i,j]-x[i,j])+c2*r_arr2[i]*(x_gbest[j]-x[i,j])
				
				v[i,j]<-((w*v[i,j])+(c1*r1*(x_lbest[i,j]-x[i,j]))+(c2*r2*(x_gbest[j]-x[i,j])))
				
				if(v[i,j]>6)
				{
					v[i,j]=6
				}	
				if(v[i,j]< -6)
				{
					v[i,j]=-6
				}
				
				S<-1/(1+exp(-v[i,j]))
				#if((S>r3) && (muttrm[(j+1),1]>0.4))	
				#if((S>r3) && (sum(muttrm[j,])>0.4))
				if(S>r3)
				#if((S>r3 && pcor[j]>0.01) || (S>r3 && pcor[j]< -0.01))
				#if((S>r3 && abspcor[j]>pcorlimit)) #|| (S>r3 && abspcorB[j]>pcorlimit))	
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
		#itr_val<-cbind(itr_val, prev_gbest)
		itr_val<-cbind(itr_val, min_fit_x)
		itr_val<-cbind(itr_val, count_feat[min_fit_index])
		itr_data<-rbind(itr_data, itr_val)				
		
	}
	#print(x_gbest)
	


filestr<-paste(outloc,  "multiobj_itr_descpso.txt", sep="")

write.table(itr_data, file=filestr, sep="\t", row.names=F)

 #write.table(itr_data, file="/home/stu/kuppal3/Research/PSO/r_python/R_code/pso_golub/result_novalid_wadapt_golub/multiobj_itr_descpso_cpg_r.txt", sep="\t", row.names=F)
#write.table(itr_data, file="multiobj_itr_descpso_cpg_r.txt", sep="\t", row.names=F)
#print("Final x_gbest vector:")
#print(x_gbest[1:10])

#print(feat_names[1:10])

	#print("kfoldbest is ")
	#print(best_kfold)
	print("numfeatg is ")
	print(num_featg)

	feat_col<-0
	feat_list<-array("",dim(1))

	feat_ind<-which(x_gbest==1)
	feat_list<-feat_names[feat_ind]
 #	print(feat_list[1:10])
       
        feat_col<-feat_ind
      #  print(feat_list)
	print("number of features selected")
	print(length(feat_list))
	print("best accuracy")
	print((-1)*fitness_gbest)
        sumr<-paste("global num features ", length(feat_list)-1, "global best set of features ", x_gbest, "global best accuracy ", 1-fitness_gbest)

filestr2<-paste(outloc,  "selected_feature_index.csv", sep="")
write.table(feat_col, file=filestr2, sep=",", row.names=FALSE)


#write.table(feat_col, file="/home/stu/kuppal3/Research/PSO/r_python/R_code/pso_golub/result_novalid_wadapt_golub/selected_feature_index.csv", sep=",", row.names=FALSE)
#write.table(feat_col, file="selected_feature_index.csv", sep=",", row.names=FALSE)
      

 #finalset<-trainm[,c(1,feat_col)]
 
 
	finalset<-traindata[,c(feat_col)]
		

	test_mod<-testdata[,c(feat_col)]


filestr2<-paste(outloc, "modified_train.csv", sep="")

#write.table(finalset, file="/home/stu/kuppal3/Research/PSO/r_python/R_code/pso_golub/result_novalid_wadapt_golub/modified_traincpg_R100_data_group_1_10ormore.csv", sep=",", row.names=FALSE)

modtrain<-cbind(finalset, trainclass)
modtest<-cbind(test_mod, testclass)

write.table(modtrain, file=filestr2, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "modified_test.csv", sep="")
write.table(modtest, file=filestr3, sep=",", row.names=FALSE)


#tune_m<-tune(svm, finalset, trainclass, type="C", kernel=kname, ranges=list(gamma=0.5^(-5:10), cost=2^(3:10), degree=1:3))
#tune_m<-tune(svm, finalset, trainclass, type="C", kernel=kname, ranges=list(gamma=0.5^(-1:3), cost=10^(-2:4), degree=1))

 #tune_gamma<-tune_m$best.model$gamma
  #     tune_degree<-tune_m$best.model$degree
   #     tune_cost<-tune_m$best.model$cost

#model_train_err<-svm(finalset, trainclass,  kernel=kname,  degree=tune_degree, gamma=tune_gamma, cost=tune_cost, type="C", cross=kcross)

#model_train_valid<-svm(finalset, trainclass,   kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, type="C")

model_train_err<-svm(finalset, trainclass,  kernel=kname, type="C", cross=10)

model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")


	
	pred_train<-predict(model_train_valid, finalset)
	
	train.table<-table(pred_train, trainclass)
	
	
#knn.table<-table(finalset[,1],knn.cv(finalset[,-c(1)],finalset[,1],k=best_k[1],l=0,prob=FALSE, use.all=TRUE))

#model_train_err<-svm(finalset[,-c(1)], finalset[,1], type="C",  kernel="sigmoid", gamma=0.25, cost=0.03125, cross=10)


print("Modified train 10 fold accuracy using train data is ")
print(model_train_err$tot.accuracy)


error<-1-sum(diag(train.table))/(dim(finalset)[1])
print("Modified train accuracy is ")
print(1-error)

print("Train dimension is ")
print(dim(finalset))
 

	pred_test<-predict(model_train_valid, test_mod)
	
	test.table<-table(pred_test, testclass)

#knn.table<-table(test_mod[,1],knn(finalset[,-c(1)],test_mod[,-c(1)],finalset[,1],k=best_k[1],l=0,prob=FALSE, use.all=TRUE))
  
print("test predicted table")
print(test.table)

error<-1-sum(diag(test.table))/(dim(test_mod)[1])
print("Test accuracy is ")
print(1-error)

finalsetx<-finalset
finalsety<-trainclass

kname="radial"

res<-svm_10fold_blind(10,finalsetx,finalsety,test_mod, testclass, kname, classA, classB)
folderror<-(res$avg)

print("Modified 10fold blind test accuracy is ")
print(1-folderror)

print("Test dimension is ")
print(dim(test_mod))

rm(test_mod)
rm(finalset)
}

run_pso(data_dim)
