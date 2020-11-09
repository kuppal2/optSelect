num_itr_select_perfeature<-apply(scoringmatrix,1,sum)

print(summary(summat))
s1<-summary(summat)
num_itr_thresh<-s1[3] #ceiling(minselect.pct*globalpso_maxitr)

pct_range<-seq(1,globalpso_maxitr)
min_fitness_res<-(100000000000)

num_itr_thresh<-max(s1)
best_thresh<-1
best_results<-seq(1,length(summat))

if(FALSE){
for(num_itr_thresh in pct_range)
{
    
    bestgenelist=which(summat>=num_itr_thresh)  #ceiling(minselect.pct*globalpso_maxitr))
    
    print(paste("Number of features selected in ",num_itr_thresh," iterations:", sep=""))
    print(length(bestgenelist))
    
    x<-rep(0,length(summat))
    
    featweightcur<-0.0
    x[bestgenelist]<-1
    
    fitness_res<- eval_fit_test_diff(particle=x,numfolds=numfolds,trainm=trainm,trainclass=trainclass,
    testm=trainm,testclass=trainclass,errortype=errortype,kname=kname,accuracyweightA=weightA,
    accuracyweightB=weightB,accuracyweightC=weightC,accuracyweightD=weightD,featweight=featweightcur,max_num_feats=maxnum)
    print(fitness_res)
    cverror<-fitness_res$cverror
    cvpermerror<-fitness_res$cvpermerror
    diff_error<-fitness_res$cverror-fitness_res$cvpermerror
    fitness_res<-fitness_res$fitfunc
    print(fitness_res)
    if(diff_error>35 && cvpermerror<65){
        if(fitness_res<=(min_fitness_res-0.5)){
            
            best_thresh<-num_itr_thresh
            best_results<-bestgenelist
            min_fitness_res<-fitness_res
        }
        
    }
    
    
    
}
}


bestgenelist<-best_results
feat_ind<-bestgenelist
feat_names<-colnames(trainm)
feat_list<-feat_names[feat_ind]
print(paste("Number of features selected in ",best_thresh," iterations:", sep=""))
print(length(bestgenelist))

feat_col<-0


feat_col<-feat_ind

if(length(feat_ind)<1){
    
    print(stop("No features selected!"))
}

trainmata<-trainm
testmata<-testm
#rm(trainm)
#rm(testm)

#print(feat_col)
#print(trainmata[1:2,])


finalset<-trainmata[,c(feat_col)]


test_mod<-testmata[,c(feat_col)]


filestr2<-paste(outloc, "modified_trainpso.csv", sep="")

Class=trainclass

modtrain<-cbind(finalset, Class)

Class=testclass
modtest<-cbind(test_mod, Class)

write.table(modtrain, file=filestr2, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "modified_testpso.csv", sep="")
write.table(modtest, file=filestr3, sep=",", row.names=FALSE)


modtrain<-data.frame(modtrain)
modtest<-data.frame(modtest)
#model_train_err<-svm(finalset, trainclass,  kernel=kname, type="C", cross=10)

#model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")

model_train_err<-svm(modtrain$Class~., data=modtrain, type="C", cross=10)

print("Modified train 10 fold accuracy using train data is ")
print(model_train_err$tot.accuracy)

filestr3<-paste(outloc, "10foldaccuracy.csv", sep="")
write.table(model_train_err$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)



#    model_train_valid<-svm(modtrain$Class~., data=modtrain, type="C")

model_train_valid<-svm(finalset, trainclass,   kernel=kname, type="C")
pred_train<-predict(model_train_valid, finalset)
train.table<-table(pred_train, trainclass)





error<-1-sum(diag(train.table))/(dim(finalset)[1])
print("Modified train accuracy is ")
print(1-error)

print("train confusion matrix is ")
print(train.table)
print("Train dimension is ")
print(dim(finalset))

#print(modtrain[1:4,])
#print(modtest[1:4,])

mod_dim=dim(modtest)[2]

test_mod<-as.data.frame(test_mod)

print("Test dimension is ")
print(dim(test_mod))

rm(test_mod)
rm(finalset)


#run_pso(data_dim)

trainf=paste(outloc, "modified_trainpso.csv", sep="")
testf=paste(outloc, "modified_testpso.csv", sep="")
trainm<-read.csv(trainf, header=TRUE)

testm<-read.csv(testf, header=TRUE)
trainm<-as.data.frame(trainm)
testm<-as.data.frame(testm)
mod<-svm(trainm$Class~., data=trainm,type="C")
testmim=dim(testm)[2]

trainmim=dim(trainm)[2]

pred<-predict(mod,testm[,-c(testmim)])
test.table=table(pred,testm$Class)
print("Test confusion matrix is ")
print(test.table)
testacc<-(sum(diag(test.table))/(dim(testm)[1]))
print("Test acc is ")
print(testacc)

filestr3<-paste(outloc, "testaccuracy.csv", sep="")
write.table(testacc, file=filestr3, sep=",", row.names=FALSE)


    trainx=subset(trainm, select=-c(Class))
    trainy=trainm$Class
    
    testx=subset(testm, select=-c(Class))
    testy=testm$Class
    
    mod<-svm(trainm$Class~., data=trainm,type="C", cross=10)
    #fitfunc<-testacc #-(0.04*(1-testmim-1))
    
    print("train 10 fold")
    print(mod$tot.accuracy)
    
    
    filestr3<-paste(outloc, "psotenfoldaccuracy.csv", sep="")
    write.table(mod$tot.accuracy, file=filestr3, sep=",", row.names=FALSE)
    
    trainf=paste(outloc, "modified_trainpso.csv", sep="")
    testf=paste(outloc, "modified_testpso.csv", sep="")
    trainm<-read.csv(trainf, header=TRUE)
    testm<-read.csv(testf, header=TRUE)
    trainm<-as.data.frame(trainm)
    testm<-as.data.frame(testm)
    
    mod<-svm(trainm$Class~., data=trainm,type="C")
    testmim=dim(testm)[2]
    
    
    pred<-predict(mod,testm[,-c(testmim)])
    test.table=table(pred,testm$Class)
    print("Test confusion matrix is ")
    print(test.table)
    
    testacc<-(sum(diag(test.table))/(dim(testm)[1]))
    print("Test acc is ")
    print(testacc)

    pred_acc<-multiclass.roc(testm$Class,as.numeric(pred))
    pred_acc_orig<-pred_acc$auc[1]
    auc_acc<-pred_acc_orig
    
    print("Test AUC:")
    print(auc_acc)

    pred<-predict(mod,trainm[,-c(trainmim)])
    train.table=table(pred,trainm$Class)
    
    trainacc<-(sum(diag(train.table))/(dim(trainm)[1]))
    print("Train acc is ")
    print(trainacc)

    

