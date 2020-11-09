
#c("rf","rfe","limma","lasso","elasticnet","f.test")
diffexp.biomarkers<-function(X=NA,Y=NA,feature_table_file=NA,class_labels_file=NA,feat.sel.methods=c("rfe","rf","limma","lasso","elasticnet","wilcox.test","pls","spls","ospls","opls","f.test","t.test"),num.var.sel=10,iter.probability.thresh=0.75,split.train.test=FALSE,train.pct=0.7,outloc,kfold=10,pca.ellipse=TRUE,Xtest=NA,Ytest=NA,rsdthresh=1,pls_vip_thresh=2,seedvalue=27611,learningsetmethod="CV",confounder.matrix=NA,confounderfdrmethod="BH",confounderfdrthresh=0.05,num.methods.sel=2,globalcor=FALSE,cor.method="spearman",networktype="complete",abs.cor.thresh=0.4,cor.fdrthresh=0.05,max.cor.num=100,net_node_colors=c("green","red"), net_legend=TRUE,niter=10,output.device.type="pdf",heatmap.col.opt="RdBu",boxplot.col.opt=c("grey57"),sample.col.opt="rainbow",mz.thresh=5,time.thresh=10,svm_kernel="radial",good_feats_index=NA,pvalue.thresh=0.05,plots.width=8,plots.height=8,plots.res=600, plots.type="cairo")
{
    
    
    
    errortype="BER"
    if(is.na(X)==TRUE){
        X<-read.table(feature_table_file,sep="\t",header=TRUE,quote = "")
    }
    
    if(is.na(Y)==TRUE){
        Y<-read.table(class_labels_file,sep="\t",header=TRUE,quote = "")
    }
    
    
    cl<-makeSOCKcluster(2)
    
    clusterExport(cl,"do_rsd")
    
    feat_rsds<-parApply(cl,X[,-c(1:2)],1,do_rsd)
    
    stopCluster(cl)
    
    abs_feat_rsds<-abs(feat_rsds)
    
    good_metabs<-which(abs_feat_rsds>rsdthresh)
    if(length(good_metabs)>0){
        
        X<-X[good_metabs,]
    }else{
        
        stop("No features meet the defined rsd threshold.")
    }
    
    mzrt<-X[,c(1:2)]
    
    sampnames<-colnames(X[,-c(1:2)])
    
    
    Xorig<-X
    
    if(num.var.sel>dim(Xorig)[1]){
        
        num.var.sel<-dim(Xorig)[1]
    }
    
    
    #Transpose X and Xtrain; rows are samples; columns are metabs
    X<-t(X[,-c(1:2)])
    
    dir.create(outloc)
    setwd(outloc)
    
    dir.create("Figures")
    
    fname<-"InputParameters.csv"
    #sink(fname)
    #   c1<-{}
    #c1<-rbind("X","Y","feature_table_file","class_labels_file","feat.sel.methods","num.var.sel","iter.probability.thresh","split.train.test","train.pct","outloc","kfold","pca.ellipse","Xtest","Ytest","rsdthresh","pls_vip_thresh","seedvalue","learningsetmethod","confounder.matrix","confounderfdrmethod","confounderfdrthresh","num.methods.sel","globalcor","cor.method","networktype","abs.cor.thresh","cor.fdrthresh","max.cor.num","net_node_colors","net_legend","niter","output.device.type","heatmap.col.opt","boxplot.col.opt","sample.col.opt","mz.thresh","time.thresh","svm_kernel")
    #c2<-{}
    
    #feat.sel.methods_str<-paste(feat.sel.methods,sep=";")
    
    #feat.sel.methods_str<-paste(feat.sel.methods,collapse=";")
    
    #c2<-rbind(X,Y,feature_table_file,class_labels_file,feat.sel.methods,num.var.sel,iter.probability.thresh,split.train.test,train.pct,outloc,kfold,pca.ellipse,Xtest,Ytest,rsdthresh,pls_vip_thresh,seedvalue,learningsetmethod,confounder.matrix,confounderfdrmethod,confounderfdrthresh,num.methods.sel,globalcor,cor.method,networktype,abs.cor.thresh,cor.fdrthresh,max.cor.num,net_node_colors,net_legend,niter,output.device.type,heatmap.col.opt,boxplot.col.opt,sample.col.opt,mz.thresh,time.thresh,svm_kernel)
    # print(dim(c1))
    #print(dim(c2))
    # c3<-cbind(c1,c2)
    #colnames(c3)<-c("Parameter","Value")
    #write.csv(c3,file="InputParameters.csv",row.names=FALSE)
    
    
    nsamp<-dim(X)[1]
    
    Xtrain<-X
    Ytrain_mat<-Y
    Ytest_mat<-NA
    
    if(split.train.test==TRUE){
        
        if(nsamp<30){
            
            print("N is too small for train/test analysis. Continuing using all data as training set.")
            
            split.train.test=FALSE
            Xtrain<-X
            Y<-as.factor(Y[,2])
            
            Ytrain_mat<-Y
            Ytest_mat<-NA
            
        }else{
            
            
            
            numtrain<-round(train.pct*nsamp)
            
            set.seed(seedvalue)
            
            train_test_sets<-GenerateLearningsets(y=Y[,2],method="MCCV",ntrain=numtrain,niter=1,strat=TRUE,fold=kfold)
            
            allindex<-1:nsamp
            set.seed(seedvalue)
            # train_index<-sample(allindex,numtrain)
            
            train_index<-train_test_sets@learnmatrix[1,]
            
            check_index<-which(train_index==0)
            
            if(length(check_index)>0){
                
                train_index<-train_index[-check_index]
            }
            
            test_index<-allindex[-train_index]
            
            Ytrain_mat<-Y[train_index,]
            Ytest_mat<-Y[test_index,]
            
            Xtest<-X[test_index,]
            Ytest<-as.factor(Y[test_index,2])
            
            
            
            #   save(Xtest,file="Xtest.Rda")
            #save(Ytest,file="Ytest.Rda")
            #save(Xorig,file="Xorig.Rda")
            #save(train_index,file="train_index.Rda")
            #save(test_index,file="test_index.Rda")
            
            
            Y<-as.factor(Y[train_index,2])
            Xtrain<-X[train_index,]
            
            print("Dim of train set")
            print(dim(Xtrain))
            
            print("Dim of test set")
            print(dim(Xtest))
            
            
            sampnames<-colnames(Xorig[,c(train_index+2)])
            
            sampnames_test<-colnames(Xorig[,c(test_index+2)])
            
            if(is.na(confounder.matrix)==FALSE){
                
                confounder.matrix<-confounder.matrix[train_index,]
            }
        }
        
    }else{
        
        Xtrain<-X
        Y<-as.factor(Y[,2])
        
        if(is.na(Xtest)==FALSE){
            
            Xtestorig<-Xtest
            
            mzrt_test<-Xtest[,c(1:2)]
            
            #print(head(mzrt))
            #print(head(mzrt_test))
            
            sampnames_test<-colnames(Xtest[,-c(1:2)])
            Xtest<-t(Xtest[,-c(1:2)])
            Ytest<-as.factor(Ytest[,2])
            
            res1<-getVenn(dataA=mzrt,dataB=mzrt_test,name_a="train",name_b="test",time.thresh=time.thresh,mz.thresh=mz.thresh,xMSanalyzer.outloc=getwd())
            
            if(nrow(res1$common)<1){
                stop("No common features found.")
            }else{
                
                if(nrow(res1$common)!=nrow(mzrt)){
                    print("Not all features were common between the train and test sets. Only using the common features for further analysis.")
                    
                }
                
                print("Number of common features:")
                print(nrow(res1$common))
                
                #print(head(res1$common))
                
                Xtrain<-Xtrain[,unique(res1$common$index.A)]
                
                #matching_train_data<-matching_train_data[order(matching_train_data$mz),]
                
                Xtest<-Xtest[,unique(res1$common$index.B)]
                
            }
            
            print("Dim of train set")
            print(dim(Xtrain))
            
            print("Dim of test set")
            print(dim(Xtest))
        }
        
    }
    
    X<-(Xtrain)
    
    svm_acc<-{}
    class_levels<-levels(as.factor(Y))
    print(class_levels)
    
    if(learningsetmethod=="bootstrap"){
        
        niter=niter
    }else{
        niter=niter
    }
    
    if(is.na(Xtest)==FALSE)
    {
        
        if(ncol(Xtrain)!=ncol(Xtest)){
            
            stop("The train and test sets should have same number of variables.")
            
        }
    }
    
    
    set.seed(seedvalue)
    fiveCV10iter<-GenerateLearningsets(y=Y,method=learningsetmethod,fold=kfold,niter=niter,strat=TRUE)
    
    feat_sel_matrix<-matrix(nrow=dim(X)[2],ncol=length(feat.sel.methods),0)
    if(is.na(good_feats_index)==TRUE){
        
        if(is.na(feat.sel.methods)==FALSE){
            for(m in 1:length(feat.sel.methods)){
                
                method=feat.sel.methods[m]
                
                if(method=="pls" || method=="spls" || method=="opls" || method=="ospls"){
                    
                    v1<-matrix(ncol=dim(fiveCV10iter@learnmatrix)[1],nrow=dim(X)[2],0)
                    
                    for(i in 1:dim(fiveCV10iter@learnmatrix)[1]){
                        
                        temptrain<-t(X[fiveCV10iter@learnmatrix[i,],])
                        tempclass<-Y[fiveCV10iter@learnmatrix[i,]]
                        
                        classlabels_sub<-cbind(paste("S",rep(1,length(tempclass)),sep=""),tempclass)
                        
                        classlabels_sub<-as.data.frame(classlabels_sub)
                        
                        if(method=="spls"){
                            sparseselect=TRUE
                        }else{
                            sparseselect=FALSE
                        }
                        
                        numcomp<-5
                        
                        set.seed(123)
                        opt_comp<-pls.lda.cv(Xtrain=X, Ytrain=Y,  ncomp=c(1:numcomp), nruncv=kfold, alpha=2/3, priors=NULL)
                        
                        if(method=="ospls"){
                            
                            Ytemp<-as.numeric(Y)
                            leukemia.pls <- plsr(Ytemp ~ X, ncomp = opt_comp, validation = "LOO")
                            ww <- leukemia.pls$loading.weights[,1]
                            pp <- leukemia.pls$loadings[,1]
                            w.ortho <- pp - crossprod(ww, pp)/crossprod(ww) * ww
                            t.ortho <- X %*% w.ortho
                            
                            p.ortho <- crossprod(X, t.ortho) / c(crossprod(t.ortho))
                            Xcorr <- X - tcrossprod(t.ortho, p.ortho)
                            
                            
                            
                            X<-Xcorr
                            method="spls"
                        }
                        
                        if(method=="opls"){
                            
                            Ytemp<-as.numeric(Y)
                            leukemia.pls <- plsr(Ytemp ~ X, ncomp = opt_comp, validation = "LOO")
                            ww <- leukemia.pls$loading.weights[,1]
                            pp <- leukemia.pls$loadings[,1]
                            w.ortho <- pp - crossprod(ww, pp)/crossprod(ww) * ww
                            t.ortho <- X %*% w.ortho
                            
                            p.ortho <- crossprod(X, t.ortho) / c(crossprod(t.ortho))
                            Xcorr <- X - tcrossprod(t.ortho, p.ortho)
                            
                            
                            
                            X<-Xcorr
                            method="pls"
                        }
                        
                        
                        
                        
                        #opt_comp<-plsres1$opt_comp
                        max_comp_sel<-opt_comp
                        if(method=="spls"){
                            
                            keep_X_vec=rep(num.var.sel,opt_comp)
                            
                            linn.pls <- splsda(X, Y,ncomp=opt_comp,keepX=keep_X_vec)
                            
                            linn.vip<-linn.pls$loadings$X
                            
                            
                            if(opt_comp>1){
                                
                                #abs
                                vip_res1<-abs(linn.vip)
                                
                                if(max_comp_sel>1){
                                    vip_res1<-apply(vip_res1,1,max)
                                    
                                }else{
                                    
                                    vip_res1<-vip_res1[,c(1)]
                                }
                            }else{
                                
                                vip_res1<-abs(linn.vip)
                            }
                            
                            pls_vip<-vip_res1 #(plsres1$vip_res)
                            
                            
                            #based on loadings for sPLS
                            #feat_sel_matrix[which(pls_vip!=0),i]<-1 #pls_vip!=0 & rand_pls_sel_fdr<fdrthresh
                            varindex<-which(pls_vip!=0)
                            if(length(varindex)>0){
                                v1[varindex,i]<-1
                            }
                            
                            
                        }else{
                            
                            
                            
                            linn.pls <- plsda(X, Y,ncomp=opt_comp)
                            
                            linn.vip<-vip(linn.pls)
                            
                            {
                                if(opt_comp>1){
                                    vip_res1<-(linn.vip)
                                    if(max_comp_sel>1){
                                        vip_res1<-apply(vip_res1,1,mean)
                                    }else{
                                        
                                        vip_res1<-vip_res1[,c(1)]
                                    }
                                }else{
                                    
                                    vip_res1<-linn.vip
                                }
                                
                            }
                            
                            #vip_res1<-plsres1$vip_res
                            pls_vip<-vip_res1
                            
                            pls_vip_order<-pls_vip[order(pls_vip,decreasing=TRUE)]
                            
                            pls_vip_thresh<-min(pls_vip_order[1:num.var.sel])[1]
                            
                            #print(pls_vip_thresh)
                            #pls
                            varindex<-which(pls_vip>=pls_vip_thresh)
                            if(length(varindex)>0){
                                v1[varindex,i]<-1
                            }
                        }
                    }
                }else{
                    
                    #set.seed(27611)
                    set.seed(seedvalue)
                    if(method=="rf"){
                        g1<-GeneSelection(X=X,y=Y,learningsets=fiveCV10iter,method=method,trace=FALSE,seed = 100)
                    }else{
                        
                        g1<-GeneSelection(X=X,y=Y,learningsets=fiveCV10iter,method=method,trace=FALSE)
                    }
                    gmatrix<-{}
                    
                    
                    rank_matrix<-g1@rankings[[1]]
                    
                    v1<-matrix(nrow=dim(X)[2],ncol=dim(rank_matrix)[1],0)
                    
                    
                    
                    for(i in 1:dim(rank_matrix)[1]){
                        
                        varindex<-{}
                        varindex1<-toplist(g1,iter=i,k=num.var.sel,show=FALSE)
                        
                        if(length(g1@rankings)>1){
                            for(gindex in 1:length(g1@rankings)){
                                
                                varindex<-c(varindex,varindex1[[gindex]][,1])
                                
                            }
                        }else{
                            varindex<-varindex1[,1]
                        }
                        
                        varindex<-unique(varindex)
                        
                        
                        v1[varindex,i]<-1
                        
                    }
                }
                
                
                
                save(v1,file="v1.Rda")
                
                #hist(svm_acc,main="Inner test set accuracy distribution",col="brown")
                
                #iter.probability.thresh: means that value is 1 in (1-iter.probability.thresh)% or more sets;
                stability_measure<-apply(v1,1,function(x){quantile(x,iter.probability.thresh)})
                
                stability_matrix<-stability_measure
                if(m==1){
                    stability_matrix_1<-cbind(mzrt,stability_measure)
                }else{
                    
                    stability_matrix_1<-cbind(stability_matrix_1,stability_measure)
                }
                
                
                feat_sel_matrix[which(stability_matrix==1),m]<-1
                
            }
            
            
            
        }else{
            
            feat_sel_matrix<-matrix(nrow=dim(X)[1],ncol=length(feat.sel.methods),1)
            
        }
        
        
        if(length(feat.sel.methods)>1){
            feat_sel_matrix<-apply(feat_sel_matrix,1,sum)
            
            
        }
        
        if(num.methods.sel>length(feat.sel.methods)){
            
            num.methods.sel=length(feat.sel.methods)
        }
        
        
        colnames(stability_matrix_1)<-c("mz","time",feat.sel.methods)
        
        pdf("Results.pdf")
        
        #hist(stability_measure,main="Stability measure distribution",col="brown")
        write.table(stability_matrix_1,file="stability_matrix.txt",sep="\t",row.names=FALSE)
        
        good_feats_index<-which(feat_sel_matrix>=num.methods.sel)
        
        if(is.na(pvalue.thresh)==FALSE){
            
            
            
            numcores<-round(detectCores()*0.8)
            
            cl <- makeCluster(getOption("cl.cores", numcores))
            
            clusterExport(cl,"diffexponewayanova",envir = .GlobalEnv)
            
            clusterExport(cl,"anova",envir = .GlobalEnv)
            
            
            clusterExport(cl,"TukeyHSD",envir = .GlobalEnv)
            
            clusterExport(cl,"aov",envir = .GlobalEnv)
            
            
            #res1<-apply(data_m_fc,1,function(x){
            res1<-parApply(cl,X,2,function(x,classlabels_response_mat){
                xvec<-x
                
                
                data_mat_anova<-cbind(xvec,classlabels_response_mat)
                
                data_mat_anova<-as.data.frame(data_mat_anova)
                cnames<-colnames(data_mat_anova)
                
                cnames[1]<-"Response"
                
                colnames(data_mat_anova)<-c("Response","Factor1")
                
                #print(data_mat_anova)
                
                data_mat_anova$Factor1<-as.factor(data_mat_anova$Factor1)
                
                anova_res<-diffexponewayanova(dataA=data_mat_anova)
                
                
                
                return(anova_res)
            },Y)
            
            stopCluster(cl)
            main_pval_mat<-{}
            
            posthoc_pval_mat<-{}
            pvalues<-{}
            
            
            #print(head(res1))
            
            for(i in 1:length(res1)){
                
                
                main_pval_mat<-rbind(main_pval_mat,res1[[i]]$mainpvalues)
                pvalues<-c(pvalues,res1[[i]]$mainpvalues[1])
                posthoc_pval_mat<-rbind(posthoc_pval_mat,res1[[i]]$posthocfactor1)
                
            }
            
            pvalues<-unlist(pvalues)
            
            good_feats_index<-which(feat_sel_matrix>=num.methods.sel & pvalues<pvalue.thresh)
            if(length(good_feats_index)<1){
                stop("No features selected.")
            }
            
        }
        good_feats<-Xtrain[,good_feats_index]
        
        
        
        mzrt_sub<-mzrt[good_feats_index,]
        
        good_feats<-t(good_feats)
        
        
        
        cnames_1<-colnames(good_feats)
        
        
        colnames(good_feats)<-sampnames
        
        
        good_feats<-cbind(mzrt_sub,good_feats)
        
        if(is.na(Xtest)==FALSE){
            good_feats_test<-Xtest[,good_feats_index]
            good_feats_test<-t(good_feats_test)
            colnames(good_feats_test)<-sampnames_test
            good_feats_test<-cbind(mzrt_sub,good_feats_test)
        }else{
            good_feats_test<-NA
            Ytest_mat<-NA
        }
        
        
        
    }else{
        pdf("Results.pdf")
        good_feats<-Xtrain[,good_feats_index]
        
        
        
        mzrt_sub<-mzrt[good_feats_index,]
        
        good_feats<-t(good_feats)
        
        
        
        cnames_1<-colnames(good_feats)
        
        
        colnames(good_feats)<-sampnames
        
        
        good_feats<-cbind(mzrt_sub,good_feats)
        
        if(is.na(Xtest)==FALSE){
            good_feats_test<-Xtest[,good_feats_index]
            good_feats_test<-t(good_feats_test)
            colnames(good_feats_test)<-sampnames_test
            good_feats_test<-cbind(mzrt_sub,good_feats_test)
        }else{
            good_feats_test<-NA
            Ytest_mat<-NA
        }
        
    }
    
    print("Number of features selected")
    print(length(good_feats_index))
    
    
    if(length(good_feats_index)>1){
        
        
        
        X<-X[,good_feats_index]
        
        if(is.na(Xtest)==FALSE){
            
            Xtest<-Xtest[,good_feats_index]
        }
        
        
        
        
        
        #if(classifier=="pls")
        if(length(good_feats_index)>5)
        {
            
            #print("Sts1")
            
            #print(X[1:3,c(1:3,240:243)])
            
            save(list=ls(),file="t1.Rda")
            tune_plslda <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_ldaCMA, grids = list(comp = 1:5),trace=FALSE)
            
            if(length(class_levels)==2){
                #     tune_plslr <- tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_lrCMA, grids = list(comp = 1:5))
            }
            #print("Sts2")
            tune_plsrf <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_rfCMA, grids = list(comp = 1:5),trace=FALSE)
        }
        #if(classifier=="scda")
        {
            
            #print("Sts3")
            tune_scda <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = scdaCMA, grids = list( ),trace=FALSE)
        }
        #if(classifier=="svm")
        {
            #print("Sts4")
            tune_svm <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA, grids = list( ), kernel = "radial",trace=FALSE)
        }
        
        if(length(good_feats_index)>2){
            class_plslda <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_ldaCMA, tuneres = tune_plslda,trace=FALSE)
            
            class_plsrf <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_rfCMA, tuneres = tune_plsrf,trace=FALSE)
        }
        
        class_scda <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = scdaCMA, tuneres = tune_scda,trace=FALSE)
        
        class_svm <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA, tuneres = tune_svm,kernel = "radial",trace=FALSE,probability=TRUE)
        class_rf <- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = rfCMA,trace=FALSE)
        
        class_nnet<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = nnetCMA,trace=FALSE)
        
        class_plr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = plrCMA,trace=FALSE)
        
        save(class_plslda,file="class_plslda.Rda")
        save(class_plsrf,file="class_plsrf.Rda")
        save(class_scda,file="class_scda.Rda")
        save(class_svm,file="class_svm.Rda")
        save(class_rf,file="class_rf.Rda")
        save(class_nnet,file="class_nnet.Rda")
        save(class_plr,file="class_plr.Rda")
        
        
        
        if(length(class_levels)==2){
            
            # class_plslr <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_lrCMA)
            
            # class_lassoplr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = LassoCMA)
            
            # class_elasticnetplr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = ElasticNetCMA)
            
            #eval_plslr<-evaluation(class_plslr)
            
            # eval_lassoplr<-evaluation(class_lassoplr)
            
            #eval_elasticnetplr<-evaluation(class_elasticnetplr)
            #measures="auc"
            
            eval_measure="auc"
            #eval_measure="misclassification"
            
        }else{
            
            eval_measure="misclassification"
        }
        
        
        print("here: evaluation")
        
        if(length(good_feats_index)>2){
            
            # if(eval_measure=="misclassification")
            
            eval_plslda<-evaluation(class_plslda,measure=eval_measure)
            eval_plsrf<-evaluation(class_plsrf,measure=eval_measure)
            eval_scda<-evaluation(class_scda,measure=eval_measure)
            eval_svm<-evaluation(class_svm,measure=eval_measure)
            eval_rf<-evaluation(class_rf,measure=eval_measure)
            eval_nnet<-evaluation(class_nnet,measure=eval_measure)
            eval_plr<-evaluation(class_plr,measure=eval_measure)
            
            if(eval_measure=="auc")
            {
                eval_plslda@score=1-mean(eval_plslda@score)
                eval_plsrf@score=1-mean(eval_plsrf@score)
                eval_scda@score=1-mean(eval_scda@score)
                eval_svm@score=1-mean(eval_svm@score)
                eval_rf@score=1-mean(eval_rf@score)
                eval_nnet@score=1-mean(eval_nnet@score)
                eval_plr@score=1-mean(eval_plr@score)
                
            }
        }
        
        save(eval_svm,file="eval_svm.Rda")
        save(class_svm,file="class_svm.Rda")
        
        
        text2<-paste(dim(v1)[2], " learning sets using training data",sep="")
        if(length(class_levels)==0){
            
            eval_mat1<-cbind(text2,100*(1-mean(eval_plslda@score)),100*(1-mean(eval_plslr@score)),100*(1-mean(eval_plsrf@score)),100*(1-mean(eval_scda@score)),100*(1-mean(eval_svm@score)),100*(1-mean(eval_rf@score)),100*(1-mean(eval_nnet@score)),100*(1-mean(eval_plr@score)),100*(1-mean(eval_lassoplr@score)),100*(1-mean(eval_elasticnetplr@score)))
            
            best_classifier<-which(eval_mat1==max(eval_mat1))
            classifier_names<-c("PLSLDA","PLSLR","PLSRF","SCDA","SVM","RF","NNet","pLR","pLRlasso","pLRelasticnet")
            
            best_classifier_name<-classifier_names[best_classifier]
            
            eval_mat1<-cbind(text2,100*(1-mean(eval_plslda@score)),100*(1-mean(eval_plslr@score)),100*(1-mean(eval_plsrf@score)),100*(1-mean(eval_scda@score)),100*(1-mean(eval_svm@score)),100*(1-mean(eval_rf@score)),100*(1-mean(eval_nnet@score)),100*(1-mean(eval_plr@score)),100*(1-mean(eval_lassoplr@score)),100*(1-mean(eval_elasticnetplr@score)))
            
            colnames(eval_mat1)<-c("Dataset","PLSLDA","PLSLR","PLSRF","SCDA","SVM","RF","NNet","pLR","pLRlasso","pLRelasticnet")
        }else{
            
            if(length(good_feats_index)>5){
                eval_mat1<-cbind(100*(1-mean(eval_plslda@score)),100*(1-mean(eval_plsrf@score)),100*(1-mean(eval_scda@score)),100*(1-mean(eval_svm@score)),100*(1-mean(eval_rf@score)),100*(1-mean(eval_nnet@score)),100*(1-mean(eval_plr@score)))
            }else{
                eval_mat1<-cbind(100*(1-1),100*(1-1),100*(1-mean(eval_scda@score)),100*(1-mean(eval_svm@score)),100*(1-mean(eval_rf@score)),100*(1-mean(eval_nnet@score)),100*(1-mean(eval_plr@score)))
                
            }
            best_classifier<-which(eval_mat1==max(eval_mat1)[1])
            
            classifier_names<-c("PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR")
            
            best_classifier_name<-classifier_names[best_classifier]
            
            best_innerkfold_acc<-max(eval_mat1)[1]
            
            eval_mat_1<-round(eval_mat1,2)
            eval_mat1<-cbind(text2,eval_mat_1)
            
            colnames(eval_mat1)<-c("Dataset","PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR")
            
        }
        
        if(output.device.type!="pdf"){
            
            temp_filename_1<-"Figures/Barplot_classifier_comparison_CVaccuracy.png"
            
            png(temp_filename_1,width=plots.width,height=plots.height,res=plots.res,type=plots.type,units="in")
        }
        
        
        
        
        barplot(eval_mat_1,xaxt="n",main="Comparison of CV accuracy using different classifiers\n based on learning sets", xlab="Classifier",ylab="kfold classification accuracy(%)",col=boxplot.col.opt,type="p",ylim=c(0,100))
        axis(side=1,at=seq(1,7),labels=c("PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR"),cex.axis=0.7)
        
        
        
        if(output.device.type!="pdf"){
            
            dev.off()
        }
        
        mzrt_1<-paste(round(good_feats[,1],5),round(good_feats[,2],1),sep="_")
        rownames(good_feats)<-mzrt_1
        
        
        Y1<-cbind(sampnames,as.character(Y))
        Y1<-as.data.frame(Y1)
        #print(head(Y1))
        
        if(length(good_feats_index)>3){
            
            if(output.device.type!="pdf"){
                
                temp_filename_1<-"Figures/HCA_sigfeats.png"
                
                png(temp_filename_1,width=plots.width,height=plots.height,res=plots.res,type=plots.type,units="in")
            }
            
            
            
            try(get_hca(parentoutput_dir=outloc,X=good_feats,Y=Y1,heatmap.col.opt=heatmap.col.opt,cor.method="spearman",is.data.znorm=FALSE,analysismode="classification",
            sample.col.opt="rainbow",plots.width=2000,plots.height=2000,plots.res=300, alphacol=0.3, hca_type="two-way",newdevice=FALSE),silent=TRUE)
            
            
            
            if(output.device.type!="pdf"){
                
                dev.off()
            }
            #    pdf("pca.pdf")
            
            if(output.device.type!="pdf"){
                
                temp_filename_1<-"Figures/PCAplots_sigfeats.pdf"
                
                #png(temp_filename_1,width=plots.width,height=plots.height,res=plots.res,type=plots.type,units="in")
                pdf(temp_filename_1)
            }
            
            
            save(list=ls(),file="debug1.Rda")
            try(get_pcascoredistplots(X=good_feats,Y=Y1,parentoutput_dir=outloc,sample.col.opt=sample.col.opt,alphacol=0.3,col_vec=NA,pairedanalysis=FALSE,pca.cex.val=3,legendlocation="topright",pca.ellipse=FALSE,ellipse.conf.level=0.5,filename="selected"),silent=TRUE)
            
        }
        
        
        if(output.device.type!="pdf"){
            
            dev.off()
        }
        
        
        if(output.device.type!="pdf"){
            
            temp_filename_1<-"Figures/Boxplots_sigfeats.pdf"
            
            #png(temp_filename_1,width=plots.width,height=plots.height,res=plots.res,type=plots.type,units="in")
            pdf(temp_filename_1)
        }
        
        
        
        
        get_boxplots(X=good_feats,Y=Y1,parentoutput_dir=outloc,boxplot.col.opt=boxplot.col.opt,sample.col.opt=sample.col.opt,alphacol=0.3,newdevice=FALSE)
        
        
        
        
        
        if(output.device.type!="pdf"){
            
            dev.off()
        }
        
        num_levels<-levels(as.factor(Y))
        if(num_levels==2){
            if(split.train.test==TRUE){
                #     X<-as.matrix(X)
                #get_roc(dataA=good_feats,classlabels=Y,classifier="svm",kname=svm_kernel,rocfeatlist=seq(2,10,1),rocfeatincrement=TRUE,testset=NA,testclasslabels=NA,mainlabel="Test set")
                
                
            }else{
                #get_roc(dataA=good_feats,classlabels=Y,classifier="svm",kname=svm_kernel,rocfeatlist=seq(2,10,1),rocfeatincrement=TRUE,testset=X,testclasslabels=Y,mainlabel="Using training set based on SVM")
                
            }
        }
        
        
        #if(is.na(Xtest)==FALSE)
        {
            
            
            
            best_kfold_acc<-best_innerkfold_acc
            #outerkfold_acc<-100*(1-mean(testeval_res@score))
            
            permkfold_acc<-{}
            permkfold_acc1<-{}
            
            permkfold_acc2<-{}
            
            permkfold_acc3<-{}
            
            permkfold_acc4<-{}
            
            permkfold_acc5<-{}
            
            permkfold_acc6<-{}
            
            permkfold_acc7<-{}
            Yorig<-Y
            
            text2B<-paste("Permuted ",dim(v1)[2], " learning sets using training data",sep="")
            #if(is.na(Xtest)==FALSE)
            for(p1 in 1:5)
            {
                #set.seed(27611)
                set.seed(seedvalue)
                Y<-Yorig[sample(1:length(Yorig),size=length(Yorig))]
                #set.seed(27611)
                set.seed(seedvalue)
                fiveCV10iter<-GenerateLearningsets(y=Y,method=learningsetmethod,fold=kfold,niter=1,strat=TRUE)
                
                classifier_names<-c("PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR","PLSLR","pLRlasso","pLRelasticnet")
                
                
                if(length(good_feats_index)>5){
                    
                    class_plslda <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_ldaCMA,trace=FALSE)
                    testeval_res1<-evaluation(class_plslda,measure=eval_measure)
                    
                    
                    class_plsrf <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_rfCMA,trace=FALSE)
                    testeval_res2<-evaluation(class_plsrf,measure=eval_measure)
                    
                }
                
                class_scda <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = scdaCMA,trace=FALSE)
                testeval_res3<-evaluation(class_scda,measure=eval_measure)
                
                
                
                class_svm <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA,kernel = "radial",trace=FALSE,probability=TRUE)
                testeval_res4<-evaluation(class_svm,measure=eval_measure)
                
                
                class_rf <- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = rfCMA,trace=FALSE)
                testeval_res5<-evaluation(class_rf,measure=eval_measure)
                
                class_nnet<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = nnetCMA,trace=FALSE)
                testeval_res6<-evaluation(class_nnet,measure=eval_measure)
                
                class_plr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = plrCMA,trace=FALSE)
                testeval_res7<-evaluation(class_plr,measure=eval_measure)
                
                
                if(eval_measure=="auc")
                {
                    eval_plslda@score=1-mean(eval_plslda@score)
                    eval_plsrf@score=1-mean(eval_plsrf@score)
                    eval_scda@score=1-mean(eval_scda@score)
                    eval_svm@score=1-mean(eval_svm@score)
                    eval_rf@score=1-mean(eval_rf@score)
                    eval_nnet@score=1-mean(eval_nnet@score)
                    eval_plr@score=1-mean(eval_plr@score)
                    
                }
                
                
                
                if(length(good_feats_index)>5){
                    permkfold_acc1<-c(permkfold_acc1,100*(1-mean(testeval_res1@score)))
                    
                    permkfold_acc2<-c(permkfold_acc2,100*(1-mean(testeval_res2@score)))
                    
                }else{
                    permkfold_acc1<-c(permkfold_acc1,0)
                    permkfold_acc2<-c(permkfold_acc2,0)
                }
                
                permkfold_acc3<-c(permkfold_acc3,100*(1-mean(testeval_res3@score)))
                
                permkfold_acc4<-c(permkfold_acc4,100*(1-mean(testeval_res4@score)))
                
                permkfold_acc5<-c(permkfold_acc5,100*(1-mean(testeval_res5@score)))
                
                permkfold_acc6<-c(permkfold_acc6,100*(1-mean(testeval_res6@score)))
                
                permkfold_acc7<-c(permkfold_acc7,100*(1-mean(testeval_res7@score)))
                
                
            }
            
            permkfold_acc1<-mean(permkfold_acc1)
            permkfold_acc2<-mean(permkfold_acc2)
            permkfold_acc3<-mean(permkfold_acc3)
            permkfold_acc4<-mean(permkfold_acc4)
            permkfold_acc5<-mean(permkfold_acc5)
            permkfold_acc6<-mean(permkfold_acc6)
            permkfold_acc7<-mean(permkfold_acc7)
            
            eval_mat_perm<-cbind(permkfold_acc1, permkfold_acc2, permkfold_acc3, permkfold_acc4, permkfold_acc5, permkfold_acc6, permkfold_acc7)
            eval_mat_perm<-round(eval_mat_perm,2)
            
            eval_mat_perm_final<-cbind(text2B,eval_mat_perm)
            
            
            
            colnames(eval_mat_perm_final)<-c("Dataset","PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR")
            
            eval_mat_actual<-as.data.frame(eval_mat_1)
            eval_mat_perm1<-as.data.frame(eval_mat_perm)
            eval_mat_perm1<-round(eval_mat_perm1,2)
            
            
            colnames(eval_mat_perm1)<-colnames(eval_mat_actual)
            emat1<-rbind(eval_mat_actual,eval_mat_perm1)
            
            emat1<-t(emat1)
            classifier_names<-c("PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR")
            
            rownames(emat1)<-classifier_names
            
            
            eval_mat_actual<-((emat1[,1]-emat1[,2])*0.5)+(0.5*(emat1[,1]))
            
            emat1<-cbind(emat1,eval_mat_actual)
            colnames(emat1)<-c("Actual.accuracy","Permuted.accuracy","Score[((Actual-Permuted)*0.5)+(0.5*Actual)]")
            print(emat1)
            
            
            
            
            test_acc<-NA
            test_acc_mat<-{}
            Y<-Yorig
            if(is.na(Xtest)==FALSE){
                
                test_acc_mat<-{}
                class_levels_vec<-levels(as.factor(Y))
                
                
                learnmatrix <- matrix(seq(1,nrow(X)), nrow = 1)
                fiveCV10iter<-new("learningsets", learnmatrix = learnmatrix, method = "none",ntrain = ncol(learnmatrix), iter = nrow(learnmatrix))
                X<-rbind(X,Xtest)
                Y<-c(Y,Ytest)
                classifier_names<-c("PLSLDA","PLSRF","SCDA","SVM","RF","NNet","pLR","PLSLR","pLRlasso","pLRelasticnet")
                
                save(X,file="X1.Rda")
                save(Y,file="Y1.Rda")
                save(learnmatrix,file="learnmatrix.Rda")
                
                save(list=ls(),file="debug3.Rda")
                
                if(length(good_feats_index)>5)
                {
                    s1<-ldply(tune_plslda@tuneres,rbind)
                    
                    s2<-apply(s1,2,mean)
                    
                    
                    t1<-new("list")
                    confusion_matrix_list<-new("list")
                    t1[[1]]<-s2
                    tune_plslda1<-tune_plslda
                    tune_plslda1@tuneres<-t1
                    save(tune_plslda,file="tune_res.Rda")
                    #
                    #new("tuneres", fold = tune_plslda@fold, method = tune_plslda@method,
                    #tuneres = s2, hypergrid = tune_plslda@hypergrid)
                    
                    class_res <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_ldaCMA, tuneres = tune_plslda1,trace=FALSE)
                    
                    b1<-best(tune_plslda1)
                    
                    #save(b1,file="b1.Rda")
                    
                    learnmatrix<-as.numeric(learnmatrix)
                    
                    class_res2<-pls_ldaCMA(X = X, y = Y, learnind = learnmatrix, comp = median(unlist(b1)))
                    
                    save(class_res2,file="pls_ldaCMA.Rda")
                    
                    print(ftable(class_res2))
                    
                    save(confusion_matrix_1,file="confusion_matrix_plslda.Rda")
                    confusion_matrix_list[[1]]<-table(class_res2@y,class_res2@yhat)
                    
                    if(length(class_levels_vec)==2){
                        testeval_res_auc<-evaluation(class_res,measure = "auc")
                        save(testeval_res_auc,file="testeval_res_auc.Rda")
                        print(testeval_res_auc)
                        test_auc<-100*(mean(testeval_res_auc@score))
                        test_acc_mat<-c(test_acc_mat,test_auc)
                        
                    }else{
                        
                        test_acc<-evaluation(class_res)
                        
                        test_acc<-100*(1-mean(testeval_res_auc@score))
                        test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                        
                        
                    }
                    
                    
                    
                    #if(best_classifier_name==classifier_names[2]){
                    
                    s1<-ldply(tune_plsrf@tuneres,rbind)
                    s2<-apply(s1,2,mean)
                    #tune_plsrf<-new("tuneres", fold = tune_plsrf@fold, method = tune_plsrf@method,
                    #tuneres = s2, hypergrid = tune_plsrf@hypergrid)
                    t1<-new("list")
                    t1[[1]]<-s2
                    
                    tune_plsrf1<-tune_plsrf
                    tune_plsrf1@tuneres<-t1
                    class_res <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_rfCMA, tuneres = tune_plsrf1,trace=FALSE)
                    
                    b1<-best(tune_plsrf1)
                    # save(b1,file="b1_pls_rfCMA.Rda")
                    
                    learnmatrix<-as.numeric(learnmatrix)
                    
                    class_res2<-pls_rfCMA(X = X, y = Y, learnind = learnmatrix, comp = median(unlist(b1)))
                    
                    save(class_res2,file="pls_rfCMA.Rda")
                    print(ftable(class_res2))
                    
                    #confusion_matrix_1<-table(class_res2@y,class_res2@yhat)
                    save(confusion_matrix_1,file="confusion_matrix_rfCMA.Rda")
                    
                    confusion_matrix_list[[2]]<-table(class_res2@y,class_res2@yhat)
                    
                    if(length(class_levels_vec)==2){
                        
                        testeval_res_auc<-evaluation(class_res,measure = "auc")
                        save(testeval_res_auc,file="testeval_res_auc.Rda")
                        
                        print(testeval_res_auc)
                        
                        test_auc<-100*(mean(testeval_res_auc@score))
                        
                        test_acc_mat<-c(test_acc_mat,test_auc)
                        
                        
                        
                    }else{
                        
                        test_acc<-evaluation(class_res)
                        
                        test_acc<-100*(1-mean(test_acc@score))
                        test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                        
                        
                    }
                    
                    
                    #}
                    
                    #}
                    
                    #if(best_classifier_name==classifier_names[3]){
                    
                    s1<-ldply(tune_scda@tuneres,rbind)
                    s2<-apply(s1,2,mean)
                    #tune_scda<-new("tuneres", fold = tune_scda@fold, method = tune_scda@method,
                    #  tuneres = s2, hypergrid = tune_scda@hypergrid)
                    
                    t1<-new("list")
                    t1[[1]]<-s2
                    
                    tune_scda1<-tune_scda
                    tune_scda1@tuneres<-t1
                    
                    class_res <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = scdaCMA, tuneres = tune_scda1,trace=FALSE)
                    #scdaCMA(X, y, f, learnind, delta = 0.5, models=FALSE,...)
                    
                    b1<-best(tune_scda1)
                    
                    save(b1,file="b1_scda.Rda")
                    learnmatrix<-as.numeric(learnmatrix)
                    class_res2<-scdaCMA(X = X, y = Y, learnind = learnmatrix, delta = median(unlist(b1)))
                    save(class_res2,file="scdaCMA.Rda")
                    print(ftable(class_res2))
                    
                    #confusion_matrix_1<-table(class_res2@y,class_res2@yhat)
                    
                    save(confusion_matrix_1,file="confusion_matrix_scdaCMA.Rda")
                    confusion_matrix_list[[3]]<-table(class_res2@y,class_res2@yhat)
                    
                    if(length(class_levels_vec)==2){
                        
                        testeval_res_auc<-evaluation(class_res,measure = "auc")
                        save(testeval_res_auc,file="testeval_res_auc.Rda")
                        
                        print(testeval_res_auc)
                        
                        test_auc<-100*(mean(testeval_res_auc@score))
                        
                        test_acc_mat<-c(test_acc_mat,test_auc)
                        
                        
                        
                    }else{
                        
                        test_acc<-evaluation(class_res)
                        
                        test_acc<-100*(1-mean(test_acc@score))
                        test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                        
                        
                    }
                    
                    
                    #  }
                    
                    #if(best_classifier_name==classifier_names[4])
                    {
                        
                        s1<-ldply(tune_svm@tuneres,rbind)
                        
                        s2<-apply(s1,2,mean)
                        # tune_svm<-new("tuneres", fold = tune_svm@fold, method = tune_svm@method,
                        #tuneres = s2, hypergrid = tune_svm@hypergrid)
                        
                        t1<-new("list")
                        t1[[1]]<-s2
                        
                        tune_svm1<-tune_svm
                        tune_svm1@tuneres<-t1
                        
                        class_res <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA, tuneres=tune_svm1, kernel ="radial",trace=FALSE,probability=TRUE)
                        
                        
                        set.seed(999)
                        class_res <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA, tuneres=tune_svm1, kernel ="radial",trace=FALSE,probability=TRUE)
                        
                        
                        
                        b1<-best(tune_svm1)
                        
                        save(b1,file="b1_svm.Rda")
                        
                        learnmatrix<-as.numeric(learnmatrix)
                        
                        set.seed(999)
                        class_res2<-svmCMA(X = X, y = Y, learnind = learnmatrix, delta = median(unlist(b1)),probability=TRUE)
                        
                        save(class_res2,file="svmCMA.Rda")
                        
                        print("svm")
                        print(ftable(class_res2))
                        
                        
                        confusion_matrix_1<-table(class_res2@y,class_res2@yhat)
                        
                        save(confusion_matrix_1,file="confusion_matrix_svmCMA.Rda")
                        confusion_matrix_list[[4]]<-table(class_res2@y,class_res2@yhat)
                        
                        if(length(class_levels_vec)==2){
                            
                            testeval_res_auc<-evaluation(class_res,measure = "auc")
                            #save(testeval_res_auc,file="testeval_res_auc.Rda")
                            
                            print(testeval_res_auc)
                            
                            test_auc<-100*(mean(testeval_res_auc@score))
                            
                            test_acc_mat<-c(test_acc_mat,test_auc)
                            
                            
                            
                        }else{
                            
                            test_acc<-evaluation(class_res)
                            
                            test_acc<-100*(1-mean(test_acc@score))
                            test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                            
                            
                        }
                        
                    }
                    
                    #if(best_classifier_name==classifier_names[5])
                    {
                        class_res <- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = rfCMA,trace=FALSE)
                        
                        learnmatrix<-as.numeric(learnmatrix)
                        
                        # class_res2<-svmCMA(X = X, y = Y, learnind = learnmatrix, delta = median(unlist(b1)))
                        
                        class_res2 <- rfCMA(X =X, y = Y, learnind=learnmatrix, varimp = FALSE)
                        
                        save(class_res2,file="rfCMA.Rda")
                        
                        print(ftable(class_res2))
                        
                        
                        confusion_matrix_1<-table(class_res2@y,class_res2@yhat)
                        
                        save(confusion_matrix_1,file="confusion_matrix_rfCMA.Rda")
                        confusion_matrix_list[[5]]<-table(class_res2@y,class_res2@yhat)
                        
                        if(length(class_levels_vec)==2){
                            
                            testeval_res_auc<-evaluation(class_res,measure = "auc")
                            #save(testeval_res_auc,file="testeval_res_auc.Rda")
                            
                            print(testeval_res_auc)
                            
                            test_auc<-100*(mean(testeval_res_auc@score))
                            
                            test_acc_mat<-c(test_acc_mat,test_auc)
                            
                            
                            
                        }else{
                            
                            test_acc<-evaluation(class_res)
                            
                            test_acc<-100*(1-mean(test_acc@score))
                            test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                            
                            
                        }
                        
                    }
                    
                    #if(best_classifier_name==classifier_names[6])
                    {
                        class_res<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = nnetCMA,trace=FALSE)
                        
                        #nnetresult <- nnetCMA(X=golubX, y=golubY, learnind=learnind, size = 3, decay = 0.01)
                        class_res2 <- nnetCMA(X =X, y = Y, learnind=learnmatrix)
                        
                        save(class_res2,file="nnetCMA.Rda")
                        
                        print(ftable(class_res2))
                        
                        
                        confusion_matrix_1<-table(class_res2@y,class_res2@yhat)
                        
                        save(confusion_matrix_1,file="confusion_matrix_nnetCMA.Rda")
                        confusion_matrix_list[[6]]<-table(class_res2@y,class_res2@yhat)
                        
                        if(length(class_levels_vec)==2){
                            
                            testeval_res_auc<-evaluation(class_res,measure = "auc")
                            #save(testeval_res_auc,file="testeval_res_auc.Rda")
                            
                            print(testeval_res_auc)
                            
                            test_auc<-100*(mean(testeval_res_auc@score))
                            
                            test_acc_mat<-c(test_acc_mat,test_auc)
                            
                            
                            
                        }else{
                            
                            test_acc<-evaluation(class_res)
                            
                            test_acc<-100*(1-mean(test_acc@score))
                            test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                            
                            
                        }
                        
                    }
                    
                    #if(best_classifier_name==classifier_names[7])
                    {
                        class_res<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = plrCMA,trace=FALSE)
                        
                        class_res2 <- plrCMA(X =X, y = Y, learnind=learnmatrix)
                        
                        save(class_res2,file="plrCMA.Rda")
                        
                        print(ftable(class_res2))
                        
                        
                        confusion_matrix_1<-table(class_res2@y,class_res2@yhat)
                        
                        save(confusion_matrix_1,file="confusion_matrix_plrCMA.Rda")
                        confusion_matrix_list[[7]]<-table(class_res2@y,class_res2@yhat)
                        
                        if(length(class_levels_vec)==2){
                            
                            testeval_res_auc<-evaluation(class_res,measure = "auc")
                            #save(testeval_res_auc,file="testeval_res_auc.Rda")
                            
                            print(testeval_res_auc)
                            
                            test_auc<-100*(mean(testeval_res_auc@score))
                            
                            test_acc_mat<-c(test_acc_mat,test_auc)
                            
                            
                            
                        }else{
                            
                            test_acc<-evaluation(class_res)
                            
                            test_acc<-100*(1-mean(test_acc@score))
                            test_acc_mat<-c(test_acc_mat,test_acc) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                            
                            
                        }
                        
                    }
                    
                    
                    if(length(class_levels_vec)==2){
                        
                        
                        
                        acc_mat<-cbind(emat1,test_acc_mat) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                        
                        
                        colnames(acc_mat)<-c("Training kfold CV Accuracy (AUC)","Training permuted kfold CV accuracy","Score", "Test accuracy(AUC)")
                        
                        print("Test set evaluation using selected features and the best classifier based on AUC measure")
                        print(acc_mat)
                        
                        #rownames(acc_mat)<-best_classifier_name[1]
                        
                        write.table(acc_mat,file="Classification_evaluation_results_AUC.txt",sep="\t")
                        
                        
                    }else{
                        
                        acc_mat<-cbind(emat1,test_acc_mat) #cbind(best_kfold_acc,permkfold_acc,test_acc)
                        
                        
                        colnames(acc_mat)<-c("Training kfold CV Accuracy (Misclassification)","Training permuted kfold CV Accuracy (Misclassification)","Score", "Test accuracy (Misclassification)")
                        
                        print("Test set evaluation using selected features and the best classifier based on misclassification rate measure")
                        print(acc_mat)
                        
                        #rownames(acc_mat)<-best_classifier_name[1]
                        
                        write.table(acc_mat,file="Classification_evaluation_results_misclassification.txt",sep="\t")
                        
                    }
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    acc_mat<-acc_mat[,-c(3)]
                    
                    
                    mainlab<-paste("Performance evaluation using classifiers and selected features",sep="")
                    
                    if(output.device.type!="pdf"){
                        
                        temp_filename_1<-"Figures/Barplot_classification_accuracy.png"
                        
                        png(temp_filename_1,width=plots.width,height=plots.height,res=plots.res,type=plots.type,units="in")
                    }
                    
                    
                    barplot(acc_mat,xaxt="n",main=mainlab,col=boxplot.col.opt,ylab="Classification accuracy(%)",ylim=c(0,100))
                    
                    if(length(class_levels_vec)==2){
                        axis(side=1,at=seq(1,4),labels=c("kfold CV","Permuted kfold CV","Test set","AUC"),cex.axis=0.9)
                    }else{
                        axis(side=1,at=seq(1,3),labels=c("kfold CV","Permuted kfold CV","Test set"),cex.axis=0.9)
                    }
                    #axis(side=1,at=seq(1,3),labels=c("kfold CV accuracy","Permuted kfold CV accuracy","Test set accuracy"),cex.axis=0.9)
                    
                    
                    
                    
                    if(output.device.type!="pdf"){
                        
                        dev.off()
                    }
                    
                    
                    
                    
                    
                    
                    
                    
                    dev.off()
                    
                }
                
            }else{
                
                #acc_mat<-cbind(best_innerkfold_acc,outerkfold_acc,outerpermkfold_acc)
                #acc_mat<-cbind(best_kfold_acc,permkfold_acc)
                
                acc_mat<-acc_mat[,-c(3)]
                
                colnames(acc_mat)<-c("kfold CV accuracy","Permuted kfold CV accuracy")
                mainlab<-paste("Performance evaluation using ",best_classifier_name," classifier and selected features",sep="")
                
                if(output.device.type!="pdf"){
                    
                    temp_filename_1<-"Figures/Barplot_classification_accuracy.png"
                    
                    png(temp_filename_1,width=plots.width,height=plots.height,res=plots.res,type=plots.type,units="in")
                }
                
                
                
                barplot(acc_mat,xaxt="n",main=mainlab,col=boxplot.col.opt,ylab="Classification accuracy(%)",ylim=c(0,100))
                axis(side=1,at=seq(1,2),labels=c("Best kfold CV","Permuted kfold CV"),cex.axis=0.9)
                
                
                
                if(output.device.type!="pdf"){
                    
                    dev.off()
                }
                
                
            }
            
            
            
            
        }
        
        
        dev.off()
        
        eval_mat1<-rbind(eval_mat1,eval_mat_perm_final)
        
        
        
        # print(eval_mat1)
        #emat1
        write.table(emat1,file="Classifier_accuracy_comparison_training_set.txt",sep="\t",row.names=TRUE)
        
        confounder_eval=NA
        if(is.na(confounder.matrix)==FALSE){
            
            response_mat<-confounder.matrix[,-c(1)]
            confounder_eval<-try(runlmreg(X=good_feats,Y=response_mat,fdrmethod=confounderfdrmethod,fdrthresh=confounderfdrthresh),silent=TRUE)
        }
        
        parentoutput_dir=outloc
        setwd(parentoutput_dir)
        
        if(globalcor==TRUE){
            
            print("##############Level 2: Correlation network analysis selected features###########")
            print(paste("Generating metabolome-wide ",cor.method," correlation network",sep=""))
            data_m_fc_withfeats<-as.data.frame(Xorig)
            
            good_feats<-as.data.frame(good_feats)
            #print(goodfeats[1:4,])
            sigfeats_index<-which(data_m_fc_withfeats$mz%in%good_feats$mz)
            sigfeats<-sigfeats_index
            
            #outloc<-paste(parentoutput_dir,"/Allcornetworksigfeats","log2fcthresh",log2.fold.change.thresh,"/",sep="")
            outloc<-paste(parentoutput_dir,"/MWASresults","/",sep="")
            
            dir.create(outloc)
            setwd(outloc)
            
            #cor.method="spearman",networktype="complete",abs.cor.thresh=0.4,cor.fdrthresh=0.05,max.cor.num=100,net_node_colors=c("green","red"), net_legend=TRUE
            
            if(networktype=="complete"){
                mwan_fdr<-do_cor(data_m_fc_withfeats,subindex=sigfeats_index,targetindex=NA,outloc,networkscope="global",cor.method,abs.cor.thresh,cor.fdrthresh,max.cor.num,net_node_colors,net_legend)
            }else{
                if(networktype=="GGM"){
                    mwan_fdr<-get_partial_cornet(data_m_fc_withfeats, sigfeats.index=sigfeats_index,targeted.index=NA,networkscope="global",cor.method,abs.cor.thresh,cor.fdrthresh,outloc=outloc,net_node_colors,net_legend)
                }else{
                    print("Invalid option. Please use complete or GGM.")
                }
            }
            
            print("##############Level 2: processing complete###########")
        }
        
        if(length(good_feats)>0){
            
            write.csv(good_feats,file="train_selected_data.csv",row.names=FALSE)
            try(write.csv(good_feats_test,file="test_selected_data.csv",row.names=FALSE),silent=TRUE)
            try(write.csv(confounder_eval,file="confounder_eval.csv",row.names=FALSE),silent=TRUE)
            
            write.csv(Ytrain_mat,file="train_classlabels_data.csv",row.names=FALSE)
            try(write.csv(Ytest_mat,file="test_classlabels_data.csv",row.names=FALSE),silent=TRUE)
        }
        
        
        #print(acc_mat)
        #bestcvacc=best_kfold_acc,permcvacc=permkfold_acc,testacc=test_acc,
        return(list(train.selected.data=good_feats,test.selected.data=good_feats_test,classifier.comparison=emat1,feature.selection.matrix=stability_matrix_1,best.performance.measures=acc_mat,train.class=Ytrain_mat,test.class=Ytest_mat,confounder.eval.res=confounder_eval))
        
    }
    
    
    
}


get_KIindex<-function(m,k){
sum1<-apply(m,2,sum)
n<-dim(m)[1]

#k<-min(sum1)


KI_res<-{}
for(i in 1:dim(m)[2]){
for(j in i:dim(m)[2]){
if(i!=j){


mi<-which(m[,i]==1)
mj<-which(m[,j]==1)
r<-length(intersect(mi,mj))


#k<-min(length(mi),length(mj))
k2_n<-(k)^2/n
KI_ind<-(r-k2_n)/(k-k2_n)
KI_res<-c(KI_res,KI_ind)
}
}

}

KI_res_mean<-mean(KI_res,na.rm=TRUE)
return(KI_res_mean)
}



get_DSindex<-function(m){

sum1<-apply(m,2,sum)

n<-dim(m)[1]

#k<-min(sum1)

DS_res<-{}

for(i in 1:dim(m)[2]){

	for(j in i:dim(m)[2]){

		if(i!=j){

	mi<-which(m[,i]==1)

	mj<-which(m[,j]==1)

	r<-length(intersect(mi,mj))

	num_mi<-length(mi)

	num_mj<-length(mj)

	dice_index<-(2*r)/(num_mi+num_mj)

	DS_res<-c(DS_res,dice_index)

		}

	}

 }

DS_res_mean<-mean(DS_res,na.rm=TRUE)

return(DS_res_mean)

}

 


monitor <- function(obj) {
         minEval = min(obj$evaluations);
         filter = obj$evaluations == minEval;
         bestObjectCount = sum(rep(1, obj$popSize)[filter]);
         # ok, deal with the situation that more than one object is best
         if (bestObjectCount > 1) {
             bestSolution = obj$population[filter,][1,];
         } else {
             bestSolution = obj$population[filter,];
         }
         outputBest = paste(obj$iter, " #selected=", sum(bestSolution),
                            " Best (Error=", minEval, "): ", sep="");
         for (var in 1:length(bestSolution)) {
             outputBest = paste(outputBest,
                 bestSolution[var], " ",
                 sep="");
         }
         outputBest = paste(outputBest, "\n", sep="");

         cat(outputBest);
	save(bestSolution,file="gabestsol.Rda")
	return(bestSolution)     
	
}


find_similar_samples<-function(part_group, particle_ind=NA,num_neighbors=3){

	bcdata<-as.matrix(part_group)

	if(is.na(particle_ind)==FALSE){
	targetdata<-as.matrix(part_group[particle_ind,])
	}else{

	targetdata<-as.matrix(part_group)
	}

	data_dim<-dim(bcdata)

	#num_neighb<-100
	affy_names<-colnames(bcdata)

	bc.names<-rownames(bcdata)

	i<-1
	
	dist.matrix<-{}
	
	#k=100, for top 100 matches
	dist.matrix<-ann(ref=as.matrix(bcdata), target=as.matrix(targetdata), tree.type="brute", k=num_neighbors, split.rule="midpt", shrink.rule="centroid", verbose=FALSE)

	#dist.matrix<-ann(ref=as.matrix(bcdata), target=as.matrix(targetdata), tree.type="kd", k=num_neighbors, split.rule="midpt", shrink.rule="centroid", verbose=TRUE)
	rm(bcdata)
	
	if(FALSE){
	dist.print<-t(dist.matrix$knnIndexDist)

	sample_names<-bc.names[dist.print[1:100,]]

	sample_list<-{}
	exp_list<-{}
	tissue_list<-{}
	pval_list<-{}
	match_bclist<-{}


	for (i in 1:100)
	{
		#format sample names##

		
		sample<-sapply(strsplit(as.character(sample_names[i]), "."), head, n=2)[1]

		sample<-toupper(sample)

		sample_list<-rbind(sample_list, sample)

		
	}

	tab<-data.frame(Number=(1:100), Sample=as.factor(sample_list), Distance=dist.print[101:200,], row.names=T)
	
	return(tab)
	}
	return(dist.matrix$knnIndexDist)
}


#function to find nearest neighbor of a particle
nn_search<-function(particle_ind, part_group)
{
	partm<-as.matrix(part_group[particle_ind,])
	
	#nn_arr<-array(0,2)
	dist_mat<-dist(as.matrix(part_group))
	dist_mat<-as.matrix(dist_mat)
	nn_order<-order(dist_mat[particle_ind,])
	nn_order<-nn_order[-c(1)]
	if(FALSE){
	for(i in 1:length(part_group))
	{
		groupm<-as.matrix(part_group[i])
		
		pdist<-dist(rbind(partm, groupm))
		
		nn_arr<-rbind(nn_arr, as.array(cbind(i,pdist)))
	}
	
	#ascending order with respect to distance
	nn_order<-order(nn_arr[,2])
	}

	return(nn_order)
}

	 eval_fit_kfold_diff_vold<-function(particle, trainm,trainclass,numfolds,errortype="AUC",accuracyweightA=5,featweight=0.06,max_num_feats=10,kname="radial")
        {
                num_feat<-0
                #select the columns with selected features
                ind<-which(particle==1)
                #need to add 1 to take into account the index of the feature in the original dataset
                col_sel<-ind
                folderror<-{}
                folderror_perm<-{}
                num_feat<-length(col_sel)
                if(num_feat>1)
                {
                        trainset<-trainm[,c(col_sel)]
                        trainset<-cbind(trainclass,trainset)
                        trainset<-data.frame(trainset)
                        folderror<-{}
                        folderror_perm<-{}
                        {
                         for(f in 1:3)
                        {
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype)
                        folderror_cur<-model$confint[1] #mean_acc
                        folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
                        rm(model)
                        {
                        rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype)
                        folderror_cur_perm<-model$confint[2]
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
                        }
                                folderror<-mean(folderror,na.rm=TRUE)
                                folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(4)))
                        }
                                if(num_feat>max_num_feats){
                                        fitfunc<-((accuracyweightA)*(folderror-folderror_perm))-(featweight*100*(num_feat/length(particle)))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderror-folderror_perm))
                                }
                print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
                }
                else
                {
                        folderror<-1
                        folderror_perm<-100
                        fitfunc<-(-100)
                }
                rm(col_sel)
                rm(num_feat)
		fitfunc<-(-1)*fitfunc
                return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))

        }



	
 #function to evaluate k-fold CV
        eval_fit_kfold_diff<-function(particle, trainm,trainclass,numfolds,errortype="AUC",accuracyweightA=5,accuracyweightB=1,featweight=0.06,max_num_feats=10,kname="radial")
        {

                num_feat<-0
                #select the columns with selected features 
                ind<-which(particle==1)

                #need to add 1 to take into account the index of the feature in the original dataset
                col_sel<-ind
		folderror<-{}
                        folderror_perm<-{}
                num_feat<-length(col_sel)
                #print("itr num feat:")
                if(num_feat>1)
                {
                        trainset<-trainm[,c(col_sel)]
                        trainset<-cbind(trainclass,trainset)
                        trainset<-data.frame(trainset)
                        folderror<-{}
			folderror_perm<-{}
                        seed_vec<-c(129532,839147,407700)
			for(f in 1:3)
                        {
                      	#setseed=runif(1,1000000,n=1)
			#setseed=round(setseed)  
			#setseed=NA #321 
			setseed=seed_vec[f]
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=setseed)
			folderror_cur<-model$confint[1] #-(model$confint[2]-model$confint[1]) #mean_acc
                        folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			rm(model)
			
			#for(f in 1:3)
                        #{
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=setseed)
                        folderror_cur_perm<-model$confint[2]
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
						rm(model)	
                        }
				###############******************************
				#Note v40: changed confint to only mean 
				folderror<-mean(folderror,na.rm=TRUE)-(1.96*(sd(folderror,na.rm=TRUE)/sqrt(2)))
				folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
				#folderror<-mean(folderror,na.rm=TRUE)
                                #folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(3)))

				if(num_feat>max_num_feats){
				fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))	
				}else{
					fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror)	
				}
                print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
	}
                else
                {
                folderror<-1
                folderror_perm<-100
				fitfunc<-(-100)
		}
                rm(col_sel)
                rm(num_feat)
		fitfunc<-(-1)*fitfunc
                return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))

        }




#function to evaluate k-fold CV
	eval_fit_test_diff_v0<-function(particle, numfolds,trainm,trainclass,testm,testclass,errortype="AUC",kname="radial",featweight=0.05,accuracyweightA=5,
	accuracyweightB=1,max_num_feats=10)
	{
		
		num_feat<-0
		#select the columns with selected features 
		ind<-which(particle==1)

		folderror_perm<-{}

		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		#print("itr num feat:")
		#print(length(ind))	
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			
			#print(trainset[1:4,])
			#trainset<-traindata[,c(col_sel)]
		
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			folderror<-{}
			folderror_vec<-{}

			#if(FALSE)
			seed_vec<-c(129532,839147,407700)
			{
			 for(f in 1:3)
                        {
			 #model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="AUC")
	
			setseed=seed_vec[f]
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=setseed)
                        folderror_cur_reg<-model$confint[1]
	
			#folderror_cur<-model$mean_acc
			#print(model)
			#print(folderror_cur)	
			#folderror<-c(folderror,folderror_cur) #model$tot.accuracy)
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=setseed)
                        folderror_cur_perm<-model$confint[2]
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
                       } 

		testset<-testm[,c(col_sel)]	
		mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testset)
svm_table<-table(predfit,testclass)

class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==testclass))/length(testclass)
for(c in 1:dim(svm_table)[1]){
testclass_ind<-which(testclass==class_names[c])
beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
}
print(beracc)
beracc<-as.numeric(beracc)
print(beracc)
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

		#	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
		folderror<-svm_acc[i]

                                folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                if(num_feat>max_num_feats){
                                fitfunc<-(accuracyweightA*(folderror_cur_reg-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderror_cur_reg-folderror_perm))+accuracyweightB*(folderror)
                                }
					
	
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		 folderror<-1
                folderror_perm<-100
                                fitfunc<-(-100)
		}
		#fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))  #-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		#return(fitfunc)
		
		fitfunc<-(-1)*fitfunc
		return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
	}


#function to evaluate k-fold CV

#function to evaluate k-fold CV
eval_fit_test_diff<-function(particle, numfolds,trainm,trainclass,testm,testclass,errortype="AUC",kname="radial",featweight=0.05,accuracyweightA=5,
accuracyweightB=1,accuracyweightC=1,accuracyweightD=0,max_num_feats=10)
{
    
    num_feat<-0
    #select the columns with selected features
    ind<-which(particle==1)
    
    folderror_perm<-{}
    
    num_itrs<-3
    #need to add 1 to take into account the index of the feature in the original dataset
    col_sel<-ind
    num_feat<-length(col_sel)
    #print("itr num feat:")
    #print(length(ind))
    
    testclassorig<-testclass
    testmorig<-testm
    
    
    if(num_feat>2)
    {
        trainset<-trainm[,c(col_sel)]
        trainset<-cbind(trainclass,trainset)
        trainset<-data.frame(trainset)
        
        #model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
        folderror<-{}
        folderror_vec<-{}
        
        #if(FALSE)
        seed_vec<-runif(num_itrs,1,1000000) #c(129532,839147,407700,100,1000,555,9999,123456,83414,1242)
        seed_vec<-c(129532)
        setseed=seed_vec[1] #129532 #seed_vec[3]
        
        origtenfoldacc<-{}
        perm_acc<-{}
        seed_vec<-c(129532,839147,407700)
        
        for(r1 in 1:num_itrs)
        #perm_acc<-lapply(1:num_itrs,function(r1)
        {
            seednum=seed_vec[r1]
            
            model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=seednum)
            foldacc<-model$confint[1] #$mean_acc #$confint[1]nohupOCFSvmay2415_v11_arcene_CV2_top5itr2.out
            
            set.seed(seed_vec[r1])
            rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
            model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=seednum)
            
            permtenfoldacc<-model$confint[2] #$mean_acc #$confint[2] #tot.accuracy
            perm_acc<-c(perm_acc,permtenfoldacc)
            #return(list("perm"=permtenfoldacc,"orig"=foldacc))
            origtenfoldacc<-c(origtenfoldacc,foldacc)
        }#)
        
        
        foldacc<-mean(origtenfoldacc,na.rm=TRUE)-1.96*sd(origtenfoldacc,na.rm=TRUE)/sqrt(num_itrs)
        perm_acc<-mean(perm_acc,na.rm=TRUE)+1.96*sd(perm_acc,na.rm=TRUE)/sqrt(num_itrs)
        
        
        trainfolderror<-(foldacc)
        trainfolderror_perm<-(perm_acc)
        
        
        #eval test accuracy
        testset<-testm[,c(col_sel)]
        
        mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
        
        predfit<-predict(mod_cv,testset)
        svm_table<-table(predfit,testclass)
        
        class_names<-rownames(svm_table)
        beracc<-{}
        i<-1
        svm_acc<-{}
        totacc<-length(which(predfit==testclass))/length(testclass)
        for(c in 1:dim(svm_table)[1]){
            testclass_ind<-which(testclass==class_names[c])
            beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
        }
        
        beracc<-as.numeric(beracc)
        
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
        
        testacc<-svm_acc[i] #test accuracy
        
        #train and train perm
        if(FALSE)
        {
            
            set.seed(seed_vec[r1])
            rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
            
            mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
            
            testclass<-trainset$trainclass
            testset<-trainset[,-c(1)]
            
            predfit<-predict(mod_cv,testset)
            svm_table<-table(predfit,testclass)
            
            class_names<-rownames(svm_table)
            beracc<-{}
            i<-1
            svm_acc<-{}
            totacc<-length(which(predfit==testclass))/length(testclass)
            for(c in 1:dim(svm_table)[1]){
                testclass_ind<-which(testclass==class_names[c])
                beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
            }
            print(beracc)
            beracc<-as.numeric(beracc)
            print(beracc)
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
            
            folderror_train<-svm_acc[i]
            
            #}
            
            
            beracc<-{}
            svm_acc<-{}
            for(i in 1:num_itrs){
                #perm train
                set.seed(seed_vec[i])
                rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                
                mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass[rand_ind], type="C",kernel=kname)
                predfit<-predict(mod_cv,trainset[,-1])
                svm_table<-table(predfit,testclass)
                
                testclass<-trainset$trainclass
                testset<-trainset[,-c(1)]
                
                class_names<-rownames(svm_table)
                totacc<-length(which(predfit==testclass))/length(testclass)
                for(c in 1:dim(svm_table)[1]){
                    testclass_ind<-which(testclass==class_names[c])
                    beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
                }
                print(beracc)
                beracc<-as.numeric(beracc)
                print(beracc)
                beracc<-mean(beracc,na.rm=TRUE)
                
                if(errortype=="CV"){
                    svm_acc<-c(svm_acc,(totacc*100))
                }else{
                    if(errortype=="AUC"){
                        pred_acc<-multiclass.roc(testclass,as.numeric(predfit))
                        pred_acc_orig<-pred_acc$auc[1]
                        auc_acc<-pred_acc_orig
                        
                        
                        svm_acc<-c(svm_acc,auc_acc*100)
                    }else{
                        svm_acc<-c(svm_acc,(beracc*100))
                    }
                }
                
            }
            folderror_train_perm<-mean(svm_acc,na.rm=TRUE)+1.96*sd(svm_acc,na.rm=TRUE)/(sqrt(num_itrs))
        }
        
        
        
        
        #REVERSE evaluation
        #if(FALSE)
        {
            
            #testm<-testmorig
            #testclass<-testclassorig
            
            testset<-testm[,c(col_sel)]
            
            trainclass_temp<-trainclass
            trainset_temp<-trainm[,c(col_sel)]
            
            trainset<-testset
            trainclass<-testclass
            
            testclass<-trainclass_temp
            testset<-trainset_temp
            
            max_lim<-max(100,dim(testset)[1])
            set.seed(321)
            subtrain_ind<-sample(x=seq(1,dim(trainset)[1]),size=max_lim,replace=TRUE)
            trainset<-trainset[subtrain_ind,]
            trainclass<-trainclass[subtrain_ind]
            
            mod_cv <- svm(x=trainset,y=trainclass, type="C",kernel=kname)
            predfit<-predict(mod_cv,testset)
            svm_table<-table(predfit,testclass)
            
            class_names<-rownames(svm_table)
            beracc<-{}
            i<-1
            svm_acc<-{}
            totacc<-length(which(predfit==testclass))/length(testclass)
            for(c in 1:dim(svm_table)[1]){
                testclass_ind<-which(testclass==class_names[c])
                beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
            }
            print(beracc)
            beracc<-as.numeric(beracc)
            print(beracc)
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
            
            #	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
            folderror_test2<-svm_acc[i] #reverse
        }
        
        
        
        #if(num_feat>max_num_feats)
        {
            
            
            #Serialization technique: all functions range between 0-100
            #fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_train-folderror_train_perm)+accuracyweightB*(folderror_test2)+((featweight*100*((max_num_feats-num_feat)/length(particle))))
            
            
            #gives golub 0.96 AUC
            if(FALSE){
                weightA<-0.7  #difference kfold and kfold permuted
                weightB<-0    #test set accuracy
                weightC<-0.05  #reverse accuracy
                weightD<-0.2  #kfold accuracy
                weightE<-0.05
                featweight<-weightE
            }
            
            
            # fitfunc<-(accuracyweightA*(trainfolderror-trainfolderror_perm))+accuracyweightB*(folderror_train-folderror_train_perm)+accuracyweightC*(folderror_test2)+accuracyweightD*(folderror)+((featweight*(max((100*(max_num_feats-num_feat))/max_num_feats,100))))
            
            fitfunc<-(accuracyweightA*(trainfolderror-trainfolderror_perm))+accuracyweightB*(testacc)+accuracyweightC*(folderror_test2)+accuracyweightD*(trainfolderror)-(featweight*100*(num_feat/length(particle)))
            
            #((featweight*(max((100*(max_num_feats-num_feat))/max_num_feats,100))))
            
        }
        
        
        
        rm(trainset)
        
    }
    else
    {
        trainfolderror<-1
        trainfolderror<-1
        testacc<-1
        folderror_test2<-1
        trainfolderror_perm<-100
        fitfunc<-(-100)
    }
    print(paste("accuracy: ", trainfolderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
    rm(col_sel)
    rm(num_feat)
    
    fitfunc<-(-1)*fitfunc
    return(list("fitfunc"=fitfunc,"cverror"=trainfolderror,"cvpermerror"=trainfolderror_perm,"testacc"=testacc,"reverseacc"=folderror_test2))
}


	#function to evaluate k-fold CV
	eval_fit_test_diff_v2015<-function(particle, numfolds,trainm,trainclass,testm,testclass,errortype="AUC",kname="radial",featweight=0.05,accuracyweightA=5,
	accuracyweightB=1,accuracyweightC=1,accuracyweightD=0,max_num_feats=10)
	{
		
		num_feat<-0
		#select the columns with selected features 
		ind<-which(particle==1)

		folderror_perm<-{}

		num_itrs<-3
		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		#print("itr num feat:")
		#print(length(ind))	

		testclassorig<-testclass
	testmorig<-testm		

		
		if(num_feat>2)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			trainset<-data.frame(trainset)
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			folderror<-{}
			folderror_vec<-{}

			#if(FALSE)
			seed_vec<-runif(num_itrs,1,1000000) #c(129532,839147,407700,100,1000,555,9999,123456,83414,1242)
			seed_vec<-c(129532)
			setseed=seed_vec[1] #129532 #seed_vec[3]
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=setseed)
                        foldacc<-model$confint[1]
		
		origtenfoldacc<-{}
                perm_acc<-{}
                seed_vec<-c(129532,839147,407700)

                for(r1 in 1:num_itrs)
		#perm_acc<-lapply(1:num_itrs,function(r1)
                {
                        seednum=seed_vec[r1]
        
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,setseed=seednum)
                        foldacc<-model$confint[1] #$mean_acc #$confint[1]nohupOCFSvmay2415_v11_arcene_CV2_top5itr2.out 

			set.seed(seed_vec[r1])
                        rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,setseed=seednum)

	                permtenfoldacc<-model$confint[2] #$mean_acc #$confint[2] #tot.accuracy
                        perm_acc<-c(perm_acc,permtenfoldacc)
             		#return(list("perm"=permtenfoldacc,"orig"=foldacc))
			origtenfoldacc<-c(origtenfoldacc,foldacc)
		   }#)

	#	perm_acc1<-as.data.frame(perm_acc)
		
		#perm_acc<-as.numeric(perm_acc1[,seq(1,dim(perm_acc1)[2],2)])
		#origtenfoldacc<-as.numeric(perm_acc1[,seq(2,dim(perm_acc1)[2],2)])
		#perm_acc<-unlist(perm_acc$perm)
		#foldacc<-unlist(perm_acc$orig)

	#write.table(perm_acc,file="perm_acc.txt",sep="\t",row.names=FALSE)	
	#perm_acc<-as.numeric(perm_acc)
		#fold_acc<-as.numeric(foldacc)
		
		#pdf("perm_acc_hist.pdf")
		#hist(perm_acc)
		#dev.off()
                
		#perm_acc<-mean(perm_acc,na.rm=TRUE)+2*sd(perm_acc,na.rm=TRUE)
                #fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])

                #fitfunc<-accuracyweightA*(foldacc-perm_acc)+accuracyweightB*(foldacc)-(100*featweight*(1-(dim(trainset)[2]/dim(trainm)[2])))

		foldacc<-mean(origtenfoldacc,na.rm=TRUE)-1.96*sd(origtenfoldacc,na.rm=TRUE)/sqrt(num_itrs)
		folderror_test<-foldacc #mean(origtenfoldacc,na.rm=TRUE)
		folderror<-(foldacc)

			
			folderror_perm<-(perm_acc)
		#folderror<-mean(folderror,na.rm=TRUE)-(1.96*(sd(folderror,na.rm=TRUE)/sqrt(2)))
                #                folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                #folderror<-mean(folderror,na.rm=TRUE)
                                #folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(3)))

#if(FALSE){

 set.seed(seed_vec[r1])
 rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))

 mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)

testclass<-trainset$trainclass
                testset<-trainset[,-c(1)]

 predfit<-predict(mod_cv,testset)
 svm_table<-table(predfit,testclass)

 class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==testclass))/length(testclass)
for(c in 1:dim(svm_table)[1]){
testclass_ind<-which(testclass==class_names[c])
beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
}
print(beracc)
beracc<-as.numeric(beracc)
print(beracc)
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

folderror_train<-svm_acc[i]

#}


beracc<-{}
svm_acc<-{}
for(i in 1:num_itrs){
#perm train
set.seed(seed_vec[i])
 rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))

 mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass[rand_ind], type="C",kernel=kname)
 predfit<-predict(mod_cv,trainset[,-1])
 svm_table<-table(predfit,testclass)

testclass<-trainset$trainclass
                testset<-trainset[,-c(1)]

 class_names<-rownames(svm_table)
totacc<-length(which(predfit==testclass))/length(testclass)
for(c in 1:dim(svm_table)[1]){
testclass_ind<-which(testclass==class_names[c])
beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
}
print(beracc)
beracc<-as.numeric(beracc)
print(beracc)
beracc<-mean(beracc,na.rm=TRUE)

if(errortype=="CV"){
        svm_acc<-c(svm_acc,(totacc*100))
}else{
if(errortype=="AUC"){
        pred_acc<-multiclass.roc(testclass,as.numeric(predfit))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig


        svm_acc<-c(svm_acc,auc_acc*100)
}else{
svm_acc<-c(svm_acc,(beracc*100))
}
}

}
folderror_train_perm<-mean(svm_acc,na.rm=TRUE)+1.96*sd(svm_acc,na.rm=TRUE)/(sqrt(num_itrs))





#REVERSE evaluation
#if(FALSE)
{

		#testm<-testmorig
		#testclass<-testclassorig

		testset<-testm[,c(col_sel)]

		trainclass_temp<-trainclass
		trainset_temp<-trainm[,c(col_sel)]
	
		trainset<-testset
		trainclass<-testclass
		
		testclass<-trainclass_temp
		testset<-trainset_temp

		max_lim<-max(100,dim(testset)[1])	
		set.seed(321)
                subtrain_ind<-sample(x=seq(1,dim(trainset)[1]),size=max_lim,replace=TRUE)
                trainset<-trainset[subtrain_ind,]	
		trainclass<-trainclass[subtrain_ind]

#testclass<-trainclass_temp
 #               testset<-trainset_temp

		mod_cv <- svm(x=trainset,y=trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testset)
svm_table<-table(predfit,testclass)

class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==testclass))/length(testclass)
for(c in 1:dim(svm_table)[1]){
testclass_ind<-which(testclass==class_names[c])
beracc<-c(beracc,length(which(predfit[testclass_ind]==testclass[testclass_ind]))/length(testclass_ind))
}
print(beracc)
beracc<-as.numeric(beracc)
print(beracc)
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

		#	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
		folderror_test2<-svm_acc[i]
}

#if(FALSE)
{
		#folderror_test<-folderror+runif(1,-5,5)
		 #folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(2*(sd(folderror_perm,na.rm=TRUE))) #/sqrt(num_itrs))

		folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(1.96*(sd(folderror_perm,na.rm=TRUE))/sqrt(num_itrs))

		#if(num_feat>max_num_feats)
		{
                
#		 fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_test-folderror_test2)-(featweight*100*(num_feat/length(particle)))
 
#	             fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(mean(c(folderror_test,folderror_test2),na.rm=TRUE))-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat)) 
       

#Serialization technique: all functions range between 0-100
 #fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_train-folderror_train_perm)+accuracyweightB*(folderror_test2)+((featweight*100*((max_num_feats-num_feat)/length(particle))))

if(FALSE){
weightA<-0.9
weightB<-0.0
weightC<-0.1
weightD<-0.001
}

#gives golub 0.96 AUC
if(FALSE){
weightA<-0.7
weightB<-0
weightC<-0.05
weightD<-0.2
weightE<-0.05
featweight<-weightE
}




fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_train-folderror_train_perm)+accuracyweightC*(folderror_test2)+accuracyweightD*(folderror)+((featweight*(max((100*(max_num_feats-num_feat))/max_num_feats,100))))


	                        #}else{
                                 #       fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_test)
                                }
}

 
		if(FALSE){
		fitfunc<-accuracyweightA*(foldacc-perm_acc)+accuracyweightB*(folderror_test)-(100*featweight*((dim(trainset)[2]/dim(trainm)[2])))
folderror_perm<-perm_acc
folderror<-foldacc
		}	
                       
		if(FALSE)
		{
				t1<-t.test(x=folderror,y=folderror_perm,paired=T,alternative="greater")
				
				if(t1$p.value<0.05){
					#folderror_cur_reg<-min(folderror)
					folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(2*(sd(folderror_perm,na.rm=TRUE))/sqrt(num_itrs)) #/sqrt(4))) #max(folderror_perm)
					folderror<-mean(folderror,na.rm=TRUE)-(2*(sd(folderror,na.rm=TRUE))/sqrt(num_itrs))
					fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror_test)-(featweight*100*(num_feat/length(particle)))
			
					# fitfunc<-accuracyweight*(foldacc-perm_acc)+1*(foldacc)-(100*featweight*(1-num_g/dim(nci_xm)[2]))

				}else{
					fitfunc<-(-100)
				}	
		}


if(FALSE)
{
			#folderror<-mean(folderror,na.rm=TRUE)-(2*(sd(folderror_perm,na.rm=TRUE)))
			folderror_perm<-mean(folderror_perm,na.rm=TRUE)+(2*(sd(folderror_perm,na.rm=TRUE)))
                #fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])
			fitfunc<-(accuracyweightA*(folderror-folderror_perm))+accuracyweightB*(folderror)+0*folderror_test-(featweight*100*(num_feat/length(particle)))
}
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		 folderror<-1
                folderror_perm<-100
                                fitfunc<-(-100)
		}
		#fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))  #-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		print(paste("accuracy: ", folderror," num_feat:",num_feat," fitness:", fitfunc,sep=""))
		rm(col_sel)
		rm(num_feat)
		#return(fitfunc)
		#folderror_perm<-mean(folderror_perm,na.rm=TRUE)
		#folderror<-mean(folderror,na.rm=TRUE)	
		fitfunc<-(-1)*fitfunc
		return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
	}



#library(CMA)
#trainm<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/golub_R/filt_proc_golub2_orig.csv", sep=",", header=T)

#trainm<-read.csv("/home/stu/kuppal3/karan_libs/R_karan/nci_R/mean_replace_nci.csv", sep=",", header=T)
#trainm<-trainm[,-c(1)]
mdist<-function(x)
{
	
	t<-as.matrix(x)
	p<-dim(t)[2]
	#m<-apply(t,2,mean)
	m<-colMeans(t)
	s<-cov(t)
	mahalanobis(t,m,s, inverted=TRUE)
}

makefactors<-function(curdata)
{
alphavec=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

	########FACTOR TEST#########
	facttest<-apply(curdata,2, function(x){

		categ=TRUE
		if(is.na(as.numeric(x[1])))
		{
			categ=FALSE;
		}
		else
		{
			for(i in 1:length(x))
			{

				mod_val=x[i]%%2

				if(mod_val!=0 && mod_val!=1)
				{
					categ=FALSE;
				}


			}	
		}
		if(categ==TRUE)
		{
		factvec<-as.factor(x)
		getlevels=levels(factvec)
		factvec=as.vector(factvec)

		   for(g in 1:length(getlevels))
		   {
		       #print(length(which(factvec==getlevels[g])))
		       #flevelind=which(factvec==getlevels[g])

		       #print(factvec[flevelind[1:3]])
		       #factvec[flevelind]=alphavec[g]

			factvec[which(factvec==getlevels[g])]=alphavec[g]

		   }

			#x=as.factor(x)
			#x=as.character(x)

		x=as.data.frame(factvec)
		}
		
		return(x)
	
})
facttest=as.data.frame(facttest)
return(facttest)
}


evaluate_accuracy<-function(){
    
    #if(classifier=="pls")
    if(length(good_feats_index)>5)
    {
        
        #print("Sts1")
        
        #print(X[1:3,c(1:3,240:243)])
        
        save(list=ls(),file="t1.Rda")
        tune_plslda <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_ldaCMA, grids = list(comp = 1:5),trace=FALSE)
        
        if(length(class_levels)==2){
            #     tune_plslr <- tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_lrCMA, grids = list(comp = 1:5))
        }
        #print("Sts2")
        tune_plsrf <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_rfCMA, grids = list(comp = 1:5),trace=FALSE)
    }
    #if(classifier=="scda")
    {
        
        #print("Sts3")
        tune_scda <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = scdaCMA, grids = list( ),trace=FALSE)
    }
    #if(classifier=="svm")
    {
        #print("Sts4")
        tune_svm <- CMA::tune(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA, grids = list( ), kernel = "radial",trace=FALSE)
    }
    
    if(length(good_feats_index)>2){
        class_plslda <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_ldaCMA, tuneres = tune_plslda,trace=FALSE)
        
        class_plsrf <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_rfCMA, tuneres = tune_plsrf,trace=FALSE)
    }
    
    class_scda <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = scdaCMA, tuneres = tune_scda,trace=FALSE)
    
    class_svm <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = svmCMA, tuneres = tune_svm,kernel = "radial",trace=FALSE,probability=TRUE)
    class_rf <- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = rfCMA,trace=FALSE)
    
    class_nnet<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = nnetCMA,trace=FALSE)
    
    class_plr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = plrCMA,trace=FALSE)
    
    save(class_plslda,file="class_plslda.Rda")
    save(class_plsrf,file="class_plsrf.Rda")
    save(class_scda,file="class_scda.Rda")
    save(class_svm,file="class_svm.Rda")
    save(class_rf,file="class_rf.Rda")
    save(class_nnet,file="class_nnet.Rda")
    save(class_plr,file="class_plr.Rda")
    
    
    
    if(length(class_levels)==2){
        
        # class_plslr <- classification(X = X, y = Y, learningsets = fiveCV10iter, classifier = pls_lrCMA)
        
        # class_lassoplr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = LassoCMA)
        
        # class_elasticnetplr<- classification(X =X, y = Y, learningsets = fiveCV10iter, classifier = ElasticNetCMA)
        
        #eval_plslr<-evaluation(class_plslr)
        
        # eval_lassoplr<-evaluation(class_lassoplr)
        
        #eval_elasticnetplr<-evaluation(class_elasticnetplr)
        #measures="auc"
        
        eval_measure="auc"
        #eval_measure="misclassification"
        
    }else{
        
        eval_measure="misclassification"
    }
    
    
    print("here: evaluation")
    
    if(length(good_feats_index)>2){
        
        # if(eval_measure=="misclassification")
        
        eval_plslda<-evaluation(class_plslda,measure=eval_measure)
        eval_plsrf<-evaluation(class_plsrf,measure=eval_measure)
        eval_scda<-evaluation(class_scda,measure=eval_measure)
        eval_svm<-evaluation(class_svm,measure=eval_measure)
        eval_rf<-evaluation(class_rf,measure=eval_measure)
        eval_nnet<-evaluation(class_nnet,measure=eval_measure)
        eval_plr<-evaluation(class_plr,measure=eval_measure)
        
        if(eval_measure=="auc")
        {
            eval_plslda@score=1-mean(eval_plslda@score)
            eval_plsrf@score=1-mean(eval_plsrf@score)
            eval_scda@score=1-mean(eval_scda@score)
            eval_svm@score=1-mean(eval_svm@score)
            eval_rf@score=1-mean(eval_rf@score)
            eval_nnet@score=1-mean(eval_nnet@score)
            eval_plr@score=1-mean(eval_plr@score)
            
        }
    }
    
    
}


performCMA<-function(trainm, trainclass, testm, testclass, outloc, maxnum, minnum, stepitr, gsmethods, pct_train=1,featweight, accuracyweight, kname, maxitrs, minpresent, norm_method, tolerance=1, classindex, numfacts,numfolds=10,evalmethod="CV",CVfoldthresh=0.3,varselmethod="backward", scheme_val="one-vs-all",iter_learn=5,boostweight=NA)
{

if(is.na(boostweight)==TRUE){

boostweight=rep(0,dim(trainm)[2])
}

if(is.na(testm)==TRUE){
	testm<-trainm
}
if(is.na(testclass)==TRUE){
	testclass<-trainclass
}

data_dim<-dim(trainm)
write.csv(testclass, paste(outloc,"orig_40test.csv",sep=""), row.names=FALSE)
write.csv(trainclass, paste(outloc,"orig_60train.csv",sep=""), row.names=FALSE)

filestr3<-paste(outloc, "original_traindata.csv", sep="")
write.table(trainm, file=filestr3, sep=",", row.names=FALSE)

filestr3<-paste(outloc, "original_testdata.csv", sep="")
write.table(testm, file=filestr3, sep=",", row.names=FALSE)



print("dim of trainm is ")
print(dim(trainm))

data_dim<-dim(trainm)
print(data_dim)


nci_x<-trainm #[,-c(classindex)]
nci_y<-as.factor(trainclass)
rm(trainm)

nci_xm<-as.matrix(nci_x)
test_xm<-as.matrix(testm)

test_y<-as.factor(testclass)
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

print(dim(nci_xm))
print(dim(test_xm))
print(factcols)
print(factnames[factcols])
#trainfactnames=colnames(nci_xm)
trainfact=nci_xm[,c(factcols)]
testfact=test_xm[,c(factcols)]

trainfactnames=colnames(trainfact)
print(trainfactnames)

print("ok")
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

#trainfact_num=apply(trainfact_check[1,],2,is.na)
#testfact_num=which(testfact_check,2,is.NA)
trainfact_num={}
#print(trainfact_check[1:10,1:5])
#if(length(trainfact_check)>0)
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
#print("test class")
#print(test_y[1:4])

#print("orig train matrix")
#print(nci_xm[1:5,1:10])
#nci_xm<-as.matrix(nci_xm)
#test_xm<-as.matrix(test_xm)




if(norm_method=="minmax")
{
	print("minmax normalization")
	nci_xm=apply(nci_xm,2,function(x){ 
	minx=min(x)
	maxx=max(x)
	if(minx!=maxx)
	{
		((x-min(x))/(max(x)-min(x)))
	}
	else
        {       
                (x-min(x))/(max(x)-min(x)+1)
        }
	})
	
	test_xm=apply(test_xm,2,function(x){ 

	minx=min(x)
        maxx=max(x)
        if(minx!=maxx)
        {
                ((x-min(x))/(max(x)-min(x)))
        }
	else
	{
		(x-min(x))/(max(x)-min(x)+1)
	}
        })
        
#((x-min(x))/(max(x)-min(x)))})
}
else
{

	if(norm_method=="znorm")
	{
		print("znorm")
        	nci_xm=apply(nci_xm,2,function(x){ (x-mean(x))/(sd(x)+0.001)})
        	test_xm=apply(test_xm,2,function(x){(x-mean(x))/(sd(x)+0.001)})
	}
}

#print("norm train matrix")
#print(nci_xm[1:5,1:10])
print("mean of feat 2")
print(mean(nci_xm[,2]))
print("sd of feat 2")
print(sd(nci_xm[,2]))
#kname="radial"


###outlier chek########
#d1=nci_xm[which(nci_y=="A"),]
#d2=nci_xm[which(nci_y=="B"),]
#print("dim of train class A")
#print(dim(d1))
#print("dim of train class B")
#print(dim(d2))

#print(d1[1:10,])
#print(d2[1:10,1:10])
#m1<-mdist(d1)
#m2<-mdist(d2)
#c<-qchisq(0.99,df=10)
#print(c)
#x1<-d1[m1<c,]
#x2<-d2[m2<c,]
#print("dim of train class A after mahalanobis test")
#print(dim(x1))
#print("dim of train class B after mahalanobis test")
#print(dim(x2))

#nci_xm<-rbind(x1,x2)

#numfolds<-10
bestmod<-0
#methods=c("t.test", "lasso", "f.test", "kruskal.test", "rfe", "rf") # "elasticnet", "boosting")
#, "lasso", "elasticnet", "boosting")
#methods=c("t.test", "rfe", "rf", "kruskal.test", "f.test")

#train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn)
if(evalmethod=="MCCV"){


#train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn,ntrain=(pct_train*nrow(trainm)))

#trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=TRUE, niter=globalpso_maxitr,ntrain=numtrain),silent=TRUE)
set.seed(321)
train<-try(GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=TRUE, niter=iter_learn,ntrain=(pct_train*nrow(nci_xm))),silent=TRUE)

if(is(train,"try-error")){
    
    print("Class-wise stratification failed. Trying without class based stratification.")
    set.seed(321)
    train<-try(GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn,ntrain=(pct_train*nrow(nci_xm))),silent=TRUE)

}


}else{

set.seed(321)
#train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn)
set.seed(321)
train<-try(GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=TRUE, niter=iter_learn),silent=TRUE)

if(is(train,"try-error")){
    print("Class-wise stratification failed. Trying without class based stratification.")
    
    set.seed(321)
    train<-try(GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn),silent=TRUE)
    
}


}



#set.seed(321)
#trainlearningsets<-GenerateLearningsets(y=trainclass, method="MCCV", fold=numfolds, strat=FALSE, niter=iter_learn,ntrain=(0.8*nrow(trainm)))

scoringmatrix=matrix(0,data_dim[2]-1,length(gsmethods))

#maxnum=round(maxnum*dim(nci_xm)[2])

if(maxnum>data_dim[2]){
        maxnum=data_dim[2]
}else{

	if(maxnum<3){

		maxnum=3
	}
}
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

if(evalmethod=="MCCV"){

	max_i<-iter_learn
}else{
	max_i<-numfolds*iter_learn

}

save(list=ls(),file="cma.Rda")

print("maxnum")
print(maxnum)
print(num_classes)

#tolerance=0.5
for(m in 1:length(gsmethods))
{
	#varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme="pairwise")
	varsel <- GeneSelection(nci_xm,nci_y, learningsets = train, method=gsmethods[m],scheme=scheme_val)
	genelist={}
	
	
	for(i in 1:(max_i))
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

	if(gsmethods[m]=="lasso"){

		print(genelist_seltable)
	}

	#select genes that were selected in at least CVfoldthresh iterations
	
	#genelist_seltable<-genelist_seltable[which(genelist_seltable>=(CVfoldthresh*numfolds*iter_learn))]
	
	#if(length(genelist_good)>0)
	{
	good_genes<-names(genelist_seltable)
	
	good_genes<-as.numeric(good_genes)
	genelist<-unique(good_genes)

	#for(num_g in seq(minnum, maxnum, step))
	num_g=min(length(genelist),maxnum)
	bestmod=0
	prevacc=0
	noimprovement=0
	bestgenelist={}
	errortype="BER"
	#while((noimprovement<=totitrs) || num_g<maxnum)
	print("varselmethod")
	print(varselmethod)	
	if(varselmethod=="backward"){

	#maxitrs=100
	
	while(num_g>minnum)
	{
		trainset<-nci_xm[,genelist[1:num_g]]
		#print(nci_y)
		#print(genelist[1:num_g])
		#model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)
		#tenfoldacc<-model$tot.accuracy
		

		model<-svm_cv(v=numfolds,x=trainset,y=nci_y,kname=kname,errortype="BER")
		tenfoldacc<-model$mean_acc

		perm_acc<-{}
		seed_vec<-c(129532,839147,407700)

		for(r1 in 1:3)
		{
			seednum=seed_vec[r1]
			nci_y_perm<-nci_y[sample(1:length(nci_y),size=length(nci_y))]
			#model<-svm_cv(trainset, nci_y_perm, type="C", kernel=kname, cross=numfolds)
                	model<-svm_cv(v=numfolds,x=trainset,y=nci_y_perm,kname=kname,errortype=errortype,seednum)
                        #folderror_cur<-model$confint[1] #mean_acc
                        #folderror<-c(folderror,folderror_cur)
			permtenfoldacc<-model$mean_acc #tot.accuracy
			perm_acc<-c(perm_acc,permtenfoldacc)
		}
		
		
		perm_acc<-mean(perm_acc,na.rm=TRUE)+2*sd(perm_acc,na.rm=TRUE)
		foldacc<-tenfoldacc	
		#fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])
		
		fitfunc<-accuracyweight*(foldacc-perm_acc)+1*(foldacc)-(100*featweight*(1-num_g/dim(nci_xm)[2]))

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
		print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
		if(noimprovement<=maxitrs)
		{
			num_g=num_g-stepitr
		}
		else
		{
			#num_g=minnum
			break
		}
		
	print(paste(gsmethods[m],":",length(bestgenelist), ":", bestmod," : ",num_g,sep=""))	
	}
	}else{
		if(varselmethod=="forward"){
			num_g=minnum
			maxitrs=5	
			#maxnum=min(maxnum,dim(nci_xm)[2])

			maxnum=min(length(genelist),maxnum)	
	while(num_g<=maxnum)
        {
                trainset<-nci_xm[,genelist[1:num_g]]
                model<-svm(trainset, nci_y, type="C", kernel=kname, cross=numfolds)
                tenfoldacc<-model$tot.accuracy
                foldacc<-tenfoldacc
                fitfunc<-accuracyweight*(foldacc) #+featweight*(1-num_g/dim(nci_xm)[2])
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
                print(paste(gsmethods[m],":",num_g, ":", fitfunc,sep=""))
                if(noimprovement<=maxitrs)
                {
                        num_g=num_g+stepitr
                }
                else
                {
			break
                }

        print(paste(gsmethods[m],":",length(bestgenelist), ":", bestmod,sep=""))
        }

			
		}else{
	
		maxnum=min(length(genelist),maxnum)
			if(varselmethod=="none"){
			bestgenelist=genelist[1:maxnum]
			}else{

				stop("varselmethod should be none,forward, or backward")
			}
		}
		
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

dicesorenson_res<-get_DSindex(scoringmatrix)

print("DS index stage 1")
print(dicesorenson_res)

print("bestgenelist")

print(bestgenelist)
if(length(bestgenelist)>0)
{
	modtrain<-as.matrix(nci_xm[,c(bestgenelist)])
	modtest<-as.matrix(test_xm[,c(bestgenelist)])

	boostweight=boostweight[c(bestgenelist)]
}
else
{
	modtrain<-as.matrix(nci_xm)
	modtest<-as.matrix(test_xm)
}
traindim=dim(modtrain)
testdim=dim(modtest)
print(modtrain[1:3,])
print(modtest[1:3,])


#modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
#modtest=cbind(apply(modtest,2,as.numeric),testfact)

print(paste("numgenes selected:",length(bestgenelist),sep=""))

#print(paste("best acc:", bestmod, sep=""))
#print(paste("best method:", bestmethod, sep=""))
#modtrain<-trainm[,c(bestgenelist,data_dim[2])]
#modtest<-testm[,c(bestgenelist, data_dim[2])]
#model_train_valid<-svm(nci_xm[,bestgenelist], nci_y,   kernel=kname, type="C")
#pred_train<-predict(model_train_valid, testm[,bestgenelist])

model_train_valid<-svm(modtrain,  nci_y,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtest)
test.table<-table(pred_test, test_y)

testacc<-sum(diag(test.table))/(dim(modtest)[1])
print(paste("test acc:", testacc, sep=""))

pred_acc<-multiclass.roc(test_y,as.numeric(pred_test))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig
print(paste("test AUC acc:", auc_acc, sep=""))

model_train<-svm(modtrain,  nci_y,   kernel=kname, type="C", cross=10)
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
dicesorenson_res<-get_DSindex(scoringmatrix)
print("DS index stage 1")
print(dicesorenson_res)
dicesorenson_res<-get_KIindex(scoringmatrix,maxnum)
print("KI index stage 1")
print(dicesorenson_res)
if(dim(scoringmatrix)[2]>1){
m1<-scoringmatrix
m2<-new("list")
write.csv(scoringmatrix,file="cma_scoringmatrix.csv")
for(i in 1:dim(scoringmatrix)[2]){m2[[i]]<-paste("var",which(m1[,i]==1),sep="")}
print(m2)
m3<-as.data.frame(m2)
m4<-t(m3)
r1<-RankAggreg(m4,k=maxnum)
bestgenelistRA<-gsub(x=r1$top.list,pattern="var",replacement="")
bestgenelistRA<-as.numeric(as.character(bestgenelistRA))
modtrainRA<-as.matrix(nci_xm[,c(bestgenelistRA)])
modtestRA<-as.matrix(test_xm[,c(bestgenelistRA)])
model_train_valid<-svm(modtrainRA,  nci_y,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtestRA)
test.table<-table(pred_test, test_y)
testacc<-sum(diag(test.table))/(dim(modtestRA)[1])
print(paste("test acc rank aggreg CE:", testacc, sep=""))
pred_acc<-multiclass.roc(test_y,as.numeric(pred_test))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig
print(paste("test AUC acc rank aggreg CE:", auc_acc, sep=""))
model_trainRA<-svm(modtrainRA,  nci_y,   kernel=kname, type="C", cross=10)
print(paste("10 fold train rank aggreg res CE", model_trainRA$tot.accuracy, sep=""))
pred10foldRA<-fitted(model_trainRA)
tenfold.tableRA<-table(pred10foldRA,nci_y)
print("confusion matrix train 10 fold rank aggreg CE")
print(tenfold.tableRA)
print("Num itr RA CE")
print(r1$num.iter)
svm_table<-test.table
class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
subtestclass<-test_y
predfit<-pred_test
totacc<-length(which(pred_test==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

print("Test BER aggreg CE is")
print(beracc)


r1<-RankAggreg(m4,k=maxnum,method="GA")
bestgenelistRA<-gsub(x=r1$top.list,pattern="var",replacement="")
bestgenelistRA<-as.numeric(as.character(bestgenelistRA))
modtrainRA<-as.matrix(nci_xm[,c(bestgenelistRA)])
modtestRA<-as.matrix(test_xm[,c(bestgenelistRA)])
model_train_valid<-svm(modtrainRA,  nci_y,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtestRA)
test.table<-table(pred_test, test_y)
testacc<-sum(diag(test.table))/(dim(modtestRA)[1])
print(paste("test acc rank aggreg GA:", testacc, sep=""))
pred_acc<-multiclass.roc(test_y,as.numeric(pred_test))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig
print(paste("test AUC acc rank aggreg GA:", auc_acc, sep=""))
model_trainRA<-svm(modtrainRA,  nci_y,   kernel=kname, type="C", cross=10)
print(paste("10 fold train rank aggreg res GA", model_trainRA$tot.accuracy, sep=""))
pred10foldRA<-fitted(model_trainRA)
tenfold.tableRA<-table(pred10foldRA,nci_y)
print("confusion matrix train 10 fold rank aggreg GA")
print(tenfold.tableRA)

print("Num itr RA GA")
print(r1$num.iter)

svm_table<-test.table
class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
subtestclass<-test_y
predfit<-pred_test
totacc<-length(which(pred_test==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

print("Test BER aggreg GA is")
print(beracc)




}

#modtrain<-cbind(modtrain, as.factor(nci_y))
#modtest<-cbind(modtest, as.factor(test_y))

#estgenelist,dim(trainm)[2])]
#modtest<-test_xm[,c(bestgenelist, dim(testm)[2])]

if(length(trainfact)>1){
colnames(trainfact)=trainfactnames
colnames(testfact)=trainfactnames
modtrain=cbind(apply(modtrain,2,as.numeric),trainfact)
modtest=cbind(apply(modtest,2,as.numeric),testfact)
}
modtrain=as.data.frame(modtrain)
modtest=as.data.frame(modtest)

return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y, numfeat=dim(modtrain)[2], testacc=testacc, tenfoldacc=model_train$tot.accuracy,learningsets=train@learnmatrix,boostweight=boostweight,scoringmatrix=scoringmatrix))
#return(list(modgenelist=bestgenelist, modtraindata=modtrain, modtestdata=modtest, method=bestmethod, blindtest=testacc))
#print("Complete")

}



####function: svm_cv
svm_cv<-function(v,x,y,kname="radial",errortype="CV",conflevel=95,setseed=321){

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


###B3PSO#############



run_pso<-function(trainm,trainclass,testm,testclass,transition_matrix,outloc,
featselmethods=c("limma","rfe"),
c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=1,
global_max_itr=3,
num_part=30,
kname="radial",
errortype="BER",
weightA=0.8,
weightB=0,
weightC=0,
weightD=0.2,
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=0.45,
confusionprob=0.25,
leaderprob=0.25,
wmax=1.2,
wmin=0.4,
behavior_reset_itr=5,
maxitrreset=30,
num_neighbors=5,
minselect.pct=0.5,
evalMode="CV2",
minfitnessthresh=50,
maxnum=0.5,minnum=3,inertia_method="global",particlebehav_method="randbased",
constriction_factor=1,select.global.best=TRUE,numnodes=20,bootstrap.itr=10,evalFunc,boostweight=NA,train.pct=0.8,evalmethod="CV",...)
{

print(paste("featselmethods: ",featselmethods,sep=""))
print(paste("c1: ",c1,sep=""))
print(paste("c2: ",c2,sep=""))
print(paste("itr: ",itr,sep=""))
print(paste("globalpso_maxitr: ",globalpso_maxitr,sep=""))
print(paste("global_max_itr: ",global_max_itr,sep=""))
print(paste("num_part: ",num_part,sep=""))
print(paste("kname: ",kname,sep=""))
print(paste("errortype: ",errortype,sep=""))
print(paste("weightA: ",weightA,sep=""))
print(paste("weightB: ",weightB,sep=""))
print(paste("weightC: ",weightC,sep=""))
print(paste("weightD: ",weightD,sep=""))
print(paste("featweight.max: ",featweight.max,sep=""))
print(paste("featweight.min: ",featweight.min,sep=""))
print(paste("numfolds: ",numfolds,sep=""))
print(paste("followerprob: ",followerprob,sep=""))
print(paste("confusionprob: ",confusionprob,sep=""))
print(paste("leaderprob: ",leaderprob,sep=""))
print(paste("wmax: ",wmax,sep=""))
print(paste("wmin: ",wmin,sep=""))
print(paste("behavior_reset_itr: ",behavior_reset_itr,sep=""))
print(paste("maxitrreset: ",maxitrreset,sep=""))
print(paste("num_neighbors: ",num_neighbors,sep=""))
print(paste("minselect.pct: ",minselect.pct,sep=""))
print(paste("minfitnessthresh: ",minfitnessthresh,sep=""))
print(paste("maxnum: ",maxnum,sep=""))
print(paste("minnum: ",minnum,sep=""))
print(paste("inertia_method: ",inertia_method,sep=""))
print(paste("particlebehav_method: ",particlebehav_method,sep=""))
print(paste("constriction_factor: ",constriction_factor,sep=""))
print(paste("select.global.best: ",select.global.best,sep=""))
print(paste("train.pct: ",train.pct,sep=""))


check_file<-try(load("res132.Rda"),silent=TRUE)
if(is(check_file,"try-error")){
    
    #evalmethod='MCCV'
    if(is.na(featselmethods)==FALSE){
    
    CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
    maxnum=maxnum,
    minnum=minnum,
    stepitr=1,
    gsmethods=c(featselmethods), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
    pct_train=train.pct,
    accuracyweight=1,
    featweight=0.06,
    minpresent=1,
    kname="radial",
    norm_method="none",
    tolerance=0.1,
    maxitrs=5,
    classindex=1,
    numfacts=0,
    evalmethod="CV",
    numfolds=numfolds,
    CVfoldthresh=0.7,
    varselmethod="none",
    scheme_val="one-vs-all",
    iter_learn=globalpso_maxitr,boostweight=rep(0,dim(trainm)[2]))
    
    trainm<-CMAres$modtraindata
    testm<-CMAres$modtestdata
    trainclass<-CMAres$modtrainclass
    testclass<-CMAres$modtestclass
    learningsets<-CMAres$learningsets
    
    }

trainm<-as.data.frame(trainm)



if(is.na(testm)==TRUE){
	testm<-trainm
}
if(is.na(testclass)==TRUE){
	testclass<-trainclass
}

parentevalMode<-evalMode

scoringmatrix=matrix(0,dim(trainm)[2],globalpso_maxitr)

max_num_feats=ceiling(maxnum*dim(trainm)[2])

overall_gbest=array(0, dim=c(d_dim[2]))


trainm<-as.data.frame(trainm)
testm<-as.data.frame(testm)

mod<-svm(x=trainm,y=trainclass,type="C", cross=10)

print("train 10 fold")
print(mod$tot.accuracy)

testmim=dim(testm)[2]


alltrainm<-trainm
alltrainclass<-trainclass

evalMode<-parentevalMode 
	 d_dim<-dim(trainm)
	 d<-dim(trainm)[2]
	

	scoringmatrix<-b3pso(outloc=outloc,dimsize=dim(trainm)[2],
transition_matrix=transition_matrix,c1=c1,
c2=c2,
globalpso_maxitr=globalpso_maxitr,
global_max_itr=global_max_itr,
num_part=num_part,
kname=kname,
errortype=errortype,
weightA=weightA,
weightB=weightB,
weightC=weightC,
weightD=weightD,
featweight.max=featweight.max,
featweight.min=featweight.min,
numfolds=numfolds,
followerprob=followerprob,
confusionprob=confusionprob,
leaderprob=leaderprob,
wmax=wmax,
wmin=wmin,
behavior_reset_itr=behavior_reset_itr,
maxitrreset=maxitrreset,
num_neighbors=num_neighbors,
minselect.pct=minselect.pct,
evalMode=evalMode,
minfitnessthresh=minfitnessthresh,
maxnum=maxnum,minnum=minnum,inertia_method=inertia_method,particlebehav_method=particlebehav_method,
constriction_factor=constriction_factor,select.global.best=select.global.best,numnodes=numnodes,bootstrap.itr=bootstrap.itr,evalFunc=evalFunc,trainm=trainm,trainclass=trainclass,
boostweight=boostweight,train.pct=train.pct,...)


		testacc<-scoringmatrix$acc
		print("testacc")
		print(testacc)
		print(mean(testacc))
		print(summary(testacc))

		print(sd(testacc))
		scoringmatrix<-scoringmatrix$scoringmatrix

		trainm<-alltrainm	
		trainclass<-alltrainclass
		
		print(scoringmatrix)
		summat=apply(scoringmatrix,1,sum)
		print("dim of scoring matrix is ")
		print(dim(scoringmatrix))
	
		dicesorenson_res<-get_DSindex(scoringmatrix)

setwd(outloc)
#save(list=ls(),file="res.Rda")

}else{

	load("res.Rda")
}

print("DS index stage 2")
print(dicesorenson_res)


k=dim(scoringmatrix)[1]
ki_res<-get_KIindex(scoringmatrix,k)

print("KI index stage 2")
print(ki_res)
		print(length(summary))

		print(summary(summat))
		               s1<-summary(summat)
                num_itr_thresh<-s1[3] #ceiling(minselect.pct*globalpso_maxitr)

                pct_range<-seq(1,globalpso_maxitr)
                min_fitness_res<-(100000000000)
                
		num_itr_thresh<-max(s1)
		best_thresh<-1
                best_results<-seq(1,length(summat))

		for(num_itr_thresh in pct_range)
		{

                bestgenelist=which(summat>=num_itr_thresh)  #ceiling(minselect.pct*globalpso_maxitr))

                print(paste("Number of features selected in ",num_itr_thresh," iterations:", sep=""))
                print(length(bestgenelist))

                x<-rep(0,length(summat))

                featweightcur<-0.0
                x[bestgenelist]<-1
                
                #fitness_res<- eval_fit_test_diff(particle=x,numfolds=numfolds,trainm=trainm,trainclass=trainclass,
                #                      testm=trainm,testclass=trainclass,errortype=errortype,kname=kname,accuracyweightA=weightA,
                #accuracyweightB=weightB,accuracyweightC=weightC,accuracyweightD=weightD,featweight=featweightcur,max_num_feats=maxnum)
                                                
                                                fitness_res<-eval_fit_test_diff(particle=x,numfolds=numfolds,trainm=trainm,trainclass=trainclass,
                                                testm=trainm,testclass=trainclass,errortype=errortype,kname=kname,accuracyweightA=0.25,
                                                accuracyweightB=0.25,accuracyweightC=0.25,accuracyweightD=0.25,featweight=featweightcur,max_num_feats=maxnum)
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

                summary_testacc<-summary(testacc,na.rm=TRUE)
                print(summary_testacc)


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
		
	filestr2<-paste(outloc,  "selected_feature_index_final.csv", sep="")
	write.table(feat_col, file=filestr2, sep=",", row.names=FALSE)

		trainmata<-trainm
		testmata<-testm
		rm(trainm)
		rm(testm)
	 
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



#	model_train_valid<-svm(modtrain$Class~., data=modtrain, type="C")

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


#testy<-testy[order(testy)]
#pred<-pred[order(testy)]


if(dim(trainm)[2]>3)
{
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
filestr3<-paste(outloc, "testaccuracy.csv", sep="")
        write.table(testacc, file=filestr3, sep=",", row.names=FALSE)
pred_acc<-multiclass.roc(testm$Class,as.numeric(pred))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig

print("Test AUC:")
print(auc_acc)
 filestr3<-paste(outloc, "testAUC.csv", sep="")
 write.table(auc_acc, file=filestr3, sep=",", row.names=FALSE)

pred<-predict(mod,trainm[,-c(trainmim)])
train.table=table(pred,trainm$Class)

trainacc<-(sum(diag(train.table))/(dim(trainm)[1]))
print("Train acc is ")
print(trainacc)
filestr3<-paste(outloc, "trainaccuracy.csv", sep="")
write.table(trainacc, file=filestr3, sep=",", row.names=FALSE)





}


print("# of features after CMA:")
print(dim(CMAres$modtrainmata))
print("# of features after PSO:")
print(dim(trainm))

return(list(scoringmatrix=scoringmatrix,bestfeatlist=bestgenelist,bestfeatnames=feat_names,model=mod,trainm=trainm,testm=testm,outercv=testacc,trainacc=trainacc,testacc=testacc,testauc=auc_acc))

}




#function to evaluate k-fold CV
	evalFunc_multiobj<-function(particle)
	{
		
		trainm=X
		trainclass=Y
		if(is.na(seednum)==TRUE){
			seednum=runif(n=1,min=1,max=999999)
		}
		if(evalMode=="CV1" || evalMode=="CV2")
		{

numtrain<-(0.8*nrow(trainm))
evalmethod='MCCV'
set.seed(seednum)
trainlearningsets<-GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=FALSE, niter=numfolds,ntrain=numtrain)
trainlearningsets<-trainlearningsets@learnmatrix
globalpso_maxitr=dim(trainlearningsets)[1]


}else{
	
		trainlearningsets<-seq(1,nrow(trainm))
		
		trainlearningsets<-as.matrix(t(trainlearningsets))
}
		errortype="AUC"
		kname="radial"
		featweight=0.05
		accuracyweightA=5
		accuracyweightB=1
		max_num_feats=10
		num_feat<-0
		alltrainm<-trainm
		alltrainclass<-as.numeric(as.factor(trainclass))
		
		overall_fitness<-{}
		for(pitr in 1:nrow(trainlearningsets)){
		
		trainm<-alltrainm
		trainclass<-alltrainclass
				 if(evalMode=="CV1")
	{
	
 
	trainm<-alltrainm[-c(trainlearningsets[pitr,]),]
	 trainclass<-alltrainclass[-c(trainlearningsets[pitr,])]
	subtest<-alltrainm[c(trainlearningsets[pitr,]),]
	subtestclass<-alltrainclass[c(trainlearningsets[pitr,])]

	
	
	#evalMode<-"bootstrap"
		set.seed(seednum)
		subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=10*dim(trainm)[1],replace=TRUE)
                                trainm<-trainm[subtrain_ind,]
                               trainclass<-trainclass[subtrain_ind]	
	}else{

		if(evalMode=="CV2"){
       
			
     
	 trainm<-alltrainm[trainlearningsets[pitr,],]
         trainclass<-alltrainclass[trainlearningsets[pitr,]]
        subtest<-alltrainm[-c(trainlearningsets[pitr,]),]
        subtestclass<-alltrainclass[-c(trainlearningsets[pitr,])]
        }else{
		
			 subtest<-alltrainm
			subtestclass<-alltrainclass
		}

	}
		#select the columns with selected features 
		ind<-which(particle==1)

		folderror_perm<-{}

		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		#print("itr num feat:")
		#print(length(ind))	
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			
			#print(trainset[1:4,])
			#trainset<-traindata[,c(col_sel)]
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			folderror<-{}
			#if(FALSE)
			
			folderrorkfold<-{}
			folderror_perm<-{}
			seed_vec<-c(129532,839147,407700)
			{
			 for(f in 1:3)
                        {
			 #model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="AUC")
	
			seednum=seed_vec[f]	
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,seednum)
			
			
			# folderror_cur<-model$confint[1]
			
			folderror_cur<-model$mean_acc[1]
			
			
			folderrorkfold<-c(folderrorkfold,folderror_cur) #model$tot.accuracy)
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,seednum)
			#if(numfolds>5){
                        #folderror_cur_perm<-model$confint[2]
			#}else{
			 folderror_cur_perm<-model$mean_acc[1]
			#}
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
			
                       } 

		testset<-subtest[,c(col_sel)]	
		mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testset)
svm_table<-table(predfit,subtestclass)

class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

if(errortype=="CV"){
        svm_acc[i]<-(totacc*100)
}else{
if(errortype=="AUC"){
        pred_acc<-multiclass.roc(subtestclass,as.numeric(predfit))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig


        svm_acc[i]<-(auc_acc*100)
}else{
svm_acc[i]<-(beracc*100)
}
}

		#	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
		folderror<-svm_acc[i]
		
	#	print(folderrorkfold)
	#	print(folderror_perm)
	#	print(num_feat)
	#	print(folderror)

				folderrorkfold<-mean(folderrorkfold) #-(1.96*(sd(folderrorkfold,na.rm=TRUE)/sqrt(2)))
                                folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                if(num_feat>max_num_feats){
                                fitfunc<-(accuracyweightA*(folderrorkfold-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderrorkfold-folderror_perm))+accuracyweightB*(folderror)
                                }
					
	
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		 folderror<-1
                folderror_perm<-100
                                fitfunc<-(-100)
		}
		#fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))  #-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		
		rm(col_sel)
		rm(num_feat)
		#return(fitfunc)
		
		fitfunc<-(-1)*fitfunc
		#return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
		overall_fitness<-c(overall_fitness,fitfunc)
	}
	overall_fitness<-mean(overall_fitness)
	print(paste("Fitness: ",overall_fitness,sep=""))
	return(overall_fitness)
}

 #numfolds, trainm, trainclass, testm, testclass,errortype = "AUC", kname = "radial", featweight = 0.05, accuracyweightA = 5, accuracyweightB = 1, max_num_feats = 10)
 
evalFunc_multiobjPSO<-function(particle,X,Y,numfolds=10,errortype = "AUC", kname = "radial", featweight = 0.05, accuracyweightA = 5, 
accuracyweightB = 1, max_num_feats = 10,seednum=NA,evalMode="CV1")
	{
		
		trainm=X
		trainclass=Y
		if(is.na(seednum)==TRUE){
			seednum=runif(n=1,min=1,max=999999)
		}
		if(evalMode=="CV1" || evalMode=="CV2")
		{

numtrain<-(0.8*nrow(trainm))
evalmethod='MCCV'
set.seed(seednum)
trainlearningsets<-GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=FALSE, niter=numfolds,ntrain=numtrain)
trainlearningsets<-trainlearningsets@learnmatrix
globalpso_maxitr=dim(trainlearningsets)[1]


}else{
	
		trainlearningsets<-seq(1,nrow(trainm))
		
		trainlearningsets<-as.matrix(t(trainlearningsets))
}
		errortype="AUC"
		kname="radial"
		featweight=0.05
		accuracyweightA=5
		accuracyweightB=1
		max_num_feats=10
		num_feat<-0
		alltrainm<-trainm
		if(is.factor(trainclass)){
		alltrainclass<-as.numeric(as.factor(trainclass))
		}else{
		alltrainclass<-as.numeric(trainclass)
		}
		overall_fitness<-{}
		for(pitr in 1:nrow(trainlearningsets)){
		
		trainm<-alltrainm
		trainclass<-alltrainclass
				 if(evalMode=="CV1")
	{
	
 
	trainm<-alltrainm[-c(trainlearningsets[pitr,]),]
	 trainclass<-alltrainclass[-c(trainlearningsets[pitr,])]
	subtest<-alltrainm[c(trainlearningsets[pitr,]),]
	subtestclass<-alltrainclass[c(trainlearningsets[pitr,])]

	
	
	
	#evalMode<-"bootstrap"
		set.seed(seednum)
		subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=10*dim(trainm)[1],replace=TRUE)
                                trainm<-trainm[subtrain_ind,]
                               trainclass<-trainclass[subtrain_ind]	
	}else{

		if(evalMode=="CV2"){
       
			
     
	 trainm<-alltrainm[trainlearningsets[pitr,],]
         trainclass<-alltrainclass[trainlearningsets[pitr,]]
        subtest<-alltrainm[-c(trainlearningsets[pitr,]),]
        subtestclass<-alltrainclass[-c(trainlearningsets[pitr,])]
        }else{
		
			 subtest<-alltrainm
			subtestclass<-alltrainclass
		}

	}
		#select the columns with selected features 
		ind<-which(particle==1)

		folderror_perm<-{}

		#need to add 1 to take into account the index of the feature in the original dataset
		col_sel<-ind
		num_feat<-length(col_sel)
		#print("itr num feat:")
		#print(length(ind))	
		if(num_feat>1)
		{
			trainset<-trainm[,c(col_sel)]
			trainset<-cbind(trainclass,trainset)
			
			trainset<-data.frame(trainset)
			
			#print(trainset[1:4,])
			#trainset<-traindata[,c(col_sel)]
			
			#model<-svm(trainset, trainclass, type="C", kernel=kname, cross=kcross)

			#model<-svm(trainset, trainclass, type="C", kernel=kname, degree=tune_degree, gamma=tune_gamma, cost=tune_cost, cross=kcross)
			folderror<-{}
			#if(FALSE)
			
			folderrorkfold<-{}
			folderror_perm<-{}
			seed_vec<-c(129532,839147,407700)
			{
			 for(f in 1:3)
                        {
			 #model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype="AUC")
			seednum=seed_vec[f]	
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass,kname=kname,errortype=errortype,seednum)
			
			
			# folderror_cur<-model$confint[1]
			
			folderror_cur<-model$mean_acc[1]
			
			
			folderrorkfold<-c(folderrorkfold,folderror_cur) #model$tot.accuracy)
			set.seed(seed_vec[f])
			rand_ind<-sample(x=seq(1,length(trainset$trainclass)),size=length(trainset$trainclass))
                        seednum=seed_vec[f]
			model<-svm_cv(v=numfolds,x=trainset[,-1],y=trainset$trainclass[rand_ind],kname=kname,errortype=errortype,seednum)
			#if(numfolds>5){
                        #folderror_cur_perm<-model$confint[2]
			#}else{
			 folderror_cur_perm<-model$mean_acc[1]
			#}
                        folderror_perm<-c(folderror_perm,folderror_cur_perm)
                                                rm(model)
                        }
			
                       } 

		testset<-subtest[,c(col_sel)]	
		mod_cv <- svm(x=trainset[,-1],y=trainset$trainclass, type="C",kernel=kname)
predfit<-predict(mod_cv,testset)
svm_table<-table(predfit,subtestclass)

class_names<-rownames(svm_table)
beracc<-{}
i<-1
svm_acc<-{}
totacc<-length(which(predfit==subtestclass))/length(subtestclass)
for(c in 1:dim(svm_table)[1]){
subtestclass_ind<-which(subtestclass==class_names[c])
beracc<-c(beracc,length(which(predfit[subtestclass_ind]==subtestclass[subtestclass_ind]))/length(subtestclass_ind))
}

beracc<-as.numeric(beracc)

beracc<-mean(beracc,na.rm=TRUE)

if(errortype=="CV"){
        svm_acc[i]<-(totacc*100)
}else{
if(errortype=="AUC"){
        pred_acc<-multiclass.roc(subtestclass,as.numeric(predfit))
        pred_acc_orig<-pred_acc$auc[1]
        auc_acc<-pred_acc_orig


        svm_acc[i]<-(auc_acc*100)
}else{
svm_acc[i]<-(beracc*100)
}
}

		#	folderror<-mean(folderror)-(1.96*(sd(folderror)/sqrt(3)))
		folderror<-svm_acc[i]
		
	#	print(folderrorkfold)
	#	print(folderror_perm)
	#	print(num_feat)
	#	print(folderror)

				folderrorkfold<-mean(folderrorkfold) #-(1.96*(sd(folderrorkfold,na.rm=TRUE)/sqrt(2)))
                                folderror_perm<-mean(folderror_perm,na.rm=TRUE) #+(1.96*(sd(folderror_perm,na.rm=TRUE)/sqrt(2)))
                                if(num_feat>max_num_feats){
                                fitfunc<-(accuracyweightA*(folderrorkfold-folderror_perm))+accuracyweightB*(folderror)-(featweight*100*(num_feat/length(particle))) #(featweight*(num_feat))
                                }else{
                                        fitfunc<-(accuracyweightA*(folderrorkfold-folderror_perm))+accuracyweightB*(folderror)
                                }
					
	
			#folderror<-model$tot.accuracy
			#folderror<-(1-ber)*100
			rm(trainset)

		}
		else
		{
		folderror<-1
		 folderror<-1
                folderror_perm<-100
                                fitfunc<-(-100)
		}
		#fitfunc<-(accuracyweight*(folderror))+(featweight*(1-num_feat))  #-(featweight*100*(num_feat/length(particle)))  #+(featweight*(1-num_feat))
		
		rm(col_sel)
		rm(num_feat)
		#return(fitfunc)
		
		fitfunc<-(-1)*fitfunc
		#return(list("fitfunc"=fitfunc,"cverror"=folderror,"cvpermerror"=folderror_perm))
		overall_fitness<-c(overall_fitness,fitfunc)
	}
	overall_fitness<-mean(overall_fitness)
	print(paste("Fitness: ",overall_fitness,sep=""))
	return(overall_fitness)
}

b3pso<-function(outloc,dimsize,
transition_matrix,c1=2.05,
c2=2.05,
itr=10,
globalpso_maxitr=1,
global_max_itr=3,
num_part=30,
kname="radial",
errortype="BER",
weightA=0.8,
weightB=0,
weightC=0,
weightD=0.2,
featweight.max=0.01,
featweight.min=0.01,
numfolds=10,
followerprob=0.45,
confusionprob=0.25,
leaderprob=0.25,
wmax=1.2,
wmin=0.4,
behavior_reset_itr=5,
maxitrreset=30,
num_neighbors=5,
minselect.pct=0.8,
evalMode="CV1",
minfitnessthresh=50,
maxnum=0.5,minnum=3,inertia_method="rankbased",particlebehav_method="rankbased",
constriction_factor=1,select.global.best=TRUE,numnodes=20,bootstrap.itr=10,seednum=NA,varnames=NA, itr.terminate=TRUE,evalFunc,trainm=NA,trainclass=NA,boostweight=NA,train.pct=0.8,evalmethod='MCCV',...)
{

if(is.na(boostweight)==TRUE){

boostweight=rep(0,dim(trainm)[2])
}

parentevalMode<-evalMode
testacc_all<-{}
print("here")

		if(evalMode=="CV1" || evalMode=="CV2")
{

numtrain<-(train.pct*nrow(trainm))
#evalmethod='MCCV'

print("s")
print(numtrain)
print(dim(trainm))
print(numfolds)

if(evalmethod=="MCCV"){
    
    set.seed(321)
    #train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn,ntrain=(pct_train*nrow(trainm)))
    trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=TRUE, niter=globalpso_maxitr,ntrain=numtrain),silent=TRUE)
    
    if(is(trainlearningsets,"try-error")){
        set.seed(321)
         trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=FALSE, niter=globalpso_maxitr,ntrain=numtrain),silent=TRUE)
    }
    
}else{
    
    set.seed(321)
    #train<-GenerateLearningsets(y=nci_y, method=evalmethod, fold=numfolds, strat=FALSE, niter=iter_learn)
    trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=TRUE, niter=globalpso_maxitr),silent=TRUE)
    
    if(is(trainlearningsets,"try-error")){
     set.seed(321)
     trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=numfolds, strat=FALSE, niter=globalpso_maxitr),silent=TRUE)
     
    }
    
}

set.seed(321)
#trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=globalpso_maxitr, strat=TRUE, niter=globalpso_maxitr,ntrain=numtrain),silent=TRUE)
#if(is(trainlearningsets,"try-error")){
    #trainlearningsets<-try(GenerateLearningsets(y=trainclass, method=evalmethod, fold=globalpso_maxitr, strat=FALSE, niter=globalpso_maxitr,ntrain=numtrain),silent=TRUE)

#}
trainlearningsets<-trainlearningsets@learnmatrix
globalpso_maxitr=dim(trainlearningsets)[1]

evalFunc=eval_fit_test_diff
}
if(evalMode!="custom"){
alltrainm<-trainm
alltrainclass<-trainclass
}

  scoringmatrix=matrix(0,dimsize,globalpso_maxitr)

	for (globalpso_itr in 1:globalpso_maxitr)
	{		
	 if(evalMode=="CV1")
	{
	print(paste("learning sets: ",globalpso_itr,sep=""))
	print(trainlearningsets[globalpso_itr,])
 
	trainm<-alltrainm[-c(trainlearningsets[globalpso_itr,]),]
	 trainclass<-alltrainclass[-c(trainlearningsets[globalpso_itr,])]
	subtest<-alltrainm[c(trainlearningsets[globalpso_itr,]),]
	subtestclass<-alltrainclass[c(trainlearningsets[globalpso_itr,])]

	print(dim(trainm))
	print(dim(subtest))
	
	#evalMode<-"bootstrap"
		set.seed(321)
		subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=10*dim(trainm)[1],replace=TRUE)
                                trainm<-trainm[subtrain_ind,]
                               trainclass<-trainclass[subtrain_ind]	
	}else{

		if(evalMode=="CV2"){
       
			print(paste("learning sets: ",globalpso_itr,sep=""))
        print(trainlearningsets[globalpso_itr,])
	 trainm<-alltrainm[trainlearningsets[globalpso_itr,],]
         trainclass<-alltrainclass[trainlearningsets[globalpso_itr,]]
        subtest<-alltrainm[-c(trainlearningsets[globalpso_itr,]),]
        subtestclass<-alltrainclass[-c(trainlearningsets[globalpso_itr,])]
        }else{

			subtest<-alltrainm
                        subtestclass<-alltrainclass
	}

	}
	
	 print(paste("Starting global iteration number : ",globalpso_itr,sep=""))
	 ll<-0

	 ul<-1

	bad_pos<-new("list")
        num_obstacles<-1

	fitness_x<-array(100000000000000000, dim=c(num_part, 1)) 
	 fitness_gbest<-100000000000000 
	cverror_gbest<-(-10000)
	cvpermerror_gbest<-(-10000)
	prev_gbest<-fitness_gbest+1 
	 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
	 if(is.na(varnames)==TRUE){
		feat_names<-paste("var",seq(1,dimsize),sep="")
	 }else{	
	 feat_names<-varnames #colnames(trainm)
		}
		feat_list<-feat_names

		global_no_change<-0
		no_change<-0
		
		global_best_index<-10000000000000000000
		
		min_fit_x<-1000000000000000

		min_fit_index<-100000000000000000
	
	#position
	 x<-array(0, dim=c(num_part,dimsize))

	#velocity
	v<-array(0, dim=c(num_part,dimsize))
	
	x_gbest<-array(0, dim=c(dimsize))

	p_gbest<-array(0, dim=c(dimsize))
	   x_lbest<-array(0, dim=c(num_part, dimsize))
	   count_feat<-array(0,dim=c(num_part))

#confused
a<-c(0.7,0.05,0.05,0.2)

#neighbor
b<-c(0.2,0.6,0.05,0.15)

#leader
c<-c(0.1,0.2,0.6,0.1)

#self
d<-c(0.25,0.1,0.05,0.6)

initial_state_prob_matrix<-rbind(a,b,c,d)

set.seed(321)
#randomly assign behavior
agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))


prob_behavior_particles<-array(0, dim=c(num_part, dim(transition_matrix)[2]))
	for (row in 1:num_part)
	 {
		num_feat<-0
		
		#prob_behavior_particles[row,]<-c(0.4,0.5,0.1,0)	
	
		prob_behavior_particles[row,]<-initial_state_prob_matrix[agent_behavior[row],]		
		
		ran<-runif(dimsize,0,1)	
		for (col in 1:dimsize)		
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
	 
	 num_featl<-dimsize
	 num_featg<-dimsize

	overall_x_gbest<-x[1,]
	
	cl<-makeCluster(numnodes)
	

	#clusterExport(cl, "evalFunc")
	
	#clusterExport(cl, library(pROC))
	clusterEvalQ<-function(cl, expr)
	clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

	#load a library on all clusters
	clusterEvalQ(cl, library(e1071))
	
	clusterEvalQ(cl, library(MASS))
	clusterEvalQ(cl, library(CMA))
	clusterCall(cl, function() library(pROC))
 	
	
	itr_data={}
        k<-0



       	rank_vec<-seq(1,num_part)


		repeat
		{
			k<-k+1
			feat_sel<-0
		
			itr_val={}	
			print("iteration number: ") 
			print(k)

			min_fit_x<-1000000000000000
			 
			min_fit_index<-100000000000000000 
			part_list=new("list") #{}
			rank_sum_vec<-summary(rank_vec)


			

			
			for (i in 1:num_part)
			{
				#each element of the list represents a particle
				#part_list=c(part_list, c=list(sapply(x[i,],head)))
				part_list[[i]]<-x[i,]				

				if(k%%behavior_reset_itr==0)
				{
					#agent_behavior=runif(num_part,0,1)
					#agent_behavior<-sample(x=seq(1,dim(transition_matrix)[1]),size=num_part,replace=TRUE)
		#set.seed(321)          
    agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))
 
			
				#if(FALSE)
				if(particlebehav_method=="rankbased")
				{	
					prob_behavior_particles[i,]<-initial_state_prob_matrix[agent_behavior[i],]
					if(rank_vec[i]>=rank_sum_vec[5]){

						#prob_behavior_particles[i,]<-c(0.1,0.4,0.5,0)
					
                                        prob_actions<-prob_behavior_particles[i,] %*% (transition_matrix %^% k)

                                        prob_actions<-as.vector(prob_actions)
                                        sum_actions<-summary(c(prob_actions))

                                        #best_action<-which(prob_actions==max(prob_actions)[1])

                                        best_actions<-which(prob_actions>=sum_actions[3])

                                                                             best_action<-sample(x=best_actions,size=1)
					}else
					{

						if(rank_vec[i]>=rank_sum_vec[3]){

                                                	#prob_behavior_particles[i,]<-c(0.333,0.333,0.333,0)
                                       
                                        prob_actions<-prob_behavior_particles[i,] %*% (transition_matrix %^% k)

                                        prob_actions<-as.vector(prob_actions)
                                        sum_actions<-summary(c(prob_actions))

                                        #best_action<-which(prob_actions==max(prob_actions)[1])

                                        best_actions<-which(prob_actions>=sum_actions[3])

                                                                             best_action<-sample(x=best_actions,size=1)

					 		}else{

								#prob_behavior_particles[i,]<-c(0.4,0.1,0,0.5)
								
								best_action<-sample(seq(1,4),size=1)
							}

					}
					 #prob_behavior_particles[i,]<-runif(4,0,1)
					
										agent_behavior[i]<-best_action
	
				}
		

				}
						
				
				}
		 	
	
				#res1=clusterCall(cl, (-1)*eval_fit_kfold(x[i,], 10,1,0.01))
				
				#res1=clusterCall(cl, function(y) eval_fit_kfold(x[y,],10), 1)
				#res1=parLapply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff)
				
				#correct method
				#res1<-clusterApply(cl,list(a=as.data.frame(x[1,]),b=as.data.frame(x[5,1:5])),eval_fit_kfold_diff))
			
				
				boot_fitness<-new("list") #{}	
				boot_cv<-new("list")
				boot_cvperm<-new("list")
				#featweightcur<-featweight.max-(((featweight.max-featweight.min)/itr)*k)
			
	if(featweight.min==featweight.max){
		featweightcur=featweight.min
	}else{	
	featweightcur<-featweight.min+(((featweight.max-featweight.min)/k))				
	}
				if(featweightcur<featweight.min){featweightcur<-featweight.min}
				#featweightcur<-(1/k)*(featweight.max-featweight.min)

		fitness_x<-{}
		cverror<-{}
		cvpermerror<-{}
		
		
		if(evalMode=="bootstrap")
			{
			
				
					clusterExport(cl, "svm_cv")
				all_ind<-seq(1,dim(trainm)[1])
				for(boot_itr in 1:bootstrap.itr)
				{
				
				subtrain_ind<-sample(x=seq(1,dim(trainm)[1]),size=dim(alltrainm)[1],replace=TRUE)
				subtrain<-trainm[subtrain_ind,]
				subtrainclass<-trainclass[subtrain_ind]

				subtrain_ind<-unique(subtrain_ind)
				subtest_ind<-all_ind[-subtrain_ind]
				subtest<-trainm[subtest_ind,]
				subtestclass<-trainclass[subtest_ind]

				
				fitness_x<-{}
				cverror<-{}
				cvpermerror<-{}
				
				evalFunc=eval_fit_test_diff
		
#				res1<-clusterApply(cl,part_list,evalFunc,numfolds=numfolds,trainm=subtrain,trainclass=subtrainclass,
#						testm=subtest,testclass=subtestclass,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
#						accuracyweightB=accuracyweightB,featweight=featweightcur,maxnum=maxnum)

			res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,trainm=subtrain,trainclass=subtrainclass,
                                                testm=subtest,testclass=subtestclass,errortype=errortype,kname=kname,accuracyweightA=weightA,
                                                accuracyweightB=weightB,accuracyweightC=weightC,accuracyweightD=weightD,featweight=featweightcur,max_num_feats=maxnum)


				 for(np in 1:num_part)
				 {
                                       

                                        fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
                                        cverror<-c(cverror,res1[[np]]$cverror)
                                        cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)

                                        }

				#res1<-clusterApply(cl,part_list,evalFunc,numfolds=numfolds,errortype="BER")
				#fitness_x<-sapply(res1,head)
				boot_fitness[[boot_itr]]<-fitness_x #c(boot_fitness[[bool_itr]],fitness_x)
				boot_cv[[boot_itr]]<-cverror
				boot_cvperm[[boot_itr]]<-cvpermerror

				}
				

	
				boot_fitness<-as.data.frame(boot_fitness)
			
				
				boot_fitness<-apply(boot_fitness,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(bootstrap.itr))}) #mean(boot_fitness,na.rm=TRUE)
				fitness_x<-(1)*boot_fitness #fitness_x
			
				cverror<-as.data.frame(boot_cv)

                                cverror<-apply(cverror,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(bootstrap.itr))}) #mean(boot_fitness,na.rm=TRUE)

				 cvpermerror<-as.data.frame(boot_cvperm)

                                cvpermerror<-apply(cvpermerror,1,function(x){mean(x,na.rm=TRUE)+1.96*(sd(x,na.rm=TRUE)/sqrt(bootstrap.itr))}) #mean(boot_fitness,na.rm=TRUE)


					
			}	
				else
				{
					if(evalMode=="CV1" || evalMode=="CV2")
					{
							clusterExport(cl, "svm_cv")
						fitness_x<-{}
                                cverror<-{}
                                cvpermerror<-{}
                               
			 
					evalFunc=eval_fit_test_diff
				
					#validation set
                                res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,trainm=trainm,trainclass=trainclass,
                                                testm=subtest,testclass=subtestclass,errortype=errortype,kname=kname,accuracyweightA=weightA,
						accuracyweightB=weightB,accuracyweightC=weightC,accuracyweightD=weightD,featweight=featweightcur,max_num_feats=maxnum)
						
                        save(res1,file="res1.Rda")
				 #  res1<-clusterApply(cl,part_list,eval_fit_test_diff,numfolds=numfolds,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
				#		accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum)			
					
				#res1<-clusterApply(cl,part_list,evalFunc_multiobjPSO, X=trainm, Y=trainclass, numfolds=numfolds,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
				#		accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum, seednum = seednum,evalMode=evalMode)



                                 for(np in 1:num_part){
                                        #fitness_x<-sapply(res1,head)
                                        
					
					fitness_x<-(1)*fitness_x

                                        fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
                                        cverror<-c(cverror,res1[[np]]$cverror)
                                        cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)

					if(cverror<cvpermerror){

                                                bad_pos[[num_obstacles]]<-x[np,]
                                                num_obstacles<-num_obstacles+1

                                        }
	
                                        }

                                #res1<-clusterApply(cl,part_list,evalFunc,numfolds=numfolds,errortype="BER")
                                #fitness_x<-sapply(res1,head)

                                }else{

					if(evalMode=="CV"){
					#kfold 	
					clusterExport(cl, "svm_cv")
					evalFunc=eval_fit_kfold_diff
					#print(dim(trainm))
				res1<-clusterApply(cl,part_list,eval_fit_kfold_diff,trainm=trainm,trainclass=trainclass,numfolds=numfolds,
				errortype=errortype,accuracyweightA=accuracyweightA,accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum,kname=kname)
				#res1<-clusterApply(cl,part_list,evalFunc_multiobjPSO, X=trainm, Y=trainclass, numfolds=numfolds,errortype=errortype,kname=kname,accuracyweightA=accuracyweightA,
				#		accuracyweightB=accuracyweightB,featweight=featweightcur,max_num_feats=maxnum, seednum = seednum,evalMode=evalMode)

					for(np in 1:num_part)
					{
			        	
					
					fitness_x<-c(fitness_x,res1[[np]]$fitfunc)
					cverror<-c(cverror,res1[[np]]$cverror)
					cvpermerror<-c(cvpermerror,res1[[np]]$cvpermerror)
					
					if(cverror<cvpermerror){

						bad_pos[[num_obstacles]]<-x[np,]
						num_obstacles<-num_obstacles+1
						
					}
						
					}
					
					fitness_x<-(1)*fitness_x
					}else{
					
					
						
						res1<-clusterApply(cl,part_list,evalFunc,...)
						for(np in 1:num_part)
						{
						
						
						fitness_x<-c(fitness_x,res1[[np]])
						
						
					
							
						}
						fitness_x<-(1)*fitness_x
					}
					
					
					
					}
					
					
				}


				
			
				med_fit_x<-(1)*median(fitness_x)	
				min_fit_x<-min(fitness_x)
				min_fit_index<-which(fitness_x==min_fit_x)
				
				bestind<-runif(1,1,length(which(fitness_x==min_fit_x)))
				bestind<-min_fit_index[bestind]
				#print(bestind)
				numfeatl<-length(which(x[bestind[1],]==1))
			
				nfeats_perpart<-{}
				for (i in 1:num_part)
				{

					nfeats_perpart<-c(nfeats_perpart,length(which(x[i,]==1)))
					if(fitness_x[i]<fitness_lbest[i])
					{
					
						fitness_lbest[i]<-fitness_x[i]
						for (j in 1:dimsize)
						{
							
							x_lbest[i,j] <- x[i,j]
							
						}
				
						num_featl<-length(which(x[i,]==1))	
					}
			
				}

					
			fitness_var<-as.vector(fitness_x)
						
			
			 rank_vec<-rank(fitness_var)
						
			 #update the global best and its fitness
			if(min_fit_x < (fitness_gbest))
			{
				no_change<-0

				print("Best fitness updated to:")
				print(min_fit_x)
				print("Best solution:")
				print(x[global_best_index,])
				print(length(which(x[global_best_index,]==1)))
				global_no_change<-0
						fitness_gbest<-min_fit_x
				
						cverror_gbest<-max(cverror)[1]
						cvpermerror_gbest<-max(cvpermerror)[1]
	
						global_best_index<-bestind
						num_featg<-num_featl

						cverror_gbest<-cverror[bestind]
                                                cvpermerror_gbest<-cvpermerror[bestind]

					
	
						#global_best_index<-round(runif(1,1,length(global_best_index)))
						
						for (j in 1:dimsize)
						{
							
							x_gbest[j]<-x[global_best_index,j]
						
								
						}
						overall_gbest=x_gbest
						
						 overall_x_gbest<-x_gbest
			}
			else
			{
				no_change<-no_change+1

				
				#maxitrreset<-5
				
				
				
				#global no change is incremented if multiple reinitializations don't improve results
				if(no_change>maxitrreset)
				{
					global_no_change<-global_no_change+1
					
					print("RE-INITIALIZING...")
				 	#stop(paste("No change for ",maxitr," iterations",sep=""))	
					 fitness_lbest<-array(100000000000000000, dim=c(num_part, 1))
					#print(paste("No change for ",maxitr," iterations. Exiting PSO.",sep=""))

					x_lbest_vec<-apply(x_lbest,2,mean)
				
					print(global_no_change)
					print(global_max_itr)
					print(length(which(x_lbest_vec>0)))

					if(global_no_change>global_max_itr & length(which(x_lbest_vec>0))>minnum)
					{
		
					x_lbest_vec[which(x_lbest_vec>=minselect.pct)]<-1
					x_lbest_vec[which(x_lbest_vec<minselect.pct)]<-0	
					d1<-dist(as.matrix(rbind(x_lbest_vec,overall_x_gbest)))^2
		                        d1pct<-100*(d1/length(x_lbest_vec))
						if(d1pct<=1 && global_no_change>(global_max_itr))
						{	
					#	print(dim(x_lbest))	
					#	x_lbest<-apply(x_lbest,2,median)
						pdf_name<-paste("vel_xgbest_itr",globalpso_itr,".pdf",sep="")

						plot(v[global_best_index,which(overall_x_gbest==1)],xlab="velocity")
						dev.off()
						
						print(paste("No change for ",global_no_change," iterations. Exiting PSO.",sep=""))
							break;
						}else{
							rand_num<-runif(num_part,0,1)
							for (row in 1:num_part)
                                        	{
                                                num_feat<-0

						 set.seed(321)
                                                 ran<-runif(1,0,1)

                                                for (col in 1:dimsize)

                                                {
                                                        #ran<-runif(1,0,1)
                                                        if (ran<0.7)
                                                        {
								set.seed(321)
                                                                ran2<-runif(1,0,1)

                                                                if(ran2<0.9)
                                                                {
                                                                        x[row,col]<-0
                                                                }
                                                                else
                                                                {
                                                                        x[row,col]<-1
                                                                        num_feat<-num_feat+1
                                                                }

                                                                #x_lbest[row,col]<-0
                                                        #       if(ran==1)
                                                        #       {
                                                        #               num_feat<-num_feat+1
                                                        #       }
                                                        }else{
                                                                x[row,col]<-x_gbest[col]
                                                                #x_lbest[row,col]<-1

                                                                #col_sel[row,num_feat]<-col

                                                        }

                                                }
                                                count_feat[row]<-num_feat

                                        	}

						}
					}
							#	print("exited")
					
		#			min_fit_x<-1000000000000000000000
		#			v1<-sample(size=num_part*dimsize,x=c(0,1),replace=TRUE,prob=c(0.4,0.6))
		#			dim(v1)<-c(num_part, dimsize)	
						
			#agent_behavior[row]<-sample(x=seq(1,dim(transition_matrix)[1]),size=num_part,replace=TRUE)
						
			#agent_behavior<-sample(x=c(1,4,3),prob=c(0.4,0.4,0.2), size=num_part,replace=TRUE)
	
			#agent_behavior<-sample(x=c(1,4),prob=c(0.3,0.7), size=num_part,replace=TRUE)
			#agent_behavior<-sample(x=c(1,4,3),prob=c(0.3,0.4,0.3), size=num_part,replace=TRUE)	
			#agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,(1-(confusionprob+followerprob)),0))

	 #agent_behavior <- sample(x=1:dim(transition_matrix)[1], size = num_part, replace = TRUE, prob = c(confusionprob, followerprob,leaderprob,(1-(confusionprob+followerprob+leaderprob))))

		print(agent_behavior)

					#new addition in v19; revert to personal best
					#x<-x_lbest	
					#agent_behavior<-rep(4,num_part)

					#new addition in v20;
					for (i in 1:num_part)
                        		{
						x[i,]<-x_lbest_vec
					}
					
					agent_behavior[row]<-sample(x=seq(1,dim(transition_matrix)[1]),prob=c(0.25,0.25,0.25,0.25),size=num_part,replace=TRUE)
					
					#new addition in v7; removed in v13
					#x_gbest<-runif(dimsize,0,1)

					if(global_no_change>(global_max_itr*1.5))
					{
						print(paste("No change for ",global_no_change," iterations. Exiting PSO.",sep=""))
                                                        break;
						
					}

				no_change<-0
				}
			}
			
			
				#print("x_lbest")
                                #print(x_lbest[i,])
                                #print(x[i,])
				#print(x_gbest)

			 ###update the particle velocity and position

				nn_search_res<-find_similar_samples(x,NA,num_neighbors)
				
				#w_vec<-(runif(num_part,wmin,wmax))

			for (i in 1:num_part)
			{
					feat_sel<-0
			     
			 
				#w<-w_vec[i]

				#w<-1/(3-exp(-num_part/200)+((rank_vec[i])/(8*dimsize))^2)
				#w<-(exp(rank_vec[i]/num_part))-(exp(1/num_part))	
				#w<-((exp(rank_vec[i]/num_part))/1.7)-((exp(1/num_part))/1.705)
				#w<-((exp(rank_vec[i]/num_part))/1.359)
				#constant; global search
				w<-1				
				
				#random inertia;
				#w<-(-1)+(runif(1,0,1)/2)
				

				if(inertia_method=="rankbased"){

				
					#linearly increasing with rank
					#w<-wmin+(((wmax-wmin)/num_part)*rank_vec[i])
					w<-wmin+(wmax-wmin)/((rank_vec[i]))
				}else{

					if(inertia_method=="random"){
				
						#random inertia;
                                		w<-(wmin)+(runif(1,wmin,wmax)/2)
					}else{

						if(inertia_method=="global"){
							w<-1
						}else{
							if(inertia_method=="dec"){

								w<-w-(wmax-wmin)/((itr*0.5))
							}
						}
					}
				}
				
				#print("x_lbest")
				#print(x_lbest[i,])
				#print(x[i,])		



				x_curbest<-x_gbest

				best_action<-agent_behavior[i]

				social_status<-1
	
					#confusion
					if(best_action==1){
									set.seed(321)
									ran<-runif(dimsize,0,1)

					                for (col in 1:dimsize)
					                {
					
					                        if (ran[col]<0.9)
					                        {
					                                x_curbest[col]<-0
					
					                        }else{
					
					                                x_curbest[col]<-1
					
					                        }
					
					                }




						}else
						{
							if(best_action==2){
							x_curbest_ind<- nn_search_res[i,c(1:num_neighbors)]  #nn_search(i,x) #getnearestneighbor
                                                       
							#using nbest in v9 
							if(FALSE)
							{
							print("nearest neighbors are ")
                                                        print(x_curbest_ind)
							print(fitness_x[x_curbest_ind])
							best_fitness_neighbor<-which(fitness_x[x_curbest_ind]==min(fitness_x[x_curbest_ind],na.rm=TRUE)[1])[1]
                                                        x_curbest<-x[x_curbest_ind[best_fitness_neighbor],]
							print(x_curbest_ind[best_fitness_neighbor])
							}

							#using summary of neighbors in v10
							#if(FALSE)
							{
							
                                                        if(num_neighbors>1){
                                                        x_curbest<-apply(x[x_curbest_ind,],2,function(x){y<-quantile(x,0.75);return(round(y));})
                                                        }else{
                                                        x_curbest<-x[x_curbest_ind[1],]
                                                        }

							}

								}else{
						
								if(best_action==3){
									x_curbest<-x_gbest	
								}else{

											#self status: select
						
										social_status<-0	
							



									
									}
									
							}	
						}
					
			#set.seed(321)	
			r1<-runif(dimsize,0,1)

			#set.seed(100045)
                                        r2<-runif(dimsize,0,1)			
			
			r3<-runif(dimsize,0,1)
	
	
			       for (j in 1:dimsize)
				{
					
					
					
					#r1<-runif(1,0,1)

					#r2<-runif(1,0,1)

					#r3<-runif(1,0,1)
							
					v[i,j]<-constriction_factor*((w*v[i,j])+(c1*r1[j]*(x_lbest[i,j]-x[i,j]))+(c2*r2[j]*(x_curbest[j]-x[i,j])*social_status))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< (-6))
					{
						v[i,j]=-6
					}
					
					S<-1/(1+exp(-v[i,j]))
					S<-S+boostweight[j]
					
					if(S>=r3[j])
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
				check_badpos<-match(bad_pos,x[i,])
				num_badpos<-length(which(is.na(check_badpos)==FALSE))
				if(num_badpos>0){

					x[i,]<-x_lbest[i,]
				}

					
			}
			
	
			
			 x_lbest_mean<-apply(x_lbest,2,mean)
	

			feat_global_num<-length(which(overall_x_gbest==1))

	                feat_ind<-which(x_lbest_mean>=(minselect.pct))
			
			len_feat_ind<-length(feat_ind)

				 if(fitness_gbest<(-90) & feat_global_num<=maxnum & (global_no_change<=global_max_itr))
				 {
				
				
                                      #print(paste("Population consensus reached after ",k," iterations! Exiting PSO.",sep=""))
                                        #break;

		                 }

			d1<-dist(as.matrix(rbind(x_lbest_mean,overall_x_gbest)))^2
			d1pct<-100*(d1/length(x_lbest_mean))
				
								
				if(k>global_max_itr)
				{
					
					#	if(itr.terminate==TRUE){
					#		break;
					#	}
						if(fitness_gbest<(-minfitnessthresh) & (global_no_change>=global_max_itr) & d1pct<=3){

						
						#break;

						}else{

							
							print(paste("Minimum fitness threshold not reached after ",k," iterations! Continuing PSO.",sep=""))
							itr=k+itr-1

						}

	
				}
				
			
			
			
			
			itr_val<-{}
				cnames_sum<-c("Iteration #", "Inertia", "Number of features", "Global best fitness","Current best fitness","Number of features in current best agent","Number of features in global best", "Number of features in local best")

			itr_val<-cbind(itr_val, k)
			itr_val<-cbind(itr_val, w)
			itr_val<-cbind(itr_val, num_featg)
			if(evalMode!="custom"){
			itr_val<-cbind(itr_val, cverror_gbest)
			itr_val<-cbind(itr_val,cvpermerror_gbest)	
			cnames_sum<-c("Iteration #", "Inertia", "Number of features","CV best", "Permuted CV best", "Global best fitness","Current best fitness","Number of features in current best agent","Number of features in global best", "Number of features in local best")

			}
			itr_val<-cbind(itr_val, (-1)*fitness_gbest)
			itr_val<-cbind(itr_val, (-1)*min_fit_x)
			itr_val<-cbind(itr_val, count_feat[min_fit_index])
			itr_val<-cbind(itr_val,length(which(overall_x_gbest==1)))
			itr_val<-cbind(itr_val,length(which(x_lbest[min_fit_index,]==1)))
	
			itr_data<-rbind(itr_data, itr_val)
		}
	
		
		filestr<-paste(outloc,  "multiobj_itr",globalpso_itr,"_descpso.txt", sep="")

	
		

		bestgenelist<-which(overall_x_gbest==1)
		
		print(bestgenelist)
		print(globalpso_itr)
		
		scoringmatrix[bestgenelist,globalpso_itr]=1
	colnames(itr_data)<-cnames_sum
	write.table(itr_data, file=filestr, sep="\t", row.names=F)


print("##################################")
			print(paste("Results summary for itr:",globalpso_itr,sep=""))
			 x_lbest_mean<-apply(x_lbest,2,mean)


                feat_ind<-which(x_lbest_mean>=minselect.pct)

                print("number of features selected using population mean")
                print(length(feat_ind))
		
			if(select.global.best==TRUE){	
				feat_ind<-which(overall_x_gbest==1)
			}
		print("number of features selected using current global best")
                print(length(which(overall_x_gbest==1)))

		print("feat ind length")
		print(length(feat_ind))

	if(length(feat_ind)>0){
		bestgenelist<-feat_ind
                scoringmatrix[bestgenelist,globalpso_itr]=1

		print("best accuracy")
		print((-1)*fitness_gbest)


		modtrain<-as.matrix(trainm[,c(bestgenelist)])
        modtest<-as.matrix(subtest[,c(bestgenelist)])
model_train_valid<-svm(modtrain,  trainclass,   kernel=kname, type="C")
pred_test<-predict(model_train_valid, modtest)
test.table<-table(pred_test, subtestclass)

testacc<-sum(diag(test.table))/(dim(modtest)[1])
}

print(paste("test acc:", testacc, sep=""))
testacc_all<-c(testacc_all,testacc)
		sumr<-paste("global num features ", length(feat_list)-1, "current best set of features ", overall_x_gbest, "global best accuracy ", 1-fitness_gbest," test acc ", testacc)

		filestr2<-paste(outloc,  "selected_feature_index_itr",globalpso_itr,".csv", sep="")
		write.table(feat_ind, file=filestr2, sep=",", row.names=FALSE)
		print("##################################")
		
	stopCluster(cl)	
	
	}

return(list("scoringmatrix"=scoringmatrix,"acc"=testacc_all))
}
