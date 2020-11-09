
R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: x86_64-redhat-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> 
> #.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
> library(snow)
> library(e1071)
Loading required package: class
> library(yaImpute)

Attaching package: ‘yaImpute’

The following object is masked from ‘package:e1071’:

    impute

> library(pROC)
Type 'citation("pROC")' for a citation.

Attaching package: ‘pROC’

The following objects are masked from ‘package:stats’:

    cov, smooth, var

> library(bioDist)
Loading required package: Biobase
Loading required package: BiocGenerics
Loading required package: parallel

Attaching package: ‘parallel’

The following objects are masked from ‘package:snow’:

    clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
    clusterExport, clusterMap, clusterSplit, makeCluster, parApply,
    parCapply, parLapply, parRapply, parSapply, splitIndices,
    stopCluster


Attaching package: ‘BiocGenerics’

The following objects are masked from ‘package:parallel’:

    clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
    clusterExport, clusterMap, parApply, parCapply, parLapply,
    parLapplyLB, parRapply, parSapply, parSapplyLB

The following objects are masked from ‘package:snow’:

    clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
    clusterExport, clusterMap, parApply, parCapply, parLapply,
    parRapply, parSapply

The following object is masked from ‘package:stats’:

    xtabs

The following objects are masked from ‘package:base’:

    anyDuplicated, as.data.frame, cbind, colnames, duplicated, eval,
    Filter, Find, get, intersect, lapply, Map, mapply, match, mget,
    order, paste, pmax, pmax.int, pmin, pmin.int, Position, rank,
    rbind, Reduce, rep.int, rownames, sapply, setdiff, sort, table,
    tapply, union, unique, unlist

Welcome to Bioconductor

    Vignettes contain introductory material; view with
    'browseVignettes()'. To cite Bioconductor, see
    'citation("Biobase")', and for packages 'citation("pkgname")'.

Loading required package: KernSmooth
KernSmooth 2.23 loaded
Copyright M. P. Wand 1997-2009
> #library(CMA, lib="/home/stu/kuppal3/karan_libs/Rlibs/")
> library(genalg)
> library(CMA)

Attaching package: ‘CMA’

The following object is masked from ‘package:pROC’:

    roc

The following object is masked from ‘package:e1071’:

    tune

> library(expm)
Loading required package: Matrix

Attaching package: ‘expm’

The following object is masked from ‘package:Matrix’:

    expm

> 
> cl<-makeCluster(1)
> 
> #source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionjuly12013/svm_cv_v1.1.R")
> #source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/OCFS_vnov252014.R")
> 
> #same results
> #source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vnov26.22014_v11.R")
> 
> #source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vapr282015.R")
> 
> #source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vapr282015_v2.R")
> 
> source("/home/stu/kuppal3/Research/Feature_selection/Rcode/versionnov2014/OCFS_vapr282015_v11.R")
> #source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/svm_cv_v1.1.R")
> 
> #source("/Users/karanuppal/Documents/Gatech/Projects/Algorithms/TwostagePSO/Rcode/versionnov2014/performCMA_consensus_tenfold_ber_nominal_lungheart_train_test_nov172014_none.R")
> 
> load("/home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/Golub.rda")
> 
> trainm<-Golub$X
> testm<-Golub$Xt
> trainclass<-Golub$y
> testclass<-Golub$yt
> 
> trainm<-t(trainm)
> testm<-t(testm)
> 
> trainm[trainm < 100] <- 100
> trainm[trainm > 16000] <- 16000
> 
> testm[testm < 100] <- 100
> testm[testm > 16000] <- 16000
> 
> mmfilt <- function(x,r = 5, d = 500, na.rm = TRUE) {
+  minval <- min(x, na.rm = na.rm)
+  maxval <- max(x, na.rm = na.rm)
+  (maxval/minval > r) && (maxval - minval > d)
+ }
> 
> #mmfun <- mmfilt()
> #ffun <- filterfun(mmfun)
> #good <- genefilter(X, ffun)
> 
> good<-apply(trainm,1,mmfilt)
> 
> 
> trainm<-trainm[good,]
> 
> print(dim(trainm))
[1] 3051   38
> testm<-testm[good,]
> 
> trainm<-log10(trainm+1)
> testm<-log10(testm+1)
> 
> 
> trainm<-t(trainm)
> testm<-t(testm)
> 
> trainm<-cbind(trainclass,trainm)
> testm<-cbind(testclass,testm)
> 
> trainm<-na.omit(trainm)
> testm<-na.omit(testm)
> 
> #outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Khan_SRBCT/OCFSv21itr1_LIMMApres1novarsel_l0.45f0.45c0.05minsel0.3randbehavfeatw0.01_local/"
> outloc<-"/home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/OCFSvapr2815v11_itrmay22_LassoRFELIMMAELpres1backwsel_l0.25f0.45c0.25_top10maxitrs100minselmedianrankbehavfeatw0.01_CV2accA100B1wrand6methodsmax100/"
> 
> 
> dir.create(outloc)
Warning message:
In dir.create(outloc) :
  '/home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/OCFSvapr2815v11_itrmay22_LassoRFELIMMAELpres1backwsel_l0.25f0.45c0.25_top10maxitrs100minselmedianrankbehavfeatw0.01_CV2accA100B1wrand6methodsmax100' already exists
> setwd(outloc)
> 
> trainm<-as.matrix(trainm)
> testm<-as.matrix(testm)
> trainclass<-trainm[,1] #CMAres$modtrainclass
> testclass<-testm[,1] #CMAres$modtestclass
> trainm<-trainm[,-c(1)] #CMAres$modtrainmata
> testm<-testm[,-c(1)] #CMAres$modtestmata
> 
> #a: Confusions
> #b: Neighbors
> #c: Global
> #d: Death
> 
> a<-c(0.25,0.25,0.25,0.25)
> b<-c(0.3,0.1,0.4,0.1)
> c<-c(0.25,0.25,0.5,0)
> d<-c(0.9,0.1,0,0.1)
> 
> a<-c(0,0.4,0.1,0.5)
> b<-c(0.3,0.1,0.4,0.1)
> c<-c(0,0.5,0.5,0)
> d<-c(0.9,0.1,0,0)
> 
> a<-c(0,0.4,0.1,0.5)
> b<-c(0.2,0.3,0.4,0.1)
> c<-c(0,0.4,0.4,0.2)
> d<-c(0.9,0.1,0,0)
> 
> transition_matrix<-rbind(a,b,c,d)
> 
> 
> dir.create(outloc)
Warning message:
In dir.create(outloc) :
  '/home/stu/kuppal3/Research/Feature_selection/Datasets/Golub/OCFSvapr2815v11_itrmay22_LassoRFELIMMAELpres1backwsel_l0.25f0.45c0.25_top10maxitrs100minselmedianrankbehavfeatw0.01_CV2accA100B1wrand6methodsmax100' already exists
> setwd(outloc)
> temp2=t(trainm)
> temp2=apply(temp2, 2, function(x){which(x=="MD")})
> temp2=unlist(temp2)
> temp2=unique(temp2)
> if(length(temp2)>1)
+ {
+ 	trainm=trainm[,-c(temp2)]
+ 
+ 	rm(temp2)
+ }
> 
> boostweight=rep(0,dim(trainm)[2])
> 
> 
> #gsmethods=c("limma","lasso","rfe","elasticnet", "f.test"),
> #1
> CMAres<-performCMA(trainm, trainclass, testm, testclass,outloc,
+ maxnum=10,
+ minnum=3,
+ stepitr=1,
+ gsmethods=c("lasso"), #"lasso","elasticnet","kruskal.test"), #"f.test", "f.test", "elasticnet", "wilcox.test", "welch.test"),
+ percentTest=0.40,
+ accuracyweight=1,
+ featweight=0.06,
+ minpresent=1,
+ kname="radial",
+ norm_method="none",
+ tolerance=0.1,
+ maxitrs=5,
+ classindex=1,
+ numfacts=0,
+ evalmethod="CV",
+ numfolds=10,
+ CVfoldthresh=0.7,
+ varselmethod="none",
+ scheme_val="one-vs-all",
+ iter_learn=1,boostweight=boostweight)
[1] "dim of trainm is "
[1]   38 3051
[1]   38 3051
[1] "length of factcols"
[1] 0
[1]   38 3051
[1]   34 3051
integer(0)
character(0)
NULL
[1] "ok"
[1] "test class"
V1 V2 V3 V4 
-1 -1 -1 -1 
Levels: -1 1
[1] "orig train matrix"
                                                                            
[1,] 2.004321 2.334454 2.902003 4.162535 3.988514 3.930949 4.178315 4.046378
[2,] 2.004321 2.068186 2.637490 2.789581 2.064458 3.181558 4.204147 4.132548
[3,] 2.021189 2.678518 3.168792 3.753583 3.514946 3.564548 4.204147 4.204147
[4,] 2.004321 2.193125 2.619093 3.685831 3.360593 3.409933 4.173798 4.058426
[5,] 2.004321 2.089905 2.684845 3.108903 3.436481 2.501059 4.165956 4.176988
                      
[1,] 4.204147 4.204147
[2,] 4.204147 4.204147
[3,] 4.204147 4.204147
[4,] 4.197859 4.204147
[5,] 4.204147 4.204147
[1] "orig train matrix"
                                                                            
[1,] 2.004321 2.334454 2.902003 4.162535 3.988514 3.930949 4.178315 4.046378
[2,] 2.004321 2.068186 2.637490 2.789581 2.064458 3.181558 4.204147 4.132548
[3,] 2.021189 2.678518 3.168792 3.753583 3.514946 3.564548 4.204147 4.204147
[4,] 2.004321 2.193125 2.619093 3.685831 3.360593 3.409933 4.173798 4.058426
[5,] 2.004321 2.089905 2.684845 3.108903 3.436481 2.501059 4.165956 4.176988
                      
[1,] 4.204147 4.204147
[2,] 4.204147 4.204147
[3,] 4.204147 4.204147
[4,] 4.197859 4.204147
[5,] 4.204147 4.204147
[1] 2.902003 2.637490 3.168792
[1] "norm train matrix"
                                                                            
[1,] 2.004321 2.334454 2.902003 4.162535 3.988514 3.930949 4.178315 4.046378
[2,] 2.004321 2.068186 2.637490 2.789581 2.064458 3.181558 4.204147 4.132548
[3,] 2.021189 2.678518 3.168792 3.753583 3.514946 3.564548 4.204147 4.204147
[4,] 2.004321 2.193125 2.619093 3.685831 3.360593 3.409933 4.173798 4.058426
[5,] 2.004321 2.089905 2.684845 3.108903 3.436481 2.501059 4.165956 4.176988
                      
[1,] 4.204147 4.204147
[2,] 4.204147 4.204147
[3,] 4.204147 4.204147
[4,] 4.197859 4.204147
[5,] 4.204147 4.204147
[1] "mean of feat 2"
[1] 2.241856
[1] "sd of feat 2"
[1] 0.2932821
[1] "maxnum is "
[1] 10
[1] "# of genes left after filtering:"
[1]   38 3051
GeneSelection: iteration 1 
Loaded glmnet 1.9-8


Attaching package: ‘glmnet’

The following object is masked from ‘package:pROC’:

    auc

GeneSelection: iteration 2 
GeneSelection: iteration 3 
GeneSelection: iteration 4 
GeneSelection: iteration 5 
GeneSelection: iteration 6 
GeneSelection: iteration 7 
GeneSelection: iteration 8 
GeneSelection: iteration 9 
GeneSelection: iteration 10 
[1] "varselmethod"
[1] "none"
[1] "dim of scoring matrix is "
[1] 3051    1
[1] 3051
[1] "DS index stage 1"
[1] NA
[1] "bestgenelist"
 [1]    1    2    3    4    5  523  808  829 2124 2670
                                                                            
[1,] 2.004321 2.334454 2.902003 4.162535 3.988514 2.786041 2.418301 2.482874
[2,] 2.004321 2.068186 2.637490 2.789581 2.064458 2.967548 2.008600 3.133219
[3,] 2.021189 2.678518 3.168792 3.753583 3.514946 3.229938 2.491362 2.406540
                      
[1,] 2.475671 2.004321
[2,] 2.488551 2.004321
[3,] 2.491362 2.004321
                                                                            
[1,] 2.004321 2.133539 2.428135 2.004321 2.004321 2.895975 2.227887 2.004321
[2,] 2.004321 2.004321 2.572872 2.004321 2.004321 2.997823 2.389166 2.004321
[3,] 2.004321 2.004321 2.220108 2.021189 2.004321 2.789581 2.004321 2.004321
                      
[1,] 2.252853 2.037426
[2,] 2.025306 2.230449
[3,] 2.403121 2.584331
[1] "numgenes selected:10"
[1] "test acc:1"
[1] "test AUC acc:1"
[1] "10 fold train100"
[1] "confusion matrix train 10 fold"
          nci_y
pred10fold -1  1
        -1 27  0
        1   0 11
[1] "confusion matrix test"
         test_y
pred_test -1  1
       -1 20  0
       1   0 14
[1] "train acc:1"
[1] "confusion matrix train"
          nci_y
pred_train -1  1
        -1 27  0
        1   0 11
Warning messages:
1: In if (is.na(testm) == TRUE) { :
  the condition has length > 1 and only the first element will be used
2: In if (is.na(testclass) == TRUE) { :
  the condition has length > 1 and only the first element will be used
3: In mean.default(DS_res, na.rm = TRUE) :
  argument is not numeric or logical: returning NA
> 
> cma_feat_list<-colnames(trainm)
> 
> save(CMAres,file="CMAres.Rda")
> write.table(cma_feat_list,file="selected_cma_feat_list.txt",sep="t",row.names=FALSE)
> 
> # modtraindata=modtrain, modtestdata=modtest, blindtest=testacc, modtrainclass=nci_y, modtestclass=test_y
> #if(FALSE)
> {
+ trainm<-CMAres$modtraindata
+ testm<-CMAres$modtestdata
+ trainclass<-CMAres$modtrainclass
+ testclass<-CMAres$modtestclass
+ learningsets<-CMAres$learningsets
+ boostweight<-CMAres$boostweight
+ }
> 
> if(FALSE)
+ {
+ trainclass<-trainm[,1] #CMAres$modtrainclass
+ testclass<-testm[,1] #CMAres$modtestclass
+ trainm<-trainm[,-c(1)] #CMAres$modtrainmata
+ testm<-testm[,-c(1)] #CMAres$modtestmata
+ 
+ }
> 
> d_dim<-dim(trainm)
> 
> print("Original dimension")
[1] "Original dimension"
> print(d_dim)
[1] 38 10
> 
> evaluateNIR <- function(chromosome=c()) {
+          if(FALSE){
+          returnVal = 100
+          minLV = 2
+          if (sum(chromosome) < minLV) {
+              returnVal
+          } else {
+              xtrain = NIR$Xtrain[,chromosome == 1];
+              pls.model = pls(xtrain, NIR$Ytrain, validation="CV", grpsize=1,
+                              ncomp=2:min(10,sum(chromosome)))
+              returnVal = pls.model$val$RMS[pls.model$val$nLV-(minLV-1)]
+              returnVal
+          }
+         }
+                 returnVal<-eval_fit_test_diff(particle=chromosome, numfolds=10,trainm=trainm,trainclass=trainclass,
+                                                 testm=testm,testclass=testclass,errortype="BER",kname="radial",accuracyweightA=100,
+                                                 accuracyweightB=1,featweight=0.01,max_num_feats=3)
+                 print(returnVal)
+                 returnVal<-returnVal$fitfunc
+         }
> nir.results = rbga.bin(size=dim(trainm)[2], zeroToOneRatio=10,
+          evalFunc=evaluateNIR, monitorFunc=monitor,
+          popSize=10, iters=10, verbose=TRUE)
Testing the sanity of parameters...
Not showing GA settings...
Starting with random values in the given domains...
Starting iteration 1 
Calucating evaluation values... Error in `[.data.frame`(y, g) : undefined columns selected
Calls: rbga.bin ... evalFunc -> eval_fit_test_diff -> svm_cv -> [ -> [.data.frame
Execution halted
