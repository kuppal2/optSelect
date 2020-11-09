get_KIindex <-
function(m,k){
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
