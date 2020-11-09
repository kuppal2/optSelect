get_DSindex <-
function(m){

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
