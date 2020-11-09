library(class)
source("nbayes.r")

v=10
k=3
count<-0



data<-read.table("zip.TRAIN", na.strings="")
data.tr<-as.matrix(data)
x.tr<-as.matrix(data[,-c(1)])
y.tr<-as.matrix(data[,1])
test<-read.table("zip.TEST", na.strings="")
data.te<-as.matrix(test)
x.te<-as.matrix(test[,-c(1)])
y.te<-as.matrix(test[,1])

get_error<-function(e,b,t)
 {
 tot=0
 j=0
for(o in b)
{
j=j+1
}
  for(i in 1:j){
  e_r=0
  for(k in 1:j){
 if(i !=k)
 {
e_r=e[i,k]+e_r
 }
 }
 tot=tot+e_r
 }
error=(tot/t)
#return(error)
#print (tot)
cat(paste("knn error for k=1 is ",error,"\n"))
}
cy<-factor(y.tr)
cylevel<-levels(cy)

dim_test<-dim(test)


for (l in 0:1)
{
	i=l

	for (m in i:1)
	{
		d1<-data[(data$V1==cylevel[l]),]
		
		print(dim(d1))
		d2<-data[(data$V1==cylevel[m]),]
		
		d3<-rbind(d1,d2)
		dx.tr<-as.matrix(d3[,-c(1)])
		dy.tr<-as.matrix(d3[,1])
		knn.3<-table(y.tr, knn.cv(dx.tr,dy.tr,k=3,prob=TRUE))
		res<-get_error(knn.3,cylevel,dim_test[1])
		filen<-paste(as.character(l),"_",as.character(m),"_","res.csv")
		#write.table(res,file=filen, sep=",",row.names=F)
		print(l,m,res)
		count<-count+1
	}

}
print(count)