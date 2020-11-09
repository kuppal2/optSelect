# x is the training input
# y is the training output
# xtest is the testing input
# cat indicate which columns are categorical inputs

nbayes<-function(x,y,xtest,cat=0){
n<-dim(x)[2]
ntest<-dim(xtest)
ncat<-length(cat)

if(cat[1]!=0){

x1<-x[,cat]
x2<-x[,-cat]
xtest1<-xtest[,cat]
xtest2<-xtest[,-cat]

y1<-factor(y)
ylevel<-levels(y1)

ind1<-c()
prob1<-c()
#calculate categorical probability
for (i in 1:ncat[1]){
x1[,i]<-factor(x1[,i])
ncat[i+1]<-length(levels(x1[,i]))
temp<-levels(x1[,i])
temp1<-c()
ind1<-c(ind1,rep(i,ncat[i+1]))
for(j in 1:ncat[i+1]){
temp2<-c()
for(k in 1:length(ylevel)){
temp2<-c(temp2,mean(x1[y1==ylevel[k],i]==temp[j]))
}
temp1<-cbind(temp1,temp2)
}
prob1<-cbind(prob1,temp1)
}

icat<-n-ncat[1]
ind2<-c()
prob2<-c()
for (i in 1:icat){
temp<-c()
for(j in 1:length(ylevel)){
temp<-rbind(temp,c(mean(x2[y1==ylevel[j],i]),sd(x2[y1==ylevel[j],i])))
}
ind2<-c(ind2,rep(i,2))
prob2<-cbind(prob2,temp)
}

pred1<-matrix(1,ntest[1],length(ylevel))
pred2<-matrix(1,ntest[1],length(ylevel))
#Categorical
for (i in 1:ntest[1]){
pred1[i,]=table(y1)/length(y1)
for(j in 1:ncat[1]){
temp<-levels(x1[,j])
temp1<-prob1[,ind1==j]
if(sum(temp==xtest1[i,j])!=0){
pred1[i,]=pred1[i,]*temp1[,temp==xtest1[i,j]]
}
else{
pred1[i,]=pred1[i,]
}
}}
#Numerical
for(i in 1:ntest[1]){
for(j in 1:icat){
temp<-prob2[,ind2==j]
pred2[i,]=pred2[i,]*((2*pi)^0.5*temp[,2])^(-1)*exp(-(xtest2[i,j]-temp[,1])^2/(2*temp[,2]^2))
}
}

pred<-pred1*pred2
prob_y<-pred
for(k in 1:length(ylevel)){
prob_y[,k]<-pred[,k]/(apply(pred,1,sum))
}
nbayes<-cbind(prob_y,apply(prob_y,1,which.max))
}

else{

y1<-factor(y)
ylevel<-levels(y1)

icat<-n

ind2<-c()
prob2<-c()
for (i in 1:icat){
temp<-c()
for(j in 1:length(ylevel)){
if(sd(x[y1==ylevel[j],i])==0){
temp<-rbind(temp,c(mean(x[y1==ylevel[j],i]),0.001))
}
else{
temp<-rbind(temp,c(mean(x[y1==ylevel[j],i]),sd(x[y1==ylevel[j],i])))

}
}
ind2<-c(ind2,rep(i,2))
prob2<-cbind(prob2,temp)
}

pred2<-matrix(0,ntest[1],length(ylevel))
for(i in 1:ntest[1]){
pred2[i,]=log(table(y1)/length(y1))
for(j in 1:icat){
temp1<-prob2[,c(2*j-1,2*j)]
pred2[i,]=pred2[i,]-log(temp1[,2])-(xtest[i,j]-temp1[,1])^2/(2*temp1[,2]^2)
}
}

prob_y<-pred2
#for(k in 1:length(ylevel)){
#prob_y[,k]<-1/(apply(exp(pred2-pred2[,k]),1,sum))
#}
nbayes<-cbind(prob_y,apply(prob_y,1,which.max))






}

}
