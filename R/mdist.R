mdist <-
function(x)
{
	
	t<-as.matrix(x)
	p<-dim(t)[2]
	#m<-apply(t,2,mean)
	m<-colMeans(t)
	s<-cov(t)
	mahalanobis(t,m,s, inverted=TRUE)
}
