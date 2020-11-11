find_similar_samples <-
function(part_group, particle_ind=NA,num_neighbors=3){

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
