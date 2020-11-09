makefactors <-
function(curdata)
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
		       ##print(length(which(factvec==getlevels[g])))
		       #flevelind=which(factvec==getlevels[g])

		       ##print(factvec[flevelind[1:3]])
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
