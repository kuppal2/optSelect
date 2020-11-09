 ###update the particle velocity and position; action

act<-function(state,action){
			for (i in 1:num_part)
			{
					feat_sel<-0
			      
				#constant; global search
				#w<-1				
				
				#random inertia;
				#w<-0.5+(runif(1,0,1)/2)
				
				#linearly increasing with rank
				w<-wmin+(((wmax-wmin)/num_part)*rank_vec[i]) #w<-wmax-(((wmax-wmin)/num_part)*rank_vec[i])
				
				
			    x_curbest<-x_gbest
               if(agent_behavior[i]=="S")
               {
              
                ran<-runif(d_dim[2])
                                             
                for (col in 1:d_dim[2]) 
                {                               
                                                        
                        if (ran[col]<0)         
                        {
                                x_curbest[col]<-0

                        }else{

                                x_curbest[col]<-1

                        }

                }
                
                }else{
                                                if(agent_behavior[i]=="N"){
                                                        x_curbest_ind<- nn_search(i,x) #getnearestneighbor
                                			#print("current best is ")
							#print(x_curbest_ind)
				                	x_curbest<-x[x_curbest_ind[1],]
						}else{
							
							if(agent_behavior[i]=="W"){
							x_curbest<-x_gbest
							}
						}

              }

				
	
			       for (j in 1:d_dim[2])
				{
					r1<-runif(1,0,1)

					r2<-runif(1,0,1)

					r3<-runif(1,0,1)
							
					v[i,j]<-((w*v[i,j])+(c1*r1*(x_lbest[i,j]-x[i,j]))+(c2*r2*(x_curbest[j]-x[i,j])))
					
					if(v[i,j]>6)
					{
						v[i,j]=6
					}	
					if(v[i,j]< (-6))
					{
						v[i,j]=-6
					}
					
					S<-1/(1+exp(-v[i,j]))
					
					if(S>r3)
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
					
			}		