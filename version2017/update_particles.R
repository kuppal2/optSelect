for (i in 1:num_part)
{
    feat_sel<-0
    #constant; global search
    w<-1
    if(inertia_method=="rankbased"){
        #linearly increasing with rank
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
    
    num_neighbors_sample<-sample(x=3:num_part,size=1)
    nn_search_res<-find_similar_samples(x,NA,num_neighbors_sample)
    
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
            
            #using nbest in v3
            #if(FALSE)
            {
                #print("nearest neighbors are ")
                #                       print(x_curbest_ind)
                #print(fitness_x[x_curbest_ind])
                best_fitness_neighbor<-which(fitness_x[x_curbest_ind]==min(fitness_x[x_curbest_ind],na.rm=TRUE)[1])[1]
                x_curbest<-x[x_curbest_ind[best_fitness_neighbor],]
                #print(x_curbest_ind[best_fitness_neighbor])
            }
            
            #using summary of neighbors in v2
            if(FALSE)
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
    
    #x_curbest<-x_gbest
    
    third_quartile_fitness<-quantile(fitness_x,na.rm=TRUE,0.75)
    top_fitness_neighbors<-which(fitness_x>=third_quartile_fitness)
    
    #using summary of neighbors in v10
    x_curbest_popsummary<-apply(x[top_fitness_neighbors,],2,function(x){y<-quantile(x,0.9);return(round(y));})
    
    #set.seed(321)
    r1<-runif(dimsize,0,1)
    
    #set.seed(100045)
    r2<-runif(dimsize,0,1)
    
    r3<-runif(dimsize,0,1)
    
    r4<-runif(dimsize,0,1)
    
    r5<-runif(dimsize,0,1)
    
    for (j in 1:dimsize)
    {
        
        
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
