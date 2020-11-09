cl <- makeCluster(2)
registerDoParallel(cl)
foreach(i=1:num_part) %dopar%
