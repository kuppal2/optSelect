monitor <-
function(obj) {
         minEval = min(obj$evaluations);
         filter = obj$evaluations == minEval;
         bestObjectCount = sum(rep(1, obj$popSize)[filter]);
         # ok, deal with the situation that more than one object is best
         if (bestObjectCount > 1) {
             bestSolution = obj$population[filter,][1,];
         } else {
             bestSolution = obj$population[filter,];
         }
         outputBest = paste(obj$iter, " #selected=", sum(bestSolution),
                            " Best (Error=", minEval, "): ", sep="");
         for (var in 1:length(bestSolution)) {
             outputBest = paste(outputBest,
                 bestSolution[var], " ",
                 sep="");
         }
         outputBest = paste(outputBest, "\n", sep="");

         cat(outputBest);
	save(bestSolution,file="gabestsol.Rda")
	return(bestSolution)     
	
}
