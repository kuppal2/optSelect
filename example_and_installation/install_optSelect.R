#Step 1: Install dependencies:


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install(c("bioDist","limma"))
BiocManager::install("CMA",dependencies=TRUE)
install.packages("expm",dependencies=TRUE)
install.packages("parallel",dependencies=TRUE)
install.packages("e1071",dependencies=TRUE)
install.packages("yaImpute",dependencies=TRUE)
install.packages("RankAggreg",dependencies=TRUE)
install.packages("pROC",dependencies=TRUE)
install.packages("devtools",dependencies=TRUE)
###############################################

#Step 2: Install optSelect
library(devtools)
install_github("kuppal2/optSelect")

###############################################