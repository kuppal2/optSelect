#Step 1: Install dependencies:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("bioDist","limma"),update=FALSE,ask=FALSE)
BiocManager::install("CMA",dependencies=TRUE,update=FALSE,ask=FALSE)
install.packages("expm",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
install.packages("parallel",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
install.packages("e1071",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
install.packages("yaImpute",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
install.packages("RankAggreg",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
install.packages("pROC",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
install.packages("devtools",dependencies=TRUE,ask=FALSE,repos="https://cran.r-project.org")
###############################################

#Step 2: Install optSelect
library(devtools)
install_github("kuppal2/optSelect")

###############################################