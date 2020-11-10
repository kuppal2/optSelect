#Step 1: Install dependencies:

Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("bioDist","limma","CMA"),update=FALSE,ask=FALSE)

###############################################

#Step 2: Install optSelect
library(devtools)
install_github("kuppal2/optSelect",upgrade="never",dependencies=c("Imports"))

###############################################