
.libPaths("/home/stu/kuppal3/karan_libs/Rlibs")
library(deal)

d1=read.csv("~/Research/Heart_data/data_nominal_IMT.csv")

n1=network(d1)
pdf("dealnet_imtheart.pdf")
plot(n1)
dev.off()
