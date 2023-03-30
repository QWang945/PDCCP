library(foreign)
library(survival)
library(caret)

setwd("D:\\01����\\����\\�����ֱ����������\\���\\CSS\\1���������")

seer<-read.table("data����.txt",header=T,sep="\t")

set.seed(20191105)

seerd<-createDataPartition(y=seer$id,p=0.70,list=F)

seerdev<-seer[seerd,]

seerv<-seer[-seerd,] 

write.csv(seerdev, "seerdev.csv")

write.csv(seerv, "seerv.csv")
