library(foreign)
library(survival)
library(caret)

setwd("D:\\01桌面\\合作\\老年结直肠早期死亡\\结果\\CSS\\1随机分两组")

seer<-read.table("data分组.txt",header=T,sep="\t")

set.seed(20191105)

seerd<-createDataPartition(y=seer$id,p=0.70,list=F)

seerdev<-seer[seerd,]

seerv<-seer[-seerd,] 

write.csv(seerdev, "seerdev.csv")

write.csv(seerv, "seerv.csv")

