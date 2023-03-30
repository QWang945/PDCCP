library(rms)
library(foreign)
library(DynNom)
library(shiny)
library(rsconnect)

setwd("D:\\01桌面\\合作\\老年结直肠早期死亡\\结果\\3绘制nomogram")

rawdata<-read.csv("seerdev.csv") 

#View(rawdata)


rawdata$Brain_metastasis<-factor(rawdata$Brain_metastasis,labels=c('No','Yes'))
relevel(rawdata$Brain_metastasis,ref= 'No')

rawdata$Liver_metastasis<-factor(rawdata$Liver_metastasis,labels=c('No','Yes'))
relevel(rawdata$Liver_metastasis,ref= 'No')

rawdata$Brain_metastasis<-factor(rawdata$Brain_metastasis,labels=c('No','Yes'))
relevel(rawdata$Brain_metastasis,ref= 'No')

rawdata$Bone_metastasis<-factor(rawdata$Bone_metastasis,labels=c('No','Yes'))
relevel(rawdata$Bone_metastasis,ref= 'No')

rawdata$Marital_status<-factor(rawdata$Marital_status,labels=c('Married','Unmarried'))
relevel(rawdata$Marital_status,ref= 'Married')

rawdata$N_stage<-factor(rawdata$N_stage,labels=c('N0','N1','N2'))
relevel(rawdata$N_stage,ref= 'N0')

rawdata$M_stage<-factor(rawdata$M_stage,labels=c('M0','M1'))
relevel(rawdata$M_stage,ref= 'M0')

rawdata$M_stage<-factor(rawdata$M_stage,labels=c('M0','M1'))
relevel(rawdata$M_stage,ref= 'M0')

rawdata$Race<-factor(rawdata$Race,labels=c('Black','Other','White'))
relevel(rawdata$N_stage,ref= 'N0')

rawdata$Primary_site<-factor(rawdata$Primary_site,labels=c('Left','Rectum','Right'))
relevel(rawdata$Primary_site,ref= 'Left')

rawdata$Sex<-factor(rawdata$Sex,labels=c('Female','Male'))
relevel(rawdata$Sex,ref= 'Female')

rawdata$Histology<-factor(rawdata$Histology,labels=c('Adenocarcinoma','Other'))
relevel(rawdata$Histology,ref= 'Adenocarcinoma')

rawdata$Surgery<-factor(rawdata$Surgery,labels=c('No','Yes'))
relevel(rawdata$Surgery,ref= 'No')

rawdata$Chemotherapy<-factor(rawdata$Chemotherapy,labels=c('No','Yes'))
relevel(rawdata$Chemotherapy,ref= 'No')

rawdata$T_stage<-factor(rawdata$T_stage,labels=c('T1','T2','T3','T4'))
relevel(rawdata$T_stage,ref= 'T1')

rawdata$Tumor_size<-factor(rawdata$Tumor_size,labels=c('≤5cm','>5cm'))
relevel(rawdata$Tumor_size,ref= '≤5cm')

rawdata$Grade<-factor(rawdata$Grade,labels=c('Well differentiation','Poor differentiation'))
relevel(rawdata$Grade,ref= 'Well differentiation')
        
        

ddist <- datadist(rawdata)
options(datadist='ddist')

#构建回归模型


modelA2 <- lrm(status ~ Histology+M_stage+T_stage+N_stage+Tumor_size+Chemotherapy+Primary_site+Race+Sex+Marital_status+Surgery+Grade+Bone_metastasis+Brain_metastasis+Liver_metastasis+Lung_metastasis,data=rawdata)


#设置列线图参数
#第一行modelA就是刚才logistic回归的模型名称。lp选择True或False，是否显示线性预测坐标（linear predictor），fun是要自己设一个函数，对lp进行转换，并建立一个新坐标轴。此处就用logit变换的反函数，将lp转换为我们熟悉的风险概率-。function(x) 1/(1+exp(-x))这一串，即使用function()构建一个自定义函数，括号中的x从lp的范围中取值，代入1/(1+exp(-x))中运算。
#fun.at则是给新的坐标轴设置范围。funlabel则是给上面转换好的新坐标轴起个名字，Diagnostic possibility。其实有了这条坐标轴，上面lp那里也可以设为F，不显示了。

nomomodelA <- nomogram(modelA2,lp=F, 
                       fun=function(x)1/(1+exp(-x)),
                       fun.at=seq(0.1,1,by=0.1),
                       funlabel="Probability of DM")

plot(nomomodelA)




modelA3<-glm(status ~ Grade+Histological_type+T_stage+N_stage,data=rawdata,x=T,y=T,
             family=binomial(link="logit"))
## 动态
DynNom(modelA3,data=rawdata)

DNbuilder(modelA3)

rsconnect::setAccountInfo(name='yxyx',
                          token='4BDB657EE0EE9BC1F70CFA70464E4B18',
                          secret='DP2/agjATv5sNw8cL2jx1wH2KmkyZvHKaRHImttc')
