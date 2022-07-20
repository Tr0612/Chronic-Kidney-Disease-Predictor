#loading the data
setwd("F:/SRET/Projects/RProgramming")
data<-read.csv('chronic_kidney_disease_full.csv')
data
names(data)
head(data)
summary(data)
factor(data$class)
data<-subset(data,data$class!="ckd\t")
factor(data$class)
colSums(is.na(data))
data$age
#replacing the null values to standard NA value in all coloumn
library(dplyr)
library(ggplot2)
data<-subset(data,data$age!="?" || data$pcc !="?" || data$ba != "?" || data$htn !="?" || data$dm !="?" || data$cad != "?" || data$appet !="?" || data$pe != "?" || data$ane != "?")
data$age
kidneydata<-data %>% mutate(bp = replace(bp,bp=="?",NA))
kidneydata<-kidneydata %>% mutate(sg = replace(sg,sg=="?",NA))
kidneydata<-kidneydata %>% mutate(al = replace(al,al=="?",NA))
kidneydata<-kidneydata %>% mutate(su = replace(su,su=="?",NA))
kidneydata<-kidneydata %>% mutate(rbc = replace(rbc,rbc=="?",NA))
kidneydata<-kidneydata %>% mutate(pc = replace(pc,pc=="?",NA))
kidneydata<-kidneydata %>% mutate(pcc = replace(pcc,pcc=="?",NA))
kidneydata<-kidneydata %>% mutate(ba = replace(ba,ba=="?",NA))
kidneydata<-kidneydata %>% mutate(bgr = replace(bgr,bgr=="?",NA))
kidneydata<-kidneydata %>% mutate(bu = replace(bu,bu=="?",NA))
kidneydata<-kidneydata %>% mutate(sc = replace(sc,sc=="?",NA))
kidneydata<-kidneydata %>% mutate(sod = replace(sod,sod=="?",NA))
kidneydata<-kidneydata %>% mutate(pot = replace(pot,pot=="?",NA))
kidneydata<-kidneydata %>% mutate(hemo = replace(hemo,hemo=="?",NA))
kidneydata<-kidneydata %>% mutate(pcv = replace(pcv,pcv=="?",NA))
kidneydata<-kidneydata %>% mutate(wbcc = replace(wbcc,wbcc=="?",NA))
kidneydata<-kidneydata %>% mutate(rbcc= replace(rbcc,rbcc=="?",NA))
kidneydata<-kidneydata %>% mutate(htn= replace(htn,htn=="?",NA))
kidneydata<-kidneydata %>% mutate(dm = replace(dm,dm=="?",NA))
kidneydata<-kidneydata %>% mutate(cad = replace(cad,cad=="?",NA))
kidneydata<-kidneydata %>% mutate(appet = replace(appet,appet=="?",NA))
kidneydata<-kidneydata %>% mutate(pe = replace(pe,pe=="?",NA))
kidneydata<-kidneydata %>% mutate(ane = replace(ane,ane=="?",NA))
data<-kidneydata
data
colSums(is.na(data))


data$age<-as.numeric(data$age)
data$bp<-as.numeric(data$bp)
data$sg<-as.numeric(data$sg)
data$al<-as.numeric(data$al)
data$su<-as.numeric(data$su)
data$bgr<-as.numeric(data$bgr)
data$bu<-as.numeric(data$bu)
data$sc<-as.numeric(data$sc)
data$sod<-as.numeric(data$sod)
data$pot<-as.numeric(data$pot)
data$hemo<-as.numeric(data$hemo)
data$pcv<-as.numeric(data$pcv)
data$wbcc<-as.numeric(data$wbcc)
data$rbcc<-as.numeric(data$rbcc)

names(data)[names(data)=='age']<-'Age'
names(data)[names(data)=='bp']<-'Blood_Pressure'
names(data)[names(data)=='sg']<-'Specific_Gravity'
names(data)[names(data)=='al']<-'Albumin'
names(data)[names(data)=='su']<-'Sugar'
names(data)[names(data)=='rbc']<-'Red_Blood_Cells'
names(data)[names(data)=='pc']<-'Pus_Cell'
names(data)[names(data)=='pcc']<-'Pus_Cell_Dumps'
names(data)[names(data)=='ba']<-'Bacteria'
names(data)[names(data)=='bgr']<-'Blood_Glucose_Random'
names(data)[names(data)=='bu']<-'Blood_Urea'
names(data)[names(data)=='sc']<-'Serum_Creatinine'
names(data)[names(data)=='sod']<-'Sodium'
names(data)[names(data)=='pot']<-'Potassium'
names(data)[names(data)=='hemo']<-'Hemoglobin'
names(data)[names(data)=='pcv']<-'Packed_Cell_Volume'
names(data)[names(data)=='wbcc']<-'White_Blood_Cell_Count'
names(data)[names(data)=='rbcc']<-'Red_Blood_Cell_Count'
names(data)[names(data)=='htn']<-'Hypertension'
names(data)[names(data)=='dm']<-'Diabetes_Mellitus'
names(data)[names(data)=='cad']<-'Coronary_Artery_Disease'
names(data)[names(data)=='appet']<-'Appetite'
names(data)[names(data)=='pe']<-'Pedal_Edema'
names(data)[names(data)=='ane']<-'Anemia'
names(data)[names(data)=='class']<-'Class'
unique(data$Coronary_Artery_Disease)
data<-subset(data,data$Coronary_Artery_Disease!="\tno")
l_unique <- lapply(data,unique)
l_unique

data$Blood_Pressure[is.na(data$Blood_Pressure)]<-mean(data$Blood_Pressure,na.rm = TRUE)
#data$Specific_Gravity[is.na(data$Specific_Gravity)]<-median(data$Specific_Gravity,na.rm = TRUE)
#data$Albumin[is.na(data$Albumin)]<-median(data$Albumin,na.rm = TRUE)
#data$Sugar[is.na(data$Sugar)]<-which.max(data$Sugar)
data$Red_Blood_Cells[is.na(data$Red_Blood_Cells)]<-"normal"
data$Pus_Cell[is.na(data$Pus_Cell)]<-"normal"
#data$Pus_Cell_Dumps[is.na(data$Pus_Cell_Dumps)]<-'notpresent'
#data$Bacteria[is.na(data$Bacteria)]<-"notpresent"
#data$Blood_Glucose_Random[is.na(data$Blood_Glucose_Random)]<-median(data$Blood_Glucose_Random,na.rm = TRUE)
#data$Blood_Urea[is.na(data$Blood_Urea)]<-median(data$Blood_Urea,na.rm = TRUE)
#data$Serum_Creatinine[is.na(data$Serum_Creatinine)]<-median(data$Serum_Creatinine,na.rm = TRUE)
#data$Sodium[is.na(data$Sodium)]<-median(data$Sodium,na.rm = TRUE)
#data$Potassium[is.na(data$Potassium)]<-median(data$Potassium,na.rm = TRUE)
#data$Hemoglobin[is.na(data$Hemoglobin)]<-mean(data$Hemoglobin,na.rm = TRUE)
#data$Packed_Cell_Volume[is.na(data$Packed_Cell_Volume)]<-median(data$Packed_Cell_Volume,na.rm = TRUE)
#data$White_Blood_Cell_Count[is.na(data$White_Blood_Cell_Count)]<-median(data$White_Blood_Cell_Count,na.rm = TRUE)
#data$Red_Blood_Cell_Count[is.na(data$Red_Blood_Cell_Count)]<-median(data$Red_Blood_Cell_Count,na.rm = TRUE)
#data$Hypertension[is.na(data$Hypertension)]<-"no"
#data$Diabetes_Mellitus[is.na(data$Diabetes_Mellitus)]<-'no'
#data$Coronary_Artery_Disease[is.na(data$Coronary_Artery_Disease)]<-"no"
#data$Appetite[is.na(data$Appetite)]<-'good'
#data$Pedal_Edema[is.na(data$Pedal_Edema)]<-'yes'
#data$Anemia[is.na(data$Anemia)]<-'no'
colSums(is.na(data))
data$Class
class(data$Red_Blood_Cells)
summary(data)
str(data)
head(data,5)
colSums(is.na(data))


#graph
ggplot(data=data,aes(y=Class,x=Blood_Pressure))+geom_point() #Scatterplot
ggplot(data=data2,aes(x=class,fill=dm))+geom_bar()
ggplot(data=data2,aes(x=bgr,y=class))+geom_boxplot()
ggplot(data=data2,aes(x=bgr,y=class,fill=su))+geom_boxplot()
ggplot(data=data2,aes(y=class,x=al))+geom_smooth()
ggplot(data=data2,aes(y=class,x=pc))+geom_point()+geom_smooth(method='lm',se=F)
ggplot(data=data2,aes(x=bp,fill=class))+geom_bar(position = "fill")
ggplot(data=data2,aes(x=bp,fill=class))+geom_bar(position = "fill")


library(caTools)
#split <- sample.split(data$Class,SplitRatio = 0.80)
#traindata <- subset(data,split==TRUE)
#testdata <- subset(train,split == FALSE)
model<- glm(as.factor(Class) ~ Red_Blood_Cells,family="binomial",data=traindata)
summary(model)
predict <- predict(model,type="response")
predict
table(traindata$Class,predict>0.5)

