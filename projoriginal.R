setwd("F:/SRET/Projects/RProgramming")
library(dplyr)
library(ggplot2)
library(caret)
library(magrittr)
library(party)
library(caTools)
#importing the dataset
kidneydata<-read.csv('chronic_kidney_disease_full.csv',stringsAsFactors = T)
#to keep Class col as string
kidneydata$class<-as.character(kidneydata$class)
head(kidneydata)
tail(kidneydata)
str(kidneydata)
summary(kidneydata)
#exploring the dataset
kidneydata
names(kidneydata)
#viewing first few rows of the dataset
head(kidneydata)
#checking about all the columns
summary(kidneydata)
#checking unique values of each column
l_unique <- lapply(kidneydata,unique)
l_unique
#dropping unwanted rows which consist invalid data
kidneydata<-subset(kidneydata,kidneydata$appet!="no")
kidneydata<-subset(kidneydata,kidneydata$rbcc !="\t?")
kidneydata<-subset(kidneydata,kidneydata$class !="ckd\t")
#correcting typing mistake
kidneydata<-kidneydata %>% mutate(dm = replace(dm,dm==" yes","yes")) %>% mutate(dm = replace(dm,dm=="\tyes","yes")) %>% mutate(dm = replace(dm,dm=="\tno","no")) %>% mutate(cad = replace(cad,cad=="\tno","no")) %>% mutate(cad = replace(cad,cad=="\tyes","yes"))
#replacing the invalid missing values with NA
colSums(is.na(kidneydata))
#dropping all rows which has missing age
kidneydata<-subset(kidneydata,kidneydata$age!="?")
kidneydata<-kidneydata %>% mutate(bp = replace(bp,bp=="?",NA))
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

#checking the total number of null in each coloumns
#converting character data which are suitable for numeric
kidneydata$age<-as.numeric(kidneydata$age)
kidneydata$bp<-as.numeric(kidneydata$bp)
kidneydata$sg<-as.numeric(kidneydata$sg)
kidneydata$al<-as.numeric(kidneydata$al)
kidneydata$su<-as.numeric(kidneydata$su)
kidneydata$bgr<-as.numeric(kidneydata$bgr)
kidneydata$bu<-as.numeric(kidneydata$bu)
kidneydata$sc<-as.numeric(kidneydata$sc)
kidneydata$sod<-as.numeric(kidneydata$sod)
kidneydata$pot<-as.numeric(kidneydata$pot)
kidneydata$hemo<-as.numeric(kidneydata$hemo)
kidneydata$pcv<-as.numeric(kidneydata$pcv)
kidneydata$wbcc<-as.numeric(kidneydata$wbcc)
kidneydata$rbcc<-as.numeric(kidneydata$rbcc)
#renaming all the coloumns for readability
names(kidneydata)[names(kidneydata)=='age']<-'Age'
names(kidneydata)[names(kidneydata)=='bp']<-'Blood_Pressure'
names(kidneydata)[names(kidneydata)=='sg']<-'Specific_Gravity'
names(kidneydata)[names(kidneydata)=='al']<-'Albumin'
names(kidneydata)[names(kidneydata)=='su']<-'Sugar'
names(kidneydata)[names(kidneydata)=='rbc']<-'Red_Blood_Cells'
names(kidneydata)[names(kidneydata)=='pc']<-'Pus_Cell'
names(kidneydata)[names(kidneydata)=='pcc']<-'Pus_Cell_Dumps'
names(kidneydata)[names(kidneydata)=='ba']<-'Bacteria'
names(kidneydata)[names(kidneydata)=='bgr']<-'Blood_Glucose_Random'
names(kidneydata)[names(kidneydata)=='bu']<-'Blood_Urea'
names(kidneydata)[names(kidneydata)=='sc']<-'Serum_Creatinine'
names(kidneydata)[names(kidneydata)=='sod']<-'Sodium'
names(kidneydata)[names(kidneydata)=='pot']<-'Potassium'
names(kidneydata)[names(kidneydata)=='hemo']<-'Hemoglobin'
names(kidneydata)[names(kidneydata)=='pcv']<-'Packed_Cell_Volume'
names(kidneydata)[names(kidneydata)=='wbcc']<-'White_Blood_Cell_Count'
names(kidneydata)[names(kidneydata)=='rbcc']<-'Red_Blood_Cell_Count'
names(kidneydata)[names(kidneydata)=='htn']<-'Hypertension'
names(kidneydata)[names(kidneydata)=='dm']<-'Diabetes_Mellitus'
names(kidneydata)[names(kidneydata)=='cad']<-'Coronary_Artery_Disease'
names(kidneydata)[names(kidneydata)=='appet']<-'Appetite'
names(kidneydata)[names(kidneydata)=='pe']<-'Pedal_Edema'
names(kidneydata)[names(kidneydata)=='ane']<-'Anemia'
names(kidneydata)[names(kidneydata)=='class']<-'Class'
colSums(is.na(kidneydata))

#replacing all the missing values with appropriate values
kidneydata$Blood_Pressure
data<-kidneydata
data$Blood_Pressure
data$Blood_Pressure[is.na(data$Blood_Pressure)]<-mean(data$Blood_Pressure,na.rm = TRUE)
data$Specific_Gravity[is.na(data$Specific_Gravity)]<-median(data$Specific_Gravity,na.rm = TRUE)
data$Albumin[is.na(data$Albumin)]<-median(data$Albumin,na.rm = TRUE)
data$Sugar[is.na(data$Sugar)]<-0
data$Red_Blood_Cells[is.na(data$Red_Blood_Cells)]<-"normal"
data$Pus_Cell[is.na(data$Pus_Cell)]<-"normal"
data$Pus_Cell_Dumps[is.na(data$Pus_Cell_Dumps)]<-'notpresent'
data$Bacteria[is.na(data$Bacteria)]<-"notpresent"
data$Blood_Glucose_Random[is.na(data$Blood_Glucose_Random)]<-median(data$Blood_Glucose_Random,na.rm = TRUE)
data$Blood_Urea[is.na(data$Blood_Urea)]<-median(data$Blood_Urea,na.rm = TRUE)
data$Serum_Creatinine[is.na(data$Serum_Creatinine)]<-median(data$Serum_Creatinine,na.rm = TRUE)
data$Sodium[is.na(data$Sodium)]<-median(data$Sodium,na.rm = TRUE)
data$Potassium[is.na(data$Potassium)]<-median(data$Potassium,na.rm = TRUE)
data$Hemoglobin[is.na(data$Hemoglobin)]<-mean(data$Hemoglobin,na.rm = TRUE)
data$Packed_Cell_Volume[is.na(data$Packed_Cell_Volume)]<-median(data$Packed_Cell_Volume,na.rm = TRUE)
data$White_Blood_Cell_Count[is.na(data$White_Blood_Cell_Count)]<-median(data$White_Blood_Cell_Count,na.rm = TRUE)
data$Red_Blood_Cell_Count[is.na(data$Red_Blood_Cell_Count)]<-median(data$Red_Blood_Cell_Count,na.rm = TRUE)
data$Hypertension[is.na(data$Hypertension)]<-"no"
data$Diabetes_Mellitus[is.na(data$Diabetes_Mellitus)]<-'no'
data$Coronary_Artery_Disease[is.na(data$Coronary_Artery_Disease)]<-"no"
data$Appetite[is.na(data$Appetite)]<-'good'
data$Pedal_Edema[is.na(data$Pedal_Edema)]<-'no'
data$Anemia[is.na(data$Anemia)]<-'no'
colSums(is.na(kidneydata))

#Visualising
kidneydata<-data
kidneydata$Sugar
head(kidneydata)
boxplot(kidneydata$Potassium)
ggplot(data=kidneydata,aes(y=Class,x=Blood_Pressure))+geom_point() #Scatterplot
ggplot(data=kidneydata,aes(x=Class,fill=Diabetes_Mellitus))+geom_bar()
ggplot(data=kidneydata,aes(y=Class,x=Albumin))+geom_smooth()
ggplot(data=kidneydata,aes(x=Age,fill=Class))+geom_histogram(bins = 30)
ggplot(data = kidneydata,aes(x=Appetite,fill=Class))+geom_bar(position = "stack")
library("vioplot")
vioplot(kidneydata$Potassium)
obj1<- ggplot(data=kidneydata,aes(x=factor(Class),y=Blood_Glucose_Random,fill=factor(Class)))+geom_boxplot()
obj2<- obj1+labs(title="Blood Glucose vs CKD",x='Class',fill='Bacteria')
obj2+theme(panel.background=element_rect(fill='palegreen'))->obj3
obj3+theme(plot.title=element_text(hjust=0.5,face="bold",colour='cadetblue')) ->obj4
obj4
#
y<-dnorm(kidneydata$Age,mean(kidneydata$Age),sd(kidneydata$Age))

plot(kidneydata,y)
png(file="dnormExample.png")




#model
#fiting the model using glm and predicting using pred

kidneydata<-kidneydata %>% mutate(Class = replace(Class,Class=="ckd",1)) %>% mutate(Class = replace(Class,Class=="notckd",0))
kidneydata$Class<-as.numeric(kidneydata$Class)
kidneydata$Class
numeric_col<-Filter(is.numeric,kidneydata)
numeric_col$Class
train.index <- createDataPartition(kidneydata$Class, p = .7, list = FALSE)
train <- kidneydata[ train.index,]
test  <- kidneydata[-train.index,]
train.index <- createDataPartition(numeric_col$Class, p = .7, list = FALSE)
train <- numeric_col[ train.index,]
test  <- numeric_col[-train.index,]
train
count(train,Class,sort=T)
count(test,Class,sort=T)
colnames(train)
formula <- as.factor(Class)~Blood_Pressure+Specific_Gravity+Albumin+Sugar+Blood_Glucose_Random+Blood_Urea+Serum_Creatinine+Sodium+Potassium+Hemoglobin+Packed_Cell_Volume+White_Blood_Cell_Count+Red_Blood_Cell_Count+Age
treemodel<-ctree(formula,train)
plot(treemodel,pretty=0.5)
summary(treemodel)
predict_model <- predict(treemodel,test)
cfm<-table(test$Class,predict_model)
cfm
ac<-sum(diag(cfm))/sum(cfm)
ac  
#logistic Regression
model <- glm(Class~.,family = "binomial",data = train,maxit=100)
summary(model)

res<-predict(model,test,type="response")
head(res)

glm.pred<-ifelse(res>0.5,1,0)
table(glm.pred,test$Class)

mean(glm.pred==test$Class)

dev.cur()
dev.off(6)
dev.cur()
