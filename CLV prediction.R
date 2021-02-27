options(scipen=999)

InputData=read.csv('C:/Users/ACER/Downloads/Fn-UseC_-Marketing-Customer-Value-Analysis.csv',na.strings = NULL,stringsAsFactors = TRUE)
View(InputData)
str(InputData)
InputData$Type.of.Open.Complaints=as.factor(InputData$Type.of.Open.Complaints)
InputData$Type.of.Policies=as.factor(InputData$Type.of.Policies)
str(InputData)

summary(InputData)

colSums(is.na(InputData))

InputData$Customer=NULL
InputData$Effective.To.Date=NULL



str(InputData)
Continuous_cols=c('Customer.Lifetime.Value','Income','Monthly.Premium.Auto','Months.Since.Last.Claim','Months.Since.Policy.Inception','Total.Claim.Amount')
sapply(InputData[,Continuous_cols],mean)
sapply(InputData[,Continuous_cols],median)

Categorical_cols=c('State','Response','Coverage','Education','EmploymentStatus','Gender','Location.Code','Marital.Status','Type.of.Open.Complaints','Policy.Type','Policy','Renew.Offer.Type','Vehicle.Class','Vehicle.Size','Sales.Channel','Type.of.Policies')
FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}
sapply(InputData[,Categorical_cols],FunctionMode)

Continuous_cols
par(mfrow=c(3,2))
library(RColorBrewer)
for (hist_cols in Continuous_cols){
  hist(InputData[,c(hist_cols)],main=paste('Histogram of:',hist_cols),
       col=brewer.pal(8,'Paired'))
}

Categorical_cols
par(mfrow=c(2,8))
for (bar_cols in Categorical_cols){
  barplot(table(InputData[,c(bar_cols)]),main=paste('Barplot of:',bar_cols),
          col=brewer.pal(8,'Paired'))
}

Continuous_cols
plot(InputData[,Continuous_cols],col='blue')

Categorical_cols
library(RColorBrewer)
par(mfrow=c(4,4))
for (bar_cols in Categorical_cols){
  boxplot(Customer.Lifetime.Value~ (InputData[,c(bar_cols)]), data=InputData,
          main=paste('Boxplot of:',bar_cols),col=brewer.pal(8,'Paired'))
}
boxplot(InputData$Total.Claim.Amount,horizontal =T)
quantiles=quantile(InputData$Total.Claim.Amount,c(0.98,0.99,0.999))
quantiles
quantiles_final=quantile(InputData$Total.Claim.Amount,0.9996)  
quantiles_final
max(InputData$Total.Claim.Amount)
InputData$Total.Claim.Amount=ifelse(InputData$Total.Claim.Amount>quantiles_final, quantiles_final,InputData$Total.Claim.Amount)
boxplot(InputData$Total.Claim.Amount,horizontal = T)
max(InputData$Total.Claim.Amount)





cor(InputData[, Continuous_cols], use = "complete.obs")

CorrData=cor(InputData[, Continuous_cols], use = "complete.obs")
CorrData
CorrData['Customer.Lifetime.Value',]
abs(CorrData['Customer.Lifetime.Value',])>0.2
names(CorrData[,'Customer.Lifetime.Value'][abs(CorrData[,'Customer.Lifetime.Value'])>0.2])

ColsForANOVA=c('State','Response','Coverage','Education','EmploymentStatus','Gender','Location.Code','Marital.Status','Type.of.Open.Complaints','Policy.Type','Policy','Renew.Offer.Type','Vehicle.Class','Vehicle.Size','Sales.Channel','Type.of.Policies')
for (catCol in ColsForANOVA){
  anovaData= InputData[, c('Customer.Lifetime.Value',catCol)]
  print(summary(aov(Customer.Lifetime.Value~.,data=anovaData)))
}

TargetVariableName=c('Customer.Lifetime.Value')
BestPredictorName=c('Type.of.Policies','Vehicle.Class','Renew.Offer.Type','Type.of.Open.Complaints','Marital.Status','EmploymentStatus','Education','Coverage','Monthly.Premium.Auto','Total.Claim.Amount')

TargetVariable=InputData[,c(TargetVariableName)]
str(TargetVariable)
PredictorVariables=InputData[,BestPredictorName]
str(PredictorVariables)

Data=data.frame(TargetVariable,PredictorVariables)
str(Data)
head(Data)

set.seed(123)
TrainingSampleIndex=sample(1:nrow(Data),size=0.7*nrow(Data))
DataTrain=Data[TrainingSampleIndex,]
DataTest=Data[-TrainingSampleIndex,]
dim(DataTrain)
dim(DataTest)

Model_Reg=lm(TargetVariable~.,data=DataTrain)
summary(Model_Reg)

Model_Reg_2=lm(TargetVariable~.-Vehicle.Class-Marital.Status-Total.Claim.Amount-Renew.Offer.Type,data=DataTrain)
summary(Model_Reg_2)

Model_Reg_3=lm(TargetVariable~.-Education,data=DataTrain)
summary(Model_Reg_3)

DataTest$Pred_LM=predict(Model_Reg_24,DataTest)
head(DataTest)

DataTest$LM_APE=100 *(abs(DataTest$TargetVariable-DataTest$Pred_LM/DataTest$TargetVariable))
head(DataTest)
MeanAPE=mean(DataTest$LM_APE)
MedianAPE=median(DataTest$LM_APE)
MeanAccuracyofLMmodel=100-MeanAPE
MedianAccuracyofLMmodel=100-MedianAPE

library(lmtest)
bptest(Model_Reg_24)

dwtest(Model_Reg_24)

library(nortest)
resid=Model_Reg_24$residuals
ad.test(resid)

library(car)
VIF=vif(Model_Reg_24)
data.frame(VIF)




