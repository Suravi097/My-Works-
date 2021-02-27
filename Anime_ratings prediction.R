#not allowing R to use scientific notations
options(scipen=999)

#importing the raw dataset in R
InputData=read.csv('C:/Users/ACER/Downloads/Anime_Final.csv',na.strings = c('','NA','NULL'),stringsAsFactors = TRUE)
View(InputData)
str(InputData)
head(InputData)

#dropping useless columns
InputData$title=NULL
InputData$description=NULL
InputData$tags=NULL
InputData$studios=NULL
InputData$contentWarn=NULL

head(InputData)
str(InputData)

#checking for missing values
colSums(is.na(InputData))

length(unique(InputData$sznOfRelease))
length(unique(InputData$duration))

#dropping the sznofRelease col due to too many missing values
InputData$sznOfRelease=NULL


#since duration and watched are continuous in nature, replacing the NA values with the Median
InputData$duration[is.na(InputData$duration)]=median(InputData$duration,na.rm=TRUE)
#since watched denotes no of ppl, rounding of the median value
InputData$watched[is.na(InputData$watched)]=round(median(InputData$watched,na.rm=TRUE),digits=0)
colSums(is.na(InputData))

#since the mediaType col is categorical in nature,replacing the NA values with the MODE
FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}
FunctionMode(InputData$mediaType)
InputData$mediaType[is.na(InputData$mediaType)]="TV"
colSums(is.na(InputData))

str(InputData)

#Measures of Central Tendency for continuous variables
Continuous_cols=c('eps','duration','watched','watching','wantWatch','dropped','rating','votes')
sapply(InputData[,Continuous_cols],mean)
sapply(InputData[,Continuous_cols],median)

#Measures of central tendency for categorical variables
Categorical_cols=c('mediaType','ongoing')
sapply(InputData[,Categorical_cols],FunctionMode)

#creating histogram for continuous cols
ColsForHist= c('eps','duration','watched','watching','wantWatch','dropped','rating','votes')
par(mfrow=c(2,4))
library(RColorBrewer)
for (ContC in ColsForHist){
  hist(InputData[,c(ContC)],main=paste('Histogram of:',ContC),
       col=brewer.pal(8,'Paired'))
}

#creating Barplots for categorical cols
ColsForBar=c('mediaType','ongoing')
par(mfrow=c(2,1))
library(RColorBrewer)
for (CatCol in ColsForBar){
  barplot(table(InputData[,c(CatCol)]),main=paste('Barplot of:',ColsForBar),
          col=brewer.pal(8,'Paired'))
}

##bivariate analysis
#Visual relationship between predictors and target variable
#Continuous vs continuous - scatter plot
#Continuous vs categorical - boxplot

#Continuous vs continuous visual analysis - Scatterplot
Continuous_cols
plot(InputData[,Continuous_cols],col='blue')

#Continuous vs categorical visual analysis - boxplot
Categorical_cols
library(RColorBrewer)
par(mfrow=c(1,2))
for (barCols in Categorical_cols){
  boxplot(rating~ (InputData[,c(barCols)]),data=InputData,main=paste('Boxplot of:',barCols),
          col=brewer.pal(8,'Paired'))
}

#treatment of outliers
boxplot(InputData$eps,horizontal = T)
quantiles_1=quantile(InputData$eps,c(0.99929,0.999301))
quantiles_1
quantiles_final_1=quantile(InputData$eps,0.999301)
quantiles_final_1
max(InputData$eps)
InputData$eps= ifelse(InputData$eps > quantiles_final_1, quantiles_final_1, InputData$eps)
boxplot(InputData$eps,horizontal = T)
quantiles_2=quantile(InputData$eps,c(0.99819,0.9982))
quantiles_2
quantiles_final_2=quantile(InputData$eps,0.99819)
quantiles_final_2
InputData$eps= ifelse(InputData$eps > quantiles_final_2, quantiles_final_2, InputData$eps)
boxplot(InputData$eps,horizontal = T)
quantiles_3=quantile(InputData$eps,c(0.99775,0.9978))
quantiles_3
quantiles_final_3=quantile(InputData$eps,0.99775)
quantiles_final_3
max(InputData$eps)
InputData$eps= ifelse(InputData$eps > quantiles_final_3, quantiles_final_3, InputData$eps)
boxplot(InputData$eps,horizontal = T)
quantiles_4=quantile(InputData$eps,c(0.997,0.99719))
quantiles_4
quantiles_final_4=round(quantile(InputData$eps,0.99719),digits = 0)
quantiles_final_4
InputData$eps=ifelse(InputData$eps > quantiles_final_4, quantiles_final_4, InputData$eps)
boxplot(InputData$eps,horizontal = T)


boxplot(InputData$watched,horizontal = T)
quantiles_5=quantile(InputData$watched,c(0.99929,0.99930))
quantiles_5
quantiles_final_5=round(quantile(InputData$watched,0.99929),digits = 0)
quantiles_final_5
InputData$watched= ifelse(InputData$watched > quantiles_final_5, quantiles_final_5, InputData$watched)
boxplot(InputData$watched,horizontal = T)


boxplot(InputData$watching,horizontal = T)
quantiles_6=quantile(InputData$watching,c(0.99886,0.99888))
quantiles_6
quantiles_final_6=quantile(InputData$watching,0.99888)
quantiles_final_6
max(InputData$watching)
InputData$watching= ifelse(InputData$watching > quantiles_final_6, quantiles_final_6, InputData$watching)
boxplot(InputData$watching,horizontal = T)
quantiles_7=quantile(InputData$watching,c(0.9982,0.9985))
quantiles_7
quantiles_final_7=round(quantile(InputData$watching,0.9982),digits = 0)
quantiles_final_7
InputData$watching= ifelse(InputData$watching > quantiles_final_7, quantiles_final_7, InputData$watching)
boxplot(InputData$watching,horizontal = T)


boxplot(InputData$wantWatch,horizontal = T)
quantiles_8=quantile(InputData$wantWatch,c(0.99949,0.99950))
quantiles_8
quantiles_final_8=quantile(InputData$wantWatch,0.99949)
quantiles_final_8
max(InputData$wantWatch)
InputData$wantWatch= ifelse(InputData$wantWatch > quantiles_final_8, quantiles_final_8, InputData$wantWatch)
boxplot(InputData$wantWatch,horizontal = T)
quantiles_9=quantile(InputData$wantWatch,c(0.99942,0.99950))
quantiles_9
quantiles_final_9=round(quantile(InputData$wantWatch,0.99950),digits = 0)
quantiles_final_9
max(InputData$wantWatch)
InputData$wantWatch= ifelse(InputData$wantWatch > quantiles_final_9, quantiles_final_9, InputData$wantWatch)
boxplot(InputData$wantWatch,horizontal = T)


boxplot(InputData$dropped,horizontal = T)
quantiles_10=quantile(InputData$dropped,c(0.9991,0.999333))
quantiles_10
quantiles_final_10=quantile(InputData$dropped,0.999333)
quantiles_final_10
max(InputData$dropped)
InputData$dropped= ifelse(InputData$dropped > quantiles_final_10, quantiles_final_10, InputData$dropped)
boxplot(InputData$dropped,horizontal = T)
quantiles_11=quantile(InputData$dropped,c(0.9988,0.9989))
quantiles_11
quantiles_final_11= round(quantile(InputData$dropped,0.9989),digits = 0)
quantiles_final_11
InputData$dropped= ifelse(InputData$dropped > quantiles_final_11, quantiles_final_11, InputData$dropped)
boxplot(InputData$dropped, horizontal = T)


boxplot(InputData$votes,horizontal = T)
quantiles_12=quantile(InputData$votes,c(0.999455,0.9996))
quantiles_12
quantiles_final_12=quantile(InputData$votes,0.999455)
quantiles_final_12
InputData$votes= ifelse(InputData$votes > quantiles_final_12, quantiles_final_12, InputData$votes)
boxplot(InputData$votes,horizontal = T)
quantiles_13=quantile(InputData$votes,c(0.99881,0.99912))
quantiles_13
quantiles_final_13=quantile(InputData$votes,0.99912)
quantiles_final_13
InputData$votes= ifelse(InputData$votes > quantiles_final_13, quantiles_final_13, InputData$votes)
boxplot(InputData$votes,horizontal = T)
quantiles_14=quantile(InputData$votes,c(0.99886,0.9989))
quantiles_14
quantiles_final_14=quantile(InputData$votes,0.99886)
quantiles_final_14
InputData$votes= ifelse(InputData$votes > quantiles_final_14, quantiles_final_14, InputData$votes)
boxplot(InputData$votes,horizontal = T)


#taking log transformations for the cols watched & votes
hist(InputData$watched)
cor(x=InputData$watched, y=InputData$rating)
min(InputData$watched)
InputData$watched[InputData$watched==0]=1
hist(log(InputData$watched))
cor(x=log(InputData$watched), y=InputData$rating)

hist(InputData$votes)
cor(x=InputData$votes, y=InputData$rating)
min(InputData$votes)
hist(log(InputData$votes))
cor(x=log(InputData$votes), y=InputData$rating)

#creating new cols after taking log transformations
InputData$watched_new = log(InputData$watched)
InputData$watched = NULL

InputData$votes_new = log(InputData$votes)
InputData$votes = NULL


##strength of relationship between predictors and target variable
#continuous vs continuous - correlation test
#continuous vs categorical - ANOVA test

#Correlation test
Continuous_cols_1 = c('eps','duration','watched_new','watching','wantWatch','dropped','rating','votes_new')
cor(InputData[,Continuous_cols_1],use='complete.obs')
CorrData=cor(InputData[,Continuous_cols_1],use='complete.obs')
CorrData
names(CorrData[,'rating'][abs(CorrData[,'rating'])>0.40])

#ANOVA test
Categorical_cols
ColsForAnova=c('mediaType','ongoing')
for (k in ColsForAnova){
  anovaData=InputData[, c('rating',k)]
  print(summary(aov(rating~.,data=anovaData)))
}

#selecting both the categorical cols since both are significant based on the p-value

#######################################
############### TASK 2 ################
#######################################

#generating the data frame for machine learning
TargetVariableName=c('rating')

#choosing the predictors which may have a relationship with the target variable based on EDA
BestPredictorName= c('watched_new','wantWatch','votes_new','mediaType','ongoing')

TargetVariable=InputData[,c(TargetVariableName)]
str(TargetVariable)

PredictorVariable=InputData[,BestPredictorName]
str(PredictorVariable)

#creating the final data to be used for linear modeling
DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

#splitting the data into 70% for training and 30% for testing
set.seed(123)
TrainingSample=sample(1:nrow(DataForML),size=0.7*nrow(DataForML))
DataForMLTrain=DataForML[TrainingSample,]
DataForMLTest=DataForML[-TrainingSample,]
head(DataForMLTest)
dim(DataForMLTrain)
dim(DataForMLTest)

## Linear Regression
Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)

Model_Reg_2=lm(TargetVariable~ watched_new+wantWatch+votes_new+I(mediaType=='Movie')+I(mediaType=='Music Video')+I(mediaType=='Other')+I(mediaType=='OVA')+I(mediaType=='TV Special')+I(mediaType=='Web')+ongoing,data = DataForMLTrain)
summary(Model_Reg_2)

Model_Reg_3=lm(TargetVariable~ watched_new+wantWatch+votes_new+I(mediaType=='Movie')+I(mediaType=='Other')+I(mediaType=='OVA')+I(mediaType=='TV Special')+I(mediaType=='Web')+ongoing,data = DataForMLTrain)
summary(Model_Reg_3)

#Checking for the presence of Multicollinearity
library(car)
VIF=vif(Model_Reg_3)
data.frame(VIF)

#since the vif of the continuous cols watched_new and votes_new is very high, dropping those variables
Model_Reg_4=lm(TargetVariable~ wantWatch+I(mediaType=='Movie')+I(mediaType=='Other')+I(mediaType=='OVA')+I(mediaType=='TV Special')+I(mediaType=='Web')+ongoing,data = DataForMLTrain)
summary(Model_Reg_4)

#checking accuracy of model on testing data
DataForMLTest$Pred_LM=predict(Model_Reg_4,DataForMLTest)
head(DataForMLTest)

#calculating the Absolute Percentage Error for each prediction
DataForMLTest$LM_APE=100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))


##### Business Recommendation #####
# after analyzing the data set using Linear Regression algorithm, i have come to the conclusion that the no of ppl who want to watch have a positive effect on the ratings. 
# the ongoing animes have a positive effect on the ratings.
# the media type also have an effect on the ratings
# the production companies should definitely focus more on the above mentioned variables which will help in improving the ratings




