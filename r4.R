#-------------data-vorbereitung----------
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)
data<-read.csv(file.choose())
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)
#-------Data-sauberen
better_data<-data%>%
 drop_na()
#--------Data-vergleichen------
str(data)
data$Churn=as.factor(data$Churn)
data$Gender <- as.factor(data$Gender)
data$ContractType <- as.factor(data$ContractType)
sum(is.na(data))

#-----------Data fur Training machen--------------
set.seed(123)
trainIndex<-createDataPartition(data$Churn,p=0.8,list=FALSE)
trainData<-data[trainIndex,]
testData<-data[-trainIndex,]
#--------------model-bauen und trenieren---------------------------------------
set.seed(123)
control<-trainControl(method="cv",number=10,verboseIter = TRUE)
model<-train(Churn~.,data=trainData,method="rf",trControl=control)
#---------------Model-evaluation-----------------------------
confusionMatrix(model)
varImp(model)
#----------------Model-testieren-------------------
new_data<-data.frame(CustomerID=101,Age=31,Gender="Male",Tenure=1,ProductUsage=6.6,ServiceCalls=7,ContractType ="One year" ,MonthlyCharges=60.6,TotalCharges =3000)
predictmodel<-predict(model,new_data)
#------------model----------------------------------------
saveRDS(model, "model.rds")
loaded_model <- readRDS("model.rds")
#-------evaluacija2--------------------------------
library(pROC)
predictions <- predict(model,newdata=testData, type = "prob")[,2]
roc_curve <- roc(testData$Churn, predictions)
plot(roc_curve)
