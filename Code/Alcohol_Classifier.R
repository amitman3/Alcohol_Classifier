library(MASS)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)

# Read input
input <- read.csv(file = "./student/student-mat.csv",header = TRUE,sep = ";")

# Check for NA
sum(is.na(input))

# Exploratory analysis of all aatributes

par(mfrow = c(2,2))
# test <- input
input[,c(7,8,13,14,24:29)] <- as.data.frame(sapply(X = input[,c(7,8,13,14,24:29)],function(z) as.factor(z)))
for (att in colnames(input)){
  att_class <- paste(att,class(input[,att]),sep = "_")
  #   print(class(input[,att]))
  print(att_class)
  #   boxplot(input[,att])
  if(class(input[,att])=="integer"){
    boxplot(x = input[,att],main=att)
  }
}

glimpse(input)

# ## Using k-fold cross validation
# k <- 5
# ## Assigning ids to each sample
# input$id <- sample(1:k,nrow(input),replace = TRUE)
# klist <- 1:k
# 
# ## Create empty data frames for predicted and testset values
# 
# predicted <- data.frame()
# testset <- data.frame()

# Partition data into test and train

inTrain <- createDataPartition(input$Dalc,p = 0.75,list = FALSE)
training <- input[inTrain,]
testing <- input[-inTrain,]



train_control <- trainControl(method = "cv",number = 10,savePredictions = TRUE)
# Set seed to control for resampling and generate reproducible results
set.seed(seed = 123)

modelrf <- train(training$Dalc~.,data = training,trControl = train_control,method = "rf")
# modellinsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "lssvmLinear")
# modelpolysvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "lssvmPoly")
# modelradsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "lssvmRadial")
modellinsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "svmLinear")
modelpolysvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "svmPoly")
# modelradsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "svmRadial")
modelnnet <- train(training$Dalc~.,data = training,trControl = train_control,method = "avNNet")
modellda <- train(training$Dalc~.,data = training,trControl = train_control,method = "lda2")
modelmnom <- train(training$Dalc~.,data = training,trControl = train_control,method = "multinom")
# modelxgbLin <- train(training$Dalc~.,data = training,trControl = train_control,method = "xgbLinear")

# Random forest stats
rffit <- predict.train(modelrf,newdata = testing)
rfprob <- predict.train(modelrf,newdata = testing,type = "prob")
head(rffit)
confusionMatrix(data = rffit,testing$Dalc)

# Neural network stats
nnetfit <- predict.train(modelnnet,newdata = testing)
nnetprob <- predict.train(modelnnet,newdata = testing,type = "prob")
head(nnetfit)
confusionMatrix(data = nnetfit,testing$Dalc)

# lda stats
ldafit <- predict.train(modellda,newdata = testing)
ldaprob <- predict.train(modellda,newdata = testing,type = "prob")
head(ldafit)
confusionMatrix(data = ldafit,testing$Dalc)

# Multinom stats
mnomfit <- predict.train(modelmnom,newdata = testing)
mnomprob <- predict.train(modelmnom,newdata = testing,type = "prob")
head(mnomfit)
confusionMatrix(data = mnomfit,testing$Dalc)

# Linear SVM stats
lsvmfit <- predict.train(modellinsvm,newdata = testing)
lsvmprob <- predict.train(modellinsvm,newdata = testing,type = "prob")
head(lsvmfit)
confusionMatrix(data = lsvmfit,testing$Dalc)

# Polynomial SVM stats
polysvmfit <- predict.train(modelpolysvm,newdata = testing)
psvmprob <- predict.train(modelpolysvm,newdata = testing,type = "prob")
head(polysvmfit)
confusionMatrix(data = polysvmfit,testing$Dalc)

## Radial SVM stats
# radsvmfit <- predict.train(modelradsvm,newdata = testing)
# rsvmprob <- predict.train(modelradsvm,newdata = testing,type = "prob")
# head(radsvmfit)
# confusionMatrix(data = radsvmfit,testing$Dalc)

## Extreme Gradient Boosting stats
# xgbfit <- predict.train(modelxgbLin,newdata = testing)
# xgbprob <- predict.train(modelxgbLin,newdata = testing,type = "prob")
# head(xgbfit)
# confusionMatrix(data = xgbfit,testing$Dalc)

# =============================================================================================
# Classification based on Weekend alcohol consumption

modelrf <- train(training$Walc~.,data = training,trControl = train_control,method = "rf")
# modellinsvm <- train(training$Walc~.,data = training,trControl = train_control,method = "lssvmLinear")
# modelpolysvm <- train(training$Walc~.,data = training,trControl = train_control,method = "lssvmPoly")
# modelradsvm <- train(training$Walc~.,data = training,trControl = train_control,method = "lssvmRadial")
modellinsvm <- train(training$Walc~.,data = training,trControl = train_control,method = "svmLinear")
modelpolysvm <- train(training$Walc~.,data = training,trControl = train_control,method = "svmPoly")
modelradsvm <- train(training$Walc~.,data = training,trControl = train_control,method = "svmRadial")
modelnnet <- train(training$Walc~.,data = training,trControl = train_control,method = "avNNet")
modellda <- train(training$Walc~.,data = training,trControl = train_control,method = "lda2")
modelmnom <- train(training$Walc~.,data = training,trControl = train_control,method = "multinom")
# modelxgbLin <- train(training$Walc~.,data = training,trControl = train_control,method = "xgbLinear")

# Random forest stats
rffit <- predict.train(modelrf,newdata = testing)
confusionMatrix(data = rffit,testing$Walc)

lsvmfit <- predict.train(modellinsvm,newdata = testing)
confusionMatrix(data = lsvmfit,testing$Walc)

psvmfit <- predict.train(modelpolysvm,newdata = testing)
confusionMatrix(data = psvmfit,testing$Walc)

rsvmfit <- predict.train(modelradsvm,newdata = testing)
confusionMatrix(data = rsvmfit,testing$Walc)

nnetfit <- predict.train(modelnnet,newdata = testing)
confusionMatrix(data = nnetfit,testing$Walc)

ldafit <- predict.train(modellda,newdata = testing)
confusionMatrix(data = ldafit,testing$Walc)

mnomfit <- predict.train(modelmnom,newdata = testing)
confusionMatrix(data = mnomfit,testing$Walc)
regressor<- "Dalc"

function classify(regressor){
  train_control <- trainControl(method = "cv",number = 10,savePredictions = TRUE)
  # Set seed to control for resampling and generate reproducible results
  set.seed(seed = 123)
  modelrf <- train(training[,regressor]~.,data = training,trControl = train_control,method = "rf")
#   modelrf <- train(Dalc~.,data = training,trControl = train_control,method = "rf")
  # modellinsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "lssvmLinear")
  # modelpolysvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "lssvmPoly")
  # modelradsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "lssvmRadial")
  modellinsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "svmLinear")
  modelpolysvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "svmPoly")
  # modelradsvm <- train(training$Dalc~.,data = training,trControl = train_control,method = "svmRadial")
  modelnnet <- train(training$Dalc~.,data = training,trControl = train_control,method = "avNNet")
  modellda <- train(training$Dalc~.,data = training,trControl = train_control,method = "lda2")
  modelmnom <- train(training$Dalc~.,data = training,trControl = train_control,method = "multinom")
  # modelxgbLin <- train(training$Dalc~.,data = training,trControl = train_control,method = "xgbLinear")
}