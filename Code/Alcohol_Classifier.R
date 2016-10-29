library(MASS)
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)

# Read input
input <- read.csv(file = "./Data/student/student-mat.csv",header = TRUE,sep = ";")

# Read list of classifiers to try
classifiers <- readLines(con = "./Data/caret_classifier_list.txt")

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

# Partition data into test and train

inTrain <- createDataPartition(input$Dalc,p = 0.75,list = FALSE)
training <- input[inTrain,]
testing <- input[-inTrain,]

# Set response variable for model
response <- "Dalc"
train_control <- trainControl(method = "cv",number = 10,savePredictions = TRUE)
# Set seed to control for resampling and generate reproducible results
set.seed(seed = 123)
# Create empty final accuracy list
model_output <- list()
accuracy_df <- data.frame(classifiers,0) 
colnames(accuracy_df) <- c("model","accuracy")
# Call model builder for each classifier
# Add progress bar
for (classifier in classifiers){
  print(paste0("Building model for: ",classifier))
  #   Build the model
  model <- buildModel(response,classifier)
  print(paste0("Completed model for: ",classifier))
  
  print(paste0("Predicting using ",classifier,"..."))
  # Predict using test data
  modelfit <- predict.train(model,newdata = testing)
  # Generate confusion matrix for comparison and accuracy
  cMat <- confusionMatrix(data = modelfit,testing$Dalc)
  #   model_output[[classifier]] <- list(cMat$overall[1])
  accuracy_df[accuracy_df$model==classifier,"accuracy"] <- round(cMat$overall[1],digits = 3)
  model_output[[classifier]] <- list(unlist(model_output[[classifier]]),cMat$table)
  print(paste0("Completed prediction using ",classifier))
}
model_output[["model_accuracy"]] <- accuracy_df
best_model <- as.character(accuracy_df[accuracy_df$accuracy==max(accuracy_df$accuracy),"model"])
print(paste0("Highest accuracy achieved with: ",best_model))
# Need to check confusion matrix display
print(paste0("Confusion matrix for ",best_model,": ",model_output[[best_model]][2]))


modelrf <- train(Dalc~.,data = training,trControl = train_control,method = "rf")
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
response <- "Dalc"
classifier <- "rf"

buildModel <- function(response,classifier){
  #   Setting train control for 10 fold cross validation
  train_control <- trainControl(method = "cv",number = 10,savePredictions = TRUE)
  # Set seed to control for resampling and generate reproducible results
  set.seed(seed = 123)
  #   build model on response for selected classifier
  fmla <- as.formula(paste0(response,"~."))
  #   built_model <- train(training[,response]~.,data = training,trControl = train_control,method = classifier)
  #   built_model <- train(training[,response]~training[,!names(training) %in% response],trControl = train_control,method = classifier)
  built_model <- train(fmla,data = training,trControl = train_control,method = classifier)
  return(built_model)
}