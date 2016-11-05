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
# Change type to factor for factor fields
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
# Display accuracies of model in decreasing order
print(paste0("Accuracy Table: "))
print.data.frame(accuracy_df[order(accuracy_df$accuracy,decreasing = TRUE),])
# Select the best model of the lot in terms of accuracy.
# If multiple models with same accuracy, select the first from the lot
best_model <- as.character(accuracy_df[accuracy_df$accuracy==max(accuracy_df$accuracy),"model"][1])
print(paste0("Highest accuracy achieved with: ",best_model))
# Need to check confusion matrix display
print(paste0("Confusion matrix for ",best_model,": ",model_output[[best_model]][2]))

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