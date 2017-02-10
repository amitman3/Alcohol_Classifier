source("./Code/reqd_packages.R")
pkgs <- c("MASS","ggplot2","dplyr","caret","doParallel","Matrix",
          "ROCR","ROSE","DMwR")
# Use sourced script to install and load required packages
reqd_packages(pkgs)

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

# Scatterplot matrix of numeric attributes
featurePlot(x = training[,c(3,15,30:33)],y = training[,27],plot = "pairs")
glimpse(input)

# Partition data into test and train

inTrain <- createDataPartition(input$Dalc,p = 0.75,list = FALSE)
training <- input[inTrain,]
testing <- input[-inTrain,]

# Set response variable for model
response <- "Dalc"
fmla <- as.formula(paste0(response,"~."))
class_imbal <- FALSE
# Trying to handle imbalanced classification by over, under and synthetic sampling using SMOTE and ROSE
# Under sampling
train_down <- downSample(x = training[,!colnames(training) %in% response],y = training[,response],yname = response)
# Over sampling
train_up <- upSample(x = training[,!colnames(training) %in% response],y = training[,response],yname = response)

table_response <- table(training[,response])
class_major <- names(table_response)[which(table_response %in% max(table_response))]
imbal_test <- (max(table_response)/nrow(training))*length(table_response)
if(imbal_test>1.2){
  cat("Class Imbalance Detected!\nClass",class_major,"dominating the overall class representation.")
  class_imbal <- TRUE
}

minor_class_list <- names(table_response[!names(table_response) %in% class_major])
paired_subsets <- sapply(X = paste(minor_class_list,class_major,sep = "_"),simplify = FALSE,USE.NAMES=TRUE, function(z) {
  subset_name <- paste0("sub_",class_major,z)
  #   paste0(subset_name) <- training[training[,response] %in% c(class_major,z),]
  #   assign(paste(subset_name),training[training[,response] %in% c(class_major,z),])
  min <-strsplit(x = z,split = "_",fixed = TRUE)[[1]][1]
  subset <- training[training[,response] %in% c(class_major,min),]
  subset[,response] <- as.factor(as.character(subset[,response]))
  return(subset)
})

# SMOTE sampling
# sub_12 <- training[training[,response] %in% c(1,2),]
# sub_12[,response] <- as.character(sub_12[,response])
# sub_12[,response] <- as.factor(sub_12[,response])
set.seed(123)
train_smote12 <- SMOTE(form = fmla,data = sub_12,perc.over = 150,k = 5,perc.under = 300)

set.seed(123)
train_smotebc <- SMOTE(form = fmla,data = sub_bc,perc.over = 400,k = 5,perc.under = 160)

train_smote <- rbind(train_smoteac,train_smotebc)

# ROSE sampling
set.seed(123)
train_roseac <- ROSE(formula = fmla,data = sub_ac,p = 0.4)
train_rosebc <- ROSE(formula = fmla,data = sub_bc,p = 0.4)
train_rose <- rbind(train_roseac$data,train_rosebc$data)



tuneLen <- 5
parallelism <- TRUE

# Create empty final accuracy list
model_output <- list()
accuracy_df <- data.frame(classifiers,0) 
colnames(accuracy_df) <- c("model","accuracy")
pb <- txtProgressBar(min = 0,max = length(classifiers),style = 3)
# Call model builder for each classifier
# Start timer
start <- proc.time()
for (classifier in classifiers){
  print(paste0("Building model for: ",classifier))
  #   Build the model
  model <- buildModel(response,classifier,tuneLen,parallelism)
  print(paste0("Completed model for: ",classifier))
  
  print(paste0("Predicting using ",classifier,"..."))
  # Predict using test data
  modelfit <- predict.train(model,newdata = testing)
  # Generate confusion matrix for comparison and accuracy
  cMat <- caret::confusionMatrix(data = modelfit,testing$Dalc)
  #   model_output[[classifier]] <- list(cMat$overall[1])
  accuracy_df[accuracy_df$model==classifier,"accuracy"] <- round(cMat$overall[1],digits = 3)
  model_output[[classifier]] <- list(unlist(model_output[[classifier]]),cMat$table)
  print(paste0("Completed prediction using ",classifier))
  setTxtProgressBar(pb = pb,value = match(classifier,classifiers),title = "Progress")
}
# Stop timer
end <- proc.time() - start
print(paste0("Time requried: ",end[3]))
model_output[["model_accuracy"]] <- accuracy_df
# Display accuracies of model in decreasing order
print(paste0("Accuracy Table: "))
print.data.frame(accuracy_df[order(accuracy_df$accuracy,decreasing = TRUE),])
# Select the best model of the lot in terms of accuracy.
# If multiple models with same accuracy, select the first from the lot
best_model <- as.character(accuracy_df[accuracy_df$accuracy==max(accuracy_df$accuracy),"model"][1])
print(paste0("Highest accuracy achieved with: ",best_model))
# Need to check confusion matrix display
print(paste0("Confusion matrix for ",best_model,": "))
print(model_output[[best_model]][2])

# =========================================================================================================
# Function to build model for selected response variable and type of classifier

buildModel <- function(input){
  train_data <- input
  #   One hot enconding for Extreme Gradient Boosting algorithms
  if(classifier=='xgbLinear'||classifier=='xgbTree'){
    sparse_matrix <- sparse.model.matrix(Customer.Type~.-1, data = train_data)
    head(sparse_matrix)
  }
  
  if(parallelism==TRUE){
    #   Set up number of parallel executors - Number of cores - 1 (Safety buffer so the machine doesn't hang up)
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl = cl)  
  }
  
  #   Setting train control for 10 fold cross validation
  train_control <- trainControl(method = "cv",number = 10,savePredictions = TRUE,classProbs = TRUE)
  # Set seed to control for resampling and generate reproducible results
  set.seed(seed = 123)
  #   build model on response for selected classifier
  #   fmla <- as.formula(paste0(response,"~."))
  if(classifier=='xgbLinear'||classifier=='xgbTree'){
    built_model <- train(x = sparse_matrix,y = train_data$Customer.Type,trControl = train_control,
                         method = classifier,tuneLength = tuneLen)
  } else{
    built_model <- train(fmla,data = train_data,trControl = train_control,
                         method = classifier,tuneLength = tuneLen)
  }
  if(parallelism==TRUE){
    stopCluster(cl = cl)
  }
  
  return(built_model)
}