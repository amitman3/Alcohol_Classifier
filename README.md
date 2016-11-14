# Alcohol_Consumption_Classifier
Predicting alcohol consumption among students using diff. ML techniques

Data Source - http://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION
Detailed data definition is covered in the Data_Definition.txt (Data folder)

Primarily built using the caret package.
Employing 10 fold cross validation on training set for each model to contain variance.

Current version has been tested on the following classfifiers-

* Random Forest (rf)
* Support Vector Machines with Linear Kernel (svmLinear)
* Support Vector Machines with Polynomial Kernel (svmPoly)
* Model averaged Neural Network (avNNet)
* Linear Discriminant Analysis (lda2)
* Multinomial Regression (multinom)

The caret_classifier_list.txt file maintains the list of classifiers to be tested in the program. One can append more classifiers from the caret supported list (https://topepo.github.io/caret/modelList.html) and simply run the program.

Tuning length set to 5 for all models. Can be changed by setting the tuneLen variable.

Default response variable for prediction set to "Dalc" - Workday alcohol consumption.
Range for this variable is - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)

Updated the code for parallel execution using the doparallel package. The program now builds and predicts for all 6 models in total time of 130-150 seconds. My machine has 4 cores (8 logical processors). I am using 7 processors for parallel execution. Without parallelism, the code was taking around 370-400 seconds.

At present, the model predicts the classes with ~73% accuracy on the test dataset (splitting the original as 75:25) with LDA, closely followed by Polynomial SVM (72%) and Random Forest and Neural Net (each at 71%)

The program can also be used for prediction on other response variables like Walc (Weekend consumption) by simply changing the "response" variable in the code.
