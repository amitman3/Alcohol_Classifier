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

Tuning parameters are at default values for this version. User defined tuning length/tuning grid to be covered in next version.

Default response variable for prediction set to "Dalc" - Workday alcohol consumption.
Range for this variable is - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)

At present, the model predicts the classes with ~71% accuracy on the test dataset (splitting the original as 75:25).

The program can also be used for prediction on other response variables like Walc (Weekend consumption) by simply changing the "response" variable in the code.
