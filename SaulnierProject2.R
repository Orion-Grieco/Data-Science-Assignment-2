# loading data and needed libraries
rm(list=ls())
getwd()
setwd("C:/Users/arjun/OneDrive/Desktop/DataSci")
load("titanic_train.RDATA")
View(titanic.train)
library("ggplot2")
library("rpart")
library("rpart.plot")
library("randomForest")

# function to save a graph to RGraphs folder given a title
img = function(title){
  ggsave(title,
         path="C:/Users/arjun/OneDrive/Desktop/DataSci/RGraphs",
         width=25,
         height=10,
         units = "cm") 
}

# creating a function clean() to preprocess data
clean = function(data){
  # removing the Ticket column
  modifiedData = subset(data, select = -Ticket)
  
  # creating the HasCabin column based on if a cabin is listed
  modifiedData$HasCabin = modifiedData$Cabin!=""
  # removing the Cabin column
  modifiedData = subset(modifiedData, select = -Cabin)
  
  # converting Survived to a factor
  modifiedData$Survived = as.factor(modifiedData$Survived)
  
  return(modifiedData)
}
df = clean(titanic.train) # storing preprocessed data in the variable 'df'



# creating the grid of values to be used in the classification tree
gminsplit = seq(2,40,2) # minsplit sequence: 2,4,6...36,38,40
gmaxdepth = seq(2,5,1) # maxdepth sequence: 5,10...25,30
gcp = 10^(-seq(2,4,1)) # cp sequence: 0.01, 0.001, 0.0001
parameters = expand.grid(minsplit = gminsplit, 
                         maxdepth = gmaxdepth, 
                         cp = gcp)

# repeated validation - classification tree
nrep = 50 # 50 repetitions per parameter combination

# creating matrices to store accuracy, sensitivity, and specificity values
accuracy = matrix(NA, nrep, nrow(parameters))
precision = matrix(NA, nrep, nrow(parameters))
specificity = matrix(NA, nrep, nrow(parameters))


for (i in 1:nrep){
  
  # creating training data sets (80% of the data) and test sets (20%)
  split = sample(nrow(df), 0.8*nrow(df))
  training_set = df[split,]
  test_set = df[-split,]
  
  for (j in 1:nrow(parameters)){ # running through each parameter combination 
    # (with same train/test sets)
    
    # creating classification tree
    mytree = rpart(Survived~., data=training_set, control=rpart.control(
      minsplit=parameters$minsplit[j],
      maxdepth=parameters$maxdepth[j],
      cp=parameters$cp[j],
    ))
    
    # creating a confusion matrix for each set of parameters
    pred = predict(mytree, newdata=test_set, type="class")
    conf_matrix = table(test_set$Survived, pred, dnn=c("Actual Value", "Classifier Prediction"))
    
    # storing accuracy/precision/specificity in a matrix by repetition and parameter combination
    accuracy[i,j] = sum(diag(conf_matrix))/sum(conf_matrix)
    precision[i,j] = conf_matrix[1,1]/sum(conf_matrix[,1])
    specificity[i,j] = conf_matrix[2,2]/sum(conf_matrix[,2])
    
  }
}

# associating the mean accuracy/precision/specificity values with each hyperparameter combination
parameters$accuracy = apply(accuracy, 2, mean)
parameters$precision = apply(precision, 2, mean)
parameters$specificity = apply(specificity, 2, mean)
# averaging those values to find the mean 'efficacy'
parameters$efficacy = (parameters$accuracy + 
                         parameters$precision + 
                         parameters$specificity) / 3

# creating and saving a graph of the efficacy depending on hyperparameters 
ggplot(parameters) + aes(x=minsplit, y=efficacy, color=as.factor(maxdepth)) + 
  geom_line() + geom_point(size=2) + labs(x="Minsplit", y="Average Accuracy", color="Maxdepth") + facet_grid(cp~.)
img("Efficacy by maxdepth, minsplit, and cp.png")

# finding the best parameter combination
best = parameters[which.max(parameters$efficacy),]
print(best)


# RANDOM FOREST

# creating the grid of values to be used in the classification tree
gntree = seq(100,2000, 250) # ntree sequence: 100,350,600...1600,1850
gmtry = seq(1,5,1) # mtry sequence: 1,2,3,4,5
parameters2 = expand.grid(ntree = gntree, 
                         mtry = gmtry)
# creating a matrix to store accuracy, sensitivity, and specificity values
accuracy2 = matrix(NA, nrep, nrow(parameters2))
precision2 = matrix(NA, nrep, nrow(parameters2))
specificity2 = matrix(NA, nrep, nrow(parameters2))

# the '2' in the variable name means it's associated with the random forest
  # this is true throughout the code


# repeated validation - random forest
for (i in 1:nrep){
  
  # creating training data sets (80% of the data) and test sets (20%)
  split = sample(nrow(df), 0.8*nrow(df))
  training_set = df[split,]
  test_set = df[-split,]
  
  for (j in 1:nrow(parameters2)){ # running through each parameter combination 
    # (with same train/test sets)
    
    # creating random forest
    mytree = randomForest(Survived~., data=training_set, 
                          ntree=parameters2$ntree[j],
                          mtry=parameters2$mtry[j]
    )
    
    # creating a confusion matrix for each set of parameters
    pred = predict(mytree, newdata=test_set, type="class")
    conf_matrix = table(test_set$Survived, pred, dnn=c("Actual Value", "Classifier Prediction"))
    
    # storing accuracy/precision/specificity in a matrix by repetition and parameter combination
    accuracy2[i,j] = sum(diag(conf_matrix))/sum(conf_matrix)
    precision2[i,j] = conf_matrix[1,1]/sum(conf_matrix[,1])
    specificity2[i,j] = conf_matrix[2,2]/sum(conf_matrix[,2])
    
  }
}

# same as above
# associating the mean accuracy/precision/specificity values with each hyperparameter combination
parameters2$accuracy = apply(accuracy2, 2, mean)
parameters2$precision = apply(precision2, 2, mean)
parameters2$specificity = apply(specificity2, 2, mean)
# averaging those values to find the mean 'efficacy'
parameters2$efficacy = (parameters2$accuracy + 
                         parameters2$precision + 
                         parameters2$specificity) / 3

# creating and saving a graph of the efficacy depending on hyperparameters 
ggplot(parameters2) + aes(x=ntree, y=efficacy, color=as.factor(mtry)) + 
  geom_line() + geom_point(size=2) + labs(x="Ntree", y="Average Accuracy", color="Mtry")
img("Efficacy by ntree and mtry.png")

# finding the best parameter combination
best2 = parameters2[which.max(parameters2$efficacy),]
print(best2)


# creating a function mimicking the best model
my_model = function(test){
  
  data = clean(test)
  train = clean(titanic.train)
  
  fittedmodel = randomForest(Survived~., data=train, 
                        ntree=best2$ntree,
                        mtry=best2$mtry)
  pred = predict(fittedmodel, newdata=data, type="class") 
  
  return(pred)
}

# saving the best model in a file called modelData.RData
save(my_model, clean, best, best2, file="modelData.RData")

rm(list=ls())
load("modelData.RData")
load("titanic_train.RData")
# my_model(newData)