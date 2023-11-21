getwd()
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)
example <- data.frame(titanic.train$Survived, titanic.train$Fare)
example$Survived <- titanic.train$Survived
example$Fare <- titanic.train$Fare
example$titanic.train.Survived <- NULL
example$titanic.train.Fare <- NULL
# "Data" is referenced by titanic.train
View(example)
print(example$Survived)
nrep <- 50
d_minsplit <- seq(from = 2, to = 40, by = 2)
d_maxdepth <- seq(from = 1, to = 5, by = 1)
d_cp <- 10^(-seq(from = 2, to = 4, by = 1))
parametros <- expand.grid(minsplit = d_minsplit, maxdepth = d_maxdepth, cp = d_cp) # nolint: line_length_linter.
modelqualityresults <- data.frame(accuracy = rep(0, nrep), sensitivity = rep(0, nrep))

df <- data.frame(example)
set.seed(1)
nreps <- 100
ccr <- rep(0, nreps)
idx_tr <- sample(nrow(df), 150)
train <- df[idx_tr, ]
test <- df[-idx_tr, ]
library(ggplot2)
ggplot(train) +
    geom_point(aes(Survived, Fare)) +
    xlab("Training Set X.1 Values") +
    ylab("Training Set X.2 Values")
ggplot(test) +
    geom_point(aes(Survived, Age)) +
    xlab("Testing Set X.1 Values") +
    ylab("Testing Set X.2 Values")

matrix_1 <- as.matrix(df)
library(rpart)
library(rpart.plot)
tree <- rpart(Survived ~ ., data = train)
pred <- predict(tree, newdata = test, type = "vector")
crr[i] <- sum(pred == test$Survived) / nrow(test)



# Cookie-cutter end function:

my_model = function(test){
    pred = predict(my.fitted.model, test, type = "class")
    return(pred)
}
