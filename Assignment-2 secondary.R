library("ggplot2")
library("rpart")
library("rpart.plot")
library("randomForest")
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)
library(dplyr)
# Drop variables
df <- data.frame(titanic.train)
clean_data_v2 <- df %>%
        select(-c(Cabin, Ticket)) %>%
        # Converting attributes of the data set
        mutate(
                Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c("Upper", "Middle", "Lower")),
                Survived = factor(Survived, levels = c(0, 1), labels = c("No", "Yes"))
        ) %>%
        na.omit() # removing na values just in case
glimpse(clean_data_v2)
View(clean_data_v2)


save <- function(title) {
        ggsave(title,
                path = "save_data",
                width = 25,
                height = 10,
                units = "cm"
        )
}

gminsplit <- seq(2, 40, 2) # minsplit sequence: 2,4,6...36,38,40
gmaxdepth <- seq(5, 30, 5) # maxdepth sequence: 5,10...25,30
gcp <- 10^(-seq(2, 4, 1)) # cp sequence: 0.01, 0.001, 0.0001
parameters <- expand.grid(
        minsplit = gminsplit,
        maxdepth = gmaxdepth,
        cp = gcp
)
accuracy <- matrix(NA, nrep, nrow(parameters))
precision <- matrix(NA, nrep, nrow(parameters))
specificity <- matrix(NA, nrep, nrow(parameters))
nrep <- 50
for (i in 1:nrep) {
        # creating training data sets (80% of the data) and test sets (20%)
        split <- sample(nrow(clean_data_v2), 0.8 * nrow(clean_data_v2))
        training_set <- clean_data_v2[split, ]
        test_set <- clean_data_v2[-split, ]

        for (j in 1:nrow(parameters)) { # running through each parameter combination
                # (with same train/test sets)

                # creating classification tree
                mytree <- rpart(Survived ~ ., data = training_set, control = rpart.control(
                        minsplit = parameters$minsplit[j],
                        maxdepth = parameters$maxdepth[j],
                        cp = parameters$cp[j],
                ))

                # creating a confusion matrix for each set of parameters
                pred <- predict(mytree, newdata = test_set, type = "class")
                conf_matrix <- table(test_set$Survived, pred, dnn = c("Actual Value", "Classifier Prediction"))

                # storing accuracy/precision/specificity in a matrix by repetition and parameter combination
                accuracy[i, j] <- sum(diag(conf_matrix)) / sum(conf_matrix)
                precision[i, j] <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
                specificity[i, j] <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
        }
}

parameters$accuracy <- apply(accuracy, 2, mean)
parameters$precision <- apply(precision, 2, mean)
parameters$specificity <- apply(specificity, 2, mean)
parameters$efficacy <- (parameters$accuracy +
        parameters$precision +
        parameters$specificity) / 3

ggplot(parameters) +
        aes(x = minsplit, y = efficacy, color = as.factor(maxdepth)) +
        geom_line() +
        geom_point(size = 2) +
        labs(x = "Minsplit", y = "Average Accuracy", color = "Maxdepth") +
        facet_grid(cp ~ .)
save("accuracy_analysis.png")

best <- parameters[which.max(parameters$efficacy), ]
best


# RANDOM FOREST

# creating the grid of values to be used in the classification tree
gntree <- seq(100, 2000, 250) # ntree sequence: 100,350,600...1600,1850
gmtry <- seq(1, 5, 1) # mtry sequence: 1,2,3,4,5
parameters2 <- expand.grid(
        ntree = gntree,
        mtry = gmtry
)
# creating a matrix to store accuracy, sensitivity, and specificity values
accuracy2 <- matrix(NA, nrep, nrow(parameters2))
precision2 <- matrix(NA, nrep, nrow(parameters2))
specificity2 <- matrix(NA, nrep, nrow(parameters2))

nrep <- 50
nrep <- 100
# repeated validation - random forest
for (i in 1:nrep) {
        # creating training data sets (80% of the data) and test sets (20%)
        split <- sample(nrow(df), 0.8 * nrow(df))
        training_set <- df[split, ]
        test_set <- df[-split, ]

        for (j in 1:nrow(parameters2)) { # running through each parameter combination
                # (with same train/test sets)

                # creating random forest
                mytree <- randomForest(Survived ~ .,
                        data = training_set,
                        ntree = parameters2$ntree[j],
                        mtry = parameters2$mtry[j]
                )

                # creating a confusion matrix for each set of parameters
                pred <- predict(mytree, newdata = test_set, type = "class")
                conf_matrix <- table(test_set$Survived, pred, dnn = c("Actual Value", "Classifier Prediction"))

                # storing accuracy/precision/specificity in a matrix by repetition and parameter combination
                accuracy2[i, j] <- sum(diag(conf_matrix)) / sum(conf_matrix)
                precision2[i, j] <- conf_matrix[1, 1] / sum(conf_matrix[, 1])
                specificity2[i, j] <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
        }
}

parameters2$accuracy <- apply(accuracy2, 2, mean)
parameters2$precision <- apply(precision2, 2, mean)
parameters2$specificity <- apply(specificity2, 2, mean)
parameters2$efficacy <- (parameters2$accuracy +
        parameters2$precision +
        parameters2$specificity) / 3

ggplot(parameters2) +
        aes(x = ntree, y = efficacy, color = as.factor(mtry)) +
        geom_line() +
        geom_point(size = 2) +
        labs(x = "Ntree", y = "Average Accuracy", color = "Mtry")
save("Efficacy by ntree and mtry.png")

best2 <- parameters2[which.max(parameters2$efficacy), ]


my_model <- function(test) {
        test$Survived <- as.factor(test$Survived)

        mytree <- randomForest(Survived ~ .,
                data = titanic.train,
                ntree = best2$ntree[j],
                mtry = best2$mtry[j]
        )
        pred <- predict(mytree, newdata = test, type = "class")

        return(pred)
}
predict_survival <- my_model(testing_data)
View(predict_survival)
