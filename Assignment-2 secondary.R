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
                path = save_data,
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
