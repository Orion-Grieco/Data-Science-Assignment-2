getwd()
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)

library(dplyr)
df <- data.frame(titanic.train)
# cleaned_df <- df %>% select(-c(Cabin, Ticket))
# Making a binary indicator of cabin status
df$CabinStatus <- ifelse(df$Cabin == "", 0, 1)

cleaned_df <- df %>% select(-c(Cabin, Ticket))
View(cleaned_df)




library(dplyr)
# Drop variables
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






create_data_sets <- function(df, size = 0.8, train = TRUE) {
        num_rows <- nrow(df)
        total_row <- size * num_rows
        train_sample <- 1:total_row
        if (train == TRUE) {
                return(df[train_sample, ])
        } else {
                return(df[-train_sample, ])
        }
}
training_data <- create_data_sets(df, 0.8, train = TRUE)
testing_data <- create_data_sets(df, 0.8, train = FALSE)

View(training_data)
View(testing_data)
# Checking data dimensions
dim(training_data)
dim(testing_data)
prop.table(table(training_data$Survived))
prop.table(table(testing_data$Survived))

install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
tree <- rpart(Survived ~ ., data = training_data, method = "class")
rpart.plot(tree, extra = 106)





create_data_sets_2 <- function(clean_data_v2, size = 0.8, train = TRUE) {
        num_rows <- nrow(clean_data_v2)
        total_row <- size * num_rows
        train_sample_v2 <- 1:total_row
        if (train == TRUE) {
                return(clean_data_v2[train_sample_v2, ])
        } else {
                return(clean_data_v2[-train_sample_v2, ])
        }
}
training_data_v2 <- create_data_sets_2(clean_data_v2, 0.8, train = TRUE)
testing_data_v2 <- create_data_sets_2(clean_data_v2, 0.8, train = FALSE)



# Viewing created data sets
View(training_data_v2)
View(testing_data_v2)
# Checking data dimensions
dim(training_data_v2)
dim(testing_data_v2)
prop.table(table(training_data_v2$Survived))
prop.table(table(testing_data_v2$Survived))

install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
tree <- rpart(Survived ~ ., data = training_data_v2, method = "class")
rpart.plot(tree, extra = 106)

predict_survival <- predict(tree, newdata = testing_data_v2, type = "class")
View(predict_survival)

compare_survival <- table(testing_data_v2$Survived, predict_survival)
View(compare_survival)

accuracy_eval <- sum(diag(compare_survival)) / sum(compare_survival)
View(accuracy_eval)
paste("Accuracy: ", accuracy_eval)

accuracy_update <- function(tree) {
        predict_survival <- predict(tree, testing_data_v2, type = "class")
        compare_survival <- table(testing_data_v2$Survived, predict_survival)
        accuracy_eval <- sum(diag(compare_survival)) / sum(compare_survival)
        return(accuracy_eval)
}

control_var <- rpart.control(minpslit = 4, minbucket = round(nrow(training_data_v2) / 3), maxdepth = 3, cp = 0)
tree <- rpart(Survived ~ ., data = training_data_v2, method = "class", control = control_var)
accuracy_update(tree)
