getwd()
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)
df <- data.frame(titanic.train)

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

# Viewing created data sets
View(training_data)
View(testing_data)
# Checking data dimensions
dim(training_data)
dim(testing_data)
prop.table(table(training_data$Survived))
prop.table(table(testing_data$Survived))


