getwd()
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)

library(dplyr)
df <- data.frame(titanic.train)
cleaned_df <- df %>% select(-c(Cabin, Ticket))
# Making a binary indicator of cabin status
df$CabinStatus <- ifelse(df$Cabin == "", 0, 1)

cleaned_df <- df %>% select(-c(Cabin, Ticket))
View(cleaned_df)

create_data_sets <- function(cleaned_df, size = 0.8, train = TRUE) {
        num_rows <- nrow(cleaned_df)
        total_row <- size * num_rows
        train_sample <- 1:total_row
        if (train == TRUE) {
                return(cleaned_df[train_sample, ])
        } else {
                return(cleaned_df[-train_sample, ])
        }
}


training_data <- create_data_sets(cleaned_df, 0.8, train = TRUE)
testing_data <- create_data_sets(cleaned_df, 0.8, train = FALSE)

# Viewing created data sets
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
