getwd()
load("titanic_train.RDATA")
head(titanic.train)
# - View data
View(titanic.train)

# "Data" is referenced by titanic.train

nrep <- 50
d_minsplit <- seq(from = 2, to = 40, by = 2)
d_maxdepth <- seq(from = 1, to = 5, by = 1)
d_cp <- 10^(-seq(from = 2, to = 4, by = 1))
parametros <- expand.grid(minsplit = d_minsplit, maxdepth = d_maxdepth, cp = d_cp) # nolint: line_length_linter.
modelqualityresults <- data.frame(accuracy = rep(0, nrep), sensitivity = rep(0, nrep))

df = data.frame(titanic.train)
set.seed(1)
idx_tr = sample(nrow(df),150)
train = df[idx_tr,]
test = df[-idx_tr,]
library(ggplot2)
ggplot(train) + geom_point(aes(Survived,Pclass))+xlab("Training Set X.1 Values")+ylab("Training Set X.2 Values")
ggplot(test) + geom_point(aes(Survived,Age))+xlab("Testing Set X.1 Values")+ylab("Testing Set X.2 Values")
