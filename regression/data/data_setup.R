library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

## Kaggle: Wisconsin breast cancer data
bc <- read_csv("breast-cancer-wisconsin-data.csv")
bc <- bc[,2:32]
bc_expanded <- bc
par(mfrow = c(5,6), mar = c(3,3,3,1))
for (i in 2:31) {
    plot(density(bc[[i]]), xlab = "", ylab = "", main = colnames(bc)[i])
}
for (i in 2:31) {
    new_col <- paste0(colnames(bc)[i], "_cat")
    bc_expanded[[new_col]] <- cut(bc[[i]], c(-Inf, quantile(bc[[i]], c(0.33,0.67,1))))
    levels(bc_expanded[[new_col]]) <- c("low", "medium", "high")
}
write_csv(bc_expanded, "breast-cancer-wisconsin-data-expanded.csv")

dataset <- read_csv("data/breast-cancer-wisconsin-data-expanded.csv")
dataset$diagnosis <- as.factor(dataset$diagnosis)
dataset$diagnosis_cat <- dataset$diagnosis
factor_cols <- colnames(dataset)[grep("_cat", colnames(dataset))]
for (x in factor_cols) {
    dataset[[x]] <- as.factor(dataset[[x]])
}

NUM_RESAMPLE <- 100
SIZE_RESAMPLE <- round(nrow(dataset)*0.6)

## Split into training and test
set.seed(16)
n_train <- floor(nrow(dataset)*0.8)
rows_train <- sample.int(nrow(dataset), size = n_train)
rows_test <- setdiff(seq_len(nrow(dataset)), rows_train)
train <- dataset[rows_train,]
test <- dataset[rows_test,]

cvfit <- cv.glmnet(x = as.matrix(train[,2:31]), y = train$diagnosis, family = "binomial", type.measure = "class")
plot(cvfit)

## bestglm
y <- train$diagnosis
X <- train[,2:31]
train_formatted <- as.data.frame(cbind(X,y))

system.time({
best_logit <- bestglm(train_formatted, family = binomial, IC = "AIC", method = "exhaustive", nvmax = 5)
})