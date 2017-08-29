library(readr)
library(glmnet)
library(dplyr)

## Kaggle: Wisconsin breast cancer data
bc <- read_csv("breast-cancer-wisconsin-data.csv")
colnames(bc)[colnames(bc)=="concave points_mean"] <- "concave_points_mean"
colnames(bc)[colnames(bc)=="concave points_se"] <- "concave_points_se"
colnames(bc)[colnames(bc)=="concave points_worst"] <- "concave_points_worst"

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

dataset <- read_csv("breast-cancer-wisconsin-data-expanded.csv")
dataset$diagnosis <- as.factor(dataset$diagnosis)
dataset$diagnosis_cat <- dataset$diagnosis
factor_cols <- colnames(dataset)[grep("_cat", colnames(dataset))]
for (x in factor_cols) {
    dataset[[x]] <- as.factor(dataset[[x]])
}

## Split into training and test
set.seed(16)
n_train <- floor(nrow(dataset)*0.8)
rows_train <- sample.int(nrow(dataset), size = n_train)
rows_test <- setdiff(seq_len(nrow(dataset)), rows_train)
train <- dataset[rows_train,]
test <- dataset[rows_test,]

cvfit_lasso <- cv.glmnet(x = as.matrix(train[,2:31]), y = train$diagnosis, family = "binomial", alpha = 1, type.measure = "class")
par(mfrow = c(1,1))
plot(cvfit_lasso)

vars <- tail(colnames(train), -1)
## 3 vars: 17.837 sec
## 4 vars: 140.064 sec
system.time({
fits <- do.call(rbind, lapply(1:4, function(i) {
	combos <- combn(vars, m = i)
	do.call(rbind, lapply(seq_len(ncol(combos)), function(j) {
		form <- paste("diagnosis ~", paste(combos[,j], collapse = "+"))
		aic <- glm(as.formula(form), data = train, family = binomial)$aic
		data.frame(num_vars = i, formula = form, aic = aic)
	}))
}))
})

best_fits_by_num_vars <- fits %>%
	group_by(num_vars) %>%
	top_n(n = -1, wt = aic) %>%
	as.data.frame %>%
	mutate(formula = as.character(formula))

predict_prob <- function(fit, data) {
	logodds <- predict(fit, data)
	odds <- exp(logodds)
	prob <- odds/(1+odds)
	return(prob)
}

optimal_prob <- function(fit, data) {
    pred_prob <- predict_prob(fit, data)
    p_seq <- seq(0, 1, 0.001)
    true_class <- as.integer(data$diagnosis)-1
    classif_acc <- sapply(p_seq, function(p) {
        classif <- pred_prob > p
        acc <- sum(classif==true_class)/length(true_class)
        return(acc)
    })
    p_seq[which.max(classif_acc)]
}

true_outcome_test <- as.integer(test$diagnosis)-1

acc_best_subsets <- sapply(1:4, function(nv) {
	form <- filter(best_fits_by_num_vars, num_vars==nv)$formula
	## Get fit from training data
	fit <- glm(as.formula(form), data = train, family = binomial)
	opt_p <- optimal_prob(fit, train)
	## Get predicted probabilities on test
	pred_prob_test <- predict_prob(fit, test)
	## Get predicted outcomes on test
	pred_outcome_test <- pred_prob_test > opt_p
	acc <- sum(pred_outcome_test==true_outcome_test)/length(true_outcome_test)
	return(acc)
})

pred_outcome_lasso_test <- predict(cvfit_lasso, newx = as.matrix(test[,2:31]), s = "lambda.1se", type = "class")
acc_lasso <- sum(pred_outcome_lasso_test==test$diagnosis)/nrow(test)

save(train, test, true_outcome_test, acc_best_subsets, acc_lasso, predict_prob, optimal_prob, file = "regression_data.rda")
