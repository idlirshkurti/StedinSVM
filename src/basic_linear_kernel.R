# Analytics Community - Deep dive into SVM Code
# Iris Dataset Example

packages <- c('e1071', 'ggplot2', "gridExtra")
lapply(packages, install.packages, character.only = TRUE)
lapply(packages, require, character.only = TRUE)

# -------- Load iris dataset -----
attach(iris)

# randomise the rows of the data
iris <- iris[,sample(ncol(iris))]

x <- subset(iris, select=-Species)
y <- Species

# get rid of one of the levels in the species column
del_idx = which(y == "versicolor")
x = x[-del_idx,]
y = y[-del_idx]
y <- factor(as.integer(y), labels = c("setosa", "virginica")) 
levels(y)

# plot 2 dimensional class separation
separations <- ggplot(x,aes(shape = factor(y), colour=factor(y), Sepal.Length, Sepal.Width)) +
  geom_point(size = 3) +
  ggtitle("Iris Dataset With 2 Classes") + geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(separations)

# Separate into train and test sets (75% train & 25% test)
index <- sample(1:nrow(x),round(0.75*nrow(x)))
xtrain <- x[index,]; xtest <- x[-index,]
ytrain <- y[index]; ytest <- y[-index]

# Use separate train/test data frames
full_train <- cbind(xtrain, ytrain = ytrain)
full_test <- cbind(xtest, ytest = ytest)

# Fit the model
svm_model <- svm(xtrain, ytrain, scale = TRUE, kernel = "linear")
summary(svm_model)
ypred <- predict(svm_model, xtest)

# Put the predictions and features in a single data frame
full_pred <- cbind(xtest, ypred = ypred)

# ----- Review Results 
system.time(ypred <- predict(svm_model,xtest))
table(ypred, ytest) # confusion matrix

test.sep <- ggplot(full_test, aes(shape = factor(ytest), Sepal.Length, Sepal.Width)) +
  geom_point(aes(size = 2, colour=factor(ytest))) + geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(test.sep)

pred.sep <- ggplot(full_pred, aes(shape = factor(ypred), Sepal.Length, Sepal.Width)) +
  geom_point(aes(size = 2, colour=factor(ypred))) + geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(pred.sep)




