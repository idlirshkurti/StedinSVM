# Analytics Community - Deep dive into SVM Code
# Iris Dataset Example (3 classes)

packages <- c('e1071', 'ggplot2', "gridExtra")
lapply(packages, install.packages, character.only = TRUE)
lapply(packages, require, character.only = TRUE)

# -------- Load iris dataset -----
attach(iris)

# randomise the rows of the data
iris <- iris[,sample(ncol(iris))]

x <- subset(iris, select=-Species)
y <- as.factor(Species)
levels(y) 

# plot 2 dimensional class separation
separations <- ggplot(x,aes(colour = factor(y), shape = factor(y), Sepal.Length, Sepal.Width)) +
  geom_point(size = 3) +
  ggtitle("Full Iris Dataset With 3 Classes") # 3 classes
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

table(ypred, ytest) # confusion matrix

test.sep <- ggplot(full_test, aes(shape = factor(ytest), colour=factor(ytest), Sepal.Length, Sepal.Width)) +
  geom_point(size = 3) +
  ggtitle("Actual Test Set Classes") # 2 dimensional
plot(test.sep)

pred.sep <- ggplot(full_pred, aes(shape = factor(ypred), colour=factor(ypred), Sepal.Length, Sepal.Width)) +
  geom_point(size = 3) +
  ggtitle("Radial Kernel Classification") # 2 dimensional
plot(pred.sep)

grid.arrange(test.sep, pred.sep, ncol=1)



# ---- Fit more complicated model -----

# Random parameter selection
csvm <- tune(svm, ytrain ~ ., data = full_train,
             ranges=list(cost=c(0.25,0.5,1,2), gamma=c(0.25,0.5,1,3,5)),
             scale=F, tune.control="logloss")

# Choose the best parameters
bestGamma <- csvm$best.parameters[[1]]
bestC <- csvm$best.parameters[[2]] # = 0.5
model <- svm(ytrain ~.,full_train,
             cost = bestC,gamma = bestGamma,
             probability=TRUE,
             scale=FALSE,cross=10)

ypred = predict(model, xtest, probability = F)
full_pred <- cbind(xtest, ypred = ypred)
table(ypred, ytest) # confusion matrix

pred.sep <- ggplot(full_pred, aes(shape = factor(ypred), colour=factor(ypred), Sepal.Length, Sepal.Width)) +
  geom_point(size = 3) +
  ggtitle("Radial Kernel Classification") # 2 dimensional
plot(pred.sep)


