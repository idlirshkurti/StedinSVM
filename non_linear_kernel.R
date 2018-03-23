# Analytics Community - Deep dive into SVM Code
# Non Linear Artificial Data Example

packages <- c('e1071', 'ggplot2', "gridExtra")
lapply(packages, install.packages, character.only = TRUE)
lapply(packages, require, character.only = TRUE)

# ---------- Non linear kernel ------ 

# Simulated Data
n = 1000
x <- runif(n, max = 2 , min = 0)
y <- runif(n, max = 2, min = 0)
df <- data.frame(y = y, x = x)

# Define non-linear circular classes
class <- NULL 
for(i in 1:n){
  if(c(c(df[i,1]-1)^2 + c(df[i,2]-1)^2) > 0.5){
    class[i] <- 1
  }else{
    class[i] <- 0
  }
}
class = as.factor(class)
df$class <- class

# Plot the original separation
separations <- ggplot(df, aes(colour=class, shape = class, x = x, y = y)) +
  geom_point(size = 3) +
  ggtitle("Simulated data with non-linear separation") #+ geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(separations)

# Separate the data into train and test as in the linear kernel case! (75% train & 25% test)
x <- subset(df, select=-class)
y <- df$class # outcome requires factor class for classification

index <- sample(1:nrow(x),round(0.75*nrow(x)))
xtrain <- x[index,]; xtest <- x[-index,]
ytrain <- y[index]; ytest <- y[-index]

# Use separate train/test data frames
full_train <- cbind(xtrain, ytrain = ytrain)
full_test <- cbind(xtest, ytest = ytest)

# Fit the model
svm_model <- svm(xtrain, ytrain, scale = TRUE, kernel = "radial")
summary(svm_model)
ypred <- predict(svm_model, xtest)

# confusion matrix
table(ypred, ytest)

# ---- Review results --- Plot the test set predictions
full_pred <- cbind(xtest, ypred = ypred)

pred.sep <- ggplot(full_pred, aes(shape = factor(ypred), colour=factor(ypred), x, y)) +
  geom_point(size = 3) +
  ggtitle("Test set predictions using Gaussian Kernel") # 2 dimensional
plot(pred.sep)



