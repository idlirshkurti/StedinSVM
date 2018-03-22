# ---------- Non linear kernel ------ 

# Simulated Data
n = 1000
x <- runif(n, max = 2 , min = 0)
y <- runif(n, max = 2, min = 0)
df <- cbind(y, x = x)

# Define non-linear circular classes
class <- NULL 
for(i in 1:n){
  if(c(c(df[i,1]-1)^2 + c(df[i,2]-1)^2) > 0.5){
    class[i] <- 1
  }else{
    class[i] <- 0
  }
}
df <- cbind(df, class = class)

# Plot the original separation
separations <- ggplot(df, aes(shape = factor(class), x = x, y = y)) +
  geom_point(aes(colour=factor(class), size = 0.1)) #+ geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(separations)

## Separate the data into train and test as in the linear kernel case!

# Fit the model
svm_model <- svm(xtrain, ytrain, scale = TRUE, kernel = "gaussian")
summary(svm_model)
ypred <- predict(svm_model, xtest)

# confusion matrix
table(ypred, ytest)
