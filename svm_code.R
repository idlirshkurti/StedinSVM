# Analytics Community - Deep dive into SVM Code

# Kaggle Titanic - Classification Competition 
packages <- c('e1071', 'ggplot2')
lapply(packages, install.packages, character.only = TRUE)
lapply(packages, require, character.only = TRUE)

# Make sure to set the correct working directory

# ---------- Load the data ----------------- 
train <- read.csv("train.csv", sep = ",", header = TRUE)
test <- read.csv("train.csv", sep = ",", header = TRUE)
head(train); head(test)
ytrain <- train$Survived
xtrain <- train[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Embarked")]

# Visualisation plot
p <- ggplot(train, aes(Parch, Age))
p + geom_point(aes(colour = factor(Survived), size = 2)) 
#+ geom_abline(intercept=0, slope=1)


# -------- Load iris dataset -----
attach(iris)

x <- subset(iris, select=-Species)
y <- Species

# get rid of one of the levels in the species column
del_idx = which(y == "versicolor")
x = x[-del_idx,]
y = y[-del_idx]
y <- factor(as.integer(y), labels = c("setosa", "virginica")) 
levels(y)

# plot 2 dimensional class separation
separations <- ggplot(x,aes(shape = factor(y), Sepal.Length, Sepal.Width)) +
  geom_point(aes(size = 2, colour=factor(y))) + geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(separations)

# Separate into train and test sets (75% train & 25% test)
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

# Put the predictions and features in a single data frame
full_pred <- cbind(xtest, ytest = ypred)

# ----- Review Results 
system.time(ypred <- predict(svm_model,xtest))
table(ypred, ytest) # confusion matrix

test.sep <- ggplot(full_test, aes(shape = factor(ytest), Sepal.Length, Sepal.Width)) +
  geom_point(aes(size = 2, colour=factor(ytest))) + geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(test.sep)

pred.sep <- ggplot(full_pred, aes(shape = factor(ypred), Sepal.Length, Sepal.Width)) +
  geom_point(aes(size = 2, colour=factor(ypred))) + geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(pred.sep)

# ---------- Non linear kernel ------ 

# Simulated Data
x <- runif(n = 100, max = 2 , min = 0)
y <- runif(n = 100, max = 2, min = 0)
df <- cbind(y, x = x)

# Define non-linear circular classes
class <- NULL 
for(i in 1:100){
   if(c(c(df[i,1]-1)^2 + c(df[i,2]-1)^2) > 0.5){class[i] <- 1}else{class[i]<-0}
}
df <- cbind(df, class = class)

# Plot the original separation
separations <- ggplot(df, aes(shape = factor(class), x, y)) +
  geom_point(aes(size = 2, colour=factor(class))) #+ geom_abline(intercept=-2.4, slope=1) # 2 dimensional
plot(separations)


# ---- More complicated model -----

# Random parameter selection
csvm <- tune(svm, ytrain ~ ., data = full_train,
             ranges=list(cost=c(0.25,0.5,1,2,10), gamma=c(0.25,0.5,1,3,5)),
             scale=F, tune.control="logloss")

# Choose the best parameters
bestGamma <- csvm$best.parameters[[1]]
bestC <- csvm$best.parameters[[2]] # = 0.5
model <- svm(ytrain ~.,full_train,
                 cost = bestC,gamma = bestGamma,
                 probability=TRUE,
                 scale=FALSE,cross=10)

ypred = predict(model, xtest, probability = F)
ypred



