
setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes/R_codes")

movie=read.csv('Movie_regression.csv')
View(movie)

# DATA PREPROCESSING
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm=TRUE)

# dummy variables
require(dummies)
movie<-dummy.data.frame(movie)
View(movie)
movie<-movie[,c(-12,-16)]


# Train-Test Split
library(caTools)
set.seed(0)
split=sample.split(movie,SplitRatio = 0.8)
train=subset(movie,split==TRUE)
test=subset(movie,split==FALSE)


# Building Model
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Run regression tree on train set
regtree=rpart(formula = Collection~., data = train,
              control = rpart.control(maxdepth = 3))
#Plot the Decision Tree
rpart.plot(regtree, box.palette = "RdBu", digits = -3)
rpart.plot(regtree, box.palette = "RdBu", digits = 0)

# prediction value
View(train)
X_test=test[,-20]
y_test=test$Collection
X_train=train[,-20]
y_train=train$Collection
y_pred=predict(regtree, X_test, type = "vector")

# MeanSquaredError
MSE2=mean((y_test-y_pred)^2)
MSE2

# TREE PRUNING
fulltree=rpart(formula = Collection~., data = train,
                    control = rpart.control(cp=0))
rpart.plot(fulltree, box.palette = "RdBu", digits = -3)

printcp(regtree)
plotcp(regtree)
mincp=regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]


prunetree=prune(fulltree,mincp)
rpart.plot(prunetree, box.palette = "RdBu", digits = -3)


full_pred=predict(fulltree, X_test, type = "vector")
mean((y_test-full_pred)^2)

prune_pred=predict(prunetree, X_test, type = "vector")
mean((y_test-prune_pred)^2)
# c'est bizzare prune doit être plus précis