setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes/R_codes")

movie=read.csv('Movie_classification.csv')


# DATA PREPROCESSING
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm=TRUE)
View(movie)



# Train-Test Split
library(caTools)
set.seed(0)
split=sample.split(movie,SplitRatio = 0.8)
train=subset(movie,split==TRUE)
test=subset(movie,split==FALSE)

# Building Model
library(rpart)
library(rpart.plot)
classtree=rpart(formula = Start_Tech_Oscar~., method="class", data = train,
                control = rpart.control(maxdepth = 3))
#Plot the Decision Tree
rpart.plot(classtree, box.palette = "RdBu", digits = -3)

# prediction value
test$predict=predict(classtree,test,type = "class")

# confusion matrix
table(test$Start_Tech_Oscar,test$predict)
