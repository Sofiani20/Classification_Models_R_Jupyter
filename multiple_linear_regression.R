
setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes")
df<-read.csv("House_Price_PP1.csv",header = TRUE,row.names = 1)
multiple_model<-lm(price~.,data = df)
summary(multiple_model)

# Test-Train Split in R

#install.packages("caTools")
library(caTools)
help("caTools")

split = sample.split(df,SplitRatio = 0.8)
View(split)
train_set= subset(df,split==TRUE)
test_set= subset(df,split==FALSE)

train_model<-lm(price~.,data = train_set)
train_a<-predict(train_model,train_set)
test_a<-predict(train_model,test_set)
mean((train_set$price-train_a)^2)
mean((test_set$price-test_a)^2) # est sensé être plus grande


# Subset selection in R
require(leaps) # leaps is regression subset selection package
View(df)
df_prime=df
best_model=regsubsets(price~.,data = df_prime, nvmax = 15) #nvmax go through 15 variables
summary(best_model) # la variable ayant l'* est meilleure
vect_r2<-summary(best_model)$adjr2 # renvoir les coef de déterminations des meilleurs models
max(vect_r2)
which.max(summary(best_model)$adjr2) # so the model which has 10 best variables
                                     # is our best model.
coef(best_model,10) # renvoie les coefs du meilleur model

# forward stepwise selection
forward_model<-regsubsets(price~.,df_prime,nvmax = 15, method = "forward")
summary(forward_model)$adjr2
which.max(summary(forward_model)$adjr2)
coef(forward_model,10)
