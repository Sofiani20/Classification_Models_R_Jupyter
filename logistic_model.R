getwd()
setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes")
df<-read.csv("House_price_prepros_class.csv")
View(df)

# for logistic reg we use glm function
glm.fit=glm(Sold~price,data = df, family = binomial)
summary(glm.fit)

# for multi logistic reg we use glm function
glm.fit=glm(Sold~.,data = df, family = binomial)
summary(glm.fit)

# probas de yes ou 1
glm.proba=predict(glm.fit,type = "response")
View(glm.proba)

# seuil de 0.5
glm.pred<-rep("NO",506)
glm.pred[glm.proba>0.5]<-"YES"
View(glm.pred)

# matrice de confusion
table(glm.pred,df$Sold)


# train test
library(caTools)
set.seed(0)
split_df<-sample.split(df,SplitRatio = 0.8)
train_set<-subset(df,split_df==TRUE)
test_set<-subset(df,split_df==FALSE)

train.fit=glm(Sold~.,data = train_set, family = binomial)
summary(train.fit)
test_probs<-predict(train.fit, test_set, type="response")
View(test_probs)
test_pred<-rep("NO",length(test_probs))
test_pred[test_probs>0.5]<-"YES"
View(test_pred)
#matrice de confusion sur test_set
table(test_set$Sold,test_pred)
