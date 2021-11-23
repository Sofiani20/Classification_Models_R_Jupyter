getwd()
setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes")
df<-read.csv("House_price_prepros_class.csv")

# on a besoin du package suivant
library(MASS, lib.loc = "C:/Program Files/R/R-4.0.1/library")
lda.fit<-lda(Sold~.,data = df)
lda.fit
table(df$Sold)

lda.pred=predict(lda.fit,df)
# pour voir les proba de 0 et 1
lda.pred$posterior
lda.class<-lda.pred$class
table(lda.class,df$Sold)

# nombre d'obs ayant une proba de 1 sup à 0.8
sum(lda.pred$posterior[,1]>0.8)

# pour l'lda quadratique uda au lieu de lda


# train test
library(caTools)
set.seed(0)
split_df<-sample.split(df,SplitRatio = 0.8)
train_set<-subset(df,split_df==TRUE)
test_set<-subset(df,split_df==FALSE)

train.fit=lda(Sold~.,data = train_set)
train.fit

test_pred=predict(train.fit,test_set)
# pour voir les proba de 0 et 1
test_pred$posterior
test_class<-test_pred$class
table(test_class,test_set$Sold)
