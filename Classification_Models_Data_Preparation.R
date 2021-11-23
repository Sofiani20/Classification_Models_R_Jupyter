getwd()
setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes/R_codes")
df<-read.csv("House_Price_classification.csv",sep = ",",header = TRUE, dec = ".")
View(df)
str(df)

# EDD
summary(df)
boxplot(df$n_hot_rooms)
pairs(~df$rainfall+df$Sold)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))
# 1) bus_ter takes only one value so it is not a variable it is a constant 
# it isn't meanfull for our model
# 2) outliers in n_hot_rooms ans rainfall
# missing value in n_hos_beds


# Outliers treatment(capping and flooring)
quantile(df$n_hot_rooms,0.99)
uv=3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv]=uv
summary(df$n_hot_rooms)

lv=quantile(df$rainfall,0.01)*0.3
df$rainfall[df$rainfall<lv]=lv
summary(df$rainfall)


# Missing values treatment 
mean(df$n_hos_beds) #return NA because there is NA within that column
moy=mean(df$n_hos_beds,na.rm = TRUE) # remove NA beforecalculating the mean
which(is.na(df$n_hos_beds)) #returns the NA's rows indexs
df$n_hos_beds[is.na(df$n_hos_beds)]=moy
length(df$n_hos_beds)

# moy_dist : remplacer les disti par leur moyenne
df$moy_dist<-(df$dist1+df$dist2+df$dist3+df$dist4)/4
summary(df)
df<-df[,-6:-9] #next we are going to delete bus_ter variable
df<-df[,-13]


# dummy variables
#Lorsque j'ai une variable qualitative avec N classes je crée N 
# variables muettes pour chaque classe
require(dummies)
df<-dummy.data.frame(df)
View(df)
df<-df[,c(-8,-14)]
write.csv(df,"House_price_prepros_class.csv", row.names = FALSE)
