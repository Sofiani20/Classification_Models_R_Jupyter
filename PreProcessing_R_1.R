
# importing data
setwd("C:/Users/dell/Documents/apprentissage_ML_DL_Python_R_udemy/codes")
df<-read.csv("House_Price.csv",header = TRUE)
View(df)
summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall,data = df) # pairplot
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))

# observations
#outliers in n_hot_rooms and rainfull
# no values (na) in n_hos_beds
# one value for bus_ter
# crime_rate has some relationship with price



# Outliers treatment(capping and flooring)
 
quantile(df$n_hot_rooms,0.99)
upper_value=3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>upper_value]=upper_value
summary(df$n_hot_rooms)

lower_value=quantile(df$rainfall,0.01)*0.3
df$rainfall[df$rainfall<lower_value]=lower_value
summary(df$rainfall)


# Missing values treatment

mean(df$n_hos_beds) #return NA because there is NA within that column
moy=mean(df$n_hos_beds,na.rm = TRUE) # remove NA before calculating the mean
which(is.na(df$n_hos_beds)) #returns the NA's rows indexes
df$n_hos_beds[is.na(df$n_hos_beds)]=moy
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))
length(df$n_hos_beds)


# transforming to linear relationship

pairs(~crime_rate+price,data=df)
#or
plot(df$crime_rate,df$price)
log_crime_rate<-log(df$crime_rate+1)
df['log_crime_rate']=log_crime_rate
plot(df$log_crime_rate,df$price)


# moy_dist : remplacer les disti par leur moyenne
df$moy_dist<-(df$dist1+df$dist2+df$dist3+df$dist4)/4
summary(df)
# then we will delete les disti variables
View(df)
df<-df[,-7:-10] #next we are going to delete bus_ter variable
df<-df[,-14]

# NO USABLE VARIABLES
# une variable prenant une unique valeur
# une variable avec un pourcentage de NA élevé
# une variable with no business sense
# une variable ayant des valeurs régulières



# dummy variables
#Lorsque j'ai une variable qualitative avec N classes je crée N 
# variables muettes pour chaque classe
require(dummies)
df<-dummy.data.frame(df)
View(df)
df<-df[,c(-9,-14)]


# Correlation Analysis
View(cor(df))
str(cor(df))
View(round(cor(df),2))
df<-df[,-16] # on supprime parks pour éviter la multicolinéarité
df<-df[,-16]
# write.csv(df,file="house_price_PP1.csv")
