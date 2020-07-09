

bikeshare <- read.csv("bikeshare.csv")
attach(bikeshare)
#1)	Working with data and exploratory analysis:
#1)a)
BADWEATHER=ifelse(WEATHERSIT==3 | WEATHERSIT==4,'YES','NO')

#1)b)plot ATEMP and COUNT with different colors to distinguish BADWEATHER days
plot(ATEMP,COUNT,col=ifelse(BADWEATHER=='YES','red','green'))

#1)c)plot ATEMP and CASUAL with different colors to distinguish BADWEATHER days
plot(ATEMP,CASUAL,col=ifelse(BADWEATHER=='YES','red','blue'))

#1)c)plot ATEMP and REGISTERED with different colors to distinguish BADWEATHER days
plot(ATEMP,REGISTERED,col=ifelse(BADWEATHER=='YES','red','black'))

#2)	Basic regression in R:
#Make categorical columns as factors
MONTH=as.factor(MONTH)
HOLIDAY=as.factor(HOLIDAY)
WEEKDAY=as.factor(WEEKDAY)
BADWEATHER=as.factor(BADWEATHER)

#Multivariate regression for COUNT
model1=lm(COUNT~MONTH+HOLIDAY+WEEKDAY+BADWEATHER+TEMP+ATEMP+HUMIDITY+WINDSPEED,data=bikeshare)
summary(model1)

#2)c)COUNT Prediction for an instance
dm_bikeshare=data.frame(MONTH='JANUARY',HOLIDAY='NO',WEEKDAY='YES',BADWEATHER='YES',TEMP=0.35,ATEMP=0.3,HUMIDITY=0.21,WINDSPEED=0.30)
est_count=predict(model1,newdata=dm_bikeshare)
est_count
#2)e)To show MLR with CASUAL and REGISTERED variables overfit the model 
model6=lm(COUNT~MONTH+HOLIDAY+WEEKDAY+BADWEATHER+TEMP+ATEMP+HUMIDITY+WINDSPEED+CASUAL+REGISTERED,data=bikeshare)
summary(model6)

#3)	More regression modeling: 
#SLR with BADWEATHER as independent variable to predict COUNT
model2=lm(COUNT~BADWEATHER,data=bikeshare)
summary(model2)

#3)b)Add interaction term WEEKDAY*BADWEATHER to the existing model
model3=lm(COUNT~WEEKDAY+BADWEATHER+WEEKDAY*BADWEATHER,data=bikeshare)
summary(model3)

#4)	Comparing two different models: 
#4)a)MLR with TEMP and ATEMP
model4=lm(COUNT~MONTH+HOLIDAY+WEEKDAY+BADWEATHER+TEMP+ATEMP,data=bikeshare)
summary(model4)

#4)a)MLR without TEMP and ATEMP
model5=lm(COUNT~MONTH+HOLIDAY+WEEKDAY+BADWEATHER,data=bikeshare)
summary(model5)