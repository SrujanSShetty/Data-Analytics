
 
getwd()
setwd("C:/Users/SRUJA/OneDrive/Desktop/Pgm in DA Project")
data<-read.csv(file="Project Data Set.csv",header=TRUE)
#1. Data preprocessing

#1.1 Exploring dataset

head(data)
summary(data)
str(data)
dim(data)

#1.2 Missing Values
colSums(is.na(data)) #Total number of missing values in a column
data$pm2.5 <-ifelse(is.na(data$pm2.5),ave(data$pm2.5, FUN= function(x)mean(x,na.rm=TRUE)),data$pm2.5)

#1.3 Data Duplicacy
any(duplicated(data))

#1.4 Convert Nominal Variable to Dummy variables
unique(data$cbwd)
# Convert the 'cbwd' column to a factor (if not already)
data$cbwd<- as.factor(data$cbwd)
data$cbwd <-as.numeric(data$cbwd) # Convert the 'Category' column to a factor



# Create dummy variables for the 'Category' column
dummy_variables <- model.matrix(~ as.factor(data$cbwd) - 1, data = data) 

# Combine the original data frame with the dummy variables
data<- cbind(data, dummy_variables)

#deleting the existing Nominal column 
data <- data[,-c(10)]

View(data)


#FOLLOWING DID NOT WORK 

library(dummy)
dummy(data,p=(5-1))
View(data)




#Before building models, You need to check correlations b/w y & x -variables 
#Apply transformations to x- variables if necessary.

cor(data)
plot(data)

#exclude nominal variables
data<-data[,1:11]
View(data)



#Training data 0.8

data=data[sample(nrow(data)),]
select.data = sample (1:nrow(data), 0.8*nrow(data))
train.data = data[select.data,]
test.data = data[-select.data,]





#Full Model 

full=lm(pm2.5~., data = train.data)
summary(full)




#Backward method using p-value in t-test as metric
 
m1=lm(pm2.5~., data = data)
summary(m1)

m1=lm(pm2.5~ .-year, data = data)
summary(m1)


#Backward method using AIC as metric

full=lm(pm2.5~., data = data)
m2=step(full, direction="backward", trace=T)
summary(m2)


#Forward method using AIC as metric
base=lm(pm2.5~1, data = data)
m3=step(base, scope=list(upper=full, lower=base), direction="forward",
     trace=F)
summary(m3)


#Stepwise method using ACI as metric

base=lm(pm2.5~1, data = data)
m5=step(base, scope=list(upper=full, lower=base), direction="both",
     trace=F)
summary(m5)

#-------------------
#Best subset method using Adj-R2 as metric(SKIP)

cor(data)
library(leaps)
m6<-leaps(y=train.data[,6],x=train.data[,-c(6,1)],names=names(train.data[,-c(6,1)]),method="adjr2") 
summary(m6)
#--------------------



#Residual Analysis of Full Model 

full=lm(pm2.5~., data = data)
summary(full)

plot( fitted(full), rstandard(full), main="Predicted vs
residuals plot")
abline(a=0, b=0, col='red') #add zero line

#Plot residuals vs predicted values: To check constant variance for the residuals


hist(log(data$pm2.5)) 
Fulll<-log(data$pm2.5)
Ddep<-lm(Fulll~ No+ year+ month+ day+hour+ DEWP+  TEMP+  PRES+   Iws  +  Is +   Ir  , data = data)




plot(train$xvar, rstandard(fit), main="xvar vs
     residuals plot")
abline(a=0, b=0,col='red') #add zero lin


logdata <- log(data$pm2.5) data <- cbind(logdata, data) #data <- data[,-c(2,7,11)] inf.val <- is.infinite(data$logdata) data <- data[!inf.val, ] model1 <- lm(logdata~., data = data) summary(model1)




#RMSE to Check Best Model 

#1.Full Model

#Using training set to full model: 
full=lm(pm2.5~., na.action = na.omit,data=train.data);
#• Creating full model using testing data:
y_pred=predict.glm(full, test.data)
y_obs=test.data[,"pm2.5"];
#•Computing prediction error RMSE:
rmse=sqrt((y_obs - y_pred)%*%(y_obs-y_pred) /nrow(test.data))
print(rmse)


#2.Backward method using p-value in t-test as metric

m1=lm(pm2.5~., data =train.data)
summary(m1)

m1=lm(pm2.5~ .-year, data = train.data)
summary(m1)

m1 <- lm(pm2.5 ~ . - year - No, data = train.data)
summary(m1)

m1=lm(pm2.5~.-year - No, na.action = na.omit,data=train.data);
#• Creating Backward method model using p-value & using testing data:
y_pred=predict.glm(m1, test.data)
y_obs=test.data[,"pm2.5"];
#•Computing prediction error RMSE:
rmse=sqrt((y_obs - y_pred)%*%(y_obs-y_pred) /nrow(test.data))
print(rmse)




#3.Backward method using AIC as metric

full=lm(pm2.5~.,na.action = na.omit, data = train.data)
m2=step(full, direction="backward", trace=T)
summary(m2)

#• Creating Backward method model using AIC as metric & using testing data:
y_pred=predict.glm(m2, test.data)
y_obs=test.data[,"pm2.5"];
#•Computing prediction error RMSE:
rmse=sqrt((y_obs - y_pred)%*%(y_obs-y_pred) /nrow(test.data))
print(rmse)



#4.Forward method using AIC as metric

base=lm(pm2.5~1, data = train.data)
m3=step(base, scope=list(upper=full, lower=base), direction="forward",
        trace=F)
summary(m3)
#• Creating Forward method using AIC as metric & using testing data:
y_pred=predict.glm(m3, test.data)
y_obs=test.data[,"pm2.5"];
#•Computing prediction error RMSE:
rmse=sqrt((y_obs - y_pred)%*%(y_obs-y_pred) /nrow(test.data))
print(rmse)


#5.Stepwise method using ACI as metric

base=lm(pm2.5~1,na.action = na.omit, data = train.data)
m4=step(base, scope=list(upper=full, lower=base), direction="both",
        trace=F)
summary(m4)

#• Creating Stepwise method using ACI as metric & using testing data:
y_pred=predict.glm(m4, test.data)
y_obs=test.data[,"pm2.5"];
#•Computing prediction error RMSE:
rmse=sqrt((y_obs - y_pred)%*%(y_obs-y_pred) /nrow(test.data))
print(rmse)




#Residual Analysis For BEST MODEL (M1-Backward method using p-value in t-test as metric)

m1=lm(pm2.5~.-year - No, na.action = na.omit,data=train.data);
summary(m1)
#Plotting residuals vs predicted values: To check constant variance
plot( fitted(m1), rstandard(m1), main="Predicted vs
residuals plot")
abline(a=0, b=0, col='red') #add zero line



#Plotting residuals vs each x-variable: 

#Plot residuals vs month:
plot(train.data$month, rstandard(m1), main="month vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs day:
plot(train.data$day, rstandard(m1), main="day vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs hour:
plot(train.data$hour, rstandard(m1), main="hour vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs DEWP:
plot(train.data$DEWP, rstandard(m1), main="DEWP vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs TEMP:
plot(train.data$TEMP, rstandard(m1), main="TEMP vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs PRES:
plot(train.data$PRES, rstandard(m1), main="PRES vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs Iws:
plot(train.data$Iws, rstandard(m1), main="Iws vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs Is:
plot(train.data$Is, rstandard(m1), main="Is vs residuals plot")
abline(a=0, b=0,col='red') #add zero line

#Plot residuals vs Ir:
plot(train.data$Ir, rstandard(m1), main="Ir vs residuals plot")
abline(a=0, b=0,col='red') #add zero line











#Tranformation of pm2.5
logdata <- log(train.data$pm2.5)
datatrans <- cbind(logdata, train.data) 
datatrans <- datatrans[,-c(2,7)]
inf.val <- is.infinite(datatrans$logdata) 
datatrans <- datatrans[!inf.val, ] 
model1 <- lm(logdata~., data = datatrans)
summary(model1)


#Transformation of Iws ,Is, Ir:


model2 <- lm(logdata~month+day+ hour + DEWP +  TEMP + PRES +log(Iws)+ log(Is) + log(Ir), data = datatrans)
summary(model2)




#Normal probability plot of residuals:

qqnorm(rstandard(model1))
qqline(rstandard(model1), col = 2) 


#Normal probability plot of residuals(pre tranformation):

qqnorm(rstandard(m1))
qqline(rstandard(m1), col = 2) 









# Evaluate Collinearity
install.packages("car")
library(car)
vif(model1) # variance inflation factors 

#Since VIF>4

cor(datatrans)


multicolm<-lm(logdata~., data = datatrans[,-8])# removing PRES  variable from the model
summary(multicolm)


#INFLUENTAIL POINTS

#Print all of the measures and influential points
influence.measures (multicolm) #influential point measures
summary (influence.measures (multicolm))#print out only influential observations


length(datatrans)
nrow(datatrans)





#INFLUENTAIL POINTS

influence.points <- influence.measures(multicolm)#influential point measures
cooksdist <- cooks.distance(multicolm)
threshold <- 4/(nrow(datatrans))
x <- which(cooksdist > threshold)
data.test <- datatrans[-x,]

finalmodel <- lm(data.test$logdata~. , data = data.test)
summary(finalmodel)



#5-Fold Cross Validation

install.packages("caret")
library(caret)

set.seed(123)
train.control<- trainControl(method = "cv" ,number=5)#Train the Model
model<-train(logdata~. , data = data.test,method="lm",trControl=train.control)
print(model)#Summary













































































