## load data
getwd()

data<- read.table("CarPrice_Assignment-updated.csv",sep=',',header=TRUE)
print(head(data))

data$fueltype<-as.factor(data$fueltype)
data$aspiration<-as.factor(data$aspiration)

#Number of rows and columns data.frame
print(dim(data))
#Row names
print(rownames(data))
#Variable names
print(colnames(data))

##first step: description

#Desc. Statistics
print(summary(data))
sd(data$price)
sd(data$price)/mean(data$price)

#203533.7/307007=0.6629611
#sd(salary)/mean(salary)=0.66 so the coefficient of variation is 66% > 30% 
#so our distribution is heterogeneous and we have dispersion around the mean

#boxplot
par(mfrow=c(2,2))
boxplot(data$price, main = 'Price')
boxplot(log(data$price), main = 'log of the Price')
#boxplot(data$fueltype)
#boxplot(data$aspiration) 
boxplot(data$horsepower, main = 'Boxplot of HorsePower')
hist(data$peakrpm, main = 'Boxplot of PeakRpm')
#?

hist(data$price, main = 'Price')
hist(log(data$price), main = 'Log Price')

#log(Y) seems to be normal
#mnshuf l normality lal Y 3shen momken t3ml mashekel bl residuals

#Scatter plot
pairs(data)
#?
#we have very strong linear relationship between x4 and x5
#y3ne 3na colinearity
#x2 dummy variable
#pos lin rel between x4 w Y w kmn between x5 w Y (tabi3e)
#adv of scatter plot: graphical interpretation about linearity

##second step: estimation

# multiple  Regression
model <- lm(log(price) ~ ., data = data)
print(model)
#summary
sm <- summary(model)
print(sm)
AIC(model) #76114.68

#eza bde a3ml backward la le2e best fitted model, hon 3nde overall sig bsir shil
#l var li 3ndo akbar pvalueb w bsir a3ml drop wahad wahad
#bwa2ef lama ysiro kel l var significant (hon at 10% l2n fi point)
#AIC=361.7155
#all var should be sig (at 10% msln) la fine est3ml AIC w a3ml comparison
#diagnostics b3melon bas 3l best fitted model

model1 <- lm(price ~ fueltype+aspiration+wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+
               +horsepower+peakrpm+citympg+highwaympg, data = data)
summary(model1)
AIC(model1) #76112.71

model2 <- lm(price ~ fueltype+wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+
               +horsepower+peakrpm+citympg+highwaympg, data = data)
summary(model2)
AIC(model2) #76110.75

model3 <- lm(price ~ fueltype+wheelbase+carlength+carwidth+carheight+curbweight+enginesize+stroke+
               +horsepower+peakrpm+citympg+highwaympg, data = data)
summary(model3)
AIC(model3) #76109.2 m3 log 4110.072


model4 <- lm(price ~ fueltype+wheelbase+carlength+carwidth+carheight+enginesize+stroke+
               +horsepower+peakrpm+citympg+highwaympg, data = data)
summary(model4)
AIC(model4) #76110.98

model5 <- lm(price ~ fueltype+wheelbase+carlength+carwidth+carheight+enginesize+stroke+
               +horsepower+peakrpm+citympg, data = data)
summary(model5)
AIC(model5) #76110.98

model6 <- lm(price ~ fueltype+carlength+carwidth+carheight+enginesize+stroke+
               +horsepower+peakrpm+citympg, data = data)
summary(model6)
AIC(model6) #76110.98

model7 <- lm(price ~ fueltype+carwidth+carheight+enginesize+stroke+
               +horsepower+peakrpm+citympg, data = data)
summary(model7)
AIC(model7) #76110.98

model8 <- lm(price ~ fueltype+carwidth+carheight+enginesize+stroke+
               +horsepower+peakrpm + citympg + highwaympg, data = data)
summary(model8)
AIC(model8) #76110.98

model9 <- lm(price ~ fueltype+carwidth+carheight+enginesize+stroke+
            +horsepower+peakrpm, data = data)
summary(model9)
AIC(model9) #76110.98

modeltest <- lm(log(price) ~ fueltype + carwidth + curbweight + enginesize + 
  stroke + horsepower + peakrpm + citympg + highwaympg, data = data)

summary(modeltest)
AIC(modeltest) #76110.98

#model9 <- lm(price ~ fueltype+carwidth+enginesize+stroke+
#               +horsepower+peakrpm, data = data)
#summary(model9)
#AIC(model9) #76110.98

#Residuals analysis
e9 <- model9$residuals #or e <- residuals(modele)
print(mean(e9))

par(mfrow=c(1,2))
plot(fitted(model9), resid(model9), pch=20, main="Residual vs. Fitted")
abline(h=0, lty=2)
lines(loess.smooth(fitted(model9), resid(model9), span=0.9), col="red")
qqnorm(resid(model9), pch=20); qqline(resid(model9))

hist(e9)

acf(e9)

#Residuals analysis
e <- modeltest$residuals #or e <- residuals(modele)
print(mean(e))

#Graph
plot(data$price,e,ylab="Residuals",xlab="Price")
abline(h=0)
#ma3nde equality of variance

par(mfrow=c(1,2))
plot(fitted(modeltest), resid(modeltest), pch=20, main="Residual vs. Fitted")
abline(h=0, lty=2)
lines(loess.smooth(fitted(modeltest), resid(modeltest), span=0.9), col="red")
qqnorm(resid(modeltest), pch=20); qqline(resid(modeltest))

#Dqqnorm
qqnorm(e)
#not normally dist, m3 log saret normal

#normality test

library(tseries)
jarque.bera.test(e)
hist(e)
#kelo 3atane not normal, m3 log sar l hist normal

#autocorelation
acf(e)
#no autocorr

#multicolinearity
library(faraway)
vif(modeltest)
#no multicolinearity

#Selection by seowise "back"
library(MASS)
modele.sel1<- stepAIC(model,direction="both")

#Selection by seowise "back"
modele.sel<- stepAIC(model,direction="backward")

#Detection of atypical and influential points
par(mfrow=c(2,2))
plot(modeltest, which=1:4, pch=20)
#Studentized residuals
res.student <- rstudent(modeltest)
print(res.student)

#Critical threshold  value for the studentized residual 
#riskalpha = 0.05
alpha <- 0.05
#calculation of the threshold from the Student's distribution to (n-p-2) degree of freedom ==> n = 40 obs., p = 13 independent variables
thershold.student <- 2
print(thershold.student)

#boolean vectors indicating atypical

atypical.rstudent <- (res.student < -thershold.student | res.student > +thershold.student)
ab.student <- data[atypical.rstudent ,]
print(ab.student)

#show atypical points in the residual graph
#construction of the studentised residuals graph
plot(log(data$price),res.student,cex=0.75)
abline(h=-thershold.student)
abline(h=+thershold.student)
abline(h=0)
text(data$price[atypical.rstudent],res.student[atypical.rstudent],rownames(data)[atypical.rstudent])

#Leverage (another way to test outliers)
indicators <- influence.measures(modeltest)
#
attributes(indicators )
# hat matrix
print(indicators $infmat)

#"hat" from the hat matrix
res.hat <- indicators$infmat[,"hat"]
print(res.hat)

#Leverage considered large if it is bigger than twice the mean leverage value 2x(p+1)/n ==> p = 13 expl., n =40 obs.
tehrshold.hat <- 2*(17)/205
print(tehrshold.hat )

#atypical leverage points
atypical.leverage<- (res.hat > tehrshold.hat)
ab.hat <- data[atypical.leverage,]
print(ab.hat)
#?

#cook's distance 
#threshold  value for COOK
cutoff<-4/(205-16-1)
plot(modeltest, which=4)
cooks.distance(modeltest)
#delete the atypical points of the database
#identify the elements to exclude
excluded <- (atypical.rstudent)
print(excluded)
dim(excluded)
summary(modeltest)
AIC(modeltest)
##model 3 : R^2 = 0.07467 and AIC= 76109.2

#new data frame : we keep the non-excluded ==> !excluded
data.clean <- data[!excluded,]
print(data.clean)
dim(data.clean)
summary(data.clean)

#??????????
#model with cleandata


modelTestNew <- lm(log(price) ~ fueltype + carwidth + curbweight + enginesize + 
       stroke + horsepower + peakrpm + citympg + highwaympg, data = data.clean)

summary(modelTestNew)
AIC(modelTestNew) #76109.2 m3 log 4110.072 #with clean  2967.43


#model4 <- lm(Salary ~ Gender+collegeGPA+English+Logical+conscientiousness+agreeableness
#            , data = data)
#summary(model4)
#AIC(model4) #76110.98
#model6<-lm(log(Salary) ~ collegeGPA+English+Logical+conscientiousness+agreeableness
#           +nueroticism, data = data.clean)
#summary(model6)
#AIC(model6) #2981.937

#Residuals analysis
e <- modelTestNew$residuals #or e <- residuals(modele)
print(mean(e))

#Graph
plot(log(data.clean$price),e,ylab="Residuals",xlab="Salary")
abline(h=0)
#3nde equality of variance

#Dqqnorm
qqnorm(e)
#not normally dist, m3 log saret normal

par(mfrow=c(1,2))
plot(fitted(modelTestNew), resid(modelTestNew), pch=20, main="Residual vs. Fitted")
abline(h=0, lty=2)
lines(loess.smooth(fitted(modelTestNew), resid(modelTestNew), span=0.9), col="red")
qqnorm(resid(modelTestNew), pch=20); qqline(resid(modelTestNew))

#normality test

library(tseries)
jarque.bera.test(e)
hist(e)
#kelo 3atane not normal, m3 log sar l hist normal

#autocorelation
acf(e)
#no autocorr

#multicolinearity
library(faraway)
vif(modelTestNew)
#no multicolinearity

#loading the second file
data0<- read.table("predictPrice.csv",sep=',',header=TRUE)
print(head(data0))

#predections(fit) with confidence interval (lwr, upr)
pred <- predict(modelTestNew,newdata=data0,interval="prediction",level=0.9)
predIni <- exp(pred)
print(exp(pred))
print(head(exp(pred)))
#we have large CI y3ne model not so gd
#hata  lw tol3 true
summary(data$price)

#Cross validatation
TrueValue<- data0$price
Yhat<-predIni
#verification
quid <- (TrueValue >= predIni[,'lwr']) & (TrueValue < predIni[,'upr'])
print(quid)
