# Mitchell Pudil
# TV Viewership



# Import libraries and Game of Thrones data -------------------------------
library(splines) # For splines/bs()
library(astsa)
library(reshape) # For melt function
library(car)
#install.packages('lmtest')
library(lmtest) # For BP test

thrones <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/1%20-%20TimeSeries/HWCaseStudy/Data/Viewership.txt', sep=' ', header=T)
thrones$logviewers <- log(10^6*thrones$Viewers) # Since viewership is in millions

# 1. Create exploratory plots and calculate summary statistics from the time series.
  # Comment on any potential relationships you see between log(Viewers) and ShowNum 
  # (note, we are using ShowNum to denote “time” in this analysis).

  # Plot of episode number vs viewership
ggplot(data=thrones, mapping=aes(x=ShowNum,y=logviewers)) +
  geom_line() + geom_smooth() +
  ggtitle('Game of Thrones Viewership by Show Number') + 
  xlab('Episode Number') + 
  ylab('Log(Viewers)') 

  # Histogram of number of viewers
ggplot(data=thrones) + geom_histogram(mapping=aes(thrones$Viewers)) +
  xlab('Number of Viewers (in millions)') +
  ggtitle('Histogram of Number of Viewers per Episode')


#2. Fit a linear regression model to log(Viewers) using ShowNum as the explanatory 
  # variable. Determine if there is temporal correlation in the residuals which 
  # should be accounted for in your model. Discuss what this temporal correlation 
  # means for viewership.

  # Fitting linear model
throneslm <- lm(logviewers~ShowNum, data=thrones)

  # Determining Temporal Correlation
my.ACF <- acf(stdres(throneslm), lag.max=70)
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col() + 
  ggtitle('ACF of TV Viewership Stadard Error Terms') 
  # This suggests that there is a negative correlation between seasons, so after a 
  # good season, viewership is lower, and vice-versa.


#3. Fixing d=0 and D=1, determine appropriate values of p, q, P, Q in your time series 
  # model (note you should be able to figure out the seasonal cycle value S). Only
  # consider p∈{0,1,2}, q∈{0,1,2}, P∈{0,1}, and Q∈{0,1} . Discuss how you came to 
  # choose your specific values. pdqPDQ

p <- c(0,1,2)
q <- c(0,1,2)
P <- c(0,1)
Q <- c(0,1)
d <- 0
D <- 1
S <- 10
all.combos <- expand.grid(p,d,q,P,D,Q)
names(all.combos) <- c("p", "d", "q", "P", "D", "Q")
aic.vals <- rep(0, nrow(all.combos))
for(m in 1:nrow(all.combos)){
  sfit1 <- sarima(log(thrones$logviewers), p=all.combos[m,"p"], d=all.combos[m,"d"],
                  q=all.combos[m,"q"], P=all.combos[m,"P"], D=all.combos[m,"D"],
                  Q=all.combos[m,"Q"], S=S, details=FALSE, xreg=thrones$ShowNum)
  aic.vals[m] <- sfit1$AIC
}

chosen.model <- all.combos[which.min(aic.vals),]


# 5. Fit your chosen time series model and validate any model assumptions you used.

# Fitting the model
X <- thrones$ShowNum 

my.ts.model <- sarima(xdata=thrones$logviewers, p=chosen.model$p, d=chosen.model$d, q=chosen.model$q, 
                      P=chosen.model$P, D=chosen.model$D, Q=chosen.model$Q, S=10, xreg=X, details=FALSE)


# Validating model assumptions

# Linearity 
avPlots(throneslm) # Model appears linear

# Independence - fixed using SARIMA model

# Normality of residuals

ggplot()+geom_histogram(mapping=aes(x=stdres(throneslm)), bins=30) +
  ggtitle('Histogram of Standard Residuals') +
  xlab('Standard Residuals')
resids <- stdres(throneslm)
ks.test(resids, 'pnorm')  # With a large p-value, we determine that the residuals 
# are normally distributed.

# Equal Variance
bptest(throneslm) # Homoskedastic (p-value = 0.215)



# 6. Perform a cross-validation of predictions generated from your model for the most 
  # recent season of shows. Report the quality of your predictions in terms of RPMSE. Provide 
  # a plot of your predictions along with observed viewership and 95% prediction interval limits.
npred <- 10

train.rows <- c(1:(nrow(thrones)-npred))
thrones.train <- thrones[train.rows,]
thrones.test <- thrones[-train.rows,]
X.train <- X[train.rows]
X.test <- X[-train.rows]


thrones.for <- with(thrones.train,{
  sarima.for(thrones.train$logviewers,n.ahead=npred,p=2,d=0,q=0,P=0,D=1,Q=1,S=10, 
             xreg=X.train, newxreg = X.test)
})

lines(x=thrones.test$ShowNum, y=thrones.test$logviewers, col = "black") 





actual <- thrones.test$logviewers
pred <- thrones.for$pred
se <- thrones.for$se

n <- nrow(thrones.train) # Since 60 observations were saved as the test set
p <- ncol(as.matrix(X)) + chosen.model$p + chosen.model$q + chosen.model$P + chosen.model$Q
df <- n-p
t <- qt(0.975, df)

lb <- pred - t*se
ub <- pred + t*se

rpmse <- (actual - pred)^2 %>% mean %>% sqrt() # RPMSE
cvg <- (actual > lb & actual < ub) %>% mean() # Coverage

list(rpmse=rpmse, coverage=cvg)

# Graphic for cross-validation
thrones$lower <- NA
thrones$lower[61:70] <- exp(lb)/10^6
thrones$upper <- NA
thrones$upper[61:70] <- exp(ub)/10^6
thrones$pred <- NA
thrones$pred[61:70] <- exp(pred)/10^6

pred.df <- subset(thrones, ShowNum >= 60)


ggplot(thrones,mapping=aes(x=ShowNum, y=Viewers)) +
  geom_ribbon(data=thrones, aes(ymin = lower, ymax = upper), alpha=0.3, fill='dark gray') +
  geom_point(data=subset(thrones, ShowNum < 60), shape=1) + geom_line(data=subset(thrones,ShowNum < 60)) +
  geom_point(data=subset(thrones, ShowNum >= 60), shape=1, colour='red') + 
  geom_line(data=subset(thrones, ShowNum >= 60), mapping=aes(x=ShowNum, y=Viewers), col='firebrick') +
  ggtitle('Cross-Validation Predictions for Viewers by Show') + 
  xlab('Show Number') + 
  ylab('Viewers (in millions)') 


# 7.	Determine if viewership is increasing or decreasing. Support your conclusions with 
# appropriate hypothesis tests and confidence intervals.7.	Determine if viewership is 
# increasing or decreasing. Support your conclusions with appropriate hypothesis tests and 
# confidence intervals.

my.ts.model$ttable

estimate <- my.ts.model$ttable[4,1]
se <- my.ts.model$ttable[4,2]
t <- qt(0.975,df=nrow(thrones)-(ncol(X) + 4))
conf <- estimate + c(-1,1)*t*se



#8. Season 8 is already in production. Forecast the log(Viewers) forward for season 8. Comment 
  # on how executives would be able to use these forecasts to gauge if the show should continue 
  # into a ninth season.

# Forecasting
pred.time.points <- max(thrones$ShowNum)+c(1:10)
#Xpred <- predict(X, newx=pred.time.points)

my.for <- sarima.for(thrones$logviewers, n.ahead=npred,p=2,d=0,q=0,P=0,D=1,Q=1,S=10,
                     xreg=X,newxreg=pred.time.points)


future.viewers <- my.for$pred


# Determine lower and upper bounds
n <- nrow(thrones) # Since 60 observations were saved as the test set
p <- ncol(as.matrix(X)) + chosen.model$p + chosen.model$q + chosen.model$P + chosen.model$Q
df <- n-p
t <- qt(0.975, df)

lb <- future.viewers - t*se
ub <- future.viewers + t*se


# Graphic for future viewers (in millions)

future <- data.frame(matrix(ncol=8, nrow=10, NA))
colnames(future) <- colnames(thrones)
future$lower[1:10] <-lb
future$upper[1:10] <- ub
future$logviewers[1:10] <- future.viewers
future$Viewers[1:10] <- exp(future.viewers)/(10^6) # Predicted viewers (millions)
future$ShowNum <- c(71:80)

ggplot(thrones,mapping=aes(x=ShowNum, y=logviewers)) + geom_point() + geom_line() +
  geom_point(data=future, mapping=aes(x=ShowNum, y=logviewers), col='red') + 
  geom_line(data=future, mapping=aes(x=ShowNum, y=logviewers), col='coral3') +
  geom_ribbon(data=future, aes(ymin = lower, ymax = upper), fill='pink', alpha=0.3) +
  ggtitle('Predictions for Season 8 Viewers') + 
  xlab('Show Number') + 
  ylab('Log(Viewers)')




ggplot(thrones,mapping=aes(x=ShowNum, y=Viewers)) +
  geom_ribbon(data=thrones, aes(ymin = lower, ymax = upper), alpha=0.3, fill='blue') +
  geom_point() + geom_line() +
  # geom_line(data=thrones, mapping=aes(x=ShowNum, y=pred)) +
  ggtitle('Cross-Validation Predictions for Viewers by Show') + 
  xlab('Show Number') + 
  ylab('Viewers (in millions)') 





