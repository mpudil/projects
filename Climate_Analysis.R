# Mitchell Pudil
# Climate Change EDA


# Import Libraries --------------------------------------------------------

library(splines) # For splines/bs()
install.packages('astsa')
library(astsa)


# EDA ---------------------------------------------------------------------


temp <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/1%20-%20TimeSeries/InClassCaseStudy/Data/AnnAvgGlobalClimate.txt', sep='')

#1: In the dataset, create a new YrMon variable.
temp$YrMon <- temp$Year + (temp$Month + 0.5)/12 # Create single month/year date variable


#2: Draw a time series plot of the temperature anomalies with YrMon along the x-axis with a smooth
  # curve overlaid to emphasize the non-linear aspect of the data.

ggplot(data=temp, mapping=aes(x=YrMon,y=AnnAnom)) +
  geom_line() + geom_smooth() +
  ggtitle('Temperature Anomaly Over Time') + # for the main title 
  xlab('Time') + # for the x axis label
  ylab('Temperature Anomaly in Celsius') # for the y axis label


#3: calculate and draw the ACF of the raw temperature anomalies to show the strong seasonal 
    # correlation in the data (note: you may have to increase lag.max to show 2 or 3 
    # seasonal cycles)

my.ACF <- acf(temp$AnnAnom, lag.max=50)
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col() + 
  ggtitle('ACF of Raw Temperature Anomalies') 



# Analysis ----------------------------------------------------------------

#1. Using bs(), define an X matrix for a linear spline for YrMon with a knot location at 1975. 
#Specify the boundaries to be the minimum of YrMon and 60 months past the maximum of YrMon 
#(because research question #2 asks us to predict forward 60 months past the end of our data). 

X <- bs(x=temp$YrMon, knots=1975, degree=1, Boundary.knots=c(min(temp$YrMon), max(temp$YrMon)+60*(1/12)))

#2. Using the X you created via linear spline, fit a linear model to fit a linear spline to the 
#annual temperature anomalies (i.e. do lm(y~X, data=)). Create a plot of the fitted regression 
#line on top of a time series plot to verify that the linear spline fits the data well.

climatelm <- lm(AnnAnom~X, data=temp)

#3: Draw an ACF of the residuals from your model in #2 to verify that there is indeed still 
  # temporal correlation in the residuals that we will need to model.

my.ACF <- acf(stdres(climatelm), lag.max=50)
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col() + 
  ggtitle('ACF of Raw Temperature Anomalies Stadard Error Terms') 





#4. Using sarima() choose which p, d, q, P, D, Q to use by comparing AIC or BIC values 
  # for the following models. Don’t forget that one seasonal cycle is 12 months so S=12 
  # and make sure to use your linear spline as the xreg= value (hint: try to set this up 
  # in a for-loop).


    # Create data frame to hold the different time series model parameters we will need
all.models <- as.data.frame(matrix(nrow=24,ncol=6, byrow=T, data=c(0,0,0,0,0,0,
                                   1,0,0,0,0,0,
                                   0,0,1,0,0,0,
                                   0,0,0,1,0,0,
                                   0,0,0,0,0,1,
                                  # 1,0,0,1,0,0, #(error here)
                                   0,0,1,0,0,1,
                                  # 1,0,1,1,0,1, #(error here)
                                   
                                   0,1,0,0,0,0,
                                   1,1,0,0,0,0,
                                   0,1,1,0,0,0,
                                   0,1,0,1,0,0,
                                   0,1,0,0,0,1,
                                   1,1,0,1,0,0,
                                   0,1,1,0,0,1,
                                   1,1,1,1,0,1,
                                   
                                   0,0,0,0,1,0,
                                   1,0,0,0,1,0,
                                   0,0,1,0,1,0,
                                   0,0,0,1,1,0,
                                   0,0,0,0,1,1,
                                   1,0,0,1,1,0,
                                   0,0,1,0,1,1,
                                   1,0,1,1,1,1)
                                   ))

colnames(all.models) <- c('p', 'd', 'q', 'P', 'D', 'Q')
all.models$AIC <- NA
all.models$BIC <- NA

  # For loop to determine AIC and BIC for each model
for(i in 1:24){
  p <- all.models[i,1]
  d <- all.models[i,2]
  q <- all.models[i,3]
  P <- all.models[i,4]
  D <- all.models[i,5]
  Q <- all.models[i,6]
  sarima.model <- sarima(xdata=temp$AnnAnom, p=p, d=d, q=q, P=P, D=D, Q=Q, S=12, xreg=X, details=FALSE)
  all.models[i,7] <- sarima.model$AIC
  all.models[i,8] <- sarima.model$BIC
}

  # Determine model by lowest AIC
chosen.model <- all.models[which.min(all.models$AIC),1:6] # Note this is the same result as 
        # if we were to choose to minimize the BIC instead


# 5. Fit your chosen model and examine the model estimates via the ttable object within a 
  # sarima() fit.

my.ts.model <- sarima(xdata=temp$AnnAnom, p=chosen.model$p, d=chosen.model$d, q=chosen.model$q, 
                      P=chosen.model$P, D=chosen.model$D, Q=chosen.model$Q, S=12, xreg=X, details=FALSE)

my.ts.model$ttable



# Model Validation --------------------------------------------------------

#1. Verify your assumptions of independence, normality and equal variance by drawing an ACF 
  # of the decorrelated residuals, a fitted values vs. residual plot and a histogram (or density 
  # plot) of the decorrelated residuals.


  # ACF of the decorrelated residuals


my.ACF <- acf(resid(my.ts.model$fit), lag.max=50)
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col() + 
  ggtitle('ACF of Decorrelated Residuals') 


  # fitted values vs. residual plot
fitted.values <- temp$AnnAnom - resid(my.ts.model$fit)
  
ggplot(data=temp, mapping=aes(x=fitted.values,y=resid(my.ts.model$fit))) +
  geom_line() + geom_smooth() +
  ggtitle('Fitted Values vs Residuals') + # for the main title 
  xlab('Fitted Values') + # for the x axis label
  ylab('Decorrelated Residuals') # for the y axis label


  # Histogram  of the decorrelated residuals

ggplot()+geom_histogram(mapping=aes(x=resid(my.ts.model$fit)), bins=30) +
  ggtitle('Histogram of Decorrelated Residuals') +
  xlab('Decorrelated Residuals')

#2. Validate your predictions by performing a cross validation. Split the last 60 time periods 
  # in your data into a test set and use the remaining as a training set (note you’ll have to 
  #split your X

npred <- 12*5
X <- with(temp,{
  bs(temp$YrMon, knots=1975, degree=1, Boundary.knots=c(min(temp$YrMon),max(temp$YrMon)+npred/12))
})
pred.time.points <- max(temp$YrMon)+seq(1/12,npred/12,by=1/12)
Xpred <- predict(X, newx=pred.time.points)
climate.for <- with(temp,{
  sarima.for(temp$AnnAnom,n.ahead=npred,p=1,d=1,q=1,P=1,D=0,Q=1,S=12,
             xreg=X,
             newxreg=Xpred)
})

actual <- temp$AnnAnom[757:816]
pred <- climate.for$pred
se <- climate.for$se

n <- length(temp$AnnAnom) - 60 # Since 60 observations were saved as the test set
p <- dim(my.ts.model$ttable)[1]
df <- n-p
t <- qt(0.975, df)

lb <- pred - t*se
ub <- pred + t*se

rpmse <- (actual - pred)^2 %>% mean %>% sqrt() # RPMSE
cvg <- (actual > lb & actual < ub) %>% mean() # Coverage

list(rpmse=rpmse, coverage=cvg)



# Statistical Inference ---------------------------------------------------

#1. Identify the p-value for a test that H0:β2=0 vs HA:β2>0. Also, calculate
  # a 95% confidence interval for β2.


  # P-value
my.ts.model$ttable[6,4]/2 # P-value for Ha: β2 > 0 is 0. So there is a change at 1975. 

  # Confidence Interval
estimate <- my.ts.model$ttable[6,1]
se <- my.ts.model$ttable[6,2]
t <- qt(0.95,df=816-(ncol(X) + 5))
conf <- estimate + c(-1,1)*t*se

#2. Predict the temperature anomalies forward 60 months (5 years). To do this, you will have to set up 
  # your linear spline forward in time.

pred.YrMon <- max(temp$YrMon) + seq(1/12, 60*(1/12), by=1/12)
Xpred <- predict(X, newx=pred.YrMon)
my.for <- sarima.for(temp$AnnAnom,n.ahead=npred,p=1,d=1,q=1,P=1,D=0,Q=1,S=12,
                     xreg=X,newxreg=Xpred)

