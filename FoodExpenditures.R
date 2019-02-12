# Mitchell Pudil
# Homework 2: Food Expenditures



# Import libraries and data -----------------------------------------------

library(ggplot2)
library(lubridate)
library(nlme)
library(magrittr)
library(lmtest) # For BP test
source("Predictgls.R", local=T) # For Dr. Heaton's predictgls function (I put
            # the predictgls.R function in the same working directory as my .R
            # file, so if you are not, you may need to change to local=F)


food <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/2%20-%20Diagonal/HWCaseStudy/Data/FoodExpenses.txt', header=T, sep=' ')


# Exploratory Data --------------------------------------------------------

# Question 1

summary(food$Income)
summary(food$EatingOut)

# Histogram of Eating Out

ggplot(food) + geom_histogram(aes(x=EatingOut))

# Scatterplot of Income & Eating Out
ggplot(data=food, mapping=aes(x=Income,y=EatingOut)) + geom_point() +
  geom_smooth() +
  ggtitle('Scatterplot of Eating Out by Income') + # for the main title 
  xlab('Income') + # for the x axis label
  ylab('Expenditures on Food not Cooked at Home') # for the y axis label


# 2. Using a homoskedastic linear model, fit a regression model to EatingOut 
# using Income as the explanatory variable. Determine if the equal variance 
# assumption is met. If it not met, discuss what impact the violation of this 
# assumption could have on an analysis on the relationship between income and 
# food expenditure.


foodlm <- lm(EatingOut ~ Income, data=food)

food$Prediction <-  predict.lm(foodlm)
food$Resids <- stdres(foodlm)

ggplot(data=food, mapping=aes(x=Prediction,y=stdres(foodlm))) + 
  geom_point() +
  ggtitle('Scatterplot of Fitted vs. Standardized Residuals') + # for the main title 
  xlab('Fitted Values') + # for the x axis label
  ylab('Standardized Residuals') # for the y axis label 

bptest(foodlm) # We have heteroskedasticity (p-value < 0.0001)



# 4.Fit your model from #3 to EatingOut. Validate the model L-I-N-E assumptions
# so you will be confident that the statistical inference you perform below will
# be correct.

foodgls <- gls(model=EatingOut~Income, data=food, weights=varExp(form=~Income), method="ML")
summary(foodgls)

# Standardize Residuals from gls

food$glsresid <- resid(object=foodgls, type="pearson")
foodgls$sigma
foodgls$modelStruct
# Plot residuals 

ggplot() + 
  geom_histogram(mapping=aes(x=food$glsresid), bins=30) +
  ggtitle('Histogram of stanardized residuals') +
  xlab('Standardized Residuals')


# 5. Validate your predictions based on your model in #3 via cross-validation
# (any of leave-one-out, Monte Carlo or K-fold). Report your model RPMSE and 
# coverage. Additionally, show your predictions and 95% prediction interval 
# bounds on a scatterplot of income vs. food expenditure.

set.seed(469)
n.cv <- 500 #Number of CV studies to run
n.test <-  round(nrow(food)*0.25) #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Select test observations
  food <- food[,c(1,2)]
  test.obs <- sample(x=1:dim(food)[1], size=n.test, replace=FALSE)
  
  ## Split into test and training sets
  test.set <- food[test.obs,]
  train.set <- food[-test.obs,]
  
  # GLS model for train data
  traingls <- gls(model=EatingOut~Income, data=train.set, weights=varExp(form=~EatingOut), method="ML")
  
  
  ## Generate predictions for the test set
  all.preds <- predictgls(traingls, newdframe=test.set)
  my.preds <- all.preds['Prediction']
  
  ## Calculate bias
  my.preds <- as.numeric(unlist(my.preds))
  bias[cv] <- mean(my.preds-as.numeric(unlist(test.set['EatingOut'])))
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Income']]-my.preds)^2 %>% mean() %>% sqrt()
  
  # Calculate prediction interval
  se.pred <- all.preds['SE.pred'] # Calculate se of predictions
  error <- qt(1-0.05/2, df=nrow(train.set)-length(coef(traingls)))*se.pred  # Calculate error of predictions
  pred.int.low <- my.preds - error
  pred.int.upr <- my.preds + error
  
  ## Calculate Coverage 
  cvg[cv] <- ((test.set[['EatingOut']] > pred.int.low) & (test.set[['EatingOut']] < pred.int.upr)) %>% mean()
  
  
}

# Report of bias, rpmse, coverage, and width FOR EACH Monte Carlo Simulation:
list(bias=bias, rpmse=rpmse, coverage=cvg)
ggplot() + geom_histogram(aes(bias))
ggplot() + geom_histogram(aes(rpmse))
ggplot() + geom_histogram(aes(cvg))

# Average bias, rpmse, coverage, and width overall ALL Monte Carlo Simulations:
list(bias=mean(bias), rpmse=mean(rpmse), coverage=mean(cvg))


# Plot of predictions
foodpreds <-  predictgls(foodgls, newdframe = food)

se.pred <- all.preds['SE.pred'] # Calculate se of predictions
error <- qt(1-0.05/2, df=dim(food)[1]-length(coef(foodgls)))*foodpreds$SE.pred  # Calculate error of predictions
foodpreds$lower <- foodpreds$Prediction - error
foodpreds$upper <- foodpreds$Prediction + error

ggplot(data=foodpreds, aes(x = unlist(Income))) + geom_point(aes(y = unlist(EatingOut))) + 
  geom_ribbon(data=foodpreds, aes(ymin = unlist(lower), ymax = unlist(upper)), fill = "blue", alpha = 0.2) +
  ggtitle('Predictions of Eating Out by Income') + # for the main title 
  xlab('Income (in $1000s)') + # for the x axis label
  ylab('Predicted Amount of Eating Out') # for the y axis label



#6 


intervals(foodgls,level=0.95)

# 95% CI for betas:
confint(foodgls)


# 7. Economists with the National Restaurant Association (which, perhaps 
  # unfortunately, shares its acronym with another institution), hypothesize 
  # that a “healthy” restaurant economy should see increases of about $0.50 or 
  # more per week for each $1000 increase in income. Using your heteroskedastic 
  # model, test if the economy is “healthy” for restaurant owners. State your hypotheses,
  # p-value and an appropriate conclusion.


n <- dim(food)[1]
P <- 1
se <- summary(foodgls)$tTable[2,2]
bta <- coef(foodgls)[2]
test.stat <- (bta-0.5)/se
pvalue <- pt(test.stat, df=n-(P+1), lower.tail=TRUE) # P-value < 0.0001, so we 
  # reject H0 that the economy is healthy in favor of an unhealthy economy. 


# 8. Predict how much you will be spending at restaurants for your desired income level
  # upon graduation (meaning at your first job). Report a 95% prediction interval and 
  # interpret the interval in context.
pred.and.sd <- predictgls(foodgls, newdframe = data.frame(Income=110))  
pred <- pred.and.sd['Prediction']
colnames(pred) <- ''
se <-  pred.and.sd['SE.pred']
error <- qt(1-0.05/2, df=dim(food)[1]-2)*se
lower <- pred - error
colnames(lower) <- ''
upper <- pred + error
colnames(upper) <- ''
list(estimate=pred, lower.bound = lower, upper.bound = upper) # Prediction & 95% Pred. Interval



