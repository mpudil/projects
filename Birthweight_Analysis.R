# Mitchell Pudil
# Birthweights Analysis

# Libraries to Download ---------------------------------------------------


library(ggplot2)  # Import GG Plot for professional graphs
library(car) # For avPlot
library(MASS) # For standardized residuals plot
library(lmtest) # For B-P test
library(multcomp) # For glht function (partial F stat)


# Birthweights EDA --------------------------------------------------------

# Import birthweight data
birthweights <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/1%20-%20IID/InClassCaseStudy/Data/BirthWeights.txt', sep='')


# 1. Scatterplot of Birthweight by Mage
ggplot(data=birthweights, mapping=aes(x=Mage,y=BirthWeight)) + geom_point() +
  ggtitle('Scatterplot of Birth Weight by Age of Mother') + # for the main title 
  xlab('Age of Mother') + # for the x axis label
  ylab('Birth Weight') # for the y axis label

# 2. Side-by-side boxplots of BirthWeight for each category in Race

ggplot(data=birthweights, mapping=aes(x=Race,y=BirthWeight)) + geom_boxplot(color=c('blue', 'red','green', 'orange')) +
  ggtitle('Boxplots of Birth Weight by Race') +
  xlab('Race') + 
  ylab('Birth Weight (in kilograms')


# 3. A scatterplot of BirthWeight by Gage where the dots are colored according to Gen

ggplot(data=birthweights, mapping=aes(x=Gage, y=BirthWeight, colour = Gen)) +
  geom_point() +
  geom_smooth() +
  ggtitle('Scatterplot of Birth Weight by Gestational Age') +
  xlab('Gestational Age (in weeks)') +
  ylab('Birth Weight (in kilograms') 

males <- subset(birthweights, birthweights$Gen=='Male')
lm(BirthWeight ~ Gage, data=males)

females <- subset(birthweights, birthweights$Gen=='Female')


# 4. The correlation between BirthWeight and Mage
birthweight.mage <- lm(BirthWeight ~ Mage, data=birthweights)
summary(birthweight.mage)
coef <- birthweight.mage$coef[2]
print(noquote(paste('The correlation between Birthweight and Mage is', round(coef,3))))


# 5. A pairs plot of all the variables in the BirthWeight dataset.

# Reorder birthweight columns to have birth weight as last column
birthweights_ordered <- birthweights[,c(2,3,4,1)]
install.packages('GGally')
library(GGally)
ggpairs(birthweights_ordered) + ggtitle('Pairs Plot of Birth Weight Data')

# Additional Summary Statistics of Data
summary(birthweights)

aggregate(birthweights$BirthWeight, by=list(birthweights$Race), FUN=mean) # Mean birthweight by race
aggregate(birthweights$BirthWeight, by=list(birthweights$Race), FUN=sd)  # SD birthweight by race
aggregate(birthweights$BirthWeight, by=list(birthweights$Gen), FUN=mean)  # Mean birthweight by gender

summary(lm(BirthWeight ~ Mage + Gage + as.factor(Race) + as.factor(Gen), data=birthweights))



# Analysis ----------------------------------------------------------------

# Begin by defining some objects that will be very useful throughout the analysis

birthweight.formula <- BirthWeight~Mage+Gage+as.factor(Race)+as.factor(Gen)
lm.birthweight <- lm(birthweight.formula, data=birthweights)

# 1a. Without the use of lm() calculate β̂ and s2.

  # Create X matrix

X <- model.matrix(object=birthweight.formula, data=birthweights)
  
  # Create y matrix
y <- birthweights$BirthWeight


  # Calculate β using β = (X'X)-1 X'y formula

beta.hat <- solve(t(X)%*%X) %*% (t(X)%*%y)
beta.hat # Prints beta hat values along with associated variables

  # Calculate s2 using (y−Xβ̂)′(y−Xβ̂)/(n−P−1) formula
n <- dim(birthweights)[1]
P <- dim(birthweights)[2] -1
yxb <- y - X%*%beta.hat
s2 <- (t(yxb)%*%yxb) * (1/(n-P-1))

s2 # Displays s2 value (appx. 79,000)


# 1b. Verify your answer using lm()

summary(lm.birthweight)
  # We can see here that the beta.hat matrix is the same as the coefficients
  # in the lm function, and that the sqrt of s2, which is the residual standard
  # error, is the same as shown in the lm() function.




# 2a: Without the use of lm() calculate the fitted values Xβ̂

fitted.values <- X%*%beta.hat  
fitted.values # Gives matrix of fitted values.



# 2b: Verify your calculations by pulling off the fitted values from an lm() object

fitted.lm <- fitted(lm.birthweight)
fitted.values - fitted.lm # If fitted.values is the same matrix as fitted.lm, which it
    # should be, then we should be getting a matrix of 0's, which we do. Therefore,
    # the calculated version, fitted.values, is equal to the fitted values from the 
    # lm function. 



# 3a. Without the use of lm() calculate the residuals y−Xβ̂

resid.values <- y-X%*%beta.hat
resid.values # Gives calculated residual values


# 3b. Verify your calculations by pulling off the residuals from an lm() object.

resid.lm <- resid(lm.birthweight)
resid.lm 

resid.values - resid.lm # Should be another column of 0's, which it is.

# 4. Identify your model R2 from the summary() output.

summary(lm.birthweight)$r.squared  # Gives R squared (~0.6)



# Checking Assumptions ----------------------------------------------------

 
# 1. Construct added variable plots and assess if the linearity assumption 
    # is OK for this data.


avPlots(lm.birthweight) # Linearity assumption is OK for this data. While there isn't
  # a strong relationship between Mage and BirthWeight, there is a pretty linear
  # relationship between Gage and Birthweight. All others are categorical, so we 
  # will disregard those.


# Independence (intuition)
   # It is likely that all babies are independent. Exceptions may include twins, or
   # if the same mother gave birth to multiple babies in the dataset, but this 
   # is a rare occurence, so we will assume independence for the dataset. 


# 2. Construct a histogram of the standardized residuals and run a KS-test to see 
  # if the normality assumption is OK for this data.

ggplot()+geom_histogram(mapping=aes(x=stdres(lm.birthweight)), bins=30) # Looks normal!

ks.test(stdres(lm.birthweight), "pnorm")  # The p-value is 0.48, so residuals are 
  # normally distributed.


# Draw a scatterplot of the fitted values vs. standardized residuals 
  # and run a BP-test to see if the equal variance assumption is OK for this data.

ggplot(data=birthweights, mapping=aes(x=fitted(lm.birthweight),y=resid(lm.birthweight))) + geom_point() +
  geom_hline(yintercept = 0, col='red') +
  ggtitle('Fitted Values vs Residual Values') + 
  xlab('Fitted Values') + 
  ylab('Standardized Residuals')


bptest(lm.birthweight)  # B-P test
    # p = 0.34, so we have homoskedasticity


# Predictions -------------------------------------------------------------

# 1. Without using predict.lm(), calculate your point prediction of the birth weight
  # for a baby with Mage=26, Gage=37, Race="hisp" and Gen="Female" using the formula
  # ŷnew=xnewβ̂, where β̂ is the maximum likelihood estimate that you calculated above


x.new <- matrix(nrow=1, c(1,26,37,1,0,0,0))
y.pred.new <- x.new %*% beta.hat
print(noquote(paste("The predicted value for this baby is", round(y.pred.new,3), "grams")))

  # Confirm that this is what predict.lm() is doing to get the point prediction.

df.newx <- as.data.frame(x.new)
colnames(df.newx) <- colnames(X)

predict.lm(lm.birthweight, newdata = data.frame(Mage=26, Gage=37, Race="hisp", Gen="Female"),
           interval="prediction", level=0.99)  # Prediction (including upper and lower bounds)



# Cross Validation --------------------------------------------------------


# 1a. Adjust the above code to run 100 Monte Carlo cross validations 


n.cv <- 100 #Number of CV studies to run
n.test <- round(dim(birthweights)[1]*0.3, 0) #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Select test observations
  n <- dim(birthweights)[1]
  test.obs <- sample(x=1:n, size=n.test)
  
  ## Split into test and training sets
  test.set <- birthweights[test.obs,]
  train.set <- birthweights[-test.obs,]
  
  ## Fit a lm() using the training data
  train.lm <- lm(formula=lm.birthweight, data=train.set)
  
  ## Generate predictions for the test set
  my.preds <- predict.lm(train.lm, newdata=test.set, interval="prediction")
  
  ## Calculate bias
  bias[cv] <- mean(test.set[['BirthWeight']]-my.preds[,'fit'])
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['BirthWeight']]-my.preds[,'fit'])^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg[cv] <- ifelse((test.set[['BirthWeight']] > my.preds[,'lwr']) & (test.set[['BirthWeight']] < my.preds[,'upr']),1,0) %>% mean()
  
  ## Calculate Width
  wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
}

#1b. Plot histograms (or density plots) of the bias, RPMSE, coverage and width.

# Create function that takes in variable and outputs a graph using ggplot
plot.hist <- function(variable){
  ggplot()+geom_histogram(mapping=aes(x=variable), bins=30)
}

plot.hist(bias) +  # Plot for bias
  ggtitle('Histogram of Bias') + 
  xlab('Bias') 

plot.hist(rpmse) +# Plot for RPMSE
  ggtitle('Histogram of RPMSE') + 
  xlab('RPMSE') 

plot.hist(cvg) +
  ggtitle('Histogram of Coverage') + 
  xlab('Coverage') # Plot for coverage

plot.hist(wid) +
  ggtitle('Histogram of Width') + 
  xlab('Width') # Plot for width


# Hypothesis Testing and Confidence Intervals -----------------------------

# 1 Using lm() construct the t−statistic and p-value for the test H0: βMage=0

a <- summary(lm.birthweight)
t.mage <- coef(a)['Mage', "t value"]
print(noquote(paste("The t value for the test H0: βMage=0 is:", round(t.mage,3))))

p.mage <- coef(a)['Mage', 'Pr(>|t|)']
print(noquote(paste("The p value for the test H0: βMage=0 is:", round(p.mage,3))))



#2. Using confint() and lm(), build a 90% confidence interval for βMage

conf <- confint(lm.birthweight,level=0.9)
conf.mage <- list(lower.bound = conf['Mage', '5 %'], upper.bound = conf['Mage', '95 %'])
conf.mage # 90% CI for βMage



#3. Using anova(), conduct a F test that race has no effect on birth weight

full.lm <- lm.birthweight
reduced.lm <- lm(BirthWeight ~ Mage + Gage + as.factor(Gen), data=birthweights)
anova(full.lm, reduced.lm) # Since the p-value on the F-stat is 0.0001 < 0.05, we 
  # determine that there is an effect of race on the weight of the baby.


#4 Using glht(), conduct a t test and 94% confidence interval for the difference in 
    # average birth weight of babies born with explantory variables Mage=24, Gage=40, 
    # Race="white" and Gen="Male" and babies born with explanatory variables Mage=34, 
    # Gage=33, Race="white" and Gen="Male".


a1 <- matrix(nrow=7,ncol=1, data=c(1, 24, 40, 0, 0, 1, 1))
a2 <- matrix(nrow=7,ncol=1, data=c(1, 34, 33, 0, 0, 1, 1))
a <- a1 - a2
a.transpose <- t(a)

mylm <- glht(lm.birthweight, linfct=a.transpose, alternative="two.sided")
summary(mylm) # t test

confint(mylm,level=0.94) # 94% CI


# END OF ASSIGNMENT -------------------------------------------------------

