# Mitchell Pudil
# Age-Related Macular Degeneration (ARMD)


# Import libraries and data -----------------------------------------------

library(nlme)
#install.packages('mvtnorm')
library(mvtnorm)
library(car)
#install.packages('multcomp')
library(multcomp)
armd <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/2%20-%20Longitudinal/InClassCaseStudy/Data/ARMD.txt', sep=' ')


# EDA ---------------------------------------------------------------------

armd$Trt <- as.factor(armd$Trt)

#Vision vs Baseline
ggplot(data=armd, mapping=aes(x=Baseline, y=Vision)) + geom_point() +
  xlab('Baseline') + ylab('Vision') + ggtitle('Scatterplot of Baseline and Vision')

# Vision over time, by Trt
ggplot(data=armd, mapping=aes(x=Time,y=Vision, color=Trt)) + geom_point() +
  geom_smooth(method=lm) +
  ggtitle('Average Vision for Treatment Over Time') + 
  xlab('Time') + 
  ylab('Vision for Treatment') +
  labs(color='Treated') +
  scale_color_manual(labels = c("No", "Yes"), values = c("steel blue", "tomato"))




# Analysis with an MLR ----------------------------------------------------


#1. Verify that the residuals of an independent MLR with a baseline effect and an 
  # interaction between Time and Trt are indeed correlated by calculating the 4×4 
  # correlation matrix Rˆ between residuals of the same person but between visits 

armd.lm <- lm(data = armd, formula=Vision ~ Baseline + Trt*Time) # Create lm
armd.resid <- resid(armd.lm)

corr.matrix <- cor(matrix(nrow=50, ncol=4, byrow=T, data=armd.resid))
corr.matrix

# Longitudinal MLR Model Fitting and Iterative Optimization ---------------

#1. Fit a linear regression model for Vision using a linear effect for Baseline, 
  # Time and Trt as well as the interaction of Time and Trt. In your model use a 
  # block diagonal general symmetric correlation structure within Subject but 
  # independent between Subject

# Vision = b0 + b1*Baseline + b2*Trt + b3*Time + b4*Trt*Time


armd.gls <- gls(model=Vision ~ Baseline + Trt*Time, data=armd, 
                correlation=corSymm(form=~1|Subject), method="ML")
  
    #Constrained estimates of the general correlation structure 

armd.gls$modelStruct

    # βˆ coefficients 
armd.gls$coefficients
  
  # Estimate of the variance parameter σ^2.
armd.gls$sigma^2


#2. In Stat 340... Verify that the iterative optimization routine returns a value near y¯.

max.like.fun <- function(data, fixed=c(FALSE, FALSE)){
  params <- fixed
  function(p){
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    -1 * sum(dnorm(data,mean=mu,sd=sigma,log=TRUE)) # Negative Log Likelihood
  }
}
y <- rnorm(100, mean=17, sd=sqrt(5))
nLL <- max.like.fun(y, c(FALSE, sqrt(5)))
optim(c(mu=13, sd=sqrt(5)), nLL)$par[1]

  # Compare with ML definition of mean

(1/length(y))*sum(y) # The estimate from the optim() returns a close appx to the mean


#3. We learned... use optim() to find the maximum likelihood estimates βˆ

ml.betas <- function(betaVec){
  X <- model.matrix(armd.lm)
  y <- armd$Vision
  -1*dmvnorm(x=y, mean=X%*%betaVec, sigma=(6.807^2)*nrow(X)*diag(200), log=TRUE)
}

beta.init <- matrix(ncol=1, data=c(mean(y), 0,0,0,0))
beta.estimates <- optim(par=c(beta.init), fn=ml.betas, method='L-BFGS-B')$par

X <- model.matrix(armd.lm)
y <- armd$Vision
beta.def <- solve(t(X)%*%X)%*%(t(X)%*%y)

beta.estimates - beta.def # The estimates from the optim() function and from the definition of
                          # beta hat are very similar.



# 4. Using myFunction below (which is bimodal), use optim() to find the maximum with 
  # starting values of -4 and 4. Show that when you start at -4, you get stuck in a 
  # local mode and fail to find the global maximum.

myFunction <- function(x){
  0.25*dnorm(x,-2,1)+0.75*dnorm(x,2,1) # Negative Log Likelihood
}

optim(par=c(x=4), fn=myFunction)$par[1] # Max is 16.4
optim(par=c(x=-4), fn=myFunction)$par[1] # Stuck at local max of -16



# Validating Longitudinal MLR Model Assumptions ---------------------------

source('stdres.gls.R')

# Linearity 
avPlots(armd.lm) # All of the graphs appear linear

# Independence
decorrelated.residuals <- stdres.gls(armd.gls)

corr.matrix.decorrelated <- cor(matrix(nrow=50, ncol=4, byrow=T, data=decorrelated.residuals))
corr.matrix.decorrelated # Within-subject residuals are not correlated

# Normality
ggplot() + geom_histogram(data=armd, mapping=aes(x=decorrelated.residuals)) +
  ggtitle('Histogram of Decorrelated Residuals') +
  xlab('Decorrelated Residuals')
  # Decorrelated residuals appear normally distributed

# Equal Variance
armd$fitted.values <- armd$Vision - decorrelated.residuals
armd$decorrelated.residuals <- decorrelated.residuals

ggplot(data=armd, mapping=aes(x=fitted.values, y=decorrelated.residuals)) + geom_point() +
  geom_hline(yintercept = 0, colour='red') +
  ggtitle('Fitted Values vs Residuals') +
  xlab('Fitted Values') + 
  ylab('Decorrelated Residuals')
  # There appears to be equal variance of error terms



# Statistical Inference ---------------------------------------------------

#1. Use a general linear hypothesis test on your longitudinal MLR to test if patients on the 
   # treatment have a significantly higher vision score at 52 than those not on the treatment.

a1 <- matrix(ncol=1, data=c(0,0,0,1,52))

test1 <- glht(armd.gls, linfct=t(a1), alternative="greater")
summary(test1)
  # p val = 0.002 < 0.05, so patients on treatment have a significantly 
  # higher vision score at 52 than those not on the treatment.


#2. Use a general linear hypothesis test on your longitudinal MLR to test if the treatment 
  # stops vision loss over time.

a2 <- matrix(ncol=1, data=c(0,0,0,1,1))

test2 <-  glht(armd.gls, linfct=t(a2), alternative="less")
summary(test2)

  # p-value < 0.0001 < 0.05, so vision still decays over time

  #2b. How much slower do those on interpheron lose their vision over Time?
a2b <- matrix(ncol=1, data=c(0,0,0,0,1))
test2b <-  glht(armd.gls, linfct=t(a2b), alternative="greater")
summary(test2b)
  # So those not on the treatment lose their vision at 0.16 lines per week faster

#3. Use a general linear hypothesis test on your longitudinal MLR to test if patients on/off 
  # the drug with a baseline of 29 are at risk of going legally blind (Vision < 20)

# On treatment
a3 <- matrix(ncol=1, data=c(1,29,1,52,52))
test3 <-  glht(armd.gls, linfct=t(a3), alternative="greater", rhs=20)
summary(test3)
  # Those on the treatment are not at risk of going blind (p-value = 0.000279)

a3b <- matrix(ncol=1, data=c(1,29,0,52,0))
test3b <-  glht(armd.gls, linfct=t(a3b), alternative="greater", rhs=20)
summary(test3b)
  # Those without the treatment are at risk of going blind (p-value 0.505)






