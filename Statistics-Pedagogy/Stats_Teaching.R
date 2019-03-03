# Mitchell Pudil
# Class Pedagogy Test

# Download Libraries
library(reshape) # For melt function
library(ggplot2) # For graphics
library(car) # For added variable plots
library(MASS) # For stdres
library(lmtest) # For B-P test
library(magrittr) # For piping
library(multcomp) # For glht function
library(stats)
library(nlme) # For gls
# Download Data
class <- read.table(file='https://mheaton.byu.edu/Courses/Stat469/Topics/1%20-%20Independence/3%20-%20Project/Data/ClassAssessment.txt', 
                    header=T, stringsAsFactors = F)


# EDA ---------------------------------------------------------------------

# Boxplot of all test, quiz, and homework scores

classmelt <- melt(class[,-c(2,9)], id=c('Semester'))
ggplot(classmelt)+geom_boxplot(aes(x=variable, y=value, col=as.factor(Semester))) +
  ggtitle('Boxplots of Student Scores by Semester') +
  xlab('Activity') +
  ylab('Average Score (in %)') +
  labs(col="Semester")


# Scatterplot of Final Test score vs number of students
ggplot(data=class, mapping=aes(x=NStudents,y=Final)) + geom_point() +
  geom_smooth() +
  ggtitle('Scatterplot of Final Test Score by Number of Students') + # for the main title 
  xlab('Number of Students') + 
  ylab('Final Exam Score')

cor(class$NStudents, class$Final)


# Scatterplot of Final Test score vs average midterm score
class$average_midterm <- NA
for(i in 1:dim(class)[1]){
  class$average_midterm[i] <- (class$Exam1[i] + class$Exam2[i] + class$Exam3[i])/3
}


ggplot(data=class, mapping=aes(x=average_midterm,y=Final)) + geom_point() +
  geom_smooth() +
  ggtitle('Scatterplot of Final Test Score by Average Midterm Score') + # for the main title 
  xlab('Average Midterm Score') + 
  ylab('Final Exam Score')

# Histogram of class sizes
ggplot() + geom_histogram(aes(class$NStudents)) + 
  xlab('Number of Students') +
  ggtitle('Histogram of Number of Students per Classroom Observation')




# Correlation of quizzes/midterm
cor(class$average_midterm, class$Final)
cor(class$Exam1, class$Exam2)
cor(class$Exam1, class$Exam3)
cor(class$Exam2, class$Exam3)
var(class$Final)

# Notes: Small sample size, small variance in dependent var (Final score)




# Analysis ----------------------------------------------------------------


# First model: basic, additive linear regression

# Create variable for fall vs winter to still use semester, but save on df
for(i in 1:dim(class)[1]){
  class$Fall[i] <- ifelse(class$Semester[i]%%2==0, 1,0) 
}

# Create dummy variable for small and large class sizes
class$LargeClass <- ifelse(class$NStudents > 400, 1, 0)

# Heteroskedastic Model ---------------------------------------------------
class$invstudents <- 1/class$NStudents
class.gls <- gls(Final~Exam1 + Exam2 + Exam3 + HW + Quiz + as.factor(Semester) + NStudents, data=class, weights=varFixed(~invstudents), method="ML")

classlm <- lm(Final~Exam1 + Exam2 + Exam3 + HW + Quiz + as.factor(Semester) + NStudents, data=class)
  # Note that for linearity assumption gls = lm model

# Check Line Assumptions

# L: Linear - added variable plot

class <- class[,-c(9)] # Get rid of average midterm variable, since we want to look at each
# individual midterm score now
install.packages('car')
library(car)
avPlots(classlm)
  # Each plot looks linear (or at least none of them seem to have an obvious different pattern)

# I : Independence
  # See project


# N: Normality of standardized residuals
ggplot()+geom_histogram(mapping=aes(x=stdres(classlm)), bins=30) +
  ggtitle('Histogram of Standard Residuals') +
  xlab('Standard Residuals')
resid.gls <- resid(class.gls, type='pearson')
ks.test(resid.gls, 'pnorm')  # With a large p-value, we determine that the residuals 
  # are normally distributed.

# E: Equal Variance

bptest(classlm)  # B-P test
# While the BP test tells us we have homoskedasticity, we still need to correct for the fact
  # that the final test score (y) is a mean of students' scores and use a fixed weight
  # of 1/n_i, where n is the number of students in the ith classroom. 


pred <- predictgls(class.gls, newdframe=class)['Prediction']
se <-  predictgls(class.gls, newdframe=class)['SE.pred']
error <- qt(1-0.05/2, df=dim(pred)[1]-2)*se
class$pred.low <- pred - error
class$pred.up <- pred + error
class$pred <- pred

# Graph of Corrected Standard Errors
ggplot(data=class, mapping=aes(x=unlist(se),y=unlist(NStudents))) + geom_point() +
  geom_smooth() +
  ggtitle('Corrected Standard Errors by Number of Students') + 
  xlab('Corrected Standard Error') + 
  ylab('Number of Students') 



# Cross Validation --------------------------------------------------------




#2. Modify the cross-validation code from the birth weight analysis to run a cross-validation 
# of your heterogeneous MLR using the predictgls() function. Report the bias, RPMSE, coverage 
# and width of prediction intervals.

source('Predictgls.R', local=TRUE)  # For Dr. Heaton's predictgls function (I put
# the predictgls.R function in the same working directory as my .R
# file, so if you are not, you may need to change to local=F)

set.seed(2)
n.cv <- 500 #Number of CV studies to run
n.test <-  round(dim(class)[1]*0.3) #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Select test observations
  test.obs <- sample(x=1:dim(class)[1], size=n.test, replace=FALSE)
  
  ## Split into test and training sets
  test.set <- class[test.obs,]
  train.set <- class[-test.obs,]
  
  ## Fit a gls() using the training data

  train.gls <- gls(Final~Exam1 + Exam2 + Exam3 + HW + Quiz + as.factor(Semester) + NStudents, data=train.set, weights=varFixed(~invstudents), method="ML")
  
  ## Generate predictions for the test set
  my.preds <- predictgls(train.gls, newdframe=test.set)['Prediction']
  
  ## Calculate bias
  
  bias[cv] <- mean(as.numeric(unlist(my.preds-test.set['Final'])))
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Final']]-my.preds)^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage 
  se.pred <- predictgls(train.gls, newdframe=test.set)['SE.pred'] # Calculate se of predictions
  lower <- predictgls(train.gls, newdframe=test.set, level=0.95)$lwr
  upper <- predictgls(train.gls, newdframe=test.set, level=0.95)$upr

  cvg[cv] <- ((test.set[['Final']] > lower) & (test.set[['Final']] <upper)) %>% mean()
  
  ## Calculate Width
  wid[cv] <- (upper - lower) %>% unlist() %>% as.numeric() %>% mean()
  
}

# Average bias, rpmse, coverage, and width overall ALL Monte Carlo Simulations:
list(average.bias=mean(bias), average.rpmse=mean(rpmse), average.coverage=mean(cvg), average.width=mean(wid))


ggplot() + geom_histogram(aes(bias))
ggplot() + geom_histogram(aes(rpmse))
ggplot() + geom_histogram(aes(cvg))
ggplot() + geom_histogram(aes(wid))


# Plot of predictions

ggplot(data=class, aes(x = unlist(pred))) + geom_point(aes(y = unlist(Final))) +   
  geom_ribbon(data=class, aes(ymin = unlist(pred.low), ymax = unlist(pred.up)), fill = "blue", alpha = 0.2) + geom_abline(intercept=0, slope=1) +
  ggtitle('Fitted vs Actual Values of Final Exam') + 
  xlab('Predicted Average Final Exam Score (in %)') + 
 ylab('Final Average Final Exam Score (in %)') 


#Analysis
summary(class.gls)
intervals(class.gls, level=0.95)

# Pseudo R-squared
cor(class$Final, class$pred)^2













