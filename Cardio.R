# Mitchell Pudil
# Cardiovascular Health Homework


# Import libraries and data -----------------------------------------------
library(nlme)
#install.packages('mvtnorm')
library(mvtnorm)
library(car)
#install.packages('multcomp')
library(multcomp)
#nstall.packages('kernlab')
library(kernlab)
library(ggplot2)
health <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/2%20-%20Longitudinal/HWCaseStudy/Data/Tachycardia.txt',sep=' ')

# Change class of health columns as appropriate
health$SEX <- as.numeric(health$SEX)
for(i in 1:nrow(health)){
  health$SEX[i] <- ifelse(health$SEX[i]==1, 'Male', 'Female')
}

health$CURSMOKE <- as.factor(health$CURSMOKE)
health$BPMEDS <- as.factor(health$BPMEDS)
health$SEX <- as.factor(health$SEX)
health$RANDID <- as.factor(health$RANDID)
health$DIABETES <- as.factor(health$DIABETES)


# Exploratory Plots -------------------------------------------------------

ggplot(data=health, mapping=aes(x=AGE, y=HEARTRTE, color=CURSMOKE)) + geom_point() + 
  geom_smooth(method=lm) + labs(title ="Scatterplot of Heart Rate by Age, by Smoking Status", 
                                x = "Age", y = "Heart Rate") +
  scale_color_manual(name='Smoker', labels = c("No", "Yes"), values = c("green", "blue"))


ggplot(data=health, mapping=aes(x=TOTCHOL, y=HEARTRTE, color=SEX)) + geom_point() + 
  geom_smooth(method=lm) + labs(title ="Scatterplot of Heart Rate by Cholesterol Level, by Sex", 
                                 x = "Cholesterol Level", y = "Heart Rate") +
  scale_color_manual(name='Sex', labels = c("Male", "Female"), values = c("blue", "hotpink3"))


ggplot(data=health, mapping=aes(x=DIABETES, y=HEARTRTE)) + geom_boxplot(color=c('green', 'red')) + 
  labs(title ="Boxplot of Heart Rate by Diabetes Status", x = "Diabetic", y = "Heart Rate") 


ggplot(data=health, mapping=aes(x=AGE, y=HEARTRTE, color=SEX)) + geom_point() + 
  geom_smooth(method=lm) + labs(title ="Scatterplot of Heart Rate by Age, by Smoking Status", 
                                x = "Age", y = "Heart Rate") +
  scale_color_manual(name='Smoker', labels = c("No", "Yes"), values = c("steel blue", "tomato"))


ggplot(data=health, mapping=aes(x=BMI, y=HEARTRTE, color=SEX)) + geom_point() + 
  geom_smooth() + labs(title ="Scatterplot of Heart Rate by Age, by Smoking Status", 
                                x = "Age", y = "Heart Rate") +
  scale_color_manual(name='Smoker', labels = c("No", "Yes"), values = c("steel blue", "tomato"))


ggplot(data=health, mapping=aes(x=SEX, y=HEARTRTE)) + geom_boxplot(color=c('hotpink3', 'blue')) + 
  labs(title ="Boxplot of Heart Rate by Sex", x = "Sex", y = "Heart Rate") 



ggplot(data=health, mapping=aes(x=GLUCOSE, y=HEARTRTE, color=SEX)) + geom_point() + 
  geom_smooth() + labs(title ="Scatterplot of Heart Rate and Glucose, by Smoking Status", 
                       x = "Glucose", y = "Heart Rate") +
  scale_color_manual(name='Sex', labels = c("Male", "Female"), values = c("blue", "hotpink3"))




# Data Analysis -----------------------------------------------------------

# 2. Fit an independent MLR model with a linear effect of all variables except RANDID and 
  # PERIOD. Explore the residuals to see if there is evidence of correlation within a patients
  # from period to period (visit to visit).

health.lm <- lm(data=health, HEARTRTE ~ . -RANDID -PERIOD)
summary(health.lm)
health.resid <- resid(health.lm)
corr.matrix <- cor(matrix(nrow=50, ncol=3, byrow=T, data=health.resid))
corr.matrix # There is a correlation within a patients from period to period (visit to visit)


# 3. To determine an appropriate correlation structure to use, fit a longitudinal MLR model 
  # with an AR1, MA1 and general symmetric correlation matrix within each patient but 
  # independent across patients. Compare the model fits using AIC (which can be extracted 
  # from a gls() object using AIC()).


# AR1
health.gls.AR <- gls(model=HEARTRTE~. -RANDID -PERIOD, data=health, correlation=corAR1(form=~1|RANDID), method="ML")
AIC(health.gls.AR)

# MA1

health.gls.MA <- gls(model=HEARTRTE~. -RANDID -PERIOD, data=health, correlation=corARMA(form=~1|RANDID, p=0, q=1), method="ML")
AIC(health.gls.MA)

# General Symmetric
health.gls.GS <- gls(model=HEARTRTE~. -RANDID -PERIOD, data=health, 
                correlation=corSymm(form=~1|RANDID), method="ML")

AIC(health.gls.GS) # This one has the minimum AIC score (as expected based off the corr matrix)


# 5. Fit your longitudinal model and validate any assumptions you made to fit the model

summary(health.gls.GS) # Longitudinal model of choice 

# Linearity
avPlots(health.lm, terms=~. -CURSMOKE -BPMEDS -SEX -RANDID -DIABETES)

# Independence
source('stdres.gls.R', local=TRUE)
decorrelated.residuals <- stdres.gls(health.gls.GS)
corr.matrix.decorrelated <- cor(matrix(nrow=50, ncol=3, byrow=T, data=decorrelated.residuals))
corr.matrix.decorrelated # Within-subject residuals are not correlated


# Normality
ggplot() + geom_histogram(data=health, mapping=aes(x=decorrelated.residuals)) +
  ggtitle('Histogram of Decorrelated Residuals') +
  xlab('Decorrelated Residuals')
# Decorrelated residuals appear normally distributed

# Equal Variance
health$fitted.values <- health.gls.GS$fitted
health$decorrelated.residuals <- decorrelated.residuals

ggplot(data=health, mapping=aes(x=fitted.values, y=decorrelated.residuals)) + geom_point() +
  geom_hline(yintercept = 0, colour='red') +
  ggtitle('Fitted Values vs Residuals') +
  xlab('Fitted Values') + 
  ylab('Decorrelated Residuals')

# 6. Is DIABETES a risk factor for Tachycardia? Justify your answer and explain any effect 
  # of DIABETES on heart rate (include uncertainty in your conclusions)
summary(health.gls.GS)
beta.diabetes <- health.gls.GS$coefficients[9]
se.diabetes <- 0.9411147
t <- qt(0.975, df=nrow(health)-ncol(health)-3) # -3 is for not including heartrte, randid, and 
    # period

conf.interval.diabetes <- beta.diabetes + c(-1,1)*t*se.diabetes
# Confidence interval: (-1.045, 2.645)


# 7. What is the expected difference in heart rate for a female patient with at age 35 who 
  # is a smoker vs. an older female of 45 but not a smoker (assume the other characteristics 
  # are the same)? What does this say about the effect of smoking?

# Female 35 yrs
a1 <- matrix(ncol=1, data=c(0,0,0,35,0,0,1,0,0,0,0))
a2 <- matrix(ncol=1, data=c(0,0,0,45,0,0,0,0,0,0,0))

test1 <-  glht(health.gls.GS, linfct=t(a1-a2), alternative="greater") # H0: both have same heart rate
                                                                      # HA: different heartrate
summary(test1)




