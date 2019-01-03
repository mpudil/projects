# Mitchell Pudil
#Framingham Heart: Stepwise
setwd("~/Desktop/framingham/FRAMINGHAM_csv/")
# Response Variable: CVD=1 if Myocardial Infarction, Fatal Coronary Heart Disease, #Atherothrombotic Infarction, Cerebral Embolism,
# Intracerebral Hemorrhage, or Subarachnoid Hemorrhage
# or Fatal Cerebrovascular Disease
# =0 otherwise
#
#
#
#
# Explanatory Variables:
#  SEX =1 if Men, =2 if Women
#  TOTCHOL serum total cholesterol (mg/dL)
#  AGE (years)

# SYSBP systolic blood pressure (mmHg)
# DIABP diastolic blood pressure (mmHg)
# CURSMOKE = 1 current smoker, 0 if not current smoker
# CIGPDay number of cigarettes smoked each day
# BMI body mass index (kg/m^2)
# DIABETES =1 if diabetic (glucose>200), 0 if not diabetic
# BPMEDS =1 if currently using anti-hypertensive medication, =0 otherwise # HEARTRATE ventrical heart rate (beats/min)
# GLUCOSE casual serum glucose (mg/dL)
# educ=1if<HS,=2ifHS,=3ifsomecollege,=4ifcollege
#4 Grimshaw
#Stat 330: Regression Homework: Stepwise Regression
#The following code creates the dataset fram for 4,240 participants who began the Framingham study without cardiovascular disease:
  # Describe where the .csv file is located
  fram<-read.csv("frmgham2.csv",header=TRUE)
# subset to participants that didn’t have CHD
fram<-subset(fram,PERIOD==1 & PREVCHD==0)
# subset to risk factors under study
fram<-fram[,c(2:14,30)]

#Delete missing observations
fram2 <- na.omit(fram)

#Create interpretable versions of all the coded explanatory variables.
# Change all factor variables to R factor objects with a reasonable comparison level.
fram2$Sex <- as.factor(ifelse(fram2$SEX==1, "Men", "Women"))
fram2$Smoke <- as.factor(ifelse(fram2$CURSMOKE==1, "Smoker", "Non-smoker"))
fram2$Diabetes <- as.factor(ifelse(fram2$DIABETES==1, "diabetic", "not diabetic"))
fram2$BPmeds <- as.factor(ifelse(fram2$BPMEDS==1, "BP meds", "No BP meds"))
fram2$educ <- as.factor(fram2$educ)

#Save only the interpretable versions of columns in the dataframe.
fram3 <- fram2[-c(1, 6, 9, 10, 13)]

#Create training and test datasets.
train.rows <- sample(1:dim(fram3)[1], 3000, replace=FALSE)
fram.train <- fram3[c(train.rows),]
fram.test <- fram3[-c(train.rows),]

#EDA
boxplot(HEARTRTE~CVD, data=fram.train)
boxplot(AGE~CVD, data=fram.test)
prop.table(table(fram.train$Smoke, fram.train$CVD), margin=1)



# Find the best predicting logistic regression model
# best means subset of all possible explanatory variables

#forward 
min.model <- glm(CVD ~ +1, data=fram.train, family="binomial")
biggest <- formula(glm(CVD ~ ., data=fram.train, family="binomial"))
out1.CVD <- step(min.model, direction="forward", scope="biggest")

summary(glm(CVD ~ SYSBP + AGE + DIABP + Gender + BMI, data=fram.train, data=fram.train, family="binomial"))

# SYSBP, AGE, DIABP, Gender, BMI most important


#backward
out2.CVD <- step(glm(CVD ~ ., data=fram.train, family=binomial))
out2.CVD

# Stepwise
fram.out3 <- step(min.model, direction="both", scope=biggest)

summary(fram.out3)



library(ROCR)

#Training data
train.pred <- prediction(predict(fram.out3, type="response"), fram.train$CVD)
train.perf <- performance(train.pred, measure="tpr", x.measure="fpr")
plot(train.perf, xlab="1-specificity", ylab="sensitivity", main="ROC Curve")


#Test Data
test.pred <- prediction(predict(fram.out3, newdata=fram.test, type="response"), fram.test$CVD)
test.perf <- performance(test.pred, measure="tpr", x.measure="fpr")
plot(test.perf, add=TRUE, col="royalblue")

# AUC

performance(train.pred, measure="auc")
performance(test.pred, measure="auc")


# Research Task and Data Features that Match Analysis Strengths: Awesome predictions where we only need to use a few variables in our
# final model.

#Analysis Weaknesses: We don't look to hard at statistical significance
  















