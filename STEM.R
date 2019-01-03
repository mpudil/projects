# Mitchell Pudil
# STAT 330
# STEM majors

#### Prepare the Data ###
rm(list=ls())

library(plyr)
library(ggplot2)
#library(lme4)

##############################################################
################### Data Preparation ############################
##############################################################

# read in the data
#dat <- read.csv(file="CSPCC_Switcher_Data.csv")
temp <- tempfile()
download.file("http://www.stat.colostate.edu/~bailey/ewExternalFiles/switcher.zip",temp)
dat <- read.csv(unz(temp, "CSPCC_Switcher_Data.csv"))

#############################
### Create Switcher Code #####
#############################

sp.vars <- c("Q26","Q3Post","Q5Post","Q3FUS_Yes","Q3FUS_No")
sp.vars%in%colnames(dat)

Q3FUS <- rep(NA,nrow(dat))
Q3FUS[dat[,"Q3FUS_Yes"]=="Yes"] <- 1
Q3FUS[dat[,"Q3FUS_No"]=="No"] <- 2

sp <- cbind(dat[,sp.vars[1:3]],Q3FUS)
dim(sp)
apply(sp,2,table)


## Q26 - beginning of term - intend to take Calc II? 1 (yes), 2(no), 3(maybe)
## Q5Post - end of term - did you at the beginning intend to take Calc II? 1 (yes), 2(no), 3(maybe)
## Q3Post - end of term - do you intend to take Calc II? 1 (yes), 2(no), 3(maybe)
## Q3FUS_Yes - follow-up - have you taken or are enrolled in Calc II? "Yes" or NA
## Q3FUS_No - follow-up - have you taken or are enrolled in Calc II? "No" or NA

##code to look at number of response combinations for Q26 & Q3Post & Q5Post
#q123 <- as.data.frame(sp)
#tmp <- ddply(q123,.(Q26,Q5Post,Q3Post,Q3FUS),nrow)
#tmp[is.na(tmp[,4]),]


# switcher code (1 = persister, 2 = switcher, 3 = other)
SC <- rep(NA,length(sp))


### Rule 1 - questions Q26 & Q3FUS ###
# switchers
SC[sp$Q26%in%c(1,3) & sp$Q3FUS==2] <- 2

# persisters
SC[sp$Q26%in%c(1,3) & sp$Q3FUS==1] <- 1


### Rule 2 - questions Q5Post & Q3FUS (did not answer Q26)###
# switchers
SC[is.na(sp$Q26) & sp$Q5Post%in%c(1,3) & sp$Q3FUS==2 ] <- 2

# persisters
SC[is.na(sp$Q26) & sp$Q5Post%in%c(1,3) & sp$Q3FUS==1 ] <- 1


### Rule 3 - questions Q26Post & Q5Post & Q3Post (did not answer Q3FUS) ###
# switchers
SC[is.na(sp$Q3FUS) & sp$Q26%in%c(1,3) & sp$Q3Post==2 ] <- 2  
SC[is.na(sp$Q3FUS) & sp$Q26%in%c(1,3) & sp$Q5Post==1 & sp$Q3Post==3 ] <- 2

# persisters
SC[is.na(sp$Q3FUS) & is.na(SC) & sp$Q26%in%c(1,3) & sp$Q3Post%in%c(1,3) ] <- 1
# need is.na(SC) since switcher is M, Y, M and Y, Y, M


### Rule 4 - questions Q5Post & Q3Post (did not answer Q26 or Q3FUS) ###
# switchers
SC[is.na(sp$Q26) & is.na(sp$Q3FUS) & sp$Q5Post%in%c(1,3) & sp$Q3Post==2 ] <- 2
SC[is.na(sp$Q26) & is.na(sp$Q3FUS) & sp$Q5Post==1 & sp$Q3Post==3 ] <- 2

# persisters
SC[is.na(sp$Q26) & is.na(sp$Q3FUS) & sp$Q5Post%in%c(1,3) & sp$Q3Post==1 ] <- 1
SC[is.na(sp$Q26) & is.na(sp$Q3FUS) & sp$Q5Post==3 & sp$Q3Post==3 ] <- 1


### FINAL switcher code results #####
table(SC)


###############################
### Previous Calculus Variable #####
###############################

calc.vars <-c("Q15_CalculusNonAPFinalGrade","Q17_CalculusABFinalGrade","Q17_CalculusBCFinalGrade","Q18")
calc.vars%in%colnames(dat)

calc <- dat[,calc.vars]
colnames(calc) <- c("NonAP","APAB","APBC","College")

#ddply(as.data.frame(calc),.(NonAP,APAB,APBC,College),nrow)

prevCalc <- rep(NA,nrow(dat))

# Calc Non AP
prevCalc[calc$NonAP>0] <- 1

# Calc APAB
prevCalc[calc$APAB>0] <- 2

# Calc APBC
prevCalc[calc$APBC>0] <- 3

# Calc College
prevCalc[calc$College==1] <- 4

# No previous calculus
prevCalc[is.na(prevCalc) & calc$College==2] <- 5

table(prevCalc)

## combine levels 1, 2 and 3 (highschool calculus experience)
prevCalc[prevCalc%in%c(2,3)] <- 1

###############################
#### ACT/SAT Math Scores  #######
###############################

SATscore <- rev(seq(200,800,by=10))
SATper <- c(99,99,99,98,97,97,96,95,95,94,93,91,90,88,87,85,83,82,79,77,75,73,70,67,64,62,59,55,52,49,45,42,40,36,33,30,27,24,21,19,16,14,12,10,9,7,6,5,4,3,3,2,2,1,1,1,1,1,0,0,0)
#plot(SATscore,SATper)

ACTscore <- 36:1
ACTper <- c(99,99,99,98,97,96,95,93,91,88,84,79,73,67,61,56,52,47,42,36,27,15,6,2)
ACTper <- c(ACTper,rep(1,length(ACTscore)-length(ACTper)))
#plot(ACTscore,ACTper)

newSAT <- dat$Q3_SATMath
newSAT[newSAT > 800] = NA
newSAT[newSAT < 200] = NA
newSAT[newSAT%%10!=0] = NA
sum(is.na(newSAT))
newSAT = SATper[match(newSAT,SATscore)]


newACT <- dat$Q7_ACTMath
newACT[newACT > 36] = NA
newACT[newACT%%1!=0] = NA
newACT = ACTper[match(newACT,ACTscore)]

#plot(newSAT,newACT)

# average of SAT & ACT if both
nST <- rep(NA,length(newACT))  # new ST (standardized test) result
nST[is.na(newACT)] <- newSAT[is.na(newACT)] 
nST[is.na(newSAT)] <- newACT[is.na(newSAT)]
nST[!is.na(newACT)& !is.na(newSAT)] <- (newSAT[!is.na(newACT)& !is.na(newSAT)]  + newACT[!is.na(newACT)& !is.na(newSAT)] )/2



###############################
#######  Intended Major ###########
###############################

newMJ <- rep(NA,nrow(dat))

origMJ <- dat$Q60
newMJ[origMJ%in%c(1,2)] <- 3
newMJ[origMJ%in%c(3,4,5,7,8,9)] <- 1
newMJ[origMJ%in%c(6)] <- 2
newMJ[origMJ%in%c(10:15)] <- 4
newMJ[origMJ%in%c(16)] <- 5


##############################
##########  Teaching  ###########
##############################

cn <- colnames(dat)
good.teach <- as.matrix(dat[,which(substr(cn,1,8)=="Q18Post_")])
amb.teach <- as.matrix(dat[,which(substr(cn,1,8)=="Q19Post_")])
amb.teach <- amb.teach[,-ncol(amb.teach)] # mislabeled Q19Post_Prepare

good.teach <- good.teach[apply(is.na(good.teach),1,sum)==0,]
amb.teach <- amb.teach[apply(is.na(amb.teach),1,sum)==0,]

pc.good <-  princomp(good.teach)
pc.amb <-  princomp(amb.teach) 

cumsum(pc.good$sdev^2)/sum(pc.good$sdev^2)
cumsum(pc.amb$sdev^2)/sum(pc.amb$sdev^2)

round(pc.good$loadings[,1],3)
round(pc.amb$loadings[,1],3)

#loadings rescaled to sum to one
pc.g.s <- abs(pc.good$loadings[,1])/sum(abs(pc.good$loadings[,1]))
pc.a.s <- abs(pc.amb$loadings[,1])/sum(abs(pc.amb$loadings[,1]))

round(pc.g.s,3)
round(pc.a.s,3)

## good teaching
goodPC <- (-1)*good.teach%*%pc.g.s

# ambitious teaching
ambPC <- (-1)*amb.teach%*%pc.a.s

## new variables with all people
goodALL <- dat[,which(substr(cn,1,8)=="Q18Post_")]
goodALL[,"Q18Post_Discouraged"] <- 7-goodALL[,"Q18Post_Discouraged"]
goodAVGall <- apply(goodALL,1,weighted.mean,pc.g.s,na.rm=TRUE) 

ambALL <- dat[,setdiff(which(substr(cn,1,8)=="Q19Post_"),which(cn%in%c("Q19Post_Prepare")))]
ambALL[,"Q19Post_Lecture"] <- 7-ambALL[,"Q19Post_Lecture"]
ambAVGall <- apply(ambALL,1,weighted.mean,pc.a.s,na.rm=TRUE) 


#####################

gender <- dat[,"Q48"]
pre.conf <- dat[,"Q29_Confident"]
post.conf <- dat[,"Q6Post_Confident"]
why <- dat[,which(substr(cn,1,7)=="Q4Post_")]

table(SC,gender)

spID <- which(!is.na(SC))
reg.vars <- data.frame(as.factor(SC),as.factor(prevCalc),nST,as.factor(newMJ),goodAVGall,ambAVGall,as.factor(gender),pre.conf,post.conf,why)

reg.vars <- reg.vars[spID,]

reg.vars <- data.frame(reg.vars,dat[spID,"Institution"]) 
colnames(reg.vars) <- c("SC","prevCalc","nST","newMJ","goodTeach","ambTeach","gender","pre.conf","post.conf",colnames(why),"Institution")

table(apply(is.na(reg.vars),1,sum))
apply(is.na(reg.vars),2,sum)

# do not require students reported pre and post confidence to be in analysis
non.miss <- which(apply(is.na(reg.vars[,-which(colnames(reg.vars)%in%c("pre.conf","post.conf",colnames(why)))]),1,sum)==0)

###########################################################
##########  Final data set for logisitc mixed-effects model  ###########
###########################################################

obs.reg.vars <- reg.vars[non.miss,]




dim(obs.reg.vars) # number of complete observations

# career choice
table(obs.reg.vars$newMJ,obs.reg.vars$gender)
round(table(obs.reg.vars$newMJ,obs.reg.vars$gender,obs.reg.vars$SC)[,,2]/table(obs.reg.vars$newMJ,obs.reg.vars$gender),3)

# prev calc
table(obs.reg.vars$prevCalc,obs.reg.vars$gender)
round(table(obs.reg.vars$prevCalc,obs.reg.vars$gender,obs.reg.vars$SC)[,,2]/table(obs.reg.vars$prevCalc,obs.reg.vars$gender),3)

# std test
tmp <- floor(obs.reg.vars$nST/10)
table(tmp,obs.reg.vars$gender)
round(table(tmp,obs.reg.vars$gender,obs.reg.vars$SC)[,,2]/table(tmp,obs.reg.vars$gender),3)

# instructor quality
new.teach <- rep(6,length(obs.reg.vars$goodTeach))
new.teach[obs.reg.vars$goodTeach < 5.5] <- 5
new.teach[obs.reg.vars$goodTeach < 4.5] <- 4
new.teach[obs.reg.vars$goodTeach < 3.5] <- 3
new.teach[obs.reg.vars$goodTeach < 2.5] <- 2
new.teach[obs.reg.vars$goodTeach < 1.5] <- 1

table(new.teach,obs.reg.vars$gender)
round(table(new.teach,obs.reg.vars$gender,obs.reg.vars$SC)[,,2]/table(new.teach,obs.reg.vars$gender),3)

# student centered teaching
new.stu <- rep(6,length(obs.reg.vars$ambTeach))
new.stu[obs.reg.vars$ambTeach < 5.5] <- 5
new.stu[obs.reg.vars$ambTeach < 4.5] <- 4
new.stu[obs.reg.vars$ambTeach < 3.5] <- 3
new.stu[obs.reg.vars$ambTeach < 2.5] <- 2
new.stu[obs.reg.vars$ambTeach < 1.5] <- 1

table(new.stu,obs.reg.vars$gender)
round(table(new.stu,obs.reg.vars$gender,obs.reg.vars$SC)[,,2]/table(new.stu,obs.reg.vars$gender),3)


# create data frame STEM for analysis 
#  response variable y=SC (0 persistors, 1 switchers)
STEM<-data.frame(y=as.numeric(obs.reg.vars$SC)-1,prevCalc=as.numeric(as.character(obs.reg.vars$prevCalc)),
                 nST=obs.reg.vars$nST,newMJ=as.numeric(obs.reg.vars$newMJ),
                 new.teach,new.stu,gender=as.numeric(obs.reg.vars$gender))

dim(STEM)



####################################
##### Data Analysis#################
####################################


#EDA 
partSTEM <- STEM[,c("y", "nST", "new.teach", "new.stu")]

par(mfrow=c(2,2))
boxplot(nST ~ y, data=STEM, col="steel blue", xlab="Left Calculus", ylab="Percentile")
boxplot(new.teach ~ y, data=STEM, col="tomato", xlab="Left Calculus", ylab="Teacher Rating")
boxplot(new.stu ~ y, data=STEM, col="dark green", xlab="Left Calculus", ylab="student-centered practices")


# Contingency tables

table(STEM$y, STEM$prevCalc)
table(STEM$y, STEM$newMJ)
table(STEM$y, STEM$gender)



# Response variable: y = 1 if switchers, =0 if persistors
# Explanatory Variable: Primary interest: gender

# Declare categorical variables as R type factor and choose base groups for comparison. Note that R chooses 1 as default.
STEM$gender <- factor(STEM$gender)
STEM$gender <- relevel(STEM$gender,ref="1")
STEM$prevCalc <- factor(STEM$prevCalc)
STEM$prevCalc <- relevel(STEM$prevCalc, ref="1")
STEM$newMJ <- factor(STEM$newMJ)
STEM$newMJ <- relevel(STEM$newMJ, ref="1") 


# Model (write quantitative first)
# logit(y=1) = beta0 + beta1*nST + beta2*new.teach + beta3*new.stu + gender_i + prevCalc_j + newMJ_k
out.STEM <- glm(y~nST + new.teach + new.stu + gender + prevCalc + newMJ, data=STEM, family="binomial")
summary(out.STEM)

# To interpret the gender difference, compute transformed coefficient
exp(coef(out.STEM))[-1] 
# Interpretation: Women are 1.43 times more likely to switch

# Set other variables: best case scenario
# Newteach=6, newstu=6, prevCalc="1", newMJ="1". Add confidence bands. 


# Create graphic showing difference between women & men
# Gender = 1 = men
x.star <- data.frame(gender="1", nST=seq(2,99, length=100), 
                     new.teach=6, new.stu=6, prevCalc="1", newMJ="1")
plot(x.star$nST, predict(out.STEM, newdata=x.star, type="response"),
     type="l", ylim=c(0,0.25), ylab="P(Switch from Calc Seq)", xlab="Percentile of Standardized Test")


# Women 

x.star <- data.frame(gender="2", nST=seq(2,99, length=100), 
                     new.teach=6, new.stu=6, prevCalc="1", newMJ="1")

lines(x.star$nST, predict(out.STEM, newdata=x.star, type="response"), col="red")

# Differene btwn men and women, holding all else constant
# 95% CI
exp(confint(out.STEM)[-1])

# z=test
summary(out.STEM)
# So there is a statistically significant difference between men and women

# X^2 test
red1.STEM <- glm(y~nST + new.teach + new.stu + prevCalc + newMJ, data=STEM, family="binomial")
anova(red1.STEM, out.STEM, test="Chisq")  # (p=0.0032)

# Is there an effect due to calculus preparation?
# NOTE: If we ever have more than two levels, use chi squared test to see if there is a significant factor

red2.STEM <- glm(y~nST + new.teach + new.stu + gender + newMJ, data=STEM, family="binomial")
anova(red2.STEM, out.STEM, test="Chisq")
# There is no effect due to calculus preparation


# If we focus on using the model to identify students at risk of switching
# ROC Curve
library(ROCR)
STEM.pred <- prediction(predict(out.STEM, type="response"), STEM$y)
STEM.perf <- performance(STEM.pred, measure="tpr", x.measure="fpr")
plot(STEM.perf, xlab="1-specificity", ylab="specificity", main="ROC Curve")
abline(0,1,col="gray")


# AUC
performance(STEM.pred, measure="auc")

# Strengths: Doing it this way rather than simply machine learning will allow us to more fully understand 
# what makes people more likely to fail. This helps if we are trying to explain to people
# why we are wanting them to see a counselor rather than just saying it's a secret or not 
# quite knowing why (e.g. Random Forest just tells us the more significant variables but not
# how relevant they actually are)

# Weaknesses: There is a tradeoff between specificity and specificity. If we want to decrease
# the probability of false positives, then we get an increase in false negatives, and vice versa.

# Challenge: Probability of a child being homeschooled. Data available on the National
# Home and Education Survey (NHES) website.








