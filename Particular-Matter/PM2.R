# Mitchell Pudi
# Particulate Matter 2


# EDA ---------------------------------------------------------------------


# Import Data and Libraries -----------------------------------------------
library(ggplot2)
library(nlme)
install.packages('mvtnorm')
library(mvtnorm)
library(car)
install.packages('multcomp')
library(multcomp)
library(astsa)
library(tidyverse)
library(magrittr)
source("~/Desktop/Winter_2019/STAT_469/stdres.gls.R")

pm <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/2%20-%20TemporalCorrelation/3%20-%20Project/Data/BreathingZonePM.txt', sep=' ')
pm$ID <- as.factor(pm$ID)

# EDA ---------------------------------------------------------------------

ggplot(data=pm, mapping=aes(x=Activity, y=Aerosol, color=Activity)) + geom_boxplot() +
  labs(title ="Aerosol Level by Activity", x = "Activity Type", y = "Aerosol Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels=c("OnPhone" = "On Phone", "PlayingOnFloor"="Playing On Floor", "PlayingOnFurniture"=
                              "Playing On Furniture", "VideoGames"="Video Games", "WatchingTV"="Watching TV"))

ggplot(data=pm, mapping=aes(x=Stationary, y=Aerosol)) + geom_point(colour='steel blue') +
  ggtitle('Stationary and Aerosal PM Levels') +
  geom_abline(intercept=0, slope = 0.1, color='red', linetype='dashed')

# Aggregate Time Spent on Each Activity
agg.id.activity <- data.frame(matrix(ncol=3, nrow=60*8, data=0))
pm$minutes <- 1
for(i in 1:length(levels(pm$Activity))){
  data <- subset(pm, Activity==levels(pm$Activity)[i])
  agg <- aggregate(x=data$minutes, by=list(data$ID), FUN=sum)
  colnames(agg) <- c('ID', 'TimeOnActivity')
  start <- 60*i-59
  end <- 60*i
  activity.i <- levels(pm$Activity)[i]
  agg.id.activity[c(start:end),1] <- as.data.frame(agg$ID)
  agg.id.activity[c(start:end),2] <- as.data.frame(agg$TimeOnActivity)
  agg.id.activity[c(start:end),3] <- activity.i
  colnames(agg.id.activity) <- c('ID', 'TimeOnActivity', 'Activity')
}


ggplot(data=agg.id.activity, mapping=aes(x=Activity, y=TimeOnActivity, color=Activity)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels=c("OnPhone" = "On Phone", "PlayingOnFloor"="Playing On Floor", "PlayingOnFurniture"=
                              "Playing On Furniture", "VideoGames"="Video Games", "WatchingTV"="Watching TV")) +
  labs(title ="Time Spent on Each Activity", x = "Activity", y = "Time Spent (Minutes)") +
  theme(legend.position = "none")



# Determining Best Model --------------------------------------------------
pm.lm <- lm(data=pm, formula=Aerosol ~ ID*Stationary + ID*Activity)

initmod <- lm(Aerosol ~., data = pm)
boxCox(initmod) # We will need to take the log of aerosol

pm$logaerosol <- log(pm$Aerosol)
logmod <- lm(data=pm, formula=logaerosol ~ ID*Stationary + ID*Activity) # Linear log model

# EDA ---------------------------------------------------------------------

ggplot(pm) + geom_histogram(aes(Aerosol)) + ggtitle('Histogram of PM Measurement on Child’s Vest')
ggplot(pm) + geom_histogram(aes(logaerosol), binwidth = 0.2) + ggtitle('Histogram of Log of PM Measurement on Child’s Vest') +
  xlab('Log of Aerosol')

ggplot(data=pm, mapping=aes(x=Activity, y=Aerosol, color=Activity)) + geom_boxplot() +
  labs(title ="Aerosol Level by Activity", x = "Activity Type", y = "Aerosol Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels=c("OnPhone" = "On Phone", "PlayingOnFloor"="Playing On Floor", "PlayingOnFurniture"=
                              "Playing On Furniture", "VideoGames"="Video Games", "WatchingTV"="Watching TV"))

ggplot(data=pm, mapping=aes(x=Stationary, y=logaerosol)) + geom_point(colour='steel blue') +
  labs(title='Stationary and Aerosal PM Levels', y='Log Aerosol')


# Aggregate Time Spent on Each Activity
agg.id.activity <- data.frame(matrix(ncol=3, nrow=60*8, data=0))
pm$minutes <- 1
for(i in 1:length(levels(pm$Activity))){
  data <- subset(pm, Activity==levels(pm$Activity)[i])
  agg <- aggregate(x=data$minutes, by=list(data$ID), FUN=sum)
  colnames(agg) <- c('ID', 'TimeOnActivity')
  start <- 60*i-59
  end <- 60*i
  activity.i <- levels(pm$Activity)[i]
  agg.id.activity[c(start:end),1] <- as.data.frame(agg$ID)
  agg.id.activity[c(start:end),2] <- as.data.frame(agg$TimeOnActivity)
  agg.id.activity[c(start:end),3] <- activity.i
  colnames(agg.id.activity) <- c('ID', 'TimeOnActivity', 'Activity')
}


ggplot(data=agg.id.activity, mapping=aes(x=Activity, y=TimeOnActivity, color=as.factor(Activity))) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels=c("OnPhone" = "On Phone", "PlayingOnFloor"="Playing On Floor", "PlayingOnFurniture"=
                              "Playing On Furniture", "VideoGames"="Video Games", "WatchingTV"="Watching TV")) +
  labs(title ="Time Spent on Each Activity", x = "Activity", y = "Time Spent (Minutes)") 


ggplot(pm,aes(x=Stationary,y=logaerosol)) + geom_point() + 
  geom_smooth(method = "lm") +
  ggtitle("Point Plot of Stationary Reading vs Vest Reading") + 
  ylab('Log of Aerosol') 
  
  # Aerosol by Activity (not logged)
ggplot(pm,aes(x=Activity,y=Aerosol, fill=Activity)) + geom_boxplot(alpha=0.3, outlier.alpha = 1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE) +
  ggtitle("Spread of Aerosol Reading for Each Activity") + 
  ylab('Aerosol Level')


pm$ID <- as.factor(pm$ID)
lvls <- levels(pm$ID)
ggplot(pm,aes(x=as.factor(ID),y=Aerosol, fill = ID)) + geom_boxplot(alpha=0.3, outlier.alpha = 1, outlier.size = 0.5, outlier.fill=) + 
  scale_x_discrete(breaks=lvls[c(1,seq(5,length(lvls),by=5))]) +
  ggtitle("PM Levels by Subject") + 
  xlab("Subject ID") + 
  theme(legend.position = "none") + 
  ylab("Recorded PM Level")


# LINE Assumptions --------------------------------------------------------

# Linearity

avPlot(logmod, variable = "Stationary", main = "Added Variable Plot of Stationary Reading vs 
       Vest Reading", pch = 16)

# Independence 
pm.resid <- resid(logmod)
corr.matrix <- cor(matrix(nrow=60, ncol=118, byrow=T, data=pm.resid))

rmat <- matrix(logmod$residuals, ncol = length(unique(pm$Minute)), byrow = TRUE)
cors <- cor(rmat)
corvec <- matrix(cors, ncol = 1, nrow = 118*118, byrow = FALSE)
ggplot() + geom_histogram(aes(x=corvec), bins = 100) + ggtitle("Residual Correlation Between Subject's Aerosol Reading Per Minute (LM)") + xlab("Residual Correlation Values") + ylab("Frequency")

corvec <- as.data.frame(corvec)
corvec.other <- subset(corvec, V1!=1)
mean(corvec.other$V1)
sd(corvec.other$V1)


ggplot(data=corvec, mapping=aes(V1)) + geom_histogram(binwidth = 0.025) +
  ggtitle('Histogram of Correlation Between Time Periods') +
  xlab('Correlation of Time Points')

  # Log model ACF
my.ACF <- acf(stdres(logmod), lag.max=118)
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col() + 
  ggtitle('ACF of PM Residual Terms') 

  # There is a correlation within a patients from period to period (visit to visit)
    # Determine best model for error terms

pm_ar <- gls(logaerosol ~ Stationary*ID + Activity*ID, data = pm, correlation = corARMA(form = ~Minute|ID, p = 1, q = 0),method = "ML")
#pm_ma <- gls(logaerosol ~ Stationary*ID + Activity*ID, data = pm, correlation = corARMA(form = ~Minute|ID, p = 0, q = 1),method = "ML")
#pm_arma <- gls(logaerosol ~ Stationary*ID + Activity*ID, data = pm, correlation = corARMA(form = ~Minute|ID, p = 1, q = 1),method = "ML")
#list(AIC(pm_ar), AIC(pm_ma), AIC(pm_arma))

    
    # Note: General symmetric will not converge here because we would be allowing each within-subject observation
    # to have a unique relationship with each other, so there would be 118 choose 2 (over 6900) combinations

decor_resids <- stdres.gls(pm_ar)
decor.resids.mat <- matrix(decor_resids, ncol = 118, byrow = TRUE)
dec.cors <- cor(decor.resids.mat)
dec.cors2 <- as.data.frame(matrix(ncol=1, data=dec.cors))

dec.cors.other <- subset(dec.cors2, V1!=1)
mean(dec.cors.other$V1)
sd(dec.cors.other$V1)

dec.res.good <- subset(dec.cors.other, abs(V1) < 0.2)
dim(dec.res.good)[1] / dim(dec.cors.other)[1]

  # Hist of correlation between time periods (accounting for AR1 correlation)
ggplot(data=dec.cors2, mapping=aes(V1)) + geom_histogram(binwidth = 0.025) +
  ggtitle('Histogram of Correlation Between Time Periods') +
  xlab('Correlation of Time Points (Using AR1 Decorrelated Residuals)')

  # ACF of decorrelated residuals
my.ACF <- acf(stdres.gls(pm_ar), lag.max=118)
ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col() + 
  ggtitle('ACF of PM Decorrelated Residual Terms')




  # The error terms look a lot more decorrelated after using an AR(1) model


# Normality

ggplot() + geom_histogram(aes(x=decor_resids), bins=50) +
  ggtitle('Histogram of Decorrelated Residuals') +
  xlab('Decorrelated Residual Value')

    # KS test

ks.test(stdres.gls(pm_ar), "pnorm") # P-value is 0.5827, so we have normality

# Equal Variance

ggplot() + geom_point(aes(x=pm_ar$fitted, y=decor_resids)) +
  geom_hline(yintercept = 0, colour = "red") +
  labs(title='Scatterplot of Fitted Values vs Decorrelated Residuals', x='Fitted Values',
                y='Decorrelated Residuals')

  # BP Test

u <- pm_ar$residuals
pm$usq.AR <- u^2
bp.lm <- lm(data=pm, formula=usq.AR ~ Stationary + Activity + ID*Stationary)
summary(bp.lm) # There is NO heteroskedasticity (p-value: 0.1863) 



# Analysis Results --------------------------------------------------------

# 1. Using results from your model, do you think that the stationary measurement alone 
# does a good job explaining PM exposure?

# ANOVA: Reg 1: with activities; Reg 2: Without activities. p < 0.05 suggest activities important

pm.ar.stationary <- gls(model=logaerosol~Stationary, data=pm, correlation=corARMA(form=~Minute|ID, p=1, q=0), method="ML")

anova(pm_ar, pm.ar.stationary)  # So stationary alone doesn't do as well as with activity p-value <0.0001
AIC(pm.ar.stationary) # AIC up by almost 3000 (7025.242)


#2. Do activities, in addition to the stationary measurement, explain PM exposure? 

no.activities.model <- gls(model=logaerosol~ID*Stationary, data=pm, correlation=corARMA(form=~Minute|ID, p=1, q=0), method="ML")
anova(no.activities.model, pm_ar)

# If so, what activities (on average) lead to higher PM exposure?
summary(pm_ar)

# So the only activity that lead to a lower PM exposure (baseline computer) is playing 
# on the furniture. However, activities as a whole do help explain PM.


# 3. Are the effects of activities/stationary child-specific? If so, discuss how much 
# variability there is in the effects from child to child?

# Method: ANOVA with Reg 1: normal gls, Reg 2: includes ID*Activity & ID*Stationary

pm.ar.noid <- gls(model=logaerosol~Stationary + Activity + ID, data=pm, correlation=corAR1(form = ~Minute|ID),method = "ML")
anova(pm.gls.pq, pm.ar.noid) # p-value <0.0001

#For boxplots of effects across children
mod <- pm_ar
ts <- summary(mod)$tTable
homeworkeffect <- subset(ts, grepl("*:ActivityHomework*",rownames(ts)))[,"Value"]
phoneeffect <- subset(ts, grepl("*:ActivityOnPhone*",rownames(ts)))[,"Value"]
furneffect <- subset(ts, grepl("*:ActivityPlayingOnFurniture*",rownames(ts)))[,"Value"]
flooreffect <- subset(ts, grepl("*:ActivityPlayingOnFloor*",rownames(ts)))[,"Value"]
walkeffect <- subset(ts, grepl("*:ActivityWalking*",rownames(ts)))[,"Value"]
vgeffect <- subset(ts, grepl("*:ActivityVid*",rownames(ts)))[,"Value"]
watchtveffect <- subset(ts, grepl("*:ActivityWatch",rownames(ts)))[,"Value"]
stationeffect <- ts[69:127,1]

hwdf <- data.frame("Homework",homeworkeffect)
phdf <- data.frame("OnPhone",phoneeffect)
fndf <- data.frame("PlayingOnFurniture",furneffect)
fldf <- data.frame("PlayingOnFloor",flooreffect)
wkdf <- data.frame("Walking",walkeffect)
vgdf <- data.frame("VideoGames",vgeffect)
wtdf <- data.frame("WatchingTV",watchtveffect)
stdf <- data.frame("Stationary",stationeffect)

names(hwdf) <- c("Activity","Effects")
names(phdf) <- c("Activity","Effects")
names(fndf) <- c("Activity","Effects")
names(fldf) <- c("Activity","Effects")
names(wkdf) <- c("Activity","Effects")
names(vgdf) <- c("Activity","Effects")
names(wtdf) <- c("Activity","Effects")
names(stdf) <- c("Activity","Effects")

dat <- rbind(hwdf,phdf,fldf,fndf,vgdf,wkdf,wtdf, stdf)

rownames(dat) <- NULL
boxplot(Effects ~ Activity, data = dat)
#Boxplot
ggplot(dat,aes(x=Activity,y=Effects, fill=Activity)) + 
  geom_boxplot(alpha = 0.2, outlier.alpha = 1) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 45,hjust = 1)) + 
  ggtitle("Spread of Effect per Child per Activity")

onlyactinter <- gls(logaerosol ~ Stationary + Activity*ID, data = pm, 
                    correlation = corAR1(form = ~Minute|ID),method = "ML")
anova(mod, onlyactinter)

#Calculate Variance
dat %>% group_by(Activity) %>% summarize("Var" = var(Effects))