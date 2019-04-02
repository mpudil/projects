# Mitchell Pudil
# Land Coverage
library(ggplot2)
#install.packages('ggmap')
library(ggmap)
#install.packages("geoR")
library(geoR)
#install.packages('spatial')
library(spatial)
library(nlme)
#install.packages('mvtnorm')
library(mvtnorm)
library(car)
#install.packages('multcomp')
library(multcomp)
#nstall.packages('kernlab')
library(kernlab)
library(tidyverse)
source('~/Desktop/Winter_2019/STAT_469/stdres.gls.R')
source('~/Desktop/Winter_2019/STAT_469/predictgls.R')
install.packages("geoR", dependencies=TRUE)


#library(geoR)
temp <- read.csv('https://mheaton.byu.edu/Courses/Stat469/Topics/3%20-%20SpatialCorrelation/1%20-%20PointReference/InClassCaseStudy/Data/SurfaceTemps.txt', sep=' ')

# EDA

# Boxplots of temperature by surface type
ggplot(data=temp, mapping=aes(x=Surface, y=Temp, color=Surface)) + geom_boxplot() +
  ggtitle('Temperature by Surface Type')

# Mapping temperature
my.api.key <- 'AIzaSyCh4VI0laihyQDN_pFDL_dkEQfEcWll9Cg'
register_google(key=my.api.key)
bb <- make_bbox(lon=Lon, lat=Lat, data=temp)
mymap <- get_map(location=bb, zoom=11, maptype="satellite")
ggmap(mymap)+geom_raster(data=temp, aes(x=Lon, y=Lat, fill=Temp),  alpha=0.8) + scale_fill_distiller(palette="Spectral", na.value=NA) + 
  coord_cartesian() + scale_fill_distiller(palette="Spectral", na.value=NA) + coord_cartesian()

# Plot residuals
temp2 <- temp[complete.cases(temp), ]
temp2.lm <- lm(data=temp2, formula=Temp~Surface)  # Should I also include lat/lon?
temp2$resids <- temp2.lm$residuals


bb.resid <- make_bbox(lon=Lon, lat=Lat, data=temp2)
mymap.resid <- get_map(location=bb.resid, zoom=11, maptype="satellite")
ggmap(mymap.resid)+geom_raster(data=temp2, aes(x=Lon, y=Lat, fill=resids),  alpha=0.8) + 
  scale_fill_distiller(palette="Spectral", na.value=NA) + 
  coord_cartesian() + scale_fill_distiller(palette="Spectral", na.value=NA) + coord_cartesian()

#Variogram (not working since geoR package can't be installed)

#variog(coords=matrix(temp2[,c(1,2)]), data=temp2$resids)


# Spatial MLR Model Fitting

# Choose (with AIC) among an exponential, spherical and Gaussian correlation structure 
# (with nuggets) by fitting each model to Temperature using land cover as a factor covariate. 
# For the best fit model, identify the constrained estimates of the correlation structure along 
# with any βˆ coefficients and the estimate of the variance parameter σ̂2.

gls.exp <- gls(model=Temp~Surface, data=temp2, correlation=corExp(form=~Lon+Lat, nugget=TRUE), method="ML")
gls.spher <- gls(model=Temp~Surface, data=temp2, correlation=corSpher(form=~Lon+Lat, nugget=TRUE), method="ML")
gls.gaus <- gls(model=Temp~Surface, data=temp2, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")

aics <- list(AIC(gls.exp), AIC(gls.spher), AIC(gls.gaus))
which.min(aics)

myGLS <- gls.gaus
coef(myGLS$modelStruct$corStruct, unconstrained=FALSE) # unconstrained parameters
myGLS$coefficients # beta estimates
myGLS$sigma^2 # Variance parameter



# Model Validation --------------------------------------------------------


  # Linearity ------

avPlots(temp2.lm, terms=~.)

  # Independence ----

decorrelated.residuals <- stdres.gls(myGLS)
#variog(coords=matrix(temp2[,c(1,2)]), data=decorrelated.residuals)


  # Normality -------

ggplot(mapping=aes(decorrelated.residuals)) + geom_histogram(bins = 50)


  # Equal Variance -----

fitted.values <- myGLS$fitted
ggplot(mapping=aes(y=decorrelated.residuals, x=fitted.values)) + geom_point()


  # Clock how long it takes to fit GLS -----

system.time({
 myGLS <- gls(model=Temp~Surface, data=temp2, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")
})


  # Assess predictive accuracy (Cross Validation for GLS) -----
pb <- txtProgressBar(min = 0, max = n.cv, style = 3)
n.cv <- 50 #Number of CV studies to run
n.test <- round(nrow(temp2)[1]*0.2, 0) #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv)
bias <- rep(x=NA, times=n.cv)
wid <- rep(x=NA, times=n.cv)
cvg <- rep(x=NA, times=n.cv)
for(cv in 1:n.cv){
  ## Select test observations
  n <- nrow(temp2)
  test.obs <- sample(x=1:n, size=n.test)
  
  ## Split into test and training sets
  test.set <- temp2[test.obs,]
  train.set <- temp2[-test.obs,]
  
  ## Fit a gls() using the training data
  train.gls <- gls(model=Temp~Surface, data=train.set, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")
  
  ## Generate predictions for the test set
  my.preds <- predictgls(train.gls, newdframe=test.set)
  
  ## Calculate bias
  bias[cv] <- mean(test.set[['Temp']]-my.preds$Prediction)
  
  ## Calculate RPMSE
  rpmse[cv] <- (test.set[['Temp']]-my.preds$Prediction)^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg[cv] <- ifelse((test.set[['Temp']] > my.preds$lwr) & (test.set[['Temp']] < my.preds$upr),1,0) %>% mean()
  
  ## Calculate Width
  wid[cv] <- (my.preds$upr - my.preds$lwr) %>% mean()
  ## Update the progress bar
  setTxtProgressBar(pb, cv)
}
close(pb)


ggplot(mapping=aes(bias)) + geom_histogram()
ggplot(mapping=aes(rpmse)) + geom_histogram()
ggplot(mapping=aes(cvg)) + geom_histogram()
ggplot(mapping=aes(wid)) + geom_histogram()


  # Assess predictive accuracy (Cross Validation for LM) -----

n.cv <- 50 #Number of CV studies to run
n.test <- round(nrow(temp2)*0.2, 0) #Number of observations in a test set
n <- nrow(temp2)

rpmse.lm <- rep(x=NA, times=n.cv)
bias.lm <- rep(x=NA, times=n.cv)
wid.lm <- rep(x=NA, times=n.cv)
cvg.lm <- rep(x=NA, times=n.cv)

pb <- txtProgressBar(min = 0, max = n.cv, style = 3)

for(cv in 1:n.cv){
  ## Select test observations
  test.obs <- sample(x=1:n, size=n.test)
  
  ## Split into test and training sets
  test.set <- temp2[test.obs,]
  train.set <- temp2[-test.obs,]
  
  ## Fit a lm() using the training data
  train.lm <- lm(formula=Temp~Surface, data=train.set)
  
  ## Generate predictions for the test set
  my.preds <- predict.lm(train.lm, newdata=test.set, interval="prediction")
  
  ## Calculate bias
  bias.lm[cv] <- mean(test.set[['Temp']]-my.preds[,'fit'])
  
  ## Calculate RPMSE
  rpmse.lm[cv] <- (test.set[['Temp']]-my.preds[,'fit'])^2 %>% mean() %>% sqrt()
  
  ## Calculate Coverage
  cvg.lm[cv] <- ifelse((test.set[['Temp']] > my.preds[,'lwr']) & (test.set[['Temp']] < my.preds[,'upr']),1,0) %>% mean()
  
  ## Calculate Width
  wid.lm[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
  ## Update the progress bar
  setTxtProgressBar(pb, cv)
}

close(pb)

ggplot(mapping=aes(bias.lm)) + geom_histogram()
ggplot(mapping=aes(rpmse.lm)) + geom_histogram()
ggplot(mapping=aes(cvg.lm)) + geom_histogram()
ggplot(mapping=aes(wid.lm)) + geom_histogram()


  # Compare lm vs gls
bias.diff <- bias.lm - bias
rpmse.diff <- rpmse.lm - rpmse
cvg.diff <- cvg.lm - cvg
wid.diff <- wid.lm - wid

list(mean(bias.diff), mean(rpmse.diff), mean(cvg.diff), mean(wid.diff))
list(sd(bias.lm), sd(bias))
list(sd(cvg.lm), sd(cvg))
  # lm is more apt to have more bias and lower coverage (although on average, gls 
    # and lm are the same here)
  # lm also has a greater average rpmse and greater average width
  # Overall, lm is not as good as gls (as expected)



# Statistical Inference ---------------------------------------------------


  # 1. Use an F-test to see if temperatures are difference across any of the land-cover types.
gls.nodiff <- gls(model=Temp~1, data=temp2, correlation=corGaus(form=~Lon+Lat, nugget=TRUE), method="ML")
anova(gls.nodiff, myGLS) # So temperature varies by surface

  # 2. Create confidence intervals for each effect of land cover and determine which land cover 
  # types result in increased temperatures.

confint(myGLS) # Urban areas result in increased temperature

  #3. Perform a GLHT to construct a confidence interval of the difference temperature between 
  # Savannah and Urban land covers.

a.savannah <- matrix(nrow=5,ncol=1, data=c(1, 0, 0, 1, 0))
a.urban <- matrix(nrow=5,ncol=1, data=c(1, 0, 0, 0, 1))
a <- a.savannah - a.urban
a.transpose <- t(a)

mygls <- glht(myGLS, linfct=a.transpose, alternative="two.sided")
summary(mygls) # p-value < 0.0001

confint(mygls,level=0.95) # 95% CI
  # Conf. interval of difference (savannah - urban) is (-0.5038, -0.1383), suggesting that 
  # urban temperatures are greater than savannah


  #4.  Create and map predictions of the temperature at each location that was impeded by cloud 
      # cover.
missing.rows <- which(is.na(temp$Temp))
missing.data <- temp[c(missing.rows),]
my.preds <- predictgls(myGLS, newdframe=missing.data)
missing.data$Temp <- my.preds$Prediction
all.temp.data <- rbind(missing.data, temp2[,-c(5)])


bb <- make_bbox(lon=Lon, lat=Lat, data=temp)
mymap <- get_map(location=bb, zoom=11, maptype="satellite")
ggmap(mymap)+geom_raster(data=all.temp.data, aes(x=Lon, y=Lat, fill=Temp),  alpha=0.8) + scale_fill_distiller(palette="Spectral", na.value=NA) + 
  coord_cartesian() + scale_fill_distiller(palette="Spectral", na.value=NA) + coord_cartesian()




