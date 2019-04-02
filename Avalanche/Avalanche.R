# Mitchell Pudil
# STAT 330
# Avalanches

# Clean Data
library(stringi)
library(dplyr)
avalanche.raw <- read.csv("avanlanche.csv")

avalanche.raw$month <- as.numeric(substr(avalanche.raw$Date, 1,2))
avalanche.raw$year <- as.numeric(stri_sub(avalanche.raw$Date, -4,-1))
avalanche.raw <- subset(avalanche.raw, avalanche.raw$year!="NA")
avalanche.raw <- avalanche.raw[-c(2293),]
avalanche.raw <- subset(avalanche.raw, avalanche.raw$year>=2000)

# Count Number of Occurrences by month

count(avalanche.raw, year, month)
#piping
occurences <- data.frame(avalanche.raw %>% count(year, month))
colnames(occurences) <- c("year", "month", "occurences")

snow.occurences <- subset(occurences, month>11 | month<4)

weather.raw <- read.csv("slc_weather.csv", sep=",")
weather.raw$year <- as.numeric(substr(weather.raw$DATE, 1,4))
weather.raw$month <- as.numeric(substr(weather.raw$DATE, 6,7))
weather.raw <- subset(weather.raw, month>11 | month<4)
avalanche <- merge(occurences, weather.raw, all.x=TRUE)
avalanche <- na.omit(avalanche)

# EDA
plot(avalanche$SNOW, avalanche$occurences, col="blue", xlab="Snow", ylab="Avalanches")
plot(avalanche$DT32, avalanche$occurences, col="green", xlab="DT32", ylab="Avalanches")
    # We see curvature here: more DT32: more avalanches, and variance is wider
    # Would normally take natural log, but all of our 0s would go away
    # Link function embeds log transformation inside the MLE
avalanche$logoccurences <- log(avalanche$occurences)

plot(avalanche$DT32, avalanche$logoccurences, col="green", xlab="DT32", ylab=" Log Avalanches")


# Model
# y ~ Poisson(exp(b0 + b1*snow + b2*tmin +b3 dt32))
out.avalanche <- glm(occurences ~ SNOW + TMIN + DT32, data=avalanche, family="poisson") # Does poisson and link
summary(out.avalanche)
#For each additional inch of snow, the log of the mean number of avalanches decreases by 0.04

# For interpretation
exp(coef(out.avalanche)[-1])  # coef for snow is 0.9579
# There is a statistically significant effect of snowfall on the mean number of avalanches.
# For an additional 1 inch of monthly cumulative snowfall we estimate the mean # of 
# avalanches will decrease by 4.3% holding all else constant

# There is also a significant effect of temperature (TMIN and DT32) on the mean number of avalanches.
# One more day of below freezing weather results in about 1.96% fewer avalanches, all else constant

# A one degree increase in temperature for the lowest temperature of the month decreases the number 
# of avalanches by about 4%, holding all else constant

#95% CI for each explanatory variable
exp(confint(out.avalanche)[-1,])

# Test H0: no temp effect via Chi Squared Test
red.avalanche <- glm(occurences ~ SNOW, data=avalanche, family="poisson") # Does poisson and link
summary(out.avalanche)
anova(red.avalanche, out.avalanche, test="Chisq")


# December prediction
summary(subset(avalanche, month==12))
predict(out.avalanche, newdata=data.frame(SNOW=12.2, TMIN= 22.8, DT32=28), type="response")


# Strengths: Allows for discrete response variables
# Weaknesses: Have to transform for interpretability

# Data: Number of strikes in a professional bowling game as a function of oil type, number of tournaments won,
# amount of money won, etc. Data available at pba.com. 






