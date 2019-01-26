#Mitchell Pudil
#STAT 330 
#Diamonds: Multiple Regression


diamonds <- read.table("http://www.amstat.org/publications/jse/v9n2/4Cdata.txt")
names(diamonds)<-c("Carat","Color","Clarity","Cert","Price")

#Tail and quick summary stats

tail(diamonds)
summary(diamonds$Price)
#Scatterplot of carat and price

plot(diamonds$Price ~ diamonds$Carat, col="dark green", xlab="Carat", ylab="Price (in Singapore $)", main="Price of Diamonds by Carat")


#Curvature and variation look multiplicative. Take natural log. 

diamonds$lncarat <- log(diamonds$Carat)
diamonds$lnprice <- log(diamonds$Price)

plot(diamonds$lnprice ~ diamonds$lncarat, col="dark blue", xlab="Log of Carat", ylab="Log of Price (in Singapore $) ", main="Log of Price of Diamonds by Log of Carat")

#It appears that taking the natural log of the data makes the data appear
# more linear, with fewer outliers, and a constant variance. Varaition increases with price.
#It's not perfect, but it's better.

#TBS: Transform both sides (response and explanatory variables)


#Analysis: 

#Response Variable: Log of the diamond price
# Explanatory Variable: Log of carat

#Model: log(price) = b0 + b1*log(carat) + e
# Price = e^(b0) * e^(b1*ln(carat)) * e^epsilon
# a0*Carat^b1 * epsilon, epsilon* ~ lognormal, skewed distribution
# multiplicative model


out.diamonds <- lm(lnprice ~ lncarat, data=diamonds)
summary(out.diamonds)

anova(out.diamonds)

confint(out.diamonds)  #Confidence intervals

#Graph of model
library(ggplot2)
qplot(lnprice, lncarat, data=diamonds,
      geom="smooth", formula= y~x, method="lm", se=TRUE,  
      xlab="Log of Carat",
      ylab="Log of Price",
)

# for a 1% increase in carat size, we estimate an expected increase of 
#  1.53% in price.
# 
#  pvalue < 0.0001


#Prediction for one-carat model

# logPriceHat = 9.13 + 1.54*ln(1) = 9.13
# PriceHat = e^9.13

#Confidence interval: mean diamond price
#Prediction interval: for a future item (here, one carat ring)

#In log price
lnpred <- predict(out.diamonds, newdata=data.frame(lncarat=1), interval="prediction")

#Price (untransform)

exp(lnpred)

# Real R Squared:

1 - sum((diamonds$Price - exp(predict(out.diamonds)))^2) / sum((diamonds$Price - mean(diamonds$Price))^2)

#Median Absolute Error: take median of the abs(y-yhat) 
#Summary

summary(abs(diamonds$Price - exp(predict(out.diamonds))))

#We find that good prediction occurs when carat size is small.

plot.df <- cbind(diamonds, exp(predict(out.diamonds, interval="prediction")))

# Publication graphic showing prediction of diamond price given carat. 
ggplot(plot.df,aes(Carat, Price)) +
    xlab("Diamond Size (Carat)") +
    ylab("Diamond Price (Singapore $)") +
    geom_point() +
    geom_line(aes(y=fit), color="royalblue") +
    geom_line(aes(y=lwr), color="red", linetype="dashed") +
    geom_line(aes(y=upr), color="red", linetype="dashed")


# Simlar Research Tasks: Finding how acreage affects housing price
# Data available at https://catalog.data.gov/dataset?tags=real-estate

# Data Weaknesses: Here, only using one variable to predict. 




