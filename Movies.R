# Mitchell Pudil
# Movie Revenue


# Importing from Libraries ------------------------------------------------

library('lubridate')

# EDA (Scatterplot) ----------------------------------------------------------

# Import Data
MovieRevenue <- na.omit(read_csv("~/Downloads/MovieRevenue.csv"))

# 1. Scatterplot of Production Budget by Domestic Gross
ggplot(data=MovieRevenue, mapping=aes(x=ProductionBudget,y=DomesticGross)) + geom_point() +
  geom_smooth() +
  ggtitle('Scatterplot of Production Budget by Domestic Gross') + # for the main title 
  xlab('Production Budget') + # for the x axis label
  ylab('Domestic Gross') # for the y axis label

# 2.Boxplot for Domestic Gross by Month 

  #Split day, month, and year and add them as columns in movie dataframe
  MovieRevenue$ReleaseDate <-  as.Date(MovieRevenue$ReleaseDate)
  datetxt <- as.Date(MovieRevenue$ReleaseDate)

  MovieDatesdf <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))



  Moviesdf <- cbind(MovieRevenue, MovieDatesdf)
  

  # Create Boxplot

  # Ask how to get this in order

  ggplot(Moviesdf, aes(x=as.factor(month), y = DomesticGross, col=as.factor(month))) + 
    geom_boxplot() +
    ggtitle("Boxplot for Domestic Gross by Month")
  
  
  # 3. Draw a scatterplot of log(DomesticGross) by log(ProductionBudget). 
      # Add a smooth line to gauge how linear the relationship is.

  
  ggplot(data=MovieRevenue, mapping=aes(x=log(ProductionBudget),y=log(DomesticGross))) + geom_point() +
    geom_smooth() +
    ggtitle('Scatterplot of Production Budget by Domestic Gross (Logged)') + # for the main title 
    xlab('Log of Production Budget') + # for the x axis label
    ylab('Log of Domestic Gross') # for the y axis label
  
  
  
# Analysis with a MLR ----------------------------------------------------------------

# 1. Fit a MLR model and build a 95% confidence interval for the effect of ProductionBudget 
    # on DomesticGross (this is the first attempt at answering research question #1).
  
  # Let's control for the month as well for better prediction
movielm <- lm(DomesticGross ~ ProductionBudget + as.factor(month), data=Moviesdf)
summary(movielm) # ANOVA

confint(movielm,level=0.95)[2,] # 95% CI for Production Budget

#2. Get a prediction for each movie in your dataset and identify the 5 movies that were the 
  # most above the predicted value. Also identify the 5 movies that were the most below the 
  #predicted value.

  # Prediction for each movie

FittedValues <- predict.lm(movielm)
Moviesdf$Prediction <- FittedValues  # Save predictions to Movies df 

resid <- resid(movielm)
Moviesdf$resid <- resid # Capture residuals for each movie

Moviesdf <-   Moviesdf[order(Moviesdf$resid),] # Order by residuals
head(Moviesdf$Movie, n=5) # Least profitable 
tail(Moviesdf$Movie, n=5) # Most profitable



# 3. Show that the assumptions of the MLR model are not met (and hence statistical 
    # inference using an MLR model are not valid) by drawing a scatterplot of fitted 
    # vs. standardized residuals and a histogram of the standardized residuals.

Moviesdf$Standard_Resid <- stdres(movielm) # Create Standardized Residuals in Movies df

  # Plot Fitted Values vs. standardized residuals 
ggplot(data=MovieRevenue, mapping=aes(x=FittedValues,y=Standard_Resid)) + geom_point() +
  ggtitle('Scatterplot of Fitted vs. Standardized Residuals') + # for the main title 
  xlab('Fitted Values') + # for the x axis label
  ylab('Standardized Residuals') # for the y axis label 
  
  # Histogram of stanardized residuals
ggplot() + 
  geom_histogram(mapping=aes(x=stdres(movielm)), bins=30) +
  ggtitle('Histogram of stanardized residuals') +
  xlab('Standardized Residuals')
    
# From these two plots, we know that our N and E assumptions don't hold, so let's try 
# transforming the data!



# 4. Try 2 or 3 transformations of DomesticGross and/or ProductionBudget 
    # and show that these transformations are not going to fix the assumptions 
    #of the MLR model.

    # i. First, let's try taking the square root of both variables:

Moviesdf$sqrtPB <- sqrt(Moviesdf$ProductionBudget)
Moviesdf$sqrtDG <- sqrt(Moviesdf$DomesticGross)

# Plot Sqrt of PB & Sqrt of DG
ggplot(data=MovieRevenue, mapping=aes(x=sqrtPB,y=sqrtDG)) + geom_point() +
  ggtitle('Scatterplot of Square Root of Production Budget vs. Squart Root of Domestic Gross') + # for the main title 
  xlab('Square Root of Production Budget') + # for the x axis label
  ylab('Squart Root of Domestic Gross') # for the y axis label 








