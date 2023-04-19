britton_a\_assignment6
================
Annie Britton
2/26/2023

## PART I: One-Way ANOVA R Tutorial (5 points)

#### Body Mass Index (BMI) is a simple index of weight-for-height that is commonly used to classify underweight, overweight and obesity in adults. It is defined as the weight in kilograms divided by the square of the height in metres (kg/m2). For example, an adult who weighs 70kg and whose height is 1.75m will have a BMI of 22.9.

``` r
# Type the following data into a vector and store it with the measurement name, radialap, at the command prompt “>.” Press enter/return.
radialap <- c(9, 11, 9, 10, 11, 11, 11, 11, 10, 10, 10, 11, 10, 9, 11, 11)

# Create a second vector with the name obeseclass using the values below. Press enter/return.
obeseclass <- c(1, 1, 1, 1, 1, 1, 2, 2 ,2, 2, 2, 3, 3, 3, 3, 3)


# Ensure obeseclass is recognized in R as a categorical variable by inquiring if it is a factor using the is.factor() function. Type in the code below and press enter/return.
is.factor(obeseclass)
```

    ## [1] FALSE

``` r
# An answer of FALSE indicates it is not a categorical variable, while TRUE means it is.
# If obeseclass is not a factor, convert it to a factor by applying the factor() function to obeseclass and saving it to a variable of the same name. Type in the programming below and press enter/return.
obeseclass <- factor(obeseclass)

# Invoke the aov() function by using the formula Y∼X or Dependent∼Independent variable. Store the function to the name of your choice. In this case, we will use result. Type in the script below and press enter/return.
result <- aov(radialap ~ obeseclass)

# Process the ANOVA results by using the summary() function and the name of the results (result). Type in the following code and press enter/return.
summary(result)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## obeseclass   2  0.204  0.1021   0.144  0.867
    ## Residuals   13  9.233  0.7103

#### Results Statement: Based on the p-value above (0.867), there is no significant difference in radial measurements among the different classifications of obesity.

## PART II: Darwin, Fitness Surrogates and P-Values (15 points)

``` r
# #To obtain Darwins Plant Data install SMPracticals package.
# install.packages("SMPracticals")
# #Grammer and Graphics Package
# install.packages("ggplot2")
# #Data Manipulator Package
# install.packages("reshape2")
# #Applied Regression Modeling Package
# install.packages("arm")

#Put our package into use by adding to our data library
library(SMPracticals)
library(reshape)
library(reshape2)
library(arm)
library(tidyverse)

# check the data
glimpse(darwin)
```

    ## Rows: 30
    ## Columns: 4
    ## $ pot    <fct> I, I, I, I, I, I, II, II, II, II, II, II, III, III, III, III, I~
    ## $ pair   <fct> 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 1~
    ## $ type   <fct> Cross, Self, Cross, Self, Cross, Self, Cross, Self, Cross, Self~
    ## $ height <dbl> 23.500, 17.375, 12.000, 20.375, 21.000, 20.000, 22.000, 20.000,~

#### Interpretation: The graph below shows the seedling height of crossed and selfed plants. It looks like the mean height of the crossed plants tends to be heigher than the selfed plants.

``` r
# Use ggplot2 package to plot height as a function of treatment
ggplot(data= darwin, aes(x=type, y=height, color= type))+ geom_point()+theme_bw()
```

![](britton_a_assignment6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### Interpretation: The graph below includes the means of each type of seedling, confirming the guess above that the mean height is higher for crossed versus selfed plants.

``` r
#A graph with means added.
p <- ggplot(data= darwin, aes(x=type, y=height, color= type))+ 
  geom_point()+ labs(title="Individual and average seedling heights", y="Seedling height (inches)", x="Pollination")+
  theme_bw()

# Create a function to calculate averages:
# Create a function to calculate averages:
midm <- function(x) mean(x) 

# add averages to plot:
p + stat_summary(aes(color= "Mean"), geom= "point", fun= midm)
```

![](britton_a_assignment6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### Interpretation: The graph below shows the SST, SSA, and SSE of the data. The SST shows us a summary of the variability across all of the observations regardless of pollination type. The SSA shows the variability due to differences of the group means, while the SSE shows the variability within each group from the group mean.

``` r
### How sum of squares works:
# Make a copy of the data re-ordered by type:
long <- darwin[ order(darwin$type), ]

# number the reordered data 1-30 for use in plotting
long$index <- c(1:30)

# landscape page with 3 panels
par(mfrow=c(1,3)) 


## SST (sum squares total)
plot(long$height, pch= 16, main= "Square and sum differences for SST", 
     ylab= "Height", 
     xlab= "Data ordered by type")
abline(mean(long$height), 0) # horizontal line at mean
for(i in 1:30) 
  lines(c(i,i), c(mean(long$height), long$height[i]), lty= 3, col="darkgrey")


## SSA
long$predictions <- predict( lm(height ~ type, data= long) )
plot(long$height, type= "n", main= "Square and sum differences for SSA"
     , ylab= "Height", xlab= "Data ordered by type")
abline(mean(long$height), 0) # line at mean with slope of zero
points(long$height[long$type== "Cross"] ~ long$index[long$type== "Cross"], pch= 4, col= "darkgrey") 
points(long$height[long$type== "Self"] ~ long$index[long$type== "Self"], pch= 1, col= "darkgrey") 

# predicted values for crossed 
points(long$predictions[long$type== "Cross"] ~ long$index[long$type== "Cross"], pch= 4, col= "red")

# predicted values for selfed 
points(long$predictions[long$type== "Self"] ~ long$index[long$type== "Self"], pch= 1)
for(i in 1:30) 
  lines( c(i,i), c( mean(long$height), long$predictions[i] ), lty= 3, col= "darkgrey")


## SSE
means <- tapply(long$height, long$type, mean)
plot(long$height, type= "n", main= "Square and sum differences for SSE"
     , ylab= "Height", xlab= "Data ordered by type")
points(long$height[long$type== "Cross"] ~ long$index[long$type== "Cross"], pch= 4, col= "red") 
points(long$height[long$type== "Self"] ~ long$index[long$type== "Self"], pch= 1)
lines(c(1, 15),c(means[1], means[1]), col= "red" )
lines(c(16, 30), c(means[2], means[2]))
for (i in 1:15) lines(c(i,i), c(means[1], long$height[i]),col="darkgrey", lty= 3)
for (i in 16:30) lines(c(i,i), c(means[2], long$height[i]),col="darkgrey", lty= 3)
```

![](britton_a_assignment6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Note: that there is a catch because of the squaring necessary to calculate the SS this estimate of the variability is not on the same scale as the original data (not inches but square inches!). Luckily we can reverse the squared variance by calculating the standard deviation.

#   The overall mean and standard deviation ( mean(darwin$height) )
(sd(darwin$height))
```

    ## [1] 3.180953

``` r
#   The standard deviation is the square root of the variance ( var(darwin$height) )
(sqrt( var(darwin$height)))
```

    ## [1] 3.180953

#### Interpretation: The graph below is a box and whisker plot, which suggests that there are differences between the two groups of pollination types. It seems that cross pollination, again, has a higher mean height than the selfed seedlings.

``` r
# OK, to recap, we have seen how the least squares process minimizes the SS to find the best estimates and how it uses these SS and DF to calculate a mean square, or variance. Finally taking the square root of the variance produces the SD which is variability in a sample that is conveniently on the same scale as the original measurements. However, we have not answered our question!

# Question: Do we, in fact, have two samples with different mean heights due to inbreeding?

# One useful approach for graphically describing this data is the now tried and true box and whisker plot invented by John Tukey.

# to have a description of the data, median, lower and upper quartile containing 50% of the data the whiskers containing 95% of the data and the outliers
ggplot(data= darwin, aes(x=type, y=height, color= type))+ 
  geom_boxplot()+ labs(title="Darwin's Maize: Box-&-Whisker plot", y="Height (inches)", x="Fertilisation")+
  theme_bw()
```

![](britton_a_assignment6_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# It can be helpful to regroup our data so that we cast a wide version (instead of a long list) that specifies pot and pollination pairs to be the rows. This makes statistical summaries a bit easier.

# This 'long' format is what we need for linear model analysis like ANOVA
# and to produce some types of graph
# But, for some uses (e.g. t-tests) we need 'wide' format:
# Using melt() and cast() from Hadley Wickham's reshape2 package:
mDarwin <- melt(darwin, id.vars= c("pot", "pair", "type"), measure.vars="height")
mDarwin
```

    ##    pot pair  type variable  value
    ## 1    I    1 Cross   height 23.500
    ## 2    I    1  Self   height 17.375
    ## 3    I    2 Cross   height 12.000
    ## 4    I    2  Self   height 20.375
    ## 5    I    3 Cross   height 21.000
    ## 6    I    3  Self   height 20.000
    ## 7   II    4 Cross   height 22.000
    ## 8   II    4  Self   height 20.000
    ## 9   II    5 Cross   height 19.125
    ## 10  II    5  Self   height 18.375
    ## 11  II    6 Cross   height 21.500
    ## 12  II    6  Self   height 18.625
    ## 13 III    7 Cross   height 22.125
    ## 14 III    7  Self   height 18.625
    ## 15 III    8 Cross   height 20.375
    ## 16 III    8  Self   height 15.250
    ## 17 III    9 Cross   height 18.250
    ## 18 III    9  Self   height 16.500
    ## 19 III   10 Cross   height 21.625
    ## 20 III   10  Self   height 18.000
    ## 21 III   11 Cross   height 23.250
    ## 22 III   11  Self   height 16.250
    ## 23  IV   12 Cross   height 21.000
    ## 24  IV   12  Self   height 18.000
    ## 25  IV   13 Cross   height 22.125
    ## 26  IV   13  Self   height 12.750
    ## 27  IV   14 Cross   height 23.000
    ## 28  IV   14  Self   height 15.500
    ## 29  IV   15 Cross   height 12.000
    ## 30  IV   15  Self   height 18.000

``` r
# or in this case, simply:  melt(darwin)
# R reports: Using pot, pair, type as id variables
# the darwin dataframe rearranged in wide format using cast():
(darwide <- cast(mDarwin, pot + pair ~ variable + type))
```

    ##    pot pair height_Cross height_Self
    ## 1    I    1       23.500      17.375
    ## 2    I    2       12.000      20.375
    ## 3    I    3       21.000      20.000
    ## 4   II    4       22.000      20.000
    ## 5   II    5       19.125      18.375
    ## 6   II    6       21.500      18.625
    ## 7  III    7       22.125      18.625
    ## 8  III    8       20.375      15.250
    ## 9  III    9       18.250      16.500
    ## 10 III   10       21.625      18.000
    ## 11 III   11       23.250      16.250
    ## 12  IV   12       21.000      18.000
    ## 13  IV   13       22.125      12.750
    ## 14  IV   14       23.000      15.500
    ## 15  IV   15       12.000      18.000

``` r
# Now, the question is: how confident are we that this observed differences reflects the negative effects of selfing. To judge this we have to assess the signal (difference between treatment means) relative to the noise (level of variation within the samples). ANOVA uses variance to quantify the signal and noise. Before going there let’s calculate the SD of the crossed and selfed samples.

#   Alternatively using the reshape FUNCTION
#   darwide <- reshape(darwin, timevar= "type", idvar= "pair", v.names= "height", direction= "wide")
#   Means for the two groups
(mean(darwide$height_Cross))
```

    ## [1] 20.19167

``` r
(mean(darwide$height_Self))
```

    ## [1] 17.575

``` r
# Standard deviations for the two groups
(sd(darwide$height_Cross))
```

    ## [1] 3.616945

``` r
(sd(darwide$height_Self))
```

    ## [1] 2.051676

``` r
# ANOVA using Linear model function:
#The least square process estimates the lines of best fit - the means for each treatment - and quantifies to variation in the data using SS.

# Intercept only - in this case the grand mean
ls0 <- lm( height ~ 1, data= darwin) 
library(arm) # for the display() function
display(ls0)
```

    ## lm(formula = height ~ 1, data = darwin)
    ##             coef.est coef.se
    ## (Intercept) 18.88     0.58  
    ## ---
    ## n = 30, k = 1
    ## residual sd = 3.18, R-Squared = 0.00

``` r
# sd is the square root of the variance
( 3.18^2)
```

    ## [1] 10.1124

``` r
# 10.1124

# Linear model including pollination type
ls1 <- lm( height ~ 1+type, data= darwin)

# note that now the intercept is type=Self
display(ls1) 
```

    ## lm(formula = height ~ 1 + type, data = darwin)
    ##             coef.est coef.se
    ## (Intercept) 20.19     0.76  
    ## typeSelf    -2.62     1.07  
    ## ---
    ## n = 30, k = 2
    ## residual sd = 2.94, R-Squared = 0.18

``` r
# Use relevel() to set type=Self to be the intercept and refit the model: 
darwin$type <- relevel(darwin$type, ref= "Self")

# ANOVA table output
anova(ls1) 
```

    ## Analysis of Variance Table
    ## 
    ## Response: height
    ##           Df  Sum Sq Mean Sq F value  Pr(>F)  
    ## type       1  51.352  51.352  5.9395 0.02141 *
    ## Residuals 28 242.083   8.646                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Not very biologically meaningful, but the first column gives the source of the variation (pollination treatment and the unexplained variation). The signal to noise ratio is calculated by dividing the treatment variance by the residual error variance to produce the F-value. A value of 5.0 means the estimated signal is nearly six times larger than the noise. The P -value quantifies our confidence that the signal to noise ratio is not a false positive. The probability value of 0.02 says that there is a 2% chance of having data that would produce a signal-to-noise ratio of this size or larger if there was actually no effect of pollination type. Essentially we have a 2% chance of a false positive.

# This is a lot of information and work, but remember that now you have the script and can modify it for your own needs. However, below is how you would write up your model results in a professional publication.

# Presenting our results:

# “The self-=pollinated maize measured 17.5 inches in height on average while the cross-pollinated plants had a height of 20.2 inches – a difference of 2.6 inches which was statistically significant (F1,28 = 5.9; p = 0.02)”.
```
