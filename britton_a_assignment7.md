britton_a\_assignment7
================
Annie Britton
3/6/2023

## PART II: Fulmars and Darwin (20 points)

##### Last week, we worked on using R to create an ANOVA table. While ANOVA has many advantages, it works with SS and variances that are on the squared scale rather than the original scale of measurement, putting them at one remove from the data. To minimize this limitation, modern statistics encourages working as closely as possible with estimates and intervals. We will start taking steps in that direction. A t-test will allow us to work on the same scale of measurement (data or coefficients).

##### The Student’s t-test is a statistical test that compares the mean and standard deviation of two samples to see if there is a significant difference between them. In an experiment, a t-test might be used to calculate whether or not differences seen between the control and each experimental group are a factor of the manipulated variable or simply the result of chance.

##### The t-test is a test of a statistical significant difference between two groups. A “significant difference” means that the results that are seen are most likely not due to chance or sampling error. In any experiment or observation that involves sampling from a population, there is always the possibility that an observed effect would have occurred due to sampling error alone. But if result is “significant,” then the investigator may conclude that the observed effect actually reflects the characteristics of the population rather than just sampling error or chance.

##### Where:

##### • x1 is the mean of sample 1

##### • s1 is the standard deviation of sample 1

##### • n1 is the sample size of sample 1

##### • x2 is the mean of sample 2

##### • s2 is the standard deviation of sample 2

##### • n2 is the sample size in sample 2

##### Next, we just determine the degrees of freedom (df) for the test. In the t-test, the degrees of freedom are the sum of the sample sizes of both groups minus 2. Then, determine the “Critical t-value” in a table by triangulating your DF and the “p value” of 0.05.

##### If your calculated t value is greater than the critical t-value from the table, you can conclude that the difference between the means for the two groups is significantly different. We reject the null hypothesis and conclude that the alternative hypothesis is correct.

``` r
## Load all library packages used in this script:
library(SMPracticals) # Statistical Modelling Practicals
library(ggplot2) # Grammer of Graphics package
library(arm) # Applied Regression Modelling
library(reshape) # Reshape package
```

##### Ok, let’s start with looking at some basic data (from Quinn & Keough 2002, p 40) and then move on to our Darwin dataset.

##### Furness & Bryant (1996) studied energy budgets of breeding northern fulmars (Fulmarus glacialis) in Shetland. As part of their study, they recorded various characteristics of individually labeled male and female fulmars. We will focus on differences in metabolic rate between sexes. There were eight males and six females labeled. The H0 was that there is no difference between the sexes in the mean metabolic rate of fulmars. This is an independent, non-paired comparison because individual fulmars can only be either male or female.

``` r
# Note that the ranges (and variances) are very different in these two samples. Note that in the below, the length of the two samples are the same but we use “NA” to indicate there is no data value.
Male <-c(2950,2308.7,2135.6,1945.6,1195.5,843.3,525.8,605.7)
Female <-c(1956.1,1490.5,1361.3,1086.5,1091,727.7, NA, NA)
t.test (Male, Female)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  Male and Female
    ## t = 0.77317, df = 10.468, p-value = 0.4565
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -518.8042 1075.3208
    ## sample estimates:
    ## mean of x mean of y 
    ##  1563.775  1285.517

##### We would not reject the H0 at the 95% confidence level and thus conclude there was no statistically significant difference in mean metabolic rate of fulmars between sexes.

``` r
# Return to Darwin Plant Inbreeding Data
# To allow comparison of one-way ANOVA with t-test we will use the same example data (the Darwin data frame).
## Load all library packages used in this script:
library(SMPracticals) # Statistical Modelling Practicals
library(ggplot2) # Grammer of Graphics package
library(arm) # Applied Regression Modelling
library(reshape) # Reshape package
```

``` r
# Let’s do a one-way ANOVA and a t-test for comparison.
# linear model including pair and type
anova(lm(height~pair+type, data= darwin))
```

    ## Analysis of Variance Table
    ## 
    ## Response: height
    ##           Df  Sum Sq Mean Sq F value Pr(>F)  
    ## pair      14  86.264   6.162  0.5536 0.8597  
    ## type       1  51.352  51.352  4.6139 0.0497 *
    ## Residuals 14 155.820  11.130                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(lm(height~pair+type, data= darwin))
```

    ## 
    ## Call:
    ## lm(formula = height ~ pair + type, data = darwin)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4958 -0.9021  0.0000  0.9021  5.4958 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  21.7458     2.4364   8.925 3.75e-07 ***
    ## pair2        -4.2500     3.3362  -1.274   0.2234    
    ## pair3         0.0625     3.3362   0.019   0.9853    
    ## pair4         0.5625     3.3362   0.169   0.8685    
    ## pair5        -1.6875     3.3362  -0.506   0.6209    
    ## pair6        -0.3750     3.3362  -0.112   0.9121    
    ## pair7        -0.0625     3.3362  -0.019   0.9853    
    ## pair8        -2.6250     3.3362  -0.787   0.4445    
    ## pair9        -3.0625     3.3362  -0.918   0.3742    
    ## pair10       -0.6250     3.3362  -0.187   0.8541    
    ## pair11       -0.6875     3.3362  -0.206   0.8397    
    ## pair12       -0.9375     3.3362  -0.281   0.7828    
    ## pair13       -3.0000     3.3362  -0.899   0.3837    
    ## pair14       -1.1875     3.3362  -0.356   0.7272    
    ## pair15       -5.4375     3.3362  -1.630   0.1254    
    ## typeSelf     -2.6167     1.2182  -2.148   0.0497 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.336 on 14 degrees of freedom
    ## Multiple R-squared:  0.469,  Adjusted R-squared:  -0.09997 
    ## F-statistic: 0.8243 on 15 and 14 DF,  p-value: 0.6434

``` r
#To do the t-test, we need the data in a different format.
# This 'long' format is what we need for linear model analysis like ANOVA
# and to produce some types of graph
# But, for some uses (e.g. t-tests) we need 'wide' format:
# Using melt() and cast() from Hadley Wickham's reshape2 package
library(reshape2)
(mDarwin <- melt(darwin, id.vars= c("pot", "pair", "type"), measure.vars="height"))
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
# or in this case, simply: melt(darwin)
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
darwide$Differences <- darwide$height_Cross - darwide$height_Self
darwide # Wide forma augmented with the differences in height
```

    ##    pot pair height_Cross height_Self Differences
    ## 1    I    1       23.500      17.375       6.125
    ## 2    I    2       12.000      20.375      -8.375
    ## 3    I    3       21.000      20.000       1.000
    ## 4   II    4       22.000      20.000       2.000
    ## 5   II    5       19.125      18.375       0.750
    ## 6   II    6       21.500      18.625       2.875
    ## 7  III    7       22.125      18.625       3.500
    ## 8  III    8       20.375      15.250       5.125
    ## 9  III    9       18.250      16.500       1.750
    ## 10 III   10       21.625      18.000       3.625
    ## 11 III   11       23.250      16.250       7.000
    ## 12  IV   12       21.000      18.000       3.000
    ## 13  IV   13       22.125      12.750       9.375
    ## 14  IV   14       23.000      15.500       7.500
    ## 15  IV   15       12.000      18.000      -6.000

``` r
# To perform the equivalent of the ANOVA we use the paired version of the t-test.
# Paired T-test
t.test(darwide$height_Cross, darwide$height_Self, paired= TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  darwide$height_Cross and darwide$height_Self
    ## t = 2.148, df = 14, p-value = 0.0497
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.003899165 5.229434169
    ## sample estimates:
    ## mean of the differences 
    ##                2.616667

``` r
#How do the results compare?
#The output for the paired t-test reminds us that we are asking whether we can reject a hypothesis that the means of the two treatments are equal. Because the values are paired, we can also take the single column of the 15 differences in height and perform the equivalent one-sample t-test.
# One sample t test
t.test(darwide$Differences)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  darwide$Differences
    ## t = 2.148, df = 14, p-value = 0.0497
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.003899165 5.229434169
    ## sample estimates:
    ## mean of x 
    ##  2.616667

##### As long as we make the equivalent comparison, we get the same results. Now notice that our output is accompanied by new numbers. These are confidence intervals (CIs). They are the lower and upper limits of a 95% confidence interval. The level of 95% for the CI is the complement to the 5% P-value. In my simple world, a CI is a range of values that we are fairly sure that our true value lies in. In essence the 95% CI is ± 2 SE of the mean.

``` r
# linear model (unpaired analysis for simplicity)
lm1 <- lm(height ~ type, data= darwin)

# or, paired analysis:
#lm1 <- lm(height ~ pair + type, data= darwin)

# Create a data frame 'estimates' to hold...
# the means, SDs, SEMs or interval bounds:
estimates <- expand.grid(type=levels(darwin$type))

# Add the mean heights to the dataframe
estimates$height <- predict(lm1, newdata= estimates)
#estimates$height <- tapply(darwin$height, darwin$type, mean)

## Means with 95% CIs
# add 95% CI upper and lower bounds to estimates dataframe
estimates$C95_lwr <- predict(lm1, newdata= estimates, interval="confidence")[,2]
estimates$C95_upr <- predict(lm1, newdata= estimates, interval="confidence")[,3]

# Plot of means with 95% CIs
p <- qplot(type, height, ylim=c(11,25), data=estimates, main="95% CIs")
(e <- p + geom_linerange(aes(min=C95_lwr, max=C95_upr)) + theme_bw())
```

![](britton_a_assignment7_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

##### How would we write the results? “The cross-pollinated maize was significantly taller than the self-pollinated maize plants, by 2.6 inches on average (95% CI: 0.003, 5.229).
