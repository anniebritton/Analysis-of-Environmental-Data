britton_a\_assignment8
================
Annie Britton
3/13/2023

``` r
# load ggplot as part of tidyverse
library(tidyverse)
```

## PART I: Spearman Rank and Pearsons (15 points)

In this assignment, you will carry out a Spearman rank correlation and a
Pearson’s product moment correlation.

``` r
# load in the mayfly data
path <- "C:\\Users\\annie\\Documents\\AEEN\\M08\\mayflycorrl.csv"
mayflycorrl <- read.csv(path)
glimpse(mayflycorrl)
```

    ## Rows: 25
    ## Columns: 7
    ## $ n      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ~
    ## $ Length <int> 20, 21, 22, 23, 21, 20, 19, 16, 15, 14, 21, 21, 21, 20, 19, 18,~
    ## $ Speed  <int> 12, 14, 12, 16, 20, 21, 17, 14, 16, 21, 21, 26, 11, 9, 9, 11, 1~
    ## $ Abund  <int> 31, 35, 33, 40, 55, 55, 45, 37, 43, 58, 60, 89, 26, 23, 18, 28,~
    ## $ Algae  <int> 40, 45, 45, 80, 75, 65, 65, 65, 35, 30, 65, 70, 85, 70, 35, 30,~
    ## $ NO3    <dbl> 2.25, 2.15, 1.75, 1.95, 1.95, 2.75, 1.85, 1.75, 1.95, 2.35, 2.3~
    ## $ BOD    <int> 200, 180, 135, 120, 110, 120, 95, 168, 180, 195, 158, 145, 140,~

``` r
# create a scatterplot of the mayfly data
ggplot(mayflycorrl, aes(x = Speed, y = Abund)) +
geom_point((aes(size=Length)))+
xlab("Stream Speed (m/s)") +
ylab("Mayfly Abundance") +
theme_bw()
```

![](britton_a_assignment8_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# adding line of best fit
ggplot(mayflycorrl, aes(x = Speed, y = Abund)) +
geom_point((aes(size=Length)))+
xlab("Stream Speed (m/s)") +
ylab("Mayfly Abundance") +
geom_smooth(method="lm") +
theme_bw()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](britton_a_assignment8_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

OK, we now have a nice graph and some data that may be correlated with
one another. but we should confirm this with a statistical test. Let’s
run two tests:Pearson’s Correlation & Spearman’s Rank Correlation.

### **How do we know to use a correlation analysis with these data?**

We didn’t set out with a directional relationship in mind of the form “X
causes Y”. There may be an association between the two species, but it
is not obvious which should be the dependent and independent variables.
Neither variable is controlled by the investigator, and we’re not
interested in using one variable to predict the values of the other.
Taken together, these observations indicate that correlation analysis is
the appropriate method to use to evaluate the significance of the
association.

### **Why are we using Pearson’s correlation?**

A test based on Pearson’s correlation will be more powerful than one
using Spearman’s correlation. We need to be confident that all the
assumptions are met though. The scatter plot indicates that the
relationship between the variables is linear, so Pearson’s correlation
is a valid measure of association. Is it appropriate to carry out a
significance test though? The data are of the right type—both variables
are measured on a ratio scale—and the two dot plots above suggest the
normality assumption is reasonable.

**Pearson’s correlation, being a parametric technique, makes some
reasonably strong assumptions:**

**• The data are on an interval or ratio scale.**

**• The relationship between the variables is linear.**

**• Both variables are normally distributed in the population.**

The requirements are fairly simple and shouldn’t need any further
explanation.

``` r
# use cor.test to run a Pearson's correlation on the speed and abund variables
cor.test(~ Speed + Abund, method = "pearson", data = mayflycorrl)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  Speed and Abund
    ## t = 22.832, df = 23, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.9514204 0.9906847
    ## sample estimates:
    ##       cor 
    ## 0.9786441

We use R’s formula system to determine which pair of variables are
analyzed. However, instead of placing a variable on the left hand side
and a variable on the right hand side (e.g. Speed \~ Abund), both two
variables appear to the right of the \~ separated by a + symbol. This
convention makes good sense if you think about where we use correlation:
a correlation analysis examines association, but it does not imply the
existence of dependent and independent variables (Regression). To
emphasize the fact that neither variable has a special status, the
cor.test function expects both variables to appear to the right of the
\~, with nothing on the left. The output from the cor.test is very
similar to that produced by the t.test function.

The first part says that the test statistic associated with a Pearson’s
correlation coefficient is a type of t-statistic. We’re not going to
spend time worrying about where this came from, other than to note that
it is interpreted in exactly the same way as any other t-statistic.

Next we see the degrees of freedom for the test. Can you see where this
comes from? It is n−2, where n is the sample size. Together, the degrees
of freedom and the t-statistic determine the p-value…

The t-statistic and associated p-value are generated under the null
hypothesis of zero correlation (r=0). Since p \< 0.05, we conclude that
there is a statistically significant correlation between speed and
abundance.

What is the actual correlation between stream speed and mayfly
abundance? That’s given at the bottom of the test output: 0.98. As
expected from the scatter plot, there is quite a strong positive
association.

### **Here’s how to report the results of this analysis:**

There is a strong positive correlation between stream speed and mayfly
abundance among the study plots (r=0.98, n=25, p \< 0.0001).

### **Now time for Spearman’s Rank Correlation**

The assumptions of Pearson’s correlation are not too restrictive, but if
the data do not match them a non-parametric method such as Spearman’s
rank correlation (ρ) is the best approach. The advantages of using
Spearman’s rank correlation are: 1) the two variables do not need to be
normally distributed, and 2) ordinal data can be used. This means
Spearman’s rank correlation can be used with data having skewed (or
other odd) distributions, or with data originally collected on a
rank/ordinal scale.

This latter feature makes it very useful for many studies in, for
example, behavior and psychology, where the original data may have been
collected on such a scale.

**The key assumptions of Spearman’s rank correlation are:**

**• Both variables are measured on ordinal, interval or ratio scales.**

**• There is a monotonic relationship between the two variables.**

A monotonic relationship occurs when, in general, the variables increase
in value together, or when the values of one variable increase, the
other variable tends to decrease. What this means in practice is that we
should not use Spearman’s rank correlation if a scatter plot of the data
forms a parabolic shape.

``` r
# use cor.test to run a Spearman's correlation on the speed and abund variables
cor.test(~ Speed + Abund, method = "spearman", data = mayflycorrl)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  Speed and Abund
    ## S = 23.103, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##      rho 
    ## 0.991114

### **Results of this analysis:**

According to the Spearman’s correlation, there is a strong positive
correlation between stream speed and mayfly abundance among the study
plots (r=0.99, n=25, p \< 0.0001). This correlation is only slightly
stronger here than indicated in the Pearson’s correlation (r=.98).

### **Quick Look at the relationship between % Algae and Abundance**

``` r
# adding line of best fit between algae and abund
ggplot(mayflycorrl, aes(x = Algae, y = Abund)) +
geom_point((aes(size=Length)))+
xlab("% Cover of Algae on Substrate") +
ylab("Mayfly Abundance") +
geom_smooth(method="lm") +
theme_bw()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](britton_a_assignment8_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# use cor.test to run a Pearson's correlation on algae and abund variables
cor.test(~ Algae + Abund, method = "pearson", data = mayflycorrl)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  Algae and Abund
    ## t = -0.41937, df = 23, p-value = 0.6788
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4661961  0.3189990
    ## sample estimates:
    ##         cor 
    ## -0.08711201

``` r
# use cor.test to run a Spearman's correlation on algae and abund variables
cor.test(~ Algae + Abund, method = "spearman", data = mayflycorrl)
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  Algae and Abund
    ## S = 3082.7, p-value = 0.3743
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.1856526

The results from both the Pearson and Spearman’s correlations above
confirm what the scatter plot suggests, in that there is a very weak
negative correlation (r=-.18) between the % of Algae cover on the
substrate and the abundance of mayflies.

## PART II: A Look at Regression (5 points)

A simple linear regression is underpinned by a statistical model. If you
skim back through the textbook you will see that the equation y = a + b
× x represents the ‘systematic component’ of the regression model. This
bit describes the component of variation in y that is explained by the
model for the dependence of y on x. The residuals correspond to the
‘random component’ of the model. These represent the component of
variation in the y variable that our regression model fails to describe.

There are various functional traits being studied by plant scientists.
One of the most popular ones is wood density. Density is measured as dry
mass divided by wet volume \[g cm-3\] and it varies considerably from
around 0.1 g cm-3 to over 1 g cm-3 worldwide. Despite its popularity the
functional role of density is not clear. In this lesson, we will use the
hardness of timber samples and the density of the wood to predict the
unknown hardness of other timber samples based on their measured
density.

So, wood density (lbs/ft3) is our predictor (or explanatory variable)
and timber hardness is the desired output (or response). Timber hardness
has been quantified using the Janka (force in lbs) scale. The Janka data
can be obtained from the SemiPar package.

``` r
# load packages
library(SemiPar) # data
library(ggplot2) # graphics
library(MASS) # Box-Cox
library(arm) # display() etc.
library(grid) # grid graphics
```

``` r
data(janka) # load data
names(janka) <- c("Density", "Hardness") # assign full names
head(janka) # print data to take a look
```

    ##   Density Hardness
    ## 1    24.7      484
    ## 2    24.8      427
    ## 3    27.3      413
    ## 4    28.4      517
    ## 5    28.4      549
    ## 6    29.0      648

If our aim is to predict hardness based on density, we can use the lm()
function:

``` r
## Normal least squares linear regression using lm() :
janka.ls1 <- lm( Hardness~Density, data= janka)
janka.ls1
```

    ## 
    ## Call:
    ## lm(formula = Hardness ~ Density, data = janka)
    ## 
    ## Coefficients:
    ## (Intercept)      Density  
    ##    -1160.50        57.51

We can produce a graph of this analysis using the qplot() function and
adding a “smooth” argument that will plot a curve with a 95% CI. We can
also create some R object to define the x-axis labels. Putting the outer
pair of parentheses around the qplot() code is a way to get the output
displayed in the screen.

``` r
# Create scatterplot with smoothing curve:
ggplot(janka, aes(x = Density, y = Hardness)) +
geom_point(size = 3) +
xlab("Wood Density lbs/cubic foot") +
ylab("Hardness lbf") +
geom_smooth(method="lm") +
theme_bw()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](britton_a_assignment8_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

This graph gets us a simple normal least squares linear regression and
CI for the Janka data set. The interval reflects our confidence in the
overall regression lines. The line of best fit minimizes the sum of the
squared differences between the data points and the line.

The linear regression can be written in the typical y = a + bx form
where x is the explanatory variable and y is the predicted variable.
Remember that there is also some unexplained residual variation that is
not included in the formula.

``` r
# to predict hardness from density...
# y = a + bx, or equivalently, a*1 + b*x
# where a = intercept and b = slope, extracted:
coef(janka.ls1)
```

    ## (Intercept)     Density 
    ## -1160.49970    57.50667

``` r
# model matrix holds the column of ones and the density values, x:
head(model.matrix(janka.ls1))
```

    ##   (Intercept) Density
    ## 1           1    24.7
    ## 2           1    24.8
    ## 3           1    27.3
    ## 4           1    28.4
    ## 5           1    28.4
    ## 6           1    29.0

We can also use the regression equation to predict a value of hardness.
For example, we could use the density value of 24.7. We can wrap the
waiting parameter value inside a new data frame named “newdata.” Then we
apply the predict function to our model along with newdata.

``` r
#Estimating using predict
newdata<- data.frame(Density=24.7)
predict(janka.ls1,newdata)
```

    ##        1 
    ## 259.9152

So according to the model, the intercept is 259.9 (R output) so we would
predict a hardness of around 260 lbf for a wood density of 24.7 lbs/ft3.

Finally, the next step of a regression analysis involves using the
fitted model to assess statistical significance. We usually want to
determine whether the slope is significantly different from zero. That
is, we want to know if the relationship between the x and y variables is
likely to be real or just the result of sampling variation. Carrying out
the required F test is actually very easy. The test relies on a function
called anova. To use this function, all we have to do is pass it one
argument: the name of the fitted regression model object…

``` r
#Performing F test
anova(janka.ls1)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Hardness
    ##           Df   Sum Sq  Mean Sq F value    Pr(>F)    
    ## Density    1 21345674 21345674  636.98 < 2.2e-16 ***
    ## Residuals 34  1139366    33511                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The F-statistic (variance ratio) is the key term. When working with a
regression model, this quantifies how much variability in the data is
explained when we include the best fit slope term in the model.

Larger values indicate a stronger relationship between x and y. The
p-value gives the probability that the relationship could have arisen
through sampling variation, if in fact there were no real association.
As always, a p-value of less than 0.05 is taken as evidence that the
relationship is real, i.e. the result is statistically significant.

There is a second function, called summary, that can be used to extract
a little more information from the fitted regression model:

``` r
#Extracting R2 and other model details
summary(janka.ls1)
```

    ## 
    ## Call:
    ## lm(formula = Hardness ~ Density, data = janka)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -338.40  -96.98  -15.71   92.71  625.06 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1160.500    108.580  -10.69 2.07e-12 ***
    ## Density        57.507      2.279   25.24  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 183.1 on 34 degrees of freedom
    ## Multiple R-squared:  0.9493, Adjusted R-squared:  0.9478 
    ## F-statistic:   637 on 1 and 34 DF,  p-value: < 2.2e-16

The only other part of the output from summary that is of interest now
is the line containing the Multiple R-squared value:

**Multiple R-squared: 0.9493, Adjusted R-squared: 0.9478**

This shows the R-squared (R2) of our model. It tells you what proportion
(sometimes expressed as a percentage) of the variation in the data is
explained, or accounted for, by the fitted line. If R2=1 the line passes
through all the points on the graph (all the variation is accounted for)
and if R2≈0% the line explains little or none of the variation in the
data. The R2 value here is 0.95. This is very very respectable, but
still indicates that there are other sources of variation (differences
between wood, inaccuracies in the assay technique, etc.) which remain
unexplained by the line.
