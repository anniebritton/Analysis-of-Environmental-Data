britton_a\_assignment10
================
Annie Britton
4/3/2023

## **PART I: ANCOVA Worked Example (5 points)**

A ‘classic’ ANOVA tests for differences in mean responses to categorical
factor (treatment) levels. When a continuous covariate is included in an
ANOVA we have the analysis of covariance (ANCOVA). We use ANCOVA when we
want to compare two or more regression lines to each other; ANCOVA will
tell you whether the regression lines are different from each other in
either slope or intercept. It is a way of comparing the Y variable among
groups while statistically controlling for variation in Y caused by
variation in the X variable.

ANCOVA (in experiments) refers to a design with one factor and one
continuous explanatory variable. But it is also used to describe
analysis in which we need to adjust for the effects of uncontrolled
variables on the design variable. For example, perhaps we are looking at
the effect of some contaminant on the growth of a species. But what if
the initial size of the species is different in each sample? Before
assessing the growth, differences based on the treatment (contaminant)
we would need to adjust for the initial size of the organism and that
can be done with an ANCOVA.

We are going to look at data that is contained in a R package called
Sleuth2 and originated by a study in the Journal of Environmental
Quality. Heggestad and Lesso (1990) did an experimental study of the
effects of low-level atmospheric pollutants and drought on agricultural
yields. The response variable is the yield of soya beans (varieties
William and Forrest – we will focus only on the Forest variety for
simplicity). The soya bean plants were grown under well-watered control
conditions or under drought stress (a two-level explanatory factor). The
beans were also exposed to controlled gradients of two atmospheric
pollutants: low level ozone (with five levels) and Sulphur dioxide (with
three levels), which we will treat as continuous explanatory variables.

#### **First Steps:**

The str function is a super useful function in R. It provides great
information about the structure of some object. Specifically, it will
return a data.frame with four columns: variable, class, levels, and
examples.

``` r
# Set up
rm(list=ls(all=TRUE)) # Clear R workspace

# Loading all library packages used: 
library(arm)
library(ggplot2)
library(Sleuth2)
library(tidyverse)

# Turn off significance stars
options(show.signif.stars = FALSE) 

# DATA
library(Sleuth2)
str(case1402) # check the structure of the data
```

    ## 'data.frame':    30 obs. of  5 variables:
    ##  $ Stress : Factor w/ 2 levels "Well-watered",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SO2    : num  0.0045 0.0045 0.0045 0.0045 0.0045 ...
    ##  $ O3     : num  0.017 0.049 0.067 0.084 0.099 ...
    ##  $ Forrest: num  4376 4544 2806 3339 3320 ...
    ##  $ William: num  5561 5947 4273 3470 3080 ...

``` r
case1402 # print the dataset
```

    ##          Stress    SO2    O3 Forrest William
    ## 1  Well-watered 0.0045 0.017    4376    5561
    ## 2  Well-watered 0.0045 0.049    4544    5947
    ## 3  Well-watered 0.0045 0.067    2806    4273
    ## 4  Well-watered 0.0045 0.084    3339    3470
    ## 5  Well-watered 0.0045 0.099    3320    3080
    ## 6  Well-watered 0.0170 0.017    3747    5092
    ## 7  Well-watered 0.0170 0.049    4570    4752
    ## 8  Well-watered 0.0170 0.067    4635    4232
    ## 9  Well-watered 0.0170 0.084    3613    2867
    ## 10 Well-watered 0.0170 0.099    3259    3106
    ## 11 Well-watered 0.0590 0.017    4179    4736
    ## 12 Well-watered 0.0590 0.049    5077    3672
    ## 13 Well-watered 0.0590 0.067    3401    3386
    ## 14 Well-watered 0.0590 0.084    3371    2854
    ## 15 Well-watered 0.0590 0.099    2158    2557
    ## 16     Stressed 0.0045 0.017    4977    4520
    ## 17     Stressed 0.0045 0.049    3780    3047
    ## 18     Stressed 0.0045 0.067    3804    3526
    ## 19     Stressed 0.0045 0.084    3941    3357
    ## 20     Stressed 0.0045 0.099    2863    2663
    ## 21     Stressed 0.0170 0.017    5573    4869
    ## 22     Stressed 0.0170 0.049    3555    3774
    ## 23     Stressed 0.0170 0.067    3340    2955
    ## 24     Stressed 0.0170 0.084    3243    3513
    ## 25     Stressed 0.0170 0.099    2802    2838
    ## 26     Stressed 0.0590 0.017    4589    4056
    ## 27     Stressed 0.0590 0.049    3250    2758
    ## 28     Stressed 0.0590 0.067    3045    3094
    ## 29     Stressed 0.0590 0.084    2827    2398
    ## 30     Stressed 0.0590 0.099    2979    2101

Note the data frame resulting from the str() command. You should have 30
observations and 5 variables. The units for yield are kg/ha and µl/l for
both SO2 and O3.

#### **Log Transformed Panels:**

Now, we might expect that both water stress and ozone pollution would
decrease yields. It also seems feasible that a plant exposed to either
of these stresses might be more vulnerable to the effect of the other
(synergy). If we do find evidence for a synergistic interaction, there
is a limit to how far yields can fall (cannot go negative) and this
might be used as a constraint. The easiest way to visualize ANCOVA is as
separate regressions with a different panel for each level of the
factor. Note that since the axis labels are cumbersome, we have used
some R code to substitute as an object into the quickplot function of
ggplot2. This has the advantage of once being created, it can be
modified and used over and over.

``` r
# Panel plot of Williams yield:
xlabel <- expression(paste("Ozone (", mu ,"L L"^"-1",")" ))
ylabel <- expression(paste("Log Yield (kg ha"^"-1",") "))
qplot(O3, log(William), data= case1402, facets=.~Stress, 
      geom= c("point", "smooth"), method= "lm",
      xlab= xlabel, ylab= ylabel, main= "Soya bean variety: 'William'") +theme_bw()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](britton_a_assignment10_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The most striking thing about this figure is the decline in yield with
increasing ozone. The intercepts and the slopes might be difference, but
the differences appear small. Let’s create a model (similar to ANOVA and
is a general linear model) to look at these interactions.

``` r
# ANCOVA with interaction term
w1 <- lm(log(William)~O3*Stress, data= case1402)

# Note that in this field it is customary to analyze yields after a log transformation. We need to keep this in mind during the interpretation as multiplicative effects become additive on a log scale.

# Coefficients for model with interaction:
display(w1)
```

    ## lm(formula = log(William) ~ O3 * Stress, data = case1402)
    ##                   coef.est coef.se
    ## (Intercept)        8.73     0.09  
    ## O3                -7.59     1.32  
    ## StressStressed    -0.27     0.13  
    ## O3:StressStressed  1.45     1.87  
    ## ---
    ## n = 30, k = 4
    ## residual sd = 0.15, R-Squared = 0.72

``` r
confint(w1)
```

    ##                        2.5 %       97.5 %
    ## (Intercept)         8.545282  8.922654891
    ## O3                -10.310533 -4.867704817
    ## StressStressed     -0.536306 -0.002620153
    ## O3:StressStressed  -2.401533  5.295787969

Now for interpreting the results. The coef.est is an estimation of
regression (lm) derived intercept and slope (first and second rows).
Look at your panel graphs to confirm that this makes sense. The slope is
negative and the intercept is the yield for zero ozone. The third row
gives the change in intercept or the reduction in yield due to water
stress. The final row gives the difference in slope for the
water-stressed treatment. Given the level of noise (coef.se) quantified
by the standard error, there is no sign of an interactive effect. We can
also look at the confidence intervals and notice that the CI for the
interaction includes zero so the analysis provides no support for
interactions or non-additivity.

#### **Examining water stress and sulphur dioxide:**

``` r
# ANCOVA of William yield as a function of water stress and SO2
# Summary table t tests of coefficients
summary(lm(log(William)~SO2*Stress, data= case1402))
```

    ## 
    ## Call:
    ## lm(formula = log(William) ~ SO2 * Stress, data = case1402)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3361 -0.1681  0.0069  0.1573  0.3763 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)         8.37111    0.09272  90.287   <2e-16
    ## SO2                -4.35168    2.60846  -1.668    0.107
    ## StressStressed     -0.19104    0.13112  -1.457    0.157
    ## SO2:StressStressed  0.48575    3.68892   0.132    0.896
    ## 
    ## Residual standard error: 0.2355 on 26 degrees of freedom
    ## Multiple R-squared:  0.2627, Adjusted R-squared:  0.1776 
    ## F-statistic: 3.088 on 3 and 26 DF,  p-value: 0.04457

``` r
# ANOVA table F tests
anova(lm(log(William)~SO2*Stress, data= case1402 ))
```

    ## Analysis of Variance Table
    ## 
    ## Response: log(William)
    ##            Df  Sum Sq  Mean Sq F value  Pr(>F)
    ## SO2         1 0.27521 0.275210  4.9624 0.03477
    ## Stress      1 0.23764 0.237642  4.2850 0.04852
    ## SO2:Stress  1 0.00096 0.000962  0.0173 0.89625
    ## Residuals  26 1.44193 0.055459

Looking at the bottom of the table where the interactions are we see no
indication of an interaction. The ANOVA table confirms the lack of any
indication for an interaction but the F-tests for the main effects of
water stress and Sulphur dioxide are now significant according to the p
\< 0.005 convention.

#### **ANCOVA as Generalized Linear Model:**

It is useful to think of ANCOVA as a simple example of a general linear
model which is why we used the general lm() function. We will finish up
this example by looking at an analysis that combines water stress, ozone
and Sulphur dioxide. It at least seems reasonable that all three
stresses could interact to reduce yield.

``` r
# General Linear Model for O3, SO2 & Stress including interactions:
summary(lm(log(William)~O3*SO2*Stress, data= case1402))
```

    ## 
    ## Call:
    ## lm(formula = log(William) ~ O3 * SO2 * Stress, data = case1402)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21510 -0.04035  0.01051  0.04181  0.22894 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)             8.8637     0.1057  83.840  < 2e-16
    ## O3                     -7.7935     1.5248  -5.111 4.03e-05
    ## SO2                    -4.8331     2.9743  -1.625   0.1184
    ## StressStressed         -0.3643     0.1495  -2.436   0.0234
    ## O3:SO2                  7.6169    42.8983   0.178   0.8607
    ## O3:StressStressed       2.7407     2.1564   1.271   0.2170
    ## SO2:StressStressed      3.5325     4.2063   0.840   0.4101
    ## O3:SO2:StressStressed -48.2079    60.6674  -0.795   0.4353
    ## 
    ## Residual standard error: 0.1104 on 22 degrees of freedom
    ## Multiple R-squared:  0.8628, Adjusted R-squared:  0.8192 
    ## F-statistic: 19.77 on 7 and 22 DF,  p-value: 4.026e-08

``` r
anova(lm(log(William)~O3*SO2*Stress, data= case1402))
```

    ## Analysis of Variance Table
    ## 
    ## Response: log(William)
    ##               Df  Sum Sq Mean Sq F value    Pr(>F)
    ## O3             1 1.14959 1.14959 94.2735 2.059e-09
    ## SO2            1 0.27521 0.27521 22.5690 9.652e-05
    ## Stress         1 0.23764 0.23764 19.4882 0.0002192
    ## O3:SO2         1 0.00360 0.00360  0.2954 0.5922353
    ## O3:Stress      1 0.01277 0.01277  1.0471 0.3172948
    ## SO2:Stress     1 0.00096 0.00096  0.0789 0.7814768
    ## O3:SO2:Stress  1 0.00770 0.00770  0.6314 0.4353199
    ## Residuals     22 0.26827 0.01219

Once again, look at the bottom of the linear model table where we see
there is no indication of a three-way interaction. The ANOVA table also
indicates no interaction but does give a strong indication of an effect
of Sulphur dioxide alongside ozone and water stress.

#### **Summary:**

So, we have seen in this lecture/example how ANCOVA of designed
experiments combines one categorical variable and one continuous
explanatory variable. Thanks to the concept of general linear models
this mix of regression and ANOVA can be performed with the lm()
function. The interpretation of the ANOVA and the summary tables gets
tricky and the equivalence of F-tests and t-tests in these tables
sometimes breaks down. When the results agree everything is fine and
dandy, but when they differ you may need to present multiple models and
explain why they differ. It is all about process and biology – don’t
ever get hung up on the stats themselves.

#### **Metadata:**

``` r
# Source: Heggestad, H.E. and Lesser, V.M. (1990) Journal of Environmental Quality 19, p.488.
# R package: Sleuth2
# Dataframe: case1402; 30 obs. of 5 variables
# Stress: Explanatory factor with 2 levels - "Well-watered", "Stressed"
# SO2: Explanatory variable - Sulphur dioxide concentration in microliters per liter.
# O3: Explanatory variable - ozone concentration in microliters in microliters per liter.
# Forrest: Response - yield of Forrest variety soja beans in Kg per ha.
# William: Response - yield of William variety soja beans in Kg per ha.
```

## **PART II: Ipomopsis aggregata -Take two with ANCOVA (15 points)**

At first glance, herbivory may be expected to reduce success of plants.
But some plants, like *Ipomopsis aggregata* actually increase in
abundance when grazed. How does this happen?

Hypotheses include:

1.  “Grazers” avoid Ipomopsis and prefer other plants (competitive
    release)

2.  “Grazers” eat Ipomopsis and other plants to reduce fitness of all,
    but other plants recover slower

3.  Relative fitness of grazed Ipomopsis is actually increased relative
    to ungrazed Ipomopsis, for example by increasing fruit production as
    a response (overcompensation).

An experiment (Bergelson & Crawley. 1992. American Naturalist
139:870-882) was conducted on 14 natural populations in 5 states of the
NW USA to test Hypothesis 3, which has been debated because it seems
counterintuitive. Plants were first measured (including rootstock
diameter, because bigger plants can make more fruits), and then growing
tips of plants were either left alone (Ungrazed) or clipped with
scissors (Grazed). Choice of treatment was random among plants in a
population.

#### **Import and attach the Ipomopsis data set.**

``` r
# set the file path
path <- "C:\\Users\\annie\\Documents\\AEEN\\M10\\ipomopsis.txt"

# import the txt data using read_table
ipo_data <- read_table(path)
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Root = col_double(),
    ##   Fruit = col_double(),
    ##   Grazing = col_character()
    ## )

``` r
# change the Grazing column from strings to factors
ipo_data$Grazing <- as.factor(ipo_data$Grazing)

# check the data
ipo_data %>% glimpse()
```

    ## Rows: 40
    ## Columns: 3
    ## $ Root    <dbl> 6.225, 6.487, 4.919, 5.130, 5.417, 5.359, 7.614, 6.352, 4.975,~
    ## $ Fruit   <dbl> 59.77, 60.98, 14.73, 19.28, 34.25, 35.53, 87.73, 63.21, 24.25,~
    ## $ Grazing <fct> Ungrazed, Ungrazed, Ungrazed, Ungrazed, Ungrazed, Ungrazed, Un~

#### **Plot the fruit mass versus root size color coding the plotted points according to their grazing status.**

``` r
# plot with fruit mass versus root size, colored be grazing status
ggplot(ipo_data, aes(x = Root, y = Fruit, color = Grazing)) +
  geom_point() +
  scale_x_continuous(breaks = seq(4, 11, by = 1)) + # adjust ticks on x axis
  scale_y_continuous(breaks = seq(0, 120, by = 20)) + # adjust ticks on y axis
  labs(x = "Root Size", y = "Fruit Mass", color = "Grazing Status") # label axes
```

![](britton_a_assignment10_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Observe that the two treatment groups are completely segregated in the
plot suggesting that there is a treatment effect, although not as simple
as an ordinary mean difference between the groups. Furthermore, it
appears that fruit mass and root size are linearly related.

#### **Compute a simple one-way ANOVA (call it model1), where Fruit is the response variable and Grazing is the treatment. Interpret the results.**

``` r
model1 <- aov(Fruit ~ Grazing, data = ipo_data)
summary(model1)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## Grazing      1   2910  2910.4   5.309 0.0268
    ## Residuals   38  20833   548.2

The results show that grazing status has a significant effect (p =
0.0268) on fruit mass. This means that there is a statistical difference
in fruit size between the grazed and ungrazed groups.

#### **Consider your graph above.**

Does this look like two different regressions? And do the two
regressions look about parallel? That is a requirement for ANCOVA; the
quantitative variable Root covaries with the results and needs to be
included in the model, and its effect is consistent (equal slopes) in
both models. Which comes first in the model? Fruit? Root? Grazing?

#### **Remember the order that variables are listed matters for ANCOVA!**

We list the covariate first, categorical treatment second. Think of it
this way: we compute an ANOVA after removing the signal due to the
quantitative “covariate”. To test this problem, try a model as Fruit \~
Root + Grazing and Fruit \~ Grazing + Root. Do you get different
answers?

``` r
# ANCOVA using Fruit ~ Root + Grazing
model2 <- lm(Fruit ~ Root + Grazing, data = ipo_data)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = Fruit ~ Root + Grazing, data = ipo_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -17.1920  -2.8224   0.3223   3.9144  17.3290 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)     -127.829      9.664  -13.23 1.35e-15
    ## Root              23.560      1.149   20.51  < 2e-16
    ## GrazingUngrazed   36.103      3.357   10.75 6.11e-13
    ## 
    ## Residual standard error: 6.747 on 37 degrees of freedom
    ## Multiple R-squared:  0.9291, Adjusted R-squared:  0.9252 
    ## F-statistic: 242.3 on 2 and 37 DF,  p-value: < 2.2e-16

``` r
# ANCOVA using Fruit ~ Grazing + Root
model3 <- lm(Fruit ~ Grazing + Root, data = ipo_data)
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = Fruit ~ Grazing + Root, data = ipo_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -17.1920  -2.8224   0.3223   3.9144  17.3290 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)     -127.829      9.664  -13.23 1.35e-15
    ## GrazingUngrazed   36.103      3.357   10.75 6.11e-13
    ## Root              23.560      1.149   20.51  < 2e-16
    ## 
    ## Residual standard error: 6.747 on 37 degrees of freedom
    ## Multiple R-squared:  0.9291, Adjusted R-squared:  0.9252 
    ## F-statistic: 242.3 on 2 and 37 DF,  p-value: < 2.2e-16

Though the orders of the independent variables in the two ANCOVA models
above are different, the models’ results are the same. This is because
the order of the independent variables in a linear regression/ANCOVA
model doesn’t matter, while the order of the covariate versus the
treatments (independent variables) does.
