britton_a\_assignment11
================
Annie Britton
4/10/2023

### **PART I: Follow along Transformation Activity (5 points)**

**Data Transformation: ANOVAs and t-tests (Taken liberally from D.
Childs (2017))**

Red wood ants, Formica rufa, forage for food (mainly insects and
‘honeydew’ produced by aphids) both on the ground and in the canopies of
trees. Rowan, oak and sycamore support very different communities of
insect herbivores (including aphids) and it would be interesting to know
whether the foraging efficiency of ant colonies is affected by the type
of trees available to them. As part of an investigation of the foraging
of Formica rufa observations were made of the prey being carried by ants
down trunks of rowan, oak and sycamore trees.

The total biomass of prey being transported was measured over a 30
minute sampling period and the data were expressed as the biomass (dry
weight in mg) of prey divided by the total number of ants leaving the
tree to give the rate of food collection per ant per half hour.
Observations were made on 28 rowan, 26 sycamore, and 27 oak trees.

``` r
# load packages
library(tidyverse)
library(car)
```

``` r
# Set working directory and load the data
setwd("C:\\Users\\annie\\Documents\\AEEN\\M11")
ants <- read.csv("ANTS1.CSV")

# Rename the Food column
colnames(ants)[1] <- "Food"

# Check the data
glimpse(ants)
```

    ## Rows: 81
    ## Columns: 2
    ## $ Food <dbl> 11.9, 33.3, 4.6, 5.5, 6.2, 11.0, 24.3, 20.7, 5.7, 12.6, 10.2, 4.7~
    ## $ Tree <chr> "Rowan", "Rowan", "Rowan", "Rowan", "Rowan", "Rowan", "Rowan", "R~

``` r
# Box and Whiskers
ggplot(ants, aes(x = Tree, y = Food)) +
geom_boxplot()
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The food collection rate is generally highest in Oaks and lowest in
Rowans (Sycamores are in between). Notice too that the sample
distribution of food collection rate is right-skewed. The test we are
most likely to want to use with these data is an ANOVA, i.e. we want to
assess whether the mean food collection rates are different among the
three tree species. Already we have an indication that an ANOVA with the
raw food values may be problematic.

**Teaching Example Checking Assumptions:**

The test we are most likely to want to use with these data is an ANOVA,
so the following assumptions must be evaluated: 1. Independence. The
experimental units of the data must be independent. 2. Measurement
scale. The dependent variable is measured on an interval or ratio scale.
3. Normality. The residuals are normally distributed in each level of
the grouping factor. 4. Equal variance. The variance in each level of
the grouping factor is the same.

We’ll have to assume the first assumption is satisfied and the food
collection rate (second assumption) is obviously measured on a ratio
scale. The distributional assumptions (normality and equality of
variance) are the ones we can address with a transformation. Let’s fit
the ANOVA model and produce regression diagnostics to evaluate
these—remember, we make these kinds of plots after we have fitted a
statistical model:

``` r
# regression diagnostics
ant_mod <- lm(Food ~ Tree, data = ants)

# produce a ‘normal probability plot’ to assess the normality assumption:
plot(ant_mod, which = 2)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

This plot exhibits the accelerating curvature that is indicative of
right-skewed residuals. This probably isn’t just sampling variation
because there is a systematic departure from the dashed line everywhere
along it. So… it looks like there is a problem. This sort of pattern is
quite common in biological data, especially when it involves counts.
Clearly we might be a bit worried about using an ANOVA with these data
since it assumes the residuals to be at least approximately normally
distributed.

Are the variances significantly different? Look at the box plots above.
The data from the three samples seem to have rather different scatter.
The sample from the rowan has less variation than that from the
sycamore, and the sycamore has less variation than the oak. Does the
scale-location plot tell the same story?

``` r
#residuals plot
plot(ant_mod, which = 3)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

This shows that the variance increases with the fitted values—it looks
like there is also a problem with the constant variance assumption.
Again, this pattern is very common in biological data. Let’s transform
the data using a log function.

**Transformations in General:**

Data transformations are an important tool for the proper statistical
analysis of biological data. To those with a limited knowledge of
statistics, however, they may seem a bit fishy, a form of playing around
with your data in order to get the answer you want. It is therefore
essential that you be able to defend your use of data transformations.

There are an infinite number of transformations you could use, but it is
better to use a transformation that other researchers commonly use in
your field, such as the square-root transformation for count data or the
log transformation for size data. Even if an obscure transformation that
not many people have heard of gives you slightly more normal or more
homoscedastic data, it will probably be better to use a more common
transformation so people don’t get suspicious.

Remember that your data don’t have to be perfectly normal and
homoscedastic; parametric tests aren’t extremely sensitive to deviations
from their assumptions.

Applying a log transform is quick and easy in R:

``` r
# log transformation
ants <- mutate(ants, logFood = log10(Food))

# store the transformed variable in a new column called logFood.
ant_mod_log <- lm(logFood ~ Tree, data = ants)

# normal probability plot’ to assess the normality assumption
plot(ant_mod_log, which = 2)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The assumptions are closer to being satisfied. Let’s carry out ANOVA
using the model with the transformed food variable.

``` r
# ANOVA
anova(ant_mod_log)
```

    ## Analysis of Variance Table
    ## 
    ## Response: logFood
    ##           Df Sum Sq Mean Sq F value   Pr(>F)   
    ## Tree       2 1.4106 0.70530  7.2867 0.001255 **
    ## Residuals 78 7.5498 0.09679                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We see evidence for a significant effect of tree (p\<0.01) with the
transformed data!

**Reporting Transformed Results (Taken from D. Childs (2017))**

Once we transform data, how should we present the results in a report?
There are three alternatives.

• We could present the transformed means (having stated what the
transformation was, e.g. log(x+1)). The disadvantage to this is that the
numbers themselves convey little information about the data values on
the original scale. This is not always a problem. For example, effects
given on a log scale act in a ‘multiplicative’ manner, so a model with
log-transformed dependent variable can still be interpreted if we know
what we’re doing.

• We could back-transform the means of the log-transformed data by
taking the antilogs: 10x (for logs to the base 10) and ex (for natural
logs). When we back-transform data, however, we need to be aware of two
things:

1.  The back-transformed mean will not be the same as mean calculated
    from the original data;

2.  We have to be careful when we back-transform standard errors. If we
    want to display the back-transformed means on a bar plot, with some
    indication of the variability of the data, we must calculate the
    standard errors and then back transform the upper and lower limits,
    which will not then be symmetrical about the mean.

• We could also present the means calculated from the original data but
state clearly that the statistical analysis was carried out on
transformed data. This is often the simplest way to proceed.

### **PART II: Shapiro-Wilk (15 points)**

**Normality, Variance, and Transforms:**

Shapiro-Wilk’s method is widely recommended for normality testing and is
based on the correlation between the data and the corresponding normal
scores.

The R function shapiro.test() can be used to perform the Shapiro-Wilk
test of normality for one variable. If the p-value is large, then the
distribution of the data are not significantly different from a normal
distribution. In other words, we can assume the normality.

• If p ≤ 0.05: then the null hypothesis can be rejected (i.e. the
variable is NOT normally distributed).

• If p \> 0.05: then the null hypothesis cannot be rejected (i.e. the
variable MAY BE normally distributed).

#### **1. Use the speciespH.txt data set available on Canvas, which represents the count of the number of species and their collective biomass in three pH treatments.**

``` r
# Load in the txt data using read.table(), specifying headers
speciespH <- read.table("speciespH.txt", header = TRUE)

# Check the data
glimpse(speciespH)
```

    ## Rows: 90
    ## Columns: 3
    ## $ pH      <chr> "high", "high", "high", "high", "high", "high", "high", "high"~
    ## $ Biomass <dbl> 0.46929722, 1.73087043, 2.08977848, 3.92578714, 4.36679265, 5.~
    ## $ Species <int> 30, 39, 44, 35, 25, 29, 23, 18, 19, 12, 39, 35, 30, 30, 33, 20~

#### **2. Make histograms or box and whisker and normality plots for the variable Species for each of Low, Mid, and High pH treatments in this data set. Do you think each data set looks normally distributed?**

``` r
# Filter the data by pH treatment level
highpH <- speciespH %>%  filter(pH == "high")
midpH <- speciespH %>%  filter(pH == "mid")
lowpH <- speciespH %>%  filter(pH == "low")

# Plot histograms for each new dataframe
hist(highpH$Species)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
hist(midpH$Species)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
hist(lowpH$Species)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

Based on the historgrams above, the data for the high and mid pH
treatments look like they are likely normally distributed, while the low
pH treatment data looks like it might be nonnormal.

#### **3. Conduct Shapiro-Wilk tests on those same data – How do these results compare to your visual interpretations from Question #2 above?**

``` r
# Define the datasets to be tested
datasets <- list(highpH = highpH$Species, midpH = midpH$Species, lowpH = lowpH$Species)

# Loop through the datasets and perform Shapiro-Wilk normality test
for (key in names(datasets)) {
  result <- shapiro.test(datasets[[key]])
  if (result$p.value > 0.05) {
    cat(paste("Data in the", key, "set is normally distributed: p-value =", round(result$p.value, 4), "\n"))
  } else {
    cat(paste("Data in the", key, "set is not normally distributed: p-value =", round(result$p.value, 4), "\n"))
  }
}
```

    ## Data in the highpH set is normally distributed: p-value = 0.5274 
    ## Data in the midpH set is normally distributed: p-value = 0.6353 
    ## Data in the lowpH set is normally distributed: p-value = 0.0796

The shapiro.tests() above report p-values \> 0.05 for all three pH
treatments, which means that we cannot reject the null hypothesis - the
data are likely to be normally distributed. However, for the lowpH
level, the p-value is 0.07964, which is marginally close to 0.05. This
suggests that there may be a slight departure from normality for the
Species variable at the low pH level, but the evidence is not strong
enough to reject the null hypothesis of normality at the significance
level of 0.05.

#### **4. Try a transformation – explain your choice of a transform and redo the Shapiro-Wilk test, and then explain in plain English how it changed results (if it did), and make it clear what information you used to support your answer.**

Since the low pH treatment data have the lowest p-value at 0.07964, I
will choose these data to try to transform.

First, I tried using a box-cox transform, since the lowpH data’s
histogram goes up and down. For data that has a histogram suggesting a
pattern of oscillation, the Box-Cox transformation is a widely used
method for stabilizing the variance and/or normalizing the distribution
of data.

The Box-Cox transformation involves raising the data to a power, where
the power parameter is estimated from the data itself. The
transformation can handle a range of power values, allowing for
flexibility in accommodating different types of data distributions.

**Source:** National Institute of Standards of Technology. (n.d.).
Box-Cox Normality Plot. Engineering Statistics Handbook.
<https://www.itl.nist.gov/div898/handbook/eda/section3/eda336.htm>

``` r
# Estimate optimal lambda using powerTransform (an estimate of Box Cox)
result <- powerTransform(lowpH$Species)

# Extract the estimated lambda value
lambda <- result$lambda

# Apply Box-Cox transformation using the estimated lambda value
hist(lowpH$Species^lambda)
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Run another Shapiro-Wilk test
shapiro.test(lowpH$Species^lambda)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  lowpH$Species^lambda
    ## W = 0.93427, p-value = 0.06381

Unfortunately, the Box-Cox transformation did not result in a higher
p-value (indicating the likelihood of normality) per the Shapiro-Wilk
test. Let’s try a log transformation:

``` r
# Histogram of natural log-transformed low pH data
hist(1/(lowpH$Species))
```

![](britton_a_assignment11_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Shapiro-Wilk test of natural log-transformed low pH data
shapiro.test(1/(lowpH$Species))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  1/(lowpH$Species)
    ## W = 0.76784, p-value = 1.793e-05

Similarly, using a log function did not result in a higher p-value for
the data. However, since the non-transformed low pH treatment data has a
p-value of 0.07964 based on the Shapiro-Wilk test, it is likely to be
normal, so no further transformation is truly needed.
