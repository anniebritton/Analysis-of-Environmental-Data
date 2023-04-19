britton_a\_assignment12
================
Annie Britton
4/17/2023

## **The Assignment on Non-Parametric Tests (20 points)**

For this assignment, you need to create your own research question to
conduct a hypothesis test. The best way to do this is to keep exploring
different datasets. So, you will need to find your own data using R
datasets. The key to this assignment is that you will use a
non-parametric test to evaluate your data.

Here are some websites with R datasets. Check out the following
websites: <https://vincentarelbundock.github.io/Rdatasets/>

The expected outcome of this assignment is a short two to three-page
write-up (not including figures) of the process you followed in and
findings from your analysis. Your submittal will be an R Markdown file
including your R functions, analysis, and findings. Also please attach
to your report the .csv data file.

------------------------------------------------------------------------

### **Pick a dataset that is interesting to you (must be environmental in nature so not the mpg dataset).**

``` r
# import libraries
library(tidyverse) # for data cleaning
library(quantreg) # for the "Mammals" dataset that we will use
```

The Mammals dataset provides observations on the maximal running speed
of mammal species and their body mass. The dataset comes from Garland,
T. (1983) The relation between maximal running speed and body mass in
terrestrial mammals, J. Zoology, 199, 1557-1570.

``` r
# Take a look at our dataset
data(Mammals)
Mammals %>% glimpse()
```

    ## Rows: 107
    ## Columns: 4
    ## $ weight   <dbl> 6000, 4000, 3000, 1400, 400, 350, 300, 260, 250, 3800, 1000, ~
    ## $ speed    <dbl> 35, 26, 25, 45, 70, 70, 64, 70, 40, 25, 60, 70, 56, 29, 57, 3~
    ## $ hoppers  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE~
    ## $ specials <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE~

We will use this dataset to examine the relationship between body mass
in Kg for “typical adult sizes” and the maximal running speed (fastest
sprint velocity on record). There is also a column indicating if the
species hops in order to move (kangaroos), and a column indicating
special animals with “lifestyles in which speed does not figure as an
important factor”: Hippopotamus, raccoon (Procyon), badger (Meles),
coati (Nasua), skunk (Mephitis), man (Homo), porcupine (Erithizon),
oppossum (didelphis), and sloth (Bradypus) (Garland, 1983).

------------------------------------------------------------------------

### **Write a biological and statistical hypothesis (null AND alternate).**

**Biological Hypothesis:**

Null Hypothesis: There is no relationship between body mass and maximal
running speed in mammals. Body mass does not significantly affect the
maximal running speed in mammals.

Alternate Hypothesis: There is a relationship between body mass and
maximal running speed in mammals - as body mass increases, maximal
running speed also increases in mammals.

**Statistical Hypothesis:**

Null Hypothesis: The slope of the regression line between body mass and
maximal running speed in mammals is zero. There is no significant linear
relationship between body mass and maximal running speed in mammals.

Alternate Hypothesis: The slope of the regression line between body mass
and maximal running speed in mammals is not equal to zero. There is a
significant linear relationship between body mass and maximal running
speed in mammals.

### **The first set of analyses you will do with your data is descriptive statistics (e.g. mean, standard deviation, summary, etc.).**

``` r
# Show the summary statistics for the dataset
Mammals %>% summary()
```

    ##      weight             speed         hoppers         specials      
    ##  Min.   :   0.016   Min.   :  1.60   Mode :logical   Mode :logical  
    ##  1st Qu.:   1.700   1st Qu.: 22.50   FALSE:96        FALSE:97       
    ##  Median :  34.000   Median : 48.00   TRUE :11        TRUE :10       
    ##  Mean   : 278.688   Mean   : 46.21                                  
    ##  3rd Qu.: 142.500   3rd Qu.: 65.00                                  
    ##  Max.   :6000.000   Max.   :110.00

``` r
# Calculate standard deviation of body mass and maximal running speed
sd_weight <- sd(Mammals$weight)
sd_speed <- sd(Mammals$speed)

# Print the calculated statistics
cat("Standard Deviation of Body Mass: ", sd_weight, "Kg\n")
```

    ## Standard Deviation of Body Mass:  839.6083 Kg

``` r
cat("Standard Deviation of Maximal Running Speed: ", sd_speed, "m/s\n")
```

    ## Standard Deviation of Maximal Running Speed:  26.71678 m/s

The results of the descriptive statistics are:

1.  Body Mass (weight): Mammals in the dataset have a minimum body mass
    (weight) of 0.016 Kg, a median of 34 Kg, and a maximum of 6000 Kg.
    The mean body mass is calculated to be \~279 Kg, with a standard
    deviation of \~840 Kg.

2.  Maximal Running Speed (speed): Mammals in the dataset have a minimum
    maximal running speed of 1.60 m/s, a median of 48 m/s, and a maximum
    of 110 m/s. The mean maximal running speed is calculated to be \~46
    m/s, with a standard deviation of \~27 m/s.

3.  Hoppers (hoppers): There are 11 animals in the dataset that are
    classified as hoppers, and 96 animals that are not hoppers.

4.  Specials (specials): As discussed above, there are 10 animals in the
    dataset classified as specials, and 97 animals that are not
    classified as specials.

------------------------------------------------------------------------

### **Create some of the most common forms of descriptive visualizations (graphs).**

``` r
# Histogram of Body Mass (Weight)
ggplot(Mammals, aes(x = weight)) +
  geom_histogram(binwidth = 50, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Body Mass (Weight)",
       x = "Body Mass (Kg)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](britton_a_assignment12_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Q-Q plot of Body Mass (Weight):
qqnorm(Mammals$weight, pch = 1, frame = FALSE, col = "green")
qqline(Mammals$weight, col = "black", lwd = 2)
title(xlab = "Theoretical Quantiles", 
      ylab = "Sample Quantiles", col.main = "lightgreen", cex.main = 1.2)
```

![](britton_a_assignment12_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# Histogram of Maximal Running Speed:
ggplot(Mammals, aes(x = speed)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Maximal Running Speed",
       x = "Maximal Running Speed (m/s)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](britton_a_assignment12_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# Q-Q plot of Maximal Running Speed:
qqnorm(Mammals$speed, pch = 1, frame = FALSE, col = "green")
qqline(Mammals$speed, col = "black", lwd = 2)
title(xlab = "Theoretical Quantiles", 
      ylab = "Sample Quantiles", col.main = "lightgreen", cex.main = 1.2)
```

![](britton_a_assignment12_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
# Boxplot of Maximal Running Speed by Hoppers:
ggplot(Mammals, aes(x = hoppers, y = speed)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Maximal Running Speed by Hoppers",
       x = "Hoppers", y = "Maximal Running Speed (m/s)") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](britton_a_assignment12_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
# Boxplot of Maximal Running Speed by Specials:
ggplot(Mammals, aes(x = specials, y = speed)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Maximal Running Speed by Specials",
       x = "Specials", y = "Maximal Running Speed (m/s)") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](britton_a_assignment12_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

------------------------------------------------------------------------

### **Study your dataset using descriptive statistics and visualization methods in R. Note down any anomalies, outliers, or extreme skews or multimodalities in your distributions that you can identify using these methods.**

**Body Mass (Weight):**

The highest body mass is 6000 Kg, which is much greater than the Q3
value of 142.5 Kg and suggests that the body mass’s upper range may
contain outliers.

Since the mean (278.688 Kg) is greater than the median (34.000 Kg), the
distribution is right-skewed, with the tail extending to higher values.
This can clearly be seen in the histogram above.

The standard deviation (839.6083 Kg) is relatively large, indicating a
wide spread of data points around the mean and potential variability in
the dataset.

``` r
# Shapiro-Wilk test on weight data to determine normal distribution
shapiro.test(Mammals$weight)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  Mammals$weight
    ## W = 0.35151, p-value < 2.2e-16

The Shapiro-Wilk test above further confirms that the weight data is not
normally distributed based on the low p-value.

**Maximal Running Speed:**

The Q3 value of 65.00 m/s is much lower than the highest running speed
of 110.00 m/s, indicating the possibility of outliers in the higher
range of running speed.

The distribution appears to have a small left-skewed distribution with
the tail extending towards lower values because the mean (46.21 m/s) is
less than the median (48.00 m/s). This can be loosely observed in the
histogram.

``` r
# Shapiro-Wilk test on speed data to determine normal distribution
shapiro.test(Mammals$speed)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  Mammals$speed
    ## W = 0.96294, p-value = 0.004446

Again, the Shapiro-Wilk test above further confirms that the speed data
is not normally distributed based on the low p-value.

------------------------------------------------------------------------

### **R provides methods to calculate correlations and significance tests. Conduct one such non-parametric test and report the results.**

For the first test, out of curiosity, we will test if running speed
differs significantly between mammals with “lifestyles in which speed
does not figure as an important factor” (specials = TRUE) and mammals
where speed is an important factor (specials = FALSE). We will test this
using the Wilcoxon rank-sum test, which is a non-parametric test of
whether two samples are drawn from the same distribution.

``` r
# Subset data for mammals with special characteristics and mammals without special characteristics
speed_specials <- Mammals$speed[Mammals$specials == TRUE]
speed_non_specials <- Mammals$speed[Mammals$specials == FALSE]

# Perform Wilcoxon rank-sum test
wilcox.test(speed_specials, speed_non_specials)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  speed_specials and speed_non_specials
    ## W = 189, p-value = 0.001553
    ## alternative hypothesis: true location shift is not equal to 0

The resulting p-value from the Wilcoxon rank-sum test was found to be
0.001553. Therefore, we can reject the null hypothesis and conclude that
there is a statistically significant difference in running speed between
mammals with with “lifestyles in which speed does not figure as an
important factor” and mammals where speed is an important factor.

------------------------------------------------------------------------

### **The final step in this assignment is to test hypotheses about your data using the appropriate non-parametric inferential tests.**

Once again, here are our hypotheses:

**Biological Hypothesis:**

Null Hypothesis: There is no relationship between body mass and maximal
running speed in mammals. Body mass does not significantly affect the
maximal running speed in mammals.

Alternate Hypothesis: There is a relationship between body mass and
maximal running speed in mammals - as body mass increases, maximal
running speed also increases in mammals.

**Statistical Hypothesis:**

Null Hypothesis: The slope of the regression line between body mass and
maximal running speed in mammals is zero. There is no significant linear
relationship between body mass and maximal running speed in mammals.

Alternate Hypothesis: The slope of the regression line between body mass
and maximal running speed in mammals is not equal to zero. There is a
significant linear relationship between body mass and maximal running
speed in mammals.

To test these hypotheses, we will use the Spearman’s rank correlation
coefficient test, which measures statistical dependence between two
variables and is non-parametric.

``` r
# Perform Spearman's rank correlation coefficient test
cor_test <- cor.test(Mammals$weight, Mammals$speed, method = "spearman")

# Extract the correlation coefficient and p-value from the test results
cor_coefficient <- cor_test$estimate
p_value <- cor_test$p.value

# Print the results
cat("Spearman's Rank Correlation Coefficient:", cor_coefficient, "\n")
```

    ## Spearman's Rank Correlation Coefficient: 0.5023751

``` r
cat("p-value:", p_value, "\n")
```

    ## p-value: 3.517285e-08

The positive correlation coefficient of 0.5023751 indicates that there
is a significant linear relationship between body mass and maximal
running speed in mammals. Since the p-value \< 0.05, we can reject the
null hypothesis and conclude that there is a statistically significant
relationship between body mass and maximal running speed in mammals.

This supports the alternate biological and statistical hypotheses that:

1.  As body mass increases, the maximal running speed of mammals tends
    to increase as well.

2.  The slope of the regression line between body mass and maximal
    running speed in mammals is not equal to zero, indicating that there
    is a significant positive association between these variables.
