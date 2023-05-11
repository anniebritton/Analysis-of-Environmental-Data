Weather Alert Data Analysis
================
Annie Britton

### **Introduction: Severe Weather Alert Data**

For this project, I am using data from the Iowa Environmental Mesonet to
examine weather alerts in the continental US and explore relationships
between alert type, length, size, and temporal variables such as the
month and year of the alert. IEM’s dataset provides information about
tornado, severe thunderstorm, flash flood, and marine warnings going
back to 1986 (Iowa State University, 2023). The dataset is an important
resource for understanding the frequency and severity of extreme weather
events in the region over time (Iowa State University, 2023).

The IEM’s dataset can be useful for a variety of other purposes, such as
assessing the accuracy and effectiveness of the NWS’s warning system,
identifying areas that are particularly prone to extreme weather events,
and predicting the size and length of future weather events (Iowa State
University, 2023). Overall, the IEM’s dataset on weather alerts provides
a valuable resource for researchers, policymakers, and the general
public to better understand and prepare for extreme weather events in
the US. Using this data, I will explore the following questions:

**Research Questions:**

-   (RQ1) Does the type of weather alert, the month of year, or the year
    significantly impact the number of weather alerts issued?

-   (RQ2) Is there a significant relationship between the geographic
    size (Km2) of the alert, and the length of the alert (seconds)?

**Variables:** (Object Name, Units) - Explanation

-   Alert Type (PHENOM): The type of alert issued. MA: Marine, SV:
    Severe Thunderstorm, TO: Tornado, FF: Flash Flood.

-   Alert Count (ALERT_COUNT): The number of alerts per day by alert
    type.

-   Month (MONTH): The month of year in which the alert was issued.

-   Year (YEAR): The year the alert was issued.

-   Alert Length (ALERT_LEN, Seconds): The length of the alert in
    seconds.

-   Alert Size (AVG_AREA, Km2): The geographic size of the alert, in
    Km2.

------------------------------------------------------------------------

``` r
# Let's first load in the packages needed for the project
library(tidyverse)
library(lubridate)
library(plotly)
library(xts)
library(tsbox)
library(forecast)
library(nortest)
library(robustbase)
```

``` r
# Next, load in the csv of the Mesonet data
# This data can be downloaded from IEM's website provided in the Reference section
iem <- read_csv("C:\\Users\\annie\\Documents\\AEED\\Final_Project\\Data\\iem_data.csv")
```

    ## Rows: 1031453 Columns: 24
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (14): WFO, PHENOM, GTYPE, SIG, STATUS, NWS_UGC, HVTEC_NWSLI, HVTEC_SEVE...
    ## dbl   (2): ETN, AREA_KM2
    ## lgl   (1): IS_EMERGENCY
    ## dttm  (7): ISSUED, EXPIRED, INIT_ISS, INIT_EXP, UPDATED, POLYBEGIN, POLYEND
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Take a look at what is contained in the data
iem %>%  glimpse()
```

    ## Rows: 1,031,453
    ## Columns: 24
    ## $ WFO            <chr> "KEY", "KEY", "KEY", "KEY", "KEY", "KEY", "KEY", "MFL",~
    ## $ ISSUED         <dttm> 2013-04-01 12:24:00, 2013-06-08 01:24:00, 2013-04-05 1~
    ## $ EXPIRED        <dttm> 2013-04-01 13:30:00, 2013-06-08 02:30:00, 2013-04-05 1~
    ## $ INIT_ISS       <dttm> 2013-04-01 12:24:00, 2013-06-08 01:24:00, 2013-04-05 1~
    ## $ INIT_EXP       <dttm> 2013-04-01 13:30:00, 2013-06-08 02:30:00, 2013-04-05 1~
    ## $ PHENOM         <chr> "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "MA", "~
    ## $ GTYPE          <chr> "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", ~
    ## $ SIG            <chr> "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W", ~
    ## $ ETN            <dbl> 21, 174, 35, 175, 20, 62, 61, 44, 16, 203, 35, 143, 142~
    ## $ STATUS         <chr> "NEW", "NEW", "NEW", "NEW", "NEW", "NEW", "NEW", "NEW",~
    ## $ NWS_UGC        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    ## $ AREA_KM2       <dbl> 1089.14, 7321.20, 2040.34, 825.20, 1523.49, 429.09, 490~
    ## $ UPDATED        <dttm> 2013-04-01 12:24:00, 2013-06-08 01:24:00, 2013-04-05 1~
    ## $ HVTEC_NWSLI    <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ HVTEC_SEVERITY <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ HVTEC_CAUSE    <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ HVTEC_RECORD   <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ IS_EMERGENCY   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,~
    ## $ POLYBEGIN      <dttm> 2013-04-01 12:24:00, 2013-06-08 01:24:00, 2013-04-05 1~
    ## $ POLYEND        <dttm> 2013-04-01 12:56:00, 2013-06-08 02:30:00, 2013-04-05 1~
    ## $ WINDTAG        <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ HAILTAG        <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ TORNADOTAG     <chr> "None", "None", "None", "None", "None", "None", "None",~
    ## $ DAMAGETAG      <chr> "None", "None", "None", "None", "None", "None", "None",~

There are a lot of columns that aren’t needed for this project, and
others that aren’t in quite the right form. The code below will do some
basic data processing to get the dataframe to a good spot to work with.

``` r
# Change the datetime columns to just the date, removing the time element
iem_date <- iem %>%
  mutate(across(c(ISSUED, EXPIRED, INIT_ISS, INIT_EXP), 
                ~ strftime(.x, format="%Y-%m-%d")))

# Check what alert types are included in the data
# "MA": "Marine"
# "SV": "Severe Thunderstorm"
# "TO": "Tornado"
# "FF": "Flash Flood"
iem_date %>%  select(PHENOM) %>% unique()
```

    ## # A tibble: 4 x 1
    ##   PHENOM
    ##   <chr> 
    ## 1 MA    
    ## 2 SV    
    ## 3 TO    
    ## 4 FF

``` r
# Create a count of alerts per day for each alert type
iem_count <- iem_date %>%  
  select(ISSUED, PHENOM) %>% 
  group_by(ISSUED) %>% 
  count(PHENOM) %>% 
  rename(ALERT_COUNT = n)

# Check the new alert count dataframe
iem_count %>% head()
```

    ## # A tibble: 6 x 3
    ## # Groups:   ISSUED [4]
    ##   ISSUED     PHENOM ALERT_COUNT
    ##   <chr>      <chr>        <int>
    ## 1 2013-01-01 FF               2
    ## 2 2013-01-06 MA              11
    ## 3 2013-01-08 FF               8
    ## 4 2013-01-08 MA              42
    ## 5 2013-01-08 SV               2
    ## 6 2013-01-09 FF              93

------------------------------------------------------------------------

### **Timeseries Plotting (RQ1)**

Next, it will be good to plot a basic visualization to better understand
the data. I will plot a timeseries of the different alert types, looking
at the number of alerts per month by alert type.

``` r
# Create a column representing the month and year for the plot
iem_date$ISSUED_MON <- strftime(iem$ISSUED, '%Y-%m')

# Select the data needed to plot, and count the number of each alert type by month
iem_date_plot <- iem_date %>%  
  select(ISSUED_MON, PHENOM) %>% 
  group_by(ISSUED_MON) %>% 
  count(PHENOM) %>% 
  rename(MONTHLY_ALERT_COUNT = n)

# Create a plot looking at trends over time in alerts
ggplot(iem_date_plot, 
       aes(x = ISSUED_MON, 
           y = MONTHLY_ALERT_COUNT, 
           color = PHENOM,
           group = PHENOM)) + 
  geom_line() +
  labs(x = "Month of Issuance", 
       y = "Number of Weather Alerts", 
       title = 'US Severe Weather Alerts, 2013 - 2022', 
       color = "Alert Type",
       caption = "FF - Flash Flood | MA - Marine | SV - Severe Thunderstorm | TO - Tornado") +
  theme(legend.position="bottom", 
        plot.caption = element_text(hjust = 0.5), # moves the caption to center
        plot.title = element_text(hjust = 0.5)) + # moves the title to center
  scale_x_discrete(breaks = c("2013-01", "2014-01","2015-01","2016-01","2017-01","2018-01","2019-01","2020-01","2021-01","2022-01")) # this adjusts the breaks so that the chart only shows one date/year
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

In the plot above, there is a clear seasonality to alerts across alert
types, as well as a clear difference in the number of alerts based on
alert type. Severe thunderstorm alerts seem to peak between April and
May, while flash flood alerts may peak just a bit later between May and
June. Marine alerts also seem to peak during spring and summer months,
while tornado alerts are greatest earlier in the spring months of
March-May. Through this quick check, one can see that these data are
aligning with the seasonality one would expect from these different
weather types.

------------------------------------------------------------------------

### **Timeseries Decomposition (RQ1)**

Looking at the plot above, it’s clear that there is a seasonality to the
alerts. Due to this, it is hard to see if there are any clear, long-term
trends in the data. To overcome this, I will perform a timeseries
decomposition using the code below, based on Michael Minn’s “Weather
data analysis in R” tutorial. A timeseries decomposition plot is a way
to break down a timeseries into its constituent components: seasonal,
trend, and remainder (Minn, 2023).

To create meaningful timeseries, I need to separate the different alert
types into different data frames, as they may not all follow the same
temporal trends. Additionally, to create timeseries objects, I will fill
any NA values with 0s using na.fill() to indicate that there were no
alerts that day.

``` r
# Create a df for flash floods
iem_ff <- iem_count %>% 
  filter(PHENOM == "FF")
# Create a timeseries object using xts() that has NAs as 0s
iem_ff <- xts(iem_ff, order.by = as.Date(iem_ff$ISSUED)) %>% 
  ts_regular() %>% 
  na.fill(0)

# Create a df for tornados
iem_to <- iem_count %>% 
  filter(PHENOM == "TO")
# Create a timeseries object using xts() that has NAs as 0s
iem_to <- xts(iem_to, order.by = as.Date(iem_to$ISSUED)) %>% 
  ts_regular() %>% 
  na.fill(0)

# Create a df for marine alerts
iem_ma <- iem_count %>% 
  filter(PHENOM == "MA")
# Create a timeseries object using xts() that has NAs as 0s
iem_ma <- xts(iem_ma, order.by = as.Date(iem_ma$ISSUED)) %>% 
  ts_regular() %>% 
  na.fill(0)

# Create a df for severe thunderstorms
iem_sv <- iem_count %>% 
  filter(PHENOM == "SV")
# Create a timeseries object using xts() that has NAs as 0s
iem_sv <- xts(iem_sv, order.by = as.Date(iem_sv$ISSUED)) %>% 
  ts_regular() %>% 
  na.fill(0)

# The stl() function performs this decomposition.
# The ts_ts() function from the library converts an xts field to a ts object that can be used with stl().
# The s.window parameter defines the smoothing to isolate the seasonal component (365 days = one year).
# The t.window parameter defines the smoothing to isolate the long-term trend (~10 years).

# Flash floods
ff_decomp = stl(ts_ts(iem_ff$ALERT_COUNT), s.window=365, t.window=3652)
plot(ff_decomp, main="Flash Flood Alert Timeseries Decomposition, 2013 - 2022")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Tornados
to_decomp = stl(ts_ts(iem_to$ALERT_COUNT), s.window=365, t.window=3652)
plot(to_decomp, main="Tornado Alert Timeseries Decomposition, 2013 - 2022")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# Marine alerts
ma_decomp = stl(ts_ts(iem_ma$ALERT_COUNT), s.window=365, t.window=3652)
plot(ma_decomp, main="Marine Alert Timeseries Decomposition, 2013 - 2022")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# Severe thunderstorms
sv_decomp = stl(ts_ts(iem_sv$ALERT_COUNT), s.window=365, t.window=3652)
plot(sv_decomp, main="Severe Thunderstorm Alert Timeseries Decomposition, 2013 - 2022")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

As mentioned above, the decomposition plots represent several aspects of
the alert data:

-   The top subplot shows the raw data representing the number of alerts
    per day for that alert type.

-   Moving down, the next subplot shows the seasonal trend, representing
    the periodic fluctuations or patterns that occur at regular
    intervals within the timeseries. These patterns seem to be on a
    yearly interval for all four alert types, with a rise and fall
    throughout each year.

-   The third subplot from the top represents the trend, or the
    long-term direction or tendency of the timeseries. One can see that
    there is a decreasing trend in the number of flood alerts, while
    there is an increasing trend in severe thunderstorm alerts, marine
    alerts, and tornado alerts. It is important to note that this does
    not necessarily mean less flooding or more tornadoes, for instance,
    as this data only implies fewer or more **alerts**.

-   The fourth subplot represents the remainder:, which is random
    variation or noise in the data that cannot be explained by the trend
    or seasonal components.

------------------------------------------------------------------------

### **Statistical Tests for Yearly and Seasonal Data across Alert Types (RQ1)**

After looking at the weather alert timeseries data above, it seems that
there may be significant differences in alert counts between years,
seasons, and alert types.

**Statistical Hypotheses**

Null Hypotheses:

1.  The type of weather phenomenon has no significant effect on the
    number of alerts

2.  The month of year has no significant effect on the number of alerts

3.  The year has no significant effect on the number of alerts

Alternative Hypotheses:

1.  The type of weather phenomenon has a statistically significant
    effect on the number of alerts

2.  The month of year has a statistically significant effect on the
    number of alerts

3.  The year has a statistically significant effect on the number of
    alerts

Before performing any hypothesis testing, I first use histograms, Q-Q
plots, the Anderson-Darling test to test if the data is normal. I am
using the Anderson-Darling test in place of the Shapiro-Wilk’s test
since this data has \> 5000 samples, which is a limitation of the
Shapiro-Wilk’s test (R Documentation, n.d).

``` r
# Convert the date column to a date object
iem_count$ISSUED <- as.Date(iem_count$ISSUED)

# Create new columns containing just the year, month, and days
iem_count$YEAR <- format(iem_count$ISSUED, "%Y")
iem_count$MONTH <- format(iem_count$ISSUED, "%m")
iem_count$DAY <- format(iem_count$ISSUED, "%d")

# Check the data
iem_count %>% glimpse()
```

    ## Rows: 9,734
    ## Columns: 6
    ## Groups: ISSUED [3,201]
    ## $ ISSUED      <date> 2013-01-01, 2013-01-06, 2013-01-08, 2013-01-08, 2013-01-0~
    ## $ PHENOM      <chr> "FF", "MA", "FF", "MA", "SV", "FF", "MA", "SV", "FF", "MA"~
    ## $ ALERT_COUNT <int> 2, 11, 8, 42, 2, 93, 129, 2, 117, 77, 31, 39, 116, 143, 36~
    ## $ YEAR        <chr> "2013", "2013", "2013", "2013", "2013", "2013", "2013", "2~
    ## $ MONTH       <chr> "01", "01", "01", "01", "01", "01", "01", "01", "01", "01"~
    ## $ DAY         <chr> "01", "06", "08", "08", "08", "09", "09", "09", "10", "10"~

<br />

Let’s take a look to see if the data is normally distributed using
histograms, a Q-Q Plot, and the Anderson-Darling test.

``` r
# Histogram
hist(iem_count$ALERT_COUNT, 
     main = 'Alert Count Distribution', 
     xlab = 'Alert Count')
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Q-Q Plot
qqnorm(iem_count$ALERT_COUNT, pch = 1, frame = FALSE)
qqline(iem_count$ALERT_COUNT, col = "steelblue", lwd = 2)
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# Anderson-Darling test
ad.test(iem_count$ALERT_COUNT)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_count$ALERT_COUNT
    ## A = 1216.2, p-value < 2.2e-16

From the histogram and Q-Q plot, it looks like the data is right skewed.
The Anderson-Darling test also returns a p-value of \< 0.05, indicating
a non-normal distribution. Let’s try to transform the data to using a
log transform (Schendzielorz, 2021):

``` r
# Log-Transformed Histogram
hist(log(iem_count$ALERT_COUNT), 
     main = 'Alert Count Distribution after Log Transform',  
     xlab = 'Alert Count')
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Log-Transformed Q-Q Plot
qqnorm(log(iem_count$ALERT_COUNT), pch = 1, frame = FALSE)
qqline(log(iem_count$ALERT_COUNT), col = "steelblue", lwd = 2)
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
# Anderson-Darling test
ad.test(log(iem_count$ALERT_COUNT))
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  log(iem_count$ALERT_COUNT)
    ## A = 34.553, p-value < 2.2e-16

While the histogram and Q-Q plot look more normal, the log-transoformed
data still fails the Anderson-Darling test for normality. I will try a
square root transformation, to see if this improves our result
(Schendzielorz, 2021).

``` r
# Sqrt-Transformed Histogram
hist(sqrt(iem_count$ALERT_COUNT), 
     main = 'Alert Count Distribution after Square Root Transform',  
     xlab = 'Alert Count')
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# Sqrt-Transformed Q-Q Plot
qqnorm(sqrt(iem_count$ALERT_COUNT), pch = 1, frame = FALSE)
qqline(sqrt(iem_count$ALERT_COUNT), col = "steelblue", lwd = 2)
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
# Anderson-Darling test
ad.test(sqrt(iem_count$ALERT_COUNT))
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  sqrt(iem_count$ALERT_COUNT)
    ## A = 339.64, p-value < 2.2e-16

Once again, the data fails the Anderson-Darling test for normality.
Rather than trying to continue to transform the data, I will proceed
without the assumption of normality. Since the alert count variable is
not normally distributed, it is not appropriate to use ANOVA, which
assumes normality, to test my hypotheses. Instead, I can use a
non-parametric test such as the Kruskal-Wallis test, a non-parametric
alternative to the one-way ANOVA, to assess differences in alert counts
between years, seasons, and alert types (Sheskin, 2020).

``` r
# Conduct the Kruskal-Wallis test to look at if phenom has an effect on alert count
kruskal.test(ALERT_COUNT ~ PHENOM, data = iem_count)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  ALERT_COUNT by PHENOM
    ## Kruskal-Wallis chi-squared = 1850.7, df = 3, p-value < 2.2e-16

``` r
# Conduct the Kruskal-Wallis test to look at if month has an effect on alert count
kruskal.test(ALERT_COUNT ~ MONTH, data = iem_count)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  ALERT_COUNT by MONTH
    ## Kruskal-Wallis chi-squared = 1336.4, df = 11, p-value < 2.2e-16

``` r
# Conduct the Kruskal-Wallis test to look at if year has an effect on alert count
kruskal.test(ALERT_COUNT ~ YEAR, data = iem_count)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  ALERT_COUNT by YEAR
    ## Kruskal-Wallis chi-squared = 17.761, df = 9, p-value = 0.03805

The results from the Kruskal-Wallis rank sum test indicate that there
are significant differences among the alert types (PHENOM), as indicated
by the highly significant chi-squared value (1839.6) and very low
p-value (\< 2.2e-16). Similarly, the results for the monthly analysis
indicate significant differences among the months, as indicated by the
highly significant chi-squared value (1356.9) and very low p-value (\<
2.2e-16). This means that I can reject null hypotheses a and b and
accept the respective alternative hypotheses.

In contrast, the analysis of the yearly data does not indicate
significant differences among the years, as indicated by the
non-significant chi-squared value (15.974) and a p-value of 0.06742.
This suggests that there are no significant differences between years in
terms of the number of alerts, and results in the acceptance of null
hypothesis c.

These results confirm that seasonality plays a crucial role in the
number of alerts. The differences among alert types and months indicate
that some weather events are more likely to occur during certain
seasons, and this affects the number of alerts received. Therefore, the
results suggest that it is essential to consider seasonality when
analyzing and interpreting the data on alert counts.

Finally, I will plot the monthly Kruskal-Wallis data so that differences
between months (i.e. seasonality) can be seen.

``` r
# Calculate medians and interquartile ranges for the Kruskal-Wallis monthly data
df <- iem_count
medians <- aggregate(ALERT_COUNT ~ PHENOM + MONTH, data = df, median)
q1 <- aggregate(ALERT_COUNT ~ PHENOM + MONTH, data = df, function(x) quantile(x, probs = 0.25))
q3 <- aggregate(ALERT_COUNT ~ PHENOM + MONTH, data = df, function(x) quantile(x, probs = 0.75))

# Create plot for the Kruskal-Wallis monthly data
ggplot(medians, aes(x = PHENOM, y = ALERT_COUNT, fill = MONTH)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = q1$ALERT_COUNT, ymax = q3$ALERT_COUNT), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Kruskal-Wallis Results",
       x = "Alert Type",
       y = "Median Number of Severe Weather Alerts",
       fill = "MONTH",
       caption = "FF - Flash Flood | MA - Marine | SV - Severe Thunderstorm | TO - Tornado") +
  theme(plot.caption = element_text(hjust = 0.5), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Month", labels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
)
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

This plot is useful since it provides specifics to the significant
relationships identified in the Kruskal-Wallis results between alert
counts, alert types, and month of alert. For instance, the plot shows
that severe thunderstorm alerts are the most common alert type, while
tornado alerts are the least common. Marine alert counts and flash flood
alert counts seem to be relatively similar overall, and across months.

In terms of the seasonal trends of alerts, the data here confirm the
general trends identified in the original timeseries plot, with flash
flood alerts occurring most often in July, marine alerts occurring most
often in July, severe thunderstorm alerts occurring most often in June,
and tornado alerts occurring most often in May.

------------------------------------------------------------------------

### **Examining Relationships between Alert Length versus Size (RQ2)**

Other than the seasonal nature of the data, I can also look at the
length and size of the alert and see if there are relationships between
these two variables based on alert type.

As a reminder:

-   Alert Length (ALERT_LEN, Seconds): The length of the alert in
    seconds.

-   Alert Size (AVG_AREA, Km2): The geographic size of the alert, in
    Km2.

<br />

**Statistical Hypotheses**

-   Null Hypothesis: There is no significant statistical relationship
    between alert length and size for any alert type.

-   Alternative Hypothesis: There is a significant statistical
    relationship between alert length and size for one or more alert
    types.

<br />

``` r
# Add a column that represents the length of the alert in seconds, as calculated by the difference between the issued datetime and expired datetime columns
iem_len <- iem %>% 
  mutate(ALERT_LEN = as.numeric(EXPIRED-ISSUED)) %>%  
  select(ISSUED, EXPIRED, ALERT_LEN, PHENOM, AREA_KM2) %>%
  group_by(ALERT_LEN) %>% 
  mutate(AVG_AREA = mean(AREA_KM2))

# Select just 2022 for an exploratory graph (otherwise it will take too long to plot)
iem_len_22 <- iem_len %>% filter(ISSUED > '2022-01-01')

# Plot the 2022 data
ggplot(iem_len_22, 
            aes(x = AVG_AREA, 
                y = ALERT_LEN, 
                color = PHENOM)) + 
  geom_point() +
  labs(x = "Average Alert Size (Km2)",
       y = "Length of Alert (Seconds)", 
       title = 'Weather Alert Size versus Length, 2022', 
       color = "Weather Type",
       caption = "FF - Flash Flood | MA - Marine | SV - Severe Thunderstorm | TO - Tornado") +
  theme(legend.position="bottom", 
        plot.caption = element_text(hjust = 0.5), # moves the caption to center
        plot.title = element_text(hjust = 0.5)) # moves the title to center
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

From the graph above, it seems like the alert types are clustered. Since
the length of the alert and the alert size appears to depend on type of
alert, I will create separate data sets for each alert type to analyze
each alert type separately.

``` r
# Create data sets for each of the alert types and remove impossible alert lengths
iem_len_ff <- iem_len %>% filter(PHENOM == 'FF', ALERT_LEN > 0)
iem_len_to <- iem_len %>% filter(PHENOM == 'TO', ALERT_LEN > 0)
iem_len_ma <- iem_len %>% filter(PHENOM == 'MA', ALERT_LEN > 0)
iem_len_sv <- iem_len %>% filter(PHENOM == 'SV', ALERT_LEN > 0) 
```

After creating four separate dataframes, I will test alerth length and
alert size data for normality to determine what tests to use.

``` r
# Anderson-Darling test for Alert Length variables - Flash Flood
ad.test(iem_len_ff$ALERT_LEN)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_ff$ALERT_LEN
    ## A = 12580, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Length variables - Tornados
ad.test(iem_len_to$ALERT_LEN)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_to$ALERT_LEN
    ## A = 157.13, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Length variables - Marine Alerts
ad.test(iem_len_ma$ALERT_LEN)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_ma$ALERT_LEN
    ## A = 2742.1, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Length variables - Severe Thunderstorms
ad.test(iem_len_sv$ALERT_LEN)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_sv$ALERT_LEN
    ## A = 691.41, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Size variables - Flash Flood
ad.test(iem_len_ff$AVG_AREA)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_ff$AVG_AREA
    ## A = 1164.2, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Size variables - Tornados
ad.test(iem_len_to$AVG_AREA)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_to$AVG_AREA
    ## A = 2659.9, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Size variables - Marine Alerts
ad.test(iem_len_ma$AVG_AREA)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_ma$AVG_AREA
    ## A = 2519.3, p-value < 2.2e-16

``` r
# Anderson-Darling test for Alert Size variables - Severe Thunderstorms
ad.test(iem_len_sv$AVG_AREA)
```

    ## 
    ##  Anderson-Darling normality test
    ## 
    ## data:  iem_len_sv$AVG_AREA
    ## A = 33664, p-value < 2.2e-16

The tests above show that none of the data are normally distributed, all
with p-values \< 0.05. This means that to assess relationships between
alert size and length, I need to use non-parametric statistics. For the
following tests, I will use the Spearman’s rank correlation coefficient
test to assess the relationship between alert size and alert length
(RPubs, 2016).

``` r
# Compute the Spearman's rank correlation coefficient test
cor.test(iem_len_ff$AVG_AREA, iem_len_ff$ALERT_LEN, method = "spearman")
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  iem_len_ff$AVG_AREA and iem_len_ff$ALERT_LEN
    ## S = 5.6709e+14, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##        rho 
    ## -0.5316636

``` r
cor.test(iem_len_to$AVG_AREA, iem_len_to$ALERT_LEN, method = "spearman")
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  iem_len_to$AVG_AREA and iem_len_to$ALERT_LEN
    ## S = 4.725e+12, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.9156878

``` r
cor.test(iem_len_ma$AVG_AREA, iem_len_ma$ALERT_LEN, method = "spearman")
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  iem_len_ma$AVG_AREA and iem_len_ma$ALERT_LEN
    ## S = 4.3835e+13, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.9523186

``` r
cor.test(iem_len_sv$AVG_AREA, iem_len_sv$ALERT_LEN, method = "spearman")
```

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  iem_len_sv$AVG_AREA and iem_len_sv$ALERT_LEN
    ## S = 1.2315e+15, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##       rho 
    ## 0.9736717

The results above show us that in each alert type dataframe, alert area
and alert size have a statistically significant relationship (p-value \<
0.05), allowing us to reject the null hypothesis in favor of the
alternative hypothesis. For the Tornado, Marine Alert, and Severe
thunderstorm data, alert size and length have a strong positive
relationship (rho \> 0), while Flash Flood alert size and length have a
moderate inverse relationship (rho \< 0). Let’s create plots to
visualize this data and represent the results.

``` r
# Write a plotting function
plot_iem_len <- function(df, color, plot_title) {
  # Calculate the Spearman's rank correlation coefficient and its p-value
  spearman_corr <- cor.test(df$ALERT_LEN, df$AVG_AREA, method = "spearman")
  r_value <- sprintf("R = %.2f", spearman_corr$estimate)
  p_value <- sprintf("p = %s", ifelse(spearman_corr$p.value < 0.001, "<0.001", format(round(spearman_corr$p.value, 3), nsmall = 3)))

  # Create the scatter plot with non-parametric smoother and correlation coefficient caption
  p <- ggplot(df, aes(x = AVG_AREA, y = ALERT_LEN)) + 
    geom_point(color = color) +
    geom_smooth(method = "gam", se = FALSE) +
    labs(x = "Average Alert Size (Km2)", y = "Length of Alert (Seconds)", 
         title = plot_title,
         caption = paste(r_value, p_value)) +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0.5), plot.title = element_text(hjust = 0.5))
  
  # Return the plot object
  return(p)
}

# Call the function on the data for each alert type
plot_iem_len(iem_len_ff, "blue", "Flash Flood Alert Size versus Length")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
plot_iem_len(iem_len_to, "red", "Tornado Alert Size versus Length")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
plot_iem_len(iem_len_ma, "green", "Marine Alert Size versus Length")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
plot_iem_len(iem_len_sv, "purple", "Severe Thunderstorm Alert Size versus Length")
```

![](Weather_Alert_Data_Project_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

The plots above show the R and p-values from the Spearman’s rank
correlation coefficient test and include a non-parametric smoother line
to help visualize the overall trend in the relationship between the two
variables. The Flash Flood plot shows us the presence of outliers in the
data, which could be part of the reason for the inverse relationship
seen in the Spearman’s test.

------------------------------------------------------------------------

### **Predicting Severe Weather Alert Lengths (RQ2)**

Because each of the alert types has a significant relationship between
alert size and length, a function can be created to predict the length
of the alert based on the alert size. Since the data are non-parametric,
I will use a function called lmrob(), which performs a a robust linear
regression (robust, 2023). One of the advantages of lmrob() is that it
can handle data with non-normal distributions, including heavy-tailed
and skewed distributions, by using a robust estimation method (robust,
2023). The robust estimation method used by lmrob() is less sensitive to
outliers and influential observations than the least squares method used
by lm(), which can be important when dealing with non-normal data
(robust, 2023).

``` r
# Create a function that takes in a data frame, an alert size, and an alert message
alert_length_predictor <- function(df, alert_size, message){
    # Fit a robust linear regression model to the data using the lmrob() function
    model <- lmrob(ALERT_LEN ~ AVG_AREA, data = df)
    
    # Predict Alert Size 
    new_data <- data.frame(AVG_AREA = alert_size)
    prediction <- predict(model, newdata = new_data)
    
    # Print the prediction
    return(paste0(message, round(prediction/60, digits = 0), " minutes"))
}

# Example predictions
alert_length_predictor(iem_len_ff, 3000, "Predicted Flash Flood Alert Length: ")
```

    ## [1] "Predicted Flash Flood Alert Length: 250 minutes"

``` r
alert_length_predictor(iem_len_to, 3000, "Predicted Tornado Alert Length: ")
```

    ## [1] "Predicted Tornado Alert Length: 79 minutes"

``` r
alert_length_predictor(iem_len_ma, 3000, "Predicted Marine Alert Length: ")
```

    ## [1] "Predicted Marine Alert Length: 65 minutes"

``` r
alert_length_predictor(iem_len_sv, 3000, "Predicted Severe Thunderstorm Alert Length: ")
```

    ## [1] "Predicted Severe Thunderstorm Alert Length: 82 minutes"

This function, if made to be slightly more accurate and sophisticated,
could be used at the onset of an alert to give an indication of how long
the alert might last, potentially improving weather event response and
public safety. In the case of a severe weather event like a flash flood
or tornado, early and accurate prediction of the duration of the alert
can be critical for emergency response and public safety.

For example, if a prediction indicates that a flash flood alert is
likely to last for several hours, authorities can prepare for a more
prolonged response effort. They can also issue updates to the public to
inform them of the expected duration of the event, which can help people
to take steps to protect themselves and their property.

------------------------------------------------------------------------

### **Conclusions**

In conclusion, through the use of plotting, normality testing,
non-parametric statistics, and modeled predictions, the data above
provided the basis for testing the following alternative hypotheses:

**(RQ1) Does the type of weather alert, the month of year, or the year
significantly impact the number of weather alerts issued?**

1.  **Accepted:** The type of weather phenomenon has a statistically
    significant effect on the number of alerts

2.  **Accepted:** The month of year has a statistically significant
    effect on the number of alerts

3.  **Not Supported:** The year has a statistically significant effect
    on the number of alerts

**(RQ2) Is there a significant relationship between the geographic size
(Km2) of the alert, and the length of the alert (seconds)?**

-   **Accepted:** There is a significant statistical relationship
    between alert length and size for one or more alert types.

Based on the results of this analysis, I can conclude that the type of
weather phenomenon and the month of year have a statistically
significant effect on the number of alerts issued, while the year does
not have a significant effect. I also found that there is a significant
relationship between the geographic size and length of the alert for one
or more alert types. These findings have important implications for
understanding and predicting extreme weather events in the US, and can
inform the development of more effective warning systems and emergency
response plans.

Overall, the IEM’s dataset on weather alerts is a valuable resource for
researchers and policymakers working to improve our understanding and
preparedness for extreme weather events. Further work could include a
spatial analysis of the weather alert data to examine regions more prone
to severe weather, and an exploration of whether the spatial nature of
weather alerts is changing over time.

### **References**

-   Iowa State University. (2023). *Archived NWS Watch, Warnings,
    Advisories* \[Data Set\]. Iowa Environmental Mesonet.
    <https://mesonet.agron.iastate.edu/request/gis/watchwarn.phtml>

-   Minn, M. (2023). *Tutorial: Weather data analysis in R*.
    <https://michaelminn.net/tutorials/r-weather/index.html>

-   R Documentation. (n.d.). *The Shapiro-Wilk test of normality*.
    r-project.org.
    <https://search.r-project.org/CRAN/refmans/dlookr/html/normality.data.frame.html>

-   RPubs. (2016). *Spearman’s rank correlation coefficient*. RPubs by
    RStudio. <https://rpubs.com/aaronsc32/spearman-rank-correlation>

-   robust. (2023). *lmRob: High Breakdown and High Efficiency Robust
    Linear Regression*. RDocumentation.
    <https://www.rdocumentation.org/packages/robust/versions/0.7-1/topics/lmRob>

-   Schendzielorz, T. M. (2021, April 18). *A guide to data
    transformation*. Medium.
    <https://medium.com/analytics-vidhya/a-guide-to-data-transformation-9e5fa9ae1ca3>

-   Sheskin, D. J. (2020). The Kruskal–Wallis one-way analysis of
    variance by ranks. *Handbook of Parametric and Nonparametric
    Statistical Procedures*, 1001-1026.
    <https://doi.org/10.1201/9780429186196-29>
