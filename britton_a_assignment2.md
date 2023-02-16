britton_a\_assignment2
================
Annie Britton
1/30/2023

# Part 1: Employment Data

``` r
# Load in the tidyverse package (which includes readr)
library(tidyverse)

# Set my working directory to the assignment folder
setwd("C:/Users/annie/Documents/AEEN/M02")

# Load in the data from the csv
employment_data <- read_csv("Successful_Employment_for_Blind_Iowans_by_Federal_Fiscal_Year.csv")
```

``` r
# Glimpse the data to explore
glimpse(employment_data)
```

    ## Rows: 606
    ## Columns: 18
    ## $ `Federal Fiscal Year`           <dbl> 2013, 2013, 2013, 2013, 2013, 2013, 20~
    ## $ `Case ID`                       <dbl> 46, 630, 1172, 1664, 1695, 2182, 2195,~
    ## $ Gender                          <chr> "Male", "Female", "Female", "Female", ~
    ## $ Minority                        <chr> "Non-Minority", "Non-Minority", "Non-M~
    ## $ Hispanic                        <chr> "Hispanic-No", "Hispanic-No", "Hispani~
    ## $ `Age Group`                     <chr> "Transition", "Transition", "Transitio~
    ## $ `Age at Application`            <dbl> 17, 17, 15, 15, 46, 14, 62, 17, 20, 14~
    ## $ `Referral Source`               <chr> "Education Elementary/Secondary", "Edu~
    ## $ Veteran                         <chr> "No", "No", "No", "No", "No", "No", "N~
    ## $ `Closure Reason Description`    <chr> "Achieved employment outcome.", "Achie~
    ## $ `Employment Status Description` <chr> "Not employed: Secondary Ed. Student",~
    ## $ `SOC Code`                      <chr> "29-1066.00", "31-1011.00", "21-1019.0~
    ## $ `Occupational Title`            <chr> "Psychiatrist", "Home Health Aide", "D~
    ## $ `Closure Hours Worked Per Week` <dbl> 40, 40, 40, 40, 40, 40, 6, 40, 38, 40,~
    ## $ `Closure Weekly Earnings`       <dbl> 3550, 360, 779, 600, 474, 450, 50, 360~
    ## $ `Hourly Wage`                   <dbl> 88.75, 9.00, 19.48, 15.00, 11.85, 11.2~
    ## $ `Monthly Earnings`              <dbl> 14200, 1440, 3116, 2400, 1896, 1800, 2~
    ## $ `Annual Earnings`               <dbl> 184600, 18720, 40508, 31200, 24648, 23~

## This dataset includes a variety of employment and demographic data on blind Iowans by fiscal year. The dataset includes the following information about each case as a column: fiscal year, case ID, gender, minority status, hispanic status, age group, age at application, referral source, veteran status, case closure reason, employment status, SOC code, occupation title, hours worked/week, earnings/week, hourly wage, monthly earnings, and annual earnings. The dataframe includes 606 cases.

### Question 1: What is the median age of a successful applicant?

``` r
# Find the median age of successful applicants using the median function on the
# "Age at Application" column of the data frame
median(employment_data$`Age at Application`)
```

    ## [1] 42.5

``` r
# Median Age of a Successful Applicant: 42.5
```

### Question 2: What is the median age of a successful minority applicant?

``` r
# Use the select, group_by, and summarise functions from dplyr to find the
# median age at application Minority (and Non-Minority) cases
employment_data %>%  # selecting the dataset
  select(`Age at Application`, Minority) %>% # selecting the columns we want
  group_by(Minority) %>% # grouping by Minority status
  summarise("Median Age at Application" = median(`Age at Application`)) # finding the median
```

    ## # A tibble: 2 x 2
    ##   Minority     `Median Age at Application`
    ##   <chr>                              <dbl>
    ## 1 Minority                              43
    ## 2 Non-Minority                          42

``` r
# Median Age of a Successful Minority Applicant: 43
```

### Question 3: Create a plot comparing age at application by the annual earnings, coloring the points according to minority status.

``` r
# Use ggplot2 to create a scatter plot from employment_data that has age on 
# the x, earnings on the y, color by Minority status, and a title
ggplot(data = employment_data, # selecting our data
       aes(x = `Age at Application`, # data for x axis
           y = `Annual Earnings`, # data for y axis
           color = Minority)) + ggtitle("Age at Application vs Annual Earnings") + geom_point()
```

![](britton_a_assignment2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Part 2: Bird Banding Data

``` r
# Load the data into a vector named bird_data for easy analysis
bird_data <- c(28, 32, 1, 0, 10, 22, 30, 19, 145, 27, 36, 25, 9, 38, 21, 12, 122, 
               87, 36, 3, 0, 5, 55, 62, 98, 32, 900, 33, 14, 39, 56, 81, 29, 38, 
               1, 0, 143, 37, 98, 77, 92, 83, 34, 98, 40, 45, 51, 17, 22, 37, 48, 
               38, 91, 73, 54, 46, 102, 273, 600, 10, 11)
```

## This dataset represents the number of birds counted by the field crew across 61 sampling sites. Each case represents the number of birds seen at a site. Sites are in order within the vector, so instead of making a dataframe, I can just index/analyze the vector itself.

### a. How many sites are there?

``` r
# Find the length of the vector, which will give no. of sites
length(bird_data)
```

    ## [1] 61

### b. How many birds were counted at site 21?

``` r
# Print the value of the 21st item in the vector
bird_data[21]
```

    ## [1] 0

### c. How many birds were counted at the last site?

``` r
# Print the value at the end of the vector using the length function 
# (better than just using [61]- this code is stable even if the data changes)
bird_data[length(bird_data)]
```

    ## [1] 11

### d. What is the total number of birds counted across all the sites?

``` r
# Sum all of the counts in the bird data vector
sum(bird_data)
```

    ## [1] 4366

### e. What is the smallest number of birds counted?

``` r
# Find the minimum value within the vector
min(bird_data)
```

    ## [1] 0

### f. What is the largest number of birds counted?

``` r
# Find the maximum value within the vector
max(bird_data)
```

    ## [1] 900

### g. What is the average number of birds seen at a site?

``` r
# Find the mean of all of the counts,rounding to the closest whole number since
# we can't have half of a bird
round(mean(bird_data), digits = 0)
```

    ## [1] 72
