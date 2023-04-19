Intro to R Assignment
================
Annie Britton
1/23/2023

**Load dplyr.**

``` r
library(dplyr)
```

**Set working directory to activity folder.**

``` r
setwd("C:/Users/annie/Documents/AEEN/M01/Activity1")
```

**Import the stream data set and save as stream1.**

``` r
stream1 <- read.csv(file="assignment1_data.csv", header=TRUE)
```

**Group the data according to stream number and calculate the mean of
fish density (number_fish column) for each stream.**

``` r
stream1 %>% 
  group_by(stream_number) %>% 
  summarise(mean_fish_density = mean(number_fish))
```

    ## # A tibble: 8 x 2
    ##   stream_number mean_fish_density
    ##           <int>             <dbl>
    ## 1             1              45.7
    ## 2             2              69.2
    ## 3             3              43  
    ## 4             4              69.7
    ## 5             5              18.5
    ## 6             6              47.5
    ## 7             7              23.2
    ## 8             8              19.2

**Group the data according to stream type and calculate the mean of fish
density for each stream type.**

``` r
stream1 %>% 
  group_by(stream_type) %>% 
  summarise(mean_fish_density = mean(number_fish))
```

    ## # A tibble: 2 x 2
    ##   stream_type mean_fish_density
    ##   <chr>                   <dbl>
    ## 1 Barrier                  27.1
    ## 2 No Barrier               56.9

**Create your first vector using the stream number and call that vector
‘a’.**

``` r
a <- stream1 %>% 
  group_by(stream_number) %>% 
  summarise(mean_fish_density = mean(number_fish)) %>% 
  pull(stream_number)
```

**Create your second vector using the mean of fish density you
calculated for each stream and call that vector ‘b’.**

``` r
b <- stream1 %>% 
  group_by(stream_number) %>% 
  summarise(mean_fish_density = mean(number_fish)) %>% 
  pull(mean_fish_density)
```

**Make a data frame called ‘stream2’ using these two vectors you just
created.**

``` r
stream2 <- tibble(stream_number = a, mean_fish_density = b)
stream2
```

    ## # A tibble: 8 x 2
    ##   stream_number mean_fish_density
    ##           <int>             <dbl>
    ## 1             1              45.7
    ## 2             2              69.2
    ## 3             3              43  
    ## 4             4              69.7
    ## 5             5              18.5
    ## 6             6              47.5
    ## 7             7              23.2
    ## 8             8              19.2

**Make a graph using this new data with the help of plot() function.**

``` r
plot(stream2, main="Fish Density by Stream",
     xlab="Stream Number",
     ylab="Fish Density Measurements (fish/m2)")
```

![](M01_markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
