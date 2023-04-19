britton_a\_assignment3
================
Annie Britton
2/6/2023

## Load in packages and set working directory

``` r
library(tidyverse)
library(datasets) 
setwd("C:/Users/annie/Documents/AEEN/M03")
```

# Fulmars and Data Input (5 points)

### This dataset represents the metabolic rates of 16 breeding northern fulmars (Fulmarus glacialis) in Shetland. The metabolic rates are divided by sex, with 8 observations each.

### Create a data set file in your favorite spreadsheet (e.g. excel, spss, etc. with the data shown above.

``` r
# Create the dataset using two vectors
male <- c(2950, 2308.7, 2135.6, 1945.6, 1195.5, 843.3, 525.8, 605.7)
female <- c(1956.1, 1490.5, 1361.3, 1086.5, 1091, 727.7, NA, NA)

# Combine the vectors to make a tibble
fulmar_messy <- tibble(male, female)
fulmar_messy # Print the tibble to view
```

    ## # A tibble: 8 x 2
    ##    male female
    ##   <dbl>  <dbl>
    ## 1 2950   1956.
    ## 2 2309.  1490.
    ## 3 2136.  1361.
    ## 4 1946.  1086.
    ## 5 1196.  1091 
    ## 6  843.   728.
    ## 7  526.    NA 
    ## 8  606.    NA

### Convert (rewrite, pivot, whatever it takes) to a tidy data set.

``` r
# Convert the data to long format so that we have a sex column 
# and a metabolic rate column. Drop rows with NA values rather than 
# approximating NA values because the sample size is very small
fulmar_clean <- fulmar_messy %>%
  gather("sex", "metabolic_rate", 1:2) %>%
  drop_na()
  
fulmar_clean #print the tibble to view
```

    ## # A tibble: 14 x 2
    ##    sex    metabolic_rate
    ##    <chr>           <dbl>
    ##  1 male            2950 
    ##  2 male            2309.
    ##  3 male            2136.
    ##  4 male            1946.
    ##  5 male            1196.
    ##  6 male             843.
    ##  7 male             526.
    ##  8 male             606.
    ##  9 female          1956.
    ## 10 female          1490.
    ## 11 female          1361.
    ## 12 female          1086.
    ## 13 female          1091 
    ## 14 female           728.

### Export/Save the data as a .csv file.

``` r
# Export the data to a csv with row.names false to avoid creating an index
write.csv(fulmar_clean, 'fulmar_clean.csv', row.names = FALSE)
```

### Import the csv file into RStudio. Check the data structure.

``` r
# Read in the data and check the data structure
fulmar <- read.csv('fulmar_clean.csv')
str(fulmar)
```

    ## 'data.frame':    14 obs. of  2 variables:
    ##  $ sex           : chr  "male" "male" "male" "male" ...
    ##  $ metabolic_rate: num  2950 2309 2136 1946 1196 ...

### View the data.

``` r
# View the data
fulmar
```

    ##       sex metabolic_rate
    ## 1    male         2950.0
    ## 2    male         2308.7
    ## 3    male         2135.6
    ## 4    male         1945.6
    ## 5    male         1195.5
    ## 6    male          843.3
    ## 7    male          525.8
    ## 8    male          605.7
    ## 9  female         1956.1
    ## 10 female         1490.5
    ## 11 female         1361.3
    ## 12 female         1086.5
    ## 13 female         1091.0
    ## 14 female          727.7

### Use qplot to generate a graph of the data by gender.

``` r
# Use qplot to generate a graph of the data
qplot(x = fulmar$sex, y = fulmar$metabolic_rate) +
  # Add title to the plot
  ggtitle("Fulmar Metabolic Rate by Sex") +
  # Label the x-axis
  xlab("Sex") +
  # Label the y-axis
  ylab("Metabolic Rate")
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Messy Data-Diaspore Mass (5 points)

### First, import the data into R.

``` r
# Import the data into R
diaspore_messy <- read.csv('Diasporemass.csv')
diaspore_messy <- as_tibble(diaspore_messy) # Convert to a tibble
diaspore_messy # Print the data
```

    ## # A tibble: 12 x 3
    ##     ï..H     M     X
    ##    <dbl> <dbl> <dbl>
    ##  1  0.25   4.6  1.3 
    ##  2  0.32   3.6 NA   
    ##  3 NA      4.5  1.5 
    ##  4  0.23   3.9  1.7 
    ##  5  0.19   5.1  2.1 
    ##  6  0.26   4.6  1.9 
    ##  7  0.27   4.7  0.98
    ##  8  0.21   4.2  1.4 
    ##  9  0.18   4.5  1.3 
    ## 10  0.33   5.2  1.5 
    ## 11 NA      7.4  1.8 
    ## 12  0.26   5.8  0.9

### Convert the dataset to Tidy Data or Long Form Data. Change the column titles from H, M and X to: Hydroriparian, Mesoriparian, Xeroriparian.

``` r
# Rename the columns in the tibble
colnames(diaspore_messy) <- c('hydroriparian', 'mesoriparian', 'xeroriparian')

# Convert the tibble to long format and remove NA values
diaspore_clean <- diaspore_messy %>% 
  gather("riparian_zone", "diaspore_mass", 1:3) %>%
  drop_na()

# Print the tibble
diaspore_clean
```

    ## # A tibble: 33 x 2
    ##    riparian_zone diaspore_mass
    ##    <chr>                 <dbl>
    ##  1 hydroriparian          0.25
    ##  2 hydroriparian          0.32
    ##  3 hydroriparian          0.23
    ##  4 hydroriparian          0.19
    ##  5 hydroriparian          0.26
    ##  6 hydroriparian          0.27
    ##  7 hydroriparian          0.21
    ##  8 hydroriparian          0.18
    ##  9 hydroriparian          0.33
    ## 10 hydroriparian          0.26
    ## # ... with 23 more rows

### Calculate these three statistics on each column: 1) mean, 2) variance, 3) standard deviation.

``` r
# Calculate the mean, variance, and standard dev for each riparian zone
diaspore_stats <- diaspore_clean %>% 
  group_by(riparian_zone) %>%
  summarize(mass_mean = mean(diaspore_mass), 
            mass_var = var(diaspore_mass), 
            mass_stdev = sd(diaspore_mass))

# Print the tibble to view
diaspore_stats
```

    ## # A tibble: 3 x 4
    ##   riparian_zone mass_mean mass_var mass_stdev
    ##   <chr>             <dbl>    <dbl>      <dbl>
    ## 1 hydroriparian      0.25  0.00249     0.0499
    ## 2 mesoriparian       4.84  0.988       0.994 
    ## 3 xeroriparian       1.49  0.137       0.370

### Create a barplot of the means.

``` r
# Plot the data using qplot and specifying x = riparian_zone, y = mass_mean, and fill = riparian_zone
qplot(x = diaspore_stats$riparian_zone, y = diaspore_stats$mass_mean, fill = diaspore_stats$riparian_zone) +
  # Change the type of plot to a bar plot
  geom_bar(stat = "identity") +
  # Add a title to the plot
  ggtitle("Mean Diaspore Mass by Riparian Zone") +
  # Label the x-axis
  xlab("Riparian Zone") +
  # Label the y-axis
  ylab("Average Diaspore Mass") +
  # Remove the unneeded legend
  guides(fill = 'none')
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# Iris Plotting (10 points)

``` r
# Load in the data
data(iris)
```

### In your R Markdown file, run these functions for the iris dataset: summary(), str(), head(). Present the results in your R Markdown file. Briefly summarize what information each function tells you.

``` r
# Calling summary() on iris shows basic summary statistics on the dataset, like
# the min, median, mean, max, and quartiles, for each column
summary(iris)
```

    ##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
    ##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
    ##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
    ##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
    ##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
    ##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
    ##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
    ##        Species  
    ##  setosa    :50  
    ##  versicolor:50  
    ##  virginica :50  
    ##                 
    ##                 
    ## 

``` r
# Calling str() on iris shows the structure of the dataframe, including the
# number of observations, number of variables, and datatypes
str(iris)
```

    ## 'data.frame':    150 obs. of  5 variables:
    ##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
    ##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
    ##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
    ##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
    ##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
# Calling head() on iris shows the first six cases in the dataset by default
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

### You can also use the filter function to look at specific sets of data. Subset() the data for species virginica using this code:

``` r
# Subset() the data for species virginia
virginica <- subset(iris, Species == "virginica")
```

### What does the “head” function do?

``` r
# The head() function shows the first 6 cases in the dataframe, unless a
# different number is specified
head(virginica)
```

    ##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
    ## 101          6.3         3.3          6.0         2.5 virginica
    ## 102          5.8         2.7          5.1         1.9 virginica
    ## 103          7.1         3.0          5.9         2.1 virginica
    ## 104          6.3         2.9          5.6         1.8 virginica
    ## 105          6.5         3.0          5.8         2.2 virginica
    ## 106          7.6         3.0          6.6         2.1 virginica

### Use the “tail” function for virginica in place of “head”. What are the results?

``` r
# Using tail() on virginica returns the last 6 cases in the dataframe
tail(virginica)
```

    ##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
    ## 145          6.7         3.3          5.7         2.5 virginica
    ## 146          6.7         3.0          5.2         2.3 virginica
    ## 147          6.3         2.5          5.0         1.9 virginica
    ## 148          6.5         3.0          5.2         2.0 virginica
    ## 149          6.2         3.4          5.4         2.3 virginica
    ## 150          5.9         3.0          5.1         1.8 virginica

### Call forth the names of the variables (column names in tidy data)

``` r
# Use colnames() to list the names of the columns in the dataset
colnames(iris)
```

    ## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"

### Lets do a simple scatter plot, petal length vs. petal width

``` r
# This produces a scatter plot of petal length vs width data from the 
# iris dataset, and sets the title to "Edgar Anderson's Iris Data"
plot(iris$Petal.Length, iris$Petal.Width, 
     main="Edgar Anderson's Iris Data")
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Write the code that would plot the Sepal.Length by the Species in the iris dataset. Be sure to get the independent and dependent variables on the correct axes!

``` r
plot(iris$Species, iris$Sepal.Length, 
     main="Sepal Length by Iris Species", 
     xlab = 'Species', 
     ylab = "Sepal Length (cm)")
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Write the code that would plot the Petal.Length by Petal.Width in the iris dataset.

``` r
# Create the scatter plot
plot(iris$Petal.Length, iris$Petal.Width, 
     main="Petal Length by Petal Width, by Species", 
     xlab = 'Petal Length (cm)', 
     ylab = 'Petal Width (cm)',
     col = c("blue", "orange", "dark green")[unclass(iris$Species)], # this indexes colors to species names
     pch = 19) # this specifies the points to be filled with color

# Add a legend for the colors
legend("topleft", levels(iris$Species), col = c("blue", "orange", "dark green"), pch = 19)
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### For this next section, let’s create a different symbol for each of the three iris species. We could use the pch argument (plot character) for this. Consulting the help, we might use pch=21 for filled circles, pch=22 for filled squares, pch=23 for filled diamonds, pch=24 or pch=25 for up/down triangles. Doing this would change all the points… the trick is to create a list mapping the species to say 23, 24 or 25 and use that as the pch argument:

``` r
# This works by using c(23,24,25) to create a vector, and then selecting elements
# 1, 2 or 3 from it. How? unclass(iris$Species) turns the list of species from a 
# list of categories (a "factor" data type in R terminology) into a list of ones,
# twos and threes.
plot(iris$Petal.Length, iris$Petal.Width, 
     pch=c(23,19,25)[unclass(iris$Species)], 
     main="Edgar Anderson's Iris Data")
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# View the unclassed data
c(23,24,25)[unclass(iris$Species)]
```

    ##   [1] 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23
    ##  [26] 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23 23
    ##  [51] 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
    ##  [76] 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24 24
    ## [101] 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25
    ## [126] 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25 25

``` r
# Note here how we can add color: 
plot(iris$Petal.Length, iris$Petal.Width, 
     pch=21, 
     bg=c("red","green3","blue")[unclass(iris$Species)], 
     main="Edgar Anderson's Iris Data")
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Create a histogram of iris sepal width.

``` r
# Plot a histogram of sepal width using
hist(iris$Sepal.Width,
     col="purple", # select a color for the bins
     xlab="Sepal Width (cm)",
     main="Histogram of Iris Sepal Width")
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### Use your textbook (p. 116) to create a box plot of abundance of the following data. These data show the abundance of flour beetles in samples taken from two different farms.

``` r
# Create the dataset using two vectors
woad_farm <- c(9, 12, 12, 13, 14, 18, 21, 21, 23, 24)
glebe_farm <- c(19, 21, 23, 25, 28, 32, 33, 34, 34, 45)

# Combine the two vectors into a tibble
beetle_abun <- tibble(woad_farm, glebe_farm)

# create a box plot showing the abundance of flour beetles on each farm
boxplot(beetle_abun, 
        xlab = "Farm",
        ylab = "Flour Beetle Abundance",
        main = "Abundance of Flour Beetles on Woad and Glebe Farms",
        col = 'beige')
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Use your textbook (p. 163) for this graph. These data show the abundance of a freshwater invertebrate and the water speed at the point of collection. Make a line graph and scatter plot of the following Mayfly data:

``` r
# Create the dataset using two vectors
speed <- c(2, 3, 5, 9, 14, 24, 29, 34)
abund <- c(6, 3, 5, 23, 16, 12, 48, 43)

# Create a scatter plot with the data
plot(speed, abund, 
     xlab = "Water Speed",
     ylab = 'Mayfly Abundance',
     main = "Water Speed versus Mayfly Abundance")

# Add a line connecting the points on the graph
lines(speed, abund)
```

![](britton_a_assignment3_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->
