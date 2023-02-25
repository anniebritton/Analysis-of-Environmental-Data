britton_a\_assignment5
================
Annie Britton
2/19/2023

## PART IV: FAO and Agriculture (10 points)

``` r
# Load in the tidyverse
library(tidyverse)

# Set my working directory
setwd("C:\\Users\\annie\\Documents\\AEEN\\M05")

# Load in the data
load("FAOData.RData")
```

### View the datasets.

``` r
head(dat1l2, 3)
```

    ##   Year   Crop Country    Yield
    ## 1 2012 Barley  Canada 38894.66
    ## 2 2012  Maize  Canada 83611.49
    ## 3 2012   Oats  Canada 24954.79

``` r
head(dat1l2, 3)
```

    ##   Year   Crop Country    Yield
    ## 1 2012 Barley  Canada 38894.66
    ## 2 2012  Maize  Canada 83611.49
    ## 3 2012   Oats  Canada 24954.79

``` r
head(dat1w, 3)
```

    ##   Year   Barley Buckwheat    Maize     Oats      Rye
    ## 1 1961 16488.52  10886.67 39183.63 15171.26 11121.79
    ## 2 1962 18839.00  11737.50 40620.80 16224.60 12892.77
    ## 3 1963 18808.27  11995.00 42595.55 16253.04 11524.11

``` r
head(dat2, 3)
```

    ##    County State B20004001 B20004002 B20004003 B20004004 B20004005 B20004006
    ## 1 Autauga    al     35881     17407     30169     35327     54917     63317
    ## 2 Baldwin    al     31439     16970     25414     31312     44940     54599
    ## 3 Barbour    al     25201     15643     20946     24201     42629     48500
    ##   B20004007 B20004008 B20004009 B20004010 B20004011 B20004012 B20004013
    ## 1     46227     26055     36440     48243     64639     79750     27799
    ## 2     40662     20401     31553     42561     61454     70349     24694
    ## 3     31623     20526     29966     32212     61528     62788     19950
    ##   B20004014 B20004015 B20004016 B20004017 B20004018
    ## 1     15634     23728     27430     42158     49829
    ## 2     11772     18670     24040     36010     51107
    ## 3     12878     16748     19479     32014     47833

``` r
head(dat2c, 3)
```

    ##   State                 County Level Region   All     F     M
    ## 1    ak Aleutians East Borough   All   West 21953 20164 22940
    ## 2    ak Aleutians East Borough  NoHS   West 21953 19250 22885
    ## 3    ak Aleutians East Borough    HS   West 20770 19671 21192

### Generate a point plot of crop yield as a function of year using the dat1l data frame.

``` r
# Use ggplot to generate a point plot using geom_point
ggplot() + geom_point(data = dat1l, aes(x = Year, y = Yield))
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Let’s include a third variable, crop type, to the map. You will need to map its aesthetics: so map Crop to the color element of the geom. Note how color acts as a grouping parameter.

``` r
# Add color to the point plot by crop by adding color = Crop into geom_point
ggplot() + geom_point(data = dat1l, aes(x = Year, y = Yield, color = Crop))
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Now plot lines instead of points, by substituting the geometry type with the geom_line() geometry.

``` r
# Change the points to lines by using the geom_line function
ggplot() + geom_line(data = dat1l, aes(x = Year, y = Yield, color = Crop))
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Use data from dat1w to generate a plot of oat yield as a function of year.

``` r
# Use geom_line to generate a line plot of oat yeilds by year
ggplot() + geom_line(data = dat1w, aes(x = Year, y = Oats))
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Modify the color and line type of this plot.

``` r
# change the line type to dashed using linetype and color to brown
ggplot() + geom_line(data = dat1w, aes(x = Year, y = Oats), linetype = "dashed", color='brown')
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## PART V: More ggplot2 using LPI (10 points)

``` r
# Import data from the Living Planet Index - population trends of vertebrate species from 1970 to 2014
LPI <- read_csv('LPIdata_CC.csv')

# View the data
head(LPI)
```

    ## # A tibble: 6 x 53
    ##   Class     Order Commo~1 Count~2 Region system biome realm `1970` `1971` `1972`
    ##   <chr>     <chr> <chr>   <chr>   <chr>  <chr>  <chr> <chr> <chr>   <dbl>  <dbl>
    ## 1 Aves      Char~ Slende~ France  Europe Fresh~ Temp~ Pale~ 0          NA     NA
    ## 2 Actinopt~ Cypr~ Rednos~ Zimbab~ Africa Fresh~ Trop~ Afro~ 0           0      0
    ## 3 Actinopt~ Cypr~ Rednos~ Zimbab~ Africa Fresh~ Trop~ Afro~ 0           0      0
    ## 4 Actinopt~ Silu~ Plain ~ Zimbab~ Africa Fresh~ Trop~ Afro~ 0           0      0
    ## 5 Mammalia  Carn~ Grey s~ United~ Europe Marine Unkn~ Atla~ 0           0     NA
    ## 6 Mammalia  Carn~ Grey s~ United~ Europe Marine Unkn~ Atla~ 0           0     NA
    ## # ... with 42 more variables: `1973` <dbl>, `1974` <dbl>, `1975` <dbl>,
    ## #   `1976` <dbl>, `1977` <dbl>, `1978` <dbl>, `1979` <dbl>, `1980` <dbl>,
    ## #   `1981` <dbl>, `1982` <dbl>, `1983` <dbl>, `1984` <dbl>, `1985` <dbl>,
    ## #   `1986` <dbl>, `1987` <dbl>, `1988` <dbl>, `1989` <dbl>, `1990` <dbl>,
    ## #   `1991` <dbl>, `1992` <dbl>, `1993` <dbl>, `1994` <dbl>, `1995` <dbl>,
    ## #   `1996` <dbl>, `1997` <dbl>, `1998` <dbl>, `1999` <dbl>, `2000` <dbl>,
    ## #   `2001` <dbl>, `2002` <dbl>, `2003` <dbl>, `2004` <dbl>, `2005` <dbl>, ...

### First, we need to get our data loaded and in the right format.

``` r
# Notice the data are in a wide format - the different years are column names, when really they should be rows
# We will reshape the data using the gather() function from the tidyr package
# Reshape data into long form 
# By adding 9:53, we select rows from 9 to 53, the ones for the different years of monitoring
LPI2 <- gather(LPI, "year", "abundance", 9:53)
glimpse(LPI2)
```

    ## Rows: 748,080
    ## Columns: 10
    ## $ Class          <chr> "Aves", "Actinopterygii", "Actinopterygii", "Actinopter~
    ## $ Order          <chr> "Charadriiformes", "Cypriniformes", "Cypriniformes", "S~
    ## $ `Common Name`  <chr> "Slender-billed gull", "Rednose labeo", "Rednose labeo"~
    ## $ `Country list` <chr> "France", "Zimbabwe", "Zimbabwe", "Zimbabwe", "United K~
    ## $ Region         <chr> "Europe", "Africa", "Africa", "Africa", "Europe", "Euro~
    ## $ system         <chr> "Freshwater", "Freshwater", "Freshwater", "Freshwater",~
    ## $ biome          <chr> "Temperate coastal rivers", "Tropical and subtropical f~
    ## $ realm          <chr> "Palearctic", "Afrotropical", "Afrotropical", "Afrotrop~
    ## $ year           <chr> "1970", "1970", "1970", "1970", "1970", "1970", "1970",~
    ## $ abundance      <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", ~

``` r
# Notice that there is an 'X' in front of all the years - when we imported the data, all column names become characters
# R put an 'X' in front of the years to turn the numbers into characters
# Now that the years are rows, not columns, we need them to be numbers
LPI2$year <- as.numeric(LPI2$year)
```

``` r
# When manipulating data, it's always good to check if the variables have stayed how we want them
# Use the str() function
str(LPI2)
```

    ## tibble [748,080 x 10] (S3: tbl_df/tbl/data.frame)
    ##  $ Class       : chr [1:748080] "Aves" "Actinopterygii" "Actinopterygii" "Actinopterygii" ...
    ##  $ Order       : chr [1:748080] "Charadriiformes" "Cypriniformes" "Cypriniformes" "Siluriformes" ...
    ##  $ Common Name : chr [1:748080] "Slender-billed gull" "Rednose labeo" "Rednose labeo" "Plain squeaker" ...
    ##  $ Country list: chr [1:748080] "France" "Zimbabwe" "Zimbabwe" "Zimbabwe" ...
    ##  $ Region      : chr [1:748080] "Europe" "Africa" "Africa" "Africa" ...
    ##  $ system      : chr [1:748080] "Freshwater" "Freshwater" "Freshwater" "Freshwater" ...
    ##  $ biome       : chr [1:748080] "Temperate coastal rivers" "Tropical and subtropical floodplain rivers and wetland complexes" "Tropical and subtropical floodplain rivers and wetland complexes" "Tropical and subtropical floodplain rivers and wetland complexes" ...
    ##  $ realm       : chr [1:748080] "Palearctic" "Afrotropical" "Afrotropical" "Afrotropical" ...
    ##  $ year        : num [1:748080] 1970 1970 1970 1970 1970 1970 1970 1970 1970 1970 ...
    ##  $ abundance   : chr [1:748080] "0" "0" "0" "0" ...

``` r
# Abundance is a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)
str(LPI2)
```

    ## tibble [748,080 x 10] (S3: tbl_df/tbl/data.frame)
    ##  $ Class       : chr [1:748080] "Aves" "Actinopterygii" "Actinopterygii" "Actinopterygii" ...
    ##  $ Order       : chr [1:748080] "Charadriiformes" "Cypriniformes" "Cypriniformes" "Siluriformes" ...
    ##  $ Common Name : chr [1:748080] "Slender-billed gull" "Rednose labeo" "Rednose labeo" "Plain squeaker" ...
    ##  $ Country list: chr [1:748080] "France" "Zimbabwe" "Zimbabwe" "Zimbabwe" ...
    ##  $ Region      : chr [1:748080] "Europe" "Africa" "Africa" "Africa" ...
    ##  $ system      : chr [1:748080] "Freshwater" "Freshwater" "Freshwater" "Freshwater" ...
    ##  $ biome       : chr [1:748080] "Temperate coastal rivers" "Tropical and subtropical floodplain rivers and wetland complexes" "Tropical and subtropical floodplain rivers and wetland complexes" "Tropical and subtropical floodplain rivers and wetland complexes" ...
    ##  $ realm       : chr [1:748080] "Palearctic" "Afrotropical" "Afrotropical" "Afrotropical" ...
    ##  $ year        : num [1:748080] 1970 1970 1970 1970 1970 1970 1970 1970 1970 1970 ...
    ##  $ abundance   : num [1:748080] 0 0 0 0 0 0 0 0 0 0 ...

### Create a graph of population trends of two species of your choice.

``` r
# Select the two species of interest
my_species <- filter(LPI2, `Common Name` == c("Bald eagle", "Pink salmon"))

# Group the species by common name and year, and then find the total abundance
my_species <- my_species %>% group_by(`Common Name`, year) %>% summarize(total_abun = sum(abundance, na.rm=TRUE))

# Plot the species on a line plot using Common Name to distinguish the colors
ggplot() + 
  geom_line(data = my_species, 
            aes(x = year, 
                y = total_abun, 
                color = `Common Name`)) +
  ylab("Total Abundance")
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Choose two species from the LPI data and display their population trends using a boxplot

``` r
# Select the two species of interest
my_species <- filter(LPI2, `Common Name` == c("Chinook salmon", "Sockeye salmon / Red salmon"))

# Group the species by common name and year, and then find the total abundance
my_species <- my_species %>% group_by(`Common Name`, year) %>% summarize(total_abun = sum(abundance, na.rm=TRUE))

# Plot the species on a box plot showing total abundance
ggplot() + 
  geom_boxplot(data = my_species, 
            aes(x = `Common Name`, 
                y = total_abun, fill= `Common Name`)) +
  ylab("Total Abundance") + 
  theme(legend.position="bottom")
```

![](M05_markdown_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
