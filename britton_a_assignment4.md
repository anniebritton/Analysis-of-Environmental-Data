britton_a\_assignment4
================
Annie Britton
2/13/2023

## Set Up

``` r
# load the tidyverse
library(tidyverse)

#set working directory
setwd("C:\\Users\\annie\\Documents\\AEEN\\M04")
```

## PART I: How to Obtain Data-GBIF Data (5 points)

The search criteria I used for my GBIF data was just a geographic area
bounded by a polygon that encompassed the River North and Gold Coast
areas of Chicago, where I live. It includes 52,404 records from 59
different data sets.

Polygon: (-87.6548 41.88703,-87.59974 41.88703,-87.59974
41.91116,-87.6548 41.91116,-87.6548 41.88703)

``` r
# import my GBIF data
chi_gbif <- read.csv('chicago_GBIF.csv')

# glimpse the data set
glimpse(chi_gbif)
```

    ## Rows: 52,405
    ## Columns: 51
    ## $ gbifID                           <dbl> 979133095, 979133076, 979133041, 9791~
    ## $ datasetKey                       <chr> "4fa7b334-ce0d-4e88-aaae-2e0c138d049e~
    ## $ occurrenceID                     <chr> "URN:catalog:CLO:EBIRD:OBS237736224",~
    ## $ kingdom                          <chr> "Animalia", "Animalia", "Animalia", "~
    ## $ phylum                           <chr> "Chordata", "Chordata", "Chordata", "~
    ## $ class                            <chr> "Aves", "Aves", "Aves", "Aves", "Aves~
    ## $ order                            <chr> "Charadriiformes", "Gaviiformes", "An~
    ## $ family                           <chr> "Laridae", "Gaviidae", "Anatidae", "A~
    ## $ genus                            <chr> "Larus", "Gavia", "Mergus", "Clangula~
    ## $ species                          <chr> "Larus glaucoides", "Gavia stellata",~
    ## $ infraspecificEpithet             <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ taxonRank                        <chr> "SPECIES", "SPECIES", "SPECIES", "SPE~
    ## $ scientificName                   <chr> "Larus glaucoides B.Meyer, 1822", "Ga~
    ## $ verbatimScientificName           <chr> "Larus glaucoides", "Gavia stellata",~
    ## $ verbatimScientificNameAuthorship <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ countryCode                      <chr> "US", "US", "US", "US", "US", "US", "~
    ## $ locality                         <chr> "Olive Park, Chicago", "Olive Park, C~
    ## $ stateProvince                    <chr> "Illinois", "Illinois", "Illinois", "~
    ## $ occurrenceStatus                 <chr> "PRESENT", "PRESENT", "PRESENT", "PRE~
    ## $ individualCount                  <int> NA, 1, NA, NA, NA, NA, 1, NA, NA, NA,~
    ## $ publishingOrgKey                 <chr> "e2e717bf-551a-4917-bdc9-4fa0f342c530~
    ## $ decimalLatitude                  <dbl> 41.89289, 41.89289, 41.89289, 41.8928~
    ## $ decimalLongitude                 <dbl> -87.61185, -87.61185, -87.61185, -87.~
    ## $ coordinateUncertaintyInMeters    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ coordinatePrecision              <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ elevation                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ elevationAccuracy                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ depth                            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ depthAccuracy                    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ eventDate                        <chr> "1982-02-21T00:00:00", "1982-02-21T00~
    ## $ day                              <int> 21, 21, 21, 21, 2, 2, 22, 22, 22, 22,~
    ## $ month                            <int> 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2~
    ## $ year                             <int> 1982, 1982, 1982, 1982, 1981, 1981, 1~
    ## $ taxonKey                         <int> 2481156, 2481958, 2498375, 2498273, 2~
    ## $ speciesKey                       <int> 2481156, 2481958, 2498375, 2498273, 2~
    ## $ basisOfRecord                    <chr> "HUMAN_OBSERVATION", "HUMAN_OBSERVATI~
    ## $ institutionCode                  <chr> "CLO", "CLO", "CLO", "CLO", "CLO", "C~
    ## $ collectionCode                   <chr> "EBIRD", "EBIRD", "EBIRD", "EBIRD", "~
    ## $ catalogNumber                    <chr> "OBS237736224", "OBS237736223", "OBS2~
    ## $ recordNumber                     <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ identifiedBy                     <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ dateIdentified                   <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ license                          <chr> "CC_BY_4_0", "CC_BY_4_0", "CC_BY_4_0"~
    ## $ rightsHolder                     <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ recordedBy                       <chr> "obsr276337", "obsr276337", "obsr2763~
    ## $ typeStatus                       <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ establishmentMeans               <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ lastInterpreted                  <chr> "2023-01-24T15:16:10.614Z", "2023-01-~
    ## $ mediaType                        <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ issue                            <chr> "CONTINENT_DERIVED_FROM_COORDINATES",~
    ## $ X                                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~

## PART III: First Brush dplyr (5 points)

``` r
# import worms.txt
worms <- read.table('worms.txt', header = T)
```

``` r
# print the column names
names(worms)
```

    ## [1] "Field.Name"   "Area"         "Slope"        "Vegetation"   "Soil.pH"     
    ## [6] "Damp"         "Worm.density"

``` r
# print the first 6 rows
head(worms)
```

    ##        Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1     Nashs.Field  3.6    11  Grassland     4.1 FALSE            4
    ## 2  Silwood.Bottom  5.1     2     Arable     5.2 FALSE            7
    ## 3   Nursery.Field  2.8     3  Grassland     4.3 FALSE            2
    ## 4     Rush.Meadow  2.4     5     Meadow     4.9  TRUE            5
    ## 5 Gunness.Thicket  3.8     0      Scrub     4.2 FALSE            6
    ## 6        Oak.Mead  3.1     2  Grassland     3.9 FALSE            2

``` r
# print the dimensions of the data
dim(worms)
```

    ## [1] 20  7

``` r
# print the structure of the data
str(worms)
```

    ## 'data.frame':    20 obs. of  7 variables:
    ##  $ Field.Name  : chr  "Nashs.Field" "Silwood.Bottom" "Nursery.Field" "Rush.Meadow" ...
    ##  $ Area        : num  3.6 5.1 2.8 2.4 3.8 3.1 3.5 2.1 1.9 1.5 ...
    ##  $ Slope       : int  11 2 3 5 0 2 3 0 0 4 ...
    ##  $ Vegetation  : chr  "Grassland" "Arable" "Grassland" "Meadow" ...
    ##  $ Soil.pH     : num  4.1 5.2 4.3 4.9 4.2 3.9 4.2 4.8 5.7 5 ...
    ##  $ Damp        : logi  FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ Worm.density: int  4 7 2 5 6 2 3 4 9 7 ...

``` r
# use glimpse() to see every column
glimpse(worms)
```

    ## Rows: 20
    ## Columns: 7
    ## $ Field.Name   <chr> "Nashs.Field", "Silwood.Bottom", "Nursery.Field", "Rush.M~
    ## $ Area         <dbl> 3.6, 5.1, 2.8, 2.4, 3.8, 3.1, 3.5, 2.1, 1.9, 1.5, 2.9, 3.~
    ## $ Slope        <int> 11, 2, 3, 5, 0, 2, 3, 0, 0, 4, 10, 1, 2, 6, 0, 0, 8, 2, 1~
    ## $ Vegetation   <chr> "Grassland", "Arable", "Grassland", "Meadow", "Scrub", "G~
    ## $ Soil.pH      <dbl> 4.1, 5.2, 4.3, 4.9, 4.2, 3.9, 4.2, 4.8, 5.7, 5.0, 5.2, 4.~
    ## $ Damp         <lgl> FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FA~
    ## $ Worm.density <int> 4, 7, 2, 5, 6, 2, 3, 4, 9, 7, 8, 1, 2, 0, 6, 8, 4, 5, 1, 3

``` r
# turn the dataframe into a tibble
tbl_df(worms)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## i Please use `tibble::as_tibble()` instead.

    ## # A tibble: 20 x 7
    ##    Field.Name         Area Slope Vegetation Soil.pH Damp  Worm.density
    ##    <chr>             <dbl> <int> <chr>        <dbl> <lgl>        <int>
    ##  1 Nashs.Field         3.6    11 Grassland      4.1 FALSE            4
    ##  2 Silwood.Bottom      5.1     2 Arable         5.2 FALSE            7
    ##  3 Nursery.Field       2.8     3 Grassland      4.3 FALSE            2
    ##  4 Rush.Meadow         2.4     5 Meadow         4.9 TRUE             5
    ##  5 Gunness.Thicket     3.8     0 Scrub          4.2 FALSE            6
    ##  6 Oak.Mead            3.1     2 Grassland      3.9 FALSE            2
    ##  7 Church.Field        3.5     3 Grassland      4.2 FALSE            3
    ##  8 Ashurst             2.1     0 Arable         4.8 FALSE            4
    ##  9 The.Orchard         1.9     0 Orchard        5.7 FALSE            9
    ## 10 Rookery.Slope       1.5     4 Grassland      5   TRUE             7
    ## 11 Garden.Wood         2.9    10 Scrub          5.2 FALSE            8
    ## 12 North.Gravel        3.3     1 Grassland      4.1 FALSE            1
    ## 13 South.Gravel        3.7     2 Grassland      4   FALSE            2
    ## 14 Observatory.Ridge   1.8     6 Grassland      3.8 FALSE            0
    ## 15 Pond.Field          4.1     0 Meadow         5   TRUE             6
    ## 16 Water.Meadow        3.9     0 Meadow         4.9 TRUE             8
    ## 17 Cheapside           2.2     8 Scrub          4.7 TRUE             4
    ## 18 Pound.Hill          4.4     2 Arable         4.5 FALSE            5
    ## 19 Gravel.Pit          2.9     1 Grassland      3.5 FALSE            1
    ## 20 Farm.Wood           0.8    10 Scrub          5.1 TRUE             3

``` r
# show a statistical summary of the data by column
summary(worms)
```

    ##   Field.Name             Area           Slope        Vegetation       
    ##  Length:20          Min.   :0.800   Min.   : 0.00   Length:20         
    ##  Class :character   1st Qu.:2.175   1st Qu.: 0.75   Class :character  
    ##  Mode  :character   Median :3.000   Median : 2.00   Mode  :character  
    ##                     Mean   :2.990   Mean   : 3.50                     
    ##                     3rd Qu.:3.725   3rd Qu.: 5.25                     
    ##                     Max.   :5.100   Max.   :11.00                     
    ##     Soil.pH         Damp          Worm.density 
    ##  Min.   :3.500   Mode :logical   Min.   :0.00  
    ##  1st Qu.:4.100   FALSE:14        1st Qu.:2.00  
    ##  Median :4.600   TRUE :6         Median :4.00  
    ##  Mean   :4.555                   Mean   :4.35  
    ##  3rd Qu.:5.000                   3rd Qu.:6.25  
    ##  Max.   :5.700                   Max.   :9.00

``` r
# select the Vegetation column
select(worms, Vegetation)
```

    ##    Vegetation
    ## 1   Grassland
    ## 2      Arable
    ## 3   Grassland
    ## 4      Meadow
    ## 5       Scrub
    ## 6   Grassland
    ## 7   Grassland
    ## 8      Arable
    ## 9     Orchard
    ## 10  Grassland
    ## 11      Scrub
    ## 12  Grassland
    ## 13  Grassland
    ## 14  Grassland
    ## 15     Meadow
    ## 16     Meadow
    ## 17      Scrub
    ## 18     Arable
    ## 19  Grassland
    ## 20      Scrub

``` r
# select everything but the Vegetation column
select(worms, -Vegetation)
```

    ##           Field.Name Area Slope Soil.pH  Damp Worm.density
    ## 1        Nashs.Field  3.6    11     4.1 FALSE            4
    ## 2     Silwood.Bottom  5.1     2     5.2 FALSE            7
    ## 3      Nursery.Field  2.8     3     4.3 FALSE            2
    ## 4        Rush.Meadow  2.4     5     4.9  TRUE            5
    ## 5    Gunness.Thicket  3.8     0     4.2 FALSE            6
    ## 6           Oak.Mead  3.1     2     3.9 FALSE            2
    ## 7       Church.Field  3.5     3     4.2 FALSE            3
    ## 8            Ashurst  2.1     0     4.8 FALSE            4
    ## 9        The.Orchard  1.9     0     5.7 FALSE            9
    ## 10     Rookery.Slope  1.5     4     5.0  TRUE            7
    ## 11       Garden.Wood  2.9    10     5.2 FALSE            8
    ## 12      North.Gravel  3.3     1     4.1 FALSE            1
    ## 13      South.Gravel  3.7     2     4.0 FALSE            2
    ## 14 Observatory.Ridge  1.8     6     3.8 FALSE            0
    ## 15        Pond.Field  4.1     0     5.0  TRUE            6
    ## 16      Water.Meadow  3.9     0     4.9  TRUE            8
    ## 17         Cheapside  2.2     8     4.7  TRUE            4
    ## 18        Pound.Hill  4.4     2     4.5 FALSE            5
    ## 19        Gravel.Pit  2.9     1     3.5 FALSE            1
    ## 20         Farm.Wood  0.8    10     5.1  TRUE            3

``` r
# return a specific row
slice(worms, 4)
```

    ##    Field.Name Area Slope Vegetation Soil.pH Damp Worm.density
    ## 1 Rush.Meadow  2.4     5     Meadow     4.9 TRUE            5

``` r
# return a sequence of rows
slice(worms, 4:8)
```

    ##        Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1     Rush.Meadow  2.4     5     Meadow     4.9  TRUE            5
    ## 2 Gunness.Thicket  3.8     0      Scrub     4.2 FALSE            6
    ## 3        Oak.Mead  3.1     2  Grassland     3.9 FALSE            2
    ## 4    Church.Field  3.5     3  Grassland     4.2 FALSE            3
    ## 5         Ashurst  2.1     0     Arable     4.8 FALSE            4

``` r
# return a subset of rows
slice(worms, c(4, 6, 8))
```

    ##    Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1 Rush.Meadow  2.4     5     Meadow     4.9  TRUE            5
    ## 2    Oak.Mead  3.1     2  Grassland     3.9 FALSE            2
    ## 3     Ashurst  2.1     0     Arable     4.8 FALSE            4

``` r
# return rows with an area greater than 3
filter(worms, Area > 3)
```

    ##         Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1      Nashs.Field  3.6    11  Grassland     4.1 FALSE            4
    ## 2   Silwood.Bottom  5.1     2     Arable     5.2 FALSE            7
    ## 3  Gunness.Thicket  3.8     0      Scrub     4.2 FALSE            6
    ## 4         Oak.Mead  3.1     2  Grassland     3.9 FALSE            2
    ## 5     Church.Field  3.5     3  Grassland     4.2 FALSE            3
    ## 6     North.Gravel  3.3     1  Grassland     4.1 FALSE            1
    ## 7     South.Gravel  3.7     2  Grassland     4.0 FALSE            2
    ## 8       Pond.Field  4.1     0     Meadow     5.0  TRUE            6
    ## 9     Water.Meadow  3.9     0     Meadow     4.9  TRUE            8
    ## 10      Pound.Hill  4.4     2     Arable     4.5 FALSE            5

``` r
# return rows with an area greater than 5 or less than 2
filter(worms, Area > 5 | Area < 2)
```

    ##          Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1    Silwood.Bottom  5.1     2     Arable     5.2 FALSE            7
    ## 2       The.Orchard  1.9     0    Orchard     5.7 FALSE            9
    ## 3     Rookery.Slope  1.5     4  Grassland     5.0  TRUE            7
    ## 4 Observatory.Ridge  1.8     6  Grassland     3.8 FALSE            0
    ## 5         Farm.Wood  0.8    10      Scrub     5.1  TRUE            3

``` r
# return rows with an area greater than 3 and a soil pH of less than 5
filter(worms, Area > 3 & Soil.pH < 5)
```

    ##        Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1     Nashs.Field  3.6    11  Grassland     4.1 FALSE            4
    ## 2 Gunness.Thicket  3.8     0      Scrub     4.2 FALSE            6
    ## 3        Oak.Mead  3.1     2  Grassland     3.9 FALSE            2
    ## 4    Church.Field  3.5     3  Grassland     4.2 FALSE            3
    ## 5    North.Gravel  3.3     1  Grassland     4.1 FALSE            1
    ## 6    South.Gravel  3.7     2  Grassland     4.0 FALSE            2
    ## 7    Water.Meadow  3.9     0     Meadow     4.9  TRUE            8
    ## 8      Pound.Hill  4.4     2     Arable     4.5 FALSE            5

``` r
# assign this data to an object to "save" it
wormarea <- filter(worms, Area > 5 | Area < 2)
wormarea # to view data
```

    ##          Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1    Silwood.Bottom  5.1     2     Arable     5.2 FALSE            7
    ## 2       The.Orchard  1.9     0    Orchard     5.7 FALSE            9
    ## 3     Rookery.Slope  1.5     4  Grassland     5.0  TRUE            7
    ## 4 Observatory.Ridge  1.8     6  Grassland     3.8 FALSE            0
    ## 5         Farm.Wood  0.8    10      Scrub     5.1  TRUE            3

``` r
# use mutate to run a function across a dataframe
wormstransformed <- mutate(worms, logArea = log(Area))
head(wormstransformed) # to view data
```

    ##        Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density   logArea
    ## 1     Nashs.Field  3.6    11  Grassland     4.1 FALSE            4 1.2809338
    ## 2  Silwood.Bottom  5.1     2     Arable     5.2 FALSE            7 1.6292405
    ## 3   Nursery.Field  2.8     3  Grassland     4.3 FALSE            2 1.0296194
    ## 4     Rush.Meadow  2.4     5     Meadow     4.9  TRUE            5 0.8754687
    ## 5 Gunness.Thicket  3.8     0      Scrub     4.2 FALSE            6 1.3350011
    ## 6        Oak.Mead  3.1     2  Grassland     3.9 FALSE            2 1.1314021

``` r
# arrange the row order by vegetation type
arrange(worms, Vegetation)
```

    ##           Field.Name Area Slope Vegetation Soil.pH  Damp Worm.density
    ## 1     Silwood.Bottom  5.1     2     Arable     5.2 FALSE            7
    ## 2            Ashurst  2.1     0     Arable     4.8 FALSE            4
    ## 3         Pound.Hill  4.4     2     Arable     4.5 FALSE            5
    ## 4        Nashs.Field  3.6    11  Grassland     4.1 FALSE            4
    ## 5      Nursery.Field  2.8     3  Grassland     4.3 FALSE            2
    ## 6           Oak.Mead  3.1     2  Grassland     3.9 FALSE            2
    ## 7       Church.Field  3.5     3  Grassland     4.2 FALSE            3
    ## 8      Rookery.Slope  1.5     4  Grassland     5.0  TRUE            7
    ## 9       North.Gravel  3.3     1  Grassland     4.1 FALSE            1
    ## 10      South.Gravel  3.7     2  Grassland     4.0 FALSE            2
    ## 11 Observatory.Ridge  1.8     6  Grassland     3.8 FALSE            0
    ## 12        Gravel.Pit  2.9     1  Grassland     3.5 FALSE            1
    ## 13       Rush.Meadow  2.4     5     Meadow     4.9  TRUE            5
    ## 14        Pond.Field  4.1     0     Meadow     5.0  TRUE            6
    ## 15      Water.Meadow  3.9     0     Meadow     4.9  TRUE            8
    ## 16       The.Orchard  1.9     0    Orchard     5.7 FALSE            9
    ## 17   Gunness.Thicket  3.8     0      Scrub     4.2 FALSE            6
    ## 18       Garden.Wood  2.9    10      Scrub     5.2 FALSE            8
    ## 19         Cheapside  2.2     8      Scrub     4.7  TRUE            4
    ## 20         Farm.Wood  0.8    10      Scrub     5.1  TRUE            3

## PART V: Data Wrangling with the GBIF (5 points)

I included head() throughout so that the full dataframe does not print.

``` r
# import my GBIF data
chi_gbif <- read.csv('chicago_GBIF.csv')

# select only the columns we want
chi_gbif <- chi_gbif %>% 
  select(kingdom:decimalLongitude, eventDate:collectionCode) 

# check out the dataset 
glimpse(chi_gbif)
```

    ## Rows: 52,405
    ## Columns: 29
    ## $ kingdom                          <chr> "Animalia", "Animalia", "Animalia", "~
    ## $ phylum                           <chr> "Chordata", "Chordata", "Chordata", "~
    ## $ class                            <chr> "Aves", "Aves", "Aves", "Aves", "Aves~
    ## $ order                            <chr> "Charadriiformes", "Gaviiformes", "An~
    ## $ family                           <chr> "Laridae", "Gaviidae", "Anatidae", "A~
    ## $ genus                            <chr> "Larus", "Gavia", "Mergus", "Clangula~
    ## $ species                          <chr> "Larus glaucoides", "Gavia stellata",~
    ## $ infraspecificEpithet             <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ taxonRank                        <chr> "SPECIES", "SPECIES", "SPECIES", "SPE~
    ## $ scientificName                   <chr> "Larus glaucoides B.Meyer, 1822", "Ga~
    ## $ verbatimScientificName           <chr> "Larus glaucoides", "Gavia stellata",~
    ## $ verbatimScientificNameAuthorship <chr> "", "", "", "", "", "", "", "", "", "~
    ## $ countryCode                      <chr> "US", "US", "US", "US", "US", "US", "~
    ## $ locality                         <chr> "Olive Park, Chicago", "Olive Park, C~
    ## $ stateProvince                    <chr> "Illinois", "Illinois", "Illinois", "~
    ## $ occurrenceStatus                 <chr> "PRESENT", "PRESENT", "PRESENT", "PRE~
    ## $ individualCount                  <int> NA, 1, NA, NA, NA, NA, 1, NA, NA, NA,~
    ## $ publishingOrgKey                 <chr> "e2e717bf-551a-4917-bdc9-4fa0f342c530~
    ## $ decimalLatitude                  <dbl> 41.89289, 41.89289, 41.89289, 41.8928~
    ## $ decimalLongitude                 <dbl> -87.61185, -87.61185, -87.61185, -87.~
    ## $ eventDate                        <chr> "1982-02-21T00:00:00", "1982-02-21T00~
    ## $ day                              <int> 21, 21, 21, 21, 2, 2, 22, 22, 22, 22,~
    ## $ month                            <int> 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2~
    ## $ year                             <int> 1982, 1982, 1982, 1982, 1981, 1981, 1~
    ## $ taxonKey                         <int> 2481156, 2481958, 2498375, 2498273, 2~
    ## $ speciesKey                       <int> 2481156, 2481958, 2498375, 2498273, 2~
    ## $ basisOfRecord                    <chr> "HUMAN_OBSERVATION", "HUMAN_OBSERVATI~
    ## $ institutionCode                  <chr> "CLO", "CLO", "CLO", "CLO", "CLO", "C~
    ## $ collectionCode                   <chr> "EBIRD", "EBIRD", "EBIRD", "EBIRD", "~

``` r
# arrange the data by kingdom and show only kingdom and species
chi_gbif %>% 
  select(kingdom, species) %>% 
  arrange(kingdom) %>% 
  head()
```

    ##    kingdom               species
    ## 1                               
    ## 2 Animalia      Larus glaucoides
    ## 3 Animalia        Gavia stellata
    ## 4 Animalia       Mergus serrator
    ## 5 Animalia     Clangula hyemalis
    ## 6 Animalia Lophodytes cucullatus

``` r
# see how many observations were from each collection
chi_gbif %>% 
  group_by(collectionCode) %>% 
  tally() %>% 
  head()
```

    ## # A tibble: 6 x 2
    ##   collectionCode     n
    ##   <chr>          <int>
    ## 1 ""               171
    ## 2 "A"               11
    ## 3 "Bird"           114
    ## 4 "Birds"         2069
    ## 5 "Birds-Eggs"       1
    ## 6 "Botany"          54

``` r
# similarly, see how many observations of each species there are
# use sort to see what species is seen most often
chi_gbif %>% 
  count(species, sort = T) %>% 
  head()
```

    ##              species    n
    ## 1  Passer domesticus 1935
    ## 2      Columba livia 1656
    ## 3 Larus delawarensis 1622
    ## 4       Pieris rapae 1604
    ## 5   Danaus plexippus 1300
    ## 6   Colias eurytheme 1230

## PART VI: Final Brush with Tidyverse and dplyr (5 points)

I included head() throughout so that the full dataframe does not print.

``` r
# import the iris data
data(iris)
```

``` r
# a. Select rows using the filter() command
filter(iris, Petal.Length > 2) %>% 
  head()
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
    ## 1          7.0         3.2          4.7         1.4 versicolor
    ## 2          6.4         3.2          4.5         1.5 versicolor
    ## 3          6.9         3.1          4.9         1.5 versicolor
    ## 4          5.5         2.3          4.0         1.3 versicolor
    ## 5          6.5         2.8          4.6         1.5 versicolor
    ## 6          5.7         2.8          4.5         1.3 versicolor

``` r
# b. Select the same rows with the %>% pipe
iris %>% 
  filter(Petal.Length > 2) %>% 
  head()
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
    ## 1          7.0         3.2          4.7         1.4 versicolor
    ## 2          6.4         3.2          4.5         1.5 versicolor
    ## 3          6.9         3.1          4.9         1.5 versicolor
    ## 4          5.5         2.3          4.0         1.3 versicolor
    ## 5          6.5         2.8          4.6         1.5 versicolor
    ## 6          5.7         2.8          4.5         1.3 versicolor

``` r
# c. Use the filtered row and add a new condition that Sepal length must be greater than 7.5
iris %>% 
  filter(Petal.Length > 2 & Sepal.Length > 7.5)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
    ## 1          7.6         3.0          6.6         2.1 virginica
    ## 2          7.7         3.8          6.7         2.2 virginica
    ## 3          7.7         2.6          6.9         2.3 virginica
    ## 4          7.7         2.8          6.7         2.0 virginica
    ## 5          7.9         3.8          6.4         2.0 virginica
    ## 6          7.7         3.0          6.1         2.3 virginica

``` r
# d. Pick rows with filter()
iris %>%
  filter(Species == "setosa" & Sepal.Width > 2) %>% 
  head()
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

``` r
# e. Remove a column with select(-)
iris %>% 
  select(-Species) %>% 
  head()
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width
    ## 1          5.1         3.5          1.4         0.2
    ## 2          4.9         3.0          1.4         0.2
    ## 3          4.7         3.2          1.3         0.2
    ## 4          4.6         3.1          1.5         0.2
    ## 5          5.0         3.6          1.4         0.2
    ## 6          5.4         3.9          1.7         0.4

``` r
# f. Combine filter() for virginica and select sepal width greater than 3.5 with %>%
iris %>% 
  filter(Species == "virginica", Sepal.Width > 3.5)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
    ## 1          7.2         3.6          6.1         2.5 virginica
    ## 2          7.7         3.8          6.7         2.2 virginica
    ## 3          7.9         3.8          6.4         2.0 virginica

``` r
# g. Create a new column using the pipe and mutate() for 
# Sepal.Area = Sepal.Width * Sepal.Length
iris %>% 
  mutate(Sepal.Area = Sepal.Width * Sepal.Length) %>% 
  select(Sepal.Area, everything()) %>% 
  head()
```

    ##   Sepal.Area Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1      17.85          5.1         3.5          1.4         0.2  setosa
    ## 2      14.70          4.9         3.0          1.4         0.2  setosa
    ## 3      15.04          4.7         3.2          1.3         0.2  setosa
    ## 4      14.26          4.6         3.1          1.5         0.2  setosa
    ## 5      18.00          5.0         3.6          1.4         0.2  setosa
    ## 6      21.06          5.4         3.9          1.7         0.4  setosa

``` r
# h. Summarize the data using mean sepal width grouped by species
iris %>% 
  group_by(Species) %>% 
  summarize(mean_sepal_width = mean(Sepal.Width))
```

    ## # A tibble: 3 x 2
    ##   Species    mean_sepal_width
    ##   <fct>                 <dbl>
    ## 1 setosa                 3.43
    ## 2 versicolor             2.77
    ## 3 virginica              2.97

``` r
# i. Finally, which flowers have sepal areas less than 15, ordered by area? 
# Hint: You will need mutate(), filter() and arrange()
iris %>% 
  mutate(Sepal.Area = Sepal.Width * Sepal.Length) %>% 
  filter(Sepal.Area < 15) %>% 
  arrange(Sepal.Area) %>% 
  select(Sepal.Area, everything()) # so that Sepal.Area is the first column
```

    ##    Sepal.Area Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
    ## 1       10.00          5.0         2.0          3.5         1.0 versicolor
    ## 2       10.35          4.5         2.3          1.3         0.3     setosa
    ## 3       11.50          5.0         2.3          3.3         1.0 versicolor
    ## 4       11.76          4.9         2.4          3.3         1.0 versicolor
    ## 5       12.25          4.9         2.5          4.5         1.7  virginica
    ## 6       12.65          5.5         2.3          4.0         1.3 versicolor
    ## 7       12.75          5.1         2.5          3.0         1.1 versicolor
    ## 8       12.76          4.4         2.9          1.4         0.2     setosa
    ## 9       12.90          4.3         3.0          1.1         0.1     setosa
    ## 10      13.20          5.5         2.4          3.8         1.1 versicolor
    ## 11      13.20          5.5         2.4          3.7         1.0 versicolor
    ## 12      13.20          4.4         3.0          1.3         0.2     setosa
    ## 13      13.20          6.0         2.2          4.0         1.0 versicolor
    ## 14      13.20          6.0         2.2          5.0         1.5  virginica
    ## 15      13.64          6.2         2.2          4.5         1.5 versicolor
    ## 16      13.75          5.5         2.5          4.0         1.3 versicolor
    ## 17      14.00          5.6         2.5          3.9         1.1 versicolor
    ## 18      14.04          5.2         2.7          3.9         1.4 versicolor
    ## 19      14.08          4.4         3.2          1.3         0.2     setosa
    ## 20      14.25          5.7         2.5          5.0         2.0  virginica
    ## 21      14.26          4.6         3.1          1.5         0.2     setosa
    ## 22      14.30          5.5         2.6          4.4         1.2 versicolor
    ## 23      14.40          4.8         3.0          1.4         0.1     setosa
    ## 24      14.40          4.8         3.0          1.4         0.3     setosa
    ## 25      14.49          6.3         2.3          4.4         1.3 versicolor
    ## 26      14.70          4.9         3.0          1.4         0.2     setosa
    ## 27      14.72          4.6         3.2          1.4         0.2     setosa
    ## 28      14.82          5.7         2.6          3.5         1.0 versicolor
    ## 29      14.88          4.8         3.1          1.6         0.2     setosa

## PART VII: Just for Fun Extra Credit (5 points)

``` r
# Load the rgbif package
library(rgbif)
```

``` r
# Create an assignment operator to look at Osprey across Maine
osprey <- occ_data(scientificName="Pandion haliaetus", stateProvince = 'Maine')
```

``` r
# Make a map of Osprey data by State
# Load the maps library
library(maps)
```

``` r
# create 2-dimensional data with latitude and longitude coordinates for all of the oak occurrence data
coords <- osprey$data[ , c("decimalLongitude", "decimalLatitude")]

# check the first several rows of data to make sure you did this correctly
head(coords)
```

    ## # A tibble: 6 x 2
    ##   decimalLongitude decimalLatitude
    ##              <dbl>           <dbl>
    ## 1            -68.7            44.9
    ## 2            -70.1            43.8
    ## 3            -70.0            43.8
    ## 4            -69.9            43.9
    ## 5            -68.7            44.9
    ## 6            -69.0            44.2

``` r
# create a map of the world with occurrence data
map('county', 'maine')  

# create points with the occurrence data
points(coords[ , c("decimalLongitude", "decimalLatitude")], pch = 19, cex = 1, col = "green")
```

![](britton_a_assignment4_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->
