britton_a\_assignment9
================
Annie Britton
3/27/2023

### **PART I: Clustering (10 points)**

The task is to read and digest the info on the abundance-based
dissimilarity index and run the analysis on the Ants data set
(ants.RData) (posted on Blackboard) from Hoffman (2003). The ants data
show the abundance of ants under several fire regimes. There are also
two sorts of soil type (black and red) denoted by a letter in the sample
name.

``` r
# Load vegan package
library(vegan)
# Load tidyverse
library(tidyverse)
```

``` r
# load in the ants data using the file path
ants <- "C:\\Users\\annie\\Documents\\AEEN\\M09\\ants.RData"
load(ants)

# take a quick look at the top of the data
head(ants)
```

    ##          E2b E3b L2b L3b Ub E2r E3r L2r L3r Ur
    ## Cera.spA   0   0   0   0  0   0   3   1   0  0
    ## Cera.spB   0   0   0   0  0   1   0   0   0  0
    ## Cera.spC   0   0   0   0  0   0   1   0   0  0
    ## Cera.spD   0   0   0   0  0   1   0   0   0  0
    ## Cera.spE   0   0   1   0  0   0   0   0   0  0
    ## Cer.spH    0   0   0   0  0   1   0   0   0  0

``` r
antfire <- t(ants) # transpose the data

# Apply dissimilarity index ‘manhattan’
d <- dist(antfire, method = "manhattan")
d # print the index
```

    ##      E2b  E3b  L2b  L3b   Ub  E2r  E3r  L2r  L3r
    ## E3b  181                                        
    ## L2b  277  276                                   
    ## L3b  219  210  296                              
    ## Ub   495  506  554  406                         
    ## E2r 1282 1325 1341 1337 1607                    
    ## E3r 1320 1345 1285 1367 1483  860               
    ## L2r 1352 1319 1279 1383 1655  914  872          
    ## L3r 1451 1458 1428 1424 1532 1055  767  819     
    ## Ur  1254 1243 1269 1273 1573  670  864  906  965

``` r
# Make a cluster dendrogram based on the manhattan method
h <- hclust(d)
plot(h)
```

![](britton_a_assignment9_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The ‘b’ and ‘r’ at the end of the sample names indicate the types of
soil (black or red) that the community of ants is found in. It’s clear
in the above that there is a split between communities based on soil
type.

``` r
antfire <- t(ants) # transpose the data

# Apply dissimilarity index ‘bray’
d <- vegdist(antfire, method = "bray")
d # print the index
```

    ##           E2b       E3b       L2b       L3b        Ub       E2r       E3r
    ## E3b 0.1282778                                                            
    ## L2b 0.1828383 0.1862348                                                  
    ## L3b 0.1759036 0.1732673 0.2249240                                        
    ## Ub  0.4156171 0.4369603 0.4389857 0.4092742                              
    ## E2r 0.5111643 0.5353535 0.5199690 0.5790385 0.7126386                    
    ## E3r 0.5309735 0.5483082 0.5025420 0.5977263 0.6641290 0.2422535          
    ## L2r 0.5252525 0.5190870 0.4835539 0.5823158 0.7130547 0.2512369 0.2411504
    ## L3r 0.5829650 0.5936482 0.5578125 0.6218341 0.6851521 0.2969322 0.2172189
    ## Ur  0.5114192 0.5138487 0.5029727 0.5650244 0.7153251 0.1905575 0.2472811
    ##           L2r       L3r
    ## E3b                    
    ## L2b                    
    ## L3b                    
    ## Ub                     
    ## E2r                    
    ## E3r                    
    ## L2r                    
    ## L3r 0.2263056          
    ## Ur  0.2529313 0.2759508

``` r
# Make a cluster dendrogram based on the bray method
h <- hclust(d)
plot(h)
```

![](britton_a_assignment9_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Again, one can see from the results that there is an obvious split
between the communities from brown soil and those from red soil.

``` r
# Try using the Euclidean method (replace the manhattan command)
antfire <- t(ants) # transpose the data

# Apply dissimilarity index ‘euclidean’
d <- dist(antfire, method = "euclidean")
d # print the index
```

    ##           E2b       E3b       L2b       L3b        Ub       E2r       E3r
    ## E3b  49.76947                                                            
    ## L2b  60.35727  53.62835                                                  
    ## L3b  70.44856  51.69139  71.11962                                        
    ## Ub  258.20728 250.09198 248.45925 219.50854                              
    ## E2r 317.77036 332.34470 324.23603 349.65840 431.52173                    
    ## E3r 297.70791 304.24825 290.70432 320.18276 359.34524 186.24715          
    ## L2r 347.04178 344.73903 329.35847 363.52304 431.03480 229.67804 180.18324
    ## L3r 363.45976 356.86972 346.87173 368.31508 389.58696 243.26323 170.33203
    ## Ur  343.47926 341.37223 341.64748 363.79802 440.70512 183.49387 189.79989
    ##           L2r       L3r
    ## E3b                    
    ## L2b                    
    ## L3b                    
    ## Ub                     
    ## E2r                    
    ## E3r                    
    ## L2r                    
    ## L3r 183.29484          
    ## Ur  245.25497 212.31816

``` r
# Make a cluster dendrogram based on the euclidean method
h <- hclust(d)
plot(h)
```

![](britton_a_assignment9_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Using the euclidean distance metric, one can again see the same split
based on soil type between ant communities.

### **PART II: PCA with IRIS Data (10 points)**

#### **Iris Take One:**

This assignment will help to demonstrate how to apply and visualize a
PCA in R. PCA (Principal Components Analysis) is easy in R, but the
standard biplot() function is a little clunky. The vegan package can do
PCA using the rda() function (normally for redundancy analysis) and has
some nice plotting functions. (Note that ggplot is also developing
biplot tools).

``` r
# load and summarize the iris package
data("iris")
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
# "[,-5]" drops that last column which is the species code
# rda = redundancy analysis
# Normally RDA is used for “constrained ordination” without predictors, RDA is the # same as PCA.
my.rda <- rda(iris[,-5])

# Make a basic plot
biplot(my.rda, display = c("sites", "species"),
type = c("text", "points"))

# vegan has lots of helpful functions for plotting ordinations.
# ordihull: convex polygons around groups of points
# ordiellipse: ellipses around centroids (SD or SE)

# Add "hulls", used to draw convex polygons around each group of points, which can help visualize the separation between the different species.
ordihull(my.rda, group = iris$Species)
```

![](britton_a_assignment9_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

This plot shows the results of the RDA performed on the iris dataset,
which contains measurements of sepal length, sepal width, petal length,
and petal width for three different species of irises. It shows both the
species and the variables on the same plot, with each point representing
a flower. The direction and length of the arrows indicate the
contribution of each variable to the ordination, while the position of
the points represents the species.

``` r
# Summary method
head(summary(my.rda))
```

    ## 
    ## Call:
    ## rda(X = iris[, -5]) 
    ## 
    ## Partitioning of variance:
    ##               Inertia Proportion
    ## Total           4.573          1
    ## Unconstrained   4.573          1
    ## 
    ## Eigenvalues, and their contribution to the variance 
    ## 
    ## Importance of components:
    ##                          PC1     PC2     PC3      PC4
    ## Eigenvalue            4.2282 0.24267 0.07821 0.023835
    ## Proportion Explained  0.9246 0.05307 0.01710 0.005212
    ## Cumulative Proportion 0.9246 0.97769 0.99479 1.000000
    ## 
    ## Scaling 2 for species and site scores
    ## * Species are scaled proportional to eigenvalues
    ## * Sites are unscaled: weighted dispersion equal on all dimensions
    ## * General scaling constant of scores:  5.109117 
    ## 
    ## 
    ## Species scores
    ## 
    ##                  PC1      PC2      PC3     PC4
    ## Sepal.Length  1.7754 -0.77277  0.38889  0.1164
    ## Sepal.Width  -0.4152 -0.85936 -0.39950 -0.1179
    ## Petal.Length  4.2086  0.20405 -0.05094 -0.1770
    ## Petal.Width   1.7602  0.08884 -0.36470  0.2780
    ## 
    ## 
    ## Site scores (weighted sums of species scores)
    ## 
    ##          PC1     PC2      PC3       PC4
    ## sit1 -0.5464 -0.2714  0.04178  0.006134
    ## sit2 -0.5525  0.1504  0.31499  0.268470
    ## sit3 -0.5881  0.1232 -0.02679  0.054136
    ## sit4 -0.5588  0.2704 -0.04723 -0.204893
    ## sit5 -0.5554 -0.2776 -0.13482 -0.166078
    ## sit6 -0.4643 -0.6299 -0.25245 -0.065611
    ## ....

The first principal component (PC1) represents the direction in the data
that has the largest variance, while the second principal component
(PC2) represents the direction with the second largest variance. Look at
the importance of the components and notices that PCA axis one explains
92% of the variation!

``` r
# Take a look at the loadings of each variable on the output of the PCA. For example, if you want to view the loadings of the variables on PC1, you can look at the first column of the rotation matrix. The absolute value of each loading represents the strength of the correlation between the variable and PC1. Positive loadings indicate a positive relationship, while negative loadings indicate a negative relationship.

# Perform PCA on the dataset
pca <- prcomp(iris[, 1:4])

# View the loadings of each variable on the principal components
pca$rotation
```

    ##                      PC1         PC2         PC3        PC4
    ## Sepal.Length  0.36138659 -0.65658877  0.58202985  0.3154872
    ## Sepal.Width  -0.08452251 -0.73016143 -0.59791083 -0.3197231
    ## Petal.Length  0.85667061  0.17337266 -0.07623608 -0.4798390
    ## Petal.Width   0.35828920  0.07548102 -0.54583143  0.7536574

Based on these results, Petal.Length and Petal.Width have high loadings
on PC1 (0.857 and 0.358, respectively), while Sepal.Length has a
moderate loading (0.361) and Sepal.Width has a low loading (-0.085).
This indicates that PC1 is primarily influenced by the Petal.Length and
Petal.Width variables, and to a lesser extent by the Sepal.Length
variable. The negative loading for Sepal.Width indicates that it has a
negative relationship with PC1.

#### **Iris Take Two:**

The following is a very brief explanation of how the PCA rotates data to
maximize its variance along orthogonal axes.

``` r
# Grouping Data
# First we create a small plotting helper that we can reuse with new data; we’ll use # coord_fixed to better illustrate the variability across the two dimensions.
plot = function (data) {
data = data %>%
as.data.frame() %>%  
mutate(Species = iris$Species)
cols = as.list(setNames(colnames(data), c('x', 'y', 'color')))
ggplot(data, do.call(aes_string, cols)) + geom_point() + coord_fixed()
}

# Original iris distribution (To see the data before its transformation, let’s plot the original iris distribution of petal width and length)
petal_data = select(iris, Petal.Width, Petal.Length)
plot(petal_data)
```

![](britton_a_assignment9_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# Rotate the data by performing PCA
pca = prcomp(petal_data)
rotated_petals = as.data.frame(pca$x)

# And plot the rotated data
plot(rotated_petals)
```

![](britton_a_assignment9_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Rotating the data by performing PCA and plotting the rotated data can
help in visualizing the relationship between the variables in a new
coordinate system that better represents the variability of the data. In
this case, the original data consists of the petal width and length
measurements of irises.

The rotated data allows us to see the relationship between the two
variables in the new coordinate system defined by the principal
components. The rotated data shows that the petal length and width are
highly correlated and form a clear cluster, while the setosa species is
separated from the other two species.

Overall, rotating the data through PCA can help in revealing patterns
and relationships that may not be easily visible in the original
coordinate system.
