project5
================
Jason Grahn
2/16/2019

``` r
library(CCA)
library(tidyverse)
library(GGally)
library(yacca)
```

Use R to solve Chapter 11 Page 402: \#11.9
==========================================

Make sure you include the commands and outputs, as well as the interpretations of the outputs.

11.9 Use the sons data of Table 3.8.

``` r
sons <- read.table(here::here("assignment06/T3_8_SONS.DAT"))  %>% 
  rename(y1 = V1, # head length
         y2 = V2, # head breadth
         x1 = V3, # head length
         x2 = V4) # head breadth

head(sons, 5)
```

    ##    y1  y2  x1  x2
    ## 1 191 155 179 145
    ## 2 195 149 201 152
    ## 3 181 148 185 149
    ## 4 183 153 188 149
    ## 5 176 144 171 142

``` r
ggpairs(sons)
```

![](project5_files/figure-markdown_github/import%20sons%20data-1.png)

``` r
#subgroup the data into first and second sons
first.son <- sons %>% select(y1, y2)
second.son <- sons %>% select(x1, x2)
```

``` r
cca_output <- cca(first.son, second.son)
cc_output <- cc(first.son, second.son)
```

(a) Find the canonical correlations between (*y*<sub>1</sub>,*y*<sub>2</sub>) and (*x*<sub>1</sub>, *x*<sub>1</sub>)·
---------------------------------------------------------------------------------------------------------------------

``` r
cca_output$corr[1]
```

    ##      CV 1 
    ## 0.7885079

*r*<sub>1</sub> is 0.7885079 and *r*<sub>2</sub> is 0.0537397

(b) Find the standardized coefficients for the canonical variates.
==================================================================

``` r
cca_output[3:4]
```

    ## $xcoef
    ##           CV 1       CV 2
    ## y1 -0.05656620 -0.1399711
    ## y2 -0.07073683  0.1869496
    ## 
    ## $ycoef
    ##          CV 1       CV 2
    ## x1 -0.0502426 -0.1761479
    ## x2 -0.0802224  0.2620836

(c) Test the significance of each canonical correlation.
========================================================

``` r
F.test.cca(cca_output)
```

    ## 
    ##  F Test for Canonical Correlations (Rao's F Approximation)
    ## 
    ##          Corr        F   Num df Den df    Pr(>F)    
    ## CV 1 0.788508 6.597193 4.000000     42 0.0003256 ***
    ## CV 2 0.053740 0.063719 1.000000     22 0.8030550    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The output of the F-test shows us the
