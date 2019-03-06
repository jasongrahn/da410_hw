da410\_project07\_grahn
================
Jason Grahn
3/5/2019

Use R to solve Chapter 13 Page 476, \#13.7(a).
==============================================

Make sure you include the commands and outputs, as well as the interpretations of the outputs.

Problem 13.7(a)
---------------

### 13.7 Use the words data of Table 5.9.

#### (a) Obtain principal component loadings for two factors.

``` r
data <- read.table(here::here("assignment08/T5_9_ESSAY.DAT")) %>% 
  rename(student = V1,
         y1 = V2, # informal_words
         y2 = V3, # informal_verbs
         x1 = V4, # formal_words
         x2 = V5) %>% # formal_verbs
  select(y1:x2)

head(data,3)
```

    ##    y1 y2  x1 x2
    ## 1 148 20 137 15
    ## 2 159 24 164 25
    ## 3 144 19 224 27

``` r
# Using Principal Components Analysis method with correclation matrix
fit <- principal(data, 
                 nfactors = 2, 
                 rotate = 'none', 
                 covar = FALSE)
fit
```

    ## Principal Components Analysis
    ## Call: principal(r = data, nfactors = 2, rotate = "none", covar = FALSE)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##     PC1   PC2   h2    u2 com
    ## y1 0.80 -0.54 0.93 0.070 1.7
    ## y2 0.86 -0.33 0.84 0.161 1.3
    ## x1 0.88  0.27 0.85 0.147 1.2
    ## x2 0.71  0.66 0.94 0.057 2.0
    ## 
    ##                        PC1  PC2
    ## SS loadings           2.67 0.90
    ## Proportion Var        0.67 0.22
    ## Cumulative Var        0.67 0.89
    ## Proportion Explained  0.75 0.25
    ## Cumulative Proportion 0.75 1.00
    ## 
    ## Mean item complexity =  1.6
    ## Test of the hypothesis that 2 components are sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.07 
    ##  with the empirical chi square  0.95  with prob <  NA 
    ## 
    ## Fit based upon off diagonal values = 0.98

\`\`\`
