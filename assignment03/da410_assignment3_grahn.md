DA410 Assignment 3
================
Jason Grahn
1/20/2019

Homework 3
==========

6.27
----

Baten, Tack, and Baeder (1958) compared judges' scores on fish prepared by three methods. Twelve fish were cooked by each method, and several judges tasted fish samples and rated each on four variables: *y*<sub>1</sub> = aroma, *y*<sub>2</sub> = flavor, *y*<sub>3</sub> = texture, and *y*<sub>4</sub> = moisture. The data are in Table 6.17. Each entry is an average score for the judges on that fish.

``` r
fish <- 
    readr::read_table2(file = here::here("/assignment03/T6_17_FISH.DAT"),
                   col_names = FALSE) %>% 
  select(X1:X5) %>% 
  mutate(X1 = factor(X1))
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_double(),
    ##   X3 = col_double(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_character()
    ## )

``` r
head(fish,10)
```

    ## # A tibble: 10 x 5
    ##    X1       X2    X3    X4    X5
    ##    <fct> <dbl> <dbl> <dbl> <dbl>
    ##  1 1       5.4   6     6.3   6.7
    ##  2 1       5.2   6.5   6     5.8
    ##  3 1       6.1   5.9   6     7  
    ##  4 1       4.8   5     4.9   5  
    ##  5 1       5     5.7   5     6.5
    ##  6 1       5.7   6.1   6     6.6
    ##  7 1       6     6     5.8   6  
    ##  8 1       4     5     4     5  
    ##  9 1       5.7   5.4   4.9   5  
    ## 10 1       5.6   5.2   5.4   5.8

### (a)

Compare the three methods using all four MANOVA tests.

``` r
manova.data <- manova(cbind(X2 ,X3, X4, X5) ~ X1,
                     data=fish)

broom::tidy(manova.data)
```

    ## # A tibble: 2 x 7
    ##   term         df pillai statistic num.df den.df    p.value
    ##   <chr>     <dbl>  <dbl>     <dbl>  <dbl>  <dbl>      <dbl>
    ## 1 X1            2  0.860      5.84      8     62  0.0000146
    ## 2 Residuals    33 NA         NA        NA     NA NA

#### Wilks' test

``` r
fish.lambda <- summary(manova.data, test = "Wilks")
fish.lambda
```

    ##           Df   Wilks approx F num Df den Df    Pr(>F)    
    ## X1         2 0.22449   8.3294      8     60 1.609e-07 ***
    ## Residuals 33                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Wilks' *Λ* is low, so we can reject *H*<sub>0</sub>.

#### Pillai test

``` r
fish.pillai <- summary(manova.data, test = "Pillai")
fish.pillai
```

    ##           Df  Pillai approx F num Df den Df    Pr(>F)    
    ## X1         2 0.85987    5.845      8     62 1.465e-05 ***
    ## Residuals 33                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Pillai test is low, so we can reject *H*<sub>0</sub>.

#### Roy test

``` r
fish.roy <- summary(manova.data, test = "Roy")
fish.roy 
```

    ##           Df    Roy approx F num Df den Df    Pr(>F)    
    ## X1         2 2.9515   22.874      4     31 7.077e-09 ***
    ## Residuals 33                                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#roy value is 137.168, which we convert...
2.9515/(1+2.9515)
```

    ## [1] 0.7469315

We can reject *H*<sub>0</sub> for Roy as well.

#### Hotelling-Lawley test:

``` r
fish.hotel <- summary(manova.data, test = "Hotelling-Lawley")
fish.hotel
```

    ##           Df Hotelling-Lawley approx F num Df den Df    Pr(>F)    
    ## X1         2           3.0788   11.161      8     58 2.161e-09 ***
    ## Residuals 33                                                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*H*<sub>0</sub> for Hotelling-Lawley test is also low, so that can be rejected.

``` r
summary.aov(manova.data)
```

    ##  Response X2 :
    ##             Df  Sum Sq Mean Sq F value Pr(>F)
    ## X1           2  1.0506 0.52528  1.2928  0.288
    ## Residuals   33 13.4083 0.40631               
    ## 
    ##  Response X3 :
    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## X1           2   4.88 2.44000  9.4953 0.000553 ***
    ## Residuals   33   8.48 0.25697                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response X4 :
    ##             Df  Sum Sq Mean Sq F value  Pr(>F)  
    ## X1           2  2.3822 1.19111  3.3863 0.04596 *
    ## Residuals   33 11.6075 0.35174                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response X5 :
    ##             Df  Sum Sq Mean Sq F value Pr(>F)
    ## X1           2  0.8106 0.40528  1.2658 0.2954
    ## Residuals   33 10.5658 0.32018

6.28
----

Table 6.18 from Keuls et al. (1984) gives data from a two-way (fixed-effects) MANOVA on snap beans showing the results of four variables: *y*<sub>1</sub> = yield earliness, *y*<sub>2</sub> = specific leaf area (SLA) earliness, *y*<sub>3</sub> = total yield, and *y*<sub>4</sub> = average SLA. The factors are sowing date (S) and variety (V).

``` r
snapbeans <- 
  readr::read_table2(file = here::here("/assignment03/T6_18_SNAPBEAN.DAT"),
                   col_names = FALSE) %>% 
  select(X1:X7) %>% 
  rename(S = X1,
         V = X2,
         pl = X3,
         y1 = X4,
         y2 = X5,
         y3 = X6,
         y4 = X7) %>% 
  mutate(S = factor(S),
         V = factor(V),
         pl = factor(pl))
```

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   X2 = col_double(),
    ##   X3 = col_double(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_logical()
    ## )

``` r
head(snapbeans, 5)
```

    ## # A tibble: 5 x 7
    ##   S     V     pl       y1    y2    y3    y4
    ##   <fct> <fct> <fct> <dbl> <dbl> <dbl> <dbl>
    ## 1 1     1     1      59.3   4.5  38.4   295
    ## 2 1     1     2      60.3   3.5  38.6   302
    ## 3 1     1     3      60.9   5.3  37.2   318
    ## 4 1     1     4      60.6   5.8  38.1   345
    ## 5 1     1     5      60.4   6    38.8   325

``` r
manova.beans <- manova(cbind(y1 ,y2, y3, y4) ~ S * V,
                       data=snapbeans)

manova.beans
```

    ## Call:
    ##    manova(cbind(y1, y2, y3, y4) ~ S * V, data = snapbeans)
    ## 
    ## Terms:
    ##                        S        V      S:V Residuals
    ## resp 1            728.79   124.52    30.29     11.90
    ## resp 2            192.87     5.69     5.12     14.40
    ## resp 3            747.78     8.40     5.87     13.66
    ## resp 4          33469.38  8188.23  1887.77   7245.60
    ## Deg. of Freedom        3        2        6        48
    ## 
    ## Residual standard errors: 0.4978286 0.5477986 0.5333854 12.28617
    ## Estimated effects may be unbalanced

### (a)

Test for main effects and interaction using all four MANOVA statistics.

#### Wilks’ Test

(this is a kind of test to measure if means are equal)

``` r
beans.lambda <- summary(manova.beans, test = "Wilks")
beans.lambda
```

    ##           Df    Wilks approx F num Df den Df    Pr(>F)    
    ## S          3 0.000645  149.831     12 119.35 < 2.2e-16 ***
    ## V          2 0.065300   32.775      8  90.00 < 2.2e-16 ***
    ## S:V        6 0.137947    5.039     24 158.20 1.611e-10 ***
    ## Residuals 48                                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Wilks *Λ* for S is low, reject *H*<sub>0</sub>. Wilks *Λ* for V is low, reject *H*<sub>0</sub>. Wilks *Λ* for the S\*V interaction is low, reject *H*<sub>0</sub>.

#### Pillai test

``` r
beans.pillai <- summary(manova.beans, test = "Pillai")
beans.pillai
```

    ##           Df Pillai approx F num Df den Df    Pr(>F)    
    ## S          3 2.3568   43.052     12    141 < 2.2e-16 ***
    ## V          2 1.1070   14.256      8     92 2.564e-13 ***
    ## S:V        6 1.3213    3.946     24    192 3.912e-08 ***
    ## Residuals 48                                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

*H*<sub>0</sub> is rejected in Pillai's test as well.

#### Roy test

``` r
beans.roy <- summary(manova.beans, test = "Roy")

#roy values are 137.168, 11.445, and 2.649
S <- 137.168/(1+137.168)
V <- 11.445/(1+11.445)
SV <- 2.649/(1+2.649)

S
```

    ## [1] 0.9927624

``` r
V
```

    ## [1] 0.9196464

``` r
SV
```

    ## [1] 0.7259523

*H*<sub>0</sub> is rejected with Roy's test.

#### Hotelling-Lawley test:

``` r
beans.hotel <- summary(manova.beans, test = "Hotelling-Lawley")
beans.hotel
```

    ##           Df Hotelling-Lawley approx F num Df den Df    Pr(>F)    
    ## S          3          142.304   517.83     12    131 < 2.2e-16 ***
    ## V          2           11.675    64.21      8     88 < 2.2e-16 ***
    ## S:V        6            3.450     6.25     24    174 8.671e-14 ***
    ## Residuals 48                                                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

And finally, we reject *H*<sub>0</sub> with Hotelling-Lawley test.
