da410\_project2
================
Jason Grahn
1/23/2019

Part 1:
=======

The problem statement
---------------------

Use Hotelling’s T^2 test to test for a difference in the mean score vector of the boys and the mean vector of the girls. Make sure you include clear command lines and relevant output/results with hypotheses, test result(s) and conclusion(s)/interpretation(s).

The data
--------

Download testscoredata.txt and read it in R

``` r
scores <- 
  readr::read_table2(file = here::here("/project2/testscoredata.txt"),
                   col_names = TRUE) %>% 
  mutate(subject = factor(subject),
         sex = factor(sex))

head(scores,5)
```

    ## # A tibble: 5 x 4
    ##   subject  math reading sex  
    ##   <fct>   <dbl>   <dbl> <fct>
    ## 1 1        83.2    79.7 boy  
    ## 2 2       103.    101.  boy  
    ## 3 3        81.6    80.5 boy  
    ## 4 4        88.2    84.6 boy  
    ## 5 5        81.5    76.5 boy

The hypothesis
--------------

> *H*<sub>0</sub>: The mean scores between sexes is equal ( *μ*<sub>*o*</sub> = *μ*<sub>1</sub>).

> *H*<sub>*a*</sub>: The mean scores between sexes is not equal ( *μ*<sub>*o*</sub> ≠ *μ*<sub>1</sub>).

``` r
#build the manova binding
scores.manova <- stats::manova(cbind(math, reading) ~ sex, scores)
scores.manova
```

    ## Call:
    ##    stats::manova(cbind(math, reading) ~ sex, scores)
    ## 
    ## Terms:
    ##                       sex Residuals
    ## resp 1             19.976  1621.466
    ## resp 2             3.7684 1663.6793
    ## Deg. of Freedom         1        60
    ## 
    ## Residual standard errors: 5.198502 5.265737
    ## Estimated effects may be unbalanced

We see that *resp 1* has a mean value of 19.976, while *resp 2* has a mean value of 3.768. Our initial impression is that the mean values are not equal.

The test
--------

``` r
#use the HL method to test results.
tidy.score.hotel <- broom::tidy(scores.manova, 
                              test = "Hotelling-Lawley",
                              intercept = FALSE)
```

The result:
-----------

``` r
tidy.score.hotel
```

    ## # A tibble: 2 x 7
    ##   term         df     hl statistic num.df den.df   p.value
    ##   <chr>     <dbl>  <dbl>     <dbl>  <dbl>  <dbl>     <dbl>
    ## 1 sex           1  0.306      9.02      2     59  0.000381
    ## 2 Residuals    60 NA         NA        NA     NA NA

The conclusion
--------------

With a *p* − *v**a**l**u**e* of 3.805224610^{-4}, we can reject *H*<sub>0</sub> and conclude that the mean scores between the sexes is not equal.

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

Part 2:
=======

The problem statement
---------------------

Suppose we have gathered the following data on female athletes in three sports. The measurements we have made are the athletes' heights and vertical jumps, both in inches. The data are listed as (height, jump) as follows:

    Basketball Players: (66, 27), (65, 29), (68, 26), (64, 29), (67, 29)
    Track Athletes:     (63, 23), (61, 26), (62, 23), (60, 26)
    Softball Players:   (62, 23), (65, 21), (63, 21), (62, 23), (63.5, 22), (66, 21.5)

-   Use R to conduct the MANOVA F-test using Wilks' Lambda to test for a difference in (height, jump) mean vectors across the three sports. Make sure you include clear command lines and relevant output/results with hypotheses, test result(s) and conclusion(s)/interpretation(s)
-   State the assumptions of your test and check to see whether assumptions are met. Do you believe your inference is valid? Why or why not?
-   Use R to examine the sample mean vectors for each group. Make sure you include clear command lines and relevant output/results. Also comment on the differences among the groups in terms of the specific variables.

The data
--------

Download testscoredata.txt and read it in R

``` r
sport  <- as.factor(c('B','B','B','B','B','T','T','T','T','S','S','S','S','S','S'))
height <- c(66,65,68,64,67,63,61,62,60,62,65,63,62,63.5,66)
jump   <- c(27,29,26,29,29,23,26,23,26,23,21,21,23,22,21.5)
```

The hypothesis
--------------

> *H*<sub>0</sub>: The mean scores between sexes is equal ( *μ*<sub>*o*</sub> = *μ*<sub>1</sub>).

> *H*<sub>*a*</sub>: The mean scores between sexes is not equal ( *μ*<sub>*o*</sub> ≠ *μ*<sub>1</sub>).

``` r
sports.manova <- stats::manova(cbind(height, jump) ~ sport, scores)
sports.manova
```

    ## Call:
    ##    stats::manova(cbind(height, jump) ~ sport, scores)
    ## 
    ## Terms:
    ##                     sport Residuals
    ## resp 1           45.62500  28.20833
    ## resp 2          101.02500  21.20833
    ## Deg. of Freedom         2        12
    ## 
    ## Residual standard errors: 1.533197 1.329421
    ## Estimated effects may be unbalanced

The test
--------

The result:
-----------

The conclusion
--------------

With a *p* − *v**a**l**u**e* of 3.805224610^{-4}, we can reject *H*<sub>0</sub> and conclude that the mean scores between the sexes is not equal.
