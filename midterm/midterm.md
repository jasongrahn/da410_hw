midterm
================
Jason Grahn
2/9/2019

4
=

In the following table, we have a comparison of four reagents. The first reagent is the one presently in use and the other three are less expensive reagents that we wish to compare with the first. All four reagents are used with a blood sample from each patient.

The three variables measured for each reagent are 𝑦1=white blood count, 𝑦2=red blood count, and 𝑦3=hemoglobin count. Compare the four reagents using all four MANOVA tests. State each hypotheses clearly, and interpret the results.

``` r
reagent <- readr::read_table2(file = here::here("/midterm/T6_19_REAGENT.DAT"),
                   col_names = FALSE) %>% 
  rename(reagent = X1,
         subject = X2,
         y1 = X3,
         y2 = X4,
         y3 = X5) %>% 
  mutate(reagent = factor(reagent),
         subject = factor(subject))

head(reagent,5)
```

    ## # A tibble: 5 x 5
    ##   reagent subject    y1    y2    y3
    ##   <fct>   <fct>   <dbl> <dbl> <dbl>
    ## 1 1       1         8    3.96  12.5
    ## 2 1       2         4    5.37  16.9
    ## 3 1       3         6.3  5.47  17.1
    ## 4 1       4         9.4  5.16  16.2
    ## 5 1       5         8.2  5.16  17

``` r
manova.reagent <- manova(cbind(y1, y2, y3) ~ reagent,
                         data=reagent)

manova.reagent
```

    ## Call:
    ##    manova(cbind(y1, y2, y3) ~ reagent, data = reagent)
    ## 
    ## Terms:
    ##                   reagent Residuals
    ## resp 1              0.955   225.163
    ## resp 2            0.06295  14.92915
    ## resp 3            2.53938 171.67050
    ## Deg. of Freedom         3        76
    ## 
    ## Residual standard errors: 1.721241 0.4432111 1.502938
    ## Estimated effects may be unbalanced

The Hypothesis
--------------

> *H*<sub>0</sub>: The mean vectors between reagents are equal ( *μ*<sub>*o*</sub> = *μ*<sub>1</sub>). *H*<sub>*a*</sub>: The mean vectors between reagents are not equal ( *μ*<sub>*o*</sub> ≠ *μ*<sub>1</sub>).

The tests
---------

``` r
reagent.lambda <- broom::tidy(manova.reagent, test = "Wilks")
reagent.lambda
```

    ## # A tibble: 2 x 7
    ##   term         df  wilks statistic num.df den.df p.value
    ##   <chr>     <dbl>  <dbl>     <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 reagent       3  0.873      1.15      9   180.   0.328
    ## 2 Residuals    76 NA         NA        NA    NA   NA

### Wilks interpretation

p-Value of reagent.lambda is 0.3280951. This value is above 0.05; so we do not reject *H*<sub>0</sub> and conclude that the means are equal.

``` r
reagent.pillai <- broom::tidy(manova.reagent, test = "Pillai")
reagent.pillai
```

    ## # A tibble: 2 x 7
    ##   term         df pillai statistic num.df den.df p.value
    ##   <chr>     <dbl>  <dbl>     <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 reagent       3  0.128      1.13      9    228   0.345
    ## 2 Residuals    76 NA         NA        NA     NA  NA

### Pillai interpretation

p-Value of reagent.pillai is 0.3453898. This value is above 0.05; so we do not reject *H*<sub>0</sub> and conclude that the means are equal.

``` r
reagent.hotel <- broom::tidy(manova.reagent, test = "Hotelling-Lawley")
reagent.hotel
```

    ## # A tibble: 2 x 7
    ##   term         df     hl statistic num.df den.df p.value
    ##   <chr>     <dbl>  <dbl>     <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 reagent       3  0.146      1.18      9    218   0.312
    ## 2 Residuals    76 NA         NA        NA     NA  NA

### Hotelling interpretation

p-Value of reagent.hotel is 0.3117863. This value is above 0.05; so we do not reject *H*<sub>0</sub> and conclude that the means are equal.

``` r
reagent.roy <- broom::tidy(manova.reagent, test = "Roy",
                           intercept = FALSE)

reagent.roy
```

    ## # A tibble: 2 x 7
    ##   term         df    roy statistic num.df den.df p.value
    ##   <chr>     <dbl>  <dbl>     <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 reagent       3  0.143      3.63      3     76  0.0166
    ## 2 Residuals    76 NA         NA        NA     NA NA

``` r
roy.theta <- reagent.roy %>%  #use that tidy tibble
  filter(term != "Residuals") %>%  #filter out residuals because I dont need them
  select(term, roy) %>% #keep the columns I want
  mutate(roys_theta = round((roy / (1 + roy)), 3)) #make roy's theta
roy.theta
```

    ## # A tibble: 1 x 3
    ##   term      roy roys_theta
    ##   <chr>   <dbl>      <dbl>
    ## 1 reagent 0.143      0.125

p-Value of reagent.roy is 0.0165718. While this value is below 0.05; Roy's Theta is 0.125 so we do not reject *H*<sub>0</sub> and conclude that the means are equal.

5
=

6
=

7
=

8
=

The following table contains data from O’Sullivan and Mahan with measurements of blood glucose levels on three occasions for 30 women. The 𝑦’s represent fasting glucose measurements on the three occasions; the 𝑥’s are glucose measurements 1 hour after sugar intake. Find the mean vector and covariance matrix for all six variables and partition them into
