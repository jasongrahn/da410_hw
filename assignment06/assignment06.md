assignment06
================
Jason Grahn
2/12/2019

Chapter 11 Page 402:

11.9 A
======

Use the sons data of Table 3.8...

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
#normalize the data
sons.std <-sweep(sons, 2, sqrt(apply(sons,2,var)), FUN="/")
head(sons.std, 5)
```

    ##         y1       y2       x1       x2
    ## 1 19.56600 21.02287 17.82824 21.60972
    ## 2 19.97576 20.20908 20.01942 22.65295
    ## 3 18.54160 20.07345 18.42583 22.20585
    ## 4 18.74648 20.75161 18.72463 22.20585
    ## 5 18.02941 19.53092 17.03144 21.16262

``` r
#subgroup the data into first and second sons
first.son <- sons.std %>% select(y1, y2)
second.son <- sons.std %>% select(x1, x2)
```

(a) Find the canonical correlations between (*y*<sub>1</sub>,*y*<sub>2</sub>) and (*x*<sub>1</sub>, *x*<sub>1</sub>)·
---------------------------------------------------------------------------------------------------------------------

### the sample correlation matrix

``` r
S <- cov(sons.std)

#find the sample correlation matrix for either son
R11 <-cor(first.son)
R11
```

    ##           y1        y2
    ## y1 1.0000000 0.7345555
    ## y2 0.7345555 1.0000000

``` r
R22 <-cor(second.son)
R22
```

    ##           x1        x2
    ## x1 1.0000000 0.8392519
    ## x2 0.8392519 1.0000000

### b) the characteristic equation

``` r
# correlate column by column for the 4 columns 
        ## first from each
R12 <-c(cor(first.son[,1], 
            second.son[,1]), 
        ## first and second
        cor(first.son[,1], 
            second.son[,2]),
        ## second and first
        cor(first.son[,2], 
            second.son[,1]),
        ## second and second
        cor(first.son[,2], 
            second.son[,2]))

# coerce r12 into a 2x2 matrix
R12 <-matrix(R12, ncol=ncol(R22), byrow=T) 

# R21 is the transpose of R12
R21 <-t(R12)
```

``` r
# we have to find the eigenvectors and values in order to write the formulas, so this 
# WILL INCLUDE part C

# Finding the E1 and E2 matrices:
E1 <-solve(R11) %*% R12 %*% solve(R22) %*% R21
# and their eigenvector
e1vectors <- round(eigen(E1)$vectors,2)
e1vectors
```

    ##      [,1]  [,2]
    ## [1,] 0.73 -0.70
    ## [2,] 0.69  0.71

``` r
E2 <-solve(R22) %*% R21 %*% solve(R11) %*% R12
e2vectors <- round(eigen(E2)$vectors,2)
e2vectors
```

    ##       [,1]  [,2]
    ## [1,] -0.68 -0.71
    ## [2,] -0.73  0.71

So the final formulas are:

    u1 =  0.73[y1] + 0.69[y2]
    v1 = -0.68[x1] - 0.71[y2]
    u2 = -0.70[y1] + 0.71[y2]
    v2 = -0.73[x1] + 0.71[y2]

### c) eigenvalues

``` r
eigen(E1)$values
```

    ## [1] 0.621744734 0.002887956

``` r
eigen(E2)$values
```

    ## [1] 0.621744734 0.002887956

``` r
#they are the same!
```

### Finally, the cannonical correlation

``` r
canon.corr <-sqrt(eigen(E1)$values)
canon.corr
```

    ## [1] 0.7885079 0.0537397

2. Chapter 11 Page 402: \#11.9 part (c)
=======================================

Test the significance of each canonical correlation.
----------------------------------------------------

``` r
# known facts
n <- nrow(sons)
p <- 2
q <- 2
k <- 2

#make $V_H$
vh <- q
vh
```

    ## [1] 2

``` r
#make $V_E$
ve <- n - 1 - q
ve
```

    ## [1] 22

We reject *H*<sub>0</sub> **IF** *Λ*<sub>1</sub> &lt;= *Λ*<sub>*a*</sub>.

``` r
# lambda is 1 - the canonical correlation eigenvalue
test1 <- 1 - canon.corr[1]
test2 <- 1 - canon.corr[2]

# look at Table A.9 for the critical value
# table A.9 shows the critical value for vh = 2 and ve = 22 is .643
CriticalValue <- .643
result1 <- test1 <= CriticalValue
result2 <- test2 <= CriticalValue
```

It is TRUE that the lambda value (0.2114921) of canonical correlation **1** (0.7885079) is less than or equal to the critical value 0.643; so we reject *H*<sub>0</sub> and conclude that the first canonical correlation is significant and could use this relationship for building out multivatiate model.

But it is FALSE that the lambda value (0.9462603) of canonical correlation **2** (0.0537397) is less than or equal to the critical value 0.643; so we fail to reject *H*<sub>0</sub> and conclude that the second canonical correlation is NOT significant.
