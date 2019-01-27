da410\_project3
================
Jason Grahn
1/27/2019

Problem 1.
==========

1. Use admission.csv as a training dataset.
-------------------------------------------

We're going to use `admission.csv` twice, so importing once and calling it good.

``` r
admission <- readr::read_csv(here::here("project3/admission.csv"), 
                             col_types = 
                               cols(De = col_factor(
                                 levels = c("admit", "notadmit", "border"))))
head(admission, 5)
```

    ## # A tibble: 5 x 3
    ##     GPA  GMAT De   
    ##   <dbl> <dbl> <fct>
    ## 1  2.96   596 admit
    ## 2  3.14   473 admit
    ## 3  3.22   482 admit
    ## 4  3.29   527 admit
    ## 5  3.69   505 admit

2. Train model using LDA by setting admit/not-admit/border with the same probabilities.
---------------------------------------------------------------------------------------

``` r
model_1 <- lda(formula = De ~ ., 
               data = admission, 
               prior = c(1,1,1)/3)

model_1
```

    ## Call:
    ## lda(De ~ ., data = admission, prior = c(1, 1, 1)/3)
    ## 
    ## Prior probabilities of groups:
    ##     admit  notadmit    border 
    ## 0.3333333 0.3333333 0.3333333 
    ## 
    ## Group means:
    ##               GPA     GMAT
    ## admit    3.403871 561.2258
    ## notadmit 2.482500 447.0714
    ## border   2.992692 446.2308
    ## 
    ## Coefficients of linear discriminants:
    ##               LD1         LD2
    ## GPA  -5.017202736  1.85401003
    ## GMAT -0.008503148 -0.01448967
    ## 
    ## Proportion of trace:
    ##    LD1    LD2 
    ## 0.9644 0.0356

3. Calculate the misclassfication rate
--------------------------------------

``` r
#apply the model to admissions with predict()
predict_admit <- predict(model_1, # predictions
                data = admissions)

#table()
```

4. Predict students with GPA and SAT score as below.
----------------------------------------------------

``` r
p_gpa <- c(3.14, 3.08, 2.08, 3.22)
p_gmat <- c(470, 591, 641, 463)
p_student <- as.factor(c("student1", "student2", "student3", "student4"))

predict_me <- tibble(p_student, p_gpa, p_gmat)
predict_me
```

    ## # A tibble: 4 x 3
    ##   p_student p_gpa p_gmat
    ##   <fct>     <dbl>  <dbl>
    ## 1 student1   3.14    470
    ## 2 student2   3.08    591
    ## 3 student3   2.08    641
    ## 4 student4   3.22    463

Problem 2.
==========

1. Use admission.csv as a training dataset.
-------------------------------------------

2. Train model using LDA by setting probability of admit is 50% while probability of not admit is 25% and probability of border is 25%.
---------------------------------------------------------------------------------------------------------------------------------------

3. Calculate the misclassfication rate
--------------------------------------

4. Predict students with GPA and SAT score as below.
----------------------------------------------------

3.14 470 3.08 591 2.08 641 3.22 463

Compare differences of the result from problem1.
------------------------------------------------

Problem 3.
==========

Explain what is Quadratic Discriminant Analysis (QDA), and use QDA to train the model, discuss if this project can be done better by QDA, why or why not.
---------------------------------------------------------------------------------------------------------------------------------------------------------
