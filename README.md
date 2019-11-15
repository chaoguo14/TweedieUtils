# TweedieUtils
[![Build Status](https://travis-ci.org/chaoguo14/TweedieUtils.svg?branch=master)](https://travis-ci.org/chaoguo14/TweedieUtils)

TweedieUtils is a collection of functions that runs tweedie-related models. Examples include tweedie GLM, tweedie GLM with penalty, and xgboost with tweedie loss. TweedieUtils is built based on several other packages, including tweedie, HDtweedie, and xgboost.

### Basic Usage
Use `tweedie_glm()` to run a tweedie GLM model on a given data set. This will return a `tweedie_glm` object. To see the summary, use `summary()`.
```{r}
my_model <- tweedie_glm(Payment ~ Kilometres + Bonus + Make, df = test_df_1, offset = log(test_df_1$Insured))
summary(my_model)
```

The output will be
```
Model Formula:  Payment ~ Kilometres + Bonus + Make 
               Estimate Std. Error 95% CI Lower Bound 95% CI Upper Bound Wald Chi-Square Significant?
(Intercept)  6.39862080  0.2240873         5.95940978          6.8378318    815.33944094          Yes
Kilometres  -1.04287457  0.3616675        -1.75174278         -0.3340064      8.31467532          Yes
Bonus        0.50995943  0.2269958         0.06504767          0.9548712      5.04702700          Yes
Make1        0.90069278  0.2482654         0.41409267          1.3872929     13.16197621          Yes
Make2        0.21298840  0.3754125        -0.52282011          0.9487969      0.32188034           No
Make4        0.05306341  0.2378039        -0.41303233          0.5191592      0.04979116           No

TECHNICAL NOTES:
 - Using Wald test.
 - Dispersion parameter phi 2172.59222021589 was estimated using MLE, unless supplied by the user.
 ```
Since `dispersion` argument is not specified when calling `tweedie_glm()`, it is estimated using MLE when `summary()` is called. By default, the confidence interval is constructed using Wald's method. In the future, TweedieUtils might support Likelihood-Ratio Test.
