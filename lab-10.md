Lab 10 - Grading the professor, Pt. 2
================
Fanyi Zeng
03/14/22

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

    ## Warning: package 'openintro' was built under R version 4.1.3

    ## Warning: package 'airports' was built under R version 4.1.3

    ## Warning: package 'cherryblossom' was built under R version 4.1.3

    ## Warning: package 'usdata' was built under R version 4.1.3

``` r
view(evals)
```

### Part 1: Simple linear regression

The linear regression model of using average beauty ratings to predict
average evaluation scores could be written as the following formula:

evaluation score = 3.88034 + 0.06664 \* beauty rating

The adjusted R squared of the model is 0.03293, which means only 3.3% of
variance in one’s evaluation score is explained by one’s average beauty
rating.

``` r
m_bty <- lm(score~bty_avg, evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

The linear regression model of using average beauty ratings and gender
to predict average evaluation scores could be written as the following
formula:

evaluation score = 3.74734 + 0.07416 \* beauty rating + 0.17239 \*
gender

The intercept of 3.74734 means female professors with a beauty rating of
zero has a baseline evaluation score of 3.74734 on average.

The slope of 0.07416 means with every one unit increase in beauty
rating, there will be 0.07416 unit increase in evaluation score, all
else held constant. The addition of gender as another predictor
increases the strength of the slope of beauty rating.

The slope of 0.17239 means that male professors have a slightly higher
average evaluation score than female professors by 0.17239 unit, all
else held constant.

The line corresponding to male professors is evaluation score = 3.74734
+ 0.07416 \* beauty rating + 0.17239 = 3.91973 + 0.07416 \* beauty
rating.

The line corresponding to female professors is evaluation score =
3.74734 + 0.07416 \* beauty rating = 3.74734 + 0.07416 \* beauty rating.

The adjusted R squared of the model is 0.05503, which means only 5.5% of
variance in one’s evaluation score is explained by one’s average beauty
rating and gender. This adjusted R squared is an increase from the first
model with beauty rating as the sole predictor. The addition of gender
explains 2.2% more variance in the evaluation score.

``` r
m_bty_gen <- lm(score~bty_avg + gender, evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

Now let’s redo the model with beauty rating and rank as the predictors
of evaluation score.

The linear regression model of using average beauty ratings and gender
to predict average evaluation scores could be written as the following
formula:

evaluation score = 3.98155 + 0.06783 \* beauty rating - 0.16070 \*
tenure track - 0.12623 \* tenured

The intercept of 3.98155 means a teaching professor with a beauty rating
of zero will have a baseline evaluation score of 3.98155.

The slope of 0.06783 means with every one unit increase in beauty
rating, there will be 0.06783 unit increase in evaluation score, all
else held constant.

The slope of -0.16070 means that tenure track professors have a slightly
lower average evaluation score than teaching professors by 0.16070 unit,
all else held constant.

The slope of -0.12623 means that tenured professors have a slightly
lower average evaluation score than teaching professors by 0.12623 unit,
all else held constant.

The adjusted R squared of the model is 0.04029, which means only 4.0% of
variance in one’s evaluation score is explained by one’s average beauty
rating and rank. This adjusted R squared is an increase from the first
model with beauty rating as the sole predictor. The addition of rank
explains 0.7% more variance in the evaluation score.

``` r
m_bty_rank <- lm(score~bty_avg + rank, evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

### Part 3: The search for the best model

I don’t think number or percent of students in class who completed
evaluation will affect evaluation score.

Well, surprisingly, they do! Both of them are significant and positive
predictors of evaluation score. If people like the class then they are
more likely to fill out the evaluation forms, which makes sense. In my
opinion, however, if people hate the class, they will also fill out the
forms more willingly. I thought these two effects would cancel out but
they didn’t.

``` r
m_bty_eval <- lm(score~cls_perc_eval+cls_did_eval, evals)
summary(m_bty_eval)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_perc_eval + cls_did_eval, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9009 -0.3614  0.1162  0.3819  1.0931 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3.6326359  0.1206437  30.110  < 2e-16 ***
    ## cls_perc_eval 0.0066696  0.0015088   4.421 1.23e-05 ***
    ## cls_did_eval  0.0012475  0.0005616   2.221   0.0268 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 460 degrees of freedom
    ## Multiple R-squared:  0.04454,    Adjusted R-squared:  0.04038 
    ## F-statistic: 10.72 on 2 and 460 DF,  p-value: 2.813e-05

Okay, now I am going to fit a full model with all the predictors except
for cls_did_eval because I will include cls_students and cls_perc_eval.

Adjusted R squared is 0.1403. 14% variance explained by this model. To
make it simpler and better, we will need to take out preditcors that are
not significant.

``` r
m_full <- lm(score~rank + ethnicity + gender + language + age + cls_perc_eval + cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, evals)
summary(m_full)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_perc_eval + cls_did_eval + cls_students + cls_level + 
    ##     cls_profs + cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.83665 -0.31377  0.08559  0.35655  1.08091 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.6035360  0.2615008  13.780  < 2e-16 ***
    ## ranktenure track      -0.1022542  0.0823357  -1.242 0.214915    
    ## ranktenured           -0.0444115  0.0652594  -0.681 0.496514    
    ## ethnicitynot minority  0.1838073  0.0776989   2.366 0.018423 *  
    ## gendermale             0.1813064  0.0516980   3.507 0.000499 ***
    ## languagenon-english   -0.1297849  0.1081723  -1.200 0.230850    
    ## age                   -0.0065680  0.0030868  -2.128 0.033900 *  
    ## cls_perc_eval          0.0046764  0.0021063   2.220 0.026904 *  
    ## cls_did_eval           0.0022369  0.0031124   0.719 0.472698    
    ## cls_students          -0.0009486  0.0019726  -0.481 0.630832    
    ## cls_levelupper         0.0103812  0.0568080   0.183 0.855084    
    ## cls_profssingle       -0.0050013  0.0516204  -0.097 0.922860    
    ## cls_creditsone credit  0.5063406  0.1171236   4.323  1.9e-05 ***
    ## bty_avg                0.0609228  0.0166912   3.650 0.000293 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5043 on 449 degrees of freedom
    ## Multiple R-squared:  0.1645, Adjusted R-squared:  0.1403 
    ## F-statistic: 6.799 on 13 and 449 DF,  p-value: 5.372e-12

Adjusted R squared increases to 0.1419. This is because we have fewer
(necessary) predictors in the model. Age becomes nonsignificant so we
will get rid of that in the next model.

``` r
m_simple <- lm(score~ethnicity + gender + age + cls_perc_eval + cls_credits + bty_avg, evals)
summary(m_simple)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.90305 -0.32025  0.08687  0.37799  1.06885 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.409832   0.202120  16.870  < 2e-16 ***
    ## ethnicitynot minority  0.240897   0.071151   3.386 0.000771 ***
    ## gendermale             0.182259   0.049942   3.649 0.000293 ***
    ## age                   -0.005087   0.002610  -1.949 0.051873 .  
    ## cls_perc_eval          0.005107   0.001440   3.547 0.000430 ***
    ## cls_creditsone credit  0.532266   0.104448   5.096 5.09e-07 ***
    ## bty_avg                0.064891   0.016353   3.968 8.41e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5038 on 456 degrees of freedom
    ## Multiple R-squared:  0.153,  Adjusted R-squared:  0.1419 
    ## F-statistic: 13.73 on 6 and 456 DF,  p-value: 2.322e-14

Now the adjusted R squared is 0.1366, which decreases from the prior
model. My guess is that the p value of age is only slightly larger than
.05, which means it is “marginally significant” in conventional terms.
Therefore, I decide to keep age in the model.

``` r
m_better <- lm(score~ethnicity + gender + cls_perc_eval + cls_credits + bty_avg, evals)
summary(m_better)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + cls_perc_eval + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8857 -0.3294  0.1066  0.3774  1.0540 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3.137381   0.146450  21.423  < 2e-16 ***
    ## ethnicitynot minority 0.233794   0.071275   3.280 0.001117 ** 
    ## gendermale            0.157832   0.048493   3.255 0.001219 ** 
    ## cls_perc_eval         0.005208   0.001443   3.608 0.000343 ***
    ## cls_creditsone credit 0.541067   0.104669   5.169 3.52e-07 ***
    ## bty_avg               0.073644   0.015773   4.669 3.98e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5053 on 457 degrees of freedom
    ## Multiple R-squared:  0.146,  Adjusted R-squared:  0.1366 
    ## F-statistic: 15.62 on 5 and 457 DF,  p-value: 3.338e-14

My final model is the following: outcome is evaluation score, and
predictors are ethnicity, gender, age, cls_perc_eval(% students
completed the evaluation), cls_credits (course credits), and bty_avg
(average beauty ratings).

evaluation score = 3.409832 + 0.240897 \* ethnicity + 0.182259 \* gender
- 0.005087 \* age + 0.005107 \* cls_perc_eval + 0.532266 \* cls_credits
+ 0.064891 \* bty_avg.

The slope of a categorical variable, cls_credits, is 0.532266. This
means that professors who teach a one credit class have a higher
evaluation score than those who teach a multi-credit class by 0.532266
unit, all else held constant.

The slope of another categorical variable, ethnicity, is 0.182259. This
means that non-minority professors have a higher evaluation score by
0.182259 unit, all else held constant.

The slope of a continuous variable, age, is -0.005087. This means that
older professors have a lower evaluation score by -0.005087 unit, all
else held constant.

Based on my model, the professors with highest average evaluation scores
at UT Austin will be those who are white (non-minority), male, young,
attractive, teaching one-credit classes, and the majority of whose
students fill out evaluation forms.

Although the sample data is collected from UT Austin only, I will be
comfortable with generalizing the conclusions to other higher
institutions, because for one, UT Austin is a big school (so sample is
larger and more diverse), and for two, I do see the same pattern in my
own school (those professors being more popular among students). But,
that’s just my own two cents.

``` r
m_simple <- lm(score~ethnicity + gender + age + cls_perc_eval + cls_credits + bty_avg, evals)
summary(m_simple)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + age + cls_perc_eval + 
    ##     cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.90305 -0.32025  0.08687  0.37799  1.06885 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.409832   0.202120  16.870  < 2e-16 ***
    ## ethnicitynot minority  0.240897   0.071151   3.386 0.000771 ***
    ## gendermale             0.182259   0.049942   3.649 0.000293 ***
    ## age                   -0.005087   0.002610  -1.949 0.051873 .  
    ## cls_perc_eval          0.005107   0.001440   3.547 0.000430 ***
    ## cls_creditsone credit  0.532266   0.104448   5.096 5.09e-07 ***
    ## bty_avg                0.064891   0.016353   3.968 8.41e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5038 on 456 degrees of freedom
    ## Multiple R-squared:  0.153,  Adjusted R-squared:  0.1419 
    ## F-statistic: 13.73 on 6 and 456 DF,  p-value: 2.322e-14
