Logistic Regression
================
Daniela Quigee (dq2147)
12/01/2019

``` r
# Loading the cleaned data
load("./data/nyc_data.RData")
```

``` r
# Extracting necessary variables
df = df_total %>% 
  filter(year == 2017) %>% # Basing model on data from 2017
  select(
    # binary respone
    current_vaping,
    # possible predictor
    texting_and_driving,
    carring_weapon,
    sad_hopeless,
    attempted_suicide,
    injurious_suicide_attempt,
    safety_concerns_at_school,
    threatened_at_school,
    physical_fighting,
    bullying_at_school,
    bullying_electronically,
    sex,
    age,
    race7,
    borough)


# Fitting a logistic regression model
fit_logistic = glm(current_vaping ~ sad_hopeless + attempted_suicide  + safety_concerns_at_school + threatened_at_school + physical_fighting + bullying_electronically + carring_weapon  , data = df, family = binomial())

fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  knitr::kable(digits = 3)
```

| term                           | estimate | std.error | statistic | p.value |    OR |
| :----------------------------- | -------: | --------: | --------: | ------: | ----: |
| (Intercept)                    |  \-1.192 |     0.172 |   \-6.915 |   0.000 | 0.304 |
| sad\_hopelessNo                |    0.278 |     0.073 |     3.823 |   0.000 | 1.321 |
| attempted\_suicideNo           |    0.526 |     0.104 |     5.036 |   0.000 | 1.692 |
| safety\_concerns\_at\_schoolNo |    0.287 |     0.121 |     2.369 |   0.018 | 1.333 |
| threatened\_at\_schoolNo       |    0.296 |     0.135 |     2.193 |   0.028 | 1.345 |
| physical\_fightingNo           |    0.773 |     0.075 |    10.347 |   0.000 | 2.165 |
| bullying\_electronicallyNo     |    0.302 |     0.094 |     3.228 |   0.001 | 1.353 |
| carring\_weaponNo              |    0.983 |     0.110 |     8.920 |   0.000 | 2.672 |

``` r
# What is the contribution of each predictor? see: https://uc-r.github.io/logistic_regression#multi
caret::varImp(fit_logistic)
```

    ##                               Overall
    ## sad_hopelessNo               3.822561
    ## attempted_suicideNo          5.036263
    ## safety_concerns_at_schoolNo  2.368532
    ## threatened_at_schoolNo       2.193178
    ## physical_fightingNo         10.347481
    ## bullying_electronicallyNo    3.227808
    ## carring_weaponNo             8.920078

``` r
# Pseudo R^2: see https://uc-r.github.io/logistic_regression#multi (Pseudo R^2 about 0.40 considered good)
pR2(  glm(current_vaping ~ sad_hopeless + attempted_suicide  + safety_concerns_at_school + threatened_at_school + physical_fighting + bullying_electronically + carring_weapon  , data = df, family = binomial())  )["McFadden"]
```

    ##  McFadden 
    ## 0.3345769

``` r
# Cross-Validating the model

data_train <- trainControl(method = "cv", number = 5)

model_caret <- train(
  current_vaping ~ sad_hopeless + attempted_suicide  + safety_concerns_at_school + threatened_at_school + physical_fighting + bullying_electronically + carring_weapon,
                   data = df,
                   trControl = data_train,
                   method = 'glm',
                   family = binomial(),
                   na.action = na.pass)
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
    ## trainInfo, : There were missing values in resampled performance measures.

``` r
# Model predictions using 4 parts of the data for training 
model_caret
```

    ## Generalized Linear Model 
    ## 
    ## 10191 samples
    ##     7 predictor
    ##     2 classes: 'Yes', 'No' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9596, 8271, 8272, 8273, 8271, 8272, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa     
    ##   0.8467252  0.08915867

``` r
AIC(fit_logistic)
```

    ## [1] 5906.472
