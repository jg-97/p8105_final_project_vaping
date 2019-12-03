logistic\_reg\_Siquan
================
Siquan
12/2/2019

``` r
# Loading the cleaned data
load("./data/nyc_data.RData")
```

\#I decide to use LASSO to automatically help me choose the important
covariates in the logistic regression model based on cross-validation
error

``` r
# Extracting necessary variables (in the following analysis, we only focus on the variables extracted by Daniela in the original'Logistics Regressin.Rmd')
df = df_total %>% 
  filter(year == 2017| year ==2015) %>% # Basing model on data from both 2015 and 2017
  select(
    year,
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

summary(df)
```

    ##       year      current_vaping       texting_and_driving carring_weapon
    ##  Min.   :2015   Yes : 2997     Yes             :  935    Yes : 1441    
    ##  1st Qu.:2015   No  :14707     No/did not drive: 3114    No  :16482    
    ##  Median :2017   NA's: 1009     NA's            :14664    NA's:  790    
    ##  Mean   :2016                                                          
    ##  3rd Qu.:2017                                                          
    ##  Max.   :2017                                                          
    ##                                                                        
    ##  sad_hopeless attempted_suicide              injurious_suicide_attempt
    ##  Yes : 5547   Yes : 1496        Yes                       :  516      
    ##  No  :12316   No  :13678        did not attempt suicide/No:14343      
    ##  NA's:  850   NA's: 3539        NA's                      : 3854      
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##  safety_concerns_at_school threatened_at_school physical_fighting
    ##  Yes : 1384                Yes : 1239           Yes : 4268       
    ##  No  :16824                No  :17128           No  :14030       
    ##  NA's:  505                NA's:  346           NA's:  415       
    ##                                                                  
    ##                                                                  
    ##                                                                  
    ##                                                                  
    ##  bullying_at_school bullying_electronically     sex      
    ##  Yes : 2716         Yes : 2341              female:9496  
    ##  No  :15490         No  :15921              male  :9032  
    ##  NA's:  507         NA's:  451              NA's  : 185  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##                     age                                 race7     
    ##  15 years old         :4776   Hispanic/Latino              :7990  
    ##  16 years old         :4360   Black or African American    :4184  
    ##  17 years old         :4092   White                        :2289  
    ##  14 years old         :3852   Asian                        :2159  
    ##  18 years old or older: 971   Multiple Races (Non-Hispanic): 582  
    ##  (Other)              : 573   (Other)                      : 506  
    ##  NA's                 :  89   NA's                         :1003  
    ##           borough    
    ##  Bronx        :3952  
    ##  Brooklyn     :5227  
    ##  Manhattan    :3857  
    ##  Queens       :3147  
    ##  Staten Island:2530  
    ##                      
    ## 

``` r
str(df)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    18713 obs. of  16 variables:
    ##  $ year                     : num  2015 2015 2015 2015 2015 ...
    ##  $ current_vaping           : Factor w/ 2 levels "Yes","No": NA 2 2 NA 2 2 2 2 2 1 ...
    ##  $ texting_and_driving      : Factor w/ 2 levels "Yes","No/did not drive": NA NA NA NA 2 NA NA 2 NA 1 ...
    ##  $ carring_weapon           : Factor w/ 2 levels "Yes","No": NA NA 2 2 1 2 2 2 2 NA ...
    ##  $ sad_hopeless             : Factor w/ 2 levels "Yes","No": NA 1 2 2 NA 2 1 1 1 NA ...
    ##  $ attempted_suicide        : Factor w/ 2 levels "Yes","No": NA 1 2 2 NA 2 NA 2 2 1 ...
    ##  $ injurious_suicide_attempt: Factor w/ 2 levels "Yes","did not attempt suicide/No": NA 2 2 2 NA 2 NA 2 2 2 ...
    ##  $ safety_concerns_at_school: Factor w/ 2 levels "Yes","No": 1 1 2 2 1 2 2 2 2 1 ...
    ##  $ threatened_at_school     : Factor w/ 2 levels "Yes","No": NA NA 2 2 1 2 2 2 2 1 ...
    ##  $ physical_fighting        : Factor w/ 2 levels "Yes","No": NA NA 2 2 1 2 1 1 2 1 ...
    ##  $ bullying_at_school       : Factor w/ 2 levels "Yes","No": 2 2 2 1 1 2 2 1 2 1 ...
    ##  $ bullying_electronically  : Factor w/ 2 levels "Yes","No": 2 2 2 2 NA 2 1 2 2 NA ...
    ##  $ sex                      : Factor w/ 2 levels "female","male": NA NA NA NA NA NA NA NA NA NA ...
    ##  $ age                      : Factor w/ 7 levels "12 years old or younger",..: NA NA NA NA 1 3 3 3 4 5 ...
    ##  $ race7                    : Factor w/ 7 levels "American Indian/Alaska Native",..: 4 NA NA 4 NA 3 7 NA 3 NA ...
    ##  $ borough                  : Factor w/ 5 levels "Bronx","Brooklyn",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
#Remove all observations with any missing value
df<-na.omit(df)
#Split data into training and test data set
sample_size = floor(0.8*nrow(df))
set.seed(8105)
picked = sample(seq_len(nrow(df)),size = sample_size)
data_tr =df[picked,]
data_te =df[-picked,]
x_tr <- data_tr[,c(1,3:16)]
y_tr <- data_tr[,2]
x_te <- data_te[,c(1,3:16)]
y_te <- data_te[,2]
#use LASSO since we have so many predictors and automatically let R help us select the important variables
colnames(data_tr)
```

    ##  [1] "year"                      "current_vaping"           
    ##  [3] "texting_and_driving"       "carring_weapon"           
    ##  [5] "sad_hopeless"              "attempted_suicide"        
    ##  [7] "injurious_suicide_attempt" "safety_concerns_at_school"
    ##  [9] "threatened_at_school"      "physical_fighting"        
    ## [11] "bullying_at_school"        "bullying_electronically"  
    ## [13] "sex"                       "age"                      
    ## [15] "race7"                     "borough"

``` r
library(glmnet)
library(gmodels)
#create train set input
Training_x <-  model.matrix( ~ year + carring_weapon +texting_and_driving +carring_weapon+sad_hopeless+attempted_suicide+injurious_suicide_attempt+ safety_concerns_at_school+threatened_at_school+physical_fighting+bullying_at_school+bullying_electronically+sex+age+race7+borough -1, data_tr)
#Use LASSO to do auto variable selection accoridng to cross-validation error
cv.lasso <- cv.glmnet(Training_x, as.matrix(as.numeric(unlist(y_tr))-1), alpha = 1, family = "binomial")
model_lasso <- glmnet(Training_x, as.matrix(as.numeric(unlist(y_tr))-1), alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
coef(model_lasso)
```

    ## 30 x 1 sparse Matrix of class "dgCMatrix"
    ##                                                                s0
    ## (Intercept)                                         -1.625174e+02
    ## year                                                 7.982888e-02
    ## carring_weaponYes                                   -5.041743e-01
    ## carring_weaponNo                                     9.114951e-15
    ## texting_and_drivingNo/did not drive                  7.694352e-01
    ## sad_hopelessNo                                       2.967696e-01
    ## attempted_suicideNo                                  6.810848e-01
    ## injurious_suicide_attemptdid not attempt suicide/No  .           
    ## safety_concerns_at_schoolNo                          3.235722e-01
    ## threatened_at_schoolNo                               7.252697e-02
    ## physical_fightingNo                                  8.646754e-01
    ## bullying_at_schoolNo                                 .           
    ## bullying_electronicallyNo                            3.467696e-01
    ## sexmale                                              .           
    ## age13 years old                                      .           
    ## age14 years old                                      4.971420e-01
    ## age15 years old                                      .           
    ## age16 years old                                      .           
    ## age17 years old                                      .           
    ## age18 years old or older                             9.396247e-02
    ## race7Asian                                           .           
    ## race7Black or African American                       7.866188e-01
    ## race7Hispanic/Latino                                 .           
    ## race7Multiple Races (Non-Hispanic)                   .           
    ## race7Native Hawaiian/Other Pacific Islander          .           
    ## race7White                                          -6.065113e-01
    ## boroughBrooklyn                                     -7.535410e-03
    ## boroughManhattan                                     7.095502e-02
    ## boroughQueens                                        .           
    ## boroughStaten Island                                 .

``` r
#Training set prediction accuracy
model_lasso_train<- round(predict(model_lasso, Training_x, type = "response"))
CrossTable(x = as.numeric(unlist(y_tr))-1,y = model_lasso_train, prop.r = F, prop.c = F, prop.chisq = F)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  1787 
    ## 
    ##  
    ##                              | model_lasso_train 
    ## as.numeric(unlist(y_tr)) - 1 |         0 |         1 | Row Total | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                            0 |       135 |       329 |       464 | 
    ##                              |     0.076 |     0.184 |           | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                            1 |        47 |      1276 |      1323 | 
    ##                              |     0.026 |     0.714 |           | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                 Column Total |       182 |      1605 |      1787 | 
    ## -----------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
mean(as.numeric(unlist(y_tr))-1 ==model_lasso_train)
```

    ## [1] 0.7895915

``` r
#Test set prediction accuracy
Testing_x <-  model.matrix( ~ year + carring_weapon +texting_and_driving +carring_weapon+sad_hopeless+attempted_suicide+injurious_suicide_attempt+ safety_concerns_at_school+threatened_at_school+physical_fighting+bullying_at_school+bullying_electronically+sex+age+race7+borough -1, data_te)

model_lasso_test<- round(predict(model_lasso, Testing_x, type = "response"))
CrossTable(x = as.numeric(unlist(y_te))-1,y = model_lasso_test, prop.r = F, prop.c = F, prop.chisq = F)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  447 
    ## 
    ##  
    ##                              | model_lasso_test 
    ## as.numeric(unlist(y_te)) - 1 |         0 |         1 | Row Total | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                            0 |        29 |        77 |       106 | 
    ##                              |     0.065 |     0.172 |           | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                            1 |        22 |       319 |       341 | 
    ##                              |     0.049 |     0.714 |           | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                 Column Total |        51 |       396 |       447 | 
    ## -----------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
mean(as.numeric(unlist(y_te))-1 ==model_lasso_test)
```

    ## [1] 0.7785235

``` r
#Use prediction accuracy as the evaluation matrix in binary classification task, train set prediction accuracy  0.7879127 and test set prediction accuracy 0.7785235. Since the training set and test set accuracy are pretty close, there is no overfitting.
```

\#Note that we could do the same procedure with the imputed data and see
whether the prediction accuracy has been imporved.

\#Also note that we could add more potential covariates into the model
and see whether the prediction accuracy has been imporved.

\#Compare with the original
model

``` r
fit_logistic = glm(current_vaping ~ sad_hopeless + attempted_suicide  + safety_concerns_at_school + threatened_at_school + physical_fighting + bullying_electronically + carring_weapon, data = data_tr, family = binomial())

fit_logistics_predict = round(predict(fit_logistic, data_te, type = "response"))
CrossTable(x = as.numeric(unlist(y_te))-1,y = fit_logistics_predict, prop.r = F, prop.c = F, prop.chisq = F)
```

    ## 
    ##  
    ##    Cell Contents
    ## |-------------------------|
    ## |                       N |
    ## |         N / Table Total |
    ## |-------------------------|
    ## 
    ##  
    ## Total Observations in Table:  447 
    ## 
    ##  
    ##                              | fit_logistics_predict 
    ## as.numeric(unlist(y_te)) - 1 |         0 |         1 | Row Total | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                            0 |        20 |        86 |       106 | 
    ##                              |     0.045 |     0.192 |           | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                            1 |        17 |       324 |       341 | 
    ##                              |     0.038 |     0.725 |           | 
    ## -----------------------------|-----------|-----------|-----------|
    ##                 Column Total |        37 |       410 |       447 | 
    ## -----------------------------|-----------|-----------|-----------|
    ## 
    ## 

``` r
mean(as.numeric(unlist(y_te))-1 ==fit_logistics_predict)
```

    ## [1] 0.7695749

The original model has a test prediction accuracy of 0.7695749, which is
a little bit lower than the LASSO model, but I think both of these two
models are okay and we could then try to model with imputede data and
even adding some more covariates we are interested in to the model.
