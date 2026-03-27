---
title: "How I built a stroke prediction app"
date: 2026-03-27
draft: false
summary: "I made a stroke prediction app with R."
tags: [""]
---

This is how I built a stroke prediction app from scratch, without asking AI.


### Setup

Prepare the libraries.

    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(caret)

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

    library(mice)

    ## 
    ## Attaching package: 'mice'
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     cbind, rbind

Prepare the file.

    file = "D://R Files//healthcare-dataset-stroke-data.csv"
    dataset_raw = read.csv(file = file, na.strings="N/A")
    head(dataset_raw)

Let's take a preliminary look.

    ##      id gender age hypertension heart_disease ever_married     work_type
    ## 1  9046   Male  67            0             1          Yes       Private
    ## 2 51676 Female  61            0             0          Yes Self-employed
    ## 3 31112   Male  80            0             1          Yes       Private
    ## 4 60182 Female  49            0             0          Yes       Private
    ## 5  1665 Female  79            1             0          Yes Self-employed
    ## 6 56669   Male  81            0             0          Yes       Private
    ##   Residence_type avg_glucose_level  bmi  smoking_status stroke
    ## 1          Urban            228.69 36.6 formerly smoked      1
    ## 2          Rural            202.21   NA    never smoked      1
    ## 3          Rural            105.92 32.5    never smoked      1
    ## 4          Urban            171.23 34.4          smokes      1
    ## 5          Rural            174.12 24.0    never smoked      1
    ## 6          Urban            186.21 29.0 formerly smoked      1

    summary(dataset_raw)

    ##        id           gender               age         hypertension    
    ##  Min.   :   67   Length:5110        Min.   : 0.08   Min.   :0.00000  
    ##  1st Qu.:17741   Class :character   1st Qu.:25.00   1st Qu.:0.00000  
    ##  Median :36932   Mode  :character   Median :45.00   Median :0.00000  
    ##  Mean   :36518                      Mean   :43.23   Mean   :0.09746  
    ##  3rd Qu.:54682                      3rd Qu.:61.00   3rd Qu.:0.00000  
    ##  Max.   :72940                      Max.   :82.00   Max.   :1.00000  
    ##                                                                      
    ##  heart_disease     ever_married        work_type         Residence_type    
    ##  Min.   :0.00000   Length:5110        Length:5110        Length:5110       
    ##  1st Qu.:0.00000   Class :character   Class :character   Class :character  
    ##  Median :0.00000   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :0.05401                                                           
    ##  3rd Qu.:0.00000                                                           
    ##  Max.   :1.00000                                                           
    ##                                                                            
    ##  avg_glucose_level      bmi        smoking_status         stroke       
    ##  Min.   : 55.12    Min.   :10.30   Length:5110        Min.   :0.00000  
    ##  1st Qu.: 77.25    1st Qu.:23.50   Class :character   1st Qu.:0.00000  
    ##  Median : 91.89    Median :28.10   Mode  :character   Median :0.00000  
    ##  Mean   :106.15    Mean   :28.89                      Mean   :0.04873  
    ##  3rd Qu.:114.09    3rd Qu.:33.10                      3rd Qu.:0.00000  
    ##  Max.   :271.74    Max.   :97.60                      Max.   :1.00000  
    ##                    NA's   :201

### Cleaning and tidying up the data.

Clean the data by making sure everything has the correct data type.

    dataset <- transform(dataset_raw, 
              bmi = as.numeric(bmi), 
              hypertension = as.logical(hypertension), 
              heart_disease = as.logical(heart_disease),
              gender = as.factor(gender),
              work_type = as.factor(work_type),
              Residence_type = as.factor(Residence_type),
              ever_married = as.factor(ever_married),
              stroke = as.factor(stroke),
              smoking_status = as.factor(smoking_status)
              )

    dataset <- dataset %>%
      mutate(ever_married = if_else(ever_married == "Yes", TRUE, if_else(ever_married=="No", FALSE, FALSE)))
    
Data should be ready by now. Let's check:

    summary(dataset)

    ##        id           gender          age        hypertension    heart_disease  
    ##  Min.   :   67   Female:2994   Min.   : 0.08   Mode :logical   Mode :logical  
    ##  1st Qu.:17741   Male  :2115   1st Qu.:25.00   FALSE:4612      FALSE:4834     
    ##  Median :36932   Other :   1   Median :45.00   TRUE :498       TRUE :276      
    ##  Mean   :36518                 Mean   :43.23                                  
    ##  3rd Qu.:54682                 3rd Qu.:61.00                                  
    ##  Max.   :72940                 Max.   :82.00                                  
    ##                                                                               
    ##  ever_married            work_type    Residence_type avg_glucose_level
    ##  Mode :logical   children     : 687   Rural:2514     Min.   : 55.12   
    ##  FALSE:1757      Govt_job     : 657   Urban:2596     1st Qu.: 77.25   
    ##  TRUE :3353      Never_worked :  22                  Median : 91.89   
    ##                  Private      :2925                  Mean   :106.15   
    ##                  Self-employed: 819                  3rd Qu.:114.09   
    ##                                                      Max.   :271.74   
    ##                                                                       
    ##       bmi                smoking_status stroke  
    ##  Min.   :10.30   formerly smoked: 885   0:4861  
    ##  1st Qu.:23.50   never smoked   :1892   1: 249  
    ##  Median :28.10   smokes         : 789           
    ##  Mean   :28.89   Unknown        :1544           
    ##  3rd Qu.:33.10                                  
    ##  Max.   :97.60                                  
    ##  NA's   :201

    head(dataset)

    ##      id gender age hypertension heart_disease ever_married     work_type
    ## 1  9046   Male  67        FALSE          TRUE         TRUE       Private
    ## 2 51676 Female  61        FALSE         FALSE         TRUE Self-employed
    ## 3 31112   Male  80        FALSE          TRUE         TRUE       Private
    ## 4 60182 Female  49        FALSE         FALSE         TRUE       Private
    ## 5  1665 Female  79         TRUE         FALSE         TRUE Self-employed
    ## 6 56669   Male  81        FALSE         FALSE         TRUE       Private
    ##   Residence_type avg_glucose_level  bmi  smoking_status stroke
    ## 1          Urban            228.69 36.6 formerly smoked      1
    ## 2          Rural            202.21   NA    never smoked      1
    ## 3          Rural            105.92 32.5    never smoked      1
    ## 4          Urban            171.23 34.4          smokes      1
    ## 5          Rural            174.12 24.0    never smoked      1
    ## 6          Urban            186.21 29.0 formerly smoked      1

    nrow(dataset)

    ## [1] 5110

Don't forget to check for missing values (NA)

    (sum(is.na(dataset$bmi)) / nrow(dataset)) * 100

    ## [1] 3.933464

    # 3% of the bmi data is missing.


### Impute missing values with MICE

Create a mice object to define methods.

    set.seed(7)

    mice_1 <- mice(dataset, maxit=0) # Set maxit to zero first because we don't want to predict yet. We are only creating a mice object.

    predM <- mice_1$predictorMatrix

id shouldn't be used to predict anything, so make sure to leave it out. 

    predM[, c("id")] <- 0 
    meth <- mice_1$method

Check the methods. Only bmi has NA values and needs to be calculated.

    meth

    ##                id            gender               age      hypertension 
    ##                ""                ""                ""                "" 
    ##     heart_disease      ever_married         work_type    Residence_type 
    ##                ""                ""                ""                "" 
    ## avg_glucose_level               bmi    smoking_status            stroke 
    ##                ""             "pmm"                ""                ""

Run mice to create dataset with 5 different possible values for the
missing values.

    mice_results <- mice(dataset, maxit = 1, 
                 predictorMatrix = predM, 
                 method = meth, print =  FALSE) 

Check the predicted values.

    head(mice_results$imp$bmi) 

    ##       1    2    3    4    5
    ## 2  21.4 26.7 28.7 21.9 33.3
    ## 9  27.3 19.4 24.2 28.6 32.2
    ## 14 37.9 40.9 35.5 27.0 23.2
    ## 20 26.7 27.8 22.1 20.5 27.0
    ## 28 31.5 23.2 31.2 31.9 39.1
    ## 30 45.2 32.8 37.3 29.1 28.6


Extract the first set and use it to complete the data.

    dataset <- mice::complete(mice_results, 1)
    head(dataset)

    ##      id gender age hypertension heart_disease ever_married     work_type
    ## 1  9046   Male  67        FALSE          TRUE         TRUE       Private
    ## 2 51676 Female  61        FALSE         FALSE         TRUE Self-employed
    ## 3 31112   Male  80        FALSE          TRUE         TRUE       Private
    ## 4 60182 Female  49        FALSE         FALSE         TRUE       Private
    ## 5  1665 Female  79         TRUE         FALSE         TRUE Self-employed
    ## 6 56669   Male  81        FALSE         FALSE         TRUE       Private
    ##   Residence_type avg_glucose_level  bmi  smoking_status stroke
    ## 1          Urban            228.69 36.6 formerly smoked      1
    ## 2          Rural            202.21 21.4    never smoked      1
    ## 3          Rural            105.92 32.5    never smoked      1
    ## 4          Urban            171.23 34.4          smokes      1
    ## 5          Rural            174.12 24.0    never smoked      1
    ## 6          Urban            186.21 29.0 formerly smoked      1

### Build prediction models

Start by preparing a training and testing set.

    control = trainControl(method="cv", number = 10)

Test different models.

    # boosted logistic regression
    set.seed(7)
    fit.logreg <- train(stroke ~ 
                          gender + age + hypertension + ever_married + 
                          work_type + Residence_type + avg_glucose_level + bmi + 
                          smoking_status, data=dataset, method="LogitBoost", metric="Accuracy", trControl=control)

    # naive Bayes
    set.seed(7)
    fit.naivebayes <- train(stroke ~ 
                          gender + age + hypertension + ever_married + 
                          work_type + Residence_type + avg_glucose_level + bmi + 
                          smoking_status, data=dataset, method="naive_bayes", metric="Accuracy", trControl=control)

    # K-nearest neighbors 
    set.seed(7)
    fit.knn <- train(stroke ~ 
                       gender + age + hypertension + ever_married + 
                       work_type + Residence_type + avg_glucose_level + bmi + 
                       smoking_status, data=dataset, method="knn", metric="Accuracy", trControl=control)

    # support vector machine
    set.seed(7)
    fit.svm <- train(stroke ~ 
                       gender + age + hypertension + ever_married + 
                       work_type + Residence_type + avg_glucose_level + bmi + 
                       smoking_status, data=dataset, method="svmLinear", metric="Accuracy", trControl=control)

    # random forest
    set.seed(7)
    fit.rf <- train(stroke ~ 
                      gender + age + hypertension + ever_married + 
                      work_type + Residence_type + avg_glucose_level + bmi + 
                      smoking_status, data=dataset, method="ranger", metric="Accuracy", trControl=control)

    # CART
    set.seed(7)
    fit.cart <- train(stroke ~ 
                        gender + age + hypertension + ever_married + 
                        work_type + Residence_type + avg_glucose_level + bmi + 
                        smoking_status, data=dataset, method="rpart", metric="Accuracy", trControl=control)

From all the models, check which is the most accurate.

    results <- resamples(list(logreg=fit.logreg, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf, naivebayes=fit.naivebayes))

    summary(results)

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: logreg, cart, knn, svm, rf, naivebayes 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##                 Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## logreg     0.9432485 0.9476517 0.9510284 0.9491193 0.9510763 0.9511719    0
    ## cart       0.9452055 0.9510763 0.9510763 0.9504896 0.9510763 0.9529412    0
    ## knn        0.9412916 0.9471624 0.9510763 0.9489240 0.9511480 0.9530333    0
    ## svm        0.9510763 0.9510763 0.9510763 0.9512724 0.9510763 0.9529412    0
    ## rf         0.9510763 0.9510763 0.9510763 0.9512724 0.9510763 0.9529412    0
    ## naivebayes 0.9510763 0.9510763 0.9510763 0.9512724 0.9510763 0.9529412    0
    ## 
    ## Kappa 
    ##                   Min.      1st Qu.       Median        Mean    3rd Qu.
    ## logreg     -0.01368083 -0.006420385 -0.001888788 0.002818109 0.00000000
    ## cart       -0.01059472  0.000000000  0.000000000 0.024640625 0.05048551
    ## knn        -0.01657825 -0.007300869  0.000000000 0.009876670 0.00000000
    ## svm         0.00000000  0.000000000  0.000000000 0.000000000 0.00000000
    ## rf          0.00000000  0.000000000  0.000000000 0.000000000 0.00000000
    ## naivebayes  0.00000000  0.000000000  0.000000000 0.000000000 0.00000000
    ##                  Max. NA's
    ## logreg     0.06731401    0
    ## cart       0.12613722    0
    ## knn        0.07343608    0
    ## svm        0.00000000    0
    ## rf         0.00000000    0
    ## naivebayes 0.00000000    0

Test with a sample patient.

    patient = tribble(~gender, ~age, ~hypertension, ~heart_disease, 
                      ~ever_married, ~work_type, ~Residence_type, 
                      ~avg_glucose_level, ~bmi, ~smoking_status, 
                      "Male", 81, TRUE, TRUE, TRUE, "Private", "Urban", 300, 28.6, "smokes")

    predict(fit.rf, patient)

    ## [1] 0
    ## Levels: 0 1

The patient is unlikely to have stroke.
