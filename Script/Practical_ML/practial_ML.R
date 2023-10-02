# Helper packages
library(dplyr)     # for data manipulation
library(ggplot2)   # for awesome graphics

# Modeling process packages
library(rsample)   # for resampling procedures
library(caret)     # for resampling and model training
library(h2o)       # for resampling and model training

# h2o set-up 
h2o.no_progress()  # turn off h2o progress bars
h2o.init()         # launch h2o

#Ames housing data
ames <- AmesHousing::make_ames()
ames.h2o <- as.h2o(ames)

# Job attrition data
churn <- modeldata::attrition %>%
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)
churn.h2o <- as.h2o(churn)

# Splitting data using simple random sampling - no control for any data attributes
set.seed(123) # Using base R
index_1 <- sample(1:nrow(names), round(nrow(names)*.7))
train_1 <- ames[index_1,]
test_1 <- ames[-index_1,]

# Using h2o package to resample
split_1 <- h2o.splitFrame(ames.h2o,ratios = .7,
                          seed = 123)
train_2 <- split_1[[1]]
test_2 <- split_1[[2]]

# Stratified sampling using rsample- explicitly control sampling for distrutions
# can be used in regression for small samples and when dv deviates from normality

table(churn$Attrition)%>% prop.table()
# 
# No       Yes 
# 0.8387755 0.1612245 

set.seed(123)
split_strat <- initial_split(churn, prop = .7,
                             strata = "Attrition")
train_strat <- training(split_strat)
test_strat <- testing(split_strat) # DO NOT USE TEST SET TO ASSESS MODEL PERFORMANCE DURING TRAINING PHASE

table(train_strat$Attrition)%>% prop.table()
# 
# No       Yes 
# 0.8394942 0.1605058 

# How to assess performance of model
# 1. Assess error metric based on training data - biased result due to lack of generalizabilty/overfitting
# 2. Validation - split the trainign set further to create two parts:
      # a training set - train model
      # a validation set - evaluate performance with validation set
# 3 . Resampling approach - repeatedly fit a model to parts of traininf data and test it on other parts
      # 1. k-fold cross validation - randomly divides the training data into k-groups ( k  = 5 or k = 10) of apprx. equal size. Model fit on k-1 folds
         # and then the remaining fold is used to compute model performance. Repeated k times.
         # the k-fold CV estimate is computed by averaging the k test errors, providing us with approx. of error we might expect on unseen data
     
         h2o.cv <- h20.glm(x = x, y = y, # Using the h2o package
                        training_frame = ames.h2o,
                        nfolds = 10 # perform 10-fold CV
                        )
      # 2. Bootstrapping - random sample taken w. replacement - contain approx. the same distributions of vals. as original data
         # Original observations not contained in a particular bootstrap sample are considered out-of-bag (OOB)
         # When bootstrapping, a model can be built on the selected samples and validated on the OOB samples
         # Problematic with smaller sets because of increased bias due to low variability in error measures
         resample::bootstraps(ames, times = 10)
        
      # 3. Rolling origin CV - mainly used when working with time series data - https://pkg.robjhyndman.com/forecast/reference/tsCV.html
      # 632 method - https://stats.stackexchange.com/questions/96739/what-is-the-632-rule-in-bootstrapping

# Hyperparameter tuning - controlling the complexity of the ML algorithms and the bias-variance trade-off
      # One way to perform hyperparameter tuning is to fiddle with hyperparameters manually until you find a great
      # combination of hyperparameters that result in high predictive accuracy as measured by k-fold CV/boostrap. 
      # One alternative approach is to perform a grid search
         
         
# Model Evaluation  - assessing predictive accuracy via loss function
         # MSE - mean squared error most common metric to minimize
         # RMSE - root mean squared error - error in same units as response used to minimize
         # Deviance - explain variation in data using maximum likelihood typically used for classification to minimize
         # MAE - mean absolute error - less emphasis on larger error than MSE. Also used to minimze
         # RMSLE - logarithmic error - used to minimize impact of large errors on metric. 
         # R^2 - proportion of variance explained used to maximize. 
         
# Worked-out example
         set.seed(123) # stratified sampling 
         split <- rsample::initial_split(ames, prop = 0.7, 
                                strata = "Sale_Price")
         ames_train  <- training(split)
         ames_test   <- testing(split)
# Using meta engine to apply knn for resample, grid search and model application
         # resample: k = 10, repeated 5 times
         # grid search, k = 2:25
         # train knn model using k-fold CV, grid search and loss function RMSE
         cv <- caret::trainControl(
           method = "repeatedcv",
           number = 10,
           repeats = 5
         )
         
         # Create grid of hyperparameters
         hyper_grid <- expand.grid(k = seq(2,25,by = 1))
         
         # Tune a knn model using grid search
         knn_fit <- caret::train(
           Sale_Price ~ .,
           data = ames_train,
           method = "knn",
           trControl = cv,
           tuneGrid = hyper_grid,
           metric = "RMSE"
         )
         plot(knn_fit$results[,1],knn_fit$results[,2])