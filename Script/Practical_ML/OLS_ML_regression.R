# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for cross-validation, etc.

# Model interpretability packages
library(vip)      # variable importance

# Performing OLS regression between above ground living space and Sale_price in Ames data set
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)
summary(model1)

# OLS regression only provides estimates of the coefficients, but not the error variance, which characterizes
    # the variability of our model
# We can use Maximum Likelihood (ML) estimation which requires us to assume a particular distribution for the random errors
# An unbiased estimate of the error variance is given as the sum of squared residuals divided by (n-p), where p is the number of regression pars.
sigma(model1) #RMSE
# [1] 56787.94
sigma(model1)^2 # MSE
# [1] 3224869786

# Building confidence intervals for model coefficients
confint(model1, level = .95)
#              2.5 %     97.5 %
# (Intercept) 8384.213 23492.1336
# Gr_Liv_Area  104.920   114.4149

# MLR
summary(model2 <- update(model1, .~. + Year_Built))
# adding interactions
summary(model3 <- update(model2, .~. + Gr_Liv_Area*Year_Built))
# include all possible main effects
summary(model4 <- lm(Sale_Price ~ ., data = ames_train))


# Assessing model accuracy

# Which model (1,2,3,4) is best? - use RMSE and cross validation caret::train
# To assess predictive accuracy, we should consider time constrains, model production cost, predictive accuracy
 set.seed(123)
 (cv_model1 <- caret::train(form = Sale_Price ~ Gr_Liv_Area,
                            data = ames_train,
                            method = "lm",
                            trControl = trainControl(method = "cv", number = 10)
  ))
 
 # model1 produces on average errors of $56,664 when compared to actual prize
 
 set.seed(123)
 cv_model2 <- train(
   Sale_Price ~ Gr_Liv_Area + Year_Built, 
   data = ames_train, 
   method = "lm",
   trControl = trainControl(method = "cv", number = 10)
 )
 
 # model 3 CV
 set.seed(123)
 cv_model3 <- train(
   Sale_Price ~ ., 
   data = ames_train, 
   method = "lm",
   trControl = trainControl(method = "cv", number = 10)
 )
 
 # Extract out of sample performance measures
 summary(resamples(list(
   model1 = cv_model1, 
   model2 = cv_model2, 
   model3 = cv_model3
 )))
 
# Principal Component Regression (PCR) when dealing with multicollinearity 
 # Used to represent correlated variables w. smaller number of uncorrelated features
      # Using the caret package, method = "pcr" and  performs pre-processing
 
set.seed(123)
cv_model_pcr <- train(Sale_Price~ .,
                      data = ames_train,
                      method = "pcr",
                      trControl = trainControl(method = "cv", number = 10))

# model with lowest RMSE
cv_model_pcr$bestTune

# results for model with lowest RMSE
cv_model_pcr$results %>%
  dplyr::filter(ncomp == pull(cv_model_pcr$bestTune))

# plot cross-validated RMSE
ggplot(cv_model_pcr)
