# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for cross-validation, etc.
library(rsample)   # for resampling procedures

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

# Alternative to reduce impact of multicollinearity - Partial Least Squares
    # A supervised dimension reduction procedure that finds new features that not only captures most 
    # of the information in the original features, but also are related to the response.
    # This technique also constructs a set of linear combinations of the inputs for regression, 
    # but unlike PCR it uses the response variable to aid the construction of the principal components

# perform 10-fold cross validation on a PLS model tuning the 
# number of principal components to use as predictors from 1-30
set.seed(123)
cv_model_pls <- train(
  Sale_Price ~ ., 
  data = ames_train, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 30
)

# model with lowest RMSE
cv_model_pls$bestTune
# results for model with lowest RMSE
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
# plot cross-validated RMSE
ggplot(cv_model_pls)


# LOGISTIC REGRESSION

churn <- modeldata::attrition %>% 
  mutate_if(is.ordered, .funs = factor, ordered = FALSE)
df <- modeldata::attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Create training (70%) and test (30%) sets for the 
# rsample::attrition data.
set.seed(123)  # for reproducibility
churn_split <- initial_split(df, prop = .7, strata = "Attrition")
churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

model1 <- glm(Attrition ~ MonthlyIncome, family = "binomial", data = churn_train) # prob. of attrition on income
model2 <- glm(Attrition ~ OverTime, family = "binomial", data = churn_train) # prob. of attrition on overtime
broom::tidy(model1)
exp(coef(model1)) # by exponentiating the coefficients, we're able to provide a straight-forward explanation (odds ratio)
exp(coef(model2))
confint(model1) # same thing for the conf. int
confint(model12)

model3 <- glm( # fitting glm model that accounts for income and OT
  Attrition ~ MonthlyIncome + OverTime,
  family = "binomial", 
  data = churn_train
)

tidy(model3)

# Assessing model accuracy - k fold Cross-Validation
set.seed(123)
cv_model1 <- train(Attrition~ MonthlyIncome,
                   data = churn_train,
                   method = "glm",
                   family = "binomial",
                  trControl = trainControl(method = "cv",number = 10))
set.seed(123) 
cv_model2 <- train(Attrition ~ OverTime + MonthlyIncome,
                   data = churn_train,
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv",number = 10))
set.seed(123)
cv_model3 <- train(Attrition ~ .,
                   data = churn_train,
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv",number = 10))
# extract out of sample performance measures
summary(
  resamples(
    list(
      model1 = cv_model1, 
      model2 = cv_model2, 
      model3 = cv_model3
    )
  )
)$statistics$Accuracy

# Model performance using confusion matrix
pred_class <- predict(cv_model3, churn_train) # create predicted class and then provide actual values

caret::confusionMatrix(data = relevel(pred_class, ref = "Yes"),
                       reference = relevel(churn_train$Attrition, ref = "Yes"))

#Comparing Accuracy using ROC curve
m1_prob <- predict(cv_model1, churn_train, type ="prob")$Yes
m3_prob <- predict(cv_model3, churn_train, type="prob")$Yes

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- ROCR::prediction(m1_prob, churn_train$Attrition) %>%
  ROCR::performance(measure = "tpr", x.measure = "fpr") # measure is True Positive Rate ("tpr") and x-axis measure is "fpr"
perf2 <- ROCR::prediction(m3_prob, churn_train$Attrition) %>%
  ROCR::performance(measure = "tpr", x.measure = "fpr")

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = "black", lty = 2)
plot(perf2, add = TRUE, col = "blue")
legend(0.8, 0.2, legend = c("cv_model1", "cv_model3"),
       col = c("black", "blue"), lty = 2:1, cex = 0.6)

# Perform 10-fold CV on a PLS model tuning the number of PCs to 
# use as predictors
set.seed(123)
cv_model_pls <- train(
  Attrition ~ ., 
  data = churn_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)
# Model with lowest RMSE
cv_model_pls$bestTune
##    ncomp
## 14    14

# results for model with lowest loss
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
##   ncomp  Accuracy     Kappa AccuracySD   KappaSD
## 1    14 0.8757518 0.3766944 0.01919777 0.1142592

# Plot cross-validated RMSE
ggplot(cv_model_pls)

# Feature Interpretation
vip::vip(cv_model3, num_features = 20)
