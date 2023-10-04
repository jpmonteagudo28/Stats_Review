# Regularized Regression - Chapter 6
      # Can be used when data presents mutlicollinearity and high variability of coefficients
      # all inputs must be numeric; however, some packages (e.g., caret and h2o) automate this process
      # cannot automatically handle missing data, which requires you to remove or impute them prior to modeling
      # not robust to outliers in both the feature and target
      # still assume a monotonic linear relationship (always increasing or decreasing in a linear fashion)
      # When features exceed number of observations (p > n)
      # OLS rgeression overfits data in presence of correlated features and high variability
            # Regularized regression puts constrains on magnitude of coefficients and will progressively shrink them towards zero.
            # This helps reduce the magnitude and variability of the coefficients and reduce variance in our model
            # Adds a penalty parameter to the SSE such that the size of the coefficient only increases when the SSE decreases.
                      # Penalty 1: Ridge
                            # Does not perform feature selection (smaller datasets w. multicollinearity)
                      # Penalty 2: LASSO (least absolute shrinkage and selection operator)
                            # Pushes variables all the way to zero
                            # Improves model and performs feature selection 
                      # Elastic net (ENET) - a combination of Ridge and LASSO
                            # enables effective regularization via the ridge penalty with the feature selection characteristics of the lasso penalty
                            # it only accepts the non-formula XY interface so prior to modeling we need to separate our feature and target sets
                            # regularized regression is sensitive to skewed response values
# Helper packages
library(recipes)  # for feature engineering

# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process

# Model interpretability packages
library(vip)      # for variable importance


#Ames housing data
ames <- AmesHousing::make_ames()
set.seed(123) # stratified sampling 
split <- rsample::initial_split(ames, prop = 0.7, 
                                strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)

# Create training features
# use model.matrix()[,-1] to discard intercept
X <- model.matrix(Sale_Price ~ ., ames_train)[,-1]
Y <- log(ames_train$Sale_Price) # log transform skewed response var

# Apply ridge regression using glmnet::glmnet and alpha = 0 for Ridge, alpha =1 for lasso and alpha (0,1) for ENET
ridge <- glmnet::glmnet(x = X,
                         y = Y,
                         alpha = 0)
plot(ridge, xvar = "lambda")
ridge$lambda # to see the exact lambda values applied
coef.glmnet(ridge) # 309 x 100 matrix with coefficients for each var across all vals of lambda. 
                   # We need to figure out which lambda value minimizes variability and max. performance
# k-fold Cv to identify optimal lambda
ames_ridge <- cv.glmnet(x = X,
                        y = Y,
                        alpha = 0) # ridge penalty
ames_lasso <- glmnet::cv.glmnet(x = X,
                                y = Y,
                                alpha = 1)

par(mfrow = c(1,2))
plot(ames_ridge, main = "Ridge penalty \n\n")
plot(ames_lasso, main = "Lasso penalty \n\n") # dotted lines represent lambda with smallest MSE and second line is lambda with MSE 1 sd. error of minimum MSE

min(ames_ridge$cvm) # min MSE for ridge model
# [1] 0.02162742
ames_ridge$lambda.min # min lambda for this MSE
# [1] 0.1266169
ames_ridge$cvm[ames_ridge$lambda == ames_ridge$lambda.1se] # MSE 1 SE away from min MSE
# [1] 0.02406178
ames_ridge$lambda.1se # lambda for this MSE
# [1] 0.5609917
min(ames_lasso$cvm) #  lasso model
# [1] 0.02255555
ames_lasso$lambda.min
# [1] 0.00328574
ames_lasso$nzero[ames_lasso$lambda == ames_lasso$lambda.min] # No. of coef | min MSE
# s48 
# 143
ames_lasso$lambda.1se
# [1] 0.01003418
ames_lasso$nzero[ames_lasso$lambda == ames_lasso$lambda.1se] # No. of coef | 1-SE MSE
# s36 
# 78 # no. of non-zero features kept to obtain 1 - SE MSE

# Ridge model
ridge_min <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Lasso model
lasso_min <- glmnet(
  x = X,
  y = Y,
  alpha = 1
)

par(mfrow = c(1, 2))
# plot ridge model
plot(ridge_min, xvar = "lambda", main = "Ridge penalty\n\n")
abline(v = log(ames_ridge$lambda.min), col = "red", lty = "dashed")
abline(v = log(ames_ridge$lambda.1se), col = "blue", lty = "dashed")

# plot lasso model
plot(lasso_min, xvar = "lambda", main = "Lasso penalty\n\n")
abline(v = log(ames_lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(ames_lasso$lambda.1se), col = "blue", lty = "dashed")

# Implementing an ENET (alpha between 0 -1)
# We'll use caret::train to automate process of finding optimal lambda value
set.seed(123)
cv_glmnet <- caret::train(x = X,
                          y = Y,
                          method = "glmnet",
                          preProcess = c("zv","center","scale"),
                          trControl = trainControl(method = "cv", number = 10),
                          tuneLength = 10)
cv_glmnet$bestTune # model with lowest RMSE
##  alpha     lambda
# 8 0.1 0.04636049

# results for model with lowest RMSE
cv_glmnet$results %>% filter(alpha == cv_glmnet$bestTune$alpha, lambda == cv_glmnet$bestTune$lambda)
#  alpha     lambda      RMSE  Rsquared        MAE    RMSESD  RsquaredSD       MAESD
# 1   0.1 0.04636049 0.1480459 0.8719676 0.08920217 0.0334738 0.06425456 0.004175786

# plot cross-validated RMSE
ggplot(cv_glmnet)

# How does this compare to our previous best model for the Ames data set?
# predict sales price on training data
pred <- predict(cv_glmnet, X)

# compute RMSE of transformed pred since weve been working with log transform response var
RMSE(exp(pred), exp(Y))
# [1] 26600.31

# FEATURE INTERPRETATION - most important feautes
vip::vip(cv_glmnet, num_features = 20, geom = "point")
vip::vi(cv_glmnet) # tibble with var importance scores

# WORKING W. ATTRITION DATA
df <- modeldata::attrition %>% mutate_if(is.ordered, factor, ordered = FALSE)
# Training set 70% and test 30%
set.seed(123)
churn_split <- rsample::initial_split(df, prop = .7, strata = "Attrition")
train <- rsample::training(churn_split)
test <-  rsample::testing(churn_split)

# train Logistic model
set.seed(123)
glm_mod <- train(Attrition~.,
                 data = train,
                 method = "glm",
                 family = "binomial",
                 preProcess = c("zv","center","scale"),
                 trControl = trainControl(method = "cv",number = 10))

# train regularized regression model
set.seed(123)
penalized_mod <- train(Attrition ~., 
                       data = train,
                       method = "glmnet",
                       family = "binomial",
                       preProcess = c("zv", "center","scale"),
                       trControl = trainControl(method = "cv",number = 10),
                       tuneLength = 10)

# Extract sample performance measures
summary(caret::resamples(list(logistic_model = glm_mod,
                              penalized_model = penalized_mod)))$statistics$Accuracy
