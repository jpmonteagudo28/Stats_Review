# MULTIVARIATE ADAPTIVE REGRESSION SPLINES (MARS)
        # algorithm that creates a piecewise linear model which provides an intuitive stepping block
        # into nonlinearity

# Helper packages
library(dplyr)     # for data wrangling
library(ggplot2)   # for awesome plotting

# Modeling packages
library(earth)     # for fitting MARS models
library(caret)     # for automating the tuning process

# Model interpretability packages
library(vip)       # for variable importance
library(pdp)       # for variable relationships

# Stratified sampling with the rsample package
set.seed(123)
ames <- AmesHousing::make_ames()
split <- rsample::initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")
ames_train  <- rsample::training(split)
ames_test   <- rsample::testing(split)

# Fit basic MARS model
mars1 <- earth::earth(Sale_Price ~.,
                      data = ames_train)
print(mars1)
summary(mars1)%>% .$coefficients %>% head(10)
plot(mars1, which = 1)

# Creating second model with interaction terms (earth::earth (degree = 2) # maximum of function to interact at once)

mars2 <- earth::earth(Sale_Price ~.,
                      data = ames_train,
                      degree =2)
summary(mars2)%>% .$coefficients %>% head(10)
