# Data Pre-processing and engineering
# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations

# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks


# Target engineering - transformation of response var

# 1. log transform if pos. skewed of training data
      # log1p - to transform if y <= 0
      # step_log - recipes package 
# 2. Box Cox Transformation - more flexible than log transform - uses lambda which ranges from -5 to 5 as an exponent to find the best approx to normal distr. 
      # Compute lambda on the training set and apply the same lambda to both the training and test set to minimize data leakage.
# Undo transformation back to their normal scale to make it more easily interpretable
      # Box Cox transform a value
      y <- forecast::BoxCox(10, lambda)

      # Inverse Box Cox function
       inv_box_cox <- function(x, lambda) {
      # for Box-Cox, lambda = 0 --> log transform
         if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda) 
       }

      # Undo Box Cox-transformation
       inv_box_cox(y, lambda)
       
# Missing Values
       # Use visualization to understand distribution of missing vals
       # Can use a heatmap(), image() or ggplot2::geom_raster()
       AmesHousing::ames_raw %>%
         is.na() %>%
         reshape2::melt() %>%
         ggplot(aes(Var2, Var1, fill=value)) + 
         geom_raster() + 
         coord_flip() +
         scale_y_continuous(NULL, expand = c(0, 0)) +
         scale_fill_grey(name = "", 
                         labels = c("Present", 
                                    "Missing")) +
         xlab("Observation") +
         theme(axis.text.y  = element_text(size = 4))
       # Replace missing vals with inputed values
            # knn and tree-based inputation (bagged trees)
            # inputation should be performed w/i the resampling process
            # knn should be used on small to moderate sized datasets
       
# Feature engineering
       # Non-parametric models are rarely affected by skewed features; however, normalizing features 
       # will not have a negative effect on these models’ performance. For example, normalizing features
       # will only shift the optimal split points in tree-based algorithms. Consequently, when in doubt, normalize.
            # Box-Cox (for strictly positive values) and Yeo_Johnson for values not strictly positive)
       
       # Standardization
            # Consider the scale on which individual features are measured - look for range in features
            # standardizing includes centering and sclaing numeric features to have zero mean and unit variance
       
       # Sequential Steps
            # 1. if using log or Box-Cox transform, don't center the dara first or do any operations that maight make it negative or zero
                  # Use the Yeo-Johnson transformation instead
            # 2. One hot or dummy encoding results in sparse data which many algorithms operate efficiently on.
                  # If you standardize sparse data it will become dense data and you lose computational efficiency
            # 3. If you're lumping infrequently occurring cateogires, do so before dummy encodin
            # 4. Do PCA on numeric features for engineering purposes
       
       # Suggested order of steps
            # 1. Filter zero or near-zero variance features
            # 2. Perform imputation if needed
            # 3. Normalize to resolve skewness - double check on that
            # 4. Standardize numeric features
            # 5. Perform dimension reduction on numeric features
            # 6. Dummy encode cat. variables
       
       # To minimize data leakage - when information from outside the sample used to create the model - perform feature engineering in isolation of each resampling iteration
       # Use recipes to accomplish this goal
            # This package allows you to do feature engineering in sequential order
            # Three main steps: 
                # 1. recipe: where you define your feature engineering steps
                # 2. prep: estimate feature engineering parameters based on training data
                # 3. bake: apply the blueprint to new data
       blueprint <- recipe(Sale_Price ~ ., data = ames_train) %>%
         step_nzv(all_nominal())  %>% # remove near-zero variance
         step_integer(matches("Qual|Cond|QC|Qu")) %>% # ordinal encode our quality-based features
         step_center(all_numeric(), -all_outcomes()) %>% # center and rescale
         step_scale(all_numeric(), -all_outcomes()) %>%
         step_pca(all_numeric(), -all_outcomes()) # perform dimension reduction
       
       prepare <- prep(blueprint, training = ames_train) # We use training data set as we don't want to train test data on transformed training features
       
       baked_train <- bake(prepare, new_data = ames_train)
       baked_test <- bake(prepare, new_data = ames_test)
       baked_train
       prepare
            