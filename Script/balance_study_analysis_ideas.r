# Trying to figure out the distribution of residuals for a possible model
summary(pred_change <- lm(Change_Cumul ~ Age + Change_Visual + Change_Vest + Gender,balance_df)) # creating model to get residuals

residuals <- pred_change$residuals # Getting residuals and plotting to check variance

layout(matrix(c(1:4), nrow = 2))
plot(residuals)
abline(h = median(residuals), col = "red", lwd = 2)
car::densityPlot(residuals)
hist(residuals)
dev <- no_glasses - mean(no_glasses)
hist(dev)
car::densityPlot(dev)


# Visualizing iV vs. dV

cumul_diff <- balance_df$Change_Cumul
gender <- balance_df$Gender
weeks <- balance_df$weeks_glasses

vars_to_plot <- list( # storing columns in var for easy manipulation
  age <- balance_df$Age,
  visual_diff <- balance_df$Change_Visual, # quadratic?
  vest_diff <- balance_df$Change_Vest,
  proprio_diff <- balance_df$Change_Proprio) # exponential? (exp(beta*proprio_diff))

for(var in vars_to_plot) {
  car::scatterplot(var, cumul_diff, smooth = TRUE)
}


cols <- c("Change_Proprio","Change_Visual","Change_Vest","Change_Cumul")
histograms(balance_df, cols)

change_stats <- t(data.frame(lapply(balance_df[,14:17], function(x) {
  mean_val <- mean(x, na.rm = TRUE) # Calculate mean
  skew_val <- skewness(x, na.rm = TRUE) # Calculate skewness 
  kurt_val <- kurtosis(x, na.rm = TRUE) # Calculate kurtosis
  return(round(c(Mean = mean_val, Skewness = skew_val, Kurtosis = kurt_val), 2))
})))
print(change_stats)

shapiro_test <- data.frame(t(sapply(balance_df[,14:17], shapiro.test)))
corr_test <- lapply(balance_df[,14:17], function(x) cor.test(x,cumul_diff,method = "kendall"))


# An idea to use exponential Iv in non-linear regression model
# first run lrm to get coefficient
log_proprio <- log(abs(proprio_diff+.0001))
fit_lm <- lm(cumul_diff~log_proprio); summary(fit_lm) # compute absolute value of proprio_diff and add .001 to every value.
b <- coef(fit_lm)[[2]] # Extract coefficient of log(abs(proprio_diff + .001))

layout(matrix(c(1:1)))
par(mar = c(5,4,4,2))
scatterplot(log_proprio, cumul_diff)# plotting cumul-diff against log_proprio to viz. effect

stacked <- stack(data.frame(cumul_diff,proprio_diff))


