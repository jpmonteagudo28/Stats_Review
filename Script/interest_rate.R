#-------------------------#
# The Big Short explained #
#-------------------------#

# Interest Rates explained

# Loan Default: 2% yearly - if money lent without charging interest, you'll end
# up losing money. Even though you don't know which clients will default, you can
# make up for the loss by charging extra.

# No. of loans approved: 25,000
# Amt lent per loan: $250,000
# Loss per foreclosure: $120,000

# Sampling Model:
n <- 25000
foreclosure_loss <- -120000
p <- .02 # default prob. 
default <- sample(c(0,1), n, prob=c(1-p,p), replace = T)
sum(default * foreclosure_loss) # random var
# [1] -60120000 We lose 60.1 million

#Simulation
losses <- replicate(1500,{
  default <- sample(c(0,1),n,prob = c(1-p,p), replace = T)
  sum(default*foreclosure_loss)
})

# Visualization
library(ggplot2)
losses_in_millions <- data.frame(losses = losses/10^6)
ggplot(losses_in_millions, aes(x = losses))+ geom_histogram()

# Expected Value and Std. error of the sum of 25,000 loans
exp_val <- n*(p*foreclosure_loss + (1-p)*0)
std_error <- sqrt(n)*abs(foreclosure_loss)*sqrt(p*(1-p))

# Setting interest rate to break even ($0 average loss)
foreclosure_loss*p + x(1-p)= 0 # Expected value given the foreclosure loss with 
# probability p and profit x if there's no foreclosure

xtra_amt = -foreclosure_loss*p/(1-p)
# interest rate on loan amount
interest <- (xtra_amt/250000)*100
# [1] 0.98

# To minimize loss, we need to set a risk limit:
# risk 1 out of 1000 - or - P(S < 0) = .001 we must now find the new interest rate
# S < 0 same as Z < -mu/sd
z <- qnorm(0.001)
nw_xtra_amt <- -foreclosure_loss*((n*p)-z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1*p)))
nw_interest <- (nw_xtra_amt/250000)*100
# [1] 1.11
foreclosure_loss*p + nw_xtra_amt*(1-p) # profit per loan
# [1] $329.43 and total expected profit of $8,235,842

# Running a MC sim to check profit/loss approximation
profit <- replicate(1500,{
  draws <- sample(c(nw_xtra_amt,foreclosure_loss),n,
                  prob=c(1-p,p), replace = T)
  sum(draws)
}) 
mean(profit) # [1] 8235351
mean(profit < 0) # [1] 0.0006666667

# Could you give out more loans to increase total profit and keep the probability
# of default below 4%? As long as the expected value is positive, you can minimize
# the chances of losing money by increasing the number of loans approved and 
# the interest rate.
p <- .04
interest <- .03
inc_xtra_amt <- interest*250000
foreclosure_loss*p + inc_xtra_amt*(1-p) # $2400 profit per loan

# How many loans to approve so that the probability of losing money is still
# 1 in 1000 (p = .001)
z <- qnorm(.001)
l <- foreclosure_loss
# n <- z^2*sd^2/mu^2 - or -
n <- ceiling((z^2*(inc_xtra_amt-l)^2*p*(1-p))/(l*p +inc_xtra_amt*(1-p)^2))
# [1] 958024 total loans approved to remain under .001 prob of losing money
nw_profit <- n*(l*p + inc_xtra_amt*(1-p))
# [1] $6,774,093,600 total profit w. 3% interest and approx. 2.8 million loans

# Running MC sim to check profit/loss approx
 profit <- replicate(1500,{
   draws <- sample(c(inc_xtra_amt,l),n,
                   prob=c(1-p,p), replace = T)
   sum(draws)
 })
 
 mean(profit) # [1] 6,773,005,515
 round(mean(profit < 0),3) # 0.000667

 # BY increasing the number of loans approved, you minimize the standard error of
 # the per-loan profit, IF X's are INDEPENDENT. The fact that one loan defaults must be 
 # independent of other loans defaulting.
 
 # Assuming loan default occurrence is not independent, let's build a model to assess
 # profit/loss
 
 p <- 0.04 # default probability
 interest <- .03
 profit<- replicate(1500,{
   nw_p <- p + sample(seq(-.02,.02,length = 100),1) # unknown probability of default (varies by 2%)
 draws <- sample(c(inc_xtra_amt,l),n,prob = c(1-nw_p,nw_p),replace = T)
 sum(draws)
 })
mean(profit) # 6746718500
mean(profit < 0) # 0.03133333
mean(profit < -10^7) # 0.03066667

# Visualizing distribution of profit/gain in new model
profit_in_millions <- data.frame(profit = profit/10^9)
ggplot(profit_in_millions, aes(x = profit)) + geom_histogram()

# Visualizing losses under new model
losses <- replicate(1500,{
  default <- sample(c(0,1),n,prob = c(1-nw_p,nw_p), replace = T)
  sum(default*l) # loss = default + foreclosure costs
})

losses_in_millions <- data.frame(losses = losses/10^9)
ggplot(losses_in_millions, aes(x = losses))+ geom_histogram()
