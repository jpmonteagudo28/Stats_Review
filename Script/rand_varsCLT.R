# ----------------------------------------------------------------------------#

# Random Variables: numeric outcomes resulting from a random process

# One of the most IMPORTANT jobs of a data scientist is BEING ABLE TO QUANTIFY
# THE UNCERTAINTY that is RANDOM in nature. 

# The data related to a specific outcome can be modeled as a random sample from 
# an urn containing the values for those outcomes for the entire population of 
# interest.

# The probability distribution of a random variable tells us the probability of
# the observed value falling in any given interval. 
n <- 1000
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)   # probability of the casino losing money

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "white", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#-----------------------------------------------------------------------------#
# Distributions vs. Probability Distributions
#-----------------------------------------------------------------------------#

# 1. A distribution of any list of numbers is a function F(a): proportion(x <= a), and when 
# the distribution is approximately normal, we define the average and sd.
# 2. A random variable "x" has a distribution function - IT'S A THEORETICAL CONCEPT -
# NOT a list of numbers, F(a): Pr(x <= a) - the probability that x is less than or 
# equal to a. 

# Notation for Random variables
# Capital letters are used to denote random vars
# lower case letters denote observed values

# Central Limit Theorem (CLT)
# When the number of independent draws - sample size - is large, the probability 
# distribution of the sum of these draws is approximately normal regardless of the
# underlying distribution of each random draw. 

# Expected Value E[X] and standard error SE[X]
# Is a random variable has a prob. dist.  that is approximated with the Gaussian
# dist. we only need to provide the average and standard deviation to describe it.

# E[x] = mean(x) or the probabilities of each outcome multiplied by the number
# of times the outcome occurred: a*p + b*(1-p) - if only two mut. exc outcomes possible. 
# The expected value of the sum of draws is the number of draws * avg of the numbers
# in the urn (sample space)

# SE[X] = sqrt(number of draws) * sd(numbers in sample space) - or -
# sqrt(n) *|b - a| * sqrt(p(1-p))

# ----------------------------------------------------------------------------#
# Averages and Proportions                                                    #
#-----------------------------------------------------------------------------#

E[X1 + X2 + X3 + ...Xn] = E[X1 + X2 + X3+...Xn] E[aX] = a*mu
#. 1 The excepted value of the sum of random vars is the sum of the expected
# values of the individual random vars.

E[aX]= a*E[X]
#. 2 The expected value of random vars times a non-random constant is the 
# expected value times than non-random constant. 

SE[X1 + X2 + X3 + ...Xn] = sqrt(Se[X1]^2 + SE[X2]^2 + SE[X3]^2 + SE[Xn]^2)
#. 3. The square of the standard error of the sum of independent random vars.
# is the sum of the square of the standard error of each random var.

SE[aX] = a * SE[X]; SE[X] = sigma/sqrt(n)
# 4. The standard error of random vars. times a non-random constant is the 
# standard error times a non-random constant. 

# 5. If X is a normally distributed random variable, then if a and b are non
# random constants, then aX  + b is also a normally distributed random variable. 

#-----------------------------------------------------------------------------#
# Law of Large Numbers or Law of Averages                                    #
#-----------------------------------------------------------------------------#

# As n increases, the standard error of the average of a random var. decreases.
# When n is large, the average of the draws converges to the average of the sample
# space. 

# The law of averages only applies when n is very large and events are independents
# and not in small samples. 

# How large is large on CTL?

# in many circumstances, as few as 30 draws is enough to make the CLT useful. 
# However, ¡¡¡this IS NOT A GENERAL RULE!!!

# When the probability of success is really small, we need larger samples. In this
# cases, the Poisson distribution is more appropriate. 