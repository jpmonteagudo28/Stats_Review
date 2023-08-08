# The addition Rule
# P(A or B) = P(A)+P(B)-P(A U B)

# The Monty Hall Problem

# Not switching
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

# Switching doors
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)

# CDF and empirical CDF (eCDF)

data(ChickWeight)
summary(ChickWeight)
ChickWeight$DIet <- as.factor(ChickWeight$Diet)
x <-subset(ChickWeight,Diet == 1); x2 <- x$weight
# defining the eCDF function
ecdf <- function(a) mean(x2<=a)
# for any given value of a, we get the proportion of values smaller or equal to a

# We can ask what is the probability of finding a chicken weighing over 135g.
# We simply subtract:
1 - ecdf(135) # 0.2545455
# Plot showing CDF for chicken weights for diet(1)
test <- sapply(0:315,ecdf)
plot(0:315,test, xlab = "Weights", ylab ="Pr(mean <= a)")

# Theoretical Distribution

# The cumulative distribution for the normal distribution is defined by a formula
# A random, continuous quantity is normally distributed with mean and sd
# if its probability distribution is defined by F(a) = pnorm(a, mean,sd)
1 -pnorm(135,mean(x2),sd(x2)) # [1] 0.283978 similar to 1 - ecdf(135)

# We define probability for continuous vars in intervals as the probability of an exact
# value is not defined. 
# Discretization: transforming a continuous var, model or function into a discrete form
# For example, the chicken weights are continuous but displayed as whole numbers.

# Probability Density

# The probability density at x is defined as the function f(x) such that the probability 
# distribution F(a), which is the probability of x being less than or equal to a
# is the integral of all values up to a of f(x)*d(x)

dnorm() #density distribution
# Plotting the density curve for the normal distribution
# dnorm(z)is the probability density of a certain z-score, using a range of z-scores
# we can plot the PDF
library(ggplot2)
z <- seq(-4,4,length = 150)
df<- data.frame(z,f=dnorm(z))
ggplot(df,aes(z,f)) + geom_line()

# dnorm() gives densities for the standard normal distribution by default.
# Probabilities for alternative normal distr. with mean mu and sd sigma can
# be evaluated with: 
dnorm(z,mu,sigma)

# Generating normally distributed data

data(ChickWeight)
ChickWeight$DIet <- as.factor(ChickWeight$Diet)
x <-subset(ChickWeight,Diet == 1); x2 <- x$weight
n <- length(x2)
avg <- mean(x2)
s <- sd(x2)
sim_heights <- rnorm(n,avg,s)
hist(sim_heights,col="royalblue", breaks = seq(floor(min(sim_heights)),ceiling(max(sim_heights)),by = 15))

# Running MC simulation to find chicken with weight > 315 g

B <- 10000
heaviest <- replicate(B,{
  sim_chickwgt <- rnorm(800,avg,s)
  max(sim_chickwgt)
})
mean(heaviest >= 315)
# [1] 0.07

hist(heaviest, col = "royalblue")

# Other distributions
# We have the student-t, chi-square,exponential, gamma, beta
# functions in R: q - quantile, p - prob, d-density, r-random
