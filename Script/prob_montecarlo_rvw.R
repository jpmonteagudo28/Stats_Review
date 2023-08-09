## Monte Carlo simulations - repeating random sampling to obtain numerical results
# Used to model the probability of diff. outcomes in a process that cannot be easily predicted
## because of intervention of random vars. 

## Main idea of MC simulation is that we can obtain an unbiased representative
## group of samples from a large pool of possibilities if the simulation is allowed
## to evolve randomly.

## Ergodicity: A point in a moving system will eventually visit all parts of the space 
## that the system moves in, in a uniform and random sense. The larger the number of 
## trials, the higher likelihood the outcome will converge to a value. 

beads <- rep(c("red","blue"), times = c(2,3))
sample(beads,1) #w/o replacement by default

B <- 10000
events <- replicate(B, sample(beads,1))
tab <-table(events)
print(tab)
prop.table(tab)
plot(tab)

# Sampling w. replacement w/o using replicate
events <- sample(beads, B, replace = T)
prop.table(table(events))

## Setting a random seed (initial value) w. set.seed()
?set.seed
set.seed(2023-7-27)
vec <- rnorm(10,5,2)
print(vec)

## Changing the seeds changes the values of the vector.
## Using the same seed allows you to produce a vector with same values. 
set.seed(89)
vec2 <- rnorm(10,5,2)
print(vec2)

## Discrete Probability Distributions
# 1.1 Independence: two events are independent if the outcome of one doesn't 
# affect the other. 
# Sampling w.o replacement leads to dependent events
x <- sample(beads,5)
x[2:5] 
#[1] "blue" "blue" "red"  "blue"
#What's the probability of drawing red? It's 1 or 100%. Every time a color
# is drawn, the probability of it occurring again will change. 

# Conditional Probabilities for dependent events. 
# if A and B are independent Pr(A|B) = Pr(A)

# Probability of two events - multiplication rule
# Pr(A U B) = Pr(A)*Pr(B|A) by the induction rule:
# Pr(A U B U C) = Pr(A)*Pr(B|A)*Pr(C|A U B)
# For independent events: Pr(A U B U C) = Pr(A)*Pr(B)*Pr(C)

# Finding probability that second card drawn is a king given that the first card drawn was a king
hands <- permutations(52,2, v = deck)
first <- hands[,1]
second <- hands[,2]
print(first); print(second)
sum(first %in% (kings <-paste("King",suits)))
# 204
sum(first %in% kings & second %in% kings)/sum(first %in% kings)
# 0.05882353

# Compute probability of natural 21 in blackjack

aces <- paste("Ace",suits)
facecard <- c("King","Queen","Jack","Ten")
facecard <- expand.grid(number = facecard, suit=suits)
facecard <- paste(facecard$number,facecard$suit)
hands2 <- combinations(52,2,v = deck)
mean(hands2[,1] %in% aces & hands2[,2] %in% facecard)
# 0.04826546

## Using MC simulation instead
hand <- sample(deck,2); print(hand)
B <- 10000
results <- replicate(B,{
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard)| (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)
# 0.047

# Birthday Problem
birth <- 1:365
bdays <- sample(birth,50,replace = T)
shared_bday <- duplicated(bdays)
sum(shared_bday)
# You can also check by using:
shared_bday2 <- any(duplicated(bdays))
# MC simulation
B <- 10000
sharedbday_check <- replicate(B, {
  bdays; any(duplicated(bdays))
})
mean(sharedbday_check)
# 1. it happens 100% of the time

## Creating MC function for bday problem
compbday <- function(n,A = 10000){
  smbdays <- replicate(A,{
    bdays <- sample(1:365,n,replace = T)
    any(duplicated(bdays))
  })
  mean(smbdays)
}

plot(1:60,sapply(1:60,compbday))
     
# Computing exact probability
#prob. of unique birthdays = (365 - (n+1))/365

excompute <- function(n){
  uniqprob <- seq(365,365-n+1)/365
  1- prod(uniqprob)
}
plot(1:60,sapply(1:60,excompute))

## Estimating how many experiments to run
B <- 10^seq(1,5, len = 100)
compbday <- function(B, n = 22){
  smbday <- replicate(B,{
    bdays <- sample(1:365,n,replace = T)
    any(duplicated(bdays))
  })
  mean(smbday)
}
prob <- sapply(B, compbday)
plot(log10(B),prob,type = "l", col = "royalblue")