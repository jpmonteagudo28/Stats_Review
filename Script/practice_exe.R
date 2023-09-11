# ---------------------------------------------------------------- #
# Continuous Probability Practice
set.seed(16)
act_scores <- rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)

# Prob. of scores greater/equal to 36
length(act_scores[which(act_scores >= 36)])
hi_act <- act_scores[which(act_scores >= 36)]
hi_prob <- length(hi_act)/length(act_scores)

# Prob of scores less than 10
lo_act <- act_scores[which(act_scores <= 10)]
low_prob <- length(lo_act)/length(act_scores)

#PDF
x <- 1:36
f_x <- data.frame(x,f=dnorm(x,20.9,5.7))
ggplot(f_x,aes(x,f)) + geom_line()

# Converting raw to z-scores
z_scores <-scale(act_scores)
length(z_scores[which(z_scores >= 2)])/length(z_scores)
quantile(act_scores, probs =.975)

x2 <- 1:36
ecdf <- function(a) mean(act_scores<=a)
act_test <-sapply(x2,ecdf)
plot(x2,act_test, lty = 2,xlab = "Scores", ylab = "Pr(x2 <= a)")
# Find the minimum integer score with a CDF of at least 0.95
min_score <- min(x2[act_test >= 0.95])

# Find 95th percentile of ACT scores
exp_95 <-qnorm(.95,20.9,5.7)

# Finding percentile for score == 26
sample_quantiles <- quantile(act_scores,probs = seq(0.01,.99,.01))
quant_26 <- sample_quantiles[which(sample_quantiles < 27 & sample_quantiles > 25.99)]
# 82% as first appearance of 26

# QQ-plot theoretical vs. sample_quantile
p <- seq(.01,.99,.01)
theoretical_quant <- qnorm(p,20.9,5.7)
sample_quantiles <- quantile(act_scores,probs = seq(0.01,.99,.01))
plot(theoretical_quant,sample_quantiles,pch = 20, col="black")
abline(0, 1, col = "red", lty = 2)

# Finding female shorter than 5 ft (60 in)
n <- 350
avg <- 64
s <- 3

# CDF
fem_height <- pnorm(60,avg,s) 
# 0.09121122

# Using the empirical CDF
emp_fheight <- rnorm(n,avg,s)
ecdf <- function(a) mean(emp_fheight<=a)
height_cdf <- sapply(emp_fheight,ecdf)
plot(emp_fheight,height_cdf, xlab = "height", ylab = "Pr(height <= a)")
ecdf(60) 
# 0.09428571

# Finding female shorter than 5 ft (60 in)
1 - ecdf(72)
# 0.002857143
1 - pnorm(72,avg,s)
# 0.003830381

# Finding female between 61 - 67 inches
ecdf(67.5) - ecdf(60.5)
# 0.7542857
pnorm(67.5,avg,s) - pnorm(60.5, avg,s)
# 0.756655

# Finding male height in 99th percentile
m <- 350
s <- 3
top_male_height <- qnorm(.99,avg,s)
# 75.97904

# Dist. of highest IQ scores in school district of 10,000 stu

B <- 1000
n <- 10000
avg <- 100
s <- 15

high_IQ <- replicate(B,{
  sim_IQ <- rnorm(n,avg,s)
  max(sim_IQ)
})
hist(high_IQ, col = "coral")

# --------------------------------------------------------------- #
# CLT, Random Variables and Law of Averages                       #
#-----------------------------------------------------------------#

#SE of guessing on all 44 questions of SAT
n <- 44
a <- 1 # correct answer
b <- -.25 # incorrect answer
p <- .2 # prob of correct guess
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))

# Use CLT to determine prob that a student guessed on SAT and scores at least 8 pts
# mean = E[X], X - number of guesses
# SE = std. error of expected value

B <- 1000 # Roundabout way of arriving at the answer
avg <- (n*a)+(4*n*b)
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
right_guess <- replicate(B,{
  sim_guess <- rnorm(1000,avg,se)
})
prop_high_8 <- sum(right_guess >= 8)/length(right_guess)
# [1] 0.00795

# Short answer
prop_high_8 <- 1 -pnorm(8,avg,se)
# [1] 0.007930666

# Given a seq. of correct answer probs p <- seq(.25,.95,.05). What is the 
# lowest value of p for which prob of test score >= 35 exceeds 80%

p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

# Five winning bets (6 to 1) on pockets of roulette, chance of losing money
# if 500 bets placed
p <- 5/38
a <- 6
b <- -1
EV <- a*p + b*(1-p)
SE <- abs(b-a)*sqrt(p*(1-p))

# Expected value of average payout over 500 bets
EV
# SE of the average payout over 500 bets
SE/sqrt(500)
# [1] 0.1058209

# EV of the sum of 500 bets
EV_500 <- EV*500
# SE of sum of 500 bets
SE_500 <- sqrt(500)*SE
# [1] 52.91045

#Prob of losing money(negative winning) over 500 bets
loss <- pnorm(0,EV_500,SE_500)

#------------------------------------------------------------------#
# Additional Practice Exercise (Ch.14  Intro to DS Irrizary)       #
#------------------------------------------------------------------#

# Prob. of getting green on American roulette
n <-38
red <- 18/38
black <- 18/38
green <- 2/38

# Payout for winning on green: 17 to 1
a <- 17
b <- -1
p <- green
set.seed(1)
x <- sample(c(17,-1),1000,replace = T, prob = c(2/38,36/38))
S <- sum(x)
# [1] $8 total winnings
prob_green <- length(x[which(x==17)])/length(x)
# [1] 0.056
EV_green <- a*p+b*(1-p) # Expected Value after 1 bet
# [1] -0.05263158
SE_green <- abs(b-a)*sqrt(p*(1-p)); SE_green # Standard Error 1 bet
# [1] 4.019344

# EV after 1000 bets
EV_1000 <- EV_green*1000
# [1] -52.63158
#SE after 1000 bets
SE_1000 <- SE_green*sqrt(1000)
# [1] 127.1028

# Prob of winning money by betting on green
win <- 1 - pnorm(0,EV_1000,SE_1000); win
# [1] 0.3394053

# MC simulation
B <- 1000
set.seed(1)
S <- replicate(B,{
  x <- sample(c(17,-1),100,replace = T, prob = c(1/19,13/19))
})
mean(S)
# [1] 0.29834 not exactly close to prob. using pnorm
sd(S)
# [1] 4.656678
sum(S)
# [1] 29834
prob_green <- length(S[which(S==17)])/length(S); prob_green
# [1] 0.07213

# Average winning/bet after betting 1000 times on green
set.seed(1)
Y2 <-mean(sample(c(17,-1),1000,replace = T, prob = c(1/19,13/19)))
# [1] .494

Y <-  replicate(2500,{
  x <- sample(c(17,-1),1000,replace = T, prob = c(1/19,13/19))
 mean(x)
})
avg<- mean(Y) # .2789
sdY <- sd(Y) # . 1475

# Histogram of average winning/bet after betting 1000 times on green
x_val <- seq(min(Y),max(Y), length.out = 100)
y_val <- abs(dnorm(x_val,avg, sdY)*diff(Y[1:2])*100)
hist(Y,col = "palegreen3", breaks =seq(min(Y), max(Y), length.out = 25),xlab = "Average winnings per bet", ylab = "Frequency",
     main = "Average winnings/bet after betting 1000 on green")
lines(x_val,y_val, col = "royalblue3",lwd = 2)

#------------------------------------------------#
# Interest rates  (CH14.12)                      #
#------------------------------------------------#

p <- .03
foreclosure_loss <- -200000
n <- 10000

S <- sample(c(0,1),n,prob = c(1-p,p), replace = T)
sum(S*foreclosure_loss)
# [1] -55800000

#MC sim
losses <- replicate(10000,{
  S <- sample(c(0,1),n,prob = c(1-p,p), replace = T)
  sum(S*foreclosure_loss)
})

losses <- data.frame(losses=losses/10^6)
ggplot(losses, aes(x = losses))+ geom_histogram()

exp_val <- n*(p*foreclosure_loss + (1-p)*0)
# [1] -6e+07
std_error <- sqrt(n)*abs(foreclosure_loss)*sqrt(p*(1-p));std_error
# [1] 3411744

# Each loan is $180,000. How much to charge in interest to break even?
xtra_amt <- -foreclosure_loss*p/(1-p) # $6185.57
interest <- (xtra_amt/180000)*100 # 3.44%

# How much to charge in interest to minimize losses? P(s < 0) = .05
z <- qnorm(.05)
nw_xtra_amt <- -foreclosure_loss*((n*p)-z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1*p)))
# [1] $6767.548
nw_interest <- (nw_xtra_amt/180000)*100
# 3.76

# If the bank wants to minimize the probability of losing money, setting a lower risk limit
# may not cause interest rates to increase. 