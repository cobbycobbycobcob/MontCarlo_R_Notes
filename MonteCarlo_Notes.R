# Monte Carlo Simulations

# Collect data on a give topic
# Find a structural equation (y = x0b0 + x1b1 + ... xnbn) that fits the data
# For each value of X in the structural equation, find the upper and lower bound, mean, standard deviation, and type of distribution
## For each point estimate X, derive the integral using the distribution parameters
# Repeatedly model that structural equation (i.e. 10000+ iterations), each time randomly selecting values of X using the above distribution parameters

# Simple Monte Carlo simulation
# Deriving an integral
# A simulation the draws 100000 samples bewtween a given distribution
## Normal
## Mean 1, SD 10
## Lower bound: 3
## Upped bound: 6

sim_count <- 100000
montecarlo_values <- rnorm(sim_count,1,10)
montecarlo_integral <- sum(montecarlo_values >=3 & montecarlo_values <= 6)/sim_count
# montecarlo_values >= 3 & montecarlo_values <= 6 is a blooean expression and will return boolean values (i.e. T or F)
# The integral is therefore the number of simulations where the value is between 3 and 6, divided by the total number of trials

# Sampling from the binomial distribution
## e.g. coin toss
## Two possible outcomes, heads and tails (0, 1)
coin.toss <- function() {
  sample(c(0,1), 1)
}

## Repeat that toss 'sim_count' times
## Collect number of times where coin.toss() == 1, i.e. tails, divided by total number of tosses
montecarlo_binom <- sum(replicate(sim_count, coin.toss() == 1))/ sim_count

## Using this simple coin toss simulation, construct a trial of 10 tosses
### For each trial of 10, record all that have more than 3 heads
### Replace = T because it is a collection of tosses, need to record separate outcome values
cointoss10 <- function() {
 sum(sample(c(0,1), 10, replace = T)) > 3
}

## Repeat sim_count number of trials
montecarlo_binomtrials <- sum(replicate(sim_count, cointoss10() == 1))/sim_count
montecarlo_binomtrials

