##### calculating power for log rank test

d <- 9 # total number of events (between both groups we think)
coef <- 1.5 #50% difference



pnorm(sqrt(d*(0.5^2*log(coef)))-qnorm(0.975))
#if I had 10 individuals die, and a 50% diff in death rates

#### for infection across all 3 experiments
d <- 23
coef <- 1.16 #### actual effect size
### power: 15% chance of detecting
coef <- 1.5 #if there was a 50% diff, what would power be? 
### power: 33.3% chance of detecting
coef <- 1.9 #if there was a 90% diff, what would power be? 
### power: 48.5% chance of detecting
pnorm(sqrt(d*(0.5^2*log(coef)))-qnorm(0.975))


#### for death across all 3 experiments
d <- 2
coef <- 1.85 #### actual effect size
### power: 7.9% chance of detecting
pnorm(sqrt(d*(0.5^2*log(coef)))-qnorm(0.975))



#### for infection in experiment 1
d <- 8
coef <- 1.262 #### actual effect size
coef <- 1.5 #if there was a 50% diff, what would power be? 
### power: 14.5% chance of detecting
coef <- 1.9 #if there was a 90% diff, what would power be? 
### power: 20.4% chance of detecting
pnorm(sqrt(d*(0.5^2*log(coef)))-qnorm(0.975))
  
#### for infection in experiment 2
d <- 9
coef <- 1.5 #if there was a 50% diff, what would power be? 
### power: 15.8% chance of detecting
coef <- 1.9 #if there was a 90% diff, what would power be? 
### power: 22.4% chance of detecting

coef <- 1.307 ## actual effect size
pnorm(sqrt(d*(0.5^2*log(coef)))-qnorm(0.975))

#### for infection in experiment 3
d <- 6
coef <- 1.5 #if there was a 50% diff, what would power be? 
### power: 11.9% chance of detecting
coef <- 1.9 #if there was a 90% diff, what would power be? 
### power: 16.4% chance of detecting

coef <- 1.101 ##actual effect size
pnorm(sqrt(d*(0.5^2*log(coef)))-qnorm(0.975))
