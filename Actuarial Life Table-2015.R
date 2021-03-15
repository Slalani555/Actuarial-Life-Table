library(tidyverse)
library(dslabs)
#Just as banks must decide how much to charge as interest on loans based on estimates of loan defaults, insurance companies must decide how much to charge as premiums for policies given estimates of the probability that an individual will collect on that policy. 
data(death_prob)
head(death_prob)
p<- death_prob%>% filter(age=="50" & sex=="Female" )
p
class(p)
p<- p%>% .$p
class(p)
#The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder remains alive is the premium $1,150.
#The expected value and Standard error of the company's net profit on one policy for a 50 year old female
loss<- -150000
gain<- 1150
exp_val<-p*loss+(1-p)*gain
exp_val
std_error<-abs(gain-loss)*sqrt(p*(1-p))
std_error
#the expected value and std error of the company's profit over all 1,000 policies for 50 year old females
n<-1000
mu<- n*exp_val
mu
se<- sqrt(n)*std_error
se
#Central Limit Theorem
#the prob that the insurance company loses money on this set of 1,000 policies
pnorm(0,mu,se)
#50 year old males have a different probability of death than 50 year old females. We will calculate a profitable premium for 50 year old males
p_male<- death_prob%>% filter(age=="50" & sex=="Male" )
p_male
class(p_male)
p_male <- p_male %>% .$p
p_male
class(p_male)
#Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000. 
#the expected value of the sum of draws
mu_male=700000
premium<- (mu_male/n)-(p_male*loss)/(1-p_male)
premium
#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums
se_male<-sqrt(n)*abs(premium-loss)*sqrt(p_male*(1-p_male)) 
se_male
#probability of losing money on a series of 1,000 policies to 50 year old males
pnorm(0,mu_male,se_male)


#Life insurance rates are calculated using mortality statistics from the recent past. They are priced such that companies are almost assured to profit as long as the probability of death remains similar. If an event occurs that changes the probability of death in a given age group, the company risks significant losses.

#we'll look at a scenario in which a lethal pandemic disease increases the probability of death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.
#expected value and std error of the company's profits over 1,000 policies
new_p<- 0.015
new_mu<-n*(new_p*loss+(1-new_p)*gain)
new_mu
new_se<- sqrt(n)*abs(gain-loss)*sqrt(new_p*(1-new_p))
new_se
#probability of the company losing money
pnorm(0,new_mu,new_se)
#Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out of business.
#What is the probability of losing more than $1 million
pnorm(-1000000,new_mu,new_se)
#the lowest death probability for which the chance of losing money exceeds 90%
p<- seq(0.01,0.03,0.001)
prob_losing<- function(s){
  exp_value<- n*(s*loss+(1-s)*gain)
  s_error <- sqrt(n)*abs(gain-loss)*sqrt(s*(1-s))
  pnorm(0,exp_value,s_error)
}
prob<- sapply(p,prob_losing)
plot(prob,p)

# the lowest death probability for which the chance of losing over $1 million exceeds 90%
p<- seq(0.01,0.03,0.0025)
prob_losing<- function(s){
  exp_value<- n*(s*loss+(1-s)*gain)
  s_error <- sqrt(n)*abs(gain-loss)*sqrt(s*(1-s))
  pnorm(-1000000,exp_value,s_error)
}
prob<- sapply(p,prob_losing)
plot(prob,p)

#a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 when there is no claim,
#reported profit(or loss) in millions
set.seed(25,sample.kind="Rounding")
n<-1000
p_loss<- 0.015
X<- sample(c(1,0),n,replace=TRUE,prob=c(p_loss,1-p_loss))
loss<- -150000*sum(X==1)/10^6
loss
profit<- 1150*sum(X==0)/10^6
profit
loss+profit
set.seed(27,sample.kind="Rounding")
B<- 10000
S<- replicate(B,{
  X<- sample(c(1,0),n,replace=TRUE,prob=c(p_loss,1-p_loss))
  loss<- -150000*sum(X==1)/10^6
  profit<- 1150*sum(X==0)/10^6
  loss+profit
  
})
mean(S<=-1)
#Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a premium cost for which the probability of losing money is under 5%, assuming the death rate stays stable at p=0.015.
z<- qnorm(0.05)
l<--150000
x<- -l * (n*p_loss-z*sqrt(n*p_loss*(1-p_loss)))/(n*(1-p_loss)+z*sqrt(n*p_loss*(1-p_loss)))
x #premium required for a 5% chance of losing money
expected_profit<- p_loss*l+(1-p_loss)*x #expected_profit over one policy
expected_profit
n*expected_profit #expected_profit over 1000 policy
#MOnte Carlo simulation
B<-10000
set.seed(28,sample.kind="Rounding")
S_new<- replicate(B,{
  draws<- sample(c(l,x),n,replace=TRUE,prob=c(p_loss,1-p_loss))
  sum(draws)
})
mean(S_new)# expected value of the profit over n loans
mean(S_new<0)# probability of losing money

#The company cannot predict whether the pandemic death rate will stay stable.
#Monte Carlo Simulation for 10000 iterations
set.seed(29,sample.kind="Rounding")
n<-1000
B<-10000
profit_predict<- replicate(B,{
  new_p<- p_loss+sample(seq(-0.01,0.01,length=100),1) # randomly changes p by adding a value between -0.01 and 0.01
  draws<- sample(c(x,l),n,prob=c(1-new_p,new_p),replace=TRUE) # uses a random p to generate a sample of 1000 policies
  sum(draws)
})
mean(profit_predict)
mean(profit_predict<0)
mean(profit_predict< -1000000)

