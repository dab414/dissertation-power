library(tidyverse)


pToOr <- function(p){
  return(log(p/(1-p)))
}

trial <- rep(1:700, 4)

difficulty <- c(rep(0, 1400), rep(1, 1400))
difference <- c(rep(c(rep(0, 700), rep(1, 700)), 2))

d <- data.frame(trial = trial, difficulty = difficulty, difference = difference)

## betas
## for some reason these values work but i have no idea why
## alright so i just trial-and-error'ed these to get these close
intercept <- pToOr(.55)
difficultyB <- pToOr(.71)
differenceB <-pToOr(.55)
twoWay <- pToOr(.84)

bias <-1

d$proba <- with(d, (intercept*bias + difficulty*difficultyB*bias + difference*differenceB*bias + difficulty*difference*twoWay*bias) * (trial / 700))

d$proba <- exp(d$proba)/(1+exp(d$proba))

# d %>% 
#   filter(trial == 25 | trial == 675) %>% 
#   mutate(trialCode = ifelse(trial == 25, 'early', 'late')) %>% 
#   ggplot(aes(x = difficulty, y = proba, group = factor(difference))) + geom_bar(stat = 'identity', aes(fill = factor(difference)), position = position_dodge(width=.9)) +
#   facet_wrap(~trialCode) + ylim(0,1)
#   

d %>% 
  filter(trial == 700)