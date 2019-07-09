library(tidyverse)
rc <- seq(0,16,.01)
sc <- c(rep(6, length(rc)), rep(5, length(rc)), rep(10, length(rc)), rep(11, length(rc)))

## RANDOM EFFECTS
## such that subject parameters have about a .4 correlation with true parameters
exponentsRandom <- 0
lossAversionRandom <- 0
pWeightRandom <- 0
interceptRandom <- 0

## within-subject noise
noiseSd <- 0

bias <- 1

subjectProfile <- buildSubjectProfile()
  
d <- data.frame(riskyCritical = rep(rc, 4), safeCritical = sc)

d$proba <- decision(riskyCritical = d$riskyCritical, safeCritical = d$safeCritical, bias = bias, subjectProfile = subjectProfile)

d %>% 
  ggplot(aes(x = riskyCritical, y = proba)) + 
  geom_line() + 
  facet_wrap(~safeCritical) + 
  scale_x_continuous(labels = 0:16, breaks = 0:16) + 
  geom_hline(yintercept = .5, linetype='dashed')


conditions <- data.frame(difficulty = factor(c(rep('easier', 2), rep('harder', 2))), 
                         difference = factor(rep(c('moderate', 'extreme'), 2)),
                         riskyCritical = c(4, 2, 12, 14),
                         safeCritical = c(6, 5, 10, 11))

conditions$proba <- with(conditions, decision(riskyCritical = riskyCritical, safeCritical = safeCritical, bias = bias, subjectProfile = subjectProfile))

conditions %>% 
  ggplot(aes(x = difficulty, y = proba, group = difference)) + 
  geom_bar(stat = 'identity', aes(fill = difference), position = position_dodge(width = .9)) + 
  ylim(0,1)
