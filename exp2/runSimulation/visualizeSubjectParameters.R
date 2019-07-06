library(tidyverse)


#exponentsRandom <- .2
exponentsRandom <- .42
lossAversionRandom <- .60
pWeightRandom <- .25


visualizeSubjectDifferences <- function(fixedEffect, effectName) {
  ## set the random effect in the environment
  d <- data.frame(bias = seq(0, 1, .001))
  d$trueParameter <- fixedEffect * convertBias(d$bias, fixedEffect)
  d$finalParameter <- 0
  subjectEffectName <- paste('subject', effectName, sep = '')
  
  for (row in 1:nrow(d)) {
     subjectProfile <- buildSubjectProfile()
     if (effectName == 'LossAversion') {
       d[row, 'finalParameter'] <- max(fixedEffect * convertBias(d[row,]$bias, fixedEffect) + subjectProfile[subjectEffectName], 1)
       #d[row, 'finalParameter'] <- fixedEffect * convertBias(d[row,]$bias, fixedEffect) + subjectProfile[subjectEffectName]
     } else {
       ## Understand that the following line isn't the actual implementation for pWeight because im not converting the bias for that 
       d[row, 'finalParameter'] <- max(min(fixedEffect * convertBias(d[row,]$bias, fixedEffect) + subjectProfile[subjectEffectName], 1), 0)
       #d[row, 'finalParameter'] <- fixedEffect * convertBias(d[row,]$bias, fixedEffect) + subjectProfile[subjectEffectName]
     }
  }
  
  return(data.frame(true = d$trueParameter, final = d$finalParameter))
}

fixedEffectContainer <- c(lossAversionFixed, exponentsFixed, pWeightFixed)
effectNameContainer <- c('LossAversion', 'Exponents', 'PWeight')

for (i in 1:3) {
  if (i == 1) {
    d <- visualizeSubjectDifferences(fixedEffectContainer[i], effectNameContainer[i])
  } else {
    d <- cbind(d, visualizeSubjectDifferences(fixedEffectContainer[i], effectNameContainer[i]))
  }
}

colnames(d) <- c('lossAversion_True', 'lossAversion_Final', 'exponents_True', 'exponents_Final', 'pWeight_True', 'pWeight_Final')

cors <- with(d, c(cor = cor(exponents_True, exponents_Final), cor = cor(lossAversion_True, lossAversion_Final), cor = cor(pWeight_True, pWeight_Final)))

cors <- data.frame(parameter = c('Diminishing Sensitivity', 'Loss Aversion', 'Probability Weight'), Truth = round(cors, 2), xCoords = c(.9, 1.25, .95), yCoords = c(.1, 4.5, .5))

d %>% 
  mutate(id = 1:nrow(.)) %>% 
  gather(variable, value, lossAversion_True:pWeight_Final) %>% 
  separate(variable, c('parameter', 'boolean')) %>% 
  spread(boolean, value) %>% 
  rename(Truth = True) %>% 
  mutate(parameter = factor(parameter)) %>%
  mutate(parameter = recode(parameter, 'lossAversion' = 'Loss Aversion', 'exponents' = 'Diminishing Sensitivity', 'pWeight' = 'Probability Weight')) %>% 
  ggplot(aes(x = Truth, y = Final)) + 
  geom_point() +
  geom_smooth(method = 'lm', color = 'black') + 
  facet_wrap(~parameter, scales = 'free') + 
  geom_text(data = cors, aes(label = paste('r=', Truth, sep = ''), x = xCoords, y = yCoords), color = 'red', fontface='bold') + 
  xlab('True Parameter') +
  ylab('Subject Parameter') +
  theme_bw() + 
  theme(strip.background = element_rect(fill = 'white', color = 'black'))  
  
  
  
  
  
  
  
  
  
  

  
