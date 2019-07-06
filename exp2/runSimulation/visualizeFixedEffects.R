rc <- seq(0,16,.01)
sc <- c(rep(2, length(rc)), rep(4, length(rc)), rep(12, length(rc)), rep(14, length(rc)))
  
d <- data.frame(riskyCritical = rep(rc, 4), safeCritical = sc)

d$proba <- decision(riskyCritical = d$riskyCritical, safeCritical = d$safeCritical, bias = 1, subjectProfile = subjectProfile)

d %>% 
  ggplot(aes(x = riskyCritical, y = proba)) + 
  geom_line() + 
  facet_wrap(safeCritical) + 
  scale_x_continuous(labels = 0:16, breaks = 0:16)


conditions$proba <- with(conditions, decision(riskyCritical = riskyCritical, safeCritical = safeCritical, bias = 1, subjectProfile = subjectProfile))

conditions %>% 
  ggplot(aes(x = difficulty, y = proba, group = difference)) + 
  geom_bar(stat = 'identity', aes(fill = difference), position = position_dodge(width = .9)) + 
  ylim(0,1)
