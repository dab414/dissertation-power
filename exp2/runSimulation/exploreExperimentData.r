source('powerSimulator.r')
oneExperiment <- powerSimulator(100, 1, 1, 1)

m1 <- ezANOVA(wid = subject, within = .(difference, difficulty), dv = riskySelection, data = oneExperiment, detailed = TRUE)
m1
paste('Difficulty Effect Size:', round(m1$ANOVA$SSn[3] /(m1$ANOVA$SSn[3] + m1$ANOVA$SSd[3]), 2))

t.test(oneExperiment[oneExperiment$difficulty == 'harder',]$riskySelection, mu = .5)
t.test(oneExperiment[oneExperiment$difficulty == 'easier',]$riskySelection, mu = .5)
t.test(oneExperiment[oneExperiment$difficulty == 'easier',]$riskySelection, oneExperiment[oneExperiment$difficulty == 'harder',]$riskySelection, paired = TRUE)

## visualize difference
oneExperiment %>% 
  group_by(subject, difference) %>% 
  summarize(riskySelection = mean(riskySelection)) %>% 
  ggplot(aes(x = difference, y = riskySelection, group = subject)) +
  geom_line()

## visualize difficulty

dCells <- oneExperiment %>% 
  group_by(subject, difficulty) %>% 
  summarize(riskySelection = mean(riskySelection)) %>% 
  group_by(difficulty) %>% 
  summarize(riskySelection = mean(riskySelection)) 

oneExperiment %>% 
  group_by(subject, difficulty) %>% 
  summarize(riskySelection = mean(riskySelection)) %>% 
  ggplot(aes(x = difficulty, y = riskySelection, group = subject)) +
  geom_line() + 
  geom_bar(data = dCells, stat = 'identity', aes(x = difficulty, y = riskySelection, group = 1), alpha = .5) + 
  ylim(0,1)

oneExperiment %>% 
  group_by(subject, difficulty, difference) %>% 
  summarize(riskySelection = mean(riskySelection)) %>% 
  group_by(difficulty, difference) %>% 
  summarize(refDev = mean(riskySelection), se = sd(riskySelection) / sqrt(n())) %>% 
  ggplot(aes(x = difficulty, y = refDev, group = difference)) +
  geom_bar(aes(fill = difference), position = position_dodge(width = .9), stat = 'identity') + 
  geom_errorbar(aes(ymin = refDev - se, ymax = refDev + se), position = position_dodge(width = .9), width = .5) + 
  ylim(0,1)

