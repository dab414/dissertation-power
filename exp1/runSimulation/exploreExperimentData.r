d0 <- powerSimulator(30, 1, 1)

d <- translateToDeviation(d0)

ezANOVA(wid = subject, within = .(difference, difficulty), dv = referenceDeviation, data = d, detailed = TRUE)

## visualize difference
d %>% 
  group_by(subject, difference) %>% 
  summarize(referenceDeviation = mean(referenceDeviation)) %>% 
  ggplot(aes(x = difference, y = referenceDeviation, group = subject)) +
  geom_line()

## visualize difficulty
d %>% 
  group_by(subject, difficulty) %>% 
  summarize(referenceDeviation = mean(referenceDeviation)) %>% 
  ggplot(aes(x = difficulty, y = referenceDeviation, group = subject)) +
  geom_line()

## interaction overall
d0 %>% 
  group_by(subject, difficulty, difference) %>% 
  summarize(referenceSelection = mean(referenceSelection)) %>% 
  group_by(difficulty, difference) %>% 
  summarize(refSel = mean(referenceSelection), se = sd(referenceSelection) / sqrt(n())) %>% 
  ggplot(aes(x = difficulty, y = refSel, group = difference)) +
  geom_bar(aes(fill = difference), position = position_dodge(width = .9), stat = 'identity') + 
  geom_errorbar(aes(ymin = refSel - se, ymax = refSel + se), position = position_dodge(width = .9), width = .5)


d %>% 
  group_by(subject, difficulty, difference) %>% 
  summarize(referenceDeviation = mean(referenceDeviation)) %>% 
  group_by(difficulty, difference) %>% 
  summarize(refDev = mean(referenceDeviation), se = sd(referenceDeviation) / sqrt(n())) %>% 
  ggplot(aes(x = difficulty, y = refDev, group = difference)) +
  geom_bar(aes(fill = difference), position = position_dodge(width = .9), stat = 'identity') + 
  geom_errorbar(aes(ymin = refDev - se, ymax = refDev + se), position = position_dodge(width = .9), width = .5)

## explore learning

## averaged across subjects
d0 %>% 
  unite(condition, c('difference', 'difficulty')) %>% 
  group_by(condition, trial) %>% 
  summarize(referenceSelection = mean(referenceSelection)) %>% 
  ggplot(aes(x = trial, y = referenceSelection, group = condition)) +
  geom_line(aes(color = condition)) + 
  ylim(0,1)

## binned and broken down by individual subjects
d0 %>% 
  mutate(bin = rep(rep(1:(175/5), each = 5), 30*4)) %>% 
  group_by(subject, difference, difficulty, bin) %>% 
  summarize(referenceSelection = mean(referenceSelection)) %>% 
  filter(subject < 11) %>% 
  unite(condition, c('difference', 'difficulty')) %>% 
  ggplot(aes(x = bin, y = referenceSelection, group = condition)) + 
  geom_line(aes(color = condition)) + 
  facet_wrap(~subject)
