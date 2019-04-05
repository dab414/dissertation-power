d %>%
  #filter(condition == 'nearHigh' | condition == 'nearLow') %>% 
  group_by(subject, condition) %>%
  summarize(lowDemandSelection = mean(lowDemandSelection)) %>%
  group_by(condition) %>%
  summarize(lds = mean(lowDemandSelection), se = sd(lowDemandSelection) / sqrt(n())) %>%
  ggplot(aes(x = condition, y = lds)) + geom_bar(stat = 'identity', width = .7) + ylim(0,1) + geom_errorbar(aes(ymin = lds - se, ymax = lds + se), width = 0.5) +
  ylim(0,1) +
  theme_bw() #+
  # theme(panel.grid.major = element_blank(), #panel.grid.minor = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title.y = element_blank())

#ggsave('barPlot.png', width = 5.5, height = 3.5, units = 'in')


## subject lines
d %>% 
 # filter(condition == 'nearHigh' | condition == 'nearLow') %>% 
  group_by(subject, condition) %>% 
  summarize(lds = mean(lowDemandSelection)) %>% 
  ggplot(aes(x = condition, y = lds)) + #geom_boxplot(width = .5, alpha = .5, fatten = NULL) +
  #stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
              # width = 0.5, size = 1, linetype = "solid") +
  geom_line(aes(group = subject)) +
  ylim(0,1) +
  theme_bw() #+
  # theme(panel.grid.major = element_blank(), #panel.grid.minor = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title.y = element_blank())

#ggsave('boxPlot.png', width = 5.5, height = 3.5, units = 'in')