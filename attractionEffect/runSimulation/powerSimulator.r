powerSimulator <- function(n, bias, nSims){
  library(ez)
  library(tidyverse)
  
  ## function takes in:
    ## n: sample size
    ## bias: functional effect size
    ## nSims: number of simulations
  ## returns:
    ## one row of a df with n, POWER, effect size, nSims, and power outcome
  ## im assuming 700 trials per subject for now
  
  oneShot <- ifelse(nSims == 1, 1, 0)
  
  ## controls size of between-subject variability in baseline preference
  intSd <- 3
  ## controls between-subject variability in size of decoy effect
  betaSd <- 0.5
  ## controls extent of within-subject variability
  noiseSd <- 0.25
  ## just for fun
  covariance <- 0
  
  experimentResults <- data.frame(n = numeric(), bias = numeric(), effectSize = numeric(), pValue = numeric(), predictedDirection = logical())
  write.csv(experimentResults, 'experimentResultsCache.csv', row.names = FALSE)
  
  for (sim in 1:nSims){
    experimentData <- data.frame(subject = numeric(), trial = numeric(), condition = character(), lowDemandSelection = numeric())
    write.csv(experimentData, 'experimentDataCache.csv', row.names = FALSE)
    print(paste('Simulation', sim, 'of', nSims))
    
    for (subject in 1:n){
      
      ## first element is the intercept, mean 0.5 (choose randomly), sd controls variability in baseline preferences, constrained between 0:1
      ## second element is scalar to control size of the bias, centered at 1, floor of 0; bigger SD means more between-subject variability in size of the effect
      subjectInt <- rnorm(1, 0, intSd)
      subjectSlope <- rnorm(1, 0, betaSd) + subjectInt * covariance
      subjectProfile <- c(subjectInt, subjectSlope)
      
      ## determine decision threshold for decoy absent
      decisionThreshold <- subjectProfile[1] + rnorm(1, 0, noiseSd)
      decisionThreshold <- exp(decisionThreshold) / (1 + exp(decisionThreshold))
      ## two-choice phase
      for (trial in 1:75) {
        experimentData <- rbind(experimentData, data.frame(subject = subject, trial = trial, condition = 'two-choice', lowDemandSelection = ifelse(runif(1) < decisionThreshold, 1, 0)))
      }
      ## determine decision threshold for decoy absent
      decisionThreshold <- subjectProfile[1] + rnorm(1, 0, noiseSd)
      decisionThreshold <- exp(decisionThreshold) / (1 + exp(decisionThreshold))
      ## filler trials
      for (trial in 76:182){
        experimentData <- rbind(experimentData, data.frame(subject = subject, trial = trial, condition = 'filler', lowDemandSelection = ifelse(runif(1) < decisionThreshold, 1, 0)))
      }
      ## three-choice
      for (trial in 151:300) {
        ## randomly generate decoy
        decoy <- ifelse(runif(1) > .5, 'nearHigh', 'nearLow')
        decoyCode <- 'nearHigh'
        decoyNum <- -1
        
        if (decoy == 'nearLow'){
          decoyCode <- 'nearLow'
          decoyNum <- 1
        }
        
        ### COMPUTE DECISION ###
        ## determine decision threshold for when the decoy is present
        ## LEVEL 2 EQUATIONS -- BETWEEN SUBJECT PROCESS
        intercept <- subjectProfile[1]
        slope <- bias + subjectProfile[2]
        ## LEVEL 1 EQUATION -- WITHIN SUBJECT PROCESS
        decisionThreshold <- intercept + decoyNum * slope + rnorm(1, 0, noiseSd)
        ## convert to probability
        decisionThreshold <- exp(decisionThreshold) / (1 + exp(decisionThreshold))
        
        # print(paste('Subject int: ', subjectProfile[1], ', Subject beta: ', subjectProfile[2], ', Overall beta: ', (subjectProfile[2] * bias) * decoyNum, sep=''))
        # print(decoyCode)
        # print(paste('Decision threshold: ', decisionThreshold))
        
        experimentData <- rbind(experimentData, data.frame(subject = subject, trial = trial, 
                                                           condition = decoyCode, 
                                                           lowDemandSelection = ifelse(runif(1) < decisionThreshold, 1, 0)))
        
      }
      
      ## every 10 subjects, save out data to cache and reset experimentData data frame
      if (subject %% 10 == 0 | subject == n){
        experimentData <- rbind(read.csv('experimentDataCache.csv', header = TRUE), experimentData)
        write.csv(experimentData, 'experimentDataCache.csv', row.names = FALSE)
        print(paste('Subject: ', subject, ' of ', n, sep = ''))
        if (subject != n){
          experimentData <- data.frame(subject = numeric(), trial = numeric(), condition = character(), lowDemandSelection = numeric())
        }
      }
      
      
    } ## end subject loop
  
    experimentResults <- computeStats(experimentData, n, bias)
    experimentResults <- rbind(read.csv('experimentResultsCache.csv', header = TRUE), experimentResults)
    write.csv(experimentResults, 'experimentResultsCache.csv', row.names = FALSE)
    
    if (sim != nSims) {
      experimentResults <- data.frame(n = numeric(), bias = numeric(), effectSize = numeric(), pValue = numeric(), predictedDirection = logical())
    }
    
  } ## end sim loop
  
  if (oneShot) {
    #plotResults(experimentData)
    return(experimentData)
  } else {
    experimentResults <- experimentResults %>% 
      mutate(isSig = ifelse(pValue < .05 & predictedDirection, 1, 0))
    return(data.frame(n = n, bias = abs(bias), power = mean(experimentResults$isSig), avgEffectSize = mean(experimentResults$effectSize), sdEffectSize = sd(experimentResults$effectSize)))
  }
  
}

computeStats <- function(experimentData, n, bias){
  ## takes as input the dataset from a single simulation and returns a dataset summarizing parameters from an ANOVA as well as the levels of the hyperparameters
  experimentData$subject <- factor(experimentData$subject)
  m1 <- ezANOVA(wid = subject, within = condition, dv = lowDemandSelection, data = experimentData, detailed = TRUE)
  effectSize <- m1$ANOVA$SSn[2] / (m1$ANOVA$SSn[2] + m1$ANOVA$SSd[2])
  pValue <- m1$ANOVA$p[2]
  
  ## test whether effects are in predicted direction
  predictedDirection <- testDirection(experimentData)
  return(data.frame(n = n, bias = abs(bias), effectSize = effectSize, pValue = pValue, predictedDirection = predictedDirection))
}

testDirection <- function(experimentData){
  ## tests whether effects are in expected direction
  ## return boolean
  
  ## convert to cell means
  cellMeans <- experimentData %>% 
    group_by(subject, condition) %>% 
    summarize(lowDemandSelection = mean(lowDemandSelection)) %>% 
    group_by(condition) %>% 
    summarize(lowDemandSelection = mean(lowDemandSelection)) 
  
  output <- 0
  
  nearLow <- cellMeans[cellMeans$condition == 'nearLow',]$lowDemandSelection
  nearHigh <- cellMeans[cellMeans$condition == 'nearHigh',]$lowDemandSelection
  twoChoice <- cellMeans[cellMeans$condition == 'two-choice',]$lowDemandSelection
  filler <- cellMeans[cellMeans$condition == 'filler',]$lowDemandSelection
  
  if  (nearLow > nearHigh & nearLow > twoChoice & nearLow > filler) {
    if (nearHigh < twoChoice & nearHigh < filler) {
      output <- 1
    }
  }
  
  return(output)
  
}


plotResults <- function(d) {
  ## bar graph
 d %>%
   group_by(subject, condition) %>%
   summarize(lowDemandSelection = mean(lowDemandSelection)) %>%
   group_by(condition) %>%
   summarize(lds = mean(lowDemandSelection), se = sd(lowDemandSelection) / sqrt(n())) %>%
   ggplot(aes(x = condition, y = lds)) + geom_bar(stat = 'identity') + ylim(0,1) + geom_errorbar(aes(ymin = lds - se, ymax = lds + se), width = 0.5) 
  

  ## faceted histograms
  d %>% 
    filter(condition == 'nearHigh' | condition == 'nearLow') %>% 
    group_by(subject, condition) %>% 
    summarize(lds = mean(lowDemandSelection)) %>% 
    ggplot(aes(x = condition, y = lds, group = subject)) + geom_line(aes(group = subject)) 
    
  
}



