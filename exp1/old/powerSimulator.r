## global vars
nTrials <- 700

## FIXED EFFECTS
interceptFixed <- .55
difficultyFixed <- .71
differenceFixed <-.55
twoWayFixed <- .84

## RANDOM EFFECTS
interceptRandom <- 3
difficultyRandom <- 1
differenceRandom <- 1 - (difficultyFixed - differenceFixed)
twoWayRandom <- 1


decision <- function(difference, difficulty, bias, trial, trialMax, subjectProfile) {
  ## takes as input difference [0, 1] and difficulty [0, 1]
  ## returns the probability of selecting the low-demand deck
  
  
  ## convert fixed effects to log odds
  interceptFixed <- toLogOdds(interceptFixed)
  difficultyFixed <- toLogOdds(difficultyFixed)
  differenceFixed <-toLogOdds(differenceFixed)
  twoWayFixed <- toLogOdds(twoWayFixed)
  
  
  ## within-subject noise
  #noiseSd <- 0.25
  noiseSd <- 0
  
  ## level 2 equations
  intercept <- interceptFixed * bias + subjectProfile['subjectIntercept']
  differenceB <- differenceFixed * bias + subjectProfile['subjectDifference']
  difficultyB <- difficultyFixed * bias + subjectProfile['subjectDifficulty']
  twoWayB <- twoWayFixed * bias + subjectProfile['subjectTwoWay']
  
  ## level 1 equation
  proba <- (intercept + difference*differenceB + difficulty*difficultyB + difficulty*difference*twoWayB) * (trial / trialMax) + rnorm(1, 0, noiseSd)
  proba <- exp(proba) / (1 + exp(proba))
  
  return(proba)
  
}

buildSubjectProfile <- function() {
  ## RANDOM EFFECTS
  interceptRandom <- 3
  difficultyRandom <- 1
  differenceRandom <- 1 - (difficultyFixed - differenceFixed)
  twoWayRandom <- 1
  
  subjectProfile <- c(subjectIntercept = rnorm(1, 0, interceptRandom), subjectDifficulty = rnorm(1, 0, difficultyRandom), subjectDifference = rnorm(1, 0, differenceRandom), subjectTwoWay = rnorm(1, 0, twoWayRandom))
  
  return(subjectProfile)
}

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

  experimentResults <- data.frame(n = numeric(), bias = numeric(), effectSize = numeric(), pValue = numeric(), predictedDirection = logical())
  write.csv(experimentResults, 'experimentResultsCache.csv', row.names = FALSE)
  
  for (sim in 1:nSims){
    experimentData <- data.frame(subject = numeric(), trial = numeric(), difference = factor(,levels=c('moderate','extreme')), difficulty = factor(, levels=c('easier', 'harder')), lowDemandSelection = numeric())
    write.csv(experimentData, 'experimentDataCache.csv', row.names = FALSE)
    print(paste('Simulation', sim, 'of', nSims))
    
    for (subject in 1:n){
      
      ## first element is the intercept, mean 0.5 (choose randomly), sd controls variability in baseline preferences, constrained between 0:1
      ## second element is scalar to control size of the bias, centered at 1, floor of 0; bigger SD means more between-subject variability in size of the effect
      print(paste('subject;', subject))
      subjectProfile <- buildSubjectProfile()
      
      ## conditions
      conditions <- data.frame(difference = c(rep(0, 2), rep(1, 2)), difficulty = rep(c(0, 1), 2))
      
      for (conditionRow in 1:4) {
        difficulty <- conditions[conditionRow,]$difficulty
        difference <- conditions[conditionRow,]$difference
        difficultyCode <- ifelse(difficulty, 'easier', 'harder')
        differenceCode <- ifelse(difference, 'moderate', 'extreme')
        count <- 0
        trialIterator <- 1:(nTrials/4)
        
        for (trial in trialIterator) {
          count <- count + 1
          
          ## compute decision threshold
          proba <- decision(difference = difference, difficulty = difficulty, bias = bias, trial = trial, trialMax = max(trialIterator), subjectProfile = subjectProfile)
          
          ## execute decision and save data
          experimentData <- rbind(experimentData, data.frame(subject = subject, trial = count, 
                                                             difference = differenceCode, difficulty = difficultyCode,
                                                             lowDemandSelection = ifelse(runif(1) < proba, 1, 0)))
          
        }
        
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
  m1 <- ezANOVA(wid = subject, within = .(differenceCode, difficultyCode), dv = lowDemandSelection, data = experimentData, detailed = TRUE)
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

toLogOdds <- function(p){
  return(log(p/(1-p)))
}

