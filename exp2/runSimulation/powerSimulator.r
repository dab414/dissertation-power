library(tidyverse)
library(ez)
## global vars

## target cycles is 17, 4 decisions per cycle
nTrials <- 17*4

## TRUE PARAMETERS -- SIGNAL
lossAversionFixed <- 2
## these two are reverse
## smaller is stronger
exponentsFixed <- .70
pWeightFixed <- .80

## RANDOM EFFECTS -- NOISE
## such that subject parameters have about a .4 correlation with true parameters
exponentsRandom <- .35
lossAversionRandom <- .60
pWeightRandom <- .20
interceptRandom <- 3.5

## within-subject noise
noiseSd <- 2

  
buildSubjectProfile <- function() {
  ## Returns a vector with values to be added to / subtracted from fixed effects for each subject
  subjectProfile <- c(subjectIntercept = rnorm(1, 0, interceptRandom), 
                      subjectExponents = rnorm(1, 0, exponentsRandom), 
                      subjectLossAversion = rnorm(1, 0, lossAversionRandom), 
                      subjectPWeight = rnorm(1, 0, pWeightRandom))
  return(subjectProfile)
}
  
valueFunction <- function(criticalDeckIntensity, bias, subjectProfile) {
  ## The coding on the line below is such that harder critical decks are to the right on the x-axis
  ## Which puts the function in the correct direction if the DV is selection of reference deck
  relativeIntensity <- criticalDeckIntensity - 8
  
  ## Adjust the value function parameters by bias and random effects
  ## These are essentially the level 2 equations
  exponentsFinal <- min(max(exponentsFixed * convertBias(bias, exponentsFixed) + subjectProfile['subjectExponents'],0),1)
  ## constained so no one can have reverse loss aversion
  lossAversionFinal <- max(lossAversionFixed * convertBias(bias, lossAversionFixed) + subjectProfile['subjectLossAversion'], 1)
  ## compute value
  ## flipping the x-axis also means that loss aversion needs to be reversed
  out <- ifelse(relativeIntensity < 0, -(-relativeIntensity)^exponentsFinal, lossAversionFinal * (relativeIntensity) ^ exponentsFinal)
  return(out)
}

pWeightFunction <- function(bias, subjectProfile) {
  ## The conceptual input to this function will always be 0.5
  ## The purpose of this function is to subjectively weight the probability as a function of both subject-level bias and the parametrically-manipulated bias
  
  ## Essentially the level 2 equation for the probability weight
  discount <- min(max(pWeightFixed * convertBias(bias, pWeightFixed * 0.5) + subjectProfile['subjectPWeight'], 0), 1)
  
  return(0.5 * discount)
}
  
decision <- function(riskyCritical, safeCritical, bias, subjectProfile) {
  ## takes as input all this stuff above
  ## returns the probability of choosing the reference deck
  
  ## Level 1 equation
  ## The flip side of the risky prospect (ie reference) will always be adding 0 value..
  riskyProba <- valueFunction(riskyCritical, bias, subjectProfile) * pWeightFunction(bias, subjectProfile)
  safeProba <- valueFunction(safeCritical, bias, subjectProfile) 
  
  proba <- subjectProfile['subjectIntercept'] + safeProba - riskyProba + rnorm(1, 0, noiseSd)
  
  proba <- exp(proba) / (1 + exp(proba))
  
  return(proba)
  
}
  
powerSimulator <- function(n, bias, nSims, threadId) {
  ## function takes in:
    ## n: sample size
    ## bias: functional effect size
    ## nSims: number of simulations
  ## returns:
    ## one row of a df with n, POWER, effect size, nSims, and power outcome
  
  ## if only 1 sim, return the experiment data instead
  oneShot <- ifelse(nSims == 1, 1, 0)

  ## initialize container for all results
  experimentResults <- refreshExperimentResults()
  
  write.csv(experimentResults, paste('data/cache/experimentResultsCacheThread', threadId, '.csv', sep = ''), row.names = FALSE)
  
  ## Sim loop
  for (sim in 1:nSims) {
    experimentData <- data.frame(subject = numeric(), trial = numeric(), difference = factor(, levels = c('moderate', 'extreme')), 
                                 difficulty = factor(,levels = c('easier', 'harder')), riskySelection = numeric())
    write.csv(experimentData, paste('data/cache/experimentDataCacheThread', threadId, '.csv', sep = ''), row.names = FALSE)
    print(paste('Simulation', sim, 'of', nSims))
    
    ## Subject loop
    for (subject in 1:n) {
      subjectProfile <- buildSubjectProfile()
      
      ## condition codes
      conditions <- data.frame(difficulty = factor(c(rep('easier', 2), rep('harder', 2))), 
                               difference = factor(rep(c('moderate', 'extreme'), 2)),
                               riskyCritical = c(4, 2, 12, 14),
                               safeCritical = c(6, 5, 10, 11))
   
      ## block loop
      for (conditionRow in 1:4) {
        ## true trial count
        count <- 0
        ## blocked trial count
        trialIterator <- 1:(nTrials/4)
        
        for (trial in trialIterator) {
          count <- count + 1
          
          ## compute decision threshold
          proba <- decision(riskyCritical = conditions[conditionRow,]$riskyCritical, 
                            safeCritical = conditions[conditionRow,]$safeCritical,
                            bias = bias, 
                            subjectProfile = subjectProfile)
          
          ## execute decision and save data
          experimentData <- rbind(experimentData, data.frame(subject = subject, trial = count,
                                                             difference = conditions[conditionRow,]$difference,
                                                             difficulty = conditions[conditionRow,]$difficulty,
                                                             riskySelection = ifelse(runif(1) < proba, 1, 0)))
          
          
        } ## end trial iterator
      } ## end block loop
      
      ## every 10 subjects, save out data to cache and dump
      if (subject %% 5 == 0 | subject == n) {
        experimentData <- rbind(read.csv(paste('data/cache/experimentDataCacheThread', threadId, '.csv', sep = ''), header = TRUE), experimentData)
        write.csv(experimentData, paste('data/cache/experimentDataCacheThread', threadId, '.csv', sep = ''), row.names = FALSE)
        print(paste('Subject: ', subject, ' of ', n, sep = ''))
        if (subject != n) {
          experimentData <- data.frame(subject = numeric(), trial = numeric(), difference = factor(, levels = c('moderate', 'extreme')), 
                                       difficulty = factor(,levels = c('easier', 'harder')), riskySelection = numeric())
        }
      }
      
    } ## end subject loop
    
    ## compile results from simulation
    experimentResults <- computeStats(experimentData, n, bias)
    experimentResults <- rbind(read.csv(paste('data/cache/experimentResultsCacheThread', threadId, '.csv', sep = ''), header = TRUE), experimentResults)
    write.csv(experimentResults, paste('data/cache/experimentResultsCacheThread', threadId, '.csv', sep = ''), row.names = FALSE)
    
    ## if it's not the last simulation, refresh experiment results
    if (sim != nSims) {
      experimentResults <- refreshExperimentResults()
    } 
    
  } ## end simLoop()
  
  if (oneShot){
    return(experimentData)
  } else {
    experimentResults <- experimentResults %>% 
      mutate(isDifferenceSig = ifelse(differencep < .05, 1, 0),
             isDifficultySig = ifelse(difficultyp < .05, 1, 0),
             isDifficultySigConstrained = ifelse(difficultyp < .05 & predictedDirection, 1, 0),
             isInteractionSig = ifelse(interactionp < .05, 1, 0))

    return(data.frame(n = n, bias = abs(bias), nSims = nSims, differencePower = mean(experimentResults$isDifferenceSig), 
                      difficultyPower = mean(experimentResults$isDifficultySig),
                      difficultyPowerConstrained = mean(experimentResults$isDifficultySigConstrained),
                      interactionPower = mean(experimentResults$isInteractionSig),
                      avgDifferenceEffectSize = mean(experimentResults$differenceEffectSize), 
                      sdDifferenceEffectSize = sd(experimentResults$differenceEffectSize),
                      avgDifficultyEffectSize = mean(experimentResults$difficultyEffectSize), 
                      sdDifficultyEffectSize = sd(experimentResults$difficultyEffectSize),
                      avgInteractionEffectSize = mean(experimentResults$interactionEffectSize), 
                      sdInteractionEffectSize = sd(experimentResults$interactionEffectSize)))
  }
  
} ## end powerSimulator()
  
  

  
computeStats <- function(experimentData, n, bias) {
  ## takes as input the dataset from a single simulation (as deviation scored on a subject-level) and returns a dataset summarizing parameters from an ANOVA as well as the levels of the hyperparameters
  experimentData$subject <- factor(experimentData$subject)
  experimentData$difference <- factor(experimentData$difference)
  experimentData$difficulty <- factor(experimentData$difficulty)

  m1 <- ezANOVA(wid = subject, within = .(difference, difficulty), dv = riskySelection, data = experimentData, detailed = TRUE)
  
  differenceEffectSize <- m1$ANOVA$SSn[2] /(m1$ANOVA$SSn[2] + m1$ANOVA$SSd[2])
  difficultyEffectSize <- m1$ANOVA$SSn[3] /(m1$ANOVA$SSn[3] + m1$ANOVA$SSd[3])
  interactionEffectSize <- m1$ANOVA$SSn[4] /(m1$ANOVA$SSn[4] + m1$ANOVA$SSd[4])
  
  differencep <- m1$ANOVA$p[2]
  difficultyp <- m1$ANOVA$p[3]
  interactionp <- m1$ANOVA$p[4]
  
  predictedDirection <- testDirection(experimentData)
  
  return(data.frame(n = n, bias = abs(bias), differenceEffectSize = differenceEffectSize,
                    difficultyEffectSize = difficultyEffectSize,
                    interactionEffectSize = interactionEffectSize,
                    differencep = differencep,
                    difficultyp = difficultyp,
                    interactionp = interactionp,
                    predictedDirection = predictedDirection))
  
}

testDirection <- function(experimentData) {
  ## takes as input experimentData 
  ## returns 1 if the results of the three planned t tests are significant and if the means are in the right direction
  
  output <- 0
  
  difficultyMeans <- experimentData %>% 
    group_by(subject, difficulty) %>% 
    summarize(riskySelection = mean(riskySelection)) %>% 
    group_by(difficulty) %>% 
    summarize(riskySelection = mean(riskySelection)) 
  
  easierMean <- difficultyMeans[difficultyMeans$difficulty == 'easier',]$riskySelection
  harderMean <- difficultyMeans[difficultyMeans$difficulty == 'harder',]$riskySelection
  
  pEasier <- t.test(experimentData[experimentData$difficulty == 'easier',]$riskySelection, mu = .5)$p.value
  pHarder <- t.test(experimentData[experimentData$difficulty == 'harder',]$riskySelection, mu = .5)$p.value
  pContrast <- t.test(experimentData[experimentData$difficulty == 'harder',]$riskySelection, experimentData[experimentData$difficulty == 'easier',]$riskySelection, paired = TRUE)$p.value
  
  if (pEasier < .05 & pHarder < .05 & pContrast < .05 & easierMean < .5 & harderMean > .5 & easierMean < harderMean) {
    output <- 1
  }
  
  return(output)
  
}


refreshExperimentResults <- function() {
  return(data.frame(n = numeric(), bias = numeric(), differenceEffectSize = numeric(),
                    difficultyEffectSize = numeric(),
                    interactionEffectSize = numeric(),
                    differencep = numeric(),
                    difficultyp = numeric(),
                    interactionp = numeric(),
                    predictedDirection = logical()))
}




# convertBias <- function(bias, factorFixed) {
#   ## This got super complicated but i'm pretty confident it works
#   ## Check in the sketch book to see the crazy linear algebra
#   ## Essentially, bias is theoretically sampled from a 0:1 parameter space, where 0 kills the effect and 1 gives it max strength
#   ## I need to convert this bias to a new vector space with range (1/exponentsFixed):1 where (1/exponentsFixed) kills the effect and 1 gives it max strength
#   ## note that (1/exponentsFixed):1 is actually max:min, thus the relationship between the theoretical new vector and the old one is r = -1
#   ## validate this function with the plotting function below
#   
#   biasVector <- seq(0,1,.001)
#   biasIndex <- which(biasVector %in% bias)
#   out <- -bias + (1/factorFixed)
#   out <- out * (seq(1/factorFixed, 1, length.out = length(biasVector))[biasIndex] / out)
#   return(out)
# }

convertBias <- function(bias, factorFixed) {
  ## This is way more robust and way simpler
  biasVector <- seq(0,1,.001)
  yvector <- seq(1/factorFixed, 1, length.out = length(biasVector))
  m1 <- lm(yvector ~ biasVector)
                 
  return(m1$coefficients[1] + m1$coefficients[2] * bias)
  
}





validateConvertBias <- function() {
  d <- data.frame(x = seq(0,1,.01))
  d$y <- convertBias(d$x, 0.3) ## 0.3 arbitrarily chosen
  plot(d$x, d$y)
}


toLogOdds <- function(p){
  return(log(p/(1-p)))
}