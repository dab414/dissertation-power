## global vars
nTrials <- 700

## FIXED EFFECTS
exponentsFixed <- .3
lossAversionFixed <- 3
learningFixed <- 200

## RANDOM EFFECTS
## keeping these pretty tight for now
#interceptRandom <- abs(toLogOdds(.05))
exponentsRandom <- .2
lossAversionRandom <- 1
learningRandom <- 150

 interceptRandom <- 0
# exponentsRandom <- 0
# lossAversionRandom <- 0


## within-subject noise
noiseSd <- 0
withinBlockLearningRateNoise <- 50
  
buildSubjectProfile <- function() {
  ## Returns a vector with values to be added to / subtracted from fixed effects for each subject
  subjectProfile <- c(subjectIntercept = rnorm(1, 0, interceptRandom), 
                      subjectExponents = rnorm(1, 0, exponentsRandom), 
                      subjectLossAversion = rnorm(1, 0, lossAversionRandom), 
                      subjectLearning = rnorm(1, 0, learningRandom))
  return(subjectProfile)
}
  
valueFunction <- function(criticalDeckIntensity, bias, subjectProfile) {
  ## The coding on the line below is such that harder critical decks are to the right on the x-axis
  ## Which puts the function in the correct direction if the DV is selection of reference deck
  relativeIntensity <- criticalDeckIntensity - 0.5
  
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
  
decision <- function(criticalDeckIntensity, bias, trial, subjectProfile, learningRate) {
  ## takes as input all this stuff above
  ## returns the probability of choosing the reference deck
  
  ## Level 1 equation
  proba <- subjectProfile['subjectIntercept'] + valueFunction(criticalDeckIntensity, bias, subjectProfile) * min((trial / learningRate), 1) + rnorm(1, 0, noiseSd) 
  proba <- exp(proba) / (1 + exp(proba))
  
  return(proba)
  
}
  
powerSimulator <- function(n, bias, nSims) {
  ## function takes in:
    ## n: sample size
    ## bias: functional effect size
    ## nSims: number of simulations
  ## returns:
    ## one row of a df with n, POWER, effect size, nSims, and power outcome
  
  ## if only 1 sim, return the experiment data instead
  oneShot <- ifelse(nSims == 1, 1, 0)
  
  experimentResults <- data.frame(n = numeric(), bias = numeric(), effectSize = numeric(), pValue = numeric(), predictedDirection = logical())
  write.csv(experimentResults, 'experimentResultsCache.csv', row.names = FALSE)
  
  ## Sim loop
  for (sim in 1:nSims) {
    experimentData <- data.frame(subject = numeric(), trial = numeric(), difference = factor(, levels = c('moderate', 'extreme')), 
                                 difficulty = factor(,levels = c('easier', 'harder')), referenceSelection = numeric())
    write.csv(experimentData, 'experimentDataCache.csv', row.names = FALSE)
    print(paste('Simulation', sim, 'of', nSims))
    
    ## Subject loop
    for (subject in 1:n) {
      print(paste('subject:', subject))
      subjectProfile <- buildSubjectProfile()
      
      ## condition codes
      conditions <- data.frame(difficulty = factor(c(rep('easier', 2), rep('harder', 2))), 
                               difference = factor(rep(c('moderate', 'extreme'), 2)),
                               criticalDeckIntensity = c(0.3, 0.1, 0.7, 0.9))
   
      ## block loop
      for (conditionRow in 1:4) {
        ## true trial count
        count <- 0
        ## blocked trial count
        trialIterator <- 1:(nTrials/4)
        
        ## create learning rate with min trials to learning being 50
        ## also between-block noise in learning rate with SD 25
        learningRate <- max(round(learningFixed + subjectProfile['subjectLearning'] + rnorm(1, 0, withinBlockLearningRateNoise)), 25)
        
        for (trial in trialIterator) {
          count <- count + 1
          
          ## compute decision threshold
          proba <- decision(criticalDeckIntensity = conditions[conditionRow,]$criticalDeckIntensity, 
                            bias = bias, 
                            trial = trial, 
                            subjectProfile = subjectProfile,
                            learningRate = learningRate)
          
          ## execute decision and save data
          experimentData <- rbind(experimentData, data.frame(subject = subject, trial = count,
                                                             difference = conditions[conditionRow,]$difference,
                                                             difficulty = conditions[conditionRow,]$difficulty,
                                                             referenceSelection = ifelse(runif(1) < proba, 1, 0)))
          
          
        } ## end trial iterator
      } ## end block loop
      
      ## every 10 subjects, save out data to cache and dump
      if (subject %% 5 == 0 | subject == n) {
        experimentData <- rbind(read.csv('experimentDataCache.csv', header = TRUE), experimentData)
        write.csv(experimentData, 'experimentDataCache.csv', row.names = FALSE)
        print(paste('Subject: ', subject, ' of ', n, sep = ''))
        if (subject != n) {
          experimentData <- data.frame(subject = numeric(), trial = numeric(), difference = factor(, levels = c('moderate', 'extreme')), 
                                       difficulty = factor(,levels = c('easier', 'harder')), referenceSelection = numeric())
        }
      }
      
    } ## end subject loop
    
    ## compile results from simulation
    #experimentResults <- computeStats(translateToDeviation(experimentData), n, bias)
    
    
  } ## end simLoop()
  
  return(experimentData)
  
} ## end powerSimulator()
  
  
  
translateToDeviation <- function(d) {
  ## takes as input experiment data 
  d <- d %>% 
    group_by(subject, difference, difficulty) %>% 
    summarize(referenceSelection = mean(referenceSelection)) %>% 
    unite('condition', c('difference', 'difficulty')) %>% 
    spread(condition, referenceSelection) %>% 
    mutate(moderate_easier_deviation = abs(moderate_easier - 0.5),
           moderate_harder_deviation = abs(moderate_harder - 0.5),
           extreme_easier_deviation = abs(extreme_easier - moderate_easier),
           extreme_harder_deviation = abs(extreme_harder - moderate_harder)) %>% 
    select(subject, contains('deviation')) %>% 
    gather(condition, referenceDeviation, contains('deviation')) %>% 
    separate(condition, c('difference', 'difficulty'))
  
  return(d)
  
}

  
computeStats <- function(experimentData, n, bias) {
  ## takes as input the dataset from a single simulation and returns a dataset summarizing parameters from an ANOVA as well as the levels of the hyperparameters
  experimentData$subject <- factor(experimentData$subject)
  experimentData$difference <- factor(experimentData$difference)
  experimentData$difficulty <- factor(experimentData$difficulty)

  m1 <- ezANOVA(wid = subject, within = .(difference, difficulty), dv = referenceDeviation, data = experimentData, detailed = TRUE)
  
  
  
}







convertBias <- function(bias, factorFixed) {
  ## This got super complicated but i'm pretty confident it works
  ## Check in the sketch book to see the crazy linear algebra
  ## Essentially, bias is theoretically sampled from a 0:1 parameter space, where 0 kills the effect and 1 gives it max strength
  ## I need to convert this bias to a new vector space with range (1/exponentsFixed):1 where (1/exponentsFixed) kills the effect and 1 gives it max strength
  ## note that (1/exponentsFixed):1 is actually max:min, thus the relationship between the theoretical new vector and the old one is r = -1
  ## validate this function with the plotting function below
  
  biasVector <- seq(0,1,.001)
  biasIndex <- which(biasVector %in% bias)
  out <- -bias + (1/factorFixed)
  out <- out * (seq(1/factorFixed, 1, length.out = length(biasVector))[biasIndex] / out)
  return(out)
}

validateConvertBias <- function() {
  d <- data.frame(x = seq(0,1,.01))
  d$y <- convertBias(d$x, 0.3) ## 0.3 arbitrarily chosen
  plot(d$x, d$y)
}


toLogOdds <- function(p){
  return(log(p/(1-p)))
}