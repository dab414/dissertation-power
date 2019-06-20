## global vars
nTrials <- 700

## FIXED EFFECTS
exponentsFixed <- .3
lossAversionFixed <- 2
fixedEffects <- c('exponentsFixed' = exponentsFixed, 'lossAversionFixed' = lossAversionFixed)

## RANDOM EFFECTS
## keeping these pretty tight for now
# interceptRandom <- abs(toLogOdds(.1))
# exponentsRandom <- .2
# lossAversionRandom <- 1

interceptRandom <- 0
exponentsRandom <- 0
lossAversionRandom <- 0

## within-subject noise
noiseSd <- 0
  
buildSubjectProfile <- function() {
  ## Returns a vector with values to be added to / subtracted from fixed effects for each subject
  subjectProfile <- c(subjectIntercept = rnorm(1, 0, interceptRandom), subjectExponents = rnorm(1, 0, exponentsRandom), subjectLossAversion = rnorm(1, 0, lossAversionRandom))
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
  
decision <- function(criticalDeckIntensity, bias, trial, trialMax, subjectProfile) {
  ## takes as input all this stuff above
  ## returns the probability of choosing the reference deck
  

  
  ## Level 1 equation
  proba <- subjectProfile['subjectIntercept'] + valueFunction(criticalDeckIntensity, bias, subjectProfile) * (trial / trialMax) + rnorm(1, 0, noiseSd) 
  proba <- exp(proba) / (1 + exp(proba))
  
  return(proba)
  
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