
exp2Power <- function(nRange, biasRange, nSims, threadId) {
  ## takes as input:
    ## nRange: vector, range of sample size
    ## biasRange: vector, range of bias size
    ## nSims: numeric, number of simulations per set of hyperparameters
  ## returns data frame with vars:
    ## effect size: the average effect size from the set of simulations for each of the fixed effects
    ## sd effect size: the sd effect size from the set of simulations for each of the fixed effects
    ## n: the sample size for the set of simulations
    ## power: the level of power for the set of simulations for each of the fixed effects
  source('powerSimulator.r')
  
  count <- 0
  
  output <- refreshOutputData()
  write.csv(output, paste('data/cache/powerAnalysisCacheThread', threadId, '.csv', sep = ''), row.names = FALSE)
  
  startTime <- Sys.time()
  
  for (bias in biasRange){
    for (n in nRange){
      count <- count + 1
      
      output <- rbind(output, powerSimulator(n = n, bias = bias, nSims = nSims, threadId = threadId))
      
      print('----------')
      print('We have just completed iteration with values: ')
      print(paste('bias: ', bias, ' of ', max(biasRange), sep = ''))
      print(paste('n: ', n, ' of ', max(nRange), sep = ''))
      print(paste('Total run time: ', round(Sys.time() - startTime, 2), sep = ''))
      print(paste('Estimated time remaining: ', round(calcRemainingTime(startTime, count, biasRange, nRange), 2), sep = ''))
      print('----------')
      
      if (count %% 10 == 0 | count == length(biasRange) * length(nRange)) {
        output <- rbind(read.csv(paste('data/cache/powerAnalysisCacheThread', threadId, '.csv', sep = ''), header = TRUE), output)
        if (count == length(biasRange) * length(nRange)) {
          write.csv(output, paste('data/powerAnalysisBias', min(biasRange)*10,'-', max(biasRange)*10,'nRange', min(nRange), '-', max(nRange), '.csv', sep = ''), row.names = FALSE)
        } else {
          write.csv(output, paste('data/cache/powerAnalysisCacheThread', threadId, '.csv', sep = ''), row.names = FALSE)
          output <- refreshOutputData()
        }
      }
      
    } ## end n iterations
  } ## end bias iterations
  
  return(output)
}

calcRemainingTime <- function(startTime, count, biasRange, nRange){
  elapsedTime <- Sys.time() - startTime
  perIteration <- elapsedTime / count
  remainingIterations <- (length(biasRange) * length(nRange)) - count
  
  return(remainingIterations * perIteration)
  
}



refreshOutputData <- function() {
  d <- data.frame(n = numeric(), bias = numeric(), nSims = numeric(), differencePower = numeric(),
                  difficultyPower = numeric(),
                  difficultyPowerConstrained = numeric(),
                  interactionPower = numeric(),
                  avgDifferenceEffectSize = numeric(),
                  sdDifferenceEffectSize = numeric(),
                  avgDifficultyEffectSize = numeric(),
                  sdDifficultyEffectSize = numeric(),
                  avgInteractionEffectSize = numeric(),
                  sdInteractionEffectSize = numeric()
                  )
  return(d)
}







