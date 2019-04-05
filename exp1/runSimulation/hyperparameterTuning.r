
exp1Power <- function(nRange, biasRange, nSims) {
  ## takes as input:
    ## nRange: vector, range of sample size
    ## biasRange: vector, range of bias size
    ## nSims: numeric, number of simulations per set of hyperparameters
  ## returns data frame with vars:
    ## effect size: the average effect size from the set of simulations
    ## n: the sample size for the set of simulations
    ## power: the level of power for the set of simulations
  source('powerSimulator.r')
  
  count <- 0
  
  output <- data.frame(n = numeric(), bias = numeric(), effectSize = numeric(), power = numeric())
  write.csv(output, 'powerAnalysisCache.csv', row.names = FALSE)
  
  startTime <- Sys.time()
  
  for (bias in biasRange){
    for (n in nRange){
      count <- count + 1
      
      output <- rbind(output, powerSimulator(n = n, bias = bias, nSims = nSims))
      
      print('We have just completed iteration with values: ')
      print(paste('bias: ', bias, ' of ', max(biasRange), sep = ''))
      print(paste('n: ', n, ' of ', max(nRange), sep = ''))
      print(paste('Total run time: ', round(Sys.time() - startTime, 2), sep = ''))
      print(paste('Estimated time remaining: ', round(calcRemainingTime(startTime, count, biasRange, nRange), 2), sep = ''))
      
      if (count %% 10 == 0 | count == length(biasRange) * length(nRange)) {
        output <- rbind(read.csv('powerAnalysisCache.csv', header = TRUE), output)
        if (count == length(biasRange) * length(nRange)) {
          write.csv(output, 'powerAnalysis.csv', row.names = FALSE)
        } else {
          write.csv(output, 'powerAnalysisCache.csv', row.names = FALSE)
          output <- data.frame(n = numeric(), bias = numeric(), effectSize = numeric(), power = numeric())
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