getPercentMissing <- function(data)
{
  percentMissing <- function(data)
  {
    result <- round((sum(is.na(data))/length(data))*100, 2)
    return (result)
  }
  
  perMiss = apply(fram,2,percentMissing)
  
  perMiss = apply(fram,2,percentMissing)
  
  perMiss <- as.matrix(perMiss)
  colnames(perMiss) <- c('Percent missing')
  
  return (perMiss)
}
