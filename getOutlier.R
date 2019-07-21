outlier <- function(data)
{
  if (class(fram) != "data.frame")
  {
    print('Not a data frame')
    return (NULL)
  }
  
  upperCount <- c()
  lowerCount <- c()
  names <- c()
  
  for (i in 1:ncol(data))
  {
    if (is.numeric(data[,i]))
    {
      x <- data[,i][!is.na(data[,i])]
      
      q25 = quantile(x)[2]
      q75 = quantile(x)[4]
      
      upperbound = q75 + 1.5*(q75-q25)
      lowerbound = q25 - 1.5*(q75-q25)
      
      names[i] = names(data[i])
      upperCount[i] = sum(x>upperbound)
      lowerCount[i] = sum(x<lowerbound)
    }
  }
  outlierTable = cbind(upperCount,lowerCount)
  rownames(outlierTable) = names
  return (outlierTable)
  
}
