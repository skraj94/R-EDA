t_test <- function(x1,x2,c1='x1',c2='x2')
{
  result <- ''
  
  test <- t.test(x1,x2,alternative = 'two.sided')
  
  if (test$p.value < 0.05)
  {
    test <- t.test(x1,x2,alternative = 'greater')
    
    if (test$p.value > 0.05)
    {
      result <- paste(c1,'is significantly lesser than',c2,'with p-value',paste0(round(test$p.value,5)))
    }else
    {
      result <- paste(c1,'is significantly greater than',c2,'with p-value',paste0(round(1-test$p.value,5)))
    }
  }else
  {
    result <- paste(c1,'is significantly equal to',c2,'with p-value',paste0(round(test$p.value,5)))
  }
  
  return(result)
}
