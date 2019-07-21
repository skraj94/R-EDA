all_chisq <- function(data,col)
{
  for(i in seq(1,length(col)-1))
  {
    for (j in seq(i+1,length(col)))
    {
      col1 <- col[i]
      col2 <- col[j]
      tbl <- table(data[,col1], data[,col2])
      
      pval <- chisq.test(tbl)[3]
      if (pval > 0.05)
      {
        print(paste(col1,'and',col2,' is independant','with p-value',paste0(pval)))
      }else
      {
        print(paste(col1,'and',col2,' is dependant','with p-value',paste0(pval)))
      }
    }
  }
}
