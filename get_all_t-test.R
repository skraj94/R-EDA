all_t_test <- function(data,col)
{
  for(i in seq(1,length(col)-1))
  {
    for (j in seq(i+1,length(col)))
    {
      c1 <- col[i]
      c2 <- col[j]
      
      x1 <- data[,c1]
      x2 <- data[,c2]
      
      print(t_test(x1,x2,c1,c2))
    }
  }
}
