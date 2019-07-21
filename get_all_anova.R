all_anova <- function(data,col1,col2)
{
  names_ <- c()
  result_ <- c()
  
  for(i in col1)
  {
    for(j in col2)
    {
      if(length(unique(data[,j])) == 2)
      {
        val <- unique(data[,j])
        print(paste('For',i,',',
                    t_test(data[data[,j]==val[1],i],
                           data[data[,j]==val[2],i],
                           paste(j,val[1]),
                           paste(j,val[2])
                    )
        )
        )
      }else if(length(unique(data[,j])) == length(data[,j]))
      {
        print(paste(i,j,'skip this since',j,'is identifier'))
      }else
      {
        anova <- aov(data[,i] ~ data[,j])
        pval = unlist(summary(anova))['Pr(>F)1']
        if(pval > 0.05)
        {
          print(paste(i,j,'no diff'))
        }else
        {
          print(paste(i,j,'diff'))
        }
      }
    }
  }
}

