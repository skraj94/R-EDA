numeric_column_boxplot <- function(data,col1,col2,loc=getwd(),custom_name)
{
  setwd(loc)
  
  for(i in col1)
  {
    for(j in col2)
    {
      if(length(unique(data[,j]))<15)
      {
        png(file=paste(custom_name,'_',i,'_',j,'.png',sep=''),bg = "transparent")
        boxplot(data[,i]~data[,j],xlab=j,ylab=i)
        dev.off()
      }
    }
  }
}
