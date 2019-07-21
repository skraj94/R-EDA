numeric_column_scatter <- function(data,cols,loc,custom_name)
{
  setwd(loc)
  
  for(i in seq(1,length(cols)-1,1))
  {
    for(j in seq(i+1,length(cols),1))
    {
      png(file=paste(custom_name,'_',cols[i],'_',cols[j],'.png',sep=''),bg = "transparent")
      plot(data[,cols[i]],data[,cols[j]],xlab = cols[i],ylab = cols[j])
      dev.off()
    }
  }
}
