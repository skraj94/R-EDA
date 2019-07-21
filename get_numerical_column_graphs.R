numeric_column_graphs <- function(data,col_list,loc,custom_name)
{
  setwd(loc)
  
  for (i in col_list)
  {
    png(file=paste(custom_name,'_',i,'.png',sep=''),bg = "transparent")
    par(mfrow = c(1,2))
    hist(data[,i], main = paste("Histogram of ", i), xlab = i, col = "skyblue2")
    boxplot(data[,i],  main = paste("Boxplot of ", i),horizontal = T,xlab = i, col = "red")
    dev.off()
  }
}
