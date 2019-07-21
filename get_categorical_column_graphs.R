categorical_column_graphs <- function(data,col_list,loc,custom_name)
{
  setwd(loc)
  
  for (i in col_list)
  {
    png(file=paste(custom_name,'_',i,'.png',sep=''),bg = "transparent")
    par(mfrow = c(1,2))
    x <- table(data[,i])
    count <- as.vector(x)
    percent <- round(100*count/sum(count), 2)
    pie(count,labels = percent,col=rainbow(length(names(x))),main=paste("Pie Chart of ",i))
    legend("topright", names(x),fill=rainbow(length(names(x))))
    barplot(x, main=paste(i," Distribution"))
    dev.off()
  }
}
