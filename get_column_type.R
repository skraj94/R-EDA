column_types <- function(data)
{
  numeric_colums <- c()
  categorical_colums <- c()
  
  for (i in names(data))
  {
    type <- class(data[,i])
    
    if((type == 'numeric' || type == 'integer') && (length(unique(data[,i])) > 5))
    {
      numeric_colums <- c(numeric_colums,i)
    }
    else
    {
      categorical_colums <- c(categorical_colums,i)
    }
  }
  
  output <- list()
  output$numeric <- numeric_colums
  output$categorical <- categorical_colums
  
  return (output)
}
