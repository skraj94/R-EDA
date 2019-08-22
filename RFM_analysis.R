am1 <- read.csv('C:\\Users\\SKR\\Desktop\\Model Interpretation\\AMbreakdown.csv')
am2 <- read.csv('C:\\Users\\SKR\\Desktop\\Model Interpretation\\AMorderlist.csv')

am <- merge(am1,am2,all.x=T)
View(am)                  
?merge.data.frame
dim(am)


## preparing data for analysis
class(am$OrderDate)

am$OrderDate <- as.character.Date(am$OrderDate)
class(am$OrderDate)
 # converting factor to date is not happening lets try in some other way
am$OrderDate <- as.character(am$OrderDate)
am$OrderDate <- as.Date(am$OrderDate)
class(am$OrderDate)
# now its converted in to date format

time_start = "2019-06-28"
time_end = "1994-12-27"
ex <- difftime(time_start,time_end,units = 'secs')
as.numeric(ex)

am$recency <- round(as.numeric(difftime('2014-12-31', am$OrderDate,units = 'days')))
View(am)

# Importing sqldf
require(sqldf)
install.packages('sqldf')

# caluclating recency , frequency and monetary (RFM) values for each unique customers
customer = sqldf('SELECT CustomerName, MIN(recency) AS Recency, 
                  COUNT(distinct(OrderID)) AS Frequency,
                  AVG(Sales) AS Money, SUM(Sales) AS TotalAmt, MAX(recency) AS FirstPurchase
                  FROM am GROUP BY CustomerName')
View(customer)


#Intial segmentation
customer$segment <- NA
customer$segment[which(customer$Recency>365*3)] = "Inactive"
customer$segment[which(customer$Recency<= 365*3 & customer$Recency > 365*2)] = "Cold"
customer$segment[which(customer$Recency<= 365*2 & customer$Recency > 365)] = "Warm"
customer$segment[which(customer$Recency < 365)] = "Active"
View(customer)


# Further segmentation
customer$segment[which(customer$segment=="Active" & customer$FirstPurchase < 365)] = "Newactive"
customer$segment[which(customer$segment=="Active" & customer$Money < 200)] = "Active low value"
customer$segment[which(customer$segment=="Active" & customer$Money >= 200)] = "Active High value"

customer$segment[which(customer$segment=="Warm" & customer$FirstPurchase < 365*2)] = "Newwarm"
customer$segment[which(customer$segment=="Warm" & customer$Money < 200)] = "Warm low value"
customer$segment[which(customer$segment=="Warm" & customer$Money >= 200)] = "Warm High value"
View(customer)


#Studying the segments

# No of customers
length(customer$CustomerName)

# frequency distribution of segments
table(customer$segment)

# To get the freq distbn in ordered way
customer$segment <- factor(customer$segment, levels = c('Inactive','Cold', 'Warm low value','Warm High value','Newwarm','Active low value','Active High value','Newactive'))

table(customer$segment)
View(customer)
