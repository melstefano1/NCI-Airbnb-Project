#Setting the working directory
getwd()
setwd("C:/Users/Stefano/Dropbox/University/NCI/Programming for Big Data/Working Folder")

#Read data from CSV file
rome <- read.csv(file = "airbnb-rome.csv", header = TRUE, sep = ";")

#Check if there is any missing price in the data frame
missingdata<-rome[!complete.cases(rome$Price),]

#Cleap up the data frame
#Set an If statement to replace automatically the missing values with median value of the data frame
a<-as.numeric(nrow(missingdata))
x<-as.numeric(median(rome$Price, na.rm = TRUE))
if(a!=0){
  rome$Price[is.na(rome$Price)]<-median(rome$Price, na.rm = TRUE)
  rome$Price[is.na(rome$Price)]
  missingdata<-rome[!complete.cases(rome$Price),]
  print(paste("The value substituted is:",x))
  print(paste("Nr",a,"Row/s have been affected"))
  a<-0 #I have included it just to be sure that if I run again the If statement the Else condition works
} else {
  print("There are no missing values")
}

#Nr1 insight
#A - Distribution of the prices overall with median and mode
#B - Distribution of the prices clustered by Room Type

#Installation of the packages and the library ggplot2 for data visualization
install.packages('ggplot2')
library(ggplot2)

#Creation of a filtered subset. We are interested to analyze data just for Apartments
romeApart <- subset(rome, Property.Type == "Apartment")

#Overall prices distribution for apartment in Rome with median(Red line) and mode(Blue line)
ggplot(data=romeApart[romeApart$Price<250,], aes(Price))+geom_density(fill = "orange")+
  geom_vline(xintercept= median(romeApart$Price), size=1, color = "red")+
  geom_vline(xintercept = mean(romeApart$Price), size=1, color= "blue")+
  labs(x= 'Price', y='Frequency', title='Price distribution in Rome (Apartment only)')

#Prices distribution splitted by categories (Boxplot)
ggplot(data = romeApart[romeApart$Price<150,], mapping = aes(x= reorder(Room.Type, Price, median, na.rm=TRUE), y= Price))+
  geom_boxplot(fill="orange")+
  labs(x= "Room Type", y= "Price (Euro)", title='Price distribution in Rome (Apartment only)')


#Insight Nr 2 - trendline of the joining hosts overtime (no distincion between property type)
#Calculation of unique hosts
rome$Host.Since_cast <- as.Date(rome$Host.Since)
hosts<-rome[c("Host.ID", "Host.Since_cast")]
uniquehosts<-unique(hosts) #Calculation of unique hosts through the command 'unique'

#Trendline - Hosts joined since 2014 
ggplot(data=uniquehosts[uniquehosts$Host.Since_cast>as.Date("2013-12-30"),], aes(Host.Since_cast)) +
  geom_line(stat='count', color="orange", size= 0.1) +
  labs(x= 'Years', y='Nr of hosts', title='New hosts overtime')



#Insight nr3 - Predictive model through linear regression

#Read data from CSV
barcelona <- read.csv(file = "airbnb-barcelona.csv", header = TRUE, sep = ";")

#Check any missing values and their replacement with the median of the dataset
missingdata<-barcelona[!complete.cases(barcelona$Price),]
summary(barcelona$Price, na.rm = TRUE)
barcelona$Price[is.na(barcelona$Price)]<-62

#Filter the dataset by Property Type and Room Type
bar <- subset(barcelona, Property.Type == "Apartment" & Room.Type == "Entire home/apt")

#Exclude from the dataframe all rows containing missing values in the columns below 
df<- bar[!is.na(bar$Accommodates|
                bar$Beds|
                bar$Bathrooms|
                bar$Bedrooms|
                bar$Number.of.Reviews| 
                bar$Minimum.Nights|
                bar$Longitude|
                bar$Latitude|
                bar$Review.Scores.Accuracy | 
                bar$Review.Scores.Cleanliness | 
                bar$Review.Scores.Checkin |
                bar$Review.Scores.Communication |
                bar$Review.Scores.Location),] 

#Split the data frame into 2 parts (80%+20%)
#This bit of code have been take from the following website -> https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
n = nrow(df)
split = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.8, 0.2))
tr = df[split, ] # This data frame contain the 80% of the records that we will use to build out model
te = df[!split, ] # This data frame contain the remaining 20% of the records. The prices in it will be
                  #compared to the new prices predicted to evaluate the level of accuracy of the model 
test = te[,-57]   # This data frame contain the same data of 'te' minus the price column


#Linear model - Second test
m1 <- lm(Price ~ Accommodates+
           Beds+
           Bathrooms+
           Bedrooms+
           Number.of.Reviews+ 
           Minimum.Nights+
           Longitude+
           Latitude+
           Minimum.Nights+
           Review.Scores.Accuracy+
           Review.Scores.Cleanliness+
           Review.Scores.Checkin+
           Review.Scores.Communication+
           Review.Scores.Location, 
           data=tr)
summary(m1) #Summary of the linear model

#Creation of a new column in the 'te' data frame containing the new prices predicted
te$pred_price <- predict(m1, test)

#Comparison of the new prices vs the old prices throug scatter plot
ggplot(data=te[te$Price<600,], 
       aes(x=pred_price, y=Price))+
  geom_point(alpha=0.2) +
  geom_smooth(method='lm') + 
  labs(x='Predicted Price', y='Actual Price', title = 'Comparison between Actual and Predicted Prices')


# Insight Nr4 - Barcelona' apartments distribution on the map

#Pachage installation
install.packages("leaflet")

#Libraries usage
library(leaflet)
library(sp)

#Map visualization
#I have taken part of the following code from the following youtube video->https://www.youtube.com/watch?v=dBk8gGX1MNk&t=449s
a<- leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = df, 
             lng = ~ Longitude, 
             lat = ~ Latitude, 
             popup = ~ paste("Price", as.character(Price), "Euro"), 
             clusterOptions = markerClusterOptions()
             )

a










































