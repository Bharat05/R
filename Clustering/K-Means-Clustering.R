


##################################################
###                                             ##
### K-Means Clustering                          ## 
##################################################
#                                               ##
##################################################
## Written by Bharat Thakur                     ##
##                                              ##  
##                                              ##
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/.../Clustering")

options(scipen=100)
options(digits=3)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")


# Read Reviews.csv file
Review_BT <- read.csv('Reviews.csv')




###################################################
##    Data transformation                        ##
###################################################

#Renaming columns 
names(Review_BT) = c('User_Id_BT','Sports_BT', 'Religious_BT', 'Nature_BT', 'Theatre_BT',
                     'Shopping_BT', 'Picnic_BT', 'Age_BT', 'Income_BT', 'Nbr_BT')

names(Review_BT)
#printing first few rows
head(Review_BT)



summary(Review_BT)

#Plotting histogram
par(mfrow=c(3,3))
hist(Review_BT$Sports_BT, main = 'Histogram of Sports Reviews', xlab =  'Sports')
hist(Review_BT$Religious_BT, main = 'Histogram of Religious Reviews', xlab = 'Religious')
hist(Review_BT$Nature_BT, main = 'Histogram of Nature Reviews', xlab = 'Nature')
hist(Review_BT$Theatre_BT, main = 'Histogram of Theatre Reviews', xlab = 'Theatre')
hist(Review_BT$Shopping_BT, main = 'Histogram of Shopping Reviews', xlab = 'Shopping')
hist(Review_BT$Picnic_BT, main = 'Histogram of Picnic Reviews', xlab = 'Picnic')
hist(Review_BT$Age_BT, main = 'Histogram of Age ', xlab = 'Age')
hist(Review_BT$Income_BT, main = 'Histogram of Income', xlab = 'Income')
hist(Review_BT$Nbr_BT, main = 'Histogram of Nbr', xlab = 'Nbr')


par(mfrow=c(1,1))

str(Review_BT)



###################################################
## Standardize Data                              ##
###################################################

#Create a standardization function
norm01 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#Standardizing Sports
Review_BT$Sports_MinMax_BT <- norm01(Review_BT$Sports_BT)

#Standardizing Religious
Review_BT$Religious_MinMax_BT <- norm01(Review_BT$Religious_BT)

#Standardizing Nature
Review_BT$Nature_MinMax_BT <- norm01(Review_BT$Nature_BT)

#Standardizing Theatre
Review_BT$Theatre_MinMax_BT <- norm01(Review_BT$Theatre_BT)

#Standardizing Shopping
Review_BT$Shopping_MinMax_BT <- norm01(Review_BT$Shopping_BT)

#Standardizing Picnic
Review_BT$Picnic_MinMax_BT <- norm01(Review_BT$Picnic_BT)

#Standardizing Age
Review_BT$Age_MinMax_BT <- norm01(Review_BT$Age_BT)


#Standardizing Income
Review_BT$Income_MinMax_BT <- norm01(Review_BT$Income_BT)

#Standardizing Nbr_BT
Review_BT$NBR_MinMax_BT <- norm01(Review_BT$Nbr_BT)


##################################################
### Descriptive Analysis                        ##
##################################################

head(Review_BT)
summary(Review_BT)

stat.desc(Review_BT)
str(Review_BT)


#Subsetting for required columns
ReviewClstrData_BT <- Review_BT[c(12,15)]   
str(ReviewClstrData_BT)



###################################################
## Create Clusters for K = 2:6                   ##
###################################################

for(i in 2:6){
  
  tmp_clstr <- paste("Clstr", "cnt", toString(i), "Rev", "BT", sep = "_")
  ClstrRev_BT <- kmeans(ReviewClstrData_BT, iter.max=10, centers=i, nstart=10)
  assign(tmp_clstr,ClstrRev_BT)
  
  Review_BT[paste("cluster_", toString(i), sep = "")]<- factor(ClstrRev_BT$cluster)   # Adding Cluster tags to variables
  Review_BT$cluster <- factor(ClstrRev_BT$cluster)
}

Review_BT$cluster <- NULL

###################################################
## Choose the Cluster via ELbow Plot             ##
###################################################

# Within SS  and Between to Total SS with k=2,3,4,5,6.
WSS_BT <- sapply(2:6, function(i){return(kmeans(ReviewClstrData_BT,iter.max=10, centers = i, nstart=10)$tot.withinss)})

TotSS_BT <- sapply(2:6, function(i){return(kmeans(ReviewClstrData_BT,iter.max=10, centers = i, nstart=10)$totss)})
BetSS_BT <- sapply(2:6, function(i){return(kmeans(ReviewClstrData_BT,iter.max=10, centers = i, nstart=10)$betweenss)})

Bet_To_TotSS_BT <- BetSS_BT/TotSS_BT
WSS_BT <- data.frame(cbind(No_Of_Clusters_BT=2:6, WSS_BT, Bet_To_TotSS_BT))
WSS_BT

# Elbow plot
ggplot() + geom_line(data=WSS_BT, aes(x=No_Of_Clusters_BT, y=WSS_BT), size =1, color = "#F8766D" ) +
  geom_line(data=WSS_BT, aes(x=No_Of_Clusters_BT, y=Bet_To_TotSS_BT*10), size =1, color = "#00BFC4" ) + 
  labs(title="Values of K for Reivews Cluster",
       x ="Number of K", y = "Total Within SS") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total Within SS",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*.1, name="Between to Total SS")
  )


###################################################
## Create Clusters and plot for K-1, K, K+1      ##
###################################################



for(i in 3:5){
  
  tmp_clstr <- paste("Clstr", "cnt", toString(i), "Rev", "BT", sep = "_")
  ClstrRev_BT <- kmeans(ReviewClstrData_BT, iter.max=10, centers=i, nstart=10)
  assign(tmp_clstr,ClstrRev_BT)
  
  Review_BT[paste("cluster_", toString(i), sep = "")]<- factor(ClstrRev_BT$cluster)   # Adding Cluster tags to variables
  Review_BT$cluster <- factor(ClstrRev_BT$cluster)
  head(Review_BT)
  
  centers_BT <- data.frame(cluster=factor(1:i), ClstrRev_BT$centers)
  
  print(ggplot(data=Review_BT, aes(x=Religious_MinMax_BT, y=Shopping_MinMax_BT, color=cluster, shape=cluster)) + 
          geom_point(alpha=.7) +
          geom_point(data=centers_BT, aes(x=Religious_MinMax_BT, y=Shopping_MinMax_BT), size=3, stroke=2) + 
          labs(title= paste("Clusters with K = ", toString(i)),
               x ="Religious Reviews", y = "Shopping Reviews") + theme(plot.title = element_text(hjust = 0.5)))
}


Review_BT$cluster <- NULL


#summary table
ReviewsSUM_BT <- Review_BT %>%
  group_by(cluster_3) %>%
  summarise(Sports_BT = mean(Sports_BT), Religious_BT = mean(Religious_BT), Nature_BT=mean(Nature_BT), Theatre_BT=mean(Theatre_BT), 
            Shopping_BT=mean(Shopping_BT), Picnic_BT=mean(Picnic_BT), Age_BT=mean(Age_BT),
            Nbr_BT=mean(Nbr_BT), N=n() )


data.frame(ReviewsSUM_BT)
# 

