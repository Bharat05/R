##################################################
### PROG8430 Assignment 2                       ##
### Multiple Linear Regression                  ## 
##################################################
#                                               ##
##################################################
# Written by Bharat Thakur
# ID: 8732715
#
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
#setwd("C:/Users/...../Assignment 2")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read text file (".txt")
# Diamond Dataset
Diamond_BT <- read.delim('diamond_val2.txt',header = TRUE)

Diamond_BT[5:10,]     #Prints data 5 to 10 to make sure it looks correct

ls(Diamond_BT)        #Lists all objects for Diamond Data
names(Diamond_BT)     #List the variables for Diamond Data
str(Diamond_BT)       #List structure of Diamond Data


#Rename for easier interpretation
names(Diamond_BT) <- c("Price_BT","Carat_BT","Source_BT","Year_BT","Clarity_BT","Color_BT","Cut_BT","Val_BT")

str(Diamond_BT)  #check changes

###################################################
## 1.Data Transformation                         ##
###################################################

# keep date as int for now



head(Diamond_BT)
str(Diamond_BT)


#Source - Convert to index (Dummy) Variables 
Src_Dummies_BT <- model.matrix(~Source_BT-1, data = Diamond_BT)
#Src_Dummies_BT

#Combine the Datasets again
Diamond_BT <- cbind(Diamond_BT, Src_Dummies_BT)
head(Diamond_BT)


str(Diamond_BT)


#Rename for easier interpretation
names(Diamond_BT) <- c("Price_BT","Carat_BT","Source_BT","Year_BT","Clarity_BT","Color_BT","Cut_BT","Val_BT","Alrosa_BT",
                       "DeBeers_BT","Debswana_BT","Petra_BT","RioTinto_BT","Rockwell_BT")

head(Diamond_BT)


###################################################
## 2. Descriptive Data Analysis                  ##
###################################################

#Supplier Distribution
counts <- table(Diamond_BT$Source_BT)
barplot(counts, main = 'Supplier Distribution', xlab ='Supplier', ylab ='Frequency')
Diamond_BT <- Diamond_BT[-c(3)]

summary(Diamond_BT)




par(mfrow=c(3,3))  #2x3 grid for graphs

# Histogram for all variables except Dummy Variables
# loop over column *names* instead of actual columns
sapply(names(Diamond_BT), function(cname){
  # (make sure we only plot the numeric columns and also exclude the dummy variables)
  if (is.numeric(Diamond_BT[[cname]]) & !(cname %in% c("Alrosa_BT","DeBeers_BT","Debswana_BT","Petra_BT","RioTinto_BT","Rockwell_BT"))) 
    # use the `main` param to put column name as plot title
    print(hist(Diamond_BT[[cname]], main=cname))
})



#par(mfrow=c(3,3))  #2x3 grid for graphs

# Histogram for all variables
# loop over column *names* instead of actual columns
#sapply(names(Diamond_BT), function(cname){
  # (make sure we only plot the numeric columns and also exclude the dummy variables)
  #if (is.numeric(Diamond_BT[[cname]]) ) 
    # use the `main` param to put column name as plot title
    #print(hist(Diamond_BT[[cname]], main=cname))
#})

#par(mfrow=c(1,1))


###################################################
## 3. Outliers                                   ##
###################################################

par(mfrow=c(3,3))

# BoxPlot for all variables except dummy variables
# loop over column *names* instead of actual columns
sapply(names(Diamond_BT), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Diamond_BT[[cname]]) & !(cname %in% c("Alrosa_BT","DeBeers_BT","Debswana_BT","Petra_BT","RioTinto_BT","Rockwell_BT")))
    # use the `main` param to put column name as plot title
    print(boxplot(Diamond_BT[[cname]], main=cname))
})

par(mfrow=c(1,1))


par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
#sapply(names(Diamond_BT), function(cname){
  # (make sure we only plot the numeric columns)
  #if (is.numeric(Diamond_BT[[cname]]))
    # use the `main` param to put column name as plot title
   # print(boxplot(Diamond_BT[[cname]], main=cname))
#})



#########################################
## 4 Exploratory Analysis              ##
## 4.1 Test Data for Normality         ##
#########################################

### NOTE - Instead of doing these one at a time, do them all together.

DiaNrm_BT <- lapply(Diamond_BT, shapiro.test)
DiaNrm_BT
str(DiaNrm_BT[[4]])

DiaNrmRes_BT <- sapply(DiaNrm_BT, `[`, c("statistic","p.value"))
DiaNrmRes_BT

DiaRest_BT <- t(DiaNrmRes_BT)
DiaRest_BT


#Graphical Tests

par(mfrow=c(3,3))

# BoxPlot for all variables except Dummy Variables
# loop over column *names* instead of actual columns
sapply(names(Diamond_BT), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Diamond_BT[[cname]]) & !(cname %in% c("Alrosa_BT","DeBeers_BT","Debswana_BT","Petra_BT","RioTinto_BT","Rockwell_BT")))
    # use the `main` param to put column name as plot title
    qqnorm(Diamond_BT[[cname]], main=cname)
  qqline(Diamond_BT[[cname]])
})

par(mfrow=c(1,1))


#########################################
## 4.2 Checking Correlations           ##
#########################################

corrgram(Diamond_BT, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Diamond Price Stats")

res <- cor(Diamond_BT, method="spearman")
round(res, 2)




#########################################
## 5. Model Development                ##
## Creating Baseline Model             ##
#########################################

Dia_lm_BT = lm(Price_BT ~ Carat_BT + Year_BT + Clarity_BT + Color_BT + Cut_BT + Val_BT +
              Alrosa_BT + DeBeers_BT + Debswana_BT + Petra_BT +  RioTinto_BT,
            data=Diamond_BT, na.action=na.omit)
Dia_lm_BT
summary(Dia_lm_BT)




#########################################
## Creating Forward  Selection Model   ##
#########################################

min_model_BT <- lm(Price_BT ~ 1, data=Diamond_BT, na.action=na.omit)
Fwd_Dia_lm_BT = step(min_model_BT, direction="forward", scope =(
  ~ Carat_BT + Year_BT + Clarity_BT + Color_BT + Cut_BT + Val_BT +
    Alrosa_BT + DeBeers_BT + Debswana_BT + Petra_BT +  RioTinto_BT), details=TRUE)

Fwd_Dia_lm_BT
summary(Fwd_Dia_lm_BT)



#########################################
## Creating Step Selection Model   ##
#########################################

stp_Dia_lm_BT <- step(Dia_lm_BT)
stp_Dia_lm_BT
summary(stp_Dia_lm_BT)


#########################################
## Evaluating the Models               ##
#########################################

###########################################
## Creating Model and Residual vectors    #
###########################################

DiaFit_BT <- predict(Dia_lm_BT)
DiaRes_BT <- residuals(Dia_lm_BT)



FwdDiaFit_BT <- predict(Fwd_Dia_lm_BT)
FwdDiaRes_BT <- residuals(Fwd_Dia_lm_BT)

StpDiaFit_BT <- predict(stp_Dia_lm_BT)
StpDiaRes_BT <- residuals(stp_Dia_lm_BT)


#Numerically

shapiro.test(DiaRes_BT)
shapiro.test(FwdDiaRes_BT)
shapiro.test(StpDiaRes_BT)


#Graphically

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Dia_lm_BT)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section




par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Fwd_Dia_lm_BT)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(stp_Dia_lm_BT)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section



#
str(Diamond_BT)


Diamond_sub_BT <- Diamond_BT[c(1,2,4,5,6)]


pairs(Diamond_sub_BT,pch =46)

Dia_Car_BT <- lm(Price_BT ~ Carat_BT, data =Diamond_sub_BT)
summary(Dia_Car_BT)

plot(Dia_Car_BT,1)

Diamond_sub_BT$lg_Price <- log(Diamond_sub_BT$Price)
head(Diamond_sub_BT)


plot(Diamond_sub_BT$Carat_BT, Diamond_sub_BT$lg_Price)

Dia_Car_lgP <- lm(lg_Price ~ Carat_BT, data =Diamond_sub_BT)
summary(Dia_Car_lgP)

plot(Dia_Car_lgP,1)



Diamond_sub_BT$lg_Carat <- log(Diamond_sub_BT$Carat_BT)
head(Diamond_sub_BT)


plot(Diamond_sub_BT$lg_Carat, Diamond_sub_BT$lg_Price)


Dia_Car_lgP_lgC <- lm(lg_Price ~ lg_Carat, data =Diamond_sub_BT)
summary(Dia_Car_lgP_lgC)

plot(Dia_Car_lgP_lgC,1)


