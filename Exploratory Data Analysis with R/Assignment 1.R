##################################################
### PROG8430                                    ##
### Assignment 1                                ## 
##################################################
#                                               ##
##################################################
# Written by Bharat Thakur

##################################################
### Basic Set Up                                ##
##################################################

# Clear all plots
if(!is.null(dev.list())) dev.off()

# Clear entire console
cat("\014") 

# Clean and clear the workspace
rm(list=ls())

#Set work directory to an appropriate location
#setwd("C:/Users/Assignment1")

options(scipen=2)

##################################################
## Install Libraries/Packages                   ##
##################################################

install.packages("dplyr")  #downloads package
library("dplyr")           #"attaches" package 


install.packages("lattice")  #downloads package
library("lattice")           #"attaches" package 

#loading data
Elect2015_BT <- read.csv('AS01.csv')

##################################################
##     Question 1    Summarizing Data           ##
##################################################

#Party Conservative
#Leader Harper
#Province BC

#1.1
#What is the total number of votes earned by your assigned party in each of the provinces?

CPV_Tot_BT <- Elect2015_BT %>%
  group_by(Prov) %>% 
  summarise(CPV_Tot = sum(CPV), .groups = 'drop')

CPV_Tot_BT


#1.2
#What is the weighted mean (average) score for the answer to the question "How do you feel about the party leaders?". 
#This should be answered for your assigned party's leader in your assigned province weighted by the number of electors 
#in each district.

WA_Har_BT <- Elect2015_BT %>% 
                 filter(Prov == "BC") %>% 
                 summarise(weighted.mean(Har,Electors,na.rm = TRUE))
WA_Har_BT

#1.3
#What is the standard deviation of the answer to the question "And what about the performance of the economy during the 
#past four years? Has it improved, stayed the same, or got worse?" for electoral districts that were won by your assigned
#political party.

SD_Econ_Won_BT <- Elect2015_BT %>% 
                       filter(Win_Party == 'CPC') %>%
                       summarise(sd(ecc_sat))
SD_Econ_Won_BT

#1.4
#What is the 28th percentile (quantile) of number of females across the study file? 

Pct_Fem_BT <-  quantile(Elect2015_BT$Female, probs = .28, na.rm = TRUE)
Pct_Fem_BT

#1.5
#What is the median absolute deviation of turnout for electoral districts in your assigned province? 
TO_MAD_BT <- Elect2015_BT %>% 
                 filter(Prov == "BC") %>% 
                 summarise(mad(TO,na.rm = TRUE))
TO_MAD_BT

##################################################
##     Question 2   Organizing Data             ##
##################################################


#2.1.a
#Create a table that shows Number of Electoral Districts (Ridings) won by each party (columns) in each
#province(rows). 

#Calculating percentage electoral districts won by each party in a province.
Cnt_Won_Long_BT <- Elect2015_BT %>%
  group_by(Prov, Win_Party) %>% 
  summarise(Nbr_Won = n(), .groups = 'drop') %>%
  mutate(Pct_Won = Nbr_Won *100 / sum(Nbr_Won , na.rm = TRUE)) %>%
  select(Prov,Win_Party, Pct_Won) 

#converting tibble to Dataframe
Cnt_Won_Long_BT <- as.data.frame(Cnt_Won_Long_BT)


#Reshaping dataframe to wide format and rounding decimal places to 2
Cnt_Won_BT <- reshape(Cnt_Won_Long_BT, idvar = "Prov", timevar = "Win_Party", direction = "wide") %>%
  mutate_if(is.numeric, round, digits = 2)

#setting Na's introduced by reshaping to 0
Cnt_Won_BT[is.na(Cnt_Won_BT)] <- 0     

Cnt_Won_BT


#2.1.b
#In what province did your party win the highest percentage of ridings?

Prov_MaxWon_Pct_BT <- Cnt_Won_BT[which.max(Cnt_Won_BT$Pct_Won.CPC),]$Prov
Prov_MaxWon_Pct_BT

#2.2.a
#Create a bar chart showing the number of Males and Females by which party won the electoral district
#NOTE - Exclude the three territories from this analysis (Nunavut, Northwest Territories and Yukon).

Elect_Flt_BT <- Elect2015_BT[!(Elect2015_BT$ED_Name %in% 
                                 c("Nunavut", "Northwest Territories/Territoires du Nord-Ouest
                     Yukon")), ]

barchart( Win_Party ~ Male + Female,
          data=Elect_Flt_BT, beside=TRUE, main="Gender Breakdown by Winning Party",
          xlab="Gender", ylab="Party", 
          auto.key=list(space='bottom'))

#2.2.b
#Which party got a higher number of female voters according to this display? 
#  Liberal


#2.3.a
#Create a histogram showing the distribution of the answer to the question "How do you feel about the party leaders?"
#Give your answer for the leader of your assigned party

Hist_Har_BT <- hist(Elect2015_BT$Har,
                    xlab="Harper",
                    main="Support for Harper(CPC) according to LPP2015",
                    xlim=c(0,70),
                    col="skyblue",
                    freq=TRUE)

#2.3.b
#Which range of values has the highest frequency? 

cat("[",Hist_Har_BT$breaks[which.max(Hist_Har_BT$counts)], ",",
      Hist_Har_BT$breaks[which.max(Hist_Har_BT$counts) +1 ],"]", sep = "")


#2.4.a
#Create a sequence of box plots showing the distribution of the answer to the question "How do you feel about the political
#parties?" NOTE - Exclude the three territories from this analysis (Nunavut, Northwest Territories and Yukon)

BWplot_CPC_BT <- bwplot(CPC ~ Win_Party , data=Elect_Flt_BT, 
                        main="Support for Conservative Party by Electoral Districts won by each Party",
                        xlab="Electoral District Winning Party",
                        ylab = "Support for Conservative Party", pch = '|')

BWplot_CPC_BT


#2.4.b
#According to the charts, in which sort of Electoral District (i.e. Winning Party) does your party have the most support?
#For example, if your party is the Green party, do they have the most support in districts won by the Liberals, the Conservatives
#, the NDP, etc.

# The Electoral Districts won by Conservative Party have the most support for the Conservatives.

#2.4.c
#In which party does your party have the least support? 
#  In Electoral Districts won by NDP have the least support for Conservatives.


#2.4.d
#Which has the greatest variability in support? 

Range_CPC_BT <- range(Elect_Flt_BT[Elect_Flt_BT$Win_Party =='CPC',"CPC"])
Range_CPC_BT[2] - Range_CPC_BT[1]


Range_LIB_BT <- range(Elect_Flt_BT[Elect_Flt_BT$Win_Party =='LIB',"CPC"],na.rm = TRUE)
Range_LIB_BT[2] - Range_LIB_BT[1]

#2.5.a
#Create a histogram for the answer to the question: "How do you feel about the political parties?" for your assigned party

Hist_CPC_BT <- hist(Elect2015_BT$CPC,
                    main="Support for Conservatives according to LPP2015",
                    xlab="Conservatives Support",
                    xlim=c(0,80),
                    col="skyblue",
                    freq=TRUE)

#2.5.b
#Create a histogram for the answer to the question: "How do you feel about the party leaders?" for your assigned leader. 

Hist_Har_BT <- hist(Elect2015_BT$Har,
                    xlab="Harper",
                    main="Support for Harper(CPC) according to LPP2015",
                    xlim=c(0,70),
                    col="skyblue",
                    freq=TRUE)


#2.5.c
#Create a scatter plot showing the relationship between the answer to the question: "How do you feel about the political 
# parties?" for your assigned party and "How do you feel about the party leaders?" for your assigned leader.

SP_Support_BT <- xyplot(CPC ~ Har, data=Elect2015_BT, color="violet", pch=20,
                        main="Support for Conservative Party v/s Support for Conservative Party Leader(Harper)",
                        xlab = 'Support for Conservative Party Leader(Harper)',
                        ylab = 'Support for Conservative Party')

SP_Support_BT

#2.5.d
#	What conclusions, if any, can you draw from the chart? 
#  Support for conservative party and its leader appear to be strongly correlated.


#2.5.e
#Calculate the correlation coefficient between these two variables. Discuss why you chose the measure you did and what conclusion you draw from it. 

#qqnorm(Elect2015_BT$CPC);qqline(Elect2015_BT$CPC, col = 2, main = 'Is Support of Conservative Party Normal')
shapiro.test(Elect2015_BT$CPC)

#p-value less than .05 so there is good evidence that data is not normally Distributed

#qqnorm(Elect2015_BT$Har);qqline(Elect2015_BT$Har, col = 2,main = 'Is Support of Conservative Party Leader Normal')
shapiro.test(Elect2015_BT$Har)

#p-value less than .05 means that data is not normally Distributed


#Since data is not normally distributed, it is better to use spearman correlation.
cor.test(Elect2015_BT$CPC , Elect2015_BT$Har, method = 'spearman')




##################################################
##             Question 3                       ##
##################################################

#3.1.a
#	Create a QQ Normal plot of Votes for your party (for example, the Green Party would be variable GRV) 

qqnorm(Elect2015_BT$CPV);qqline(Elect2015_BT$CPV, col = 2, main = 'Is No of Votes for Conservative Party Normal')

#3.1.b
#Conduct a statistical test for normality on Votes for your party. 

shapiro.test(Elect2015_BT$CPV)

#3.1.c
#Are the Votes for your party normally distributed? 
#  P-value of Shapiro-wilk test is less than .05 means that data in not normally distributed.

#3.2.a
#Compare average votes for your party in electoral districts your party won to average votes for your party
#in electoral districts your party did not win using a suitable hypothesis test.

Won_VOT_BT <- Elect2015_BT %>% 
                filter(Win_Party == "CPC") %>% 
                select(CPV)
shapiro.test(Won_VOT_BT$CPV)

Lost_VOT_BT <- Elect2015_BT %>% 
                filter(Win_Party != "CPC") %>% 
                select(CPV)
shapiro.test(Lost_VOT_BT$CPV)


Avg_Vot_Test_BT <- wilcox.test(Won_VOT_BT$CPV,
                               Lost_VOT_BT$CPV,
                               exact = FALSE)
Avg_Vot_Test_BT

#3.2.b
#Explain why you chose the test you did.


#P-value of Shapiro-wilk test is less than .05 for both the groups of electoral districts(one where CPC won and other where CPC lost)which means that data in not normally distributed. 
#Since data is not normally distributed we cannot use T-test. Instead, we used a 'non-parametric' test: The Wilcoxon signed rank test.

#3.2.b
#Do you have strong evidence that the average votes are different? 
#  Very low p-value of 2.2e-16 shows that there is strong evidence that average votes are different.

