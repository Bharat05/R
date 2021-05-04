  ##################################################
  ### Assignment 4 Classification                 ## 
  ##################################################
 
  
  ##################################################
  # Written by Bharat Thakur                      ##
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
  setwd("C:/Users/Kala/Google Drive/Conestoga/Data Analysis/Assignment 4 Classification/")
  
  options(scipen=9)
  
  ##################################################
  ### Install Libraries                           ##
  ##################################################
  
  #If the library is not already downloaded, download it
  
  if(!require(pROC)){install.packages("pROC")}
  library(pROC)
  
  if(!require(klaR)){install.packages("klaR")}
  library("klaR")
  
  # For LDA
  
  if(!require(MASS)){install.packages("MASS")}
  library("MASS")
  
  
  
  ##################################################
  ### Read data and do preliminary data checks    ##
  ##################################################
  
  # Read "comma separated value" files (".csv")
  # Tumor data set
  Tumor_BT <- read.csv("Tumor_20F.csv", header = TRUE, sep = ",")
 
  head(Tumor_BT,5)  #Print a Few Observations to Verify
  
  #Rename for easier interpretation
  
  names(Tumor_BT) <- c("Outcome_BT", "Age_BT", "Sex_BT", "Bone_Density_BT", "Bone_Marrow_BT", "Lung_Spot_BT", "Pleura_BT",
                       "Liver_Spot_BT", "Brain_Scan_BT", "Skin_Lesions_BT", "Stiff_Neck_BT", "Supraclavicular_BT",
                       "Axillar_BT", "Mediastinum_BT")
  names(Tumor_BT)
  str(Tumor_BT)
  
  summary(Tumor_BT)
  
  
  
  #Adjust for 0 or 1
  for( i in names(Tumor_BT)){
    Tumor_BT[i] = Tumor_BT[i] - 1 
  }  
  Tumor_BT$Age_BT <- Tumor_BT$Age_BT - 1  #Age took values {2,3}
  
  #So that 0 means No and 1 means Yes consistently throughout the dataset
  Tumor_BT$Supraclavicular_BT = ifelse(Tumor_BT$Supraclavicular_BT == 0, 1,0)
  Tumor_BT$Axillar_BT = ifelse(Tumor_BT$Axillar_BT == 0, 1,0)
    

  head(Tumor_BT,5)  #Print a Few Observations to Verify
  
  ##################################################
  ### Descriptive Analysis                        ##
  ##################################################
  
  summary(Tumor_BT)
 
  
  par(mfrow=c(3,5))
  
  #barplot outcome 
  Out_table_BT <- table(Tumor_BT$Outcome_BT)
  names(Out_table_BT) <- c('Not Present', 'Present')
  barplot(Out_table_BT, main="Tumor Outcome")
 
  #barplot age 
  Age_table_BT <- table(Tumor_BT$Age_BT)
  names(Age_table_BT) <- c('Younger', 'Older')
  barplot(Age_table_BT, main="Age")
  
  #barplot sex
  Sex_table_BT <- table(Tumor_BT$Sex_BT)
  names(Sex_table_BT) <- c('Female', 'Male')
  barplot(Sex_table_BT, main="Sex")
   
  #barplot bone-density 
  Bone_Den_table_BT <- table(Tumor_BT$Bone_Density_BT)
  names(Bone_Den_table_BT) <- c('Good', 'Bad')
  barplot(Bone_Den_table_BT, main="Bone Density")
  
  #barplot bone-marrow
  Bone_Mar_table_BT <- table(Tumor_BT$Bone_Marrow_BT)
  names(Bone_Mar_table_BT) <- c('Good', 'Bad')
  barplot(Bone_Mar_table_BT, main="Bone Marrow")
  
  #barplot Lung Spot
  Lung_Spot_table_BT <- table(Tumor_BT$Lung_Spot_BT)
  names(Lung_Spot_table_BT) <- c('No', 'Yes')
  barplot(Lung_Spot_table_BT, main="Lung Spot")
  
  #barplot Pleura
  Pleura_table_BT <- table(Tumor_BT$Pleura_BT)
  names(Pleura_table_BT) <- c('No', 'Yes')
  barplot(Pleura_table_BT, main="Pleura")
  
  
  #barplot Live Spot
  Liver_Spot_table_BT <- table(Tumor_BT$Liver_Spot_BT)
  names(Liver_Spot_table_BT) <- c('No', 'Yes')
  barplot(Liver_Spot_table_BT,  main="Liver Spot")
  
  #barplot Brain Scan
  Brain_table_BT <- table(Tumor_BT$Brain_Scan_BT)
  names(Brain_table_BT) <- c('No', 'Yes')
  barplot(Brain_table_BT, main="Brain Scan")
  
  #barplot Skin Lesions
  Skin_table_BT <- table(Tumor_BT$Skin_Lesions_BT)
  names(Skin_table_BT) <- c('No', 'Yes')
  barplot(Skin_table_BT, main="Skin Lesions")
  
  #barplot Stiff Neck
  Neck_table_BT <- table(Tumor_BT$Stiff_Neck_BT)
  names(Neck_table_BT) <- c('No', 'Yes')
  barplot(Neck_table_BT, main="Stiff Neck")
  
  
  #barplot Supraclavicular
  Supra_table_BT <- table(Tumor_BT$Supraclavicular_BT)
  names(Supra_table_BT) <- c('No', 'Yes')
  barplot(Supra_table_BT, main="Supraclavicular")
  
  
  
  #barplot Axillar
  Axil_table_BT <- table(Tumor_BT$Axillar_BT)
  names(Axil_table_BT) <- c('No', 'Yes')
  barplot(Axil_table_BT, main = 'Axillar')
  
  #barplot Mediastinum
  Media_table_BT <- table(Tumor_BT$Mediastinum_BT)
  names(Media_table_BT) <- c('No', 'Yes')
  barplot(Media_table_BT, main = 'Mediastinum')
  
  par(mfrow=c(1,1))
  
  ##################################################
  ### Exploratory Analysis                        ##
  ##################################################
  
  Tumor_Corr_BT <- cor(Tumor_BT, method="spearman")
  round(Tumor_Corr_BT, 2) 
  

  ######## Contigency table for Medistinum_BT variable
  
  Tbl_Media_BT <- table(Tumor_BT$Outcome_BT, Tumor_BT$Mediastinum_BT, dnn=list("Outcome", "Mediastinum"))
  Tbl_Media_BT
  prop.table(Tbl_Media_BT, 2) # col percentages
  
  

  
  #Check the Chi Squared Test - NOTE Removal of Yate's Continuity Correction
  
  chisq_Media_BT <- chisq.test(Tumor_BT$Outcome_BT, Tumor_BT$Mediastinum_BT,   correct=FALSE)      
  chisq_Media_BT
  
  chisq_Media_BT$observed   # What we observed
  chisq_Media_BT$expected   # If there were no relationship
  
  

  ######## Contigency table for Sex_BT variable
  
  Tbl_Sex_BT <- table(Tumor_BT$Outcome_BT, Tumor_BT$Sex_BT, dnn=list("Outcome", "Sex"))
  Tbl_Sex_BT
  prop.table(Tbl_Sex_BT, 2) # col percentages
  
  # 47% Females of females had tumors compared to men who had tumor 75% of the times.
  
  #Check the Chi Squared Test - NOTE Removal of Yate's Continuity Correction
  
  chisq_Sex_BT <- chisq.test(Tumor_BT$Outcome_BT, Tumor_BT$Sex_BT,   correct=FALSE)      
  chisq_Sex_BT
  
  chisq_Sex_BT$observed   # What we observed
  chisq_Sex_BT$expected   # If there were no relationship
  
  # If there were no relationship around 60% of both men and women should have tumors, but this is not what
  # we observed that means that outcome and sex are correlated.
  
  #Mediastinum Bar Chart
  
  barplot(prop.table(Tbl_Media_BT,2), xlab='Mediastinum',ylab='Outcome',main="Outcome by Mediastinum",
          col=c("darkblue","darkred")
          ,legend=rownames(Tbl_Media_BT), args.legend = list(x = "topleft"))
  
  
  #Sex Bar Chart
  
  barplot(prop.table(Tbl_Sex_BT,2), xlab='Sex',ylab='Outcome',main="Outcome by Sex",
          col=c("darkblue","darkred"),legend=rownames(Tbl_Sex_BT), args.legend = list(x = "topleft"))
  
  
  
  ##################################################
  ### Building the Model                          ##
  ##################################################
  
  #stepwise
  
  Out_glm_BT = glm(Outcome_BT ~ Age_BT + Sex_BT + Bone_Density_BT + Bone_Marrow_BT + 
                  Lung_Spot_BT + Pleura_BT + Liver_Spot_BT + Brain_Scan_BT +
                  Skin_Lesions_BT + Stiff_Neck_BT + Supraclavicular_BT+
                  Axillar_BT + Mediastinum_BT,
                family="binomial", data=Tumor_BT, na.action=na.omit)
  
  stp_Out_glm_BT <- step(Out_glm_BT)
  
  summary(stp_Out_glm_BT)
  #same signs as correlation coefficient
  
  
  #UserModel 1 (Dropping Brain_Scan_BT)
  
  Out_UM_1_BT = glm(Outcome_BT ~ Sex_BT + Bone_Density_BT + Skin_Lesions_BT + 
                           Stiff_Neck_BT + Supraclavicular_BT+ Axillar_BT + Mediastinum_BT,
                family="binomial", data=Tumor_BT, na.action=na.omit)
  
  summary(Out_UM_1_BT)
  
  
  #UserModel 2(Dropping Supraclavicular_BT)
  start_time <- Sys.time()
  
  Out_UM_2_BT = glm(Outcome_BT ~ Sex_BT + Bone_Density_BT + Brain_Scan_BT + Skin_Lesions_BT + 
                      Stiff_Neck_BT + Axillar_BT + Mediastinum_BT,
                         family="binomial", data=Tumor_BT, na.action=na.omit)
  end_time <- Sys.time()
  UM2_time_BT = end_time - start_time
  
  
  summary(Out_UM_2_BT)
  
  

  
  ## Check the User Models
  
  #Confusion Matrix User Model 1
  resp_UM_1_BT <- predict(Out_UM_1_BT, type="response")   # creates probabilities
  head(resp_UM_1_BT,20)
  Class_UM_1_BT <- ifelse(resp_UM_1_BT > 0.5,1,0)           # Classifies probabilities (i.e. >50% then likely to donate)
  head(Class_UM_1_BT)
  True_log_BT <- Tumor_BT$Outcome_BT                        #Creates a vector of the true outcomes
  T1_BT <- table(True_log_BT, Class_UM_1_BT, dnn=list("Act Outcome","Predicted") )  # Creates a Contingency Table
                                                                       
  T1_BT
  
  #Confusion Matrix User Model 2
  resp_UM_2_BT <- predict(Out_UM_2_BT, type="response")   # creates probabilities
  head(resp_UM_2_BT,20)
  Class_UM_2_BT <- ifelse(resp_UM_2_BT > 0.5,1,0)           # Classifies probabilities (i.e. >50% then likely to donate)
  head(Class_UM_2_BT)

  T2_BT <- table(True_log_BT, Class_UM_2_BT, dnn=list("Act Outcome","Predicted") )  # Creates a Contingency Table
  
  T2_BT
  
  


  #ROC Curve (and Area Under the Curve)
  
  plot(roc(Tumor_BT$Outcome_BT,resp_UM_1_BT, direction="<"),
       col="red", lwd=2, main='ROC Curve for Logistic Regression - Outcome')
  
  auc(Tumor_BT$Outcome_BT, resp_UM_1_BT)
  
  #better than random chance, trade off TP and FP, slightly better 1st, but depends upon what cut off of TP and FP is
  # desired
 
  
  #ROC Curve  (and Area Under the Curve)
  
  plot(roc(Tumor_BT$Outcome_BT,resp_UM_2_BT, direction="<"),
     col="blue", lwd=2, main='ROC Curve for Logistic Regression - Outcome', add = TRUE)
  
  auc(Tumor_BT$Outcome_BT, resp_UM_2_BT)
  
  legend(1, .97, legend=c("User Model 1", "User Model 2"),
         col=c("red", "blue"), lty=1:2, cex=0.8)
  #add a legend

  
  ### SECOND PART ####
  
  ########################################
  ### 2. Logistic Regression - Stepwise  #
  ########################################
  
  #Confusion Matrix Step model

  start_time_BT <- Sys.time()
  
  Out_glm_BT = glm(Outcome_BT ~ Age_BT + Sex_BT + Bone_Density_BT + Bone_Marrow_BT + 
                     Lung_Spot_BT + Pleura_BT + Liver_Spot_BT + Brain_Scan_BT +
                     Skin_Lesions_BT + Stiff_Neck_BT + Supraclavicular_BT+
                     Axillar_BT + Mediastinum_BT,
                   family="binomial", data=Tumor_BT, na.action=na.omit)

  
  stp_Out_glm_BT <- step(Out_glm_BT)
  
  end_time_BT <- Sys.time()
  
  # Calculate the model fitting time
  sw_time_BT <- end_time_BT - start_time_BT
  
  summary(stp_Out_glm_BT)
  
  #confusion matrix
  resp_SW_BT <- predict(stp_Out_glm_BT, type="response")   # creates probabilities
  head(resp_SW_BT,20)
  Class_SW_BT <- ifelse(resp_SW_BT > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely to donate)
  head(Class_SW_BT)
  
  #Creates Confusion Matrix
  CF_SW_BT <- table(True_log_BT, Class_SW_BT, dnn=list("Act Outcome","Predicted") )  # Creates a Contingency Table
  
  CF_SW_BT
  

  
  ##################################
  # 3. Naive-Bayes Classification  #
  ##################################
  
  
  
  str(Tumor_BT)

  Tumor_BT$Outcome_BT <- as.factor(Tumor_BT$Outcome_BT)
  str(Tumor_BT)

  start_time_BT <- Sys.time()

  Tumor_Naive_BT <- NaiveBayes(Outcome_BT ~ Age_BT + Sex_BT + Bone_Density_BT + Bone_Marrow_BT +
                                 Lung_Spot_BT + Pleura_BT + Liver_Spot_BT + Brain_Scan_BT +
                                 Skin_Lesions_BT + Stiff_Neck_BT + Supraclavicular_BT+
                                 Axillar_BT + Mediastinum_BT,
                            data = Tumor_BT, na.action=na.omit)

  end_time_BT <- Sys.time()

  NB_Time_BT <- end_time_BT - start_time_BT

  #Classifies
  pred_bay_BT <- predict(Tumor_Naive_BT,Tumor_BT)
  

  #Creates Confusion Matrix
  CF_NB_BT <- table(Actual=Tumor_BT$Outcome_BT, Predicted=pred_bay_BT$class)
  CF_NB_BT

  
  
  ##################################
  ## 4. LDA                        #
  ##################################
  
  
  start_time_BT <- Sys.time()
  
  Tumor_Discrim_BT <- lda(Outcome_BT ~ Age_BT + Sex_BT + Bone_Density_BT + Bone_Marrow_BT + 
                            Lung_Spot_BT + Pleura_BT + Liver_Spot_BT + Brain_Scan_BT +
                            Skin_Lesions_BT + Stiff_Neck_BT + Supraclavicular_BT+
                            Axillar_BT + Mediastinum_BT,
                          data = Tumor_BT, na.action=na.omit)
  
  end_time_BT <- Sys.time()
  
  LDA_Time_BT <- end_time_BT - start_time_BT
  
  #Classifies
  pred_dis_BT <- predict(Tumor_Discrim_BT, data=Tumor_BT)
  #head(pred_dis$posterior,20)
  #Confusion Matrix
  CF_LDA_BT <- table(Actual=Tumor_BT$Outcome_BT, Predicted=pred_dis_BT$class)
  
  
  
  #Comparing all three
  
  #Confusion Matrix
  CF_NB_BT
  CF_LDA_BT
  CF_SW_BT
  
  #Run times
  NB_Time_BT
  LDA_Time_BT
  sw_time_BT
  
  

  