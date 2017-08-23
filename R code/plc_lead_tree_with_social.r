setwd ("C:\\Users\\wanghaiy\\Documents\\IMA\\Frank\\Frank2\\R_codes")

source('myRfunctions_plc.r')

library("rpart")

library("rattle")

load('lead_soc.RData')

X <-lead_soc

str(X)
names(X)
#summary(X$firstInterestMatchYN)

nrow(X)   #currentlead: 283543; leadmarch1: 277494; march31; 284256

names(X)[22] <- 'y'
names(X)


sum(X$y)  

sum(X$y)/nrow(X)  



################################################################################
###hold out method 
################################################################################
set.seed(123456)

# randomly split X into train and test:
train_ind <- sample(c(FALSE, TRUE), size=nrow(X), replace=TRUE, prob=c(0.3,0.7))
Xtrain <- X[train_ind,]
Xtest <- X[!train_ind,]

nrow(Xtrain)    #currentlead: 198524;   march31: 199031


################################################################################
### no model
################################################################################ 
same_p <- mean(Xtrain$y)


################################################################################
### classification tree
################################################################################
  
 myformula <- (y~
 #+ `LeadID`  # 194338 unique values
#+ `dwContactPK`  # 192040 unique values
#+ `RespID`  # 194338 unique values
#+ `GRMID` 
#+ `RespDttm` 
#+ `CampaignPK`  # 15790 unique values
 `CampaignBusinessGroup`  # 12 unique values
+ `CampaignBusinessDivision`  # 58 unique values
+ `CampaignBusinessUnit`  # 47 unique values
+ ifelse(is.na(`CampaignChannel`),'Unknown',as.character(`CampaignChannel`))  # 14 unique values
+ ifelse(is.na(`CampaignTactic`),'Unknown',as.character(`CampaignTactic`))  # 21 unique values
+ ifelse(is.na(`CampaignProductLine1`),'Unknown',as.character(`CampaignProductLine1`))  # 28 unique values
+ ifelse(is.na(`CampaignProductLine2`),'Unknown',as.character(`CampaignProductLine2`))  # 17 unique values
+ ifelse(is.na(`CampaignProductLine3`),'Unknown',as.character(`CampaignProductLine3`))  # 14 unique values
+ ifelse(is.na(`CampaignProductLine4`),'Unknown',as.character(`CampaignProductLine4`))  # 9 unique values
+ ifelse(is.na(`CampaignProductLine5`),'Unknown',as.character(`CampaignProductLine5`))  # 6 unique values
+ ifelse(is.na(`ProductLineInterest1`),'Unknown',as.character(`ProductLineInterest1`))  # 132 unique values
+ ifelse(is.na(`ProductLineInterest2`),'Unknown',as.character(`ProductLineInterest2`))  # 99 unique values
+ ifelse(is.na(`ProductLineInterest3`),'Unknown',as.character(`ProductLineInterest3`))  # 77 unique values
+ ifelse(is.na(`ProductLineInterest4`),'Unknown',as.character(`ProductLineInterest4`))  # 65 unique values
+ ifelse(is.na(`ProductLineInterest5`),'Unknown',as.character(`ProductLineInterest5`))  # 43 unique values
#+ `ConvertedWithAmount` 
#+ ifelse(is.na(`OppDateCreated`),as.POSIXct('2012-10-01 13:04:00'),`OppDateCreated`) 
#+ ifelse(is.na(`OppCloseDate`),as.POSIXct('2012-10-24 23:59:00'),`OppCloseDate`) 
#+ ifelse(is.na(`OppStageName`),'Unknown',as.character(`OppStageName`))  # 10 unique values
#+ ifelse(is.na(`OppStatus`),'Unknown',as.character(`OppStatus`))  # 2 unique values
#+ ifelse(is.na(`OppOwnerGBU`),'Unknown',as.character(`OppOwnerGBU`))  # 33 unique values
#+ ifelse(is.na(`OppCountry`),'Unknown',as.character(`OppCountry`))  # 87 unique values
#+ ifelse(is.na(`OppCurrency`),'Unknown',as.character(`OppCurrency`))  # 18 unique values
#+ ifelse(is.na(`OppClosed01`),'Unknown',`OppClosed01`) 
#+ ifelse(is.na(`OppAmount`),-1,`OppAmount`) 
#+ `LifeStatus` 
#+ ifelse(is.na(`CompCity`),'Unknown',as.character(`CompCity`))  # 21915 unique values
#+ ifelse(is.na(`CompStPv`),'Unknown',as.character(`CompStPv`))  # 2509 unique values
#+ `CompCountry`  # 218 unique values
#+ ifelse(is.na(`ContProd1`),'Unknown',as.character(`ContProd1`))  # 1261 unique values
+ `RespRespType_s`  # 14 unique values
+ ifelse(is.na(`RespRespMethod_s`),'Unknown',as.character(`RespRespMethod_s`))  # 21 unique values
+ ifelse(is.na(`RespPriority`),'Unknown',as.character(`RespPriority`))  # 5 unique values
+ `RespShipMeth`  # 7 unique values
#+ ifelse(is.na(`RespTriggeringCd`),'Unknown',as.character(`RespTriggeringCd`))  # 523 unique values
+ ifelse(is.na(`RespTimeFrame`),'Unknown',as.character(`RespTimeFrame`))  # 51 unique values
+ `RespWillingToBuy`  # 3 unique values
+ ifelse(is.na(`ContProdQuant1`),-1,`ContProdQuant1`) 
+ `RespPartnerLeadShareFlg`  # 3 unique values
+ ifelse(is.na(`RespContactReq`),'Unknown',as.character(`RespContactReq`))  # 8 unique values
+ `BusinessUnit_s`  # 53 unique values
+ `ContactedByTMFlag`  # 2 unique values
+ ifelse(is.na(`BANTValidatedYN`),'Unknown',as.character(`BANTValidatedYN`))  # 3 unique values
#+ ifelse(is.na(`GeneratedToEloquaSFDCDttm`),as.POSIXct('1899-12-31'),`GeneratedToEloquaSFDCDttm`) 
#+ ifelse(is.na(`RespTouchPoint`),'Unknown',as.character(`RespTouchPoint`))  # 273 unique values
#+ ifelse(is.na(`department_name`),'Unknown',as.character(`department_name`))  # 2386 unique values
+ ifelse(is.na(`job_level_category`),'Unknown',as.character(`job_level_category`))  # 9 unique values
+ ifelse(is.na(`job_function_category`),'Unknown',as.character(`job_function_category`))  # 41 unique values
#+ ifelse(is.na(`countryName`),'Unknown',as.character(`countryName`))  # 222 unique values
+ ifelse(is.na(`BusinessFuction`),'Unknown',as.character(`BusinessFuction`))  # 55 unique values
+ ifelse(is.na(`JobRole`),'Unknown',as.character(`JobRole`))  # 51 unique values
#+ ifelse(is.na(`ContRespAreaTx`),'Unknown',as.character(`ContRespAreaTx`))  # 1216 unique values
+ ifelse(is.na(`MasterLeadFlg`),'Unknown',as.character(`MasterLeadFlg`))  # 3 unique values
+ ifelse(is.na(`Email_validity`),'Unknown',as.character(`Email_validity`))  # 3 unique values
#+ `Email_Validity_Dttm` 
+ ifelse(is.na(`Language`),'Unknown',as.character(`Language`))  # 78 unique values
+ ifelse(is.na(`JobSeniority`),'Unknown',as.character(`JobSeniority`))  # 8 unique values
+ ifelse(is.na(`JobResponsibilities`),'Unknown',as.character(`JobResponsibilities`))  # 33 unique values
+ ifelse(is.na(`PurchasingRole`),'Unknown',as.character(`PurchasingRole`))  # 32 unique values
+ ifelse(is.na(`Suppress_from_Campaigns`),'Unknown',as.character(`Suppress_from_Campaigns`))  # 2 unique values
#+ ifelse(is.na(`SFDCAccountID`),'Unknown',as.character(`SFDCAccountID`))  # 2283 unique values
+ `numOfRespInLast12M` 
+ `numOfRespInHistory` 
#+ ifelse(is.na(`PreviousRespDate`),as.POSIXct('1913-12-14'),`PreviousRespDate`) 
+ ifelse(is.na(`PreviousREspShipMeth`),'Unknown',as.character(`PreviousREspShipMeth`))  # 8 unique values
+ `NumOfResToDirectMail` 
+ `NumOfResToWeb` 
+ `NumOfResToEmail` 
+ `NumOfResToPhone` 
+ `NumOfResToManagedBySales` 
+ `NumOfResToCustomerVisit` 
+ ifelse(is.na(`Tactic`),'Unknown',as.character(`Tactic`))  # 21 unique values
+ ifelse(is.na(`PreviousTactic`),'Unknown',as.character(`PreviousTactic`))  # 16 unique values
+ `NumOfResToDigitalMarketing` 
+ `NumOfResToDirectMarketing` 
+ `NumOfResToEventHPDriven` 
+ `NumOfResToEventPartnerDriven` 
+ `NumOfResToCatchAllActivity` 
+ `NumOfResToAdvertising` 
+ `NumOfResToCustEval` 
+ `NumOfResToFieldSales` 
+ `NumOfResToTraining` 
+ `NumOfResToChannelPartnerMktDev` 
+ `NumOfResToWebMarketing` 
+ `cumulativeNumOfResp`
+ `recency` 
+ `numOfProdInterests` 
+ ifelse(`firstInterestMatchYN`==FALSE, FALSE, TRUE) 
+ ifelse(is.na(`NegativeTweets1`),0,`NegativeTweets1`) 
+ ifelse(is.na(`NeutralTweets1`),0,`NeutralTweets1`) 
+ ifelse(is.na(`PositiveTweets1`),0,`PositiveTweets1`) 
+ ifelse(is.na(`TotalTweets1`),1,`TotalTweets1`) 
+ ifelse(is.na(`AverageNegativeSentiment1`),-9.38,`AverageNegativeSentiment1`) 
+ ifelse(is.na(`AverageNeutralSentiment1`),-1,`AverageNeutralSentiment1`) 
+ ifelse(is.na(`AveragePositiveSentiment1`),2,`AveragePositiveSentiment1`) 
+ ifelse(is.na(`AverageTotalSentiment1`),-6,`AverageTotalSentiment1`) 
+ ifelse(is.na(`PercentNegativeTweets1`),'Unknown',`PercentNegativeTweets1`) 
+ ifelse(is.na(`PercentNeutralTweets1`),'Unknown',`PercentNeutralTweets1`) 
+ ifelse(is.na(`PercentPositiveTweets1`),'Unknown',`PercentPositiveTweets1`) 
+ ifelse(is.na(`NegativeTweets2`),0,`NegativeTweets2`) 
+ ifelse(is.na(`NeutralTweets2`),1,`NeutralTweets2`) 
+ ifelse(is.na(`PositiveTweets2`),0,`PositiveTweets2`) 
+ ifelse(is.na(`TotalTweets2`),1,`TotalTweets2`) 
+ ifelse(is.na(`AverageNegativeSentiment2`),-7.7,`AverageNegativeSentiment2`) 
+ ifelse(is.na(`AverageNeutralSentiment2`),-1,`AverageNeutralSentiment2`) 
+ ifelse(is.na(`AveragePositiveSentiment2`),2,`AveragePositiveSentiment2`) 
+ ifelse(is.na(`AverageTotalSentiment2`),-6,`AverageTotalSentiment2`) 
+ ifelse(is.na(`PercentNegativeTweets2`),'Unknown',`PercentNegativeTweets2`) 
+ ifelse(is.na(`PercentNeutralTweets2`),'Unknown',`PercentNeutralTweets2`) 
+ ifelse(is.na(`PercentPositiveTweets2`),'Unknown',`PercentPositiveTweets2`) 
+ ifelse(is.na(`NegativeTweets3`),0,`NegativeTweets3`) 
+ ifelse(is.na(`NeutralTweets3`),1,`NeutralTweets3`) 
+ ifelse(is.na(`PositiveTweets3`),0,`PositiveTweets3`) 
+ ifelse(is.na(`TotalTweets3`),1,`TotalTweets3`) 
+ ifelse(is.na(`AverageNegativeSentiment3`),-7,`AverageNegativeSentiment3`) 
+ ifelse(is.na(`AverageNeutralSentiment3`),-1,`AverageNeutralSentiment3`) 
+ ifelse(is.na(`AveragePositiveSentiment3`),2,`AveragePositiveSentiment3`) 
+ ifelse(is.na(`AverageTotalSentiment3`),-1.33,`AverageTotalSentiment3`) 
+ ifelse(is.na(`PercentNegativeTweets3`),'Unknown',`PercentNegativeTweets3`) 
+ ifelse(is.na(`PercentNeutralTweets3`),'Unknown',`PercentNeutralTweets3`) 
+ ifelse(is.na(`PercentPositiveTweets3`),'Unknown',`PercentPositiveTweets3`) 
+ ifelse(is.na(`NegativeTweets4`),0,`NegativeTweets4`) 
+ ifelse(is.na(`NeutralTweets4`),1,`NeutralTweets4`) 
+ ifelse(is.na(`PositiveTweets4`),0,`PositiveTweets4`) 
+ ifelse(is.na(`TotalTweets4`),1,`TotalTweets4`) 
+ ifelse(is.na(`AverageNegativeSentiment4`),-7,`AverageNegativeSentiment4`) 
+ ifelse(is.na(`AverageNeutralSentiment4`),-1,`AverageNeutralSentiment4`) 
+ ifelse(is.na(`AveragePositiveSentiment4`),2,`AveragePositiveSentiment4`) 
+ ifelse(is.na(`AverageTotalSentiment4`),-1.33,`AverageTotalSentiment4`) 
+ ifelse(is.na(`PercentNegativeTweets4`),'Unknown',`PercentNegativeTweets4`) 
+ ifelse(is.na(`PercentNeutralTweets4`),'Unknown',`PercentNeutralTweets4`) 
+ ifelse(is.na(`PercentPositiveTweets4`),'Unknown',`PercentPositiveTweets4`) 
+ ifelse(is.na(`NegativeTweets5`),0,`NegativeTweets5`) 
+ ifelse(is.na(`NeutralTweets5`),2,`NeutralTweets5`) 
+ ifelse(is.na(`PositiveTweets5`),0,`PositiveTweets5`) 
+ ifelse(is.na(`TotalTweets5`),2,`TotalTweets5`) 
+ ifelse(is.na(`AverageNegativeSentiment5`),-7,`AverageNegativeSentiment5`) 
+ ifelse(is.na(`AverageNeutralSentiment5`),-1,`AverageNeutralSentiment5`) 
+ ifelse(is.na(`AveragePositiveSentiment5`),2,`AveragePositiveSentiment5`) 
+ ifelse(is.na(`AverageTotalSentiment5`),-1.33,`AverageTotalSentiment5`) 
+ ifelse(is.na(`PercentNegativeTweets5`),'Unknown',`PercentNegativeTweets5`) 
+ ifelse(is.na(`PercentNeutralTweets5`),'Unknown',`PercentNeutralTweets5`) 
+ ifelse(is.na(`PercentPositiveTweets5`),'Unknown',`PercentPositiveTweets5`) 
)
 
 
 
 

# define the prior probability:
priorp <- sum(Xtrain$y)/dim(Xtrain)[[1]]  
priorp

# defines the loss matrix:
loss <- matrix(c(0,1, 1,0), nrow=2)

# set the control parameter on the complexity parameter:
rc <- rpart.control(minsplit = 20, cp = 0.001) # cp=0.01 is the default

#The model
a.r <- rpart(myformula,
method="class", control=rc, data=Xtrain,
parms=list(prior=c(1-priorp,priorp), loss=loss))


tree_nleafnodes(a.r)
tree_largest_smallest_leaf(a.r)
tree_depth(a.r)

#print the complexity table
printcp(a.r) 

# plot xerror against nsplit
xerror.plot(a.r)

# plot the cp as a function of nsplit:
plotcp(a.r)

# prune back the tree
a.r <- rpart.prune(a.r)
a.r <- rpart.prune.0SE(a.r)

#show results
variables_used_in_tree(a.r)

a <- variable.importance.used.in.tree(a.r)
a
str(a)


# output (to a file "VariableImportance.txt") the variable importance:
write.table(cbind(sapply(names(a), formula_term_to_column_name), a), file="VariableImportance_lead_social.txt", sep='|', row.names=names(a))


#output the rules 
asRules(a.r) # requires  package rattle

#######################################################################################
### now, use the tree to predict!
#######################################################################################

 Xtest <- ResetUnseenLevels(Xtrain, Xtest, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "RespPriority", "JobSeniority", "Suppress_from_Campaigns", "PreviousREspShipMeth")) #this will replace the unseen levels in Xtest to the corresponding most popular levels

Xtest <- ResetUnseenLevels(Xtrain, Xtest, columnnames=c("PercentNegativeTweets5", "PercentNeutralTweets5"))
Xtest$PercentNegativeTweets5 <- NA

Xtest$PercentNeutralTweets5 <- NA

Xtest$PercentPositiveTweets5 <- NA

#re-run the model with the Tweet related attributes commented out 
rpart_test_p_2 <- predict(a.r, newdata=Xtest)[,2]  

head(rpart_test_p)
str(rpart_test_p)
summary(rpart_test_p)
sort(unique(rpart_test_p))
hist(rpart_test_p)
rpart_test_p<-as.vector(rpart_test_p)




myCaptureCurve(ActualAmount=Xtest$y,
 RankVectors=cbind(rpart_test_p, rpart_test_p_2),
 MarkPoints=c(mean(Xtest$y),0.1,0.2),
 RankVectorsToMark=c(TRUE, TRUE),
 LegendTexts=c("IncludeTweet", "ExcludeTweet"),
 ylab="% of converted Opportunities captured by selecting the best X%",
 xlab=paste("Top % of Lead (total", nrow(Xtest), "out-sample)"),
 showAUC=TRUE)  
 
