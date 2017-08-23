setwd ("C:\\Users\\wanghaiy\\Documents\\IMA\\Frank\\Frank2\\R_codes")

 
source('myRfunctions_plc.r')

library("rpart")

library("rattle")


options(digits=4)


###### load the saved data first (could pick one interested set of data for modeling)
load('leadmarch1.RData')

X <- leadmarch1


load('currentLead.RData')

X <- currentLead

load('leadmarch31.RData')

X <-leadmarch31

load('leadmarch11.RData') 
X <- leadmarch11

load('leadmarch21.RData')
X <- leadmarch21

load('leadapril11.RData')
X <- leadapril11


load('currentlead_full.RData')
X <-currentlead_full

load('currentleadnoacc.RData')
str(currentleadnoacc)
names(currentleadnoacc)

#X <- currentleadnoacc

load('currentlead2.RData')
X <- currentlead2



str(X)
names(X)

nrow(X)   #currentlead: 283543; leadmarch1: 277494; march31; 284256

names(X)[22] <- 'y'
names(X)


sum(X$y)  #currentlead: 4268; leadmarch1: 3596   march31: 4042

sum(X$y)/nrow(X)  #currentlead: 0.01505; leadmarch1: 0.01296; march31: 0.01422

sum(X$dwContactPK == '0') # no such record

load('testsetmarch1.RData')

load('testsetnow.RData')   

load('testsetmarch31.RData')

load('testsetmarch11.RData')

load('testsetmarch21.RData')

load('testsetapril11.RData')

     
#x_noacc <- currentleadnoacc
#
#x_noacc <- subset (x_noacc, select = -c(GRMID, CompCity, CompStPv, CompCountry))
#
#str(x_noacc)

#x_acc <- currentleadacc [,  c(1:88, 224:226)] 
#str(x_acc)
#
#X <-rbind(x_noacc, x_acc)
##### combined: 21 is y
#names(X)[21] <- 'y'
#





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
#+ `cumulativeNumOfResp`  # for data sets currentlead2 and currentlead_full
+ `recency` 
+ `numOfProdInterests` 
+ ifelse(`firstInterestMatchYN`==FALSE, FALSE, TRUE) 
)
 
 

 
# define the prior probability
priorp <- sum(Xtrain$y)/dim(Xtrain)[[1]]  
priorp

# defines the loss matrix
loss <- matrix(c(0,1, 1,0), nrow=2)

# set the control parameter on the complexity parameter
rc <- rpart.control(minsplit = 20, cp = 0.001) # cp=0.01 is the default

#The model
a.r <- rpart(myformula,
method="class", control=rc, data=Xtrain,
parms=list(prior=c(1-priorp,priorp), loss=loss))


#plot(a.r)
#text(a.r)

tree_nleafnodes(a.r)
tree_largest_smallest_leaf(a.r)
tree_depth(a.r)

#print the complexity table
printcp(a.r) 

# plot xerror against nsplit
xerror.plot(a.r)

# plot the cp as a function of nsplit:
plotcp(a.r)

# prune back the tree (pick one of the following)
#a.r <- rpart.prune(a.r)
a.r <- rpart.prune.0SE(a.r)

#show results
variables_used_in_tree(a.r)

a <- variable.importance.used.in.tree(a.r)
a
str(a)


# output (to a file "VariableImportance.txt") the variable importance:
write.table(cbind(sapply(names(a), formula_term_to_column_name), a), file="VariableImportance_currentlead_full.txt", sep='|', row.names=names(a))


#output the rules 
asRules(a.r) # requires  package rattle

#######################################################################################
### now, use the tree to predict
#######################################################################################

Xtest <- ResetUnseenLevels(Xtrain, Xtest, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "RespPriority", "JobSeniority", "Suppress_from_Campaigns", "PreviousREspShipMeth")) #this will replace the unseen levels in Xtest to the corresponding most popular levels


rpart_test_p <- predict(a.r, newdata=Xtest)[,2]  


head(rpart_test_p)
str(rpart_test_p)
summary(rpart_test_p)
sort(unique(rpart_test_p))
hist(rpart_test_p)
rpart_test_p<-as.vector(rpart_test_p)




myCaptureCurve(ActualAmount=Xtest$y,
 RankVectors=cbind(rpart_test_p),
 MarkPoints=c(mean(Xtest$y),0.1,0.2),
 RankVectorsToMark=c(TRUE),
 LegendTexts=c("Tree"),
 ylab="% of converted Opportunities captured by selecting the best X%",
 xlab=paste("Top % of Lead (total", nrow(Xtest), "out-sample)"),
 showAUC=TRUE)  
 
 
 


################################################################################
### stratified sampling
################################################################################
# pick 10% of the negative samples 
Xtrain_neg <- Xtrain[Xtrain$y==0, ]
Xtrain_pos <- Xtrain[Xtrain$y==1, ]
sub_train_neg_id <- sample (c(FALSE, TRUE), size = nrow(Xtrain_neg), replace=TRUE, prob=c(0.9, 0.1))  
Xtrain_sub <- rbind(Xtrain_pos, Xtrain_neg[sub_train_neg_id,])

lead_sub <-rbind(Xtrain_sub, Xtest)


# the mean conversion rate in train and test
c(sum(lead_sub$y), sum(Xtrain_sub$y), sum(Xtest$y))
c(mean(lead_sub$y), mean(Xtrain_sub$y), mean(Xtest$y))
c(nrow(lead_sub), nrow(Xtrain_sub), nrow(Xtest))
c(1, nrow(Xtrain_sub)/nrow(lead_sub), nrow(Xtest)/nrow(lead_sub))

# no model in the sampled train and test
same_p_sub <- mean(Xtrain_sub$y)



priorp_sub <- sum(Xtrain_sub$y)/dim(Xtrain_sub)[[1]]
priorp_sub


a.r_sub <- rpart(myformula, method="class", control=rc, data=Xtrain_sub, parms=list(prior=c(1-priorp_sub, priorp_sub), loss=loss))

######################## weighted rpart model #############
#a.r_sub_w <- rpart(myformula, method="class", control=rc, data=Xtrain_sub, weight = c(rep(1, nrow(Xtrain_pos)), rep(10, nrow(Xtrain_sub)-nrow(Xtrain_pos))), parms=list(prior=c(1-priorp_sub, priorp_sub), loss=loss))
#
#tree_nleafnodes(a.r_sub_w)
#tree_largest_smallest_leaf(a.r_sub_w)
#tree_depth(a.r_sub_w)
#
# 
#printcp(a.r_sub_w)
##############################################################




tree_nleafnodes(a.r_sub)
tree_largest_smallest_leaf(a.r_sub)
tree_depth(a.r_sub)



printcp(a.r_sub)



xerror.plot(a.r_sub)



plotcp(a.r_sub)

# prune the tree 
#a.r_sub <-rpart.prune(a.r_sub)
a.r_sub <- rpart.prune.0SE(a.r_sub)


variables_used_in_tree(a.r_sub)


a_sub <- variable.importance.used.in.tree(a.r_sub)
a_sub

write.table(cbind(sapply(names(a_sub), formula_term_to_column_name), a_sub), file="VariableImportance_currentlead_full_sampling.txt", sep='|', row.names=names(a_sub))

asRules(a.r_sub)

################################################################################
### predict with the tree after sampling
################################################################################

Xtest <- ResetUnseenLevels(Xtrain_sub, Xtest, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "CampaignChannel", "JobSeniority", "Suppress_from_Campaigns", "PreviousREspShipMeth", "CampaignProductLine5")) 
 
 Xtest <- ResetUnseenLevels(Xtrain_sub, Xtest, columnnames=c("CampaignProductLine5", "RespPriority"))
 levels(Xtrain_sub$CampaignProductLine5)
 
 Xtest$CampaignProductLine5 <- NA
 
rpart_test_sub_p <- predict(a.r_sub, newdata=Xtest) [,2]
summary(rpart_test_sub_p)
hist(rpart_test_sub_p)

str(rpart_test_sub_p)
head(rpart_test_sub_p)

rpart_test_sub_p_adjusted <- rpart_test_sub_p 
length(rpart_test_sub_p_adjusted)

### post stratified sampling adjustment
for (i in 1:length(rpart_test_sub_p)) {
  rpart_test_sub_p_adjusted[i] <- 0.1*rpart_test_sub_p[i]/(1-0.9*rpart_test_sub_p[i])
} 

summary(rpart_test_sub_p_adjusted)

##### weighted rpart prediction ########
#rpart_test_sub_w_p <- predict(a.r_sub_w, newdata=Xtest) [,2]
#summary(rpart_test_sub_w_p)

summary(rpart_test_sub_p)


#hist(rpart_test_sub_p-rpart_test_sub_p_adjusted)

 myCaptureCurve(ActualAmount=Xtest$y,
 RankVectors=cbind(rpart_test_p, rpart_test_sub_p_adjusted),
 MarkPoints=c(mean(Xtest$y),0.1, 0.2),
 RankVectorsToMark=c(TRUE, TRUE),
 LegendTexts=c("NoSampling", "StratifiedSampling"),
 ylab="% of converted Opportunities captured by selecting the best X%",
 xlab=paste("Top % of Leads (total", nrow(Xtest), "out-sample)"),
 showAUC=TRUE)
 

##########################################################################################
### optimization on selecting the best channel  for testset using stratified sampling
##########################################################################################
levels(testsetnow$RespShipMeth)
levels(Xtrain$RespShipMeth)

unique(Xtrain$RespShipMeth)
unique(testsetnow$RespShipMeth)

testsetnow_open <-testsetnow[testsetnow$Converted == 0, ]


nrow(testsetnow_open)  #273718
nrow(testsetnow)    #274007
sum(testsetnow$Converted)  #289


#c_all <-levels(testsetnow$RespShipMeth)

testsetmarch31_open<-testsetmarch31[testsetmarch31[, 'Converted']==0,]

nrow(testsetmarch31_open) #273775
nrow(testsetmarch31)
sum(testsetmarch31$Converted)

testsetmarch1_open <- testsetmarch1[testsetmarch1$Converted ==0, ]
nrow(testsetmarch1_open)     #all open

testsetmarch11_open <- testsetmarch11[testsetmarch11$Converted ==0,]
nrow(testsetmarch11)
nrow(testsetmarch11_open)

testsetmarch21_open <- testsetmarch21[testsetmarch21$Converted ==0,]
nrow(testsetmarch21)
nrow(testsetmarch21_open)

testsetapril11_open <- testsetapril11[testsetapril11$Converted ==0,]
nrow(testsetapril11)
nrow(testsetapril11_open)

Xtest_channel <-testsetnow_open

Xtest_channel <-testsetmarch31_open 

Xtest_channel <- testsetmarch1_open

Xtest_channel <- testsetmarch11_open

Xtest_channel <- testsetmarch21_open

Xtest_channel <- testsetapril11_open

str(Xtest_channel)

c_train <-unique(Xtrain$RespShipMeth)  #c means channel

##### this is to use stratified sampling model ####
Xtest_channel <- ResetUnseenLevels(Xtrain_sub, Xtest_channel, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "CampaignChannel", "JobSeniority", "CampaignBusinessDivision", "CampaignBusinessUnit", "Suppress_from_Campaigns","RespRespType_s", "BusinessUnit_s", "PreviousREspShipMeth"))

###### this is to use the original model ####
Xtest_channel <- ResetUnseenLevels(Xtrain, Xtest_channel, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "CampaignChannel", "JobSeniority", "CampaignBusinessDivision", "CampaignBusinessUnit", "Suppress_from_Campaigns","RespRespType_s", "BusinessUnit_s", "PreviousREspShipMeth"))

unique(Xtest_channel$RespShipMeth)
unique(Xtest$RespShipMeth)

summary(testsetnow$Suppress_from_Campaigns)  #why Suppress_from_Campaign has new levels 1????
summary(Xtrain$Suppress_from_Campaigns)

unique(Xtest_channel$PreviousREspShipMeth)

### re-set the PreviousREspShipMeth: use the current RespShipMeth if current RespShipMeth is not null

Xtest_channel$PreviousREspShipMeth <-ifelse(is.na(Xtest_channel$RespShipMeth), as.character(Xtest_channel$PreviousREspShipMeth), as.character(Xtest_channel$RespShipMeth))
Xtest_channel$PreviousREspShipMeth <-as.factor(Xtest_channel$PreviousREspShipMeth)

unique(Xtrain$PreviousREspShipMeth)
#unique(Xtest$PreviousREspShipMeth)
unique(Xtest_channel$PreviousREspShipMeth)

#### this is to use the stratified smapling model
Xtest_channel <- ResetUnseenLevels(Xtrain_sub, Xtest_channel, columnnames=c("PreviousREspShipMeth", "CampaignBusinessGroup", "CampaignProductLine5")) #

### this is to use the original model ################################
Xtest_channel <- ResetUnseenLevels(Xtrain, Xtest_channel, columnnames=c("PreviousREspShipMeth", "CampaignBusinessGroup", "CampaignProductLine5"))

Xtest_channel$CampaignProductLine5 <- NA


p_channel<-Xtest_channel$LeadID
p_channel <-as.data.frame(p_channel)
colnames(p_channel) <- "LeadID"


#### this is to use the stratified modeling
for (c in c_train) {
  Xtest_channel$RespShipMeth <- c
  p <-predict(a.r_sub, newdata=Xtest_channel)[,2]
  p <-as.vector (p)
  p_channel <-cbind(p_channel, p)
}
###### this is to use the original model
for (c in c_train) {
  Xtest_channel$RespShipMeth <- c
  p <-predict(a.r, newdata=Xtest_channel)[,2]
  p <-as.vector (p)
  p_channel <-cbind(p_channel, p)
}




head(p_channel)


p_channel <-cbind(p_channel, propensityToBeQualified=apply(p_channel[,-1], 1, max))

nc <-length(c_train)

for (i in 1:nc) {
  equ_max <- p_channel[, i+1]== p_channel[, 'propensityToBeQualified']
  p_channel<-cbind(p_channel, equ_max)     # the column number of whether ith channel equals to the max is 1+nc+1+i
} 




#the input to the bestChannel function is an vector which indicates which channel reaches the max probability  a vector of 1 or 0 
#equ_max <-c(1,1,0,0,0,0)
bestChannel <-function(equ_max) { 
  c_train_ind<- c(1:nc)*equ_max
  c <-c_train[c_train_ind]
  best <-sample(c, 1)
  best <- as.character(best)
  return (best)
  }
 


best <- apply(p_channel[, (nc+2+1):(nc+2+nc)], 1, bestChannel)
best

p_channel <- cbind(p_channel, bestChannel = best)

#p_channel$max_p <- as.numeric(p_channel$max_p)

### post stratified sampling probability adjustment

#save(p_channel, file ='p_channel_b4_adjust_march31.RData')
#load('p_channel_b4_adjust_march31.RData')

a <-p_channel$propensityToBeQualified
str(a)

for (i in 1:length(a)) {
  a[i] <- 0.1*a[i]/(1-0.9*a[i])
} 



p_channel$propensityToBeQualified <- as.vector(a)
a[1]
#p_channel$propensityToBeQualified[273772]  #0.5833 correct


p_channel$neg_p <- 1-p_channel$propensityToBeQualified

p_channel$rank <-rank(p_channel$neg_p, ties.method = "random")

p_channel$topPercentage <-round(100*p_channel$rank/nrow(p_channel), 2)

p_channel[p_channel$topPercentage < 5,]




p_channel$decisionStage <-ifelse(p_channel$topPercentage <= 20, "Qualify", "Evaluate")

head(p_channel)

ind_1resp <- c(1:nrow(testsetmarch31_open))[testsetmarch31_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

ind_1resp <- c(1:nrow(testsetnow_open))[testsetnow_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

ind_1resp <- c(1:nrow(testsetmarch1_open))[testsetmarch1_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

ind_1resp <- c(1:nrow(testsetmarch11_open))[testsetmarch11_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

ind_1resp <- c(1:nrow(testsetmarch21_open))[testsetmarch21_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

ind_1resp <- c(1:nrow(testsetapril11_open))[testsetapril11_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

ind_1resp <- c(1:nrow(testsetmarch11_open))[testsetmarch11_open$numOfRespInLast12M == 1 & p_channel$topPercentage >20]

head(ind_1resp)
#'217433'%in% ind_1resp
str(ind_1resp)



nrow(testsetmarch31_open)
length(ind_1resp)
str(ind_1resp)
summary(ind_1resp)
head(ind_1resp)


head(ind_1resp)

p_channel$decisionStage[ind_1resp] <- "Research"

p_channel$predictionDate <-as.POSIXlt('2014-03-11')

output <-p_channel[, c("predictionDate","LeadID", "propensityToBeQualified", "rank", "topPercentage", "bestChannel", "decisionStage")]

output$propensityToBeQualified<-round(output$propensityToBeQualified, 4)

head(output) 
head(testsetmarch31)

write.table(output, file="output_march11.csv", sep="|", row.names=F, col.names=T)





