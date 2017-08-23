setwd ("C:\\Users\\wanghaiy\\Documents\\IMA\\Frank\\Frank2\\R_codes")

library(RJDBC)

source('myRfunctions_plc.r')

options(java.parameters = "-Xmx24g")
options(digits=4)

vDriver = JDBC(driverClass="com.vertica.jdbc.Driver", classPath="C:/vertica-jdbc-7.0.1-0.jar")
vertica_pro=dbConnect(vDriver, "jdbc:vertica://shr2-vrt-pro-vglb1.houston.hp.com:5433/shr1_vrt_pro", "haiyan.wang2@hp.com", "your password here")

##### lead exploration before we got the outcome/opportunity data #####
lead <- dbGetQuery(vertica_pro, "select * from Marketing_IDS_stage.plc_rinput_resp_cont_camp_prodlineName where random() < 0.1")
save(lead, file="resp_cont_camp_prodlineName_sample.RData")

load("resp_cont_camp_prodlineName_sample.RData")

dim(lead)  
str(lead) 
names(lead)


##### get the corresponding RespDttm of opportunities #####
RespDttmOfOppo <- dbGetQuery(vertica_pro, "select RespID, RespDttm, DateCreated, CloseDate from marketing_ids_stage.plc_response_act_contact_outcome
where dwContactPK <> '0'
and RespDttm > '1990-01-01'
and RespDttm < '2014-05-01'
and Converted = '1'")

save(RespDttmOfOppo, file="respdttm_oppr.RData")

str(RespDttmOfOppo)



##### format change #####
lead[,c('RespID', 'dwContactPK')] <- lapply(lead[,c('RespID', 'dwContactPK')], as.factor) 

lead[, c('RespDttm', 'Email_Validity_Dttm')] <- lapply(lead[, c('RespDttm', 'Email_Validity_Dttm')], as.POSIXlt) 

lead[, c('GeneratedToEloquaSFDCDttm', 'PreviousRespDate')] <- lapply(lead[, c('GeneratedToEloquaSFDCDttm', 'PreviousRespDate')], as.POSIXlt)

lead[, 'ContProdQuant1'] <- as.numeric(lead[, 'ContProdQuant1'])


lead[, c('CompCity', 'CompStPv','CompCountry', 'ContProd1', 'RespRespType_s',  'RespRespMethod_s', 'RespPriority', 'RespShipMeth', 'RespTriggeringCd', 'RespTimeFrame', 'RespWillingToBuy',  'RespPartnerLeadShareFlg', 'RespContactReq', 'BusinessUnit_s', 'ContactedByTMFlag', 'BANTValidatedYN', 'RespTouchPoint', 'department_name', 'job_level_category', 'job_function_category', 'countryName', 'BusinessFuction', 'JobRole', 'ContRespAreaTx', 'MasterLeadFlg', 'Email_validity', 'Language', 'JobSeniority', 'JobResponsibilities', 'PurchasingRole', 'Suppress_from_Campaigns', 'SFDCAccountID', 'mappedHPProductline1', 'PreviousREspShipMeth')] <- lapply (lead[, c('CompCity','CompStPv',  'CompCountry', 'ContProd1', 'RespRespType_s',  'RespRespMethod_s', 'RespPriority', 'RespShipMeth', 'RespTriggeringCd', 'RespTimeFrame', 'RespWillingToBuy',  'RespPartnerLeadShareFlg', 'RespContactReq', 'BusinessUnit_s', 'ContactedByTMFlag', 'BANTValidatedYN', 'RespTouchPoint', 'department_name', 'job_level_category', 'job_function_category', 'countryName', 'BusinessFuction', 'JobRole', 'ContRespAreaTx', 'MasterLeadFlg', 'Email_validity', 'Language', 'JobSeniority', 'JobResponsibilities', 'PurchasingRole', 'Suppress_from_Campaigns', 'SFDCAccountID', 'mappedHPProductline1', 'PreviousREspShipMeth')], as.factor)
        
save(lead, file="resp_cont_camp_prodlineName_sample.RData")


##### exploring the dataset #####
s = summary(lead)
head(s)
write.table(s, file="lead_sample_summary.csv", sep="|")

str(s)

load('resp_cont_camp_prodlineName_sample.RData')
summary(lead)
str(lead)  #10% of all the leads

##### get the list of respIDs in this sample #####
write.table(lead[,'RespID'], file = "sample_RespID.csv", sep="|", row.names=F, col.names=T)


##### download the converted RespIDs #####
converted <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_sample_RespID_converted_noD")
str(converted)      #data frame with column name RespID

##### append the outcome column to lead  #####
y <-lead$RespID %in% converted$RespID

sum(y)   #5548

lead <-cbind(lead, y=y)
save(lead, file='sample_lead_with_outcome_Apr25.RData')
load('sample_lead_with_outcome_Apr25.RData')

ncol(X)

############### may 7 2014  with leadID
currentLead <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current where random() < 0.1") 
save(currentLead, file='currentLead.RData') 

str(currentLead)     #284004

##################################################################
## current lead format change
##################################################################
 
 
predictionDate <- as.POSIXlt('2014-04-22')

currentLead <- lead_format_change (currentLead, pedictionDate)
str(currentLead)
save(currentLead, file='currentLead.RData') 
 

################################################################################
##    start from March 1, 2014 
################################################################################
predictionDate<- as.POSIXlt('2014-03-01')

leadmarch1 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_march12014 where random() <0.1") 
save(leadmarch1, file='leadmarch1.RData')
str(leadmarch1)



leadmarch1 <-lead_format_change(leadmarch1, predictionDate)
 
str(leadmarch1)
save(leadmarch1, file='leadmarch1.RData')
sum(leadmarch1[,'LifeStatus'])

 
 

####################################################################
## get data on March 31 2014
####################################################################

predictionDate<- as.POSIXlt('2014-03-31')

leadmarch31 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_march312014 where random() <0.1") 
save(leadmarch31, file='leadmarch31.RData')
str(leadmarch31)

sum(leadmarch31$Converted)

leadmarch31 <- lead_format_change(leadmarch31, predictionDate)
 
 
str(leadmarch31)
save(leadmarch31, file='leadmarch31.RData')
sum(leadmarch31[,'LifeStatus'])  #4042

################################################################################
### get data on March 11 2014
################################################################################
predictionDate<- as.POSIXlt('2014-03-11')

leadmarch11 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_march112014 where random() <0.1") 
save(leadmarch11, file='leadmarch11.RData')
str(leadmarch11)

sum(leadmarch11$Converted)

leadmarch11 <- lead_format_change(leadmarch11, predictionDate)
 
str(leadmarch11)
save(leadmarch11, file='leadmarch11.RData')
sum(leadmarch11[,'LifeStatus'])


################################################################################
### get data on March 21 2014
################################################################################
predictionDate<- as.POSIXlt('2014-03-21')

leadmarch21 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_march212014 where random() <0.1") 
save(leadmarch21, file='leadmarch21.RData')
str(leadmarch21)

sum(leadmarch21$Converted)

leadmarch21 <- lead_format_change(leadmarch21, predictionDate)
 
 
str(leadmarch21)
save(leadmarch21, file='leadmarch21.RData')
sum(leadmarch21[,'LifeStatus'])


################################################################################
### get data on April 11 2014
################################################################################
predictionDate<- as.POSIXlt('2014-04-11')

leadapril11 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_april112014 where random() <0.1") 
save(leadapril11, file='leadapril11.RData')
str(leadapril11)

sum(leadapril11$Converted) #4010


leadapril11 <- lead_format_change (leadapril11, predictionDate)
 
 
 
str(leadapril11)
save(leadapril11, file='leadapril11.RData')
sum(leadapril11[,'LifeStatus'])




########################### get a 10% of open leads on March 1 2014

testsetmarch1 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_march12014 where LifeStatus = '0' and random() <0.1") 
save(testsetmarch1, file='testsetmarch1.RData') 

str(testsetmarch1)   #274008 

predictionDate<- as.POSIXlt('2014-03-01') 

testsetmarch1 <- lead_format_change (testsetmarch1)

write.table(as.numeric(as.character(testsetmarch1[,'LeadID'])), file = "testsetmarch_LeadID.csv", sep="|", row.names=F, col.names=F) 
str(testsetmarch1)
testsetmarch1 <-testsetmarch1[-is.na(testsetmarch1$LeadID), ]
 
save(testsetmarch1, file='testsetmarch1.RData')
 
  
 

########################## the corresponding test set profile at current time point: April 22, 2014
testsetnow <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_testsetnow") 
save(testsetnow, file='testsetnow.RData')
str(testsetnow)  

predictionDate <- as.POSIXlt('2014-04-22')   

testsetnow <- lead_format_change(testsetnow, predictionDate)

summary(testsetnow[, 'recency'])

str(testsetnow)
save(testsetnow, file='testsetnow.RData')
 
 
########################## the corresponding test set profile on March 31, 2014 
testsetmarch31 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_testsetmarch31") 
save(testsetmarch31, file='testsetmarch31.RData')

str(testsetmarch31)  

predictionDate <- as.POSIXlt('2014-03-31')
   
testsetmarch31 <- lead_format_change(testsetmarch31)

str(testsetmarch31)

save(testsetmarch31, file='testsetmarch31.RData')
 
########################## the corresponding test set profile on March 11, 2014 
testsetmarch11 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_testsetmarch11") 
save(testsetmarch11, file='testsetmarch11.RData')

str(testsetmarch11)

predictionDate <- as.POSIXlt('2014-03-11')     

testsetmarch11 <- lead_format_change(testsetmarch11, predictionDate)

str(testsetmarch11)

save(testsetmarch11, file='testsetmarch11.RData')
 
 
########################## the corresponding test set profile on March 21, 2014 
testsetmarch21 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_testsetmarch21") 
save(testsetmarch21, file='testsetmarch21.RData')

str(testsetmarch21) 

predictionDate <- as.POSIXlt('2014-03-21')    

testsetmarch21 <- lead_format_change(testsetmarch21, predictionDate)

str(testsetmarch21)
save(testsetmarch21, file='testsetmarch21.RData')
 
########################## the corresponding test set profile on April 11, 2014 
testsetapril11 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_testsetapril11") 
save(testsetapril11, file='testsetapril11.RData')

predictionDate <- as.POSIXlt('2014-04-11')
str(testsetapril11)

testsetapril11 <-lead_format_change(testsetapril11, predictionDate)
save(testsetapril11, file='testsetapril11.RData')
   
   
################################################################################
## get lead with account
################################################################################
currentleadacc <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current_with_account where random() < 0.1") 
save(currentleadacc, file='currentleadacc.RData')

str(currentleadacc)

predictionDate <- as.POSIXlt('2014-04-22')

currentleadacc <- lead_format_change(currentleadacc, predictionDate)

currentleadacc <- subset(currentleadacc, select = -c(BusinessName , Address, SecondAddress, City, ZipCode, sduns, amidl2, company_name))

currentleadacc <- subset(currentleadacc, select = -c(GRMID))

currentleadacc <- subset(currentleadacc, select = -c(CompCity, CompStPv, CompCountry))

load('currentleadacc.RData')


summary(currentleadacc)


currentleadacc <- lead_acc_format_change (currentleadacc)

currentleadacc$recency[currentleadacc$recency<0] <-0

save(currentleadacc, file='currentleadacc.RData')


################################################################################
## get the lead without account informaiton
################################################################################
currentleadnoacc <-  dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current_without_account where random() < 0.1") 
save(currentleadnoacc, file='currentleadnoacc.RData')

str(currentleadnoacc)

predictionDate <- as.POSIXlt('2014-04-22')

currentleadnoacc <- lead_format_change(currentleadnoacc, predictionDate)

currentleadnoacc$recency[currentleadnoacc$recency<0] <-0

save(currentleadnoacc, file='currentleadnoacc.RData')



################################################################################
## get another set of current lead
################################################################################
currentlead2 <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current where random() < 0.2")
save(currentlead2, file = 'currentlead2.RData')

str(currentlead2)

predictionDate <- as.POSIXlt('2014-04-22')

currentlead2 <- lead_format_change(currentlead2, predictionDate)

currentlead2$recency[currentlead2$recency<0] <-0

save(currentlead2, file = 'currentlead2.RData')

################################################################################
## get full set of current lead
################################################################################
currentlead_full <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current")
save(currentlead_full, file = 'currentlead_full.RData')

str(currentlead_full)

predictionDate <- as.POSIXlt('2014-04-22')

currentlead_full <- lead_format_change(currentlead_full, predictionDate)

currentlead_full$recency[currentlead_full$recency<0] <-0

save(currentlead_full, file = 'currentlead_full.RData')

################################################################################
## get a set of lead with social data (twitter)
################################################################################
lead_soc <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current_with_social where random() < 0.2")
save(lead_soc, file = 'lead_soc.RData')

str(lead_soc)

predictionDate <- as.POSIXlt('2014-04-22')

lead_soc <- lead_format_change(lead_soc, predictionDate)

lead_soc$recency[lead_soc$recency<0] <-0

save(lead_soc, file = 'lead_soc.RData')


################################################################################
## get lead with tactic
################################################################################

leadtactic <- dbGetQuery(vertica_pro, "select * from marketing_ids_stage.plc_lead_current_tactic")


   








