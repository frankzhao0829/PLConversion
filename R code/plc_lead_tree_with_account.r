setwd ("C:\\Users\\wanghaiy\\Documents\\IMA\\Frank\\Frank2\\R_codes")

source('myRfunctions_plc.r')

library("rpart")

library("rattle")

load('currentleadacc.RData')


X <- currentleadacc
names(X)[21] <- 'y'  #the structure of lead with accounts is different

summary(X$totalConvertedInLast12MAtContactLevel)

X[X$y ==1, 'totalConvertedInLast12MAtContactLevel'] <- X[X$y == 1, 'totalConvertedInLast12MAtContactLevel']-1
X[X$y ==1, 'totalConvertedInHistoryAtContactLevel'] <- X[X$y == 1, 'totalConvertedInHistoryAtContactLevel']-1

X[X$y ==1, 'totalAccountConvertedInLast12M'] <- X[X$y == 1, 'totalAccountConvertedInLast12M']-1
X[X$y ==1, 'totalAccountConvertedInHistory'] <- X[X$y == 1, 'totalAccountConvertedInHistory']-1



str(X)
names(X)
#summary(X$firstInterestMatchYN)

nrow(X)  


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

nrow(Xtrain)    



################################################################################
### no model
################################################################################ 
same_p <- mean(Xtrain$y)


################################################################################
### classification tree
################################################################################
 
#### this formula incorporates account attributes
myformula <- (y ~
 #+ `LeadID`  # 72150 unique values
#+ `dwContactPK`  # 71067 unique values
#+ `RespID`  # 72150 unique values
#+ `RespDttm` 
#+ `CampaignPK`  # 9016 unique values
+ `CampaignBusinessGroup`  # 9 unique values
+ `CampaignBusinessDivision`  # 45 unique values
+ `CampaignBusinessUnit`  # 39 unique values
+ ifelse(is.na(`CampaignChannel`),'Unknown',as.character(`CampaignChannel`))  # 14 unique values
#+ ifelse(is.na(`CampaignTactic`),'Unknown',as.character(`CampaignTactic`))  # 18 unique values
+ ifelse(is.na(`CampaignProductLine1`),'Unknown',as.character(`CampaignProductLine1`))  # 25 unique values
+ ifelse(is.na(`CampaignProductLine2`),'Unknown',as.character(`CampaignProductLine2`))  # 13 unique values
+ ifelse(is.na(`CampaignProductLine3`),'Unknown',as.character(`CampaignProductLine3`))  # 10 unique values
+ ifelse(is.na(`CampaignProductLine4`),'Unknown',as.character(`CampaignProductLine4`))  # 5 unique values
+ ifelse(is.na(`CampaignProductLine5`),'Unknown',as.character(`CampaignProductLine5`))  # 4 unique values
+ ifelse(is.na(`ProductLineInterest1`),'Unknown',as.character(`ProductLineInterest1`))  # 107 unique values
+ ifelse(is.na(`ProductLineInterest2`),'Unknown',as.character(`ProductLineInterest2`))  # 63 unique values
+ ifelse(is.na(`ProductLineInterest3`),'Unknown',as.character(`ProductLineInterest3`))  # 46 unique values
+ ifelse(is.na(`ProductLineInterest4`),'Unknown',as.character(`ProductLineInterest4`))  # 31 unique values
+ ifelse(is.na(`ProductLineInterest5`),'Unknown',as.character(`ProductLineInterest5`))  # 20 unique values
#+ `ConvertedWithAmount` 
#+ ifelse(is.na(`OppDateCreated`),as.POSIXct('2012-09-27 14:10:00'),`OppDateCreated`) 
#+ ifelse(is.na(`OppCloseDate`),as.POSIXct('2012-10-31 23:59:00'),`OppCloseDate`) 
#+ ifelse(is.na(`OppStageName`),'Unknown',as.character(`OppStageName`))  # 10 unique values
#+ ifelse(is.na(`OppStatus`),'Unknown',as.character(`OppStatus`))  # 2 unique values
#+ ifelse(is.na(`OppOwnerGBU`),'Unknown',as.character(`OppOwnerGBU`))  # 21 unique values
#+ ifelse(is.na(`OppCountry`),'Unknown',as.character(`OppCountry`))  # 46 unique values
#+ ifelse(is.na(`OppCurrency`),'Unknown',as.character(`OppCurrency`))  # 10 unique values
#+ ifelse(is.na(`OppClosed01`),'Unknown',`OppClosed01`) 
#+ ifelse(is.na(`OppAmount`),-1,`OppAmount`) 
#+ `LifeStatus` 
#+ ifelse(is.na(`ContProd1`),'Unknown',as.character(`ContProd1`))  # 709 unique values
+ `RespRespType_s`  # 13 unique values
+ ifelse(is.na(`RespRespMethod_s`),'Unknown',as.character(`RespRespMethod_s`))  # 13 unique values
+ ifelse(is.na(`RespPriority`),'Unknown',as.character(`RespPriority`))  # 5 unique values
+ `RespShipMeth`  # 7 unique values
#+ ifelse(is.na(`RespTriggeringCd`),'Unknown',as.character(`RespTriggeringCd`))  # 323 unique values
+ ifelse(is.na(`RespTimeFrame`),'Unknown',as.character(`RespTimeFrame`))  # 24 unique values
+ `RespWillingToBuy`  # 3 unique values
+ ifelse(is.na(`ContProdQuant1`),-1,`ContProdQuant1`) 
+ `RespPartnerLeadShareFlg`  # 3 unique values
+ ifelse(is.na(`RespContactReq`),'Unknown',as.character(`RespContactReq`))  # 7 unique values
+ `BusinessUnit_s`  # 45 unique values
+ `ContactedByTMFlag`  # 2 unique values
+ ifelse(is.na(`BANTValidatedYN`),'Unknown',as.character(`BANTValidatedYN`))  # 3 unique values
#+ ifelse(is.na(`GeneratedToEloquaSFDCDttm`),as.POSIXct('1899-12-31'),`GeneratedToEloquaSFDCDttm`) 
+ ifelse(is.na(`RespTouchPoint`),'Unknown',as.character(`RespTouchPoint`))  # 73 unique values
#+ ifelse(is.na(`department_name`),'Unknown',as.character(`department_name`))  # 268 unique values
+ ifelse(is.na(`job_level_category`),'Unknown',as.character(`job_level_category`))  # 9 unique values
+ ifelse(is.na(`job_function_category`),'Unknown',as.character(`job_function_category`))  # 38 unique values
+ ifelse(is.na(`countryName`),'Unknown',as.character(`countryName`))  # 126 unique values
+ ifelse(is.na(`BusinessFuction`),'Unknown',as.character(`BusinessFuction`))  # 32 unique values
+ ifelse(is.na(`JobRole`),'Unknown',as.character(`JobRole`))  # 44 unique values
#+ ifelse(is.na(`ContRespAreaTx`),'Unknown',as.character(`ContRespAreaTx`))  # 779 unique values
+ ifelse(is.na(`MasterLeadFlg`),'Unknown',as.character(`MasterLeadFlg`))  # 3 unique values
+ ifelse(is.na(`Email_validity`),'Unknown',as.character(`Email_validity`))  # 3 unique values
#+ `Email_Validity_Dttm` 
+ ifelse(is.na(`Language`),'Unknown',as.character(`Language`))  # 37 unique values
+ ifelse(is.na(`JobSeniority`),'Unknown',as.character(`JobSeniority`))  # 7 unique values
+ ifelse(is.na(`JobResponsibilities`),'Unknown',as.character(`JobResponsibilities`))  # 19 unique values
+ ifelse(is.na(`PurchasingRole`),'Unknown',as.character(`PurchasingRole`))  # 15 unique values
+ ifelse(is.na(`Suppress_from_Campaigns`),'Unknown',as.character(`Suppress_from_Campaigns`))  # 2 unique values
#+ ifelse(is.na(`SFDCAccountID`),'Unknown',as.character(`SFDCAccountID`))  # 136 unique values
+ `numOfRespInLast12M` 
+ `numOfRespInHistory` 
#+ ifelse(is.na(`PreviousRespDate`),as.POSIXct('1976-07-21'),`PreviousRespDate`) 
+ ifelse(is.na(`PreviousREspShipMeth`),'Unknown',as.character(`PreviousREspShipMeth`))  # 7 unique values
+ `NumOfResToDirectMail` 
+ `NumOfResToWeb` 
+ `NumOfResToEmail` 
+ `NumOfResToPhone` 
+ `NumOfResToManagedBySales` 
+ `NumOfResToCustomerVisit` 
+ ifelse(is.na(`Tactic`),'Unknown',as.character(`Tactic`))  # 18 unique values
+ ifelse(is.na(`PreviousTactic`),'Unknown',as.character(`PreviousTactic`))  # 10 unique values
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
+ ifelse(is.na(`PastSalesOfProductLineInterest1`),-1068.17,`PastSalesOfProductLineInterest1`) 
+ `totalConvertedInLast12MAtContactLevel` 
+ `totalConvertedInHistoryAtContactLevel` 
#+ `totalConvertedWithAmountInHistoryAtContactLevel` 
#+ ifelse(is.na(`totalOppAmountInLast12MAtContactLevel`),-1,`totalOppAmountInLast12MAtContactLevel`) 
#+ ifelse(is.na(`totalOppAmountInHistoryAtContactLevel`),-1,`totalOppAmountInHistoryAtContactLevel`) 
#+ ifelse(is.na(`totalCloseDaysAtContactLevel`),-1,`totalCloseDaysAtContactLevel`) 
#+ ifelse(is.na(`aveCloseDayAtContactLevel`),-1,`aveCloseDayAtContactLevel`) 
#+ ifelse(is.na(`aveOppAmountAtContactLevel`),-1,`aveOppAmountAtContactLevel`) 
+ `totalAccountConvertedInLast12M` 
+ `totalAccountConvertedInHistory` 
#+ `totalAccountConvertedWithAmountInHistory` 
#+ ifelse(is.na(`totalAccountOppAmountInLast12M`),-1,`totalAccountOppAmountInLast12M`) 
#+ ifelse(is.na(`totalAccountOppAmountInHistory`),-1,`totalAccountOppAmountInHistory`) 
#+ ifelse(is.na(`totalAccountCloseDays`),-1,`totalAccountCloseDays`) 
#+ ifelse(is.na(`aveAccountCloseDay`),-1,`aveAccountCloseDay`) 
#+ ifelse(is.na(`aveAccountOppAmount`),-1,`aveAccountOppAmount`) 
+ `Country`  # 126 unique values
+ `Continent`  # 6 unique values
+ ifelse(is.na(`SalesVolumeType`),'Unknown',as.character(`SalesVolumeType`))  # 4 unique values
+ `AnnualSales` 
+ `TotalEmployeeCount` 
+ ifelse(is.na(`TotalEmployeeCountType`),'Unknown',as.character(`TotalEmployeeCountType`))  # 4 unique values
+ `LocalEmployeeCount` 
+ ifelse(is.na(`LocalEmployeeCountType`),'Unknown',as.character(`LocalEmployeeCountType`))  # 4 unique values
+ ifelse(is.na(`YearStart`),1699,`YearStart`) 
+ `SmallBusinessFlag`  # 2 unique values
+ `PubPrivateFlag`  # 2 unique values
#+ `LineOfBusiness`  # 947 unique values
+ ifelse(is.na(`Recent3YearSalesTrend`),'Unknown',as.character(`Recent3YearSalesTrend`))  # 3 unique values
+ `Recent3YearSalesTrendPer` 
+ ifelse(is.na(`Recent3YearEmpTrend`),'Unknown',as.character(`Recent3YearEmpTrend`))  # 3 unique values
+ `Recent3YearEmpTrendPer` 
+ `ThreeYearAgoSalesAmount` 
+ `ThreeYearAgoEmpCount` 
+ ifelse(is.na(`Recent5YearSalesTrend`),'Unknown',as.character(`Recent5YearSalesTrend`))  # 3 unique values
+ `Recent5YearSalesTrendPer` 
+ ifelse(is.na(`Recent5YearEmpTrend`),'Unknown',as.character(`Recent5YearEmpTrend`))  # 3 unique values
+ `Recent5YearEmpTrendPer` 
+ `FiveYearAgoSalesAmount` 
+ `FiveYearAgoEmpCount` 
+ `CurrentYearSalesAmount` 
+ `CurrentYearEmpCount` 
+ ifelse(is.na(`industry_vertical_cd`),'Unknown',as.character(`industry_vertical_cd`))  # 8 unique values
+ ifelse(is.na(`industry_vertical_nm`),'Unknown',as.character(`industry_vertical_nm`))  # 8 unique values
+ ifelse(is.na(`industry_segment_cd`),'Unknown',as.character(`industry_segment_cd`))  # 58 unique values
+ ifelse(is.na(`industry_segment_nm`),'Unknown',as.character(`industry_segment_nm`))  # 58 unique values
+ ifelse(is.na(`sitesubgrp`),'Unknown',as.character(`sitesubgrp`))  # 21 unique values
+ ifelse(is.na(`sitegrp`),'Unknown',as.character(`sitegrp`))  # 9 unique values
+ ifelse(is.na(`partner`),'Unknown',as.character(`partner`))  # 3 unique values
+ ifelse(is.na(`sweetspot_3yrs`),'Unknown',as.character(`sweetspot_3yrs`))  # 3 unique values
+ ifelse(is.na(`sweetspot_5yrs`),'Unknown',as.character(`sweetspot_5yrs`))  # 3 unique values
#+ ifelse(is.na(`first_purchase_dt`),as.POSIXct('2008-10-31'),`first_purchase_dt`) 
#+ ifelse(is.na(`last_purchase_dt`),as.POSIXct('2008-10-31'),`last_purchase_dt`) 
+ ifelse(is.na(`total_revenue`),-1914306579.1297,`total_revenue`) 
+ ifelse(is.na(`total_direct_revneue`),-11880122.8428,`total_direct_revneue`) 
+ ifelse(is.na(`total_indirect_revenue`),-1916336321.9697,`total_indirect_revenue`) 
+ ifelse(is.na(`total_eg_revenue`),-115252555.7278,`total_eg_revenue`) 
+ ifelse(is.na(`total_ipg_revenue`),-8613971.7375,`total_ipg_revenue`) 
+ ifelse(is.na(`total_psg_revenue`),-5919993.7,`total_psg_revenue`) 
+ ifelse(is.na(`total_sw_revenue`),-2442557.1004,`total_sw_revenue`) 
+ ifelse(is.na(`total_services_revenue`),-1619627.52,`total_services_revenue`) 
+ ifelse(is.na(`total_finance_revenue`),-1,`total_finance_revenue`) 
+ ifelse(is.na(`total_revenue_3yrs`),-1675414806.9409,`total_revenue_3yrs`) 
+ ifelse(is.na(`total_direct_revenue_3yrs`),-13357411.5084,`total_direct_revenue_3yrs`) 
+ ifelse(is.na(`total_indirect_revenue_3yrs`),-1676978254.5009,`total_indirect_revenue_3yrs`) 
+ ifelse(is.na(`total_eg_revenue_3yrs`),-82010320.3478,`total_eg_revenue_3yrs`) 
+ ifelse(is.na(`total_ipg_revenue_3yrs`),-3820002.6909,`total_ipg_revenue_3yrs`) 
+ ifelse(is.na(`total_psg_revenue_3yrs`),-3786642.6518,`total_psg_revenue_3yrs`) 
+ ifelse(is.na(`total_sw_revenue_3yrs`),-1611105.2,`total_sw_revenue_3yrs`) 
+ ifelse(is.na(`total_services_revenue_3yrs`),-578989.4533,`total_services_revenue_3yrs`) 
+ ifelse(is.na(`total_finance_revenue_3yrs`),-1,`total_finance_revenue_3yrs`) 
+ ifelse(is.na(`total_revenue_5yrs`),-2167212125.4802,`total_revenue_5yrs`) 
+ ifelse(is.na(`total_direct_revenue_5yrs`),-12643146.8802,`total_direct_revenue_5yrs`) 
+ ifelse(is.na(`total_indirect_revenue_5yrs`),-2169093010.8902,`total_indirect_revenue_5yrs`) 
+ ifelse(is.na(`total_eg_revenue_5yrs`),-111906112.1078,`total_eg_revenue_5yrs`) 
+ ifelse(is.na(`total_ipg_revenue_5yrs`),-8645001.5735,`total_ipg_revenue_5yrs`) 
+ ifelse(is.na(`total_psg_revenue_5yrs`),-5470471.78,`total_psg_revenue_5yrs`) 
+ ifelse(is.na(`total_sw_revenue_5yrs`),-1318742.23,`total_sw_revenue_5yrs`) 
+ ifelse(is.na(`total_services_revenue_5yrs`),-1269627.52,`total_services_revenue_5yrs`) 
+ ifelse(is.na(`total_finance_revenue_5yrs`),-1,`total_finance_revenue_5yrs`) 
+ ifelse(is.na(`fy09_revenue`),-25222568.44,`fy09_revenue`) 
+ ifelse(is.na(`fy09_direct_revenue`),-3664479.7457,`fy09_direct_revenue`) 
+ ifelse(is.na(`fy09_indirect_revenue`),-25226152.82,`fy09_indirect_revenue`) 
+ ifelse(is.na(`fy09_eg_revenue`),-9320284.93,`fy09_eg_revenue`) 
+ ifelse(is.na(`fy09_ipg_revenue`),-3348004.3795,`fy09_ipg_revenue`) 
+ ifelse(is.na(`fy09_psg_revenue`),-1436414.49,`fy09_psg_revenue`) 
+ ifelse(is.na(`fy09_sw_revenue`),-2711241.8,`fy09_sw_revenue`) 
+ ifelse(is.na(`fy09_services_revenue`),-750001,`fy09_services_revenue`) 
+ ifelse(is.na(`fy09_finance_revenue`),-1,`fy09_finance_revenue`) 
+ ifelse(is.na(`fy10_revenue`),-452290107.84,`fy10_revenue`) 
+ ifelse(is.na(`fy10_direct_revenue`),-1828123.6,`fy10_direct_revenue`) 
+ ifelse(is.na(`fy10_indirect_revenue`),-452422151.44,`fy10_indirect_revenue`) 
+ ifelse(is.na(`fy10_eg_revenue`),-16031350.9,`fy10_eg_revenue`) 
+ ifelse(is.na(`fy10_ipg_revenue`),-4600083.4,`fy10_ipg_revenue`) 
+ ifelse(is.na(`fy10_psg_revenue`),-1646824.86,`fy10_psg_revenue`) 
+ ifelse(is.na(`fy10_sw_revenue`),-530550.16,`fy10_sw_revenue`) 
+ ifelse(is.na(`fy10_services_revenue`),-1864074.28,`fy10_services_revenue`) 
+ ifelse(is.na(`fy10_finance_revenue`),-1,`fy10_finance_revenue`) 
+ ifelse(is.na(`fy11_revenue`),-584847883.1609,`fy11_revenue`) 
+ ifelse(is.na(`fy11_direct_revenue`),-12784234.6425,`fy11_direct_revenue`) 
+ ifelse(is.na(`fy11_indirect_revenue`),-584920072.5509,`fy11_indirect_revenue`) 
+ ifelse(is.na(`fy11_eg_revenue`),-21762695.63,`fy11_eg_revenue`) 
+ ifelse(is.na(`fy11_ipg_revenue`),-3140564.3434,`fy11_ipg_revenue`) 
+ ifelse(is.na(`fy11_psg_revenue`),-6945206.62,`fy11_psg_revenue`) 
+ ifelse(is.na(`fy11_sw_revenue`),-977349.28,`fy11_sw_revenue`) 
+ ifelse(is.na(`fy11_services_revenue`),-1990078.86,`fy11_services_revenue`) 
+ ifelse(is.na(`fy11_finance_revenue`),-1,`fy11_finance_revenue`) 
+ ifelse(is.na(`fy12_revenue`),-566004792.1475,`fy12_revenue`) 
+ ifelse(is.na(`fy12_direct_revenue`),-2920841.5987,`fy12_direct_revenue`) 
+ ifelse(is.na(`fy12_indirect_revenue`),-566546843.1875,`fy12_indirect_revenue`) 
+ ifelse(is.na(`fy12_eg_revenue`),-15104823.94,`fy12_eg_revenue`) 
+ ifelse(is.na(`fy12_ipg_revenue`),-3074567.7408,`fy12_ipg_revenue`) 
+ ifelse(is.na(`fy12_psg_revenue`),-3740176,`fy12_psg_revenue`) 
+ ifelse(is.na(`fy12_sw_revenue`),-657139.42,`fy12_sw_revenue`) 
+ ifelse(is.na(`fy12_services_revenue`),-22764362.15,`fy12_services_revenue`) 
+ ifelse(is.na(`fy12_finance_revenue`),-1,`fy12_finance_revenue`) 
+ ifelse(is.na(`fy13_revenue`),-487991682.0199,`fy13_revenue`) 
+ ifelse(is.na(`fy13_direct_revenue`),-1993541.5098,`fy13_direct_revenue`) 
+ ifelse(is.na(`fy13_indirect_revenue`),-488132333.6499,`fy13_indirect_revenue`) 
+ ifelse(is.na(`fy13_eg_revenue`),-28395555.7538,`fy13_eg_revenue`) 
+ ifelse(is.na(`fy13_ipg_revenue`),-399298.17,`fy13_ipg_revenue`) 
+ ifelse(is.na(`fy13_psg_revenue`),-466743.841,`fy13_psg_revenue`) 
+ ifelse(is.na(`fy13_sw_revenue`),-1172827.19,`fy13_sw_revenue`) 
+ ifelse(is.na(`fy13_services_revenue`),-1130692.2,`fy13_services_revenue`) 
+ ifelse(is.na(`fy13_finance_revenue`),-1,`fy13_finance_revenue`) 
+ ifelse(is.na(`fy14_revenue`),-230890948.7235,`fy14_revenue`) 
+ ifelse(is.na(`fy14_direct_revenue`),-3407119.55,`fy14_direct_revenue`) 
+ ifelse(is.na(`fy14_indirect_revenue`),-231731575.6135,`fy14_indirect_revenue`) 
+ ifelse(is.na(`fy14_eg_revenue`),-24637849.574,`fy14_eg_revenue`) 
+ ifelse(is.na(`fy14_ipg_revenue`),-1140333.1897,`fy14_ipg_revenue`) 
+ ifelse(is.na(`fy14_psg_revenue`),-766903.1431,`fy14_psg_revenue`) 
+ ifelse(is.na(`fy14_sw_revenue`),-996589.85,`fy14_sw_revenue`) 
+ ifelse(is.na(`fy14_services_revenue`),-857956,`fy14_services_revenue`) 
+ ifelse(is.na(`fy14_finance_revenue`),-1,`fy14_finance_revenue`) 
+ `recency` 
+ `numOfProdInterests` 
 #`firstInterestMatchYN` 
)


#### this formula does not include account attributes 
 myformula <- (y~
 #+ `LeadID`  # 194338 unique values
#+ `dwContactPK`  # 192040 unique values
#+ `RespID`  # 194338 unique values
#+ `GRMID` 
#+ `RespDttm` 
#+ `CampaignPK`  # 15790 unique values
+ `CampaignBusinessGroup`  # 12 unique values
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
+ `recency` 
+ `numOfProdInterests` 
+ ifelse(`firstInterestMatchYN`==FALSE, FALSE, TRUE) 
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
#a.r <- rpart.prune(a.r)
a.r <- rpart.prune.0SE(a.r)

#show results
variables_used_in_tree(a.r)

a <- variable.importance.used.in.tree(a.r)
a
str(a)


# output (to a file "VariableImportance.txt") the variable importance:
write.table(cbind(sapply(names(a), formula_term_to_column_name), a), file="VariableImportance_leadaccount_oldmodel_updateNumofconver.txt", sep='|', row.names=names(a))



#output the rules 
asRules(a.r) # requires  package rattle


#######################################################################################
### now, use the tree to predict!
#######################################################################################

 Xtest <- ResetUnseenLevels(Xtrain, Xtest, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "RespPriority", "JobSeniority", "Suppress_from_Campaigns", "PreviousREspShipMeth")) #this will replace the unseen levels in Xtest to the corresponding most popular levels

 Xtest <- ResetUnseenLevels(Xtrain, Xtest, columnnames=c("RespTouchPoint", "countryName", "sitesubgrp", "sitegrp"))

 rpart_test_p <- predict(a.r, newdata=Xtest)[,2]  

head(rpart_test_p)
str(rpart_test_p)
summary(rpart_test_p)
sort(unique(rpart_test_p))
hist(rpart_test_p)
rpart_test_p<-as.vector(rpart_test_p)





myCaptureCurve(ActualAmount=Xtest$y,
 RankVectors=cbind(rpart_test_p),
 MarkPoints=c(mean(Xtest$y),0.2),
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




tree_nleafnodes(a.r_sub)
tree_largest_smallest_leaf(a.r_sub)
tree_depth(a.r_sub)



printcp(a.r_sub)



xerror.plot(a.r_sub)
#a <- locator(n=1)



plotcp(a.r_sub)

# prune the tree 
a.r_sub <-rpart.prune(a.r_sub)
a.r_sub <- rpart.prune.0SE(a.r_sub)


variables_used_in_tree(a.r_sub)


a_sub <- variable.importance.used.in.tree(a.r_sub)
a_sub

write.table(cbind(sapply(names(a_sub), formula_term_to_column_name), a_sub), file="VariableImportance_sampling_leadwithacc.txt", sep='|', row.names=names(a_sub))

################################################################################
### predict with the tree after sampling
################################################################################

Xtest <- ResetUnseenLevels(Xtrain_sub, Xtest, columnnames=c("RespRespMethod_s", "RespTimeFrame", "RespContactReq", "job_function_category", "BusinessFuction", "JobRole", "Language", "JobResponsibilities", "PurchasingRole", "ProductLineInterest1", "ProductLineInterest2", "ProductLineInterest3", "ProductLineInterest4", "ProductLineInterest5", "CampaignProductLine1", "CampaignProductLine2","CampaignProductLine3","CampaignProductLine4","CampaignProductLine5", "CampaignTactic", "Tactic", "PreviousTactic", "CampaignChannel", "JobSeniority", "Suppress_from_Campaigns", "PreviousREspShipMeth", "CampaignProductLine5")) #this w

Xtest <- ResetUnseenLevels(Xtrain_sub, Xtest, columnnames=c("RespTouchPoint", "countryName", "sitesubgrp", "sitegrp", "ProductLineInterest5"))
 
 Xtest <- ResetUnseenLevels(Xtrain_sub, Xtest, columnnames=c("CampaignProductLine5"))
 levels(Xtrain_sub$CampaignProductLine5)

 
 Xtest$CampaignProductLine5 <- NA
 Xtest$ProductLineInterest5 <- NA
 
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



summary(rpart_test_sub_p)


hist(rpart_test_sub_p-rpart_test_sub_p_adjusted)

 myCaptureCurve(ActualAmount=Xtest$y,
 RankVectors=cbind(rpart_test_p, rpart_test_sub_p_adjusted),
 MarkPoints=c(mean(Xtest$y),0.2),
 RankVectorsToMark=c(TRUE, TRUE),
 LegendTexts=c("NoSampling", "StratifiedSampling"),
 ylab="% of converted Opportunities captured by selecting the best X%",
 xlab=paste("Top % of Leads (total", nrow(Xtest), "out-sample)"),
 showAUC=TRUE)
 

